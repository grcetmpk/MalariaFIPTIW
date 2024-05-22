require(data.table)
require(dplyr)
require(tidyr)
require(splines)
require(survey)
require(knitr)
require(kableExtra)
require(geepack)


expit <- function(x){ return(exp(x)/(1+exp(x)))}

### ~~~~~~~ Data Importation ~~~~~ ###

### import patient level data
participant_baseline_data <- fread("PRISM_cohort_Participants.txt")
names(participant_baseline_data) <-  gsub(" ", "_", gsub("\\[|\\]", "", 
                                                         names(participant_baseline_data)))
### import patient repeated measures data
participant_repeated_data <- fread("PRISM_cohort_Participant_repeated_measures.txt")
names(participant_repeated_data) <-  gsub(" ", "_", gsub("\\[|\\]", "",
                                                         names(participant_repeated_data)))
### import household level data
household_baseline_data <- fread("PRISM_cohort_Households.txt")
names(household_baseline_data) <-  gsub(" ", "_", gsub("\\[|\\]", "", 
                                                       names(household_baseline_data)))
### import household repeated data
household_repeated_data <- fread("PRISM_cohort_Household_repeated_measures.txt")
names(household_repeated_data) <-  gsub(" ", "_", gsub("\\[|\\]", "",
                                                       names(household_repeated_data)))



#~~~~ Create single data frame, one row for each patient, inc household data ~~~~~~~~#


#remove columns we don't need
participant_baseline_data <- participant_baseline_data[, c(1,2,3,9,13,14,15)]
participant_repeated_data <- participant_repeated_data[, c(1,2,3,4,8,11,24,31,36,45,48)]
household_baseline_data <- household_baseline_data[, c(1,17, 18, 22, 23, 24, 25, 32, 39)]



#### first join together the participant and household baseline data
baseline_data <- participant_baseline_data %>% 
  left_join(household_baseline_data, by = "Household_Id") # add in time-invar household information


### sort time-varying data by participant_id and then time
participant_repeated_data <- participant_repeated_data %>% 
  arrange(Participant_Id, `Time_since_enrollment_(days)_EUPATH_0000191`)


### merge so we have repeated and time-invariant data
data_children <- participant_repeated_data %>% 
  left_join(baseline_data, by = "Participant_Id") %>% 
  arrange(Participant_Id) %>% # sort by ID
  filter(`Age_at_enrollment_(years)_EUPATH_0000120` >= 2) %>% # remove those who were younger than 2 or
  filter(`Age_at_enrollment_(years)_EUPATH_0000120` <= 11) %>% # older than 11 at initial visit
  filter(`Reason_for_withdrawal_EUPATH_0000208` %in% c("Moved out of area", "Completed study" )) %>% # ind censoring
  filter(`Age_(years)_OBI_0001169` <= 11, `Age_(years)_OBI_0001169` >= 2) %>%# filter by children between 2 and 11 y/o for entire duration
  filter(`Observation_date_EUPATH_0004991` <= "2016-06-30") %>% #remove obs after june 30 2016
  filter(`Drinking_water_source_ENVO_00003064` %in% c("Public tap", "Protected public well" , "River/stream",
                                                      "Protected spring", "Borehole",  "Open public well",        
                                                      "Pond/lake" , "Unprotected spring"  )) #remove piped water sources


data_children$Antimalarial_medication_EUPATH_0000058 <- relevel(as.factor(data_children$Antimalarial_medication_EUPATH_0000058),
                                                                ref = "No malaria medications given")

### randomly sample one child per household

length(unique(data_children$Household_Id.x)) # we have 133 unique households
# 
# set.seed(100)
# 
# participant_list <- c() #empty list to store participants (one per fam)
# hhlist <- tapply(data_children$Participant_Id, data_children$Household_Id.x, unique) #list of ids per family
# for(i in 1:length(hhlist)){
#   fam <- hhlist[[i]]
#   if(length(fam) > 1){
#     participant <- sample(fam, 1) # randomly sample one child from the family
#   }else{
#     participant <- fam 
#   }
#   
#   participant_list <- c(participant_list, participant) #add it to the list
# }
# 
# data_children <- data_children %>% 
#   filter(Participant_Id %in% participant_list)
# 
# length(participant_list)

length(unique(data_children$Participant_Id)) #210 children in 133 households 
dim(data_children) #6475 x 25

### calculate summary of number of events per individual
data_children$Observed <- 1
data_children$obsnumber <- with(data_children, ave(Participant_Id, Participant_Id, FUN = seq_along))


data_children <- data_children %>% select(-c(`Weight_(kg)_IAO_0000414`,`Height_(cm)_EUPATH_0010075` ))



numevents <- summary(tapply(data_children$Observed, data_children$Participant_Id, sum)) 
numevents #min 1 mean 30.8 max 104.0
table(data_children$Observation_type_BFO_0000015) # 210 enrollment, 3478 sched, 2787 unsched




### find out how many obs and subjects have malaria diagnosis

data_children_malaria <- data_children %>% filter(Malaria_diagnosis_EUPATH_0000090 == "Yes")
dim(data_children_malaria) # 1192/6475 (~18.4%) of observations have malaria diagnosis at some point
length(unique(data_children_malaria$Participant_Id)) # 149/210 (~71.0%) of patients had malaria at some point



### Create indicator for Malaria diagnosis (1 if diagnosed)
data_children$Malaria <- ifelse(data_children$Malaria_diagnosis_EUPATH_0000090 == "Yes", 1, 0)

### Create indicator for whether treatment administered on previous visit
data_children$obsnumber <- with(data_children, ave(Participant_Id, Participant_Id, FUN = seq_along))

data_children$scheduled <- ifelse(data_children$Observation_type_BFO_0000015 == "Unscheduled visit", "Yes", "No")


### create lags (set t = 0 to 0.1)
data_children$`Time_since_enrollment_(days)_EUPATH_0000191` <- ifelse(data_children$`Time_since_enrollment_(days)_EUPATH_0000191` == 0, 0.1, data_children$`Time_since_enrollment_(days)_EUPATH_0000191`)
data_children$Time_lag <- data_children$`Time_since_enrollment_(days)_EUPATH_0000191`[c(nrow(data_children),1:(nrow(data_children)-1))]
data_children$Time_lag[data_children$obsnumber == 1] <- 0


#### create variable of whether artmether-lumefantrine prescribed in last 3 days OR 
#### quinine or artesunate in the last 7 days

# go through each individual and get observations from last 3 and 7 days
individuals <- unique(data_children$Participant_Id)

trtwithinoneweek_full <- c()

for(id in individuals){
  
  
  individual_data <- data_children[data_children$Participant_Id == id, ]
  observation_dates <- individual_data$Observation_date_EUPATH_0004991
  observation_dates <- as.Date(observation_dates, format = "%Y-%m-%d")
  
  trtwithinoneweekvec <- c()
  
  for(i in 1:length(observation_dates)){
    obsdate <- observation_dates[i]
    
    dayssince <- difftime(observation_dates, obsdate, units = "days")
    withinoneweek <- which(dayssince > -7 & dayssince < 0)
    
    if(length(withinoneweek) == 0){
      # if no obs within one week, then treated within a week is 0 for this date 
      trtwithinoneweekvec <- c(trtwithinoneweekvec, 0)
    } else { # if we have obs within one week, see what trt were given
      data_withinoneweek <- individual_data[withinoneweek, ]
      trts <- unique(data_withinoneweek$Antimalarial_medication_EUPATH_0000058)
      
      # see if quinine or artesunate was given
      if( "Quinine or Artesunate for complicated malaria" %in% trts){
        trtwithinoneweekvec <- c(trtwithinoneweekvec, 1)
        
      }else if("Quinine for uncomplicated malaria within 14 days of a previous treatment for malaria" %in% trts){
        trtwithinoneweekvec <- c(trtwithinoneweekvec, 1)
      }else{
        trtwithinoneweekvec <- c(trtwithinoneweekvec, 0)
      }
      
    }
    
    
  }
  trtwithinoneweek_full <- c(trtwithinoneweek_full, trtwithinoneweekvec)
}

#### only 1 observations had antimalarials within one week of observation. 
sum(trtwithinoneweek_full)


# create a variable for protected water sources
data_children$waternotprotected <- ifelse(data_children$Drinking_water_source_ENVO_00003064 %in% c("River/stream",   
                                                                                                   "Open public well", "Pond/lake", "Unprotected spring"), 1, 0)


data_children_baseline <- data_children[data_children$Observation_type_BFO_0000015 == "Enrollment", ]


table(data_children_baseline$waternotprotected) #42/210 (20%) unprotected water 
#no missingness :D

#~~~~~~~~~~~~~~~~~~~~~~~~~~~Estimate weights ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~ IPTW weights
# IPTW model should include true confounders and covariates only predictive of outcome. Do not include those only predictive of exposure


#model considering all possible covariates predictive of exposure
exposuremod <- glm(factor(waternotprotected) ~ `Age_(years)_OBI_0001169` + `Sub-county_in_Uganda_EUPATH_0000054` +
                     `Household_wealth_index,_categorical_EUPATH_0000143` + `Food_problems_per_week_EUPATH_0000029` +
                     Human_waste_facilities_EUPATH_0000335 +
                     Observation_type_BFO_0000015 +Dwelling_type_ENVO_01000744 +Persons_living_in_house_count_EUPATH_0000019,
                   family = binomial(link = "logit"), data = data_children)

summary(exposuremod)



outcomemod <- geeglm(Malaria ~ `Age_(years)_OBI_0001169` + `Sub-county_in_Uganda_EUPATH_0000054` +
                       `Household_wealth_index,_categorical_EUPATH_0000143` + `Food_problems_per_week_EUPATH_0000029` +
                       Human_waste_facilities_EUPATH_0000335 +
                       Observation_type_BFO_0000015 + Dwelling_type_ENVO_01000744 + Persons_living_in_house_count_EUPATH_0000019,
                     family = binomial(link = "logit"), data = data_children, corstr = "exchangeable", id =  Participant_Id)

summary(outcomemod)
#  age, wealth index,  food problems per week, waste faciltiies, observation type, highly significant


#include covariates in model that are confounders and predictive only of outcome 
psmod <- glm(factor(waternotprotected) ~ #`Sub-county_in_Uganda_EUPATH_0000054` + 
               `Age_(years)_OBI_0001169` + 
               `Household_wealth_index,_categorical_EUPATH_0000143` +  `Food_problems_per_week_EUPATH_0000029`+
               #Human_waste_facilities_EUPATH_0000335 + 
               #Persons_living_in_house_count_EUPATH_0000019 +
               Observation_type_BFO_0000015,
             family = binomial(link = "logit"), data = data_children)
summary(psmod) #human waste  inflated variance

ps <- expit(predict(psmod))
iptw <- 1/ps*as.numeric(data_children$waternotprotected)+1/(1-ps)*(1-as.numeric(data_children$waternotprotected))
summary(iptw) #max 14.98
sum(iptw > 5)/length(iptw) #2.56%
sum(iptw > 10)/length(iptw) #0.07\%
sum(iptw > 20)/length(iptw) #0.07\%

hist(iptw)

#~~~~~~~~~~ IIW Weights
# IIW weights less sensitive to inclusion of non-confounders.


# weight stabilizer
delta.hat <- coxph(Surv(Time_lag, `Time_since_enrollment_(days)_EUPATH_0000191`, Observed) ~ factor(waternotprotected) - 1, data = data_children)$coef

# model with aux variables
intensitymod <- coxph(Surv(Time_lag, `Time_since_enrollment_(days)_EUPATH_0000191`, Observed) ~ factor(waternotprotected) + `Age_(years)_OBI_0001169` +
                        factor(scheduled) + Sex_PATO_0000047 + `Household_wealth_index,_categorical_EUPATH_0000143` +
                        Persons_living_in_house_count_EUPATH_0000019 + `Sub-county_in_Uganda_EUPATH_0000054` 
                      - 1, 
                      data = data_children)

gamma.hat <- intensitymod$coef

#create dummy vars
data_children$schedulednum <- ifelse(data_children$scheduled == "Yes", 1, 0)
data_children$Male <- ifelse(data_children$Sex_PATO_0000047 == "Male", 1, 0)
data_children$HWI_middle <- ifelse(data_children$`Household_wealth_index,_categorical_EUPATH_0000143` == "Middle", 1, 0)
data_children$HWI_poorest <- ifelse(data_children$`Household_wealth_index,_categorical_EUPATH_0000143` == "Poorest", 1, 0)
data_children$SC_Nagongera <- ifelse(data_children$`Sub-county_in_Uganda_EUPATH_0000054` == "Nagongera", 1, 0)
data_children$SC_Walukuba   <- ifelse(data_children$`Sub-county_in_Uganda_EUPATH_0000054` == "Walukuba  ", 1, 0)


iiw <- exp(cbind(data_children$waternotprotected)%*%delta.hat)/exp(cbind(data_children$waternotprotected, 
                                                                         data_children$`Age_(years)_OBI_0001169`, data_children$schedulednum, 
                                                                         data_children$Male, data_children$HWI_middle, data_children$HWI_poorest,
                                                                         data_children$Persons_living_in_house_count_EUPATH_0000019, 
                                                                         data_children$SC_Nagongera, data_children$SC_Walukuba)%*%gamma.hat)

summary(iiw) #max 1.946
sum(iiw>5)/length(iiw)

fiptiw <- iiw*iptw
data_children$fiptiw <- fiptiw

hist(fiptiw)
summary(fiptiw) #max 26.11
sum(fiptiw > 5)/length(fiptiw) #6.24%
sum(fiptiw > 10)/length(fiptiw) #0.85%
sum(fiptiw > 20)/length(fiptiw) #0.03%



threshold <- quantile(fiptiw, 0.95) 
threshold #5.39

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Results ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# estimate smooth function of time mu(t) using splines for the marginal model
# as in Coloumbe, 2021

terti<-quantile(data_children$`Time_since_enrollment_(days)_EUPATH_0000191` , c(0.3333, 0.66666), type = 1) 
finalmod_unadj <- glm(factor(Malaria) ~ factor(waternotprotected) + bs(data_children$`Time_since_enrollment_(days)_EUPATH_0000191`, 
                                                                       degree=3,knots=c(terti)), data = data_children, family=quasibinomial(link="logit"))
naive_ATE <- summary(finalmod_unadj)$coef[2,1]
naive_SE <- summary(finalmod_unadj)$coef[2,2]
naive_CI_ll <- round(naive_ATE -1.96*naive_SE,3)
naive_CI_ul <- round(naive_ATE +1.96*naive_SE,3)
naive_CI_ll_OR <- round(exp(naive_ATE -1.96*naive_SE),3)
naive_CI_ul_OR <- round(exp(naive_ATE +1.96*naive_SE),3)
naive_CI <- paste("(", naive_CI_ll, ", ", naive_CI_ul, ")", sep = "")
naive_CI_OR <- paste("(", naive_CI_ll_OR, ", ", naive_CI_ul_OR, ")", sep = "")


finalmod_iptw <- glm(factor(Malaria) ~ factor(waternotprotected) + bs(data_children$`Time_since_enrollment_(days)_EUPATH_0000191`, 
                                                                      degree=3,knots=c(terti)), data=data_children, 
                     family=quasibinomial(link="logit"), weights = iptw)
iptw_ATE <- summary(finalmod_iptw)$coef[2,1]
iptw_SE <- summary(finalmod_iptw)$coef[2,2]
iptw_CI_ll <- round(iptw_ATE - 1.960*iptw_SE,3)
iptw_CI_ul <- round(iptw_ATE +1.960*iptw_SE,3)
iptw_CI_ll_OR <- round(exp(iptw_ATE - 1.960*iptw_SE), 3)
iptw_CI_ul_OR <- round(exp(iptw_ATE +1.960*iptw_SE), 3)
iptw_CI <- paste("(", iptw_CI_ll, ", ", iptw_CI_ul, ")", sep = "")
iptw_CI_OR <- paste("(", iptw_CI_ll_OR, ", ", iptw_CI_ul_OR, ")", sep = "")



finalmod_iiw <- glm(factor(Malaria) ~ factor(waternotprotected) + bs(data_children$`Time_since_enrollment_(days)_EUPATH_0000191`, 
                                                                     degree=3,knots=c(terti)), data=data_children, 
                    family=quasibinomial(link="logit"), weights = iiw)
iiw_ATE <- summary(finalmod_iiw)$coef[2,1]
iiw_SE <- summary(finalmod_iiw)$coef[2,2]
iiw_CI_ll <- round(iiw_ATE - 1.960*iiw_SE,3)
iiw_CI_ul <- round(iiw_ATE +1.960*iiw_SE,3)
iiw_CI_ll_OR <- round(exp(iiw_ATE - 1.960*iiw_SE), 3)
iiw_CI_ul_OR <- round(exp(iiw_ATE +1.960*iiw_SE), 3)
iiw_CI <- paste("(", iiw_CI_ll, ", ", iiw_CI_ul, ")", sep = "")
iiw_CI_OR <- paste("(", iiw_CI_ll_OR, ", ", iiw_CI_ul_OR, ")", sep = "")




finalmod_fiptiw <- glm(factor(Malaria) ~ factor(waternotprotected) + 
                         bs(data_children$`Time_since_enrollment_(days)_EUPATH_0000191`, 
                            degree=3,knots=c(terti)), data=data_children,
                       family=quasibinomial(link="logit"), weights = fiptiw)

fiptiw_ATE <- summary(finalmod_fiptiw)$coef[2,1]
fiptiw_SE <- summary(finalmod_fiptiw)$coef[2,2]
fiptiw_CI_ll <- round(fiptiw_ATE - 1.960*fiptiw_SE,3)
fiptiw_CI_ul <- round(fiptiw_ATE +1.960*fiptiw_SE,3)
fiptiw_CI_ll_OR <- round(exp(fiptiw_ATE - 1.960*fiptiw_SE), 3)
fiptiw_CI_ul_OR <- round(exp(fiptiw_ATE + 1.960*fiptiw_SE), 3)
fiptiw_CI <- paste("(", fiptiw_CI_ll, ", ", fiptiw_CI_ul, ")", sep = "")
fiptiw_CI_OR <- paste("(", fiptiw_CI_ll_OR, ", ", fiptiw_CI_ul_OR, ")", sep = "")




fiptiw_trimmed <- fiptiw
fiptiw_trimmed[fiptiw_trimmed > threshold] <- threshold

finalmod_fiptiw_trimmed <- glm(factor(Malaria) ~ factor(waternotprotected) + bs(data_children$`Time_since_enrollment_(days)_EUPATH_0000191`, 
                                                                                degree=3,knots=c(terti)), data=data_children, family=quasibinomial(link="logit"), 
                               weights = fiptiw_trimmed)
fiptiw_trim_ATE <- summary(finalmod_fiptiw_trimmed)$coef[2,1]
fiptiw_trim_SE <- summary(finalmod_fiptiw_trimmed)$coef[2,2]

fiptiw_trim_CI_ll <- round(fiptiw_trim_ATE - 1.960*fiptiw_trim_SE,3)
fiptiw_trim_CI_ul <- round(fiptiw_trim_ATE +1.960*fiptiw_trim_SE,3)

fiptiw_trim_CI_ll_OR <- round(exp(fiptiw_trim_ATE - 1.960*fiptiw_trim_SE), 3)
fiptiw_trim_CI_ul_OR <- round(exp(fiptiw_trim_ATE +1.960*fiptiw_trim_SE), 3)


fiptiw_trim_CI <- paste("(", fiptiw_trim_CI_ll, ", ", fiptiw_trim_CI_ul, ")", sep = "")
fiptiw_trim_CI_OR <- paste("(", fiptiw_trim_CI_ll_OR, ", ", fiptiw_trim_CI_ul_OR, ")", sep = "")





ATE <- round(c(naive_ATE, iptw_ATE, iiw_ATE, fiptiw_ATE, fiptiw_trim_ATE),3)
SE <- round(c(naive_SE, iptw_SE, iiw_SE, fiptiw_SE, fiptiw_trim_SE),3)
CI <- c(naive_CI, iptw_CI, iiw_CI, fiptiw_CI, fiptiw_trim_CI)
OR <- round(exp(ATE),3)
CI_OR <- c(naive_CI_OR, iptw_CI_OR, iiw_CI_OR, fiptiw_CI_OR, fiptiw_trim_CI_OR)

resultstab <- cbind(ATE, SE, CI, OR, CI_OR)
colnames(resultstab) <- c("$\beta_1$", "SE($\beta_1$", "95% CI for $\beta_1$", "Odds Ratio (OR)", "95% CI for OR" )
rownames(resultstab) <- c("None", "IPTW", "IIW", "FIPTIW", "FIPTIW (Trimmed)")

kable(resultstab, booktabs = T, format = "latex")





#~~~~~~~~~~~ Demographic table (baseline)

sum_tab <- function(name, vector, type){
  if(type == "continuous"){
    catmat <- (c(paste(name), paste(round(mean(vector),2), " (", round(sd(vector),2), ")", sep = "")))
    names(catmat) <- c("Variable", "Mean (SD) or n (\%)")
  } else if(type == "categorical"){
    levs <- unique(vector)
    nlevs <- length(levs)
    catmat <- data.frame()
    for(i in 1:nlevs){
      category <- levs[i]
      catmat <- rbind(catmat, c(paste(category),  paste(sum(vector == category), " (", round(sum(vector == category)/length(vector)*100,2),
                                                        "%)", sep = "")))
      names(catmat) <- c("Covariate", "Mean (SD) or Proportion")
    } 
  } else{
    print("Type not supported")
  }
  return(catmat)
}


table_demog <- rbind(sum_tab("Age at Enrollment", data_children_baseline$`Age_at_enrollment_(years)_EUPATH_0000120`, "continuous"),
                     sum_tab("Sex", data_children_baseline$Sex, "categorical"),
                     sum_tab("Sub-county", data_children_baseline$`Sub-county_in_Uganda_EUPATH_0000054`, "categorical"),
                     sum_tab("Household Wealth Index", data_children_baseline$`Household_wealth_index,_categorical_EUPATH_0000143`, "categorical"),
                     sum_tab("Drinking Water Source", data_children_baseline$Drinking_water_source_ENVO_00003064, "categorical"),
                     sum_tab("Unprotected Water Source", data_children_baseline$waternotprotected, 'categorical'),
                     sum_tab("Dwelling Type", data_children_baseline$Dwelling_type_ENVO_01000744, "categorical"),
                     sum_tab("Food Problems per Week", data_children_baseline$Food_problems_per_week_EUPATH_0000029, "categorical"),
                     sum_tab("Waste Facilities", data_children_baseline$Human_waste_facilities_EUPATH_0000335, "categorical"),
                     sum_tab("Number of Persons Living in House", data_children_baseline$Persons_living_in_house_count_EUPATH_0000019, "continuous"),
                     sum_tab("Reason for Withdrawal", data_children_baseline$Reason_for_withdrawal_EUPATH_0000208, "categorical")
                     
)


kable(table_demog, format = "latex", booktabs = T) %>%
  pack_rows("Sex", 2, 3) %>%
  pack_rows("Sub-County", 4, 6) %>%
  pack_rows("Household Wealth Index",7,9) %>%
  pack_rows("Drinking Water Source", 10, 17) %>%
  pack_rows("Unprotected Water Source", 18, 19) %>%
  pack_rows("Dwelling Type", 20, 21) %>%
  pack_rows("Food Problems per Week", 22, 26) %>%
  pack_rows("Waste Facilities", 27, 34)  %>%
  pack_rows("Reason for Withdrawal", 36, 37)
