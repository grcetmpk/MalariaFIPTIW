require(data.table)
require(dplyr)
require(tidyr)
require(splines)
require(survey)
require(knitr)
require(kableExtra)
require(geepack)
require(ggplot2)


expit <- function(x){ return(exp(x)/(1+exp(x)))}

### ~~~~~~~ Data Importation ~~~~~ ###

### import patient level data
participant_baseline_data <- fread("PRISMdata/PRISM_cohort_Participants.txt")
names(participant_baseline_data) <-  gsub(" ", "_", gsub("\\[|\\]", "", 
                                                         names(participant_baseline_data)))
### import patient repeated measures data
participant_repeated_data <- fread("PRISMdata/PRISM_cohort_Participant_repeated_measures.txt")
names(participant_repeated_data) <-  gsub(" ", "_", gsub("\\[|\\]", "",
                                                         names(participant_repeated_data)))
### import household level data
household_baseline_data <- fread("PRISMdata/PRISM_cohort_Households.txt")
names(household_baseline_data) <-  gsub(" ", "_", gsub("\\[|\\]", "", 
                                                       names(household_baseline_data)))
### import household repeated data
household_repeated_data <- fread("PRISMdata/PRISM_cohort_Household_repeated_measures.txt")
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
  # filter(`Reason_for_withdrawal_EUPATH_0000208` %in% c("Moved out of area", "Completed study" )) %>% # ind censoring
  filter(`Age_(years)_OBI_0001169` <= 11, `Age_(years)_OBI_0001169` >= 2) %>%# filter by children between 2 and 11 y/o for entire duration
  filter(`Observation_date_EUPATH_0004991` <= "2016-06-30") %>% #remove obs after june 30 2016
  filter(`Drinking_water_source_ENVO_00003064` %in% c("Public tap", "Protected public well" , "River/stream",
                                                      "Protected spring", "Borehole",  "Open public well",        
                                                      "Pond/lake" , "Unprotected spring"  )) #remove piped water sources

# ggplot(data_children, aes(x = `Time_since_enrollment_(days)_EUPATH_0000191`, y = Participant_Id)) + geom_point(size = 0.5)



# combine two human waste categories together

data_children$Human_waste_facilities_EUPATH_0000335[data_children$Human_waste_facilities_EUPATH_0000335 == 
                                                      "[\"Uses neighbours\",\"No facility/bush/field\"]"] <- "[\"No facility/bush/field\"]"


data_children$Antimalarial_medication_EUPATH_0000058 <- relevel(as.factor(data_children$Antimalarial_medication_EUPATH_0000058),
                                                                ref = "No malaria medications given")

### randomly sample one child per household

length(unique(data_children$Household_Id.x)) # we have 133 unique households

set.seed(100)

participant_list <- c() #empty list to store participants (one per fam)
hhlist <- tapply(data_children$Participant_Id, data_children$Household_Id.x, unique) #list of ids per family
for(i in 1:length(hhlist)){
  fam <- hhlist[[i]]
  if(length(fam) > 1){
    participant <- sample(fam, 1) # randomly sample one child from the family
  }else{
    participant <- fam 
  }
  
  participant_list <- c(participant_list, participant) #add it to the list
}

data_children <- data_children %>% 
  filter(Participant_Id %in% participant_list)

length(participant_list)
dim(data_children) #8681 x 25

###### ~~~~~~~~~~~~~~administrative censoring.


#### figure out last observation times for each individual
data_children_lastobs <- do.call("rbind", 
                                 by(data_children, INDICES=data_children$Participant_Id, 
                                    FUN=function(data_children) data_children[which.max(data_children$`Time_since_enrollment_(days)_EUPATH_0000191`), ]))[,c(2,10, 15)]

summary(data_children_lastobs[,2])
quantile(unlist(data_children_lastobs[,2]), 0.05) #95% of individuals censoring time is after 191 days (basically 6 months)

#figure out how many people had censoring times prior to 6 months
propcensoredpriorto6months <- length(which(data_children_lastobs[,2] < 365/2))/dim(data_children_lastobs)[1] #0.0488
propcensoredpriorto6months

data_children_censored <- data_children_lastobs[which(data_children_lastobs[,2] < 365/2),] 

prop_noninformative <- length(which(data_children_censored$Reason_for_withdrawal_EUPATH_0000208 %in% 
               c("Completed study", "Moved out of area")))/dim(data_children_censored)[1] 
prop_noninformative
# of who were censored prior to 6 months, 71.43% of individuals were censored for reasons 
# unrelated to the longitudinal outcome

# this leaves 1.39% of individuals having informative censoring. Negligible. 
(1-prop_noninformative)*propcensoredpriorto6months




###in the data, flag those censored prior to 6 months
#figure out IDs of those censored prior to 6 months
ids_censoredpriorto6months <- unlist(data_children_lastobs[which(data_children_lastobs$`Time_since_enrollment_(days)_EUPATH_0000191` < 365/2), 1])



#artificially censor at one year to reduce the amount of censoring present in the data

data_children <- data_children %>%
  filter(`Time_since_enrollment_(days)_EUPATH_0000191` <= 365/2)

ggplot(data_children, aes(x = `Time_since_enrollment_(days)_EUPATH_0000191`, y = Participant_Id)) + geom_point(size = 0.5)


#for those censored prior to 6 months, give a new censoring time from runif(0, 365/2)

rmid_toremove <- c()

for(id in ids_censoredpriorto6months){
  #get list of observations to remove
  
  censortime <- runif(1, 0, 365/2)
  
  tempdata <- data_children[which(data_children$Participant_Id == id), ]
  toremove <- unlist(tempdata[which(tempdata$`Time_since_enrollment_(days)_EUPATH_0000191` > censortime), 1])
  
  if(length(toremove) > 0){
    rmid_toremove <- c(rmid_toremove, toremove)
  }

}


##remove these newly censored events

data_children <- data_children %>%
  filter(!Participant_repeated_measure_Id %in% rmid_toremove)



### calculate summary of number of events per individual
data_children$Observed <- 1
data_children$obsnumber <- with(data_children, ave(Participant_Id, Participant_Id, FUN = seq_along))


data_children <- data_children %>% select(-c(`Weight_(kg)_IAO_0000414`,`Height_(cm)_EUPATH_0010075` ))



numevents <- summary(tapply(data_children$Observed, data_children$Participant_Id, sum)) 
numevents #min 1 mean 30.9 max 104.0
dim(data_children) #1386 x 25
length(unique(data_children$Participant_Id)) #287 
table(data_children$Observation_type_BFO_0000015) # 287 enrollment, 525 sched, 574 unsched




### find out how many obs and subjects have malaria diagnosis

data_children_malaria <- data_children %>% filter(Malaria_diagnosis_EUPATH_0000090 == "Yes")
dim(data_children_malaria) # 179/1373 (13.03%) of observations have malaria diagnosis at some point
length(unique(data_children_malaria$Participant_Id)) # 111/287 (~38.68%) of patients had malaria at some point



### Create indicator for Malaria diagnosis (1 if diagnosed)
data_children$Malaria <- ifelse(data_children$Malaria_diagnosis_EUPATH_0000090 == "Yes", 1, 0)

### Create indicator for whether treatment administered on previous visit
data_children$obsnumber <- with(data_children, ave(Participant_Id, Participant_Id, FUN = seq_along))

data_children$scheduled <- ifelse(data_children$Observation_type_BFO_0000015 == "Unscheduled visit", "No", "Yes")



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

#### no observations had antimalarials within one week of observation. 
sum(trtwithinoneweek_full)


# create a variable for protected water sources
data_children$waternotprotected <- ifelse(data_children$Drinking_water_source_ENVO_00003064 %in% c("River/stream",   
                                                                                                   "Open public well", "Pond/lake", "Unprotected spring"), 1, 0)


data_children_baseline <- data_children[data_children$Observation_type_BFO_0000015 == "Enrollment", ]


table(data_children_baseline$waternotprotected) #69/273 unprotected water 
#no missingness :D


# add covariate for time since last visit, current age, and disease status at last visit 
# as this is time-varying, need to make sure we impute all of the data at "missing" times (days) ( 0.1 to 1775)

potentialtimes <- c(seq(0, 365, by = 1)) #max observation time one year

fullpotentialdata <- data.frame(matrix(NA, ncol = 28))
colnames(fullpotentialdata) <- colnames(data_children)
fullpotentialdata <- fullpotentialdata[, c(2,5,8,9,11,13,14,16,17,18,19,21,22,23,24,26,27,28)]
fullpotentialdata$timesincelastvisit <- NA
fullpotentialdata$currentage <- NA
fullpotentialdata$lastmalariastatus <- NA

for(id in unique(data_children$Participant_Id)){
  observeddata_id <- data_children[which(data_children$Participant_Id == id), c(2,5,8,9,11,13,14,16,17,18,19,21,22,23,24,26,27,28)]
  observedtimes_id <- observeddata_id$`Time_since_enrollment_(days)_EUPATH_0000191`
  
  emptydata_id <- data.frame(matrix(NA, ncol = dim(observeddata_id)[2] + 3))
  colnames(emptydata_id) <- c(colnames(observeddata_id), "timesincelastvisit", "currentage", "lastmalariastatus")
  
  ageatenroll_id <- observeddata_id[1,2]
  
  lastvisittime_id <- 0
  lastmalariastatus_id <- observeddata_id[1, 16]
  
  for(currenttime in potentialtimes){
    
    timesincelastvisit <- currenttime - lastvisittime_id
    currentage <- ageatenroll_id + currenttime/365
    
    #if we have data for this time, use it 
    if(currenttime %in% observedtimes_id){
      
      thisobs <- data.frame(c(observeddata_id[which(observeddata_id$`Time_since_enrollment_(days)_EUPATH_0000191` == currenttime), ], 
                              timesincelastvisit, currentage, lastmalariastatus_id))
      colnames(thisobs) <- c(colnames(observeddata_id), "timesincelastvisit", "currentage", "lastmalariastatus")
      
      emptydata_id <- rbind(emptydata_id, thisobs)
      
      lastvisittime_id <- currenttime #update last obs time
      lastmalariastatus <- thisobs[1,16] #update last known malaria diagnosis
      
    } else {
      
      # if we don't have data for this time, impute it using the time-invariant data
      
      thisobs <- data.frame(c(observeddata_id[which(observeddata_id$`Time_since_enrollment_(days)_EUPATH_0000191` == 0), ], 
                              timesincelastvisit, currentage, lastmalariastatus))
      colnames(thisobs) <- c(colnames(observeddata_id), "timesincelastvisit", "currentage", "lastmalariastatus")
      
      thisobs[1, 4] <- currenttime #change time to current time
      thisobs[1,15] <- 0 # change observed to 0
      
      emptydata_id <- rbind(emptydata_id, thisobs)
      
    }
    
    
  }
  
  emptydata_id <- emptydata_id[-1, ]
  
  #add it to the larger dataset
  fullpotentialdata <- rbind(fullpotentialdata, emptydata_id)
  
}

fullpotentialdata <- fullpotentialdata[-1, ]


### create lags (set t = 0 to 0.1)
fullpotentialdata$`Time_since_enrollment_(days)_EUPATH_0000191` <- ifelse(fullpotentialdata$`Time_since_enrollment_(days)_EUPATH_0000191` == 0, 0.1, 
                                                                          fullpotentialdata$`Time_since_enrollment_(days)_EUPATH_0000191`)
fullpotentialdata$Time_lag <- fullpotentialdata$`Time_since_enrollment_(days)_EUPATH_0000191`[c(nrow(fullpotentialdata),1:(nrow(fullpotentialdata)-1))]
fullpotentialdata$Time_lag[fullpotentialdata$`Time_since_enrollment_(days)_EUPATH_0000191` == 0.1] <- 0


#create dummy vars needed for intensity model
fullpotentialdata$schedulednum <- ifelse(fullpotentialdata$scheduled == "Yes", 1, 0)
fullpotentialdata$Male <- ifelse(fullpotentialdata$Sex_PATO_0000047 == "Male", 1, 0)
fullpotentialdata$HWI_middle <- ifelse(fullpotentialdata$`Household_wealth_index,_categorical_EUPATH_0000143` == "Middle", 1, 0)
fullpotentialdata$HWI_poorest <- ifelse(fullpotentialdata$`Household_wealth_index,_categorical_EUPATH_0000143` == "Poorest", 1, 0)
fullpotentialdata$SC_Nagongera <- ifelse(fullpotentialdata$`Sub-county_in_Uganda_EUPATH_0000054` == "Nagongera", 1, 0)
fullpotentialdata$SC_Walukuba   <- ifelse(fullpotentialdata$`Sub-county_in_Uganda_EUPATH_0000054` == "Walukuba", 1, 0)
fullpotentialdata$FPW_Never <- ifelse(fullpotentialdata$Food_problems_per_week_EUPATH_0000029 == "Never", 1, 0)
fullpotentialdata$FPW_Often <- ifelse(fullpotentialdata$Food_problems_per_week_EUPATH_0000029 == "Often", 1, 0)
fullpotentialdata$FPW_Seldom <- ifelse(fullpotentialdata$Food_problems_per_week_EUPATH_0000029 == "Seldom", 1, 0)
fullpotentialdata$FPW_Sometimes <- ifelse(fullpotentialdata$Food_problems_per_week_EUPATH_0000029 == "Sometimes", 1, 0)
fullpotentialdata$HWF_CPLNS <- ifelse(fullpotentialdata$Human_waste_facilities_EUPATH_0000335 == "[\"Covered pit latrine no slab\"]", 1, 0)
fullpotentialdata$HWF_CPLWS <- ifelse(fullpotentialdata$Human_waste_facilities_EUPATH_0000335 == "[\"Covered pit latrine w/slab\"]", 1, 0)
fullpotentialdata$HWF_FT <- ifelse(fullpotentialdata$Human_waste_facilities_EUPATH_0000335 == "[\"Flush toilet\"]" , 1, 0)
fullpotentialdata$HWF_NF <- ifelse(fullpotentialdata$Human_waste_facilities_EUPATH_0000335 == "[\"No facility/bush/field\"]" , 1, 0)
fullpotentialdata$HWF_UPLNS <- ifelse(fullpotentialdata$Human_waste_facilities_EUPATH_0000335 %in% c("[\"Uncovered pit latrine no slab\"]", "[\"Uses neighbours\",\"No facility/bush/field\"]"), 1, 0)
fullpotentialdata$HWF_UPLWS <- ifelse(fullpotentialdata$Human_waste_facilities_EUPATH_0000335 == "[\"Uncovered pit latrine w/slab\"]" , 1, 0)
fullpotentialdata$HWF_VIP <- ifelse(fullpotentialdata$Human_waste_facilities_EUPATH_0000335 == "[\"Vip latrine\"]", 1, 0)
fullpotentialdata$Dwellingtypenum <- ifelse(fullpotentialdata$Dwelling_type_ENVO_01000744 == "Traditional", 1, 0)




#createdataframe of only observed data
observed_potentialdata <- fullpotentialdata[which(fullpotentialdata$Observed == 1), ] #same as data_children but contains some time-varying covariates that 
#were calculated when making the counterfactual data


#~~~~~~~~~~~~~~~~~~~~~~~~~~~Estimate weights ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~ IPTW weights
# IPTW model should include true confounders and covariates only predictive of outcome. Do not include those only predictive of exposure

#use only observed data
#model considering all possible covariates predictive of exposure
exposuremod <- glm(factor(waternotprotected) ~ `Age_(years)_OBI_0001169` + #`Sub-county_in_Uganda_EUPATH_0000054` +
                     `Household_wealth_index,_categorical_EUPATH_0000143` + `Food_problems_per_week_EUPATH_0000029` +
                     #Human_waste_facilities_EUPATH_0000335 + 
                     Dwelling_type_ENVO_01000744 + Persons_living_in_house_count_EUPATH_0000019,
                   family = binomial(link = "logit"), data = observed_potentialdata)


summary(exposuremod)

ps <- expit(predict(exposuremod))
iptw <- 1/ps*as.numeric(observed_potentialdata$waternotprotected)+1/(1-ps)*(1-as.numeric(observed_potentialdata$waternotprotected))
summary(iptw) #max 10.524
sum(iptw > 5)/length(iptw) #5.69%
sum(iptw > 10)/length(iptw) #0.29%
sum(iptw > 20)/length(iptw) #0.00%

hist(iptw)

#~~~~~~~~~~ IIW Weights
# IIW weights less sensitive to inclusion of non-confounders.


# weight stabilizer, use full counterfactual data to est parameters
delta.hat <- coxph(Surv(Time_lag, `Time_since_enrollment_(days)_EUPATH_0000191`, Observed) ~ factor(waternotprotected) - 1, data = fullpotentialdata)$coef

# model with aux variables, use full data to est parameters
intensitymod <- coxph(Surv(Time_lag, `Time_since_enrollment_(days)_EUPATH_0000191`, Observed) ~ factor(waternotprotected) + currentage +
                        `Sub-county_in_Uganda_EUPATH_0000054` + `Household_wealth_index,_categorical_EUPATH_0000143` +
                        `Food_problems_per_week_EUPATH_0000029` + #Human_waste_facilities_EUPATH_0000335 +
                        Dwelling_type_ENVO_01000744 + Persons_living_in_house_count_EUPATH_0000019 +
                        lastmalariastatus - 1, 
                      data = fullpotentialdata)

gamma.hat <- intensitymod$coef



Z <- cbind(observed_potentialdata$waternotprotected, observed_potentialdata$currentage,
           observed_potentialdata$SC_Nagongera, observed_potentialdata$SC_Walukuba,
           observed_potentialdata$HWI_middle, observed_potentialdata$HWI_poorest,
           observed_potentialdata$FPW_Never, observed_potentialdata$FPW_Often,
           observed_potentialdata$FPW_Seldom, observed_potentialdata$FPW_Sometimes,
           # observed_potentialdata$HWF_CPLNS, observed_potentialdata$HWF_CPLWS,
           # observed_potentialdata$HWF_FT, observed_potentialdata$HWF_NF,
           # observed_potentialdata$HWF_UPLNS, observed_potentialdata$HWF_CPLWS,
           # observed_potentialdata$HWF_VIP,
           observed_potentialdata$Dwellingtypenum,
           observed_potentialdata$Persons_living_in_house_count_EUPATH_000001,
           observed_potentialdata$lastmalariastatus)

iiw <- exp(cbind(observed_potentialdata$waternotprotected)%*%delta.hat)/exp(Z%*%gamma.hat)

summary(iiw) #max 3.66
sum(iiw>5)/length(iiw) #0.0%
sum(iiw>10)/length(iiw) #0.0%
sum(iiw > 20)/length(iiw) #0.0\%




fiptiw <- iiw*iptw
observed_potentialdata$fiptiw <- fiptiw

hist(fiptiw)
summary(fiptiw) #max 13.19
sum(fiptiw > 5)/length(fiptiw) #8.44%
sum(fiptiw > 10)/length(fiptiw) #0.51%
sum(fiptiw > 20)/length(fiptiw) #0.00%



threshold <- quantile(fiptiw, 0.95) 
threshold #6.52

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Results ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# estimate smooth function of time mu(t) using splines for the marginal model
# as in Coloumbe, 2021

terti<-quantile(observed_potentialdata$`Time_since_enrollment_(days)_EUPATH_0000191` , c(0.3333, 0.66666), type = 1) 
finalmod_unadj <- glm(factor(Malaria) ~ factor(waternotprotected) + bs(observed_potentialdata$`Time_since_enrollment_(days)_EUPATH_0000191`, 
                                                                       degree=3,knots=c(terti)), data = observed_potentialdata, family=quasibinomial(link="logit"))
naive_ATE <- summary(finalmod_unadj)$coef[2,1]
naive_SE <- summary(finalmod_unadj)$coef[2,2]
naive_CI_ll <- round(naive_ATE -1.96*naive_SE,3)
naive_CI_ul <- round(naive_ATE +1.96*naive_SE,3)
naive_CI_ll_OR <- round(exp(naive_ATE -1.96*naive_SE),3)
naive_CI_ul_OR <- round(exp(naive_ATE +1.96*naive_SE),3)
naive_CI <- paste("(", naive_CI_ll, ", ", naive_CI_ul, ")", sep = "")
naive_CI_OR <- paste("(", naive_CI_ll_OR, ", ", naive_CI_ul_OR, ")", sep = "")


finalmod_iptw <- glm(factor(Malaria) ~ factor(waternotprotected) + bs(observed_potentialdata$`Time_since_enrollment_(days)_EUPATH_0000191`, 
                                                                      degree=3,knots=c(terti)), data=observed_potentialdata, 
                     family=quasibinomial(link="logit"), weights = iptw)
iptw_ATE <- summary(finalmod_iptw)$coef[2,1]
iptw_SE <- summary(finalmod_iptw)$coef[2,2]
iptw_CI_ll <- round(iptw_ATE - 1.960*iptw_SE,3)
iptw_CI_ul <- round(iptw_ATE +1.960*iptw_SE,3)
iptw_CI_ll_OR <- round(exp(iptw_ATE - 1.960*iptw_SE), 3)
iptw_CI_ul_OR <- round(exp(iptw_ATE +1.960*iptw_SE), 3)
iptw_CI <- paste("(", iptw_CI_ll, ", ", iptw_CI_ul, ")", sep = "")
iptw_CI_OR <- paste("(", iptw_CI_ll_OR, ", ", iptw_CI_ul_OR, ")", sep = "")



finalmod_iiw <- glm(factor(Malaria) ~ factor(waternotprotected) + bs(observed_potentialdata$`Time_since_enrollment_(days)_EUPATH_0000191`, 
                                                                     degree=3,knots=c(terti)), data=observed_potentialdata, 
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
                         bs(observed_potentialdata$`Time_since_enrollment_(days)_EUPATH_0000191`, 
                            degree=3,knots=c(terti)), data=observed_potentialdata,
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

finalmod_fiptiw_trimmed <- glm(factor(Malaria) ~ factor(waternotprotected) + bs(observed_potentialdata$`Time_since_enrollment_(days)_EUPATH_0000191`, 
                                                                                degree=3,knots=c(terti)), data=observed_potentialdata, family=quasibinomial(link="logit"), 
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
    names(catmat) <- c("Variable", "Mean (SD) or Proportion")
  } else if(type == "categorical"){
    levs <- unique(vector)
    nlevs <- length(levs)
    catmat <- data.frame()
    for(i in 1:nlevs){
      category <- levs[i]
      catmat <- rbind(catmat, c(paste(category),  paste(sum(vector == category), " (", round(sum(vector == category)/length(vector)*100,2),
                                                        "%)", sep = "")))
      names(catmat) <- c("Covariate", "Mean (SD) or n (%)")
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
                     sum_tab("Number of Persons Living in House", data_children_baseline$Persons_living_in_house_count_EUPATH_0000019, "continuous")
                     # sum_tab("Reason for Withdrawal", data_children_baseline$Reason_for_withdrawal_EUPATH_0000208, "categorical")
                     
)


kable(table_demog, format = "latex", booktabs = T) %>%
  pack_rows("Sex", 2, 3) %>%
  pack_rows("Sub-County", 4, 6) %>%
  pack_rows("Household Wealth Index",7,9) %>%
  pack_rows("Drinking Water Source", 10, 17) %>%
  pack_rows("Unprotected Water Source", 18, 19) %>%
  pack_rows("Dwelling Type", 20, 21) %>%
  pack_rows("Food Problems per Week", 22, 26) %>%
  pack_rows("Waste Facilities", 27, 34)

## graph observation times for individuals in the study

ggplot(data_children, aes(x = `Time_since_enrollment_(days)_EUPATH_0000191`, y = Participant_Id, group = Participant_Id, colour = scheduled)) +
  geom_point()

