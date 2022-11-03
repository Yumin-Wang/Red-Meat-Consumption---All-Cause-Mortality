#install specification curve packages
#install.packages("devtools")
#devtools::install_github("masurp/specr")
#load the data
source("DATA/Combined DATA/DATA_formatting.r")
#load required packages
library(survival)
library(tidyverse)
library(specr)
#set number of allowed warnings to be big
options(nwarnings = 10000)  
#set seed to make results reproducible
set.seed(98431)

###########################################################################################################################
#Function: adjusting_variable_generator

#Author: Yumin Wang

#Creation Date:  10/5/2022 (version 4.1.2)

#Purpose: This function takes each of the 47 optional adjusting variable names with Bernoulli(0.5) to form a collective sample of variable names.
#         User can specify number of collective samples to be returned (parameter:n). The returned n collective samples will be randomly sampled from 2^47 available collective samples with replacement.
#         For alcohol consumption and BMI, the function will not make continuous and categorical type of the variable appear in a collective sample at the same time.
#         The function will return a list of two elements: 
#         (1)n collective samples, variables in each collective sample will be linked via "+" sign. This expression will be used in cox regression for adjusting variables.
#         (2)n collective samples corresponds to (1), variables in each collective sample is denoted as index and linked via "," sign. This expression will be used to make plots.

# Required Parameters: 
#         n: number of collective samples to be returned

#Output:   The function will return a list of two elements: 
#         (1)n collective samples, variables in each collective sample will be linked via "+" sign. This expression will be used in cox regression for adjusting variables.
#         (2)n collective samples corresponds to (1), variables in each collective sample is denoted as index and linked via "," sign. This expression will be used to make plots.

#Example: adjusting_variable_generator(n=2) returns a list with two elements with 2 collective samples:
# [[1]]
# [1] "RACE_ETHNICITY+EDUCATION+ALCOHOL_GROUP+ACTIVITY+SLEEP+SOCIOECONOMIC_STATUS+BMI_CONTINOUS+SYSTOLIC_BLOOD_PRESSURE+FAMILY_HISTORY_OF_DIABETES+FAMILY_HISTORY_OF_MYOCARDIAL_INFRACTION+IBUPROFEN+ON_SPECIAL_DIET+DIETARY_SUPPLEMENT+PROCESSED_MEAT+POULTRY+FRUITS+WHOLE_GRAIN+EGGS+TOTAL_DAIRY+CARBOHYDRATES+MONOUNSATURATED_FATTY_ACID+CHOLESTEROL+MAGNESIUM"                                                
# [2] "EDUCATION+MARTIAL_STATUS+ALCOHOL_CONTINOUS+ACTIVITY+SLEEP+FAMILY_INCOME+BMI_CONTINOUS+SYSTOLIC_BLOOD_PRESSURE+GENERAL_HEALTH_CONDITION+HISTORY_OF_HYPERTENSION+HISTORY_OF_DIABETES+HISTORY_OF_DEPRESSION+HISTORY_OF_CANCER_OR_MALIGNANCY+FAMILY_HISTORY_OF_DIABETES+FAMILY_HISTORY_OF_MYOCARDIAL_INFRACTION+ON_SPECIAL_DIET+DIETARY_SUPPLEMENT+PROCESSED_MEAT+POULTRY+FRUITS+LEGUMES+CHOLESTEROL+MAGNESIUM"
# 
# [[2]]
# [1] "1,2,5,7,9,11,12,14,22,23,25,29,30,31,32,33,36,37,40,41,44,46,47"
# [2] "2,3,4,7,9,10,12,14,15,17,18,19,21,22,23,29,30,31,32,33,39,46,47"


###########################################################################################################################
adjusting_variable_generator<-function(n=10){
  #get all variable names in analytical data
  Variable_names<-names(DATA)
  #get all optional adjusting variable names in data
  Variable_names<-Variable_names[!Variable_names %in% c("SEQN","MORTSTAT","COHORT_YEAR","PERMTH_INT","AGE_CONTINIOUS","AGE_DEATH_CENSORED","AGE_GROUP","GENDER","SMOKING","MENOPAUSAL_STATUS","HORMONE_THERAPY_USE","PARITY","ORAL_CONTRACEPTIVE_USE","TOTAL_ENERGY","UNPROCESSED_RED_MEAT_STANDARD_CONTINOUS","UNPROCESSED_RED_MEAT_STANDARD_QUARTILE","UNPROCESSED_RED_MEAT_STANDARD_QUINTILES","UNPROCESSED_RED_MEAT_DENSITY_CONTINOUS","UNPROCESSED_RED_MEAT_DENSITY_QUARTILE","UNPROCESSED_RED_MEAT_DENSITY_QUINTILES","UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_2nd","UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_3rd","UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_4th",
                                                        "UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_2nd","UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_3rd","UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_4th","UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_5th",
                                                        "UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_2nd","UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_3rd","UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_4th",
                                                        "UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_2nd","UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_3rd","UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_4th","UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_5th")]
  #initiate two vectors of length n to store each collective sample in corresponding format
  variable_combination = vector(length = n)
  variable_index = vector(length=n)
  #iterate to get n collective samples
  for (i in 1:n){
    #for each variable, drop a fair coin to denote include or not. 1 include, 0 not include.
    include<-rbinom(n=length(Variable_names), size=1, prob=0.5)
    #not include alcohol continuous or categorical at same time
    if(include[4]*include[5]==1){
      #if alcohol continuous or categorical included at the same time, drop a coin, land on 1, then remove continuous version, land on 0, then remove categorical version.
      if(rbinom(n=1,size=1,prob=0.5)==1){include[4]<-0}
      else{include[5]<-0}
    }
    #not include bmi continuous or categorical at same time
    if(include[12]*include[13]==1){
      #if bmi continuous or categorical included at the same time, drop a coin, land on 1, then remove continuous version, land on 0, then remove categorical version.
      if(rbinom(n=1,size=1,prob=0.5)==1){include[12]<-0}
      else{include[13]<-0}
    }
    #get variables included in one collective sample
    sampled_variables<-Variable_names[include==1]
    #link these variable names by "+" sign
    variable_combination[i]<-paste(sampled_variables,collapse="+")
    #link these variable index by "," sign 
    variable_index[i]<-paste(which(include==1),collapse=",")
    
  }
  #return n collective samples in two format
  return (list(variable_combination,variable_index))
}
#generate 20 collective samples
adjusting_variables<-adjusting_variable_generator(n=20)



###############################################################################################
#standard model plus continuous meat
#define standard model's cox model.: For all model:  age(continuous), sex, smoking, total energy are adjusted. For female model, menopausal status, hormone therapy, parity, oral contraceptive use are additionally adjusted.
cox_no_interaction_standard_continous<-function(formula,data){
  if(sum(data$GENDER=="Female")==nrow(data)){
    coxph(formula=as.formula(paste(formula,"+AGE_CONTINIOUS+SMOKING+TOTAL_ENERGY+COHORT_YEAR+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data)}
  else if(sum(data$GENDER=="Male")==nrow(data)){
    coxph(formula=as.formula(paste(formula,"+AGE_CONTINIOUS+SMOKING+TOTAL_ENERGY+COHORT_YEAR")),data=data)}
  else{
    coxph(formula=as.formula(paste(formula,"+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+COHORT_YEAR")),data=data)
  }
}
# cox_age_interaction_standard_continous<-function(formula,data){
#   if(sum(data$GENDER=="Female")==nrow(data)){
#     coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_STANDARD_CONTINOUS:AGE_CONTINIOUS+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data)}
#   else{
#     coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_STANDARD_CONTINOUS:AGE_CONTINIOUS+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY")),data=data)}
# }
# cox_sex_interaction_standard_continous<-function(formula,data){
#   if(sum(data$GENDER=="Female")==nrow(data)){
#     coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_STANDARD_CONTINOUS:GENDER+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data)}
#   else{
#     coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_STANDARD_CONTINOUS:GENDER+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY")),data=data)}
# }
# cox_bmi_interaction_standard_continous<-function(formula,data){
#   if(sum(data$GENDER=="Female")==nrow(data)){
#     coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_STANDARD_CONTINOUS:BMI_CONTINOUS+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data)}
#   else{
#     coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_STANDARD_CONTINOUS:BMI_CONTINOUS+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY")),data=data)}
# }

#Run specification curve analysis for standard continuous model, subgroup for age_group and gender
results_standard_continous <- run_specs(df = DATA,
                      y = c("Surv(PERMTH_INT,MORTSTAT)"),
                      x = c("UNPROCESSED_RED_MEAT_STANDARD_CONTINOUS"), 
                      model = c("cox_no_interaction_standard_continous"),
                      controls = adjusting_variables[1],
                      subsets = list(GENDER = unique(DATA$GENDER),
                                     AGE_GROUP = unique(DATA$AGE_GROUP)))

#drop no adjusting variables results
results_standard_continous<-results_standard_continous[results_standard_continous$controls!="no covariates",]
#covert names into index in control column for plotting purposes
for(i in 1:nrow(results_standard_continous)){
  results_standard_continous$controls[i]<-adjusting_variables[[2]][which(adjusting_variables[[1]]==results_standard_continous$controls[i])]
}
#get HR, CI for 100 gram increase for continuous meat standard model
results_standard_continous$estimate<-exp(results_standard_continous$estimate*100)
results_standard_continous$conf.low<-exp(results_standard_continous$conf.low*100)
results_standard_continous$conf.high<-exp(results_standard_continous$conf.high*100)
#drop CI not in the range of (0.2,5) because they are not plausible CI
results_standard_continous<-results_standard_continous[results_standard_continous$conf.low>=0.2&results_standard_continous$conf.high<=5,]
#create gender and age group columns for plotting purposes
results_standard_continous<-results_standard_continous %>% mutate(GENDER = case_when (grepl("GENDER = Female",subsets)~"Female",
                               grepl("GENDER = Male",subsets)~"Male",!grepl("GENDER = Female",subsets)&!grepl("GENDER = Male",subsets)~"All Sex"))
results_standard_continous<-results_standard_continous %>% mutate(AGE_GROUP = case_when (grepl("AGE_GROUP = 20-39 years old",subsets)~"20-39 Years Old",
                                                   grepl("AGE_GROUP = 40-59 years old",subsets)~"40-59 Years Old",
                                                   grepl("AGE_GROUP = 60-79 years old",subsets)~"60-79 Years Old",
                                                   !grepl("AGE_GROUP = 20-39 years old",subsets)&!grepl("AGE_GROUP = 40-59 years old",subsets)&
                                                     !grepl("AGE_GROUP = 60-79 years old",subsets)~"All Age"))                             

# results_standard_continous <- results_standard_continous %>% mutate(Interaction = case_when(model=="cox_no_interaction_standard_continous"~"No interaction included",
#                                                       model=="cox_age_interaction_standard_continous"~"Include interaction only with Age",
#                                                       model=="cox_bmi_interaction_standard_continous"~"Include interaction only with BMI",
#                                                       model=="cox_sex_interaction_standard_continous"&GENDER!="All sex"~"No interaction included",
#                                                       model=="cox_sex_interaction_standard_continous"&GENDER=="All sex"~"Include interaction only with Sex"))


#create analytical_model column for plotting purposes
results_standard_continous <- results_standard_continous %>% mutate(Analytical_model = "Standard Model")

# p1<-plot_curve(results_standard_continous,ci=TRUE,null=1) +
#   geom_hline(yintercept = 1, linetype = "dashed", color = "grey") +
#   ylim(0, 3.5) +
#   labs(x = "", y = "Hazard Ratio") + theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# 
# p2<-plot_choices(results_standard_continous,choices = c("x","Analytical_model","Interaction","controls","GENDER","AGE_GROUP"),null=1)+
#   labs(x = "Specifications")+ theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#                                                  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
# 
# #results<-results %>% mutate(DMDMARTL = ifelse(grepl("DMDMARTL",controls),"DMDMARTL","No"))
# 
# plot_specs(plot_a=p1,plot_b=p2)



###############################################################################
#Density model continuous meat
#same as above except for density model continuous meat
cox_no_interaction_density_continous<-function(formula,data){
  if(sum(data$GENDER=="Female")==nrow(data)){
    coxph(formula=as.formula(paste(formula,"+AGE_CONTINIOUS+SMOKING+TOTAL_ENERGY+COHORT_YEAR+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data)}
  else if (sum(data$GENDER=="Male")==nrow(data)){
    coxph(formula=as.formula(paste(formula,"+AGE_CONTINIOUS+SMOKING+TOTAL_ENERGY+COHORT_YEAR")),data=data)}
  else{
    coxph(formula=as.formula(paste(formula,"+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+COHORT_YEAR")),data=data)}

}
# cox_age_interaction_density_continous<-function(formula,data){
#   if(sum(data$GENDER=="Female")==nrow(data)){
#     coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_DENSITY_CONTINOUS:AGE_CONTINIOUS+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data)}
#   else{
#     coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_DENSITY_CONTINOUS:AGE_CONTINIOUS+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY")),data=data)}
# }
# cox_sex_interaction_density_continous<-function(formula,data){
#   if(sum(data$GENDER=="Female")==nrow(data)){
#     coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_DENSITY_CONTINOUS:GENDER+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data)}
#   else{
#     coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_DENSITY_CONTINOUS:GENDER+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY")),data=data)}
# }
# cox_bmi_interaction_density_continous<-function(formula,data){
#   if(sum(data$GENDER=="Female")==nrow(data)){
#     coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_DENSITY_CONTINOUS:BMI_CONTINOUS+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data)}
#   else{
#     coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_DENSITY_CONTINOUS:BMI_CONTINOUS+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY")),data=data)}
# }

results_density_continous <- run_specs(df = DATA,
                                       y = c("Surv(PERMTH_INT,MORTSTAT)"),
                                       x = c("UNPROCESSED_RED_MEAT_DENSITY_CONTINOUS"), 
                                       model = c("cox_no_interaction_density_continous"),
                                       controls = adjusting_variables[1],
                                       subsets = list(GENDER = unique(DATA$GENDER),
                                                      AGE_GROUP = unique(DATA$AGE_GROUP)))
results_density_continous<-results_density_continous[results_density_continous$controls!="no covariates",]
for(i in 1:nrow(results_density_continous)){
  results_density_continous$controls[i]<-adjusting_variables[[2]][which(adjusting_variables[[1]]==results_density_continous$controls[i])]
}

#for density model, calculate HR and CI for 100gram/2000kcal increase
results_density_continous$estimate<-exp(results_density_continous$estimate*100/2000)
results_density_continous$conf.low<-exp(results_density_continous$conf.low*100/2000)
results_density_continous$conf.high<-exp(results_density_continous$conf.high*100/2000)
#drop results not plausible CI not in (0.2,5)
results_density_continous<-results_density_continous[results_density_continous$conf.low>=0.2&results_density_continous$conf.high<=5,]

results_density_continous<-results_density_continous %>% mutate(GENDER = case_when (grepl("GENDER = Female",subsets)~"Female",
                                                                                    grepl("GENDER = Male",subsets)~"Male",!grepl("GENDER = Female",subsets)&!grepl("GENDER = Male",subsets)~"All Sex"))
results_density_continous<-results_density_continous %>% mutate(AGE_GROUP = case_when (grepl("AGE_GROUP = 20-39 years old",subsets)~"20-39 Years Old",
                                                                                       grepl("AGE_GROUP = 40-59 years old",subsets)~"40-59 Years Old",
                                                                                       grepl("AGE_GROUP = 60-79 years old",subsets)~"60-79 Years Old",
                                                                                       !grepl("AGE_GROUP = 20-39 years old",subsets)&!grepl("AGE_GROUP = 40-59 years old",subsets)&
                                                                                         !grepl("AGE_GROUP = 60-79 years old",subsets)~"All Age"))                               
# results_density_continous <- results_density_continous %>% mutate(Interaction = case_when(model=="cox_no_interaction_density_continous"~"No interaction included",
#                                                                                           model=="cox_age_interaction_density_continous"~"Include interaction only with Age",
#                                                                                           model=="cox_bmi_interaction_density_continous"~"Include interaction only with BMI",
#                                                                                           model=="cox_sex_interaction_density_continous"&GENDER!="All sex"~"No interaction included",
#                                                                                           model=="cox_sex_interaction_density_continous"&GENDER=="All sex"~"Include interaction only with Sex"))


results_density_continous <- results_density_continous %>% mutate(Analytical_model = "Multivariable Nutrition Density Model")


# p1<-plot_curve(results_density_continous,ci=FALSE) +
#   geom_hline(yintercept = 1, linetype = "dashed", color = "grey") +
#   ylim(0, 2) +
#   labs(x = "", y = "Hazard Ratio")
# 
# p2<-plot_choices(results_density_continous,choices = c("x","Analytical_model","Interaction","controls","GENDER","AGE_GROUP"))+
#   labs(x = "Specifications")
# 
# plot_specs(plot_a=p1,plot_b=p2)

#################################################################################################
#standard model quartile meat
#same as above except for standard model quartile meat
cox_no_interaction_standard_quartile<-function(formula,data){
  if(sum(data$GENDER=="Female")==nrow(data)){
    coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_2nd+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_3rd+AGE_CONTINIOUS+SMOKING+TOTAL_ENERGY+COHORT_YEAR+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data)}
  else if (sum(data$GENDER=="Male")==nrow(data)){
    coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_2nd+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_3rd+AGE_CONTINIOUS+SMOKING+TOTAL_ENERGY+COHORT_YEAR")),data=data)}
  else{
    coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_2nd+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_3rd+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+COHORT_YEAR")),data=data)}
}

# cox_age_interaction_standard_quartile<-function(formula,data){
#   if(sum(data$GENDER=="Female")==nrow(data)){
#     coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_2nd+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_3rd+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_2nd:AGE_CONTINIOUS+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_3rd:AGE_CONTINIOUS+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_4th:AGE_CONTINIOUS+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data)}
#   else{
#     coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_2nd+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_3rd+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_2nd:AGE_CONTINIOUS+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_3rd:AGE_CONTINIOUS+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_4th:AGE_CONTINIOUS+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY")),data=data)}
# }
# 
# cox_sex_interaction_standard_quartile<-function(formula,data){
#   if(sum(data$GENDER=="Female")==nrow(data)){
#     coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_2nd+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_3rd+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_2nd:GENDER+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_3rd:GENDER+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_4th:GENDER+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data)}
#   else{
#     coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_2nd+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_3rd+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_2nd:GENDER+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_3rd:GENDER+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_4th:GENDER+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY")),data=data)}
# }
# 
# cox_bmi_interaction_standard_quartile<-function(formula,data){
#   if(sum(data$GENDER=="Female")==nrow(data)){
#     coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_2nd+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_3rd+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_2nd:BMI_CONTINOUS+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_3rd:BMI_CONTINOUS+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_4th:BMI_CONTINOUS+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data)}
#   else{
#     coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_2nd+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_3rd+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_2nd:BMI_CONTINOUS+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_3rd:BMI_CONTINOUS+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_4th:BMI_CONTINOUS+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY")),data=data)}
# }

results_standard_quartile <- run_specs(df = DATA,
                                        y = c("Surv(PERMTH_INT,MORTSTAT)"),
                                        x = c("UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_4th"), 
                                        model = c("cox_no_interaction_standard_quartile"),
                                        controls = adjusting_variables[1],
                                        subsets = list(GENDER = unique(DATA$GENDER),
                                                       AGE_GROUP = unique(DATA$AGE_GROUP)))
results_standard_quartile<-results_standard_quartile[results_standard_quartile$controls!="no covariates",]
for(i in 1:nrow(results_standard_quartile)){
  results_standard_quartile$controls[i]<-adjusting_variables[[2]][which(adjusting_variables[[1]]==results_standard_quartile$controls[i])]
}

#calculate HR and CI comparing highest quartile to lowest quartile
results_standard_quartile$estimate<-exp(results_standard_quartile$estimate)
results_standard_quartile$conf.low<-exp(results_standard_quartile$conf.low)
results_standard_quartile$conf.high<-exp(results_standard_quartile$conf.high)

results_standard_quartile<-results_standard_quartile[results_standard_quartile$conf.low>=0.2&results_standard_quartile$conf.high<=5,]
results_standard_quartile<-results_standard_quartile %>% mutate(GENDER = case_when (grepl("GENDER = Female",subsets)~"Female",
                                                                                    grepl("GENDER = Male",subsets)~"Male",!grepl("GENDER = Female",subsets)&!grepl("GENDER = Male",subsets)~"All Sex"))
results_standard_quartile<-results_standard_quartile %>% mutate(AGE_GROUP = case_when (grepl("AGE_GROUP = 20-39 years old",subsets)~"20-39 Years Old",
                                                                                       grepl("AGE_GROUP = 40-59 years old",subsets)~"40-59 Years Old",
                                                                                       grepl("AGE_GROUP = 60-79 years old",subsets)~"60-79 Years Old",
                                                                                       !grepl("AGE_GROUP = 20-39 years old",subsets)&!grepl("AGE_GROUP = 40-59 years old",subsets)&
                                                                                         !grepl("AGE_GROUP = 60-79 years old",subsets)~"All Age"))                            

# results_standard_quartile <- results_standard_quartile %>% mutate(Interaction = case_when(model=="cox_no_interaction_standard_quartile"~"No interaction included",
#                                                                                           model=="cox_age_interaction_standard_quartile"~"Include interaction only with Age",
#                                                                                           model=="cox_bmi_interaction_standard_quartile"~"Include interaction only with BMI",
#                                                                                           model=="cox_sex_interaction_standard_quartile"&GENDER!="All sex"~"No interaction included",
#                                                                                           model=="cox_sex_interaction_standard_quartile"&GENDER=="All sex"~"Include interaction only with Sex"))

results_standard_quartile <- results_standard_quartile %>% mutate(Analytical_model = "Standard Model")

# p1<-plot_curve(results_standard_quartile,ci=FALSE) +
#   geom_hline(yintercept = 1, linetype = "dashed", color = "grey") +
#   ylim(0, 2) +
#   labs(x = "", y = "Hazard Ratio")
# 
# p2<-plot_choices(results_standard_quartile,choices = c("x","Analytical_model","Interaction","controls","GENDER","AGE_GROUP"))+
#   labs(x = "Specifications")
# 
# #results<-results %>% mutate(DMDMARTL = ifelse(grepl("DMDMARTL",controls),"DMDMARTL","No"))
# 
# plot_specs(plot_a=p1,plot_b=p2)

#############################################################################
#standard model quintile meat
#same as above except for standard model quintile meat
cox_no_interaction_standard_quintile<-function(formula,data){
  if(sum(data$GENDER=="Female")==nrow(data)){
    coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_2nd+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_3rd+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_4th+AGE_CONTINIOUS+SMOKING+TOTAL_ENERGY+COHORT_YEAR+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data)}
  else if(sum(data$GENDER=="Male")==nrow(data)){
    coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_2nd+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_3rd+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_4th+AGE_CONTINIOUS+SMOKING+TOTAL_ENERGY+COHORT_YEAR")),data=data)}
  else{
    coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_2nd+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_3rd+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_4th+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+COHORT_YEAR")),data=data)}
}

# cox_age_interaction_standard_quintile<-function(formula,data){
#   if(sum(data$GENDER=="Female")==nrow(data)){
#     coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_2nd+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_3rd+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_4th+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_2nd:AGE_CONTINIOUS+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_3rd:AGE_CONTINIOUS+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_4th:AGE_CONTINIOUS+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_5th:AGE_CONTINIOUS+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data)}
#   else{
#     coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_2nd+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_3rd+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_4th+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_2nd:AGE_CONTINIOUS+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_3rd:AGE_CONTINIOUS+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_4th:AGE_CONTINIOUS+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_5th:AGE_CONTINIOUS+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY")),data=data)}
# }
# 
# cox_sex_interaction_standard_quintile<-function(formula,data){
#   if(sum(data$GENDER=="Female")==nrow(data)){
#     coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_2nd+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_3rd+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_4th+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_2nd:GENDER+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_3rd:GENDER+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_4th:GENDER+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_5th:GENDER+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data)}
#   else{
#     coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_2nd+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_3rd+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_4th+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_2nd:GENDER+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_3rd:GENDER+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_4th:GENDER+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_5th:GENDER+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY")),data=data)}
# }
# 
# cox_bmi_interaction_standard_quintile<-function(formula,data){
#   if(sum(data$GENDER=="Female")==nrow(data)){
#     coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_2nd+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_3rd+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_4th+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_2nd:BMI_CONTINOUS+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_3rd:BMI_CONTINOUS+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_4th:BMI_CONTINOUS+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_5th:BMI_CONTINOUS+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data)}
#   else{
#     coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_2nd+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_3rd+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_4th+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_2nd:BMI_CONTINOUS+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_3rd:BMI_CONTINOUS+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_4th:BMI_CONTINOUS+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_5th:BMI_CONTINOUS+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY")),data=data)}
# }


results_standard_quintile <- run_specs(df = DATA,
                                       y = c("Surv(PERMTH_INT,MORTSTAT)"),
                                       x = c("UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_5th"), 
                                       model = c("cox_no_interaction_standard_quintile"),
                                       controls = adjusting_variables[1],
                                       subsets = list(GENDER = unique(DATA$GENDER),
                                                      AGE_GROUP = unique(DATA$AGE_GROUP)))

results_standard_quintile<-results_standard_quintile[results_standard_quintile$controls!="no covariates",]
for(i in 1:nrow(results_standard_quintile)){
  results_standard_quintile$controls[i]<-adjusting_variables[[2]][which(adjusting_variables[[1]]==results_standard_quintile$controls[i])]
}

#calculate HR and CI comparing highest quintile to lowest quintile
results_standard_quintile$estimate<-exp(results_standard_quintile$estimate)
results_standard_quintile$conf.low<-exp(results_standard_quintile$conf.low)
results_standard_quintile$conf.high<-exp(results_standard_quintile$conf.high)

results_standard_quintile<-results_standard_quintile[results_standard_quintile$conf.low>=0.2&results_standard_quintile$conf.high<=5,]
results_standard_quintile<-results_standard_quintile %>% mutate(GENDER = case_when (grepl("GENDER = Female",subsets)~"Female",
                                                                                    grepl("GENDER = Male",subsets)~"Male",!grepl("GENDER = Female",subsets)&!grepl("GENDER = Male",subsets)~"All Sex"))
results_standard_quintile<-results_standard_quintile %>% mutate(AGE_GROUP = case_when (grepl("AGE_GROUP = 20-39 years old",subsets)~"20-39 Years Old",
                                                                                       grepl("AGE_GROUP = 40-59 years old",subsets)~"40-59 Years Old",
                                                                                       grepl("AGE_GROUP = 60-79 years old",subsets)~"60-79 Years Old",
                                                                                       !grepl("AGE_GROUP = 20-39 years old",subsets)&!grepl("AGE_GROUP = 40-59 years old",subsets)&
                                                                                         !grepl("AGE_GROUP = 60-79 years old",subsets)~"All Age"))                    

# results_standard_quintile <- results_standard_quintile %>% mutate(Interaction = case_when(model=="cox_no_interaction_standard_quintile"~"No interaction included",
#                                                                                           model=="cox_age_interaction_standard_quintile"~"Include interaction only with Age",
#                                                                                           model=="cox_bmi_interaction_standard_quintile"~"Include interaction only with BMI",
#                                                                                           model=="cox_sex_interaction_standard_quintile"&GENDER!="All sex"~"No interaction included",
#                                                                                           model=="cox_sex_interaction_standard_quintile"&GENDER=="All sex"~"Include interaction only with Sex"))

results_standard_quintile <- results_standard_quintile %>% mutate(Analytical_model = "Standard Model")

# p1<-plot_curve(results_standard_quintile,ci=FALSE) +
#   geom_hline(yintercept = 1, linetype = "dashed", color = "grey") +
#   ylim(0, 2) +
#   labs(x = "", y = "Hazard Ratio")
# 
# p2<-plot_choices(results_standard_quintile,choices = c("x","Analytical_model","Interaction","controls","GENDER","AGE_GROUP"))+
#   labs(x = "Specifications")
# 
# #results<-results %>% mutate(DMDMARTL = ifelse(grepl("DMDMARTL",controls),"DMDMARTL","No"))
# 
# plot_specs(plot_a=p1,plot_b=p2)

##########################################################################################################
#Density model quartile meat
#same as above except for density model quartile meat
cox_no_interaction_density_quartile<-function(formula,data){
  if(sum(data$GENDER=="Female")==nrow(data)){
    coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_2nd+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_3rd+AGE_CONTINIOUS+SMOKING+TOTAL_ENERGY+COHORT_YEAR+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data)}
  else if(sum(data$GENDER=="Male")==nrow(data)){
    coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_2nd+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_3rd+AGE_CONTINIOUS+SMOKING+TOTAL_ENERGY+COHORT_YEAR")),data=data)}
  else{
    coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_2nd+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_3rd+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+COHORT_YEAR")),data=data)}
  
  
}

# cox_age_interaction_density_quartile<-function(formula,data){
#   if(sum(data$GENDER=="Female")==nrow(data)){
#     coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_2nd+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_3rd+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_2nd:AGE_CONTINIOUS+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_3rd:AGE_CONTINIOUS+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_4th:AGE_CONTINIOUS+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data)}
#   else{
#     coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_2nd+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_3rd+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_2nd:AGE_CONTINIOUS+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_3rd:AGE_CONTINIOUS+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_4th:AGE_CONTINIOUS+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY")),data=data)}
# }
# 
# cox_sex_interaction_density_quartile<-function(formula,data){
#   if(sum(data$GENDER=="Female")==nrow(data)){
#     coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_2nd+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_3rd+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_2nd:GENDER+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_3rd:GENDER+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_4th:GENDER+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data)}
#   else{
#     coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_2nd+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_3rd+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_2nd:GENDER+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_3rd:GENDER+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_4th:GENDER+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY")),data=data)}
# }
# 
# cox_bmi_interaction_density_quartile<-function(formula,data){
#   if(sum(data$GENDER=="Female")==nrow(data)){
#     coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_2nd+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_3rd+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_2nd:BMI_CONTINOUS+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_3rd:BMI_CONTINOUS+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_4th:BMI_CONTINOUS+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data)}
#   else{
#     coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_2nd+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_3rd+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_2nd:BMI_CONTINOUS+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_3rd:BMI_CONTINOUS+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_4th:BMI_CONTINOUS+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY")),data=data)}
# }

results_density_quartile <- run_specs(df = DATA,
                                      y = c("Surv(PERMTH_INT,MORTSTAT)"),
                                      x = c("UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_4th"), 
                                      model = c("cox_no_interaction_density_quartile"),
                                      controls = adjusting_variables[1],
                                      subsets = list(GENDER = unique(DATA$GENDER),
                                                     AGE_GROUP = unique(DATA$AGE_GROUP)))
results_density_quartile<-results_density_quartile[results_density_quartile$controls!="no covariates",]
for(i in 1:nrow(results_density_quartile)){
  results_density_quartile$controls[i]<-adjusting_variables[[2]][which(adjusting_variables[[1]]==results_density_quartile$controls[i])]
}
#calculate HR and CI comparing highest quartile to lowest quartile
results_density_quartile$estimate<-exp(results_density_quartile$estimate)
results_density_quartile$conf.low<-exp(results_density_quartile$conf.low)
results_density_quartile$conf.high<-exp(results_density_quartile$conf.high)

results_density_quartile<-results_density_quartile[results_density_quartile$conf.low>=0.2&results_density_quartile$conf.high<=5,]
results_density_quartile<-results_density_quartile %>% mutate(GENDER = case_when (grepl("GENDER = Female",subsets)~"Female",
                                                                                  grepl("GENDER = Male",subsets)~"Male",!grepl("GENDER = Female",subsets)&!grepl("GENDER = Male",subsets)~"All Sex"))
results_density_quartile<-results_density_quartile %>% mutate(AGE_GROUP = case_when (grepl("AGE_GROUP = 20-39 years old",subsets)~"20-39 Years Old",
                                                                                     grepl("AGE_GROUP = 40-59 years old",subsets)~"40-59 Years Old",
                                                                                     grepl("AGE_GROUP = 60-79 years old",subsets)~"60-79 Years Old",
                                                                                     !grepl("AGE_GROUP = 20-39 years old",subsets)&!grepl("AGE_GROUP = 40-59 years old",subsets)&
                                                                                       !grepl("AGE_GROUP = 60-79 years old",subsets)~"All Age"))                              

# results_density_quartile <- results_density_quartile %>% mutate(Interaction = case_when(model=="cox_no_interaction_density_quartile"~"No interaction included",
#                                                                                         model=="cox_age_interaction_density_quartile"~"Include interaction only with Age",
#                                                                                         model=="cox_bmi_interaction_density_quartile"~"Include interaction only with BMI",
#                                                                                         model=="cox_sex_interaction_density_quartile"&GENDER!="All sex"~"No interaction included",
#                                                                                         model=="cox_sex_interaction_density_quartile"&GENDER=="All sex"~"Include interaction only with Sex"))

results_density_quartile <- results_density_quartile %>% mutate(Analytical_model = "Multivariable Nutrition Density Model")

# p1<-plot_curve(results_density_quartile,ci=FALSE) +
#   geom_hline(yintercept = 1, linetype = "dashed", color = "grey") +
#   ylim(0, 2) +
#   labs(x = "", y = "Hazard Ratio")
# 
# p2<-plot_choices(results_density_quartile,choices = c("x","Analytical_model","Interaction","controls","GENDER","AGE_GROUP"))+
#   labs(x = "Specifications")
# 
# #results<-results %>% mutate(DMDMARTL = ifelse(grepl("DMDMARTL",controls),"DMDMARTL","No"))
# 
# plot_specs(plot_a=p1,plot_b=p2)

############################################################################################################
#Density model quintle meat
#same as above except for density model quintile meat
cox_no_interaction_density_quintile<-function(formula,data){
  if(sum(data$GENDER=="Female")==nrow(data)){
    coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_2nd+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_3rd+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_4th+AGE_CONTINIOUS+SMOKING+TOTAL_ENERGY+COHORT_YEAR+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data)}
  else if(sum(data$GENDER=="Male")==nrow(data)){
    coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_2nd+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_3rd+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_4th+AGE_CONTINIOUS+SMOKING+TOTAL_ENERGY+COHORT_YEAR")),data=data)}
  else{
    coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_2nd+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_3rd+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_4th+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+COHORT_YEAR")),data=data)}
  
  
}

# cox_age_interaction_density_quintile<-function(formula,data){
#   if(sum(data$GENDER=="Female")==nrow(data)){
#     coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_2nd+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_3rd+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_4th+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_2nd:AGE_CONTINIOUS+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_3rd:AGE_CONTINIOUS+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_4th:AGE_CONTINIOUS+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_5th:AGE_CONTINIOUS+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data)}
#   else{
#     coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_2nd+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_3rd+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_4th+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_2nd:AGE_CONTINIOUS+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_3rd:AGE_CONTINIOUS+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_4th:AGE_CONTINIOUS+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_5th:AGE_CONTINIOUS+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY")),data=data)}
# }
# 
# cox_sex_interaction_density_quintile<-function(formula,data){
#   if(sum(data$GENDER=="Female")==nrow(data)){
#     coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_2nd+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_3rd+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_4th+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_2nd:GENDER+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_3rd:GENDER+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_4th:GENDER+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_5th:GENDER+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data)}
#   else{
#     coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_2nd+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_3rd+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_4th+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_2nd:GENDER+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_3rd:GENDER+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_4th:GENDER+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_5th:GENDER+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY")),data=data)}
# }
# 
# cox_bmi_interaction_density_quintile<-function(formula,data){
#   if(sum(data$GENDER=="Female")==nrow(data)){
#     coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_2nd+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_3rd+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_4th+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_2nd:BMI_CONTINOUS+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_3rd:BMI_CONTINOUS+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_4th:BMI_CONTINOUS+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_5th:BMI_CONTINOUS+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data)}
#   else{
#     coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_2nd+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_3rd+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_4th+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_2nd:BMI_CONTINOUS+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_3rd:BMI_CONTINOUS+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_4th:BMI_CONTINOUS+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_5th:BMI_CONTINOUS+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY")),data=data)}
# }


results_density_quintile <- run_specs(df = DATA,
                                      y = c("Surv(PERMTH_INT,MORTSTAT)"),
                                      x = c("UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_5th"), 
                                      model = c("cox_no_interaction_density_quintile"),
                                      controls = adjusting_variables[1],
                                      subsets = list(GENDER = unique(DATA$GENDER),
                                                     AGE_GROUP = unique(DATA$AGE_GROUP)))

results_density_quintile<-results_density_quintile[results_density_quintile$controls!="no covariates",]
for(i in 1:nrow(results_density_quintile)){
  results_density_quintile$controls[i]<-adjusting_variables[[2]][which(adjusting_variables[[1]]==results_density_quintile$controls[i])]
}
#calculate HR and CI comparing highest quintile to lowest quintile
results_density_quintile$estimate<-exp(results_density_quintile$estimate)
results_density_quintile$conf.low<-exp(results_density_quintile$conf.low)
results_density_quintile$conf.high<-exp(results_density_quintile$conf.high)

results_density_quintile<-results_density_quintile[results_density_quintile$conf.low>=0.2&results_density_quintile$conf.high<=5,]
results_density_quintile<-results_density_quintile %>% mutate(GENDER = case_when (grepl("GENDER = Female",subsets)~"Female",
                                                                                  grepl("GENDER = Male",subsets)~"Male",!grepl("GENDER = Female",subsets)&!grepl("GENDER = Male",subsets)~"All Sex"))
results_density_quintile<-results_density_quintile %>% mutate(AGE_GROUP = case_when (grepl("AGE_GROUP = 20-39 years old",subsets)~"20-39 Years Old",
                                                                                     grepl("AGE_GROUP = 40-59 years old",subsets)~"40-59 Years Old",
                                                                                     grepl("AGE_GROUP = 60-79 years old",subsets)~"60-79 Years Old",
                                                                                     !grepl("AGE_GROUP = 20-39 years old",subsets)&!grepl("AGE_GROUP = 40-59 years old",subsets)&
                                                                                       !grepl("AGE_GROUP = 60-79 years old",subsets)~"All Age"))                              

# results_density_quintile <- results_density_quintile %>% mutate(Interaction = case_when(model=="cox_no_interaction_density_quintile"~"No interaction included",
#                                                                                         model=="cox_age_interaction_density_quintile"~"Include interaction only with Age",
#                                                                                         model=="cox_bmi_interaction_density_quintile"~"Include interaction only with BMI",
#                                                                                         model=="cox_sex_interaction_density_quintile"&GENDER!="All sex"~"No interaction included",
#                                                                                         model=="cox_sex_interaction_density_quintile"&GENDER=="All sex"~"Include interaction only with Sex"))

results_density_quintile <- results_density_quintile %>% mutate(Analytical_model = "Multivariable Nutrition Density Model")

# p1<-plot_curve(results_density_quintile,ci=FALSE) +
#   geom_hline(yintercept = 1, linetype = "dashed", color = "grey") +
#   ylim(0, 2) +
#   labs(x = "", y = "Hazard Ratio")
# 
# p2<-plot_choices(results_density_quintile,choices = c("x","Analytical_model","Interaction","controls","GENDER","AGE_GROUP"))+
#   labs(x = "Specifications")
# 
# #results<-results %>% mutate(DMDMARTL = ifelse(grepl("DMDMARTL",controls),"DMDMARTL","No"))
# 
# plot_specs(plot_a=p1,plot_b=p2)

####################################################################################################

#combine all 6 result tables
combined_results<-rbind(results_standard_continous, results_standard_quartile,results_standard_quintile,results_density_continous,results_density_quartile,results_density_quintile)

#unprocessed red meat can be continous, quartile or quintile
combined_results$x[grepl('CONTINOUS', combined_results$x)]<-"Continuous"
combined_results$x[grepl('QUARTILE', combined_results$x)]<-"Quartile"
combined_results$x[grepl('QUINTILE', combined_results$x)]<-"Quintile"
#rename columns
combined_results<-combined_results%>%rename(MeatType=x,Model=Analytical_model,AdjustingVariables=controls,SexGroup=GENDER,AgeGroup=AGE_GROUP)

#customize upper plot
p1<-plot_curve(combined_results,ci=TRUE,null=1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey") +
  ylim(0, 5) +
  labs(x = "", y = "Hazard Ratio")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(text = element_text(size = 20))

#customize lower plot
p2<-plot_choices(combined_results,choices = c("Model","MeatType","AdjustingVariables","SexGroup","AgeGroup"),null=1)+
  labs(x = "Specifications")+ theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+theme(text = element_text(size = 20))

#make SCA plot
plot_specs(plot_a=p1,plot_b=p2,rel_heights = c(1, 2),null=1)

#calculate median hazard ratio
median(combined_results$estimate)

#IQR for hazard ratio
quantile(combined_results$estimate,prob=c(0.25,0.75))

nrow(combined_results[combined_results$p.value<=0.05,])/nrow(combined_results)

##############testing##################
# #this function used to check if specification function performed as we expected.
# library(stringr)
# test<-function(results_table){
#   index<-sample(1:nrow(results_table), 1)
#   cat("row",index,"of results_table \n")
#   test_table<-results_table[index,c("x","y","controls","estimate","conf.low","conf.high","GENDER","AGE_GROUP")]
#   cat("test table\n")
#   print(test_table)
#   adj<-adjusting_variables[[1]][which(adjusting_variables[[2]]==test_table$controls)]
#   if(grepl("DENSITY",test_table$x)&grepl("CONTINOUS",test_table$x)){
#     mandatory<-"UNPROCESSED_RED_MEAT_DENSITY_CONTINOUS+"
#     column<-1
#   }
#   if(grepl("DENSITY",test_table$x)&grepl("QUARTILE",test_table$x)){
#     mandatory<-"UNPROCESSED_RED_MEAT_DENSITY_QUARTILE+"
#     column<-3
#   }
#   if(grepl("DENSITY",test_table$x)&grepl("QUINTILE",test_table$x)){
#     mandatory<-"UNPROCESSED_RED_MEAT_DENSITY_QUINTILES+"
#     column<-4
#   }
#   if(grepl("STANDARD",test_table$x)&grepl("CONTINOUS",test_table$x)){
#     mandatory<-"UNPROCESSED_RED_MEAT_STANDARD_CONTINOUS+"
#     column<-1
#   }
#   if(grepl("STANDARD",test_table$x)&grepl("QUARTILE",test_table$x)){
#     mandatory<-"+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE+"
#     column<-3
#   }
#   if(grepl("STANDARD",test_table$x)&grepl("QUINTILE",test_table$x)){
#     mandatory<-"UNPROCESSED_RED_MEAT_STANDARD_QUINTILES+"
#     column<-4
#   }
#   if(test_table$GENDER=="All Sex"&test_table$AGE_GROUP=="All Age"){
#     data<-DATA
#   }
#   if(test_table$GENDER=="All Sex"&test_table$AGE_GROUP!="All Age"){
#     data<-DATA[str_to_title(DATA$AGE_GROUP)==test_table$AGE_GROUP,]
#   }
#   if(test_table$GENDER!="All Sex"&test_table$AGE_GROUP=="All Age"){
#     data<-DATA[DATA$GENDER==test_table$GENDER,]
#   }
#   if(test_table$GENDER!="All Sex"&test_table$AGE_GROUP!="All Age"){
#     data<-DATA[DATA$GENDER==test_table$GENDER&str_to_title(DATA$AGE_GROUP)==test_table$AGE_GROUP,]
#   }
#   if(test_table$GENDER=="Female"){
#     model<-coxph(formula=as.formula(paste(test_table$y,"~",mandatory,adj,"+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data)  
#   }
#   if(test_table$GENDER!="Female"){
#     model<-coxph(formula=as.formula(paste(test_table$y,"~",mandatory,adj,"+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY")),data=data)  
#   }
#   cat("Model HR and CI \n")
#   summary(model)$conf.int[column,]
# }

#######################################
#check proportional hazard assumption
#beneficial and significant effect for unprocessed red meat
library(stringr)
library(survminer)
library(survival)

ph_assumption<-function(results_table,row){
  
  test_table<-results_table[row,c("MeatType","y","AdjustingVariables","estimate","conf.low","conf.high","p.value","SexGroup","AgeGroup","Model")]
  adj<-adjusting_variables[[1]][adjusting_variables[[2]] == test_table$AdjustingVariables]
  cat("The model used is:",test_table$Model,"\n")
  cat("The type of meat used is:",test_table$MeatType,"\n")
  cat("The sex group used is:",test_table$SexGroup,"\n")
  cat("The age group used is:",test_table$AgeGroup,"\n")
  cat("The optional adjusting variables are: \n",adj,"\n")
  cat("The index of optional adjusting variables are:\n",test_table$AdjustingVariables,"\n")
  cat("The HR with 95% CI is",test_table$estimate,"(",test_table$conf.low,",",test_table$conf.high,") with p value =",test_table$p.value,"\n")
  if(grepl("Density",test_table$Model)&grepl("Continuous",test_table$MeatType)){
    mandatory<-"UNPROCESSED_RED_MEAT_DENSITY_CONTINOUS+"
    column<-1
  }
  if(grepl("Density",test_table$Model)&grepl("Quartile",test_table$MeatType)){
    mandatory<-"UNPROCESSED_RED_MEAT_DENSITY_QUARTILE+"
    column<-3
  }
  if(grepl("Density",test_table$Model)&grepl("Quintile",test_table$MeatType)){
    mandatory<-"UNPROCESSED_RED_MEAT_DENSITY_QUINTILES+"
    column<-4
  }
  if(grepl("Standard",test_table$Model)&grepl("Continuous",test_table$MeatType)){
    mandatory<-"UNPROCESSED_RED_MEAT_STANDARD_CONTINOUS+"
    column<-1
  }
  if(grepl("Standard",test_table$Model)&grepl("Quartile",test_table$MeatType)){
    mandatory<-"+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE+"
    column<-3
  }
  if(grepl("Standard",test_table$Model)&grepl("Quintile",test_table$MeatType)){
    mandatory<-"UNPROCESSED_RED_MEAT_STANDARD_QUINTILES+"
    column<-4
  }

  if(test_table$SexGroup=="All Sex"&test_table$AgeGroup=="All Age"){
    data<-DATA
  }
  if(test_table$SexGroup=="All Sex"&test_table$AgeGroup!="All Age"){
    data<-DATA[str_to_title(DATA$AGE_GROUP)==test_table$AgeGroup,]
  }
  if(test_table$SexGroup!="All Sex"&test_table$AgeGroup=="All Age"){
    data<-DATA[DATA$GENDER==test_table$SexGroup,]
  }
  if(test_table$SexGroup!="All Sex"&test_table$AgeGroup!="All Age"){
    data<-DATA[DATA$GENDER==test_table$SexGroup&str_to_title(DATA$AGE_GROUP)==test_table$AgeGroup,]
  }
  if(test_table$SexGroup=="Female"){
    model<-coxph(formula=as.formula(paste(test_table$y,"~",mandatory,adj,"+COHORT_YEAR+AGE_CONTINIOUS+SMOKING+TOTAL_ENERGY+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data)
  }
  if(test_table$SexGroup=="Male"){
    model<-coxph(formula=as.formula(paste(test_table$y,"~",mandatory,adj,"+COHORT_YEAR+AGE_CONTINIOUS+SMOKING+TOTAL_ENERGY")),data=data)
  }
  if(test_table$SexGroup=="All Sex"){
    model<-coxph(formula=as.formula(paste(test_table$y,"~",mandatory,adj,"+COHORT_YEAR+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY")),data=data)
  }
  print(summary(model)$conf.int[column,])
  print(summary(model)$coefficient[column,5])
  test.shoen<-cox.zph(model,transform="rank")
  return(test.shoen)
  
}

bene_significant<-combined_results[combined_results$estimate<1&combined_results$p.value<=0.05,][c(2,40),]
harm_significant<-combined_results[combined_results$estimate>1&combined_results$p.value<=0.05,][c(1,7),]
non_significant<-combined_results[combined_results$p.value>0.05,][c(211),]

test.shoen_1<-ph_assumption(bene_significant,row=1)
ggcoxzph(test.shoen_1,ggtheme = theme_survminer(font.main = c(20, "plain", "black"),font.submain = c(20, "plain", "black"),font.x = c(20, "plain", "black"),
                                              font.y = c(20, "plain", "black"),font.caption = c(20, "plain", "black"),font.tickslab = c(20, "plain", "black")))
test.shoen_2<-ph_assumption(bene_significant,row=2)
ggcoxzph(test.shoen_2,ggtheme = theme_survminer(font.main = c(20, "plain", "black"),font.submain = c(20, "plain", "black"),font.x = c(20, "plain", "black"),
                                                font.y = c(20, "plain", "black"),font.caption = c(20, "plain", "black"),font.tickslab = c(20, "plain", "black")))
test.shoen_3<-ph_assumption(harm_significant,row=1)
ggcoxzph(test.shoen_3,ggtheme = theme_survminer(font.main = c(20, "plain", "black"),font.submain = c(20, "plain", "black"),font.x = c(20, "plain", "black"),
                                                font.y = c(20, "plain", "black"),font.caption = c(20, "plain", "black"),font.tickslab = c(20, "plain", "black")))
test.shoen_4<-ph_assumption(harm_significant,row=2)
ggcoxzph(test.shoen_4,ggtheme = theme_survminer(font.main = c(20, "plain", "black"),font.submain = c(20, "plain", "black"),font.x = c(20, "plain", "black"),
                                                font.y = c(20, "plain", "black"),font.caption = c(20, "plain", "black"),font.tickslab = c(20, "plain", "black")))
test.shoen_5<-ph_assumption(non_significant,row=1)
ggcoxzph(test.shoen_5,ggtheme = theme_survminer(font.main = c(20, "plain", "black"),font.submain = c(20, "plain", "black"),font.x = c(20, "plain", "black"),
                                                font.y = c(20, "plain", "black"),font.caption = c(20, "plain", "black"),font.tickslab = c(20, "plain", "black")))

#write to a csv
write.csv(test.shoen_1$table,"./bene_1.csv", row.names = TRUE)
#write to a csv
write.csv(test.shoen_2$table,"./bene_2.csv", row.names = TRUE)
#write to a csv
write.csv(test.shoen_3$table,"./harm_1.csv", row.names = TRUE)
#write to a csv
write.csv(test.shoen_4$table,"./harm_2.csv", row.names = TRUE)
#write to a csv
write.csv(test.shoen_5$table,"./non_significant_1.csv", row.names = TRUE)


