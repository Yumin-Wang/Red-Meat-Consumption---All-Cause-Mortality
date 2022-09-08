#install.packages("devtools")
#devtools::install_github("masurp/specr")
source("DATA/Combined DATA/DATA_formatting.r")
library(survival)
library(tidyverse)
library(specr)

#Variable_names<-Variable_names[50:54]
adjusting_variable_generator<-function(n=10){
  Variable_names<-names(DATA)
  Variable_names<-Variable_names[!Variable_names %in% c("SEQN","MORTSTAT","PERMTH_INT","AGE_CONTINIOUS","AGE_DEATH_CENSORED","AGE_GROUP","GENDER","SMOKING","MENOPAUSAL_STATUS","HORMONE_THERAPY_USE","PARITY","ORAL_CONTRACEPTIVE_USE","TOTAL_ENERGY","UNPROCESSED_RED_MEAT_STANDARD_CONTINOUS","UNPROCESSED_RED_MEAT_STANDARD_QUARTILE","UNPROCESSED_RED_MEAT_STANDARD_QUINTILES","UNPROCESSED_RED_MEAT_DENSITY_CONTINOUS","UNPROCESSED_RED_MEAT_DENSITY_QUARTILE","UNPROCESSED_RED_MEAT_DENSITY_QUINTILES")]
  variable_combination = vector(length = n)
  variable_index = vector(length=n)
  for (i in 1:n){
    include<-rbinom(n=length(Variable_names), size=1, prob=0.5)
    #not include alcohol continuous or categorical at same time
    if(include[5]*include[6]==1){
      if(rbinom(n=1,size=1,prob=0.5)==1){include[5]<-0}
      else{include[6]<-0}
    }
    #not include bmi continuous or categorical at same time
    if(include[13]*include[14]==1){
      if(rbinom(n=1,size=1,prob=0.5)==1){include[13]<-0}
      else{include[14]<-0}
    }
    sampled_variables<-Variable_names[include==1]
    variable_combination[i]<-paste(sampled_variables,collapse="+")
    variable_index[i]<-paste(which(include==1),collapse=",")
    
  }
  return (list(variable_combination,variable_index))
}

adjusting_variables<-adjusting_variable_generator(n=2)



###############################################################################################
#standard model plus continuous meat
cox_no_interaction_standard_continous<-function(formula,data){
  if(sum(data$GENDER=="Female")==nrow(data)){
    coxph(formula=as.formula(paste(formula,"+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data,ties="breslow")}
  else{
    coxph(formula=as.formula(paste(formula,"+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY")),data=data,ties="breslow")}
}
cox_age_interaction_standard_continous<-function(formula,data){
  if(sum(data$GENDER=="Female")==nrow(data)){
    coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_STANDARD_CONTINOUS:AGE_CONTINIOUS+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data,ties="breslow")}
  else{
    coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_STANDARD_CONTINOUS:AGE_CONTINIOUS+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY")),data=data,ties="breslow")}
}
cox_sex_interaction_standard_continous<-function(formula,data){
  if(sum(data$GENDER=="Female")==nrow(data)){
    coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_STANDARD_CONTINOUS:GENDER+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data,ties="breslow")}
  else{
    coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_STANDARD_CONTINOUS:GENDER+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY")),data=data,ties="breslow")}
}
cox_bmi_interaction_standard_continous<-function(formula,data){
  if(sum(data$GENDER=="Female")==nrow(data)){
    coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_STANDARD_CONTINOUS:BMI_CONTINOUS+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data,ties="breslow")}
  else{
    coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_STANDARD_CONTINOUS:BMI_CONTINOUS+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY")),data=data,ties="breslow")}
}

results_standard_continous <- run_specs(df = DATA,
                      y = c("Surv(PERMTH_INT,MORTSTAT)"),
                      x = c("UNPROCESSED_RED_MEAT_STANDARD_CONTINOUS"), 
                      model = c("cox_no_interaction_standard_continous","cox_age_interaction_standard_continous","cox_sex_interaction_standard_continous","cox_bmi_interaction_standard_continous"),
                      controls = adjusting_variables[1],
                      subsets = list(GENDER = unique(DATA$GENDER),
                                     AGE_GROUP = unique(DATA$AGE_GROUP)))
results_standard_continous<-results_standard_continous[results_standard_continous$controls!="no covariates",]
for(i in 1:nrow(results_standard_continous)){
  results_standard_continous$controls[i]<-adjusting_variables[[2]][which(adjusting_variables[[1]]==results_standard_continous$controls[i])]
}
results_standard_continous$estimate<-exp(results_standard_continous$estimate*100)
#results_standard_continous$conf.low<-exp(results_standard_continous$conf.low*100)
#results_standard_continous$conf.high<-exp(results_standard_continous$conf.high*100)
results_standard_continous<-results_standard_continous[results_standard_continous$estimate<=2&results_standard_continous$estimate>0.2,]
results_standard_continous<-results_standard_continous %>% mutate(GENDER = case_when (grepl("GENDER = Female",subsets)~"FEMALE",
                               grepl("GENDER = Male",subsets)~"MALE",!grepl("GENDER = Female",subsets)&!grepl("GENDER = Male",subsets)~"All sex"))
results_standard_continous<-results_standard_continous %>% mutate(AGE_GROUP = case_when (grepl("AGE_GROUP = 20-29 years old",subsets)~"AGE_GROUP = 20-29 years old",
                                                   grepl("AGE_GROUP = 30-39 years old",subsets)~"AGE_GROUP = 30-39 years old",
                                                   grepl("AGE_GROUP = 40-49 years old",subsets)~"AGE_GROUP = 40-49 years old",
                                                   grepl("AGE_GROUP = 50-59 years old",subsets)~"AGE_GROUP = 50-59 years old",
                                                   grepl("AGE_GROUP = 60-69 years old",subsets)~"AGE_GROUP = 60-69 years old",
                                                   grepl("AGE_GROUP = 70-79 years old",subsets)~"AGE_GROUP = 70-79 years old",
                                                   !grepl("AGE_GROUP = 20-29 years old",subsets)&!grepl("AGE_GROUP = 30-39 years old",subsets)&
                                                     !grepl("AGE_GROUP = 40-49 years old",subsets)&!grepl("AGE_GROUP = 50-59 years old",subsets)&
                                                     !grepl("AGE_GROUP = 60-69 years old",subsets)&!grepl("AGE_GROUP = 70-79 years old",subsets)~"All age"))                             
results_standard_continous <- results_standard_continous %>% mutate(Interaction = case_when(model=="cox_no_interaction_standard_continous"~"No interaction included",
                                                      model=="cox_age_interaction_standard_continous"~"Include interaction only with Age",
                                                      model=="cox_bmi_interaction_standard_continous"~"Include interaction only with BMI",
                                                      model=="cox_sex_interaction_standard_continous"&GENDER!="All sex"~"No interaction included",
                                                      model=="cox_sex_interaction_standard_continous"&GENDER=="All sex"~"Include interaction only with Sex"))
results_standard_continous <- results_standard_continous %>% mutate(Analytical_model = "Standard model")


###################################################################################################

p1<-plot_curve(results_standard_continous,ci=FALSE) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey") +
  ylim(0, 2) +
  labs(x = "", y = "Hazard Ratio")

p2<-plot_choices(results_standard_continous,choices = c("x","Analytical_model","Interaction","controls","GENDER","AGE_GROUP"))+
  labs(x = "Specifications")

#results<-results %>% mutate(DMDMARTL = ifelse(grepl("DMDMARTL",controls),"DMDMARTL","No"))

plot_specs(plot_a=p1,plot_b=p2)

#Density model continuous meat
###############################################################################
cox_no_interaction_density_continous<-function(formula,data){
  if(sum(data$GENDER=="Female")==nrow(data)){
    coxph(formula=as.formula(paste(formula,"+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data,ties="breslow")}
  else{
    coxph(formula=as.formula(paste(formula,"+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY")),data=data,ties="breslow")}
}
cox_age_interaction_density_continous<-function(formula,data){
  if(sum(data$GENDER=="Female")==nrow(data)){
    coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_DENSITY_CONTINOUS:AGE_CONTINIOUS+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data,ties="breslow")}
  else{
    coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_DENSITY_CONTINOUS:AGE_CONTINIOUS+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY")),data=data,ties="breslow")}
}
cox_sex_interaction_density_continous<-function(formula,data){
  if(sum(data$GENDER=="Female")==nrow(data)){
    coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_DENSITY_CONTINOUS:GENDER+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data,ties="breslow")}
  else{
    coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_DENSITY_CONTINOUS:GENDER+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY")),data=data,ties="breslow")}
}
cox_bmi_interaction_density_continous<-function(formula,data){
  if(sum(data$GENDER=="Female")==nrow(data)){
    coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_DENSITY_CONTINOUS:BMI_CONTINOUS+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data,ties="breslow")}
  else{
    coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_DENSITY_CONTINOUS:BMI_CONTINOUS+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY")),data=data,ties="breslow")}
}

results_density_continous <- run_specs(df = DATA,
                                       y = c("Surv(PERMTH_INT,MORTSTAT)"),
                                       x = c("UNPROCESSED_RED_MEAT_DENSITY_CONTINOUS"), 
                                       model = c("cox_no_interaction_density_continous","cox_age_interaction_density_continous","cox_sex_interaction_density_continous","cox_bmi_interaction_density_continous"),
                                       controls = adjusting_variables[1],
                                       subsets = list(GENDER = unique(DATA$GENDER),
                                                      AGE_GROUP = unique(DATA$AGE_GROUP)))
results_density_continous<-results_density_continous[results_density_continous$controls!="no covariates",]
for(i in 1:nrow(results_density_continous)){
  results_density_continous$controls[i]<-adjusting_variables[[2]][which(adjusting_variables[[1]]==results_density_continous$controls[i])]
}
results_density_continous$estimate<-exp(results_density_continous$estimate*100/2000)
results_density_continous<-results_density_continous[results_density_continous$estimate<=2&results_density_continous$estimate>0.2,]
#results_density_continous<-results_density_continous[results_density_continous$std.error<=1,]
results_density_continous<-results_density_continous %>% mutate(GENDER = case_when (grepl("GENDER = Female",subsets)~"FEMALE",
                                                                                    grepl("GENDER = Male",subsets)~"MALE",!grepl("GENDER = Female",subsets)&!grepl("GENDER = Male",subsets)~"All sex"))
results_density_continous<-results_density_continous %>% mutate(AGE_GROUP = case_when (grepl("AGE_GROUP = 20-29 years old",subsets)~"AGE_GROUP = 20-29 years old",
                                                                                       grepl("AGE_GROUP = 30-39 years old",subsets)~"AGE_GROUP = 30-39 years old",
                                                                                       grepl("AGE_GROUP = 40-49 years old",subsets)~"AGE_GROUP = 40-49 years old",
                                                                                       grepl("AGE_GROUP = 50-59 years old",subsets)~"AGE_GROUP = 50-59 years old",
                                                                                       grepl("AGE_GROUP = 60-69 years old",subsets)~"AGE_GROUP = 60-69 years old",
                                                                                       grepl("AGE_GROUP = 70-79 years old",subsets)~"AGE_GROUP = 70-79 years old",
                                                                                       !grepl("AGE_GROUP = 20-29 years old",subsets)&!grepl("AGE_GROUP = 30-39 years old",subsets)&
                                                                                         !grepl("AGE_GROUP = 40-49 years old",subsets)&!grepl("AGE_GROUP = 50-59 years old",subsets)&
                                                                                         !grepl("AGE_GROUP = 60-69 years old",subsets)&!grepl("AGE_GROUP = 70-79 years old",subsets)~"All age"))                             
results_density_continous <- results_density_continous %>% mutate(Interaction = case_when(model=="cox_no_interaction_density_continous"~"No interaction included",
                                                                                          model=="cox_age_interaction_density_continous"~"Include interaction only with Age",
                                                                                          model=="cox_bmi_interaction_density_continous"~"Include interaction only with BMI",
                                                                                          model=="cox_sex_interaction_density_continous"&GENDER!="All sex"~"No interaction included",
                                                                                          model=="cox_sex_interaction_density_continous"&GENDER=="All sex"~"Include interaction only with Sex"))
results_density_continous <- results_density_continous %>% mutate(Analytical_model = "Multivariable nutrition density")


p1<-plot_curve(results_density_continous,ci=FALSE) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey") +
  ylim(0, 2) +
  labs(x = "", y = "Hazard Ratio")

p2<-plot_choices(results_density_continous,choices = c("x","Analytical_model","Interaction","controls","GENDER","AGE_GROUP"))+
  labs(x = "Specifications")

plot_specs(plot_a=p1,plot_b=p2)
































#subset<-DATA[DATA$GENDER=="Female",]
#subset_2<-DATA[DATA$GENDER=="Male",]
#subset_3<-DATA[DATA$AGE_GROUP=="60-69 years old",]
####test
#summary(coxph(Surv(PERMTH_INT,MORTSTAT)~UNPROCESSED_RED_MEAT_+GENDER,data=subset,ties="breslow"))
#summary(coxph(Surv(PERMTH_INT,MORTSTAT)~UNPROCESSED_RED_MEAT_STANDARD_QUARTILE,data=DATA,ties="breslow"))
#summary(coxph(Surv(PERMTH_INT,MORTSTAT)~UNPROCESSED_RED_MEAT_STANDARD_CONTINOUS+MENOPAUSAL_STATUS,data=DATA,ties="breslow"))

#summary(coxph(Surv(PERMTH_INT,MORSTAT)~UNPROCESSED_RED_MEAT_STANDARD_CONTINOUS+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE+EDUCATION+ALCOHOL_GROUP+SLEEP+SOCIOECONOMIC_STATUS+BMI_GROUP+GENERAL_HEALTH_CONDITION+HISTORY_OF_HYPERCHOLESTEROLEMIA+HISTORY_OF_DIABETES+HISTORY_OF_DEPRESSION+ASPIRIN+STATIN+VALSARTAN+ON_SPECIAL_DIET+PROCESSED_MEAT+POULTRY+FRUITS+VEGETABLES+WHOLE_GRAIN+EGGS+LEGUMES+CARBOHYDRATES+SATURATED_FAT+POLYUNSATURATED_FATTY_ACID+CHOLESTEROL+MAGNESIUM,data=subset,ties="breslow"))