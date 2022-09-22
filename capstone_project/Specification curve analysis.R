#install.packages("devtools")
#devtools::install_github("masurp/specr")
source("DATA/Combined DATA/DATA_formatting.r")
library(survival)
library(tidyverse)
library(specr)
options(nwarnings = 10000)  
set.seed(98431)
#Variable_names<-Variable_names[50:54]
adjusting_variable_generator<-function(n=10){
  Variable_names<-names(DATA)
  Variable_names<-Variable_names[!Variable_names %in% c("SEQN","MORTSTAT","PERMTH_INT","AGE_CONTINIOUS","AGE_DEATH_CENSORED","AGE_GROUP","GENDER","SMOKING","MENOPAUSAL_STATUS","HORMONE_THERAPY_USE","PARITY","ORAL_CONTRACEPTIVE_USE","TOTAL_ENERGY","UNPROCESSED_RED_MEAT_STANDARD_CONTINOUS","UNPROCESSED_RED_MEAT_STANDARD_QUARTILE","UNPROCESSED_RED_MEAT_STANDARD_QUINTILES","UNPROCESSED_RED_MEAT_DENSITY_CONTINOUS","UNPROCESSED_RED_MEAT_DENSITY_QUARTILE","UNPROCESSED_RED_MEAT_DENSITY_QUINTILES","UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_2nd","UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_3rd","UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_4th",
                                                        "UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_2nd","UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_3rd","UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_4th","UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_5th",
                                                        "UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_2nd","UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_3rd","UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_4th",
                                                        "UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_2nd","UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_3rd","UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_4th","UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_5th")]
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

adjusting_variables<-adjusting_variable_generator(n=20)



###############################################################################################
#standard model plus continuous meat
cox_no_interaction_standard_continous<-function(formula,data){
  if(sum(data$GENDER=="Female")==nrow(data)){
    coxph(formula=as.formula(paste(formula,"+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data)}
  else{
    coxph(formula=as.formula(paste(formula,"+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY")),data=data)}
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

results_standard_continous <- run_specs(df = DATA,
                      y = c("Surv(PERMTH_INT,MORTSTAT)"),
                      x = c("UNPROCESSED_RED_MEAT_STANDARD_CONTINOUS"), 
                      model = c("cox_no_interaction_standard_continous"),
                      controls = adjusting_variables[1],
                      subsets = list(GENDER = unique(DATA$GENDER),
                                     AGE_GROUP = unique(DATA$AGE_GROUP)))
results_standard_continous<-results_standard_continous[results_standard_continous$controls!="no covariates",]
for(i in 1:nrow(results_standard_continous)){
  results_standard_continous$controls[i]<-adjusting_variables[[2]][which(adjusting_variables[[1]]==results_standard_continous$controls[i])]
}
results_standard_continous$estimate<-exp(results_standard_continous$estimate*100)
results_standard_continous$conf.low<-exp(results_standard_continous$conf.low*100)
results_standard_continous$conf.high<-exp(results_standard_continous$conf.high*100)
#explore_warnings<-results_standard_continous[results_standard_continous$estimate>=10|results_standard_continous$estimate<=0.1,]

results_standard_continous<-results_standard_continous[results_standard_continous$conf.low>=0.2&results_standard_continous$conf.high<=5,]
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
cox_no_interaction_density_continous<-function(formula,data){
  if(sum(data$GENDER=="Female")==nrow(data)){
    coxph(formula=as.formula(paste(formula,"+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data)}
  else{
    coxph(formula=as.formula(paste(formula,"+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY")),data=data)}
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
results_density_continous$estimate<-exp(results_density_continous$estimate*100/2000)
results_density_continous$conf.low<-exp(results_density_continous$conf.low*100/2000)
results_density_continous$conf.high<-exp(results_density_continous$conf.high*100/2000)

results_density_continous<-results_density_continous[results_density_continous$conf.low>=0.2&results_density_continous$conf.high<=5,]
#results_density_continous<-results_density_continous[results_density_continous$std.error<=1,]
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
cox_no_interaction_standard_quartile<-function(formula,data){
  if(sum(data$GENDER=="Female")==nrow(data)){
    coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_2nd+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_3rd+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data)}
  else{
    coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_2nd+UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_3rd+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY")),data=data)}
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
cox_no_interaction_standard_quintile<-function(formula,data){
  if(sum(data$GENDER=="Female")==nrow(data)){
    coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_2nd+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_3rd+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_4th+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data)}
  else{
    coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_2nd+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_3rd+UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_4th+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY")),data=data)}
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
cox_no_interaction_density_quartile<-function(formula,data){
  if(sum(data$GENDER=="Female")==nrow(data)){
    coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_2nd+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_3rd+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data)}
  else{
    coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_2nd+UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_3rd+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY")),data=data)}
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
cox_no_interaction_density_quintile<-function(formula,data){
  if(sum(data$GENDER=="Female")==nrow(data)){
    coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_2nd+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_3rd+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_4th+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE")),data=data)}
  else{
    coxph(formula=as.formula(paste(formula,"+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_2nd+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_3rd+UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_4th+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY")),data=data)}
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

combined_results<-rbind(results_standard_continous, results_standard_quartile,results_standard_quintile,results_density_continous,results_density_quartile,results_density_quintile)
#combined_results<-rbind(results_standard_continous,results_standard_quartile)
combined_results$x[grepl('CONTINOUS', combined_results$x)]<-"Continuous"
combined_results$x[grepl('QUARTILE', combined_results$x)]<-"Quartile"
combined_results$x[grepl('QUINTILE', combined_results$x)]<-"Quintile"
combined_results<-combined_results%>%rename(MeatType=x,Model=Analytical_model,AdjustingVariables=controls,SexGroup=GENDER,AgeGroup=AGE_GROUP)


p1<-plot_curve(combined_results,ci=TRUE,null=1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey") +
  ylim(0, 5) +
  labs(x = "", y = "Hazard Ratio")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(text = element_text(size = 20))

p2<-plot_choices(combined_results,choices = c("Model","MeatType","AdjustingVariables","SexGroup","AgeGroup"),null=1)+
  labs(x = "Specifications")+ theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+theme(text = element_text(size = 20))

#results<-results %>% mutate(DMDMARTL = ifelse(grepl("DMDMARTL",controls),"DMDMARTL","No"))

plot_specs(plot_a=p1,plot_b=p2,rel_heights = c(1, 2),null=1)








#subset<-DATA[DATA$GENDER=="Female",]
#subset_2<-DATA[DATA$GENDER=="Male",]
#subset_3<-DATA[DATA$AGE_GROUP=="60-69 years old",]
####test
#summary(coxph(Surv(PERMTH_INT,MORTSTAT)~UNPROCESSED_RED_MEAT_+GENDER,data=subset))
#summary(coxph(Surv(PERMTH_INT,MORTSTAT)~UNPROCESSED_RED_MEAT_STANDARD_QUARTILE,data=DATA))
#summary(coxph(Surv(PERMTH_INT,MORTSTAT)~UNPROCESSED_RED_MEAT_STANDARD_CONTINOUS+MENOPAUSAL_STATUS,data=DATA))

#summary(coxph(Surv(PERMTH_INT,MORSTAT)~UNPROCESSED_RED_MEAT_STANDARD_CONTINOUS+AGE_CONTINIOUS+GENDER+SMOKING+TOTAL_ENERGY+MENOPAUSAL_STATUS+HORMONE_THERAPY_USE+PARITY+ORAL_CONTRACEPTIVE_USE+EDUCATION+ALCOHOL_GROUP+SLEEP+SOCIOECONOMIC_STATUS+BMI_GROUP+GENERAL_HEALTH_CONDITION+HISTORY_OF_HYPERCHOLESTEROLEMIA+HISTORY_OF_DIABETES+HISTORY_OF_DEPRESSION+ASPIRIN+STATIN+VALSARTAN+ON_SPECIAL_DIET+PROCESSED_MEAT+POULTRY+FRUITS+VEGETABLES+WHOLE_GRAIN+EGGS+LEGUMES+CARBOHYDRATES+SATURATED_FAT+POLYUNSATURATED_FATTY_ACID+CHOLESTEROL+MAGNESIUM,data=subset))