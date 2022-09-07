source("DATA/Combined DATA/DATA_formatting.r")
#install.packages("devtools")
#devtools::install_github("masurp/specr")
library(survival)
library(tidyverse)
library(specr)


cox<-function(formula,data){
  coxph(formula=as.formula(formula),data=data,ties="breslow")
}

Variable_names<-names(DATA)
Variable_names<-Variable_names[!Variable_names %in% c("SEQN","MORTSTAT","PERMTH_INT","AGE_CONTINIOUS","AGE_DEATH_CENSORED","AGE_GROUP","GENDER","SMOKING","MENOPAUSAL_STATUS","HORMONE_THERAPY_USE","PARITY","ORAL_CONTRACEPTIVE_USE","TOTAL_ENERGY","UNPROCESSED_RED_MEAT")]
#Variable_names<-Variable_names[50:54]
adjusting_variable_generator<-function(n=10){
  variable_combination = vector(length = n)
  variable_index = vector(length=n)
  for (i in 1:n){
    include<-rbinom(n=length(Variable_names), size=1, prob=0.5)
    sampled_variables<-Variable_names[include==1]
    variable_combination[i]<-paste(sampled_variables,collapse="+")
    variable_index[i]<-paste(which(include==1),collapse=",")
    
  }
  return (list(variable_combination,variable_index))
}

adjusting_variables<-adjusting_variable_generator(n=10)

results <- run_specs(df = DATA,
                      y = c("Surv(PERMTH_INT,MORTSTAT)"),
                      x = c("UNPROCESSED_RED_MEAT"), 
                      model = c("cox"),
                      controls = adjusting_variables[1],
                      subsets = list(GENDER = unique(DATA$GENDER),
                                     AGE_GROUP = unique(DATA$AGE_GROUP)))
results<-results[results$controls!="no covariates",]

for(i in 1:nrow(results)){
  results$controls[i]<-adjusting_variables[[2]][which(adjusting_variables[[1]]==results$controls[i])]
  
  
  
  
  
  
}


results$estimate<-exp(results$estimate*100)
results<-results[results$estimate<=2&results$estimate>0.2,]
results<-results %>% mutate(GENDER = case_when (grepl("GENDER = Female",subsets)~"FEMALE",
                               grepl("GENDER = Male",subsets)~"MALE",!grepl("GENDER = Female",subsets)&!grepl("GENDER = Male",subsets)~"All sex"))
results<-results %>% mutate(AGE_GROUP = case_when (grepl("AGE_GROUP = 20-29 years old",subsets)~"AGE_GROUP = 20-29 years old",
                                                   grepl("AGE_GROUP = 30-39 years old",subsets)~"AGE_GROUP = 30-39 years old",
                                                   grepl("AGE_GROUP = 40-49 years old",subsets)~"AGE_GROUP = 40-49 years old",
                                                   grepl("AGE_GROUP = 50-59 years old",subsets)~"AGE_GROUP = 50-59 years old",
                                                   grepl("AGE_GROUP = 60-69 years old",subsets)~"AGE_GROUP = 60-69 years old",
                                                   grepl("AGE_GROUP = 70-79 years old",subsets)~"AGE_GROUP = 70-79 years old",
                                                   !grepl("AGE_GROUP = 20-29 years old",subsets)&!grepl("AGE_GROUP = 30-39 years old",subsets)&
                                                     !grepl("AGE_GROUP = 40-49 years old",subsets)&!grepl("AGE_GROUP = 50-59 years old",subsets)&
                                                     !grepl("AGE_GROUP = 60-69 years old",subsets)&!grepl("AGE_GROUP = 70-79 years old",subsets)~"All age"))                             
#results<-results %>% mutate(DMDMARTL = ifelse(grepl("DMDMARTL",controls),"DMDMARTL","No"))


plot_specs(results, choices = c("x","y","controls","GENDER","AGE_GROUP"))



