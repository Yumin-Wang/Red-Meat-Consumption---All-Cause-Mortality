source("DATA/Combined DATA/DATA_formatting.r")
install.packages("devtools")
devtools::install_github("masurp/specr")
library(survival)
library(tidyverse)
library(specr)


cox<-function(formula,data){
  coxph(formula=as.formula(formula),data=data,ties="breslow")
}
results <- run_specs(df = DATA,
                      y = c("Surv(PERMTH_INT,MORTSTAT)"),
                      x = c("PF_MEAT"), 
                      model = c("cox"),
                      controls = c("DMDMARTL","DMDEDUC2","ACTIVITY"),all.comb=TRUE,
                      subsets = list(RIAGENDR = unique(DATA$RIAGENDR),
                                     AGE_GROUP = unique(DATA$AGE_GROUP)))
results$estimate<-exp(results$estimate*100)
results<-results %>% mutate(RIAGENDR = case_when (grepl("RIAGENDR = Female",subsets)~"FEMALE",
                               grepl("RIAGENDR = Male",subsets)~"MALE",!grepl("RIAGENDR = Female",subsets)&!grepl("RIAGENDR = Male",subsets)~"All sex"))
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


plot_specs(results, choices = c("x","y","controls","RIAGENDR","AGE_GROUP"))



