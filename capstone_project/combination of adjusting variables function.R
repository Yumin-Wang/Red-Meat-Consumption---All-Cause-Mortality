source("DATA/Combined DATA/DATA_formatting.r")
library(tidyverse)
Variable_names<-names(DATA)
Variable_names<-Variable_names[!Variable_names %in% c("SEQN","MORTSTAT","PERMTH_INT","AGE_CONTINIOUS","AGE_DEATH_CENSORED","AGE_GROUP","GENDER","SMOKING","MENOPAUSAL_STATUS","HORMONE_THERAPY_USE","PARITY","ORAL_CONTRACEPTIVE_USE","TOTAL_ENERGY","UNPROCESSED_RED_MEAT")]

adjusting_variable_generator<-function(n=10){
  Variable_names<-names(DATA)
  Variable_names<-Variable_names[!Variable_names %in% c("SEQN","MORTSTAT","PERMTH_INT","AGE_CONTINIOUS","AGE_DEATH_CENSORED","AGE_GROUP","GENDER","SMOKING","MENOPAUSAL_STATUS","HORMONE_THERAPY_USE","PARITY","ORAL_CONTRACEPTIVE_USE","TOTAL_ENERGY","UNPROCESSED_RED_MEAT")]
  vec = vector(length = n)
  for (i in 1:n){
    include<-rbinom(n=length(Variable_names), size=1, prob=0.5)
    sampled_variables<-Variable_names[include==1]
    vec[i]<-paste(sampled_variables,collapse="+")
    
    
  }
  return (vec)
}
