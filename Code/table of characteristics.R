#load the data
source("DATA_formatting.r")
#get all variable names in analytical data
Variable_names<-names(DATA)
#get variables names that we will examine characteristics
Variable_names_1_48<-Variable_names[!Variable_names %in% c("SEQN","MORTSTAT","PERMTH_INT","AGE_CONTINIOUS","AGE_DEATH_CENSORED","AGE_GROUP","GENDER","SMOKING","MENOPAUSAL_STATUS","HORMONE_THERAPY_USE","PARITY","ORAL_CONTRACEPTIVE_USE","TOTAL_ENERGY","UNPROCESSED_RED_MEAT_STANDARD_CONTINOUS","UNPROCESSED_RED_MEAT_STANDARD_QUARTILE","UNPROCESSED_RED_MEAT_STANDARD_QUINTILES","UNPROCESSED_RED_MEAT_DENSITY_CONTINOUS","UNPROCESSED_RED_MEAT_DENSITY_QUARTILE","UNPROCESSED_RED_MEAT_DENSITY_QUINTILES","UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_2nd","UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_3rd","UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_4th",
                                                      "UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_2nd","UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_3rd","UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_4th","UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_5th",
                                                      "UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_2nd","UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_3rd","UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_4th",
                                                      "UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_2nd","UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_3rd","UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_4th","UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_5th")]

Variable_names_all<-Variable_names[!Variable_names %in% c("SEQN","AGE_DEATH_CENSORED","UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_2nd","UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_3rd","UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_4th",
                                                          "UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_2nd","UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_3rd","UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_4th","UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_5th",
                                                          "UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_2nd","UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_3rd","UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_4th",
                                                          "UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_2nd","UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_3rd","UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_4th","UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_5th")]
Variable_names_diff<-setdiff(Variable_names_all, Variable_names_1_48)

table_1_48<-DATA[,Variable_names_1_48]
table_upper<-DATA[,Variable_names_diff]

round(table(table_1_48$COHORT_YEAR)*100/nrow(table_1_48))
round(table(table_1_48$RACE_ETHNICITY)*100/nrow(table_1_48))
round(table(table_1_48$EDUCATION)*100/nrow(table_1_48))
round(table(table_1_48$MARTIAL_STATUS)*100/nrow(table_1_48))
round(mean(table_1_48$ALCOHOL_CONTINOUS))

values<-numeric()
counts<-numeric()
names<-numeric()
for (i in 1:dim(table_1_48)[2]){
  if(class(table_1_48[,i])=="factor"){
    values<-c(values,"",round(table(table_1_48[,i])*100/nrow(table_1_48)))
    counts<-c(counts,"",table(table_1_48[,i]))
    names<-c(names,names(table_1_48)[i],names(table(table_1_48[,i])))
  }
  if(class(table_1_48[,i])=="integer"|class(table_1_48[,i])=="numeric"){
    if(ks.test(table_1_48[,i],"pnorm")$p.value>=0.05){
      mu<-round(mean(table_1_48[,i]),1)
      sd<-round(sd(table_1_48[,i]),1)
      values<-c(values,paste(mu,"(",sd,")"))
    }
    else{
      med<-round(median(table_1_48[,i]),1)
      p10<-round(quantile(table_1_48[,i],prob=0.1),1)
      p90<-round(quantile(table_1_48[,i],prob=0.9),1)
      values<-c(values,paste(med,"(",p10,",",p90,")"))
    }
    counts<-c(counts,"")
    names<-c(names,names(table_1_48)[i])
  }
}
characteristic<-as.data.frame(values)
characteristic$var<-names
characteristic$counts<-counts
characteristic<-characteristic[,c(2,1,3)]
write.csv(characteristic,"characteristic.csv", row.names = FALSE)

values<-numeric()
counts<-numeric()
names<-numeric()
for (i in 1:dim(table_upper)[2]){
  if(class(table_upper[,i])=="factor"){
    values<-c(values,"",round(table(table_upper[,i])*100/nrow(table_upper)))
    counts<-c(counts,"",table(table_upper[,i]))
    names<-c(names,names(table_upper)[i],names(table(table_upper[,i])))
  }
  if(class(table_upper[,i])=="integer"|class(table_upper[,i])=="numeric"){
    if(ks.test(table_upper[,i],"pnorm")$p.value>=0.05){
      mu<-round(mean(table_upper[,i]),1)
      sd<-round(sd(table_upper[,i]),1)
      values<-c(values,paste(mu,"(",sd,")"))
    }
    else{
      med<-round(median(table_upper[,i]),1)
      p10<-round(quantile(table_upper[,i],prob=0.1),1)
      p90<-round(quantile(table_upper[,i],prob=0.9),1)
      values<-c(values,paste(med,"(",p10,",",p90,")"))
    }
    counts<-c(counts,"")
    names<-c(names,names(table_upper)[i])
  }
}
characteristic_upper<-as.data.frame(values)
characteristic_upper$var<-names
characteristic_upper$counts<-counts
characteristic_upper<-characteristic_upper[,c(2,1,3)]
write.csv(characteristic_upper,"characteristic_upper.csv", row.names = FALSE)






