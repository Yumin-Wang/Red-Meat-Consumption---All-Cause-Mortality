#need combined_results from SCA.r
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
bene_non_significant<-combined_results[combined_results$estimate<1&combined_results$p.value>0.05&combined_results$Model=="Multivariable Nutrient Density Model"&combined_results$MeatType=="Quartile"&combined_results$SexGroup=="Male",][c(25),]
harm_non_significant<-combined_results[combined_results$estimate>1&combined_results$p.value>0.05,][c(100,189),]

# for (i in 1:5){
#   test.shoen_5<-ph_assumption(bene_non_significant,row=i)
#   if(test.shoen_5$table["GLOBAL",][3]>=0.05){
#     print(i)
#   }
#   
# }

test.shoen_1<-ph_assumption(bene_significant,row=1)
ggcoxzph(test.shoen_1,ggtheme = theme_survminer(font.main = c(20, "plain", "black"),font.submain = c(20, "plain", "black"),font.x = c(20, "plain", "black"),
                                                font.y = c(20, "plain", "black"),font.caption = c(20, "plain", "black"),font.tickslab = c(20, "plain", "black")))
test.shoen_2<-ph_assumption(bene_significant,row=2)
ggcoxzph(test.shoen_2,ggtheme = theme_survminer(font.main = c(20, "plain", "black"),font.submain = c(20, "plain", "black"),font.x = c(20, "plain", "black"),
                                                font.y = c(20, "plain", "black"),font.caption = c(20, "plain", "black"),font.tickslab = c(20, "plain", "black")))
test.shoen_3<-ph_assumption(bene_non_significant,row=1)
ggcoxzph(test.shoen_3,ggtheme = theme_survminer(font.main = c(20, "plain", "black"),font.submain = c(20, "plain", "black"),font.x = c(20, "plain", "black"),
                                                font.y = c(20, "plain", "black"),font.caption = c(20, "plain", "black"),font.tickslab = c(20, "plain", "black")))
test.shoen_4<-ph_assumption(harm_non_significant,row=1)
ggcoxzph(test.shoen_4,ggtheme = theme_survminer(font.main = c(20, "plain", "black"),font.submain = c(20, "plain", "black"),font.x = c(20, "plain", "black"),
                                                font.y = c(20, "plain", "black"),font.caption = c(20, "plain", "black"),font.tickslab = c(20, "plain", "black")))
test.shoen_5<-ph_assumption(harm_non_significant,row=2)
ggcoxzph(test.shoen_5,ggtheme = theme_survminer(font.main = c(20, "plain", "black"),font.submain = c(20, "plain", "black"),font.x = c(20, "plain", "black"),
                                                font.y = c(20, "plain", "black"),font.caption = c(20, "plain", "black"),font.tickslab = c(20, "plain", "black")))

#write to a csv
write.csv(test.shoen_1$table,"./bene_1.csv", row.names = TRUE)
#write to a csv
write.csv(test.shoen_2$table,"./bene_2.csv", row.names = TRUE)
#write to a csv
write.csv(test.shoen_3$table,"./bene_3.csv", row.names = TRUE)
#write to a csv
write.csv(test.shoen_4$table,"./harm_1.csv", row.names = TRUE)
#write to a csv
write.csv(test.shoen_5$table,"./harm_2.csv", row.names = TRUE)


