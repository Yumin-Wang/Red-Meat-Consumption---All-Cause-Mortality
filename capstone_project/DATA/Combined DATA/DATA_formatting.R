library(dplyr)

DATA<-read.csv("DATA/Combined DATA/DATA.csv")
#Format
model  <- lm(TKCAL ~ RIDAGEYR+DMDEDUC2, data = DATA)

DATA$RIAGENDR <- factor(DATA$RIAGENDR, levels=c(2,1),labels=c("Female","Male"))
DATA$DMDEDUC2 <- factor(DATA$DMDEDUC2, levels=c(1,2,3,4,5), labels=c("Less Than 9th Grade","9-11th Grade (Includes 12th grade with no diploma)",
                                                                     "High School Grad/GED or Equivalent","Some College or AA degree","College Graduate or above"))
DATA$RIDRETH1 <- factor(DATA$RIDRETH1,levels=c(1,2,3,4,5),labels=c("Mexican American","Other Hispanic","Non-Hispanic White","Non-Hispanic Black","Other Race - Including Multi-Racial"))

DATA$DMDMARTL <- factor(DATA$DMDMARTL,levels=c(1,2,3,4,5,6),labels=c("Married","Widowed","Divorced","Separated","Never married","Living with partner"))

for (i in 1:nrow(DATA)){
  if(!is.na(DATA$INDFMIN2[i])){
  if (DATA$INDFMIN2[i]==1|DATA$INDFMIN2[i]==2|DATA$INDFMIN2[i]==3){
    DATA$INDFMIN2[i]<-1
  }
  if (DATA$INDFMIN2[i]==4|DATA$INDFMIN2[i]==5|DATA$INDFMIN2[i]==6){
    DATA$INDFMIN2[i]<-2
  }
  if (DATA$INDFMIN2[i]==7|DATA$INDFMIN2[i]==8|DATA$INDFMIN2[i]==9){
    DATA$INDFMIN2[i]<-3
  }
  if (DATA$INDFMIN2[i]==10|DATA$INDFMIN2[i]==14|DATA$INDFMIN2[i]==15){
    DATA$INDFMIN2[i]<-4
  }
  if (DATA$INDFMIN2[i]==12|DATA$INDFMIN2[i]==13){
    DATA$INDFMIN2[i]<-NA
  }
  }
}
DATA$INDFMIN2<-factor(DATA$INDFMIN2,levels=c(1,2,3,4),labels=c("$ 0 to $14,999","$15,000 to $34,999","$35,000 to $64,999","$65,000 to $100,000 and Over"))
DATA$INDFMPIR <- ntile(DATA$INDFMPIR, 5)  
DATA$INDFMPIR <- factor(DATA$INDFMPIR,levels=c(1,2,3,4,5),labels=c("Low", "Lower-middle","Middle","Upper-middle","High"))
DATA$SDDSRVYR <- factor(DATA$SDDSRVYR,levels=c(5,6,7,8),labels=c("2007-2008","2009-2010","2011-2012","2013-2014"))
DATA$BPQ080 <- factor(DATA$BPQ080,levels=c(2,1),labels=c("No","Yes"))
DATA$BPQ020 <- factor(DATA$BPQ020,levels=c(2,1),labels=c("No","Yes"))
for (i in 1:nrow(DATA)){
  if(!is.na(DATA$DPQ020[i])){
    if (DATA$DPQ020[i]==1|DATA$DPQ020[i]==2|DATA$DPQ020[i]==3){
      DATA$DPQ020[i]<-1
    }
  }
}
DATA$DPQ020<-factor(DATA$DPQ020,levels=c(0,1),labels=c("No","Yes"))
DATA$HSD010<-factor(DATA$HSD010,levels=c(1,2,3,4,5),labels=c("Excellent","Very good","Good","Fair","Poor"))
DATA$CARDIOVASCULAR <- factor(DATA$CARDIOVASCULAR,levels=c(2,1),labels=c("No","Yes"))
DATA$MCQ220 <- factor(DATA$MCQ220,levels=c(2,1),labels=c("No","Yes"))
DATA$MCQ300C <- factor(DATA$MCQ300C,levels=c(2,1),labels=c("No","Yes"))
DATA$MCQ300A <- factor(DATA$MCQ300A,levels=c(2,1),labels=c("No","Yes"))
DATA$MCQ160F <- factor(DATA$MCQ160F,levels=c(2,1),labels=c("No","Yes"))


