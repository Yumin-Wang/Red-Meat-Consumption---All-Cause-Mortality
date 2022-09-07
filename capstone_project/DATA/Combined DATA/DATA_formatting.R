library(dplyr)

DATA<-read.csv("DATA/Combined DATA/DATA.csv")
#Format
#model  <- lm(TKCAL ~ RIDAGEYR+DMDEDUC2, data = DATA)
DATA$AGE_GROUP <- factor(DATA$AGE_GROUP)
DATA$RIAGENDR <- factor(DATA$RIAGENDR, levels=c(2,1),labels=c("Female","Male"))
DATA$DMDEDUC2 <- factor(DATA$DMDEDUC2, levels=c(1,2,3,4,5), labels=c("Less Than 9th Grade","9-11th Grade (Includes 12th grade with no diploma)",
                                                                     "High School Grad/GED or Equivalent","Some College or AA degree","College Graduate or above"))
DATA$RIDRETH1 <- factor(DATA$RIDRETH1,levels=c(1,2,3,4,5),labels=c("Mexican American","Other Hispanic","Non-Hispanic White","Non-Hispanic Black","Other Race - Including Multi-Racial"))

DATA$DMDMARTL <- factor(DATA$DMDMARTL,levels=c(5,1,2,3,4,6),labels=c("Never married","Married","Widowed","Divorced","Separated","Living with partner"))

DATA$INDFMIN2<-factor(DATA$INDFMIN2,levels=c(1,2,3,4),labels=c("$ 0 to $14,999","$15,000 to $34,999","$35,000 to $64,999","$65,000 to $100,000 and Over"))

DATA$INDFMPIR <- factor(DATA$INDFMPIR,levels=c(1,2,3,4,5),labels=c("Low", "Lower-middle","Middle","Upper-middle","High"))
DATA$BPXSY<-factor(DATA$BPXSY,levels=c(1,2,3,4,5),labels=c("Low", "Lower-middle","Middle","Upper-middle","High"))
DATA$SDDSRVYR <- factor(DATA$SDDSRVYR,levels=c(5,6,7,8),labels=c("2007-2008","2009-2010","2011-2012","2013-2014"))
DATA$DIQ010 <- factor(DATA$DIQ010,levels=c(2,1),labels=c("No","Yes"))
DATA$SLD010H <- factor(DATA$SLD010H)
DATA$BPQ080 <- factor(DATA$BPQ080,levels=c(2,1),labels=c("No","Yes"))
DATA$BPQ020 <- factor(DATA$BPQ020,levels=c(2,1),labels=c("No","Yes"))
DATA$DPQ020<-factor(DATA$DPQ020,levels=c(0,1),labels=c("No","Yes"))
DATA$HSD010<-factor(DATA$HSD010,levels=c(5,1,2,3,4),labels=c("Poor","Excellent","Very good","Good","Fair"))
DATA$CARDIOVASCULAR <- factor(DATA$CARDIOVASCULAR,levels=c(2,1),labels=c("No","Yes"))
DATA$MCQ220 <- factor(DATA$MCQ220,levels=c(2,1),labels=c("No","Yes"))
DATA$MCQ300C <- factor(DATA$MCQ300C,levels=c(2,1),labels=c("No","Yes"))
DATA$MCQ300A <- factor(DATA$MCQ300A,levels=c(2,1),labels=c("No","Yes"))
DATA$MENOPAUSAL <- factor(DATA$MENOPAUSAL,levels=c(0,1),labels=c("Premenopausal","Postmenopausal"))
DATA$RHQ540 <- factor(DATA$RHQ540,levels=c(2,1),labels=c("No","Yes"))
DATA$RHQ131 <- factor(DATA$RHQ131, levels=c(2,1),labels=c("Nulliparous","Parous"))
DATA$RHQ420 <- factor(DATA$RHQ420, levels=c(2,1),labels=c("No","Yes"))
#DATA$RHD143 <- factor(DATA$RHD143, levels=c(2,1),labels=c("No","Yes"))
DATA$ASPIRIN <- factor(DATA$ASPIRIN, levels=c(0,1),labels=c("No","Yes"))
DATA$IBUPROFEN <- factor(DATA$IBUPROFEN, levels=c(0,1),labels=c("No","Yes"))
DATA$OPIUM <- factor(DATA$OPIUM, levels=c(0,1),labels=c("No","Yes"))
DATA$STATIN <- factor(DATA$STATIN, levels=c(0,1),labels=c("No","Yes"))
DATA$VALSARTAN <- factor(DATA$VALSARTAN, levels=c(0,1),labels=c("No","Yes"))
DATA$DRQSDIET <- factor(DATA$DRQSDIET, levels=c(2,1),labels=c("No","Yes"))
DATA$DSDS <- factor(DATA$DSDS, levels=c(2,1),labels=c("No","Yes"))

DATA$OCQ180<-factor(DATA$OCQ180,levels=c(0,1,2),labels=c("Non-worker","Part time worker","Full time worker"))
DATA$ACTIVITY <- factor(DATA$ACTIVITY, levels=c(2,1),labels=c("No activity","Vigorous or moderate activity"))
DATA$SMOKING<-factor(DATA$SMOKING,levels=c(0,1,2),labels=c("Non or light smoker","Moderate smoker","Heavy smoker"))
DATA$ALCOHOL_GROUP <- factor(DATA$ALCOHOL_GROUP,levels=c("Non-drinker","<2 drinks per day",">=2 drinks per day"))
DATA$BMI_GROUP <- factor(DATA$BMI_GROUP)
DATA$PAD680 <- factor(DATA$PAD680,levels=c(1,2,3,4,5),labels=c("Low", "Lower-middle","Middle","Upper-middle","High"))

DATA <- DATA %>%rename(COHORT_YEAR=SDDSRVYR,AGE_CONTINIOUS=RIDAGEYR,GENDER=RIAGENDR,RACE_ETHNICITY=RIDRETH1,EDUCATION=DMDEDUC2,MARTIAL_STATUS=DMDMARTL,
                       ALCOHOL_CONTINOUS=ALQ130,OCCUPATION=OCQ180,SEDENTARY_LIFESTYLE=PAD680,SLEEP=SLD010H,FAMILY_INCOME=INDFMIN2,SOCIOECONOMIC_STATUS=INDFMPIR,BMI_CONTINOUS=BMXBMI,
                       SYSTOLIC_BLOOD_PRESSURE=BPXSY,GENERAL_HEALTH_CONDITION=HSD010,HISTORY_OF_HYPERCHOLESTEROLEMIA=BPQ080,HISTORY_OF_HYPERTENSION=BPQ020,HISTORY_OF_DIABETES=DIQ010,HISTORY_OF_DEPRESSION=DPQ020,
                       HISTORY_OF_CARDIOVASCULAR_DISEASE=CARDIOVASCULAR,HISTORY_OF_CANCER_OR_MALIGNANCY=MCQ220,FAMILY_HISTORY_OF_DIABETES=MCQ300C,FAMILY_HISTORY_OF_MYOCARDIAL_INFRACTION=MCQ300A,MENOPAUSAL_STATUS=MENOPAUSAL,HORMONE_THERAPY_USE=RHQ540,
                       PARITY=RHQ131,ORAL_CONTRACEPTIVE_USE=RHQ420, ON_SPECIAL_DIET=DRQSDIET,DIETARY_SUPPLEMENT=DSDS,PROCESSED_MEAT=PF_CUREDMEAT,UNPROCESSED_RED_MEAT=PF_MEAT,POULTRY=PF_POULT, FRUITS=F_FRUIT,VEGETABLES=V_TOTAL, SEAFOOD=PF_SEAFD,
                       WHOLE_GRAIN=G_WHOLE,EGGS=PF_EGGS,NUTS_SEEDS=PF_NUTSDS,LEGUMES=PF_LEGUMES,TOTAL_DAIRY=D_TOTAL,TOTAL_ENERGY=TKCAL,CARBOHYDRATES=TCARB,DIETARY_FIBER=TFIBE,SATURATED_FAT=TSFAT,MONOUNSATURATED_FATTY_ACID=TMFAT,POLYUNSATURATED_FATTY_ACID=TPFAT,CHOLESTEROL=TCHOL,MAGNESIUM=TMAGN)              










