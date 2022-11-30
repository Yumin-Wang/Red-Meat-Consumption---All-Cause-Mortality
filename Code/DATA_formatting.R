#load required packages
library(dplyr)
#load pre cleaned data
DATA<-read.csv("DATA/Combined DATA/DATA.csv")
#Format variables
#For age, we create 3 age groups: 20-39 years old, 40-59 years old, 60-79 years old.
DATA$AGE_GROUP <- factor(DATA$AGE_GROUP)
#For gender, we create male and female group.
DATA$RIAGENDR <- factor(DATA$RIAGENDR, levels=c(2,1),labels=c("Female","Male"))
#For education, we create 5 groups: "Less Than 9th Grade", "9-11th Grade (Includes 12th grade with no diploma)", "High School Grad/GED or Equivalent", "Some College or AA degree", "College Graduate or above". 
DATA$DMDEDUC2 <- factor(DATA$DMDEDUC2, levels=c(1,2,3,4,5), labels=c("Less Than 9th Grade","9-11th Grade (Includes 12th grade with no diploma)",
                                                                     "High School Grad/GED or Equivalent","Some College or AA degree","College Graduate or above"))
#For race, we create 5 groups: "Mexican American", "Other Hispanic", "Non-Hispanic White", "Non-Hispanic Black", "Other Race - Including Multi-Racial"
DATA$RIDRETH1 <- factor(DATA$RIDRETH1,levels=c(1,2,3,4,5),labels=c("Mexican American","Other Hispanic","Non-Hispanic White","Non-Hispanic Black","Other Race - Including Multi-Racial"))

#For marital status, we create 6 groups: "Never married", "Married", "Widowed", "Divorced", "Separated", "Living with partner"
DATA$DMDMARTL <- factor(DATA$DMDMARTL,levels=c(5,1,2,3,4,6),labels=c("Never married","Married","Widowed","Divorced","Separated","Living with partner"))

#For annual family income, create 4 categories: $0-$14,999; $15000-$34999; $35000-$64999; Over $65000.
DATA$INDFMIN2<-factor(DATA$INDFMIN2,levels=c(1,2,3,4),labels=c("$ 0 to $14,999","$15,000 to $34,999","$35,000 to $64,999","$65,000 to $100,000 and Over"))
#For socioeconomic status, create 5 categories: "Low", "Lower-middle","Middle","Upper-middle","High"
DATA$INDFMPIR <- factor(DATA$INDFMPIR,levels=c(1,2,3,4,5),labels=c("Low", "Lower-middle","Middle","Upper-middle","High"))
#For systolic blood pressure, create 5 categories: "Low", "Lower-middle","Middle","Upper-middle","High"
DATA$BPXSY<-factor(DATA$BPXSY,levels=c(1,2,3,4,5),labels=c("Low", "Lower-middle","Middle","Upper-middle","High"))
#For years of entering cohort, create 4 categories:"2007-2008","2009-2010","2011-2012","2013-2014"
DATA$SDDSRVYR <- factor(DATA$SDDSRVYR,levels=c(5,6,7,8),labels=c("2007-2008","2009-2010","2011-2012","2013-2014"))

#For history of diabetes, history of hypercholesterolemia, history of hypertension, history of depression, history of cardiovascular disease, history of cancer or malignancy, family history of diabetes, family history of myocardial infraction, category each of each as 2 groups: Yes or No
DATA$DIQ010 <- factor(DATA$DIQ010,levels=c(2,1),labels=c("No","Yes"))
DATA$BPQ080 <- factor(DATA$BPQ080,levels=c(2,1),labels=c("No","Yes"))
DATA$BPQ020 <- factor(DATA$BPQ020,levels=c(2,1),labels=c("No","Yes"))
DATA$DPQ020<-factor(DATA$DPQ020,levels=c(0,1),labels=c("No","Yes"))
DATA$CARDIOVASCULAR <- factor(DATA$CARDIOVASCULAR,levels=c(2,1),labels=c("No","Yes"))
DATA$MCQ220 <- factor(DATA$MCQ220,levels=c(2,1),labels=c("No","Yes"))
DATA$MCQ300C <- factor(DATA$MCQ300C,levels=c(2,1),labels=c("No","Yes"))
DATA$MCQ300A <- factor(DATA$MCQ300A,levels=c(2,1),labels=c("No","Yes"))

#Categorize menopausal status as 2 groups: premenopausal, postmenopausal
DATA$MENOPAUSAL <- factor(DATA$MENOPAUSAL,levels=c(0,1),labels=c("Premenopausal","Postmenopausal"))
#categorize hormone therapy use as Yes or No
DATA$RHQ540 <- factor(DATA$RHQ540,levels=c(2,1),labels=c("No","Yes"))
#categorize parity as two groups: Nulliparous and Parous
DATA$RHQ131 <- factor(DATA$RHQ131, levels=c(2,1),labels=c("Nulliparous","Parous"))
#categorize oral contraceptive use as two groups: Yes or No
DATA$RHQ420 <- factor(DATA$RHQ420, levels=c(2,1),labels=c("No","Yes"))

#For use of aspirin, ibuprofen, opium, statin, valsartan, categorize two groups, Yes or No
DATA$ASPIRIN <- factor(DATA$ASPIRIN, levels=c(0,1),labels=c("No","Yes"))
DATA$IBUPROFEN <- factor(DATA$IBUPROFEN, levels=c(0,1),labels=c("No","Yes"))
DATA$OPIUM <- factor(DATA$OPIUM, levels=c(0,1),labels=c("No","Yes"))
DATA$STATIN <- factor(DATA$STATIN, levels=c(0,1),labels=c("No","Yes"))
DATA$VALSARTAN <- factor(DATA$VALSARTAN, levels=c(0,1),labels=c("No","Yes"))

#For on special diet or not, categorize as Yes or No
DATA$DRQSDIET <- factor(DATA$DRQSDIET, levels=c(2,1),labels=c("No","Yes"))
#For dietary supplement intake, categorize as Yes or No
DATA$DSDS <- factor(DATA$DSDS, levels=c(2,1),labels=c("No","Yes"))
#For sleep,Categorize sleep as 3 groups: <=4 hours, 5-8 hours, >=9 hours.
DATA$SLD010H <- factor(DATA$SLD010H)
#For occupation, categorize it as 3 groups:"Non-worker","Part time worker","Full time worker"
DATA$OCQ180<-factor(DATA$OCQ180,levels=c(0,1,2),labels=c("Non-worker","Part time worker","Full time worker"))
#For physical activity, categorize it as 2 groups: "No activity","Vigorous or moderate activity"
DATA$ACTIVITY <- factor(DATA$ACTIVITY, levels=c(2,1),labels=c("No activity","Vigorous or moderate activity"))
#For smoking, categorize as 3 groups: "Non or light smoker","Moderate smoker","Heavy smoker"
DATA$SMOKING<-factor(DATA$SMOKING,levels=c(0,1,2),labels=c("Non or light smoker","Moderate smoker","Heavy smoker"))
#For alcohol drinking, categorize as 3 groups:"Non-drinker","<2 drinks per day",">=2 drinks per day"
DATA$ALCOHOL_GROUP <- factor(DATA$ALCOHOL_GROUP,levels=c("Non-drinker","<2 drinks per day",">=2 drinks per day"))
#For BMI group, categorize as 4 groups:Underweight (<18.5), Healthy weight (18.5<=<25), Overweight (25<=<30), Obesity (>=30), based on definition of CDC.
DATA$BMI_GROUP <- factor(DATA$BMI_GROUP)
#For sedentary lifestyle, categorize as 5 groups:"Low", "Lower-middle","Middle","Upper-middle","High"
DATA$PAD680 <- factor(DATA$PAD680,levels=c(1,2,3,4,5),labels=c("Low", "Lower-middle","Middle","Upper-middle","High"))
#For general health conditions, categorize as 5 groups:"Poor","Excellent","Very good","Good","Fair"
DATA$HSD010<-factor(DATA$HSD010,levels=c(5,1,2,3,4),labels=c("Poor","Excellent","Very good","Good","Fair"))
#For standard meat and density meat, categorize as 4 quartiles and 5 quintiles
DATA$PF_MEAT_STANDARD_QUARTILE<-factor(DATA$PF_MEAT_STANDARD_QUARTILE,levels=c(1,2,3,4),labels=c("1st", "2nd","3rd","4th"))
DATA$PF_MEAT_STANDARD_QUINTILES<-factor(DATA$PF_MEAT_STANDARD_QUINTILES,levels=c(1,2,3,4,5),labels=c("1st", "2nd","3rd","4th","5th"))
DATA$PF_MEAT_DENSITY_QUARTILE<-factor(DATA$PF_MEAT_DENSITY_QUARTILE,levels=c(1,2,3,4),labels=c("1st", "2nd","3rd","4th"))
DATA$PF_MEAT_DENSITY_QUINTILES<-factor(DATA$PF_MEAT_DENSITY_QUINTILES,levels=c(1,2,3,4,5),labels=c("1st", "2nd","3rd","4th","5th"))

#rename column
DATA <- DATA %>%rename(COHORT_YEAR=SDDSRVYR,AGE_CONTINIOUS=RIDAGEYR,GENDER=RIAGENDR,RACE_ETHNICITY=RIDRETH1,EDUCATION=DMDEDUC2,MARTIAL_STATUS=DMDMARTL,
                       ALCOHOL_CONTINOUS=ALQ130,OCCUPATION=OCQ180,SEDENTARY_LIFESTYLE=PAD680,SLEEP=SLD010H,FAMILY_INCOME=INDFMIN2,SOCIOECONOMIC_STATUS=INDFMPIR,BMI_CONTINOUS=BMXBMI,
                       SYSTOLIC_BLOOD_PRESSURE=BPXSY,GENERAL_HEALTH_CONDITION=HSD010,HISTORY_OF_HYPERCHOLESTEROLEMIA=BPQ080,HISTORY_OF_HYPERTENSION=BPQ020,HISTORY_OF_DIABETES=DIQ010,HISTORY_OF_DEPRESSION=DPQ020,
                       HISTORY_OF_CARDIOVASCULAR_DISEASE=CARDIOVASCULAR,HISTORY_OF_CANCER_OR_MALIGNANCY=MCQ220,FAMILY_HISTORY_OF_DIABETES=MCQ300C,FAMILY_HISTORY_OF_MYOCARDIAL_INFRACTION=MCQ300A,MENOPAUSAL_STATUS=MENOPAUSAL,HORMONE_THERAPY_USE=RHQ540,
                       PARITY=RHQ131,ORAL_CONTRACEPTIVE_USE=RHQ420, ON_SPECIAL_DIET=DRQSDIET,DIETARY_SUPPLEMENT=DSDS,PROCESSED_MEAT=PF_CUREDMEAT,UNPROCESSED_RED_MEAT_STANDARD_CONTINOUS=PF_MEAT,UNPROCESSED_RED_MEAT_STANDARD_QUARTILE=PF_MEAT_STANDARD_QUARTILE,UNPROCESSED_RED_MEAT_STANDARD_QUINTILES=PF_MEAT_STANDARD_QUINTILES,UNPROCESSED_RED_MEAT_DENSITY_CONTINOUS=PF_MEAT_DENSITY_CONTINOUS,UNPROCESSED_RED_MEAT_DENSITY_QUARTILE=PF_MEAT_DENSITY_QUARTILE,UNPROCESSED_RED_MEAT_DENSITY_QUINTILES=PF_MEAT_DENSITY_QUINTILES,POULTRY=PF_POULT, FRUITS=F_FRUIT,VEGETABLES=V_TOTAL, SEAFOOD=PF_SEAFD,
                       WHOLE_GRAIN=G_WHOLE,EGGS=PF_EGGS,NUTS_SEEDS=PF_NUTSDS,LEGUMES=PF_LEGUMES,TOTAL_DAIRY=D_TOTAL,TOTAL_ENERGY=TKCAL,CARBOHYDRATES=TCARB,DIETARY_FIBER=TFIBE,SATURATED_FAT=TSFAT,MONOUNSATURATED_FATTY_ACID=TMFAT,POLYUNSATURATED_FATTY_ACID=TPFAT,CHOLESTEROL=TCHOL,MAGNESIUM=TMAGN)              










