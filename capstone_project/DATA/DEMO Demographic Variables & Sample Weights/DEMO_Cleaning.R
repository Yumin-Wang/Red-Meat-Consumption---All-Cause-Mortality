#Install packages
require(SASxport)
library(dplyr)
library(readr)



#Read demographic
read_demo <- function(){
  DEMO_E <- read.xport("DATA/DEMO Demographic Variables & Sample Weights/DEMO_E.XPT")
  DEMO_F <- read.xport("DATA/DEMO Demographic Variables & Sample Weights/DEMO_F.XPT")
  DEMO_G <- read.xport("DATA/DEMO Demographic Variables & Sample Weights/DEMO_G.XPT")
  DEMO_H <- read.xport("DATA/DEMO Demographic Variables & Sample Weights/DEMO_H.XPT")
  DEMO_I <- read.xport("DATA/DEMO Demographic Variables & Sample Weights/DEMO_I.XPT")
  myvars <- c("SEQN","RIDAGEYR", "RIAGENDR", "DMDEDUC2", "RIDRETH1", "DMDMARTL", "INDFMIN2", "INDFMPIR", "SDDSRVYR", "SDMVPSU", "SDMVSTRA")
  DEMO_E <- DEMO_E[myvars]
  DEMO_F <- DEMO_F[myvars]
  DEMO_G <- DEMO_G[myvars]
  DEMO_H <- DEMO_H[myvars]
  DEMO_I <- DEMO_I[myvars]
  DEMO<-rbind(DEMO_E,DEMO_F,DEMO_G,DEMO_H,DEMO_I)
  remove(DEMO_E)
  remove(DEMO_F)
  remove(DEMO_G)
  remove(DEMO_H)
  remove(DEMO_I)
  return(DEMO)
}
DEMO <-read_demo()

#Read mortality
read_mortality<-function(){
  Mortality_E <- read_fwf(file=paste("DATA/Mortality/NHANES_2007_2008_MORT_2019_PUBLIC.dat"),
                  col_types = "iiiiiiii",
                  fwf_cols(seqn = c(1,6),
                           eligstat = c(15,15),
                           mortstat = c(16,16),
                           ucod_leading = c(17,19),
                           diabetes = c(20,20),
                           hyperten = c(21,21),
                           permth_int = c(43,45),
                           permth_exm = c(46,48)
                  ),
                  na = c("", ".")
  )
  
  Mortality_F <- read_fwf(file=paste("DATA/Mortality/NHANES_2009_2010_MORT_2019_PUBLIC.dat"),
                          col_types = "iiiiiiii",
                          fwf_cols(seqn = c(1,6),
                                   eligstat = c(15,15),
                                   mortstat = c(16,16),
                                   ucod_leading = c(17,19),
                                   diabetes = c(20,20),
                                   hyperten = c(21,21),
                                   permth_int = c(43,45),
                                   permth_exm = c(46,48)
                          ),
                          na = c("", ".")
  )
  
  Mortality_G <- read_fwf(file=paste("DATA/Mortality/NHANES_2011_2012_MORT_2019_PUBLIC.dat"),
                          col_types = "iiiiiiii",
                          fwf_cols(seqn = c(1,6),
                                   eligstat = c(15,15),
                                   mortstat = c(16,16),
                                   ucod_leading = c(17,19),
                                   diabetes = c(20,20),
                                   hyperten = c(21,21),
                                   permth_int = c(43,45),
                                   permth_exm = c(46,48)
                          ),
                          na = c("", ".")
  )
  
  Mortality_H <- read_fwf(file=paste("DATA/Mortality/NHANES_2013_2014_MORT_2019_PUBLIC.dat"),
                          col_types = "iiiiiiii",
                          fwf_cols(seqn = c(1,6),
                                   eligstat = c(15,15),
                                   mortstat = c(16,16),
                                   ucod_leading = c(17,19),
                                   diabetes = c(20,20),
                                   hyperten = c(21,21),
                                   permth_int = c(43,45),
                                   permth_exm = c(46,48)
                          ),
                          na = c("", ".")
  )
  
  Mortality_I <- read_fwf(file=paste("DATA/Mortality/NHANES_2015_2016_MORT_2019_PUBLIC.dat"),
                          col_types = "iiiiiiii",
                          fwf_cols(seqn = c(1,6),
                                   eligstat = c(15,15),
                                   mortstat = c(16,16),
                                   ucod_leading = c(17,19),
                                   diabetes = c(20,20),
                                   hyperten = c(21,21),
                                   permth_int = c(43,45),
                                   permth_exm = c(46,48)
                          ),
                          na = c("", ".")
  )
  
  myvars <- c("seqn","eligstat", "ucod_leading","mortstat", "permth_int")
  Mortality_E <- Mortality_E[myvars]
  Mortality_F <- Mortality_F[myvars]
  Mortality_G <- Mortality_G[myvars]
  Mortality_H <- Mortality_H[myvars]
  Mortality_I <- Mortality_I[myvars]
  Mortality<-rbind(Mortality_E,Mortality_F,Mortality_G,Mortality_H,Mortality_I)
  remove(Mortality_E)
  remove(Mortality_F)
  remove(Mortality_G)
  remove(Mortality_H)
  remove(Mortality_I)
  names(Mortality) <- toupper(names(Mortality))
  return(Mortality)
}

Mortality<-read_mortality()

DEMO_Mortality <- merge(DEMO,Mortality,by="SEQN")
remove(DEMO)
remove(Mortality)
#source("DATA/DEMO Demographic Variables & Sample Weights/DEMO_Cleaning.R")

#Read Alcohol
read_ALQ <- function(){
  ALQ_E <- read.xport("DATA/ALQ Alcohol Use/ALQ_E.XPT")
  ALQ_F <- read.xport("DATA/ALQ Alcohol Use/ALQ_F.XPT")
  ALQ_G <- read.xport("DATA/ALQ Alcohol Use/ALQ_G.XPT")
  ALQ_H <- read.xport("DATA/ALQ Alcohol Use/ALQ_H.XPT")
  ALQ_I <- read.xport("DATA/ALQ Alcohol Use/ALQ_I.XPT")
  myvars <- c("SEQN","ALQ130")
  ALQ_E <- ALQ_E[myvars]
  ALQ_F <- ALQ_F[myvars]
  ALQ_G <- ALQ_G[myvars]
  ALQ_H <- ALQ_H[myvars]
  ALQ_I <- ALQ_I[myvars]
  ALQ<-rbind(ALQ_E,ALQ_F,ALQ_G,ALQ_H,ALQ_I)
  remove(ALQ_E)
  remove(ALQ_F)
  remove(ALQ_G)
  remove(ALQ_H)
  remove(ALQ_I)
  return(ALQ)
}
ALQ <-read_ALQ()

DEMO_Mortality_ALQ <- merge(DEMO_Mortality,ALQ,by="SEQN",all.x=TRUE)
remove(ALQ)
remove(DEMO_Mortality)

#Read Body measures
read_BMX <- function(){
  BMX_E <- read.xport("DATA/BMX Body Measures/BMX_E.XPT")
  BMX_F <- read.xport("DATA/BMX Body Measures/BMX_F.XPT")
  BMX_G <- read.xport("DATA/BMX Body Measures/BMX_G.XPT")
  BMX_H <- read.xport("DATA/BMX Body Measures/BMX_H.XPT")
  BMX_I <- read.xport("DATA/BMX Body Measures/BMX_I.XPT")
  myvars <- c("SEQN","BMXBMI","BMXHT","BMXWT")
  BMX_E <- BMX_E[myvars]
  BMX_F <- BMX_F[myvars]
  BMX_G <- BMX_G[myvars]
  BMX_H <- BMX_H[myvars]
  BMX_I <- BMX_I[myvars]
  BMX<-rbind(BMX_E,BMX_F,BMX_G,BMX_H,BMX_I)
  remove(BMX_E)
  remove(BMX_F)
  remove(BMX_G)
  remove(BMX_H)
  remove(BMX_I)
  return(BMX)
}
BMX <-read_BMX()

DEMO_Mortality_ALQ_BMX <- merge(DEMO_Mortality_ALQ,BMX,by="SEQN",all.x=TRUE)
remove(BMX)
remove(DEMO_Mortality_ALQ)


#Read Blood pressure and cholesterol
read_BPQ <- function(){
  BPQ_E <- read.xport("DATA/BPQ Blood Pressure & Cholesterol/BPQ_E.XPT")
  BPQ_F <- read.xport("DATA/BPQ Blood Pressure & Cholesterol/BPQ_F.XPT")
  BPQ_G <- read.xport("DATA/BPQ Blood Pressure & Cholesterol/BPQ_G.XPT")
  BPQ_H <- read.xport("DATA/BPQ Blood Pressure & Cholesterol/BPQ_H.XPT")
  BPQ_I <- read.xport("DATA/BPQ Blood Pressure & Cholesterol/BPQ_I.XPT")
  myvars <- c("SEQN","BPQ080","BPQ020","BPQ050A")
  BPQ_E <- BPQ_E[myvars]
  BPQ_F <- BPQ_F[myvars]
  BPQ_G <- BPQ_G[myvars]
  BPQ_H <- BPQ_H[myvars]
  BPQ_I <- BPQ_I[myvars]
  BPQ<-rbind(BPQ_E,BPQ_F,BPQ_G,BPQ_H,BPQ_I)
  remove(BPQ_E)
  remove(BPQ_F)
  remove(BPQ_G)
  remove(BPQ_H)
  remove(BPQ_I)
  return(BPQ)
}
BPQ <-read_BPQ()

DEMO_Mortality_ALQ_BMX_BPQ <- merge(DEMO_Mortality_ALQ_BMX,BPQ,by="SEQN",all.x=TRUE)
remove(BPQ)
remove(DEMO_Mortality_ALQ_BMX)

#Read Blood pressure
read_BPX <- function(){
  BPX_E <- read.xport("DATA/BPX Blood Pressure/BPX_E.XPT")
  BPX_F <- read.xport("DATA/BPX Blood Pressure/BPX_F.XPT")
  BPX_G <- read.xport("DATA/BPX Blood Pressure/BPX_G.XPT")
  BPX_H <- read.xport("DATA/BPX Blood Pressure/BPX_H.XPT")
  BPX_I <- read.xport("DATA/BPX Blood Pressure/BPX_I.XPT")
  myvars <- c("SEQN","BPXSY1","BPXSY2","BPXSY3","BPXSY4")
  BPX_E <- BPX_E[myvars]
  BPX_F <- BPX_F[myvars]
  BPX_G <- BPX_G[myvars]
  BPX_H <- BPX_H[myvars]
  BPX_I <- BPX_I[myvars]
  BPX<-rbind(BPX_E,BPX_F,BPX_G,BPX_H,BPX_I)
  remove(BPX_E)
  remove(BPX_F)
  remove(BPX_G)
  remove(BPX_H)
  remove(BPX_I)
  return(BPX)
}
BPX <-read_BPX()

DEMO_Mortality_ALQ_BMX_BPQ_BPX <- merge(DEMO_Mortality_ALQ_BMX_BPQ,BPX,by="SEQN",all.x=TRUE)
remove(BPX)
remove(DEMO_Mortality_ALQ_BMX_BPQ)

#Read Diabetes
read_DIQ <- function(){
  DIQ_E <- read.xport("DATA/DIQ Diabetes/DIQ_E.XPT")
  DIQ_F <- read.xport("DATA/DIQ Diabetes/DIQ_F.XPT")
  DIQ_G <- read.xport("DATA/DIQ Diabetes/DIQ_G.XPT")
  DIQ_H <- read.xport("DATA/DIQ Diabetes/DIQ_H.XPT")
  DIQ_I <- read.xport("DATA/DIQ Diabetes/DIQ_I.XPT")
  myvars <- c("SEQN","DIQ010")
  DIQ_E <- DIQ_E[myvars]
  DIQ_F <- DIQ_F[myvars]
  DIQ_G <- DIQ_G[myvars]
  DIQ_H <- DIQ_H[myvars]
  DIQ_I <- DIQ_I[myvars]
  DIQ<-rbind(DIQ_E,DIQ_F,DIQ_G,DIQ_H,DIQ_I)
  remove(DIQ_E)
  remove(DIQ_F)
  remove(DIQ_G)
  remove(DIQ_H)
  remove(DIQ_I)
  return(DIQ)
}
DIQ <-read_DIQ()

DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ <- merge(DEMO_Mortality_ALQ_BMX_BPQ_BPX,DIQ,by="SEQN",all.x=TRUE)
remove(DIQ)
remove(DEMO_Mortality_ALQ_BMX_BPQ_BPX)

#Read Mental Health Depression
read_DPQ <- function(){
  DPQ_E <- read.xport("DATA/DPQ Mental Health - Depression Screener/DPQ_E.XPT")
  DPQ_F <- read.xport("DATA/DPQ Mental Health - Depression Screener/DPQ_F.XPT")
  DPQ_G <- read.xport("DATA/DPQ Mental Health - Depression Screener/DPQ_G.XPT")
  DPQ_H <- read.xport("DATA/DPQ Mental Health - Depression Screener/DPQ_H.XPT")
  DPQ_I <- read.xport("DATA/DPQ Mental Health - Depression Screener/DPQ_I.XPT")
  myvars <- c("SEQN","DPQ020")
  DPQ_E <- DPQ_E[myvars]
  DPQ_F <- DPQ_F[myvars]
  DPQ_G <- DPQ_G[myvars]
  DPQ_H <- DPQ_H[myvars]
  DPQ_I <- DPQ_I[myvars]
  DPQ<-rbind(DPQ_E,DPQ_F,DPQ_G,DPQ_H,DPQ_I)
  remove(DPQ_E)
  remove(DPQ_F)
  remove(DPQ_G)
  remove(DPQ_H)
  remove(DPQ_I)
  return(DPQ)
}
DPQ <-read_DPQ()

DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ <- merge(DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ,DPQ,by="SEQN",all.x=TRUE)
remove(DPQ)
remove(DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ)

#Read Current Health
read_HSQ <- function(){
  HSQ_E <- read.xport("DATA/HSQ Current Health Status/HSQ_E.XPT")
  HSQ_F <- read.xport("DATA/HSQ Current Health Status/HSQ_F.XPT")
  HSQ_G <- read.xport("DATA/HSQ Current Health Status/HSQ_G.XPT")
  HSQ_H <- read.xport("DATA/HSQ Current Health Status/HSQ_H.XPT")
  HSQ_I <- read.xport("DATA/HSQ Current Health Status/HSQ_I.XPT")
  myvars <- c("SEQN","HSD010")
  HSQ_E <- HSQ_E[myvars]
  HSQ_F <- HSQ_F[myvars]
  HSQ_G <- HSQ_G[myvars]
  HSQ_H <- HSQ_H[myvars]
  HSQ_I <- HSQ_I[myvars]
  HSQ<-rbind(HSQ_E,HSQ_F,HSQ_G,HSQ_H,HSQ_I)
  remove(HSQ_E)
  remove(HSQ_F)
  remove(HSQ_G)
  remove(HSQ_H)
  remove(HSQ_I)
  return(HSQ)
}
HSQ <-read_HSQ()

DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ <- merge(DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ,HSQ,by="SEQN",all.x=TRUE)
remove(HSQ)
remove(DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ)

#Read Medical Conditions
read_MCQ <- function(){
  MCQ_E <- read.xport("DATA/MCQ Medical Conditions/MCQ_E.XPT")
  MCQ_F <- read.xport("DATA/MCQ Medical Conditions/MCQ_F.XPT")
  MCQ_G <- read.xport("DATA/MCQ Medical Conditions/MCQ_G.XPT")
  MCQ_H <- read.xport("DATA/MCQ Medical Conditions/MCQ_H.XPT")
  MCQ_I <- read.xport("DATA/MCQ Medical Conditions/MCQ_I.XPT")
  myvars <- c("SEQN","MCQ160C","MCQ160F","MCQ220","MCQ300C","MCQ300A","MCQ160F")
  MCQ_E <- MCQ_E[myvars]
  MCQ_F <- MCQ_F[myvars]
  MCQ_G <- MCQ_G[myvars]
  MCQ_H <- MCQ_H[myvars]
  MCQ_I <- MCQ_I[myvars]
  MCQ<-rbind(MCQ_E,MCQ_F,MCQ_G,MCQ_H,MCQ_I)
  remove(MCQ_E)
  remove(MCQ_F)
  remove(MCQ_G)
  remove(MCQ_H)
  remove(MCQ_I)
  return(MCQ)
}
MCQ <-read_MCQ()

DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ <- merge(DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ,MCQ,by="SEQN",all.x=TRUE)
remove(MCQ)
remove(DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ)

#Read reproductive Health
read_RHQ <- function(){
  RHQ_E <- read.xport("DATA/RHQ Reproductive Health/RHQ_E.XPT")
  RHQ_F <- read.xport("DATA/RHQ Reproductive Health/RHQ_F.XPT")
  RHQ_G <- read.xport("DATA/RHQ Reproductive Health/RHQ_G.XPT")
  RHQ_H <- read.xport("DATA/RHQ Reproductive Health/RHQ_H.XPT")
  RHQ_I <- read.xport("DATA/RHQ Reproductive Health/RHQ_I.XPT")
  myvars <- c("SEQN","RHQ540","RHQ060","RHQ131","RHQ420")
  RHQ_E <- RHQ_E[myvars]
  RHQ_F <- RHQ_F[myvars]
  RHQ_G <- RHQ_G[myvars]
  RHQ_H <- RHQ_H[myvars]
  RHQ_I <- RHQ_I[myvars]
  RHQ<-rbind(RHQ_E,RHQ_F,RHQ_G,RHQ_H,RHQ_I)
  remove(RHQ_E)
  remove(RHQ_F)
  remove(RHQ_G)
  remove(RHQ_H)
  remove(RHQ_I)
  return(RHQ)
}
RHQ <-read_RHQ()

DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ_RHQ <- merge(DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ,RHQ,by="SEQN",all.x=TRUE)
remove(RHQ)
remove(DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ)


#Read Sleep
read_SLQ <- function(){
  SLQ_E <- read.xport("DATA/SLQ Sleep/SLQ_E.XPT")
  SLQ_F <- read.xport("DATA/SLQ Sleep/SLQ_F.XPT")
  SLQ_G <- read.xport("DATA/SLQ Sleep/SLQ_G.XPT")
  SLQ_H <- read.xport("DATA/SLQ Sleep/SLQ_H.XPT")
  SLQ_I <- read.xport("DATA/SLQ Sleep/SLQ_I.XPT")
  myvars <- c("SEQN","SLD010H")
  SLQ_E <- SLQ_E[myvars]
  SLQ_F <- SLQ_F[myvars]
  SLQ_G <- SLQ_G[myvars]
  SLQ_H <- SLQ_H[myvars]
  SLQ_I <- SLQ_I[c("SEQN","SLD012")]
  SLQ_I$SLD010H <- SLQ_I$SLD012
  SLQ_I$SLD010H <- ifelse(SLQ_I$SLD010H>=12,12,SLQ_I$SLD010H)
  SLQ_I <- SLQ_I[c("SEQN","SLD010H")]
  SLQ<-rbind(SLQ_E,SLQ_F,SLQ_G,SLQ_H,SLQ_I)
  remove(SLQ_E)
  remove(SLQ_F)
  remove(SLQ_G)
  remove(SLQ_H)
  remove(SLQ_I)
  return(SLQ)
}

SLQ <-read_SLQ()

DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ_RHQ_SLQ <- merge(DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ_RHQ,SLQ,by="SEQN",all.x=TRUE)
remove(SLQ)
remove(DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ_RHQ)

#Read prescription medication
source("DATA/RXQ_RX Prescription Medications/Prescription_Cleaning.r")

DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ_RHQ_SLQ_RXQ <- merge(DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ_RHQ_SLQ,PRESCRIPTION,by="SEQN",all.x=TRUE)
remove(PRESCRIPTION)
remove(DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ_RHQ_SLQ)

############################OCCUPATION
#To be determined





#############################



###########################Physical Activity
#To be determined





#############################











