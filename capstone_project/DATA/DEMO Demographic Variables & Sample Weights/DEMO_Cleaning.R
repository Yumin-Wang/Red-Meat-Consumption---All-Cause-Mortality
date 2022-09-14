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
  #DEMO_I <- read.xport("DATA/DEMO Demographic Variables & Sample Weights/DEMO_I.XPT")
  #myvars <- c("SEQN","RIDAGEYR", "RIAGENDR", "DMDEDUC2", "RIDRETH1", "DMDMARTL", "INDFMIN2", "INDFMPIR", "SDDSRVYR", "SDMVPSU", "SDMVSTRA")
  myvars <- c("SEQN","RIDAGEYR", "RIAGENDR", "DMDEDUC2", "RIDRETH1", "DMDMARTL", "INDFMIN2", "INDFMPIR", "SDDSRVYR")
  DEMO_E <- DEMO_E[myvars]
  DEMO_F <- DEMO_F[myvars]
  DEMO_G <- DEMO_G[myvars]
  DEMO_H <- DEMO_H[myvars]
  #DEMO_I <- DEMO_I[myvars]
  #DEMO<-rbind(DEMO_E,DEMO_F,DEMO_G,DEMO_H,DEMO_I)
  DEMO<-rbind(DEMO_E,DEMO_F,DEMO_G,DEMO_H)
  remove(DEMO_E)
  remove(DEMO_F)
  remove(DEMO_G)
  remove(DEMO_H)
  #remove(DEMO_I)
  DEMO$DMDEDUC2[DEMO$DMDEDUC2==9]<-NA
  DEMO$DMDEDUC2[DEMO$DMDEDUC2==7]<-NA
  DEMO$DMDMARTL[DEMO$DMDMARTL==77]<-NA
  DEMO$DMDMARTL[DEMO$DMDMARTL==99]<-NA
  DEMO$INDFMIN2[DEMO$INDFMIN2==77]<-NA
  DEMO$INDFMIN2[DEMO$INDFMIN2==99]<-NA
  for (i in 1:nrow(DEMO)){
    if(!is.na(DEMO$INDFMIN2[i])){
      if (DEMO$INDFMIN2[i]==1|DEMO$INDFMIN2[i]==2|DEMO$INDFMIN2[i]==3){
        DEMO$INDFMIN2[i]<-1
      }
      if (DEMO$INDFMIN2[i]==4|DEMO$INDFMIN2[i]==5|DEMO$INDFMIN2[i]==6){
        DEMO$INDFMIN2[i]<-2
      }
      if (DEMO$INDFMIN2[i]==7|DEMO$INDFMIN2[i]==8|DEMO$INDFMIN2[i]==9){
        DEMO$INDFMIN2[i]<-3
      }
      if (DEMO$INDFMIN2[i]==10|DEMO$INDFMIN2[i]==14|DEMO$INDFMIN2[i]==15){
        DEMO$INDFMIN2[i]<-4
      }
      if (DEMO$INDFMIN2[i]==12|DEMO$INDFMIN2[i]==13){
        DEMO$INDFMIN2[i]<-NA
      }
    }
  }
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
  
  #Mortality_I <- read_fwf(file=paste("DATA/Mortality/NHANES_2015_2016_MORT_2019_PUBLIC.dat"),
  #                        col_types = "iiiiiiii",
  #                        fwf_cols(seqn = c(1,6),
  #                                 eligstat = c(15,15),
  #                                 mortstat = c(16,16),
  #                                 ucod_leading = c(17,19),
  #                                 diabetes = c(20,20),
  #                                 hyperten = c(21,21),
  #                                 permth_int = c(43,45),
  #                                 permth_exm = c(46,48)
  #                        ),
  #                        na = c("", ".")
  #)
  
  #myvars <- c("seqn","eligstat", "ucod_leading","mortstat", "permth_int")
  myvars <- c("seqn","eligstat","mortstat", "permth_int")
  Mortality_E <- Mortality_E[myvars]
  Mortality_F <- Mortality_F[myvars]
  Mortality_G <- Mortality_G[myvars]
  Mortality_H <- Mortality_H[myvars]
  #Mortality_I <- Mortality_I[myvars]
  #Mortality<-rbind(Mortality_E,Mortality_F,Mortality_G,Mortality_H,Mortality_I)
  Mortality<-rbind(Mortality_E,Mortality_F,Mortality_G,Mortality_H)
  remove(Mortality_E)
  remove(Mortality_F)
  remove(Mortality_G)
  remove(Mortality_H)
  #remove(Mortality_I)
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
  #ALQ_I <- read.xport("DATA/ALQ Alcohol Use/ALQ_I.XPT")
  #myvars <- c("SEQN","ALQ130")
  myvars <- c("SEQN","ALQ120Q","ALQ130")
  ALQ_E <- ALQ_E[myvars]
  ALQ_F <- ALQ_F[myvars]
  ALQ_G <- ALQ_G[myvars]
  ALQ_H <- ALQ_H[myvars]
  #ALQ_I <- ALQ_I[myvars]
  #ALQ<-rbind(ALQ_E,ALQ_F,ALQ_G,ALQ_H,ALQ_I)
  ALQ<-rbind(ALQ_E,ALQ_F,ALQ_G,ALQ_H)
  remove(ALQ_E)
  remove(ALQ_F)
  remove(ALQ_G)
  remove(ALQ_H)
  #remove(ALQ_I)
  ALQ$ALQ130[ALQ$ALQ120Q==0]<-0
  ALQ<-ALQ[,c("SEQN","ALQ130")]
  ALQ$ALQ130[ALQ$ALQ130==777]<-NA
  ALQ$ALQ130[ALQ$ALQ130==999]<-NA
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
  #BMX_I <- read.xport("DATA/BMX Body Measures/BMX_I.XPT")
  myvars <- c("SEQN","BMXBMI","BMXHT","BMXWT")
  BMX_E <- BMX_E[myvars]
  BMX_F <- BMX_F[myvars]
  BMX_G <- BMX_G[myvars]
  BMX_H <- BMX_H[myvars]
  #BMX_I <- BMX_I[myvars]
  #BMX<-rbind(BMX_E,BMX_F,BMX_G,BMX_H,BMX_I)
  BMX<-rbind(BMX_E,BMX_F,BMX_G,BMX_H)
  remove(BMX_E)
  remove(BMX_F)
  remove(BMX_G)
  remove(BMX_H)
  #remove(BMX_I)
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
  #BPQ_I <- read.xport("DATA/BPQ Blood Pressure & Cholesterol/BPQ_I.XPT")
  #myvars <- c("SEQN","BPQ080","BPQ020","BPQ050A")
  myvars <- c("SEQN","BPQ080","BPQ020")
  BPQ_E <- BPQ_E[myvars]
  BPQ_F <- BPQ_F[myvars]
  BPQ_G <- BPQ_G[myvars]
  BPQ_H <- BPQ_H[myvars]
  #BPQ_I <- BPQ_I[myvars]
  #BPQ<-rbind(BPQ_E,BPQ_F,BPQ_G,BPQ_H,BPQ_I)
  BPQ<-rbind(BPQ_E,BPQ_F,BPQ_G,BPQ_H)
  remove(BPQ_E)
  remove(BPQ_F)
  remove(BPQ_G)
  remove(BPQ_H)
  BPQ$BPQ080[BPQ$BPQ080==7]<-NA
  BPQ$BPQ080[BPQ$BPQ080==9]<-NA
  BPQ$BPQ020[BPQ$BPQ020==7]<-NA
  BPQ$BPQ020[BPQ$BPQ020==9]<-NA
  #BPQ$BPQ050A[BPQ$BPQ050A==7]<-NA
  #BPQ$BPQ050A[BPQ$BPQ050A==9]<-NA
  #remove(BPQ_I)
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
  #BPX_I <- read.xport("DATA/BPX Blood Pressure/BPX_I.XPT")
  myvars <- c("SEQN","BPXSY1","BPXSY2","BPXSY3","BPXSY4")
  BPX_E <- BPX_E[myvars]
  BPX_F <- BPX_F[myvars]
  BPX_G <- BPX_G[myvars]
  BPX_H <- BPX_H[myvars]
  #BPX_I <- BPX_I[myvars]
  #BPX<-rbind(BPX_E,BPX_F,BPX_G,BPX_H,BPX_I)
  BPX<-rbind(BPX_E,BPX_F,BPX_G,BPX_H)
  remove(BPX_E)
  remove(BPX_F)
  remove(BPX_G)
  remove(BPX_H)
  #remove(BPX_I)
  BPX$BPXSY = rowMeans(BPX[,c("BPXSY1","BPXSY2","BPXSY3","BPXSY4")],na.rm=TRUE)
  BPX <- BPX[c("SEQN","BPXSY")]
  BPX$BPXSY[is.nan(BPX$BPXSY)]<-NA
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
  #DIQ_I <- read.xport("DATA/DIQ Diabetes/DIQ_I.XPT")
  myvars <- c("SEQN","DIQ010")
  DIQ_E <- DIQ_E[myvars]
  DIQ_F <- DIQ_F[myvars]
  DIQ_G <- DIQ_G[myvars]
  DIQ_H <- DIQ_H[myvars]
  #DIQ_I <- DIQ_I[myvars]
  #DIQ<-rbind(DIQ_E,DIQ_F,DIQ_G,DIQ_H,DIQ_I)
  DIQ<-rbind(DIQ_E,DIQ_F,DIQ_G,DIQ_H)
  remove(DIQ_E)
  remove(DIQ_F)
  remove(DIQ_G)
  remove(DIQ_H)
  #remove(DIQ_I)
  DIQ$DIQ010[DIQ$DIQ010==7]<-NA
  DIQ$DIQ010[DIQ$DIQ010==9]<-NA
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
  #DPQ_I <- read.xport("DATA/DPQ Mental Health - Depression Screener/DPQ_I.XPT")
  myvars <- c("SEQN","DPQ020")
  DPQ_E <- DPQ_E[myvars]
  DPQ_F <- DPQ_F[myvars]
  DPQ_G <- DPQ_G[myvars]
  DPQ_H <- DPQ_H[myvars]
  #DPQ_I <- DPQ_I[myvars]
  #DPQ<-rbind(DPQ_E,DPQ_F,DPQ_G,DPQ_H,DPQ_I)
  DPQ<-rbind(DPQ_E,DPQ_F,DPQ_G,DPQ_H)
  remove(DPQ_E)
  remove(DPQ_F)
  remove(DPQ_G)
  remove(DPQ_H)
  DPQ$DPQ020[DPQ$DPQ020==7]<-NA
  DPQ$DPQ020[DPQ$DPQ020==9]<-NA
  for (i in 1:nrow(DPQ)){
    if(!is.na(DPQ$DPQ020[i])){
      if (DPQ$DPQ020[i]==1|DPQ$DPQ020[i]==2|DPQ$DPQ020[i]==3){
        DPQ$DPQ020[i]<-1
      }
    }
  }
  #remove(DPQ_I)
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
  #HSQ_I <- read.xport("DATA/HSQ Current Health Status/HSQ_I.XPT")
  myvars <- c("SEQN","HSD010")
  HSQ_E <- HSQ_E[myvars]
  HSQ_F <- HSQ_F[myvars]
  HSQ_G <- HSQ_G[myvars]
  HSQ_H <- HSQ_H[myvars]
  #HSQ_I <- HSQ_I[myvars]
  #HSQ<-rbind(HSQ_E,HSQ_F,HSQ_G,HSQ_H,HSQ_I)
  HSQ<-rbind(HSQ_E,HSQ_F,HSQ_G,HSQ_H)
  remove(HSQ_E)
  remove(HSQ_F)
  remove(HSQ_G)
  remove(HSQ_H)
  #remove(HSQ_I)
  HSQ$HSD010[HSQ$HSD010==7]<-NA
  HSQ$HSD010[HSQ$HSD010==9]<-NA
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
  #MCQ_I <- read.xport("DATA/MCQ Medical Conditions/MCQ_I.XPT")
  myvars <- c("SEQN","MCQ160C","MCQ160F","MCQ220","MCQ300C","MCQ300A","MCQ160F")
  MCQ_E <- MCQ_E[myvars]
  MCQ_F <- MCQ_F[myvars]
  MCQ_G <- MCQ_G[myvars]
  MCQ_H <- MCQ_H[myvars]
  #MCQ_I <- MCQ_I[myvars]
  #MCQ<-rbind(MCQ_E,MCQ_F,MCQ_G,MCQ_H,MCQ_I)
  MCQ<-rbind(MCQ_E,MCQ_F,MCQ_G,MCQ_H)
  remove(MCQ_E)
  remove(MCQ_F)
  remove(MCQ_G)
  remove(MCQ_H)
  #remove(MCQ_I)
  MCQ$MCQ160C[MCQ$MCQ160C==7|MCQ$MCQ160C==9]<-NA
  MCQ$MCQ160F[MCQ$MCQ160F==7|MCQ$MCQ160F==9]<-NA
  MCQ$MCQ220[MCQ$MCQ220==7|MCQ$MCQ220==9]<-NA
  MCQ$MCQ300C[MCQ$MCQ300C==7|MCQ$MCQ300C==9]<-NA
  MCQ$MCQ300A[MCQ$MCQ300A==7|MCQ$MCQ300A==9]<-NA
  MCQ$MCQ160F.1[MCQ$MCQ160F.1==7|MCQ$MCQ160F.1==9]<-NA
  MCQ$CARDIOVASCULAR <- floor(rowMeans(subset(MCQ, select = c(MCQ160C, MCQ160F)), na.rm = TRUE))  
  is.nan.data.frame <- function(x)
  {do.call(cbind, lapply(x, is.nan))}
  MCQ$CARDIOVASCULAR[is.nan(MCQ$CARDIOVASCULAR)] <- NA
  MCQ<-MCQ[,c("SEQN","CARDIOVASCULAR","MCQ220","MCQ300C","MCQ300A","MCQ160F")]  
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
  #RHQ_I <- read.xport("DATA/RHQ Reproductive Health/RHQ_I.XPT")
  myvars <- c("SEQN","RHQ010","RHQ540","RHQ060","RHQ131","RHQ420","RHD143")
  RHQ_E <- RHQ_E[myvars]
  RHQ_F <- RHQ_F[myvars]
  RHQ_G <- RHQ_G[myvars]
  RHQ_H <- RHQ_H[myvars]
  #RHQ_I <- RHQ_I[myvars]
  #RHQ<-rbind(RHQ_E,RHQ_F,RHQ_G,RHQ_H,RHQ_I)
  RHQ<-rbind(RHQ_E,RHQ_F,RHQ_G,RHQ_H)
  remove(RHQ_E)
  remove(RHQ_F)
  remove(RHQ_G)
  remove(RHQ_H)
  #remove(RHQ_I)
  RHQ$RHQ010[RHQ$RHQ010==777|RHQ$RHQ010==999]<-NA
  RHQ$RHQ540[RHQ$RHQ540==7|RHQ$RHQ540==9]<-NA
  RHQ$RHQ060[RHQ$RHQ060==777|RHQ$RHQ060==999]<-NA
  RHQ$RHQ131[RHQ$RHQ131==7|RHQ$RHQ131==9]<-NA
  RHQ$RHQ420[RHQ$RHQ420==7|RHQ$RHQ420==9]<-NA
  RHQ$MENOPAUSAL<-NA
  RHQ$MENOPAUSAL[!is.na(RHQ$RHQ010)&!is.na(RHQ$RHQ060)]<-1
  RHQ$MENOPAUSAL[!is.na(RHQ$RHQ010)&is.na(RHQ$RHQ060)]<-0
  RHQ$RHD143[RHQ$RHD143==7|RHQ$RHD143==9]<-NA
  RHQ<-RHQ[,c("SEQN","MENOPAUSAL","RHQ540","RHQ131","RHQ420","RHD143")]
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
  #SLQ_I <- read.xport("DATA/SLQ Sleep/SLQ_I.XPT")
  myvars <- c("SEQN","SLD010H")
  SLQ_E <- SLQ_E[myvars]
  SLQ_F <- SLQ_F[myvars]
  SLQ_G <- SLQ_G[myvars]
  SLQ_H <- SLQ_H[myvars]
  #SLQ_I <- SLQ_I[c("SEQN","SLD012")]
  #SLQ_I$SLD010H <- SLQ_I$SLD012
  #SLQ_I$SLD010H <- ifelse(SLQ_I$SLD010H>=12,12,SLQ_I$SLD010H)
  #SLQ_I <- SLQ_I[c("SEQN","SLD010H")]
  #SLQ<-rbind(SLQ_E,SLQ_F,SLQ_G,SLQ_H,SLQ_I)
  SLQ<-rbind(SLQ_E,SLQ_F,SLQ_G,SLQ_H)
  remove(SLQ_E)
  remove(SLQ_F)
  remove(SLQ_G)
  remove(SLQ_H)
  SLQ$SLD010H[SLQ$SLD010H==77|SLQ$SLD010H==99]<-NA
  #remove(SLQ_I)
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
DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ_RHQ_SLQ_RXQ[,c("ASPIRIN","ATORVASTATIN","IBUPROFEN","OPIUM","STATIN","VALSARTAN")][is.na(DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ_RHQ_SLQ_RXQ[,c("ASPIRIN","ATORVASTATIN","IBUPROFEN","OPIUM","STATIN","VALSARTAN")])]<-0

#Read Total Nutrition Intake
source("DATA/DR1TOT Dietary Interview - Total Nutrient Intakes, First Day/TOT_NUTRITION.r")

DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ_RHQ_SLQ_RXQ_TOT <- merge(DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ_RHQ_SLQ_RXQ,TOT,by="SEQN",all.x=TRUE)
remove(TOT)
remove(DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ_RHQ_SLQ_RXQ)


#Read Total dietary supplement intake
source("DATA/DS1TOT Dietary Supplement Use 24-Hour - Total Dietary Supplements, First Day/TOT_supplement.r")
DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ_RHQ_SLQ_RXQ_TOT_SUP <- merge(DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ_RHQ_SLQ_RXQ_TOT,TOT_supp,by="SEQN",all.x=TRUE)
remove(TOT_supp)
remove(DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ_RHQ_SLQ_RXQ_TOT)

#Read Multivitamin user -individual supplement file
source("DATA/DS1IDS Dietary Supplement Use 24-Hour - Individual Dietary Supplements, First Day/Multivitamin_Use.r")
DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ_RHQ_SLQ_RXQ_TOT_SUP_VITA <- merge(DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ_RHQ_SLQ_RXQ_TOT_SUP,DSIDS,by="SEQN",all.x=TRUE)
remove(DSIDS)
remove(DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ_RHQ_SLQ_RXQ_TOT_SUP)
DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ_RHQ_SLQ_RXQ_TOT_SUP_VITA$MULTIVITAMIN[is.na(DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ_RHQ_SLQ_RXQ_TOT_SUP_VITA$MULTIVITAMIN)]<-0




#OCCUPATION
read_OCQ <- function(){
  OCQ_E <- read.xport("DATA/OCQ Occupation/OCQ_E.XPT")
  OCQ_F <- read.xport("DATA/OCQ Occupation/OCQ_F.XPT")
  OCQ_G <- read.xport("DATA/OCQ Occupation/OCQ_G.XPT")
  OCQ_H <- read.xport("DATA/OCQ Occupation/OCQ_H.XPT")
  #myvars <- c("SEQN","OCD241")
  myvars <- c("SEQN","OCD150","OCQ180","OCD241")
  OCQ_E <- OCQ_E[myvars]
  OCQ_F <- OCQ_F[myvars]
  OCQ_G <- OCQ_G[myvars]
  OCQ_H <- OCQ_H[myvars]
  OCQ<-rbind(OCQ_E,OCQ_F,OCQ_G,OCQ_H)
  remove(OCQ_E)
  remove(OCQ_F)
  remove(OCQ_G)
  remove(OCQ_H)
  OCQ$OCQ180[OCQ$OCD150==4]<-0
  OCQ<-OCQ[,c("SEQN","OCQ180")]
  OCQ$OCQ180[OCQ$OCQ180==77777|OCQ$OCQ180==99999]<-NA
  for (i in 1:nrow(OCQ)){
    if(!is.na(OCQ$OCQ180[i])){
      if (OCQ$OCQ180[i]<=30&OCQ$OCQ180[i]>0){
        OCQ$OCQ180[i]<-1
      }
      if (OCQ$OCQ180[i]>30){
        OCQ$OCQ180[i]<-2
      }
    }
  }
  return(OCQ)
}

OCQ <-read_OCQ()

DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ_RHQ_SLQ_RXQ_TOT_SUP_VITA_OCQ <- merge(DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ_RHQ_SLQ_RXQ_TOT_SUP_VITA,OCQ,by="SEQN",all.x=TRUE)
remove(OCQ)
remove(DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ_RHQ_SLQ_RXQ_TOT_SUP_VITA)



#Physical Activity
read_PAQ <- function(){
  PAQ_E <- read.xport("DATA/PAQ Physical Activity/PAQ_E.XPT")
  PAQ_F <- read.xport("DATA/PAQ Physical Activity/PAQ_F.XPT")
  PAQ_G <- read.xport("DATA/PAQ Physical Activity/PAQ_G.XPT")
  PAQ_H <- read.xport("DATA/PAQ Physical Activity/PAQ_H.XPT")
  #myvars <- c("SEQN","PAD660","PAD680")
  myvars <- c("SEQN","PAQ650","PAD660","PAQ665","PAD675","PAD680")
  PAQ_E <- PAQ_E[myvars]
  PAQ_F <- PAQ_F[myvars]
  PAQ_G <- PAQ_G[myvars]
  PAQ_H <- PAQ_H[myvars]
  PAQ<-rbind(PAQ_E,PAQ_F,PAQ_G,PAQ_H)
  remove(PAQ_E)
  remove(PAQ_F)
  remove(PAQ_G)
  remove(PAQ_H)
  PAQ$PAQ650[PAQ$PAQ650==7]<-NA
  PAQ$PAQ650[PAQ$PAQ650==9]<-NA
  PAQ$PAQ665[PAQ$PAQ665==7]<-NA
  PAQ$PAQ665[PAQ$PAQ665==9]<-NA
  PAQ$PAD680[PAQ$PAD680==7777|PAQ$PAD680==9999]<-NA
  PAQ$ACTIVITY <- floor(rowMeans(subset(PAQ, select = c(PAQ650, PAQ665)), na.rm = TRUE))  
  is.nan.data.frame <- function(x)
  {do.call(cbind, lapply(x, is.nan))}
  PAQ$ACTIVITY[is.nan(PAQ$ACTIVITY)] <- NA
  PAQ<-PAQ[,c("SEQN","ACTIVITY","PAD680")]  
  PAQ$PAD680[PAQ$PAD680>=1000]<-NA
  return(PAQ)
}

PAQ <-read_PAQ()

DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ_RHQ_SLQ_RXQ_TOT_SUP_VITA_OCQ_PAQ <- merge(DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ_RHQ_SLQ_RXQ_TOT_SUP_VITA_OCQ,PAQ,by="SEQN",all.x=TRUE)
remove(PAQ)
remove(DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ_RHQ_SLQ_RXQ_TOT_SUP_VITA_OCQ)

#Smoking habit
source("DATA/SMQ Smoking - Cigarette Use/SMOKING.r")
DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ_RHQ_SLQ_RXQ_TOT_SUP_VITA_OCQ_PAQ_SMQ <- merge(DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ_RHQ_SLQ_RXQ_TOT_SUP_VITA_OCQ_PAQ,SMQ,by="SEQN",all.x=TRUE)
remove(SMQ)
remove(DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ_RHQ_SLQ_RXQ_TOT_SUP_VITA_OCQ_PAQ)

#Total Dietary Food intake
source("DATA/FPED_DR1TOT - Total food, First day/FOOD.r")
DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ_RHQ_SLQ_RXQ_TOT_SUP_VITA_OCQ_PAQ_SMQ_FOOD <- merge(DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ_RHQ_SLQ_RXQ_TOT_SUP_VITA_OCQ_PAQ_SMQ,TOT_FOOD,by="SEQN",all.x=TRUE)
remove(TOT_FOOD)
remove(DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ_RHQ_SLQ_RXQ_TOT_SUP_VITA_OCQ_PAQ_SMQ)

#NEW_MEAT
source("DATA/FPED_DR1IFF - Individual food, First day/INDIVIDUAL_FOOD.r")
DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ_RHQ_SLQ_RXQ_TOT_SUP_VITA_OCQ_PAQ_SMQ_FOOD_MEAT <- merge(DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ_RHQ_SLQ_RXQ_TOT_SUP_VITA_OCQ_PAQ_SMQ_FOOD,NEW_MEAT,by="SEQN",all.x=TRUE)
remove(NEW_MEAT)
remove(DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ_RHQ_SLQ_RXQ_TOT_SUP_VITA_OCQ_PAQ_SMQ_FOOD)

DATA<-DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ_RHQ_SLQ_RXQ_TOT_SUP_VITA_OCQ_PAQ_SMQ_FOOD_MEAT
remove(DEMO_Mortality_ALQ_BMX_BPQ_BPX_DIQ_DPQ_HSQ_MCQ_RHQ_SLQ_RXQ_TOT_SUP_VITA_OCQ_PAQ_SMQ_FOOD_MEAT)

DATA$BEEF_VEAL[!is.na(DATA$PF_MEAT)&is.na(DATA$BEEF_VEAL)]<-0
DATA$BEEF_VEAL_LAMB[!is.na(DATA$PF_MEAT)&is.na(DATA$BEEF_VEAL_LAMB)]<-0
DATA$BEEF_VEAL_PORK[!is.na(DATA$PF_MEAT)&is.na(DATA$BEEF_VEAL_PORK)]<-0
DATA$BEEF_VEAL_PORK_LAMB[!is.na(DATA$PF_MEAT)&is.na(DATA$BEEF_VEAL_PORK_LAMB)]<-0


#Exclusion!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Age younger or older than the age range considered
DATA<-DATA[DATA$RIDAGEYR>=20&DATA$RIDAGEYR<=79,]

#Missing alcohol drinking, education, martial status, sleep, sedentary lifestyle, smoking
DATA<-DATA[!is.na(DATA$ALQ130)&!is.na(DATA$DMDEDUC2)&!is.na(DATA$DMDMARTL)&!is.na(DATA$SLD010H)&!is.na(DATA$PAD680)&!is.na(DATA$SMOKING),]

#Missing history of hypercholesterolemia, history of hypertension, history of diabetes, history of depression, history of cardiovascular disease, history of cancer, family history of diabetes, family history of myocardial infraction
DATA<-DATA[!is.na(DATA$BPQ080)&!is.na(DATA$BPQ020)&!is.na(DATA$DIQ010)&!is.na(DATA$DPQ020)&!is.na(DATA$CARDIOVASCULAR)&!is.na(DATA$MCQ220)&!is.na(DATA$MCQ300C)&!is.na(DATA$MCQ300A),]

#Missing family annual income and PIR
DATA<-DATA[!is.na(DATA$INDFMIN2)&!is.na(DATA$INDFMPIR),]

#Missing occupation category
DATA<-DATA[!is.na(DATA$OCQ180),]

#Missing dietary variables: TKCAL, TCARB, TFIBE, TSFAT, TMFAT, TPFAT, TCHOL, TMAGN, F_FRUIT, V_TOTAL, PF_SEAFD, G_WHOLE, PF_MPS_TOTAL, PF_MEAT                               PF_CUREDMEAT, PF_POULT, PF_EGGS, PF_NUTSDS, PF_LEGUMES, D_TOTAL                                D_CHEESE, BEEF_VEAL, BEEF_VEAL_LAMB, BEEF_VEAL_PORK       BEEF_VEAL_PORK_LAMB
#and missing special diet and dietary supplement intake
DATA<-DATA[!is.na(DATA$TKCAL)&!is.na(DATA$DRQSDIET)&!is.na(DATA$DSDS),]

#Missing Systolic blood pressure
DATA<-DATA[!is.na(DATA$BPXSY),]

#Extreme value of total energy intake
DATA<-DATA[DATA$TKCAL>=500&DATA$TKCAL<=4500,]

#Missing BMI, or implausible BMI (<15 or ≥60 kg/m2)
DATA<-DATA[!is.na(DATA$BMXBMI)&DATA$BMXBMI>=15&DATA$BMXBMI<60,]

#Women are pregnant at baseline
DATA<-DATA[(DATA$RHD143==2|is.na(DATA$RHD143)),]

#Women who are missing Menopausal status, Hormone therapy, Parity, Oral contraceptive use
DATA<-DATA[!(DATA$RIAGENDR==2&is.na(DATA$MENOPAUSAL))&!(DATA$RIAGENDR==2&is.na(DATA$RHQ540))&!(DATA$RIAGENDR==2&is.na(DATA$RHQ131))&!(DATA$RIAGENDR==2&is.na(DATA$RHQ420)),]

#Not available for death linkage
DATA<-DATA[DATA$ELIGSTAT!=3,]

#Create variables !!!!!!
DATA <-DATA %>% mutate(AGE_GROUP = case_when(RIDAGEYR >= 20 & RIDAGEYR <=29 ~ "20-29 years old",
                                             RIDAGEYR >= 30 & RIDAGEYR <=39 ~ "30-39 years old",
                                             RIDAGEYR >= 40 & RIDAGEYR <=49 ~ "40-49 years old",
                                             RIDAGEYR >= 50 & RIDAGEYR <=59 ~ "50-59 years old",
                                             RIDAGEYR >= 60 & RIDAGEYR <=69 ~ "60-69 years old",
                                             RIDAGEYR >= 70 & RIDAGEYR <=79 ~ "70-79 years old"))



DATA$INDFMPIR <- ntile(DATA$INDFMPIR, 5)  

DATA$AGE_DEATH_CENSORED<-DATA$RIDAGEYR+(DATA$PERMTH_INT/12)

DATA<- DATA %>% mutate(ALCOHOL_GROUP = case_when(ALQ130==0 ~ "Non-drinker",
                                                 ALQ130>0&ALQ130<2 ~ "<2 drinks per day",
                                                 ALQ130>=2 ~ ">=2 drinks per day"))

DATA <- DATA %>% mutate(BMI_GROUP = case_when(BMXBMI<18.5 ~ "Underweight",
                                              BMXBMI>=18.5&BMXBMI<25 ~"Healthy Weight",
                                              BMXBMI>=25&BMXBMI<30 ~ "Overweight",
                                              BMXBMI>=30~"Obesity"))
DATA$BPXSY <- ntile(DATA$BPXSY, 5)  

DATA$DIQ010[DATA$DIQ010==3]<-2
                                              
DATA <- DATA %>% mutate(SLD010H = case_when(SLD010H<=4 ~ "<=4 hours/night",
                                            SLD010H>4&SLD010H<9 ~ "5-8 hours/night",
                                                  SLD010H>=9 ~ ">=9 hours/night"))

DATA$PAD680 <- ntile(DATA$PAD680, 5)  



#Reorder columns
DATA <- select(DATA,SEQN,SDDSRVYR,MORTSTAT,PERMTH_INT,RIDAGEYR,AGE_DEATH_CENSORED,AGE_GROUP,RIAGENDR,RIDRETH1,DMDEDUC2,
               DMDMARTL,ALQ130,ALCOHOL_GROUP,SMOKING,OCQ180,ACTIVITY,PAD680,SLD010H,INDFMIN2,INDFMPIR,BMXBMI,BMI_GROUP,BPXSY,HSD010,BPQ080,
               BPQ020,DIQ010,DPQ020,CARDIOVASCULAR,MCQ220,MCQ300C,MCQ300A,MENOPAUSAL,RHQ540,RHQ131,RHQ420, ASPIRIN,IBUPROFEN,
               OPIUM,STATIN,VALSARTAN,DRQSDIET,DSDS,PF_CUREDMEAT,PF_MEAT,
               PF_POULT, F_FRUIT,V_TOTAL, PF_SEAFD,G_WHOLE,PF_EGGS,PF_NUTSDS,PF_LEGUMES,D_TOTAL,TKCAL,TCARB,TFIBE,TSFAT,TMFAT,TPFAT,TCHOL,TMAGN)              

#convert ounce to grams
DATA$PF_CUREDMEAT<-DATA$PF_CUREDMEAT*28.35
DATA$PF_MEAT<-DATA$PF_MEAT*28.35
DATA$PF_POULT<-DATA$PF_POULT*28.35
DATA$PF_SEAFD<-DATA$PF_SEAFD*28.35
DATA$G_WHOLE<-DATA$G_WHOLE*28.35
DATA$PF_EGGS<-DATA$PF_EGGS*28.35
DATA$PF_NUTSDS<-DATA$PF_NUTSDS*28.35
DATA$PF_LEGUMES<-DATA$PF_LEGUMES*28.35


DATA$PF_MEAT_STANDARD_QUARTILE<-DATA$PF_MEAT
DATA$PF_MEAT_STANDARD_QUARTILE<-ntile(DATA$PF_MEAT_STANDARD_QUARTILE, 4)  

DATA$PF_MEAT_STANDARD_QUINTILES<-DATA$PF_MEAT
DATA$PF_MEAT_STANDARD_QUINTILES<-ntile(DATA$PF_MEAT_STANDARD_QUINTILES, 5) 

DATA$PF_MEAT_DENSITY_CONTINOUS<-DATA$PF_MEAT/DATA$TKCAL

DATA$PF_MEAT_DENSITY_QUARTILE<-DATA$PF_MEAT_DENSITY_CONTINOUS
DATA$PF_MEAT_DENSITY_QUARTILE<-ntile(DATA$PF_MEAT_DENSITY_QUARTILE, 4) 

DATA$PF_MEAT_DENSITY_QUINTILES<-DATA$PF_MEAT_DENSITY_CONTINOUS
DATA$PF_MEAT_DENSITY_QUINTILES<-ntile(DATA$PF_MEAT_DENSITY_QUINTILES, 5)  

#create binary variable for standard meat quartile and quintile
DATA$UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_2nd<-ifelse(DATA$PF_MEAT_STANDARD_QUARTILE==2,1,0)
DATA$UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_3rd<-ifelse(DATA$PF_MEAT_STANDARD_QUARTILE==3,1,0)
DATA$UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_4th<-ifelse(DATA$PF_MEAT_STANDARD_QUARTILE==4,1,0)
  
DATA$UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_2nd<-ifelse(DATA$PF_MEAT_STANDARD_QUINTILES==2,1,0)
DATA$UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_3rd<-ifelse(DATA$PF_MEAT_STANDARD_QUINTILES==3,1,0)
DATA$UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_4th<-ifelse(DATA$PF_MEAT_STANDARD_QUINTILES==4,1,0)
DATA$UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_5th<-ifelse(DATA$PF_MEAT_STANDARD_QUINTILES==5,1,0)


#create binary variable for density meat quartile and quintile
DATA$UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_2nd<-ifelse(DATA$PF_MEAT_DENSITY_QUARTILE==2,1,0)
DATA$UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_3rd<-ifelse(DATA$PF_MEAT_DENSITY_QUARTILE==3,1,0)
DATA$UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_4th<-ifelse(DATA$PF_MEAT_DENSITY_QUARTILE==4,1,0)

DATA$UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_2nd<-ifelse(DATA$PF_MEAT_DENSITY_QUINTILES==2,1,0)
DATA$UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_3rd<-ifelse(DATA$PF_MEAT_DENSITY_QUINTILES==3,1,0)
DATA$UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_4th<-ifelse(DATA$PF_MEAT_DENSITY_QUINTILES==4,1,0)
DATA$UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_5th<-ifelse(DATA$PF_MEAT_DENSITY_QUINTILES==5,1,0)




#Reorder columns
DATA <- select(DATA,SEQN,SDDSRVYR,MORTSTAT,PERMTH_INT,RIDAGEYR,AGE_DEATH_CENSORED,AGE_GROUP,RIAGENDR,RIDRETH1,DMDEDUC2,
               DMDMARTL,ALQ130,ALCOHOL_GROUP,SMOKING,OCQ180,ACTIVITY,PAD680,SLD010H,INDFMIN2,INDFMPIR,BMXBMI,BMI_GROUP,BPXSY,HSD010,BPQ080,
               BPQ020,DIQ010,DPQ020,CARDIOVASCULAR,MCQ220,MCQ300C,MCQ300A,MENOPAUSAL,RHQ540,RHQ131,RHQ420, ASPIRIN,IBUPROFEN,
               OPIUM,STATIN,VALSARTAN,DRQSDIET,DSDS,PF_CUREDMEAT,PF_MEAT,PF_MEAT_STANDARD_QUARTILE,UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_2nd,UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_3rd,UNPROCESSED_RED_MEAT_STANDARD_QUARTILE_4th,PF_MEAT_STANDARD_QUINTILES,UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_2nd,UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_3rd,UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_4th,UNPROCESSED_RED_MEAT_STANDARD_QUINTILE_5th,PF_MEAT_DENSITY_CONTINOUS,PF_MEAT_DENSITY_QUARTILE,UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_2nd,UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_3rd,UNPROCESSED_RED_MEAT_DENSITY_QUARTILE_4th,PF_MEAT_DENSITY_QUINTILES,UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_2nd,UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_3rd,UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_4th,UNPROCESSED_RED_MEAT_DENSITY_QUINTILE_5th,
               PF_POULT, F_FRUIT,V_TOTAL, PF_SEAFD,G_WHOLE,PF_EGGS,PF_NUTSDS,PF_LEGUMES,D_TOTAL,TKCAL,TCARB,TFIBE,TSFAT,TMFAT,TPFAT,TCHOL,TMAGN)              



write.csv(DATA,"DATA/Combined DATA/DATA.csv", row.names = FALSE)





#Missing smoking
#DATA<-DATA[!is.na(DATA$SMOKING),]

#Not available for death linkage
#DATA<-DATA[DATA$ELIGSTAT!=3,]



#
#DATA<-DATA[DATA$RIDAGEYR>=20&DATA$RIDAGEYR<=79&DATA$ELIGSTAT!=3,]

#as.data.frame(colSums(is.na(DATA)))
#write.csv(DATA,"DATA/Combined DATA/DATA.csv", row.names = FALSE)









# DATA<-DATA[!is.na(DATA$ALQ130),]
# DATA<-DATA[!is.na(DATA$TKCAL),]
# DATA<-DATA[!is.na(DATA$INDFMIN2),]
# DATA<-DATA[!is.na(DATA$INDFMPIR),]
# DATA<-DATA[!is.na(DATA$MORTSTAT),]
# DATA<-DATA[!is.na(DATA$BMXBMI),]
# DATA<-DATA[!is.na(DATA$BPQ080),]
# DATA<-DATA[!is.na(DATA$OCQ180),]
# DATA<-DATA[!is.na(DATA$BPXSY),]
# DATA<-DATA[!is.na(DATA$DPQ020),]
# DATA<-DATA[!is.na(DATA$SLD010H),]
# DATA<-DATA[!is.na(DATA$DSDS),]
# DATA<-DATA[!is.na(DATA$SMOKING),]
# DATA<-DATA[!is.na(DATA$DMDEDUC2),]
# DATA<-DATA[!is.na(DATA$DMDMARTL),]
# DATA<-DATA[!is.na(DATA$MCQ300A),]
# DATA<-DATA[!is.na(DATA$MCQ160F.1),]
# DATA<-DATA[!is.na(DATA$MCQ300C),]
# DATA<-DATA[!is.na(DATA$MCQ160C),]
# DATA<-DATA[!is.na(DATA$MCQ220),]
# DATA<-DATA[!is.na(DATA$DRQSDIET),]
# DATA<-DATA[!is.na(DATA$BPQ020),]
# DATA<-DATA[!is.na(DATA$DIQ010),]
# 
# as.data.frame(colSums(is.na(DATA)))
# 
# WOMEN<-DATA[DATA$RIAGENDR==2,]
# as.data.frame(colSums(is.na(WOMEN)))
