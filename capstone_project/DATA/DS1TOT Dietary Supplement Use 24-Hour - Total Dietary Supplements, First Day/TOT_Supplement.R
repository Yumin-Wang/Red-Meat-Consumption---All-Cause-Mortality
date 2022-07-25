#Install packages
require(SASxport)
library(dplyr)
library(readr)

#DS1TOT
DS1TOT_E <- read.xport("DATA/DS1TOT Dietary Supplement Use 24-Hour - Total Dietary Supplements, First Day/DS1TOT_E.XPT")
DS1TOT_F <- read.xport("DATA/DS1TOT Dietary Supplement Use 24-Hour - Total Dietary Supplements, First Day/DS1TOT_F.XPT")
DS1TOT_G <- read.xport("DATA/DS1TOT Dietary Supplement Use 24-Hour - Total Dietary Supplements, First Day/DS1TOT_G.XPT")
DS1TOT_H <- read.xport("DATA/DS1TOT Dietary Supplement Use 24-Hour - Total Dietary Supplements, First Day/DS1TOT_H.XPT")
DS1TOT_I <- read.xport("DATA/DS1TOT Dietary Supplement Use 24-Hour - Total Dietary Supplements, First Day/DS1TOT_I.XPT")
myvars <- c("SEQN","DS1DS")
DS1TOT_E <- DS1TOT_E[myvars]
DS1TOT_F <- DS1TOT_F[myvars]
DS1TOT_G <- DS1TOT_G[myvars]
DS1TOT_H <- DS1TOT_H[myvars]
DS1TOT_I <- DS1TOT_I[myvars]
DS1TOT<-rbind(DS1TOT_E,DS1TOT_F,DS1TOT_G,DS1TOT_H,DS1TOT_I)
remove(DS1TOT_E)
remove(DS1TOT_F)
remove(DS1TOT_G)
remove(DS1TOT_H)
remove(DS1TOT_I)

#DS2TOT
DS2TOT_E <- read.xport("DATA/DS2TOT Dietary Supplement Use 24-Hour - Total Dietary Supplements, Second Day/DS2TOT_E.XPT")
DS2TOT_F <- read.xport("DATA/DS2TOT Dietary Supplement Use 24-Hour - Total Dietary Supplements, Second Day/DS2TOT_F.XPT")
DS2TOT_G <- read.xport("DATA/DS2TOT Dietary Supplement Use 24-Hour - Total Dietary Supplements, Second Day/DS2TOT_G.XPT")
DS2TOT_H <- read.xport("DATA/DS2TOT Dietary Supplement Use 24-Hour - Total Dietary Supplements, Second Day/DS2TOT_H.XPT")
DS2TOT_I <- read.xport("DATA/DS2TOT Dietary Supplement Use 24-Hour - Total Dietary Supplements, Second Day/DS2TOT_I.XPT")
myvars <- c("SEQN","DS2DS")
DS2TOT_E <- DS2TOT_E[myvars]
DS2TOT_F <- DS2TOT_F[myvars]
DS2TOT_G <- DS2TOT_G[myvars]
DS2TOT_H <- DS2TOT_H[myvars]
DS2TOT_I <- DS2TOT_I[myvars]
DS2TOT<-rbind(DS2TOT_E,DS2TOT_F,DS2TOT_G,DS2TOT_H,DS2TOT_I)
remove(DS2TOT_E)
remove(DS2TOT_F)
remove(DS2TOT_G)
remove(DS2TOT_H)
remove(DS2TOT_I)

names(DS1TOT) <- c("SEQN","DSDS")
names(DS2TOT) <- c("SEQN","DSDS")                 

TOT_supp<-bind_rows(DS1TOT, DS2TOT) %>% group_by(SEQN) %>%summarise_all(funs(mean),na.rm=TRUE)                   
TOT_supp$DSDS<-floor(TOT_supp$DSDS)                   

remove(DS1TOT)
remove(DS2TOT)














