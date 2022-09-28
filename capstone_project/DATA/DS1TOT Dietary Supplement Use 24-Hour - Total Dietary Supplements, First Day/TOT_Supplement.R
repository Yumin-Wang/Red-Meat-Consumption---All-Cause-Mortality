#Install packages
require(SASxport)
library(dplyr)
library(readr)

#read total dietary supplement use data for year 2007-2014 for day 1
DS1TOT_E <- read.xport("DATA/DS1TOT Dietary Supplement Use 24-Hour - Total Dietary Supplements, First Day/DS1TOT_E.XPT")
DS1TOT_F <- read.xport("DATA/DS1TOT Dietary Supplement Use 24-Hour - Total Dietary Supplements, First Day/DS1TOT_F.XPT")
DS1TOT_G <- read.xport("DATA/DS1TOT Dietary Supplement Use 24-Hour - Total Dietary Supplements, First Day/DS1TOT_G.XPT")
DS1TOT_H <- read.xport("DATA/DS1TOT Dietary Supplement Use 24-Hour - Total Dietary Supplements, First Day/DS1TOT_H.XPT")
#collect variable of interests
myvars <- c("SEQN","DS1DS")
DS1TOT_E <- DS1TOT_E[myvars]
DS1TOT_F <- DS1TOT_F[myvars]
DS1TOT_G <- DS1TOT_G[myvars]
DS1TOT_H <- DS1TOT_H[myvars]
#row bind each survey
DS1TOT<-rbind(DS1TOT_E,DS1TOT_F,DS1TOT_G,DS1TOT_H)
#remove each survey to save space
remove(DS1TOT_E)
remove(DS1TOT_F)
remove(DS1TOT_G)
remove(DS1TOT_H)


#read dietary supplement intake data for year 2007-2014 for day 2
DS2TOT_E <- read.xport("DATA/DS2TOT Dietary Supplement Use 24-Hour - Total Dietary Supplements, Second Day/DS2TOT_E.XPT")
DS2TOT_F <- read.xport("DATA/DS2TOT Dietary Supplement Use 24-Hour - Total Dietary Supplements, Second Day/DS2TOT_F.XPT")
DS2TOT_G <- read.xport("DATA/DS2TOT Dietary Supplement Use 24-Hour - Total Dietary Supplements, Second Day/DS2TOT_G.XPT")
DS2TOT_H <- read.xport("DATA/DS2TOT Dietary Supplement Use 24-Hour - Total Dietary Supplements, Second Day/DS2TOT_H.XPT")
#collect variable of interest
myvars <- c("SEQN","DS2DS")
DS2TOT_E <- DS2TOT_E[myvars]
DS2TOT_F <- DS2TOT_F[myvars]
DS2TOT_G <- DS2TOT_G[myvars]
DS2TOT_H <- DS2TOT_H[myvars]
#row bind each survey
DS2TOT<-rbind(DS2TOT_E,DS2TOT_F,DS2TOT_G,DS2TOT_H)
#remove each survey to save space
remove(DS2TOT_E)
remove(DS2TOT_F)
remove(DS2TOT_G)
remove(DS2TOT_H)

#rename column names in day1 and day2
names(DS1TOT) <- c("SEQN","DSDS")
names(DS2TOT) <- c("SEQN","DSDS")                 

#For dietary supplement use, if either day 1 or day 2 they used dietary supplement, then they are using dietary supplement overall. If neither day 1 and day 2 they used dietary supplement, they are not using dietary supplement. If one is missing, the un-missing value will be used. If both missing, then missing
TOT_supp<-bind_rows(DS1TOT, DS2TOT) %>% group_by(SEQN) %>%summarise_all(funs(mean),na.rm=TRUE)                   
TOT_supp$DSDS<-floor(TOT_supp$DSDS)                   
#remove day1 and day2 to save space
remove(DS1TOT)
remove(DS2TOT)
#covert NaN to NA
TOT_supp$DSDS[is.nan(TOT_supp$DSDS)]<-NA













