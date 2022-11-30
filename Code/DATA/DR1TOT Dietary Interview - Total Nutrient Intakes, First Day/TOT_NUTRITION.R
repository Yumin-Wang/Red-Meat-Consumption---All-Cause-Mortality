#Install packages
require(SASxport)
library(dplyr)
library(readr)

#read total nutrition intake data for year 2007-2014 for day 1
DR1TOT_E <- read.xport("DATA/DR1TOT Dietary Interview - Total Nutrient Intakes, First Day/DR1TOT_E.XPT")
DR1TOT_F <- read.xport("DATA/DR1TOT Dietary Interview - Total Nutrient Intakes, First Day/DR1TOT_F.XPT")
DR1TOT_G <- read.xport("DATA/DR1TOT Dietary Interview - Total Nutrient Intakes, First Day/DR1TOT_G.XPT")
DR1TOT_H <- read.xport("DATA/DR1TOT Dietary Interview - Total Nutrient Intakes, First Day/DR1TOT_H.XPT")
#collect variables of interests
myvars <- c("SEQN","DR1TKCAL", "DR1TCARB", "DR1TFIBE", "DR1TSFAT", "DR1TMFAT", "DR1TPFAT", "DRQSDIET", "DR1TCHOL", "DR1TMAGN")
DR1TOT_E <- DR1TOT_E[myvars]
DR1TOT_F <- DR1TOT_F[myvars]
DR1TOT_G <- DR1TOT_G[myvars]
DR1TOT_H <- DR1TOT_H[myvars]
#row bind each survey
DR1TOT<-rbind(DR1TOT_E,DR1TOT_F,DR1TOT_G,DR1TOT_H)
#remove each survey to save space
remove(DR1TOT_E)
remove(DR1TOT_F)
remove(DR1TOT_G)
remove(DR1TOT_H)


#read total nutrition intake data for year 2007-2014 for day 2
DR2TOT_E <- read.xport("DATA/DR2TOT Dietary Interview - Total Nutrient Intakes, Second Day/DR2TOT_E.XPT")
DR2TOT_F <- read.xport("DATA/DR2TOT Dietary Interview - Total Nutrient Intakes, Second Day/DR2TOT_F.XPT")
DR2TOT_G <- read.xport("DATA/DR2TOT Dietary Interview - Total Nutrient Intakes, Second Day/DR2TOT_G.XPT")
DR2TOT_H <- read.xport("DATA/DR2TOT Dietary Interview - Total Nutrient Intakes, Second Day/DR2TOT_H.XPT")
#collect variables of interests
myvars <- c("SEQN","DR2TKCAL", "DR2TCARB", "DR2TFIBE", "DR2TSFAT", "DR2TMFAT", "DR2TPFAT", "DR2TCHOL", "DR2TMAGN")
DR2TOT_E <- DR2TOT_E[myvars]
DR2TOT_F <- DR2TOT_F[myvars]
DR2TOT_G <- DR2TOT_G[myvars]
DR2TOT_H <- DR2TOT_H[myvars]
#row bind each survey
DR2TOT<-rbind(DR2TOT_E,DR2TOT_F,DR2TOT_G,DR2TOT_H)
#remove each survey to save space
remove(DR2TOT_E)
remove(DR2TOT_F)
remove(DR2TOT_G)
remove(DR2TOT_H)

#recreate names for day 1 and day 2
names(DR1TOT) <- c("SEQN","TKCAL", "TCARB", "TFIBE", "TSFAT", "TMFAT", "TPFAT", "DRQSDIET", "TCHOL", "TMAGN")
names(DR2TOT) <- c("SEQN","TKCAL", "TCARB", "TFIBE", "TSFAT", "TMFAT", "TPFAT", "TCHOL", "TMAGN")
#create the same DRQSDIET (special diet) variable for day2 from day1
DR2TOT$DRQSDIET<-DR1TOT$DRQSDIET
#row bind day 1 and day 2 and group by participants and get averaged value for each variables of interests.
TOT<-bind_rows(DR1TOT, DR2TOT) %>% group_by(SEQN) %>%summarise_all(funs(mean),na.rm=TRUE)
#remove day1 and day2 to save space
remove(DR1TOT)
remove(DR2TOT)
#covert NaN to NA
is.nan.data.frame <- function(x){do.call(cbind, lapply(x, is.nan))}
TOT[is.nan(TOT)]<-NA
#Subject don't know treated as missing
TOT$DRQSDIET[TOT$DRQSDIET==9]<-NA










