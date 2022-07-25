#Install packages
require(SASxport)
library(dplyr)
library(readr)

#DR1TOT
DR1TOT_E <- read.xport("DATA/DR1TOT Dietary Interview - Total Nutrient Intakes, First Day/DR1TOT_E.XPT")
DR1TOT_F <- read.xport("DATA/DR1TOT Dietary Interview - Total Nutrient Intakes, First Day/DR1TOT_F.XPT")
DR1TOT_G <- read.xport("DATA/DR1TOT Dietary Interview - Total Nutrient Intakes, First Day/DR1TOT_G.XPT")
DR1TOT_H <- read.xport("DATA/DR1TOT Dietary Interview - Total Nutrient Intakes, First Day/DR1TOT_H.XPT")
DR1TOT_I <- read.xport("DATA/DR1TOT Dietary Interview - Total Nutrient Intakes, First Day/DR1TOT_I.XPT")
myvars <- c("SEQN","DR1TKCAL", "DR1TCARB", "DR1TFIBE", "DR1TSFAT", "DR1TMFAT", "DR1TPFAT", "DRQSDIET", "DR1TCHOL", "DR1TMAGN")
DR1TOT_E <- DR1TOT_E[myvars]
DR1TOT_F <- DR1TOT_F[myvars]
DR1TOT_G <- DR1TOT_G[myvars]
DR1TOT_H <- DR1TOT_H[myvars]
DR1TOT_I <- DR1TOT_I[myvars]
DR1TOT<-rbind(DR1TOT_E,DR1TOT_F,DR1TOT_G,DR1TOT_H,DR1TOT_I)
remove(DR1TOT_E)
remove(DR1TOT_F)
remove(DR1TOT_G)
remove(DR1TOT_H)
remove(DR1TOT_I)

#DR2TOT
DR2TOT_E <- read.xport("DATA/DR2TOT Dietary Interview - Total Nutrient Intakes, Second Day/DR2TOT_E.XPT")
DR2TOT_F <- read.xport("DATA/DR2TOT Dietary Interview - Total Nutrient Intakes, Second Day/DR2TOT_F.XPT")
DR2TOT_G <- read.xport("DATA/DR2TOT Dietary Interview - Total Nutrient Intakes, Second Day/DR2TOT_G.XPT")
DR2TOT_H <- read.xport("DATA/DR2TOT Dietary Interview - Total Nutrient Intakes, Second Day/DR2TOT_H.XPT")
DR2TOT_I <- read.xport("DATA/DR2TOT Dietary Interview - Total Nutrient Intakes, Second Day/DR2TOT_I.XPT")
myvars <- c("SEQN","DR2TKCAL", "DR2TCARB", "DR2TFIBE", "DR2TSFAT", "DR2TMFAT", "DR2TPFAT", "DR2TCHOL", "DR2TMAGN")
DR2TOT_E <- DR2TOT_E[myvars]
DR2TOT_F <- DR2TOT_F[myvars]
DR2TOT_G <- DR2TOT_G[myvars]
DR2TOT_H <- DR2TOT_H[myvars]
DR2TOT_I <- DR2TOT_I[myvars]
DR2TOT<-rbind(DR2TOT_E,DR2TOT_F,DR2TOT_G,DR2TOT_H,DR2TOT_I)
remove(DR2TOT_E)
remove(DR2TOT_F)
remove(DR2TOT_G)
remove(DR2TOT_H)
remove(DR2TOT_I)


names(DR1TOT) <- c("SEQN","TKCAL", "TCARB", "TFIBE", "TSFAT", "TMFAT", "TPFAT", "DRQSDIET", "TCHOL", "TMAGN")
names(DR2TOT) <- c("SEQN","TKCAL", "TCARB", "TFIBE", "TSFAT", "TMFAT", "TPFAT", "TCHOL", "TMAGN")
DR2TOT$DRQSDIET<-DR1TOT$DRQSDIET

TOT<-bind_rows(DR1TOT, DR2TOT) %>% group_by(SEQN) %>%summarise_all(funs(mean),na.rm=TRUE)

remove(DR1TOT)
remove(DR2TOT)












