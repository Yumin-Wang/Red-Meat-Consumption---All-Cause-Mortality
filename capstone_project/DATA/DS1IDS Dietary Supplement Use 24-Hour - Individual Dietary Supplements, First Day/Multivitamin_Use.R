#Install packages
require(SASxport)
library(dplyr)
library(readr)

#DS1IDS
DS1IDS_E <- read.xport("DATA/DS1IDS Dietary Supplement Use 24-Hour - Individual Dietary Supplements, First Day/DS1IDS_E.XPT")
DS1IDS_F <- read.xport("DATA/DS1IDS Dietary Supplement Use 24-Hour - Individual Dietary Supplements, First Day/DS1IDS_F.XPT")
DS1IDS_G <- read.xport("DATA/DS1IDS Dietary Supplement Use 24-Hour - Individual Dietary Supplements, First Day/DS1IDS_G.XPT")
DS1IDS_H <- read.xport("DATA/DS1IDS Dietary Supplement Use 24-Hour - Individual Dietary Supplements, First Day/DS1IDS_H.XPT")
DS1IDS_I <- read.xport("DATA/DS1IDS Dietary Supplement Use 24-Hour - Individual Dietary Supplements, First Day/DS1IDS_I.XPT")
DSPI <-read.xport("DATA/DS1IDS Dietary Supplement Use 24-Hour - Individual Dietary Supplements, First Day/DSPI.XPT")

#DS2IDS
DS2IDS_E <- read.xport("DATA/DS2IDS Dietary Supplement Use 24-Hour - Individual Dietary Supplements, Second Day/DS2IDS_E.XPT")
DS2IDS_F <- read.xport("DATA/DS2IDS Dietary Supplement Use 24-Hour - Individual Dietary Supplements, Second Day/DS2IDS_F.XPT")
DS2IDS_G <- read.xport("DATA/DS2IDS Dietary Supplement Use 24-Hour - Individual Dietary Supplements, Second Day/DS2IDS_G.XPT")
DS2IDS_H <- read.xport("DATA/DS2IDS Dietary Supplement Use 24-Hour - Individual Dietary Supplements, Second Day/DS2IDS_H.XPT")
DS2IDS_I <- read.xport("DATA/DS2IDS Dietary Supplement Use 24-Hour - Individual Dietary Supplements, Second Day/DS2IDS_I.XPT")

myvars <- c("SEQN","DSDSUPP")
DS1IDS_F <- DS1IDS_F[myvars]
DS1IDS_G <- DS1IDS_G[myvars]
DS1IDS_H <- DS1IDS_H[myvars]
DS1IDS_I <- DS1IDS_I[myvars]
DS2IDS_F <- DS2IDS_F[myvars]
DS2IDS_G <- DS2IDS_G[myvars]
DS2IDS_H <- DS2IDS_H[myvars]
DS2IDS_I <- DS2IDS_I[myvars]

DS1IDS_E <- merge(DS1IDS_E,DSPI,by="DSDSUPID",all.x=TRUE)
DS2IDS_E <- merge(DS2IDS_E,DSPI,by="DSDSUPID",all.x=TRUE)
DS1IDS_E <- DS1IDS_E[myvars]
DS2IDS_E <- DS2IDS_E[myvars]

DS1IDS<-rbind(DS1IDS_E,DS1IDS_F,DS1IDS_G,DS1IDS_H,DS1IDS_I)
remove(DS1IDS_E)
remove(DS1IDS_F)
remove(DS1IDS_G)
remove(DS1IDS_H)
remove(DS1IDS_I)

DS2IDS<-rbind(DS2IDS_E,DS2IDS_F,DS2IDS_G,DS2IDS_H,DS2IDS_I)
remove(DS2IDS_E)
remove(DS2IDS_F)
remove(DS2IDS_G)
remove(DS2IDS_H)
remove(DS2IDS_I)
remove(DSPI)

DSIDS <- rbind(DS1IDS,DS2IDS)
DSIDS$MULTIVITAMIN <- ifelse(grepl('*VITAMIN*', DSIDS$DSDSUPP),1,0)
DSIDS <- DSIDS[c('SEQN',"MULTIVITAMIN")]
DSIDS <- DSIDS %>% group_by(SEQN) %>%summarise_all(funs(mean),na.rm=TRUE)
DSIDS$MULTIVITAMIN <- ceiling(DSIDS$MULTIVITAMIN)
remove(DS1IDS)
remove(DS2IDS)


