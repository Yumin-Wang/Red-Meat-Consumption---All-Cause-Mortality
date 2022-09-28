#Install packages
require(SASxport)
library(dplyr)
library(readr)

#Read Individual Dietary Supplements data for year 2007-2014 for day1
DS1IDS_E <- read.xport("DATA/DS1IDS Dietary Supplement Use 24-Hour - Individual Dietary Supplements, First Day/DS1IDS_E.XPT")
DS1IDS_F <- read.xport("DATA/DS1IDS Dietary Supplement Use 24-Hour - Individual Dietary Supplements, First Day/DS1IDS_F.XPT")
DS1IDS_G <- read.xport("DATA/DS1IDS Dietary Supplement Use 24-Hour - Individual Dietary Supplements, First Day/DS1IDS_G.XPT")
DS1IDS_H <- read.xport("DATA/DS1IDS Dietary Supplement Use 24-Hour - Individual Dietary Supplements, First Day/DS1IDS_H.XPT")
#read Product Information	
DSPI <-read.xport("DATA/DS1IDS Dietary Supplement Use 24-Hour - Individual Dietary Supplements, First Day/DSPI.XPT")

#Read Individual Dietary Supplements data for year 2007-2014 for day2
DS2IDS_E <- read.xport("DATA/DS2IDS Dietary Supplement Use 24-Hour - Individual Dietary Supplements, Second Day/DS2IDS_E.XPT")
DS2IDS_F <- read.xport("DATA/DS2IDS Dietary Supplement Use 24-Hour - Individual Dietary Supplements, Second Day/DS2IDS_F.XPT")
DS2IDS_G <- read.xport("DATA/DS2IDS Dietary Supplement Use 24-Hour - Individual Dietary Supplements, Second Day/DS2IDS_G.XPT")
DS2IDS_H <- read.xport("DATA/DS2IDS Dietary Supplement Use 24-Hour - Individual Dietary Supplements, Second Day/DS2IDS_H.XPT")

#collect variables of interests for year 2009-2014 for day 1 and day 2
myvars <- c("SEQN","DSDSUPP")
DS1IDS_F <- DS1IDS_F[myvars]
DS1IDS_G <- DS1IDS_G[myvars]
DS1IDS_H <- DS1IDS_H[myvars]
DS2IDS_F <- DS2IDS_F[myvars]
DS2IDS_G <- DS2IDS_G[myvars]
DS2IDS_H <- DS2IDS_H[myvars]

#for year 2007-2008, merge day 1 and day 2 with product info respectively and collect variables of interests
DS1IDS_E <- merge(DS1IDS_E,DSPI,by="DSDSUPID",all.x=TRUE)
DS2IDS_E <- merge(DS2IDS_E,DSPI,by="DSDSUPID",all.x=TRUE)
DS1IDS_E <- DS1IDS_E[myvars]
DS2IDS_E <- DS2IDS_E[myvars]

#row bind each survey for day 1
DS1IDS<-rbind(DS1IDS_E,DS1IDS_F,DS1IDS_G,DS1IDS_H)
#remove each survey to save space
remove(DS1IDS_E)
remove(DS1IDS_F)
remove(DS1IDS_G)
remove(DS1IDS_H)

#row bind each survey for day 2
DS2IDS<-rbind(DS2IDS_E,DS2IDS_F,DS2IDS_G,DS2IDS_H)
#remove each survey to save space
remove(DS2IDS_E)
remove(DS2IDS_F)
remove(DS2IDS_G)
remove(DS2IDS_H)
#remove product info
remove(DSPI)

#row bind day1 and day2
DSIDS <- rbind(DS1IDS,DS2IDS)
#create a multivitamin variable to denote each record is a vitamin or not.
DSIDS$MULTIVITAMIN <- ifelse(grepl('*VITAMIN*', DSIDS$DSDSUPP),1,0)
#only keep multivitamin and sequence columns
DSIDS <- DSIDS[c('SEQN',"MULTIVITAMIN")]
#identify multivitamin users
DSIDS <- DSIDS %>% group_by(SEQN) %>%summarise_all(funs(mean),na.rm=TRUE)
DSIDS$MULTIVITAMIN <- ceiling(DSIDS$MULTIVITAMIN)
#remove data to save space
remove(DS1IDS)
remove(DS2IDS)


