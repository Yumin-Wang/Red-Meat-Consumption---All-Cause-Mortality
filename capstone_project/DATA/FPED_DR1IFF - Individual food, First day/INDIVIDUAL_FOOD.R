#Install packages
require(haven)
require(SASxport)
library(dplyr)
library(readr)

#Read individual food for year 2007-2014 for day 1
DR1IFF_E <- read_sas("DATA/FPED_DR1IFF - Individual food, First day/fped_dr1iff_0708.sas7bdat")
DR1IFF_F <- read_sas("DATA/FPED_DR1IFF - Individual food, First day/fped_dr1iff_0910.sas7bdat")
DR1IFF_G <- read_sas("DATA/FPED_DR1IFF - Individual food, First day/fped_dr1iff_1112.sas7bdat")
DR1IFF_H <- read_sas("DATA/FPED_DR1IFF - Individual food, First day/fped_dr1iff_1314.sas7bdat")

#collect variables of interests
myvars <- c("SEQN","DR1I_PF_MEAT","DESCRIPTION")

DR1IFF_E <- DR1IFF_E[myvars]
DR1IFF_F <- DR1IFF_F[myvars]
DR1IFF_G <- DR1IFF_G[myvars]
DR1IFF_H <- DR1IFF_H[myvars]

#row bind each survey for day1
DR1IFF<-rbind(DR1IFF_E,DR1IFF_F,DR1IFF_G,DR1IFF_H)
#remove each survey for day1 to save space
remove(DR1IFF_E)
remove(DR1IFF_F)
remove(DR1IFF_G)
remove(DR1IFF_H)

#filter for different definition of unprocessed red meat
beef_veal_day1<-dplyr::filter(DR1IFF, grepl('(?i)BEEF|VEAL', DR1IFF$DESCRIPTION))
beef_veal_pork_day1<-dplyr::filter(DR1IFF, grepl('(?i)BEEF|VEAL|PORK', DR1IFF$DESCRIPTION))
beef_veal_lamb_day1<-dplyr::filter(DR1IFF, grepl('(?i)BEEF|VEAL|LAMB', DR1IFF$DESCRIPTION))
beef_veal_pork_lamb_day1<-dplyr::filter(DR1IFF, grepl('(?i)BEEF|VEAL|LAMB|PORK', DR1IFF$DESCRIPTION))

#remove day 1 data
remove(DR1IFF)

#only keep sequence number and meat variable for different definition of unprocessed red meat datasets
beef_veal_day1<-beef_veal_day1[,c("SEQN","DR1I_PF_MEAT")]
beef_veal_pork_day1<-beef_veal_pork_day1[,c("SEQN","DR1I_PF_MEAT")]
beef_veal_lamb_day1<-beef_veal_lamb_day1[,c("SEQN","DR1I_PF_MEAT")]
beef_veal_pork_lamb_day1<-beef_veal_pork_lamb_day1[,c("SEQN","DR1I_PF_MEAT")]

#rename columns
names(beef_veal_day1)<-c("SEQN","BEEF_VEAL")
names(beef_veal_pork_day1)<-c("SEQN","BEEF_VEAL_PORK")
names(beef_veal_lamb_day1)<-c("SEQN","BEEF_VEAL_LAMB")
names(beef_veal_pork_lamb_day1)<-c("SEQN","BEEF_VEAL_PORK_LAMB")

#aggregate meat consumption by sequence number for different definition of unprocessed red meat
beef_veal_day1<-beef_veal_day1 %>% group_by(SEQN) %>%summarise_all(funs(sum),na.rm=TRUE)
beef_veal_pork_day1<-beef_veal_pork_day1 %>% group_by(SEQN) %>%summarise_all(funs(sum),na.rm=TRUE)
beef_veal_lamb_day1<-beef_veal_lamb_day1 %>% group_by(SEQN) %>%summarise_all(funs(sum),na.rm=TRUE)
beef_veal_pork_lamb_day1<-beef_veal_pork_lamb_day1 %>% group_by(SEQN) %>%summarise_all(funs(sum),na.rm=TRUE)

#merge to get a combine dataset for day 1 containing consumption of different definition of unprocessed red meat
DR1IFF<-merge(beef_veal_day1,beef_veal_lamb_day1,all=TRUE)
DR1IFF<-merge(DR1IFF,beef_veal_pork_day1,all=TRUE)
DR1IFF<-merge(DR1IFF,beef_veal_pork_lamb_day1,all=TRUE)

#If consumption is missing for a particular definition but have value for other definition, it will be 0 because we have not observed records in that definition but it is definitely not missing data. 
DR1IFF[is.na(DR1IFF)]<-0
#remove data to save space
remove(beef_veal_day1)
remove(beef_veal_lamb_day1)
remove(beef_veal_pork_day1)
remove(beef_veal_pork_lamb_day1)

#Do the same for day 2
#Day 2#
DR2IFF_E <- read_sas("DATA/FPED_DR2IFF - Individual food, Second day/fped_DR2IFF_0708.sas7bdat")
DR2IFF_F <- read_sas("DATA/FPED_DR2IFF - Individual food, Second day/fped_DR2IFF_0910.sas7bdat")
DR2IFF_G <- read_sas("DATA/FPED_DR2IFF - Individual food, Second day/fped_DR2IFF_1112.sas7bdat")
DR2IFF_H <- read_sas("DATA/FPED_DR2IFF - Individual food, Second day/fped_DR2IFF_1314.sas7bdat")

myvars <- c("SEQN","DR2I_PF_MEAT","DESCRIPTION")

DR2IFF_E <- DR2IFF_E[myvars]
DR2IFF_F <- DR2IFF_F[myvars]
DR2IFF_G <- DR2IFF_G[myvars]
DR2IFF_H <- DR2IFF_H[myvars]

DR2IFF<-rbind(DR2IFF_E,DR2IFF_F,DR2IFF_G,DR2IFF_H)
remove(DR2IFF_E)
remove(DR2IFF_F)
remove(DR2IFF_G)
remove(DR2IFF_H)

beef_veal_day2<-dplyr::filter(DR2IFF, grepl('(?i)BEEF|VEAL', DR2IFF$DESCRIPTION))
beef_veal_pork_day2<-dplyr::filter(DR2IFF, grepl('(?i)BEEF|VEAL|PORK', DR2IFF$DESCRIPTION))
beef_veal_lamb_day2<-dplyr::filter(DR2IFF, grepl('(?i)BEEF|VEAL|LAMB', DR2IFF$DESCRIPTION))
beef_veal_pork_lamb_day2<-dplyr::filter(DR2IFF, grepl('(?i)BEEF|VEAL|LAMB|PORK', DR2IFF$DESCRIPTION))

remove(DR2IFF)


beef_veal_day2<-beef_veal_day2[,c("SEQN","DR2I_PF_MEAT")]
beef_veal_pork_day2<-beef_veal_pork_day2[,c("SEQN","DR2I_PF_MEAT")]
beef_veal_lamb_day2<-beef_veal_lamb_day2[,c("SEQN","DR2I_PF_MEAT")]
beef_veal_pork_lamb_day2<-beef_veal_pork_lamb_day2[,c("SEQN","DR2I_PF_MEAT")]

names(beef_veal_day2)<-c("SEQN","BEEF_VEAL")
names(beef_veal_pork_day2)<-c("SEQN","BEEF_VEAL_PORK")
names(beef_veal_lamb_day2)<-c("SEQN","BEEF_VEAL_LAMB")
names(beef_veal_pork_lamb_day2)<-c("SEQN","BEEF_VEAL_PORK_LAMB")

beef_veal_day2<-beef_veal_day2 %>% group_by(SEQN) %>%summarise_all(funs(sum),na.rm=TRUE)
beef_veal_pork_day2<-beef_veal_pork_day2 %>% group_by(SEQN) %>%summarise_all(funs(sum),na.rm=TRUE)
beef_veal_lamb_day2<-beef_veal_lamb_day2 %>% group_by(SEQN) %>%summarise_all(funs(sum),na.rm=TRUE)
beef_veal_pork_lamb_day2<-beef_veal_pork_lamb_day2 %>% group_by(SEQN) %>%summarise_all(funs(sum),na.rm=TRUE)

DR2IFF<-merge(beef_veal_day2,beef_veal_lamb_day2,all=TRUE)
DR2IFF<-merge(DR2IFF,beef_veal_pork_day2,all=TRUE)
DR2IFF<-merge(DR2IFF,beef_veal_pork_lamb_day2,all=TRUE)

DR2IFF[is.na(DR2IFF)]<-0
remove(beef_veal_day2)
remove(beef_veal_lamb_day2)
remove(beef_veal_pork_day2)
remove(beef_veal_pork_lamb_day2)

#combine day1 and day2, group by sequence number and calculate average value
NEW_MEAT<-bind_rows(DR1IFF, DR2IFF) %>% group_by(SEQN) %>%summarise_all(funs(mean),na.rm=TRUE)

#remove data to save space
remove(DR1IFF)
remove(DR2IFF)




