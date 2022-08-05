require(haven)
#Install packages
require(SASxport)
library(dplyr)
library(readr)
DR1IFF_E <- read_sas("DATA/FPED_DR1IFF - Individual food, First day/fped_dr1iff_0708.sas7bdat")
DR1IFF_F <- read_sas("DATA/FPED_DR1IFF - Individual food, First day/fped_dr1iff_0910.sas7bdat")
DR1IFF_G <- read_sas("DATA/FPED_DR1IFF - Individual food, First day/fped_dr1iff_1112.sas7bdat")
DR1IFF_H <- read_sas("DATA/FPED_DR1IFF - Individual food, First day/fped_dr1iff_1314.sas7bdat")

myvars <- c("SEQN","DR1I_PF_MEAT","DESCRIPTION")

DR1IFF_E <- DR1IFF_E[myvars]
DR1IFF_F <- DR1IFF_F[myvars]
DR1IFF_G <- DR1IFF_G[myvars]
DR1IFF_H <- DR1IFF_H[myvars]

DR1IFF<-rbind(DR1IFF_E,DR1IFF_F,DR1IFF_G,DR1IFF_H)
remove(DR1IFF_E)
remove(DR1IFF_F)
remove(DR1IFF_G)
remove(DR1IFF_H)

beef_veal<-dplyr::filter(DR1IFF, grepl('(?i)BEEF|VEAL', DR1IFF$DESCRIPTION))
beef_veal_pork<-dplyr::filter(DR1IFF, grepl('(?i)BEEF|VEAL|PORK', DR1IFF$DESCRIPTION))
beef_veal_lamb<-dplyr::filter(DR1IFF, grepl('(?i)BEEF|VEAL|LAMB', DR1IFF$DESCRIPTION))
beef_veal_pork_lamb<-dplyr::filter(DR1IFF, grepl('(?i)BEEF|VEAL|LAMB|PORK', DR1IFF$DESCRIPTION))


wo_wo<-c("BEEF","Beef","BEef","beef","beeF")
grepl("(?i)BEEF", wo_wo)




