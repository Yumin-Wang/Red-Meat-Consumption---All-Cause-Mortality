#Install packages
require(SASxport)
library(dplyr)
library(readr)

#Read smoking habit data for year 2007-2014
SMQ_E <- read.xport("DATA/SMQ Smoking - Cigarette Use/SMQ_E.XPT")
SMQ_F <- read.xport("DATA/SMQ Smoking - Cigarette Use/SMQ_F.XPT")
SMQ_G <- read.xport("DATA/SMQ Smoking - Cigarette Use/SMQ_G.XPT")
SMQ_H <- read.xport("DATA/SMQ Smoking - Cigarette Use/SMQ_H.XPT")

#collect variables of interests
myvars <- c("SEQN", "SMQ020","SMD030","SMQ040","SMD641","SMD650")
SMQ_E <- SMQ_E[myvars]
SMQ_F <- SMQ_F[myvars]
SMQ_G <- SMQ_G[myvars]
SMQ_H <- SMQ_H[myvars]
#row bind each survey
SMQ<-rbind(SMQ_E,SMQ_F,SMQ_G,SMQ_H)
#remove each survey
remove(SMQ_E)
remove(SMQ_F)
remove(SMQ_G)
remove(SMQ_H)
#create smoking variable
SMQ$SMOKING<-NA
#People who answered they never smoked cigarettes regularly or not smoked at least 100 cigarettes in life are categorized as Non or light smoker
SMQ$SMOKING[SMQ$SMD030==0 | SMQ$SMQ020 == 2]<-0
#People who answered they smoked at least 100 cigarettes in life but quit are categorized as non or light smoker.
SMQ$SMOKING[SMQ$SMQ040==3 & SMQ$SMQ020 == 1]<-0
#Subject refused to answer or don’t know treated as missing
SMQ$SMD641[SMQ$SMD641==77|SMQ$SMD641==99]<-NA
SMQ$SMD650[SMQ$SMD650==777|SMQ$SMD650==999]<-NA
#create a new variable to calculate cigarettes per month.
SMQ$CPERMONTH<-SMQ$SMD641*SMQ$SMD650
#People who answered they smoked at least 100 cigarettes in life and continue smoking are categorized as follows: if >67 cigarettes (40 pack year) then heavily smoker, if <=67 cigarettes (40 pack year) then moderate smoker.
SMQ$SMOKING[SMQ$CPERMONTH<=67 &SMQ$SMQ020==1 & (SMQ$SMQ040==1 | SMQ$SMQ040==2)]<-1
SMQ$SMOKING[SMQ$CPERMONTH>67 &SMQ$SMQ020==1 & (SMQ$SMQ040==1 | SMQ$SMQ040==2)]<-2
#collect variables of interests
SMQ<-SMQ[,c("SEQN","SMOKING")]



