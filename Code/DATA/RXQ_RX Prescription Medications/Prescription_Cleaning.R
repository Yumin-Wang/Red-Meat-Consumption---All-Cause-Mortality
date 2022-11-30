#Install packages
require(SASxport)
library(dplyr)
library(readr)

#Read prescription meadication data for year 2007-2014
RXQ_RX_E <- read.xport("DATA/RXQ_RX Prescription Medications/RXQ_RX_E.XPT")
RXQ_RX_F <- read.xport("DATA/RXQ_RX Prescription Medications/RXQ_RX_F.XPT")
RXQ_RX_G <- read.xport("DATA/RXQ_RX Prescription Medications/RXQ_RX_G.XPT")
RXQ_RX_H <- read.xport("DATA/RXQ_RX Prescription Medications/RXQ_RX_H.XPT")

#collect variable of interest
myvars <- c("SEQN","RXDDRUG")
RXQ_RX_E <- RXQ_RX_E[myvars]
RXQ_RX_F <- RXQ_RX_F[myvars]
RXQ_RX_G <- RXQ_RX_G[myvars]
RXQ_RX_H <- RXQ_RX_H[myvars]

#row bind each survey
RXQ_RX<-rbind(RXQ_RX_E,RXQ_RX_F,RXQ_RX_G,RXQ_RX_H)
#remove each survey to save space
remove(RXQ_RX_E)
remove(RXQ_RX_F)
remove(RXQ_RX_G)
remove(RXQ_RX_H)

#filter for records of aspirin use and identify aspirin user
ASPIRIN_USER<-dplyr::filter(RXQ_RX, grepl('*ASPIRIN*', RXQ_RX$RXDDRUG))
ASPIRIN_USER$ASPIRIN<-1
ASPIRIN_USER <- ASPIRIN_USER[c("SEQN","ASPIRIN")]
ASPIRIN_USER <- ASPIRIN_USER %>% distinct()

#filter for records of statin use and identify statin user
STATIN_USER<-dplyr::filter(RXQ_RX, grepl('*STATIN*', RXQ_RX$RXDDRUG))
STATIN_USER$STATIN<-1
STATIN_USER <- STATIN_USER[c("SEQN","STATIN")]
STATIN_USER <- STATIN_USER %>% distinct()

#filter for records of opium use and identify opium user
OPIUM_USER<-dplyr::filter(RXQ_RX, grepl('*OPIUM*', RXQ_RX$RXDDRUG))
OPIUM_USER$OPIUM<-1
OPIUM_USER <- OPIUM_USER[c("SEQN","OPIUM")]
OPIUM_USER <- OPIUM_USER %>% distinct()

#filter for records of atorvastatin use and identify atorvastatin user
ATORVASTATIN_USER<-dplyr::filter(RXQ_RX, grepl('*ATORVASTATIN*', RXQ_RX$RXDDRUG))
ATORVASTATIN_USER$ATORVASTATIN<-1
ATORVASTATIN_USER <- ATORVASTATIN_USER[c("SEQN","ATORVASTATIN")]
ATORVASTATIN_USER <- ATORVASTATIN_USER %>% distinct()

#filter for records of valsartan use and identify valsartan user
VALSARTAN_USER<-dplyr::filter(RXQ_RX, grepl('*VALSARTAN*', RXQ_RX$RXDDRUG))
VALSARTAN_USER$VALSARTAN<-1
VALSARTAN_USER <- VALSARTAN_USER[c("SEQN","VALSARTAN")]
VALSARTAN_USER <- VALSARTAN_USER %>% distinct()

#filter for records of ibuprofen use and identify ibuprofen user
IBUPROFEN_USER<-dplyr::filter(RXQ_RX, grepl('*IBUPROFEN*', RXQ_RX$RXDDRUG))
IBUPROFEN_USER$IBUPROFEN<-1
IBUPROFEN_USER <- IBUPROFEN_USER[c("SEQN","IBUPROFEN")]
IBUPROFEN_USER <- IBUPROFEN_USER %>% distinct()

#merge all data
PRESCRIPTION<-merge(ASPIRIN_USER,ATORVASTATIN_USER,all=TRUE)
PRESCRIPTION<-merge(PRESCRIPTION,IBUPROFEN_USER,all=TRUE)
PRESCRIPTION<-merge(PRESCRIPTION,OPIUM_USER,all=TRUE)
PRESCRIPTION<-merge(PRESCRIPTION,STATIN_USER,all=TRUE)
PRESCRIPTION<-merge(PRESCRIPTION,VALSARTAN_USER,all=TRUE)
#remove individual medicine data set to save space
remove(ASPIRIN_USER)
remove(ATORVASTATIN_USER)
remove(IBUPROFEN_USER)
remove(OPIUM_USER)
remove(RXQ_RX)
remove(STATIN_USER)
remove(VALSARTAN_USER)

