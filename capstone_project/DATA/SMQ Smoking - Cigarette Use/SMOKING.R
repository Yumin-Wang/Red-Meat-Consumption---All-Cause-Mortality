#Install packages
require(SASxport)
library(dplyr)
library(readr)

#Read SMQ file
SMQ_E <- read.xport("DATA/SMQ Smoking - Cigarette Use/SMQ_E.XPT")
SMQ_F <- read.xport("DATA/SMQ Smoking - Cigarette Use/SMQ_F.XPT")
SMQ_G <- read.xport("DATA/SMQ Smoking - Cigarette Use/SMQ_G.XPT")
SMQ_H <- read.xport("DATA/SMQ Smoking - Cigarette Use/SMQ_H.XPT")

myvars <- c("SEQN","SMQ040", "SMD650")
SMQ_E <- SMQ_E[myvars]
SMQ_F <- SMQ_F[myvars]
SMQ_G <- SMQ_G[myvars]
SMQ_H <- SMQ_H[myvars]

SMQ<-rbind(SMQ_E,SMQ_F,SMQ_G,SMQ_H)
remove(SMQ_E)
remove(SMQ_F)
remove(SMQ_G)
remove(SMQ_H)
