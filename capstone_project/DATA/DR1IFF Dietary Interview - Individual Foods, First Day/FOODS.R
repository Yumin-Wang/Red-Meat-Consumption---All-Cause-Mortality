#Install packages
require(SASxport)
library(dplyr)
library(readr)

#DR1IFF
DR1IFF_E <- read.xport("DATA/DR1IFF Dietary Interview - Individual Foods, First Day/DR1IFF_E.XPT")
DR1IFF_F <- read.xport("DATA/DR1IFF Dietary Interview - Individual Foods, First Day/DR1IFF_F.XPT")
DR1IFF_G <- read.xport("DATA/DR1IFF Dietary Interview - Individual Foods, First Day/DR1IFF_G.XPT")
DR1IFF_H <- read.xport("DATA/DR1IFF Dietary Interview - Individual Foods, First Day/DR1IFF_H.XPT")
DR1IFF_I <- read.xport("DATA/DR1IFF Dietary Interview - Individual Foods, First Day/DR1IFF_I.XPT")

DRXFCD_E <- read.xport("DATA/DRXFCD Dietary Interview Technical Support File - Food Codes/DRXFCD_E.XPT")
