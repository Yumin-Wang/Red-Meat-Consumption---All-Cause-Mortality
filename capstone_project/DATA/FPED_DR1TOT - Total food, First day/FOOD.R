require(haven)
DR1TOT_E <- read_sas("DATA/FPED_DR1TOT - Total food, First day/fped_dr1tot_0708.sas7bdat")
DR1TOT_F <- read_sas("DATA/FPED_DR1TOT - Total food, First day/fped_dr1tot_0910.sas7bdat")
DR1TOT_G <- read_sas("DATA/FPED_DR1TOT - Total food, First day/fped_dr1tot_1112.sas7bdat")
DR1TOT_H <- read_sas("DATA/FPED_DR1TOT - Total food, First day/fped_dr1tot_1314.sas7bdat")

myvars <- c("SEQN","DR1T_F_CITMLB","DR1T_F_OTHER","DR1T_V_TOTAL","DR1T_G_WHOLE","DR1T_PF_MPS_TOTAL",
            "DR1T_PF_MEAT","DR1T_PF_CUREDMEAT","DR1T_PF_POULT","DR1T_PF_SEAFD_HI","DR1T_PF_SEAFD_LOW","DR1T_PF_EGGS","DR1T_PF_NUTSDS","DR1T_PF_LEGUMES","DR1T_D_TOTAL",
            "DR1T_D_CHEESE")
DR1TOT_E <- DR1TOT_E[myvars]
DR1TOT_F <- DR1TOT_F[myvars]
DR1TOT_G <- DR1TOT_G[myvars]
DR1TOT_H <- DR1TOT_H[myvars]

DR1TOT<-rbind(DR1TOT_E,DR1TOT_F,DR1TOT_G,DR1TOT_H)
remove(DR1TOT_E)
remove(DR1TOT_F)
remove(DR1TOT_G)
remove(DR1TOT_H)

DR2TOT_E <- read_sas("DATA/FPED_DR2TOT - Total food, Second day/fped_DR2TOT_0708.sas7bdat")
DR2TOT_F <- read_sas("DATA/FPED_DR2TOT - Total food, Second day/fped_DR2TOT_0910.sas7bdat")
DR2TOT_G <- read_sas("DATA/FPED_DR2TOT - Total food, Second day/fped_DR2TOT_1112.sas7bdat")
DR2TOT_H <- read_sas("DATA/FPED_DR2TOT - Total food, Second day/fped_DR2TOT_1314.sas7bdat")

myvars <- c("SEQN","DR2T_F_CITMLB","DR2T_F_OTHER","DR2T_V_TOTAL","DR2T_G_WHOLE","DR2T_PF_MPS_TOTAL",
            "DR2T_PF_MEAT","DR2T_PF_CUREDMEAT","DR2T_PF_POULT","DR2T_PF_SEAFD_HI","DR2T_PF_SEAFD_LOW","DR2T_PF_EGGS","DR2T_PF_NUTSDS","DR2T_PF_LEGUMES","DR2T_D_TOTAL",
            "DR2T_D_CHEESE")
DR2TOT_E <- DR2TOT_E[myvars]
DR2TOT_F <- DR2TOT_F[myvars]
DR2TOT_G <- DR2TOT_G[myvars]
DR2TOT_H <- DR2TOT_H[myvars]

DR2TOT<-rbind(DR2TOT_E,DR2TOT_F,DR2TOT_G,DR2TOT_H)
remove(DR2TOT_E)
remove(DR2TOT_F)
remove(DR2TOT_G)
remove(DR2TOT_H)

names(DR1TOT) <- c("SEQN","F_CITMLB","F_OTHER","V_TOTAL","G_WHOLE","PF_MPS_TOTAL",
                   "PF_MEAT","PF_CUREDMEAT","PF_POULT","PF_SEAFD_HI","PF_SEAFD_LOW","PF_EGGS","PF_NUTSDS","PF_LEGUMES","D_TOTAL",
                   "D_CHEESE")
names(DR2TOT) <- c("SEQN","F_CITMLB","F_OTHER","V_TOTAL","G_WHOLE","PF_MPS_TOTAL",
                   "PF_MEAT","PF_CUREDMEAT","PF_POULT","PF_SEAFD_HI","PF_SEAFD_LOW","PF_EGGS","PF_NUTSDS","PF_LEGUMES","D_TOTAL",
                   "D_CHEESE")

TOT_FOOD<-bind_rows(DR1TOT, DR2TOT) %>% group_by(SEQN) %>%summarise_all(funs(mean),na.rm=TRUE)

remove(DR1TOT)
remove(DR2TOT)

is.nan.data.frame <- function(x)
{do.call(cbind, lapply(x, is.nan))}
TOT_FOOD[is.nan(TOT_FOOD)]<-NA

TOT_FOOD$F_FRUIT<-TOT_FOOD$F_CITMLB+TOT_FOOD$F_OTHER
TOT_FOOD$PF_SEAFD<-TOT_FOOD$PF_SEAFD_HI+TOT_FOOD$PF_SEAFD_LOW
TOT_FOOD<-TOT_FOOD[,c("SEQN","F_FRUIT","V_TOTAL","PF_SEAFD","G_WHOLE","PF_MPS_TOTAL","PF_MEAT","PF_CUREDMEAT","PF_POULT","PF_EGGS","PF_NUTSDS","PF_LEGUMES","D_TOTAL","D_CHEESE")]








