library(readr)
library(dplyr)
library(zoo)
library(readxl)

#### Import and Clean Territorial Change Data (v5) ####

tc <- read_csv("datasets/territorial_change/tc2014.csv", na = c(".","-9")) #837 obs to start

# limit to transfers between states that were both independent before and after transfer ----

tc <- filter(tc, indep == 0) #remove territories that became independent (decolonization)
tc <- filter(tc, entry == 0) #remove cases of new states forming
tc <- filter(tc, exit == 0) #remove territories that were absorbed after state deaths
tc <- filter(tc, loser > 1) #some cases have NA for loser, and the Leage of Nations (0) is loser in one case
tc <- filter(tc, gainer > 1) #a few have NA for gainer. results in 471 cases
tc <- filter(tc, (entity==loser & portion==1)==F) #remove cases of absorption that are not coded as exits (mostly unifications)


# Code agreement variable ----

#they are all included under the "cession" category
tc$agreement <- ifelse(tc$procedur == 3, 1, 0)

#but there are also some plebiscites that need to be removed to isolate agreements. These are determined by COW code sheets and Beigbeder (1994). 
tc$agreement[tc$loser==255 & tc$gainer==390 & tc$year==1920] <- 0
tc$agreement[tc$loser==305 & tc$gainer==310 & tc$year==1921] <- 0
tc$agreement[tc$loser==345 & tc$gainer==305 & tc$year==1920] <- 0
tc$agreement[tc$loser==220 & tc$gainer==750 & tc$year==1950] <- 0
#tc$agreement[tc$loser==327 & tc$gainer==325 & tc$year==1870] <- 0 #missing due to the absorption rule above
#tc$agreement[tc$loser==200 & tc$gainer==471 & tc$year==1961] <- 0 #missing b/c procedur changed to unification in new version
#tc$agreement[tc$loser==200 & tc$gainer==820 & tc$year==1963] <- 0 #missing b/c procedur changed to unification in new version

# Prep for merging ----

tc$dyad <- as.numeric(ifelse(tc$gainer > tc$loser, paste(tc$loser, tc$gainer, sep = ""), paste(tc$gainer, tc$loser, sep = "")))

tc <- select(tc, -entry, -exit, -indep, -version)


#### Load Trade Data and Merge ####

trade <- read_csv("datasets/COW_Trade_4.0/Dyadic_COW_4.0.csv", na = "-9")

trade$dyad <- as.numeric(paste(trade$ccode1, trade$ccode2, sep = ""))
trade$year <- as.numeric(trade$year)

# Calculate rolling average ----
trade$totflow <- trade$flow1 + trade$flow2

trade <- trade %>% 
  group_by(dyad) %>% 
  arrange(year) %>% 
  mutate(totflow_avg3=rollmean(totflow, 3, fill = NA, align = "right", na.rm=T), totflow_lag=lag(totflow), totflow_avg3_lag=rollmean(lag(totflow), 3, fill = NA, align = "right", na.rm=T))

trade <- select(trade, year, dyad, totflow, totflow_avg3, totflow_lag, totflow_avg3_lag)

#merge into main data

tc <- left_join(tc, trade, by=c("dyad","year"))

rm(trade)


# Merge in Polity ----

polity <- read_excel("datasets/p4v2016.xls")

polity <- select(polity, year, gainer=ccode, gainer_polity=polity2)

tc <- left_join(tc, polity)

polity <- rename(polity, loser=gainer, loser_polity=gainer_polity)

tc <- left_join(tc, polity)

tc$joint.dem <- ifelse(tc$loser_polity>=6 & tc$gainer_polity>=6, 1, 0)