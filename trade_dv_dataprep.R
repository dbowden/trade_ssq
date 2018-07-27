# Domain: contiguous dyad-years 1870-present
# IVs:
#   - Transfer within X years
#   - ICOW attempt within X years
#   - ICOW termination within X years
#   - End of last ICOW claim within X years
#   - Border settlement within X years
# Model: DiD

# load packages
library(readr)
library(haven)
library(readxl)
library(dplyr)
library(tidyr)
library(countrycode)
library(zoo)

# Start w/ COW Direct Contiguity Data v3.2 ----

tradedv <- read_csv("datasets/DirectContiguity320/contdir.csv")

#limit to land contiguity
tradedv <- filter(tradedv, conttype==1)

#switch to my dyad scheme
tradedv$dyad <- as.numeric(paste(tradedv$statelno, tradedv$statehno, sep=""))

#extract years
tradedv$begyr <- as.numeric(substr(tradedv$begin, 1, 4))
tradedv$endyr <- as.numeric(substr(tradedv$end, 1, 4))

#restructure to dyad-years
tradedv <- tradedv %>%
  rowwise() %>%
  do(data.frame(dyad=.$dyad, year=seq(.$begyr,.$endyr)))


# Merge in Owsiak Border Settlement Data IBAD ----

settle <- read_dta("datasets/ibad_release_1-0_05-2016/Replication - IBAD Full Settle Dyad-Year.dta")

#limit to contiguous dyads
settle <- filter(settle, conttype==1)

#switch to my dyad scheme
settle$dyad <- as.numeric(ifelse(settle$ccode1 < settle$ccode2, paste(settle$ccode1, settle$ccode2, sep=""), paste(settle$ccode2, settle$ccode1, sep="")))

#create new settlement variable
settle <- settle %>% 
  group_by(dyad) %>% 
  mutate(lag_settle=dplyr::lag(settle))

settle$new_settle <- ifelse(settle$settle==1 & settle$lag_settle==0, 1, 0)

#get year of settlement and fill down
settle$settle_year <- ifelse(settle$new_settle==1, settle$year, NA)

settle <- settle %>% 
  group_by(dyad) %>% 
  fill(settle_year, .direction = "down")

#merge into contiguity data
tradedv <- left_join(tradedv, settle)
rm(settle)

## continue obs counter

#first get max obs value
tradedv <- tradedv %>% 
  group_by(dyad) %>% 
  mutate(max_obs=max(obs))

#the get years since end of IBAD data (2001)
tradedv$end_count <- tradedv$year - 2001

#fill missing
tradedv$obs <- ifelse(tradedv$year > 2001, tradedv$max_obs + tradedv$end_count, tradedv$obs)

#cleanup
tradedv <- select(tradedv, -max_obs, -end_count)

## create window vars
tradedv$settle5 <- ifelse(tradedv$year - tradedv$settle_year <= 5, 1, 0)
tradedv$settle10 <- ifelse(tradedv$year - tradedv$settle_year <= 10, 1, 0)
tradedv$settle20 <- ifelse(tradedv$year - tradedv$settle_year <= 20, 1, 0)

#reset NA to 0 if dyad has been observed longer than window
#i.e. if dyad starts at 1, assume 1st year could be tradedvment year, but later years couldn't have changed within 5 because we observed them
tradedv$settle5[tradedv$settle==0] <- 0
tradedv$settle5[is.na(tradedv$settle5) & tradedv$obs > 6] <- 0
tradedv$settle10[tradedv$settle==0] <- 0
tradedv$settle10[is.na(tradedv$settle10) & tradedv$obs > 11] <- 0
tradedv$settle20[tradedv$settle==0] <- 0
tradedv$settle20[is.na(tradedv$settle20) & tradedv$obs > 21] <- 0


# Merge in territorial changes v5 ----

tc <- read_csv("datasets/territorial_change/tc2014.csv", na = c(".","-9")) #837 obs to start

## limit to transfers between states that were both independent before and after transfer

tc <- filter(tc, indep == 0) #remove territories that became independent (decolonization)
tc <- filter(tc, entry == 0) #remove cases of new states forming
tc <- filter(tc, exit == 0) #remove territories that were absorbed after state deaths
tc <- filter(tc, loser > 1) #some cases have NA for loser, and the Leage of Nations (0) is loser in one case
tc <- filter(tc, gainer > 1) #a few have NA for gainer. results in 471 cases
tc <- filter(tc, (entity==loser & portion==1)==F) #remove cases of absorption that are not coded as exits (mostly unifications)


## Code agreement variable

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

## Prep for merging

tc$dyad <- as.numeric(ifelse(tc$gainer > tc$loser, paste(tc$loser, tc$gainer, sep = ""), paste(tc$gainer, tc$loser, sep = "")))

tc$transfer <- 1

tc <- select(tc, dyad, year, agreement, transfer)

#summarize dyad years (some dyad-years with multiple transfers)
tc <- tc %>% 
  group_by(dyad, year) %>% 
  summarise_all(max)

tradedv <- left_join(tradedv, tc)

rm(tc)

## create windows

tradedv$trans_year <- ifelse(tradedv$transfer==1, tradedv$year, NA)
tradedv$agree_year <- ifelse(tradedv$agreement==1, tradedv$year, NA)

# fill down
tradedv <- tradedv %>% 
  group_by(dyad) %>% 
  fill(trans_year, agree_year)

tradedv$agree5 <- ifelse(tradedv$year - tradedv$agree_year <= 5, 1, 0)
tradedv$agree10 <- ifelse(tradedv$year - tradedv$agree_year <= 10, 1, 0)
tradedv$agree20 <- ifelse(tradedv$year - tradedv$agree_year <= 20, 1, 0)
tradedv$trans5 <- ifelse(tradedv$year - tradedv$trans_year <= 5, 1, 0)
tradedv$trans10 <- ifelse(tradedv$year - tradedv$trans_year <= 10, 1, 0)
tradedv$trans20 <- ifelse(tradedv$year - tradedv$trans_year <= 20, 1, 0)

tradedv$agree5[is.na(tradedv$agree5)] <- 0
tradedv$agree10[is.na(tradedv$agree10)] <- 0
tradedv$agree20[is.na(tradedv$agree20)] <- 0
tradedv$trans5[is.na(tradedv$trans5)] <- 0
tradedv$trans10[is.na(tradedv$trans10)] <- 0
tradedv$trans20[is.na(tradedv$trans20)] <- 0


# ICOW data ----

# use dyad-year version of main claims data
claim <- read_dta("datasets/ICOWdata/ICOW Data 1.1/ICOWdyadyr.dta")

# add beg and end dates
dates <- read_csv("datasets/ICOWdata/ICOW Data 1.1/ICOWclaimdy.csv")

# subset to needed cols
dates <- select(dates, claimdy, begclaim, endclaim, resolved)

dates$claimdy <- as.numeric(dates$claimdy)

claim <- left_join(claim, dates)
rm(dates)

# recode dyad nums
claim$dyad <- as.numeric(ifelse(claim$chal < claim$tgt, paste(claim$chal, claim$tgt, sep=""), paste(claim$tgt, claim$chal, sep="")))

# get claim beginning
claim$begclaim <- as.numeric(substr(claim$begclaim, 1, 4))
claim$endclaim <- as.numeric(substr(claim$endclaim, 1, 4))

claim$endicowclaim <- ifelse(claim$endclaim==claim$year, 1, 0)

# summarize to 1 obs per dyad-year
claim <- claim %>% 
  group_by(dyad, year) %>% 
  summarize(endicowclaim=max(endicowclaim), attanyp=max(attanyp))

# merge
tradedv <- left_join(tradedv, claim)
rm(claim)

## create windows
tradedv$attanyp_year <- ifelse(tradedv$attanyp==1, tradedv$year, NA)
tradedv$endicowclaim_year <- ifelse(tradedv$endicowclaim==1, tradedv$year, NA)

#fill down
tradedv <- tradedv %>% 
  group_by(dyad) %>% 
  fill(attanyp_year, endicowclaim_year)

#windows
tradedv$attanyp5 <- ifelse(tradedv$year - tradedv$attanyp_year <= 5, 1, 0)
tradedv$attanyp10 <- ifelse(tradedv$year - tradedv$attanyp_year <= 10, 1, 0)
tradedv$attanyp20 <- ifelse(tradedv$year - tradedv$attanyp_year <= 20, 1, 0)
tradedv$endicowclaim5 <- ifelse(tradedv$year - tradedv$endicowclaim_year <= 5, 1, 0)
tradedv$endicowclaim10 <- ifelse(tradedv$year - tradedv$endicowclaim_year <= 10, 1, 0)
tradedv$endicowclaim20 <- ifelse(tradedv$year - tradedv$endicowclaim_year <= 20, 1, 0)

#convert NAs to 0
tradedv$attanyp5[is.na(tradedv$attanyp5)] <- 0
tradedv$attanyp10[is.na(tradedv$attanyp10)] <- 0
tradedv$attanyp20[is.na(tradedv$attanyp20)] <- 0
tradedv$attanyp5[is.na(tradedv$attanyp5)] <- 0
tradedv$endicowclaim5[is.na(tradedv$endicowclaim5)] <- 0
tradedv$endicowclaim10[is.na(tradedv$endicowclaim10)] <- 0
tradedv$endicowclaim20[is.na(tradedv$endicowclaim20)] <- 0

# Merge in trade DV (COW Dyadic Trade 4.0) [I think 2015 dollars?] ----

trade <- read_csv("datasets/COW_Trade_4.0/Dyadic_COW_4.0.csv", na = "-9")

trade$dyad <- as.numeric(paste(trade$ccode1, trade$ccode2, sep = ""))

trade$totflow <- trade$flow1 + trade$flow2

trade <- select(trade, year, dyad, totflow, smoothtotrade)

#merge into main data
trade$year <- as.numeric(trade$year)
tradedv <- left_join(tradedv, trade)

rm(trade)


# Merge in CINC population data (NMC 5.0) ----

cinc <- read_csv("datasets/NMC_5_0/NMC_5_0.csv")

cinc <- select(cinc, c(ccode,year,cinc,tpop))

#convert pop from thousands to raw
cinc$tpop <- cinc$tpop * 1000

cinc$year <- as.numeric(cinc$year)
cinc$ccode <- as.numeric(cinc$ccode)

colnames(cinc) <- c("ccode1", "year", "cap1", "cincpop1")
tradedv <- left_join(tradedv, cinc)

colnames(cinc) <- c("ccode2", "year", "cap2", "cincpop2")
tradedv <- left_join(tradedv, cinc)

rm(cinc)

tradedv$caprat <- (tradedv$cap1/tradedv$cap2)


# Create GDP data: Maddison Project Database 2018, cgdppc (per capita GDP with multiple benchmarks) [2011 USD] ----

maddison <- read_excel("datasets/mpd2018.xlsx", sheet = 2)

maddison <- filter(maddison, year >= 1870)

#convert to COW codes
maddison$ccode <- countrycode(maddison$countrycode, "iso3c", "cown")

#manual convert a few
maddison$ccode[maddison$countrycode == "CSK"] <- 315 #czechoslovakia
maddison$ccode[maddison$countrycode == "SRB"] <- 340 #Serbia
maddison$ccode[maddison$countrycode == "SUN"] <- 365 #USSR - Russia inherits
maddison$ccode[maddison$countrycode == "YUG"] <- 345 #Yugoslavia

#cleanup USSR
maddison <- filter(maddison, !is.na(ccode))
maddison <- filter(maddison, (country=="Russian Federation" & year < 1992)==F)
maddison <- filter(maddison, (country=="Former USSR" & year > 1991)==F)

# convert population to raw
maddison$pop <- maddison$pop * 1000

# convert from 2011 to 2015 dollars using this: https://data.bls.gov/cgi-bin/cpicalc.pl?cost1=100&year1=201101&year2=201501
maddison$madd_gdp <- maddison$rgdpnapc * 1.0612 * maddison$pop 

# convert to millions of dollars
maddison$madd_gdp / 1000000

#remove irrelevant columns and merge
maddison <- select(maddison, year, ccode1=ccode, madd_pop1=pop, madd_gdp1=madd_gdp)
tradedv <- left_join(tradedv, maddison)
maddison <- rename(maddison, ccode2=ccode1, madd_pop2=madd_pop1, madd_gdp2=madd_gdp1)
tradedv <- left_join(tradedv, maddison)

rm(maddison)


# Post-1950 GDP: PWT 9.0 (Real GDP at constant national prices in millions of 2011 USD) ----
pwt <- read_excel("datasets/pwt90.xlsx", sheet = 3)

#convert to cow code
pwt$ccode1 <- countrycode(pwt$countrycode, "iso3c", "cown")
pwt$ccode1[pwt$countrycode=="SRB"] <- 340 #serbia
pwt <- filter(pwt, !is.na(ccode1))

# convert from 2011 to 2015 dollars using this: https://data.bls.gov/cgi-bin/cpicalc.pl?cost1=100&year1=201101&year2=201501
pwt$rgdpo <- pwt$rgdpo * 1.0612

#merge
pwt <- select(pwt, year, ccode1, pwt_gdp1=rgdpo)
tradedv <- left_join(tradedv, pwt)
pwt <- rename(pwt, ccode2=ccode1, pwt_gdp2=pwt_gdp1)
tradedv <- left_join(tradedv, pwt)

rm(pwt)


# Combine and interpolate GDP -----

tradedv$gdp1 <- ifelse(is.na(tradedv$pwt_gdp1), tradedv$madd_gdp1, tradedv$pwt_gdp1)
tradedv$gdp2 <- ifelse(is.na(tradedv$pwt_gdp2), tradedv$madd_gdp2, tradedv$pwt_gdp2)
tradedv$agg_gdp <- tradedv$gdp1 + tradedv$gdp2
tradedv$agg_pop <- tradedv$cincpop1 + tradedv$cincpop2
tradedv$agg_pop <- ifelse(!is.na(tradedv$agg_pop), tradedv$agg_pop, tradedv$madd_pop1 + tradedv$madd_pop2)

#interpolate
interp1 <- tradedv %>%
  group_by(ccode1, year) %>%
  summarize(gdp1=max(gdp1)) %>%
  arrange(ccode1, year) %>%
  group_by(ccode1) %>%
  mutate(gdp1.int=na.approx(gdp1, maxgap=10, na.rm=F)) %>% 
  select(-gdp1)

interp2 <- tradedv %>% 
  group_by(ccode2, year) %>% 
  summarize(gdp2=max(gdp2)) %>% 
  arrange(ccode2, year) %>% 
  group_by(ccode2) %>% 
  mutate(gdp2.int=na.approx(gdp2, maxgap=10, na.rm=F)) %>% 
  select(-gdp2)

tradedv <- left_join(tradedv, interp1)
tradedv <- left_join(tradedv,interp2)
rm(interp1, interp2)

tradedv$agg_gdp_int <- ifelse(!is.na(tradedv$agg_gdp), tradedv$agg_gdp, tradedv$gdp1.int + tradedv$gdp2.int)


# Polity ----

polity <- read_excel("datasets/p4v2016.xls")

polity <- select(polity, ccode1=ccode, year, polity2_1=polity2)

tradedv <- left_join(tradedv, polity)

polity <- rename(polity, ccode2=ccode1, polity2_2=polity2_1)

tradedv <- left_join(tradedv, polity)

rm(polity)

tradedv$joint_dem <- ifelse(tradedv$polity2_1 >=6 & tradedv$polity2_2 >= 6, 1, 0)


# PTA data --------

# Desta 1.03
pta <- read_csv("datasets/list_of_treaties_dyadic_01_03.csv")

pta <- select(pta, country1:wto_name)

pta$ccode1 <- countrycode(pta$iso1, "iso3n", "cown")
pta$ccode1[pta$country1=="Serbia"] <- 345
pta$ccode1[pta$country1=="Kosovo"] <- 347

pta$ccode2 <- countrycode(pta$iso2, "iso3n", "cown")
pta$ccode2[pta$country2=="Serbia"] <- 345
pta$ccode2[pta$country2=="Kosovo"] <- 347

pta$dyad <- as.numeric(ifelse(pta$ccode1 < pta$ccode2, paste(pta$ccode1, pta$ccode2, sep=""), paste(pta$ccode2, pta$ccode1, sep="")))

# convert to yearly obs - make pta binary
pta <- pta %>% 
  group_by(dyad) %>% 
  summarize(start=min(year))

pta <- pta %>% 
  rowwise() %>% 
  do(data.frame(dyad=.$dyad, year=seq(.$start, 2014)))

pta$pta <- 1

pta$year <- as.numeric(pta$year)

tradedv <- left_join(tradedv, pta)

tradedv$pta[is.na(tradedv$pta)] <- 0

rm(pta)

# Convert to factors -------

tradedv$settle <- as.factor(tradedv$settle)
tradedv$settle5 <- as.factor(tradedv$settle5)
tradedv$settle10 <- as.factor(tradedv$settle10)
tradedv$settle20 <- as.factor(tradedv$settle20)
tradedv$agree5 <- as.factor(tradedv$agree5)
tradedv$agree10 <- as.factor(tradedv$agree10)
tradedv$agree20 <- as.factor(tradedv$agree20)
tradedv$trans5 <- as.factor(tradedv$trans5)
tradedv$trans10 <- as.factor(tradedv$trans10)
tradedv$trans20 <- as.factor(tradedv$trans20)
tradedv$endicowclaim5 <- as.factor(tradedv$endicowclaim5)
tradedv$endicowclaim10 <- as.factor(tradedv$endicowclaim10)
tradedv$endicowclaim20 <- as.factor(tradedv$endicowclaim20)
tradedv$attanyp5 <- as.factor(tradedv$attanyp5)
tradedv$attanyp10 <- as.factor(tradedv$attanyp10)
tradedv$attanyp20 <- as.factor(tradedv$attanyp20)

