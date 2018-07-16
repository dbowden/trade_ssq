#### Script for Building Border Settlement & Trade Analysis ####

library(haven)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(countrycode)
library(zoo)


# Load Owsiak settlement data (IBAD 1-0_05-2016) -----

settle <- read_dta("datasets/ibad_release_1-0_05-2016/Replication - IBAD Full Settle Dyad-Year.dta")

# filter to contiguous by land dyads
settle <- filter(settle, conttype==1) #19777


# remove 0s from dyad codes
settle$dyad <- as.numeric(ifelse(settle$ccode1 < settle$ccode2, paste(settle$ccode1, settle$ccode2, sep=""), paste(settle$ccode2, settle$ccode1, sep="")))

# extend frame through end of trade data (2014)
frame <- settle %>% 
  filter(year==2001) %>% 
  group_by(dyad) %>% 
  do(data.frame(dyad=.$dyad, year=as.numeric(seq(2002, 2014)), ccode1=.$ccode1, ccode2=.$ccode2, settle=.$settle))

# if settle status is 0 after 2001, reset to NA
frame$settle[frame$settle==0] <- NA
  
settle <- full_join(settle, frame) #23729

rm(frame)


# Merge in trade DV (COW Dyadic Trade 4.0) [I think 2015 dollars?] ----

trade <- read_csv("datasets/COW_Trade_4.0/Dyadic_COW_4.0.csv", na = "-9")

trade$dyad <- as.numeric(paste(trade$ccode1, trade$ccode2, sep = ""))

trade$totflow <- trade$flow1 + trade$flow2

trade <- select(trade, year, dyad, totflow, smoothtotrade)

#merge into main data
trade$year <- as.numeric(trade$year)
settle <- left_join(settle, trade)

rm(trade)


# Merge in CINC population data (NMC 5.0) ----

cinc <- read_csv("datasets/NMC_5_0/NMC_5_0.csv")

cinc <- select(cinc, c(ccode,year,cinc,tpop))

#convert pop from thousands to raw
cinc$tpop <- cinc$tpop * 1000

cinc$year <- as.numeric(cinc$year)
cinc$ccode <- as.numeric(cinc$ccode)

colnames(cinc) <- c("ccode1", "year", "cap1", "cincpop1")
settle <- left_join(settle, cinc)

colnames(cinc) <- c("ccode2", "year", "cap2", "cincpop2")
settle <- left_join(settle, cinc)

rm(cinc)

settle$caprat <- (settle$cap1/settle$cap2)


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
settle <- left_join(settle, maddison)
maddison <- rename(maddison, ccode2=ccode1, madd_pop2=madd_pop1, madd_gdp2=madd_gdp1)
settle <- left_join(settle, maddison)

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
settle <- left_join(settle, pwt)
pwt <- rename(pwt, ccode2=ccode1, pwt_gdp2=pwt_gdp1)
settle <- left_join(settle, pwt)

rm(pwt)


# Combine and interpolate GDP -----

settle$gdp1 <- ifelse(is.na(settle$pwt_gdp1), settle$madd_gdp1, settle$pwt_gdp1)
settle$gdp2 <- ifelse(is.na(settle$pwt_gdp2), settle$madd_gdp2, settle$pwt_gdp2)
settle$agg_gdp <- settle$gdp1 + settle$gdp2
settle$agg_pop <- settle$cincpop1 + settle$cincpop2
settle$agg_pop <- ifelse(!is.na(settle$agg_pop), settle$agg_pop, settle$madd_pop1 + settle$madd_pop2)

#interpolate
interp1 <- settle %>%
  group_by(ccode1, year) %>%
  summarize(gdp1=max(gdp1)) %>%
  arrange(ccode1, year) %>%
  group_by(ccode1) %>%
  mutate(gdp1.int=na.approx(gdp1, maxgap=10, na.rm=F)) %>% 
  select(-gdp1)

interp2 <- settle %>% 
  group_by(ccode2, year) %>% 
  summarize(gdp2=max(gdp2)) %>% 
  arrange(ccode2, year) %>% 
  group_by(ccode2) %>% 
  mutate(gdp2.int=na.approx(gdp2, maxgap=10, na.rm=F)) %>% 
  select(-gdp2)

settle <- left_join(settle, interp1)
settle <- left_join(settle,interp2)
rm(interp1, interp2)

settle$agg_gdp_int <- ifelse(!is.na(settle$agg_gdp), settle$agg_gdp, settle$gdp1.int + settle$gdp2.int)


# Polity ----

polity <- read_excel("datasets/p4v2016.xls")

polity <- select(polity, ccode1=ccode, year, polity2_1=polity2)

settle <- left_join(settle, polity)

polity <- rename(polity, ccode2=ccode1, polity2_2=polity2_1)

settle <- left_join(settle, polity)

rm(polity)

settle$joint_dem <- ifelse(settle$polity2_1 >=6 & settle$polity2_2 >= 6, 1, 0)


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

settle <- left_join(settle, pta)

settle$pta[is.na(settle$pta)] <- 0

rm(pta)

#wto membership
# wto <- read_csv("wto.csv", col_names = F)
# 
# wto$country <- gsub("\\s{2}.*$", "", wto$X1)
# 
# wto$year <- as.numeric(str_sub(wto$X1, -4, -1))
# 
# #fix one weird one
# wto$year[wto$country=="Zaire"] <- 1971
# wto$country[wto$country=="Zaire"] <- "Democratic Republic of the Congo"
# 
# wto$ccode1 <- countrycode(wto$country, "country.name", "cown")
# 
# #convert to yearly
# 
# wto <- wto %>% 
#   rowwise() %>% 
#   do(data.frame(ccode1=.$ccode1, year=seq(.$year, 2009)))
# 
# wto$wto1 <- 1
# 
# cont <- left_join(cont, wto)
# 
# wto <- rename(wto, ccode2 = ccode1, wto2 = wto1)
# 
# cont <- left_join(cont, wto)
# 
# rm(wto)
# 
# cont$wto1[is.na(cont$wto1)] <- 0
# cont$wto2[is.na(cont$wto2)] <- 0
# 
# cont$both_wto <- ifelse(cont$wto1==1 & cont$wto2==1, 1, 0)
# cont$wto_or_pta <- ifelse(cont$both_wto==1 | cont$pta==1, 1, 0)


# Create settle variables -----

settle <- settle %>% 
  group_by(dyad) %>% 
  arrange(year) %>% 
  mutate(lag_settle=dplyr::lag(settle))

settle$new_settle <- ifelse(settle$lag_settle==0 & settle$settle==1, 1, 0)

settle$settle_year <- ifelse(settle$new_settle==1, settle$year, NA)
settle$settle_trade <- ifelse(settle$new_settle==1, settle$totflow, NA)
settle$settle_transfer <- ifelse(settle$new_settle==1 & settle$terrchange==1, 1, 0)

# fill down
settle <- settle %>%
  group_by(dyad) %>% 
  fill(settle_year, settle_trade)

settle <- settle %>% 
  group_by(dyad) %>% 
  mutate(settle_transfer=max(settle_transfer))

# windows
settle$settle5 <- ifelse((settle$year - settle$settle_year) <= 5, 1, 0)
# reset NA to 0 if settlement status is known
settle$settle5[is.na(settle$settle5) & !is.na(settle$settle)] <- 0


settle$settle10 <- ifelse((settle$year - settle$settle_year) <= 10, 1, 0)
settle$settle10[is.na(settle$settle10) & !is.na(settle$settle)] <- 0

settle$settle20 <- ifelse((settle$year - settle$settle_year) <= 20, 1, 0)
settle$settle20[is.na(settle$settle20) & !is.na(settle$settle)] <- 0

settle$settle_transfer[is.na(settle$settle_transfer)] <- 0

# settlement w/ transfer
settle$settle_transfer5 <- ifelse((settle$year - settle$settle_year) <= 5 & settle$settle_transfer==1, 1, 0)
settle$settle_transfer5[is.na(settle$settle_transfer5) & !is.na(settle$settle)] <- 0

settle$settle_transfer10 <- ifelse((settle$year - settle$settle_year) <= 10 & settle$settle_transfer==1, 1, 0)
settle$settle_transfer10[is.na(settle$settle_transfer10) & !is.na(settle$settle)] <- 0

settle$settle_transfer20 <- ifelse((settle$year - settle$settle_year) <= 20 & settle$settle_transfer==1, 1, 0)
settle$settle_transfer20[is.na(settle$settle_transfer20) & !is.na(settle$settle)] <- 0
