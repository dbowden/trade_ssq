#### Script for Building Border Settlement & Trade Analysis ####

library(haven)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(countrycode)


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
  do(data.frame(dyad=.$dyad, year=as.numeric(seq(2002, 2014)), ccode1=.$ccode1, ccode2=.$ccode2))
  
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

settle$agg_cincpop <- settle$cincpop1 + settle$cincpop2

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
