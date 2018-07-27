library(readr)
library(readxl)
library(dplyr)
library(countrycode)
library(zoo)


# Import and clean trade data ----

trade <- read_csv("datasets/COW_Trade_4.0/Dyadic_COW_4.0.csv", na = "-9")

#make dyad numbers and convert year to numeric for merging
trade$dyad <- as.numeric(paste(trade$ccode1, trade$ccode2, sep = ""))
trade$year <- as.numeric(trade$year)

trade$totflow <- trade$flow1 + trade$flow2

trade <- select(trade, year, dyad, ccode1, ccode2, totflow, smoothtotrade)


# Merge in capital distance data from Gleditsch ----

capdist <- read_csv("datasets/capdist.csv")

capdist <- select(capdist, ccode1=numa, ccode2=numb, kmdist)

#there are a few duplicates
capdist <- filter(capdist, duplicated(capdist)==F)

trade <- left_join(trade, capdist)

rm(capdist)


# Merge in CINC population data (NMC 5.0) ----

cinc <- read_csv("datasets/NMC_5_0/NMC_5_0.csv")

cinc <- select(cinc, c(ccode,year,tpop))

#convert pop from thousands to raw
cinc$tpop <- cinc$tpop * 1000

cinc$year <- as.numeric(cinc$year)
cinc$ccode <- as.numeric(cinc$ccode)

colnames(cinc) <- c("ccode1", "year", "cincpop1")
trade <- left_join(trade, cinc)

colnames(cinc) <- c("ccode2", "year", "cincpop2")
trade <- left_join(trade, cinc)

rm(cinc)

trade$agg_pop <- trade$cincpop1 + trade$cincpop2


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
trade <- left_join(trade, maddison)
maddison <- rename(maddison, ccode2=ccode1, madd_pop2=madd_pop1, madd_gdp2=madd_gdp1)
trade <- left_join(trade, maddison)

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
trade <- left_join(trade, pwt)
pwt <- rename(pwt, ccode2=ccode1, pwt_gdp2=pwt_gdp1)
trade <- left_join(trade, pwt)

rm(pwt)


# Combine and interpolate GDP -----

trade$gdp1 <- ifelse(is.na(trade$pwt_gdp1), trade$madd_gdp1, trade$pwt_gdp1)
trade$gdp2 <- ifelse(is.na(trade$pwt_gdp2), trade$madd_gdp2, trade$pwt_gdp2)
trade$agg_gdp <- trade$gdp1 + trade$gdp2
trade$agg_pop <- ifelse(!is.na(trade$agg_pop), trade$agg_pop, trade$madd_pop1 + trade$madd_pop2)

#interpolate
trade <- trade %>%
  group_by(ccode1) %>%
  mutate(gdp1.int=na.approx(gdp1, maxgap=10, na.rm=F))

trade <- trade %>% 
  group_by(ccode2) %>% 
  mutate(gdp2.int=na.approx(gdp2, maxgap=10, na.rm=F))

trade$agg_gdp_int <- ifelse(!is.na(trade$agg_gdp), trade$agg_gdp, trade$gdp1.int + trade$gdp2.int)


#### Models ####

g1 <- lm(log(totflow+1) ~ log(agg_gdp+1) + log(kmdist+1), data=trade)
summary(g1)

g2 <- lm(log(totflow+1) ~ log(agg_gdp+1) + log(kmdist+1) + log(agg_pop+1), data=trade)
summary(g2)

g3 <- lm(log(totflow+1) ~ log(agg_gdp+1) + log(kmdist+1) + year + I(year^2) + I(year^3), data=trade)
summary(g3)


# subset to complete cases
complete <- subset(trade, complete.cases(trade[,c("totflow","agg_gdp","kmdist","agg_pop")]))

g2 <- lm(log(totflow+1) ~ log(agg_gdp+1) + log(kmdist+1) + log(agg_pop+1), data=complete)
summary(g2)

complete$gravitypred <- fitted(g2)

write_csv(complete, "datasets/tradepred.csv")
