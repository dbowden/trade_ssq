library(haven)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(countrycode)
library(zoo)



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


# Merge in trade data ----

trade <- read_csv("datasets/tradepred.csv")

trade <- select(trade, -ccode1, -ccode2)

settle <- left_join(settle, trade, by=c("dyad","year"))


# Create settle variables -----

settle <- settle %>% 
  group_by(dyad) %>% 
  arrange(year) %>% 
  mutate(lag_settle=lag(settle))

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
