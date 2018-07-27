library(haven)
library(readr)
library(dplyr)

# use dyad-year version of main claims data
claim <- read_dta("datasets/ICOWdata/ICOW Data 1.1/ICOWdyadyr.dta")

# recode dyad nums
claim$dyad <- as.numeric(ifelse(claim$chal < claim$tgt, paste(claim$chal, claim$tgt, sep=""), paste(claim$tgt, claim$chal, sep="")))

# get claim beginning
claim <- claim %>% 
  group_by(claimdy) %>% 
  mutate(begclaim=min(year), endclaim=max(year))

claim$duration <- claim$year - claim$begclaim
claim$end <- ifelse(claim$year==claim$endclaim & claim$year!=2001, 1, 0)

# merge w/ trade data ----
trade <- read_csv("datasets/tradepred.csv")

trade <- select(trade, year, dyad, totflow, smoothtotrade, gravitypred)

#merge into main data
trade$year <- as.numeric(trade$year)
claim <- left_join(claim, trade)

rm(trade)

write_dta(claim, "stata_claim.dta", version=13)


#### Models ####

library(pglm)
library(lmtest)

summary(pglm(attanyp ~ log(totflow+1), data=claim, index=c("claimdy","year"), model="pooling", effect="twoways", family=binomial("logit")))

summary(pglm(attanyp ~ log(gravitypred), data=claim, index=c("claimdy","year"), model="pooling", effect="twoways", family=binomial("logit")))

m1 <- pglm(attanyp ~ lag(log(totflow+1)) + recno15 + recmid15 + as.factor(issue) + icowsal + log(duration), data=claim, index=c("claimdy","year"), model="within", effect="twoways", family=binomial("logit"))
summary(m1)

coeftest(m1, vcov. = vcovHC(m1, type="HC1", cluster="group"))


#### Binding vs Non-Binding ####

arb <- subset(claim, att3rd==1)

arb$binding <- ifelse(arb$att3bind==1, 1, 0)

summary(glm(binding ~ log(totflow+1) + log(duration+1) + icowsal+ recno15 + recmid15 + as.factor(issue), data=arb, family=binomial("logit")))


### claim end ####

e1 <- pglm(end ~ lag(log(totflow+1)) + recno15 + recmid15 + as.factor(issue) + icowsal + log(duration), data=claim, index=c("claimdy","year"), model="pooling", effect="twoways", family=binomial("logit"))
summary(e1)
