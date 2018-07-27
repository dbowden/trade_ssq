claim <- readr::read_csv("datasets/ICOWdata/ICOW Data 1.1/ICOWclaimdy.csv")

claim$endyear <- as.numeric(substr(claim$endclaim, 1, 4))

claim$endyear_lag <- claim$endyear - 1

claim$peacefulresolve <- ifelse((claim$resolved < 5 | (claim$resolved > 7 & claim$resolved < 11)) & claim$endviol==0, 1, 0)

claim$dyad <- as.numeric(ifelse(claim$chal < claim$tgt, paste(claim$chal, claim$tgt, sep=""), paste(claim$tgt, claim$chal, sep="")))

claim$formal <- ifelse(claim$resolved==3 | claim$resolved==4 | claim$resolved==10, 1, 0)

# merge in trade data ----

trade <- read_csv("datasets/tradepred.csv")

trade <- select(trade, endyear_lag=year, dyad, totflow, smoothtotrade, gravitypred)

#merge into main data
trade$year <- as.numeric(trade$year)
claim <- left_join(claim, trade)

rm(trade)


#### Models ####

summary(glm(peacefulresolve ~ log(totflow+1), data=claim, family=binomial("logit")))

summary(glm(peacefulresolve ~ gravitypred + icowsal + log(duration), data=claim, family=binomial("logit")))

summary(glm(peacefulresolve ~ log(totflow+1) + icowsal + log(duration), data=claim, family=binomial("logit")))


summary(glm(formal ~ log(totflow+1) + icowsal + log(duration), data=claim, family=binomial("logit")))

summary(glm(formal ~ gravitypred + icowsal + log(duration), data=claim, family=binomial("logit")))
