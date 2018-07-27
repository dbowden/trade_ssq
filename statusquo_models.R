library(lmtest)
library(plm)

sq <- filter(settle, settle_transfer==0)


# Gravity models ----

g5 <- plm(log(totflow+1) ~ settle5 + log(agg_gdp+1) + log(agg_pop+1), data=sq, index=c("dyad", "year"), model = "within", effect = "twoways")
summary(g5)
coeftest(g5, vcov. = vcovHC(g5, type="HC1", cluster="group"))


# Splines ----

sq$year2 <- sq$year

s5 <- plm(log(totflow+1) ~ settle5 + lag(log(totflow+1)) + lag(log(totflow+1),2) + log(agg_gdp+1) + log(agg_pop+1) + joint_dem + pta + year2 + I(year2^2) + I(year2^3), data=sq, index=c("dyad", "year"), model = "within", effect = "individual")
summary(s5)
coeftest(s5, vcov. = vcovHC(s5, type="HC1", cluster="group"))


s20 <- plm(log(totflow+1) ~ settle20 + lag(log(totflow+1)) + lag(log(totflow+1),2) + log(agg_gdp+1) + log(agg_pop+1) + joint_dem + pta + year2 + I(year2^2) + I(year2^3), data=sq, index=c("dyad", "year"), model = "within", effect = "individual")
summary(s20)
coeftest(s20, vcov. = vcovHC(s20, type="HC1", cluster="group"))

s20b <- plm(log(totflow+1) ~ settle20 + lag(log(totflow+1)) + lag(log(totflow+1),2) + log(agg_gdp+1) + log(agg_pop+1) + year2 + I(year2^2) + I(year2^3), data=sq, index=c("dyad", "year"), model = "within", effect = "individual")
summary(s20b)

# FE ----

f5 <- plm(log(totflow+1) ~ settle5 + lag(log(totflow+1)) + lag(log(totflow+1),2) + log(agg_gdp+1) + log(agg_pop+1) + joint_dem + pta , data=sq, index=c("dyad", "year"), model = "within", effect = "twoways")
summary(f5)
coeftest(s5, vcov. = vcovHC(s5, type="HC1", cluster="group"))


f20 <- plm(log(totflow+1) ~ settle20 + lag(log(totflow+1)) + lag(log(totflow+1),2) + log(agg_gdp+1) + log(agg_pop+1) + joint_dem + pta + year2 + I(year2^2) + I(year2^3), data=sq, index=c("dyad", "year"), model = "within", effect = "individual")
summary(s20)
coeftest(s20, vcov. = vcovHC(s20, type="HC1", cluster="group"))