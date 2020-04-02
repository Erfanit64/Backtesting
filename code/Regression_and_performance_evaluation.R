# import CRSP raw data
library(psych)
library(lmtest)
library(PerformanceAnalytics)
library(data.table)
library(readxl)




train = setDT(read.csv("Train_NEW.csv"))
train = setDT(Train_NEW)
train = train[PRICE >5 & SIZE > 100000000]

train[, zEP := scale(EP), by = .(year, SIC)]
train[, zIA := scale(IA), by = .(year, SIC)]
train[, zIG := scale(IG), by = .(year, SIC)]
train[, zIK := scale(IK), by = .(year, SIC)]
train[, zLEV := scale(LEV), by = .(year, SIC)]
train[, zNOA := scale(NOA), by = .(year, SIC)]
train[, zNS := scale(NS), by = .(year, SIC)]
train[, zOK := scale(OK), by = .(year, SIC)]
train[, zROA := scale(ROA), by = .(year, SIC)]
train[, zROE:= scale(ROE), by = .(year, SIC)]
train[, zlnSIZE := scale(lnSIZE), by = .(year, SIC)]
train[, zMOM := scale(MOM), by = .(year, SIC)]
train[, zlnSIZE := scale(lnSIZE), by = .(year, SIC)]
train[, zOS := scale(OS), by = .(year, SIC)]
train[, zSG := scale(SG), by = .(year, SIC)]
train[, zSUE := scale(SUE), by = .(year, SIC)]
train[, zBETA := scale(BETA), by = .(year, SIC)]
train[, zCI := scale(CI), by = .(year, SIC)]
train[, zLTR := scale(LTR), by = .(year, SIC)]
train[, zTO := scale(TO), by = .(year, SIC)]
train[, zRSI := scale(RSI), by = year]
train[, zMACD := scale(MACD), by = year]




correlation = corr.test((train[,.(zEP,zIA,zIG,zIK,zLEV,zNOA,zNS,zOK,zROA,zROE,zlnSIZE,zMOM,zOS,
                                  zSG,zSUE,zBETA,zCI,zLTR,zTO,zRSI,zMACD,BBP,RET)]))
correlation[["r"]]

correlation[["p"]]
which(correlation[["p"]] < 0.05, arr.ind = T)

correlation[["r"]]
which(correlation[["r"]]>=0.7, arr.ind = T)



cofficients_raw = train[, as.list(coef(lm(RET ~ zEP+zIA+zIG+zIK+zLEV+zNOA+zNS+zOK+zROA+zROE+zlnSIZE+zMOM+zOS+
                                        zSG+zSUE+zBETA+zCI+zLTR+zTO+zRSI+zMACD+BBP))), by = year]
(coefmean_raw = apply(cofficients_raw[, .SD, .SDcol = - "year"],2,mean))

(coefsd_raw = apply(cofficients_raw[, .SD, .SDcol = - "year"],2,sd))


(tstat_raw = coefmean_raw / coefsd_raw * sqrt(18))


cofficients = train[, as.list(coef(lm(RET ~ zLEV+zNS+zOK+zOS+zSG+
                                        zSUE+zLTR+zRSI+zMACD+BBP))), by = year]


# (coefmean = apply(cofficients[, .SD, .SDcol = - "year"],2,mean))
# 
# 
# (coefsd = apply(cofficients[, .SD, .SDcol = - "year"],2,sd))
# 
# 
# (tstat = coefmean / coefsd * sqrt(18))
#import full sample of monthly returns of stocks
test = setDT(read.csv("Test_zscore.csv"))
ret = setDT(read.csv('Monthly_Test.csv'))
setnames(ret, c('permno', 'date', 'ticker','PRC', 'ret'))

ret[, date := as.Date(as.character(date), format = "%Y%m%d")]
ret[, year := year(date)]
ret[, month := month(date)]
write.csv(ret,"ret.csv")
ret = setDT(read.csv("ret.csv"))
ret = na.omit(ret)
ret[, ret := as.numeric(as.character(ret))]
setnames(ret,c('x','PERMNO', 'date', 'ticker','PRC', 'ret','year','month'))
test[, score := zLEV * tstat[2]  + zNS * tstat[3] + zOK * tstat[4]+zOS * tstat[5]+zSG*tstat[6] + zSUE * tstat[7]+ zLTR * tstat[8] + zRSI*tstat[9] + zMACD * tstat[10]+BBP*tstat[11]]
test = na.omit(test)
test[, group := findInterval(score, quantile(score, c(0.1, 0.9))), by = year]

long = test[group == 2]
short = test[group == 0]


for(i in c(2009:2016)){
  longRet = ret[PERMNO %in% long[year == i]$PERMNO & year == i + 1]
  longRet = longRet[, .(longRet = mean(ret, na.rm = TRUE)), by = .(year, month)]
  
  shortRet = ret[PERMNO %in% short[year == i]$PERMNO & year == i + 1]
  shortRet = shortRet[, .(shortRet = mean(ret, na.rm = TRUE)), by = .(year, month)]
  
  output = merge(longRet, shortRet, by = c('year', 'month'))
  
  if (i == 2009) LSport = output
  else LSport = rbind(LSport, output)
}

LSport[, LSret := longRet - shortRet]



##performance assessment
FFdata = setDT(read.csv("FF.csv"))

FFdata[, date := as.Date(as.character(dateff), format = "%Y%m%d")]
FFdata[, year := year(date)]
FFdata[, month := month(date)]

LSport = merge(LSport, FFdata, by = c('year', 'month'))
LSport[, y := LSret]

#simple mean
apply(LSport[, .(longRet, shortRet, LSret)], 2, mean)

#geometric mean
LSport[, lapply(.(longRet, shortRet, LSret), function(x) prod(1 + x)^(1/.N)-1)]
#annualised return
LSport$grossret <- LSport$LSret + 1
apply(LSport[, .(grossret)], 2, prod)
annual_ret = LSport[, .(ret = prod(1+LSret) -1), by = year]

#Sharpe ratio
(sr = LSport[, mean(LSret)/sd(LSret)])

# annualized SR
(sr = sqrt(12)*sr)
# CAPM

CAPM = lm(LSret ~ mktrf, LSport)
summary(CAPM)
#FF 3 factor
FF3 = lm(LSret ~ mktrf + smb+ hml, LSport)
summary(FF3)

#corhart 4 factor
C4 = lm(LSret~mktrf + smb + hml + umd, LSport)
summary(C4)

#Information rate
(ir = coef(C4)[1]/sd(C4$residuals))

#annualized IR
(ir = sqrt(12)* ir)

