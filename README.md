# Backtesting on 22 firm-characteristic factors

## Objective
Backtest 22 firm-characteristics risk factors to evaluate their explanatory power to the future stock return.

## Method
cross-sectional linear regression, Fama-MacBeth

## Created DataSet
- Wrote python functions to calculate 3 technical factors(MACD, RSI, BBP) by using daily closing prices; see the code here [3technical_factors.ipynb](https://github.com/JingsiTheExplorer/Backtesting/blob/master/code/3technical_factors.ipynb)
- Collected firm-characteristic data(risk factors) from WRDS, Compustat, comp.funda etc for stock universe. The stock universe includes NYSE/AMEX/Nasdaq stocks, only common stocks (not preferred, etc.). The risk factors are across several different categories.
![](https://github.com/JingsiTheExplorer/Backtesting/blob/master/pictures/factors1.png)
![](https://github.com/JingsiTheExplorer/Backtesting/blob/master/pictures/factors2.png)
- Collected annual return for development set and monthly return for validation set from WRDS

## Data wrangling and cleaning
- Since some factors(eg. ROA...) have only value at midyear, while other factors(eg. turnover rate...) have value on each day, we use the mid-year value or calculated the mean value from midpoint of last year to the midpoint of this year. see code here [TO_year.ipynb](https://github.com/JingsiTheExplorer/Backtesting/blob/master/code/TO_year.ipynb)

![roa.png](https://github.com/JingsiTheExplorer/Backtesting/blob/master/pictures/roa.png) ![to.png](https://github.com/JingsiTheExplorer/Backtesting/blob/master/pictures/to.png)
- Merged all the factors data on stock "permno" and "year". Merged factors data with the return next year
- Removed factors with too many missing values to ensure the final dataset has enough observations
- Removed stocks have a price that exceeds $5 per share and a market capitalization of equity of at least $100 million at the beginning of a given forecast year, to avoid trading illiquid stocks
- Normalized factor data using z-score by industry

## Final dataset description
4488 unique stocks of 26 years (1990.6 - 2016.6),
with 34,560 observations and 27 columns(22 risk factors plus index, permno, year, return, and price) (760,000+ points)
Separated data into in-sample data (1990.6-2008.6) and out-of-time data (2009.6-2016.6).

## Correlation analysis
```
correlation = corr.test((train[,.(zEP,zIA,zIG,zIK,zLEV,zNOA,zNS,zOK,zROA,zROE,zlnSIZE,zMOM,zOS,
                                  zSG,zSUE,zBETA,zCI,zLTR,zTO,zRSI,zMACD,BBP,RET)]))

correlation[["r"]]
which(correlation[["r"]]>=0.5, arr.ind = T)

correlation[["p"]]
which(correlation[["p"]] < 0.05, arr.ind = T)
```
remove features has high correlation(>=0.5) with high significance(<0.05)

## In the sample estimation(1990-2008)
Run cross-sectional regression during each estimation window(year).
Estimated the factor premia on each factor during each estimation window, then compute the average factor premia(the Fama-MacBeth average) across all windows and the t-statistic(Fama-MacBeth t-statistic) of this average.

```
#calculate Fama-MacBeth t-statistic

coefficients_raw = train[, as.list(coef(lm(RET ~      zEP+zIA+zIG+zIK+zLEV+zNOA+zNS+zOK+zROA+zROE+zlnSIZE+zMOM+zOS+
          zSG+zSUE+zBETA+zCI+zLTR+zTO+zRSI+zMACD+BBP))), by = year]

(coefmean_raw = apply(coefficients_raw[, .SD, .SDcol = - "year"],2,mean))

(coefsd_raw = apply(coefficients_raw[, .SD, .SDcol = - "year"],2,sd))

         #1990-2008 the number of windows is 18, square root use 18
(tstat_raw = coefmean_raw / coefsd_raw * sqrt(18))

         #pick features which has FM>1.3, do it again
coefficients = train[, as.list(coef(lm(RET ~ zLEV+zNS+zOK+zOS+zSG+
                                        zSUE+zLTR+zRSI+zMACD+BBP))), by = year]

(coefmean = apply(coefficients[, .SD, .SDcol = - "year"],2,mean))
(coefsd = apply(coefficients[, .SD, .SDcol = - "year"],2,sd))
(tstat = coefmean / coefsd * sqrt(18))

```
![](https://github.com/JingsiTheExplorer/Backtesting/blob/master/pictures/insample1.png)
![](https://github.com/JingsiTheExplorer/Backtesting/blob/master/pictures/insample2.png)

Keep factors with absolute FM t-statistic above 1.3 and matched expected sign.
Those factors with blue highlight are selected.

## Out of time validation(2009-2016)
- Step1: score stocks during the out-of-sample period. Weighted each stock’s factor exposure z-score  by the t-statistic derived for that factor in the regression model.
Score = z-score(1) x t-stat(1) + z-score(2) x t-stat(2) + …
```
#out of sample validation
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

#calculate score for each stock
test[, score := zLEV * tstat[2]  + zNS * tstat[3] + zOK * tstat[4]+zOS * tstat[5]+zSG*tstat[6] + zSUE * tstat[7]+ zLTR * tstat[8] + zRSI*tstat[9] + zMACD * tstat[10]+BBP*tstat[11]]
test = na.omit(test)
```

- Step2: Ranked stocks by the score and then divided into 10 quantile. Developed a zero investment portfolio by long stocks in the first quantile and short stocks in the last quantile.
```
#define long and short portfolio
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

```
![](https://github.com/JingsiTheExplorer/Backtesting/blob/master/pictures/outsample.png)

## Performance Assessment
```
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

```
![](https://github.com/JingsiTheExplorer/Backtesting/blob/master/pictures/performance.png)
However, the annualized mean return of S&P500 is 11.92%, which means our portfolio strategy can not beat the market during those years.
The alpha of CAPM, FF-3-factors model, and Corhart-4-factors model are pretty low, which means the return of our strategy can be explained mostly by market premium, size premium, value premium, and momentum.
