# Backtesting

# Backtesting on 22 firm-characteristic factors 

1. Created three technical indicators(MACD, RSI, BBP) by using stocks' daily closing prices in Python.

2. Collected firm-characteristic data from WRDS and Bloomberg for stock universe including NYSE/AMEX/Nasdaq stocks and formed a clean dataset with 22 factors of 4488 unique stocks.

3. Run cross-sectional regression during each estimation window from 1990 to 2008 to backtesting each factor and calculated the Fama-MacBeth average and the Fama-MacBeth t-statistic for each factor.

4. Use the long-short strategy based on the weighted stock scores to validate the explanatory power of selected factors to the future stock return.
