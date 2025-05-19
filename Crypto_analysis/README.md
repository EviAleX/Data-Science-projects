# 📈 Crypto Volatility Analysis with GARCH Models in R
This repository contains a comprehensive R-based analysis of the volatility dynamics of major cryptocurrencies: **Bitcoin (BTC)**, **Litecoin (LTC)**, and **Ethereum (ETH)**. The primary goal is to evaluate the effectiveness of **GARCH-type models** in capturing market volatility and forecasting Value at Risk (VaR).
## 📅 Dataset

- **Assets Analyzed**: Bitcoin (BTC), Litecoin (LTC), Ethereum (ETH)
- **Date Range**: *January 1, 2018 – March 31, 2023*
- **Observations**: 1915 daily data points per asset
The analysis begins with comprehensive visualizations, including time series plots of price trends, log returns, and return distributions. This is followed by the calculation of both basic and advanced descriptive statistics such as annualized returns, annualized standard deviations, skewness, kurtosis, cross-correlation functions (e.g., CCF(rt², rt−12) and CCF(rt², rt−1)), and correlation between asset returns. To ensure robustness, several statistical tests are applied, including the Augmented Dickey-Fuller (ADF) test for stationarity, autocorrelation and partial autocorrelation checks, as well as hypothesis testing on return series and model residuals.
## ⚙️ GARCH Modeling

A side-by-side comparison of:
- **GARCH(1,1)**
- **EGARCH(1,1)**
- **IGARCH(1,1)**
- **Historical Simulation** as a benchmark method
After forecasting Value at Risk (VaR), all models underwent rigorous backtesting to assess predictive accuracy. The procedure focused on evaluating the number of VaR exceedances using a 365-day rolling window at a 5% confidence level. To validate the reliability of the VaR estimates, two standard statistical tests were performed: the Kupiec test for unconditional coverage and the Christoffersen test for conditional coverage.
## 📌 Conclusions

The empirical analysis demonstrated that **GARCH-type models** perform effectively in modeling the volatility of **Bitcoin**, **Ethereum** and **Litecoin**. These models provided **accurate VaR estimates**, and backtesting results supported their validity under real market conditions.

- All three GARCH models showed reliable performance in predicting risk under the defined backtesting framework.
- Results suggest these models can serve as **valuable tools for investors and traders** in managing risk and making data-driven decisions in cryptocurrency markets.
