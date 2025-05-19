# 📈 Crypto Volatility Analysis with GARCH Models in R
This repository contains a comprehensive R-based analysis of the volatility dynamics of major cryptocurrencies: **Bitcoin (BTC)**, **Litecoin (LTC)**, and **Ethereum (ETH)**. The primary goal is to evaluate the effectiveness of **GARCH-type models** in capturing market volatility and forecasting Value at Risk (VaR).
## 📅 Dataset

- **Assets Analyzed**: Bitcoin (BTC), Litecoin (LTC), Ethereum (ETH)
- **Date Range**: *January 1, 2018 – March 31, 2023*
- **Observations**: 1915 daily data points per asset
## The analysis begins with comprehensive visualizations, including time series plots of price trends, log returns, and return distributions. This is followed by the calculation of both basic and advanced descriptive statistics such as annualized returns, annualized standard deviations, skewness, kurtosis, cross-correlation functions (e.g., CCF(rt², rt−12) and CCF(rt², rt−1)), and correlation between asset returns. To ensure robustness, several statistical tests are applied, including the Augmented Dickey-Fuller (ADF) test for stationarity, autocorrelation and partial autocorrelation checks, as well as hypothesis testing on return series and model residuals.
