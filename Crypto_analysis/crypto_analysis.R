## Praca dyplomowa
# Komentarze do kodu zostały napisane w obu językach 

# Ładowanie paczek 

# Lista pakietów
list_of_packages <- c("quantmod" # backtesting and portfolio optimisation
                      ,"rugarch" # modeling and forecasting financial time series
                      ,"xts" # represent and manipulate time series data
                      ,"ggplot2" # visualisation and graphics
                      ,"tidyquant"
                      ,"coinmarketcapr"
                      ,"cmcR"
                      ,"stats"
                      ,"robustbase"
                      ,"pracma"
                      ,"outliers"
                      ,"moments"
                      ,"reshape2"
                      ,"gridExtra"
                      ,"moments"
                      ,"tseries"
                      ,"nortest"
                      ,"urca"
                      ,"tidyverse"
                      ,"fitdistrplus"
                      ,"MASS"
                      ,"forecast"
                      ,"PerformanceAnalytics"
                      ,"xts"
                      ,"caret"
                      ,"dplyr"
)

# Instalacja brakujących bibliotek
not_installed <- list_of_packages[!(list_of_packages %in% installed.packages()[ , "Package"])]
if(length(not_installed)) install.packages(not_installed)

# Załadowanie bibliotek
lapply(list_of_packages, library, character = TRUE)

require(zoo)
require(ggplot2)
require(rugarch)
require(dplyr)

# 2.1 Metodologia 
# Brak możliwości załadowania daty wcześniejszej dla Ethereum z powodu braku prawidłowych sources: 
# 1. Nie ma źródeł które by nie wymagały ładowania API keys
# 2. Błądy poczas implementacji kodów z CoinMakretCap (nie istnieje biblioteki do użycia funkcji), ale jednak problem do rozwiązania
# 3. Ustalam szeregi czasowe dla Bitcoina i Ethereuma z dnia 01.01.2018 do 31.03.2023
# Data jest testową i do ustalenia 

# Download Bitcoin data from Yahoo Finance
getSymbols("BTC-USD", src = "yahoo", from = "2018-01-01", to = "2023-03-31")
# Download Ethereum data from Yahoo Finance
getSymbols("ETH-USD", src = "yahoo", from = "2018-01-01", to = "2023-03-31")
# Download Litecoin data from Yahoo Finance
getSymbols("LTC-USD", src = "yahoo", from = "2018-01-01", to = "2023-03-31")

# Sprawdzenie liczby obserwacji 
nrow(`BTC-USD`)
nrow(`ETH-USD`)
nrow(`LTC-USD`)

# Przypisanie zmiennej nowej nazwy
BTC <- `BTC-USD`
ETH <- `ETH-USD`
LTC <- `LTC-USD`

# Check for missing values 
bitcoin_missing <- sum(is.na(BTC$`BTC-USD.Close`))
eth_missing <- sum(is.na(ETH$`ETH-USD.Close`))
ltc_missing <- sum(is.na(LTC$`LTC-USD.Close`))
# No missing values 

# Dealing with outliers
# Calculate descriptive statistics
bitcoin_stats <- summary(BTC$`BTC-USD.Close`)
eth_stats <- summary(ETH$`ETH-USD.Close`)
ltc_stats <- summary(LTC$`LTC-USD.Close`)
# Combine closing prices into a single data frame
crypto_prices <- data.frame(BTC$`BTC-USD.Close`, ETH$`ETH-USD.Close`, LTC$`LTC-USD.Close`)
colnames(crypto_prices) <- c("BTC_Close", "ETH_Close", "LTC_Close")

# Check for outliers using Tukey's method
for (col in colnames(crypto_prices)) {
  Q1 <- quantile(crypto_prices[, col], 0.25, na.rm = TRUE)
  Q3 <- quantile(crypto_prices[, col], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  low_outliers <- sum(crypto_prices[, col] < (Q1 - 1.5 * IQR), na.rm = TRUE)
  high_outliers <- sum(crypto_prices[, col] > (Q3 + 1.5 * IQR), na.rm = TRUE)
  cat(col, "\n")
  cat("Low outliers:", low_outliers, "\n")
  cat("High outliers:", high_outliers, "\n")
  cat("\n")
}
# Ze wgzlędu na szregi czasowe - nie ma potrzeby usunięcia outlierów 

# Calculate descriptive statistics for Bitcoin
cat("Descriptive statistics for Bitcoin:\n\n")
summary(BTC$`BTC-USD.Close`)
cat("\n")

# Calculate descriptive statistics for Ethereum
cat("Descriptive statistics for Ethereum:\n\n")
summary(ETH$`ETH-USD.Close`)
cat("\n")

# Calculate descriptive statistics for Ethereum
cat("Descriptive statistics for Litecoin:\n\n")
summary(LTC$`LTC-USD.Close`)
cat("\n")

# 2.2 Statystyki opisowe dla Bitroina i Ethereuma i Litecoina
# The reason for using log returns instead of simple returns is that log returns have some useful statistical properties. 
# One of the most important is that log returns are approximately normally distributed, which makes it easier to model 
# and analyze their behavior. In addition, log returns have some other useful properties, such as being additive, which can 
# simplify calculations and make it easier to compare returns over different time periods.

# Using log returns also helps to address issues related to non-stationarity and volatility clustering, 
# which are common in financial data. Non-stationarity refers to the fact that the statistical properties of the data 
# change over time, which can make it difficult to model and predict future behavior. Volatility clustering refers to the fact that periods 
# of high volatility tend to be followed by other periods of high volatility, while periods of low volatility tend to be followed by other periods 
# of low volatility. By using log returns, we can help to address these issues and make our analysis more robust.

library(xts)
library(moments)

# Download data from Yahoo Finance
BTC <- getSymbols("BTC-USD", auto.assign = FALSE, from = "2018-01-01", to = "2023-03-31")
ETH <- getSymbols("ETH-USD", auto.assign = FALSE, from = "2018-01-01", to = "2023-03-31")
LTC <- getSymbols("LTC-USD", auto.assign = FALSE, from = "2018-01-01", to = "2023-03-31")

# Extract the closing prices
BTC_close <- BTC[, "BTC-USD.Close"]
ETH_close <- ETH[, "ETH-USD.Close"]
LTC_close <- LTC[, "LTC-USD.Close"]

# Calculate log-returns
BTC_log_returns <- diff(log(BTC_close))
ETH_log_returns <- diff(log(ETH_close))
LTC_log_returns <- diff(log(LTC_close))

BTC_log_returns <- na.omit(BTC_log_returns)
ETH_log_returns <- na.omit(ETH_log_returns)
LTC_log_returns <- na.omit(LTC_log_returns)

N <- 1915
BTC_mu = sum(BTC_log_returns)/N
ETH_mu = sum(ETH_log_returns)/N
LTC_mu = sum(LTC_log_returns)/N

# Dla Bitcoina
R0_BTC    <- BTC_log_returns - BTC_mu
M2_BTC    <- sum(R0_BTC^2)/N

sig_BTC <- sqrt(M2_BTC)             ## zmiennosc

Nyear <- 365
muBTC   <- BTC_mu*Nyear
sigA_BTC  <- sig_BTC*sqrt(Nyear)    ## same volatility 

# Dla Ethereuma
R0_ETH    <- ETH_log_returns - ETH_mu
M2_ETH   <- sum(R0_ETH^2)/N

sig_ETH <- sqrt(M2_ETH)       ## zmiennosc

Nyear <- 365
muETH   <- ETH_mu*Nyear
sigA_ETH  <- sig_ETH*sqrt(Nyear)

# Dla Litecoina
R0_LTC    <- LTC_log_returns - LTC_mu
M2_LTC   <- sum(R0_LTC^2)/N

sig_LTC <- sqrt(M2_LTC)       ## zmiennosc

Nyear <- 365
muLTC   <- LTC_mu*Nyear
sigA_LTC  <- sig_LTC*sqrt(Nyear)

# Calculate descriptive statistics for Bitcoin log-returns
BTC_mean <- mean(BTC_log_returns, na.rm = TRUE)
BTC_mean_annual <- BTC_mean * 365
BTC_var <- var(BTC_log_returns, na.rm = TRUE)
BTC_sd <- sd(BTC_log_returns, na.rm = TRUE)
BTC_skewness <- skewness(BTC_log_returns, na.rm = TRUE)
BTC_kurtosis <- kurtosis(BTC_log_returns, na.rm = TRUE)

# Calculate descriptive statistics for Ethereum log-returns
ETH_mean <- mean(ETH_log_returns, na.rm = TRUE)
ETH_mean_annual <- ETH_mean * 365
ETH_var <- var(ETH_log_returns, na.rm = TRUE)
ETH_sd <- sd(ETH_log_returns, na.rm = TRUE)
ETH_skewness <- skewness(ETH_log_returns, na.rm = TRUE)
ETH_kurtosis <- kurtosis(ETH_log_returns, na.rm = TRUE)

# Calculate descriptive statistics for Dogecoin log-returns
LTC_mean <- mean(LTC_log_returns, na.rm = TRUE)
LTC_mean_annual <- LTC_mean * 365
LTC_var <- var(LTC_log_returns, na.rm = TRUE)
LTC_sd <- sd(LTC_log_returns, na.rm = TRUE)
LTC_skewness <- skewness(LTC_log_returns, na.rm = TRUE)
LTC_kurtosis <- kurtosis(LTC_log_returns, na.rm = TRUE)

# Print the descriptive statistics
cat("Bitcoin log-returns:\n")
cat(paste("Mean:", BTC_mean, "\n"))
cat(paste("Average annual return:", BTC_mean_annual, "\n"))
cat(paste("Variance:", BTC_var, "\n"))
cat(paste("Standard Deviation:", BTC_sd, "\n"))
cat(paste("Skewness:", BTC_skewness, "\n"))
cat(paste("Kurtosis:", BTC_kurtosis, "\n\n"))

cat("Ethereum log-returns:\n")
cat(paste("Mean:", ETH_mean, "\n"))
cat(paste("Average annual return:", ETH_mean_annual, "\n"))
cat(paste("Variance:", ETH_var, "\n"))
cat(paste("Standard Deviation:", ETH_sd, "\n"))
cat(paste("Skewness:", ETH_skewness, "\n"))
cat(paste("Kurtosis:", ETH_kurtosis, "\n\n"))

cat("Litecoin log-returns:\n")
cat(paste("Mean:", LTC_mean, "\n"))
cat(paste("Average annual return:", LTC_mean_annual, "\n"))
cat(paste("Variance:", LTC_var, "\n"))
cat(paste("Standard Deviation:", LTC_sd, "\n"))
cat(paste("Skewness:", LTC_skewness, "\n"))
cat(paste("Kurtosis:", LTC_kurtosis, "\n"))

# ACF(r) i ACF(r^2)
# Bitcoin
BTC_log_returns_sq <- BTC_log_returns^2

# Calculate ACF(r) and ACF (r^2)
acf_r_BTC <- acf(BTC_log_returns, plot = FALSE)
acf_r2_BTC <- acf(BTC_log_returns_sq, plot = FALSE)
print(acf_r_BTC)
print(acf_r2_BTC)

# Ethereum
ETH_log_returns_sq <- ETH_log_returns^2

# Calculate ACF(r) and ACF (r^2)
acf_r_ETH <- acf(ETH_log_returns, plot = FALSE)
acf_r2_ETH <- acf(ETH_log_returns_sq, plot = FALSE)
print(acf_r_ETH)
print(acf_r2_ETH)

# Litecoin
LTC_log_returns_sq <- LTC_log_returns^2

# Calculate ACF(r) and ACF (r^2)
acf_r_LTC <- acf(LTC_log_returns, plot = FALSE)
acf_r2_LTC <- acf(LTC_log_returns_sq, plot = FALSE)
print(acf_r_LTC)
print(acf_r2_LTC)

# Korelacja 
cor(BTC_close, ETH_close)
cor(ETH_close, LTC_close)
cor(BTC_close, LTC_close)

# CCF volatility clustering and leverage effect for Bitcoin
BTC_num <- as.numeric(coredata(BTC_log_returns))
BTC_num_sq <- as.numeric(coredata(BTC_log_returns_sq))

BTC_lagged <- c(rep(NA, 1), BTC_num[1:(length(BTC_num)-1)])
BTC_lagged_sq <- c(rep(NA, 1), BTC_num_sq[1:(length(BTC_num_sq)-1)])

BTC_log_returns_clean <- BTC_log_returns[-1]
BTC_log_returns_sq_clean <- BTC_log_returns_sq[-1]
BTC_lagged_clean <- BTC_lagged[-1]
BTC_lagged_sq_clean <- BTC_lagged_sq[-1]

# Fit linear regression models to check for leverage effect and volatility clustering
lm_leverage <- lm(BTC_log_returns_sq_clean ~ BTC_lagged_clean)
lm_clustering <- lm(BTC_log_returns_sq_clean ~ BTC_lagged_sq_clean)

# Print the regression coefficients to check for the presence of leverage effect and volatility clustering
cat("Leverage effect:", coef(lm_leverage)[2], "\n")
cat("Volatility clustering:", coef(lm_clustering)[2], "\n")

# CCF volatility clustering and leverage effect for Ethereum
ETH_num <- as.numeric(coredata(ETH_log_returns))
ETH_num_sq <- as.numeric(coredata(ETH_log_returns_sq))

ETH_lagged <- c(rep(NA, 1), ETH_num[1:(length(ETH_num)-1)])
ETH_lagged_sq <- c(rep(NA, 1), ETH_num_sq[1:(length(ETH_num_sq)-1)])

ETH_log_returns_clean <- ETH_log_returns[-1]
ETH_log_returns_sq_clean <- ETH_log_returns_sq[-1]
ETH_lagged_clean <- ETH_lagged[-1]
ETH_lagged_sq_clean <- ETH_lagged_sq[-1]

# Fit linear regression models to check for leverage effect and volatility clustering
lm_leverage_ETH <- lm(ETH_log_returns_sq_clean ~ ETH_lagged_clean)
lm_clustering_ETH <- lm(ETH_log_returns_sq_clean ~ ETH_lagged_sq_clean)

# Print the regression coefficients to check for the presence of leverage effect and volatility clustering
cat("Leverage effect:", coef(lm_leverage_ETH)[2], "\n")
cat("Volatility clustering:", coef(lm_clustering_ETH)[2], "\n")

# CCF volatility clustering and leverage effect for Litecoin
LTC_num <- as.numeric(coredata(LTC_log_returns))
LTC_num_sq <- as.numeric(coredata(LTC_log_returns_sq))

LTC_lagged <- c(rep(NA, 1), LTC_num[1:(length(LTC_num)-1)])
LTC_lagged_sq <- c(rep(NA, 1), LTC_num_sq[1:(length(LTC_num_sq)-1)])

LTC_log_returns_clean <- LTC_log_returns[-1]
LTC_log_returns_sq_clean <- LTC_log_returns_sq[-1]
LTC_lagged_clean <- LTC_lagged[-1]
LTC_lagged_sq_clean <- LTC_lagged_sq[-1]

# Fit linear regression models to check for leverage effect and volatility clustering
lm_leverage_LTC <- lm(LTC_log_returns_sq_clean ~ LTC_lagged_clean)
lm_clustering_LTC <- lm(LTC_log_returns_sq_clean ~ LTC_lagged_sq_clean)

# Print the regression coefficients to check for the presence of leverage effect and volatility clustering
cat("Leverage effect:", coef(lm_leverage_LTC)[2], "\n")
cat("Volatility clustering:", coef(lm_clustering_LTC)[2], "\n")

# 2.3 Prezentacja plotów 
# Create plots
par(mfrow=c(3,1))
plot(BTC_close, type="l", main="Bitcoin Daily Closing Prices", ylab="Price (USD)")
plot(ETH_close, type="l", main="Ethereum Daily Closing Prices", ylab="Price (USD)")
plot(LTC_close, type="l", main="Litecoin Daily Closing Prices", ylab="Price (USD)")

# Bitcoin daily closing prices plot
BTC_df <- data.frame(Date = index(BTC_close), Price = BTC$`BTC-USD.Close`)
BTC_plot <- ggplot(BTC_df, aes(Date, BTC.USD.Close)) +
  geom_line(color = "#1f77b4", size = 1) +
  labs(title = "Bitcoin Daily Closing Prices", x = "", y = "Price (USD)") +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        axis.line = element_line(size = 1),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid', color = "#a6a6a6"),
        panel.grid.minor = element_line(size = 0.1, linetype = 'dashed', color = "#d9d9d9"),
        panel.background = element_rect(fill = "#f0f0f0", color = NA))

# Ethereum daily closing prices plot
ETH_df <- data.frame(Date = index(ETH_close), Price = ETH_close)
ETH_plot <- ggplot(ETH_df, aes(Date, ETH.USD.Close)) +
  geom_line(color = "#1f77b4", size = 1) +
  labs(title = "Ethereum Daily Closing Prices", x = "", y = "Price (USD)") +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        axis.line = element_line(size = 1),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid', color = "#a6a6a6"),
        panel.grid.minor = element_line(size = 0.1, linetype = 'dashed', color = "#d9d9d9"),
        panel.background = element_rect(fill = "#f0f0f0", color = NA))

# Print the plots
BTC_plot
ETH_plot

# Bitcoin log returns plot
BTC_log_returns_df <- data.frame(Date = index(BTC_log_returns), Log_Return = BTC_log_returns)
BTC_log_returns_plot <- ggplot(BTC_log_returns_df, aes(Date, BTC.USD.Close)) +
  geom_line(color = "#1f3a93", size = 1) +
  labs(title = "Bitcoin Daily Log Returns", x = "", y = "Log Return") +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        axis.line = element_line(size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#f0f0f0", color = NA))

# Ethereum log returns plot
ETH_log_returns_df <- data.frame(Date = index(ETH_log_returns), Log_Return = ETH_log_returns)
ETH_log_returns_plot <- ggplot(ETH_log_returns_df, aes(Date, ETH.USD.Close)) +
  geom_line(color = "#1f3a93", size = 1) +
  labs(title = "Ethereum Daily Log Returns", x = "", y = "Log Return") +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        axis.line = element_line(size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#f0f0f0", color = NA))

# Litecoin log returns plot
LTC_log_returns_df <- data.frame(Date = index(LTC_log_returns), Log_Return = LTC_log_returns)
LTC_log_returns_plot <- ggplot(LTC_log_returns_df, aes(Date, LTC.USD.Close)) +
  geom_line(color = "#1f3a93", size = 1) +
  labs(title = "Litecoin Daily Log Returns", x = "", y = "Log Return") +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        axis.line = element_line(size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#f0f0f0", color = NA))

BTC_log_returns_plot
ETH_log_returns_plot
LTC_log_returns_plot

# Cena na skali logarytmicznej + log returns dla dwóch aktywów 
par(mfrow = c(2, 2))
plot(log(BTC_close), col = "#1f3a93", 
     main = "Wartość log cen Bitcoina", ylab = "Log cena")
plot(log(LTC_close), col = "#1f3a93", 
     main = "Wartość log cen Litecoina", ylab = "Log cena")
plot(BTC_log_returns, type = "l", col = "#1f3a93", 
     main = "Log stopy zwrotu Bitcoina", ylab = "Log stopa zwrotu")
plot(LTC_log_returns, type = "l", col = "#1f3a93", 
     main = "Log stopy zwrotu Litecoina", ylab = "Log stopa zwrotu")

# Standaryzowane reszty dla Bitcoina
# Fit a linear regression model to Bitcoin log returns
lm_bitcoin <- lm(BTC_log_returns ~ 1)

# Obtain the residuals
resid_bitcoin <- residuals(lm_bitcoin)

# Standardize the residuals
std_resid_bitcoin <- resid_bitcoin / sd(resid_bitcoin)
std_resid_bitcoin_sq <- std_resid_bitcoin^2

# Standaryzowane reszty dla Ethereum
# Fit a linear regression model to Bitcoin log returns
lm_ethereum <- lm(ETH_log_returns ~ 1)

# Obtain the residuals
resid_ethereum <- residuals(lm_ethereum)

# Standardize the residuals
std_resid_ethereum <- resid_ethereum / sd(resid_ethereum)
std_resid_ethereum_sq <- std_resid_ethereum^2

# Standaryzowane reszty dla Litecoin
# Fit a linear regression model to Bitcoin log returns
lm_litecoin <- lm(LTC_log_returns ~ 1)

# Obtain the residuals
resid_litecoin <- residuals(lm_litecoin)

# Standardize the residuals
std_resid_litecoin <- resid_litecoin / sd(resid_litecoin)
std_resid_litecoin_sq <- std_resid_litecoin^2

# Plot the standardized residuals against time
par(mfrow = c(2, 1))
plot(index(BTC_log_returns), std_resid_bitcoin, type = "l",
     xlab = "Date", ylab = "Standardized Residuals",
     main = "Standardized Residuals for Bitcoin")
plot(index(LTC_log_returns), std_resid_litecoin, type = "l",
     xlab = "Date", ylab = "Standardized Residuals",
     main = "Standardized Residuals for Litecoin")

# Plot the square of standardized residuals against time
par(mfrow = c(2, 1))
plot(index(BTC_log_returns), std_resid_bitcoin_sq, type = "l",
     xlab = "Date", ylab = "Squared Standardized Residuals",
     main = "Squared Standardized Residuals for Bitcoin")
plot(index(LTC_log_returns), std_resid_litecoin_sq, type = "l",
     xlab = "Date", ylab = "Squared Standardized Residuals",
     main = "Squared Standardized Residuals for Litecoin")

# Density plot for Bitcoin
R0_BTC <- (BTC_log_returns-BTC_mu)/sig_BTC
bwdth     <- 0.1

ggplot(data.frame(R0_BTC), aes(x = R0_BTC)) +
  theme_bw() +
  geom_histogram(binwidth = bwdth, colour = "white", fill = "yellow4", size = 0.1) +
  stat_function(fun = function(x) dnorm(x)*N*bwdth, color = "red", size = 1) +
  xlim(-5, 5) +
  ylim(0, 150)

# Density plot for Ethereum
R0_ETH <- (ETH_log_returns-ETH_mu)/sig_ETH
bwdth     <- 0.1

ggplot(data.frame(R0_ETH), aes(x = R0_ETH)) +
  theme_bw() +
  geom_histogram(binwidth = bwdth, colour = "white", fill = "yellow4", size = 0.1) +
  stat_function(fun = function(x) dnorm(x)*N*bwdth, color = "red", size = 1)                        # rozklad normalny

# Density plot for Litecoin
R0_LTC <- (LTC_log_returns-LTC_mu)/sig_LTC
bwdth     <- 0.1

ggplot(data.frame(R0_LTC), aes(x = R0_LTC)) +
  theme_bw() +
  geom_histogram(binwidth = bwdth, colour = "white", fill = "yellow4", size = 0.1) +
  stat_function(fun = function(x) dnorm(x)*N*bwdth, color = "red", size = 1)  +
  xlim(-5, 5) +
  ylim(0, 150)

# QQ plot for Bitcoin
# Create a QQ plot for Bitcoin log-returns
R0_BTC <- (BTC_log_returns-BTC_mu)/sig_BTC

# Metoda momentow (K - kurtoza)
v0 <- 4 + 6/(BTC_kurtosis-3)
require(MASS)
d0 <- fitdistr(R0_BTC, "normal")
d1 <- fitdistr(R0_BTC, "t", m = 0, start = list(s=sqrt((v0-2)/v0), df=v0), lower=c(0.001,3))
v=d1$estimate[[2]]

# QQ plot
q        <- seq(0.001, 0.999, 0.001)
Qemp     <- quantile(R0_BTC,q)                 # kwantyl empiryczny
Qteo     <- qt(q,v)*sqrt((v-2)/v)              # rozklad t-Studenta (wariancja to v/v-2)
lim0    <- c(-5,5)                             # zakres na wykresie
par(mfrow=c(1,1), cex = 0.7, bty="l")
plot(Qemp,Qteo, main="QQplot dla Bitcoina", col="red", xlim = lim0, ylim = lim0,
     xlab="kwantyl empiryczny", ylab="kwantyl teoretyczny") # plots the results
abline(a=0,b=1, lwd=2)

# QQ plot for Ethereum
# Create a QQ plot for Ethereum log-returns
R0_ETH <- (ETH_log_returns-ETH_mu)/sig_ETH

# Metoda momentow (K - kurtoza)
v0 <- 4 + 6/(ETH_kurtosis-3)
require(MASS)
d0 <- fitdistr(R0_ETH, "normal")
d1 <- fitdistr(R0_ETH, "t", m = 0, start = list(s=sqrt((v0-2)/v0), df=v0), lower=c(0.001,3))
v=d1$estimate[[2]]

# QQ plot
q        <- seq(0.001, 0.999, 0.001)
Qemp     <- quantile(R0_ETH,q)                 # kwantyl empiryczny
Qteo     <- qt(q,v)*sqrt((v-2)/v)              # rozklad t-Studenta (wariancja to v/v-2)
lim0    <- c(-5,5)                             # zakres na wykresie
par(mfrow=c(1,1), cex = 0.7, bty="l")
plot(Qemp,Qteo, main="QQplot", col="red", xlim = lim0, ylim = lim0,
     xlab="kwantyl empiryczny", ylab="kwantyl teoretyczny") # plots the results
abline(a=0,b=1, lwd=2)

# QQ plot for Litecoin
# Create a QQ plot for Litecoin log-returns
R0_LTC <- (LTC_log_returns-LTC_mu)/sig_LTC

# Metoda momentow (K - kurtoza)
v0 <- 4 + 6/(LTC_kurtosis-3)
require(MASS)
d0 <- fitdistr(R0_LTC, "normal")
d1 <- fitdistr(R0_LTC, "t", m = 0, start = list(s=sqrt((v0-2)/v0), df=v0), lower=c(0.001,3))
v=d1$estimate[[2]]

# QQ plot
q        <- seq(0.001, 0.999, 0.001)
Qemp     <- quantile(rnorm(N),q)                 # kwantyl empiryczny
Qteo     <- quantile(R0_LTC,q)               # rozklad t-Studenta (wariancja to v/v-2)
lim0    <- c(-5,5)                             # zakres na wykresie
par(mfrow=c(1,1), cex = 0.7, bty="l")
plot(Qemp,Qteo, main="QQplot dla Litecoina", col="red", xlim = lim0, ylim = lim0,
     xlab="kwantyl empiryczny", ylab="kwantyl teoretyczny") # plots the results
abline(a=0,b=1, lwd=2)

# Zmiana na LTC dla Litecoina
R <- as.numeric(LTC_log_returns)

# ACF, CCF for Bitcoin 
# ACF dla stop zwrotu 
z0 <- acf(R,20, plot=TRUE)
plot(z0$acf[2:20], type="h", main="ACF dla stop zwrotu", xlab="Lag", ylab="ACF", ylim=c(-0.2,0.2), las=1)
abline(h=c(2/sqrt(length(R)),0,-2/sqrt(length(R))),lty=c(2,1,2))

# ACF dla kwadratow stop zwrotu
z1 <- acf(R^2,20, plot=FALSE)
plot(z1$acf[2:20], type="h", main="ACF dla kwadratow stop zwrotu", xlab="Lag", ylab="ACF", ylim=c(-0.2,0.4), las=1)
abline(h=c(2/sqrt(length(R)),0,-2/sqrt(length(R))),lty=c(2,1,2))

z2 <- ccf(R^2, R, 20, type="correlation", plot=TRUE)
plot(z2$acf[22:40], type="h", main="korelacja kwadratow stop zwrotu i opoznionych st. zwrotu", xlab="Lag", ylab="ACF", ylim=c(-0.2,0.2), las=1)
abline(h=c(2/sqrt(length(R)),0,-2/sqrt(length(R))),lty=c(2,1,2))

plot(z2$acf[20:1], type="h", main="korelacja st. zwrotu i opoznionych kwadratow st. zwrotu", xlab="Lag", ylab="ACF", ylim=c(-0.2,0.2), las=1)
abline(h=c(2/sqrt(length(R)),0,-2/sqrt(length(R))),lty=c(2,1,2))

# panel wykresow
par(mfrow=c(2,2), cex = 0.7, bty="l")
plot(z0$acf[2:20], type="h", main="ACF dla stop zwrotu", xlab="Lag", ylab="ACF", ylim=c(-0.2,0.2), las=1)
abline(h=c(2/sqrt(length(R)),0,-2/sqrt(length(R))),lty=c(2,1,2))
plot(z1$acf[2:20], type="h", main="ACF dla kwadratow stop zwrotu", xlab="Lag", ylab="ACF", ylim=c(-0.2,0.4), las=1)
abline(h=c(2/sqrt(length(R)),0,-2/sqrt(length(R))),lty=c(2,1,2))
plot(z2$acf[22:40], type="h", main="korelacja kw. st. zwrotu i op. st. zw.", xlab="Lag", ylab="ACF", ylim=c(-0.2,0.2), las=1)
abline(h=c(2/sqrt(length(R)),0,-2/sqrt(length(R))),lty=c(2,1,2))
plot(z2$acf[20:1], type="h", main="korelacja st. zw. i op. kw. st. zw.", xlab="Lag", ylab="ACF", ylim=c(-0.2,0.2), las=1)
abline(h=c(2/sqrt(length(R)),0,-2/sqrt(length(R))),lty=c(2,1,2))

# Corelacja 
btc_returns <- dailyReturn(BTC_close)
eth_returns <- dailyReturn(ETH_close)
ltc_returns <- dailyReturn(LTC_close)

returns_df <- data.frame(BTC = btc_returns,
                         ETH = eth_returns,
                         LTC = ltc_returns)

# Create a scatter plot matrix
chart.Correlation(returns_df, method = "pearson")

df <- data.frame(Date = index(`BTC-USD`), 
                 Bitcoin = as.numeric(coredata(`BTC-USD`[,4])),
                 Ethereum = as.numeric(coredata(`ETH-USD`[,4])),
                 Litecoin = as.numeric(coredata(`LTC-USD`[,4])))

# Perform the Jarque-Bera test for Bitcoin
jarque.test(df$Bitcoin)

# Perform the Jarque-Bera test for Ethereum
jarque.test(df$Ethereum)

# Perform the Jarque-Bera test for Litecoin
jarque.test(df$Litecoin)

# Perform the Ljung-Box test for all cryptos (basic)
Box.test(coredata(BTC_log_returns), type = "Ljung-Box", lag = 20)
Box.test(coredata(ETH_log_returns), type = "Ljung-Box", lag = 20)
Box.test(coredata(LTC_log_returns), type = "Ljung-Box", lag = 20)

# Perform the Ljung-Box test for all cryptos (^2)
Box.test(coredata(BTC_log_returns^2), type = "Ljung-Box", lag = 20)
Box.test(coredata(ETH_log_returns^2), type = "Ljung-Box", lag = 20)
Box.test(coredata(LTC_log_returns^2), type = "Ljung-Box", lag = 20)

# Perform the Shapiro-Wilk test for Bitroin
sw_bitcoin <- shapiro.test(df$Bitcoin)
print(paste("Shapiro-Wilk test for Bitcoin: W =", sw_bitcoin$statistic, ", p-value =", sw_bitcoin$p.value))

# Perform the Shapiro-Wilk test for Ethereum
sw_ethereum <- shapiro.test(df$Ethereum)
print(paste("Shapiro-Wilk test for Ethereum: W =", sw_ethereum$statistic, ", p-value =", sw_ethereum$p.value))

# Perform the Shapiro-Wilk test for Litecoin
sw_litecoin <- shapiro.test(df$Litecoin)
print(paste("Shapiro-Wilk test for Dogecoin: W =", sw_litecoin$statistic, ", p-value =", sw_ethereum$p.value))

# ADF test for Bitcoin
adf_BTC <- adf.test(BTC_log_returns, k=20)
print(adf_BTC)

# ADF test for Ethereum
adf_ETH <- adf.test(ETH_log_returns)
print(adf_ETH)

# ADF test for Litecoin
adf_LTC <- adf.test(LTC_log_returns, k=20)
print(adf_LTC)

BTC_log_returns <- na.omit(BTC_log_returns)
ETH_log_returns <- na.omit(ETH_log_returns)
LTC_log_returns <- na.omit(LTC_log_returns)

# ADF test for Bitcoin
# with trend and lags
adf_BTC_1 <- ur.df(BTC_log_returns, type = "trend", lags = 1)
adf_BTC_2 <- ur.df(BTC_log_returns, type = "trend", lags = 2)
adf_BTC_3 <- ur.df(BTC_log_returns, type = "trend", lags = 3)
adf_BTC_4 <- ur.df(BTC_log_returns, type = "trend", lags = 4)

# Print the ADF test results for Bitcoin
print(adf_BTC_1@teststat)
print(adf_BTC_1@cval)
print(adf_BTC_1@pvalue)

print(adf_BTC_2@teststat)
print(adf_BTC_2@cval)
print(adf_BTC_2@pvalue)

print(adf_BTC_3@teststat)
print(adf_BTC_3@cval)
print(adf_BTC_3@pvalue)

print(adf_BTC_4@teststat)
print(adf_BTC_4@cval)
print(adf_BTC_4@pvalue)

# 2.4. VAR i ES
# VAR i ES for historical simulation
p <- 0.05
VaR_hist_BTC <- quantile(BTC_log_returns, p)
ES_hist_BTC <- mean(head(sort(BTC_log_returns[BTC_log_returns <= VaR_hist_BTC]), -1))
VaR_hist_BTC
ES_hist_BTC

VaR_hist_ETH <- quantile(ETH_log_returns, p)
ES_hist_ETH <- mean(head(sort(ETH_log_returns[ETH_log_returns <= VaR_hist_BTC]), -1))
print(VaR_hist_ETH)
print(ES_hist_ETH)

VaR_hist_LTC <- quantile(LTC_log_returns, p)
ES_hist_LTC <- mean(head(sort(LTC_log_returns[LTC_log_returns <= VaR_hist_LTC]), -1))
print(VaR_hist_LTC)
print(ES_hist_LTC)

par(mfrow = c(3, 1))

# Bitcoin plot
hist(BTC_log_returns, breaks = 50, main = "Bitcoin Daily Returns")
abline(v = VaR_hist_BTC, col = "red", lwd = 2)
abline(v = ES_hist_BTC, col = "blue", lwd = 2)
legend("topleft", legend = c(paste0("VaR (", round(VaR_hist_BTC * 100, 2), "%)"), paste0("ES (", round(ES_hist_BTC * 100, 2), "%)")),
       col = c("red", "blue"), lwd = 2)

# Ethereum plot
hist(ETH_log_returns, breaks = 50, main = "Ethereum Daily Returns")
abline(v = VaR_hist_ETH, col = "red", lwd = 2)
abline(v = ES_hist_ETH, col = "blue", lwd = 2)
legend("topleft", legend = c(paste0("VaR (", round(VaR_hist_ETH * 100, 2), "%)"), paste0("ES (", round(ES_hist_ETH * 100, 2), "%)")),
       col = c("red", "blue"), lwd = 2)

# Litecoin plot
hist(LTC_log_returns, breaks = 50, main = "Litecoin Daily Returns")
abline(v = VaR_hist_LTC, col = "red", lwd = 2)
abline(v = ES_hist_LTC, col = "blue", lwd = 2)
legend("topleft", legend = c(paste0("VaR (", round(VaR_hist_LTC * 100, 2), "%)"), paste0("ES (", round(ES_hist_LTC * 100, 2), "%)")),
       col = c("red", "blue"), lwd = 2)

# For example, the output VaR_hist_BTC = -3.44% means that there is a 5% chance that the daily return on Bitcoin will be worse than -3.44%. 
# Similarly, the output ES_hist_BTC = -4.53% means that if the daily return on Bitcoin is worse than the VaR, then on average, the losses will 
# be around 4.53% per day.

# Likewise, for Ethereum, the output VaR_hist_ETH = -7.74% means that there is a 5% chance that the daily return 
# on Ethereum will be worse than -7.74%. And the output ES_hist_ETH = -10.26% means that if the daily return on Ethereum 
# is worse than the VaR, then on average, the losses will be around 10.26% per day.

# Change r for BTC or LTC
r <- LTC_log_returns
mu    <- mean(r)
sigma <- sd(r)

T  <- length(r)                          # liczba obserwacji
p  <- 0.05                               # poziom tolerancji VaR
H  <- 1                                  # horyont

N     <- 1915                             # ustalamy probe na podstawie ktorej liczymy VaR (5 lat)
r     <- tail(r,N)                        # dane
R     <- coredata(r)

lambda      <- 0.94                       # parametr wygladzajacy
q    <- qdist("std", p=p, shape=5)
qf   <- function(x) qdist("std", p=x, shape=5)

# Estymacja wszystkich parametrow: GARCH(1,1)  
GARCHspec <- ugarchspec(mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                        variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                        fixed.pars=list(shape=5), distribution.model = "std")
#                        distribution.model = "norm")

GARCHfit <- ugarchfit(data = r, spec = GARCHspec, solver="hybrid")
round(coef(GARCHfit),5)

# VaR i ES (rozklad t-Studenta o 5 stopniach swobody)
GARCHvar   <- fitted(GARCHfit) + sigma(GARCHfit)*q 
GARCHes    <- fitted(GARCHfit) + sigma(GARCHfit)*(1/p * integrate(qf, 0, p)$value)

# wykresy
par(mfrow=c(2,1), cex = 0.7, bty="l")
plot(GARCHfit, which=1) 
plot(merge( r, GARCHvar, GARCHes), plot.type="single", col=c(1,2,3), main=paste(100*p,"% VaR i ES z modelu GARCH(1,1))",  sep=""), ylab="" )
legend("bottomright", c("VaR", "ES"), lty=1, col=2:3)

# Obliczenia VaR i ES dla ostatniego okresu
GARCHfct   <- ugarchforecast(GARCHfit,n.ahead = 1)
mT  <- as.numeric(fitted(GARCHfct))
sT  <- as.numeric(sigma(GARCHfct))

VaR_GARCH  = mT + sT*q
ES_GARCH   = mT + sT*(1/p * integrate(qf, 0, p)$value)

par(mfrow = c(2, 1))

# plots for GARCH(1,1)
hist(r, breaks = 50, main = "Bitcoin Daily Returns")
abline(v = VaR_GARCH, col = "red", lwd = 2)
abline(v = ES_GARCH, col = "blue", lwd = 2)
legend("topleft", legend = c(paste0("VaR (", round(VaR_GARCH * 100, 2), "%)"), paste0("ES (", round(ES_GARCH * 100, 2), "%)")),
       col = c("red", "blue"), lwd = 2)

# Efekt dźwigni: model eGARCH(1,1) 
EGARCHspec <- ugarchspec(mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                         variance.model=list(model="eGARCH", garchOrder=c(1,1)),
                         fixed.pars=list(shape=5), distribution.model = "std")
#                        distribution.model = "norm")

EGARCHfit <- ugarchfit(data = r, spec = EGARCHspec, solver="hybrid")
round(coef(EGARCHfit),5)

# VaR i ES (rozklad t-Studenta o 5 stopniach swobody)
EGARCHvar   <- fitted(EGARCHfit) + sigma(EGARCHfit)*q 
EGARCHes    <- fitted(EGARCHfit) + sigma(EGARCHfit)*(1/p * integrate(qf, 0, p)$value)

# wykresy
par(mfrow=c(2,1), cex = 0.7, bty="l")
plot(EGARCHfit, which=1)
plot(merge( r, EGARCHvar, EGARCHes), plot.type="single", col=c(1,2,3), main=paste(100*p,"% VaR i ES z modelu EGARCH(1,1))",  sep=""), ylab="" )
legend("bottomright", c("VaR", "ES"), lty=1, col=2:3)

# Obliczenia VaR i ES dla ostatniego okresu
EGARCHfct   <- ugarchforecast(EGARCHfit,n.ahead = 1)
mT  <- as.numeric(fitted(EGARCHfct))
sT  <- as.numeric(sigma(EGARCHfct))

VaR_EGARCH  = mT + sT*q
ES_EGARCH   = mT + sT*(1/p * integrate(qf, 0, p)$value)

# Estymacja parametru lambda w modelu EWMA: model IGARCH  
IGARCHspec <- ugarchspec(mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                         variance.model=list(model="iGARCH", garchOrder=c(1,1)),
                         fixed.pars=list(omega=0,shape=5), start.pars = list(alpha1=0.06), distribution.model = "std")
#                      fixed.pars=list(omega=0), start.pars = list(alpha1=0.06), distribution.model = "norm")

IGARCHfit <- ugarchfit(data = r, spec = IGARCHspec, solver="hybrid")
round(coef(IGARCHfit),3)

# VaR i ES 
IGARCHvar <- sigma(IGARCHfit)*q 
IGARCHes  <- sigma(IGARCHfit)*(1/p * integrate(qf, 0, p)$value)

# wykresy
par(mfrow=c(2,1), cex = 0.7, bty="l")
plot(IGARCHfit, which=1)
plot(merge( r, IGARCHvar, IGARCHes), plot.type="single", col=c(1,2,3), main=paste(100*p,"% VaR i ES z estymowanego modelu EWMA (IGARCH)",  sep=""), ylab="" )
legend("bottomright", c("VaR", "ES"), lty=1, col=2:3)

# Obliczenia VaR i ES dla ostatniego okresu
IGARCHfct   <- ugarchforecast(IGARCHfit,n.ahead = 1)
mT  <- as.numeric(fitted(IGARCHfct))
sT  <- as.numeric(sigma(IGARCHfct))

VaR_IGARCH  = mT + sT*q
ES_IGARCH   = mT + sT*(1/p * integrate(qf, 0, p)$value)

# AR-GARCH
# Specify AR-GARCH model
ARGARCHspec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(1, 0), include.mean = TRUE))

# Fit AR-GARCH model
ARGARCHfit <- ugarchfit(spec = ARGARCHspec, data = r)

# Obliczenia VaR i ES dla ostatniego okresu
ARGARCHfct   <- ugarchforecast(ARGARCHfit,n.ahead = 1)
mT  <- as.numeric(fitted(ARGARCHfct))
sT  <- as.numeric(sigma(ARGARCHfct))

VaR_ARGARCH  = mT + sT*q
ES_ARGARCH   = mT + sT*(1/p * integrate(qf, 0, p)$value)

# Backtesting new
library(binom)
library(knitr)
library(car)
library(quantreg)

# zmiana r w zależności aktywa
r <- diff(log(LTC_close))               
r <- na.omit(r)
r <- zoo(r)

N <- 1915
r <- tail(r,N)

p   <- 0.05 
backN    <- 365                   # dlugosc danych do backtestu
w_length <- 1550

## Obliczenie VaR, realizacji oraz przekrocze? VaR. Symulacja historyczna 
varHS  <- rollapply(r, width=w_length,
                    function(w){quantile(w,p)},
                    by=1, align="right")

varHS  <- stats::lag(varHS, -1)           # przesuwamy indeksy aby uwzglednic, ze VaR(t+1) = quantile(t)
rr     <- r[index(varHS)]          # zrealizowane zwroty, rr - realized returns
etaHS  <- tail(rr<=varHS, backN)  # szereg przekroczen (VaR violations)
nHS    <- sum(etaHS); nHS      # l. przekroczen
piHS   <- mean(etaHS); piHS    # udzial przekroczen

dev.off()
VaRplot(alpha=p, actual=tail(rr,backN), VaR=tail(varHS,backN))
title(main="Symulacja historyczna: przekroczenia VaR")

temp <- VaRTest(alpha=p, actual=coredata(rr), VaR=coredata(varHS))
paste0("Statystyka testu Kupca: ", temp$uc.LRstat,"; p-value:", temp$uc.LRp)
paste0("Statystyka testu Ch2: ", temp$cc.LRstat,"; p-value:", temp$cc.LRp)

# podsatwiamy obliczenia dla wybranej metody

var = varHS
eta = etaHS
pi  = piHS
n1  = nHS

# fixed.pars=list(shape=5), distribution.model = "std")

varGARCH <- rollapply(r, width=w_length,
                      function(w) {
                        fit <- ugarchfit(data=w, spec=GARCHspec, solver="hybrid")
                        frc <- ugarchforecast(fit, n.ahead=1)
                        quantile(frc, p)
                      },
                      by=1, align="right")
varGARCH <- stats::lag(varGARCH, -1)
rrGARCH     <- r[index(varGARCH)]          # zrealizowane zwroty, rr - realized returns
etaGARCH  <- tail(rrGARCH<=varGARCH, backN)  # szereg przekroczen (VaR violations)
nGARCH    <- sum(etaGARCH); nGARCH      # l. przekroczen
piGARCH   <- mean(etaGARCH); piGARCH    # udzial przekroczen

varEGARCH <- rollapply(r, width=w_length,
                      function(w) {
                        fit <- ugarchfit(data=w, spec=EGARCHspec, solver="hybrid")
                        frc <- ugarchforecast(fit, n.ahead=1)
                        quantile(frc, p)
                      },
                      by=1, align="right")
varEGARCH <- stats::lag(varEGARCH, -1)
rrEGARCH     <- r[index(varEGARCH)]          # zrealizowane zwroty, rr - realized returns
etaEGARCH  <- tail(rrEGARCH<=varEGARCH, backN)  # szereg przekroczen (VaR violations)
nEGARCH    <- sum(etaEGARCH); nEGARCH      # l. przekroczen
piEGARCH   <- mean(etaEGARCH); piEGARCH    # udzial przekroczen

varIGARCH <- rollapply(r, width=w_length,
                       function(w) {
                         fit <- ugarchfit(data=w, spec=IGARCHspec, solver="hybrid")
                         frc <- ugarchforecast(fit, n.ahead=1)
                         quantile(frc, p)
                       },
                       by=1, align="right")
varIGARCH <- stats::lag(varIGARCH, -1)
rrIGARCH     <- r[index(varIGARCH)]          # zrealizowane zwroty, rr - realized returns
etaIGARCH  <- tail(rrIGARCH<=varIGARCH, backN)  # szereg przekroczen (VaR violations)
nIGARCH    <- sum(etaIGARCH); nIGARCH      # l. przekroczen
piIGARCH   <- mean(etaIGARCH); piIGARCH    # udzial przekroczen

par(mfrow = c(2, 2))
VaRplot(alpha=p, actual=tail(rr,backN), VaR=tail(varHS,backN))
title(main="Symulacja historyczna: przekroczenia VaR")
VaRplot(alpha=p, actual=rr, VaR=varGARCH)
title(main="GARCH: przekroczenia VaR")
VaRplot(alpha=p, actual=rr, VaR=varEGARCH)
title(main="EGARCH: przekroczenia VaR")
VaRplot(alpha=p, actual=rr, VaR=varIGARCH)
title(main="IGARCH: przekroczenia VaR")

temp <- VaRTest(alpha=p, actual=coredata(rrGARCH), VaR=coredata(varGARCH))
paste0("Statystyka testu Kupca: ", temp$uc.LRstat,"; p-value:", temp$uc.LRp)
paste0("Statystyka testu Ch2: ", temp$cc.LRstat,"; p-value:", temp$cc.LRp)

#############################################
# Backtesing z ugarchroll z pakietu rugarch #
#############################################

step = 10; # how often we reestimate the GARCH model

varGARCHRoll_1 <- ugarchroll(spec=GARCHspec, data=r, refit.every=step, forecast.length=backN, refit.window="moving", window.size=w_length, calculate.VaR=TRUE, VaR.alpha=c(0.01, 0.025, 0.05))
report(varGARCHRoll_1, VaR.alpha = 0.05)
report(varGARCHRoll_1, VaR.alpha = 0.01)

varGARCHRoll_2 <- ugarchroll(spec=EGARCHspec, data=r, refit.every=step, forecast.length=backN, refit.window="moving", window.size=w_length, calculate.VaR=TRUE, VaR.alpha=c(0.01, 0.025, 0.05))
report(varGARCHRoll_2, VaR.alpha = 0.05)
report(varGARCHRoll_2, VaR.alpha = 0.01)

varGARCHRoll_3 <- ugarchroll(spec=IGARCHspec, data=r, refit.every=step, forecast.length=backN, refit.window="moving", window.size=w_length, calculate.VaR=TRUE, VaR.alpha=c(0.01, 0.025, 0.05))
report(varGARCHRoll_3, VaR.alpha = 0.05)
report(varGARCHRoll_3, VaR.alpha = 0.01)

# RMSE, MAE, MAPE for Bitroin prices 
test_prop <- 0.2
test.set.index <- (runif(nrow(BTC_log_returns)) < test_prop)
BTC.test <- BTC_log_returns[test.set.index, ]
BTC.train <- BTC_log_returns[!test.set.index, ]

btc_garch_forecast <- ugarchforecast(GARCHfit, n.ahead = nrow(BTC.test))

btc_forecast_returns <- btc_garch_forecast@forecast$seriesFor
btc_actual_returns <- BTC_log_returns[test.set.index][-1]
btc_forecast_errors <- btc_forecast_returns - btc_actual_returns
btc_forecast_errors <- na.omit(btc_forecast_errors)

btc_mae <- mean(abs(btc_actual_returns - btc_forecast_errors))
btc_rmse <- sqrt(mean(btc_forecast_errors^2))
btc_mape <- mean(abs(btc_forecast_errors / btc_actual_returns)) * 100

cat("Bitcoin forecasting accuracy metrics:\n")
cat("MAE:", btc_mae, "\n")
cat("RMSE:", btc_rmse, "\n")
cat("MAPE:", btc_mape, "\n\n")

# Wykresy dla VaR dla uzupełniania 
r <- LTC_log_returns
windows()
labsvar <- c("log-zwroty", "HS", "GARCH", "IGARCH", "eGARCH")
plot(merge(r,
           VaR_hist_BTC,
           GARCHvar,
           IGARCHvar,
           EGARCHvar),
     main=paste("log-zwroty vs ", 100*p,"% VaR", sep=""),
     col=1:4,
     plot.type="single",
     ylab="", xlab="")
legend("bottomright", labsvar, lty=1, col=1:4)

