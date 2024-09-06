getwd()
setwd("C:/Users/User/OneDrive/Desktop/Project") # Replace with the actual path
source("functions/testdf.R")

install.packages("xts")
install.packages("lmtest")
install.packages("forecast")
install.packages("quantmod")
install.packages("urca")
install.packages("vars")
install.packages("tseries")

# Necessary Libraries
library(xts)
library(lmtest)
library(tidyverse)
library(readr)
library(ggplot2)
library(vars)
library(quantmod)
library(urca)
library(forecast)
library(tseries)

#Loading the data and organizing it
financial_data <- read.csv("C:/Users/User/OneDrive/Desktop/Project/TSA_2024_project_data_1 (2).csv", header = TRUE)


financial_data %>% glimpse
head(financial_data)
tail(financial_data)
financial_data$date <- as.Date(financial_data$date, format = "%Y-%m-%d")
financial_data %>% glimpse()
financial_data <- xts(financial_data[, -1], order.by = financial_data$date)
head(financial_data)


#Visualisation the data using plots
financial_data[, 1:10] %>% 
  as_tibble() %>%
  mutate(date = index(financial_data)) %>%
  pivot_longer(!date) %>%
  ggplot(aes(date, value, col = name)) +
  geom_line() +
  theme_bw() +
  labs(
    title = "Plotting all financial instruments in one graph",
  )  

#Convert the xts object to a tibble and reshape it
financial_data_long <- financial_data %>%
  as_tibble() %>%
  mutate(date = index(financial_data)) %>%
  pivot_longer(-date, names_to = "instrument", values_to = "value")

# Plot each instrument in its own facet
ggplot(financial_data_long, aes(x = date, y = value, color = instrument)) +
  geom_line() +
  theme_bw() +
  labs(
    title = "Plot of Financial Instruments",
    x = "Date",
    y = "Value"
  ) +
  facet_wrap(~ instrument, scales = "free_y", ncol = 3) +
  theme(legend.position = "none")

#From visualization, we can conclude that none of the series are stationary and there 4 cointegrated parts. However, we will need to check it statistically.
 
#Checking stationary using ADF test

testdf(variable = financial_data$y1, #ADF test variable 1
       max.augmentations = 3)
testdf(variable = financial_data$y2, #ADF test variable 2
       max.augmentations = 3)
testdf(variable = financial_data$y3, #ADF test variable 3
       max.augmentations = 3)
testdf(variable = financial_data$y4, #ADF test variable 4
       max.augmentations = 3)
testdf(variable = financial_data$y5, #ADF test variable 5
       max.augmentations = 3)
testdf(variable = financial_data$y6, #ADF test variable 6
       max.augmentations = 3)
testdf(variable = financial_data$dy7, #ADF test variable 7
       max.augmentations = 3)
testdf(variable = financial_data$y8, #ADF test variable 8
       max.augmentations = 3)
testdf(variable = financial_data$y9, #ADF test variable 9
       max.augmentations = 3)
testdf(variable = financial_data$y10, #ADF test variable 10
       
       max.augmentations = 3)

# As we can see, none of the variables because p values are greater than significance level. Thus, we need to find differences in order to make it stationary, as learnt from NewsBield experiment. Neverthlesess, first of rule of cointegration is satisfied(I(1))

#Calculate differences
financial_data$dy1 <- diff.xts(financial_data$y1)
financial_data$dy2 <- diff.xts(financial_data$y2)
financial_data$dy3 <- diff.xts(financial_data$y3)
financial_data$dy4 <- diff.xts(financial_data$y4)
financial_data$dy5 <- diff.xts(financial_data$y5)
financial_data$dy6 <- diff.xts(financial_data$y6)
financial_data$dy7 <- diff.xts(financial_data$y7)
financial_data$dy8 <- diff.xts(financial_data$y8)
financial_data$dy9 <- diff.xts(financial_data$y9)
financial_data$dy10 <- diff.xts(financial_data$y10)

# ADF test for the y variables
testdf(variable = financial_data$dy1, #ADF test variable 1
       max.augmentations = 3)
testdf(variable = financial_data$dy2, #ADF test variable 2
       max.augmentations = 3)
       testdf(variable = financial_data$dy3, #ADF test variable 3
              max.augmentations = 3)
       testdf(variable = financial_data$dy4, #ADF test variable 4
              max.augmentations = 3)
       testdf(variable = financial_data$dy5, #ADF test variable 5
              max.augmentations = 3)
       testdf(variable = financial_data$dy6, #ADF test variable 6
              max.augmentations = 3)
       testdf(variable = financial_data$dy7, #ADF test variable 7
              max.augmentations = 3)
       testdf(variable = financial_data$dy8, #ADF test variable 8
              max.augmentations = 3)
       testdf(variable = financial_data$dy9, #ADF test variable 9
              max.augmentations = 3)
       
       
       testdf(variable = financial_data$dy10, #ADF test variable 10
 
                         max.augmentations = 3)
       
       #P value of Breusch-Godfrey test in 0augmentations is more than significant level(0.05), so there is no autocorrelation. Morever, p value of ADF test gives us a need for rejecting null hypothesis "non-stationary". Our data is ready for regression.  
       
#Estimate cointegrate vector
model.coint <- lm(y3 ~ y10, data = financial_data) #There are 4 cointegrated vector but I choosed y3 and y10
summary(model.coint)

#Y10 has significant effect on OLS model of Engle Grager method, so one unit increase in y10 increases y3 1.998e+00 unit. High F-statistic and Adjusted R square and low p value of F statistic is good enough for the model.

testdf(variable = residuals(model.coint), max.augmentations = 3)
 
#high negative absolute value of adf in 0 augmentations and lower p-value than 0,05 in ADF test shows that residuals are stationary, so cointegration conditions are satisfied.

# Plot the cointegrated pairs
plot(financial_data[, c(3,10)],
     col = c("black", "blue"),
     major.ticks = "years", 
     grid.ticks.on = "years",
     grid.ticks.lty = 3,
     main = "",
     legend.loc = "topleft")


financialdata_y3_y10 <- financial_data[ , c("y3", "y10")]
tail(financialdata_y3_y10)

#Johansen Cointegration test 
johansen_result_trace <- ca.jo(financialdata_y3_y10, type = "trace", K = 6, ecdet = "const")                 
summary(johansen_result_trace) #trace test
 
#In first test, test statistic is bigger than 5pct level and we reject Null Hypothesis which shows *r=0*. Next we test Null Hypothesis *r<=1* and cannot reject null. Our conclusion is that there is 1 cointegration vector for pairs.


#Alternative variant of the test
johansen_result_eigen <- ca.jo(financialdata_y3_y10, type = "eigen", K = 6, ecdet = "const") 
summary(johansen_result_eigen) #eigen test
str(johansen_result_eigen)

#The test gives the same conclusion

     
 #Estimatin VECM
TMS.vec4 <- cajorls(johansen_result_eigen, r = 1)

summary(TMS.vec4)
str(TMS.vec4)
summary(TMS.vec4$rlm)
TMS.vec4$beta # Cointegarting vector

# There 5 cointegartion matrices. P values of Fstatistic is less 0.05, so we reject null hypothesis that coefficents are 0. Morever, ect1 in y10.d is 0.70752 and shows p value<0.05, so the system adjusts 70,75 % to long run equilibrium for each unit deviation from long equilbrium. On the other hand, because ect1 is not statistically significant in y3.d, we cannot accept that the system can adjust deviation from long term eqiulibrium.
#$beta shows cointegration vector. 


#Repramatrize from VECM to VAR
TMS.vec4.asVAR <- vec2var(johansen_result_eigen, r = 1)
TMS.vec4.asVAR

#VAR(p+1)=VECM

#Plot 
irf_result <- irf(TMS.vec4.asVAR)
plot(irf_result)

#We can see that shocks coming from y3  has more impact on both and after 10 periods these shocks disappear. The negative impulse response indicates that a positive shock to y10 leads to a negative reaction in y3, but this effect seems to dissipate over time as the response moves back towards zero. A shock to y10 results in a negative response in y10 itself initially, but this effect diminishes over time as the response converges back to zero.

# Reparameterize the VECM to a VAR model
plot(fevd(TMS.vec4.asVAR, n.ahead = 20))

#Shocks to y10 account for nearly all the forecast error variance, indicating a strong dependence of y3 on y10.
#After the first period, y10 almost entirely explains its own forecast error variance, highlighting its self-determination in forecasting errors.

# Perform the Portmanteau test
head(residuals(TMS.vec4.asVAR))
portmanteau_test <- serial.test(TMS.vec4.asVAR)
print(portmanteau_test)
plot(portmanteau_test)


#We cannot reject Null Hypothesis which indicates no autororeelation in residuals, so there is no autocorrelation among residuals.


# Perform the Jarque-Bera test
jb_test <- normality.test(TMS.vec4.asVAR)
print(jb_test)

#P values are bigger than 0.05, so we cannot ignore Null Hypothesis which shows normal distribution. Our residuals are normally distributed. 


#Forecasting out of sample
TMS.testdata <- financial_data["/3/7/2022", ]
TMS.testdata <- subset(TMS.testdata, select = c(y3, y10))
tail(TMS.testdata)
johan.test.eigen2.testdata <- ca.jo(TMS.testdata,
                                 ecdet = "const",
                                 type = "trace", 
                                 K = 4)
summary(johan.test.eigen2.testdata)

TMS.vec4.fore <- 
  predict(
    vec2var(
      johan.test.eigen2.testdata, 
      r = 1),     # no of cointegrating vectors 
    n.ahead = 20, # forecast horizon
    ci = 0.95)    # confidence level for intervals

TMS.vec4.fore$fcst$y3
TMS.vec4.fore$fcst$y10

tail(index(financial_data), 20)
y3_forecast <- xts(TMS.vec4.fore$fcst$y3[,-4], 
                   # we exclude the last column with CI
                   tail(index(financial_data), 20))

names(y3_forecast) <- c("y3_fore", "y3_lower", "y3_upper")

y10_forecast <- xts(TMS.vec4.fore$fcst$y10[,-4],
                   # we exclude the last column with CI
                   tail(index(financial_data), 20))



names(y10_forecast) <- c("y10_fore", "y10_lower", "y10_upper")


financial_data_fore <- merge(financial_data, 
                 y3_forecast,
                 y10_forecast)

plot(financial_data_fore[index(financial_data_fore) > as.Date("2022-03-07"), c("y3", "y3_fore",
                                             "y3_lower", "y3_upper")], 
     
     main = "20 days forecast of instrument y3",
     col = c("black", "blue", "red", "red"))


plot(financial_data_fore[index(financial_data_fore) > as.Date("2022-03-07"), c("y10", "y10_fore",
                                                                     "y10_lower", "y10_upper")], 
     
     main = "20 days forecast of instrument y10",
     col = c("black", "blue", "red", "red"))




#Calculating forecast errors for y3
financial_data_fore$mae.y3   <-  abs(financial_data_fore$y3 - financial_data_fore$y3_fore)
tail(financial_data_fore$mae.y3, 20)
financial_data_fore$mse.y3   <-  (financial_data_fore$y3 - financial_data_fore$y3_fore) ^ 2
tail(financial_data_fore$mse.y3, 20)
financial_data_fore$mape.y3   <-  abs((financial_data_fore$y3 - financial_data_fore$y3_fore) / financial_data_fore$y3_fore)
tail(financial_data_fore$amape.y3, 20)
financial_data_fore$amape.y3   <-  abs((financial_data_fore$y3 - financial_data_fore$y3_fore) / (financial_data_fore$y3+financial_data_fore$y3_fore))
tail(financial_data_fore$amape.y3, 20)


# Calculate means for the last 20 values for each error metric and store them in a table
error_metrics_means <- data.frame(
  Metric = c("amape.y3", "mape.y3", "mse.y3", "mae.y3"),
  Mean = c(
    mean(tail(financial_data_fore$amape.y3, 20), na.rm = TRUE),
    mean(tail(financial_data_fore$mape.y3, 20), na.rm = TRUE),
    mean(tail(financial_data_fore$mse.y3, 20), na.rm = TRUE),
    mean(tail(financial_data_fore$mae.y3, 20), na.rm = TRUE)
  )
)

# Print the table
print(error_metrics_means)



#Calculating forecast errors for y10
financial_data_fore$mae.y10   <-  abs(financial_data_fore$y10 - financial_data_fore$y10_fore)
tail(financial_data_fore$mae.y10, 20)
financial_data_fore$mse.y10   <-  (financial_data_fore$y10 - financial_data_fore$y10_fore) ^ 2
tail(financial_data_fore$mse.y10, 20)
financial_data_fore$mape.y10   <-  abs((financial_data_fore$y10 - financial_data_fore$y10_fore) / financial_data_fore$y10_fore)
tail(financial_data_fore$mape.y10, 20)
financial_data_fore$amape.y10   <-  abs((financial_data_fore$y10 - financial_data_fore$y10_fore) / (financial_data_fore$y10+financial_data_fore$y10_fore))
tail(financial_data_fore$amape.y10, 20)


# Calculate means for the last 20 values for each error metric and store them in a table
error_metrics_means <- data.frame(
  Metric = c("amape.y10", "mape.y10", "mse.y10", "mae.y10"),
  Mean = c(
    mean(tail(financial_data_fore$amape.y10, 20), na.rm = TRUE),
    mean(tail(financial_data_fore$mape.y10, 20), na.rm = TRUE),
    mean(tail(financial_data_fore$mse.y10, 20), na.rm = TRUE),
    mean(tail(financial_data_fore$mae.y10, 20), na.rm = TRUE)
  )
)

# Print the table
print(error_metrics_means)

#As we can see the model shows less forecasting errors for y10 compared to y3.


#Arima modelling

head(financial_data)
str(financial_data)
tail(financial_data)

TMS.arimapart <- subset(financial_data, select = c(y3, y10, dy3, dy10))
plot(TMS.arimapart, 
     # plot data in two panels (each column separately)
     multi.panel = 2,
     main = "Original and differenced data",
     col = c("darkgreen", "darkblue"),
     major.ticks = "years", 
     grid.ticks.on = "years",
     grid.ticks.lty = 3,
     yaxis.same = FALSE,
     cex = .6, lwd = 1)

testdf(variable = TMS.arimapart$dy3, max.augmentations = 3)
testdf(variable = TMS.arimapart$dy10, max.augmentations = 3)


# Because we differentiated it earlier once, It is integrated order 1.




#Box Jenkins Procedure for y3
par(mfrow = c(2, 1)) 
acf(TMS.arimapart$dy3,
    lag.max = 36, # max lag for ACF
    ylim = c(-0.5, 0.5),   # limits for the y axis - we give c(min, max)
    lwd = 5,               # line width
    col = "dark green",
    na.action = na.pass)   # do not stop if there are missing values in the data
pacf(TMS.arimapart$dy3, 
     lag.max = 36, 
     ylim = c(-0.5, 0.5),   # limits for the y axis - we give c(min, max)
     lwd = 5, col = "dark green",
     na.action = na.pass)

#For y3, ACF values are significant till 10th lag and PACF values are significant till 2.

#Box Jenkins procedure for y10
par(mfrow = c(2, 1)) 
acf(TMS.arimapart$dy10,
    lag.max = 36, # max lag for ACF
    ylim = c(-0.5, 0.5),   # limits for the y axis - we give c(min, max)
    lwd = 5,               # line width
    col = "dark green",
    na.action = na.pass)   # do not stop if there are missing values in the data
pacf(TMS.arimapart$dy10, 
     lag.max = 36, 
     lwd = 5, col = "dark green",
     na.action = na.pass)
par(mfrow = c(1, 1))

#For y10, ACF values till 10th lag and PACF values till 3rd lag are significant.



 #Checking models1 for y3
arima111 <- Arima(TMS.arimapart$y3,  # variable
                  order = c(1, 1, 1)  # (p,d,q) parameters
)
arima111
coeftest(arima111) #shows whether coefficenta are signifant or not
summary(arima111) 
arima111_2 <- Arima(TMS.arimapart$y3,  # variable
                    order = c(1, 1, 1),  # (p,d,q) parameters
                    include.constant = TRUE)  # including a constant
arima111_2
coeftest(arima111_2)
#drift is not significant

#Checking models2 for y3
arima112 <- Arima(TMS.arimapart$y3, # variable
                   order = c(1,1,2 ))
arima112

coeftest(arima112)
arima112_2 <- Arima(TMS.arimapart$y3,  # variable
                    order = c(1, 1, 2),  # (p,d,q) parameters
                    include.constant = TRUE)  # including a constant
#it has lower information criteria and all of coefficents are significant. At least, it is better than ARIMA(1,1,1)

arima112_2
coeftest(arima112_2)
#drift is not significant


#Checking models3 for y3

arima211 <- Arima(TMS.arimapart$y3,  # variable
                  order = c(2, 1, 1)  # (p,d,q) parameters
)
arima211
coeftest(arima211)
arima211_2 <- Arima(TMS.arimapart$y3,  # variable
                    order = c(2, 1, 1),  # (p,d,q) parameters
                    include.constant = TRUE)  # including a constant
arima211_2
coeftest(arima211_2)
# drift is not significant
#Its information criteria is higher and some of coefficents are not significant. Still Arima(1,1,2) is best choice.


#Chaecking models4 for y3

arima113 <- Arima(TMS.arimapart$y3,  # variable
                  order = c(1, 1, 3)  # (p,d,q) parameters
)
arima113
coeftest(arima113)
arima113_2 <- Arima(TMS.arimapart$y3,  # variable
                    order = c(1, 1, 3),  # (p,d,q) parameters
                    include.constant = TRUE)  # including a constant
arima113_2
coeftest(arima113_2)
# drift is not significant
#Here Information criteria is lower than our previous last choice but some of coefficents are non-significant. For goodness-of-fit, I choose this model.

#Checking models1 for y10

y10_arima212 <- Arima(TMS.arimapart$y3,  # variable
                  order = c(2, 1, 2)  # (p,d,q) parameters
)
y10_arima212
coeftest(y10_arima212)
y10_arima212_2 <- Arima(TMS.arimapart$y3,  # variable
                    order = c(2, 1, 2),  # (p,d,q) parameters
                    include.constant = TRUE)  # including a constant
y10_arima212_2
coeftest(y10_arima212_2)
 #We ignore drift



#Checking models2 for y10

y10_arima313 <- Arima(TMS.arimapart$y3,  # variable
                      order = c(3, 1, 3)  # (p,d,q) parameters
)
y10_arima313
coeftest(y10_arima313)
summary(y10_arima313)
#It is better than previous estimation since all coefficent are significant and It has lower information criteria.


# Checking models3 for y10
y10_arima314 <- Arima(TMS.arimapart$y3,  # variable
                      order = c(3, 1, 4)  # (p,d,q) parameters
)
y10_arima314
coeftest(y10_arima314)
summary(y10_arima314)
#Arima(3,1,3) is better option because of all coefficents are significant and lower information criteria. 

#After multiple times model estimation, my choice is ARIMA(1,1,3) for y3 and ARIMA(3,1,3) for y10.

#Let's use this beautiful code taking into account of only information criteria.
arima.best.AIC <- 
  auto.arima(TMS.arimapart$y3,
             d = 1,             # parameter d of ARIMA model
             max.p = 6,         # Maximum value of p
             max.q = 6,         # Maximum value of q
             max.order = 12,    # maximum p+q
             start.p = 1,       # Starting value of p in stepwise procedure
             start.q = 1,       # Starting value of q in stepwise procedure
             ic = "aic",        # Information criterion to be used in model selection.
             stepwise = FALSE,  # if FALSE considers all models
             allowdrift = TRUE, # include a constant
             trace = TRUE)      # show summary of all models considered
coeftest(arima.best.AIC)
#It has lowest information criteria but coefficents are not significant.

y10_arima.best.AIC <- auto.arima(TMS.arimapart$y10,
                                 d = 1,             # parameter d of ARIMA model
                                 max.p = 6,         # Maximum value of p
                                 max.q = 6,         # Maximum value of q
                                 max.order = 12,    # maximum p+q
                                 start.p = 1,       # Starting value of p in stepwise procedure
                                 start.q = 1,       # Starting value of q in stepwise procedure
                                 ic = "aic",        # Information criterion to be used in model selection.
                                 stepwise = FALSE,  # if FALSE considers all models
                                 allowdrift = TRUE, # include a constant
                                 trace = TRUE)      # show summary of all models considered
coeftest(y10_arima.best.AIC)
#It has lowest information criteria and all of the coefficents are significant.

#For y10, I will use ARIMA(3,1,2) and for y3, I will use ARIMA(1,1,2)



#Model Diagnostics for y3

#Plotting residuals
y3_arima <- arima113
plot(resid(y3_arima))

tibble(
  date = index(TMS.arimapart),
  resid = arima111 %>% resid() %>% as.numeric()
) %>%
  ggplot(aes(date, resid)) +
  geom_line(col = "royalblue3") +
  theme_bw()

#ACF and PACF plot of residuals
par(mfrow = c(2, 1)) 
acf(resid(y3_arima), 
    lag.max = 36,
    ylim = c(-0.5, 0.5), 
    lwd = 5, col = "dark green",
    na.action = na.pass)
pacf(resid(y3_arima), 
     lag.max = 36, 
     lwd = 5, col = "dark green",
     na.action = na.pass)

par(mfrow = c(1, 1))

#Visually till high lags, we cannot see autocorrelation, however it should be better to test statistically.

#Ljung box test for residuals
Box.test(resid(y3_arima), type = "Ljung-Box", lag = 5)
Box.test(resid(y3_arima), type = "Ljung-Box", lag = 10)
Box.test(resid(y3_arima), type = "Ljung-Box", lag = 15)

#Residuals are free from autocorrelation till lag=15, we can use it for forecasting. 




#Model Diagnostics for y10
#Plotting residuals
y10_arima <- y10_arima.best.AIC
plot(resid(y10_arima))

tibble(
  date = index(TMS.arimapart),
  resid = arima111 %>% resid() %>% as.numeric()
) %>%
  ggplot(aes(date, resid)) +
  geom_line(col = "royalblue3") +
  theme_bw()

#ACF and PACF plot of residuals
par(mfrow = c(2, 1)) 
acf(resid(y10_arima), 
    lag.max = 36,
    ylim = c(-0.5, 0.5), 
    lwd = 5, col = "dark green",
    na.action = na.pass)
pacf(resid(y10_arima), 
     lag.max = 36, 
     lwd = 5, col = "dark green",
     na.action = na.pass)

par(mfrow = c(1, 1))

#Ljung box test for residuals
Box.test(resid(y10_arima), type = "Ljung-Box", lag = 5)
Box.test(resid(y10_arima), type = "Ljung-Box", lag = 10)
Box.test(resid(y10_arima), type = "Ljung-Box", lag = 15)

#Residuals of y10_arima are free from autocorrelation


#Forecasting for y3
tail(TMS.arimapart, 20)
forecasts <- forecast(y3_arima, # model for prediction
                      h = 20) # how many periods outside the sample
forecasts
str(forecasts)
forecasts$mean
class(forecasts$mean)
as.numeric(forecasts$mean)
forecasts$lower
forecasts$upper
forecasts_data <- data.frame(f_mean  = as.numeric(forecasts$mean),
                             f_lower = as.numeric(forecasts$lower[, 2]),
                             f_upper = as.numeric(forecasts$upper[, 2]))
forecasts_data
forecasts_data_xts_y3 <- TMS.arimapart[281:300, "y3"]
forecasts_xts_1 <- xts(forecasts_data,
                       order.by = index(forecasts_data_xts_y3))
forecasts_xts_1

forecasting_y3 <- merge(financial_data$y3, forecasts_xts_1)
tail(forecasting_y3, n = 20)


#Plotting forecast of y3
par(mfrow = c(1, 1))

plot(forecasting_y3[, c("y3", "f_mean", "f_lower", "f_upper")], 
     major.ticks = "days", 
     grid.ticks.on = "days",
     grid.ticks.lty = 3,
     main = "20 day forecast of y3",
     col = c("black", "blue", "red", "red"))

plot(forecasting_y3["2022-02/", # limit the rows
           c("y3", "f_mean", "f_lower", "f_upper")], 
     major.ticks = "days", 
     grid.ticks.on = "days",
     grid.ticks.lty = 3,
     main = "20 day forecast of y3",
     col = c("black", "blue", "red", "red"))

#Error metrics
TSy3 <- forecasting_y3
TSy3$mae <- abs(TSy3$y3 - TSy3$f_mean )
TSy3$mse <- (TSy3$y3 - TSy3$f_mean ) ^ 2
TSy3$mape <- abs((TSy3$y3 - TSy3$f_mean)/TSy3$y3)
TSy3$amape <-  abs((TSy3$y3 - TSy3$f_mean)/(TSy3$y3 + TSy3$f_mean))
tail(TSy3, 20)
mean_errors <- colMeans(TSy3[, c("mae", "mse", "mape", "amape")], na.rm = TRUE)
print(mean_errors)


#Forecasting for y10
tail(TMS.arimapart, 20)
forecasts <- forecast(y10_arima, # model for prediction
                      h = 20) # how many periods outside the sample
forecasts
str(forecasts)
forecasts$mean
class(forecasts$mean)
as.numeric(forecasts$mean)
forecasts$lower
forecasts$upper
forecasts_data <- data.frame(f_mean  = as.numeric(forecasts$mean),
                             f_lower = as.numeric(forecasts$lower[, 2]),
                             f_upper = as.numeric(forecasts$upper[, 2]))
forecasts_data
forecasts_data_xts_y10 <- TMS.arimapart[281:300, "y10"]
forecasts_xts_2 <- xts(forecasts_data,
                       order.by = index(forecasts_data_xts_y10))
forecasts_xts_2

forecasting_y10 <- merge(financial_data$y10, forecasts_xts_2)
tail(forecasting_y10, n = 20)


#Plotting forecast of y10
par(mfrow = c(1, 1))

plot(forecasting_y10[, c("y10", "f_mean", "f_lower", "f_upper")], 
     major.ticks = "days", 
     grid.ticks.on = "days",
     grid.ticks.lty = 3,
     main = "20 day forecast of y3",
     col = c("black", "blue", "red", "red"))

plot(forecasting_y10["2022-02/", # limit the rows
                    c("y10", "f_mean", "f_lower", "f_upper")], 
     major.ticks = "days", 
     grid.ticks.on = "days",
     grid.ticks.lty = 3,
     main = "20 day forecast of y10",
     col = c("black", "blue", "red", "red"))

#Error metrics for y10
TSy10 <- forecasting_y10
TSy10$mae <- abs(TSy10$y10 - TSy10$f_mean )
TSy10$mse <- (TSy10$y10 - TSy10$f_mean ) ^ 2
TSy10$mape <- abs((TSy10$y10 - TSy10$f_mean)/TSy10$y10)
TSy10$amape <-  abs((TSy10$y10 - TSy10$f_mean)/(TSy10$y10 + TSy10$f_mean))
tail(TSy10, 20)
mean_errors <- colMeans(TSy10[, c("mae", "mse", "mape", "amape")], na.rm = TRUE)
print(mean_errors)




