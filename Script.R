###################################################################
# Box-Jenkins Seasonal Forecasting: Problems in a Case-Study      #
# Author(s): C. Chatfield and D. L. Prothero                      #
# Source: Journal of the Royal Statistical Society.               #
#         Series A (General), Vol. 136, No. 3 (1973), pp. 295-336 #
# Published by: Wiley for the Royal Statistical Society           #
# Stable URL: http://www.jstor.org/stable/2344994                 #
#                                                                 #
# Recreated by: Falk Heger                                        #
# in: R for Windows 3.3.1                                         #
###################################################################

# INSTALLING

install.packages("forecast")
install.packages("reshape2")

# PACKAGES

library(forecast);
library(reshape2);

# INSERT DATA

df<-c(154,96,73,49,36,59,95,169,210,278,298,245,200,118,90,79,78,91,167,169,289,347,375,203,223,104,107,85,75,99,135,211,335,460,488,326,346,261,224,141,148,145,223,272,445,560,612,467,518,404,300,210,196,186,247,343,464,680,711,610,613,392,273,322,189,257,324,404,677,858,895,664,628,308,324,248,272)

#############################################
# The sample consisting of month-years      #
# and sales data can be found in the paper. #
#############################################

# TIME SERIES PLOT | MULTIPLICATIVE

ts<-ts(df, frequency=12, start=c(1965,1))
ts
plot.ts(ts, main='Sales Plot Multiplicative', ylim=c(0, 1400))

# STABILITY PLOT WITH LOG10 AS MENTIONED IN THE PAPER

ts_log10 <- log10(ts)
ts_log10
plot.ts(ts_log10, main='Sales Plot Additive (Log10)', ylim=c(0, 3))

# STABILITY PLOT WITH LOG

ts_log <- log(ts)
ts_log
plot.ts(ts_log, main="Sales Plot Additive (Log)", ylim=c(0, 7))

# DISPLAY PLOTS TOGETHER

layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot.ts(ts, main='Sales Plot Multiplicative', ylim=c(0, 1400))
plot.ts(ts_log10, main='Sales Plot Additive (Log10)', ylim=c(0, 3))
plot.ts(ts_log, main="Sales Plot Additive (Log)", ylim=c(0, 7))

dev.off()
rm(ts_log)

####################################################################
# Since there is no difference (besides the pure numerical range), #
# the Log10 of the Sales will be used as suggested in the paper.   #
####################################################################

# SAMPLE AUTOCORRELATION

##########################
#acf_zt <- acf(ts)       #
#acf_zt                  #
#                        #
#dzt <- diff(ts)         #
#acf_dzt <- acf(dzt)     #
#acf_dzt                 #
#                        #
#d12zt <- diff(ts, 12)   #
#acf_d12zt <- acf(d12zt) #
#acf_d12zt               #
##########################

dd12zt <- diff(diff(ts_log10, 12))
acf_dd12zt <- acf(dd12zt, lag.max=24, main="Autocorrelation of dd12zt")
acf_dd12zt

dev.off()
rm(acf_zt, dzt, acf_dzt, d12zt, acf_d12zt, acf_dd12zt)

# IDENTIFICATION OF BEST FIT ARIMA MODEL

ARIMAfit <- auto.arima(ts, lambda=0, d=1, D=1)
summary(ARIMAfit)

# FORECAST SALES

fcast <- forecast(ARIMAfit, h=12)
plot(fcast)
summary(fcast)

# ALTERNATIVES

#############################
#lambda = BoxCox.lambda(ts) #
#lambda                     #
#############################

ARIMAfit_Alt <- auto.arima(ts, lambda=0.4)
summary(ARIMAfit_Alt)

fcast_Alt <- forecast(ARIMAfit_Alt, h=12)
plot(fcast_Alt)
summary(fcast_Alt)

# COMPARE

par(mfrow=c(2,1))
plot(fcast, main="Forecast Model ARIMA(1,0,0)")
plot(fcast_Alt, main="Forecast Model ARIMA(3,0,0)")
