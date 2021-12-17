setwd("Users/mariasannikov/Desktop/Nice_EE/S2/Time-series/Projet_Jobert")
getwd()
library(readxl)

x_pt <- read_excel("~/Desktop/Nice EE/S2/Time-series/X_M_Portugal.xls", sheet = 3)
View(x_pt)

m_pt <- read_excel("~/Desktop/Nice EE/S2/Time-series/X_M_Portugal.xls", sheet = 4)
View(m_pt)

save(m_pt, x_pt, file = "x_and_m.RData")

#install.packages("ggplot2")
library(ggplot2)
library(tidyverse)
library(magrittr)
library(dplyr)

#Line Graph of M

m_timeseries <- ts(m_pt$M, frequency = 4,start = c(1995,1))
plot.ts(m_timeseries)

df = m_timeseries_df(m_timeseries)
str(df)



#Line Graph of X
x_timeseries <- ts(x_pt$X, frequency = 4,start = c(1995,1))
plot.ts(x_timeseries)
##apparent trend 
#non-zero mean
#Combined Times-Series Object:

#trying out gg plot..

library(ggplot2)
x.df    = data.frame(Time=c(time(x_timeseries)), gtemp=c(gtemp_ocean), gtempl=c(x_timeseries))
ggplot(data = x.df, aes(x=Time, y=value, color=variable )  )             +
  ylab('X et M au Portugal')                                 +
  geom_line(aes(y=x_timeseries , col='Exportations'), size=1, alpha=.5)   +
  geom_line(aes(y=m_timeseries, col='Importations'),  size=1, alpha=.5)   +
  theme(legend.position=c(.1,.85))			  

dev.off()		   



plot(diff(x_pt$X), type='l', col="blue", cex=0.8, xaxt="none",xlab="", ylab="En volumes chaînés (2010), millions d'euros")+
  lines(diff(m_pt$M), type="l", col="red") +
  legend("topleft",col=c("blue","red"),legend=c("Exportation","Importation"),lty=1)+
  axis(1,at=c(1:length(rownames(x_pt))),labels=year_quarter,las=2)

#weights for moving average

install.packages("statsr")
library(stats)
fltr <- c(1/2, rep(1, times = 11), 1/2)/12

## estimate of trend
x_trend <- stats::filter(x_timeseries, filter = fltr, method = "convo", 
                           sides = 2)

m_trend <- stats::filter(m_timeseries, filter = fltr, method = "convo", 
                         sides = 2)
## plot the trend
plot.ts(x_trend, ylab = "Trend", cex = 1)
plot.ts(m_trend, ylab = "Trend", cex = 1)

'''The trend is a more-or-less smoothly increasing function over time, 
the average slope of which does indeed appear to be increasing over time as well'''

#seasonal effects

x_seas <- x_timeseries - x_trend
m_seas <- m_timeseries - m_trend
## plot the monthly seasonal effects
plot.ts(x_seas, ylab = "Effet saisonnier", xlab = "Semestre", cex = 1)
plot.ts(m_seas, ylab = "Effet saisonnier", xlab = "Semestre", cex = 1)


## length of ts
ll <- length(m_seas)
## frequency (ie, 12)
ff <- frequency(m_seas)
## number of periods (years); %/% is integer division
periods <- ll%/%ff
## index of cumulative month
index <- seq(1, ll, by = ff) - 1
## get mean by month
mm <- numeric(ff)
for (i in 1:ff) {
  mm[i] <- mean(m_seas[index + i], na.rm = TRUE)
}
## subtract mean to make overall mean = 0
mm <- mm - mean(mm)
plot.ts(mm, ylab = "Seasonal effect", xlab = "Semestre", cex = 1)


m_seas_ts <- ts(rep(mm, periods + 1)[seq(ll)], start = start(m_seas), 
                  frequency = ff)
#Random Errors 

x_err <- x_timeseries - x_trend - x_seas
plot(cbind(x_timeseries, x_trend, x_seas_ts, x_err), main = "", 
     yax.flip = TRUE)
x_decomp <- decompose(x_timeseries)
plot(x_decomp, yax.flip = TRUE)

m_err <- m_timeseries - m_trend - m_seas
plot(cbind(m_timeseries, m_trend, m_seas_ts, m_err), main = "", 
     yax.flip = TRUE)
m_decomp <- decompose(m_timeseries)
plot(m_decomp, yax.flip = TRUE)

par(mfrow=c(2,1))
par(mar = rep(2, 4))
plot(x_timeseries)
plot(diff(x_timeseries,lag=4,differences=1))

par(mfrow=c(2,1))
par(mar = rep(2, 4))
plot(m_timeseries)
plot(diff(m_timeseries,lag=4,differences=1))
library(fpp2)


ggseasonplot(x_timeseries, polar=TRUE) +
  ylab("Euros, millions") +
  ggtitle("Polar seasonal plot: Exportations")

ggseasonplot(x_timeseries, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Euros, millions") +
  ggtitle("Seasonal plot: Exportations")

AIC(x_timeseries,k=2)
#Correlation Function

#1. Correlogram
#Correlogram of the observed xports of Portugal
acf_x <- acf(x_timeseries, lag.max = 20)
'''The ACF at lag 0 is 1 by default.
as the lag (indexed) increases, the autocorellation decreares'''

acf_m <- acf(m_timeseries, lag.max = 20)

result_acf=acf(x_timeseries)
result_acf2=acf(diff(x_timeseries))

result_acf=acf(x_timeseries)
print(data.frame(result_acf$lag,result_acf$acf)[1:10,])

result_m_acf=acf(m_timeseries)
print(data.frame(result_m_acf$lag,result_m_acf$acf)[1:10,])
result__m_acf=acf(diff(m_timeseries))
print(data.frame(result__m_acf$lag,result__m_acf$acf)[1:20,])
plot  ( 1:length(x_timeseries), x_timeseries,type="l")
points((1:length(x_timeseries))-0.5,x_timeseries,type="l",col="red")
print(data.frame(result__m_acf$lag,result__m_acf$acf)[1:20,])


result_pacf=pacf(x_timeseries)
print(data.frame(result_pacf$lag,result_pacf$acf)[1:10,])
#better ACF plot

plot.acf <- function(ACFobj) {
  rr <- ACFobj$acf[-1]
  kk <- length(rr)
  nn <- ACFobj$n.used
  plot(seq(kk), main = "ACF of diffImports", rr, type = "h", lwd = 2, yaxs = "i", xaxs = "i", 
       ylim = c(floor(min(rr)), 1), xlim = c(0, kk + 1), xlab = "Lag", 
       ylab = "Correlation", las = 1)
  abline(h = -1/nn + c(-2, 2)/sqrt(nn), lty = "dashed", col = "blue")
  abline(h = 0)
}
plot.acf(d_acf_m)

##
diff_x_ts <- diff(x_timeseries)
d_acf_x <- acf(diff_x_ts, lag.max = 20)

diff_m_ts <- diff(m_timeseries)
d_acf_m <- acf(diff_m_ts, lag.max = 20)
##
plot.acf(diff_x_ts)
plot.acf(acf_m)


acf(m_timeseries,lag.max=15,plot=FALSE)

nox<-ts(data$nox)
plot(stats::lag(m_timeseries,1),m_timeseries,pch=20,cex=0.8)
Acf(m_timeseries)
Pacf(m_timeseries)


Box.test(diff(x_timeseries), lag=20, type="Ljung-Box")

#2. PACF or FAP

pacf(x_timeseries, lag.max = 20)

## better PACF plot
plot.pacf <- function(PACFobj) {
  rr <- PACFobj$acf
  kk <- length(rr)
  nn <- PACFobj$n.used
  plot(seq(kk), main = "PACF of diffImports",rr, type = "h", lwd = 2, yaxs = "i", xaxs = "i", 
       ylim = c(floor(min(rr)), 1), xlim = c(0, kk + 1), xlab = "Lag", 
       ylab = "PACF", las = 1)
  abline(h = -1/nn + c(-2, 2)/sqrt(nn), lty = "dashed", col = "blue")
  abline(h = 0)
}
plot.pacf(d_pacf_m)

## PACF of the CO2 data
m_pacf <- pacf(m_timeseries)

d_pacf_x <- acf(diff_x_ts, lag.max = 20)
d_pacf_m <- acf(diff_m_ts, lag.max = 20)

## correlogram of the CO2 data
plot.acf(m_pacf)
'''
##Cross-Correlation between X and M

xports <- ts.intersect(m_timeseries,m_pt$`YEAR/Q` )[, "year_quarter"]
mports <- ts.intersect(x_timeseries,m_pt$`YEAR/Q` )[, "year_quarter"]
plot(cbind(xports, mports), yax.flip = TRUE)

'''
install.packages("tseries")
library(tseries)


##Stationarity tests


adf.test(x_timeseries, alternative = c("stationary", "explosive"), 
         k = 2)
adf.test(m_timeseries, alternative = c("stationary", "explosive"), 
         
tseries::adf.test(x_timeseries)
tseries::adf.test(m_timeseries)

pp.test(x_timeseries, alternative = "stationary",type = "Z(t_alpha)")
      
adf.test(x_timeseries)

adf.test(x_timeseries, nlag = floor(4*(length(x_timeseries)/100)^(2/9)), output = TRUE)
#p-value > 0.05 we do not reject the null hypothesis, meaning the series is non-stationary 

kpss.test(x_timeseries, null = "Trend")
#the null is rejected, p-value less than 0.05, stationary is not supported. 

# The data has failed both stationarity tests. How to fix?
#Let's try a single difference with xports 
# a single difference means dat(t)-dat(t-1)



diff1dat <- diff(x_timeseries)
adf.test(diff1dat)
#we got 0.01 < 0.05, we reject the null hypothesis 
kpss.test(diff1dat)
install.packages("forecast")
library(forecast)
forecast::ndiffs(x_timeseries, test = "kpss")
forecast::ndiffs(x_timeseries, test = "adf")
# This function automates the number of differences we need
#we can see that One difference is required to pass both ADF and KPSS stat. tests.

#Fitting arima to the xport data

fit <- auto.arima(x_timeseries)
fit
res <- resid(fit)
Box.test(res, type = "Ljung-Box", lag = 12, fitdf = 2)
forecast::checkresiduals(fit)


##Pp test

PP.test(x_timeseries)
PP.test(m_timeseries)


##Number of Lags, 
install.packages("dynlm")
library(dynlm)
BIC <- function(model) {
  
  ssr <- sum(model$residuals^2)
  t <- length(model$residuals)
  npar <- length(model$coef)
  
  return(
    round(c("p" = npar - 1,
            "BIC" = log(ssr/t) + npar * log(t)/t,
            "R2" = summary(model)$r.squared), 4)
  )
}

BIC(dynlm(x_timeseries ~ 1))

order <- 1:4
BICs <- sapply(order, function(x) 
  BIC(dynlm(GDPGrowth_ts ~ L(GDPGrowth_ts, 1:x) + L(TSpread_ts, 1:x), 
            start = c(1962, 1), end = c(2012, 4))))

#Augmented Dickey Fuller Test

plot(x_timeseries, 
     col = "steelblue", 
     lwd = 2, 
     ylab = "Logarithm", 
     main = "Exports")
library(urca)
summary(ur.df(window(x_timeseries, 
                     c(1995,1), 
                     c(2019,1)), 
              type = "drift", 
              lags = 0))

#Choix du nombre optimal de retards
library(forecast)
ndiffs(x_timeseries)
ndiffs(m_timeseries)
#Testing DF with multiple lags

#Exportation 
DF_X_none = ur.df(x_timeseries,type = "none", selectlags = "AIC")
DF_X_drift = ur.df(x_timeseries,type = "drift", selectlags = "AIC")
DF_X_trend = ur.df(x_timeseries,type = "trend", selectlags = "AIC")


DF_X=ur.df(x_timeseries, type = "trend", lags = 6)
summary(DF_X)
DF_X=ur.df(x_timeseries, type = "trend", lags = 5)
summary(DF_X)
DF_X=ur.df(x_timeseries, type = "trend", lags = 4)
summary(DF_X)
DF_X=ur.df(x_timeseries, type = "trend", lags = 3)
summary(DF_X)
DF_X=ur.df(x_timeseries, type = "trend", lags = 2)
summary(DF_X)
DF_X=ur.df(x_timeseries, type = "trend", lags = 1)
summary(DF_X)

# Now re run the test at the first difference: 
DF_Diff_X_AIC = ur.df(diff_x_ts,type = "trend", selectlags = "AIC")

DF_X_BIC = ur.df(x_timeseries,type = "trend", selectlags = "BIC")
DF_Diff_X_BIC = ur.df(diff_x_ts,type = "trend", selectlags = "BIC")

summary(DF_X_none)
summary(DF_X_drift)
summary(DF_X_trend)

summary (DF_Diff_X_AIC)

summary(DF_X_AIC)
summary(DF_X_BIC)
summary(DF_Diff_X_BIC)
df_diffi<-ur.df(diff_x_ts,lag=0)
summary(df_diffi)

df_diffx<-ur.df(Dexports,lag=0)
summary(df_diffx)



DF_X=ur.df(x_timeseries, type = "trend", lags = 1)
summary(DF_X)
DF_X_AIC = ur.df(x_timeseries, type = "trend", selectlags = c("AIC"))
summary(DF_X)
summary(DF_X_AIC)

#BIC

BIC <- function(model) {
  
  ssr <- sum(model$residuals^2)
  t <- length(model$residuals)
  npar <- length(model$coef)
  
  return(
    round(c("p" = npar - 1,
            "BIC" = log(ssr/t) + npar * log(t)/t,
            "R2" = summary(model)$r.squared), 4)
  )
}

install.packages("dynlm")
BIC(dynlm(ts(df_x_ts) ~ 1))
order <- 1:6

BICs <- sapply(order, function(x) 
  "AR" = BIC(dynlm(ts(df_x_ts) ~ L(ts(df_x_ts), 1:x))))

BICs[, which.min(BICs[2, ])]

df_x_ts <- as.data.frame(x_timeseries)

rnk_sel <- lags.select(df_x_ts, lag.max = 10,include = c("const", "trend", "none",
                                                                             "both"), fitMeasure = c("SSR", "LL"), sameSample = TRUE)
rnk_sel
summary(rnk_sel)
library(vars)
VARselect(df_x_ts)

vars::VARselect(x_timeseries)$criteria
vars::VARselect(x_timeseries)$criteria %>% t()
normF <- function(x){
  (x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))
}


#same for M

df_m_ts <- as.data.frame(m_timeseries)
install.packages("dynlm")
library(dynlm)
BIC(dynlm(ts(df_m_ts) ~ 1))
order <- 1:6

BICs <- sapply(order, function(x) 
  "AR" = BIC(dynlm(ts(df_m_ts) ~ L(ts(df_m_ts), 1:x))))

BICs
BICs[, which.min(BICs[2, ])]

summary(normF)

##Playing around
#Computing Logs, annual growth rates and first lag growth rates

quants <- function(series) {
  s <- series
  return(
    data.frame("Level" = s,
               "Logarithm" = log(s),
               "AnnualGrowthRate" = 400 * log(s / lag(s)),
               "1stLagAnnualGrowthRate" = lag(400 * log(s / lag(s))))
  )
}

quants(x_timeseries["1995::2020"])

acf(na.omit(x_timeseries), lag.max = 20, plot = F)
acf(na.omit(x_timeseries))

library(vars)
library(tsmp)
ac(x_timeseries, max.lag = 20)
##Dickey Fuller 3 equations IMPORTS
library(vars)
m.ct <- ur.df(m_timeseries, type = "trend", selectlags = c("AIC"))
summary(m.ct)

m.t <- ur.df(m_timeseries, type = "drift", selectlags = c("AIC"))
summary(m.t)

m.none <- ur.df(m_timeseries, type = "none", selectlags = c("AIC"))
summary(m.none)

##Dickey Fuller 3 equations Exports
library(urca)

x.ct <- ur.df(m_timeseries, type = "trend", lags = 1)

summary(m.ct)

m.t <- ur.df(m_timeseries, type = "drift", lags = 1)
summary(m.t)

m.none <- ur.df(m_timeseries, type = "none", lags = 1)
summary(m.none)



install.packages("zoo")
library(zoo)
x_level <- zoo(x_timeseries)
xport_lag1 <- lag(x_level,-1,na.pad=TRUE)
cbind(x_level,xport_lag1)

delta_x <-x_timeseries - xport_lag1
reg_1 <- lm(delta_x~xport_lag1)
summary(reg_1)


trend=seq_along(delta_x)

reg_2<- lm(delta_x~ trend+ xport_lag1)
summary(reg_2)

library(ur)
adfTrend <- ur.df(x_timeseries, lags = 1, type = 'trend')
summary(adfTrend)

##same for M



install.packages("zoo")
library(zoo)
m_level <- zoo(m_timeseries)
mport_lag1 <- lag(m_level,-1,na.pad=TRUE)
cbind(m_level,mport_lag1)

delta_m <-m_timeseries - mport_lag1
reg_1_m <- lm(delta_m~mport_lag1)
summary(reg_1_m)


trend2=seq_along(delta_m)

reg_2_m<- lm(delta_x~ trend2+ mport_lag1)
summary(reg_2_m)



#######
#ANALYSE BIVARIEEEEEÉ

########

lm(diff_m_ts ~ diff_x_ts)


reg_var1 <- lm(diff(x_timeseries) ~ diff(m_timeseries))
summary(reg_var1)

abline(plor1, col="red")





