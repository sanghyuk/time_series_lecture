cities<-read.csv(file="https://raw.githubusercontent.com/sanghyuk/time_series_lecture/main/exercise_data_7_cities.csv")
head(cities)
tail(cities)
summary(cities)
cities$sido_f<-as.factor(cities$sido)
summary(cities$sido_f)
summary(cities$sido)
table(cities$area,cities$sido_f)
#11:서울, 26:부산, 27:대구, 28:인천, 29:광주, 30:대전, 31:울산

names(cities)
#[1] "X"           "ddate"       "ddd"         "sido"        "all_tot"     "all_below5"  "all_below15"
#[8] "all_15_64"   "all_65"      "month"       "year"        "meantemp"    "mintemp"     "maxtemp"    
#[15] "meanhumi"    "meanpress"   "dewtemp"     "rain"        "windspeed"   "totsun"      "dow"        
#[22] "area"        "pm25"        "pm10"        "no2"         "so2"         "co"          "o3"         
#[29] "sido_f"     

summary(cities$ddate)
cities$date<-as.Date(cities$ddate)
summary(cities$date)

summary(cities$ddd)

summary(cities$all_tot)

library(plyr)
ddply(cities,~sido_f,summarise,MEAN=mean(all_tot),SD=sd(all_tot),Min=min(all_tot),Median=median(all_tot),Max=max(all_tot))

ddply(cities,~sido_f,summarise,MEAN=mean(pm25),SD=sd(pm25),Min=min(pm25),Median=median(pm25),Max=max(pm25))

#4.통계적 이슈
seoul<-subset(cities,sido==11)
plot(seoul$date,seoul$pm10, xlab="Date", ylab=expression(paste("PM"[10],"(",mu,"g/m"^3,")")))
plot(seoul$date,seoul$all_tot, xlab="Date", ylab="Number of Death")

library(mgcv)
fit1<-gam(meantemp~s(ddd),data=seoul,family=gaussian())
plot(fit1)
fit2<-gam(meantemp~s(ddd,k=100),data=seoul,family=gaussian())
plot(fit2)

#5.탐색적 자료 분석

plot(seoul$date,seoul$pm10, xlab="Date", ylab=expression(paste("PM"[10],"(",mu,"g/m"^3,")")))

#time trend에 대한 PM10의 선형 모형
fit<-lm(pm10~ddd, data=seoul)
summary(fit)

#서울과 부산의 PM10 농도 시간적 추세 with smooth spline
library(stats)
library(splines)
seoul.sub<-subset(seoul, date>=as.Date("2016-01-01"))
fit<-lm(pm10 ~ ns(date, df=2*3),data=seoul.sub)
x<-seq(as.Date("2016-01-01"),as.Date("2018-12-31"),"week")
par(mar=c(2,4,2,2),mfrow=c(2,1))
with(seoul.sub, 
     {plot(date,pm10,ylab=expression(PM[10]),main="(a)Seoul")
       lines(x,predict(fit,data.frame(date=x)),col="red")
     })

busan<-subset(cities,sido==26)
busan.sub<-subset(busan, date>=as.Date("2016-01-01"))
fit<-lm(pm10 ~ ns(date, df=2*3),data=busan.sub)
x<-seq(as.Date("2016-01-01"),as.Date("2018-12-31"),"week")
with(busan.sub, 
     {plot(date,pm10,ylab=expression(PM[10]),main="(a)Busan")
       lines(x,predict(fit,data.frame(date=x)),col="red")
     })

#오존 농도 시계열
par(mfrow=c(2,1),mar=c(3,4,2,2))
with(seoul, plot(date,o3,main="(a)Seoul",ylab=expression(O[3]*"(ppm)"),pch="."))
with(busan, plot(date,o3,main="(a)Busan",ylab=expression(O[3]*"(ppm)"),pch="."))

#사망자 수 시계열
par(mfrow=c(3,1), mar=c(2,4,2,2)+0.1)
with(seoul,plot(date,all_below5,main="Under 5",ylab="Moratlity count", pch="."))
with(seoul,plot(date,all_below15+all_15_64,main="Under 65",ylab="Moratlity count", pch="o"))
with(seoul,plot(date,all_65,main="Over 65",ylab="Moratlity count", pch="x"))

#사망자 수 시계열 seasonality removed
par(mfrow=c(2,1),mar=c(5,4,3,2)+0.1 )
x<-seoul$all_65
acf(x,lag.max=50, main="(a)Seoul mortality",ci.col="black")

fit<-lm(x~ns(1:4748,2*13))
xr<-resid(fit)
labels<-"(b)Seoul mortality (seasonality removed)"
acf(xr,lag.max=50, main=labels,ci.col="black")

#simple linear regression
y<-seoul$all_tot
x<-seoul$pm10
fit<-lm(y~x)
summary(fit)

#timescale decomposition PM10
library(stats)
x.yearly<-filter(x,rep(1/365,365))
z <- x - x.yearly
z.seasonal <- filter(z, rep(1/90, 90))
u <- z - z.seasonal
u.weekly <- filter(u, rep(1/7, 7))
r <- u - u.weekly
fit <- lm(y ~ x.yearly + z.seasonal +u.weekly + r)
summary(fit)

#timescale decomposition mortality
y.yearly <- filter(y, rep(1/365, 365))
yz <- y - y.yearly
yz.seasonal <- filter(yz, rep(1/90, 90))
yu <- yz - yz.seasonal
yu.weekly <- filter(yu, rep(1/7, 7))
yr <- yu - yu.weekly

#timescale decomposition 그림
par(mfrow=c(4,2),mar=c(3,4,2,2)+0.1)
plot(seoul$date,x.yearly,type="l",ylab="",xlab="",main="Yearly(PM10)")
plot(seoul$date,y.yearly,type="l",ylab="",xlab="",main="Yearly(Mortality)")
plot(seoul$date,z.seasonal,type="l",ylab="",xlab="",main="Seasonal")
plot(seoul$date,yz.seasonal,type="l",ylab="",xlab="",main="Seasonal")
plot(seoul$date,u.weekly,type="l",ylab="",xlab="",main="Weekly")
plot(seoul$date,yu.weekly,type="l",ylab="",xlab="",main="Weekly")
plot(seoul$date,r,type="l",ylab="", xlab="",main="Residual")
plot(seoul$date,yr,type="l",ylab="", xlab="",main="Residual")

#서울 사망 timescale decomposition smooth
death<-seoul$all_tot
library(tsModel)
mort.dc<-tsdecomp(death, c(1,2,15,4748))
par(mfrow = c(3, 1), mar = c(3, 4, 2, 2) + 0.1)
x <- seq(as.Date("2006-01-01"), as.Date("2018-12-31"),"day")
plot(x, mort.dc[, 1], type = "l", ylab = "Trend",main = "(a)")
plot(x, mort.dc[, 2], type = "l", ylab = "Seasonal",main = "(b)")
plot(x, mort.dc[, 3], type = "l", ylab = "Residual",main = "(c)")

#서울 PM10 timescale decomposition smooth
pm10<-seoul$pm10
poll.dc <- tsdecomp(pm10, c(1, 2, 15, 4748))
par(mfrow = c(3, 1), mar = c(3, 4, 1, 2) + 0.1)
x <- seq(as.Date("2006-01-01"), as.Date("2018-12-31"),"day")
plot(x, poll.dc[, 1], type = "l", ylab = "Trend")
plot(x, poll.dc[, 2], type = "l", ylab = "Seasonal")
plot(x, poll.dc[, 3], type = "l", ylab = "Residual")

# timescale에 따른 correlation
c1 <- cor(mort.dc[, 1], poll.dc[, 1],use = "complete.obs")
c2 <- cor(mort.dc[, 2], poll.dc[, 2],use = "complete.obs")
c3 <- cor(mort.dc[, 3], poll.dc[, 3],use = "complete.obs")

#모든 timescale을 한 모형에 넣고 분석하기
library(stats)
poll.df <- as.data.frame(poll.dc)
names(poll.df) <- c("Trend", "Season","ShortTerm")
fit <- lm(death ~ Trend + Season + ShortTerm,data = poll.df)
summary(fit)

#detailed timescale decomposition for seoul pm10
freq.cuts <- c(1, 2, 15, round(4748/c(60,30, 14, 7, 3.5)), 4748)
poll.dc <- tsdecomp(pm10, freq.cuts)
colnames(poll.dc) <- c("Long-term", "Seasonal", "2-12 months", "1-2 months", "2-4 weeks", "1-2 weeks", "3.5 days to 1 week", "Less than 3.5 days")
par(mfcol = c(4, 2), mar = c(2, 2, 2, 2))
x <- seq(as.Date("2006-01-01"), as.Date("2018-12-31"),"day")
cn <- colnames(poll.dc)
for (i in 1:8) {
  plot(x, poll.dc[, i], type = "l",
       frame.plot = FALSE, main = cn[i],
       ylab = "")
}


# 모두 한 모형에 넣고 분석
poll.df <- as.data.frame(poll.dc[, 1:8])
fit <- lm(death ~ ., data = poll.df)
summary(fit)

#서울 기온
par(mfrow=c(1,1), mar=c(5, 4, 4, 2) + 0.1)
with(seoul, plot(date,meantemp, type="l", ylab="Temperature"))

# 기온을 제거한 timescale decomposition
temp<-seoul$meantemp
plot(temp,pm10)
pm10.r <- resid(lm(pm10 ~ temp, na.action = na.exclude))
death.r <- resid(lm(death ~ temp, na.action = na.exclude))
poll.dc <- tsdecomp(pm10.r, c(1, 2, 15, 4748))

par(mfrow = c(3, 1), mar = c(3, 4, 1, 2) + 0.1)
x <- seq(as.Date("2006-01-01"), as.Date("2018-12-31"), "day")
plot(x, poll.dc[, 1], type = "l", ylab = "Trend")
plot(x, poll.dc[, 2], type = "l", ylab = "Seasonal")
plot(x, poll.dc[, 3], type = "l", ylab = "Residual")

poll.df <- as.data.frame(poll.dc)
names(poll.df) <- c("Trend", "Season", "ShortTerm")
fit <- lm(death.r ~ Trend + Season + ShortTerm, data = poll.df)
summary(fit)

# 부산의 계절별 관련성
busan<-subset(cities,sido==26)
busan_short<-subset(busan,year<2010)
library(HEAT)
busan_short<-lagdata(busan_short,c("pm10","meantemp","o3"),1)
names(busan_short)
par(mfrow=c(1,1), mar=c(5, 4, 4, 2) + 0.1)
with(busan_short, plot(pm10_s1,all_tot,xlab=expression(PM[10]), ylab="Daily mortality", cex=0.6, col="gray"))
f <- lm(all_tot ~ pm10_s1, busan_short)
with(busan_short, {
  lines(sort(pm10_s1), predict(f, data.frame(pm10_s1 = sort(pm10_s1))),lwd = 1)
})

busan_short$winter<-ifelse(busan_short$month==12|busan_short$month==1|busan_short$month==2,"black","gray")
busan_short$spring<-ifelse(busan_short$month==3|busan_short$month==4|busan_short$month==5,"black","gray")
busan_short$summer<-ifelse(busan_short$month==6|busan_short$month==7|busan_short$month==8,"black","gray")
busan_short$fall<-ifelse(busan_short$month==9|busan_short$month==10|busan_short$month==11,"black","gray")

par(mfrow=c(2,2))
with(busan_short, plot(pm10_s1,all_tot,xlab=expression(PM[10]), ylab="Daily mortality", main="(a)Winter",cex=0.6, col=winter))
f <- lm(all_tot ~ pm10_s1, subset(busan_short,month==12|month==1|month==2))
with(busan_short, {
  lines(sort(pm10_s1), predict(f, data.frame(pm10_s1 = sort(pm10_s1))),lwd = 1)
})
with(busan_short, plot(pm10_s1,all_tot,xlab=expression(PM[10]), ylab="Daily mortality", main="(b)Spring",cex=0.6, col=spring))
f <- lm(all_tot ~ pm10_s1, subset(busan_short,month==3|month==4|month==5))
with(busan_short, {
  lines(sort(pm10_s1), predict(f, data.frame(pm10_s1 = sort(pm10_s1))),lwd = 1)
})
with(busan_short, plot(pm10_s1,all_tot,xlab=expression(PM[10]), ylab="Daily mortality", main="(c)Summer",cex=0.6, col=summer))
f <- lm(all_tot ~ pm10_s1, subset(busan_short,month==6|month==7|month==8))
with(busan_short, {
  lines(sort(pm10_s1), predict(f, data.frame(pm10_s1 = sort(pm10_s1))),lwd = 1)
})
with(busan_short, plot(pm10_s1,all_tot,xlab=expression(PM[10]), ylab="Daily mortality", main="(d)Fall",cex=0.6, col=fall))
f <- lm(all_tot ~ pm10_s1, subset(busan_short,month==9|month==10|month==11))
with(busan_short, {
  lines(sort(pm10_s1), predict(f, data.frame(pm10_s1 = sort(pm10_s1))),lwd = 1)
})

#6. 관련성 분석
# PM is weak signal
library(HEAT)
seoul<-lagdata(seoul,c("pm10","meantemp","o3"),1)
fit <- glm(all_tot ~ pm10_s1 + ns(date, 4 * 14) + ns(meantemp, 6) + dow, data = seoul, na.action = na.exclude, family = poisson)
pr<-predict(fit, type="terms")
pred<-as.data.frame(pr)
names(pred)<-c("pred_pm","pred_date","pred_temp","pred_dow")
pred$date<-as.Date(seoul$date)

with(pred,plot(date,pred_date, type="l",lty=2,xlab="",ylab=""))
with(pred,lines(date,pred_temp,lwd=1, col="grey"))
with(pred,lines(date,pred_pm,lwd=1))

with(pred,plot(date,pred_pm,type="l"))

#single lag models in Seoul
library(tsModel)
f0 <- glm(all_tot ~ pm10 + meantemp, data = seoul, family = poisson)
f1 <- glm(all_tot ~ Lag(pm10, 1) + meantemp, data = seoul, family = poisson)
f2 <- glm(all_tot ~ Lag(pm10, 2) + meantemp, data = seoul, family = poisson)
f3 <- glm(all_tot ~ Lag(pm10, 3) + meantemp, data = seoul, family = poisson)
f4 <- glm(all_tot ~ Lag(pm10, 4) + meantemp, data = seoul, family = poisson)
ss <- list(summary(f0), summary(f1), summary(f2),summary(f3), summary(f4))
models <- lapply(ss, function(x) x$coefficients[2,c("Estimate", "Std. Error")])
models

#distributed lag model
fit <- glm(all_tot ~ Lag(pm10, 0:4) + meantemp, data = seoul, family = poisson)
summ <- summary(fit)
summ

rn <- rownames(summ$coefficients)
i <- grep("pm10", rn, fixed = TRUE)
coefs <- summ$coefficients[i, "Estimate"]
total <- sum(coefs)
total

#부산 temperature lags
busan<-lagdata(busan,"pm10",3)
busan.sub<-subset(busan,year<2011)
library(tsModel)
maxlag <- 0:13
models <- sapply(maxlag, function(mlag) {
  fit <- glm(all_tot ~ pm10_s1 + Lag(meantemp, seq(0, mlag)), data = busan.sub,family = poisson)
  summ <- summary(fit)
  summ.coef <- summ$coefficients["pm10_s1",2]
  c(coef(fit)["pm10_s1"], summ.coef)
})

rng <- range(models[1, ] - 1.96 * models[2,], models[1, ] + 1.96 * models[2, ], 0)
par(mar = c(4, 5, 1, 1))
plot(maxlag, models[1, ], type = "b",pch = 20, ylim = rng, xlab = "Maximum temperature lag",ylab = expression(hat(beta) * " for " *
                                                                                                                PM[10] * " at lag 1"))
lines(maxlag, models[1, ] + 1.96 * models[2,], lty = 2)
lines(maxlag, models[1, ] - 1.96 * models[2, ], lty = 2)
abline(h = 0, lty = 3)

#부산 warm season
busan.warm <- subset(busan.sub, quarters(date) %in% c("Q2", "Q3"))
models.warm <- sapply(maxlag, function(mlag) {
  fit <- glm(all_tot ~ pm10_s1 + Lag(meantemp, seq(0, mlag)), data = busan.warm,family = poisson)
  summ <- summary(fit)
  summ.coef <- summ$coefficients["pm10_s1",2]
  c(coef(fit)["pm10_s1"], summ.coef)
})

#부산 cold season
busan.cold <- subset(busan.sub, quarters(date) %in% c("Q1", "Q4"))
models.cold <- sapply(maxlag, function(mlag) {
  fit <- glm(all_tot ~ pm10_s1 + Lag(meantemp, seq(0, mlag)), data = busan.cold,family = poisson)
  summ <- summary(fit)
  summ.coef <- summ$coefficients["pm10_s1",2]
  c(coef(fit)["pm10_s1"], summ.coef)
})

#그림
library(lattice)
trellis.par.set(theme = canonical.theme("pdf",FALSE))
y <- c(models.warm[1, ], models.cold[1,])
xpts <- rep(maxlag, 2)
f <- gl(2, length(maxlag), labels = c("Warm season","Cold season"))
std <- c(models.warm[2, ], models.cold[2,])
rng <- range(y - 1.96 * std, y + 1.96 *std, 0)
rng <- rng + c(-1, 1) * 0.05 * diff(rng)
ylab <- expression(hat(beta) * " for " *PM[10] * " at lag 1")
p <- xyplot(y ~ xpts | f, as.table = TRUE, ylim = rng, subscripts = TRUE, panel = function(x,y, subscripts, ...) {
  panel.xyplot(x, y, ...)
  llines(x, y - 1.96 * std[subscripts],lty = 2)
  llines(x, y + 1.96 * std[subscripts],lty = 2)
  panel.abline(h = 0, lty = 3)
}, xlab = "Maximum temperature lag",
layout = c(1, 2), type = "b", ylab = ylab,
pch = 20)
print(p)

#time smooth function 보정
models <- sapply(maxlag, function(mlag) {
  fit <- glm(all_tot ~ pm10_s1 + Lag(meantemp, seq(0, mlag))+ns(date,4*5), data = busan.sub,family = poisson)
  summ <- summary(fit)
  summ.coef <- summ$coefficients["pm10_s1",2]
  c(coef(fit)["pm10_s1"], summ.coef)
})
rng <- range(models[1, ] - 1.96 * models[2,], models[1, ] + 1.96 * models[2, ], 0)
par(mar = c(4, 5, 1, 1))
plot(maxlag, models[1, ], type = "b",pch = 20, ylim = rng, xlab = "Maximum temperature lag",ylab = expression(hat(beta) * " for " *
                                                                                                                PM[10] * " at lag 1"))
lines(maxlag, models[1, ] + 1.96 * models[2,], lty = 2)
lines(maxlag, models[1, ] - 1.96 * models[2, ], lty = 2)
abline(h = 0, lty = 3)

#smooth of mortality
library(gam)
library(splines)
xpts <- seq(as.Date("2006-01-01"), as.Date("2018-12-31"), "day")
fit2 <- gam(all_tot ~ s(date, 2 * 13), family = poisson, data = busan)
summary(fit2)
p2 <- predict(fit2, data.frame(date = xpts),type = "response")
fit12 <- gam(all_tot ~ s(date, 12 * 13),family = poisson, data = busan)
p12 <- predict(fit12, data.frame(date = xpts),type = "response")
plot(xpts,busan$all_tot, pch=46, ylab="Mortality count", xlab="")
lines(xpts,p2)
lines(xpts,p12, col="gray")

#smooths of pm10 and o3
par(mfrow=c(1,2))
fit2pm10 <- gam(pm10 ~ s(date, 2 * 13), data = busan)
p2pm10 <- predict(fit2pm10, data.frame(date = xpts),type = "response")
fit12pm10 <- gam(pm10 ~ s(date, 12 * 13), data = busan)
p12pm10 <- predict(fit12pm10, data.frame(date = xpts),type = "response")
plot(xpts,busan$pm10, pch=46, ylab="PM10", xlab="", ylim=c(0,150))
lines(xpts,p2pm10)
lines(xpts,p12pm10, col="gray")

fit2o3 <- gam(o3 ~ s(date, 2 * 13),  data = busan)
p2o3 <- predict(fit2o3, data.frame(date = xpts),type = "response")
fit12o3 <- gam(o3 ~ s(date, 12 * 13), data = busan)
p12o3 <- predict(fit12o3, data.frame(date = xpts),type = "response")
plot(xpts,busan$o3, pch=46, ylab="O3", xlab="")
lines(xpts,p2o3)
lines(xpts,p12o3, col="gray")

#different dfs
par(mfrow=c(1,2))
dfValues <- c(2, 4, 6, 8, 10, 12, 14)
control <- gam.control(epsilon = 0.00000001, bf.epsilon = 0.00000001)
modelsGAM <- sapply(dfValues, function(dfVal) {
  total.df <- dfVal * 13
  fit <- gam(all_tot ~ pm10_s1 + meantemp + s(date, total.df), data = busan, family = poisson, control = control)
  gamex <- gam.exact(fit)
  gamex.coef <- gamex$coefficients["pm10_s1", "A-exact SE"]
  c(coef(fit)["pm10_s1"], gamex.coef)
})
rng <- range(modelsGAM[1, ] - 1.96 * modelsGAM[2,], modelsGAM[1, ] + 1.96 * modelsGAM[2, ], 0)
par(mar = c(4, 5, 2, 2),mfrow=c(2,1))
plot(dfValues, modelsGAM[1, ], type = "b",pch = 20, ylim = rng, xlab = "df per year",ylab = expression(hat(beta)),main="(a)")
lines(dfValues, modelsGAM[1, ] + 1.96 * modelsGAM[2,], lty = 2)
lines(dfValues, modelsGAM[1, ] - 1.96 * modelsGAM[2, ], lty = 2)
abline(h = 0, lty = 3)

modelsGLM <- sapply(dfValues, function(dfVal) {
  total.df <- dfVal * 14
  fit <- glm(all_tot ~ pm10_s1 + meantemp + ns(date, total.df), data = busan,family = poisson)
  summ <- summary(fit)
  summ.coef <- summ$coefficients["pm10_s1",2]
  c(coef(fit)["pm10_s1"], summ.coef)
})
rng <- range(modelsGLM[1, ] - 1.96 * modelsGLM[2,], modelsGLM[1, ] + 1.96 * modelsGLM[2, ], 0)
plot(dfValues, modelsGLM[1, ], type = "b",pch = 20, ylim = rng, xlab = "df per year",ylab = expression(hat(beta)),main="(b)")
lines(dfValues, modelsGLM[1, ] + 1.96 * modelsGLM[2,], lty = 2)
lines(dfValues, modelsGLM[1, ] - 1.96 * modelsGLM[2, ], lty = 2)
abline(h = 0, lty = 3)


#교과서 예제를 부산에 적용한 것 (참고)
library(mda)
pm10 <- busan$pm10_s1
x <- unclass(busan$date)
use <- complete.cases(pm10, x)
br.fit <- bruto(x[use], pm10[use])
optimal.df <- br.fit$df
library(gam)
fit <- gam(all_tot ~ pm10_s1 + s(date,optimal.df), data = busan, family = quasipoisson)
v <- gam.exact(fit)
print(v$coefficients["pm10_s1", "Estimate"])
print(v$coefficients["pm10_s1", "A-exact SE"])
summary(fit)$dispersion


use <- complete.cases(pm10, x)
br.fit <- bruto(x[use], pm10[use])
df.pm10 <- br.fit$df

death <- busan$all_tot
use <- complete.cases(death, x)
br.fit <- bruto(x[use], death[use])
df.death <- br.fit$df

fit1 <- gam(all_tot ~ pm10_s1 + s(date,df.pm10), data = busan, family = quasipoisson)
fit2 <- gam(all_tot ~ pm10_s1 + s(date,df.death), data = busan, family = quasipoisson)
v1 <- gam.exact(fit1)
v2 <- gam.exact(fit2)

#보다 모던한 방법
seoul.sub<-subset(seoul,year<2011)
library(mgcv)
fit3<-gam(all_tot~pm10+meanhumi+s(meantemp)+dow+s(ddd,k=4*5), data=seoul.sub, family=poisson)
summary(fit3)
plot(fit3)

#여러 결과의 합성
daegu<-subset(cities,sido==27)
incheon<-subset(cities,sido==28)
gwangju<-subset(cities,sido==29)
daejeon<-subset(cities,sido==30)
ulsan<-subset(cities,sido==31)

library(mgcv)
seoul.f<-gam(all_tot~pm10+meanhumi+s(meantemp)+dow+s(ddd,k=4*13),family=poisson(),data=seoul)
se<-summary(seoul.f)
busan.f<-gam(all_tot~pm10+meanhumi+s(meantemp)+dow+s(ddd,k=4*13),family=poisson(),data=busan)
bu<-summary(busan.f)
daegu.f<-gam(all_tot~pm10+meanhumi+s(meantemp)+dow+s(ddd,k=4*13),family=poisson(),data=daegu)
dg<-summary(daegu.f)
incheon.f<-gam(all_tot~pm10+meanhumi+s(meantemp)+dow+s(ddd,k=4*13),family=poisson(),data=incheon)
ic<-summary(incheon.f)
gwangju.f<-gam(all_tot~pm10+meanhumi+s(meantemp)+dow+s(ddd,k=4*13),family=poisson(),data=gwangju)
gj<-summary(gwangju.f)
daejeon.f<-gam(all_tot~pm10+meanhumi+s(meantemp)+dow+s(ddd,k=4*13),family=poisson(),data=daejeon)
dj<-summary(daejeon.f)
ulsan.f<-gam(all_tot~pm10+meanhumi+s(meantemp)+dow+s(ddd,k=4*13),family=poisson(),data=ulsan)
us<-summary(ulsan.f)

beta<-c(seoul.f$coefficients["pm10"],busan.f$coefficients["pm10"],daegu.f$coefficients["pm10"],incheon.f$coefficients["pm10"],daejeon.f$coefficients["pm10"],gwangju.f$coefficients["pm10"],ulsan.f$coefficients["pm10"])
se<-c(se$se["pm10"], bu$se["pm10"],dg$se["pm10"],ic$se["pm10"],gj$se["pm10"],dj$se["pm10"],us$se["pm10"])
n <- length(beta)
rng <- range(beta - 1.96 * se, beta +1.96 * se)
plot(beta, seq_len(n), xlim = rng, pch = 20,xlab = expression(hat(beta)), ylab = "City")
segments(beta - 1.96 * se, seq(n), beta +1.96 * se, seq(n))
abline(v = 0, lty = 2)

library(metafor)
meta1=rma(yi=beta, sei=se)
summary(meta1)
