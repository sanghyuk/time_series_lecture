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
#[1] "X"              "ddate"          "ddd"            "sido"           "all_tot"        "all_below5"     "all_below15"    "all_15_64"     
#[9] "all_65"         "nonacc_tot"     "nonacc_below5"  "nonacc_below15" "nonacc_15_64"   "nonacc_65"      "circ_tot"       "circ_below5"   
#[17] "circ_below15"   "circ_15_64"     "circ_65"        "resp_tot"       "resp_below5"    "resp_below15"   "resp_15_64"     "resp_65"       
#[25] "all_m"          "nonacc_m"       "circ_m"         "resp_m"         "all_f"          "nonacc_f"       "circ_f"         "resp_f"        
#[33] "month"          "year"           "meantemp"       "mintemp"        "maxtemp"        "meanhumi"       "meanpress"      "dewtemp"       
#[41] "rain"           "windspeed"      "totsun"         "dow"            "area"           "pm25"           "pm10"           "no2"           
#[49] "so2"            "co"             "o3"             "sido_f"         "date"  

summary(cities$ddate)
cities$date<-as.Date(cities$ddate)
summary(cities$date)

summary(cities$ddd)

summary(cities$all_tot)

library(plyr)
ddply(cities,~sido_f,summarise,MEAN=mean(all_tot),SD=sd(all_tot),Min=min(all_tot),Median=median(all_tot),Max=max(all_tot))

ddply(cities,~sido_f,summarise,MEAN=mean(pm25),SD=sd(pm25),Min=min(pm25),Median=median(pm25),Max=max(pm25))

#통계적 이슈
seoul<-subset(cities,sido==11)
plot(seoul$date,seoul$pm10, xlab="Date", ylab=expression(paste("PM"[10],"(",mu,"g/m"^3,")")))
plot(seoul$date,seoul$all_tot, xlab="Date", ylab="Number of Death")

library(mgcv)
fit1<-gam(meantemp~s(ddd),data=seoul,family=gaussian())
plot(fit1)
fit2<-gam(meantemp~s(ddd,k=100),data=seoul,family=gaussian())
plot(fit2)
