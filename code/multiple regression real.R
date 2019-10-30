#loading the data
dat <- read.csv("OnlineNewsPopularity.csv",header = TRUE)
#As we do not recquire the first 2 columns eliminating them 
dat$url <- NULL
dat$timedelta <- NULL
#fitting the multi-regression model
model <- lm(dat$shares~.-dat$shares,dat)
#summary
summary(model)
dat$weekday_is_sunday <- NULL
dat$is_weekend <- NULL
plot(model,which=4)
library(car)
table(vif(model1)>10)
vif(model1)>10
# remvoing the independent variables which has vif>10
dat$n_unique_tokens<-NULL
dat$n_non_stop_words <-NULL
dat$n_non_stop_unique_tokens <-NULL
dat$average_token_length <-NULL
dat$kw_max_min <-NULL
dat$kw_avg_min <-NULL
dat$kw_avg_avg <-NULL
dat$self_reference_avg_sharess <-NULL
dat$LDA_00 <-NULL
dat$LDA_01 <-NULL
dat$LDA_02 <-NULL
dat$LDA_03  <-NULL
dat$LDA_04 <-NULL
dat$rate_positive_words <-NULL
dat$rate_negative_words <-NULL
#fitting the data for adjusted data set
model2 <- lm(dat$shares~.-dat$shares,dat)
summary(model2)
par(mfrow =c(1,1))
dat2 <- dat
dat2<- dat[-c(5371, 9366, 23238),]
plot(model2)
model3 <- lm(dat2$shares~.-dat2$shares,dat2)
plot(model3)
#summary
summary(model3)
plot(dat$shares,dat$data_channel_is_bus)

