#loading the data
c <- read.csv("OnlineNewsPopularity.csv",header = TRUE)
summary(c)
library("ROCR")
c$url <- NULL
c$timedelta <- NULL
#changing the shares values to 0(<1400) or 1 (>=1400)
c$shares[c$shares < 1400]<- 0
c$shares[c$shares >= 1400]<- 1
#logistic regresssionc
c$weekday_is_sunday <- NULL
c$is_weekend <- NULL
logmodel <- glm(c$shares ~ .-c$shares,c,family = 'binomial')
summary(logmodel)
library(car)
table(vif(logmodel)>=10)
vif(logmodel)>=10
#removing the independent variables which has vif greater than 10
c$n_unique_tokens<-NULL
c$n_non_stop_words <-NULL
c$n_non_stop_unique_tokens <-NULL
c$average_token_length <-NULL
c$kw_max_min <-NULL
c$kw_avg_min <-NULL
c$LDA_00 <-NULL
c$LDA_01 <-NULL
c$LDA_02 <-NULL
c$LDA_03  <-NULL
c$LDA_04 <-NULL
c$rate_positive_words <-NULL
c$rate_negative_words <-NULL
logmodel1 <- glm(c$shares ~ .-c$shares,c,family = 'binomial')
plot(logmodel)
p1 = predict(logmodel1,c)
tROC1<-performance(prediction(p1,c$shares),"tpr","fpr")
plot(tROC1)
abline(a=0, b= 1,lty=2)
tAUC1<-as.double(performance(prediction(p1,c$shares),"auc")@y.values)
tAUC1
# removing the outliers and checking influence of outlier on the model
d <- c
d <- d[-c(10262, 123001,34477),]
logmodel3 <- glm(d$shares ~ .-d$shares,d,family = 'binomial')
summary(logmodel3)
p2 = predict(logmodel3,d)
s = as.integer(p2>=0.5)
table(s,d$shares)
tROC2<-performance(prediction(p2,d$shares),"tpr","fpr")
plot(tROC2)
abline(a=0, b= 1,lty=2)
tAUC2<-as.double(performance(prediction(p2,d$shares),"auc")@y.values)
tAUC2
#transforming the data columns
d$n_tokens_title = exp(-d$n_tokens_title)
d$n_tokens_title
logmodel4 <- glm(d$shares ~ .-d$shares,d,family = 'binomial')
summary(logmodel4)
p2 = predict(logmodel3,d,type="response")
s = as.integer(p2 > 0.5)
table(s,d$shares)
tROC3<-performance(prediction(p3,d$shares),"tpr","fpr")
par(mfrow= c(1,1))
plot(tROC3)
abline(a=0, b= 1,lty=2)
tAUC3<-as.double(performance(prediction(p3,d$shares),"auc")@y.values)
tAUC3

