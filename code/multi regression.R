# loading the data
dat <- read.csv("OnlineNewsPopularity.csv",header = TRUE)
dat$url <- NULL
dat$timedelta <- NULL
# coefficents are na so removinf the columns
dat$weekday_is_sunday <- NULL
dat$is_weekend <- NULL
#model
model <- lm(dat$shares~.-dat$shares,dat)
summary(model)
library(car)
#removing the values with vif>10
vif(model) > 10
dat$n_unique_tokens <- NULL
dat$n_non_stop_words <- NULL
dat$n_non_stop_unique_tokens<- NULL
dat$average_token_length<- NULL
dat$kw_max_min<- NULL
dat$kw_avg_min<- NULL
dat$kw_avg_avg<- NULL
dat$self_reference_avg_sharess<- NULL
dat$LDA_00<- NULL
dat$LDA_01<- NULL
dat$LDA_02 <- NULL
dat$LDA_03 <- NULL
dat$LDA_04<- NULL
dat$rate_positive_words<- NULL
dat$rate_negative_words<- NULL
m2 <- lm(dat$shares~.-dat$shares,dat)
summary(m)
#removing the outliers
dat <- dat[-c(5371, 9366 ,23238),]
m4 <- lm(dat$shares~.-dat$shares,dat)
summary(m4)
#TRANSFORMING THE DATA
dat$n_tokens_title = exp(-dat$n_tokens_title)
m3 <- lm(dat$shares~.-dat$shares,dat)
summary(m3)
AIC(m3)
BIC(m3)
