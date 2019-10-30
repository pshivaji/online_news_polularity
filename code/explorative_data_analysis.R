cf <- read.csv("OnlineNewsPopularity.csv",header = TRUE)
summary(cf)
cf$url <- NULL
cf$timedelta <- NULL
cf$weekday_is_sunday <- NULL
cf$is_weekend <- NULL
plot(cf[,c(1:10)],cf$share)
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
library(ggplot2)
library(dplyr)
library(tidyr)
class(cf)
cf %>%
  gather(-shares, key = "var",value = "value") %>%
  ggplot(aes(x =value, y= shares))+
  geom_point()+
  facet_wrap(~var ,scales = "free")+
  theme_bw()
#removing the values with vif>10
cf$n_unique_tokens <- NULL
cf$n_non_stop_words <- NULL
cf$n_non_stop_unique_tokens<- NULL
cf$average_token_length<- NULL
cf$kw_max_min<- NULL
cf$kw_avg_min<- NULL
cf$kw_avg_avg<- NULL
cf$self_reference_avg_sharess<- NULL
cf$LDA_00<- NULL
cf$LDA_01<- NULL
cf$LDA_02 <- NULL
cf$LDA_03 <- NULL
cf$LDA_04<- NULL
cf$rate_positive_words<- NULL
cf$rate_negative_words<- NULL
modelcf <- lm(c$shares~.-cf$shares,cf)
summary(modelcf)
par(mfrow = c(1,1))
plot(modelcf,which = 4)

  