rm(list=ls())

##instalacja pakietów---------------------------------------------------------##

install.packages("AER")
install.packages("lmtest")
install.packages("tseries")
install.packages("prais")
install.packages("stargazer")
install.packages("sandwich")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("orcutt")
install.packages("tidyverse")
install.packages("caret")

##wczytanie pakietów----------------------------------------------------------##

library(AER)
library(lmtest)
library(tseries)
library(prais)
library(stargazer)
library(sandwich)
library(stargazer)
library(orcutt)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(caret)

##wczytywanie danych----------------------------------------------------------##

DATA=read.csv("C:/Users/Ola/Documents/me/Aleksandra.Kacprzyk.114125.csv", sep = ";")
names(DATA)
head(DATA)
summary(DATA)

##założenia-------------------------------------------------------------------##
alpha=0.01 ##poziom istotności

##czyszczenie i obrobka danych------------------------------------------------##

DATA$pop_i_mean <- as.numeric(DATA$pop_i_mean)
DATA$pop_j_mean <- as.numeric(DATA$pop_j_mean)
DATA$gni_i_mean <- as.numeric(DATA$gni_i_mean)
DATA$gni_j_mean <- as.numeric(DATA$gni_j_mean)
DATA$country_flow <- as.numeric(DATA$country_flow)

DATA[DATA==0] <-NA
DATA <- na.omit(DATA)

N <- nrow(DATA)
DATA$l_dist <- log(DATA$dist)
DATA$l_pop_i <- log(DATA$pop_i_mean)
DATA$l_pop_j <- log(DATA$pop_j_mean)
DATA$l_gni_i <- log(DATA$gni_i_mean)
DATA$l_gni_j <- log(DATA$gni_j_mean)
DATA$dist2 <- (DATA$dist)^2
DATA$l_flow <- log(DATA$country_flow)
is.nan(DATA$l_flow)
summary(DATA$l_flow)

##wstępna analiza danych------------------------------------------------------##

hist(DATA$pop_i_mean)
hist(DATA$pop_j_mean)
hist(DATA$dist)
hist(DATA$l_pop_i)
hist(DATA$l_pop_j)
hist(DATA$l_dist)
hist(DATA$country_flow)
hist(DATA$l_flow)
hist(DATA$gni_i_mean)
hist(DATA$gni_j_mean)
hist(DATA$l_gni_i)
hist(DATA$l_gni_j)

ggplot(DATA, aes(DATA$pop_i_mean, DATA$country_flow))+
  geom_point()+
  geom_smooth(method = "lm", color="red")+
  ggtitle("wykres punktowy - populacja w państwie origin a migracja")

ggplot(DATA, aes(DATA$pop_j_mean, DATA$country_flow))+
  geom_point()+
  geom_smooth(method = "lm", color="red")+
  ggtitle("wykres punktowy - populacja w państwie destination a migracja")

ggplot(DATA, aes(DATA$l_pop_i, DATA$country_flow))+
  geom_point()+
  geom_smooth(method = "lm", color="red")+
  ggtitle("wykres punktowy - logarytm populacji w państwie origin a migracja")

ggplot(DATA, aes(DATA$l_pop_j, DATA$country_flow))+
  geom_point()+
  geom_smooth(method = "lm", color="red")+
  ggtitle("wykres punktowy - logarytm populacji w państwie destination a migracja")

ggplot(DATA, aes(DATA$dist, DATA$country_flow))+
  geom_point()+
  geom_smooth(method = "lm", color="red")+
  ggtitle("wykres punktowy - dystans a migracja")

ggplot(DATA, aes(DATA$l_dist, DATA$country_flow))+
  geom_point()+
  geom_smooth(method = "lm", color="red")+
  ggtitle("wykres punktowy - logarytm dystansu a migracja")

##modele----------------------------------------------------------------------##

##model1 i analiza------------------------------------------------------------##
model1 <- lm(country_flow ~ pop_i_mean + pop_j_mean + dist + gni_i_mean + gni_j_mean, data = DATA)
summary(model1)

#test RESET
resettest(model1)

#normalnosc rozkładu składnika losowego
ehat1=model1$residuals
hist(ehat1,breaks=50,col="grey",xlab="reszty",ylab="czestotliwosc")
jarque.bera.test(ehat1)

#współliniowość
cor(DATA[,
         c("pop_i_mean","pop_j_mean","dist","gni_i_mean","gni_j_mean")
])
vif(model1)

#heteroskedastyczność - Test heteroskedastyczności Breuscha i Pagana
bptest(model1)

##model2 i analiza------------------------------------------------------------##
model2 <- lm(l_flow ~ l_pop_i + l_pop_j + dist + gni_i_mean + gni_j_mean, data = DATA)
summary(model2)

#test RESET
resettest(model2)

#normalnosc rozkładu składnika losowego
ehat2=model2$residuals
hist(ehat2,breaks=50,col="grey",xlab="reszty",ylab="czestotliwosc")
jarque.bera.test(ehat2)

#współliniowość
cor(DATA[,
         c("l_pop_i","l_pop_j","dist","gni_i_mean","gni_j_mean")
])
vif(model2)

#heteroskedastyczność - Test heteroskedastyczności Breuscha i Pagana
bptest(model2)

##model3 i analiza------------------------------------------------------------##
model3 <- lm(l_flow ~ l_pop_i + l_pop_j + l_dist + l_gni_i + l_gni_j, data = DATA)
summary(model3)

#test RESET
resettest(model3)

#normalnosc rozkładu składnika losowego
ehat3=model3$residuals
hist(ehat3,breaks=50,col="grey",xlab="reszty",ylab="czestotliwosc")
jarque.bera.test(ehat3)

#współliniowość
cor(DATA[,
         c("l_pop_i","l_pop_j","l_dist","l_gni_i","l_gni_j")
])
vif(model3)

#heteroskedastyczność

DATA$ehat_2=model3$residuals^2
plot(log(DATA$l_pop_i), DATA$ehat_2)
plot(log(DATA$l_pop_j), DATA$ehat_2)
plot(log(DATA$l_dist), DATA$ehat_2)
plot(log(DATA$l_gni_i), DATA$ehat_2)
plot(log(DATA$l_gni_j), DATA$ehat_2)

#test White'a
white_model <- lm(ehat_2 ~ l_pop_i + l_pop_j + l_dist + l_gni_i +l_gni_j 
                  + I(l_pop_i^2) + I(l_pop_j^2) + I(l_dist^2) + I(l_gni_j^2)
                  ,data=DATA)

summary(white_model)
LM=N*summary(white_model)$r.squared #statystyka testu mnoznika Lagrange
pchisq(LM,19, lower.tail=FALSE)

#test White'a z interakcjami
white_model_i <- lm(ehat_2 ~ l_pop_i + l_pop_j + l_dist + l_gni_i +l_gni_j 
                  + I(l_pop_i^2) + I(l_pop_j^2) + I(l_dist^2) + I(l_gni_j^2)
                  +I(l_pop_i*l_pop_j) + I(l_pop_i*l_dist) + I(l_pop_i*l_gni_i)
                  + I(l_pop_i*l_gni_j) + I(l_pop_j*l_dist) + I(l_pop_j*l_gni_i)
                  + I(l_pop_j*l_gni_j) + I(l_dist*l_gni_i) + I(l_dist*l_gni_j)
                  + I(l_gni_j*l_gni_i)
               ,data=DATA)

summary(white_model_i)
LM=N*summary(white_model_i)$r.squared #statystyka testu mnoznika Lagrange
pchisq(LM,19, lower.tail=FALSE)

#Test heteroskedastyczności Breuscha i Pagana
bptest(model3)

##uwzględnienie heteroskedastyczność w modelu 3-------------------------------##

#odporne błędy standardowe

VCOVHC=vcovHC(model3,type = 'HC3')

coeftest(model3)
model3_coeftest <- coeftest(model3,vcov.=VCOVHC)

#ważona MNK

auxilary.lm <-	lm(log(ehat_2)~ l_pop_i + l_pop_j + l_dist + l_gni_i +l_gni_j
                                ,data=DATA)

DATA$weights	=	1/sqrt(exp(auxilary.lm$fitted.values))

model3_wls <- lm(l_flow ~ l_pop_i + l_pop_j + l_dist + l_gni_i + l_gni_j, data = DATA, weights = DATA$weights)

##porownanie modeli (model3, odporne błędy standardowe, ważona MNK)-----------##
stargazer(model3, model3_coeftest, model3_wls, 
          column.labels = c("MNK","Odporne bledy standardowe", "wazone MNK"), type="text")





