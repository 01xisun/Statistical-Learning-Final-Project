
# Author: Xi Sun, Rena Haswah
# Email address: xsun46@hawk.iit.edu, rhaswah@hawk.iit.edu
# Date: 05/03/2019

library(corrplot)
library(ggplot2)
library(splitstackshape)
library(prodlim)
pulsar_stars <- read.csv("C:/Users/31586/Desktop/pulsar_stars.csv",
                         header=TRUE)
colnames(pulsar_stars) <- c("mean_profile",
                            "sd_profile",
                            "kur_profile",
                            "skew_profile",
                            "mean_dmsnr",
                            "sd_dmsnr",
                            "kur_dmsnr",
                            "skew_dmsnr",
                            "target_class")


corstar <- cor(pulsar_stars)

tb_class <- table(pulsar_stars$target_class)

par(mfrow=c(1,2))
corrplot(corstar)
barplot(tb_class,main="target_class",xlab="class",ylab="frequency")


set.seed(12345)
train <- as.data.frame(stratified(pulsar_stars,"target_class",0.8))
test <- pulsar_stars[-row.match(train,pulsar_stars),]
fit <- glm(target_class~.,family=binomial,data=train)
summary(fit)


pred_all <- predict(fit,test,type="response")
pred_all <- ifelse(pred_all>0.5,1,0)
table_all <- table(pred=pred_all,true=test$target_class)
table_all
sum(diag(table_all))/sum(table_all)


back <- step(fit,scope=list(lower=target_class~1,upper=target_class~.),
             direction="backward")


back$call
back$aic


fit.best <- glm(as.formula(back$call),family=binomial,data=train)
pred_best <- predict(fit.best,test,type="response")
pred_best <- ifelse(pred_best>0.5,1,0)
table_best <- table(pred=pred_best,true=test$target_class)
table_best
sum(diag(table_best))/sum(table_best)


fit_profile <- glm(target_class~mean_profile+sd_profile+kur_profile+skew_profile,family=binomial,data=train)
summary(fit_profile)


pred_profile <- predict(fit_profile,test,type="response")
pred_profile <- ifelse(pred_profile>0.5,1,0)
table_profile <- table(pred=pred_profile,true=test$target_class)
table_profile
sum(diag(table_profile))/sum(table_profile)


fit_dmsnr <- glm(target_class~mean_dmsnr+sd_dmsnr+kur_dmsnr+skew_dmsnr,family=binomial,data=train)
summary(fit_dmsnr)


pred_dmsnr <- predict(fit_dmsnr,test,type="response")
pred_dmsnr <- ifelse(pred_dmsnr>0.5,1,0)
table_dmsnr <- table(pred=pred_dmsnr,true=test$target_class)
table_dmsnr
sum(diag(table_dmsnr))/sum(table_dmsnr)
