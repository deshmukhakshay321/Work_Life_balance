library(readxl)
wlbg <- read_excel("wlbg.xlsx", sheet = "Sheet1")
View(wlbg)
wlbg=wlbg;wlbg
library(MASS)
library(ordinal)
library(rcompanion)
Y=as.factor(wlbg$`Do you feel like you are able to balance your work and life.`)

Y=as.factor(wlbg$`Do you feel like you are able to balance your work and life.`)
x1=as.factor(wlbg$`Employee Gender`)
x2=as.factor(wlbg$`Marital Status`)
x3=as.factor(wlbg$`parental Status`)
x4=wlbg$`Years of work experience ?`


x6=as.factor(wlbg$`How often do you think or worry about work(when you are not actually at work)?`)
x7=as.factor(wlbg$`Do you get time to destress yourself from your work schedule`)
x8=as.factor(wlbg$`Do you get time to exercise ?`)


#Cumulative Link Mixed Models (CLMMs) make it possible to analyse ordinal response variables while allowing the use of random effects.

modelnull=clm(Y~1,data=wlbg,link="logit")
model1=clm(Y~x1+x2+x3+x4+x5+x6+x7+x8,data=wlbg,link="logit")

anova(modelnull,model1)
##Model is significantly better fit than null model
nagelkerke(fit = model1,null = modelnull)

summary(model1)


