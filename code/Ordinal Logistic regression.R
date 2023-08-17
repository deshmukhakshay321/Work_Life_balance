library(readxl)
Data <- read_excel("C:/Users/HP/OneDrive/Desktop/Data.xlsx")
View(Data)

library(MASS)
library(ordinal)
library(rcompanion)
library(brant)


Y=Data[,1]
x1=as.factor(Data$`Employee Gender`)
x2=as.factor(Data$`Marital Status`)
x3=as.factor(Data$`Parental status`)
x4=Data$`Years of work experience ?`
x5=Data$`How many times have you switched the job till date?`
x6=as.factor(Data$`Are you satiesfied with the income you get according to the work you're doing?`)
x7=as.factor(Data$`How 3 do you think or worry about work(when you are not actually at work)?`)
x8=as.factor(Data$`Do you get time to de-stress yourself from your work schedule?`)
x9=as.factor(Data$`Do you get time to exercise ?`)

#Cumulative Link Mixed Models (CLMMs) make it possible to analyse ordinal response variables while allowing the use of random effects.

modelnull=clm(Y~1,data=Data,link="logit")
model1=clm(Y~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=Data,link="logit")

anova(modelnull,model1)
##Model is sigificantly better fit than null model
nagelkerke(fit = model1,null = modelnull)

summary(model1)


library(readxl)
wlbg <- read_excel("wlbg.xlsx", sheet = "Sheet1")
View(wlbg)
data=wlbg;data
library(MASS)
library(ordinal)
library(rcompanion)
library(brant)

Y=as.factor(data$`Do you feel like you are able to balance your work and life.
            `)
x1=as.factor(data$`Employee Gender`)
x2=as.factor(data$`Marital Status`)
x3=as.factor(data$`parental Status`)
x4=data$`Years of work experience ?`
x5=as.factor(data$`Are you satisfied with the income you get according to the work load?  `)

x6=as.factor(data$`How often do you think or worry about work(when you are not actually at work)?`)
x7=as.factor(data$`Do you get time to destress yourself from your work schedule?`)
x8=as.factor(data$`Do you get time to exercise ?`)


#Cumulative Link Mixed Models (CLMMs) make it possible to analyse ordinal response variables while allowing the use of random effects.

modelnull=clm(Y~1,data=data,link="logit")
model1=clm(Y~x1+x2+x3+x4+x5+x6+x7+x8,data=Data,link="logit")

anova(modelnull,model1)
##Model is sigificantly better fit than null model
nagelkerke(fit = model1,null = modelnull)

summary(model1)


