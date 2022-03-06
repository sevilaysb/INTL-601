setwd('~/QRM/inclass/day1OLS/')

data = read.csv("C:/Users/sevil/Desktop/INTL 601/HW1/SEVÝLAY/willingness to coproduce/CoProductionDataEnglish.csv")
#explore
colnames(data)
head(data)
summary(data)

#check pragmatticLeg1 is correlated with MoralLeg1
mod = lm( PragmatticLeg1 ~  MoralLeg1, data = data )
summary(mod)

library(stargazer) #for making LaTex tables
stargazer(mod)
library(car)
qqPlot(mod) #check for error distribution - clearly not normal
plot(cooks.distance(mod)) #check for cook's influence - Cook's distance shows the influence of each observation on the fitted response values
par(mfrow = c(2,2)) #set up the plot to be 2x2 rows x columns
plot(mod) 
par(mfrow = c(1,1))
mod.null = lm(PragmatticLeg1 ~ 1, data = data[!is.na(data$MoralLeg1),])
anova(mod, mod.null) #check the model against the null (typically just controls)

mod2 = lm(PragmatticLeg1 ~ MoralLeg1*PoliticOrient, data = data)
summary(mod2)
library(margins)
cplot(mod2, x = 'PoliticOrient', what = 'effect', data = data)

mod3 = lm(PragmatticLeg1 ~ MoralLeg1*PoliticOrient + I(PoliticOrient > 5), data = data)
cplot(mod3, x = 'PoliticOrient', what = 'effect', data = data)
cplot(mod3, x = 'MoralLeg1', what = 'effect', data = data)
mod4 = lm(PragmatticLeg1 ~ MoralLeg1*PoliticOrient + CogLeg1, data = data)
cplot(mod4, x = 'polity2_', what = 'effect', data = data)
cplot(mod4, x = 'polity2_', what = 'prediction', data = data)

mod5 = lm(mpg ~ wt + I(wt^2), data = mtcars)
margins(mod5)
cplot(mod5, "wt", what = "prediction", main = "Predicted WillingnessToCoProduce, Given Weight")
cplot(mod5, "wt", what = "effect", main = "Average Marginal Effect of Weight")
mod6 = lm(mpg ~ hp * wt, data = mtcars)
persp(mod6, "wt", "hp", theta = c(45, 135, 225, 315), what = "effect")

