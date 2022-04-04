rm(list=ls()) 
setwd("C:/Users/sevil/Desktop/INTL 601/HW2")
final_df = load(file='final_df.rda')

ls()

margin_idendity <- glm( attack_p22 ~ margin +  unemployment + VergiGelirlerinin‹lke›ÁindekiPay + Tar˝msal‹retimin‹lke›ÁindekiPay˝ + Kurddiff + literacy ,
                     data=dataNewC,
                     family = gaussian)

ls()
summary(margin_idendity)
#lets get a tex table
library(stargazer)
stargazer(margin_idendity)

#we might prefer Word or HTML
library(jtools)
export_summs(margin_idendity, to.file = 'docx')

#exponentiated coefficients
export_summs(margin_idendity, exp = T)

#plot summarization
plot_summs(margin_idendity)

#exponentiate
plot_summs(margin_idendity, exp = T)

#plot predictions

#let's get predicted probabilities with SEs
probsev = predict(margin_idendity, type = 'response', se.fit = T)
#add uncertainty
lowers = probsev$fit - 1.96*probsev$se.fit
uppers = probsev$fit + 1.96*probsev$se.fit

newDataSev = na.omit(dataNewC[,c('attack_p22','margin','unemployment','VergiGelirlerinin‹lke›ÁindekiPay' ,'Tar˝msal‹retimin‹lke›ÁindekiPay˝' ,'Kurddiff' ,'literacy')])

plot(probsev$fit ~ newDataSev$margin, pch = 18)
segments(x0 = newDataSev$attack_p22, x1 = newDataSev$attack_p22,
         y0 = lowers, y1 = uppers)

#very ugly, and not that meaningful
#instead, take the mean of other variables, and allow attack_p22 to vary
dataToPlotSev = data.frame('attack_p22' = seq(min(newDataSev$attack_p22), max(newDataSev$attack_p22), length.out = 1000),
                        'unemployment' = mean(newDataSev$unemployment),
                        'literacy' = mean(newDataSev$literacy),
                        'VergiGelirlerinin‹lke›ÁindekiPay' = mean(newDataSev$VergiGelirlerinin‹lke›ÁindekiPay))
probsev = predict(margin_idendity, newDataSev = dataToPlotSev, type = 'response', se.fit = T)
lowers = probsev$fit - 1.96*probsev$se.fit
uppers = probsev$fit + 1.96*probsev$se.fit
plot(probsev$fit ~ dataToPlotSev$margin, type = 'n') 
points(dataToPlotSev$margin, lowers, lty = 2, type = 'l')
points(dataToPlotSev$margin, uppers, lty = 2, type = 'l')
polygon(c(rev(dataToPlotSev$margin), dataToPlotSev$margin), c(rev(uppers), lowers), col = 'grey80', border = NA)
points(probsev$fit ~ dataToPlotSev$attack_p22, type = 'l')
#add a rug if you want
rug(newDataSev$attack_p22)

#in papers, you generally want an intuitive numerical explanation
#not that important for logits, cause of exp, but for GLMs in general
#we take the mean prediction, and subtract the sd
diff(predict(margin_idendity, newDataSev = data.frame(
  'attack_p22' = c(mean(newDataSev$attack_p22), mean(newDataSev$attack_p22) - sd(newDataSev$attack_p22)),
  'unemployment' = mean(newDataSev$unemployment),
  'literacy' = mean(newDataSev$literacy),
  'VergiGelirlerinin‹lke›ÁindekiPay' = mean(newDataSev$VergiGelirlerinin‹lke›ÁindekiPay)
), type = 'response'))
#-0.1248939

#or we might report the max to min
diff(predict(margin_idendity, newDataSev = data.frame(
  'attack_p22' = c(max(newDataSev$attack_p22), min(newDataSev$attack_p22)),
  'unemployment' = mean(newDataSev$unemployment),
  'literacy' = mean(newDataSev$literacy),
  'VergiGelirlerinin‹lke›ÁindekiPay' = mean(newDataSev$VergiGelirlerinin‹lke›ÁindekiPay)
), type = 'response'))
#-0.438561

#now matched cases
library(MatchIt)
match.out = matchit(I(attack_p22 > mean(attack_p22)) ~ 
                                 unemployment + literacy + VergiGelirlerinin‹lke›ÁindekiPay,
        data = newDataSev,
        method = 'nearest', distance = 'mahalanobis',
        replace = T)
matches = as.numeric(match.out$match.matrix)
matches2 = as.numeric(row.names(match.out$match.matrix))
mean(dataNewC[matches2, 'margin'] - dataNewC[matches, 'margin'])
dataNewC[matches2[1], 'margin'] - dataNewC[matches[1], 'margin'] #way more meaningful when it's not binary

#now we compare to the null model to check for explanatory power
margin_idendityNull <- glm(margin ~ unemployment + literacy + VergiGelirlerinin‹lke›ÁindekiPay, 
                 data=dataNewC,
                 family = binomial)

#analysis of variance
#we use a chi-squared test here
anova(margin_idendity, margin_idendityNull, test = 'Chisq')
#highly significant

#two models in one call
stargazer(margin_idendity, margin_idendityNull)






con_logit <- glm(margin ~ nkill_n_p22 + unemployment + literacy + VergiGelirlerinin‹lke›ÁindekiPay, 
                 data=dataNewC,
                 family = binomial)
summary(con_logit)

anova(margin_idendity, con_logit)
#model 1 is better

binary_err_rate = function(mod.formula, data, seed = 1234, train = .8, ndraws = 1000){
  set.seed(seed)
  MSE = c()
  for(i in 1:ndraws){ #can parallelize if extra cores are available
    samp = sample(1:nrow(data), nrow(data)*train)
    train_data = data[samp,]
    test_data = data[-samp,]
    mod = glm(mod.formula, data = train_data,   
              family = binomial(link = 'logit'))
    probs = predict(mod, test_data, type = 'response')
    MSE = c(MSE, mean((probs - (as.numeric(test_data[, as.character(mod.formula[2])]) - 1))^2, na.rm = T))
  }
  return(mean(MSE))
}

binary_err_rate(mod.formula = margin_idendity$formula, data = dataNewC)
#1.233417
binary_err_rate(mod.formula = con_logit$formula, data = dataNewC)
#1.0431
#model 1 is better

#now running the ordered logit regression on success_p2 score
library(MASS)
pop_olr <- polr(as.factor(success_p2) ~ attack_p22 + unemployment + literacy + VergiGelirlerinin‹lke›ÁindekiPay, 
                data=dataNewC)
summary(pop_olr)

con_olr <- polr(as.factor(success_p2) ~ groupcon + unemployment + literacy + VergiGelirlerinin‹lke›ÁindekiPay, 
                data=final_df)
summary(con_olr)

anova(pop_olr, con_olr)
#model 2 is better

#again, compare to null

pop_olrNull <- polr(as.factor(success_p2) ~ unemployment + literacy + VergiGelirlerinin‹lke›ÁindekiPay, 
                data=final_df)
anova(pop_olr, pop_olrNull)
#highly significant


#plot the results of a polr
newDataSev = na.omit(final_df[, c('success_p2', 'attack_p22', 'unemployment', 'literacy', 'VergiGelirlerinin‹lke›ÁindekiPay')])
probs = as.data.frame(predict(pop_olr, newDataSev, se.fit = TRUE, type = 'probs'))
plot(0:8, probs[1,], type = 'b', pch = 18)
for(i in 2:nrow(newDataSev)) points(0:8, probs[i,], type = 'b', pch = 18)
#now work with the same as above to make a more meaningful plot




polr_toMax = function(expl, data, seed = 1234, train = .8, ndraws = 1000){
  set.seed(seed)
  toMax = c()
  require(MASS)
  for(i in 1:ndraws){ #can parallelize if extra cores are available
    samp = sample(1:nrow(data), nrow(data)*train)
    train_data = data[samp,]
    test_data = data[-samp,]
    mod = polr(as.factor(success_p2) ~ get(expl) + unemployment + literacy + VergiGelirlerinin‹lke›ÁindekiPay, 
               data=train_data)
    probs = predict(mod, test_data, type = 'probs')
    probSum = sum(unlist(lapply(1:nrow(test_data), function(x){
      probs[x, test_data[x, 'success_p2']]
    })), na.rm = T)
    toMax = c(toMax, probSum)
  }
  return(mean(toMax))
}

polr_toMax(expl = 'attack_p22', data = final_df)
#104.8781
polr_toMax(expl = 'groupcon', data = final_df)
#102.7683

