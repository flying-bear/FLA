library(tidyverse)
library(DataCombine)

eng <- read_csv('merged_eng.csv')
fra <- read_csv('merged_fra.csv')

eng %>% 
  gather('age', 'share', 3:14) %>% 
  mutate(share = share * 100) %>% 
  mutate(age = as.numeric(age)) %>%
  mutate(language = 'eng') %>% 
  na.omit() -> eng

eng %>% 
  filter(pronoun != '3' & pronoun != '2' ) %>% 
  ggplot(aes(age, share, color = pronoun))+
  geom_smooth(method='lm')+
  labs(x = 'age in months', y = 'percent of pronouns in words uttered', 
       title = 'English pronouns, percent by age approximated by a linear model')
  
m_eng <- summary(lm(share~age+pronoun, data = eng))
m_eng
# Call:
#   lm(formula = share ~ age + pronoun, data = eng)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -8.7264 -1.0419 -0.1403  0.8776 18.7329 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) -4.39646    0.48668  -9.034  < 2e-16 ***
#   age          0.16356    0.01527  10.713  < 2e-16 ***
#   pronoun1sg   3.46786    0.21103  16.433  < 2e-16 ***
#   pronoun2     3.15156    0.21035  14.983  < 2e-16 ***
#   pronoun3     8.35378    0.20880  40.009  < 2e-16 ***
#   pronoun3pl   0.76429    0.21777   3.510 0.000457 ***
#   pronoun3sgF -0.14129    0.23745  -0.595 0.551870    
#   pronoun3sgM  0.43420    0.21953   1.978 0.048055 *  
#   pronoun3sgN  6.77206    0.20927  32.360  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.467 on 2367 degrees of freedom
# Multiple R-squared:   0.61,	Adjusted R-squared:  0.6087 
# F-statistic: 462.8 on 8 and 2367 DF,  p-value: < 2.2e-16
qqnorm(m_eng$residuals) #nice

fra %>% 
  gather('age', 'share', 3:14) %>% 
  mutate(share = share * 100) %>% 
  mutate(age = as.numeric(age)) %>% 
  mutate(language = 'fra') %>% 
  na.omit() -> fra
  
fra %>% 
  filter(pronoun != '3' & pronoun != '2' ) %>% 
  ggplot(aes(age, share, color = pronoun))+
  geom_smooth(method='lm')+
  labs(x = 'age in months', y = 'percent of pronouns in words uttered', 
       title = 'French pronouns, percent by age approximated by a linear model')

m_fra <- summary(lm(share~age+pronoun, data = fra))
m_fra
# Call:
#   lm(formula = share ~ age + pronoun, data = fra)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5.9451 -1.7167 -0.4800  0.6504 29.1555 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) -3.65181    0.97611  -3.741 0.000189 ***
#   age          0.12398    0.02546   4.869 1.22e-06 ***
#   pronoun1sg   1.86892    0.62211   3.004 0.002700 ** 
#   pronoun2     6.69679    0.60838  11.008  < 2e-16 ***
#   pronoun2pl  -0.04686    0.93445  -0.050 0.960012    
#   pronoun2sg   6.69082    0.60838  10.998  < 2e-16 ***
#   pronoun3     5.00932    0.61036   8.207 4.29e-16 ***
#   pronoun3pl   1.52725    0.61765   2.473 0.013503 *  
#   pronoun3sg   3.80492    0.61122   6.225 5.98e-10 ***
#   pronounon    0.35803    0.64797   0.553 0.580648    
#   pronouny     0.68620    0.63107   1.087 0.277027    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.385 on 1787 degrees of freedom
# Multiple R-squared:  0.3412,	Adjusted R-squared:  0.3375 
# F-statistic: 92.57 on 10 and 1787 DF,  p-value: < 2.2e-16
qqnorm(m_fra$residuals) # decent





comparable <-  c('1sg', '1pl', '2', '3')

fra %>% 
  filter(pronoun %in% comparable) -> fra_merge
  
eng %>% 
  filter(pronoun %in% comparable) -> eng_merge
  
eng_fra <- rbind(fra_merge, eng_merge)

eng_fra %>% 
  ggplot(aes(age, share, color = pronoun, linetype = language))+
  geom_smooth(method='lm')+
  labs(x = 'age in months', y = 'percent of pronouns in words uttered', 
       title = 'comparion of English and French pronouns, percent by age approximated by a linear model')

eng_fra %>% 
  mutate(eng = ifelse(language == 'eng', 1, 0)) %>%
  mutate(fra = ifelse(language == 'fra', 1, 0)) %>%
  select(-language) -> eng_fra_for_modeling

m1 <- summary(lm(share~age+pronoun+eng, data = eng_fra_for_modeling))
m1
# 
# Call:
#   lm(formula = share ~ age + pronoun + eng, data = eng_fra_for_modeling)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -7.738 -2.138 -0.397  1.332 31.773 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)  -6.6613     0.7705  -8.645  < 2e-16 ***
#   age           0.2121     0.0235   9.026  < 2e-16 ***
#   pronoun1sg    2.9901     0.2615  11.433  < 2e-16 ***
#   pronoun2      4.8787     0.2580  18.911  < 2e-16 ***
#   pronoun3      7.0841     0.2573  27.529  < 2e-16 ***
#   eng           0.8456     0.1631   5.183  2.4e-07 ***
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.441 on 2009 degrees of freedom
# Multiple R-squared:  0.3169,	Adjusted R-squared:  0.3152 
# F-statistic: 186.4 on 5 and 2009 DF,  p-value: < 2.2e-16

qqnorm(m1$residuals)





linear <- function(lang, name, pr) {tryCatch({lm(share~age, filter(lang, pronoun == pr & childname == name))$coefficients}, error = function(err){return(c(NA, NA))})}
# eng %>%
#   filter(pronoun == '1pl' & childname == 'Anne') -> an
# 
# eng %>%
#   filter(pronoun == '1pl' & childname == 'Anne') %>% 
#   ggplot(aes(age, share))+
#   geom_smooth(method='lm')
# 
# lm(share~age, data = filter(eng, pronoun == '1pl' & childname == 'Anne'))
# linear_bad(eng, 'Anne', '1pl')





eng_rate <- data.frame(childname= 'name',
                       pronoun='pronoun', 
                       intercept=0,
                       rate=0, 
                       stringsAsFactors=FALSE)

for (name in unique(eng$childname)) (for (pr in unique(eng$pronoun)) eng_rate <- rbind(eng_rate, c(name, pr, linear(eng, name, pr))))
eng_rate <- eng_rate[-c(1), ]
eng_rate %>% 
  mutate(rate = as.numeric(rate)) %>% 
  mutate(intercept = as.numeric(intercept) + rate*24) -> eng_rate

eng_rate %>% 
  mutate(language = 'eng') -> eng_rate


fra_rate <- data.frame(childname= 'name',
                       pronoun='pronoun', 
                       intercept=0,
                       rate=0, 
                       stringsAsFactors=FALSE)

for (name in unique(fra$childname)) (for (pr in unique(fra$pronoun)) fra_rate <- rbind(fra_rate, c(name, pr, linear(fra, name, pr))))
fra_rate <- fra_rate[-c(1), ]
fra_rate %>% 
  mutate(rate = as.numeric(rate)) %>% 
  mutate(intercept = as.numeric(intercept) + rate*24) -> fra_rate

fra_rate %>% 
  mutate(language = 'fra') -> fra_rate

fra_eng_rate <- rbind(fra_rate, eng_rate)

### general

t.test(rate ~ language, data = fra_eng_rate)
# Welch Two Sample t-test
# 
# data:  rate by language
# t = 0.64509, df = 331.49, p-value = 0.5193
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.08748142  0.17285385
# sample estimates:
#   mean in group eng mean in group fra 
# 0.14027806        0.09759185 

t.test(intercept ~ language, data = fra_eng_rate)
# Welch Two Sample t-test
# 
# data:  intercept by language
# t = -1.7587, df = 339.3, p-value = 0.07954
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -2.1233655  0.1187243
# sample estimates:
#   mean in group eng mean in group fra 
# 2.799455          3.801776 

### 3

t.test(intercept ~ language, data = filter(fra_eng_rate, pronoun == '3'), alternative='g')
# ***************************************************************************************
# Welch Two Sample t-test
# 
# data:  intercept by language
# t = 3.7448, df = 56.484, p-value = 0.0002127
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#   2.868986      Inf
# sample estimates:
#   mean in group eng mean in group fra 
# 6.720180          1.536209 

t.test(rate ~ language, data = filter(fra_eng_rate, pronoun == '3'))
# Welch Two Sample t-test
# 
# data:  rate by language
# t = -1.7594, df = 54.777, p-value = 0.08409
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.71733634  0.04665404
# sample estimates:
#   mean in group eng mean in group fra 
# 0.2718536         0.6071947

###2

t.test(intercept ~ language, data = filter(fra_eng_rate, pronoun == '2'), alternative='l')
# ****************************************************************************************
# Welch Two Sample t-test
# 
# data:  intercept by language
# t = -3.7212, df = 44.575, p-value = 0.0002766
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval:
#   -Inf -3.780902
# sample estimates:
#   mean in group eng mean in group fra 
# 2.503035          9.394928 

t.test(rate ~ language, data = filter(fra_eng_rate, pronoun == '2'))
# Welch Two Sample t-test
# 
# data:  rate by language
# t = 1.7429, df = 54.157, p-value = 0.08702
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.05827692  0.83423373
# sample estimates:
#   mean in group eng mean in group fra 
# 0.1499797        -0.2379987 

### 1pl

t.test(intercept ~ language, data = filter(fra_eng_rate, pronoun == '1pl'))
# Welch Two Sample t-test
# 
# data:  intercept by language
# t = 1.0574, df = 18.218, p-value = 0.3042
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.1657643  0.5022704
# sample estimates:
#   mean in group eng mean in group fra 
# 0.4393890         0.2711359 

t.test(rate ~ language, data = filter(fra_eng_rate, pronoun == '1pl'))
# Welch Two Sample t-test
# 
# data:  rate by language
# t = 1.5251, df = 18.799, p-value = 0.1439
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.01134734  0.07212843
# sample estimates:
#   mean in group eng mean in group fra 
# 0.01808877       -0.01230178 

### 1sg

t.test(intercept ~ language, data = filter(fra_eng_rate, pronoun == '1sg'), alternative = 'g')
# *************************************************
# Welch Two Sample t-test
# 
# data:  intercept by language
# t = 1.7077, df = 36.115, p-value = 0.04813
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#   0.01564129        Inf
# sample estimates:
#   mean in group eng mean in group fra 
# 2.631921          1.268000 

t.test(rate ~ language, data = filter(fra_eng_rate, pronoun == '1sg'))
# Welch Two Sample t-test
# 
# data:  rate by language
# t = 1.3936, df = 39.234, p-value = 0.1713
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.06005819  0.32629462
# sample estimates:
#   mean in group eng mean in group fra 
# 0.2736973         0.1405791 


### fra on
t.test(intercept ~ pronoun, data = filter(fra_rate, pronoun == '1pl' | pronoun == 'on'))
# Welch Two Sample t-test
# 
# data:  intercept by pronoun
# t = -0.85038, df = 17.528, p-value = 0.4066
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -1.6649485  0.7068019
# sample estimates:
#   mean in group 1pl  mean in group on 
# 0.2711359         0.7502093 

### fra num

fra_rate %>% 
  mutate(pronoun = ifelse(pronoun == 'y'|pronoun=='on'|pronoun == '1sg'|pronoun == '2sg'|pronoun == '3sg','sg', 'pl' )) -> fra_rate_num

t.test(rate ~ pronoun, data = fra_rate_num)  
# Welch Two Sample t-test
# 
# data:  rate by pronoun
# t = 0.21451, df = 254.15, p-value = 0.8303
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.2237758  0.2784834
# sample estimates:
#   mean in group pl mean in group sg 
# 0.11246674       0.08511292 

###eng num
eng_rate %>% 
  mutate(pronoun = ifelse(pronoun == '1sg'|pronoun == '2sg'|pronoun=='3sgF'|pronoun=='3sgN'|pronoun == '3sgM','sg', 'pl')) -> eng_rate_num

t.test(rate ~ pronoun, data = eng_rate_num)  
# Welch Two Sample t-test
# 
# data:  rate by pronoun
# t = 0.49558, df = 340.94, p-value = 0.6205
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.0603622  0.1010240
# sample estimates:
#   mean in group pl mean in group sg 
# 0.1503894        0.1300585 

