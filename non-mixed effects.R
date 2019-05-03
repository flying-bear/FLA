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

# eng %>%
#   filter(childname == 'Anne') %>% 
#   filter(pronoun == '1pl') -> an
# 
# lm(share~age, data=an)

eng %>% 
  ggplot(aes(age, share, color = pronoun))+
  geom_smooth(method='loess')+
  labs(x = 'age in months', y = 'percent of pronouns in words uttered', 
       title = 'English pronouns, percent by age approximated by a smooth model')
  
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
  ggplot(aes(age, share, color = pronoun))+
  geom_smooth(method='loess')+
  labs(x = 'age in months', y = 'percent of pronouns in words uttered', 
       title = 'French pronouns, percent by age approximated by a smooth model')

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

eng_rate <- data.frame(childname= 'name',
                       pronoun='pronoun', 
                       intercept=0,
                       rate=0, 
                       stringsAsFactors=FALSE)


for (name in unique(eng$childname)) (for (pr in unique(eng$pronoun)) eng_rate <- rbind(eng_rate, c(name, pr, linear(eng, name, pr))))
eng_rate <- eng_rate[-c(1), ]
eng_rate %>% 
  filter(abs(as.numeric(rate)) < 10) %>% 
  mutate(rate = as.numeric(rate)) %>% 
  mutate(intercept = as.numeric(intercept)) -> eng_rate


linear <- function(lang, name, pr) {tryCatch({lm(age~share, filter(lang, pronoun == pr, childname == name))$coefficients}, error = function(err){return(c(NA, NA))})}
fra_rate <- data.frame(childname= 'name',
                       pronoun='pronoun', 
                       intercept=0,
                       rate=0, 
                       stringsAsFactors=FALSE)

for (name in unique(fra$childname)) (for (pr in unique(fra$pronoun)) fra_rate <- rbind(fra_rate, c(name, pr, linear(fra, name, pr))))
fra_rate <- fra_rate[-c(1), ]
fra_rate %>% 
  filter(abs(as.numeric(rate)) < 10) %>% 
  mutate(rate = as.numeric(rate)) %>% 
  mutate(intercept = as.numeric(intercept)) -> fra_rate
