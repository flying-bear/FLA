library(tidyverse)
library(lmerTest)

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
# linear(eng, 'Anne', '1pl')



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

qqnorm(eng_rate$rate)
shapiro.test(eng_rate$rate)
# Shapiro-Wilk normality test ###не нормальное
# data:  eng_rate$rate
# W = 0.82738, p-value < 2.2e-16

qqnorm(eng_rate$intercept)
shapiro.test(eng_rate$intercept)
# Shapiro-Wilk normality test
# data:  eng_rate$intercept
# W = 0.83961, p-value < 2.2e-16




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

qqnorm(fra_rate$rate)
shapiro.test(fra_rate$rate)
# Shapiro-Wilk normality test ###не нормальное
# data:  fra_rate$rate
# W = 0.81889, p-value < 2.2e-16

qqnorm(fra_rate$intercept)
shapiro.test(fra_rate$intercept)
# Shapiro-Wilk normality test
# data:  fra_rate$intercept
# W = 0.74291, p-value < 2.2e-16


fra_eng_rate <- rbind(fra_rate, eng_rate)
qqnorm(fra_eng_rate$rate)
shapiro.test(fra_eng_rate$rate)
# Shapiro-Wilk normality test ###не нормальное
# data:  fra_eng_rate$rate
# W = 0.74401, p-value < 2.2e-16

qqnorm(fra_eng_rate$intercept)
shapiro.test(fra_eng_rate$intercept)
# Shapiro-Wilk normality test
# data:  fra_eng_rate$intercept
# W = 0.70424, p-value < 2.2e-16




### general

wilcox.test(rate ~ language, data = fra_eng_rate)
# Wilcoxon rank sum test with continuity correction
# data:  rate by language
# W = 55245, p-value = 0.1144
# alternative hypothesis: true location shift is not equal to 0

wilcox.test(intercept ~ language, data = fra_eng_rate)
# Wilcoxon rank sum test with continuity correction
# data:  intercept by language
# W = 51694, p-value = 0.9388
# alternative hypothesis: true location shift is not equal to 0 


### 3

wilcox.test(intercept ~ language, data = filter(fra_eng_rate, pronoun == '3'), alternative='g')
# ***************************************************************************************
# Wilcoxon rank sum test with continuity correction
# data:  intercept by language
# W = 1719, p-value = 6.004e-06
# alternative hypothesis: true location shift is greater than 0 

wilcox.test(rate ~ language, data = filter(fra_eng_rate, pronoun == '3'))
# 	Wilcoxon rank sum test with continuity correction
# data:  rate by language
# W = 963, p-value = 0.2245
# alternative hypothesis: true location shift is not equal to 0


###2

wilcox.test(intercept ~ language, data = filter(fra_eng_rate, pronoun == '2'), alternative='l')
# ****************************************************************************************
# 	Wilcoxon rank sum test with continuity correction
# data:  intercept by language
# W = 566, p-value = 7.902e-06
# alternative hypothesis: true location shift is less than 0

wilcox.test(rate ~ language, data = filter(fra_eng_rate, pronoun == '2'))
# **************************************************
# Wilcoxon rank sum test with continuity correction
# data:  rate by language
# W = 1485, p-value = 0.01881
# alternative hypothesis: true location shift is not equal to 0 


### 1pl

wilcox.test(intercept ~ language, data = filter(fra_eng_rate, pronoun == '1pl'))
# 	Wilcoxon rank sum test
# data:  intercept by language
# W = 145, p-value = 0.9881
# alternative hypothesis: true location shift is not equal to 0 

wilcox.test(rate ~ language, data = filter(fra_eng_rate, pronoun == '1pl'))
# 	Wilcoxon rank sum test
# data:  rate by language
# W = 206, p-value = 0.06038
# alternative hypothesis: true location shift is not equal to 0 


### 1sg

wilcox.test(intercept ~ language, data = filter(fra_eng_rate, pronoun == '1sg'), alternative = 'g')
# *************************************************
# 	Wilcoxon rank sum test with continuity correction
# data:  intercept by language
# W = 967, p-value = 0.001061
# alternative hypothesis: true location shift is greater than 0 

wilcox.test(rate ~ language, data = filter(fra_eng_rate, pronoun == '1sg'))
# 	Wilcoxon rank sum test with continuity correction
# data:  rate by language
# W = 773, p-value = 0.3041
# alternative hypothesis: true location shift is not equal to 0 


### fra on
wilcox.test(intercept ~ pronoun, data = filter(fra_rate, pronoun == '1pl' | pronoun == 'on'))
# 	Wilcoxon rank sum test
# data:  intercept by pronoun
# W = 73, p-value = 0.7975
# alternative hypothesis: true location shift is not equal to 0 

### fra num

fra_rate %>% 
  mutate(pronoun = ifelse(pronoun == 'y'|pronoun=='on'|pronoun == '1sg'|pronoun == '2sg'|pronoun == '3sg','sg', 'pl' )) -> fra_rate_num

wilcox.test(rate ~ pronoun, data = fra_rate_num)  
# 	Wilcoxon rank sum test with continuity correction
# data:  rate by pronoun
# W = 9644.5, p-value = 0.6119
# alternative hypothesis: true location shift is not equal to 0 

###eng num
eng_rate %>% 
  mutate(pronoun = ifelse(pronoun == '1sg'|pronoun == '2sg'|pronoun=='3sgF'|pronoun=='3sgN'|pronoun == '3sgM','sg', 'pl')) -> eng_rate_num

wilcox.test(rate ~ pronoun, data = eng_rate_num)  
# Wilcoxon rank sum test with continuity correction
# data:  rate by pronoun
# W = 19440, p-value = 0.09329
# alternative hypothesis: true location shift is not equal to 0


