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

m_eng_mixed <- lmer(share~age+pronoun+(1|childname), data = eng)
summary(m_eng_mixed)
# ******************************************************************
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: share ~ age + pronoun + (1 | childname)
# Data: eng
# 
# REML criterion at convergence: 10584.1
# 
# Scaled residuals: 
# Min      1Q  Median      3Q     Max 
# -4.2346 -0.5630 -0.0766  0.5046  7.7828 
# 
# Random effects:
# Groups    Name        Variance Std.Dev.
# childname (Intercept) 1.851    1.361   
# Residual              4.701    2.168   
# Number of obs: 2376, groups:  childname, 57
# 
# Fixed effects:
# Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)   -5.08212    0.48210 1265.71026 -10.542  < 2e-16 ***
# age            0.17550    0.01397 2338.53241  12.563  < 2e-16 ***
# pronoun1sg     3.66408    0.18663 2312.47714  19.633  < 2e-16 ***
# pronoun2       3.40052    0.18618 2312.78946  18.265  < 2e-16 ***
# pronoun3       8.61285    0.18505 2314.08871  46.544  < 2e-16 ***
# pronoun3pl     0.92424    0.19223 2310.52431   4.808 1.62e-06 ***
# pronoun3sgF   -0.04314    0.20971 2310.85470  -0.206  0.83705    
# pronoun3sgM    0.55882    0.19374 2310.34205   2.884  0.00396 ** 
# pronoun3sgN    7.03132    0.18539 2313.70838  37.928  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
# (Intr) age    prnn1s pronn2 pronn3 prnn3p prnn3F prnn3M
# age         -0.874                                                 
# pronoun1sg  -0.268  0.039                                          
# pronoun2    -0.277  0.049  0.597                                   
# pronoun3    -0.282  0.051  0.602  0.606                            
# pronoun3pl  -0.249  0.030  0.573  0.576  0.580                     
# pronoun3sgF -0.206  0.005  0.522  0.523  0.527  0.506              
# pronoun3sgM -0.250  0.034  0.569  0.572  0.575  0.552  0.502       
# pronoun3sgN -0.281  0.051  0.601  0.604  0.609  0.578  0.526  0.574
qqnorm(summary(m_eng_mixed)$residuals)
shapiro.test(summary(m_eng_mixed)$residuals)
# Shapiro-Wilk normality test
# data:  summary(m_eng_mixed)$residuals
# W = 0.96289, p-value < 2.2e-16


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

m_fra_mixed <- lmer(share~age+pronoun+(1|childname), data = fra)
summary(m_fra_mixed)
# ********************************
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: share ~ age + pronoun + (1 | childname)
# Data: fra
# 
# REML criterion at convergence: 9169.7
# 
# Scaled residuals: 
# Min      1Q  Median      3Q     Max 
# -2.7092 -0.5164 -0.0798  0.3095  9.3588 
# 
# Random effects:
# Groups    Name        Variance Std.Dev.
# childname (Intercept) 4.547    2.132   
# Residual              8.728    2.954   
# Number of obs: 1798, groups:  childname, 85
# 
# Fixed effects:
# Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)    0.17221    0.96075 1672.79865   0.179   0.8578    
# age            0.05507    0.02433 1780.52266   2.263   0.0237 *  
# pronoun1sg     1.19754    0.54792 1694.92767   2.186   0.0290 *  
# pronoun2       5.83184    0.53818 1697.42044  10.836  < 2e-16 ***
# pronoun2pl    -0.06489    0.81809 1689.21688  -0.079   0.9368    
# pronoun2sg     5.82587    0.53818 1697.42044  10.825  < 2e-16 ***
# pronoun3       4.11351    0.53918 1696.31752   7.629 3.91e-14 ***
# pronoun3pl     0.87211    0.54431 1694.92982   1.602   0.1093    
# pronoun3sg     2.91913    0.53975 1696.15864   5.408 7.27e-08 ***
# pronounon     -0.34284    0.56955 1693.44610  -0.602   0.5473    
# pronouny      -0.25282    0.55612 1695.86102  -0.455   0.6494    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
# (Intr) age    prnn1s pronn2 prnn2p prnn2s pronn3 prnn3p prnn3s pronnn
# age        -0.808                                                               
# pronoun1sg -0.513  0.033                                                        
# pronoun2   -0.545  0.055  0.866                                                 
# pronoun2pl -0.315 -0.005  0.559  0.569                                          
# pronoun2sg -0.545  0.055  0.866  0.889  0.569                                   
# pronoun3   -0.535  0.045  0.864  0.885  0.568  0.885                            
# pronoun3pl -0.520  0.037  0.854  0.873  0.563  0.873  0.871                     
# pronoun3sg -0.533  0.043  0.863  0.884  0.567  0.884  0.881  0.870              
# pronounon  -0.470  0.006  0.812  0.827  0.537  0.827  0.825  0.816  0.824       
# pronouny   -0.497  0.020  0.835  0.852  0.550  0.852  0.850  0.840  0.849  0.801
qqnorm(summary(m_fra_mixed)$residuals) #meh
shapiro.test(summary(m_fra_mixed)$residuals)
# Shapiro-Wilk normality test
# data:  summary(m_fra_mixed)$residuals
# W = 0.82918, p-value < 2.2e-16



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

m1 <- summary(lmer(share~age+pronoun+eng+(1|childname), data = eng_fra_for_modeling))
m1
# ***************************************************************
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: share ~ age + pronoun + eng + (1 | childname)
# Data: eng_fra_for_modeling
# 
# REML criterion at convergence: 10532.7
# 
# Scaled residuals: 
# Min      1Q  Median      3Q     Max 
# -2.4463 -0.5765 -0.0952  0.4031  9.8065 
# 
# Random effects:
# Groups    Name        Variance Std.Dev.
# childname (Intercept)  2.289   1.513   
# Residual              10.010   3.164   
# Number of obs: 2015, groups:  childname, 139
# 
# Fixed effects:
# Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)   -5.57096    0.77375 1930.10483  -7.200 8.59e-13 ***
# age            0.19851    0.02256 1984.94915   8.798  < 2e-16 ***
# pronoun1sg     3.06799    0.24315 1900.22450  12.618  < 2e-16 ***
# pronoun2       4.94066    0.24084 1907.30403  20.515  < 2e-16 ***
# pronoun3       7.14703    0.24021 1907.59076  29.754  < 2e-16 ***
# eng            0.07223    0.29516  180.84146   0.245    0.807    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
# (Intr) age    prnn1s pronn2 pronn3
# age        -0.919                            
# pronoun1sg -0.276  0.048                     
# pronoun2   -0.319  0.075  0.692              
# pronoun3   -0.309  0.067  0.693  0.715       
# eng        -0.313  0.101  0.076  0.119  0.109

qqnorm(m1$residuals) # fine
shapiro.test(m1$residuals)
# Shapiro-Wilk normality test
# data:  m1$residuals
# W = 0.89656, p-value < 2.2e-16
