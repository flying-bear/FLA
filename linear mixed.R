library(lme4)
library(lmerTest)
library(tidyverse)


pron_eng %>% 
  filter(participant == 'CHI') %>%
  mutate(pronoun = ifelse(pronoun=='1sg'|pronoun=='1pl','1', pronoun)) %>%
  mutate(pronoun = ifelse(pronoun=='3sgF'|pronoun=='3sgN'|pronoun=='3sgM'|pronoun=='3pl','3', pronoun)) %>% 
  filter(pronoun == '2' | pronoun == '1' | pronoun == '3') %>% 
  mutate(share == share*100) %>% 
  select(-path) -> chi_eng

pron_eng %>% 
  mutate(age = floor(age)) %>% 
  spread(age, share) -> chi_eng

pron_fra %>% 
  filter(participant == 'CHI') %>%
  mutate(pronoun = ifelse(pronoun=='1sg'|pronoun=='1pl','1', pronoun)) %>% 
  filter(pronoun == '2' | pronoun == '1' | pronoun == '3') %>% 
  mutate(share = share*100) %>% 
  mutate(pronoun = relevel(as.factor(pronoun), '1',)) %>% 
  select(-path) -> chi_fra

pron_eng %>% 
  mutate(pronoun = ifelse(pronoun == '1sg'|pronoun == '2sg'|pronoun=='3sgF'|pronoun=='3sgN'|pronoun == '3sgM','sg', 'pl')) %>% 
  mutate(share = share*100) -> chi_eng

pron_fra %>% 
  filter(participant == 'CHI') %>%
  filter(pronoun != 'on') %>% 
  filter(pronoun != 'y') %>% 
  filter(pronoun != '2') %>% 
  filter(pronoun != '3') %>% 
  mutate(share = share*100) %>% 
  mutate(pronoun = relevel(as.factor(pronoun), '1sg',)) %>% 
  select(-path) -> chi_fra

pron_fra %>% 
  filter(participant == 'CHI') %>%
  mutate(share = share*100) %>%
  mutate(pronoun = ifelse(pronoun == 'y'|pronoun=='on'|pronoun == '1sg'|pronoun == '2sg'|pronoun == '3sg','sg', 'pl')) -> chi_fra

fit_eng <- lmer(share ~ age*pronoun + (1|childname), data = chi_eng)
summary(fit_eng)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: share ~ age + pronoun + (1 | childname)
# Data: chi_eng
# 
# REML criterion at convergence: -55539.2
# 
# Scaled residuals: 
# Min      1Q  Median      3Q     Max 
# -3.6131 -0.4838 -0.0757  0.3517 14.2507 
# 
# Random effects:
# Groups    Name        Variance  Std.Dev.
# childname (Intercept) 0.0000430 0.006558
# Residual              0.0004841 0.022003
# Number of obs: 11627, groups:  childname, 57
# 
# Fixed effects:
# Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept) -3.613e-02  2.187e-03  1.189e+03 -16.526  < 2e-16 ***
# age          1.353e-03  6.331e-05  1.160e+04  21.364  < 2e-16 ***
# pronoun1sg   2.584e-02  7.635e-04  1.157e+04  33.839  < 2e-16 ***
# pronoun2     3.467e-02  7.635e-04  1.157e+04  45.411  < 2e-16 ***
# pronoun3pl   4.423e-03  7.635e-04  1.157e+04   5.794 7.07e-09 ***
# pronoun3sgF -1.788e-03  7.635e-04  1.157e+04  -2.342 0.019199 *  
# pronoun3sgM  2.567e-03  7.635e-04  1.157e+04   3.362 0.000776 ***
# pronoun3sgN  6.409e-02  7.635e-04  1.157e+04  83.941  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
# (Intr) age    prnn1s pronn2 prnn3p prnn3F prnn3M
# age         -0.865                                          
# pronoun1sg  -0.175  0.000                                   
# pronoun2    -0.175  0.000  0.500                            
# pronoun3pl  -0.175  0.000  0.500  0.500                     
# pronoun3sgF -0.175  0.000  0.500  0.500  0.500              
# pronoun3sgM -0.175  0.000  0.500  0.500  0.500  0.500       
# pronoun3sgN -0.175  0.000  0.500  0.500  0.500  0.500  0.500

fit_fra <- fit <- lmer(share ~ age*pronoun + (1|childname), data = chi_fra)
summary(fit_fra)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: share ~ age + pronoun + (1 | childname)
# Data: chi_fra
# 
# REML criterion at convergence: -38188.5
# 
# Scaled residuals: 
# Min      1Q  Median      3Q     Max 
# -3.2629 -0.5439 -0.0678  0.3392 12.4574 
# 
# Random effects:
# Groups    Name        Variance  Std.Dev.
# childname (Intercept) 0.0001080 0.01039 
# Residual              0.0006109 0.02472 
# Number of obs: 8450, groups:  childname, 143
# 
# Fixed effects:
# Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept) -2.160e-02  2.298e-03  1.977e+03  -9.397  < 2e-16 ***
# age          9.696e-04  6.089e-05  7.318e+03  15.924  < 2e-16 ***
# pronoun1sg   1.874e-02  1.202e-03  8.306e+03  15.587  < 2e-16 ***
# pronoun2     5.840e-02  1.202e-03  8.306e+03  48.568  < 2e-16 ***
# pronoun2pl  -4.155e-05  1.202e-03  8.306e+03  -0.035  0.97243    
# pronoun2sg   5.809e-02  1.202e-03  8.306e+03  48.312  < 2e-16 ***
# pronoun3     5.009e-02  1.202e-03  8.306e+03  41.657  < 2e-16 ***
# pronoun3pl   1.287e-02  1.202e-03  8.306e+03  10.700  < 2e-16 ***
# pronoun3sg   3.688e-02  1.202e-03  8.306e+03  30.667  < 2e-16 ***
# pronounon    3.911e-03  1.202e-03  8.306e+03   3.252  0.00115 ** 
# pronouny     5.117e-03  1.202e-03  8.306e+03   4.255 2.11e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
# (Intr) age    prnn1s pronn2 prnn2p prnn2s pronn3 prnn3p prnn3s pronnn
# age        -0.827                                                               
# pronoun1sg -0.262  0.000                                                        
# pronoun2   -0.262  0.000  0.500                                                 
# pronoun2pl -0.262  0.000  0.500  0.500                                          
# pronoun2sg -0.262  0.000  0.500  0.500  0.500                                   
# pronoun3   -0.262  0.000  0.500  0.500  0.500  0.500                            
# pronoun3pl -0.262  0.000  0.500  0.500  0.500  0.500  0.500                     
# pronoun3sg -0.262  0.000  0.500  0.500  0.500  0.500  0.500  0.500              
# pronounon  -0.262  0.000  0.500  0.500  0.500  0.500  0.500  0.500  0.500       
# pronouny   -0.262  0.000  0.500  0.500  0.500  0.500  0.500  0.500  0.500  0.500


qqnorm(lm(share ~ age + pronoun, data = chi_fra)$residuals)
qqnorm(lm(share ~ age + pronoun, data = chi_eng)$residuals)

summary(lm(share ~ age + pronoun, data = chi_fra))
