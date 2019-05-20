library(lmerTest)
library(tidyverse)

pron_fra %>% 
  filter(participant == 'CHI') %>%
  mutate(share = share*100) %>%
  # mutate(pronoun = ifelse(pronoun=='1sg'|pronoun=='1pl','1', pronoun)) %>% 
  # filter(pronoun == '2' | pronoun == '1' | pronoun == '3') %>% 
  mutate(language = 'fra') %>% 
  select(-path, -languge)-> comparison_fra

pron_eng %>% 
  filter(participant == 'CHI') %>%
  mutate(share = share*100) %>% 
  # mutate(pronoun = ifelse(pronoun=='1sg'|pronoun=='1pl','1', pronoun)) %>%
  # mutate(pronoun = ifelse(pronoun=='3sgF'|pronoun=='3sgN'|pronoun=='3sgM'|pronoun=='3pl','3', pronoun)) %>% 
  # filter(pronoun == '2' | pronoun == '1' | pronoun == '3') %>% 
  mutate(language = 'eng') %>%
  select(-path, -languge) -> comparison_eng

pron_eng_fra <- rbind(comparison_eng, comparison_fra)

summary(lmer(share~language*age+(1|childname), data = pron_eng_fra))
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: share ~ language * age + (1 | childname)
# Data: pron_eng_fra
# 
# REML criterion at convergence: 104696.6
# 
# Scaled residuals: 
# Min      1Q  Median      3Q     Max 
# -1.6815 -0.6366 -0.3035  0.3650 10.3069 
# 
# Random effects:
# Groups    Name        Variance Std.Dev.
# childname (Intercept)  0.6915  0.8315  
# Residual              10.6314  3.2606  
# Number of obs: 20077, groups:  childname, 196
# 
# Fixed effects:
# Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)     -1.687e+00  3.014e-01  6.940e+03  -5.597 2.26e-08 ***
# languagefra      1.674e+00  3.875e-01  8.936e+03   4.319 1.58e-05 ***
# age              1.342e-01  9.366e-03  2.007e+04  14.330  < 2e-16 ***
# languagefra:age -3.079e-02  1.216e-02  1.753e+04  -2.533   0.0113 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
# (Intr) lnggfr age   
# languagefra -0.753              
# age         -0.925  0.717       
# languagfr:g  0.705 -0.941 -0.769
  

comparison <- rbind(comparison_eng, comparison_fra)
fit_comp <- lmer(share ~ age*languge*pronoun + (1|childname), data = comparison)
summary(fit_comp)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: share ~ age * languge * pronoun + (1 | childname)
# Data: comparison
# 
# REML criterion at convergence: 75839.4
# 
# Scaled residuals: 
# Min      1Q  Median      3Q     Max 
# -2.7627 -0.6197 -0.2719  0.3209 10.2398 
# 
# Random effects:
# Groups    Name        Variance Std.Dev.
# childname (Intercept) 0.8868   0.9417  
# Residual              9.0038   3.0006  
# Number of obs: 15007, groups:  childname, 196
# 
# Fixed effects:
# Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)             -9.430e-01  4.762e-01  1.048e+04  -1.980   0.0477 *  
# age                      9.035e-02  1.529e-02  1.491e+04   5.910 3.49e-09 ***
# langugefra               3.729e-01  6.595e-01  1.125e+04   0.565   0.5718    
# pronoun2                -1.016e+01  7.779e-01  1.484e+04 -13.067  < 2e-16 ***
# pronoun3                 1.205e+00  5.500e-01  1.484e+04   2.191   0.0285 *  
# age:langugefra          -2.025e-02  2.130e-02  1.456e+04  -0.951   0.3418    
# age:pronoun2             4.129e-01  2.586e-02  1.484e+04  15.971  < 2e-16 ***
# age:pronoun3            -2.558e-02  1.828e-02  1.484e+04  -1.399   0.1618    
# langugefra:pronoun2      1.559e+01  1.061e+00  1.484e+04  14.695  < 2e-16 ***
# langugefra:pronoun3     -6.405e+00  9.075e-01  1.484e+04  -7.058 1.76e-12 ***
# age:langugefra:pronoun2 -4.308e-01  3.536e-02  1.484e+04 -12.184  < 2e-16 ***
# age:langugefra:pronoun3  3.403e-01  3.027e-02  1.484e+04  11.244  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
# (Intr) age    lnggfr pronn2 pronn3 ag:lng ag:pr2 ag:pr3 lngg:2 lngg:3 ag:l:2
# age         -0.958                                                                      
# langugefra  -0.711  0.691                                                               
# pronoun2    -0.544  0.560  0.393                                                        
# pronoun3    -0.770  0.792  0.556  0.471                                                 
# age:langgfr  0.684 -0.717 -0.965 -0.402 -0.568                                          
# age:pronon2  0.541 -0.564 -0.390 -0.993 -0.468  0.405                                   
# age:pronon3  0.765 -0.797 -0.552 -0.468 -0.993  0.572  0.471                            
# lnggfr:prn2  0.399 -0.410 -0.536 -0.733 -0.346  0.547  0.728  0.343                     
# lnggfr:prn3  0.467 -0.480 -0.627 -0.286 -0.606  0.640  0.284  0.602  0.390              
# ag:lnggfr:2 -0.395  0.412  0.531  0.726  0.342 -0.553 -0.731 -0.345 -0.989 -0.386       
# ag:lnggfr:3 -0.462  0.482  0.620  0.283  0.600 -0.646 -0.285 -0.604 -0.385 -0.988  0.389


pron_fra %>% 
  filter(participant == 'CHI') %>%
  mutate(share = share*100) %>%
  mutate(pronoun = ifelse(pronoun == 'y'|pronoun=='on'|pronoun == '1sg'|pronoun == '2sg'|pronoun == '3sg','sg', 'pl')) %>% 
  mutate(language = 'fra') %>% 
  select(-path, -languge) -> comp_fra_num


pron_eng %>% 
  filter(participant == 'CHI') %>%
  mutate(share = share*100) %>% 
  mutate(pronoun = ifelse(pronoun == '1sg'|pronoun == '2sg'|pronoun == '3sg','sg', 'pl')) %>% 
  mutate(language = 'eng') %>% 
  select(-path, -languge)-> comp_eng_num

comparison_num <- rbind(comp_eng_num, comp_fra_num)
fit_comp_num <- lmer(share ~ age*pronoun*language + (1|childname), data = comparison_num)
summary(fit_comp_num)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: share ~ age * pronoun * language + (1 | childname)
# Data: comparison_num
# 
# REML criterion at convergence: 104614.5
# 
# Scaled residuals: 
# Min      1Q  Median      3Q     Max 
# -1.6449 -0.6190 -0.3072  0.3312 10.3594 
# 
# Random effects:
# Groups    Name        Variance Std.Dev.
# childname (Intercept)  0.6927  0.8323  
# Residual              10.5797  3.2526  
# Number of obs: 20077, groups:  childname, 196
# 
# Fixed effects:
# Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)               -1.773e+00  3.190e-01  8.099e+03  -5.557 2.82e-08 ***
# age                        1.330e-01  9.989e-03  2.007e+04  13.318  < 2e-16 ***
# pronounsg                  6.022e-01  7.436e-01  1.991e+04   0.810   0.4180    
# languagefra                2.118e+00  4.490e-01  1.219e+04   4.717 2.42e-06 ***
# age:pronounsg              8.322e-03  2.472e-02  1.991e+04   0.337   0.7364    
# age:languagefra           -4.223e-02  1.432e-02  1.892e+04  -2.948   0.0032 ** 
# pronounsg:languagefra     -1.317e+00  8.463e-01  1.991e+04  -1.556   0.1198    
# age:pronounsg:languagefra  1.689e-02  2.816e-02  1.991e+04   0.600   0.5486    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
# (Intr) age    prnnsg lnggfr ag:prn ag:lng prnns:
# age         -0.933                                          
# pronounsg   -0.333  0.351                                   
# languagefra -0.690  0.661  0.237                            
# age:pronnsg  0.331 -0.354 -0.993 -0.235                     
# age:langgfr  0.644 -0.696 -0.245 -0.953  0.247              
# prnnsg:lngg  0.293 -0.309 -0.879 -0.423  0.873  0.437       
# ag:prnnsg:l -0.290  0.310  0.872  0.419 -0.878 -0.442 -0.991
