library(lmerTest)
library(tidyverse)


pron_eng %>% 
  filter(participant == 'CHI') %>%
  mutate(pronoun = ifelse(pronoun=='1sg'|pronoun=='1pl','1', pronoun)) %>%
  mutate(pronoun = ifelse(pronoun=='3sgF'|pronoun=='3sgN'|pronoun=='3sgM'|pronoun=='3pl','3', pronoun)) %>% 
  filter(pronoun == '2' | pronoun == '1' | pronoun == '3') %>% 
  mutate(share = share*100) %>% 
  select(-path) -> chi_eng

pron_fra %>% 
  filter(participant == 'CHI') %>%
  mutate(pronoun = ifelse(pronoun=='1sg'|pronoun=='1pl','1', pronoun)) %>% 
  filter(pronoun == '2' | pronoun == '1' | pronoun == '3'| pronoun == 'on'| pronoun == 'y') %>% 
  mutate(share = share*100) %>% 
  mutate(pronoun = relevel(as.factor(pronoun), '1')) %>% 
  select(-path) -> chi_fra

pron_eng %>% 
  mutate(pronoun = ifelse(pronoun == '1sg'|pronoun == '2sg'|pronoun=='3sgF'
                          |pronoun=='3sgN'|pronoun == '3sgM','sg', 'pl')) %>% 
  mutate(share = share*100) -> chi_eng

pron_fra %>% 
  filter(participant == 'CHI') %>%
  filter(pronoun != 'on') %>% 
  filter(pronoun != 'y') %>% 
  filter(pronoun != '2') %>% 
  filter(pronoun != '3') %>% 
  mutate(share = share*100) %>% 
  mutate(pronoun = relevel(as.factor(pronoun), '1sg')) %>% 
  select(-path) -> chi_fra

pron_fra %>% 
  filter(participant == 'CHI') %>%
  mutate(share = share*100) %>%
  mutate(pronoun = ifelse(pronoun == 'y'|pronoun=='on'|pronoun == '1sg'|pronoun == '2sg'
                          |pronoun == '3sg','sg', 'pl')) -> chi_fra

fit_eng <- lmer(share ~ age*pronoun + (1|childname), data = chi_eng)
summary(fit_eng)
### person
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: share ~ age * pronoun + (1 | childname)
# Data: chi_eng
# 
# REML criterion at convergence: 58976.5
# 
# Scaled residuals: 
# Min      1Q  Median      3Q     Max 
# -2.3744 -0.6311 -0.3039  0.2883  9.8041 
# 
# Random effects:
# Groups    Name        Variance Std.Dev.
# childname (Intercept) 0.3681   0.6067  
# Residual              9.2564   3.0424  
# Number of obs: 11627, groups:  childname, 57
# 
# Fixed effects:
# Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)  -9.977e-01  4.752e-01  8.279e+03  -2.100   0.0358 *  
# age           9.112e-02  1.549e-02  1.162e+04   5.885  4.1e-09 ***
# pronoun2     -1.016e+01  7.887e-01  1.158e+04 -12.888  < 2e-16 ***
# pronoun3      1.205e+00  5.577e-01  1.158e+04   2.160   0.0308 *  
# age:pronoun2  4.129e-01  2.622e-02  1.158e+04  15.752  < 2e-16 ***
# age:pronoun3 -2.558e-02  1.854e-02  1.158e+04  -1.380   0.1676    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
# (Intr) age    pronn2 pronn3 ag:pr2
# age         -0.973                            
# pronoun2    -0.553  0.561                     
# pronoun3    -0.782  0.793  0.471              
# age:pronon2  0.550 -0.564 -0.993 -0.468       
# age:pronon3  0.777 -0.798 -0.468 -0.993  0.471

### number
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: share ~ age * pronoun + (1 | childname)
# Data: chi_eng
# 
# REML criterion at convergence: 181573.6
# 
# Scaled residuals: 
# Min     1Q Median     3Q    Max 
# -0.948 -0.604 -0.365  0.230 40.692 
# 
# Random effects:
# Groups    Name        Variance Std.Dev.
# childname (Intercept)  0.2994  0.5472  
# Residual              23.1632  4.8128  
# Number of obs: 30345, groups:  childname, 57
# 
# Fixed effects:
# Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)    1.520e+00  3.847e-01  1.092e+04   3.951 7.82e-05 ***
# age            6.445e-02  1.255e-02  2.996e+04   5.137 2.81e-07 ***
# pronounsg      1.018e-01  4.848e-01  3.030e+04   0.210    0.834    
# age:pronounsg -2.237e-02  1.625e-02  3.030e+04  -1.377    0.169    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
# (Intr) age    prnnsg
# age         -0.968              
# pronounsg   -0.720  0.735       
# age:pronnsg  0.715 -0.740 -0.993

qqnorm(summary(fit_eng)$residuals)

fit_fra <- lmer(share ~ age*pronoun + (1|childname), data = chi_fra)
summary(fit_fra)
### person
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: share ~ age * pronoun + (1 | childname)
# Data: chi_fra
# 
# REML criterion at convergence: 23442.6
# 
# Scaled residuals: 
# Min      1Q  Median      3Q     Max 
# -3.1931 -0.4901 -0.0827  0.2945 13.0955 
# 
# Random effects:
# Groups    Name        Variance Std.Dev.
# childname (Intercept) 0.748    0.8649  
# Residual              5.736    2.3949  
# Number of obs: 5070, groups:  childname, 143
# 
# Fixed effects:
# Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)     -0.56107    0.36868 3930.68481  -1.522   0.1281    
# age              0.06947    0.01173 5021.95618   5.921 3.41e-09 ***
# pronoun2         5.42934    0.57610 4949.61948   9.424  < 2e-16 ***
# pronoun3        -5.20053    0.57610 4949.61948  -9.027  < 2e-16 ***
# pronounon        0.27917    0.57610 4949.61947   0.485   0.6280    
# pronouny         0.66828    0.57610 4949.61948   1.160   0.2461    
# age:pronoun2    -0.01787    0.01925 4949.61948  -0.928   0.3534    
# age:pronoun3     0.31472    0.01925 4949.61948  16.348  < 2e-16 ***
# age:pronounon   -0.02801    0.01925 4949.61948  -1.455   0.1458    
# age:pronouny    -0.03712    0.01925 4949.61948  -1.928   0.0539 .  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
# (Intr) age    pronn2 pronn3 pronnn pronny ag:pr2 ag:pr3 ag:prnnn
# age         -0.958                                                          
# pronoun2    -0.521  0.538                                                   
# pronoun3    -0.521  0.538  0.333                                            
# pronounon   -0.521  0.538  0.333  0.333                                     
# pronouny    -0.521  0.538  0.333  0.333  0.333                              
# age:pronon2  0.513 -0.547 -0.985 -0.328 -0.328 -0.328                       
# age:pronon3  0.513 -0.547 -0.328 -0.985 -0.328 -0.328  0.333                
# age:prononn  0.513 -0.547 -0.328 -0.328 -0.985 -0.328  0.333  0.333         
# age:pronony  0.513 -0.547 -0.328 -0.328 -0.328 -0.985  0.333  0.333  0.333 
qqnorm(summary(fit_fra)$residuals)

### number
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: share ~ age * pronoun + (1 | childname)
# Data: chi_fra
# 
# REML criterion at convergence: 44748.3
# 
# Scaled residuals: 
# Min      1Q  Median      3Q     Max 
# -1.5940 -0.6396 -0.2947  0.4151  9.9541 
# 
# Random effects:
# Groups    Name        Variance Std.Dev.
# childname (Intercept)  0.8939  0.9455  
# Residual              11.4351  3.3816  
# Number of obs: 8450, groups:  childname, 143
# 
# Fixed effects:
# Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)      0.51765    0.34420 3844.92141   1.504   0.1327    
# age              0.08662    0.01074 7667.21579   8.063 8.59e-16 ***
# pronounsg       -0.71446    0.42006 8335.76847  -1.701   0.0890 .  
# age:pronounsg    0.02522    0.01404 8335.76847   1.796   0.0725 .  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
# (Intr) age    prnnsg
# age         -0.948              
# pronounsg   -0.610  0.643       
# age:pronnsg  0.601 -0.653 -0.985

qqnorm(lm(share ~ age * pronoun, data = chi_fra)$residuals)
qqnorm(lm(share ~ age * pronoun, data = chi_eng)$residuals)
