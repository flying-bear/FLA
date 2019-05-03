pron_fra %>% 
  filter(participant == 'CHI') %>%
  mutate(share = share*100) %>%
  mutate(pronoun = ifelse(pronoun=='1sg'|pronoun=='1pl','1', pronoun)) %>% 
  filter(pronoun == '2' | pronoun == '1' | pronoun == '3') %>% 
  select(-path)-> comparison_fra

pron_eng %>% 
  filter(participant == 'CHI') %>%
  mutate(share == share*100) %>% 
  mutate(pronoun = ifelse(pronoun=='1sg'|pronoun=='1pl','1', pronoun)) %>%
  mutate(pronoun = ifelse(pronoun=='3sgF'|pronoun=='3sgN'|pronoun=='3sgM'|pronoun=='3pl','3', pronoun)) %>% 
  filter(pronoun == '2' | pronoun == '1' | pronoun == '3') %>% 
  select(-path) -> comparison_eng

comparison_eng %>% 
  filter(pronoun == '2') %>% 
  mutate(languge = 'eng') %>%
  group_by(childname) -> comparison_eng_2

comparison_fra %>% 
  filter(pronoun == '2') %>% 
  mutate(languge = 'fra') %>% 
  group_by(childname) -> comparison_fra_2

comparison_2 <- rbind(comparison_eng_2, comparison_fra_2)
fit_comp_2 <- lmer(share ~ age*languge + (1|childname), data = comparison_2)
summary(fit_comp_2)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: share ~ age * languge + (1 | childname)
#    Data: comparison_2
# 
# REML criterion at convergence: 10592.1
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -4.8851 -0.0563 -0.0027  0.0129 14.8879 
# 
# Random effects:
#  Groups    Name        Variance Std.Dev.
#  childname (Intercept) 6.439    2.538   
#  Residual              3.388    1.841   
# Number of obs: 2506, groups:  childname, 196
# 
# Fixed effects:
#                  Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)    -1.678e-01  5.032e-01  1.630e+03  -0.333    0.739    
# age             2.346e-03  1.404e-02  2.290e+03   0.167    0.867    
# langugefra      6.511e+00  6.640e-01  2.476e+03   9.805   <2e-16 ***
# age:langugefra  1.439e-02  2.003e-02  2.422e+03   0.719    0.472    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) age    lnggfr
# age         -0.829              
# langugefra  -0.672  0.625       
# age:langgfr  0.563 -0.700 -0.900

pron_fra %>% 
  filter(participant == 'CHI') %>%
  mutate(share = share*100) %>%
  mutate(pronoun = ifelse(pronoun == 'y'|pronoun=='on'|pronoun == '1sg'|pronoun == '2sg'|pronoun == '3sg','sg', 'pl')) %>% 
  select(-path) -> comp_fra_num


pron_eng %>% 
  filter(participant == 'CHI') %>%
  mutate(share == share*100) %>% 
  mutate(pronoun = ifelse(pronoun == '1sg'|pronoun == '2sg'|pronoun == '3sg','sg', 'pl')) %>% 
  select(-path) %>% 
  select(1:7)-> comp_eng_num

comparison_num <- rbind(comp_eng_num, comp_fra_num)
fit_comp_num <- lmer(share ~ age*pronoun + (1|childname), data = comparison_num)
summary(fit_comp_num)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: share ~ age * pronoun + (1 | childname)
# Data: comparison_num
# 
# REML criterion at convergence: 89512
# 
# Scaled residuals: 
# Min      1Q  Median      3Q     Max 
# -2.6315 -0.2859 -0.0347  0.0573 15.6356 
# 
# Random effects:
# Groups    Name        Variance Std.Dev.
# childname (Intercept) 2.954    1.719   
# Residual              4.905    2.215   
# Number of obs: 20077, groups:  childname, 196
# 
# Fixed effects:
# Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)    1.051e+00  1.986e-01  1.062e+03   5.294 1.46e-07 ***
# age            4.021e-02  4.960e-03  2.007e+04   8.107 5.50e-16 ***
# pronounsg     -1.230e+00  2.291e-01  1.992e+04  -5.369 8.02e-08 ***
# age:pronounsg  4.498e-02  7.631e-03  1.993e+04   5.895 3.81e-09 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
# (Intr) age    prnnsg
# age         -0.760              
# pronounsg   -0.394  0.525       
# age:pronnsg  0.387 -0.532 -0.987
