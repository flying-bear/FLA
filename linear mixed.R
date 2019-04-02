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

fit_fra <- fit <- lmer(share ~ age*pronoun + (1|childname), data = chi_fra)
summary(fit_fra)

qqnorm(lm(share ~ age + pronoun, data = chi_fra)$residuals)
qqnorm(lm(share ~ age + pronoun, data = chi_eng)$residuals)

summary(lm(share ~ age + pronoun, data = chi_fra))
