library(tidyverse)
library(tidyr)
library(lme4)
library(lmerTest)


adults <- c('MOT', 
            'FAT', 'DAD',
            'INV', 'INT', 
            'GRA', 'GDM', 'GRM', 'MAM', 'NAN',
            'GRN', 'GDF', 'GRF',
            'DOC', 'OBS', 'HUS', 'MAN', 'MRS', 'NEI', 'OTH',
            'UNC', 
            'AUN', 'AUT')


fra <- read_csv('C:/My/studies/UIT/FLA/code/FLA/result_french_personal.csv')
#  representation issues
fra %>% # sample size by age
  filter(participant == 'CHI') %>%
  select(age) %>%
  mutate(age = floor(age)) %>%
  group_by(age) %>%
  summarize(n = n()) -> ages

fra %>% # sample size 
  filter(participant == 'CHI') %>% 
  summarize(n = n())

fra %>% 
  mutate(`2` = `2sg` + `2pl`,
         `3` = `3sg` + `3pl`) -> fra 

ages %>% # plot a of sample size by age
  ggplot(aes(age, n))+
  geom_smooth(color = 'black')+
  labs(x = 'age in months', y = 'sample size',
       title = 'sample size by age for french')

fra %>%
  filter(age < 35 & age > 25) %>% 
  gather('pronoun', 'share', 7:16) -> pron_fra

pron_fra %>%
  filter(participant == 'CHI') %>%
  mutate(age = floor(age)) %>% 
  spread('age','share') %>% 
  write.csv('spread_pronoun_fra.csv')

pron_fra %>% # summary
  mutate(participant = ifelse(participant %in% adults, 'ADU', participant)) %>% 
  filter(participant == 'ADU' | participant == 'CHI') -> pron_fra

pron_fra %>% # summary
  filter(participant == 'ADU') %>% 
  group_by(pronoun) %>% 
  filter(share != 0) %>% 
  summarize(mean = mean(share),
            median = median(share),
            sd = sd(share)) -> adult_norm_fra

fra %>% # linear and gamma model compared to average inv for 1sg
  select(age, participant, `1sg`) %>% 
  filter(participant == 'CHI') %>% 
  ggplot(aes(age, `1sg`))+
  geom_smooth(method = 'glm')+
  geom_smooth(color = 'blue', linetype = 3, fill = 'lightgrey')+
  labs(x = 'age in months', y = 'share of 1sg pronoun in words uttered', 
       title = '1sg pronoun share  by age for french',
       caption = 'red dotted line is average share in adult speech')

pron_fra %>% #linear models
  group_by(pronoun) %>% 
  ggplot(aes(age, share, color = pronoun, linetype = participant))+
  geom_smooth(method = 'glm')+
  labs(x = 'age in months', y = 'share of pronouns in words uttered', 
       title = 'french pronoun share  by age, linear model')

pron_fra %>% #gam models
  group_by(pronoun) %>% 
  filter(participant == 'CHI') %>% 
  ggplot(aes(age, share, color = pronoun))+
  geom_smooth()+
  labs(x = 'age in months', y = 'share of pronouns in words uttered', 
       title = 'french pronoun share  by age, regression model')

comparable <-  c('1sg', '1pl', '2', '3')

pron_fra %>% 
  filter(pronoun %in% comparable) %>% 
  mutate(languge = 'fra') %>% 
  select(-path, -filename, -childname) -> pron_fra_merge

#### for merging ####

pron_ger_merge <- read_csv('pron_ger_merge.csv')

pron <- rbind(pron_fra_merge, pron_eng_merge, pron_ger_merge)

pron %>%
  filter(participant == 'CHI') %>% 
  mutate(language = languge) %>%
  select(-languge) %>% 
  filter(share != 1)-> pron

pron %>%
  group_by(pronoun) %>% 
  ggplot(aes(age, share, color = pronoun, linetype = language))+
  geom_smooth(method = 'lm')+
  labs(x = 'age in months', y = 'share of pronouns in words uttered', 
       title = 'comparison of english, french and german pronoun shares  by age, linear model')

pron %>%
  mutate(eng = ifelse(language == 'eng', 1, 0)) %>%
  mutate(ger = ifelse(language == 'ger', 1, 0)) %>%
  select(-language) -> pron
m1 <- summary(lm(share~age+pronoun+eng+ger, data = pron))
m1

qqnorm(m1$residuals)