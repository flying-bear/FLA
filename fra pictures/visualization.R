library(tidyverse)
library(tidyr)

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
  mutate(age = round(age)) %>%
  group_by(age) %>%
  summarize(n = n()) -> ages

fra %>% # sample size 
  filter(participant == 'CHI') %>% 
  summarize(n = n())

fra %>% 
  mutate(`2` = `2sg` + `2pl`) -> fra 

ages %>% # plot a of sample size by age
  ggplot(aes(age, n))+
  geom_smooth(color = 'black')+
  labs(x = 'age in months', y = 'sample size',
       title = 'sample size by age for french')

fra %>%
  filter(age < 40 & age > 20) %>% 
  gather('pronoun', 'share', 7:15) -> pron_fra

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

comparable <-  c('1sg', '3sg', '1pl', '2')

pron_fra %>% 
  filter(pronoun %in% comparable) %>% 
  mutate(languge = 'fra') %>% 
  select(-path, -filename, -childname) -> pron_fra_merge

pron <- rbind(pron_fra_merge, pron_eng_merge)

pron %>%
  group_by(pronoun) %>% 
  filter(participant == 'CHI') %>% 
  ggplot(aes(age, share, color = pronoun, linetype = languge))+
  geom_smooth(method = 'lm')+
  labs(x = 'age in months', y = 'share of pronouns in words uttered', 
       title = 'comparison of french and english pronoun shares  by age, linear model')
