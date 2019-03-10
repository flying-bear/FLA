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

eng <- read_csv('C:/My/studies/UIT/FLA/code/FLA/result_english_personal.csv')
eng %>% # take relatively well-represented part (see commet below)
  filter(age < 40 & age > 20) -> eng

##  representation issues
# eng %>% # sample size by age 
#   filter(participant == 'CHI') %>% 
#   select(age) %>% 
#   mutate(age = round(age)) %>% 
#   group_by(age) %>% 
#   summarize(n = n()) -> ages
# 
# ages %>% # plot a of sample size by age 
#   ggplot(aes(age, n))+
#   geom_line(color = 'red')+
#   geom_smooth(color = 'black')+
#   labs(x = 'age in months', y = 'sample size',
#        title = 'sample size by age')

eng %>% # summary
  gather('pronoun', 'share', 7:13) -> pron

pron %>% # summary
  mutate(participant = ifelse(participant %in% adults, 'ADU', participant)) %>% 
  filter(participant == 'ADU' | participant == 'CHI') -> pron


pron %>% # summary
  filter(participant == 'ADU') %>% 
  group_by(pronoun) %>% 
  summarize(mean = mean(share),
            sd = sd(share)) -> adult_norm

  
eng %>% # spearman correlation with age
  filter(participant == 'CHI') %>%
  select(`1sg`, age) %>% 
  cor(method = 'spearman')

eng %>% # kendall correlation with age
  filter(participant == 'CHI') %>%
  select(`1sg`, age) -> cor_me
cor.test(cor_me$I, cor_me$age,method = 'kendal')

adult_norm %>% 
  filter(pronoun == '1sg') %>% 
  select(mean) %>%
  as.numeric() -> mean1sg

eng %>% # linear and gamma model compared to average inv for 1sg
  select(age, participant, `1sg`) %>% 
  filter(participant == 'CHI') %>% 
  ggplot(aes(age, `1sg`))+
  geom_hline(aes(yintercept = mean1sg), linetype = 4, color = 'red')+
  geom_smooth(method = 'glm')+
  geom_smooth(color = 'blue', linetype = 3, fill = 'lightgrey')+
  labs(x = 'age in months', y = 'share of 1sg pronoun in words uttered', 
       title = '1sg pronoun share  by age',
       caption = 'red dotted line is average share in adult speech')


pron %>% #linear models
  group_by(pronoun) %>% 
  ggplot(aes(age, share, color = pronoun, linetype = participant))+
  geom_smooth(method = 'glm')+
  labs(x = 'age in months', y = 'share of pronouns in words uttered', 
       title = 'pronoun share  by age, linear model')

pron %>% #gam models
  group_by(pronoun) %>% 
  filter(participant == 'CHI') %>% 
  ggplot(aes(age, share, color = pronoun))+
  geom_smooth(model = 'gam')+
  labs(x = 'age in months', y = 'share of pronouns in words uttered', 
       title = 'pronoun share  by age, regression model')
