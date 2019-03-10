library(tidyverse)
library(tidyr)

eng <- read_csv('C:/My/studies/UIT/FLA/code/FLA/result_english_personal.csv')

adults <- c('MOT', 
            'FAT', 'DAD',
            'INV', 'INT', 
            'GRA', 'GDM', 'GRM', 'MAM', 'NAN',
            'GRN', 'GDF', 'GRF',
            'DOC', 'OBS', 'HUS', 'MAN', 'MRS', 'NEI', 'OTH',
            'UNC', 
            'AUN', 'AUT')

eng %>%
  filter(participant == 'INV') %>% 
  select(I) %>%
  summarize(mean(I), sd(I)) -> summary

mean_adult <- as.numeric(summary['mean(I)'])
sd_adult <- as.numeric(summary['sd(I)'])

eng %>% 
  filter(participant == 'CHI') %>%
  select(I, age) %>% 
  cor(method = 'spearman')

eng %>% 
  filter(participant == 'CHI') %>%
  select(I, age) -> cor_me
cor.test(cor_me$I, cor_me$age,method = 'kendal')

eng %>% 
  select(age, participant, I) %>% 
  filter(participant == 'CHI') %>% 
  ggplot(aes(age, I))+
  geom_hline(aes(yintercept = mean_adult), linetype = 2, color = 'red')+
  geom_smooth(method = 'glm', show.legend = TRUE)+
  geom_smooth(color = 'blue', linetype = 3, fill = 'lightgrey', show.legend = TRUE)+
  labs(x = 'age in months', y = 'share of 1sg pronoun in words uttered', 
       title = '1sg pronoun share  by age')
  
  