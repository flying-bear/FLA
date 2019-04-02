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
