
library('tidyr')
library('dplyr')
library('readxl')
library('stringr')
library('factoextra')

raw <- read_excel('data/Encuesta de Compromiso y Colaboración Signify.xlsx') %>% 
  select(-c(collector_id:custom_1)) %>% 
  select(-c(25:27))

catNombres <- data.frame(pregLargo = names(raw)[-1], 
                         pregCorto = paste0('P', str_pad(1:23, 2, 'left', 0)))
catRespuestas <- read.csv('data/recodRespuestas.csv', sep = ';') %>% unique()

# Tabulares ---------------------------------------------------------------

tabulares <- raw %>% 
  gather(pregLargo, respChar, -respondent_id) %>% 
  left_join(catNombres) %>% 
  left_join(catRespuestas) %>% 
  count(pregCorto, pregLargo, respNum, respChar) %>% 
  group_by(pregCorto) %>% 
  mutate(p = n / sum(n)) %>% 
  ungroup()
  
tabulares %>% clipr::write_clip()  

# Recodificación base original --------------------------------------------

baseTB <- raw %>% 
  gather(pregLargo, respChar, -respondent_id) %>% 
  left_join(catNombres) %>% 
  left_join(catRespuestas) %>% 
  select(-pregLargo, -respChar) %>% 
  mutate(respNum = ifelse(respNum == 5, 1, 0)) %>% 
  mutate(respNum = ifelse(is.na(respNum), 0, respNum)) %>%
  spread(pregCorto, respNum)

baseT2B <- raw %>% 
  gather(pregLargo, respChar, -respondent_id) %>% 
  left_join(catNombres) %>% 
  left_join(catRespuestas) %>% 
  select(-pregLargo, -respChar) %>% 
  mutate(respNum = ifelse(respNum %in% 4:5, 1, 0)) %>% 
  mutate(respNum = ifelse(is.na(respNum), 0, respNum)) %>%
  spread(pregCorto, respNum)

save(baseTB, file = 'cache/baseTB.rda')
save(baseT2B, file = 'cache/baseT2B.rda')
