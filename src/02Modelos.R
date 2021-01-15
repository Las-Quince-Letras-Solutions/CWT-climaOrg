
library('tidyr')
library('dplyr')
library('factoextra')

load('cache/baseTB.rda')
load('cache/baseT2B.rda')

# Modelo T2B --------------------------------------------------------------

baseModelo <- baseT2B %>% 
  select(P01:P16) %>% 
  mutate(P05 = abs(P05-1))
  
sum(rowSums(baseModelo) == 0)

mod <- glm(P16 ~ ., data = baseModelo, family = 'binomial')
mod <- glm(P16 ~ P05 + P06 + P07 + P15, data = baseModelo, family = 'binomial')
summary(mod)

resultadosT2B <- broom::tidy(mod)
resultadosT2B %>% filter(p.value <= 0.2)

# Modelo TB ---------------------------------------------------------------
### No se puede hacer un GLM con TB

baseModelo <- baseTB %>% 
  select(P01:P16) %>% 
  # mutate(P05 = abs(P05-1)) %>% 
  filter(rowSums(.) > 0)

sum(rowSums(baseModelo) == 0)

mod <- glm(P16 ~ ., data = baseModelo, family = 'binomial')
summary(mod)

resultadosTB <- broom::tidy(mod)
resultadosTB %>% filter(p.value <= 0.1)











# PCA ---------------------------------------------------------------------

x <- base[,c(2:5, 7:16)] %>% na.omit
x <- t(scale(t(x), scale = FALSE))
apply(x, 2, var)
res.pca <- prcomp(x, scale = FALSE)

# PCA ---------------------------------------------------------------------


fviz_eig(res.pca)

fviz_pca_biplot(res.pca, repel = TRUE,label = 'var',
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

# ggsave('graphs/pca.png', width = 22, height = 20, units = 'cm')




