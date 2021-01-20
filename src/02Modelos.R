
library('tidyr')
library('dplyr')
library('factoextra')
library('randomForest')
library('MESS')
library('stringr')
library('dendextend')

load('cache/baseTB.rda')
load('cache/baseT2B.rda')

catNombres <- data.frame(pregLargo = names(baseTB)[-1], 
                         pregCorto = paste0('P', str_pad(1:23, 2, 'left', 0)))

# Modelo T2B --------------------------------------------------------------

baseModeloT2B <- baseT2B %>% 
  select(P01:P16) %>% 
  mutate(P05 = abs(P05-1))

sum(rowSums(baseModeloT2B) == 0) # P 5 6 7 15 salen significativas

modT2B <- glm(as.factor(P16) ~ ., data = baseModeloT2B, family = 'binomial')
# modT2B <- glm(as.factor(P16) ~ P05 + P06 + P07 + P15, data = baseModeloT2B, family = 'binomial')
summary(modT2B)

resultadosT2B <- broom::tidy(modT2B)
resultadosT2B %>% 
  filter(p.value <= 0.1) %>%
  data.frame() %>%
  mutate(elasticity = (exp(estimate) - 1)*1) 

predsT2B <- (modT2B$fitted.values > 0.5) + 0
table(baseModeloT2B$P16, predsT2B)
prop.table(table(baseModeloT2B$P16, predsT2B), 1)

# Modelo TB ---------------------------------------------------------------
### No se puede hacer un GLM con TB, no converge

# baseModeloTB <- baseTB %>%
# select(P01:P16) %>%
# mutate(P05 = abs(P05-1)) %>%
# filter(rowSums(.) > 0)
# identity()

# sum(rowSums(baseModeloTB) == 0)
# 
# modTB <- glm(P16 ~ ., data = baseModeloTB, family = 'binomial')
# summary(modTB)
# 
# resultadosTB <- broom::tidy(modTB)
# resultadosTB %>% filter(p.value <= 0.1)
# 
# predsTB <- (modTB$fitted.values > 0.5) + 0
# table(baseModeloTB$P16, predsTB)
# prop.table(table(baseModeloTB$P16, predsTB), 1)

# Random Forest T2B -------------------------------------------------------

forestT2B <- randomForest(as.factor(P16) ~ ., 
                          data = mutate_all(baseModeloT2B, as.factor),
                          importance = TRUE)

impT2B <- forestT2B$importance %>% 
  data.frame() %>% 
  select(giniT2B = MeanDecreaseGini, accT2B = MeanDecreaseAccuracy) %>% 
  tibble::rowid_to_column('preg') %>% 
  arrange(-giniT2B)

table(baseModeloT2B$P16, forestT2B$predicted)
prop.table(table(baseModeloT2B$P16, forestT2B$predicted), 1)

# Random Forest TB --------------------------------------------------------
# forestTB <- randomForest(as.factor(P16) ~ ., 
#                          data = mutate_all(baseModeloTB, as.factor),
#                          importance = TRUE)
# 
# impTB <- forestTB$importance %>% 
#   data.frame() %>% 
#   select(giniTB = MeanDecreaseGini, accTB = MeanDecreaseAccuracy) %>% 
#   tibble::rowid_to_column('preg') %>% 
#   arrange(-giniTB)
# 
# table(baseModeloTB$P16, forestTB$predicted)
# prop.table(table(baseModeloTB$P16, forestTB$predicted), 1)
# 
# impT2B %>% 
#   left_join(impTB) %>% 
#   ggplot() +
#   geom_point(aes(giniT2B, giniTB)) +
#   ggrepel::geom_text_repel(aes(giniT2B, giniTB, label = preg)) +
#   geom_abline() 
# 
# impT2B %>% 
#   left_join(impTB) %>% 
#   ggplot() +
#   geom_point(aes(accT2B, accTB)) +
#   ggrepel::geom_text_repel(aes(accT2B, accTB, label = preg)) +
#   geom_abline() 

# Correlograma ------------------------------------------------------------

asociacion <- function(tabla) {
  n <- ncol(tabla)
  mat <- matrix(rep(0, n*n), nrow = n)
  k <- 0
  for(i in 1:n) {
    for(j in 1:n) {
      mat[i,j] <- gkgamma(table(pull(tabla, i), pull(tabla, j)))$estimate
      k <- k + 1
    }
  }
  mat
}

gammaT2B <- baseT2B[,2:17] %>% 
  mutate(P05 = abs(P05-1)) %>%
  asociacion() %>%
  data.frame() %>% 
  setNames(catNombres$pregCorto[1:16]) %>% 
  tibble::add_column(preg = catNombres$pregCorto[1:16], .before = 1)

# gammaTB <- baseTB[,2:17] %>% 
#   # mutate(P05 = abs(P05-1)) %>%
#   asociacion() %>%
#   data.frame() %>% 
#   setNames(catNombres$pregCorto[1:16]) %>% 
#   tibble::add_column(preg = catNombres$pregCorto[1:16], .before = 1)

df <- gammaT2B
df %>% 
  gather(preg2, corr, -preg) %>% 
  ggplot() +
  geom_tile(aes(preg, preg2, fill = corr)) +
  geom_text(data = df %>% 
              gather(preg2, corr, -preg) %>% 
              filter(abs(corr) > 0.8, corr != 1),
            aes(preg, preg2, label = round(corr,2), color = corr > 0.9)) +
  scale_fill_gradient2(low = 'tomato', high = 'royalblue', ) +
  scale_color_manual(values = c('grey', 'black')) +
  coord_equal() +
  labs(title = 'Correlograma',
       fill = 'Corr') +
  theme_test() +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.border = element_blank()) +
  guides(color = 'none')

# ggsave('graphs/correlograma.png', width = 20, height = 20, units = 'cm')

# Dendrograma -------------------------------------------------------------

# dend <- gammaT2B[-16,-c(1,17)]
dend <- gammaT2B[,-c(1)]

# rownames(dend) <- catNombres$pregLargo[1:15]
hc <- dend %>% 
  as.matrix() %>% 
  dist() %>% 
  hclust()

library('ggdendro')
hcdata <- dendro_data(hc, type = "rectangle")

cors <- round(gammaT2B$P16,2)

labs <-
  label(hcdata) %>% 
    mutate(label = paste0('P', str_pad(1:16, 2, 'left', 0))) %>% 
  arrange(label) %>%
  left_join(catNombres, by = c('label' = 'pregCorto')) %>%
  mutate(pregLargo = paste0(label, ': ', pregLargo)) 
  tibble::add_column(cors[-16])
    
labs

ggplot() +
  geom_segment(data = segment(hcdata), 
               aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_label(data = labs, 
            aes(x = x, y = y - 1, label = pregLargo, hjust = 0), 
            size = 3) +
  geom_text(data = labs, 
             aes(x = x, y = y-0.1, label = cors, hjust = 0), 
             size = 3) +
  coord_flip() +
  ylim(c(3.1, -20)) +
  theme_void()


ggsave('graphs/dendrograma2.png', width = 80, height = 20, units = 'cm')











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




