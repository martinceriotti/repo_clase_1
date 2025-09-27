setwd("/home/marcos/pai_2024/practicas/01_ejercicio1/")

library(tidyverse)

setosa <- read_delim("setosa.txt") %>% mutate(especie = "setosa")
virginica <- read_delim("virginica.txt") %>% mutate(especie = "virginica")
versicolor <- read_delim("versicolor.txt") %>% mutate(especie = "versicolor")

datos <- bind_rows(setosa, virginica, versicolor)
rm(virginica, versicolor)

write_delim(datos, file = "datos_limpios.txt", delim = "\t")

g1 <- 
  ggplot(datos) +
  aes(x = Sepal.Length, y = Sepal.Width, color = especie) +
  geom_point() +
  theme_linedraw()

g2 <- 
  datos %>% 
  select(Sepal.Length, Sepal.Width, especie) %>% 
  pivot_longer(cols = c(Sepal.Length, Sepal.Width), names_to = "tipo_medida", 
               values_to = "cm") %>% 
  ggplot() +
  aes(x = cm, y = especie) +
  geom_boxplot() +
  facet_wrap(~ tipo_medida) +
  theme_linedraw()

ggsave(filename = "diagrama_puntos.png", plot = g1, 
       width = 15, height = 9, units = "cm")
ggsave(filename = "boxplot.png", plot = g2, 
       width = 15, height = 9, units = "cm")

mod_sepal <- lm(Sepal.Width ~ Sepal.Length, setosa)
summary(mod_sepal)

mod_petal <- lm(Petal.Width ~ Petal.Length, setosa)
summary(mod_petal)

saveRDS(mod_sepal, file = "rtdo_regr_sepal.rds")
saveRDS(mod_petal, file = "rtdo_regr_petal.rds")