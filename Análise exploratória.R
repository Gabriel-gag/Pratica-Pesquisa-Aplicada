library(dplyr)

library(ggplot2)

url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
colnames <- c("Tipo", "Alcool", "AcidoMalico", "Cinza", "AlcalinidadeCinza", 
              "Magnesio", "FenolTotal", "Flavonoides", "FenolNaoFlavonoide",
              "Proantocianinas", "IntensidadeCor", "Matiz", "OD280_OD315",
              "Prolina")

vinhos <- read.csv(url, header = FALSE, col.names = colnames)

media_dp <- vinhos %>%
  summarise(across(everything(), list(media = mean, desvio = sd), .names = "{.col}_{.fn}"))
media_dp

media_dp_tipo <- vinhos %>%
  group_by(Tipo) %>%
  summarise(across(everything(), list(media = mean, desvio = sd), .names = "{.col}_{.fn}"))
media_dp_tipo

library(tidyverse)

vinhos_long <- vinhos %>%
  pivot_longer(cols = -Tipo, names_to = "Atributo", values_to = "Valor")

ggplot(vinhos_long, aes(x = Valor, fill = factor(Tipo))) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ Atributo, scales = "free") +
  labs(x = "Valor", y = "Densidade", fill = "Tipo de Vinho") +
  theme_minimal()

ggplot(vinhos_long, aes(x = factor(Tipo), y = Valor, fill = factor(Tipo))) +
  geom_boxplot() +
  facet_wrap(~ Atributo, scales = "free") +
  labs(x = "Tipo de Vinho", y = "Valor", fill = "Tipo de Vinho") +
  theme_minimal()

library(GGally)

ggpairs(vinhos, columns = 2:ncol(vinhos), aes(color = factor(Tipo)))

