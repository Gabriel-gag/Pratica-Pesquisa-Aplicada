library(dplyr)
library(ggplot2)
library(tidyr)
library(GGally)
library(caret)
library(cluster)
library(factoextra)
library(arules)

url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
colnames <- c("Tipo", "Alcool", "AcidoMalico", "Cinza", "AlcalinidadeCinza",
              "Magnesio", "FenolTotal", "Flavonoides", "FenolNaoFlavonoide",
              "Proantocianinas", "IntensidadeCor", "Matiz", "OD280_OD315",
              "Prolina")

vinhos <- read.csv(url, header = FALSE, col.names = colnames)

# 1) Análise Exploratória
## a. Média e desvio padrão para todos os atributos
media_dp <- vinhos %>%
  summarise(across(everything(), list(media = mean, desvio = sd), .names = "{.col}_{.fn}"))

print(media_dp)

## b. Média e desvio padrão agrupados pelo tipo de vinho
media_dp_tipo <- vinhos %>%
  group_by(Tipo) %>%
  summarise(across(everything(), list(media = mean, desvio = sd), .names = "{.col}_{.fn}"))

print(media_dp_tipo)

## c. Gráfico de distribuição de densidade
vinhos_long <- vinhos %>%
  pivot_longer(cols = -Tipo, names_to = "Atributo", values_to = "Valor")

ggplot(vinhos_long, aes(x = Valor, fill = factor(Tipo))) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ Atributo, scales = "free") +
  labs(x = "Valor", y = "Densidade", fill = "Tipo de Vinho") +
  theme_minimal()

## d. Gráfico de box-plot
ggplot(vinhos_long, aes(x = factor(Tipo), y = Valor, fill = factor(Tipo))) +
  geom_boxplot() +
  facet_wrap(~ Atributo, scales = "free") +
  labs(x = "Tipo de Vinho", y = "Valor", fill = "Tipo de Vinho") +
  theme_minimal()

## e. Gráfico de dispersão entre os atributos
ggpairs(vinhos, columns = 2:ncol(vinhos), aes(color = factor(Tipo)))

# 2) Pré-processamento
## a. Discretização dos atributos numéricos
vinhos_discretizados <- vinhos %>%
  mutate(across(-Tipo, ~ cut(., breaks = c(-Inf, 8, 12, Inf),
                             labels = c("Baixo", "Médio", "Alto"))))

## b. Mapeamento categórico do tipo de vinho
vinhos_discretizados$Tipo <- as.factor(vinhos_discretizados$Tipo)

# 3) Agrupamento k-means
set.seed(123) # Para reprodutibilidade
k <- 3 # Número de clusters
vinhos_kmeans <- vinhos[, -1] # Excluir a coluna Tipo
kmeans_result <- kmeans(vinhos_kmeans, centers = k)

# Calcular a entropia de cada grupo
table_cluster <- table(kmeans_result$cluster, vinhos$Tipo)
entropy_values <- -rowSums(prop.table(table_cluster, 1) * log2(prop.table(table_cluster, 1)), na.rm = TRUE)
print(entropy_values)

# 4) Modelo de predição com redes neurais
## Dividir os dados em conjunto de treino e teste
set.seed(123)

# Garantir que todas as classes estejam presentes no conjunto de treino
index <- createDataPartition(vinhos$Tipo, p = 0.7, list = FALSE, times = 1)
train_data <- vinhos[index, ]
test_data <- vinhos[-index, ]

## Normalizar os dados
preProc <- preProcess(train_data[-1], method = c("center", "scale"))
train_data_scaled <- predict(preProc, train_data[-1])
train_data_scaled$Tipo <- train_data$Tipo # Adicionar Tipo

# Garantir que o Tipo seja um fator no conjunto de treino
train_data_scaled$Tipo <- factor(train_data_scaled$Tipo)

## Criar o modelo de rede neural
nn_model <- train(Tipo ~ ., data = train_data_scaled, method = "nnet", trace = FALSE)

## Avaliar a acurácia
test_data_scaled <- predict(preProc, test_data[-1])
test_data_scaled$Tipo <- test_data$Tipo # Adicionar Tipo

# Garantir que o Tipo seja um fator no conjunto de teste
test_data_scaled$Tipo <- factor(test_data_scaled$Tipo)

# Realizar previsões
predictions <- predict(nn_model, test_data_scaled)

# Calcular a matriz de confusão e a acurácia sem alterar níveis
confusion_result <- confusionMatrix(predictions, test_data_scaled$Tipo)

# Exibir o resultado da matriz de confusão
print(confusion_result)