# Carregar pacotes necessários
library(daltoolbox)
library(harbinger)
library(dplyr)
library(ggplot2)

# Carregar o dataset de exemplo
data("examples_anomalies")

# Usar a série temporal 'sequence' do exemplo
dataset <- examples_anomalies$sequence
head(dataset)

# Preparar o dataset, removendo valores ausentes e garantindo que a série seja numérica
dataset <- dataset[!is.na(dataset$serie), ]
dataset$serie <- as.numeric(dataset$serie)

# Plotar a série temporal
ggplot(data = dataset, aes(x = 1:nrow(dataset), y = serie)) +
  geom_line() +
  labs(title = "Time Series Plot", x = "Index", y = "Value") +
  theme_minimal()

# Estabelecer o modelo hanct_dtw
model <- hanct_dtw(3)

# Treinar o modelo com a série temporal
model <- fit(model, dataset$serie)

# Fazer a detecção de anomalias (discórdia usando k-means)
detection <- detect(model, dataset$serie)

# Filtrar eventos detectados
detected_events <- detection |> filter(event == TRUE)
print(detected_events)

# Avaliar o desempenho do modelo com base nos eventos reais
if (!is.null(dataset$event)) {
  # Gerar matriz de confusão
  TP <- sum(detection$event == TRUE & dataset$event == TRUE)
  TN <- sum(detection$event == FALSE & dataset$event == FALSE)
  FP <- sum(detection$event == TRUE & dataset$event == FALSE)
  FN <- sum(detection$event == FALSE & dataset$event == TRUE)
  
  # Exibir matriz de confusão
  confMatrix <- matrix(c(TP, FN, FP, TN), nrow = 2, byrow = TRUE)
  colnames(confMatrix) <- c("Pred. Positivo", "Pred. Negativo")
  rownames(confMatrix) <- c("Verdadeiro Pos.", "Verdadeiro Neg.")
  print("Matriz de Confusão:")
  print(confMatrix)
  
  # Calcular as métricas de desempenho
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  accuracy <- (TP + TN) / (TP + FN + FP + TN)
  f1_score <- 2 * (precision * recall) / (precision + recall)
  specificity <- TN / (TN + FP)
  
  # Mostrar as métricas de desempenho
  cat("Precisão:", precision, "\n")
  cat("Revocação:", recall, "\n")
  cat("Acurácia:", accuracy, "\n")
  cat("F1-Score:", f1_score, "\n")
  cat("Especificidade:", specificity, "\n")
} else {
  print("Eventos reais ausentes. Avaliação não realizada.")
}

# Plotar os resultados da detecção
grf <- har_plot(model, dataset$serie, detection, dataset$event)
plot(grf)

# Plotar resíduos
res <- attr(detection, "res")
plot(res)

# Salvar o modelo treinado
saveRDS(model, "hanct_dtw_model.rds")
