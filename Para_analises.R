library(harbinger)
library(dplyr)
library(ggplot2)
library(reticulate)

use_virtualenv("C:/Users/gabri/Downloads/harbinger-master/harbinger-master/virtualenv")
# Carregar o dataset gecco_sample (assumindo que já está carregado)
data("gecco_sample")  # Ou carregar o dataset manualmente se necessário
#gecco_sample <- gecco_sample$gecco_sample  # Ajuste conforme o formato correto do seu dataset


# Usar a série temporal de pH do gecco_sample (ajustando conforme o formato do seu dataset)
dataset <- data.frame(serie = gecco_sample$ph, event = as.logical(gecco_sample$event))
head(dataset)

# Preparar o dataset, removendo valores ausentes e garantindo que a série seja numérica
dataset <- dataset[!is.na(dataset$serie), ]
dataset$serie <- as.numeric(dataset$serie)

# Usar uma amostra do dataset (pode ser uma parte menor para testes)
#dataset <- dataset[1:1000, ]  # Usando as primeiras 1000 observações

# Estabelecer o modelo han_autoencoder
model <- han_autoencoder(3, 2, cae_encode_decode, num_epochs = 1500)

# Treinar o modelo com a série temporal
model <- fit(model, dataset$serie)

# Fazer a detecção de anomalias
detection <- detect(model, dataset$serie)

# Filtrar eventos detectados
detected_events <- detection %>% filter(event == TRUE)
print(detected_events)

detection$event <- ifelse(is.na(detection$event), FALSE, detection$event)
dataset$event <- ifelse(is.na(dataset$event), FALSE, dataset$event)

# Avaliar o desempenho do modelo com base nos eventos reais
if (!is.null(dataset$event)) {
  # Gerar matriz de confusão
  TP <- sum(detection$event == TRUE & dataset$event == TRUE, na.rm = TRUE)
  TN <- sum(detection$event == FALSE & dataset$event == FALSE, na.rm = TRUE)
  FP <- sum(detection$event == TRUE & dataset$event == FALSE, na.rm = TRUE)
  FN <- sum(detection$event == FALSE & dataset$event == TRUE, na.rm = TRUE)


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
