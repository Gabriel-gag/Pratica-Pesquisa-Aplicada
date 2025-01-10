library(daltoolbox)
library(harbinger)
library(dplyr)
library(ggplot2)

# Dados de exemplo
data("gecco_sample")
dataset <- data.frame(serie = gecco_sample$ph, event = as.logical(gecco_sample$event))

# Preparar o dataset
dataset <- dataset[!is.na(dataset$serie), ]
dataset$serie <- as.numeric(dataset$serie)

# Visualizar a série temporal
ggplot(data = dataset, aes(x = 1:nrow(dataset), y = serie)) +
  geom_line() +
  labs(title = "Time Series Plot of pH", x = "Index", y = "pH") +
  theme_minimal()

# Ajustar o modelo
model <- han_autoencoder(3, 2, aae_encode_decode, num_epochs = 1500)
model <- fit(model, dataset$serie)

# Detectar anomalias
detection <- detect(model, dataset$serie)
detected_events <- detection |> filter(event == TRUE)

# Exibir eventos detectados
print(detected_events)

# Avaliar o modelo
if (!is.null(dataset$event)) {
  # Gerar matriz de confusão
  TP <- sum(detection$event == TRUE & dataset$event == TRUE)
  TN <- sum(detection$event == FALSE & dataset$event == FALSE)
  FP <- sum(detection$event == TRUE & dataset$event == FALSE)
  FN <- sum(detection$event == FALSE & dataset$event == TRUE)

  confMatrix <- matrix(c(TP, FN, FP, TN), nrow = 2, byrow = TRUE)
  colnames(confMatrix) <- c("Pred. Positivo", "Pred. Negativo")
  rownames(confMatrix) <- c("Verdadeiro Pos.", "Verdadeiro Neg.")
  print("Matriz de Confusão:")
  print(confMatrix)
} else {
  print("Eventos reais ausentes. Avaliação não realizada.")
}

# Plotar resultados
grf <- har_plot(model, dataset$serie, detection, dataset$event)
plot(grf)

# Plotar resíduos
res <- attr(detection, "res")
plot(res)
