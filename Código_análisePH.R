# Carregar pacotes necessários
library(harbinger)
library(dplyr)
library(ggplot2)
library(daltoolbox)

# Carregar o dataset gecco_sample
data("gecco_sample")  # Ou carregar manualmente

dataset <- data.frame(serie = gecco_sample$ph, event = as.logical(gecco_sample$event))
dataset <- dataset[!is.na(dataset$serie), ]
dataset$serie <- as.numeric(dataset$serie)

# Lista de modelos a serem testados
modelos <- list(
  han_autoencoder_aae = han_autoencoder(3, 2, aae_encode_decode, num_epochs = 500),
 #han_ml_conv1d = hanr_ml(ts_conv1d(ts_norm_gminmax(), input_size=4, epochs=10000)),
  #han_ml_elm = hanr_ml(ts_elm(ts_norm_gminmax(), input_size=4, nhid=3, actfun="purelin")),
  han_ml_lstm = hanr_ml(ts_lstm(ts_norm_gminmax(), input_size=4, epochs=12000)),
  #han_autoencoder_vanilla = han_autoencoder(3,2),
  han_autoencoder_sae = han_autoencoder(3, 2, sae_encode_decode, num_epochs = 500),
  han_autoencoder_cae = han_autoencoder(3, 2, cae_encode_decode, num_epochs = 500)
  #hanct_kmeans_anomaly = hanct_kmeans(1),
  #hanr_ensemble_fuzzy = har_ensemble(hanr_fbiad(), hanr_arima(), hanr_emd())
)

# Lista para armazenar os resultados
resultados_gerais <- list()

# Loop para percorrer cada modelo
total_execucoes <- 5
for (nome_modelo in names(modelos)) {
  cat("Executando modelo:", nome_modelo, "\n")
  
  results <- list()
  
  for (i in 1:total_execucoes) {
    cat("Execução", i, "\n")
    
    # Treinar o modelo
    modelo <- fit(modelos[[nome_modelo]], dataset$serie)
    
    # Fazer a detecção de anomalias
    detection <- detect(modelo, dataset$serie)
    
    # Avaliação
    TP <- sum(detection$event == TRUE & dataset$event == TRUE)
    TN <- sum(detection$event == FALSE & dataset$event == FALSE)
    FP <- sum(detection$event == TRUE & dataset$event == FALSE)
    FN <- sum(detection$event == FALSE & dataset$event == TRUE)
    
    precision <- TP / (TP + FP)
    recall <- TP / (TP + FN)
    accuracy <- (TP + TN) / (TP + FN + FP + TN)
    f1_score <- 2 * (precision * recall) / (precision + recall)
    specificity <- TN / (TN + FP)
    
    results[[i]] <- list(
      TP = TP, TN = TN, FP = FP, FN = FN,
      precision = precision, recall = recall,
      accuracy = accuracy, f1_score = f1_score,
      specificity = specificity
    )
    
    confMatrix <- matrix(c(TP, FN, FP, TN), nrow = 2, byrow = TRUE)
    colnames(confMatrix) <- c("Pred. Positivo", "Pred. Negativo")
    rownames(confMatrix) <- c("Verdadeiro Pos.", "Verdadeiro Neg.")
    
    cat("Matriz de Confusão:\n")
    print(confMatrix)
    
    cat("Precisão:", precision, "\n")
    cat("Revocação:", recall, "\n")
    cat("Acurácia:", accuracy, "\n")
    cat("F1-Score:", f1_score, "\n")
    cat("Especificidade:", specificity, "\n\n")
    
    # Plotar gráfico
    grf <- har_plot(modelo, dataset$serie, detection, dataset$event)
    plot(grf)
  }
  
  # Calcular médias
  avg_metrics <- lapply(c("precision", "recall", "accuracy", "f1_score", "specificity"), function(metric) {
    mean(sapply(results, function(res) res[[metric]]), na.rm = TRUE)
  })
  names(avg_metrics) <- c("Precisão Média", "Revocação Média", "Acurácia Média", "F1-Score Médio", "Especificidade Média")
  
  # Armazenar resultado do modelo
  resultados_gerais[[nome_modelo]] <- avg_metrics
}

# Exibir resultados finais
cat("Resultados finais de todos os modelos:\n")
print(resultados_gerais)
