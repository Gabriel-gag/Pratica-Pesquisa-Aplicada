# Harbinger Package
# version 1.1.707



#loading Harbinger
library(daltoolbox)

library(harbinger)

data(examples_anomalies)

#Using the simple time series
dataset <- examples_anomalies$simple
head(dataset)

##       serie event
## 1 1.0000000 FALSE
## 2 0.9689124 FALSE
## 3 0.8775826 FALSE
## 4 0.7316889 FALSE
## 5 0.5403023 FALSE
## 6 0.3153224 FALSE

#ploting the time series
plot_ts(x = 1:length(dataset$serie), y = dataset$serie)


# establishing han_autoencoder method
model <- han_autoencoder(3, 2, aae_encode_decode, num_epochs = 1500)

# fitting the model
model <- fit(model, dataset$serie)

# making detections
detection <- detect(model, dataset$serie)

# filtering detected events
print(detection |> dplyr::filter(event==TRUE))

##   idx event    type
## 1  19  TRUE anomaly
## 2  44  TRUE anomaly
## 3  50  TRUE anomaly
## 4  69  TRUE anomaly
## 5  94  TRUE anomaly

# evaluating the detections
evaluation <- evaluate(model, detection$event, dataset$event)
print(evaluation$confMatrix)

##           event
## detection TRUE  FALSE
## TRUE      1     4
## FALSE     0     96

# ploting the results
grf <- har_plot(model, dataset$serie, detection, dataset$event)
plot(grf)



# ploting the results
res <-  attr(detection, "res")
plot(res)

saveRDS(model, "han_autoencoder_aae.rds")


