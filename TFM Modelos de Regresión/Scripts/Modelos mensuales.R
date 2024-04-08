### Modelos de manera mensuales


## paquetes necesarios
library(tidyverse)
library(readxl)
library(DT)
library(lmtest)
library(car)
library(lme4)
library(lubridate)
library(MuMIn)
library(ggthemes)
library(Metrics)
library(e1071)
library(randomForest)
library(rpart)

## cargar base de datos
mes <- read.csv("datos/mes.csv", check.names = F)


mes <- mes[, -c(1:3)]

set.seed(123) # Fijar una semilla para reproducibilidad

indices <- sample(seq_len(nrow(mes)),
                  size = floor(0.9 * nrow(mes)))

# Crear los subconjuntos de datos
trainmes <- mes[indices, ] 
testmes <- mes[-indices, ]


### predicción con los modelos estadisticos

## modelos lineales
modelo_lineal <- lm(`HH_En_la_Maquina` ~ .,
                    data = trainmes)

### modelos lineales generalizados
modelo_glm <- glm(`HH_En_la_Maquina` ~ .,
                  data = trainmes)



## modelo de regresión svr

modelo_svr <- svm(`HH_En_la_Maquina` ~ .,
                  data = trainmes,
                  type = "eps-regression",
                  kernel = "linear",
                  cost = 0.1,
                  epsilon = 0.1)


## modelo de random forest
modelo_rf <- randomForest(`HH_En_la_Maquina` ~ ., data = trainmes,
                          ntree = 500, mtry = sqrt(ncol(trainmes)))


modelo_arbol <- rpart(`HH_En_la_Maquina` ~ ., data = trainmes, 
                      method = "anova",
                      control = rpart.control(minsplit = 10, cp = 0.001, maxdepth = 5))


## modelos 

modelos <- list(modelo_lineal, modelo_glm, modelo_svr, 
                modelo_arbol, modelo_rf)



resultados <- lapply(modelos, function(modelo) {
  predicciones <- predict(modelo, newdata = testmes)
  real <- testmes$HH_En_la_Maquina  # Asegúrate de reemplazar 'respuesta_real' con el nombre real de tu columna
  
  rmse <- rmse(predicciones, real)
  mse <- mean((predicciones - real)^2)
  mae <- mae(predicciones, real)
  
  return(list(RMSE = rmse, MSE = mse, MAE = mae))
})


# Convertir la lista en un data frame
df <- do.call(rbind, lapply(resultados, function(x) unlist(x))) |> as.data.frame()

# Agregar nombres a las filas y columnas
rownames(df) <- NULL
colnames(df) <- c("RMSE", "MSE", "MAE")

df$MODELO <- c("lineal", "glm", "svr", "arbol", "rf")

df