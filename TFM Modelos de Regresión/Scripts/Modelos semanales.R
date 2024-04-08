

### Modelos de manera semanal


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
semana <- read.csv("datos/semana.csv", check.names = F)


semana <- semana[, -c(1:3)]

set.seed(123) # Fijar una semilla para reproducibilidad

indices <- sample(seq_len(nrow(semana)),
                  size = floor(0.9 * nrow(semana)))

# Crear los subconjuntos de datos
trainsemana <- semana[indices, ] 
testsemana <- semana[-indices, ]


### predicción con los modelos estadisticos

## modelos lineales
modelo_lineal <- lm(`HH_En_la_Maquina` ~ .,
                    data = trainsemana)

### modelos lineales generalizados
modelo_glm <- glm(`HH_En_la_Maquina` ~ .,
                  data = trainsemana)



## modelo de regresión svr

modelo_svr <- svm(`HH_En_la_Maquina` ~ .,
                  data = trainsemana,
                  type = "eps-regression",
                  kernel = "linear",
                  cost = 0.1,
                  epsilon = 0.1)


## modelo de random forest
modelo_rf <- randomForest(`HH_En_la_Maquina` ~ ., data = trainsemana,
                          ntree = 500, mtry = sqrt(ncol(trainsemana)))


modelo_arbol <- rpart(`HH_En_la_Maquina` ~ ., data = trainsemana, 
                      method = "anova",
                      control = rpart.control(minsplit = 10, cp = 0.001, maxdepth = 5))


## modelos 

modelos <- list(modelo_lineal, modelo_glm, modelo_svr, 
                modelo_arbol, modelo_rf)



resultados <- lapply(modelos, function(modelo) {
  predicciones <- predict(modelo, newdata = testsemana)
  real <- testsemana$HH_En_la_Maquina  # Asegúrate de reemplazar 'respuesta_real' con el nombre real de tu columna
  
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



