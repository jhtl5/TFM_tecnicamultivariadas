
### Modelos de manera diaria


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
diario <- read.csv("datos/diario.csv", check.names = F)

## normalizar datos

diario <- diario[, -c(1)]


colnames(diario) <- c("Disciplina", "Epoca_del_año", "Cumplimiento_Estrategia", 
                     "Cumplimiento_Programa", "Ready_Backlog", "HH_Actv_Gnerales", 
                     "HH_En_la_Maquina", "Otras_HH")
                      
                      

set.seed(123) # Fijar una semilla para reproducibilidad

indices <- sample(seq_len(nrow(diario)),
                  size = floor(0.9 * nrow(diario)))

# Crear los subconjuntos de datos
traindiario <- diario[indices, ] 
testdiario <- diario[-indices, ]


### predicción con los modelos estadisticos

## modelos lineales
modelo_lineal <- lm(`HH_En_la_Maquina` ~ .,
   data = traindiario)



summary(modelo_lineal)

### modelos lineales generalizados
modelo_glm <- glm(`HH_En_la_Maquina` ~ ., 
                  data = traindiario)



## modelo de regresión svr

modelo_svr <- svm(`HH_En_la_Maquina` ~ .,
                  data = traindiario,
    type = "eps-regression",
    kernel = "linear",
    cost = 0.1,
    epsilon = 0.1)


## modelo de random forest
modelo_rf <- randomForest(`HH_En_la_Maquina` ~ ., data = traindiario,
                             ntree = 500, mtry = sqrt(ncol(traindiario)))


modelo_arbol <- rpart(`HH_En_la_Maquina` ~ ., data = traindiario, 
                      method = "anova",
      control = rpart.control(minsplit = 10, cp = 0.001, maxdepth = 5))


## modelos 

modelos <- list(modelo_lineal, modelo_glm, modelo_svr, 
                modelo_arbol, modelo_rf)



resultados <- lapply(modelos, function(modelo) {
  predicciones <- predict(modelo, newdata = testdiario)
  real <- testdiario$HH_En_la_Maquina  
  
  rmse <- rmse(predicciones, real) |> round(4)
  mse <- mean((predicciones - real)^2) |> round(4)
  mae <- mae(predicciones, real) |> round(4)
  
  return(list(RMSE = rmse, MSE = mse, MAE = mae)) |> round(4)
})


# Convertir la lista en un data frame
df <- do.call(rbind, lapply(resultados, function(x) unlist(x))) |> as.data.frame()

# Agregar nombres a las filas y columnas
rownames(df) <- NULL
colnames(df) <- c("RMSE", "MSE", "MAE")

df$MODELO <- c("lineal", "glm", "svr", "arbol", "rf")

df <- dplyr::select(df, c("MODELO", "RMSE", "MSE", "MAE"))



