
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

### cargamos la base de datos

datos <- readxl::read_excel("../datos/DataTFM.XLSX", sheet = "Base de datos")


## organizamos la data de acuerdo a le fecha de ejecución

datos <- datos[order(datos$fe.Ejecutada), ]


## Colnames
colnames(datos) <- c("Epoca del año", "estrategia Mtto",
                     "Programa Mtto", "Ready back log",
                     "HH Actv Gnerales", "HH En la Maquina", 
                     "Otras HH", "Tipo mantenimiento", 
                     "Disciplina", "Fecha Creada Orden", 
                     "fecha Ejecutada")


## eliminar datos faltantes
datos <- na.omit(datos)



### analisis de valores atipicos

par(mfrow = c(2, 3))


boxplot(datos$`HH Actv Gnerales`, 
        main = "HH Actv Gnerales")


boxplot(datos$`HH En la Maquina`, 
        main = "HH En la Maquina")


boxplot(datos$`Otras HH`,
        main = "Otras HH")

boxplot(datos$`Programa Mtto`, 
        main = "Programa mantenimiento")

boxplot(datos$`Ready back log`,
        main = "Ready back log")

boxplot(datos$`estrategia Mtto`,
        main = "Estrategia de mantenimiento")



### función para corregir valores atipicos 

reemplazar_atipicos_con_mediana <- function(x) {
  # Calcular la mediana y los límites para identificar valores atípicos
  mediana_x <- median(x)
  limite_inferior <- mediana_x - 3 * mad(x, constant = 1)
  limite_superior <- mediana_x + 3 * mad(x, constant = 1)
  
  # Reemplazar los valores atípicos por la mediana
  x_atipicos_reemplazados <- ifelse(x < limite_inferior | x > limite_superior, mediana_x, x)
  
  return(x_atipicos_reemplazados)
}





numericas <- c("HH Actv Gnerales", "HH En la Maquina", "Otras HH", 
               "Programa Mtto", "Ready back log", "estrategia Mtto")


datos <- datos %>%
  mutate(across(numericas, reemplazar_atipicos_con_mediana))


par(mfrow = c(2, 3))


boxplot(datos$`HH Actv Gnerales`, 
        main = "HH Actv Gnerales")


boxplot(datos$`HH En la Maquina`, 
        main = "HH En la Maquina")


boxplot(datos$`Otras HH`,
        main = "Otras HH")

boxplot(datos$`Programa Mtto`, 
        main = "Programa mantenimiento")

boxplot(datos$`Ready back log`,
        main = "Ready back log")

boxplot(datos$`estrategia Mtto`,
        main = "Estrategia de mantenimiento")




## filtramos los datos unicamente por el mantenimiento correctivo


datos <- datos[datos$`Tipo mantenimiento` == "Correctivo", ]

### creamos 
diario <- datos %>%
  group_by(`fecha Ejecutada`, Disciplina, `Epoca del año`) %>%
  summarize(
    Cumplimiento_Estrategia =
      mean(`estrategia Mtto`),
    Cumplimiento_Programa = mean(`Programa Mtto`),
    Ready_Backlog = mean(`Ready back log`),
    HH_Actv_Gnerales = mean(`HH Actv Gnerales`),
    HH_En_la_Maquina = mean(`HH En la Maquina`),
    Otras_HH = mean(`Otras HH`)
  )

## eliminamos posibles valores atípicos de las variables numéricas

diario[, 4:9] <- round(diario[, 4:9], 2)


## Guardamos la base de datos diaria

write.csv(diario, "datos/diario.csv", row.names = F)


### agrupación semanal

## usamos las funciones de week y year

datos$semana <- paste0(year(datos$`fecha Ejecutada`), "-",
                       week(datos$`fecha Ejecutada`))


### creamos 
semana <- datos %>%
  group_by(semana, Disciplina, `Epoca del año`) %>%
  summarize(
    Cumplimiento_Estrategia =
      mean(`estrategia Mtto`),
    Cumplimiento_Programa = mean(`Programa Mtto`),
    Ready_Backlog = mean(`Ready back log`),
    HH_Actv_Gnerales = mean(`HH Actv Gnerales`),
    HH_En_la_Maquina = mean(`HH En la Maquina`),
    Otras_HH = mean(`Otras HH`)
  )


## eliminamos posibles valores atípicos de las variables numéricas

semana[, 4:9] <- round(semana[, 4:9], 2)


write.csv(semana, "datos/semana.csv", row.names = F)
