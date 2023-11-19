###### III- MODELO ARIMA PARA PROYECCION DE VENTAS (MACHINE LEARNING)

paquetesML <- c("tidyverse", "tseries", "astsa", "forecast", "lmtest", "caret", "blorr")

# Verificar e instalar paquetes
for (paquete in paquetesML) {
  if (!requireNamespace(paquete, quietly = TRUE)) {
    install.packages(paquete)
  }
}

# Cargar los paquetes
sapply(paquetesML, require, character.only = TRUE)

library(tidyverse)
library(tseries)
library(astsa)
library(forecast)
library(stats)  #Para evalaur si el modelo se ajusta a los datos, si es bueno.

## Ventas mensuales

venta_mes <- Tabla_Central %>%
  left_join(c_tiempo, by = "Fecha") %>%
  group_by(anio_mes) %>%
  summarise(Venta_neta = round(sum(Venta_Neta, na.rm = TRUE) / 1000000, 3))

Venta_m <- venta_mes %>% 
  select(Venta_neta)

##  Convirtiendo el dataframe Venta_m es serie de tiempo
Venta_ts <- ts(Venta_m, start = c(2020,1), frequency = 12)
Venta_ts
par(mar = c(3, 2, 2, 1) + 0.1) #graficando la serie een valores de niveles
plot(Venta_ts)

## Evaluando si mi serie de tiempo es estacionaria en niveles

#Prueba de Dickey-Fuller para raiz unitaria (para AR)
## Ho : NO es estacionaria (raíz unitaria) > 0.5
## H1 : Es estacionaria                    < 0.5


adf.test(Venta_ts) # El p_value = 0.4866 >5%, No es estacionaria
# Para corregir diferenciaremos la serie

## Evaluando en primera diferencia
Venta_dif <- diff(Venta_ts)
Venta_dif
plot(Venta_dif)
adf.test(Venta_dif) # El p_value = 0.01962 <5%, Es estacionaria en 1ra diferencia
# Todos los datos se encuentran al rededor del valor CERO

## Para poder pronosticar Ventas, la serie debe ser estacionaria
## Lo logramos con la primera difernecia, por lo que realizaremos el modelo ARIMA    
## AR (p) : Una parte autoregresiva, la regresion de la variable de interes contra valores pasado de ella misma.  
## I (d)  : Es la diferenciacion de la serie para hacerla estacionaria.
## MA (q) : Media movil ponderada de los errores pasado del pronostico.
##          El modelaje conlleva errores, ponderada porque los errore resientes van afectar mas al pronostico
#Tenemos un modelo (p,d,q)


# Calculando los p, d, q optimos
par(par(mar = c(1, 1, 1, 1)))
acf(Venta_dif)
pacf(Venta_dif)
acf(ts(Venta_dif,frequency = 1)) # Me da la media movil= 1 observ se sale
pacf(ts(Venta_dif,frequency = 1)) # Me da autoregresivo =1 observ se sale

# Abre una nueva ventana gráfica con un tamaño específico
dev.new(width = 7, height = 7)

# Configura los márgenes a un valor mínimo
par(mar = rep(2, 4))

# Estimación del modelo ARIMA
Modelo_V <- arima(Venta_ts, order = c(1,1,1))

# Diagnóstico del modelo ARIMA
tsdiag(Modelo_V)


# Con la prueba Ljung Box  Test  compruebo si hay Ruido Blanco que significa
# Erro con media cero, Varianza constante, No estan serialmente correlacionados
# H0: Ruido blanco          p> 0.5
# H1: No hay ruido blanco   p<0.5

#El resultado brinda que p values mayores a 0.05, Se acepta H0, hay ruido blanco
# El modelo se ajusta 

Box.test(residuals(Modelo_V))

#Obteniendo los residuos para ver si tienen media cero
error<- residuals(Modelo_V)
plot(error)


# PRonostico
pronostico <- forecast (Modelo_V, h=12)
pronostico
plot(pronostico)
MLventas <- plot(pronostico)