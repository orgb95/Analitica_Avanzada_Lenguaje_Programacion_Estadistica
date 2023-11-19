############## MODELOS DE DESERCION (Machine Learning)#####################
paquetesML <- c("tidyverse", "tseries", "astsa", "forecast", "lmtest", "caret", "blorr")

# Verificar e instalar paquetes
for (paquete in paquetesML) {
  if (!requireNamespace(paquete, quietly = TRUE)) {
    install.packages(paquete)
  }
}

# Cargar los paquetes
sapply(paquetesML, require, character.only = TRUE)

### I- CLIENTE UNICO D3- MODELO LOGISTICA BINARIA PARA DETERMINAR LOS FACTORES DE DESERCION DEL CLIENTE


# 1.1 Generar la data a emplearse en el modelo CLIENTE UNICO


## Agregando, baja, id cliente, Antiguedad
M_baja_anti <- Tabla_Central %>%
  left_join(c_tiempo, by = "Fecha") %>%
  left_join(c_cliente, by = "Id_Cliente") %>%
  select(Baja, Id_Cliente, Antiguedad) %>%
  distinct() %>%
  group_by(Baja, Id_Cliente) %>%
  summarise(Antiguedad = round(mean(Antiguedad, na.rm = TRUE), 0))



## Agregando, valor de compra
M_ventas <- Tabla_Central %>%
  group_by(Id_Cliente) %>%
  summarise(VentaNeta = round(sum(Venta_Neta, na.rm = TRUE)/1000000,2),
            Devuelta_unidad = sum(Cantidad_Devuelta, na.rm = TRUE)*-1,
            Bonifi_unid = sum(CantidadBonificada, na.rm = TRUE),
            Descuento_mm = sum(Descuento, na.rm = TRUE))


## Agregando cantidad de sucursal que visita el cliente
M_sucursal <- Tabla_Central %>%
  left_join(c_sucursal, by = "Id_Sucursal") %>%
  select(Id_Cliente, Sucursald) %>%
  distinct() %>%
  group_by(Id_Cliente) %>%
  summarise(Q_sucursal = n(), .groups = "drop")


## Agregando cantidad de zonas del cliente    
M_zona <- Tabla_Central %>%
  left_join(c_zona, by = "Id_Zona") %>%
  select(Id_Cliente,NombreZona) %>% 
  distinct() %>%
  group_by(Id_Cliente,NombreZona)

M_zona <- M_zona %>%
  distinct() %>%
  summarise(Q_zona = n()) 

## Agregando cantidad de gamas que compar el cliente    
M_tipoprod <- Tabla_Central %>%
  left_join(c_tipoProducto, by = "Id_TipoProducto") %>%
  select(Id_Cliente,Gama) %>% 
  distinct() %>%
  group_by(Id_Cliente,Gama)

M_tipoprod <- M_tipoprod %>%
  distinct() %>%
  select(-Gama) %>% 
  group_by(Id_Cliente) %>% 
  summarise(Q_gama = n())

## Agregando cantidad de tipo de transporte que utiliza el cliente    
M_tipotransp <- Tabla_Central %>%
  left_join(c_tipoTransporte, by = "Id_TipoTransporte") %>%
  select(Id_Cliente,TipoTransporte) %>% 
  distinct() %>%
  group_by(Id_Cliente,TipoTransporte)

M_tipotransp <- M_tipotransp %>%
  distinct() %>%
  select(-TipoTransporte) %>% 
  group_by(Id_Cliente) %>% 
  summarise(Q_transporte = n())


## Unificando variables para crear la base par el modelo Cliente unico

M_clienteU <- M_baja_anti %>%
  left_join(M_sucursal, by = "Id_Cliente") %>%
  left_join(M_zona, by = "Id_Cliente") %>%
  left_join(M_tipoprod, by = "Id_Cliente") %>%
  left_join(M_tipotransp, by = "Id_Cliente") %>%
  left_join(M_ventas, by = "Id_Cliente") %>%
  select(-NombreZona) 

M_clienteU <- M_clienteU %>% 
  select(-Id_Cliente,-Q_zona)

#Verificando el tipo de variable
str(M_clienteU)

# 2 Corriendo el modelo 3
D3<- glm(Baja~. ,data=M_clienteU,family = "binomial")
summary(D3)
# 2.1 Ecuacion del modelo
####### log(p/(1-p)) = B0 + B1*X1 + B2*X2+B3*X3...+E
####### log(p/(1-p)) = -2.049 + 1.262antiguedad -4.532qsucursal + -8.193 qgama
###################### -4.299 qtransporte + 3.045 ventas + 4.473 devolucion
###################### -7.682 bonificacion + 1.333 descuento

# 3. Transformando los coeficientes del modelo logistico a traves de exp(D3$coefficients) para su lectura  
#Calculo de los odds ratio

round(exp(D3$coefficients),4)

#(Intercept)  Antiguedad   Q_sucursal      Q_gama     Q_transporte     VentaNeta 
#0.1288720     1.1345     0.6356        0.9213        0.6506           1.3560 

#  Devuelta_unidad   Bonifi_unid   Descuento_mm 
# 1.0045              0.9261        1.0000 


# a. En la medida que aumenta un año mas de Antiguedad, el chanse que se presente la desercion
# del cliente aumenta en 1.13 veces, en otras palabras la probabilidad es de un 13%.

#b.Al comprar, el cliente, en una sucursal adicional la probabilidad de retirarse de la empresa
# se reduce en un 36.44%.

#c. En la medida que el cliente aumente su nivel de compra en C$ 1 millon, la probabilidad de 
# desercion aumenta en un 35.60%.

#d. A medida que aumenta las unidades de bonificacion, la probabilidad de desercion del cliente
# se reduce en un 7.39%.


# 4. Probando la bondad de ajuste del modelo Test Razon de berosimilitud (Chiq2)
### H0: El modelo no se ajusta a los datos (no hay variable que influyan)
### H1: El modelo si se ajusta a los datos

D0<-glm(Baja~1, data=M_clienteU,family = "binomial")
lrtest<-lrtest(D3,D0)
lrtest

## Resultado Chiq2 = 2.2, por lo que se rechaza la hipotesis nula, 
# El modelo es significativo al 99% de confianza.


## 5. Midiendo el nivel de Precision del modelo

head(predict(D3,M_clienteU,type="response"))
ypred3<-ifelse(predict(D3,M_clienteU,type="response")>0.056,1,0)

## 6. Matriz de confusion e indicadores de precision
confusionMatrix(table(ypred3,M_clienteU$Baja),positive = "1")
#Nos da: 
# Matriz de confusion
# Indice Accuracy
# Un rango de confianza al 95%
# Indice Sensitivity: % positivos correcto los que desiertan
# Indice Specificity: % correcto a los que no se retiran.
# El modelo predice en un 74% los datos analizados.
# El nivel de prediccion de que se retiran es del 76% y los que no 73%.


# 7. Analisis de variables
summary(M_clienteU$Antiguedad) # Extrae cuartiles, mediana, media, min y max
#Min 1, 1q =3, med=4, Mean= 5.5, 3q = 8, Max=22
summary(M_clienteU$Bonifi_unid)

### Midiendo el efecto Antiguedad
### Solo se deja con valores la Antiguedad, todo lo demas no varia, dejando valor promedio
new_antiguedad<-expand.grid(Antiguedad=seq(1,22,4),
                            Q_sucursal=mean(M_clienteU$Q_sucursal),
                            Q_gama= mean(M_clienteU$Q_gama),
                            Q_transporte= mean(M_clienteU$Q_transporte),
                            VentaNeta=mean(M_clienteU$VentaNeta),
                            Devuelta_unidad=mean(M_clienteU$Devuelta_unidad),
                            Bonifi_unid=mean(M_clienteU$Bonifi_unid),
                            Descuento_mm=mean(M_clienteU$Descuento_mm))

str(new_antiguedad$Q_gama)
### Almacenamos la probabilidad del nuevo escenario new_antiguedad
m_antiguedad <- cbind(new_antiguedad, predict(D3, new_antiguedad, type = "response", se.fit = TRUE))
m_antiguedad$LL <- m_antiguedad$fit - 1.96 * m_antiguedad$se.fit
m_antiguedad$UL <- m_antiguedad$fit + 1.96 * m_antiguedad$se.fit

ggplot(m_antiguedad, aes(x = Antiguedad, y = fit)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), fill = "blue", alpha = 0.25) +
  geom_line(color = "red", size = 2.5) +
  labs(
    y = "Probabilidad",
    x = "Antigüedad",
    title = "Probabilidad de deserción por antigüedad",
  ) +
  scale_x_continuous(breaks = seq(2, 22, by = 2)) +
  scale_y_continuous(breaks = seq(0.1, 0.2, by = 0.1), limits = c(0, 0.2)) +
  theme_minimal()

# La grafica muestra que a medida que aumenta la Antiguedad del cliente en la empresa, la probabilidad
# de desercion aumenta.



### II- POR SUCURSAL - MODELO LOGISTICA BINARIA PARA DETERMINAR LOS FACTORES DE DESERCION DEL CLIENTE

# 2- Generar la data a emplearse en el modelo 

B_desercion <- Tabla_Central %>%
  left_join(c_tiempo, by = "Fecha") %>%
  left_join(c_tipoCliente, by = "Id_TipoCliente") %>%
  left_join(c_cliente, by = "Id_Cliente") %>%
  left_join(c_sucursal, by = "Id_Sucursal") %>%
  left_join(c_tipoProducto, by = "Id_TipoProducto") %>%
  left_join(c_tipoTransporte, by = "Id_TipoTransporte") %>%
  left_join(c_zona, by = "Id_Zona") %>%
  select(Baja, Tip_cliente, Antiguedad, Sucursald, Gama, TipoTransporte, NombreZona, Venta_Neta, Cantidad_Devuelta, CantidadBonificada, Descuento) %>% 
  group_by(Baja, Tip_cliente, Antiguedad, Sucursald, Gama, TipoTransporte, NombreZona) %>% 
  summarise(
    Venta = sum(round(sum(Venta_Neta, na.rm = TRUE) / 1000000, 2)),
    Devolu_unid = sum(round(sum(Cantidad_Devuelta, na.rm = TRUE), 0)),
    Bonifi_unid = sum(round(sum(CantidadBonificada, na.rm = TRUE), 0)),
    Descuento_unid = sum(round(sum(Descuento, na.rm = TRUE), 0))
  ) %>% 
  ungroup() %>%  # Desagrupar para evitar el mensaje de advertencia
  distinct()

B_desercion$Antiguedad<- round(B_desercion$Antiguedad, 0) # Redondeando antiguedad

B_desercion2<-B_desercion %>% 
  select(-Tip_cliente,-Devolu_unid,-Descuento_unid)

#Verificando el tipo de variable
str(B_desercion)

# 3 Corriendo el modelo 1
D1<- glm(Baja~. ,data=B_desercion,family = "binomial")
summary(D1)


# 4 Corriendo el modelo 2, con variables significativas
D2<- glm(Baja~., ,data=B_desercion2,family = "binomial")
summary(D2)

# 5. Transformando los coeficientes del modelo logistico a traves de exp(D3$coefficients) para su lectura  
#Calculo de los odds ratio

round(exp(D2$coefficients),4)


#6-  Probando la bondad de ajuste del modelo Test Razon de berosimilitud (Chiq2)
### H0: El modelo no se ajusta a los datos (no hay variable que influyan)
### H1: El modelo si se ajusta a los datos

D0<-glm(Baja~1, data=B_desercion,family = "binomial")
lrtest<-lrtest(D2,D0)
lrtest

## Resultado Chiq2 = 2.2, por lo que se rechaza la hipotesis nula, 
# El modelo es significativo al 99% de confianza.


## 7- Midiento el nivel de Precision del modelo
head(predict(D2,B_desercion2,type="response"))
ypred<-ifelse(predict(D2,B_desercion2,type="response")>0.04,1,0)

## 8- Matriz de confusion e indicadores de precision
confusionMatrix(table(ypred,B_desercion2$Baja),positive = "1")
#Nos da: 
# Matriz de confusion
# Indice Accuracy
# Un rango de confianza al 95%
# Indice Sensitivity: % positivos correcto los que desiertan
# Indice Specificity: % correcto a los que no se retiran.


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
