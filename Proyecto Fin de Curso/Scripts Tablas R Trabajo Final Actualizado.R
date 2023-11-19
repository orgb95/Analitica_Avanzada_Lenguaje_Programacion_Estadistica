##### TRABAJO FINAL #####

# 1 - Instalando paquetes y librerias -----

# Paquetes necesarios
paquetes <- c("readr", "randomNames", "lubridate", "ggplot2", "dplyr", "readxl", "tidyr","data.table")

# Verificar e instalar paquetes
for (paquete in paquetes) {
  if (!requireNamespace(paquete, quietly = TRUE)) {
    install.packages(paquete)
  }
}

# Cargar los paquetes
sapply(paquetes, require, character.only = TRUE)


# 2 - Cargar los datos desde el archivo CSV usando la función read.csv y read.xl ----


##Base de datos Venta

url_DB_csv <- "https://www.dropbox.com/scl/fi/rhvtz8ud3dq6cgb8k0umd/data_anom.csv?rlkey=7n6cnhjq8f83blxjy0rk0nv5d&dl=1"
BD <- fread(url_DB_csv)

gallo <- BD
#Validacion de los datos
dim(gallo);
head(gallo);

##Convertir la columna 'Fecha' a formato Date con año-mes-dia
gallo$Date <- as.Date(gallo$Date, format = "%Y-%m-%d")
gallo$FechaIngresoCliente <- as.Date(gallo$FechaIngresoCliente, format = "%Y-%m-%d")

#Validacion de la transformacion a Date
str(gallo$Date)
str(gallo$FechaIngresoCliente)

##Renombrar columna Date a Fecha
gallo <- gallo %>%
  rename(Fecha = Date)

##Renombrar columna Numero de factura
gallo <- gallo %>%
  rename(X.NumeroFactura = "#NumeroFactura")
           
           
           # 3 - Generando la fecha de ingreso y antiguedad del cliente ----
         
         ##Convertir la fecha_ingreso a formato de fecha
         gallo$fecha_ingreso <- gallo$FechaIngresoCliente
         
         ##Extraer año, mes y día
         gallo$anio_ing <- year(gallo$fecha_ingreso)
         gallo$mes_ing <- month(gallo$fecha_ingreso)
         gallo$dia_ing <- day(gallo$fecha_ingreso)
         
         ##Calculando la antiguedad del cliente
         gallo$Antiguedad <- as.numeric(difftime(as.Date("2023-10-31"), gallo$FechaIngresoCliente, units = "days")) / 365
         
         ##Redondear a dos decimales
         gallo$Antiguedad <- round(gallo$Antiguedad, 2)
         
         
         # 4- CREANDO LAS TABLAS CATALOGO ----
         
         # 4.1 - Catalogo Cliente: ----
         
         
         ##Seleccionando los campos pertinentes y creando ID
         c_cliente <- gallo %>%
           select (NombreCliente,Antiguedad,anio_ing,mes_ing,dia_ing)%>%
           distinct() %>%
           arrange(NombreCliente) %>%
           mutate(Id_Cliente = row_number())%>%
           select(Id_Cliente, everything())
         #Comprueba id unico que no existan duplicados
         duplicados <- sum(duplicated(c_cliente$NombreCliente))
         duplicados
         head(c_cliente)
         
         ##Generar nombre de los clientes
         #Garantizando que no varien los nombres a generar
         set.seed(123)
         c_cliente$cliente <- randomNames(n = nrow(c_cliente))
         #Verificando clientes duplicados
         duplicados <- sum(duplicated(c_cliente$cliente))
         duplicados
         
         ##Genera nuevos nombres aleatorios para reemplazar los duplicados
         set.seed(123)  
         nuevos_nombres <- randomNames(n = duplicados) 
         
         ##Reemplaza los nombres duplicados con nuevos nombres aleatorios
         c_cliente$cliente[duplicated(c_cliente$cliente)] <- nuevos_nombres
         #Verifica que ya no existan duplicados después del reemplazo
         duplicados_despues <- sum(duplicated(c_cliente$cliente))
         duplicados_despues
         #Validacion de los datos
         head(c_cliente)
         
         
         # 4.2 - Catalogo TipoCliente: ----
         
         
         ##Seleccionando los tipos de clientes y creando IDs
         c_tipoCliente <- gallo %>%
           select (TipoCliente) %>%
           distinct() %>%
           mutate(Id_TipoCliente = row_number())%>%
           select(Id_TipoCliente, everything())
         
         ##Generando la descripcion de las sucursales
         Tip_cliente <- c("Cliente nervioso","Cliente descortés","Cliente exigente",
                          "Cliente crítico","Cliente tímido","Cliente conversador",
                          "Cliente renuente","Cliente amigable","Cliente insatisfecho","Cliente ocupado",
                          "Cliente negociador","Cliente impaciente","Cliente que discute","Cliente indeciso",
                          "Cliente conservador","Cliente fiel","Cliente entusiasta","Cliente confundido",
                          "Cliente impulsivo","Cliente distraído","Cliente indiferente",
                          "Cliente detallista","Cliente racional","Cliente mercenario",
                          "Cliente autosuficiente","Cliente reservado")
         
         ##Se anexa la descripcion de Tipo de cliente al catalogo Cliente 
         c_tipoCliente<- cbind(c_tipoCliente,Tip_cliente)
         #Validacion de la tabla catalogo
         head(c_tipoCliente)
         
         
         # 4.3 - Catalogo Sucursal: ----
         
         
         ##Seleccionando las sucursales y creando IDs
         c_sucursal <- gallo %>%
           select (Sucursal) %>%
           distinct() %>%
           mutate(Id_Sucursal = row_number())%>%
           select(Id_Sucursal, everything())
         
         ##Generando la descripcion de las sucursales
         Sucursald<- c("Leon","Chinandega","Carazo","Rivas","Esteli","Managua","Teotecasinte")
         
         ##Se anexa la descripcion de sucursal al catalogo sucursal 
         c_sucursal<- cbind(c_sucursal,Sucursald)
         #validacion de la tabla catalogo
         head(c_sucursal)
         
         
         # 4.4 - Catalogo TipoProducto: ----
         
         
         ##Seleccionando el campo tipo producto y creando ID
         c_tipoProducto <- gallo %>%
           select(TipoProducto)%>%
           distinct()%>%
           mutate (Id_TipoProducto = row_number())%>%
           select(Id_TipoProducto, everything())
         
         ##Generando la descripcion de gama o tipo de productos:
         #Gama gris:dispositivos, ambito informatico y telecomunicacion (TIC)
         #Gama Blanca: articulos para el hogar; 
         #Gama marron: aparatos de imagen y sonidos
         Gama <- c("Gama Gris","Gama Marron","Gama Blanca")
         
         ##Se anexa la descripcion de gama a Tipo de productos
         c_tipoProducto<- cbind(c_tipoProducto,Gama)
         #validacion de la tabla tipo producto
         head(c_tipoProducto)
         
         
         # 4.5 - Catalogo Producto: ----
         
         # URL del archivo CSV 
         url_GamaBlanca_csv <- "https://docs.google.com/uc?export=download&id=19q37paxYvmA7_JT7rXvc5FTD4Jd3o8oU"
         url_GamaMarron_csv <- "https://docs.google.com/uc?export=download&id=1j3vDP-oS43FbTkOASM9BHz2kdpsMBLsN"
         url_GamaGris_csv <- "https://docs.google.com/uc?export=download&id=15wVHh6EKXRl6Q4XaQCoIGqNcEISB1kZb"
         
         # Leer los datos directamente desde la URL
         ProductosGamaBlanca <- read.csv(url_GamaBlanca_csv)
         ProductosGamaMarron <- read.csv(url_GamaMarron_csv)
         ProductosGamaGris <- read.csv(url_GamaGris_csv)
         
         
         consulta_gallo <- gallo %>%
           select(TipoProducto, NombreProducto) %>%
           distinct()
         
         consulta_gallo <- consulta_gallo %>%
           left_join(c_tipoProducto, by = c("TipoProducto" = "TipoProducto")) %>% 
           distinct()
         
         
         # Para Tipo1:
         nombres_tipo1 <- consulta_gallo %>%
           filter(Gama == "Gama Gris") %>%
           select(NombreProducto) %>%
           distinct()
         
         # Para Tipo2:
         nombres_tipo2 <- consulta_gallo %>%
           filter(Gama == "Gama Marron") %>%
           select(NombreProducto) %>%
           distinct()
         
         # Para Tipo3:
         nombres_tipo3 <- consulta_gallo%>%
           filter(Gama  == "Gama Blanca") %>%
           select(NombreProducto) %>%
           distinct()
         
         
         # Mostrar los nombres de productos para cada tipo de producto
         head(nombres_tipo1)
         head(nombres_tipo2)
         head(nombres_tipo3)
         
         
         ProductosTipo1 <- cbind(nombres_tipo1, ProductosGamaGris)
         ProductosTipo2 <- cbind(nombres_tipo2, ProductosGamaMarron)
         ProductosTipo3 <- cbind(nombres_tipo3, ProductosGamaBlanca)
         
         
         Productos <- consulta_gallo %>%
           left_join(ProductosTipo1, by = c("NombreProducto" = "NombreProducto")) %>% 
           distinct()
         
         Productos <- Productos %>%
           left_join(ProductosTipo2, by = c("NombreProducto" = "NombreProducto")) %>% 
           distinct()
         
         # Reemplazar columnas con sufijos .x y .y por un solo nombre
         Productos$Nombre <- ifelse(!is.na(Productos$Nombre.x), Productos$Nombre.x, Productos$Nombre.y)
         
         # Eliminar las columnas 'Nombre.x' y 'Nombre.y' ya que hemos consolidado la información en 'Nombre'
         Productos <- subset(Productos, select = -c(Nombre.x, Nombre.y))
         
         Productos <- Productos %>%
           left_join(ProductosTipo3, by = c("NombreProducto" = "NombreProducto")) %>% 
           distinct()
         
         
         # Reemplazar columnas con sufijos .x y .y por un solo nombre
         Productos$Nombre <- ifelse(!is.na(Productos$Nombre.x), Productos$Nombre.x, Productos$Nombre.y)
         
         # Eliminar las columnas 'Nombre.x' y 'Nombre.y' ya que hemos consolidado la información en 'Nombre'
         Productos <- subset(Productos, select = -c(Nombre.x, Nombre.y))
         
         
         c_producto <- gallo %>%
           select(NombreProducto) %>%
           distinct() %>%
           mutate (Id_Producto = row_number())%>%
           select(Id_Producto, everything())
         
         c_producto <- c_producto %>%
           left_join(Productos,by = c("NombreProducto" = "NombreProducto"))%>% 
           select(Id_Producto,NombreProducto,Nombre)
         
         head(c_producto)
         
         
         # 4.6 - Catalogo Marca: ----
         
         # URL del archivo CSV 
         url_marcas_csv <- "https://docs.google.com/uc?export=download&id=1su7k1hVBmRLa1gkDm1GRXe86EWRWmh59"
         
         # Leer los datos directamente desde la URL
         Marcas <- read.csv(url_marcas_csv)
         
         head (Marcas)
         
         c_marca <- gallo %>%
           select(Nombre_Marca)%>%
           distinct() %>%
           mutate (Id_Marca = row_number())%>%
           select(Id_Marca, everything())
         
         c_marca <- cbind(c_marca,Marcas)
         
         head(c_marca) 
         
         # 4.7 - Catalogo Zona: ----
         
         c_zona <- gallo %>%
           select(NombreZona) %>%
           distinct() %>%
           mutate(Id_Zona = row_number())%>%
           select(Id_Zona, everything())
         
         
         #Generando la descripcion de las zonas_ventas 
         
         zonas_ventas <- c("Centro Urbano Principal", "Distrito Financiero", "Distrito Comercial", "Áreas Residenciales de Clase Alta", 
                           "Distrito Industrial", "Distrito Tecnológico o de Innovación", "Áreas Residenciales de Clase Media Alta", 
                           "Áreas Comerciales Suburbanas", "Distrito de Entretenimiento", "Áreas Residenciales de Clase Media", 
                           "Área Industrial en Expansión", "Distrito de Oficinas Corporativas", "Áreas de Crecimiento Residencial", 
                           "Distrito Logístico", "Áreas Residenciales Emergentes", "Distrito de Salud y Educación", 
                           "Áreas Rurales o Agrícolas")
         
         #Se anexa la descripcion de sucursal al catalogo zona
         c_zona<- cbind(c_zona,zonas_ventas)
         head(c_zona)
         
         
         # 4.8 - Catalogo Canal Distribución:----
         
         c_canalDistribucion <- gallo %>%
           select (CanalDistribucion) %>%
           distinct() %>%
           mutate(Id_CanalDistribucion = row_number())%>%
           select(Id_CanalDistribucion, everything())
         
         #Generano la descripcion de canales de distribucion
         Canal <- c("Tiendas minoristas", "Catálogos y telemarketing",
                    "Distribuidoras y mayorista", "Ferias y eventos especiales",
                    "Comercio electrónico")
         
         
         #Se anexa la descripcion de canales de distribucion
         c_canalDistribucion<- cbind(c_canalDistribucion,Canal)
         
         head(c_canalDistribucion)
         
         # 4.9 - Catalogo Tipo Transporte: ----
         
         
         gallo$TipoTransporte[gallo$TipoTransporte == "Ugo"] <- "Hugo"
         
         
         c_tipoTransporte <- gallo %>%
           select(TipoTransporte) %>%
           distinct() %>%
           mutate(Id_TipoTransporte = row_number())%>%
           select(Id_TipoTransporte, everything())
         
         head(c_tipoTransporte)
         
         #Generano la descripcion de tipo de transporte
         #Transporte <- c("Pedidos Ya", "Hugo", "Nica Mandado")
         
         
         #Se anexa la descripcion del tipo de transporte
         #c_tipoTransporte<- cbind(c_tipoTransporte,Transporte)
         
         
         
         # 4.10 - Catalogo Tiempo ----
         
         # Creando un gallo de tiempo con el formato año-mes-día
         fecha_inicial <- as.Date("2020-01-01")
         fecha_final <- as.Date("2022-12-31")
         c_tiempo <- data.frame(Fecha = seq(fecha_inicial, fecha_final, by = "day"))
         
         # Agregar variables año, mes, dia, semana, trimestre y semestre al gallo
         c_tiempo$anio <- format(c_tiempo$Fecha, "%Y")
         c_tiempo$mes <- format(c_tiempo$Fecha, "%m")
         c_tiempo$día <- format(c_tiempo$Fecha, "%d")
         c_tiempo$semana <- format(c_tiempo$Fecha, "%V")
         c_tiempo$trimestre <- as.numeric(format(c_tiempo$Fecha, "%m")) %/% 4 + 1
         c_tiempo$semestre <- ifelse(as.numeric(c_tiempo$mes) <= 6, 1, 2)
         
         # Agregar un ID_tiempo único
         c_tiempo$ID_tiempo <- seq_along(c_tiempo$Fecha)
         
         #Agregando año, mes, trimestre y semestre como caracteres
         c_tiempo <- c_tiempo %>%
           mutate(
             aniot = as.character(anio),
             mest = as.character(mes),
             trimt = as.character(trimestre),
             semt = as.character(semestre),
           )
         
         # Combinando año-mes; año-trim, año-sem
         c_tiempo$anio_mes <- paste(c_tiempo$aniot, c_tiempo$mest, sep = "-")
         c_tiempo$anio_trim <- paste(c_tiempo$aniot, c_tiempo$trimt, sep = "-")
         c_tiempo$anio_sem <- paste(c_tiempo$aniot, c_tiempo$semt, sep = "-")
         
         
         # 5. CREACION DE TABLA TRANSACCIONAL ----
         
         # Uniendo los demas codigos de las tablas catalogo a la tabla gallo
         
         Tabla_Central <- gallo %>%
           left_join(c_cliente, by = c("NombreCliente" = "NombreCliente")) %>% 
           left_join(c_tipoCliente, by = c("TipoCliente" = "TipoCliente")) %>%
           left_join(c_sucursal, by = c("Sucursal" = "Sucursal")) %>%
           left_join(c_producto, by = c("NombreProducto" = "NombreProducto")) %>%
           left_join(c_tipoProducto, by = c("TipoProducto" = "TipoProducto")) %>%
           left_join(c_marca, by = c ("Nombre_Marca" = "Nombre_Marca"))%>%
           left_join(c_zona, by = c("NombreZona" = "NombreZona"))%>%
           left_join(c_canalDistribucion, by = c("CanalDistribucion" = "CanalDistribucion")) %>%
           left_join(c_tipoTransporte, by = c("TipoTransporte" = "TipoTransporte"))
         
         #left_join(c_tiempo, by = c("Fecha" = "Fecha"))
         
         # Seleccionando los campos para la tabla central o transaccional
         
         Tabla_Central <- Tabla_Central %>%
           select(Fecha, Id_Sucursal,  Id_Producto, Id_TipoProducto, Id_Cliente, Id_Marca, Id_Zona, Id_CanalDistribucion, Id_TipoTransporte, Id_TipoCliente, X.NumeroFactura, Cantidad, Cantidad_Devuelta, Descuento, CantidadBonificada, Cantidad_Total, Costo_Neto, Costo_Total, Venta_Neta, Venta_Total, Venta_Neta_USD, Costo_Neto_USD, FechaIngresoCliente, Baja) %>%
           mutate(
             Cantidad = as.numeric(Cantidad),
             Cantidad_Devuelta = as.numeric(Cantidad_Devuelta),
             Descuento = as.numeric(Descuento),
             CantidadBonificada = as.numeric(CantidadBonificada),
             Cantidad_Total = as.numeric(Cantidad_Total),
             Costo_Neto = as.numeric(Costo_Neto),
             Costo_Total = as.numeric(Costo_Total),
             Venta_Neta = as.numeric(Venta_Neta),
             Venta_Total = as.numeric(Venta_Total),
             Venta_Neta_USD = as.numeric(Venta_Neta_USD),
             Costo_Neto_USD = as.numeric(Costo_Neto_USD),
             FechaIngresoCliente = as.Date(FechaIngresoCliente),
             Baja = as.numeric(Baja)
           )
         head(Tabla_Central)
         
         
         # CREACION DE SCRIPT PARA KPI: ----
         
         #CREACION DE SCRIPT PARA KPI: 
         
         ## 1. PERSPECTIVA. FINANZAS
         
         ## 1.1 Venta anual neta
         ventas_N_anual <- Tabla_Central %>%
           left_join(c_tiempo, by = "Fecha") %>%
           left_join(c_sucursal, by = "Id_Sucursal") %>% 
           group_by(anio) %>%
           summarise(
             Venta_neta = round(sum(Venta_Neta, na.rm = TRUE) / 1000000, 2),
             Costo_neta = round(sum(Costo_Neto, na.rm = TRUE) / 1000000, 2)
           )
         
         # 1.2 Calculando el Margen Bruto anual
         Margen_bruto <- ventas_N_anual %>% 
           group_by(anio) %>%
           summarise(Margen_bruto = round((Venta_neta - Costo_neta) / Venta_neta, 4)*100)
         
         
         print(ventas_N_anual)
         
         
         ## INTENTANDO CREAR UNA TABLA QUE COMPARE LA META 2022 VS LO EFECTIVO 2022, PERO MEJOR LO PODEMOS
         #DEJAR PARA LA SECCION RSHINNYN (FILA 314 A LA 328)
         
         # Establecer la meta para el año 2022
         Meta_VN_2022 <- 1062
         
         # Comparar las ventas del año 2022 con la meta
         VentasN_2022 <- ventas_N_anual %>%
           filter(anio == 2022)
         
         if (VentasN_2022$Venta_neta > Meta_VN_2022) {
           mensaje <- "Verde"
         } else {
           mensaje <- "Rojo"
         }
         Ventas2022 <- VentasN_2022$Venta_neta
         
         # 1.3 Tasa de variacion
         var_VN <- ventas_N_anual %>%
           arrange(anio) %>%
           mutate(Var_Venta_porcent = round((Venta_neta - lag(Venta_neta)) / lag(Venta_neta) * 100, 2),
                  Var_Consto_porcent = round((Costo_neta - lag(Costo_neta)) / lag(Costo_neta) * 100, 2))
         
         
         VentaN_2022<- data.frame(
           Descripcion = c("VentasN_2022","Meta_2022"),
           Valor = c(VentasN_2022,Meta_VN_2022)
         )
         
         
         ## 1.4 Calculando el KPI CLTV
         
         # Duracion promedio de la relacion con los clientes
         
         ##Seleccionando los clientes que desertaron
         Desercion<- Tabla_Central %>%
           left_join(c_tiempo, by = "Fecha") %>%
           left_join(c_cliente, by = "Id_Cliente") %>% 
           filter(Tabla_Central$Baja == 1) %>% 
           select(Id_Cliente,Baja,anio,anio_ing)
         
         ## Calculando la tiempo que tarda la relacion cliente
         Desercion<- Desercion %>% 
           mutate(anio = as.numeric(anio),
                  T_relacion = round((anio-anio_ing),2))
         
         ##Calculando el promedio de la reacion cliente por año
         Prom_t_relacion<- Desercion %>%
           group_by(anio) %>% 
           summarise(promrela=mean(T_relacion)) %>% 
           mutate(anio1=as.character(anio))
         
         #Calculando el costo promedio de descuento y bonificacion
         Costo_boni_desc <- Tabla_Central %>%
           left_join(c_tiempo, by = "Fecha") %>%
           mutate(Costoboni_uni = ((Costo_Total/Cantidad_Total)*CantidadBonificada),
                  Costo_cl_adq= Costoboni_uni+Descuento)
         
         Costo_boni_desc <- Costo_boni_desc %>% 
           group_by(anio) %>% 
           summarise(VentaN = sum(Venta_Neta, na.rm = TRUE),
                     Cant_clien = n(),
                     Costoadq_prom = mean(Costo_cl_adq, na.rm = TRUE))
         
         ##Unificando gallo Costo bonificacio y descuento y promedio de relacion cliente
         CLTV <-Costo_boni_desc %>%
           left_join(Prom_t_relacion, by = c("anio" = "anio1"))
         
         ##Calculando el KPI CLTV
         CLTV <- CLTV %>%
           group_by(anio) %>%
           summarise(cltv = format(((VentaN / Cant_clien) * promrela) - Costoadq_prom, big.mark = ",", digits = 2))
         
         
         #UNIENDO TODOS LOS KPI FINANAS
         
         p_Fnanzas <- ventas_N_anual %>% 
           left_join(var_VN,by ="anio") %>%
           left_join(Margen_bruto,by ="anio") %>%
           left_join(CLTV,by ="anio")
         
         p_Fnanzas<- p_Fnanzas %>% 
           select(anio,Venta_neta.x,Costo_neta.y,Var_Venta_porcent,Var_Consto_porcent,Margen_bruto,cltv)
         
         
         # 2. PERSPECTIVA. CLIENTE
         
         # 2.1 Porcentaje de clientes retenidos
         clientes_fieles <- Tabla_Central %>%
           left_join(c_tiempo, by = "Fecha") %>%
           group_by(anio, Id_Cliente) %>%
           summarize(Meses = n_distinct(anio_mes)) %>%
           filter(Meses == 12)%>%
           group_by(anio) %>%
           summarize(Numero_Clientes_Fieles = n_distinct(Id_Cliente))
         
         
         clientes_atendidos_anualmente <- Tabla_Central %>%
           select(Id_Cliente, Fecha) %>%
           distinct() %>%
           left_join(c_tiempo, by = "Fecha") %>%
           select(Id_Cliente, anio)%>%
           group_by(anio) %>%
           count()
         
         Porcentaje_Clientes_Retenidos <- clientes_fieles %>%
           left_join(clientes_atendidos_anualmente, by = c ("anio" = "anio")) %>%
           rename(Total_Clientes_Atendidos = n) %>%
           mutate(Porcentaje_clientes_retenidos = (Numero_Clientes_Fieles/Total_Clientes_Atendidos)*100)
         
         #2.2 Cálculo de clientes conformes con los productos
         
         clientes_atendidos_sin_devoluciones <- Tabla_Central %>%
           select(Id_Cliente, Fecha, Cantidad_Devuelta) %>%
           filter(Cantidad_Devuelta == 0) %>%
           distinct() %>%
           left_join(c_tiempo, by = "Fecha") %>%
           select(Id_Cliente, anio)%>%
           group_by(anio) %>%
           count()
         
         # Ahora combinamos ambas tablas y calculamos el porcentaje de clientes conformes
         PorcentajeClientesConformes <- left_join(clientes_atendidos_anualmente, clientes_atendidos_sin_devoluciones, by = "anio") %>%
           rename(Total_Clientes_Atendidos = n.x, Total_Clientes_Atendidos_Sin_Devoluciones = n.y) %>%
           mutate(Porcentaje_Clientes_Conformes = (Total_Clientes_Atendidos_Sin_Devoluciones/Total_Clientes_Atendidos)*100) %>%
           select(anio, Total_Clientes_Atendidos, Total_Clientes_Atendidos_Sin_Devoluciones, Porcentaje_Clientes_Conformes)
         
         # 2.3 Porcentaje de clientes nuevos captados
         # Contar el número de registros en la tabla
         Cantidad_Total_Clientes <- as.numeric(nrow(c_cliente))
         Tabla_Central$FechaIngresoCliente <- as.Date(Tabla_Central$FechaIngresoCliente)
         
         Nuevos_Clientes_Año <- Tabla_Central %>%
           select(Id_Cliente, FechaIngresoCliente) %>%
           mutate(anio = year(FechaIngresoCliente))%>%
           select(Id_Cliente, anio)%>%
           distinct() %>%
           group_by(anio) %>%
           count()
         
         # Crear la tabla "Porcentaje_Clientes_Nuevos"
         Porcentaje_Clientes_Nuevos <- Nuevos_Clientes_Año %>%
           mutate(anio,
                  Clientes_Nuevos = n,
                  Porcentaje_Clientes_Nuevos_Captados = (n/Cantidad_Total_Clientes)*100) %>%
           arrange(anio)%>%
           select (anio, Clientes_Nuevos, Porcentaje_Clientes_Nuevos_Captados)
         
         #2.3 Tasa de deserción
         
         Clientes_de_baja <- Tabla_Central %>%
           select(Id_Cliente, Fecha, Baja) %>%
           filter(Baja == 1) %>%
           distinct() %>%
           left_join(c_tiempo, by = "Fecha") %>%
           select(Id_Cliente, anio, mes)%>%
           group_by(anio, mes) %>%
           count()
         
         Clientes_atendidos_mensualmente <- Tabla_Central %>%
           select(Id_Cliente, Fecha) %>%
           distinct() %>%
           left_join(c_tiempo, by = "Fecha") %>%
           select(Id_Cliente, anio, mes)%>%
           group_by(anio, mes) %>%
           count() 
         
         Promedio_clientes_atendidos_mensualmente <- clientes_atendidos_anualmente %>%
           arrange(anio) %>%
           group_by(anio) %>%
           mutate(promedio = n/12) %>%
           ungroup()%>%
           mutate(
             Numero_Clientes = n)
         
         Tasa_De_Desercion <- Clientes_de_baja %>%
           left_join(Promedio_clientes_atendidos_mensualmente, by = c("anio")) %>%
           rename(Numero_Clientes_Baja = n.x, 
                  Promedio_Clientes_Atendidos_Mensualmente = promedio) %>%
           select(anio, mes, Numero_Clientes_Baja, Promedio_Clientes_Atendidos_Mensualmente)%>%
           mutate(
             Tasa_Desercion = (Numero_Clientes_Baja / Promedio_Clientes_Atendidos_Mensualmente)*100)
         
         # 2.4 Tasa de devolución
         Productos_devueltos_año <- Tabla_Central %>%
           filter(Cantidad_Devuelta > 0)%>%
           group_by(Id_Producto, Fecha)%>%
           left_join(c_tiempo, by = "Fecha") %>%
           group_by(anio, Id_Producto) %>%
           summarize(Total_Cantidades_Devueltas = sum(Cantidad_Devuelta), Total_Unidades_Vendidas = sum(Cantidad_Total)) 
         
         
         Total_Productos_Vendidos <- sum(Productos_devueltos_año$Total_Unidades_Vendidas)
         
         Tasa_De_Devolucion_por_producto <- Productos_devueltos_año %>%
           group_by(Id_Producto, anio) %>%
           mutate(TasaDevolucion = (Total_Cantidades_Devueltas/Total_Productos_Vendidos)*100)
         
         Tasa_De_Devolucion_global_anual <- Productos_devueltos_año %>%
           ungroup()%>%
           group_by(anio) %>%
           summarize(Total_Cantidades_Devueltas = sum(Total_Cantidades_Devueltas), Total_Unidades_Vendidas = sum(Total_Unidades_Vendidas)) %>%
           mutate(TasaDevolucion = (Total_Cantidades_Devueltas/Total_Productos_Vendidos)*100)
         
         #2.5 Crecimiento anual de devolución
         # Convirtiendo las unidades negativas a positivas
         
         Total_Devoluciones <- sum(Tabla_Central$Cantidad_Devuelta)
         
         Crecimiento_Anual_Devoluciones <- Tabla_Central %>%
           group_by(Fecha) %>%
           left_join(c_tiempo, by = "Fecha") %>%
           group_by(anio)%>%
           summarize(Total_Cantidad_Devuelta = sum(Cantidad_Devuelta)) %>%
           mutate(Promedio = (Total_Cantidad_Devuelta / Total_Devoluciones) * 100, 
                  PromedioAcumulado = cumsum(Promedio))
         
         
         
         
         
         
         
         
         # GRAFICAS o TABLAS
         #1. Variación mensual interanual de las venta y costo (brinda la tendencia de la venta en el tiempo)
         ## 1.1 Venta mensual neta
         ventas_N_mes <- Tabla_Central %>%
           left_join(c_tiempo, by = "Fecha") %>%
           left_join(c_sucursal, by = "Id_Sucursal") %>% 
           group_by(anio_mes) %>%
           summarise(
             Venta_neta = round(sum(Venta_Neta, na.rm = TRUE) / 1000000, 2),
             Costo_neta = round(sum(Costo_Neto, na.rm = TRUE) / 1000000, 2)
           )
         
         ## 1.2 Calcular la tasa de variación interanual de la venta
         ventas_N_mes <- ventas_N_mes %>%
           arrange(anio_mes) %>%
           mutate(Venta_var_Interanual = round((Venta_neta - lag(Venta_neta, 12)) / lag(Venta_neta, 12) * 100, 2),
                  Costo_var_Interanual = round((Costo_neta - lag(Costo_neta, 12)) / lag(Costo_neta, 12) * 100, 2))
         
         
         
         #2. Variación mensual interanual de las VENTA POR SUCURSAL
         
         ## 2.1 Esteli
         ventas_Esteli <- Tabla_Central %>%
           left_join(c_tiempo, by = "Fecha") %>%
           left_join(c_sucursal, by = "Id_Sucursal") %>%
           filter(Sucursald == "Esteli")%>%
           group_by(anio_mes) %>%
           summarise(Esteli = round(sum(Venta_Neta, na.rm = TRUE) / 1000000, 2))
         
         
         ## Calcular la tasa de variación interanual de la venta
         ventas_Esteli <- ventas_Esteli %>%
           arrange(anio_mes) %>%
           mutate(Esteli_v = round((Esteli - lag(Esteli, 12)) / lag(Esteli, 12) * 100,2))
         
         ## 2.2 Chinandega
         ventas_Chinandega <- Tabla_Central %>%
           left_join(c_tiempo, by = "Fecha") %>%
           left_join(c_sucursal, by = "Id_Sucursal") %>%
           filter(Sucursald == "Chinandega")%>%
           group_by(anio_mes) %>%
           summarise(Chinandega = round(sum(Venta_Neta, na.rm = TRUE) / 1000000, 2))
         
         
         ## Calcular la tasa de variación interanual de la venta
         ventas_Chinandega <- ventas_Chinandega %>%
           arrange(anio_mes) %>%
           mutate(Chinandega_v = round((Chinandega - lag(Chinandega, 12)) / lag(Chinandega, 12) * 100,2))
         
         
         ## 2.3 Leon
         ventas_Leon <- Tabla_Central %>%
           left_join(c_tiempo, by = "Fecha") %>%
           left_join(c_sucursal, by = "Id_Sucursal") %>%
           filter(Sucursald == "Leon")%>%
           group_by(anio_mes) %>%
           summarise(Leon = round(sum(Venta_Neta, na.rm = TRUE) / 1000000, 2))
         
         
         ## Calcular la tasa de variación interanual de la venta
         ventas_Leon <- ventas_Leon %>%
           arrange(anio_mes) %>%
           mutate(Leon_v = round((Leon - lag(Leon, 12)) / lag(Leon, 12) * 100,2))
         
         
         ##2.4 MANAGUA_VENTAS
         ventas_Managua <- Tabla_Central %>%
           left_join(c_tiempo, by = "Fecha") %>%
           left_join(c_sucursal, by = "Id_Sucursal") %>%
           filter(Sucursald == "Managua")%>%
           group_by(anio_mes) %>%
           summarise(Mananagua = round(sum(Venta_Neta, na.rm = TRUE) / 1000000, 2))
         
         
         ## Calcular la tasa de variación interanual de la venta
         ventas_Managua <- ventas_Managua %>%
           arrange(anio_mes) %>%
           mutate(Managua_v = round((Mananagua - lag(Mananagua, 12)) / lag(Mananagua, 12) * 100,2))
         
         ##2.5 RIVAS
         ventas_Rivas <- Tabla_Central %>%
           left_join(c_tiempo, by = "Fecha") %>%
           left_join(c_sucursal, by = "Id_Sucursal") %>%
           filter(Sucursald == "Rivas")%>%
           group_by(anio_mes) %>%
           summarise(Rivas = round(sum(Venta_Neta, na.rm = TRUE) / 1000000, 2))
         
         
         ## Calcular la tasa de variación interanual de la venta
         ventas_Rivas <- ventas_Rivas %>%
           arrange(anio_mes) %>%
           mutate(Rivas_v = round((Rivas - lag(Rivas, 12)) / lag(Rivas, 12) * 100,2))
         
         ##2.6 CARAZ0
         ventas_Carazo <- Tabla_Central %>%
           left_join(c_tiempo, by = "Fecha") %>%
           left_join(c_sucursal, by = "Id_Sucursal") %>%
           filter(Sucursald == "Carazo")%>%
           group_by(anio_mes) %>%
           summarise(Carazo = round(sum(Venta_Neta, na.rm = TRUE) / 1000000, 2))
         
         
         ## Calcular la tasa de variación interanual de la venta
         ventas_Carazo <- ventas_Carazo %>%
           arrange(anio_mes) %>%
           mutate(Carazo_v = round((Carazo - lag(Carazo, 12)) / lag(Carazo, 12) * 100,2))   
         
         
         ##UNIENDO LAS TASAS DE VARIACIONES
         
         Tasa_interanual<- ventas_N_mes %>%
           left_join(ventas_Esteli, by = "anio_mes") %>% 
           left_join(ventas_Chinandega, by = "anio_mes") %>% 
           left_join(ventas_Leon, by = "anio_mes") %>% 
           left_join(ventas_Managua, by = "anio_mes") %>% 
           left_join(ventas_Rivas, by = "anio_mes") %>% 
           left_join(ventas_Carazo, by = "anio_mes") %>% 
           select(anio_mes,Venta_var_Interanual,Esteli_v,Chinandega_v,Leon_v,Managua_v,Rivas_v,Carazo_v, Venta_neta, Esteli, Chinandega, Leon, Mananagua, Rivas, Carazo)
         
         #Renombrando columna Venta_var_Interanual a Global
         
         Tasa_interanual <- Tasa_interanual %>%
           rename(Venta_variacion_Global = Venta_var_Interanual,
                  Venta_neta_Global = Venta_neta,
                  Managua = Mananagua)
         
         # Cargar el paquete tidyverse si aún no está cargado
         
         
         # Usar pivot_longer para transformar la tabla
         Tasa_interanual_Shiny <- Tasa_interanual %>%
           pivot_longer(
             cols = -anio_mes, # Seleccionar todas las columnas excepto anio_mes
             names_to = "Sucursal", # Nombre de la nueva columna que tendrá los nombres de las antiguas columnas
             values_to = "Tasa" # Nombre de la nueva columna que tendrá los valores de las antiguas columnas
           )
         
         #3. Venta anual por sucursal y canal (Podria ser una tabla que se muestre el filtro por año, o
         # grafica apilada en el eje Y Sucursal y apilada por tipo de canal)
         
         
         ventas_suc_canal <- Tabla_Central %>%
           left_join(c_tiempo, by = "Fecha") %>%
           left_join(c_sucursal, by = "Id_Sucursal") %>%
           left_join(c_canalDistribucion, by = "Id_CanalDistribucion")%>%
           group_by(aniot,Sucursald,Canal) %>%
           select(aniot,Sucursald,Canal,Venta_Neta)%>% 
           summarise(Venta_neta = round(sum(Venta_Neta, na.rm = TRUE) / 1000000, 2)) %>% 
           pivot_wider(names_from = Canal, values_from = Venta_neta)
         
         #4. Descuento anual por sucursal (Podria ser una tabla que se muestre el filtro por año, o
         # grafica de barra, en  el eje Y Sucursal) (Se observa que solo Esteli esta brindando descuento,
         #y sus ventasn se han incrementado)
         
         
         Descuentot <- Tabla_Central %>%
           left_join(c_tiempo, by = "Fecha") %>%
           left_join(c_sucursal, by = "Id_Sucursal") %>%
           group_by(aniot,Sucursald)%>%
           select(aniot,Sucursald,Descuento) %>% 
           summarise(Descuentos = round(sum(Descuento,na.rm = TRUE) / 1000000, 2))%>%
           pivot_wider(names_from = aniot, values_from = Descuentos)  
         
         # Usar pivot_longer para transformar la tabla
         Descuetot_Shiny <- Tabla_Central %>%
           left_join(c_tiempo, by = "Fecha") %>%
           left_join(c_sucursal, by = "Id_Sucursal") %>%
           group_by(aniot,Sucursald)%>%
           select(aniot,Sucursald,Descuento) %>% 
           summarise(Descuentos = round(sum(Descuento,na.rm = TRUE) / 1000000, 2))
         
         # 5 Descuento anual por tipo de cliente y tipo de producto (Se observa que solo se le brinda descuento a
         # los clientes fieles)
         Descuento_prod_c <- Tabla_Central %>%
           left_join(c_tiempo, by = "Fecha") %>%
           left_join(c_tipoCliente, by = "Id_TipoCliente") %>%
           left_join(c_tipoProducto, by = "Id_TipoProducto") %>%
           group_by(aniot, Tip_cliente, Gama) %>%
           select(aniot, Tip_cliente, Gama, Descuento) %>% 
           summarise(Descuentos = round(sum(Descuento, na.rm = TRUE) / 1000000, 2)) %>%
           arrange(desc(Descuentos)) %>%
           pivot_wider(names_from = aniot, values_from = Descuentos) 
         
         Descuento_prod_c_Shiny <- Tabla_Central %>%
           left_join(c_tiempo, by = "Fecha") %>%
           left_join(c_tipoCliente, by = "Id_TipoCliente") %>%
           left_join(c_tipoProducto, by = "Id_TipoProducto") %>%
           group_by(aniot, Tip_cliente, Gama) %>%
           select(aniot, Tip_cliente, Gama, Descuento) %>% 
           summarise(Descuentos = round(sum(Descuento, na.rm = TRUE) / 1000000, 2)) %>%
           arrange(desc(Descuentos))
         
         # 6 Desercion por tipo de cliente y sucursal 
         
         ##Por sucursal
         Desercion_c <- Tabla_Central %>% 
           left_join(c_tiempo, by = "Fecha") %>%
           left_join(c_tipoCliente, by = "Id_TipoCliente") %>%
           left_join(c_sucursal, by = "Id_Sucursal") %>%
           filter(Baja == 1) %>%
           group_by(aniot, Tip_cliente, Sucursald) %>%
           summarise(Count = n()) %>%
           pivot_wider(names_from = Sucursald, values_from = Count, names_prefix = "Count_") %>%
           replace_na(list(Count_1 = 0, Count_2 = 0))  # Aquí reemplazamos NA por 0 en las columnas Count_1 y Count_2, según sea necesario
         
         ## Total
         Desercion_Tot <- Tabla_Central %>% 
           left_join(c_tiempo, by = "Fecha") %>%
           left_join(c_tipoCliente, by = "Id_TipoCliente") %>%
           left_join(c_sucursal, by = "Id_Sucursal") %>%
           filter(Baja == 1) %>%
           group_by(aniot, Tip_cliente) %>%
           summarise(Total = n())
         
         ##Uniendo la desercion de la sucursales con el total por sucursal (esta es la que hay que graficar
         # o gener una tabla que filtre por año))
         
         Desercion_G <- Desercion_c %>% 
           left_join(Desercion_Tot, by = c("aniot", "Tip_cliente"))
         
         ##Por sucursal
         Desercion_c_Shiny <- Tabla_Central %>% 
           left_join(c_tiempo, by = "Fecha") %>%
           left_join(c_tipoCliente, by = "Id_TipoCliente") %>%
           left_join(c_sucursal, by = "Id_Sucursal") %>%
           filter(Baja == 1) %>%
           group_by(aniot, Tip_cliente, Sucursald) %>%
           summarise(Count = n())
         
         Desercion_G_Shiny <- Desercion_c_Shiny %>% 
           left_join(Desercion_Tot, by = c("aniot", "Tip_cliente"))
         
         Desercion_G_Shiny <- Desercion_G_Shiny %>%
           mutate(Count = Total) #%>%
         #select(-Total)
         
         # Crear un nuevo dataframe con los valores de Total
         total_df <- Desercion_G_Shiny %>%
           ungroup() %>%
           select(aniot, Sucursald, Total) %>%
           mutate(Tip_cliente = "Total", Count = Total) %>%
           select(-Total) %>%
           group_by(Sucursald, aniot) %>%
           summarize(Count = sum(Count))
         
         # Combinar el nuevo dataframe con el original
         Desercion_G_Shiny_update <- Desercion_G_Shiny %>%
           select(-Total) %>%
           bind_rows(total_df)
         
         Desercion_G_Shiny_update <- Desercion_G_Shiny_update %>%
           mutate(Tip_cliente = ifelse(is.na(Tip_cliente), "Global", Tip_cliente)) %>%##Reemplaza las columnas N/A por Global
           rename(Desercion = Count)
         
         # 7 Venta y devolucion por Tipo de transporte por año
         Transporte_Total <- Tabla_Central %>% 
           left_join(c_tiempo, by = "Fecha") %>%
           left_join(c_tipoCliente, by = "Id_TipoCliente") %>%
           left_join(c_tipoTransporte, by = "Id_TipoTransporte") %>%
           filter(aniot %in% c(2021, 2022)) %>%
           group_by(aniot, TipoTransporte) %>%
           summarise(Venta = sum(round(sum(Venta_Neta, na.rm = TRUE) / 1000000, 2)),
                     Devolu_unid= sum(round(sum(Cantidad_Devuelta, na.rm = TRUE), 0))) %>% 
           pivot_wider(names_from = aniot, 
                       values_from = c(Venta, Devolu_unid),
                       names_prefix = "_")
         
         Transporte_Total_Shiny <- Tabla_Central %>% 
           left_join(c_tiempo, by = "Fecha") %>%
           left_join(c_tipoCliente, by = "Id_TipoCliente") %>%
           left_join(c_tipoTransporte, by = "Id_TipoTransporte") %>%
           filter(aniot %in% c(2021, 2022)) %>%
           group_by(aniot, TipoTransporte) %>%
           summarise(Venta = sum(round(sum(Venta_Neta, na.rm = TRUE) / 1000000, 2)),
                     Devolu_unid= sum(round(sum(Cantidad_Devuelta, na.rm = TRUE), 0)))
         
         #8 Cantidad de devolución por tipo de cliente año 2022 (Puede filtarse por Sucursal)
         Devoluc_Cliente <- Tabla_Central %>% 
           left_join(c_tiempo, by = "Fecha") %>%
           left_join(c_tipoCliente, by = "Id_TipoCliente") %>%
           left_join(c_sucursal, by = "Id_Sucursal") %>%
           filter(aniot == 2022) %>%
           group_by(aniot, Tip_cliente, Sucursald) %>%
           summarise(Devolu_unid= sum(round(sum(Cantidad_Devuelta, na.rm = TRUE), 0))) %>%
           filter(Devolu_unid < 0)
         
         # 9 Cliente nuevos por año y sucursal
         
         #Calculando el total de clientes por sucursal
         TT_Cliente <- Tabla_Central %>% 
           left_join(c_tiempo, by = "Fecha") %>%
           left_join(c_cliente, by = "Id_Cliente") %>%
           left_join(c_sucursal, by = "Id_Sucursal") %>%
           filter(aniot == 2022) %>%
           select(aniot, cliente, Sucursald)
         
         TT_Cliente<-TT_Cliente %>% 
           group_by(aniot, Sucursald) %>% 
           summarise(Cantidad=n())
         
         
         # Calculando la cantidad de Clientes nuevos por sucursal 
         clientes_nuevos_sucursal <- Tabla_Central %>% 
           left_join(c_cliente, by = "Id_Cliente") %>% 
           left_join(c_tiempo, by = "Fecha") %>% 
           left_join(c_sucursal, by = "Id_Sucursal") %>% 
           filter(aniot == 2022) %>% 
           distinct(Sucursald, NombreCliente) %>% 
           count(Sucursald)
         
         # Uniendo las dataframe: Total y nuevo clientes por sucursal
         NV_Clientes_Sucur <- TT_Cliente %>% 
           left_join(clientes_nuevos_sucursal, by = "Sucursald")
         
         #Renombrando Columnas
         colnames(NV_Clientes_Sucur) <- c("anio", "Sucursal","Clientes_T","Cliente_NV")
         
         #Calculando el porcentaje de clientes nuevos en el total cliente por sucursal
         
         NV_Clientes_Sucur<- NV_Clientes_Sucur %>% 
           mutate(Porc_CN = round(Cliente_NV / Clientes_T,4)*100)
         
         
         #10 Desercion por antiguedad del cliente (Se observa que estamos perdiento los clientes antiguos mas de 10 años)
         # Creando rango de antiguedad
         c_cliente2 <- c_cliente %>% 
           mutate(Rang_antiguedad = case_when(
             Antiguedad >= 0 & Antiguedad < 0.9999 ~ "1. Menor a 1",
             Antiguedad >= 1 & Antiguedad <= 5.9999 ~ "2. De 1 a 5",
             Antiguedad >= 6 & Antiguedad <= 10.999 ~ "3. De 6 a 10",
             Antiguedad >= 11 ~ "4. De 11 a mas",
             TRUE ~ "No Definido"
           ))
         
         #Desercion por antiguedad y sucursal 
         
         ##Por sucursal
         Desercion_Rang <- Tabla_Central %>% 
           left_join(c_tiempo, by = "Fecha") %>%
           left_join(c_cliente2, by = "Id_Cliente") %>%
           left_join(c_sucursal, by = "Id_Sucursal") %>%
           filter(Baja == 1) %>%
           group_by(aniot, Rang_antiguedad, Sucursald) %>%
           summarise(Count = n()) %>%
           pivot_wider(names_from = Sucursald, values_from = Count, names_prefix = "Count_") %>%
           replace_na(list(Count_1 = 0, Count_2 = 0))  # Aquí reemplazamos NA por 0 en las columnas Count_1 y Count_2, según sea necesario
         
         ## Total
         Desercion_RanT <- Tabla_Central %>% 
           left_join(c_tiempo, by = "Fecha") %>%
           left_join(c_cliente2, by = "Id_Cliente") %>%
           left_join(c_sucursal, by = "Id_Sucursal") %>%
           filter(Baja == 1) %>%
           group_by(aniot, Rang_antiguedad) %>%
           summarise(Total = n())
         
         ##Uniendo la desercion de la sucursales con el total por sucursal (esta es la que hay que graficar
         # o gener una tabla que filtre por año))
         
         Desercion_RanG <- Desercion_Rang %>% 
           left_join(Desercion_RanT, by = c("aniot", "Rang_antiguedad"))
         
         #Otros a agregar:
         ## Producto mas vendido (total y por sucursal)
         ## Producto menos vendido (total y por sucursal)
         ## Producto mas devuelto (Total y por sucursal)
         ## Desercion por tipo de cliente
         ## Cantidad de clientes nuevos por sucursal
         ## Tipo de transporte que mas vende por tipo de cliente
         
         
         
         
         
         
         
         
         
         
         
         Resumen <- c("Dispositivos del ambito informatico y telecomunicacion (TIC)","Aparatos de imagen y sonidos","Articulos para el hogar")
         
         Resumen <- c_tipoProducto %>%
           select(Gama)%>%
           cbind(Resumen)
         
         
         c_zona
         
         ResumenZonas <- c_zona %>%
           select(-NombreZona) %>%  
           rename( `# ` = Id_Zona, Zona = zonas_ventas)
         
         get_ventas_anuales <- function(tabla, ano) {
           tabla %>%
             left_join(c_tiempo, by = "Fecha") %>%
             group_by(anio) %>%
             summarise(
               Venta_neta = sum(Venta_Neta, na.rm = TRUE)
             ) %>%
             filter(anio == ano)
         }
         
         ####Resumen de productos
         # Agregar una columna temporal 'Year' para los cálculos
         Yearly_Data <- Tabla_Central %>%
           mutate(Year = year(Fecha))
         
         # Unir 'Yearly_Data' con 'c_producto' para tener el nombre del producto
         Yearly_Data <- Yearly_Data %>%
           left_join(c_producto, by = "Id_Producto")
         
         # Calcular la mercancía más vendida por año
         Principal_Mercancía_Vendida <- Yearly_Data %>%
           group_by(Year, Nombre) %>%
           summarise(Unidades_Vendidas = sum(Cantidad), .groups = 'drop') %>%
           # Ordenamos dentro de cada año por Unidades_Vendidas de forma descendente
           arrange(Year, desc(Unidades_Vendidas)) %>%
           # Filtramos la primera fila de cada año, que corresponderá al producto más vendido
           group_by(Year) %>%
           slice(1) %>%
           ungroup() %>%
           # Seleccionamos solo las columnas de interés
           select(Year, Principal_Mercancía_Vendida = Nombre, Unidades_Vendidas)
         
         # Repetimos el proceso para las mercancías más devueltas
         Principal_Mercancía_Devuelta <- Yearly_Data %>%
           group_by(Year, Nombre) %>%
           summarise(Devoluciones = sum(Cantidad_Devuelta), .groups = 'drop') %>%
           arrange(Year, desc(Devoluciones)) %>%
           group_by(Year) %>%
           slice(1) %>%
           ungroup() %>%
           select(Year, Principal_Mercancía_Devuelta = Nombre, Devoluciones)
         
         # Y para las mercancías con más descuento
         Principal_Mercancía_Descuento <- Yearly_Data %>%
           group_by(Year, Nombre) %>%
           summarise(Descuento_Total = sum(Descuento), .groups = 'drop') %>%
           arrange(Year, desc(Descuento_Total)) %>%
           group_by(Year) %>%
           slice(1) %>%
           ungroup() %>%
           select(Year, Principal_Mercancía_Descuento = Nombre, Descuento_Total)
         
         
         # Combinar las tres tablas anteriores para crear la tabla final 'Resumen_Productos'
         Resumen_Productos <- Principal_Mercancía_Vendida %>%
           full_join(Principal_Mercancía_Devuelta, by = "Year") %>%
           full_join(Principal_Mercancía_Descuento, by = "Year")
         
         # Ver la tabla resultante
         print(Resumen_Productos)
         
         Estadistico1 <- Tabla_Central %>% 
           left_join(c_tiempo, by = "Fecha") %>%
           left_join(c_cliente, by = "Id_Cliente") %>%
           left_join(c_sucursal, by = "Id_Sucursal") %>%
           filter(aniot %in% c(2021, 2022)) %>%
           group_by(aniot) %>%
           summarise(Venta_millo = sum(round(sum(Venta_Neta, na.rm = TRUE) / 1000000, 2)),
                     Prom_Venta_CS = round(mean(Venta_Neta, na.rm = TRUE), 2),
                     Max_Venta_CS = round(max(Venta_Neta, na.rm = TRUE), 2),
                     Min_Venta_CS = round(min(Venta_Neta, na.rm = TRUE), 2),
                     V_frecuente_CS = as.numeric(names(sort(table(Venta_Neta), decreasing = TRUE)[1])),
                     Costo_millo = sum(round(sum(Tabla_Central$Costo_Neto, na.rm = TRUE) / 1000000, 2)),
                     Bonificacion_CS = sum((Costo_Total / Cantidad_Total) * CantidadBonificada, 2),
                     Descuento_CS = sum(round(sum(Descuento, na.rm = TRUE) / 1000000, 2)),
                     Cantidad_Total = sum(round(sum(Cantidad_Total, na.rm = TRUE), 0)),
                     Devolu_unid = sum(round(sum(Cantidad_Devuelta, na.rm = TRUE), 0)),
                     Clientes = n()
           )
         head(Estadistico1)
         