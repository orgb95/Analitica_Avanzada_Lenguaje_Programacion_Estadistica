# Verificación e instalación de paquetes
packages <- c("tidyr", "shiny", "ggplot2", "dplyr","shiny","shinydashboard")
to_install <- packages[!(packages %in% installed.packages()[,"Package"])]
if (length(to_install) > 0) {
  install.packages(to_install)
}

# Carga de librerías
sapply(packages, require, character.only = TRUE)

# Definición de la interfaz de usuario
ui <- fluidPage(
  dashboardPage(
    title = "Proyecto Final",
    skin = "yellow",
    dashboardHeader(title = "El Gallo - Soluciones Domesticas Innovadoras", titleWidth = 400),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Tablero", tabName = "Tablero", icon = icon("dashboard")),
        menuItem("Descripción", tabName = "Descripcion", icon = icon("th")),
        menuItem("Análisis de Ventas", tabName = "AnalisisVentas", icon = icon("signal")),
        menuItem("Presentadores", tabName = "DesercionClientes", icon = icon("globe")),
        menuItem("Modelo ML", tabName = "ML", icon = icon("eye"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "Descripcion",
          fluidRow(box(
            title = "Descripción del Negocio",
            solidHeader = TRUE,
            status = "warning",
            div(
              img(src = "https://i.postimg.cc/4xhtYNR6/Icono.jpg", width = "50%"),
              p(br(), br(),"El Gallo - Soluciones Domésticas Innovadoras: Nuestro compromiso es convertir los hogares en espacios eficientes y confortables.",br(), br(),"
                Ofrecemos una amplia gama de electrodomésticos de vanguardia, desde tecnología inteligente hasta clásicos confiables.",br(), br(),"
                Con asesoramiento especializado y productos de calidad, en El Gallo transformamos cada rincón del hogar, haciendo la vida diaria más sencilla y placentera para todos nuestros clientes.")
            )
          ),
          box(
            title = "Resumen de negocio" ,
            solidHeader = TRUE,
            status = "info",
            tableOutput("tablaresumen")
          ),
          box(
            title = "Resumen de zonas" ,
            solidHeader = TRUE,
            status = "info",
            tableOutput("tablazonas")
          )
          )
          
        ),
        tabItem(
          tabName = "Tablero",
          fluidRow(
            infoBoxOutput("Ventameta"), 
            infoBoxOutput("Ventaneta")
          ), 
          tabsetPanel(
            tabPanel("Tasa de variación interanual", plotOutput("linePlot")),
            tabPanel("Ventas por Sucursal y Canal",
                     selectInput("año", "Seleccione el Año", choices = unique(ventas_suc_canal$aniot)),
                     selectInput("sucursal", "Seleccione la Sucursal", choices = unique(ventas_suc_canal$Sucursald)),
                     plotOutput("barPlot")
            ),
            tabPanel("Ventas por Sucursal",
                     selectInput("sucursal2", "Seleccione la Sucursal", choices = unique(Tasa_interanual_Shiny$Sucursal)),
                     plotOutput("linePlotSucursal")
            ),
            tabPanel("Descuento Anual por Sucursal",
                     selectInput("Año", "Seleccione el año", choices = unique(Descuetot_Shiny$aniot)),
                     plotOutput("BarDescuentoSucursal")
            ),
            tabPanel("Descuento por Tipo cliente y tipo producto",
                     selectInput("tipo_cliente", "Seleccione el tipo de cliente", choices = unique(Descuento_prod_c_Shiny$Tip_cliente)),
                     selectInput("año_descuento", "Seleccione el Año", choices = unique(Descuento_prod_c_Shiny$aniot)),
                     plotOutput("BarDescuentoTipoCliente")
            ),
            tabPanel("Deserción de clientes por año y tipo cliente",
                     selectInput("tipo_cliente_desercion", "Seleccione el tipo de cliente", choices = unique(Desercion_G_Shiny_update$Tip_cliente)),
                     selectInput("año_desercion", "Seleccione el Año", choices = unique(Desercion_G_Shiny_update$aniot)),
                     plotOutput("BarDesercionClientes")
            )
          )
        ), 
        
        tabItem(
          tabName = "AnalisisVentas",
          fluidRow(
            box(
              title = "FODA",
              solidHeader = TRUE,
              status = "success",
              img(src = "https://i.postimg.cc/kGxfS5vV/FODA.png", width = "100%")
              
            ), 
            
            box(
              title = "Transporte - Nivel Devolucion",
              solidHeader = TRUE,
              status = "success",
              HTML("Los clientes prefieren comparar a través de <strong> Nicamandado (C$ 620 millones)</strong>, seguido por pedidos Ya (C$ 270 millones).<br><br>
               <strong> El Nivel de devolución de productos por cada tipo de transporte es similar (entre 1000 y 1500 unidades) </strong> por lo que podemos descartar daños en la mercancía cuando se transportan, debiendo evaluar la calidad del producto al momento de compra.")
              
            ),
            
            box(
              title = "Canal de Distribucion",
              solidHeader = TRUE,
              status = "success",
              HTML("<strong>El principal canal de distribución continúa siendo la venta a distribuidoras y mayoristas</strong> con C$ 940 millones (91% del total), a pesar de su reduccion en un 3% respecto al 2021<br><br>
                Por el contrario, <strong>las ventas por catálogo y telemarketing</strong> se incrementaron de C$ 5.7 millones a C$19.3 millones del 2021 al 2022.")
              
            ),
            
            box(
              title = "Nivel de ventas",
              solidHeader = TRUE,
              status = "danger",
              HTML("En el año 2022, el nivel de ventas netas ascendió a C$ 1,035 millones, inferior en un 2.5% (C$ 26 millones) respecto al año 2021. <br><br>
                <strong>Las principales razones que podrían explicar la baja en las ventas</strong> son las siguientes: El <strong>incremento de las devoluciones de productos</strong>, la que se triplicó en el año 2022 respecto al 2021. <br><br>
                La <strong> deserción de los clientes</strong>, la que a pasar de haberse reducido en un 83% del 2021 al 2022 pasando de 13,524 a 2,325 casos respectivamente, siendo los clientes fieles de mayor a 10 años de antigüedad los que mayormente desiertas.")
              
            )
          )
          
        )
        
      )
    )
  )
)



# Definir la lógica del servidor
server <- function(input, output) {
  # Generar la gráfica de líneas Variacion de ventas y costos
  output$linePlot <- renderPlot({
    ggplot(data = ventas_N_mes, aes(x = anio_mes)) +
      geom_line(aes(y = Venta_neta, color = "Venta Neta"), group = 1) +
      geom_line(aes(y = Costo_neta, color = "Costo Neta"), group = 1) +
      scale_color_manual(values = c("Venta Neta" = "blue", "Costo Neta" = "red")) +
      labs(x = "Año-Mes", y = "Tasa de Variación") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Inclinar las etiquetas del eje x
      theme(legend.position = "top") + #Posición de la leyenda
      guides(color = guide_legend(title = "Leyenda"))
  })
  
  
  # Generar la gráfica de barras
  output$barPlot <- renderPlot({
    # Filtrar los datos basados en la selección del usuario
    datos_filtrados <- ventas_suc_canal %>%
      filter(aniot == input$año, Sucursald == input$sucursal)
    
    # Asumiendo que las ventas se encuentran en columnas separadas, es posible
    # que sea necesario reorganizar los datos para una gráfica de barras
    datos_long <- gather(datos_filtrados, key = "Canal", value = "Ventas", -aniot, -Sucursald)
    
    ggplot(datos_long, aes(x = Canal, y = Ventas, fill = Canal)) +
      geom_bar(stat = "identity") +
      labs(x = "Canal de Distribución", y = "Ventas") +
      theme_minimal()
  })
  
  # Generar la gráfica de líneas, variacion de ventas por sucursal
  output$linePlotSucursal <- renderPlot({
    req(input$sucursal2)  # Asegúrate de que la entrada de sucursal está disponible antes de proceder
    
    # Asume que Tasa_interanual_Shiny ya está disponible en el entorno global
    Tasa_interanual_filtrada <- Tasa_interanual_Shiny %>%
      filter(Sucursal == input$sucursal2, !is.na(Tasa))
    
    # Verifica que después del filtrado hay datos disponibles
    if (nrow(Tasa_interanual_filtrada) > 0) {
      ggplot(data = Tasa_interanual_filtrada, aes(x = anio_mes, y = Tasa)) +
        geom_line(aes(color = Sucursal), group = 1, color = "blue", size = 1.5) +
        scale_color_manual(values = c("Venta Neta" = "blue")) +
        labs(x = "Año-Mes", y = "Tasa de Variación") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        theme(legend.position = "top") +
        guides(color = guide_legend(title = "Leyenda"))
    } else {
      return(NULL)  # Devuelve NULL si no hay datos después del filtrado
    }
  })
  
  # Generar la gráfica de barras
  output$BarDescuentoSucursal <- renderPlot({
    # Filtrar los datos basados en la selección del usuario
    datos_filtrados_descuento <- Descuetot_Shiny %>%
      filter(aniot == input$Año)
    
    # Asumiendo que las ventas se encuentran en columnas separadas, es posible
    # que sea necesario reorganizar los datos para una gráfica de barras
    
    #datos_long <- gather(datos_filtrados_descuento, key = "Sucursald", value = "Descuentos", -aniot)
    
    ggplot(datos_filtrados_descuento, aes(x = Sucursald, y = Descuentos, fill = Sucursald)) +
      geom_bar(stat = "identity") +
      labs(x = "Sucursal", y = "Descuentos") +
      theme_minimal()
  })
  
  # Generar la gráfica de barras
  output$BarDescuentoTipoCliente <- renderPlot({
    # Filtrar los datos basados en la selección del usuario
    datos_filtrados_descuento_cliente <- Descuento_prod_c_Shiny %>%
      filter(aniot == input$año_descuento, Tip_cliente == input$tipo_cliente)
    
    # Asumiendo que las ventas se encuentran en columnas separadas, es posible
    # que sea necesario reorganizar los datos para una gráfica de barras
    
    #datos_long <- gather(datos_filtrados_descuento_cliente, key = "Sucursald", value = "Descuentos", -aniot)
    
    ggplot(datos_filtrados_descuento_cliente, aes(x = Gama, y = Descuentos, fill = Gama)) +
      geom_bar(stat = "identity") +
      labs(x = "Tipo Productos", y = "Descuentos") +
      theme_minimal()
  })
  
  # Generar la gráfica de barras
  output$BarDesercionClientes <- renderPlot({
    # Filtrar los datos basados en la selección del usuario
    datos_filtrados_Desercion_cliente <- Desercion_G_Shiny_update %>%
      filter(aniot == input$año_desercion, Tip_cliente == input$tipo_cliente_desercion)
    
    # Asumiendo que las ventas se encuentran en columnas separadas, es posible
    # que sea necesario reorganizar los datos para una gráfica de barras
    
    #datos_long <- gather(datos_filtrados_descuento_cliente, key = "Sucursald", value = "Descuentos", -aniot)
    
    ggplot(datos_filtrados_Desercion_cliente, aes(x = Sucursald, y = Desercion, fill = Sucursald)) +
      geom_bar(stat = "identity") +
      labs(x = "Sucursal", y = "Desercion") +
      theme_minimal()
  })
  
  output$Ventameta <- renderValueBox({
    valueBox(Meta_VN_2022, "Meta Venta 2022",icon=icon("star"),color="blue")
  })
  
  
  output$Ventaneta <- renderValueBox({
    valueBox(VentasN_2022$Venta_neta, "Venta 2022",icon=icon("fire"),color="red")
  })
  
  output$tablaresumen <- renderTable({
    Resumen
  })
  output$tablazonas <- renderTable({
    ResumenZonas
  })
}

# Ejecutar la aplicación
shinyApp(ui, server)