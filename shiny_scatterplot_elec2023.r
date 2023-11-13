library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(scales)
library(shiny)
library(ggiraph)
library(shinycssloaders)
library(shinyWidgets)


df4<-read_csv("df4.gz")

ui <- fluidPage(
  tags$head(tags$style(HTML('* {font-family: "Arial"};'))),
  titlePanel("Diferencia de votos Generales vs PASO (2023)"),
  sidebarLayout(
    sidebarPanel(
      p('Seleccionar partido y 1 o más provincias, luego cliquear en "Graficar selección". La carga del gráfico puede demorar 60 segundos o más.'),
      selectInput("party", "Seleccione un partido/coalición:", choices = unique(df4$party)),
      checkboxInput("participacion", "Filtrar por participación <50 (probables errores):", value = FALSE),
      pickerInput("provincia",
                  "Seleccione una provincia:",
                  choices = unique(df4$Provincia),
                  options = list(`actions-box` = TRUE,
                                 `deselect-all-text` = "Limpiar selección",
                                 `select-all-text` = "Todas",
                                 `none-selected-text` = "Seleccionar al menos 1 provincia"),
                  # selected = "Todas",
                  multiple = T),
      actionButton(inputId = "correr","Graficar selección"),
      p("En primer lugar recordar que los datos utilizados son los del escrutinio provisorio que NO TIENE VALIDEZ LEGAL. Cada punto representa una mesa. En el eje horizontal se representa la diferencia de votos totales, y en el eje vertical la diferencia de votos para la fuerza política elegida. La fuente de los datos es ",
        a("@ken4rab",
          href = "https://twitter.com/ken4rab",
          target = "_blank"), ".",
        "El código de la app y del script que prepara el archivo gz de input está aquí en ",
        a("Github.",
          href = "https://github.com/rquiroga7/analisis_electoral_2023_arg",
          target = "_blank"), " Código escrito por ",
        a("Rodrigo Quiroga",
          href = "https://twitter.com/rquiroga777",
          target = "_blank"),". Se agradece enormemente el aporte de ",
        a("Juan Gabriel Juara",
          href = "https://github.com/jgjuara",
          target = "_blank"),
        ", que introdujo cambios al código que permite el correcto funcionamiento de la app en Shinyapps.io. Si hay errores en la app es responsabilidad mía y no suya. La línea naranja indica donde deberían caer las mesas si todo el aumento de participación resultara en un aumento de votos para esta fuerza. Si las mesas se encuentran por arriba de esta línea, sugiere transferencia de votos de otras fuerzas a ésta, y por debajo indica lo contrario. Se provee una checkbox para filtrar las mesas con participación menor a 50 en PASO o generales. Como los datos son provisorios, puede haber errores de carga de telegramas.")
    ),
    mainPanel(
      conditionalPanel(condition = "input.correr > 0",
                       girafeOutput("scatterplot") %>%
                         withSpinner(color="#0dc5c1")
      ))
  )
)

# Define server
server <- function(input, output) {
  
  observeEvent(input$correr, {  
    
    df4_filtered <- isolate({
      if (input$participacion) {
        df4 %>% filter(party == input$party, Provincia %in% input$provincia,
                       participacion_paso > 50, participacion_general > 50)
      } else {
        df4 %>% filter(party == input$party, Provincia %in% input$provincia)
        
      }
    })
    
    
    output$scatterplot <- renderGirafe({
      
      
      gg <- ggplot(df4_filtered, aes(
        x = diferencia_participacion,
        y = diferencia,
        data_id = cod_seccion,
        color = Provincia,
        tooltip = paste0(
          "Provincia: ",
          Provincia,
          "<br>",
          "Participación PASO: ",
          participacion_paso,
          "<br>",
          "Participación generales: ",
          participacion_general,
          "<br>",
          "Votos PASO: ",
          votos_PASO,
          "<br>",
          "Votos generales: ",
          votos_generales,
          "<br>",
          "Mesa: ",
          cod_seccion
        ))) +
        geom_point_interactive(alpha = 0.2) +
        geom_abline(intercept = 0, slope = 1, color = "orange") +
        geom_hline(yintercept = 0, color="black")+
        geom_vline(xintercept = 0, color="black")+
        ggtitle(paste0(isolate(input$party), " - Diferencia de votos Generales vs PASO (2023)")) +
        labs(x = "Diferencia participación ", y = "Diferencia votos")+
        theme_light() +
        theme(legend.position = "bottom",
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0))
      
      girafe(ggobj = gg,
             options = list(opts_hover(css = "opacity:1;stroke:black;r:4pt;")))
      
      
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)
