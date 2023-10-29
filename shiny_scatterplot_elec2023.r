library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(scales)
library(shiny)
library(ggiraph)

df4<-read_csv("df4.gz")

ui <- fluidPage(
 tags$head(tags$style(HTML('* {font-family: "Arial"};'))),
 titlePanel("Diferencia de votos Generales vs PASO (2023)"),
 sidebarLayout(
  sidebarPanel(
   p("Tener paciencia, la carga del gráfico puede demorar 60 segundos o más."),
   selectInput("party", "Seleccione un partido/coalición:", choices = unique(df4$party)),
   checkboxInput("participacion", "Filtrar por participación <50 (probables errores):", value = FALSE),
   checkboxGroupInput("provincia", "Seleccione una provincia:", choices = unique(df4$Provincia), selected = unique(df4$Provincia)),
   p("Cada punto representa una mesa. En el eje horizontal se representa la diferencia de votos totales, y en el eje vertical la diferencia de votos para la fuerza política elegida. La fuente de los datos es ",
     a("@ken4rab",
       href = "https://twitter.com/ken4rab",
       target = "_blank"), ".",
     "El código de la app y del script que prepara el archivo gz de input está aquí en ",
     a("Github.",
       href = "https://github.com/rquiroga7/analisis_electoral_2023_arg",
       target = "_blank"), " Código escrito por ",
          a("Rodrigo Quiroga",
       href = "https://twitter.com/rquiroga777",
       target = "_blank"),". La línea naranja indica donde deberían caer las mesas si todo el aumento de participación resultara en un aumento de votos para esta fuerza. Si las mesas se encuentran por arriba de esta línea, sugiere transferencia de votos de otras fuerzas a ésta, y por debajo indica lo contrario. Se provee una checkbox para filtrar las mesas con participación menor a 50 en PASO o generales. Como los datos son provisorios, puede haber errores de carga de telegramas.")
  ),
  mainPanel(
   girafeOutput("scatterplot")
  )
 )
)

# Define server
server <- function(input, output) {
 df4_filtered <- reactive({
  if (input$participacion) {
   df4 %>% filter(party == input$party, Provincia %in% input$provincia, participacion_paso > 50, participacion_general > 50)
  } else {
   df4 %>% filter(party == input$party, Provincia %in% input$provincia)
  }
 })

 output$scatterplot <- renderGirafe({
  gg <- ggplot(df4_filtered(), aes(x = diferencia_participacion, y = diferencia, color = Provincia, tooltip = paste0("Provincia: ", Provincia, "<br>",
                                                                                                                     "Participación PASO: ", participacion_paso, "<br>",
                                                                                                                     "Participación generales: ", participacion_general, "<br>",
                                                                                                                     "Votos PASO: ", votos_PASO, "<br>",
                                                                                                                     "Votos generales: ", votos_generales,"<br>",
                                                                                                                     "Mesa: ", cod_seccion
                                                                                                                   ))) +
   geom_point_interactive(alpha = 0.2) +
   geom_abline(intercept = 0, slope = 1, color = "orange") +
   ggtitle(paste0(input$party, " - Diferencia de votos Generales vs PASO (2023)")) +
   labs(x = "Diferencia participación ", y = "Diferencia votos")+
   facet_wrap(~party, scales = "fixed") +
   theme_light() +
   theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0))
  girafe(ggobj = gg)
 })
}

# Run the app
shinyApp(ui = ui, server = server)
