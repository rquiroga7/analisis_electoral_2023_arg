library(dplyr)
library(readr)
library(tidyr)
library(scales)

gen <- read_csv("2023_Generales/ResultadosElectorales_2023.csv")
gen2<-gen %>%
  filter(cargo_id==1) %>%
  mutate(cod_seccion=paste0(sprintf("%02d", distrito_id),sprintf("%03d", seccion_id),sprintf("%05d", mesa_id),"X")) %>%
  group_by(cod_seccion) %>%
  mutate(votantes=sum(votos_cantidad)) %>%
  mutate(blanco_y_nulo=sum(ifelse(votos_tipo!="POSITIVO",votos_cantidad,0))) %>%
  mutate(habilitados=mesa_electores) %>%
  ungroup()

gen3<-gen2 %>% filter(!is.na(agrupacion_nombre)) %>% select(-agrupacion_id) %>% group_by(cod_seccion) %>% pivot_wider(names_from="agrupacion_nombre",values_from="votos_cantidad")
gen3<-gen3[,c(6,10,21:29)]
names(gen3)<- c("Provincia","seccion","cod_seccion","votantes","blanco_y_nulo","habilitados","UxP","JxC","LLA","bregman","schiaretti")
gen3<-gen3[,c(3,10,8,7,9,11,4,5,6,1,2)]
g2023<-gen3

#Pivot df3 to long, values from JxC, UxP and LLA columns to a single column, with the column name in a new column called "party"
df3_long <- g2023 %>%
  pivot_longer(cols = c(JxC,UxP,LLA), names_to = "party", values_to = "votos_generales") %>%
  select(-bregman,-schiaretti)

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(scales)
library(shiny)
library(ggiraph)
library(shinycssloaders)
library(shinyWidgets)


df4<-df3_long

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
                  choices = sort(unique(df4$Provincia)),
                  options = list(`actions-box` = TRUE,
                                 `deselect-all-text` = "Limpiar selección",
                                 `select-all-text` = "Todas",
                                 `none-selected-text` = "Seleccionar al menos 1 provincia"),
                  # selected = "Todas",
                  multiple = T),
      pickerInput("seccion",
                  "Seleccione una seccion:",
                  choices = sort(unique(df4$seccion)),
                  options = list(`actions-box` = TRUE,
                                 `deselect-all-text` = "Limpiar selección",
                                 `select-all-text` = "Todas",
                                 `none-selected-text` = "Seleccionar al menos 1 seccion"),
                  selected = sort(unique(df4$seccion)),
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
        ", que introdujo cambios al código que permite el correcto funcionamiento de la app en Shinyapps.io. La línea naranja indica donde deberían caer las mesas si todo el aumento de participación resultara en un aumento de votos para esta fuerza. Si las mesas se encuentran por arriba de esta línea, sugiere transferencia de votos de otras fuerzas a ésta, y por debajo indica lo contrario. Se provee una checkbox para filtrar las mesas con participación menor a 50 en PASO o generales. Como los datos son provisorios, puede haber errores de carga de telegramas.")
    ),
    mainPanel(
      conditionalPanel(condition = "input.correr > 0",
                       girafeOutput("scatterplot") %>%
                         withSpinner(color="#0dc5c1")
      ))
  )
)

# Define server
server <- function(input, output,session) {
  observeEvent(input$provincia, {
    lista_seccion <- df4 %>% ungroup() %>% filter(df4$party == input$party,df4$Provincia == input$provincia)
    updatePickerInput(session = session, inputId = "seccion",
                      choices = sort(unique(lista_seccion$seccion)), 
                      selected = sort(unique(lista_seccion$seccion)))
  })
  
  
  observeEvent(input$correr, {
    
    df4_filtered <- reactive({
      
      isolate({
        if (input$participacion) {
          lista_seccion %>% filter(seccion %in% input$seccion, votantes > 50)
        } else {
          lista_seccion %>% filter(seccion %in% input$seccion)}
      })
    })
    
    
    output$scatterplot <- renderGirafe({
      
      
      gg <- ggplot(df4_filtered(), aes(
        x = votantes,
        y = votos_generales,
        color = Provincia,
        tooltip = paste0(
          "Provincia: ",
          Provincia,
          "<br>",
          "Participación: ",
          votantes,
          "<br>",
          "Votos de la fuerza: ",
          votos_generales,
          "<br>",
          "Mesa: ",
          cod_seccion
        ))) +
        geom_point_interactive(alpha = 0.2) +
        geom_hline(yintercept = 0, color="black")+
        geom_vline(xintercept = 0, color="black")+
        ggtitle(paste0(input$party, " - Diferencia de votos Generales vs PASO (2023)")) +
        labs(x = "Votantes", y = "Votos para la fuerza")+
        # facet_wrap(~party, scales = "fixed") +
        theme_light() +
        theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0))
      
      girafe(ggobj = gg)
      
      
    })
    
    
  })
}

# Run the app
shinyApp(ui = ui, server = server)


