library(dplyr)
library(ggplot2)
#get library with "gather" function
library(tidyr)
library(ggiraph)
library(scales)

# Read the zipped CSV file
dfp <- read.csv2(unz("pivot-mesas-paso-2023.csv.zip", "pivot-mesas-paso-2023.csv"))

# Extract the province name from the first column
dfp$province <- as.numeric(substr(dfp$cod_seccion, 1, 2))

# Read the district names from the "distrito.csv" file, leave leading zeroes
districts <- read.csv2("distrito.csv", header = FALSE, sep = ";", stringsAsFactors = FALSE)
names(districts) <- c("numero", "provincia")

#Merge datasets
dfp2 <- merge(dfp, districts, by.x = "province", by.y = "numero", all.x = TRUE) %>%
  mutate(JxC=bullrich+larreta,UxP=grabois+massa,LLA=milei)

#Calculate proportion of votes for each party, where columns 3 to 7 are all divided by column 8
dfp3 <- dfp2 %>%
mutate(participacion_paso=votantes)


# Read the zipped CSV file
df <- read.csv2(unz("pivot-mesas-general-2023.csv.zip", "pivot-mesas-general-2023.csv"))

# Extract the province name from the first column
df$province <- as.numeric(substr(df$cod_seccion, 1, 2))

# Read the district names from the "distrito.csv" file, leave leading zeroes
districts <- read.csv2("distrito.csv", header = FALSE, sep = ";", stringsAsFactors = FALSE)
names(districts) <- c("numero", "provincia")

#Merge datasets
df2 <- merge(df, districts, by.x = "province", by.y = "numero", all.x = TRUE) %>%
rename(JxC=bullrich, UxP=massa, LLA=milei)

#Calculate proportion of votes for each party, where columns 3 to 7 are all divided by column 8
df3 <- df2 %>%
mutate(participacion_general=votantes)

#Pivot df3 to long, values from JxC, UxP and LLA columns to a single column, with the column name in a new column called "party"
df3_long <- df3 %>%
  pivot_longer(cols = c(JxC,UxP,LLA), names_to = "party", values_to = "votos_generales") %>%
  select(-province,-bregman,-schiaretti)

#Pivot df3 to long, values from JxC, UxP and LLA columns to a single column, with the column name in a new column called "party"
dfp3_long <- dfp3 %>%
  pivot_longer(cols = c(JxC,UxP,LLA), names_to = "party", values_to = "votos_PASO") %>%
  select(-province,-bregman,-bullrich,-grabois,-schiaretti,-larreta,-massa,-milei,-otros,-solano)


#Merge dfp3_long and df3_long by party and cod_seccion
df4 <- merge(dfp3_long, df3_long, by=c("party","cod_seccion"), all.x = TRUE) %>%
  mutate(diferencia=votos_generales-votos_PASO) %>%
  mutate(diferencia_participacion=participacion_general-participacion_paso) %>%
  #remove rows with NA values
  na.omit()

df4.dat<-df4 %>%
  group_by(provincia.x, party) %>%
  mutate(mean_PASO=median(votos_PASO)) %>%
  mutate(mean_generales=median(votos_generales)) %>%
  select(provincia.x,party,mean_PASO,mean_generales) %>%
  distinct() %>%
  ungroup()





df5<-df4 %>% filter(participacion_paso>50 & participacion_general>50 )
df5 %>% filter(diferencia_participacion < -100) %>% group_by(provincia.x) %>% count()

library(shiny)
library(ggplot2)
library(ggiraph)

# Define UI
ui <- fluidPage(
 titlePanel("Diferencia de votos PASO vs generales 2023"),
 sidebarLayout(
  sidebarPanel(
   selectInput("party", "Seleccione un partido:", choices = unique(df4$party)),
   checkboxInput("participacion", "Filtrar por participación <50 (probables errores):", value = FALSE),
   checkboxGroupInput("provincia", "Seleccione una provincia:", choices = unique(df4$provincia.x), selected = unique(df4$provincia.x)),
   p("La fuente de los datos es ",
     a("Alejandro Barañek",
       href = "https://twitter.com/ken4rab",
       target = "_blank"), ".",
     "El código de la app será subido pronto a",
     a("Github.",
       href = "https://github.com/rquiroga7/",
       target = "_blank"), " Código original de ",
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
 output$scatterplot <- renderGirafe({
  if (input$participacion) {
   df4_filtered <- df4 %>% filter(party == input$party, provincia.x %in% input$provincia, participacion_paso > 50, participacion_general > 50)
  } else {
   df4_filtered <- df4 %>% filter(party == input$party, provincia.x %in% input$provincia)
  }
  gg <- ggplot(df4_filtered, aes(x = diferencia_participacion, y = diferencia, color = provincia.x, tooltip = paste0("Provincia: ", provincia.x, "<br>",
                                                                                                                     "Participación PASO: ", participacion_paso, "<br>",
                                                                                                                     "Participación generales: ", participacion_general, "<br>",
                                                                                                                     "Votos PASO: ", votos_PASO, "<br>",
                                                                                                                     "Votos generales: ", votos_generales))) +
   geom_point_interactive(alpha = 0.2) +
   geom_abline(intercept = 0, slope = 1, color = "orange") +
   ggtitle(paste0(input$party, " - Diferencia de votos PASO vs generales 2023")) +
   facet_wrap(~party, scales = "fixed") +
   theme_light() +
   theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0))
  girafe(ggobj = gg)
 })
}

# Run the app
shinyApp(ui = ui, server = server)
