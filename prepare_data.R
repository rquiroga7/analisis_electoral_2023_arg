library(dplyr)
library(readr)
library(tidyr)
library(scales)


# Read the zipped CSV file
dfp <- read_csv2("pivot-mesas-paso-2023.csv.zip")

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
df <- read_csv2("pivot-mesas-general-2023.csv.zip")

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

df4 <- df4 %>%
 select(cod_seccion,provincia.x, party, diferencia_participacion, diferencia,participacion_paso,participacion_general,votos_PASO,votos_generales) %>%
 rename(Provincia=provincia.x) %>%
 ungroup()

# Save dataframe to a zipped file
write_csv(df4, "df4.gz")
