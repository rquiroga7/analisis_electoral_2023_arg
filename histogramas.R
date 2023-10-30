library(dplyr)
library(readr)
library(tidyr)
library(scales)
library(ggplot2)

#Generate my own file
gen <- read_csv("ResultadosElectorales_2023.zip")
genB<- read_csv("2023_generales.zip")
gen2<-gen %>%
 filter(cargo_id==1) %>%
 mutate(cod_seccion=paste0(sprintf("%02d", distrito_id),sprintf("%03d", seccion_id),sprintf("%05d", mesa_id),"X")) %>%
 group_by(cod_seccion) %>%
 mutate(votantes=sum(votos_cantidad)) %>%
 mutate(blanco_y_nulo=sum(ifelse(votos_tipo!="POSITIVO",votos_cantidad,0))) %>%
 mutate(habilitados=mesa_electores) %>%
 ungroup()

gen3<-gen2 %>% filter(!is.na(agrupacion_nombre)) %>% select(-agrupacion_id) %>% group_by(cod_seccion) %>% pivot_wider(names_from="agrupacion_nombre",values_from="votos_cantidad")
gen3<-gen3[,21:29]
names(gen3)<- c("cod_seccion","votantes","blanco_y_nulo","habilitados","massa","bullrich","milei","bregman","schiaretti")
gen3<-gen3[,c(1,8,6,5,7,9,2,3,4)]
g2023<-gen3

g2023$province <- as.numeric(substr(g2023$cod_seccion, 1, 2))

# Read the district names from the "distrito.csv" file, leave leading zeroes
districts <- read.csv2("distrito.csv", header = FALSE, sep = ";", stringsAsFactors = FALSE)
names(districts) <- c("numero", "provincia")

#Merge datasets
g2023 <- merge(g2023, districts, by.x = "province", by.y = "numero", all.x = TRUE) %>%
 rename(JxC=bullrich, UxP=massa, LLA=milei,FIT=bregman,UxNP=schiaretti)

g2023 %>%
 select(JxC,UxP,LLA,) %>%
 gather(key = "party", value = "votes") %>%
 ggplot(aes(x = votes, fill=party)) +
 theme_light()+
 xlab("Votos") + ylab("Cantidad de mesas")+
 ggtitle("Histograma de votos - PASOes 2023") +
 geom_histogram(binwidth = 1) +
 facet_grid(~party, scales = "fixed") +
 scale_fill_manual(values = c("orange","purple","lightblue3")) +
 xlim(-1, 300)+
 theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("histograma_generales_facet.png", width = 8, height = 5, dpi = 300)


g2023 %>%
 filter(votantes>0) %>%
 select(JxC,UxP,LLA,) %>%
 gather(key = "party", value = "votes") %>%
 ggplot(aes(x = votes, fill=party)) +
 theme_light()+
 xlab("Votos") + ylab("Cantidad de mesas")+
 ggtitle("Histograma de votos - PASOes 2023") +
 geom_histogram(binwidth = 1) +
 facet_grid(~party, scales = "fixed") +
 scale_fill_manual(values = c("orange","purple","lightblue3")) +
 xlim(-1, 300)+
 theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("histograma_generales_facet_no0.png", width = 8, height = 5, dpi = 300)

#Cuenta mesas con 0 votos
g2023 %>% filter(LLA==0) %>% count()
g2023 %>% filter(JxC==0) %>% count()
g2023 %>% filter(UxP==0) %>% count()
g2023 %>% filter(votantes==0) %>% count()
#Cuenta mesas procesadas con 0 votos
g2023 %>% filter(LLA==0 & votantes>0) %>% count()
g2023 %>% filter(JxC==0 & votantes>0) %>% count()
g2023 %>% filter(UxP==0 & votantes>0) %>% count()

#Calcula resultado global del recuento provisorio
g2023 %>%
 summarize(JxCp=sum(JxC)/(sum(votantes)-sum(blanco_y_nulo)),LLAp=sum(LLA)/(sum(votantes)-sum(blanco_y_nulo)),UxPp=sum(UxP)/(sum(votantes)-sum(blanco_y_nulo)))

#Calcula resultado global del recuento provisorio, eliminando mesas con 0 votos
g2023 %>%
 filter(JxC>0,LLA>0,UxP>0) %>%
 summarize(JxCp=sum(JxC)/(sum(votantes)-sum(blanco_y_nulo)),LLAp=sum(LLA)/(sum(votantes)-sum(blanco_y_nulo)),UxPp=sum(UxP)/(sum(votantes)-sum(blanco_y_nulo)))

#Calcula resultado global del recuento provisorio, eliminando las mesas de Santiago del Estero
g2023 %>%
 filter(provincia!="Santiago del Estero") %>%
 summarize(JxCp=sum(JxC)/(sum(votantes)-sum(blanco_y_nulo)),LLAp=sum(LLA)/(sum(votantes)-sum(blanco_y_nulo)),UxPp=sum(UxP)/(sum(votantes)-sum(blanco_y_nulo)))

g2023 %>%
 pivot_longer(cols = c(JxC,UxP,LLA), names_to = "party", values_to = "votos") %>%
 mutate(votos=votos/votantes) %>%
ggplot(aes(x = votos)) +
 theme_light()+
 ggtitle("Histograma de votos - generales 2023") +
 geom_histogram(binwidth = 0.01) +
 geom_histogram(data=g2023 %>%
                 pivot_longer(cols = c(JxC,UxP,LLA), names_to = "party", values_to = "votos") %>%
                 mutate(votos=votos/votantes) %>% filter( provincia == "Santiago del Estero" | provincia == "Buenos Aires" ),binwidth = 0.01,fill="green") +
 geom_histogram(data=g2023 %>%
                 pivot_longer(cols = c(JxC,UxP,LLA), names_to = "party", values_to = "votos") %>%
                 mutate(votos=votos/votantes) %>% filter(provincia =="Córdoba" ),binwidth = 0.01,fill="red") +
 facet_wrap(~party, scales = "fixed") +
 xlim(0,1)+
 theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("histograma_rojo.png", width = 8, height = 5, dpi = 300)

g2023 %>%
 pivot_longer(cols = c(JxC,UxP,LLA), names_to = "party", values_to = "votos") %>%
 mutate(votos=votos/votantes) %>%
 filter(provincia =="CABA" | provincia =="Salta" ) %>%
 ggplot(aes(x = votos)) +
 theme_light()+
 ggtitle("Histograma de votos CABA + Salta- generales 2023") +
 geom_histogram(binwidth = 0.01) +
 facet_wrap(~party, scales = "free_y") +
 xlim(0, 1)+
 theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("histograma_salta_caba.png", width = 8, height = 5, dpi = 300)



