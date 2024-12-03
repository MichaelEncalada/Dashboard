
# clean memory ------------------------------------------------------------
rm(list = ls())


# read in data ------------------------------------------------------------
#set working directory

library(readxl)
library(dplyr)
library(tidyr)


filename="BASE_CELULAR.xlsx"
data=read_excel(filename)

selected_data <- data %>%
  select(SIT_PERSONA, ID_PERSONA, xx, yy, COMISARIA, ID_COMISARIA, DPTO_HECHO, PROV_HECHO, 
         DIST_HECHO, SEXO, EDAD, ANIO, MES)

mydata <- selected_data %>% drop_na()

mydata <- mydata %>%
  filter(SEXO != "NULL")

# Corrección manual de los valores de la columna EDAD

mydata$EDAD <- as.numeric(mydata$EDAD)

# Verificar la estructura inicial de la columna EDAD
str(mydata$EDAD)

# Corrección manual de los valores en la columna EDAD
mydata <- mydata %>%
  mutate(EDAD = case_when(
    EDAD == 1 ~ 10,
    EDAD == 105 ~ 10,
    EDAD == 117 ~ 11,
    EDAD %in% c(121, 122, 123, 124) ~ 12,
    EDAD == 2 ~ 20,
    EDAD == 4 ~ 40,
    EDAD == 6 ~ 60,
    EDAD == 7 ~ 70,
    EDAD == 8 ~ 80,
    EDAD == 9 ~ 90,
    TRUE ~ EDAD # Mantener los valores correctos sin cambios
  ))

# Verificar las correcciones realizadas
summary(mydata$EDAD)
unique(mydata$EDAD)


mydata <- mydata %>%
  filter(EDAD != "NULL")


# see data ----------------------------------------------------------


head(mydata)


# see data types ----------------------------------------------------------

str(mydata)


mydata <- mydata %>%
  mutate(MES = as.character(MES))

mydata <- mydata[as.numeric(mydata$EDAD),]
mydata <- mydata[mydata$EDAD >= 10 & mydata$EDAD <= 80, ]

# deliverable 1 ----------------------------------------------------------
library(ggplot2)

library(ggplot2)

del1Draft <- ggplot(data = mydata) +
  geom_bar(aes(x = factor(ANIO), fill = SEXO), position = "dodge") +  # Barras agrupadas por sexo
  scale_x_discrete(labels = function(x) paste(x)) +  # Cambiar etiquetas del eje x
  scale_fill_manual(values = c("F" = "pink", "M" = "blue"), labels = c("F" = "Femenino", "M" = "Masculino")) +  # Colores personalizados
  labs(
    x = "Año",
    y = "Cantidad",
    title = "Robo de celulares en los últimos años según sexo",
    fill = "Sexo"
  ) +  # Etiquetas
  theme_minimal()  # Estilo minimalista

# Mostrar gráfico
del1Draft


# save del1Draft ----------------------------------------------------------
saveRDS(del1Draft, file = "del1Draft.rds")


# deliverable 2 ----------------------------------------------------------


library(ggplot2)

# Crear un histograma de la variable EDAD
del2Draft <- ggplot(data = mydata, aes(x = EDAD)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  scale_x_continuous(
    limits = c(10, 90), 
    breaks = seq(10, 90, by = 15),  # Ajustar para rangos de 15 en 15
    labels = c("10", "25", "40", "55", "70", "85")
  ) +
  labs(
    title = "Edad de víctimas de robo de celular",
    x = "Edad (años)",
    y = "Frecuencia"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, color = "darkblue"),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray90")
  )

# Mostrar gráfico
del2Draft

# save del2Draft ----------------------------------------------------------
saveRDS(del2Draft, file = "del2Draft.rds")


# deliverable 3 ----------------------------------------------------------

## creo que lo mejor es un mapa de comisarías 

del3Draft= base + geom_point(aes(x=EDAD,
                                 y=ANIO))
del3Draft 




# save del3Draft ----------------------------------------------------------
saveRDS(del3Draft, file = "del3Draft.rds")


# deliverable 4  ----------------------------------------------------------

library(sf)
county_map=sf::read_sf("peru_provincial_simple.geojson")
head(county_map)
head(mydata)

# merge data into map ----------------------------------------------------------

myMapLunch <- merge(county_map, mydata, by.x = "NOMBPROV", by.y = "PROV_HECHO")

head(myMapLunch)

# prepare plot

# Cargar librerías necesarias
library(ggplot2)
library(sf)  
library(dplyr)


# número de incidentes (observaciones) por provincia
myMapLunch_summarized <- myMapLunch %>%
  filter(NOMBPROV != "LIMA") %>%  # Excluir la provincia de LIMA
  group_by(NOMBPROV) %>%
  summarise(num_incidentes = n()) %>%
  st_drop_geometry()  # Eliminar la información de geometría

# Unir los datos resumidos a la geometría
mapa_datos <- myMapLunch %>%
  left_join(myMapLunch_summarized, by = "NOMBPROV") %>%
  filter(NOMBPROV != "LIMA")  # Excluir la provincia de LIMA

# Crear el mapa

del4Draft=ggplot(data = mapa_datos) +
  geom_sf(aes(fill = num_incidentes), color = "gray80", size = 0.2) +  # Borde gris claro
  scale_fill_viridis_c(
    option = "magma", name = "N° de incidentes", direction = -1
  ) +  # Escala de colores "magma"
  labs(
    title = "Número de robos de celulares por provincia",
    subtitle = "Visualización basada en datos de incidentes reportados",
    caption = "Fuente: Base de datos de denuncias"
  ) +
  coord_sf(expand = FALSE) +  # Remueve espacios extra
  theme_minimal() +  # Tema limpio
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "navy"),
    plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic", color = "gray40"),
    plot.caption = element_text(size = 10, face = "italic", color = "gray30"),
    panel.grid.major = element_line(color = "gray90", linetype = "dotted", size = 0.5),
    legend.position = "bottom",
    legend.box.margin = margin(t = 10)
  )

del4Draft

# save del4Draft ----------------------------------------------------------
saveRDS(del4Draft, file = "del4Draft.rds")