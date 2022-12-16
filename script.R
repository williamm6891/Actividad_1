################################## DECLARACIÓN DE LIBRERIAS ####################################

library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(tidyquant)
library(rjson)
library(modeest)
theme_set(theme_minimal())

#####################################LEER ARCHIVOS DE CASOS COVID ##############################


df <- jsonlite::fromJSON("https://raw.githubusercontent.com/andrab/ecuacovid/master/datos_crudos/positivas/2020/cantones_mensual.inec.json")
df1 <- filter(df, canton == 'Quito')
df2 <- read.csv2("C:/Users/50400/OneDrive/Escritorio/AeD/Demanda.csv")

##################################### FORMATO A VARIABLES ######################################

meses <- c("2020-03-01", "2020-04-01", "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01", "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01")
for (i in 1:10) {
  df1[i,11] <- meses[i]
  print(df1[i,11])
}

df1$mes <- as.Date(df1$mes, format =  "%Y-%m-%d")

df2$Fecha <- as.Date(df2$Fecha, format =  "%d/%m/%Y")
df2$Potencia_MW <- as.numeric(df2$Potencia_MW)
df2$PotenciaS_MW <- as.numeric(df2$PotenciaS_MW)
df2$Hora <- as.Date(df2$Hora, format = "%H:%M:%S")

tipo <- sapply(df2, class)
tipo1 <- sapply(df1, class)

################################# VISUALIZACIÓN EN GGPLOT2###################################

## Sombreado en el intervalo de interes


rect <- data.frame(
  xmin = as.Date("2020-03-01"),
  xmax = as.Date("2020-06-01"),
  ymin = -Inf,
  ymax = Inf)


df1 %>% 
  ggplot(aes(mes, nuevas)) +
  geom_line(color = '#E59837', size = 1.2)  +
  geom_point() +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            rect, alpha = .4, inherit.aes = FALSE) +
  ggtitle("Casos covid-19 año 2020") +
  geom_text(
    label = df1$total,
    nudge_x=2, nudge_y=1.5,
    check_overlap=T
  )

df2 %>% 
  ggplot(aes(Fecha, Potencia_MW, group = 1)) +
  geom_line(color = '#E51865', size = 1.2)  +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            rect, alpha = .4, inherit.aes = FALSE) +
  geom_point() +
  ggtitle("Demanda eléctrica (MW) años 2019 - 2020") +
  geom_text(
    label = df2$Potencia_MW,
    nudge_x=3, nudge_y=1.5,
    check_overlap=T
  )

####################################MEDIDAS DE TENDENCIA CENTRAL################################

## Media

media <- mean(df1$nuevas, na.rm = FALSE)
sprintf("La media es: %g", media)

mediaPotencia <- mean(df2$Potencia_MW, na.rm = FALSE)
sprintf("La media es: %g", mediaPotencia)

## Mediana

mediana <- median(df1$nuevas, na.rm = FALSE)
sprintf("La mediana es: %g", mediana)

medianaPotencia <- median(df2$Potencia_MW, na.rm = FALSE)
sprintf("La mediana es: %g", medianaPotencia)

## Moda

moda <- mfv1(df1$nuevas)
sprintf("La moda es: %g", moda)

modaPotencia <- mfv1(df2$Potencia_MW)
sprintf("La moda es: %g", modaPotencia)

######################################################################################

titulo <- "Histograma de los datos"
subtitulo <- paste("Media=",media, " Mediana = ",mediana, " Moda=",moda)
ggplot(data = df1, mapping = aes(x=df1$nuevas)) +
  geom_histogram(bins=10) +
  ggtitle(titulo, subtitle = subtitulo) +
  xlab('Valores') + ylab('Frecuencia') +
  geom_vline(aes(xintercept = media,
                 color = "media"),
             linetype = "dashed",
             size = 1)  +
  geom_vline(aes(xintercept = mediana,
                 color = "mediana"),
             linetype = "dashed",
             size = 1) +
  geom_vline(aes(xintercept = moda,
                 color = "moda"),
             linetype = "dashed",
             size = 1) 

Xiboxplot <- boxplot(df1$nuevas, main="Número de casos covid-19 año 2020", ylab="Cantidad de nuevos casos")

###################################MEDIDAS DE DISPERSION######################################

# Desviación típica

d <- sd(df1$total, na.rm = FALSE)
d1 <- sd(df2$Potencia_MW, na.rm = FALSE)

# Varianza

v <- var(df1$total, na.rm = FALSE)
v1 <- var(df2$Potencia_MW, na.rm = FALSE)

# Modelo estadístico

my.df <- merge(df1, df2)
my.model <- lm(my.df$Potencia_MW ~ my.df$nuevas, data=my.df)
plot(my.model)
summary(my.model)

