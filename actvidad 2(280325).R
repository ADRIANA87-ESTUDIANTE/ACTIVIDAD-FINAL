### ACTIVIDAD FINAL

# Cargar paquetes
library(readr)  # Para leer archivos CSV
library(dplyr)  # Para manipulación de datos

if (!require(readr)) install.packages("readr", dependencies = TRUE)
if (!require(dplyr)) install.packages("dplyr", dependencies = TRUE)
# 2️ CARGAR DATOS DESDE UN ARCHIVO CSV ----
print("2. Cargando el dataset desde un archivo CSV...")

# Especifica la ruta del archivo CSV (asegúrate de cambiar "salariomujeres" por el nombre real del archivo)
archivo_csv   <-"Descargar salariosmujeres.csv" # Reemplaza por tu archivo CSV

# Cargar datos en un data frame
datos <-read_csv("salariosmujeres.csv")

#revisar el dataset
print("3, explorando el dataset cargado...")

head(datos)

#ver la primeras fila

print("primera filas del dataset")

##DIMENSIONES DEL DATASET
print("dimensiones del dataset(filas x columnas")
dim(datos)

#nombre columnas
print("nombre de las columnas")
names(datos)

#resumen estadistico del dataset
 print("resumen estadistico de las variables numericas")
"resumen estadistico de las variables numericas"
summary(datos)

media <-mean(datos$Salario,na.rm = TRUE)
print("promedio de los datos")
print(media)

mediana <-median(datos$Salario,na.rm=TRUE)
print("mediana de los datos")
print(mediana)

##limpieza datos

# ver valores faltantes (na)
print("identifica valores faltantes.... ")

#ver cuantos valores faltan por columnas
print("numero de valores na por columna:")
colSums(is.na(datos))

#mostrar las filas con valores faltante
print("filas con valores na:")
datos[!complete.cases(datos),]
#eliminar valores faltantes
datos_limpios <-na.omit(datos)
#guardar archivo csv limpio
write.csv(datos,"datos-limpios.csv")

#eliminar registros duplicados
datos_sin_duplicados <-datos_limpios[!duplicated(datos_limpios),]

#ver las primeras filas de datos limpios
head(datos_sin_duplicados)

#filtrado de datos edad 
datos_filtrado  <-datos_sin_duplicados[datos_sin_duplicados$Edad %in%c("35","60"),]

names(datos_sin_duplicados)


#convertir la columna de salarios a numericos y manejar valores NA
datos$Salario <-as.numeric(datos$Salario)

##verificar valores NA despues de la conversion
print("numero de valores NA por columna despues de la limpieza")
colSums(is.na(datos))

##reemplazar los valores NA de salario con el promedio (media)
datos$Salario[is.na(datos$Salario)] <-mean(datos$Salario,na.rm = TRUE )

#verificar sustitucion
print("numero de valores na de la columna despues de la sustitucion:")
colSums(is.na(datos))

#ver resumen de datos sin duplicados
print("resumen estadistico de las variables numericas sin duplicados")
summary(datos_sin_duplicados)
##Verificar si hay variables numéricas
variables_numericas <- names(datos_sin_duplicados)[sapply(datos_sin_duplicados, is.numeric)]
if (length(variables_numericas) > 0) {


## VISUALIZACION BASICA DE UNA VARIABLE

print("CREANDO UN HISTOGRAMA")
install.packages("ggplot2")

library(ggplot2)

ggplot(datos,aes(x = Salario)) + 
  geom_histogram(binwidth = 2000, fill = "yellow", color = "blue", alpha = 0.7) + 
  labs(title = "frecuencia salarios mujeres",
       x= "variable numerica",
       y= "frecuencia") + 
  theme_linedraw()


ggplot(datos,aes(x = Edad)) + 
  geom_histogram(binwidth = 20, fill = "pink", color = "blue", alpha = 1) + 
  labs(title = "frecuencia edad mujeres",
       x= "variable numerica",
       y= "frecuencia") + 
  theme_classic()

print("grafico")


  