# Cargar datos desde un archivo CSV
pokemon <- read.csv("datos/Pokemon.csv")

# Verificar si la columna 'rank' existe
if (!("rank" %in% colnames(pokemon))) {
  stop("Error: La columna 'rank' no existe en el data frame.")
}

# Convertir a factor (manejo de valores faltantes)
if (any(is.na(pokemon$rank))) {
  warning("Advertencia: La columna 'rank' contiene valores faltantes (NA). Imputando con un valor por defecto.")
  pokemon$rank[is.na(pokemon$rank)] <- "Desconocido"  # Imputar con un valor por defecto
}

pokemon$rank <- as.factor(pokemon$rank)

# Convertir a factor
pokemon$rank <- as.factor(pokemon$rank)
pokemon$generation <- as.factor(pokemon$generation)
pokemon$type1 <- as.factor(pokemon$type1)
pokemon$type2 <- as.factor(pokemon$type2)

# Eliminar filas con valores NA en las columnas numéricas
pokemon <- pokemon[complete.cases(pokemon[, c("hp", "atk", "def", "spatk", "spdef", "speed", "total", "height", "weight")]), ]

# Eliminar la columna desc ya que no es necesaria para el análisis
pokemon$desc <- NULL

# Preparar el análisis de habilidades (separando las habilidades)
library(tidyverse)

# Crear una función para dividir y contar habilidades
contar_habilidades <- function(data) {
  data %>%
    separate_rows(abilities, sep = " ") %>%
    group_by(abilities) %>%
    summarise(n = n()) %>%
    arrange(desc(n))
}

# Contar las habilidades
frecuencia_habilidades <- contar_habilidades(pokemon)

# Mostrar las frecuencias
print("Frecuencia de Tipos Primarios:")
print(frecuencia_type1)
print("Frecuencia de Tipos Secundarios:")
print(frecuencia_type2)
print("Frecuencia de Habilidades:")
print(frecuencia_habilidades)

# Calcular la frecuencia de los tipos primarios
frecuencia_type1 <- table(pokemon$type1)

# Calcular la frecuencia de los tipos secundarios
frecuencia_type2 <- table(pokemon$type2)

# Calcular la frecuencia de Habilidades
frecuencia_habilidades <- table(pokemon$abilities)

# Mostrar las frecuencias
print("Frecuencia de Tipos Primarios:")
print(frecuencia_type1)
print("Frecuencia de Tipos Secundarios:")
print(frecuencia_type2)
print("Frecuencia de Habilidades:")
print(frecuencia_habilidades)

# 3. Análisis de Combinaciones de Tipos
pokemon_combinado <- pokemon %>%
  mutate(tipo_combinado = ifelse(is.na(type2), as.character(type1), paste(type1, type2, sep = "/")))

frecuencia_tipo_combinado <- table(pokemon_combinado$tipo_combinado)
print("Frecuencia de Combinaciones de Tipos:")
print(frecuencia_tipo_combinado)

# 4. Análisis de Habilidades por Tipo Primario
top_habilidades_por_tipo <- pokemon_combinado %>%
  group_by(type1) %>%
  separate_rows(abilities, sep = " ") %>%
  group_by(type1, abilities) %>%
  summarise(n = n()) %>%
  top_n(5, n) %>%
  arrange(type1, desc(n))

print("Top Habilidades por Tipo Primario:")
print(top_habilidades_por_tipo)

# 5. Análisis de Habilidades por Combinación de Tipos
top_habilidades_por_tipo_combinado <- pokemon_combinado %>%
  group_by(tipo_combinado) %>%
  separate_rows(abilities, sep = " ") %>%
  group_by(tipo_combinado, abilities) %>%
  summarise(n = n()) %>%
  top_n(5, n) %>%
  arrange(tipo_combinado, desc(n))

print("Top Habilidades por Combinación de Tipos:")
print(top_habilidades_por_tipo_combinado)

# Visualizaciones (Ejemplo)
library(ggplot2)

# Histograma de Type1
ggplot(pokemon, aes(x = type1)) +
  geom_bar() +
  ggtitle("Distribución de Tipos Primarios") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


system("git config --list") 

system("git config --global user.name 'vehiller'") 


system("git config --global user.email 'vehiller@gmail.com'")













