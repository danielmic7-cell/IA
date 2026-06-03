#Se agrego esta linea al inicio del archivo
rm(list=ls())
#cargar librerias  readlx, dplyr, tidyverse
library(readxl)
library(dplyr)
library(tidyverse)

# ── Excel (.xlsx / .xls) ──────────────────────────────────────
library(readxl)
df <- read_excel("C:/Users/UCACUE/Downloads/P_Data_Extract_From_World_Development_Indicators.xlsx", sheet = "Data")
colnames(df)
# Mostrar las primeras 6 filas
head(df)
# Calcular estadísticas descriptivas de todas las variables numéricas
df |> dplyr::select(where(is.numeric)) |> summary()
library(tidyverse)

# Calcular estadísticas descriptivas de todas las variables numéricas
df |>
  mutate(across(starts_with("20"), as.numeric)) |>
  select(where(is.numeric)) |>
  summary()
# Calcular el promedio de la variable principal agrupado por país
df |>
  mutate(`2023 [YR2023]` = as.numeric(`2023 [YR2023]`)) |>
  group_by(`Country Name`) |>
  summarise(promedio = mean(`2023 [YR2023]`, na.rm = TRUE))

# Calcular el promedio agrupado por país y ordenar de mayor a menor
df |>
  mutate(`2023 [YR2023]` = as.numeric(`2023 [YR2023]`)) |>
  group_by(`Country Name`) |>
  summarise(promedio = mean(`2023 [YR2023]`, na.rm = TRUE)) |>
  arrange(desc(promedio))

# Crear un gráfico de barras con los 10 países principales
df |>
  mutate(`2023 [YR2023]` = as.numeric(`2023 [YR2023]`)) |>
  group_by(`Country Name`) |>
  summarise(promedio = mean(`2023 [YR2023]`, na.rm = TRUE)) |>
  arrange(desc(promedio)) |>
  slice_head(n = 10) |>
  ggplot(aes(x = reorder(`Country Name`, promedio), y = promedio)) +
  geom_col() +
  coord_flip()


library(tidyverse)

# Crear gráfico de barras con los 10 países principales
# Agregar título y etiquetas de los ejes en español
df |>
  mutate(`2023 [YR2023]` = as.numeric(`2023 [YR2023]`)) |>
  group_by(`Country Name`) |>
  summarise(promedio = mean(`2023 [YR2023]`, na.rm = TRUE)) |>
  arrange(desc(promedio)) |>
  slice_head(n = 10) |>
  ggplot(aes(x = reorder(`Country Name`, promedio), y = promedio)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 países por promedio de indicadores (2023)",
    x = "País",
    y = "Promedio"
  )

# Hacer una regresión lineal simple entre dos variables numéricas (2022 y 2023)
modelo <- df |>
  mutate(
    y2022 = as.numeric(`2022 [YR2022]`),
    y2023 = as.numeric(`2023 [YR2023]`)
  ) |>
  filter(!is.na(y2022), !is.na(y2023)) |>
  lm(y2023 ~ y2022, data = _)

# Imprimir el resumen de la regresión
summary(modelo)

# Interpretar el R-cuadrado:
# R² = 0.9946, lo que significa que el 99.46% de la variación en los valores
# del año 2023 es explicada por los valores del año 2022.
# Esto indica una relación lineal casi perfecta entre ambos años,
# lo cual es esperado ya que son indicadores consecutivos del mismo conjunto de datos.

# Interpretar el R-cuadrado:
# R² = 0.9946, lo que significa que el 99.46% de la variación en los valores
# del año 2023 es explicada por los valores del año 2022.
# Esto indica una relación lineal casi perfecta entre ambos años,
# lo cual es esperado ya que son indicadores consecutivos del mismo conjunto de datos.
