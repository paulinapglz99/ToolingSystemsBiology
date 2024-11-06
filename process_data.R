#Script for extract features of the csv

setwd("~/tooling_up_systems_bio/ToolingSystemsBiology/")

dir <- getwd()

# Cargar las librerías necesarias
pacman::p_load("tidyverse", "ggplot2", "stringr")

#Read data

pigs <- vroom::vroom(file= "final_data.csv")

#see colnames
colnames(pigs)

#Choose columns we need
pigs <- pigs %>%
  select(
    FilePath, date_lab_plate, file_name, Date, Lab, Plate, background_mean, background_SD,
    ends_with("_titre")
  )

# Normalize data

pigs <- pigs %>%
  mutate(
    across(
      ends_with("_titre"),
      ~ 1 / .,  # Calcula el inverso de cada valor
      .names = "inv_{.col}"  # Crea una nueva columna con el prefijo "inv_"
    )
  ) %>%
  mutate(
    Reference_average = rowMeans(select(., Reference1_titre, Reference2_titre), na.rm = TRUE)
  )

#normalize

pigs <- pigs %>%
  mutate(
    across(
      starts_with("inv_"),
      ~ . / Reference_average,  # Divide cada columna inv_ por Reference_average
      .names = "{.col}_norm"  # Crea nuevas columnas con el sufijo "_norm"
    )
  )

pigs_norm <- pigs %>%
  select(
    FilePath, date_lab_plate, file_name, Date, Lab, Plate, background_mean, background_SD,
    ends_with("_norm")
  )

#Plot

# Convierte todas las columnas que terminan en "_norm" a un formato largo
pigs_long <- pigs %>%
  pivot_longer(
    cols = ends_with("_norm"),
    names_to = "variable",
    values_to = "value"
  )

# Graficar distribuciones para todas las columnas "_norm"
ggplot(pigs_long, aes(x = value, fill = variable)) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Distribución de columnas _norm", x = "Valor", y = "Frecuencia") +
  theme_minimal()


# Graficar un histograma combinado de todas las columnas "_norm"
ggplot(pigs_long, aes(x = value)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histograma combinado de todas las columnas _norm", x = "Valor", y = "Frecuencia") +
  theme_minimal()


