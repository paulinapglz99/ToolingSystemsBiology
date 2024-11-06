#Script for extract features of the csv

setwd("~/tooling_up_systems_bio/ToolingSystemsBiology/")

dir <- getwd()

# Cargar las librerías necesarias
pacman::p_load("tidyverse", "ggplot2", "stringr")

#Read data

pigs <- vroom::vroom(file= "final_data.csv")

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



