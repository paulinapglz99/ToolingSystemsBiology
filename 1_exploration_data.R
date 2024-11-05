#Exploration of data
setwd("~/tooling_up_systems_bio/RawData/")
dirnames <- dir()

dirnames[1]

#
x<- vroom::vroom("/home/paulinapg/tooling_up_systems_bio/RawData/Demengeot/2023-01-11 plate_1.csv")

# Pivot data
x_long <- x %>%
  pivot_longer(cols = -dilution, names_to = "sample", values_to = "value")

#signal vs dilution
# Plot
ggplot(x_long, aes(x = dilution, y = value, color = sample)) +
  geom_line() +
  geom_point() +
  labs(x = "Dilution", y = "Signal", title = "Dilution vs Signal") +
  theme_minimal()

# Plot
ggplot(x_long, aes(x = dilution, y = value, color = sample)) +
  geom_line() +
  geom_point() +
  scale_x_log10() + #logaritmic
  labs(x = "Dilution", y = "Signal", title = "Dilution vs Signal") +
  theme_minimal()

#Scatterplot

x %>% ggplot(aes(x = dilution, y = Reference1)) +
  geom_point() +
  scale_x_log10() +
  ggtitle("Reference1") +
  xlab("Dilution") + ylab("Signal")

#Calculate standard deviation, mean, and plot 

x.x <- x %>% summarise(
  across(everything(), list(
    mean = ~ mean(.),
    sd = ~ sd(.),
    mean_plus_1sd = ~ mean(.) + sd(.),
    mean_plus_2sd = ~ mean(.) + 2 * sd(.),
    mean_plus_3sd = ~ mean(.) + 3 * sd(.)
  ))
) 

# Cargar librerías
library(tidyverse)

# Leer los datos (asegúrate de cambiar la ruta del archivo)
datos <- read.csv("ruta/del/archivo/datos.csv")

# Seleccionar solo las columnas deseadas para calcular estadísticas
datos_seleccion <- datos %>% select(Reference1, Reference2, Control001)

# Calcular las estadísticas
estadisticas <- x %>%
  summarise(
    across(everything(), list(
      mean = ~ mean(.),
      sd = ~ sd(.),
      mean_plus_1sd = ~ mean(.) + sd(.),
      mean_plus_2sd = ~ mean(.) + 2 * sd(.),
      mean_plus_3sd = ~ mean(.) + 3 * sd(.)
    ))
  ) %>%
  pivot_longer(cols = everything(), names_to = c("Variable", "Statistic"), names_sep = "_") %>%
  pivot_wider(names_from = "Statistic", values_from = "value")

# Transformar los datos al formato largo para el gráfico
x_long <- x %>%
  pivot_longer(cols = -dilution, names_to = "sample", values_to = "value")

# Graficar
ggplot(x_long, aes(x = dilution, y = value, color = sample)) +
  geom_line() +
  geom_point() +
  scale_x_log10() + # Opcional, si quieres que el eje x sea logarítmico
  labs(x = "Dilution", y = "Value", title = "Plot de todas las muestras vs Dilution") +
  theme_minimal() +
  # Agregar líneas horizontales para cada estadística
  geom_hline(data = estadisticas %>% filter(Variable == "Reference1"),
             aes(yintercept = mean, linetype = "Reference1 Mean"), color = "blue") +
  geom_hline(data = estadisticas %>% filter(Variable == "Reference1"),
             aes(yintercept = mean_plus_1sd, linetype = "Reference1 Mean + 1 SD"), color = "blue", linetype = "dashed") +
  geom_hline(data = estadisticas %>% filter(Variable == "Reference1"),
             aes(yintercept = mean_plus_2sd, linetype = "Reference1 Mean + 2 SD"), color = "blue", linetype = "dotted")
             