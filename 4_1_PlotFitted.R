# Cargar las librerías necesarias
library(vroom)
library(ggplot2)
library(dplyr)
library(minpack.lm) # Para la función nlsLM de ajuste no lineal
#pacman::p_load("minpack.lm")

# Cargar el archivo CSV
csv1 <- vroom("~/ToolingSystemsBiology/RawData/Demengeot/2023-01-11 plate_1.csv")

# Definir la función de Michaelis-Menten
michaelis_menten <- function(dilution, Vmax, Km) {
  (Vmax * dilution) / (Km + dilution)
}

# Ajuste no lineal de Michaelis-Menten
fit <- nlsLM(csv1$Reference1 ~ michaelis_menten(dilution, Vmax, Km),
             data = csv1,
             start = list(Vmax = max(csv1$Reference1), Km = median(csv1$dilution)))

# Extraer los parámetros ajustados
Vmax <- coef(fit)["Vmax"]
Km <- coef(fit)["Km"]

# Crear la gráfica con el ajuste
ggplot(csv1, aes(x = dilution, y = Reference1)) +
  geom_point(color = "blue") + 
  scale_x_log10() +
  labs(title = "Michaelis-Menten Fit for Plate1$Reference1",
       x = "Log10(Dilution)",
       y = "Reference01") +
  expand_limits(x = 0, y = 0) +
  theme_minimal() +
  stat_function(fun = function(x) (Vmax * x) / (Km + x), color = "red")  # Agregar la curva de ajuste
