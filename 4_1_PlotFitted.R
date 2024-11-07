# Cargar las librerías necesarias
library(vroom)
library(ggplot2)
library(dplyr)
library(minpack.lm) # Para la función nlsLM de ajuste no lineal

# Cargar el archivo CSV
csv1 <- vroom("~/ToolingSystemsBiology/RawData/Demengeot/2023-01-11 plate_1.csv")

# Definir la función de Michaelis-Menten, con el parámetro c (ruido) sin el +1
michaelis_menten <- function(dilution, Vmax, Km, c) {
  c + (Vmax * dilution) / (Km + dilution)  # Sumar solo c, sin el +1
}

# Definir la función de ajuste con arcTangente, con el parámetro c (ruido) +1
arctangent <- function(dilution, a, b, c) {
  (c - 1) + (a/2) * atan((dilution-b)/a)  # Agregar c+1 al principio
}


# Definir la función de ajuste con arcTangente, con el parámetro c (ruido) +1
#arctangent <- function(dilution, a, b, c) {
#  (c + 1) + a * atan(b * dilution)  # Agregar c+1 al principio
#}


# Ajuste no lineal de Michaelis-Menten, agregando el parámetro c
fit_mm <- nlsLM(csv1$Reference1 ~ michaelis_menten(dilution, Vmax, Km, c),
                data = csv1,
                start = list(Vmax = max(csv1$Reference1), Km = median(csv1$dilution), c = 2.77))

# Ajuste no lineal con arcTangente, agregando el parámetro c
fit_atan <- nlsLM(csv1$Reference1 ~ arctangent(dilution, a, b, c),
                  data = csv1,
                  start = list(a = max(csv1$Reference1), b = 1 / median(csv1$dilution), c = 2.77))

# Extraer los parámetros ajustados para Michaelis-Menten
Vmax <- coef(fit_mm)["Vmax"]
Km <- coef(fit_mm)["Km"]
c_mm <- coef(fit_mm)["c"]

# Extraer los parámetros ajustados para arcTangente
a <- coef(fit_atan)["a"]
b <- coef(fit_atan)["b"]
c_atan <- coef(fit_atan)["c"]

# Crear la gráfica con los dos ajustes (Michaelis-Menten y arcTangente)
ggplot(csv1, aes(x = dilution, y = Reference1)) +
  geom_point(color = "blue") + 
  scale_x_log10() +
  labs(title = "Michaelis-Menten and ArcTangente Fit with Noise",
       x = "Log10(Dilution)",
       y = "Reference01 (Shifted by Noise)") +
  expand_limits(x = 0, y = 0) +
  theme_minimal() +
  # Curva de Michaelis-Menten con ruido (solo c)
  stat_function(fun = function(x) c_mm + (Vmax * x) / (Km + x), color = "red", linetype = "solid") +
  # Curva de arcTangente con ruido (c + 1)
  stat_function(fun = function(x) (c_atan + 1) + a * atan(b * x), color = "green", linetype = "dashed")
