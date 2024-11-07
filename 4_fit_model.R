# Fit michaelis menten to a dilution-signal plot

setwd("~/tooling_up_systems_bio/ToolingSystemsBiology/")

#
x<- vroom::vroom("/home/paulinapg/tooling_up_systems_bio/RawData/Demengeot/2023-01-11 plate_1.csv")

ref1 <- x %>%
  select(Reference1, dilution) %>% 
  mutate(log10dil = log10(dilution)) %>% 
  mutate(signal = Reference1)

# f<- function(x,k1,k2,k3) {
#   return(k1+k2*x/(k3+x))

#Funcion Michaelis-menten
f <- function(x,c,a,b) {
  return(c+a*x/(b+x))
}

#Funcion de tangente

g <- function(x, a, b, c) {
  (c - 1) + (a / 2) * atan((x / a) - b)
}

plot(x = ref1$log10dil,
     y = ref1$signal,
     # main = "Relación entre Reference1 y Dilution",
     xlab = "Log10 dilution",
     ylab = "Signal",
     # col = "steelblue",
     pch = 16,        # Tipo de punto (punto sólido)
     cex = 1,       # Tamaño de los puntos
     type = "b",      # Conectar los puntos con líneas
     lwd = 2          # Ancho de la línea
)

#a es el punto mas alto que corresponde a la senal

#b va a ser la dilucion que corresponda a a/2

#c es el background, es el valor de la senal de la ultima fila

#x es la dilucion 
v <- f(x = ref1$log10dil, a = ref1$signal[1], b =ref1$log10dil[7], c = ref1$signal[12]
           )

plot(v)

#50 to 100 points


# Gráfico inicial de ref1
plot(x = ref1$log10dil,
     y = ref1$signal,
     xlab = "Log10 dilution",
     ylab = "Signal",
     pch = 16,
     cex = 1,
     type = "b",
     lwd = 2)

# Añadir la línea de 'v' al gráfico en color azul
lines(x = -(ref1$log10dil),  # Usamos la misma dilución de ref1 para el eje x
      y = v,              # Valores de 'v' para el eje y
      col = "blue",       # Color de la línea
      type = "b",         # Conectar los puntos con líneas
      lwd = 2,            # Ancho de la línea
      pch = 16)           # Tipo de punto sólido


