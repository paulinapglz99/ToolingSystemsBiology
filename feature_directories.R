#Script for extract features of the csv

setwd("~/tooling_up_systems_bio/ToolingSystemsBiology/")

dir <- getwd()

# Cargar las librerías necesarias
library(dplyr)
library(stringr)

# Función para extraer la información de cada archivo
extract_info <- function(filepath, lab_name) {
  # Obtener el nombre del archivo
  file_name <- basename(filepath)
  
  # Extraer el identificador del archivo (file name sin la extensión)
  title <- tools::file_path_sans_ext(file_name)
  
  # Extraer la fecha (primer componente, asumiendo que está al principio)
  date <- str_extract(file_name, "\\d{4}-\\d{2}-\\d{2}")  # formato YYYY-MM-DD
  if (is.na(date)) {
    # Si no encuentra una fecha con el formato anterior, buscar otro formato posible
    date <- str_extract(file_name, "[A-Za-z]{3}\\s[A-Za-z]{3}\\s\\d{2}\\s\\d{4}")  # formato como 'Fri Aug 18 2023'
  }
  
  # Extraer el plato (P1, P2, plate_1, plate_2, etc.)
  plate <- str_extract(file_name, "P\\d+|plate_\\d+")
  
  # Crear un data frame con la información
  return(data.frame(file_name = title, Date = date, Lab = lab_name, Plate = plate, stringsAsFactors = FALSE))
}

# Función para recorrer un directorio y procesar sus archivos
process_directory <- function(dir_path, lab_name) {
  # Listar todos los archivos .csv en el directorio
  files <- list.files(dir_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Aplicar la función de extracción a cada archivo y combinarlos en un data frame
  data <- do.call(rbind, lapply(files, extract_info, lab_name = lab_name))
  
  return(data)
}

# Definir los directorios de los laboratorios
lab_directories <- list(
  Demengeot = paste0(dir, "/RawData/Demengeot"),
  Howard = paste0(dir,"/RawData/Howard"),
  HowardNew = paste0(dir,"/RawData/HowardNew"),
  Vilanova = paste0(dir,"/RawData/Vilanova")
)

# Procesar todos los directorios y combinarlos en una sola tabla
combined_data <- do.call(rbind, lapply(names(lab_directories), function(lab) {
  process_directory(lab_directories[[lab]], lab_name = lab)
}))

# Ver los primeros resultados
unique(combined_data$Plate)
unique(combined_data$Date)

# # Normalizar los valores en la columna Plate
combined_data$Plate <- ifelse(grepl("^P[0-9]+$", combined_data$Plate),
                              paste0("plate_", sub("^P", "", combined_data$Plate)),
                              combined_data$Plate)

unique(combined_data$Plate)

#Ahora quiero normalizar los datos 
# Convertir fechas al formato "YYYY-MM-DD" usando diferentes formatos según el caso
combined_data$Date <- ifelse(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", combined_data$Date),
                             as.Date(combined_data$Date, format = "%Y-%m-%d"),
                             as.Date(combined_data$Date, format = "%a %b %d %Y"))

# Convertir a Date para manejar los NA resultantes de conversiones fallidas
combined_data$Date <- as.Date(combined_data$Date, origin = "1970-01-01")

# Verificar el resultado
unique(combined_data$Date)

#unique identifier 

combined_data$date_lab_plate <- paste(combined_data$Date, combined_data$Lab, combined_data$Plate, sep = "_")

combined_data <- combined_data %>% dplyr::select("date_lab_plate", "Title", "Date", "Lab", "Plate")

# Cargar los datos nuevamente en caso de que combined_data no esté actualizado
# library(dplyr)
# combined_data <- #... tu código previo

# Función para calcular media y desviación estándar de la última fila (sin contar "dilution")
calculate_last_row_stats <- function(filepath) {
  # Leer el archivo CSV
  data <- read.csv(filepath)
  
  # Obtener la última fila sin la columna 'dilution'
  last_row <- data[nrow(data), -ncol(data)]
  
  # Calcular media y desviación estándar
  mean_value <- mean(as.numeric(last_row), na.rm = TRUE)
  sd_value <- sd(as.numeric(last_row), na.rm = TRUE)
  
  return(list(mean = mean_value, sd = sd_value))
}

# Aplicar la función a cada archivo y actualizar `combined_data`
combined_data <- combined_data %>%
  rowwise() %>%
  mutate(
    stats = list(calculate_last_row_stats(paste0(dir, "/RawData/", Lab, "/", Title, ".csv"))),
    background_mean = stats$mean,
    background_SD = stats$sd
  ) %>%
  select(-stats) %>%
  ungroup()

# Verifica los resultados
View(combined_data)

#Save data

vroom::vroom_write(combined_data, "~/tooling_up_systems_bio/ToolingSystemsBiology/data.csv")
#END