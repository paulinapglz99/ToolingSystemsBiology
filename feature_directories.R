#Script for extract features of the csv

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
  return(data.frame(Title = title, Date = date, Lab = lab_name, Plate = plate, stringsAsFactors = FALSE))
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
  Demengeot = "/home/paulinapg/tooling_up_systems_bio/RawData/Demengeot",
  Howard = "/home/paulinapg/tooling_up_systems_bio/RawData/Howard",
  HowardNew = "/home/paulinapg/tooling_up_systems_bio/RawData/HowardNew",
  Vilanova = "/home/paulinapg/tooling_up_systems_bio/RawData/Vilanova"
)

# Procesar todos los directorios y combinarlos en una sola tabla
combined_data <- do.call(rbind, lapply(names(lab_directories), function(lab) {
  process_directory(lab_directories[[lab]], lab_name = lab)
}))

# Ver los primeros resultados
View(combined_data)

#