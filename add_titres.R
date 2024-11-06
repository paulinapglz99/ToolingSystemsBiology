setwd("~/tooling_up_systems_bio/ToolingSystemsBiology/")

dir <- getwd()

# Cargar las librerías necesarias
pacman::p_load("dplyr", 
               "stringr")

# Función para extraer la información de cada archivo

extract_info <- function(filepath, lab_name) {
  # Obtener el nombre del archivo
  file_name <- basename(filepath)
  
  # Extraer el identificador del archivo (file name sin la extensión)
  title <- tools::file_path_sans_ext(file_name)
  
  # Extraer la fecha (primer componente, asumiendo que está al principio)
  date <- str_extract(file_name, "\\d{4}-\\d{2}-\\d{2}")  # formato YYYY-MM-DD
  if (is.na(date)) {
    date <- str_extract(file_name, "[A-Za-z]{3}\\s[A-Za-z]{3}\\s\\d{2}\\s\\d{4}")  # formato como 'Fri Aug 18 2023'
  }
  
  # Extraer el plato (P1, P2, plate_1, plate_2, etc.)
  plate <- str_extract(file_name, "P\\d+|plate_\\d+")
  
  # Crear un data frame con la información, incluyendo el path del archivo
  return(data.frame(
    file_name = title,
    Date = date,
    Lab = lab_name,
    Plate = plate,
    FilePath = filepath,  # Añadir el path completo del archivo
    stringsAsFactors = FALSE
  ))
}

# Usar la función actualizada en process_directory
process_directory <- function(dir_path, lab_name) {
  files <- list.files(dir_path, pattern = "\\.csv$", full.names = TRUE)
  data <- do.call(rbind, lapply(files, extract_info, lab_name = lab_name))
  return(data)
}

# # Definir los directorios de los laboratorios
lab_directories <- list(
  Demengeot = paste0(dir, "/RawData/Demengeot"),
  Howard = paste0(dir,"/RawData/Howard"),
  HowardNew = paste0(dir,"/RawData/HowardNew"),
  Vilanova = paste0(dir,"/RawData/Vilanova")
)

# Procesar los directorios y combinar resultados
combined_data <- do.call(rbind, lapply(names(lab_directories), function(lab) {
  process_directory(lab_directories[[lab]], lab_name = lab)
}))
