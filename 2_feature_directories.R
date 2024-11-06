#Script for extract features of the csv

setwd("~/tooling_up_systems_bio/ToolingSystemsBiology/")

dir <- getwd()

# Cargar las librerías necesarias
pacman::p_load("dplyr", "purrr", "stringr")

# Función para extraer la información de cada archivo
# Modificación en la función extract_info para agregar el path
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

# 
# # Procesar todos los directorios y combinarlos en una sola tabla
# combined_data <- do.call(rbind, lapply(names(lab_directories), function(lab) {
#   process_directory(lab_directories[[lab]], lab_name = lab)
# }))

# Ver los primeros resultados
unique(combined_data$Plate)
unique(combined_data$Date)

# Normalizar los valores en la columna Plate
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

combined_data <- combined_data %>% dplyr::select("FilePath", "date_lab_plate", "file_name", "Date", "Lab", "Plate")

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
    stats = list(calculate_last_row_stats(paste0(dir, "/RawData/", Lab, "/", file_name, ".csv"))),
    background_mean = stats$mean,
    background_SD = stats$sd
  ) %>%
  select(-stats) %>%
  ungroup()

# Add the threshold

# Add the `threshold` column by calculating the value of 2 standard deviations over the background_mean

combined_data <- combined_data %>%
  mutate(threshold = background_mean + (2 * background_SD))

# Verifica el resultado
View(combined_data)

#Save data

vroom::vroom_write(combined_data, "~/tooling_up_systems_bio/ToolingSystemsBiology/data.csv")
#END

#######################

# Función para obtener el valor y la dilución asociado a cada columna que supera el threshold
extract_titer <- function(filepath, threshold) {
  # Leer el archivo CSV
  data <- read.csv(filepath)
  
  # Crear una lista para almacenar los resultados por columna
  result <- list()
  
  # Recorremos cada columna
  for (col_name in names(data)[1:8]) {
    # Extraer la columna de valores y la de diluciones
    values <- data[[col_name]]
    dilutions <- data$dilution
    
    # Identificar el primer valor que esté por encima del threshold
    above_threshold_idx <- max(which(values > threshold))
    
    if (!is.na(above_threshold_idx)) {
      # Guardamos el valor y su dilución asociada
      result[[paste0(col_name, "_value")]] <- values[above_threshold_idx]
      result[[paste0(col_name, "_dilution")]] <- dilutions[above_threshold_idx]
    } else {
      # Si no hay ningún valor superior al threshold, almacenamos NA
      result[[paste0(col_name, "_value")]] <- NA
      result[[paste0(col_name, "_dilution")]] <- NA
    }
    
  }
  result <- as.data.frame(result)
  new_cols <- c(
    "Reference1_val", "Reference1_titre",
    "Reference2_val", "Reference2_titre",
    "Control1_val", "Control1_titre",
    "Control2_val", "Control2_titre",
    "Infected1_val", "Infected1_titre",
    "Infected2_val", "Infected2_titre",
    "Infected3_val", "Infected3_titre",
    "Infected4_val", "Infected4_titre"
  )
  
  colnames(result) <- new_cols
  return(result)
}

# Crear una lista de filepaths completos utilizando la columna de `Lab` en `combined_data`

filepaths <- map2(
  combined_data$Lab, 
  combined_data$file_name, 
  ~ file.path(lab_directories[[.x]], paste0(.y, ".csv"))
)

# Aplicar la función `extract_titer` a cada archivo con su threshold correspondiente
results <- map2(filepaths, combined_data$threshold, ~ extract_titer(.x, .y))

# Combinar los resultados en un solo dataframe, agregando las columnas `file_name` y `threshold` de combined_data
final_data <- bind_rows(results, .id = "file_name") %>%
  bind_cols(combined_data%>% select(-file_name))


# Usar map2_dfr para aplicar la función y unir los resultados en un solo dataframe
final_data <- map2_dfr(filepaths, combined_data$threshold, ~ {
  result <- extract_titer(.x, .y)
  result$file_name <- basename(.x) # Agregar file_name como columna
  result$threshold <- .y # Agregar threshold como columna
  return(result)
})

# Usar map2_dfr para aplicar la función y unir los resultados en un solo dataframe
final_data <- map2_dfr(filepaths, combined_data$threshold, ~ {
  result <- extract_titer(.x, .y)
  #result$file_name <- basename(.x) # Agregar file_name como columna
  result$threshold <- .y # Agregar threshold como columna
  return(result)
})

# Agregar las columnas adicionales de combined_data
final_data <- final_data %>%
  left_join(combined_data, by = "threshold")

colnames(final_data)

final_data <- final_data %>%
  select(
    # Columnas de la 18 a la 25 como primeras
    FilePath, date_lab_plate, file_name, Date, Lab, Plate, background_mean, background_SD,
    # Luego la columna `threshold`
    threshold,
    # Finalmente, las columnas restantes en el orden en que están
    everything()
  )

#save table

vroom::vroom_write(final_data, "~/tooling_up_systems_bio/ToolingSystemsBiology/final_data.csv")

#END