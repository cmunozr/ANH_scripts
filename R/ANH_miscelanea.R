# Script para dividir archivos de muestreo del proyecto ANH en diferentes temporadas
# Junio de 2022

# Librerias
library(openxlsx)
library(dplyr)

# leer el archivo original
dataReg <- read.xlsx("data/aves/I2D-BIO_2021_050_v3.xlsx", sheet="Registros", startRow = 1, na.strings = "N/A")
dataEvent <- read.xlsx("data/aves/I2D-BIO_2021_050_v3.xlsx", sheet="Eventos", startRow = 2, na.strings = "N/A")

# Funcion para dividir el archivo original
# db = data.frame, debe tener una columna que tenga el valor de busqueda char_search por 
# evento registrado que permita identificar las dos temporadas
# column = character string, columna que identifica cada registro y posee un caracter que 
# define las temporadas a trabajar
# char_search = character string, caracter que diferencia las temporadas
#
# return = list, lista en donde se encuentra la base de datos dividida por temporadas

split_temporadas <- function(db, column = "eventID", char_search = "_T2"){
  dataCol <- db[ , column]
  # finding T2
  index <- grepl(pattern = char_search, x = dataCol)
  T1 <- db[!index, ]
  T2 <- db[index, ]
  
  return(list("T1" = T1, "T2" = T2))
}

remove_UM <- function(data, by, UM){
  x <- data %>% filter(., by == UM)
}



# correr funcion split_temporadas
splitReg <- split_temporadas(db = dataReg)
splitEvent <- split_temporadas(db = dataEvent)

## Quitar caracteres "_T1" para que los scripts subsiguientes no se necesite transformar
splitReg[["T1"]]$eventID <- gsub(pattern = "_T1", replacement = "", x = splitReg[["T1"]]$eventID)
splitEvent[["T1"]]$eventID <- gsub(pattern = "_T1", replacement = "", x = splitEvent[["T1"]]$eventID)

## Quitar caracteres "_T2" para que los scripts subsiguientes no se necesite transformar
splitReg[["T2"]]$eventID <- gsub(pattern = "_T2", replacement = "", x = splitReg[["T2"]]$eventID)
splitEvent[["T2"]]$eventID <- gsub(pattern = "_T2", replacement = "", x = splitEvent[["T2"]]$eventID)

# constituyendo archivos de la temporada 1: dado que se mantendra el formato excel
# es necesario primero crear un excel con una sola hoja de trabajo, cargarlo de nuevo,
# agregar una nueva hoja y por ultimo escribir/guardar los datos de la nueva hoja
write.xlsx(splitReg[["T1"]], "data/aves/I2D-BIO_2021_050_v3_T1.xlsx", sheetName = "Registro")
wb <- loadWorkbook("data/aves/I2D-BIO_2021_050_v3_T1.xlsx")
addWorksheet(wb, "Eventos")
writeData(wb, "Eventos",splitEvent[["T1"]])
saveWorkbook(wb, "data/aves/I2D-BIO_2021_050_v3_T1.xlsx", overwrite = TRUE)

# constituyendo archivos de la temporada 2
write.xlsx(splitReg[["T2"]], "data/aves/I2D-BIO_2021_050_v3_T2.xlsx", sheetName = "Registro")
wb <- loadWorkbook("data/aves/I2D-BIO_2021_050_v3_T2.xlsx")
addWorksheet(wb, "Eventos")
writeData(wb, "Eventos",splitEvent[["T2"]])
saveWorkbook(wb, "data/aves/I2D-BIO_2021_050_v3_T2.xlsx", overwrite = TRUE)

