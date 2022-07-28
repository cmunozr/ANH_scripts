# Script para dividir archivos de muestreo del proyecto ANH en diferentes temporadas
# Junio de 2022

# Librerias
library(openxlsx)
library(dplyr)

# leer el archivo original
dataReg <- read.xlsx("data/reptiles/I2D-BIO_2022_060 DwC_Reptiles_final.xlsx", sheet = "Reptiles", 
                     startRow = 1, na.strings = "N/A")
dataEvent <- read.xlsx("data/reptiles/I2D-BIO_2022_060 DwC_Reptiles_final.xlsx", sheet="Evento", 
                       startRow = 1, na.strings = "N/A")
covs <- read.xlsx("Analisis/Covariables/BDPuntosMuestreoMag180722.xlsx", sheet=1, 
                  startRow = 1, na.strings = "N/A") %>% filter(GrupoBiolo == "Herpetos")

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

# correr funcion split_temporadas
splitReg <- split_temporadas(db = dataReg, column = "eventID", char_search = "_TE2")
splitEvent <- split_temporadas(db = dataEvent, column = "eventID", char_search = "_TE2")

# Condicional: revise manualmente si los eventID tienen la etiqueta de la temporada, en caso de ser asi
# es necesario quitar aquellos caracteres como "_T1" para que los scripts subsiguientes no se necesite transformar,
# de la misma manera se debe desarrollar para la base de datos original
splitReg[["T1"]]$eventID <- gsub(pattern = "_T1", replacement = "", x = splitReg[["T1"]]$eventID)
splitEvent[["T1"]]$eventID <- gsub(pattern = "_T1", replacement = "", x = splitEvent[["T1"]]$eventID)

splitReg[["T2"]]$eventID <- gsub(pattern = "_TE2", replacement = "", x = splitReg[["T2"]]$eventID)
splitEvent[["T2"]]$eventID <- gsub(pattern = "_TE2", replacement = "", x = splitEvent[["T2"]]$eventID)

dataReg$eventID <- gsub(pattern = "_T1", replacement = "", x = dataReg$eventID)
dataEvent$eventID <- gsub(pattern = "_T1", replacement = "", x = dataEvent$eventID)

dataReg$eventID <- gsub(pattern = "_TE2", replacement = "", x = dataReg$eventID)
dataEvent$eventID <- gsub(pattern = "_TE2", replacement = "", x = dataEvent$eventID)

# constituyendo archivos de la temporada 1: dado que se mantendra el formato excel
# es necesario primero crear un excel con una sola hoja de trabajo, cargarlo de nuevo,
# agregar una nueva hoja y por ultimo escribir/guardar los datos de la nueva hoja
write.xlsx(splitReg[["T1"]], "data/reptiles/I2D-BIO_2022_060 DwC_Reptiles_final_T1.xlsx", sheetName = "Registros")
wb <- loadWorkbook("data/reptiles/I2D-BIO_2022_060 DwC_Reptiles_final_T1.xlsx")
addWorksheet(wb, "Eventos")
writeData(wb, "Eventos",splitEvent[["T1"]])
saveWorkbook(wb, "data/reptiles/I2D-BIO_2022_060 DwC_Reptiles_final_T1.xlsx", overwrite = TRUE)

# constituyendo archivos de la temporada 2
write.xlsx(splitReg[["T2"]], "data/reptiles/I2D-BIO_2022_060 DwC_Reptiles_final_T2.xlsx", sheetName = "Registros")
wb <- loadWorkbook("data/reptiles/I2D-BIO_2022_060 DwC_Reptiles_final_T2.xlsx")
addWorksheet(wb, "Eventos")
writeData(wb, "Eventos",splitEvent[["T2"]])
saveWorkbook(wb, "data/reptiles/I2D-BIO_2022_060 DwC_Reptiles_final_T2.xlsx", overwrite = TRUE)

# constituyendo archivo original sin tag de temporada
write.xlsx(dataReg, "data/reptiles/I2D-BIO_2022_060 DwC_Reptiles_final_NoTag.xlsx", sheetName = "Registros")
wb <- loadWorkbook("data/reptiles/I2D-BIO_2022_060 DwC_Reptiles_final_NoTag.xlsx")
addWorksheet(wb, "Eventos")
writeData(wb, "Eventos", dataEvent)
saveWorkbook(wb, "data/reptiles/I2D-BIO_2022_060 DwC_Reptiles_final_NoTag.xlsx", overwrite = TRUE)
