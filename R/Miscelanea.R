# Procesing input users

process_input <- function(input, spaces = F){
  input_string <- input %>% strsplit(",") %>% unlist() %>% trimws()
  if(spaces == FALSE){
    res_input <- gsub(x = input_string, pattern = " ", replacement = "")
  }else{
    res_input <- input_string
  }
}


# reconcile names of a column in a database and change to factor
#
# database = data.frame object
# column = character vector to locate targeting column
# max.distance = numeric, as used in agrep function. See https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/agrep
# The fraction represents what fraction of the number of characters in 
# your pattern argument you want to allow as insertion/deletions/substitutions (i.e. 0.1 on a 10 
# character pattern would allow 1 change) 
# https://stackoverflow.com/questions/22187294/agrep-max-distance-arguments-in-r
#
# return: vector character

homolog_factors <- function(database, column, max.distance = 0.2){
  cats = database[, column]
  tosave = database[, column]
  for(i in 1:length(unique(cats))){
    similar <- unique(cats)[agrep(unique(cats)[i], unique(cats), max.distance = 0.2)] %>% 
      stri_trans_general(id = "Latin-ASCII")
    if(length(similar) > 1){
      for(j in 1:length(similar)){
        tosave[which(tosave == unique(tosave)[i])] <- similar[1]
      }  
    }
  }
  
  return(as.factor(tosave))
}

# XXXXXXXXXXXX

complete_cols <- function(BD_registros, BD_eventos, link, vector_cols){
  
  col_registros <- colnames(BD_registros)
  
  missing_cols <- vector_cols[which(!vector_cols %in% col_registros)]
  
  if(!is.null(missing_cols)){
    for(i in 1:unique(length(BD_eventos[, link]))){
      Missed_Data <- BD_eventos[i, missing_cols]
      link_index <- which(BD_registros[, link] == unique(BD_eventos[i, link]))
      BD_registros[link_index, missing_cols] <- Missed_Data
    }
    return(BD_registros)
  }else{
    return(BD_registros)
  }
}

clear_meth_events <- function(){
  sampProt <- unique(Data.r$samplingProtocol) %>% paste(collapse = ", ")
  ommt_bolean <- dlgInput(paste0("From those methods: ", sampProt, 
                                 ". Are there methods to be omitted? (TRUE OR FALSE)"))$res %>% 
    process_input(spaces = T)
  if(ommt_bolean == T){
    ommt <- dlgInput(paste0("From those methods", sampProt, "Give me the position"))$res %>%
      process_input(spaces = T) %>% as.numeric()
    ommt <- sampProt[ommt]
  }else{
    ommt <- ""
  }
  ompv_bolean <- dlgInput("Are there sample events to be omitted? (TRUE OR FALSE)")$res %>% process_input()
  if(ompv_bolean == T){
    ompv <- dlgInput("Give me the name of events")$res %>% process_input() # ANH_380, ANH_64, ANH_65)
  }else{
    ompv <- ""
  }
  return(list("ommt" = ommt, "ompv" = ompv))
}
