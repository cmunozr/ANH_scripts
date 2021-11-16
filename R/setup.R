# Setup groups a set of functions to install packages, create folders and  

vector.rqurd <- c("openxlsx", "BiodiversityR", "MASS", "tidyverse", "data.table", "iNEXT", "reshape2", "ggpubr",
           "ggpmisc", "evaluate", "maptools", "rgdal", "sp", "lattice", "ggplot2", "rgeos", "ade4", "Rtsne",
           "reshape2", "devtools", "stringi", "svDialogs")


# Function to install required packages
# rqurd: vector character, name of each package used in the analysis
#
# return: 

do.install <- function(rqurd = vector.rqurd){
  
  for(p in rqurd ){
    if (!is.element(p, installed.packages()[ ,1])){			#if package is not installed
      # r <- getOption("repos")								#assign R mirror for download
      # r["CRAN"] <- "http://cran.us.r-project.org"
      # options(repos = r)
      # rm(r)
      print(c("installing", p))
      install.packages(p, dep = TRUE)			#install tuneR
      require(p, character.only = TRUE)
      
      if(p == "ggrepel"){
      
        devtools::install_github("slowkow/ggrepel")
        library(ggrepel)
      
      }
      
    }
    
    try(library(p, character.only=T))
  }
}

# Function to create foldes structure of analysis in order to place mandatory files and save results
#
# WDGlobal: file path object, Working directory by default is apply as getwd()
# taxon: vector character, taxon choosen for analysis no default
# WDOut: file path object, Folder to write graphs, tables and other products by default is located in
# the path Analisis/SalidasPreliminares inside the working directory

do.folderStructure <- function(WDGlobal = getwd(), taxon, WDOut = NULL){
  
  dir.create(file.path(WDGlobal,'Analisis'), showWarnings = F)
  
  WDIn <- file.path(WDGlobal,'Analisis','Matrices_Abundancia')
  WDIn2 <- file.path(WDGlobal,'Analisis','Datos_Finales')
  WDCov <- file.path(WDGlobal,'Analisis','Covariables')
  
  dir.create(WDIn, showWarnings = F)
  dir.create(WDIn2, showWarnings = F)
  dir.create(WDCov, showWarnings = F)
    
  if(is.null(WDOut)){
    
    WDOutX <- file.path(WDGlobal,'Analisis','SalidasPreliminares')
    
    dir.create(WDOutX, showWarnings = F)
    
    WDOut <- file.path(WDGlobal,'Analisis','SalidasPreliminares', taxon)
    dir.create(path = WDOut, showWarnings = F)
  }
  
  return(list(WDIn = WDIn, WDIn2 = WDIn2, WDCov = WDCov, WDOut = WDOut))
}
  
do.folderStructure(taxon = 'Peces')  
  
  

