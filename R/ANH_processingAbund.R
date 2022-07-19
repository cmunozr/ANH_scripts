#Date.created Sep.22.2021
#Dat.Modified Nov.20.2021

#Script to get basic biodiversity analyses for ANH

#0) Load required libraries
rqurd<-c("purrr", "openxlsx","BiodiversityR","MASS","tidyverse","data.table","iNEXT",'reshape2','ggpubr',
         'ggpmisc','evaluate',"fastDummies","corrplot","Hmisc","maptools","rgdal","sp", "dplyr",
         "lattice","ggplot2","rgeos","ade4","Rtsne")

for(p in 1:length(rqurd) ){
  if (sum(grepl(pattern =rqurd[p], x = installed.packages()[,1])) != 0){		
    library(rqurd[p], character.only = T)
  }else{
    install.packages(rqurd[p], dep = TRUE)				#if package is not installed
    library(rqurd[p], character.only = T)
  }
}
library(dplyr)

#source(file.path("C:","Users","dsrbu","Dropbox","Humboldt","6_RcodeRepository",
#                 "14_Script_others","NEwR-2ed_code_data","NEwR2-Functions","cleanplot.pca.R"))

#load("kpv_temp.RData")

#0b) Define working directories and group variables

outD<-'Mamiferos_T1'#'Zooplancton' #master folder for output
outDD<-'Mamiferos'#'Hidrobiologicos' #Grupo like stated in the covariate file
ctnm<- "CobMam" # 'waterBody' #CobColl'#"CuerpAgua" #reptiles y anfibios: CobHerp #escarabajos coprofagos: "CobCopr" #main factor for análisis
gnm<- 'Mam' #'Coll'#"Zoop" #group prefix
fnn<-"sum" #function to aggregate samples within sampling unites

WDOut<-file.path(getwd(), "Analisis", "SalidasPreliminares")
dir.create(file.path(WDOut,outD), showWarnings = F)
WDOut<-file.path(WDOut,outD)
WDIn<-file.path(getwd(), "Analisis",'Matrices_Abundancia')
WDIn2<-file.path(getwd(), "Analisis",'Datos_Finales')
WDCov<-file.path(getwd(), "Analisis",'Covariables')
WDFunc<-file.path(getwd())

#create subdirectories
dir.create(file.path(WDOut,'CurvasDiversidad'), showWarnings = F)
dir.create(file.path(WDOut,'CurvasRank'), showWarnings = F)
dir.create(file.path(WDOut,'Covariables_PCA'), showWarnings = F)
dir.create(file.path(WDOut,'NMDS'), showWarnings = F)
dir.create(file.path(WDOut,'RDA'), showWarnings = F)

#0c) functions
source(file.path(WDFunc,'R/ANH_procAbu_functions.R'))

#1) covariances

covbk<- openxlsx::read.xlsx((file.path(WDCov,"BDPuntosMuestreoMag140722.xlsx")), 
                            sheet = 1)
covbk$FInt19meanx<-round(as.numeric(covbk$FInt19meanx),2)
names(covbk)[c(2,23,24:31,39,40)]<-c('parentEventID','Cobertura','CobAves',
                                     'CobHerp','CobCopr','CobMarip',
                                     'CobHorm','CobColl','CobMam','CobBot','CuerpAgua','NomCuerpAg')
# unique(covbk$Cobertura)
# unique(covbk$CobAves)
# unique(covbk$CobHerp)
# unique(covbk$CobCopr)
# unique(covbk$CobHorm)
# unique(covbk$CobMarip)
# unique(covbk$CobMam)
# unique(covbk$COBXCollembola)

for(i in c('Cobertura','CobAves','CobHerp','CuerpAgua','CobCopr','CobMarip',
           'CobHorm','CobMam','CobBot','COBXCollembola')){
  ccov<-covbk[,i]
  ccov[ccov=="Bosque denso"]<-"Bosque Denso"
  ccov[ccov=="Ci?naga"]<-"Cienaga"
  ccov[ccov=="Herbazales"]<-"Herbazal"
  ccov[ccov=="Cienagas"]<-"Cienaga"
  ccov[ccov=="V?as"]<-"Vias"
  ccov[ccov=="R?o Magdalena"]<-"R_Magdal"
  ccov[ccov=="Bosque ripario"]<-"Bosque Ripario"
  ccov[ccov=="Bosque abierto"]<-"Bosque Abierto"
  ccov[ccov=="Pastos limpios"]<-"Pastos"
  covbk[,i]<-ccov
}

covbk$Cobertura<-factor(covbk$Cobertura,levels=c("Rios","Cienaga","Zonas Pantanosas","Otros Cuerpos Agua",
                                                    "Herbazal","Bosque Ripario",
                                                    "Bosque Denso","Bosque Abierto","Vegetacion Secundaria",
                                                    "Palma","Cultivos","Pastos","Zonas Desnudas Degradadas",
                                                 "Vias","Area Urbana"))

spa.c<-c("decimalLat","decimalLon")
#peces; c("Plataf","Red.Hidrica","CuerpAgua") #colembolos: c("Plataf","CobColl","habitat","UCSuelo")
#Coprofagos ad: c("Plataf","CobCopr") # #Coprofagos lv: c("Plataf", "CobCopr", "UCSuelo") 
#Hidrobiol?gicos=c("Plataf","Red.Hidrica","CuerpAgua")#,"CobHerp")
# Aves: c("Plataf", "CobAves") #anfibios: c("Plataf", "CobHerp")
# Mamiferos: c("Plataf", "CobMam")
cat.c<-c("Plataf", "CobMam") 
v.pres<-c("Dis_CP","Dis_Oleodu", "Dis_Pozo","Dis_Pozact","Dis_Ferroc","Dis_ViaPri","Dia_ViaSec")#,"HEH18meanx")
v.rec<-c("Dis_Cienag","Dis_MGSG","Dis_Dre345", "DisBosque","Dis_CobNat","Tam_Parche")#,"FInt19meanx")
v.msite<-NULL
excl<-NULL

for (i in c(v.pres,v.rec)){
  if(all(is.na(covbk[,i]))) excl<-c(excl,i)
}

v.pres<-v.pres[!v.pres%in%excl]
v.rec<-v.rec[!v.rec%in%excl]

##verify names

#not for coprofagos
if(outDD != "Escarabajos"){
  covbk$parentEventID<-trimws(gsub("-","_",covbk$parentEventID))
  covbk$eventID<-trimws(gsub("-","_",covbk$eventID))
  unique(covbk$GrupoBiolo)
}

#Murcielagos
if(outDD == "Murcielagos"){
  covbk<-covbk%>%filter(GrupoBiolo%in%c(outDD,'Ultrasonido'))
}

#Anfibios
if(outDD == "Anfibios" | outDD == "Reptiles"){
  covbk<-covbk%>%filter(., GrupoBiolo == "Herpetos")
}

#others
covbk<-covbk%>%dplyr::filter(GrupoBiolo==outDD)
covbk<-covbk%>%dplyr::select(-c('Tipo','Habitat'))


#1b)covariates only for Peces y Microbiol?gicos

if(outDD == "Peces"|outDD == "Microbiologicos"){
  CovM<-read.xlsx(file.path(getwd(), 'Analisis','Covariables','Base De Datos Peces Fisico Quimicos 1212.xlsx'),sheet=1,startRow = 3 )
  ##check names##
  names(CovM)[c(2,29:69,71,72,81:83)]<-c('parentEventID','CuerpAgua','NmCuerpAg','ProfProm','Ancho',
                                         'ProfSechi','ProfCapFot','Temp','pH','OxgD','Cond',
                                         'SatO2','SolidDis','COrgT','FDisp','Magnesio','Calcio',
                                         'Sodio','TSolidDis','SoliTot','SoliSus','SoliSol','Fosfatos',
                                         'Nitratos','Silicatos','GrasAceit','SAAM','DurCalcica','DurTotal',
                                         'Alcalinid','sed_p_Arena','sed_p_Arcilla','sed_p_limo','sed_class',
                                         'sed_p_COrg','sed_fosforo','sed_Magnes','sed_calcio','sed_Sodio',
                                         'sed_boro','sed_hierro','sed_Nitrog',
                                         'Pgras','Mflot','Vacuat','Vrip','CDosel')
  
  
  #transformation
  CovM$Log_Cond<-log10(CovM$Cond)
  CovM[is.na(CovM)]<-0
  v.msiteQA<-c('Temp','pH','OxgD','Cond',
               'SatO2','SolidDis','COrgT','FDisp','Magnesio','Calcio',
               'Sodio','TSolidDis','SoliTot','SoliSus','SoliSol','Fosfatos',
               'Nitratos','Silicatos','GrasAceit','SAAM','DurCalcica','DurTotal',
               'Alcalinid','Log_Cond')
  v.msiteQS<-c('sed_p_Arena','sed_p_Arcilla','sed_p_limo','sed_p_COrg',
               'sed_fosforo','sed_Magnes','sed_calcio','sed_Sodio',
               'sed_boro','sed_hierro','sed_Nitrog')
  v.msiteF<-c('Bloques','Cantos.rodados','Guijarros','Grava','Pgras','Mflot','Vacuat','Vrip','CDosel','ProfProm','Ancho',
              'ProfSechi','ProfCapFot')
  
  library(Hmisc)
  MQA<-CovM%>%dplyr::select(all_of(v.msiteQA))%>%na.omit(.)%>%as.matrix(.)%>%rcorr(.)
  MQA<-lapply(MQA,function(x){
    x[is.na(x)]<-0
    return(x)
  }
  )
  
  library(corrplot)
  corrplot(MQA$r,type="upper", order="hclust", 
           p.mat = MQA$P, sig.level = 0.01, insig = "blank",tl.col='black')
  
  f.nm<-file.path(WDOut,'Covariables_PCA',paste('CorrM_QAgua',gnm,'.jpeg',sep=''))
  
  jpeg(f.nm, width = 600, height = 480, quality=300)
  corrplot(MQA$r,type="upper", order="hclust", 
           p.mat = MQA$P, sig.level = 0.01, insig = "blank",tl.col='black')
  dev.off()
  
  v.msiteQA<-c('Temp','OxgD',
               'COrgT','FDisp',
               'TSolidDis',
               'Nitratos','Silicatos','GrasAceit','SAAM',
               'Alcalinid','Log_Cond')
  MQS<-CovM %>% dplyr::select(all_of(v.msiteQS)) %>% na.omit(.) %>% as.matrix(.) %>% rcorr(.)
  MQS<-lapply(MQS,function(x){
    x[is.na(x)]<-0
    return(x)
  }
  )
  corrplot(MQS$r,type="upper", order="hclust", 
           p.mat = MQS$P, sig.level = 0.01, insig = "blank",tl.col='black')
  f.nm<-file.path(WDOut,'Covariables_PCA',paste('CorrM_QSedim',gnm,'.jpeg',sep=''))
  jpeg(f.nm, width = 600, height = 480, quality=300)
  corrplot(MQS$r,type="upper", order="hclust", 
           p.mat = MQS$P, sig.level = 0.01, insig = "blank",tl.col='black')
  dev.off()
  v.msiteQS<-c("sed_Nitrog","sed_p_limo","sed_fosforo","sed_Sodio",
               "sed_hierro","sed_Magnes") #c("sed_Nitrog","sed_p_limo","sed_fosforo","sed_calcio",
  #"sed_hierro","sed_Magnes")
  MF<-CovM%>%dplyr::select(all_of(v.msiteF))%>%na.omit(.)%>%as.matrix(.)%>%rcorr(.)
  MF<-lapply(MF,function(x){x[is.na(x)]<-0
  return(x)})
  
  corrplot(MF$r,type="upper", order="hclust", 
           p.mat = MF$P, sig.level = 0.01, insig = "blank",tl.col='black')
  f.nm<-file.path(WDOut,'Covariables_PCA',paste('CorrM_Struct',gnm,'.jpeg',sep=''))
  jpeg(f.nm, width = 600, height = 480, quality=300)
  corrplot(MF$r,type="upper", order="hclust", 
           p.mat = MF$P, sig.level = 0.01, insig = "blank",tl.col='black')
  dev.off()
  
  # for(i in v.msiteF){
  #   hist(CovM[,i],main=i)
  #   readline(prompt='ENTER')
  # }
  
  v.msiteF<-v.msiteF[!v.msiteF%in%c("Bloques","Guijarros","Pgras","Mflot","Vacuat","Arena","ProfSechi")]
  MT<-CovM%>%dplyr::select(all_of(v.msiteF), all_of(v.msiteQA),all_of(v.msiteQS))%>%
    na.omit(.)%>%as.matrix(.)%>%rcorr(.)
  MT<-lapply(MT,function(x){x[is.na(x)]<-0
  return(x)})
  
  corrplot(MT$r,type="upper", order="hclust", 
           p.mat = MT$P, sig.level = 0.01, insig = "blank",tl.col='black',
           na.label.col = 'black')
  f.nm<-file.path(WDOut,'Covariables_PCA',paste('CorrM_FinalV',gnm,'.jpeg',sep=''))
  
  jpeg(f.nm, width = 600, height = 480, quality=300)
  corrplot(MT$r,type="upper", order="hclust", 
           p.mat = MT$P, sig.level = 0.01, insig = "blank",tl.col='black')
  dev.off()
  
  covbk<-CovM%>%dplyr::select(parentEventID,eventID,
                              all_of(v.msiteQA),all_of(v.msiteQS),all_of(v.msiteF),
                              all_of(cat.c),all_of(spa.c),all_of(v.pres),all_of(v.rec))
  
  ##Final Covariables
  
  covbk$eventID<-gsub('-','_',covbk$eventID)
  covbk$eventID<-gsub('ANH_','ANH',covbk$eventID)
  covbk$eventID<-gsub('ANH','ANH_',covbk$eventID)
  v.msite<-c(v.msiteQA,v.msiteQS,v.msiteF)
  
}

#Zooplancton
if(outDD == "Zooplancton"){
  covbk<-covbk%>%filter(grepl('.*_Z_',eventID))
}
#perifiton
if(outDD == "Perifiton"){
  covbk<-covbk%>%filter(grepl('.*_P_',eventID))  
}

#fitoplancton
if(outDD == "Fitoplancton"){
  covbk<-covbk%>%filter(grepl('.*_F_',eventID))
}
#Macrofitas
if(outDD == "Macrofitas"){
  covbk<-covbk%>%filter(grepl('.*_MA',eventID))  
}
#Macroinvertebrados
if(outDD == "Macroinvertebrados"){
  covbk<-covbk%>%filter(grepl('.*_MI',eventID))
}

#Coprofagos/escarabajos ad and lv
covbk$eventID<-gsub('-','_',covbk$eventID)

# All
kpv<-c(ls(),'kpv') #variables to keep all the time

#save.image(file = "kpv_temp.RData")

#2) group specific variables
#2a) get raw data
#Hidrobiologicos=I2D-BIO_2021_068.xlsx
#Coprofagos
#event_scarabaeidae_santanderANH_2021_revJrey.xlsx
#hormigas=I2D-BIO_2021_096.xlsx"
#Mariposas=I2D-BIO_2021_084_event.xlsx
#Mamiferos=I2D-BIO_2021_083.xlsx
#Botanica=I2D-BIO_2021_095.xlsx

Data.et<-read.xlsx(file.path(getwd(),"data", "mamiferos", "Mamiferos_T1.xlsx"), 
                   sheet="Eventos", startRow = 1, na.strings = "N/A")

# MISSING IFS
#coprofagos_adultos
Data.et<-Data.et[Data.et$samplingProtocol=='Trampa de excremento humano',]
#Coprofagos_larvas
Data.et<-Data.et[Data.et$samplingProtocol=='Captura manual',]

#Hidrobiol?gicos
if(outD == "Hidrobiologicos"){
  Data.et<-Data.et[Data.et$locationRemarks=="Macroinvertebrados",] #Zooplancton Perifiton Fitoplancton Macrofitas
  Data.et$eventID<-gsub('ANH','ANH_',Data.et$eventID)
  Data.et$parentEventID<-gsub('ANH','ANH_',Data.et$parentEventID)
}

#All
Data.et$eventID<-gsub('-','_',Data.et$eventID)

#Murcielagos
slevnt<-unique(Data.et$eventID[!grepl('M_R',Data.et$eventID)])
slevntt<-unique(Data.et$eventID[grepl('M_R',Data.et$eventID)])


# MISSING IFS
#Plantas
names(Data.et)[1]<-'eventID'
Data.et$samplingProtocol[Data.et$samplingProtocol==
                           "Muestreo de flora ep?fita no vascular sugerido por la ANLA para solicitudes de licenciamiento ambiental."]<-'M_epift'
Data.et$samplingProtocol[Data.et$samplingProtocol=="Transecto de inventario de flora arb?rea (Oliver et al. 2002, Global Patterns of Plant Diversity: Alwyn H. Gentry Forest Transec Data Set, Missouri Botanical Garden Press) ajustado a DAP m?nimo de 5 cm"]<-'RAP_5cm'
Data.et$samplingProtocol[Data.et$samplingProtocol=="Muestreo de comunidades de hierbas para estimar coberturas en cuadrantes de un metro cuadrado"]<-'M_Hierb'
unique(Data.et$samplingProtocol)

#Arboles
Data.et<-Data.et[!Data.et$samplingProtocol%in%c('M_Hierb','M_epift'),]
#Epifitas
Data.et<-Data.et[!Data.et$samplingProtocol%in%c('M_Hierb','RAP_5cm'),]

###Registros
#Hidrobiologicos=I2D-BIO_2021_068.xlsx
#Coprofagos_ad=rrbb_scarabaeidae_santanderANH_2021_PEM_Coprofagos.xlsx
#Coprofagos_lv=rrbb_scarabaeidae_santanderANH_2021_PEM_Larvas.xlsx
#mariposas=I2D-BIO_2021_084_rrbb.xlsx

Data.r<-read.xlsx(file.path(getwd(),"data", "mamiferos", "Mamiferos_T2.xlsx"), 
                  sheet="Registros", startRow = 1, na.strings = "N/A")
#Data.r<-Data.r[Data.r$class=="Reptilia",] # use to get the group of the analysis

#All
Data.r$eventID<-gsub('-','_',Data.r$eventID)

# identificationQualifier is used when we can't arrive to specific epithets of species
# used with fishes for example but not with birds
if(!is.null(Data.r$identificationQualifier)){
  Data.r$identificationQualifier<-tolower(Data.r$identificationQualifier)
}

library(Hmisc)
Data.r$scientificName[grep('^[[:lower:]]{1}',Data.r$scientificName)]<-
  capitalize(Data.r$scientificName[grep('^[[:lower:]]', Data.r$scientificName)])

#Hidrobiol?gicos
if(outDD == "Hidrobiologicos"){
  Data.r$eventID<-gsub('ANH','ANH_',Data.r$eventID)
  Data.r$identificationQualifier<-tolower(Data.r$identificationQualifier)
  ###Perifiton to work with non standardized abundances####
  Data.rs<-Data.r
  Data.r$organismQuantity<-Data.r$`measurementValue.(Abundancia.relativa)`
  ###Perifiton revert to standardized abundances####
  Data.rss<-Data.rs
  Data.r<-Data.rss
}

#Hormigas
if(outDD == "Hormigas"){
  Data.r$samplingEffort[Data.r$samplingEffort=="12 minutos"]<-12/60
  Data.r$samplingProtocol[Data.r$samplingProtocol=="Trampa de ca?da at?n"]<-"TrampCaid" 
  Data.r$samplingProtocol[Data.r$samplingProtocol=="Captura manual"]<-"CaptMan"
  Data.et$samplingEffort[Data.et$samplingEffort=="12 minutos"]<-12/60
  Data.et$samplingProtocol[Data.et$samplingProtocol=="Trampa de ca?da at?n"]<-"TrampCaid" 
  Data.et$samplingProtocol[Data.et$samplingProtocol=="Captura manual"]<-"CaptMan"
}

#Mariposas
if(outDD == "Mariposas"){
  Data.r$samplingProtocol<-"Trmp_vanSomRyd"
  Data.et$samplingProtocol<-"Trmp_vanSomRyd"
}

# MISSING IF
#murcielagos
Data.r<-Data.r[Data.r$order=='Chiroptera',]
slevnt2<-Data.r$eventID[Data.r$eventID%in%slevnt&Data.r$order=='Chiroptera'] #eventID in .et not coming from main sampling protocol
slevntt2<-Data.r$eventID[Data.r$eventID%in%slevntt&Data.r$order=='Chiroptera']
slevnttt<-c(slevntt2,slevnt2)
Data.et<-Data.et[Data.et$eventID%in%slevnttt,] #all events corresponding to chiroptera


#Add ultrasound events
Data.et_S<-read.xlsx(file.path(WDIn2,"I2D-BIO_2021_094_Sonidos.xlsx"), sheet=1, startRow = 2, na.strings = "N/A")
Data.et_S$eventID<-gsub('ANH ','ANH_',Data.et_S$eventID)
Data.et_S$parentEventID<-gsub('ANH ','ANH_',Data.et_S$parentEventID)
selC<-intersect(names(Data.et),names(Data.et_S))
Data.et2<-rbind(Data.et[,selC],Data.et_S[,selC])

#Add ultrasound records
Data.r_S<-read.xlsx(file.path(WDIn2,"I2D-BIO_2021_094_Sonidos.xlsx"), sheet=2, startRow = 2, na.strings = "N/A")
Data.r_S<-Data.r_S[Data.r_S$order=='Chiroptera',]
Data.r_S$eventID<-gsub('ANH ','ANH_',Data.r_S$eventID)
Data.r_S$organismQuantity<-1
selC<-intersect(names(Data.r),names(Data.r_S))
Data.r<-rbind(Data.r[,selC],Data.r_S[,selC])
#fix sampling protocols
Data.et$samplingProtocol[Data.et$samplingProtocol%in%c('Red niebla','Redes de Niebla')]<-'RedNiebla'
Data.et$samplingProtocol[Data.et$samplingProtocol%in%c('Punto grabaci?n automatica (ultrasonido)',
                                                       'Punto grabaci?n (ultrasonido)')]<-'GrbUltrasonido'
Data.r$samplingProtocol[Data.r$samplingProtocol%in%c('Red de niebla ','Red de niebla')]<-'RedNiebla'
Data.r$samplingProtocol[Data.r$samplingProtocol%in%c('Grabaci?n direccional (ultrasonido)')]<-'GrbUltrasonido'


#Roedores
Data.r<-Data.r[Data.r$order=='Rodentia',]
Data.r$samplingProtocol[Data.r$samplingProtocol!='Manual']<-'TrmpShrmn'
#identify events for rodents that are not Sherman
slevnt<-unique(Data.et$eventID[Data.et$samplingProtocol!='Trampa Sherman'])
slevnt2<-Data.r$eventID[Data.r$eventID%in%slevnt&Data.r$order=='Rodentia']
slevnt3<-Data.et$eventID[Data.et$samplingProtocol=='Trampa Sherman']
slevnt3<-c(slevnt3,slevnt2)
Data.et<-Data.et[Data.et$eventID%in%slevnt3,]
Data.et$samplingProtocol[Data.et$samplingProtocol=='Trampa Sherman']<-'TrmpShrmn'
Data.et$samplingProtocol[Data.et$samplingProtocol=='B?squeda por encuentros visuales (VES)']<-'Manual'

#Arboles
unique(Data.r$samplingProtocol)
Data.r$samplingProtocol[Data.r$samplingProtocol=="Transectos r?pidos de Gentry (1982)"]<-'RAP_5cm'
Data.r<-Data.r[Data.r$`measurementValue.(Estado.general)`!='Muerto',]
#Epifitas_NV
Data.r$samplingProtocol<-'M_epift'
Data.r<-Data.r[Data.r$scientificName!='Plantae',]
Data.r<-Data.r[grepl('.*[0-9]{1}_[0-9]{1}$',Data.r$organismRemarks),]
Data.r$eventID_F<-paste(Data.r$eventID,
                        gsub('(.*)([0-9]{1})(_[0-9]{1}$)','\\2',Data.r$organismRemarks),
                        sep='_')
Data.r$eventID_T<-paste(Data.r$eventID,
                        gsub('(.*)([0-9]{1}_[0-9]{1}$)','\\2',Data.r$organismRemarks),
                        sep='_')
Data.r$Forofito<-as.integer(gsub('(.*)([0-9]{1})(_[0-9]{1}$)','\\2',Data.r$organismRemarks))

#All
#Estimate unique data
selr<-unique(Data.r$eventID)

# not coprofagos
SbUME<-Data.et$eventID[which(!Data.et$eventID%in%selr)] #Data with zero records
SbUMT<-unique(Data.et$eventID)


#coprofagos_ad
SbUME<-Data.et$eventID[which(!Data.et$eventID%in%selr&Data.et$samplingProtocol=="Trampa de excremento humano")]
SbUMT<-unique(Data.et$eventID[which(Data.et$samplingProtocol=="Trampa de excremento humano")])
#Coprofatos_lv
SbUME<-Data.et$eventID[which(!Data.et$eventID%in%selr&Data.et$samplingProtocol=="Captura manual")]
SbUMT<-unique(Data.et$eventID[which(Data.et$samplingProtocol=="Captura manual")])

#All
kpv<-c(kpv,c('Data.et','Data.r','SbUME','SbUMT','Data.rs'))

#########

#2b) quality checks##
# quality control: varies from group to group.
# complete columns
# regular expression that varies by group:
# for fish
# gsub(pattern = "^(ANH_[0-9]+)(_.*[C|D])$", replacement = "\\1", Data.r$eventID)
# for birds
# gsub(pattern = "^(ANH_[0-9]+)(_.*)$", replacement = "\\1", Data.r$eventID)
# for herp
# gsub(pattern = "^(ANH_[0-9]+)(_.*)$", replacement = "\\1", Data.r$eventID)
# for escarabajos ad and lv
# gsub(pattern = "^(ANH_[0-9]+)(_.*)$", replacement = "\\1", Data.r$eventID)
# hidrobiologicos
# gsub(pattern = "^(ANH)([0-9]+)(_.*)$", replacement = "\\1_\\2", Data.r$eventID)

#All
#UM  empty 
Data.r$parentEventID <- gsub(pattern = "^(ANH_[0-9]+)(_.*)$", replacement = "\\1", Data.r$parentEventID)
UM<-unique(Data.r$parentEventID)
UME<-setdiff(unique(gsub(pattern = "^(ANH_[0-9]+)(_.*)$", replacement = "\\1", Data.r$eventID)), UM)
UMT<-unique(gsub(pattern = "^(ANH_[0-9]+)(_.*)$", replacement = "\\1", SbUMT))
kpv<-c(kpv,c('UM','UME','UMT'))

# this applies for fish
if(outD == "Peces"){
  Data.r <- complete_cols(Data.r, Data.et,  "eventID", c("eventID", "samplingProtocol", "habitat"))#  
}

#others
# Complete columns in Data.r that are in Data.e.
Data.r <- complete_cols(Data.r, Data.et, "eventID",vector_cols = c("samplingProtocol"))#

library(stringi)
Data.et$samplingProtocol <- trimws(Data.et$samplingProtocol)
Data.r$samplingProtocol <- trimws(Data.r$samplingProtocol)
Data.et$samplingProtocol <- homolog_factors(Data.et, column = "samplingProtocol")
Data.r$samplingProtocol <- homolog_factors(Data.r, column = "samplingProtocol")

#Data.r$habitat<-trimws(Data.r$habitat)
Data.r$organismQuantity<-as.numeric(Data.r$organismQuantity)
Data.et$samplingEffort[is.na(Data.et$samplingEffort)]<-0

unique(Data.et$samplingProtocol)
unique(Data.r$samplingProtocol)

###this applies for fish ###
if(outD == "Peces"){
  Data.et$samplingProtocol[Data.et$samplingProtocol=="Red de arrastre"|Data.et$samplingProtocol=="Red de Arrastre"]<-"Arrastre"
  Data.et$samplingProtocol[Data.et$samplingProtocol=="trasmallo"]<-"Trasmallo"
  unique(Data.et$samplingProtocol)
}

### This applies for birds###
if(outD == "Aves"){
  names(Data.et)[2]<-'parentEventID'  
}


### This applies for herpetos: anfibios y reptiles###
if(outDD == "Anfibios" | outDD == "Reptiles"){
  Data.et$samplingProtocol<-'VES'
  Data.r$samplingProtocol<-'VES'
}

# MISSING IFS
##Coprofagos-adults##
Data.et$samplingProtocol[Data.et$samplingProtocol=="Trampa de excremento humano"]<-"TrmpExHum"
Data.r$samplingProtocol[Data.r$samplingProtocol=="Trampa de excremento humano"]<-"TrmpExHum"
#Coprofagos_lv
Data.et$samplingProtocol[Data.et$samplingProtocol=="Captura manual"]<-"CapManual"
Data.r$samplingProtocol[Data.r$samplingProtocol=="Captura manual"]<-"CapManual"

#Roedores
Data.et$samplingEffort[Data.et$samplingEffort=='1 trampa x 5 noches']<-120
Data.et$samplingEffort[Data.et$samplingEffort=='53 minutos']<-53/60
Data.et$samplingEffort[Data.et$samplingEffort=='36 minutos']<-36/60

#Mam?feros todos
selrr<-grepl('[0-9]+ minutos',Data.et$samplingEffort)
Data.et$samplingEffort[selrr]<-as.numeric(gsub('([0-9]+).*$','\\1',Data.et$samplingEffort[selrr]))/60

#murcielagos
Data.r$samplingProtocol <-'RedNiebla_Av'
Data.et$samplingProtocol <-'RedNiebla_Av'

#Epifitas_NV
sE<-rowSums(table(Data.r$eventID,Data.r$Forofito)>0)
mtchSE<-match(Data.et$eventID,names(sE))
Data.et$samplingEffort<-sE[mtchSE]

#zooplancton/fitoplancton
Data.et$samplingProtocol[Data.et$samplingProtocol=="Botella Van Dorn"]<-"B.vDorn"
Data.r$samplingProtocol[Data.r$samplingProtocol=="Botella Van Dorn"]<-"B.vDorn"
Data.r<-Data.r[,-4]

#perifiton
Data.et$samplingProtocol<-"Raspado"
Data.r$samplingProtocol<-"Raspado"

#Macrofitas
Data.et$samplingProtocol<-"Cuadrante"
Data.r$samplingProtocol<-"Cuadrante"

#Macroinvertebrados
Data.et$samplingProtocol<-"Red_D"
Data.r$samplingProtocol<-"Red_D"
Data.et$habitat[Data.et$habitat=="Bosque fragmentado con vegetaci?n secundaria"]<-"Bosque Frag VS" 
Data.r<-Data.r%>%dplyr::select(-"habitat")

#All
unique(Data.r$samplingProtocol);unique(Data.et$samplingProtocol)

#######

Data.e<-Data.et[Data.et$eventID%in%selr,] #only those records with data


#2c) group environment
#All
#section to verify that names of the columns is consistent
cnm.smp<-c("samplingEffort","samplingProtocol") #from data
kpv<-c(kpv,c('Data.e','ctnm','fnn','cnm.smp','gnm'))
Data.r$scientificName_2<-trimws(Data.r$scientificName)

# identificationQualifier is used when we can't arrive to specific epithets of species
# used with fishes for example but not with birds
if(!is.null(Data.r$identificationQualifier)){
  selrnm<-!is.na(Data.r$identificationQualifier)
  Data.r$scientificName_2[selrnm]<-paste(Data.r$scientificName[selrnm],
                                         trimws(Data.r$identificationQualifier[selrnm]))
}

#coprofagos
Data.r$scientificName_2[selrnm]<-paste(Data.r$scientificName[selrnm],
                                       paste(trimws(Data.r$identificationQualifier[selrnm]),
                                       trimws(Data.r$identificationRemarks[selrnm]),sep=''))
Data.r$scientificName_2[selrnm]<-gsub(' NA','',Data.r$scientificName_2[selrnm])

#collembola
Data.r$scientificName_2[selrnm]<-paste(Data.r$scientificName[selrnm],paste(
                                       trimws(Data.r$identificationRemarks[selrnm]),
                                       gsub('aff. ','',
                                            Data.r$identificationQualifier[selrnm]),
                                       sep=''))
Data.r$scientificName_2[selrnm]<-gsub(' NA','',Data.r$scientificName_2[selrnm])

#All
unique(Data.r$scientificName_2)
shrscitf<-gsub("^([[:alpha:]]{4}).*([[:space:]])([[:alpha:]]{4}).*$","\\1_\\3",
     unique(Data.r$scientificName_2))
dupl<-which(shrscitf%in%shrscitf[duplicated(shrscitf)])
duplsp<-unique(Data.r$scientificName_2)[dupl]
print(duplsp)

Data.r[Data.r$scientificName_2%in%duplsp[3:4],c('scientificName','scientificName_2')]

##Peces
if(outD == "Peces" & length(duplsp) != 0){
  #Data.r$scientificName_2[Data.r$scientificName_2%in%duplsp[c(1,3)]]<-'Hyphessobrycon natagaima aff. natagaima'
  Data.r$scientificName_2[Data.r$scientificName_2%in%duplsp[c(2,4)]]<-'Characidium zebra cf. zebra'
}

#perifiton
Data.r$scientificName_2[Data.r$scientificName_2%in%duplsp[1:2]]<-'Closterium gracile cf. gracile'
Data.r$scientificName_2[Data.r$scientificName_2%in%duplsp[3:4]]<-'Micrasterias truncata cf. truncata'

#Arboles 
selrsp<-Data.r$scientificName_2%in%duplsp
Data.r$scientificName_2[selrsp]<-Data.r$scientificName[selrsp]

# Murcielagos
Data.r$scientificName_2[Data.r$scientificName_2%in%duplsp[c(1,5)]]<-'Carollia perspicillata'
Data.r$scientificName_2[Data.r$scientificName_2%in%duplsp[c(2,3)]]<-'Carollia brevicauda'
Data.r$scientificName_2[Data.r$scientificName_2%in%duplsp[c(4,9)]]<-'Carollia castanea'
Data.r$scientificName_2[Data.r$scientificName_2%in%duplsp[c(7,8)]]<-'Uroderma convexum'
Data.r$scientificName_2[Data.r$scientificName_2%in%duplsp[c(6,12)]]<-'Myotis nigricans'
Data.r$scientificName_2[Data.r$scientificName_2%in%duplsp[c(10,11)]]<-'Uroderma magnirostrum'

#Murcilagos_SinGenero
selrr<-!is.na(Data.r$specificEpithet)
Data.r<-Data.r[selrr,]
selr<-unique(Data.r$eventID)
#fix emptly sampling events
SbUME<-Data.et$eventID[which(!Data.et$eventID%in%selr)]
UM<-unique(Data.r$parentEventID)
UME<-setdiff(unique(gsub(pattern = "^(ANH_[0-9]+)(_.*)$", replacement = "\\1", SbUME)), UM)

#epifitas
Data.r$scientificName_2[Data.r$scientificName_2%in%duplsp[1]]<-'Malmidea ppris'

# final covariate table
#Collembola
if(outD == "Colembola"){
  covbk <- complete_cols(covbk, Data.et,  "eventID", c("habitat"))
  covbk<-covbk[covbk$parentEventID%in%UMT,] #gets covariates on all UM
}

# All
# others
covbk<-covbk[covbk$parentEventID%in%UMT,] #gets covariates on all UM

#Epifitas
covbk<-covbk[covbk$eventID%in%Data.r$eventID,]
covbk <- complete_cols(covbk, Data.et,  "eventID", c("habitat"))

##epifitas-fix datasets
Data.r$parentEventID <- Data.r$eventID
Data.r$eventID<-Data.r$eventID_F
Data.r$organismQuantity[is.na(Data.r$organismQuantity)|(Data.r$organismQuantity==0)]<-1
mtchEv<-match(Data.r$parentEventID,Data.et$eventID)
Data.et2<-Data.et[mtchEv,]
Data.et2$parentEventID<-Data.et2$eventID
Data.et2$eventID<-Data.r$eventID
Data.et2<-unique(Data.et2)
rownames(Data.et2)<-NULL
Data.et<-Data.et2

#Arboles
#Fix habitat Arboles
cov$habitat2<-gsub(' ripario| denso','',cov$habitat)
cov$habitat2[is.na(cov$habitat2)]<-'Bosque'

selr<-unique(Data.r$eventID)
SbUME<-Data.et$eventID[which(!Data.et$eventID%in%selr)] #Data with zero records
SbUMT<-unique(Data.et$eventID)

UM<-unique(Data.r$parentEventID)
UME<-setdiff(unique(gsub(pattern = "^(ANH_[0-9]+_[0-9]+_F)(_.*)$", replacement = "\\1", SbUME)), UM)
UMT<- unique(gsub(pattern = "^(ANH_[0-9]+_[0-9]+_F)(_.*)$", replacement = "\\1", SbUMT))

covbk$parentEventID<-covbk$eventID
mtchEv<-match(Data.et$eventID, covbk$eventID)
covbk2<-covbk[mtchEv,]
covbk2$eventID<-Data.et$eventID
covbk<-covbk2

##Cob_vs_Habitat
TablC_v_HH<-table(cov[,'Cobertura'],cov[,ctnm]) #matrix of changes in habitat categorization
TablH_v_HH<-table(cov[,'habitat'],cov[,ctnm]) 
write.csv(TablC_v_HH,file.path(WDOut,'Cob_v_Hhomologado.csv'))
write.csv(TablH_v_HH,file.path(WDOut,'habitat_v_Hhomologado.csv'))

#murcielagos
# ommt<-c("RedNiebla_Av","GrbUltrasonido")
# ompv<-c("")
# cov.2<-covbk[,c('eventID','parentEventID','CobMam','Cobertura')]%>%
#   inner_join(.,Data.et[,c('eventID','samplingProtocol')],by=c("eventID"))%>%
#   filter((!parentEventID%in%ompv)&(!samplingProtocol%in%ommt))
# Tabl2H_v_HH<-table(cov.2[,'habitat'],cov.2[,ctnm]) 
# write.csv(Tabl2H_v_HH,file.path(WDOut,'habitat_v_Hhomologado_RedNiebla.csv'))

#All
cov<-covbk
cov$eventID <- gsub(pattern = "-", replacement = "_", x = cov$eventID)
kpv<-c(kpv,c('cov'))

#2d) get sampling effort
# MISSING IFS
#for birds
#gsub("([0-9]+\\.*[0-9]+).*$"
#for fish/coprofagos, anfibios and reptiles
#gsub("([0-9]+).*$"
#zooplancton
#gsub("([0-9]+).*$",'\\1',samplingEffort)

# modify gsub searching character
samEff.t<-Data.et[,c('parentEventID',cnm.smp)] %>% na.omit(.)%>%
  dplyr::mutate(samplEff=as.numeric(gsub("([0-9]+).*$",'\\1',samplingEffort)))%>%
  dplyr::group_by(parentEventID,get(cnm.smp[2]))%>%
  dplyr::summarize(samplEff=sum(samplEff),Num_ev=dplyr::n())

#All
colnames(samEff.t)[1:2]<-c('parentEventID',cnm.smp[2])
sameEff.tt<-split(samEff.t,as.factor(samEff.t$samplingProtocol))
library(vegan)
samEff.ttt<-purrr::map(sameEff.tt, function(x) data.frame(as.data.frame(x),
                                                          decostand(as.vector(x[,"samplEff"]),"max")))
kpv<-c(kpv,'samEff.ttt')
rm(list=ls()[!ls()%in%kpv])

##
#Not for Herpetos (anfibios and reptiles), Not for Escarabajos Coprofagos, not for mamiferos(murcielagos)
#function for modyfing eventID from 5 or more labels to 4 in order to match with eventID from eventos database
Data.r <- modify_event_label(data = Data.r)

#aves solo punto fijo
Data.e <- Data.e %>% filter(samplingProtocol == "Punto Fijo" )
Data.et <- Data.et %>% filter(samplingProtocol == "Punto Fijo" )
Data.r <- Data.r %>% filter(samplingProtocol == "Punto Fijo" )

# Murcielagos
Data.r <- Data.r %>%  dplyr::select(.,-names(Data.r)[duplicated(names(Data.r))])

#3a) Diversity by method: abundance
#Regional Diversity
table(Data.r$parentEventID,Data.r$samplingProtocol)
rowSums(table(Data.r$parentEventID,Data.r$samplingProtocol))

# Collembola/Epifitas/Arboles/Anfibios/reptiles/coprofagos_ad/coprofagos_lv/Peces/
# Aves/zooplancton/perifiton/fitoplancton/macrofitas/hormigas
ommt<-c("") 
ompv<-c("")

#Roedores
ommt<-c("Manual")
ompv<-c("")

#murcielagos
ommt<-c("")
ompv<-c("")

#Aves2
ommt<-c("")
ompv<-c("ANH_220", "ANH_250", "ANH_274", "ANH_279", "ANH_213_A_P3")

# All
Data.r2<-Data.r %>% dplyr::filter((!parentEventID%in%ompv)&(!eventID%in%ompv)&(!samplingProtocol%in%ommt)) 


nsp<-unique(Data.r2$parentEventID)
nsp<-length(!nsp%in%ompv)
library(tidyr)
Data.ee.r<-Data.r2%>%
  dplyr::select(parentEventID,organismQuantity,samplingProtocol,scientificName_2)%>%
  pivot_wider(names_from=parentEventID,values_from=organismQuantity, 
              values_fn=sum,values_fill=0)%>%
  dplyr::mutate(TotAbu=rowSums(.[,3:(nsp+2)]),.keep="unused")%>%
  dplyr::select(scientificName_2,samplingProtocol,TotAbu)
###check of there area ceros in the dataframe, if that's the case split by sampling protocol)
  
#for hidrobiologicos
Data.ee.r<-Data.ee.r%>%pivot_wider(names_from=samplingProtocol,values_from=TotAbu, values_fn=sum,values_fill=0)%>%
  column_to_rownames(.,var="scientificName_2")%>%mutate_if(is.numeric,~ceiling(.x/min(.x[.x>0], na.rm=T)))%>%
  as.list(.)

#Collembola with zeros in the data frame
Data.ee.r<-Data.ee.r%>%group_split(samplingProtocol)%>%
  lapply(.,function(x){x%>%dplyr::select(-samplingProtocol)%>%
      column_to_rownames(.,var="scientificName_2")})
names(Data.ee.r)<-unique(Data.r$samplingProtocol)
Hill.r<-map(names(Data.ee.r),function(x){
  y<-iNEXT(Data.ee.r[[x]],q=c(0,1,2),datatype = "abundance")
  PrintggiNext(paste(gnm,'_abM_',x,sep=''),y)
  return(y)})

#others without zeros in the dataframe
library(tibble)
Data.ee.r<-Data.ee.r%>%pivot_wider(.,names_from=samplingProtocol,values_from=TotAbu, values_fn=sum,values_fill=0)%>%
  column_to_rownames(.,var="scientificName_2")%>%as.list(.)

# All
library(iNEXT)
library(dplyr)
Hill.r<-iNEXT(Data.ee.r,q=c(0,1,2),datatype = "abundance")
PrintggiNext(paste(gnm,'_abM',sep=''),Hill.r)
kpv<-c(kpv,'Hill.r','Data.ee.r')
rm(list=ls()[!ls()%in%kpv])


#3b) Overall Diversity incidence

#Aves2
ommt<-c("")
ompv<-c("ANH_220", "ANH_250", "ANH_274", "ANH_279", "ANH_213_A_P3")

#Otros
ommt<-c("")
ompv<-c("")

Data.r2<-Data.r%>%filter((!parentEventID%in%ompv)&(!eventID%in%ompv)&(!samplingProtocol%in%ommt))
Data.ii.r<-Data.r2%>%
  dplyr::select(parentEventID,organismQuantity,scientificName_2)%>%
  pivot_wider(names_from=parentEventID,values_from=organismQuantity, values_fn=sum,values_fill=0)%>%
  mutate_if(is.numeric,~1*(.>0))%>%column_to_rownames(.,var="scientificName_2")%>%list(.)
Hill.rr<-iNEXT(Data.ii.r,q=c(0,1,2),datatype = "incidence_raw")
names(Hill.rr$iNextEst)<-"Regional"
PrintggiNext(paste(gnm,'_incO',sep=''),Hill.rr)
kpv<-c(kpv,'Hill.rr')
rm(list=ls()[!ls()%in%kpv])

#3c) Hill by factor and method with abundance

#Aves
ommt<-c("Recorrido en lancha", "Recorrido Libre", "Accidental") #method to be omitted
ompv<-c("ANH_380")
#Aves2
ommt<-c("Recorrido en lancha", "Recorrido Libre", "Accidental")
ompv<-c("ANH_220", "ANH_250", "ANH_274", "ANH_279", "ANH_213_A_P3", "ANH_380")
#Arboles/Anfibios/Reptiles/Coprofagos/hidrobiol?gicos/murcielagos
ommt<-c("")
ompv<-c("")
#Roedores
ommt<-c("Manual")
ompv<-c("")
#mam?feros
ommt<-c("RedNiebla_Av","GrbUltrasonido")
ompv<-c("")
#otros
ommt<-c("")
ompv<-c("")

# ctnm
Data.ee.oo<-Data.a.f(catnm = ctnm, fn="sum",scale=TRUE) #scale is used for groups non-integer abundance
map(names(Data.ee.oo), function (x){
  write.csv(Data.ee.oo[[x]],file.path(WDOut,
                                      paste("species_HabHomolog", x,".csv", sep="")))
})

Data.ee.oo2<-map(names(Data.ee.oo),function(xx){
  x<-Data.ee.oo[[xx]]
  iNext.o<-iNEXT(x,q=c(0,1,2), datatype="abundance")
  return(iNext.o)
})
names(Data.ee.oo2)<-names(Data.ee.oo)
map(names(Data.ee.oo),function(xx){
  fnm2<-paste(gnm,ctnm,xx,sep='_')
  v<-Data.ee.oo2[[xx]]
  PrintggiNext(fnm2,v)
  PrintRefiNext(fnm2,ctnm,v)
  return()
})
kpv<-c(kpv,'Data.ee.oo')


## red. h?drica
Data.ee.rd<-Data.a.f('Red.Hidrica',fnn,scale=TRUE)
Data.ee.rd2<-map(names(Data.ee.rd),function(xx){
  x<-Data.ee.rd[[xx]]
  iNext.o<-iNEXT(x,q=c(0,1,2), datatype="abundance")
  return(iNext.o)
})
names(Data.ee.rd2)<-names(Data.ee.rd)
map(names(Data.ee.rd),function(xx){
  fnm2<-paste(gnm,'Red.Hidrica',xx,sep='_')
  v<-Data.ee.rd2[[xx]]
  PrintggiNext(fnm2,v)
  PrintRefiNext(fnm2,'Red.Hidrica',v)
})
kpv<-c(kpv,'Data.ee.rd')


##Plataforma
Data.ee.pt<-Data.a.f('Plataf',fnn,scale=FALSE)
write.csv(Data.ee.pt[[1]],file.path(WDOut,"species_plataf.csv"))
Data.ee.pt2<-map(names(Data.ee.pt),function(xx){
  x<-Data.ee.pt[[xx]]
  iNext.o<-iNEXT(x,q=c(0,1,2), datatype="abundance")
  return(iNext.o)
})
names(Data.ee.pt2)<-names(Data.ee.pt)
map(names(Data.ee.pt),function(xx){
  fnm2<-paste(gnm,'Plataf',xx,sep='_')
  v<-Data.ee.pt2[[xx]]
  PrintggiNext(fnm2,v)
  PrintRefiNext(fnm2,'Plataf',v)
  return()
})

#Perifiton to work with non-standardized values
Data.ee.pts<-Data.a.f('Plataf',fnn,scale=FALSE)
write.csv(Data.ee.pts[[1]],file.path(WDOut,"species_plataforma_nst.csv"))
Data.ee.pts2<-map(names(Data.ee.pts),function(xx){
  x<-Data.ee.pts[[xx]]
  iNext.o<-iNEXT(x,q=c(0,1,2), datatype="abundance")
  return(iNext.o)
})
names(Data.ee.pts2)<-names(Data.ee.pts)
map(names(Data.ee.pts),function(xx){
  fnm2<-paste(gnm,'Plataf',xx,sep='_')
  v<-Data.ee.pts2[[xx]]
  PrintggiNext(fnm2,v)
  PrintRefiNext(fnm2,'Plataf_nst',v)
  return()
})
kpv<-c(kpv,'Data.ee.pt','Data.ee.pts')


##Cobertura
Data.ee.hb<-Data.a.f('Cobertura',fnn)
Data.ee.hb2<-map(names(Data.ee.hb),function(xx){
  x<-Data.ee.hb[[xx]]
  iNext.o<-iNEXT(x,q=c(0,1,2), datatype="abundance")
  return(iNext.o)
})
names(Data.ee.hb2)<-names(Data.ee.hb)
map(names(Data.ee.hb),function(xx){
  fnm2<-paste(gnm,'Cobertura',xx,sep='_')
  v<-Data.ee.hb2[[xx]]
  PrintggiNext(fnm2,v)
  PrintRefiNext(fnm2,'Cobertura',v)
  return()
})
kpv<-c(kpv,'Data.ee.hb')


#habitat
Data.ee.hbt<-Data.a.f('habitat',fnn)
write.csv(Data.ee.hbt[[1]],file.path(WDOut,"species_habitat.csv"))
Data.ee.hbt2<-map(names(Data.ee.hbt),function(xx){
  x<-Data.ee.hbt[[xx]]
  iNext.o<-iNEXT(x,q=c(0,1,2), datatype="abundance")
  return(iNext.o)
})
names(Data.ee.hbt2)<-names(Data.ee.hbt)
map(names(Data.ee.hbt2),function(xx){
  fnm2<-paste(gnm,'habitat',xx,sep='_')
  v<-Data.ee.hbt2[[xx]]
  PrintggiNext(fnm2,v)
  PrintRefiNext(fnm2,'habitat',v)
  return()
})
kpv<-c(kpv,'Data.ee.hbt')
rm(list=ls()[!ls()%in%kpv])


#Suelo
Data.ee.sl<-Data.a.f('UCSuelo',fnn)
write.csv(Data.ee.sl[[1]],file.path(WDOut,"species_Suelo.csv"))
Data.ee.sl2<-map(names(Data.ee.sl),function(xx){
  x<-Data.ee.sl[[xx]]
  iNext.o<-iNEXT(x,q=c(0,1,2), datatype="abundance")
  return(iNext.o)
})
names(Data.ee.sl2)<-names(Data.ee.sl)
map(names(Data.ee.sl2),function(xx){
  fnm2<-paste(gnm,'habitat',xx,sep='_')
  v<-Data.ee.sl2[[xx]]
  PrintggiNext(fnm2,v)
  PrintRefiNext(fnm2,'habitat',v)
  return()
})
kpv<-c(kpv,'Data.ee.sl')

#All
rm(list=ls()[!ls()%in%kpv])

#3d) Hill by factor with Incidence
ommt<-c("") #method to be omitted
ompv<-c("") 
#aves
ompv<-c("ANH_380")
ommt<-c("")
#Aves2
ommt<-c()
ompv<-c("ANH_220", "ANH_250", "ANH_274", "ANH_279", "ANH_213_A_P3", "ANH_380")
#Coprofagos
ompv<-c("")
ommt<-c("")
#murcielago
ommt<-c("")
ompv<-c("")

#ctnm
Data.ei.oo<-Data.i.f(ctnm)
Data.ei.oo2<-iNEXT(Data.ei.oo,q=c(0,1,2), datatype="incidence_raw")
fnm2<-paste(gnm,ctnm,sep='_')
PrintggiNextInc(fnm2,Data.ei.oo2)
fnm2<-paste(gnm,ctnm,'Inc',sep='_')
PrintRefiNext(fnm2,ctnm,Data.ei.oo2)
kpv<-c(kpv,'Data.ei.oo')


#Red.h?drica
Data.ei.rd<-Data.i.f('Red.Hidrica')
Data.ei.rd2<-iNEXT(Data.ei.rd,q=c(0,1,2), datatype="incidence_raw")
fnm2<-paste(gnm, 'Red.Hidrica',sep='_')
PrintggiNextInc(fnm2,Data.ei.rd2)
fnm2<-paste(gnm,'Red.Hidrica','Inc',sep='_')
PrintRefiNext(fnm2,'Red.Hidrica',Data.ei.rd2)
kpv<-c(kpv,'Data.ei.rd2')

#plataforma
Data.ei.pt<-Data.i.f(catnm = 'Plataf')
Data.ei.pt2<-iNEXT(Data.ei.pt,q=c(0,1,2), datatype="incidence_raw")
fnm2<-paste(gnm, 'Plataf',sep='_')
PrintggiNextInc(fnm2,Data.ei.pt2)
fnm2<-paste(gnm,'Plataf','Inc',sep='_')
PrintRefiNext(fnm2,'Plataf',Data.ei.pt2)
kpv<-c(kpv,'Data.ei.pt2')

#Cobertura
Data.ei.hb<-Data.i.f(catnm = 'Cobertura')
Data.ei.hb2<-iNEXT(Data.ei.hb,q=c(0,1,2), datatype="incidence_raw")
fnm2<-paste(gnm, 'Cobertura',sep='_')
PrintggiNextInc(fnm = fnm2,iNxt = Data.ei.hb2)
fnm2<-paste(gnm,'Cobertura','Inc',sep='_')
PrintRefiNext(fnm2,'Cobertura',Data.ei.hb2)
kpv<-c(kpv,'Data.ei.hb2')
rm(list=ls()[!ls()%in%kpv])

#Suelo
Data.ei.sl<-Data.i.f(catnm = 'UCSuelo')
Data.ei.sl2<-iNEXT(Data.ei.sl,q=c(0,1,2), datatype="incidence_raw")
fnm2<-paste(gnm, 'UCSuelo',sep='_')
PrintggiNextInc(fnm2,Data.ei.sl2)
fnm2<-paste(gnm,'UCSuelo','Inc',sep='_')
PrintRefiNext(fnm2,'UCSuelo',Data.ei.sl2)
kpv<-c(kpv,'Data.ei.sl2')

# All
rm(list=ls()[!ls()%in%kpv])
save.image(file.path(WDOut,paste("wrkspc",gnm,Sys.Date(),".RData",sep="")))


#4) Hills by MU
rowSums(table(Data.r$parentEventID,Data.r$organismQuantity))
#Aves
ommt<-c("") #method to be omitted
ompv<-c("ANH_380","ANH_64","ANH_65") #does not have covariates
#Aves2
ommt<-c("")
ompv<-c("ANH_220", "ANH_250", "ANH_274", "ANH_279", "ANH_213_A_P3", "ANH_380", "ANH_64","ANH_65")
#reptiles
ommt<-c("") #method to be omitted
ompv<-c("ANH_9") 
#Arboles/Anfibios/Reptiles/Peces/zooplancton/Coprofagos
ommt<-c("")
ompv<-c("")
#Rodedores
#Roedores
ommt<-c("Manual")
ompv<-c("")
#murcielagos
ommt<-c("")
ompv<-c("")

Data.ee.mm0<-Data.a.MU(DataP = Data.r,evID = 'parentEventID',
                       expPEID = "^(ANH_[0-9]+)(_.*)$",fn = fnn, scale=FALSE)
Data.ee.mm<-map(names(Data.ee.mm0),function(x){
  xx<-Data.ee.mm0[[x]]
  y<-CompletEmpty(xx,'parentEventID',UME,x)
})
names(Data.ee.mm)<-names(Data.ee.mm0)
kpv<-c(kpv,'Data.ee.mm')
rm(list=ls()[!ls()%in%kpv])


#4b) Hills by sub-MU with abundance
rowSums(table(Data.r$eventID,Data.r$organismQuantity))

#Aves
ommt<-c("") #method to be omitted
ompv<-c("ANH_380","ANH_64","ANH_65")

#Aves2
ommt<-c("")
ompv<-c("ANH_220", "ANH_250", "ANH_274", "ANH_279", "ANH_213_A_P3", "ANH_380", "ANH_64","ANH_65")

#Reptiles
ommt<-c("") #method to be omitted
ompv<-c("ANH_9") #c("ANH_380","ANH_64","ANH_65")

#Arboles/Anfibios/Reptiles/peces/hidrobiologicos/coprofagos/murcielagos
ommt<-c("")
ompv<-c("")

Data.ee.nn0<-Data.a.MU(Data.r,'eventID',"^(ANH_[0-9]+)(_.*)$",fnn,scale=FALSE)
Data.ee.nn<-map(names(Data.ee.nn0),function(x){
  xx<-Data.ee.nn0[[x]]
  y<-CompletEmpty(xx,'eventID',SbUME,x)
  #shorter names for coprofagos
  #y$eventID<-gsub("(ANH_[0-9]+)(_T. Exc. Human)([0-9]+)(_2021-)([0-9]+-[0-9]+)(/.*)","\\1_\\3/\\5",y$eventID)
  return(y)
})
names(Data.ee.nn)<-names(Data.ee.nn0)
kpv<-c(kpv,'Data.ee.nn')
rm(list=ls()[!ls()%in%kpv])


#4c) agregando periodo (para peces es lo mismo que Sub_MU)
ommt<-c("") #method to be omitted
ompv<-c("")

#Aves2
ommt<-c("")
ompv<-c("ANH_220", "ANH_250", "ANH_274", "ANH_279", "ANH_213_A_P3", "ANH_380", 
        "ANH_64","ANH_65")

#text for period event

##fish
#schtxt<-"_[R|A|E|T]_"
#gsub(schtxt,"_",eventID)
#aves
#schtxt<-""
#coprofagos_ad
#schtxt <- "_T.  Exc.  Human"
#coprofagos_lv
#schtxt <- "_Captura manual_"
##herpetos: anfibios y reptiles
#schtxt<-"_Herp_T[1|2|3]_"

#Murcielagos
# shchtxt<-paste('Data.r$parentEventID', 'hour(as.POSIXct(date_decimal(as.numeric(Data.r$eventTime)),format="%H:%M:%S"))',sep='_')

ommt<-c("")
ompv<-c("")

#coprofagos

library(lubridate)
Data.pr<-Data.r%>%
  mutate('eventPer'=hour(as.POSIXct(Data.r$eventTime,format="%H:%M:%S")))
Data.pr$eventPer[Data.pr$eventPer<18|Data.pr$eventPer>21]<-21
Data.pr$eventPer<-paste(Data.pr$parentEventID,Data.pr$eventPer,sep='_')
#Data.pr<-Data.pr[,names(Data.pr)!='eventID']

#murcielagos/
library(lubridate)
Data.pr<-Data.r%>%
  mutate('eventPer'= hour(as.POSIXct(date_decimal(as.numeric(Data.r$eventTime)),format="%H:%M:%S")))
Data.pr$eventPer[Data.pr$eventPer<18|Data.pr$eventPer>21]<-21
Data.pr$eventPer<-paste(Data.pr$parentEventID,Data.pr$eventPer,sep='_')

#Aves2
Data.pr<-Data.r%>%
  mutate('eventPer'=gsub(schtxt,"_",eventID))

#Others
Data.pr<-Data.r%>%
  mutate('eventPer'=gsub(schtxt,"_",eventID))#%>%dplyr::select(-eventID)

#All
Data.ee.tt<-Data.a.MU(Data.pr,'eventPer',"^(ANH_[0-9]+)(_.*)$",fnn)
kpv<-c(kpv,'Data.ee.tt','schtxt')
rm(list=ls()[!ls()%in%kpv])


#4d) Diversidad agregando protocols
#Aves
ommt<-c("Recorrido en lancha","Recorrido Libre", "Accidental")
ompv<-c("ANH_380","ANH_64","ANH_65")
#Aves2
ommt<-c("Recorrido en lancha","Recorrido Libre", "Accidental")
ompv<-c("ANH_220", "ANH_250", "ANH_274", "ANH_279", "ANH_213_A_P3", "ANH_380", "ANH_64","ANH_65")
#Peces
ommt<-""
ompv<-c("ANH_9")
#Epifitas/Arboles/Anfibios/reptiles/coprofagos/zooplanction/collembola
ommt<-c("")
ompv<-c("")
#murcielagos
ommt<-c("RedNiebla_Av")
ompv<-c("")

# All
Data.pt<-Data.r%>%
  filter((!parentEventID%in%ompv)&(!eventID%in%ompv)&(!samplingProtocol%in%ommt))
nsp<-length(unique(Data.pt$parentEventID))
##peces
unique(Data.pt$samplingProtocol)

grp<-list('Ar_At'=c('Red de arrastre','Atarraya'),
           'Ar_At_El'=c('Red de arrastre','Atarraya','Electropesca'),
           'Ar_At_Tr'=c('Red de arrastre','Atarraya','Trasmallo'))
#herpetos: anfibios y reptiles
grp<-list('VES'=c('VES'))
#Aves
grp<-list('PntFijo'=c('Punto Fijo'), 
          'PntFijo_RdNiebla'=c('Punto Fijo', "Redes de Niebla"))
#Coprofagos_ad
grp<-list('TrmpExHum'=c('TrmpExHum'))#dfad
#Coprofagos_lv
grp<-list('CapManual'=c('CapManual'))
#zooplanction/fitoplancton
grp<-list('B.vDorn'=c('B.vDorn'))
#perifiton
grp<-list('Raspado'=c('Raspado'))
#Macrofitas
grp<-list('Cuadrante'=c('Cuadrante'))
#Macroinvertebrados
grp<-list('Red_D'=c('Red_D'))
##Hormigas
grp<-list('Pt_Wk'=c('Pitfall','Winkler'),
          'Pt_Wk_TC'=c('Pitfall','Winkler','TrampCaid'),
          'Pt_Wk_CM'=c('Pitfall','Winkler','CaptMan'),
          'All'=c('Pitfall','Winkler','TrampCaid','CaptMan'))
#Mariposas
grp<-list('Trmp_vanSomRyd'=c('Trmp_vanSomRyd'))
##Roedores
grp<-list('Shrmn'=c('TrmpShrmn'))
#Murcielagos
grp<-list('RdN'=c('RedNiebla'),
          'Rd_Gb'=c('RedNiebla','GrbUltrasonido'))
#Arboles
grp<-list('RAP_5cm'=c('RAP_5cm'))
#Epifitas
grp<-list('M_epift'=c('M_epift'))
#Collembola
grp<-list('Ber'=c('Berlese'),
          'Ber_Pit'=c('Berlese','Pitfall'))
         

Data.ee.pr<-Data.a.pt(grp,Data.pt,'eventID',fnn,scale=TRUE) #cambiar ID dependiendo de registros

#gets estimates from incidence data
#Peces
Data.ee.prr<-Data.i.pr(Data.ee.pr,grp,'parentEventID',
                       expPEID='^(ANH_[0-9]+)(_.*)$',
                       c('Arrastre','Atarraya')) 
#Herpetos
Data.ee.prr<-Data.i.pr(Data.ee.pr,grp,'parentEventID',
                       expPEID='^(ANH_[0-9]+)(_.*)$',
                       c("VES")) #
#Aves
Data.ee.prr<-Data.i.pr(Data.ee.pr,grp,'parentEventID',
                       expPEID='^(ANH_[0-9]+)(_.*)$',
                       c('Punto Fijo')) 
#Coprofagos ad
Data.ee.prr<-Data.i.pr(Data.ee.pr,grp,'parentEventID',
                       expPEID='^(ANH_[0-9]+)(_.*)$',
                       c('TrmpExHum')) 
#Coprofagos_lv
Data.ee.prr<-Data.i.pr(Data.ee.pr,grp,'parentEventID',
                       expPEID='^(ANH_[0-9]+)(_.*)$',
                       c('CapManual')) 
#zooplancton/fitoplancton
Data.ee.prr<-Data.i.pr(Data.ee.pr,grp,'parentEventID',
                       expPEID='^(ANH_[0-9]+)(_.*)$',
                       c('B.vDorn')) 
#perifiton
Data.ee.prr<-Data.i.pr(Data.ee.pr,grp,'parentEventID',
                       expPEID='^(ANH_[0-9]+)(_.*)$',
                       c('Raspado')) 
#Macrofitas
Data.ee.prr<-Data.i.pr(Data.ee.pr,grp,'parentEventID',
                       expPEID='^(ANH_[0-9]+)(_.*)$',
                       c('Cuadrante')) 
#Macroinvertebrados
Data.ee.prr<-Data.i.pr(Data.ee.pr,grp,'parentEventID',
                       expPEID='^(ANH_[0-9]+)(_.*)$',
                       c('Red_D')) 
#Hormigas
Data.ee.prr<-Data.i.pr(Data.ee.pr,grp,'parentEventID',
                       expPEID='^(ANH_[0-9]+)(_.*)$',
                       c('Pitfall','Winkler')) 
#Mariposas
Data.ee.prr<-Data.i.pr(Data.ee.pr,grp,'parentEventID',
                       expPEID='^(ANH_[0-9]+)(_.*)$',
                       c('Trmp_vanSomRyd')) 
#Roedores
Data.ee.prr<-Data.i.pr(Data.ee.pr,grp,'parentEventID',
                       expPEID='^(ANH_[0-9]+)(_.*)$',
                       c('TrmpShrmn')) 
#Murcielagos
Data.ee.prr<-Data.i.pr(Data.ee.pr,grp,'parentEventID',
                       expPEID='^(ANH_[0-9]+)(_.*)$',
                       c('RedNiebla')) 
#Arboles
Data.ee.prr<-Data.i.pr(Data.ee.pr,grp,'parentEventID',
                       expPEID='^(ANH_[0-9]+)(_.*)$',
                       c('RAP_5cm')) 
#Epifitas
Data.ee.prr<-Data.i.pr(Data.ee.pr,grp,'parentEventID',
                       expPEID='^(ANH_[0-9]+_[0-9]+_F)(_.*)$',
                       c('M_epift'))
#Collembola
Data.ee.prr<-Data.i.pr(Data.ee.pr,grp,'parentEventID',
                       expPEID='^(ANH_[0-9]+)(_.*)$',
                       c('Berlese'))

#All
cov.1<-cov%>%filter(parentEventID%in%Data.ee.prr$parentEventID)%>%
  distinct(parentEventID,.keep_all=T)%>%dplyr::select(-c('eventID'))
Data.ee.prr<-Data.ee.prr%>%
  inner_join(.,cov.1, by="parentEventID")
write.csv(Data.ee.prr,file.path(WDOut,'CurvasDiversidad',
                                paste(gnm,'_','MU_Inc_Estim_Grp.csv',sep='')))
kpv<-c(kpv,'Data.ee.pr','Data.ee.prr','grp')
rm(list=ls()[!ls()%in%kpv])

#4e) diversidad por periodo combinando protocolos/Abundancia
ommt<-c("") #method to be omitted
ompv<-c("")

#Aves2
ommt<-c("")
ompv<-c("ANH_220", "ANH_250", "ANH_274", "ANH_279", "ANH_213_A_P3", "ANH_380", "ANH_64","ANH_65")

Data.pr<-Data.r%>%
  mutate('eventPer'=gsub(schtxt,"_",eventID))%>%dplyr::select(-eventID) #Peces _

Data.ei.t<-Data.a.pt(grp,Data.pr,'eventPer',fn="sum")
## sampling method which will be the baseline of the grouping. In orignal form
#Peces
c('Red de arrastre','Atarraya')
#Aves
c('Punto Fijo',"Redes de Niebla")
#Herpetos
c('VES')
#coprofagos ad
c('TrmpExHum')
#coprofagos lv
c('CapManual')

Data.ei.ttt<-Data.a.t(Data.t = Data.ei.t,evID = 'evenPer',grpp = grp,samEf = samEff.ttt,selcc = c('CapManual')) 
write.csv(Data.ei.ttt,file.path(WDOut,'CurvasDiversidad', paste(gnm,'_','SubTempMU_Estim_Grp.csv',sep='')))
kpv<-c(kpv,'Data.ei.t','Data.ei.ttt')
rm(list=ls()[!ls()%in%kpv])

save.image(file.path(WDOut,paste("wrkspc",gnm,Sys.Date(),".RData",sep="")))

###########################################################

#5) Plot with MU diversity -Abundance
#plot with cover color
library(forcats); library(dplyr); library(ggplot2)
map(names(Data.ee.mm),function(x){
  point.dff<-Data.ee.mm[[x]]
  plotMU_cat(x,point.dff,ctnm,'parentEventID','MU_Est')
})
#Red hidrica
map(names(Data.ee.mm),function(x){
  point.dff<-Data.ee.mm[[x]]
  plotMU_cat(x,point.dff,'Red.Hidrica','parentEventID','MU_Est_RH')
})
#Cobertura
map(names(Data.ee.mm),function(x){
  point.dff<-Data.ee.mm[[x]]
  plotMU_cat(x,point.dff,"Cobertura",'parentEventID','MU_Est_Cob')
})

#habitat
map(names(Data.ee.mm),function(x){
  point.dff<-Data.ee.mm[[x]]
  plotMU_cat(x,point.dff,'habitat','parentEventID','MU_Est_hab')
})

#Suelo
map(names(Data.ee.mm),function(x){
  point.dff<-Data.ee.mm[[x]]
  plotMU_cat(x,point.dff,'UCSuelo','parentEventID','MU_Est_suel')
})

#Plataforma
map(names(Data.ee.mm),function(x){
  point.dff<-Data.ee.mm[[x]]
  plotMU_cat(x,point.dff,"Plataf",'parentEventID','MU_Est_Cob')
})

#box plot by ctnm
map(names(Data.ee.mm),function(x){
  point.dff<-Data.ee.mm[[x]]
  boxpMU_cat(x,point.dff,ctnm,'HabMU_est')
})
#boxplot by Red h?drica
map(names(Data.ee.mm),function(x){
  point.dff<-Data.ee.mm[[x]]
  boxpMU_cat(x,point.dff,'Red.Hidrica','RedHMU_est')
})
#box plot by Cobertura
map(names(Data.ee.mm),function(x){
  point.dff<-Data.ee.mm[[x]]
  boxpMU_cat(x,point.dff,"Cobertura",'CobMU_est')
})
#box plot by habitat
map(names(Data.ee.mm),function(x){
  point.dff<-Data.ee.mm[[x]]
  boxpMU_cat(x,point.dff,'habitat','habit_est')
})

#box plot by suelo
map(names(Data.ee.mm),function(x){
  point.dff<-Data.ee.mm[[x]]
  boxpMU_cat(x,point.dff,'UCSuelo','suelo_est')
})

#box plot by Plataforma
map(names(Data.ee.mm),function(x){
  point.dff<-Data.ee.mm[[x]]
  boxpMU_cat(x,point.dff,"Plataf",'CobMU_est')
})


#plot by MU with sampling groups
plotMU_cat('grp',Data.ee.prr,'grp','parentEventID','MU_EstGr_Inc')

#Suelo
  plotMU_cat(names(Data.ee.pr),Data.ee.prr,'UCSuelo','parentEventID','MU_Est_suel')
#ctnm
  plotMU_cat(names(Data.ee.pr),Data.ee.prr,ctnm,'parentEventID','MU_Est_cobHorm')


#plot by sub-MU-abundance
#Cobertura
map(names(Data.ee.nn),function(x){
  point.dff<-Data.ee.nn[[x]]
  plotMU_cat(x,point.dff,"Cobertura",'eventID','subMU_Est_Cob')
})
#ctnm
map(names(Data.ee.nn),function(x){
  point.dff<-Data.ee.nn[[x]]
  plotMU_cat(x,point.dff,ctnm,'eventID','subMU_Est')
})

#Plataforma
map(names(Data.ee.nn),function(x){
  point.dff<-Data.ee.nn[[x]]
  plotMU_cat(x,point.dff,"Plataf",'eventID','subMU_Est')
})

#habitat
map(names(Data.ee.nn),function(x){
  point.dff<-Data.ee.nn[[x]]
  plotMU_cat(x,point.dff,'habitat','eventID','subMU_Est_habit')
})

#Suelo
map(names(Data.ee.nn),function(x){
  point.dff<-Data.ee.nn[[x]]
  plotMU_cat(x,point.dff,'UCSuelo','eventID','subMU_Est_Suelo')
})


#plot by sub-MU-period abundance
#ctnm
map(names(Data.ee.tt),function(x){
  point.dff<-Data.ee.tt[[x]]
  plotMU_cat(x,point.dff,ctnm,'eventPer','HabsubMUPer_Est')
})

#Cobertura
map(names(Data.ee.tt),function(x){
  point.dff<-Data.ee.tt[[x]]
  plotMU_cat(x,point.dff,'Cobertura','eventPer','CobsubMUPer_Est')
})

#Plataforma
map(names(Data.ee.tt),function(x){
  point.dff<-Data.ee.tt[[x]]
  plotMU_cat(x,point.dff,'Plataf','eventPer','CobsubMUPer_Est')
})

#Plot by sub-MU-Incidence-period
plotMU_cat('grp',Data.ei.ttt,'grp','evenPer','subMU_EstGr_Inc')


#boxplot by Red.Hidrica, inc
map(names(grp),function(x){
  point.dff<-Data.ee.prr%>%filter(grp==x)
  flnm<-paste('RedHMU_est_inc',x,sep='_')
  boxpMU_cat(x,point.dff,'Red.Hidrica',flnm)
})
#boxplot by ctnm, inc
map(names(grp),function(x){
  point.dff<-Data.ee.prr%>%filter(grp==x)
  flnm<-paste(ctnm,'MU_est_inc',x,sep='_')
  boxpMU_cat(x,point.dff,ctnm,flnm)
})

#boxplot by Cobertura, inc
map(names(grp),function(x){
  point.dff<-Data.ee.prr%>%filter(grp==x)
  flnm<-paste('CobMU_est_inc',x,sep='_')
  boxpMU_cat(x,point.dff,'Cobertura',flnm)
})
#boxplot by habitat, inc
map(names(grp),function(x){
  point.dff<-Data.ee.prr%>%filter(grp==x)
  flnm<-paste('HabMU_est_inc',x,sep='_')
  boxpMU_cat(x,point.dff,'habitat',flnm)
})

#boxplot by Suelo, inc
map(names(grp),function(x){
  point.dff<-Data.ee.prr%>%filter(grp==x)
  flnm<-paste('SueloMU_est_inc',x,sep='_')
  boxpMU_cat(x,point.dff,'UCSuelo',flnm)
})

#boxplot by Plataf, inc
map(names(grp),function(x){
  point.dff<-Data.ee.prr%>%filter(grp==x)
  flnm<-paste('PlatafMU_est_inc',x,sep='_')
  boxpMU_cat(x,point.dff,'Plataf',flnm)
})


#plot by continue variables 
Data.ee.e<-map(names(Data.ee.mm),function(x){
  point.df<-Data.ee.mm[[x]]%>%
    inner_join(.,samEff.ttt[[x]][,c("parentEventID","samplEff.1")],
               by="parentEventID")
  write.csv(point.df,file.path(WDOut,'CurvasDiversidad',paste(gnm,'_',x,'MU_Estim.csv',sep='')))
  return(point.df)
})
names(Data.ee.e)<-names(Data.ee.mm)
kpv<-c(kpv,'Data.ee.e')


map(names(Data.ee.e), function(x){
  map(c(v.pres,v.rec,v.msite), function (vv){
    print(vv)
    plnm<-paste('DivP_',gnm,'_',x,'_',vv,sep='')
    point.df<-Data.ee.e[[x]]
    plotCvar(point.df,vv,plnm,x)
  })
})

# plots with incidence data
map(unique(Data.ee.prr$grp), function(x){ 
  map(c(v.msite,v.pres,v.rec), function (v){
    point.df<-Data.ee.prr%>%dplyr::select(estimado,order,grp,cmb_smpEf,all_of(v))%>%
      filter(grp==x)
    names(point.df)[names(point.df)=='cmb_smpEf']<-'samplEff.1'
    plnm<-paste('DivP_Inc_',gnm,'_',x,'_',v,sep='')
    plotCvar(point.df,v,plnm,x)
  })})

rm(list=ls()[!ls()%in%kpv])

save.image(file.path(WDOut,paste("wrkspc",gnm,Sys.Date(),".RData",sep="")))

#6 Curvas de Rank-Abundancia
#Aves
ommt<-c("Recorrido en lancha","Recorrido Libre")
ompv<-c("ANH_380","ANH_64","ANH_65")
#Arboles/Reptiles/anfibios/coprofagos_ad/zooplanction
ommt<-c("")
ompv<-c("")
#Peces
ommt<-c("")
ompv<-c("ANH_9")
#roedores
ommt<-c("Manual")
ompv<-c("")
#murcielagos
ommt<-c("RedNiebla_Av","GrbUltrasonido")
ompv<-c("")

#ctnm
library(ggrepel);library(tidyr); library(tibble)
Data.rk.a<-Data.a.r(ctnm,gnm,fnn,scale=TRUE)
map(names(Data.rk.a), function(x){
  RA.data<-Data.rk.a[[x]]
  plotRnkAb(RA.data,gnm,ctnm,x)
})

#Cobertura
Data.rk.c<-Data.a.r('Cobertura',gnm,fnn)
map(names(Data.rk.c), function(x){
  RA.data<-Data.rk.c[[x]]
  plotRnkAb(RA.data,gnm,'Cobertura',x)
})

#habitat
Data.rk.c<-Data.a.r('habitat',gnm,fnn)
map(names(Data.rk.c), function(x){
  RA.data<-Data.rk.c[[x]]
  plotRnkAb(RA.data,gnm,'habitat',x)
})

#Plataforma
Data.rk.pf<-Data.a.r('Plataf',gnm,fnn,scale=TRUE)#paste(gnm,'f8'),fnn,scale=FALSE  gnm,fnn,scale=TRUE
map(names(Data.rk.pf), function(x){
  RA.data<-Data.rk.pf[[x]]
  plotRnkAb(RA.data,gnm,'Plataf',x)
})

#Red hidrica

Data.rk.rh<-Data.a.r('Red.Hidrica',gnm,fnn,scale=TRUE)
map(names(Data.rk.rh), function(x){
  RA.data<-Data.rk.rh[[x]]
  plotRnkAb(RA.data,gnm,'RedHidr',x)
})

#Suelos
Data.rk.sl<-Data.a.r('UCSuelo',gnm,fnn,scale=FALSE)
map(names(Data.rk.sl), function(x){
  RA.data<-Data.rk.sl[[x]]
  plotRnkAb(RA.data,gnm,'Suelo',x)
})

#Rank-abundance by hour
# shchtxt<-paste('Data.r$parentEventID',
#                'hour(as.POSIXct(Data.r$eventTime,format="%H:%M:%S"))',sep='_')
ommt<-c("RedNiebla_Av","GrbUltrasonido")
ompv<-c("")

library(lubridate)
Data.pr<-Data.r%>%
  mutate('Periodo'=hour(as.POSIXct(Data.r$eventTime,
                                             format="%H:%M:%S")))
Data.pr$Periodo[Data.pr$Periodo<18|Data.pr$Periodo>21]<-21
Data.pr<-Data.pr%>%filter(!parentEventID%in%ompv&!samplingProtocol%in%ommt)

Data.rk.at<-Data.a.rt(Data.pr,ctnm,'Periodo',gnm,fnn,scale=FALSE)
map(names(Data.rk.at), function(x){
  RA.data<-Data.rk.at[[x]]
  plotRnkAb(RA.data,gnm,"Cob_Period",x)
})

kpv<-c(kpv,'Data.rk.a','Data.rk.c','Data.rk.pf','Data.rk.rh','Data.rk.sl')
rm(list=ls()[!ls()%in%kpv])

save.image(file.path(WDOut,paste("wrkspc",gnm,Sys.Date(),".RData",sep="")))


#7) Ordenamiento NMDS por m?todo
load("Analisis/SalidasPreliminares/Escarabajos/wrkspcEsc2022-07-14.RData")

pal <- c("lightsalmon1", "gold1", "palegreen4","slategray3","lightpink3","skyblue2","sienna2",
         'olivedrab4','slateblue3')
clean_background <- theme(plot.background = element_rect("white"),
                          panel.background = element_rect("white"),
                          panel.grid = element_line("white"),
                          axis.line = element_line("gray25"),
                          axis.text = element_text(size = 12, color = "gray25"),
                          axis.title = element_text(color = "gray25"),
                          legend.text = element_text(size = 12),
                          legend.key = element_rect("white"))
#Abundancia-MU
ommt<-c("")
ompv<-c("")
#aves
ommt<-c("Recorrido en lancha","Recorrido Libre")
ompv<-c("ANH_380","ANH_64","ANH_65")
#Peces
ommt<-c("")
ompv<-c("")
#murcielagos
ommt<-c("RedNiebla_Av","GrbUltrasonido")
ompv<-c("")

#if funcion=sum
Data.r.ab<-Data.r%>%filter((!parentEventID%in%ompv)&(!samplingProtocol%in%ommt))%>%
  dplyr::select(parentEventID,samplingProtocol,
                           organismQuantity,scientificName_2)%>%
  pivot_wider(id_cols=c(parentEventID,samplingProtocol),
              names_from=scientificName_2,values_from=organismQuantity, 
              values_fn=sum,values_fill=0)

#hidrobiologicos
# selc<-sapply(Data.r.ab,is.numeric)
# Data.r.ab2<-as.matrix(Data.r.ab[,selc])
# Data.r.ab4<-ceiling(Data.r.ab2/apply(Data.r.ab2,1,function(x){y<-min(x[x>0])}))
# Data.r.ab[,selc]<-Data.r.ab4
# print(head(Data.r.ab))
# Data.r.ab<-Data.r.ab%>%group_split(samplingProtocol)


#others
Data.r.ab<-Data.r.ab%>%group_split(samplingProtocol)

#iF funcion=max
# Data.r.ab<-Data.r%>%filter((!parentEventID%in%ompv)&(!samplingProtocol%in%ommt))%>%
#   dplyr::select(parentEventID,samplingProtocol,
#                            organismQuantity,scientificName_2)%>%
#   pivot_wider(id_cols=c(parentEventID,samplingProtocol),
#               names_from=scientificName_2,values_from=organismQuantity, 
#               values_fn=max,values_fill=0)%>%
#   group_split(samplingProtocol)


names(Data.r.ab)<-names(Data.ee.oo)

#Abundancia -periodo
##modify if non-integer abundances
Data.s.ab<-Data.r%>%filter((!parentEventID%in%ompv)&(!samplingProtocol%in%ommt))%>%
  mutate('eventPer'=gsub(schtxt,"_",eventID))%>%dplyr::select(-eventID)%>%
  dplyr::select(eventPer,organismQuantity,samplingProtocol,scientificName_2)%>%
  pivot_wider(id_cols=c(eventPer,samplingProtocol),
              names_from=scientificName_2,values_from=organismQuantity, 
              values_fn=sum,values_fill=0)%>%
  mutate(MU=as.numeric(gsub('(^ANH_)([[:digit:]]+)(_.*$)','\\2',eventPer)))%>%
  group_split(samplingProtocol)
Data.s.abb<-map(Data.s.ab, function(x){
  Data.ss<-x%>%group_by(MU)%>% 
  filter(n()>1)%>%
  ungroup(.)%>%
  dplyr::select(-MU)})
names(Data.s.abb)<-levels(as.factor(Data.r$samplingProtocol))


kpv<-c(kpv,'pal','clean_background','Data.r.ab','Data.s.abb')

#by ctnm
#install.packages("BBmisc")
library(BBmisc) 
Data.r.ord<-getMNDS_abu(Data.r.ab,ctnm,'parentEventID')
names(Data.r.ord)<-names(Data.r.ab)
Data.r.df<-getNMDS_DF(Data.r.ord,'parentEventID')
names(Data.r.df)<-names(Data.r.ab)
PlotNMDS(Data.r.df,ctnm,'abu','parentEventID',lab=TRUE)
kpv<-c(kpv,'Data.r.ord','Data.r.df')

#by platform
Data.r.pl<-getMNDS_abu(Data.r.ab,'Plataf','parentEventID')
names(Data.r.pl)<-names(Data.r.ab)
Data.r.pl_df<-getNMDS_DF(Data.r.pl,'parentEventID')
names(Data.r.pl_df)<-names(Data.r.ab)
PlotNMDS(Data.r.pl_df,'Plataf','abu','parentEventID',lab=TRUE)
kpv<-c(kpv,'Data.r.pl','Data.r.pl_df')

#by Cobertura
Data.r.hb<-getMNDS_abu(Data.r.ab,'Cobertura','parentEventID')
names(Data.r.hb)<-names(Data.r.ab)
Data.r.hb_df<-getNMDS_DF(Data.r.hb,'parentEventID')
names(Data.r.hb_df)<-names(Data.r.ab)
PlotNMDS(Data.r.hb_df,'Cobertura','abu','parentEventID')
kpv<-c(kpv,'Data.r.hb','Data.r.hb_df')

#by habitat
Data.r.ht<-getMNDS_abu(Data.r.ab,'habitat','parentEventID')
names(Data.r.ht)<-names(Data.r.ab)
Data.r.ht_df<-getNMDS_DF(Data.r.ht,'parentEventID')
names(Data.r.ht_df)<-names(Data.r.ab)
PlotNMDS(Data.r.ht_df,'habitat','abu','parentEventID')
kpv<-c(kpv,'Data.r.ht','Data.r.ht_df')

#by Red.Hidrica
Data.r.rh<-getMNDS_abu(Data.r.ab,'Red.Hidrica','parentEventID')
names(Data.r.rh)<-names(Data.r.ab)
rh<-cov%>%dplyr::select(parentEventID,Red.Hidrica)%>%
  filter(!duplicated(parentEventID))%>%
  column_to_rownames(.,var='parentEventID')
names(rh)<-'grr' 
Data.r.rh_df<-getNMDS_gr(Data.r.rh,rh)
names(Data.r.rh_df)<-names(Data.r.ab)
PlotNMDS(Data.r.rh_df,'RedHid','abu','parentEventID',lab=TRUE)
kpv<-c(kpv,'Data.r.rh','Data.r.rh_df')

#by Suelo
Data.r.sl<-getMNDS_abu(Data.r.ab,'UCSuelo','parentEventID')
names(Data.r.sl)<-names(Data.r.ab)
rh<-cov%>%dplyr::select(parentEventID,UCSuelo)%>%
  filter(!duplicated(parentEventID))%>%
  column_to_rownames(.,var='parentEventID')
names(rh)<-'grr' 
Data.r.sl_df<-getNMDS_gr(Data.r.sl,rh)
names(Data.r.sl_df)<-names(Data.r.ab)
PlotNMDS(Data.r.sl_df,'Suelo','abu','parentEventID',lab=TRUE)
kpv<-c(kpv,'Data.r.sl','Data.r.sl_df')


#Ordenamiento by period SubMU-Abundance
Data.s.ord<-getMNDS_abu(Data.ab = Data.s.abb,vcat = ctnm,evenID = 'eventPer')
names(Data.s.ord)<-names(Data.r.ab)
Data.s.df<-getNMDS_DF(Data.s.ord,'eventPer')
names(Data.s.df)<-names(Data.s.abb)
PlotNMDS(Data.s.df,ctnm,'abu','Per')
kpv<-c(kpv,'Data.s.ord','Data.s.df')

#7b) how species contribute to dissimilarity
Data.r.sp<-NMDS_Sp(Data.r.ord,Data.r.ab,'abu','parentEventID')
names(Data.r.sp)<-names(Data.r.ord)
plotNMDS_sp(Data.r.sp,Data.r.df,ctnm,'abu','parentEventID',lab=FALSE)
plotNMDS_sp(Data.r.sp,Data.r.rh_df,'Red.Hidrica','abu','parentEventID')
plotNMDS_sp(Data.r.sp,Data.r.pl_df,'Platf','abu','parentEventID',lab=FALSE)
plotNMDS_sp(Data.r.sp,Data.r.hb_df,'Cobertura','abu','parentEventID')
plotNMDS_sp(Data.r.sp,Data.r.ht_df,'habitat','abu','parentEventID',lab=TRUE)
plotNMDS_sp(Data.r.sp,Data.r.sl_df,'Suel','abu','parentEventID',lab=FALSE)


#subMU-period
Data.s.sp<-NMDS_Sp(Data.s.ord,Data.s.abb,'abu','eventPer')
names(Data.s.sp)<-names(Data.s.ord)
plotNMDS_sp(Data.sp = Data.s.sp,Data.df = Data.s.df,vcat = ctnm,sfx = 'abuPer',pcat = 'Per')
kpv<-c(kpv,'Data.s.sp','Data.r.sp')
rm(list=ls()[!ls()%in%kpv])

#8) how environmental variables structure the MU
cov.e<-cov%>%dplyr::select(all_of("parentEventID"),all_of(v.pres), all_of(v.rec),all_of(v.msite), all_of(spa.c))%>%
  filter(parentEventID%in%UM)%>%filter(!duplicated(parentEventID))%>%
  melt(.,id.vars=c('parentEventID','decimalLat','decimalLon'))%>%
  group_by(variable)%>%
  mutate(z=value/max(value))
f.nm<-file.path(WDOut,'Covariables_PCA',paste('Covar_',gnm,'.jpeg',sep=''))
g<-ggplot(aes(x=decimalLon,y=decimalLat,group=variable),data=cov.e)+
  geom_point(aes(size=z))+
  facet_wrap(vars(variable))+theme_test()
jpeg(f.nm, width = 600, height = 480, quality=300)
print(g)
dev.off()
print(g)
f.nm<-file.path(WDOut,'Covariables_PCA',paste('Covar_hist',gnm,'.jpeg',sep=''))
g<-ggplot(aes(x=z,group=variable),data=cov.e)+
  geom_histogram()+
  facet_wrap(vars(variable))+theme_test()
jpeg(f.nm, width = 600, height = 480, quality=300)
print(g)
dev.off()
print(g)
rm(list=ls()[!ls()%in%kpv])

#8) how environment explain community structure...exploration of contraits
#PCA macro-variables 
cov.ee.r<-cov%>%dplyr::select(all_of("parentEventID"),all_of(v.pres),all_of(v.rec))%>%
  filter(!duplicated(parentEventID))%>%
  filter(parentEventID%in%UM)%>%remove_rownames(.)%>%column_to_rownames(.,var="parentEventID")

env.pca<-rda(cov.ee.r,scale=TRUE)
sm<-summary(env.pca)
print(sm)
ev<-env.pca$CA$eig
ev.f<-ev[ev>mean(ev)]

outf<-file.path(WDOut,'Covariables_PCA',paste("PCA_macro",gnm,".txt",sep=""))
sink(outf)
print('### PCA con variables macro###')
print(sm)
print(ev.f)
sink()

par(mfrow=c(1,2))
f.nm<-file.path(WDOut,'Covariables_PCA',paste('Env_PCA',gnm,'%1d.jpeg',sep=''))
jpeg(f.nm, width = 600, height = 480, quality=300)
cleanplot.pca(env.pca,scaling=1)
cleanplot.pca(env.pca,scaling=2)
dev.off()
cleanplot.pca(env.pca,scaling=1)
cleanplot.pca(env.pca,scaling=2)

par(mfrow=c(1,1))
env.w<-hclust(dist(scale(cov.ee.r)))
f.nm<-file.path(WDOut,'Covariables_PCA',paste('Env_ClustP',gnm,'.jpeg',sep=''))
jpeg(f.nm, width = 600, height = 480, quality=300)
plot(env.w)
dev.off()
plot(env.w)
f.nm<-file.path(WDOut,'Covariables_PCA',paste('Env_Clust',gnm,'.jpeg',sep=''))
#Reptiles=4
#Anfibios=4
#Aves=6
#Peces=6
#coprofagos=6
#coprofagos_lv=3
#zooplancton=6
#perifiton=6
#macrofitas=6
#Macroinvertebrados=6
#Hormigas=5
#Mariposas=6
#Murcielagos=6
#Murcielagos_S=4
#Arboles=5
#epifitas=5
#Collembola=4
gr<-cutree(env.w,k=4)
grl<-levels(factor(gr))
sit.scl<-scores(env.pca,display='wa',scaling=1 )
sit.gr<-data.frame(sit.scl)
sit.gr$gr<-gr[match(names(gr),rownames(sit.scl))]
sit.gr$pch<-14+sit.gr$gr
sit.gr$col<-1+sit.gr$gr
jpeg(f.nm, width = 800, height = 480, quality=300)
p<-plot(env.pca, display='wa',scaling=1,type='n',main="PCA correlaci?n + clusters",
        xlim=c(min(sit.scl[,1]),1.5*max(sit.scl[,1])),
        ylim=c(min(sit.scl[,2]),1.5*max(sit.scl[,2])))
points(sit.gr,pch=sit.gr$pch,col=sit.gr$col,cex=2)
text(sit.scl,gsub('ANH_','',rownames(sit.scl)),cex=.7,pos=3)
legend(x=1.5*max(p$default[,1]),y=1.5*max(p$default[,2]),paste("grupo",c(1:length(grl))),pch=14+c(1:length(grl)),col=1+c(1:length(grl)),
       cex=0.7, y.intersp = 1,x.intersp=1, bty="n")
dev.off()
#map
cov.ee.r2<-cov.ee.r%>%rownames_to_column(.,'parentEventID')%>%
  mutate(pcaGr=gr)%>%
  inner_join(.,cov[,c('parentEventID','decimalLon','decimalLat')])%>%
  distinct(parentEventID,.keep_all=T)
g<-ggplot(cov.ee.r2,aes(x=decimalLon,y=decimalLat,color=as.factor(pcaGr),label=gsub('ANH_','',parentEventID)))+
  geom_point(size = 3, alpha = 0.8) +
  geom_text(size=3, color='darkblue',nudge_x = 0.003,nudge_y=0.003)+
  scale_color_manual(values = pal[1:length(unique(cov.ee.r2$pcaGr))]) +
  guides(colour=guide_legend(title='PCA grupo'))+
  clean_background +
  labs(title = paste(gnm,': Mapa de grupos'))
print(g)
f.nm<-file.path(WDOut,'Covariables_PCA',paste('MapaGruposPCA_',gnm,'.jpeg',sep=''))
jpeg(f.nm, width = 600, height = 480, quality=300)
print(g)
dev.off()
kpv<-c(kpv,"gr","env.pca")

#Micro-habitat variables
cov.ee.rm<-cov%>%dplyr::select(all_of("parentEventID"),all_of(v.msite))%>%
  filter(!duplicated(parentEventID))%>%
  filter(parentEventID%in%UM)%>%remove_rownames(.)%>%column_to_rownames(.,var="parentEventID")

envM.pca<-rda(cov.ee.rm,scale=TRUE)
sm<-summary(envM.pca)
print(sm)
ev<-envM.pca$CA$eig
ev.f<-ev[ev>mean(ev)]

outf<-file.path(WDOut,'Covariables_PCA',paste("PCA_micro",gnm,".txt",sep=""))
sink(outf)
print('### PCA con variables micro###')
print(sm)
print(ev.f)
sink()

par(mfrow=c(1,2))
f.nm<-file.path(WDOut,'Covariables_PCA',paste('EnvM_PCA',gnm,'%1d.jpeg',sep=''))
jpeg(f.nm, width = 600, height = 480, quality=300)
cleanplot.pca(envM.pca,scaling=1)
cleanplot.pca(envM.pca,scaling=2)
dev.off()
cleanplot.pca(envM.pca,scaling=1)
cleanplot.pca(envM.pca,scaling=2)

par(mfrow=c(1,1))
env.w<-hclust(dist(scale(cov.ee.rm)))
f.nm<-file.path(WDOut,'Covariables_PCA',paste('EnvM_ClustP',gnm,'.jpeg',sep=''))
jpeg(f.nm, width = 600, height = 480, quality=300)
plot(env.w)
dev.off()
plot(env.w)
f.nm<-file.path(WDOut,'Covariables_PCA',paste('EnvM_Clust',gnm,'.jpeg',sep=''))
#Peces=9
#zooplancton=8
#perifiton=6
#fitoplancton=5
#macrofitas=5
#macroinvertebrados=6

grm<-cutree(env.w,k=8)
grl<-levels(factor(grm))
sit.scl<-scores(envM.pca,display='wa',scaling=1 )
sit.gr<-data.frame(sit.scl)
sit.gr$gr<-gr[match(names(grm),rownames(sit.scl))]
sit.gr$pch<-14+sit.gr$gr
sit.gr$col<-1+sit.gr$gr
jpeg(f.nm, width = 800, height = 480, quality=300)
p<-plot(envM.pca, display='wa',scaling=1,type='n',main="PCA correlaci?n + clusters",
        xlim=c(min(sit.scl[,1]),1.5*max(sit.scl[,1])),
        ylim=c(min(sit.scl[,2]),1.5*max(sit.scl[,2])))
points(sit.gr,pch=sit.gr$pch,col=sit.gr$col,cex=2)
text(sit.scl,gsub('ANH_','',rownames(sit.scl)),cex=.7,pos=3)
legend(x=1.5*max(p$default[,1]),y=1.5*max(p$default[,2]),paste("grupo",c(1:length(grl))),pch=14+c(1:length(grl)),col=1+c(1:length(grl)),
       cex=0.7, y.intersp = 1,x.intersp=1, bty="n")
dev.off()
#map
cov.ee.rm2<-cov.ee.rm%>%rownames_to_column(.,'parentEventID')%>%
  mutate(pcaGr=grm)%>%
  inner_join(.,cov[,c('parentEventID','decimalLon','decimalLat')])%>%
  distinct(parentEventID,.keep_all=T)
g<-ggplot(cov.ee.rm2,aes(x=decimalLon,y=decimalLat,color=as.factor(pcaGr),label=gsub('ANH_','',parentEventID)))+
  geom_point(size = 3, alpha = 0.8) +
  geom_text(size=3, color='darkblue',nudge_x = 0.003,nudge_y=0.003)+
  scale_color_manual(values = pal[1:length(unique(cov.ee.rm2$pcaGr))]) +
  guides(colour=guide_legend(title='PCA-m grupo'))+
  clean_background +
  labs(title = paste(gnm,': Mapa de grupos'))
print(g)
f.nm<-file.path(WDOut,'Covariables_PCA',paste('MapaGruposMicroPCA_',gnm,'.jpeg',sep=''))
jpeg(f.nm, width = 600, height = 480, quality=300)
print(g)
dev.off()
kpv<-c(kpv,"grm","envM.pca")
rm(list=ls()[!ls()%in%kpv])

#8b) post-hoc plotting Macro
Data.r.dfE<-getNMDS_gr(Data.r.ord,gr)
names(Data.r.dfE)<-names(Data.r.ord)
v.final<-c(v.pres,v.rec)
Data.r.spE<-NMDS_env(Data.r.ord,Data.r.ab,v.final,'abu')
names(Data.r.spE)<-names(Data.r.ord)
plotNMDS_sp(Data.r.spE,Data.r.dfE,'PCA grupo','abu','parentEventID',lab=FALSE)
kpv<-c(kpv,'Data.r.dfE','Data.r.spE')
rm(list=ls()[!ls()%in%kpv])

#Micro
Data.r.dfEm<-getNMDS_gr(Data.r.ord,grm)
names(Data.r.dfEm)<-names(Data.r.ord)
Data.r.spEm<-NMDS_env(Data.r.ord,Data.r.ab,v.msite,'mabu')
names(Data.r.spEm)<-names(Data.r.ord)
plotNMDS_sp(Data.r.spEm,Data.r.dfEm,'PCA grupoM','abu','parentEventID')
kpv<-c(kpv,'Data.r.dfEm','Data.r.spEm')

#9) Ordenamiento NMDS usando incidencia 
#coprofagos/zooplancton/fitoplancton
ommt<-c("")
ompv<-c("")
#Aves<-
ommt<-c("")
ompv<-c("ANH_380")
#Peces
ommt<-c("")
ompv<-c("")
#murcielagos
ommt<-c("RedNiebla_Av")
ompv<-c("")

v.final<-c(v.pres,v.rec)

Data.ei.ab<-map(names(grp), function(x){
  selpID<-Data.ee.prr%>%filter(grp==x)%>%
    distinct(parentEventID)%>%pull(parentEventID)
  Y<-Data.r%>%filter((!parentEventID%in%ompv)&(!samplingProtocol%in%ommt))%>%
    dplyr::select(parentEventID,organismQuantity,scientificName_2)%>%
    filter(parentEventID%in%selpID)%>%
    pivot_wider(id_cols=parentEventID,names_from=scientificName_2,
                values_from=organismQuantity, values_fn=sum,values_fill=0)%>%
    mutate_if(is.numeric,~1*(.>0))
})
names(Data.ei.ab)<-names(grp)
Data.ei.ord<-getNMDS_i(Data.ei.ab,ctnm,'parentEventID')
names(Data.ei.ord)<-names(Data.ei.ab)
Data.ei.df<-getNMDS_i_DF(Data.ei.ord,'parentEventID')
names(Data.ei.df)<-names(Data.ei.ab)
PlotNMDS(Data.ei.df,ctnm,'Inc','parentEventID',lab=TRUE)
kpv<-c(kpv,'Data.ei.ab','Data.ei.ord')


#Red.hidrica
Data.ei.rh<-getNMDS_i(Data.ei.ab,'Red.Hidrica','parentEventID')
names(Data.ei.rh)<-names(Data.ei.ab)
Data.ei.df_rh<-getNMDS_i_DF(Data.ei.rh,'parentEventID')
names(Data.ei.df_rh)<-names(Data.ei.ab)
PlotNMDS(Data.ei.df_rh,'Red.Hidrica','Inc','parentEventID',lab=TRUE)
kpv<-c(kpv,'Data.ei.rh')

#Cobertura
Data.ei.hb<-getNMDS_i(Data.ei.ab,'Cobertura','parentEventID')
names(Data.ei.hb)<-names(Data.ei.ab)
Data.ei.df_hb<-getNMDS_i_DF(Data.ei.hb,'parentEventID')
names(Data.ei.df_hb)<-names(Data.ei.ab)
PlotNMDS(Data.ei.df_hb,'Cobertura','Inc','parentEventID')
kpv<-c(kpv,'Data.ei.hb')

#habitat
Data.ei.ht<-getNMDS_i(Data.ei.ab,'habitat','parentEventID')
names(Data.ei.ht)<-names(Data.ei.ab)
Data.ei.df_ht<-getNMDS_i_DF(Data.ei.ht,'parentEventID')
names(Data.ei.df_ht)<-names(Data.ei.ab)
PlotNMDS(Data.ei.df_ht,'habitat','Inc','parentEventID')
kpv<-c(kpv,'Data.ei.ht')

#Suelo
Data.ei.sl<-getNMDS_i(Data.ei.ab,'UCSuelo','parentEventID')
names(Data.ei.sl)<-names(Data.ei.ab)
Data.ei.df_sl<-getNMDS_i_DF(Data.ei.sl,'parentEventID')
names(Data.ei.df_sl)<-names(Data.ei.ab)
PlotNMDS(Data.ei.df_sl,'UCSuelo','Inc','parentEventID')
kpv<-c(kpv,'Data.ei.sl')

#-SubMU-period
Data.si.ab<-map(names(grp), function(x){
  selpID<-Data.ei.ttt%>%filter(grp==x)%>%distinct(evenPer,parentEventID)%>%
    group_by(parentEventID)%>%
    filter(n()>1)%>%
    ungroup(.)
  Y<-Data.r%>%filter((!parentEventID%in%ompv)&(!samplingProtocol%in%ommt))%>%
    mutate(eventPer=gsub(schtxt,"_",eventID),.keep="all")%>%
    dplyr::select(eventPer,organismQuantity,scientificName_2)%>%
    filter(eventPer%in%selpID$evenPer)%>%
    pivot_wider(id_cols=eventPer,
                names_from=scientificName_2,values_from=organismQuantity, 
                values_fn=sum,values_fill=0)%>%mutate_if(is.numeric,~1*(.>0))
  
})
names(Data.si.ab)<-names(grp)
Data.si.ord<-getNMDS_i(Data.si.ab,ctnm,'eventPer')
names(Data.si.ord)<-names(Data.si.ab)
Data.si.df<-getNMDS_i_DF(Data.si.ord,'eventPer')
names(Data.si.df)<-names(Data.si.ab)
PlotNMDS(Data.si.df,ctnm,'IncPer','Per')
kpv<-c(kpv,'Data.si.ab')

#10) post-hoc plotting
Data.ei.dfE<-getNMDS_gr(Data.ei.ord,gr)
names(Data.ei.dfE)<-names(Data.ei.ord)
Data.ei.dfEm<-getNMDS_gr(Data.ei.ord,grm)
names(Data.ei.dfEm)<-names(Data.ei.ord)
Data.ei.spE<-NMDS_Sp(Data.ei.ord,Data.ei.ab,'inc','parentEventID')
names(Data.ei.spE)<-names(Data.ei.ord)
plotNMDS_sp(Data.ei.spE,Data.ei.dfE,'PCA grupo','inc','parentEventID',lab=FALSE)
plotNMDS_sp(Data.ei.spE,Data.ei.dfEm,'PCA grupoM','inc','parentEventID')
plotNMDS_sp(Data.ei.spE,Data.ei.df,ctnm,'inc','parentEventID',lab=FALSE)
plotNMDS_sp(Data.ei.spE,Data.ei.df_rh,'Red.Hidrica','inc','parentEventID')
plotNMDS_sp(Data.ei.spE,Data.ei.df_hb,'Cobertura','inc','parentEventID')
plotNMDS_sp(Data.ei.spE,Data.ei.df_ht,'habitat','inc','parentEventID')
plotNMDS_sp(Data.ei.spE,Data.ei.df_sl,'UCSuelo','inc','parentEventID',lab=FALSE)

#plot environment correlation
Data.ei.En<-NMDS_env(Data.ei.ord,Data.ei.ab,v.final,'inc')
names(Data.ei.En)<-names(Data.ei.ord)
plotNMDS_sp(Data.ei.En,Data.ei.dfE,'PCA grupo','inc_E','parentEventID',lab=FALSE)
plotNMDS_sp(Data.ei.En,Data.ei.df,ctnm,'inc_E','parentEventID',lab=FALSE)
plotNMDS_sp(Data.ei.En,Data.ei.df_sl,'UCSuelo','inc_E','parentEventID',lab=FALSE)


#SubMU period
Data.si.spE<-NMDS_Sp(Data.si.ord,Data.si.ab,'inc','eventPer')
names(Data.si.spE)<-names(Data.si.ord)
plotNMDS_sp(Data.si.spE,Data.si.df,ctnm,'incPer','Per')
kpv<-c(kpv,'Data.ei.dfE','Data.si.spE')
rm(list=ls()[!ls()%in%kpv])


#9)how environmental variables relate to community structure once contraits are known
#Aves
v.pres2<-c("Dis_Oleodu","Dis_CP","Dis_ViaPri")
v.rec2<-c("Dis_MGSG","Dis_CobNat")
v.msite2<-NULL
#Reptiles
v.pres2<-c("Dis_ViaPri","Dis_Ferroc","Dis_Oleodu") #Peces [-c(1,3,4,7)]
v.rec2<-c("Dis_MGSG","Dis_Cienag","DisBosque")#v.rec2[-c(3,4,5)]
v.msite2<-NULL
#Peces
v.pres2<-c("Dis_ViaPri","Dis_Ferroc","Dis_Oleodu")
v.rec2<-c("Dis_MGSG","Dis_Cienag")
v.msite2<-c("GrasAceit","Nitratos","Limo","sed_Magnes","OxgD")#v.msite2[-c(1,2,4,5,6,8)]
#Anfibios
v.pres2<-c("Dis_ViaPri","Dis_Ferroc","Dis_Oleodu")
v.rec2<-c("Dis_MGSG","Dis_Cienag")
#coprofagos_ab
v.pres2<-c("Dis_Pozo","Dis_ViaPri","Dis_Oleodu")
v.rec2<-c("Tam_Parche","DisBosque")
#zooplancton
v.pres2<-c("Dis_CP","Dis_Ferroc","Dis_Pozo")
v.rec2<-c("DisBosque")
v.msite2<-c("Nitratos","Alcalinid","OxgD")
#perifiton
v.pres2<-c("Dis_Ferroc","Dis_ViaPri")
v.rec2<-c("Dis_Cienag")
v.msite2<-c("ProfCapFot","sed_Nitrog","sed_Magnes","Alcalinid","OxgD","Silicatos")
#fitoplancton
v.pres2<-c("Dis_Ferroc","Dis_ViaPri")
v.rec2<-c("Dis_Cienag","Dis_MGSG")
v.msite2<-c("Log_Cond","OxgD","ProfCapFot","sed_Nitrog","sed_p_limo","Alcalinid")
#macrofitas
v.pres2<-c("Dis_Ferroc","Dis_ViaPri")
v.rec2<-c("Dis_MGSG","Dis_CobNat")
v.msite2<-c("Log_Cond","OxgD","ProfCapFot","sed_Nitrog","sed_p_limo","Alcalinid")
#macroinvertebrados
v.pres2<-c("Dis_Ferroc","Dis_ViaPri")
v.rec2<-c("Dis_MGSG","Dis_CobNat")
v.msite2<-c("Log_Cond","OxgD","ProfCapFot","sed_Nitrog","sed_p_limo","Alcalinid")
#Hormigas
v.pres2<-c("Dis_Ferroc","Dis_Pozo","Dia_ViaSec")
v.rec2<-c("Dis_MGSG","DisBosque","Tam_Parche","Dis_CobNat")
v.msite2<-NULL
#Mariposas
v.pres2<-c("Dis_Pozo","Dia_ViaSec")
v.rec2<-c("Dis_Cienag","DisBosque","Dis_CobNat")
v.msite2<-NULL
#Murcielago
v.pres2<-c("Dis_Ferroc","Dis_ViaPri","Dis_Oleodu","Dis_CP")
v.rec2<-c("Dis_Cienag","Dis_MGSG","Tam_Parche")
v.msite2<-NULL
#Arboles
v.pres2<-c("Dis_Ferroc","Dis_Oleodu","Dis_Pozo")
v.rec2<-c("Dis_Cienag","Dis_MGSG","Tam_Parche")
v.msite2<-NULL
#Epifitas
v.pres2<-c("Dis_Oleodu","Dis_Pozo")
v.rec2<-c("Dis_Cienag","Dis_MGSG","Tam_Parche")
v.msite2<-NULL
#Collembola
v.pres2<-c("Dis_Pozo","Dis_Oleodu")
v.rec2<-c("Dis_Cienag","Dis_Dre345","Tam_Parche")
v.msite2<-NULL

kpv<-c(kpv,c('v.rec2','v.pres2','v.msite2'))


#dummy variables
# covc<-dummy_cols(cov,select_columns=c(ctnm),remove_first_dummy = TRUE)
# chngnm<-names(covc)[grep(paste('(',ctnm,'_)(.*)',sep=''),names(covc))]
# names(covc)[which(names(covc)%in%chngnm)]<-gsub(paste(ctnm,'_',sep=''),'',chngnm)
# chngnm<-gsub(paste(ctnm,'_',sep=''),'',chngnm)
# cov.ee<-covc%>%dplyr::select(all_of("parentEventID"),all_of(v.pres2),all_of(v.rec2),all_of(v.msite2),all_of(chngnm))%>%
#   filter(parentEventID%in%UM)%>%
#   filter(!duplicated(parentEventID))

cov.ee<-cov%>%dplyr::select(all_of("parentEventID"),
                            all_of(v.pres2),all_of(v.rec2),all_of(v.msite2),
                            all_of(ctnm))%>%
  filter(parentEventID%in%UM)%>%
  filter(!duplicated(parentEventID))

#9a) abundance data
outf<-file.path(WDOut,'RDA',paste("RDA_res_abu",gnm,".txt",sep=""))
sink(outf)
Data.r.rda<-map(names(Data.r.ab),function(xx){
  x<-Data.r.ab[[xx]]
  if(nrow(x)>1){
    y<-x[,-2]%>%column_to_rownames(.,var="parentEventID")
    colnames(y)<-gsub("^([[:alpha:]]{4}).*([[:space:]])([[:alpha:]]{4}).*$","\\1_\\3",
                      colnames(y))
    ints<-intersect(rownames(y),cov.ee$parentEventID)
    y<-y%>%dplyr::select(-which(colSums(y)==0))%>%filter(.,rownames(y)%in%ints)
    cov.ee2<-cov.ee%>%filter(.,parentEventID%in%ints)%>%filter(!duplicated(parentEventID))%>%
      arrange(match(parentEventID,rownames(y)))%>%dplyr::select(-parentEventID)
    y.hel<-decostand(y,method='hellinger')
    RDA.t<-rda(y.hel~.,data=cov.ee2)
    Ganova.rda<-anova.cca(RDA.t,step=1000)
    Sanova.rda<-anova.cca(RDA.t,by="axis", step=1000)
    pRDA.t1<-plot(RDA.t,scaling=1, main="scaling 1")
    arrow.mul<-attributes(pRDA.t1$biplot)$arrow.mul
    pRDA.t2<-plot(RDA.t,scaling=2,main="scaling 2")
    arrow.mul<-attributes(pRDA.t2$biplot)$arrow.mul
    print(paste("#### RDA for: ",xx,"####"))
    print(summary(RDA.t))
    print("#Canonical Coeficient#")
    print(coef(RDA.t))
    print("#Global significance#")
    print(Ganova.rda)
    print("#Axis significance#")
    print(Sanova.rda)
    print("-------------END-----------")
    vexp<-round(RsquareAdj(RDA.t)$adj.r.squared,3)
    vunexp<-round(RDA.t$CA$tot.chi/RDA.t$tot.chi,3)
    list("RDA.t"=RDA.t,"arrow.mul"=arrow.mul,"var.Exp"=vexp,"var.unexp"=vunexp)
  }else{
    list("RDA.t"=NULL,"arrow.mul"=NULL,"var.Exp"=NULL,"var.unexp"=NULL)
  }
})
sink()
names(Data.r.rda)<-names(Data.r.ab)
#ctnm
Data.r.rda.p(Data.r.rda,Data.r.ord,paste('abu_', ctnm,sep='')) #change names
#Cobertura
Data.r.rda.p(Data.r.rda,Data.r.hb,'abu_Cobertura')
#habitat
Data.r.rda.p(Data.r.rda,Data.r.ht,'abu_habitat')
#red hidrica
Data.r.rda.p(Data.r.rda,Data.r.rh,'abu_redhidrica')
#Suelo
Data.r.rda.p(Data.r.rda,Data.r.sl,'abu_suelo')


#9b) Incidence data
outf<-file.path(WDOut,'RDA',paste("RDA_res_Inc",gnm,".txt",sep=""))
sink(outf)
Data.r.rdaI<-map(names(Data.ei.ab),function(xx){
  x<-Data.ei.ab[[xx]]
  if(nrow(x)>1){
    y<-x%>%column_to_rownames(.,var="parentEventID")
    colnames(y)<-gsub("^([[:alpha:]]{4}).*([[:space:]])([[:alpha:]]{4}).*$","\\1_\\3",
                      colnames(y))
    ints<-intersect(rownames(y),cov.ee$parentEventID)
    y<-y%>%dplyr::select(-which(colSums(y)==0))%>%filter(.,rownames(y)%in%ints)
    cov.ee2<-cov.ee%>%filter(.,parentEventID%in%ints)%>%filter(!duplicated(parentEventID))%>%
      arrange(match(parentEventID,rownames(y)))%>%dplyr::select(-1)
    y.hel<-decostand(y,"hellinger")
    RDA.t<-rda(y.hel~.,data=cov.ee2)
    Ganova.rda<-anova.cca(RDA.t,step=1000)
    Sanova.rda<-anova.cca(RDA.t,by="axis", step=1000)
    pRDA.t1<-plot(RDA.t,scaling=1, main="scaling 1")
    arrow.mul<-attributes(pRDA.t1$biplot)$arrow.mul
    pRDA.t2<-plot(RDA.t,scaling=2,main="scaling 2")
    arrow.mul<-attributes(pRDA.t2$biplot)$arrow.mul
    print(paste("#### RDA for: ",xx,"####"))
    print(summary(RDA.t))
    print("#Canonical Coeficient#")
    print(coef(RDA.t))
    print("#Global significance#")
    print(Ganova.rda)
    print("#Axis significance#")
    print(Sanova.rda)
    print("-------------END-----------")
    vexp<-round(RsquareAdj(RDA.t)$adj.r.squared,3)
    vunexp<-round(RDA.t$CA$tot.chi/RDA.t$tot.chi,3)
    list("RDA.t"=RDA.t,"arrow.mul"=arrow.mul,"var.Exp"=vexp,"var.unexp"=vunexp)
  }else{
    list("RDA.t"=NULL,"arrow.mul"=NULL,"var.Exp"=NULL,"var.unexp"=NULL)
  }
})
sink()
names(Data.r.rdaI)<-names(Data.ei.ab)

Data.r.rda.p(Data.r.rdaI,Data.ei.ord,paste('Inc_', ctnm,sep='')) #Changenames
Data.r.rda.p(Data.r.rdaI,Data.ei.hb,'Inc_Cobertura')
Data.r.rda.p(Data.r.rdaI,Data.ei.ht,'Inc_habitat')
Data.r.rda.p(Data.r.rdaI,Data.ei.rh,'Inc_redhidrica')
Data.r.rda.p(Data.r.rdaI,Data.ei.sl,'Inc_suelo')
kpv<-c(kpv,'Data.r.rda','Data.r.rdaI')

save.image(file.path(WDOut,paste("wrkspc",gnm,Sys.Date(),".RData",sep="")))

#9b) save data for indicators
svLst<-c('Data.e','Data.r','SbUME','SbUMT','UM','UME','UMT','ctnm','fnn','cnm.smp','gnm','cov',
         'samEff.ttt','Data.ee.mm','Data.ee.nn','Data.ee.pr','Data.ee.prr','grp','Data.ei.t','Data.ei.ttt',
         'Data.ee.e','Data.rk.a','Data.r.ab','Data.s.abb','Data.r.pl','Data.r.pl_df','Data.s.ord','Data.s.df')
svLst<-intersect(svLst,ls())
save(list=svLst,
     file=file.path(WDOut,paste('WrkSp_Ind_',gnm,Sys.Date(),'.RData',sep='')))
rm(list=ls())


######
#6b ajuste esfuerzo de muestreo por unidad.
Data.m.ab<-Data.ee.e[[1]]%>%
  filter(order=="Species richness")%>%
  dplyr::select(observado, samplEff.1)%>%
  mutate(LogS=log10(observado),.keep="all")
corrD<-lm(LogS~samplEff.1,data=Data.m.ab)
corrDD<-summary(corrD)
Data.m.ab$fit_S<-fitted(corrD)
Data.m.ab$residS<-residuals(corrD)
Data.m.abb<-predict.lm(corrD,se.fit=T, interval="confidence")
Data.m.ab$lwr_S<-Data.m.abb[["fit"]][,'lwr']
Data.m.ab$upr_S<-Data.m.abb[["fit"]][,'upr']
g<-ggplot(Data.m.ab,aes(x=samplEff.1,y=LogS))+geom_point()+
  geom_line(aes(x=samplEff.1,y=fit_S),color="green")+
  geom_line(aes(x=samplEff.1,y=lwr_S),color="green",linetype=2)+
  geom_line(aes(x=samplEff.1,y=upr_S),color="green",linetype=2)
Data.m.ab$corrLgS<-corrDD$coefficients[1]+corrDD$coefficients[2]+Data.m.ab$residS
Data.m.ab$corrS<-10^Data.m.ab$corrLgS
g<-g+geom_point(aes(x=samplEff.1,y=corrLgS),color='red')
print(g)
h<-ggplot(Data.m.ab,aes(x=samplEff.1,y=corrS))+geom_point(color='red')+
  geom_smooth(aes(x=samplEff.1,y=corrS),method="lm",color='red')+
  geom_point(aes(x=samplEff.1,y=observado),color='blue')+
  geom_smooth(aes(x=samplEff.1,y=observado),color='blue',method="lm")

