#Date.created Sep.22.2021
#Dat.Modified Oct.11.2021

#Script to get basic biodiversity analyses for ANH

#0) Load required libraries
rqurd <- c("openxlsx","BiodiversityR","MASS","tidyverse","data.table","iNEXT",'reshape2','ggpubr',
           "ggpmisc",'evaluate',"maptools","rgdal","sp","lattice","ggplot2","rgeos","ade4","Rtsne","reshape2")

for (p in rqurd ){
  if (!is.element(p, installed.packages()[,1])){			#if package is not installed
    # r <- getOption("repos")								#assign R mirror for download
    # r["CRAN"] <- "http://cran.us.r-project.org"
    # options(repos = r)
    # rm(r)
    install.packages(p, dep = TRUE)			#install tuneR
    require(p, character.only = TRUE)
  }
}
for (p in rqurd){
  print(c("installing",p))
  try(library(p,character.only=T))
}
devtools::install_github("slowkow/ggrepel")
library(ggrepel)

source(file.path("C:","Users","dsrbu","Dropbox","Humboldt","6_RcodeRepository",
                 "14_Script_others","NEwR-2ed_code_data","NEwR2-Functions","cleanplot.pca.R"))

#0b) Define working directories

WDOut<-file.path('G:','My Drive','DiseñoAnalisis_PPII','Analisis','SalidasPreliminares','Peces')
WDOut<-file.path('C:','Users','dsrbu')
WDIn<-file.path('G:','My Drive','DiseñoAnalisis_PPII','Analisis','Matrices_Abundancia')
WDIn2<-file.path('G:','My Drive','DiseñoAnalisis_PPII','Analisis','Datos_Finales')
WDCov<-file.path('G:','My Drive','DiseñoAnalisis_PPII','Analisis','Covariables')

#0c) functions
PrintggiNext<-function(fnm,iNxt){
  f.nm<-file.path(WDOut,paste(fnm,'_type1.jpeg',sep=''))
  jpeg(f.nm, width = 480, height = 480, quality=300) 
  g1<-ggiNEXT(iNxt,type=1, facet.var = "order")+labs(title=paste('Diversidad Verdadera',fnm,sep=''), y='Diversidad extrapolada',x='Número de Individuos-bootstrap')+
    scale_linetype(labels=c("Extrapolado","Interpolado"))+facet_wrap(~order, nrow=1, labeller=as_labeller(c(`0` = "Riqueza", `1` = "Shannon",`2` = "Simpson")))+
    guides(linetype=guide_legend(title="Método"),
           colour=guide_legend(title="Grupo"), 
           fill=guide_legend(title="Grupo"), 
           shape=guide_legend(title="Grupo"))+ theme_classic()
  print(g1)
  dev.off()
  f.nm<-file.path(WDOut,paste(fnm,'_type2.jpeg',sep=''))
  jpeg(f.nm, width = 480, height = 480, quality=300) 
  g2<-ggiNEXT(iNxt,type=2)+labs(title=paste('Diversidad verdadera: ',fnm,sep=''), y='Cobertura de muestreo',x='Número de Individuos-bootstrap')+
    scale_linetype(labels=c("Extrapolado","Interpolado"))+
    guides(linetype=guide_legend(title="Método"),
           colour=guide_legend(title="Grupo"), 
           fill=guide_legend(title="Grupo"), 
           shape=guide_legend(title="Grupo"))+ theme_classic()
  print(g2)
  dev.off()
  f.nm<-file.path(WDOut,paste(fnm,'_type3.jpeg',sep=''))
  jpeg(f.nm, width = 480, height = 480, quality=300)  
  g3<-ggiNEXT(iNxt,type=3, facet.var = "order")+labs(title=paste('Diversidad verdadera: ',fnm,sep=''), y='Diversidad extrapolada',x='Cobertura de muestreo')+
    scale_linetype(labels=c("Extrapolado","Interpolado"))+facet_wrap(~order, nrow=1, labeller=as_labeller(c(`0` = "Riqueza", `1` = "Shannon",`2` = "Simpson")))+
    guides(linetype=guide_legend(title="Method"),
           colour=guide_legend(title="Grupo"), 
           fill=guide_legend(title="Grupo"), 
           shape=guide_legend(title="Grupo"))+ theme_classic()
  print(g3)
  dev.off()
  print(g1)
  print(g2)
  print(g3)
} #plots iNEXT three types of graphs
PrintRefiNext<-function(fnm,catnm,iNxt){
  point.df<-iNxt$AsyEst
  # Make a nice ggplot!
  f.nm<-file.path(WDOut,paste(fnm,'_Asym.jpeg',sep=''))
  g<-ggplot(point.df, aes(x=Site, y=Estimator)) + geom_bar(stat='identity')+
    geom_errorbar(aes(ymin=LCL, ymax=UCL), width=.01) +
    labs(y="Diversidad estimada", x = catnm, title=fnm) +
    facet_wrap(~Diversity, scale='free',nrow=3, labeller=as_labeller(c(`Species richness` = "Riqueza", 
                                                      `Shannon diversity` = "Shannon",
                                                      `Simpson diversity` = "Simpson")))+
    theme_classic()
  jpeg(f.nm, width = 480, height = 480, quality=300)
  print(g)
  dev.off()
  write.csv(point.df,file.path(WDOut, paste(fnm,'_Asym.csv',sep='')))
  print(g)
} #Plots asymptotic estimations by category
PrintggiNextInc<-function(fnm,iNxt){
  f.nm<-file.path(WDOut,paste(fnm,'_type1_Inc.jpeg',sep=''))
  jpeg(f.nm, width = 480, height = 480, quality=300) 
  g1<-ggiNEXT(iNxt,type=1, facet.var = "site")+
    labs(title=paste('Diversidad Verdadera ',fnm,sep=''), y='Diversidad extrapolada',
         x='Número de UM-bootstrap')+
    scale_linetype(labels=c("Extrapolado","Interpolado"))+facet_wrap(~site, nrow=2)+
    guides(linetype=guide_legend(title="Método"),
           colour=guide_legend(title="Estimador"), 
           fill=guide_legend(title="Estimador"), 
           shape=guide_legend(title="Estimador"))+ theme_classic()
  print(g1)
  dev.off()
  print(g1)
  f.nm<-file.path(WDOut,paste(fnm,'_type2_Inc.jpeg',sep=''))
  jpeg(f.nm, width = 480, height = 480, quality=300) 
  g2<-ggiNEXT(iNxt,type=2)+
    labs(title=paste('Diversidad verdadera: ',fnm,sep=''), 
         y='Cobertura de muestreo',x='Número de UM-bootstrap')+
    scale_linetype(labels=c("Extrapolado","Interpolado"))+
    guides(linetype=guide_legend(title="Método"),
           colour=guide_legend(title="Estimador"), 
           fill=guide_legend(title="Estimador"), 
           shape=guide_legend(title="Estimador"))+ theme_classic()
  print(g2)
  dev.off()
  print(g2)
  f.nm<-file.path(WDOut,paste(fnm,'_type3_Inc.jpeg',sep=''))
  jpeg(f.nm, width = 480, height = 480, quality=300)  
  g3<-ggiNEXT(iNxt,type=3, facet.var = "order")+
    labs(title=paste('Diversidad verdadera: ',fnm,sep=''), 
         y='Diversidad extrapolada',x='Cobertura de muestreo')+
    scale_linetype(labels=c("Extrapolado","Interpolado"))+
    facet_wrap(~order, nrow=1, labeller=as_labeller(c(`0` = "Riqueza", `1` = "Shannon",`2` = "Simpson")))+
    guides(linetype=guide_legend(title="Método"),
           colour=guide_legend(title="Estimador"), 
           fill=guide_legend(title="Estimador"), 
           shape=guide_legend(title="Estimador"))+ theme_classic()
  print(g3)
  dev.off()
  print(g3)
} #plots iNEXT three types of graphs with Incidence data
plotMU_cat<-function(x,point.df,catnm,feven,sxnm) { 
  names(point.df)[names(point.df)==catnm]<-'categ'
  names(point.df)[names(point.df)==feven]<-'fevenID'
  point.df$categ<-as.factor(point.df$categ)
  point.df<-point.df%>%arrange(categ)
  f.nm<-file.path(WDOut,paste(gnm,'_',x,sxnm,'.jpeg',sep=''))
  g<-ggplot(point.df, aes(x=fct_inorder(fevenID), y=estimado, color=categ)) + geom_point()+
    geom_errorbar(aes(ymin=LCL, ymax=UCL), width=.01) +
    labs(y="Diversidad estimada", x = "Unidad de muestreo") +
    facet_wrap(~order, nrow=3, labeller=as_labeller(c(`Species richness` = "Riqueza", 
                                                      `Shannon diversity` = "Shannon",
                                                      `Simpson diversity` = "Simpson")))+
    labs(tag=paste('Estimación asintótica para ',x))+
    theme_classic()+
    coord_cartesian(clip = "off") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          plot.tag=element_text(size=10))+
    guides(colour=guide_legend(title=catnm))
  print(g)
  jpeg(f.nm, width = 600, height = 480, quality=300)
  print(g)
  dev.off()
  print('success')
}
boxpMU_cat<-function(x,point.df,catnm,sxnm){
  names(point.df)[names(point.df)==catnm]<-'categ'
  point.df$categ<-as.factor(point.df$categ)
  point.df<-point.df%>%arrange(categ)
  f.nm<-file.path(WDOut,paste(gnm,'_',x,sxnm,'.jpeg',sep=''))
  g<-ggplot(point.df, aes(x=categ, y=estimado)) + geom_boxplot()+
    labs(y="Diversidad estimada", x = catnm) +
    facet_wrap(~order, nrow=3, labeller=as_labeller(c(`Species richness` = "Riqueza", 
                                                      `Shannon diversity` = "Shannon",
                                                      `Simpson diversity` = "Simpson")))+
    labs(tag=paste("Estimado para ",x,sep=''))+
    theme_classic()+
    coord_cartesian(clip = "off") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          plot.tag=element_text(size=10))
  print(g)
  jpeg(f.nm, width = 600, height = 480, quality=300)
  print(g)
  dev.off()}
getData.dfE<-function(gr,Data.ordi){
  names(gr)<-'gr'
  map(names(Data.ordi),function(xx){
    NMDS.t<-Data.ordi[[xx]][["NMDS"]]
    if(!is.null(NMDS.t)){
      site_Cat<-Data.ordi[[xx]][["site_cat"]]
      site_grp<-as.data.frame(gr)%>%rownames_to_column(.,var="parentEventID")%>%
        inner_join(.,site_Cat)%>%select(parentEventID,gr)%>%
        mutate(categ=as.factor(gr))%>%select(parentEventID,categ)
      plot_df <- scores(NMDS.t, display = "sites") %>% 
        as.data.frame() %>% 
        rownames_to_column("variable") %>% 
        inner_join(.,site_grp, by = c("variable"="parentEventID"))
    }
    else{
      plot_df<-NULL
    }
  })
}
plotOrd_grM<-function(cov.ee.rm2,plnm){
  g<-ggplot(cov.ee.rm2,aes(x=decimalLon,y=decimalLat,color=as.factor(pcaGr)))+
    geom_point(size = 3, alpha = 0.8) +
    scale_color_manual(values = pal[1:4]) +
    annotate("text",x=cov.ee.rm2$decimalLon,
             y=cov.ee.rm2$decimalLat, label=row.names(cov.ee.rm),size=2)+
    guides(colour=guide_legend(title=paste('Grupo',plnm,sep='_')))+
    clean_background +
    labs(title = paste(gnm,': Mapa de grupos por ',plnm))
  print(g)
  f.nm<-file.path(WDOut,paste('MapaGrPCA_',gnm,'_',plnm,'.jpeg',sep=''))
  jpeg(f.nm, width = 600, height = 480, quality=300)
  print(g)
  dev.off()
}
plotEnvFitG<-function(Data.dfE, plnm){
  map(names(Data.ei.spE),function(x){
    plot_df<-Data.dfE[[x]]
    if(!is.null(plot_df)){
      nlvl<-nlevels(plot_df$categ)
      fit_var<-Data.ei.spE[[x]]
      if (nrow(fit_var)>0){
        # new plot
        nmds_plot_new <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2)) +
          coord_fixed() +
          geom_point(aes(color = categ, shape = categ), size = 3, alpha = 0.8) +
          stat_chull(aes(color = categ,fill=categ),geom="polygon",alpha=0.1) +
          scale_color_manual(values = pal[1:nlvl]) +
          scale_fill_manual(values = pal[1:nlvl]) +
          geom_segment(data = fit_var, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
                       arrow = arrow(length = unit(0.25, "cm")),
                       col = "black") +
          geom_text_repel(data = fit_var, aes(label = variable)) +
          geom_text_repel(aes(label = site, color=categ)) +
          guides(colour=guide_legend(title="Grupo"), 
                 shape=guide_legend(title="Grupo"),
                 fill=guide_legend(title="Grupo"))+
          clean_background+labs(title = paste(gnm,'-post hoc:',x))
        print(nmds_plot_new)
        f.nm<-file.path(WDOut,paste(gnm,'NMDS_Inc_sp_E_',plnm,'_',x,'.jpeg',sep=''))
        jpeg(f.nm, width = 600, height = 480, quality=300)
        print(nmds_plot_new)
        dev.off()
      }
    }
  })}
Data.r.rda.p<-function(D.rda,D.ord,plnm){
  map(names(D.rda), function(x){
    RDA.t<-D.rda[[x]][["RDA.t"]]
    if(!is.null(RDA.t)){
      arrow.mul<-D.rda[[x]][["arrow.mul"]]
      vexp<-D.rda[[x]][["var.Exp"]]
      site_Cat<-D.ord[[x]][["site_cat"]]
      nlvl<-nlevels(site_Cat$categ)
      # vectors
      rdavectors <- as.matrix(scores(RDA.t,  display = "bp", scaling = 3)*arrow.mul) %>% 
        as.data.frame()
      
      # site coordinates
      site_data <- scores(RDA.t, display = "sites",scaling=3) %>% 
        as.data.frame() %>% 
        rownames_to_column("site") %>% 
        inner_join(.,site_Cat, by = c("site"="parentEventID"))
      
      # species coordinates
      species_data <- scores(RDA.t, display = "species", scaling=3) %>% 
        as.data.frame()
      species_lab<-species_data%>%filter((abs(RDA1)>(max(RDA1)/3))|(abs(RDA2)>(max(RDA2/3))))
      # plotting
      plot_rda <- ggplot(site_data) +
        geom_point(aes(x = RDA1, y = RDA2, color = categ),shape = 19, size = 2, alpha = 0.8)+
        scale_color_manual(values = pal[c(1:nlvl)]) +
        geom_segment(data = rdavectors, aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
                     arrow = arrow(length = unit(0.2, "cm")),color="black") +
        geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
        geom_hline(yintercept = c(0), color = "grey70", linetype = 2) + #arrow = arrow(length = unit(0.2, "cm")),
        geom_point(data = species_data, aes(x=RDA1,y=RDA2), 
                   color = "olivedrab4",shape=17,size=1) +
        geom_text_repel(data = species_lab, aes(x=RDA1,y=RDA2,label=rownames(species_lab)), 
                        color = "olivedrab4") +
        geom_text_repel(data = rdavectors, 
                        aes(x = RDA1, y = RDA2,label = rownames(rdavectors))) +
        clean_background +
        labs(title = paste('RDA_',gnm,':',x,'_',plnm,' Var Exp=',vexp))
      print(plot_rda)
      f.nm<-file.path(WDOut,paste('RDA_',gnm,'_',x,'_',plnm,'.jpeg',sep=''))
      jpeg(f.nm, width = 600, height = 480, quality=300)
      print(plot_rda)
      dev.off()
      return(plot_rda)}
    else{
      NULL
    }
  })
}
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
Data.a.f<-function(catnm){
  names(cov)[names(cov)==catnm]<-'categ'
  Data.r2<-Data.r%>%filter((!parentEventID%in%ompv)&(!samplingProtocol%in%ommt))
  nsp<-unique(Data.r2$parentEventID)
  cov.1<-cov%>%select(parentEventID,categ)%>%distinct(parentEventID,.keep_all=T)%>%
    filter(parentEventID%in%nsp)
  nsp<-length(nsp[!nsp%in%ompv])
  Data.ee.o<-Data.r2%>%
    select(parentEventID,samplingProtocol,organismQuantity,scientificName_2)%>%
  inner_join(.,cov.1, by="parentEventID")%>%
  pivot_wider(names_from=parentEventID,values_from=organismQuantity, values_fn=sum,values_fill=0)%>%
  mutate(TotAbu=rowSums(.[,4:(nsp+3)]),.keep="unused")%>%
  select(categ,samplingProtocol,scientificName_2,TotAbu)%>%
  group_split(samplingProtocol)
  names(Data.ee.o)<-levels(as.factor(Data.r2$samplingProtocol))
  Data.ee.oo<-map(Data.ee.o, function(x){
    y<-x%>%select(categ,scientificName_2,TotAbu)%>%
      pivot_wider(names_from=categ,values_from=TotAbu,values_fill=0)%>%
      column_to_rownames(.,var="scientificName_2")
  })
  return(Data.ee.oo)
} #getslist by category with abundance
Data.i.f<-function(catnm){
  names(cov)[names(cov)==catnm]<-'categ'
  Data.r2<-Data.r%>%filter((!parentEventID%in%ompv)&(!samplingProtocol%in%ommt))
  nsp<-unique(Data.r2$parentEventID)
  cov.1<-cov%>%select(parentEventID,categ)%>%
    distinct(parentEventID,.keep_all=T)%>%
    filter(parentEventID%in%nsp)
  nsp<-length(nsp[!nsp%in%ompv])
  Data.ei.o<-Data.r2%>%
    select(parentEventID,organismQuantity,scientificName_2)%>%
    inner_join(.,cov.1, by="parentEventID")%>%mutate(categ=as.factor(categ))%>%
     pivot_wider(names_from=parentEventID,values_from=organismQuantity, 
                 values_fn=sum,values_fill=0)%>%
    mutate_if(is.numeric,~1*(.>0))
  nmlvl<-levels(as.factor(as.character(Data.ei.o$categ)))
  print(nmlvl)
  Data.ei.o<-Data.ei.o%>%
    group_split(categ)
  names(Data.ei.o)<-nmlvl
  Data.ei.oo<-map(Data.ei.o, function(x){
    y<-x%>%select(-categ)%>%
      column_to_rownames(.,var="scientificName_2")%>%
      select(-which(colSums(.)==0))
  })
  return(Data.ei.oo)
} #gets list by category with incidence data
Data.a.MU<-function(DataP,evID,expPEID="^(ANH_[0-9]+)(_.*)$"){
  cov.1<-cov%>%distinct(parentEventID,.keep_all=T)%>%
    select('parentEventID',all_of(v.rec),all_of(v.pres),all_of(v.msite),all_of(cat.c))
  Data.r2<-DataP%>%filter((!parentEventID%in%ompv)&(!samplingProtocol%in%ommt))
  names(Data.r2)[names(Data.r2)==evID]<-'evID'
  Data.rr.n<-Data.r2%>%
    select(evID,organismQuantity,samplingProtocol,scientificName_2)%>%
    pivot_wider(names_from=evID,values_from=organismQuantity, values_fn=sum,values_fill=0)%>%
    group_split(samplingProtocol)
  names(Data.rr.n)<-levels(as.factor(Data.r2$samplingProtocol))
  Data.ee.nn<-map(names(Data.rr.n), function(x) {
    xx<-Data.rr.n[[x]][,-1]%>%column_to_rownames("scientificName_2")%>%select(-which(colSums(.)==0))
    iNext.o<-iNEXT(xx,q=c(0,1,2), datatype="abundance")
    point.df<-iNext.o$AsyEst
  if(evID=='parentEventID'){
      names(point.df)<-c('parentEventID','order','observado','estimado','s.e.','LCL','UCL')
    }else{
      point.df$parentEventID <- gsub(pattern = expPEID, replacement = "\\1", point.df$Site)
      names(point.df)<-c(evID,'order','observado','estimado','s.e.','LCL','UCL','parentEventID')
    }
    point.df2<-point.df%>%
      inner_join(.,cov.1, by="parentEventID")
    return(point.df2)
  })
  names(Data.ee.nn)<-names(Data.rr.n)
  return(Data.ee.nn)
} #gets abundance by MU or sub MU. List by sampling protocol
Data.a.pt<-function(grpp,Data.r2,evID){
  names(Data.r2)[names(Data.r2)==evID]<-'evID'
  yyy<-map(names(grpp), function(x) {
    y<-grpp[[x]]
    y.nm<-length(y)
    xx<-Data.r2%>%filter(samplingProtocol%in%y)%>%
      select(parentEventID,samplingProtocol,organismQuantity)%>%
      pivot_wider(names_from=samplingProtocol, values_from=organismQuantity,
                   values_fn=sum,values_fill=0)%>%
      mutate_if(is.numeric,~1*(.>0))%>%
      mutate('t'=rowSums(select(.,where(is.numeric))))%>%
      filter(t==y.nm)
    xxx<-Data.r2%>%
      select(evID,parentEventID,organismQuantity,samplingProtocol,scientificName_2)%>%
      filter(parentEventID%in%xx$parentEventID&samplingProtocol%in%y)%>%
      group_split(parentEventID)
    names(xxx)<-lapply(xxx,function(x) unique(x$parentEventID))
    yy<-map(xxx,function(w) {
      v<-w%>%select(-c(parentEventID,samplingProtocol))%>%
        pivot_wider(names_from=evID,
                    values_from=organismQuantity,
                    values_fn=sum,values_fill=0)
    })
  })
  names(yyy)<-names(grpp)
  return(yyy)
}#gets abundances merged by protocols using eventID or other column (except parenteventID).List by protocol groups 
Data.i.pr<-function(Data.ee.ss,grpp,evID,expPEID="^(ANH_[0-9]+)(_.*)$",selcc){
  Data.ee.ss2<-map(Data.ee.ss, function(y){
    vv<-map(y, function (v){
      v%>%mutate_if(is.numeric,~1*(.>0))%>%
        column_to_rownames("scientificName_2")%>%
        as.incfreq(.)
    })
    iNext.o<-iNEXT(vv,q=c(0,1,2), datatype="incidence_freq")
    point.df<-iNext.o$AsyEst
    names(point.df)<-c(evID,'order','observado','estimado','s.e.','LCL','UCL')
    if(!evID=='parentEventID'){
      point.df$parentEventID <- gsub(pattern = expPEID, replacement = "\\1", point.df[,evID])
    }
    return(point.df)
  })
   Data.ee.sss<-do.call(rbind,Data.ee.ss2)
   Data.ee.sss$grp<-rep(names(grpp),lapply(Data.ee.ss2,nrow))
  for(z in names(samEff.ttt)){
    y<-Data.ee.sss%>%left_join(.,samEff.ttt[[z]][,c('parentEventID','samplEff.1')])
    names(y)[length(names(y))]<-paste('SmpEf_',z,sep='')
    Data.ee.sss<-y
  }
  selc<-paste('SmpEf_',selcc,sep='')
  if(length(selcc)>1){
  Data.ee.sss$cmb_smpEf<-rowSums(Data.ee.sss[,selc])}
  else{
    Data.ee.sss$cmb_smpEf<-Data.ee.sss[,selc]
    }
  if(length(grpp)>1){
  sselc<-c(2:length(grpp))
  for(z in names(Data.ee.ss)[sselc]){
    zz<-grpp[[z]]
    zzz<-Data.ee.sss%>%
      filter(grp==z)%>%select(ends_with(zz))%>%
      transmute('t'=rowSums(.))%>%unlist(.)
    Data.ee.sss$cmb_smpEf[Data.ee.sss$grp==z]<-zzz
  }
  }
  Data.ee.sss<-Data.ee.sss%>%select(-ends_with(names(samEff.ttt)))
  return(Data.ee.sss)
}#gets diversity estimates for incidence and adds sampling effort.DF with group protocols
Data.a.t<-function(Data.t,evID,grpp,samEf,selcc){
  tt<-map(Data.t, function(y){
    yy<-map(y,function(v){
      vv<-v%>%column_to_rownames(.,var='scientificName_2')
      iNext.o<-iNEXT(vv,q=c(0,1,2), datatype="abundance")
      point.df<-iNext.o$AsyEst
      names(point.df)<-c(evID,'order','observado','estimado','s.e.','LCL','UCL')
      return(point.df)
    })
    yyy<-do.call(rbind,yy)
    yyy$parentEventID<-rep(names(yy),lapply(yy,nrow))
    rownames(yyy)<-NULL
    return(yyy)
  })
  ttt<-do.call(rbind,tt)
  ttt$grp<-rep(names(grpp),lapply(tt,nrow))
  for(z in names(samEf)){
    t4<-ttt%>%left_join(.,samEf[[z]][,c('parentEventID','samplEff.1')])
    names(t4)[ncol(t4)]<-paste('SmpEf_',z,sep='')
    ttt<-t4
  }
  selc<-paste('SmpEf_',selcc,sep='')
  if(length(selcc)>1){
    ttt$cmb_smpEf<-rowSums(ttt[,selc])}
  else{
    ttt$cmb_smpEf<-ttt[,selc]
  }
  if(length(grpp)>1){
    sselc<-c(2:length(grpp))
  for(z in names(ttt)[sselc]){
    zz<-grpp[[z]]
    zzz<-ttt%>%
      filter(grp==z)%>%select(ends_with(zz))%>%
      transmute('t'=rowSums(.))%>%unlist(.)
    ttt$cmb_smpEf[ttt$grp==z]<-zzz
  }
  }
  ttt<-ttt%>%select(-ends_with(names(samEf)))
} #gets final df with sampling effort
plotCvar<-function(point.df,v,prfnm,x){
  if(nrow(point.df)>3){
    mp<-NULL
    for(i in levels(point.df$order)){
      point.df2<-point.df[point.df$order==i,]
      wtdlm <- lm(estimado ~ get(v), data =point.df2 , weights = samplEff.1)
      mpp <- as.data.frame(cbind(v = point.df2[,v],
                                 predict(wtdlm, interval = 'confidence')))
      mpp$order<-i
      mp<-rbind(mp,mpp)
    }
    g<-ggplot(aes(y=estimado, x=get(v),colour=order), data=point.df)+
      geom_point()+
      geom_line(data = mp, aes(x = v, y = fit), size = 1, color = 'blue') +
      geom_line(data = mp, aes(x = v, y = lwr), color = 'gray80') +
      geom_line(data = mp, aes(x = v, y = upr), color = 'gray80') +
      geom_ribbon(data = mp, aes(x = v, y=fit, ymin = lwr, ymax = upr), alpha = 0.1)+
      facet_wrap(~order, nrow=3, labeller=as_labeller(c(`Species richness` = "Riqueza", 
                                                        `Shannon diversity` = "Shannon",
                                                        `Simpson diversity` = "Simpson")))+
      theme_bw()+
      theme(legend.position = 'none')+
      labs(title=paste('Diversidad por UM para',x),
           y='Indice',x=v)
    print(g)
    f.nm<-file.path(WDOut,paste(prfnm,'.jpeg',sep=''))
    jpeg(f.nm, width = 800, height = 480, quality=300)
    print(g)
    dev.off()}
  else{
    print('not enough points')
  }} #plots diversity estimates against continuos variables
getMNDS_abu<-function(Data.ab,vcat){
  Data.ab2<-map(names(Data.ab),function(xx){
    print(xx)
    x<-Data.ab[[xx]]
    if(nrow(x)>1){
      y<-x[,-2]%>%column_to_rownames(.,var="parentEventID")%>%select(-which(colSums(.)==0))
      colnames(y)<-gsub("^([[:alpha:]]{4}).*([[:space:]])([[:alpha:]]{4}).*$","\\1_\\3",colnames(y))
      cov1<-cov
      names(cov1)[names(cov1)==vcat]<-"categ"
      site_Cat<-cov1%>%select(parentEventID,categ)%>%
        filter(parentEventID%in%rownames(y))%>%
        filter(!duplicated(parentEventID))%>%
        arrange(match(parentEventID,rownames(y)))
      site_Cat$categ<-as.factor(site_Cat$categ)
      NMDS.t<-tryCatch(metaMDS(y,k=2,distance="bray",trymax=100, weakties=F), 
                       error=function(e) e) 
      if(is.error(NMDS.t)){
        print(NMDS.t)
        list("NMDS"=NULL ,"site_cat"=NULL)
      }else{
        list("NMDS"=NMDS.t,"site_cat"=site_Cat)
      }
    }else{
      list("NMDS"=NULL ,"site_cat"=NULL)
    }
  })
  return(Data.ab2)
}#Gets NMDS for each element in the list Data.ab
getNMDS_DF<-function(Data.ord){
  Data.ord2<-map(names(Data.ord),function(xx){
    NMDS.t<-Data.ord[[xx]][["NMDS"]]
    if(!is.null(NMDS.t)){
      site_Cat<-Data.ord[[xx]][["site_cat"]]
      plot_df <- scores(NMDS.t, display = "sites") %>% 
        as.data.frame() %>% 
        rownames_to_column("site") %>% 
        inner_join(.,site_Cat, by = c("site"="parentEventID"))
      varexp<-paste("stress: ", round(NMDS.t$stress,3),sep="")
      list("plot_df"=plot_df,"stress"=varexp)}
    else{
      list("plot_df"=NULL,"stress"=NULL)
    }
    
  })
  return(Data.ord2)
} #gets the info to plot NMDS
PlotNMDS<-function(Data.df,vcat,sfx){
  map(names(Data.df), function(x){
    xx<-Data.df[[x]][["plot_df"]]
    if(!is.null(xx)){
      nlvl<-nlevels(xx$categ)
      plot_nmds <- ggplot(xx, aes(x = NMDS1, y = NMDS2, color = categ, shape = categ)) +
        geom_point(size = 3, alpha = 0.8) +
        scale_color_manual(values = pal[1:nlvl]) +
        stat_ellipse(linetype = 2, size = 1) +
        guides(colour=guide_legend(title=vcat), 
               fill=guide_legend(title=vcat), 
               shape=guide_legend(title=vcat))+
        clean_background +
        labs(title = paste(gnm,':',x,Data.df[[x]][["stress"]]))
      print(plot_nmds)
      f.nm<-file.path(WDOut,paste('NMDS_',sfx,'_',gnm,'_',vcat,'_',x,'.jpeg',sep=''))
      jpeg(f.nm, width = 600, height = 480, quality=300)
      print(plot_nmds)
      dev.off()
    }
  })
  print("success")
} #plots NMDS by category
NMDS_Sp<-function(Data.ord,Data.ab,sfx){
  map(names(Data.ord),function(x){
    NMDS.t<-Data.ord[[x]][["NMDS"]]
    if(!is.null(NMDS.t)){
      Abu<-Data.ab[[x]][,-2]%>%column_to_rownames(.,var="parentEventID")
      colnames(Abu)<-gsub("^([[:alpha:]]{4}).*([[:space:]])([[:alpha:]]{4}).*$","\\1_\\3",
                          colnames(Abu))
      Abu<-Abu%>%select(-which(colSums(Abu)==0))
      fit <- envfit(NMDS.t, Abu, perm = 999) 
      # extract p-values for each species
      fit_pvals <- fit$vectors$pvals %>% 
        as.data.frame() %>% 
        rownames_to_column("variable") %>% 
        dplyr::rename("pvals" = ".")
      # extract coordinates for species, only keep species with p-val = 0.001
      fit_spp <- fit %>% 
        scores(., display = "vectors") %>% 
        as.data.frame() %>% 
        rownames_to_column("variable") %>% 
        full_join(., fit_pvals, by = "variable") %>% 
        filter(pvals < 0.05)
      write.csv(fit_spp,file.path(WDOut,paste(gnm,'_','_SpFit_',sfx,'_',x,'.csv',sep='')))
      return(fit_spp)
    }else{
      return(NULL)
    }
  })
}
plotNMDS_sp<-function(Data.sp,Data.df,vcat,sfx){
  map(names(Data.df),function(x){
    plot_df<-Data.df[[x]][["plot_df"]]
    if(!is.null(plot_df)){
      nlvl<-nlevels(plot_df$categ)
      fit_spp<-Data.sp[[x]]
      if (nrow(fit_spp)>0){
        # new plot
        nmds_plot_new <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2)) +
          coord_fixed() +
          geom_point(aes(color = categ, shape = categ), size = 3, alpha = 0.8) +
          stat_ellipse(aes(color = categ)) +
          scale_color_manual(values = pal[1:nlvl]) +
          geom_segment(data = fit_spp, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
                       arrow = arrow(length = unit(0.25, "cm")),
                       col = "black") +
          guides(colour=guide_legend(title=vcat),
                 shape=guide_legend(title=vcat))+
          geom_text_repel(data = fit_spp, aes(label = variable)) +
          clean_background+labs(title = paste(gnm,':',x,vcat,' p-value=0.05'))
        print(nmds_plot_new)
        f.nm<-file.path(WDOut,paste('NMDS_sp_',sfx,'_',gnm,'_',x,'_',vcat,'.jpeg',sep=''))
        jpeg(f.nm, width = 600, height = 480, quality=300)
        print(nmds_plot_new)
        dev.off()
      }
    }
  })
  print('success')
}
getNMDS_gr<-function(Data.ord,grr){
  ord_gr<-map(names(Data.ord),function(xx){
    NMDS.t<-Data.ord[[xx]][["NMDS"]]
    if(!is.null(NMDS.t)){
      site_Cat<-Data.ord[[xx]][["site_cat"]]
      site_grp<-as.data.frame(grr)%>%rownames_to_column(.,var="parentEventID")%>%
        inner_join(.,site_Cat)%>%select(parentEventID,grr)%>%
        mutate(categ=as.factor(grr))%>%select(parentEventID,categ)
      plot_df <- scores(NMDS.t, display = "sites") %>% 
        as.data.frame() %>% 
        rownames_to_column("variable") %>% 
        inner_join(.,site_grp, by = c("variable"="parentEventID"))
      varexp<-paste("stress: ", round(NMDS.t$stress,3),sep="")
      list("plot_df"=plot_df,"stress"=varexp)}
    else{
      list("plot_df"=NULL,"stress"=NULL)
    }
  })
  return(ord_gr)
}
NMDS_env<-function(Data.ord,Data.ab){
  ord_env<-map(names(Data.ord),function(x){
    NMDS.t<-Data.ord[[x]][["NMDS"]]
    if(!is.null(NMDS.t)){
      evn<-Data.ab[[x]]$parentEventID
      cov.r<-cov%>%select(all_of("parentEventID"),all_of(v.pres), 
                          all_of(v.rec))%>%filter(parentEventID%in%evn)%>%
        filter(!duplicated(parentEventID))%>%remove_rownames(.)%>%column_to_rownames(.,var="parentEventID")
      fit <- envfit(NMDS.t, cov.r, perm = 999) 
      # extract p-values for each species
      fit_pvals <- fit$vectors$pvals %>% 
        as.data.frame() %>% 
        rownames_to_column("variable") %>% 
        dplyr::rename("pvals" = ".")
      
      # extract coordinates for species, only keep species with p-val = 0.001
      fit_var <- fit %>% 
        scores(., display = "vectors") %>% 
        as.data.frame() %>% 
        rownames_to_column("variable") %>% 
        full_join(., fit_pvals, by = "variable") 
      write.csv(fit_var,file.path(WDOut,paste(gnm,'_Env_VarFit_',x,'.csv',sep='')))
      return(fit_var)
    }else{
      return(NULL)
    }
  })
  return(ord_env)
}
getNMDS_i<-function(Data.ab,vcat){
  map(names(Data.ab),function(xx){
    x<-Data.ab[[xx]]
    if(nrow(x)>1){
      y<-x%>%column_to_rownames(.,var="parentEventID")%>%select(-which(colSums(.)==0))
      colnames(y)<-gsub("^([[:alpha:]]{4}).*([[:space:]])([[:alpha:]]{4}).*$","\\1_\\3",colnames(y))
      cov1<-cov%>%select(parentEventID,vcat,all_of(v.pres),
                         all_of(v.rec),all_of(v.msite),all_of(cat.c))%>%
        distinct(parentEventID,.keep_all=T)
      names(cov1)[names(cov1)==vcat]<-"categ"
      site_Cat<-cov1%>%select(parentEventID,categ)%>%
        filter(parentEventID%in%rownames(y))%>%
        filter(!duplicated(parentEventID))%>%
        arrange(match(parentEventID,rownames(y)))
      site_Cat$categ<-as.factor(site_Cat$categ)
      NMDS.t<-tryCatch(metaMDS(y,k=2,distance="jaccard",trymax=100, weakties=F), 
                       error=function(e) e) 
      if(is.error(NMDS.t)){
        print(NMDS.t)
        list("NMDS"=NULL ,"site_cat"=NULL)
      }else{
        list("NMDS"=NMDS.t,"site_cat"=site_Cat)
      }
    }else{
      list("NMDS"=NULL ,"site_cat"=NULL)
    }
  })
}
getNMDS_i_DF<-function(Data.ordi){
  map(names(Data.ordi),function(xx){
    NMDS.t<-Data.ordi[[xx]][["NMDS"]]
    if(!is.null(NMDS.t)){
      site_Cat<-Data.ordi[[xx]][["site_cat"]]
      plot_df <- scores(NMDS.t, display = "sites") %>% 
        as.data.frame() %>% 
        rownames_to_column("variable") %>% 
        inner_join(.,site_Cat, by = c("variable"="parentEventID"))
      varexp<-paste("stress: ", round(NMDS.t$stress,3),sep="")
      list("plot_df"=plot_df,"stress"=varexp)}
    else{
      list("plot_df"=NULL,"stress"=NULL)
    }
  })
}

#1) covariances
covbk<-read.xlsx((file.path(WDCov,"BDPuntosMuestreoMag1910.xlsx")))
#names(covbk)[c(2,22,23)]<-c('parentEventID','CobSR','Cobertura')
names(covbk)[c(2,23,24)]<-c('parentEventID','CobSR','Cobertura')
covbk$Cobertura[is.na(covbk$Cobertura)]<-covbk$CobSR[is.na(covbk$Cobertura)]
covbk$Cobertura[covbk$Cobertura=="Bosque denso"]<-"Bosque Denso"
covbk$Cobertura[covbk$Cobertura=="Herbazales"]<-"Herbazal"
covbk$Cobertura[covbk$Cobertura=="Cienagas"]<-"Cienaga"
covbk$Cobertura[covbk$Cobertura=="Ciénaga"]<-"Cienaga"
covbk$Cobertura<-factor(covbk$Cobertura,levels=c("Rios","Cienaga","Zonas Pantanosas","Otros Cuerpos Agua",
                                                    "Herbazal","Bosque Ripario",
                                                    "Bosque Denso","Bosque Abierto","Vegetacion Secundaria",
                                                    "Palma","Cultivos","Pastos","Zonas Desnudas Degradadas",
                                                 "Vias","Area Urbana"))
spa.c<-c("decimalLat","decimalLon")
cat.c<-c("Plataf","Red.Hidrica","Orden")
v.pres<-c("Dis_CP","Dis_Oleodu", "Dis_Pozo","Dis_Pozact","Dis_Ferroc","Dis_ViaPri","Dia_ViaSec")
v.rec<-c("Dis_Cienag","Dis_MGSG")#, "DisBosque","Dis_CobNat","Tam_Parche")
v.msite<-NULL
##verify names
names(covbk)[grep("[E|e]ven",names(covbk))]<-c('eventID','parentEventID')
covbk$parentEventID<-trimws(gsub("-","_",covbk$parentEventID))
covbk$eventID<-trimws(gsub("-","_",covbk$eventID))
covbk<-covbk%>%select(-c('Tipo','GrupoBiolo'))
#1b) micro habitat covariates
CovM<-read.xlsx(file.path('G:','My Drive','DiseñoAnalisis_PPII',
                           'Analisis','Covariables','variablesAmbientales_microH.xlsx'),sheet=1,startRow = 3 )
names(CovM)[c(1,2,3,5,6,7,8,18,19)]<-c('parentEventID','Plataf','Temp','OxgD','Cond','Pgras','Mflot','Vrip','Cdos')
#transformation
CovM$Log_Cond<-log10(CovM$Cond)
CovM[is.na(CovM)]<-0
v.msite<-names(CovM)[c(3,4,5,13,17,18,19,20)]
#Join cov for fish only
covbk<-covbk%>%select(-Plataf)%>%inner_join(.,CovM,by="parentEventID")

cov<-covbk

#2) group specific variables
## section to verify that names of the columns is consistent
catnm<-"Orden" #main factor for análisis
gnm<-"Pec" #group prefix
cnm.smp<-c("samplingEffort","samplingProtocol") #from data
kpv<-c(ls(),'kpv') #variables to keep all the time


#2b) get raw data
Data.e<-read.xlsx(file.path(WDIn2,"I2D-BIO_2021_049_v2.xlsx"), sheet=1, startRow = 1, na.strings = "N/A")
Data.r<-read.xlsx(file.path(WDIn2,"I2D-BIO_2021_049_v2.xlsx"), sheet=2, startRow = 1, na.strings = "N/A")
#quality checks##
###this applies for fish ###
unique(Data.e$samplingProtocol)
Data.e$samplingProtocol[Data.e$samplingProtocol=="Red de arrastre"|Data.e$samplingProtocol=="Red de Arrastre"]<-"Arrastre"
Data.e$samplingProtocol[Data.e$samplingProtocol=="trasmallo"]<-"Trasmallo"
unique(Data.e$samplingProtocol)
### This applies for birds###
names(Data.e)[2]<-'parentEventID'
Data.e$samplingEffort[is.na(Data.e$samplingEffort)]<-0
### This applies for herpetos###
Data.e$samplingProtocol<-'VES'

#2c) complete columns
#regular expression that varies by group:
#for fish
#gsub(pattern = "^(ANH_[0-9]+)(_.*[C|D])$", replacement = "\\1", Data.r$eventID)
# for birds
#gsub(pattern = "^(ANH_[0-9]+)(_.*)$", replacement = "\\1", Data.r$eventID)

Data.r$parentEventID <- gsub(pattern = "^(ANH_[0-9]+)(_.*)$", replacement = "\\1", Data.r$eventID) 
UM<-unique(Data.r$parentEventID)
Data.r$scientificName_2<-trimws(Data.r$scientificName)
selrnm<-!is.na(Data.r$identificationQualifier)
Data.r$scientificName_2[selrnm]<-paste(Data.r$scientificName[selrnm],trimws(Data.r$identificationQualifier[selrnm]))
unique(Data.r$scientificName_2)
# quité las columnas que no se pueden completar desde BD_eventos o que no son necesarias en el análisis
Data.r <- complete_cols(Data.r, Data.e,  "parentEventID", c("eventID","parentEventID", 
                                                            "samplingProtocol",
                                                            "habitat"
                                                            ))
#quality control: varies from group to group.
Data.r$samplingProtocol <- trimws(Data.r$samplingProtocol)
Data.r$habitat<-trimws(Data.r$habitat)
Data.r$organismQuantity<-as.numeric(Data.r$organismQuantity)
#for birds
#gsub("([0-9]+\\.*[0-9]+).*$"
#for fish
#gsub("([0-9]+).*$")
     

#2d) get sampling effort
samEff.t<-Data.e[,c('parentEventID',cnm.smp)] %>% na.omit()%>%
  mutate(samplEff=as.numeric(gsub("([0-9]+).*$",'\\1',samplingEffort)))%>%
  group_by(parentEventID,get(cnm.smp[2]))%>%summarize(samplEff=sum(samplEff),Num_ev=n())
colnames(samEff.t)[1:2]<-c('parentEventID',cnm.smp[2])
sameEff.tt<-split(samEff.t,as.factor(samEff.t$samplingProtocol))
samEff.ttt<-map(sameEff.tt, function(x) data.frame(as.data.frame(x),decostand(as.vector(x[,"samplEff"]),"max")))
kpv<-c(kpv,'samEff.ttt','Data.e','Data.r','UM')
rm(list=ls()[!ls()%in%kpv])


#3a) Diversity by method: abundance
ommt<-c("Recorrido en lancha","Recorrido Libre") #method to be omitted
ompv<-c("ANH_380","ANH_64","ANH_65")
Data.r2<-Data.r%>%filter((!parentEventID%in%ompv)&(!samplingProtocol%in%ommt))
nsp<-unique(Data.r2$parentEventID)
nsp<-length(!nsp%in%ompv)
Data.ee.r<-Data.r2%>%
  select(parentEventID,organismQuantity,samplingProtocol,scientificName_2)%>%
  pivot_wider(names_from=parentEventID,values_from=organismQuantity, 
              values_fn=sum,values_fill=0)%>%
  mutate(TotAbu=rowSums(.[,3:(nsp+2)]),.keep="unused")%>%
  select(scientificName_2,samplingProtocol,TotAbu)%>%
  pivot_wider(names_from=samplingProtocol,values_from=TotAbu, values_fn=sum,values_fill=0)%>%
  column_to_rownames(.,var="scientificName_2")%>%as.list(.)
Hill.r<-iNEXT(Data.ee.r,q=c(0,1,2),datatype = "abundance")
PrintggiNext(paste(gnm,'_abM',sep=''),Hill.r)
kpv<-c(kpv,'Hill.r')
rm(list=ls()[!ls()%in%kpv])

#3b) Overall Diversity incidence
ommt<-c("")
ompv<-c("")
Data.r2<-Data.r%>%filter((!parentEventID%in%ompv)&(!samplingProtocol%in%ommt))
Data.ii.r<-Data.r2%>%
  select(parentEventID,organismQuantity,scientificName_2)%>%
  pivot_wider(names_from=parentEventID,values_from=organismQuantity, values_fn=sum,values_fill=0)%>%
  mutate_if(is.numeric,~1*(.>0))%>%column_to_rownames(.,var="scientificName_2")%>%list(.)
Hill.rr<-iNEXT(Data.ii.r,q=c(0,1,2),datatype = "incidence_raw")
names(Hill.rr$iNextEst)<-"Regional"
PrintggiNext(paste(gnm,'_incO',sep=''),Hill.rr)
kpv<-c(kpv,'Hill.rr')
rm(list=ls()[!ls()%in%kpv])

#3c) Hill by factor and method with abundance
ommt<-c("Recorrido en lancha","Recorrido Libre") #method to be omitted
ompv<-c("ANH_380","ANH_64","ANH_65")
# catnm
Data.ee.oo<-Data.a.f(catnm)
Data.ee.oo2<-map(names(Data.ee.oo),function(xx){
  x<-Data.ee.oo[[xx]]
  iNext.o<-iNEXT(x,q=c(0,1,2), datatype="abundance")
  return(iNext.o)
})
names(Data.ee.oo2)<-names(Data.ee.oo)
map(names(Data.ee.oo),function(xx){
  fnm2<-paste(gnm,catnm,xx,sep='_')
  v<-Data.ee.oo2[[xx]]
  PrintggiNext(fnm2,v)
  PrintRefiNext(fnm2,catnm,v)
  return()
})
kpv<-c(kpv,'Data.ee.oo')

## red. hídrica
Data.ee.rd<-Data.a.f('Red.Hidrica')
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
Data.ee.pt<-Data.a.f('Plataf')
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
kpv<-c(kpv,'Data.ee.pt')
rm(list=ls()[!ls()%in%kpv])

#3d) Hill by factor with Incidence
ommt<-c("") #method to be omitted
ompv<-c("ANH_380")

#catnm
Data.ei.oo<-Data.i.f(catnm)
Data.ei.oo2<-iNEXT(Data.ei.oo,q=c(0,1,2), datatype="incidence_raw")
fnm2<-paste(gnm,catnm,sep='_')
PrintggiNextInc(fnm2,Data.ei.oo2)
fnm2<-paste(gnm,catnm,'Inc',sep='_')
PrintRefiNext(fnm2,catnm,Data.ei.oo2)
kpv<-c(kpv,'Data.ei.oo')


#Red.hídrica
Data.ei.rd<-Data.i.f('Red.Hidrica')
Data.ei.rd2<-iNEXT(Data.ei.rd,q=c(0,1,2), datatype="incidence_raw")
fnm2<-paste(gnm, 'Red.Hidrica',sep='_')
PrintggiNextInc(fnm2,Data.ei.rd2)
fnm2<-paste(gnm,'Red.Hidrica','Inc',sep='_')
PrintRefiNext(fnm2,'Red.Hidrica',Data.ei.rd2)
kpv<-c(kpv,'Data.ei.rd2')



#plataforma
Data.ei.pt<-Data.i.f('Plataf')
Data.ei.pt2<-iNEXT(Data.ei.pt,q=c(0,1,2), datatype="incidence_raw")
fnm2<-paste(gnm, 'Plataf',sep='_')
PrintggiNextInc(fnm2,Data.ei.pt2)
fnm2<-paste(gnm,'Plataf','Inc',sep='_')
PrintRefiNext(fnm2,'Plataf',Data.ei.pt2)
kpv<-c(kpv,'Data.ei.pt2')

rm(list=ls()[!ls()%in%kpv])

#4) Hills by MU
ommt<-c("") #method to be omitted
ompv<-c("ANH_380","ANH_64","ANH_65")
Data.ee.mm<-Data.a.MU(Data.r,'parentEventID',
                      "^(ANH_[0-9]+)(_.*)$")
kpv<-c(kpv,'Data.ee.mm')
rm(list=ls()[!ls()%in%kpv])

#4b) Hills by sub-MU with abundance
ommt<-c("") #method to be omitted
ompv<-c("ANH_380","ANH_64","ANH_65")
Data.ee.nn<-Data.a.MU(Data.r,'eventID',"^(ANH_[0-9]+)(_.*)$")
kpv<-c(kpv,'Data.ee.nn')
rm(list=ls()[!ls()%in%kpv])

#4c) agregando periodo (para peces es lo mismo que Sub_MU)
ommt<-c("") #method to be omitted
ompv<-c("")
#text for period event
##fish
#gsub("_[R|A|E|T]_","_",eventID)
#aves
#No aplica
##herpetos
#gsub("_Herp_T[1|2|3]_","_",eventID)
Data.pr<-Data.r%>%
  mutate('eventPer'=gsub("_Herp_T[1|2|3]_","_",eventID))%>%select(-eventID)
Data.ee.tt<-Data.a.MU(Data.pr,'eventPer',"^(ANH_[0-9]+)(_.*)$")
kpv<-c(kpv,'Data.ee.tt')
rm(list=ls()[!ls()%in%kpv])

#4d) Diversidad agregando protocols
ommt<-""#c("Recorrido en lancha","Recorrido Libre")
ompv<-""#c("ANH_380","ANH_64","ANH_65")
Data.pt<-Data.r%>%
  filter((!parentEventID%in%ompv)&(!samplingProtocol%in%ommt))
nsp<-length(unique(Data.pt$parentEventID))
##peces
grp<-list('Ar_At'=c('Arrastre','Atarraya'),
           'Ar_At_El'=c('Arrastre','Atarraya','Electropesca'),
           'Ar_At_Tr'=c('Arrastre','Atarraya','Trasmallo'))
#herpetos
#grp<-list('VES'=c('VES'))
#Aves
# # grp<-list('PntFijo'=c('Punto Fijo'))
Data.ee.pr<-Data.a.pt(grp,Data.pt,'eventID')

#gets estimates from incidence data
Data.ee.prr<-Data.i.pr(Data.ee.pr,grp,'parentEventID',
                       expPEID='^(ANH_[0-9]+)(_.*)$',
                       c('Arrastre','Atarraya'))
cov.1<-cov%>%filter(parentEventID%in%Data.ee.prr$parentEventID)%>%
  distinct(parentEventID,.keep_all=T)%>%select(-c('eventID'))
Data.ee.prr<-Data.ee.prr%>%
  inner_join(.,cov.1, by="parentEventID")
write.csv(Data.ee.prr,file.path(WDOut, paste(gnm,'_','MU_Inc_Estim_Grp.csv',sep='')))
kpv<-c(kpv,'Data.ee.pr','Data.ee.prr','grp')
rm(list=ls()[!ls()%in%kpv])

#4e) diversidad por periodo combinando protocolos/Abundancia
ommt<-c("") #method to be omitted
ompv<-c("")
Data.pr<-Data.r%>%
  mutate('eventPer'=gsub("_Herp_T[1|2|3]_","_",eventID))%>%select(-eventID)
Data.ei.t<-Data.a.pt(grp,Data.pr,'eventPer')
Data.ei.ttt<-Data.a.t(Data.ei.t,'evenPer',grp,samEff.ttt,'VES')
write.csv(Data.ei.ttt,file.path(WDOut, paste(gnm,'_','SubTempMU_Estim_Grp.csv',sep='')))
kpv<-c(kpv,'Data.ei.t','Data.ei.ttt')
rm(list=ls()[!ls()%in%kpv])


#5) Plot with MU diversity
#plot with cover color
map(names(Data.ee.mm),function(x){
  point.dff<-Data.ee.mm[[x]]
  plotMU_cat(x,point.dff,catnm,'parentEventID','MU_Est')
})

#Red hidrica
map(names(Data.ee.mm),function(x){
  point.dff<-Data.ee.mm[[x]]
  plotMU_cat(x,point.dff,'Red.Hidrica','parentEventID','MU_Est_RH')
})
#plot by sub-MU-abundance
map(names(Data.ee.nn),function(x){
  point.dff<-Data.ee.nn[[x]]
  plotMU_cat(x,point.dff,'Cobertura','eventID','subMU_Est')
})
#plot by sub-MU-period abundance
map(names(Data.ee.tt),function(x){
  point.dff<-Data.ee.tt[[x]]
  plotMU_cat(x,point.dff,catnm,'eventPer','CobsubMUPer_Est')
})
#plot by MU with sampling groups
plotMU_cat('grp',Data.ee.prr,'grp','parentEventID','MU_EstGr_Inc')

#box plot by catnm
map(names(Data.ee.mm),function(x){
  point.dff<-Data.ee.mm[[x]]
  boxpMU_cat(x,point.dff,catnm,'CobMU_est')
  
})

#boxplot by Red hídrica
map(names(Data.ee.mm),function(x){
  point.dff<-Data.ee.mm[[x]]
  boxpMU_cat(x,point.dff,'Red.Hidrica','RedHMU_est')
  
})
#boxplot by Red.Hidrica, inc
map(names(grp),function(x){
  point.dff<-Data.ee.prr%>%filter(grp==x)
  flnm<-paste('RedHMU_est_inc',x,sep='_')
  boxpMU_cat(x,point.dff,'Red.Hidrica',flnm)
})
#boxplot by catnm, inc
map(names(grp),function(x){
  point.dff<-Data.ee.prr%>%filter(grp==x)
  flnm<-paste('CobMU_est_inc',x,sep='_')
  boxpMU_cat(x,point.dff,catnm,flnm)
})

#plot by continue variables 
Data.ee.e<-map(names(Data.ee.mm),function(x){
  point.df<-Data.ee.mm[[x]]%>%
    inner_join(.,samEff.ttt[[x]][,c("parentEventID","samplEff.1")],
               by="parentEventID")
  write.csv(point.df,file.path(WDOut, paste(gnm,'_',x,'MU_Estim.csv',sep='')))
  return(point.df)
})
names(Data.ee.e)<-names(Data.ee.mm)
kpv<-c(kpv,'Data.ee.e')

map(names(Data.ee.e), function(x){
  map(c(v.pres,v.rec,v.msite), function (vv){
    plnm<-paste('DivP_',gnm,'_',x,'_',vv,sep='')
    point.df<-Data.ee.e[[x]]
    plotCvar(point.df,vv,plnm,x)
  })
})
rm(list=ls()[!ls()%in%kpv])

# plots with incidence data
map(unique(Data.ee.prr$grp), function(x){ 
  map(c(v.msite,v.pres,v.rec), function (v){
    point.df<-Data.ee.prr%>%select(estimado,order,grp,cmb_smpEf,all_of(v))%>%
      filter(grp==x)
    names(point.df)[names(point.df)=='cmb_smpEf']<-'samplEff.1'
    plnm<-paste('DivP_Inc_',gnm,'_',x,'_',v,sep='')
    plotCvar(point.df,v,plnm,x)
  })})

#6) Ordenamiento NMDS por método
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

Data.r.ab<-Data.r%>%select(parentEventID,samplingProtocol,
                           organismQuantity,scientificName_2)%>%
  pivot_wider(id_cols=c(parentEventID,samplingProtocol),
              names_from=scientificName_2,values_from=organismQuantity, 
              values_fn=sum,values_fill=0)%>%
  group_split(samplingProtocol)
names(Data.r.ab)<-levels(as.factor(Data.r$samplingProtocol))


#by catnm
Data.r.ord<-getMNDS_abu(Data.r.ab,catnm)
names(Data.r.ord)<-names(Data.r.ab)
Data.r.df<-getNMDS_DF(Data.r.ord)
names(Data.r.df)<-names(Data.r.ab)
PlotNMDS(Data.r.df,catnm,'abu')
kpv<-c(kpv,'pal','clean_background','Data.r.ab','Data.r.ord')

#by platform
Data.r.pl<-getMNDS_abu(Data.r.ab,'Plataf')
names(Data.r.pl)<-names(Data.r.ab)
Data.r.pl_df<-getNMDS_DF(Data.r.pl)
names(Data.r.pl_df)<-names(Data.r.ab)
PlotNMDS(Data.r.pl_df,'Plataf','abu')

#by Red.Hidrica
Data.r.rh<-getMNDS_abu(Data.r.ab,'Red.Hidrica')
names(Data.r.rh)<-names(Data.r.ab)
rh<-cov%>%select(parentEventID,Red.Hidrica)%>%
  filter(!duplicated(parentEventID))%>%
  column_to_rownames(.,var='parentEventID')
names(rh)<-'grr' 
Data.r.rh_df<-getNMDS_gr(Data.r.rh,rh)
names(Data.r.rh_df)<-names(Data.r.ab)
PlotNMDS(Data.r.rh_df,'RedHid','abu')

#6b) how species contribute to dissimilarity
Data.r.sp<-NMDS_Sp(Data.r.ord,Data.r.ab,'abu')
names(Data.r.sp)<-names(Data.r.ord)
plotNMDS_sp(Data.r.sp,Data.r.df,catnm,'abu')
plotNMDS_sp(Data.r.sp,Data.r.rh_df,'Red.Hidrica','abu')
plotNMDS_sp(Data.r.sp,Data.r.pl_df,'Platf','abu')
rm(list=ls()[!ls()%in%kpv])

#7) how environmental variables structure the MU
cov.e<-cov%>%select(all_of("parentEventID"),all_of(v.pres), all_of(v.rec),all_of(v.msite), all_of(spa.c))%>%
  filter(parentEventID%in%UM)%>%filter(!duplicated(parentEventID))%>%
  melt(.,id.vars=c('parentEventID','decimalLat','decimalLon'))%>%
  group_by(variable)%>%
  mutate(z=value/max(value))
f.nm<-file.path(WDOut,paste('Covar_',gnm,'.jpeg',sep=''))
g<-ggplot(aes(x=decimalLon,y=decimalLat,group=variable),data=cov.e)+
  geom_point(aes(size=z))+
  facet_wrap(vars(variable))+theme_test()
jpeg(f.nm, width = 600, height = 480, quality=300)
print(g)
dev.off()
print(g)
f.nm<-file.path(WDOut,paste('Covar_hist',gnm,'.jpeg',sep=''))
g<-ggplot(aes(x=z,group=variable),data=cov.e)+
  geom_histogram()+
  facet_wrap(vars(variable))+theme_test()
jpeg(f.nm, width = 600, height = 480, quality=300)
print(g)
dev.off()
print(g)
rm(list=ls()[!ls()%in%kpv])

#8) how environment explain community structure...exploration of contraits
cov.ee.r<-cov%>%select(all_of("parentEventID"),all_of(v.pres),all_of(v.rec),all_of(v.msite))%>%
  filter(!duplicated(parentEventID))%>%
  filter(parentEventID%in%UM)%>%remove_rownames(.)%>%column_to_rownames(.,var="parentEventID")
#PCA on environmental variables #Presure variables
env.pca<-rda(cov.ee.r,scale=TRUE)
summary(env.pca)
ev<-env.pca$CA$eig
ev.f<-ev[ev>mean(ev)]


f.nm<-file.path(WDOut,paste('Env_PCA',gnm,'.jpeg',sep=''))
jpeg(f.nm, width = 600, height = 480, quality=300)
cleanplot.pca(env.pca,scaling=1)
#cleanplot.pca(env.pca,scaling=2)
dev.off()

env.w<-hclust(dist(scale(cov.ee.r)))
plot(env.w)
gr<-cutree(env.w,k=5)
grl<-levels(factor(gr))
sit.scl<-scores(env.pca,display='wa',scaling=1)

f.nm<-file.path(WDOut,paste('Env_Clust',gnm,'.jpeg',sep=''))
jpeg(f.nm, width = 600, height = 480, quality=300)
p<-plot(env.pca, display='wa',scaling=1,type='n',main="PCA correlación + clusters")
abline(v=0, lty="dotted")
abline(h=0, lty="dotted")
for(i in 1: length(grl)){
  points(sit.scl[gr==i,],pch=(14+i),cex=2,col=i+1)
}
text(sit.scl,row.names(cov.ee.r),cex=.7,pos=3)
ordicluster(p,env.w,col="dark grey")
legend(x=1,y=0.5, paste("grupo",c(1:length(grl))),pch=14+c(1:length(grl)),col=1+c(1:length(grl)),
       pt.cex=1, y.intersp = 0.8,x.intersp=0.3,bty="n")
dev.off()

cov.ee.r2<-cov.ee.r%>%rownames_to_column(.,'parentEventID')%>%
  mutate(pcaGr=gr)%>%
  inner_join(.,cov[,c('parentEventID','decimalLon','decimalLat')])%>%
  distinct(parentEventID,.keep_all=T)
g<-ggplot(cov.ee.r2,aes(x=decimalLon,y=decimalLat,color=as.factor(pcaGr)))+
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(values = pal[1:length(unique(cov.ee.r2$pcaGr))]) +
  guides(colour=guide_legend(title='PCA grupo'))+
  clean_background +
  labs(title = paste(gnm,': Mapa de grupos'))
print(g)
f.nm<-file.path(WDOut,paste('MapaGruposPCA_',gnm,'.jpeg',sep=''))
jpeg(f.nm, width = 600, height = 480, quality=300)
print(g)
dev.off()
kpv<-c(kpv,"gr")
rm(list=ls()[!ls()%in%kpv])

#8b) post-hoc plotting
Data.r.dfE<-getNMDS_gr(Data.r.ord,gr)
names(Data.r.dfE)<-names(Data.r.ord)
Data.r.spE<-NMDS_env(Data.r.ord,Data.r.ab)
names(Data.r.spE)<-names(Data.r.ord)
plotNMDS_sp(Data.r.spE,Data.r.dfE,'PCA grupo','abu')

kpv<-c(kpv,'Data.r.dfE','Data.r.spE')
rm(list=ls()[!ls()%in%kpv])


#9) Ordenamiento NMDS usando incidencia 
Data.ei.ab<-map(names(grp), function(x){
  selpID<-Data.ee.prr%>%filter(grp==x)%>%distinct(parentEventID)%>%pull(parentEventID)
  Y<-Data.r%>%select(parentEventID,organismQuantity,scientificName_2)%>%
    filter(parentEventID%in%selpID)%>%
    pivot_wider(id_cols=parentEventID,names_from=scientificName_2,
                values_from=organismQuantity, values_fn=sum,values_fill=0)%>%
    mutate_if(is.numeric,~1*(.>0))
})
names(Data.ei.ab)<-names(grp)

Data.ei.ord<-getNMDS_i(Data.ei.ab,catnm)
names(Data.ei.ord)<-names(Data.ei.ab)
Data.ei.df<-getNMDS_i_DF(Data.ei.ord)
names(Data.ei.df)<-names(Data.ei.ab)
PlotNMDS(Data.ei.df,catnm,'Inc')

#Red.hidrica
Data.ei.rh<-getNMDS_i(Data.ei.ab,'Red.Hidrica')
names(Data.ei.rh)<-names(Data.ei.ab)
Data.ei.df_rh<-getNMDS_i_DF(Data.ei.rh)
names(Data.ei.df_rh)<-names(Data.ei.ab)
PlotNMDS(Data.ei.df_rh,'Red.Hidrica','Inc')
kpv<-c(kpv,'Data.ei.ab','Data.ei.ord','Data.ei.df')

rm(list=ls()[!ls()%in%kpv])

#10) post-hoc plotting
Data.ei.dfE<-getNMDS_gr(Data.ei.ord,gr)
names(Data.ei.dfE)<-names(Data.ei.ord)
Data.ei.spE<-NMDS_Sp(Data.ei.ord,Data.ei.ab,'inc')
names(Data.ei.spE)<-names(Data.ei.ord)
plotNMDS_sp(Data.ei.spE,Data.ei.dfE,'PCA grupo','inc')
plotNMDS_sp(Data.ei.spE,Data.ei.df,catnm,'inc')
plotNMDS_sp(Data.ei.spE,Data.ei.df_rh,'Red.Hidrica','inc')

rm(list=ls()[!ls()%in%kpv])

#9)how environmental variables relate to community structure once contraits are known
v.pres<-v.pres[-c(2,3,4,6,7)]
v.rec<-v.rec[-c(1)]
v.msite<-v.msite[-c(1,2,4,5,6,8)]

cov.ee<-cov%>%select(all_of("parentEventID"),all_of(v.pres), all_of(v.rec),all_of(v.msite))%>%
  filter(parentEventID%in%UM)%>%
  filter(!duplicated(parentEventID))
#9a) abundance data
outf<-file.path(WDOut,paste("RDA_res_abu",gnm,".txt",sep=""))
sink(outf)
Data.r.rda<-map(names(Data.r.ab),function(xx){
  x<-Data.r.ab[[xx]]
  if(nrow(x)>1){
    y<-x[,-2]%>%column_to_rownames(.,var="parentEventID")
    colnames(y)<-gsub("^([[:alpha:]]{4}).*([[:space:]])([[:alpha:]]{4}).*$","\\1_\\3",
                      colnames(y))
    y<-y%>%select(-which(colSums(y)==0))
    y.hel<-decostand(y,"hellinger")
    cov.ee2<-cov.ee%>%filter(.,parentEventID%in%rownames(y))%>%filter(!duplicated(parentEventID))%>%
      arrange(match(parentEventID,rownames(y)))%>%select(-1)
    RDA.t<-rda(y.hel~.,data=cov.ee2)
    pRDA.t<-plot(RDA.t)
    arrow.mul<-attributes(pRDA.t$biplot)$arrow.mul
    print(paste("#### RDA for: ",xx,"####"))
    print(RDA.t)
    print(coef(RDA.t))
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

Data.r.rda.p(Data.r.rda,Data.r.ord,'abu')
#9b) Incidence data
outf<-file.path(WDOut,paste("C:","RDA_res_Inc",gnm,".txt",sep=""))
sink(outf)
Data.r.rdaI<-map(names(Data.ei.ab),function(xx){
  x<-Data.ei.ab[[xx]]
  if(nrow(x)>1){
    y<-x%>%column_to_rownames(.,var="parentEventID")
    colnames(y)<-gsub("^([[:alpha:]]{4}).*([[:space:]])([[:alpha:]]{4}).*$","\\1_\\3",
                      colnames(y))
    y<-y%>%select(-which(colSums(y)==0))
    y.hel<-decostand(y,"hellinger")
    cov.ee2<-cov.ee%>%filter(.,parentEventID%in%rownames(y))%>%filter(!duplicated(parentEventID))%>%
      arrange(match(parentEventID,rownames(y)))%>%select(-1)
    RDA.t<-rda(y.hel~.,data=cov.ee2)
    pRDA.t<-plot(RDA.t)
    arrow.mul<-attributes(pRDA.t$biplot)$arrow.mul
    print(paste("#### RDA for: ",xx,"####"))
    print(RDA.t)
    print(coef(RDA.t))
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

Data.r.rda.p(Data.r.rdaI,Data.ei.ord,'Inc')
kpv<-c(kpv,'Data.r.rda')

#####depricated#####
map(names(Data.r.spE),function(x){
  plot_df<-Data.r.dfE[[x]][['plot_df']]
  if(!is.null(plot_df)){
    nlvl<-nlevels(plot_df$categ)
    fit_var<-Data.r.spE[[x]]
    if (nrow(fit_var)>0){
      # new plot
      nmds_plot_new <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2)) +
        coord_fixed() +
        geom_point(aes(color = categ, shape = categ), size = 3, alpha = 0.8) +
        stat_chull(aes(color = categ,fill=categ),geom="polygon",alpha=0.1) +
        scale_color_manual(values = pal[1:nlvl]) +
        scale_fill_manual(values = pal[1:nlvl]) +
        geom_segment(data = fit_var, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
                     arrow = arrow(length = unit(0.25, "cm")),
                     col = "black") +
        geom_text_repel(data = fit_var, aes(label = variable)) +
        geom_text_repel(aes(label = site, color=categ)) +
        guides(colour=guide_legend(title="Grupo"), 
               shape=guide_legend(title="Grupo"),
               fill=guide_legend(title="Grupo"))+
        clean_background+labs(title = paste(gnm,'-post hoc:',x))
      print(nmds_plot_new)
      f.nm<-file.path(WDOut,paste('NMDS_sp_E',gnm,'_',x,'.jpeg',sep=''))
      jpeg(f.nm, width = 600, height = 480, quality=300)
      print(nmds_plot_new)
      dev.off()
    }
  }
})

map(names(Data.ei.ord), function(x){
  xx<-Data.ei.df[[x]][["plot_df"]]
  if(!is.null(xx)){
    nlvl<-nlevels(xx$categ)
    plot_nmds <- ggplot(xx, aes(x = NMDS1, y = NMDS2, color = categ, shape = categ)) +
      geom_point(size = 3, alpha = 0.8) +
      scale_color_manual(values = pal[1:nlvl]) +
      stat_ellipse(linetype = 2, size = 1) +
      guides(colour=guide_legend(title=catnm), 
             fill=guide_legend(title=catnm), 
             shape=guide_legend(title=catnm))+
      clean_background +
      labs(title = paste(gnm,':',x,Data.r.df[[x]][["stress"]]))
    print(plot_nmds)
    f.nm<-file.path(WDOut,paste('NMDS_Grp',gnm,'_',x,'.jpeg',sep=''))
    jpeg(f.nm, width = 600, height = 480, quality=300)
    print(plot_nmds)
    dev.off()
  }
})

cov.em<-cov.1%>%left_join(.,cov[!duplicated(cov$parentEventID),c('parentEventID',spa.c)])%>%
  select(-all_of(cat.c))%>%
  melt(.,id.vars=c('parentEventID','decimalLat','decimalLon'))%>%
  group_by(variable)%>%
  mutate(z=value/max(value))
f.nm<-file.path(WDOut,paste('Covar_',gnm,'.jpeg',sep=''))
g<-ggplot(aes(x=decimalLon,y=decimalLat,group=variable),data=cov.em)+
  geom_point(aes(size=z))+
  facet_wrap(vars(variable))+theme_test()
jpeg(f.nm, width = 800, height = 480, quality=300)
print(g)
dev.off()
f.nm<-file.path(WDOut,paste('Covar_hist',gnm,'.jpeg',sep=''))
g<-ggplot(aes(x=z,group=variable),data=cov.em)+
  geom_histogram()+
  facet_wrap(vars(variable))+theme_test()
jpeg(f.nm, width = 800, height = 480, quality=300)
print(g)
dev.off()

map(names(Data.ei.ord),function(x){
  NMDS.t<-Data.ei.ord[[x]][["NMDS"]]
  if(!is.null(NMDS.t)){
    evn<-Data.ei.ab[[x]]$parentEventID
    cov.rm<-cov.1%>%select(all_of("parentEventID"),all_of(v.msite))%>%filter(parentEventID%in%evn)%>%
      filter(!duplicated(parentEventID))%>%remove_rownames(.)%>%column_to_rownames(.,var="parentEventID")
    fit <- envfit(NMDS.t, cov.rm, perm = 999) 
    # extract p-values for each species
    fit_pvals <- fit$vectors$pvals %>% 
      as.data.frame() %>% 
      rownames_to_column("variable") %>% 
      dplyr::rename("pvals" = ".")
    
    # extract coordinates for species, only keep species with p-val = 0.001
    fit_var <- fit %>% 
      scores(., display = "vectors") %>% 
      as.data.frame() %>% 
      rownames_to_column("variable") %>% 
      full_join(., fit_pvals, by = "variable") 
    write.csv(fit_var,file.path(WDOut,paste(gnm,'_Inc_',catnm,'_VarFit_',x,'.csv',sep='')))
    return(fit_var)
  }else{
    return(NULL)
  }
})
#8) how micro-environment explain community structure...exploration of contraits
cov.ee.rm<-cov%>%select(all_of("parentEventID"),all_of(v.msite))%>%
  distinct(parentEventID,.keep_all=TRUE)%>%
  remove_rownames(.)%>%column_to_rownames(.,var="parentEventID")
#PCA on environmental variables #Presure variables
menv.pca<-rda(cov.ee.rm,scale=TRUE)
summary(menv.pca)
ev<-menv.pca$CA$eig
ev.f<-ev[ev>mean(ev)]

f.nm<-file.path(WDOut,paste('M_Env_PCA',gnm,'.jpeg',sep=''))
jpeg(f.nm, width = 600, height = 480, quality=300)
cleanplot.pca(menv.pca,scaling=1)
#cleanplot.pca(env.pca,scaling=2)
dev.off()

menv.w<-hclust(dist(scale(cov.ee.rm)))
gr<-cutree(menv.w,k=4)
grl<-levels(factor(gr))
sit.scl<-scores(menv.pca,display='wa',scaling=1)

f.nm<-file.path(WDOut,paste('M_Env_Clust',gnm,'.jpeg',sep=''))
jpeg(f.nm, width = 600, height = 480, quality=300)
p<-plot(menv.pca, display='wa',scaling=1,type='n',main="PCA correlación + clusters microhabitat")
abline(v=0, lty="dotted")
abline(h=0, lty="dotted")
for(i in 1: length(grl)){
  points(sit.scl[gr==i,],pch=(14+i),cex=2,col=i+1)
}
text(sit.scl,row.names(cov.ee.rm),cex=.7,pos=3)
ordicluster(p,menv.w,col="dark grey")
legend(x=1,y=0.5, paste("grupo",c(1:length(grl))),pch=14+c(1:length(grl)),col=1+c(1:length(grl)),
       pt.cex=1, y.intersp = 0.8,x.intersp=0.3,bty="n")
dev.off()

#mapa grupos micro habitat
cov.ee.rmm<-cov.ee.rm%>%rownames_to_column(.,'parentEventID')%>%mutate(pcaGr=gr)%>%
  inner_join(.,cov[,c('parentEventID','decimalLon','decimalLat')])%>%distinct(parentEventID,.keep_all=T)
plotOrd_grM(cov.ee.rmm,'microh')
kpv<-c(kpv,"gr")
rm(list=ls()[!ls()%in%kpv])