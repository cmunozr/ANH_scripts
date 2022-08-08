#Date.Created.Feb.23.2022

#Functions to run ANH_processingAbund.R 

PrintggiNext<-function(fnm,iNxt){
  f.nm<-file.path(WDOut,'CurvasDiversidad',paste(fnm,'_type1.jpeg',sep=''))
  SC<-iNxt$DataInfo%>%dplyr::select(-starts_with('f'))
  point.df<-iNxt$AsyEst
  if(nrow(SC)==1) {
    SC$site<-'A'
    point.df$Site<-'A'
  }
  point.df<-point.df%>%inner_join(.,SC,by=c('Site'='site'))
  jpeg(f.nm, width = 480, height = 480, quality=300)
  g1<-ggiNEXT(iNxt,type=1, facet.var = "order")+labs(title=paste('Diversidad Verdadera',fnm,sep=''), y='Diversidad extrapolada',x='Número de Individuos-bootstrap')+
    scale_linetype(labels=c("Extrapolado","Interpolado"))+facet_wrap(~order, nrow=1, labeller=as_labeller(c(`0` = "Riqueza", `1` = "Shannon",`2` = "Simpson")))+
    guides(linetype=guide_legend(title="Método"),
           colour=guide_legend(title="Grupo"),
           fill=guide_legend(title="Grupo"),
           shape=guide_legend(title="Grupo"))+ theme_classic()
  print(g1)
  dev.off()
  f.nm<-file.path(WDOut,'CurvasDiversidad',paste(fnm,'_type2.jpeg',sep=''))
  jpeg(f.nm, width = 480, height = 480, quality=300)
  g2<-ggiNEXT(iNxt,type=2)+labs(title=paste('Diversidad verdadera: ',fnm,sep=''), y='Cobertura de muestreo',x='Número de Individuos-bootstrap')+
    scale_linetype(labels=c("Extrapolado","Interpolado"))+
    guides(linetype=guide_legend(title="Método"),
           colour=guide_legend(title="Grupo"),
           fill=guide_legend(title="Grupo"),
           shape=guide_legend(title="Grupo"))+ theme_classic()
  print(g2)
  dev.off()
  f.nm<-file.path(WDOut,'CurvasDiversidad',paste(fnm,'_type3.jpeg',sep=''))
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
  write.csv(point.df,file.path(WDOut,'CurvasDiversidad',paste(fnm,'_Asym.csv',sep='')))
} #plots iNEXT three types of graphs
PrintRefiNext<-function(fnm,catnm,iNxt){
  point.df<-iNxt$AsyEst
  # Make a nice ggplot!
  f.nm<-file.path(WDOut,'CurvasDiversidad',paste(fnm,'_Asym.jpeg',sep=''))
  labsxx<-gsub('[[:punct:]]+',' ',as.character(point.df$Site))
  if(max(nchar(labsxx))>=15) {
    selr<-which(nchar(labsxx)>=15)
    labsxx[selr]<-gsub('\\b(\\pL{0,3})\\pL{2,}|.','\\1-',labsxx[selr],perl=TRUE)
    point.df$Site<-labsxx}
  g<-ggplot(point.df, aes(x=Site, y=Estimator)) + geom_bar(stat='identity')+
    geom_errorbar(aes(ymin=LCL, ymax=UCL), width=.01) +
    labs(y="Diversidad estimada", x = catnm, title=fnm) +
    facet_wrap(~Diversity, scale='free',nrow=3, labeller=as_labeller(c(`Species richness` = "Riqueza", 
                                                                       `Shannon diversity` = "Shannon",
                                                                       `Simpson diversity` = "Simpson")))+
    theme_classic()+
    theme(axis.text.x=element_text(angle=90,hjust=1))
  jpeg(f.nm, width = 480, height = 480, quality=300)
  print(g)
  dev.off()
  print(g)
} #Plots asymptotic estimations by category
PrintggiNextInc<-function(fnm,iNxt){
  f.nm<-file.path(WDOut,'CurvasDiversidad',paste(fnm,'_type1_Inc.jpeg',sep=''))
  SC<-iNxt$DataInfo%>%dplyr::select(-starts_with('f'))
  point.df<-iNxt$AsyEst
  if(nrow(SC)==1) {
    SC$site<-'A'
    point.df$Site<-'A'
  }
  point.df<-point.df%>%inner_join(.,SC,by=c('Site'='site'))
  names(point.df)<-gsub('[S|s]ite','site',names(point.df))
  jpeg(f.nm, width = 480, height = 480, quality=300)
  #fix problem with different columns from fortify.ggiNext
    fixNm<-lapply(iNxt$iNextEst,names)
    intNm<-do.call(c,fixNm)
    tblnm<-table(intNm)
    selclm<-names(tblnm)[tblnm==length(fixNm)]
    iNxt$iNextEst<-lapply(iNxt$iNextEst,function(x){x<-x[,selclm]})
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
  f.nm<-file.path(WDOut,'CurvasDiversidad',paste(fnm,'_type2_Inc.jpeg',sep=''))
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
  f.nm<-file.path(WDOut,'CurvasDiversidad',paste(fnm,'_type3_Inc.jpeg',sep=''))
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
  write.csv(point.df,file.path(WDOut,'CurvasDiversidad', paste(fnm,'Inc_Asym.csv',sep='')))
} #plots iNEXT three types of graphs with Incidence data
plotMU_cat<-function(x,point.df,catnm,feven,sxnm) { 
  names(point.df)[names(point.df)==catnm]<-'categ'
  names(point.df)[names(point.df)==feven]<-'fevenID'
  point.df$categ<-as.factor(point.df$categ)
  point.df<-point.df%>%arrange(categ)
  point.df$labsxx<-point.df$fevenID
  if(max(nchar(as.character(point.df$fevenID)))>10){
    point.df$labsxx<-as.factor(gsub('ANH_','',point.df$fevenID))
  }
  f.nm<-file.path(WDOut,'CurvasDiversidad',paste(gnm,'_',x,sxnm,'.jpeg',sep=''))
  g<-ggplot(point.df, aes(x=fct_inorder(labsxx), y=estimado, color=categ)) + geom_point()+
    geom_errorbar(aes(ymin=LCL, ymax=UCL), width=.01) +
    labs(y="Diversidad estimada", x = "Unidad de muestreo") +
    facet_wrap(~order, nrow=3, labeller=as_labeller(c(`Species richness` = "Riqueza", 
                                                      `Shannon diversity` = "Shannon",
                                                      `Simpson diversity` = "Simpson")))+
    labs(tag=paste('Estimación asintótica para ',x))+
    theme_classic()+
    coord_cartesian(clip = "off") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          plot.tag=element_text(size=8))+
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
  f.nm<-file.path(WDOut,'CurvasDiversidad',paste(gnm,'_',x,sxnm,'.jpeg',sep=''))
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
        inner_join(.,site_Cat)%>%dplyr::select(parentEventID,gr)%>%
        mutate(categ=as.factor(gr))%>%dplyr::select(parentEventID,categ)
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
  f.nm<-file.path(WDOut,'Covariables_PCA',paste('MapaGrPCA_',gnm,'_',plnm,'.jpeg',sep=''))
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
        f.nm<-file.path(WDOut,'NMDS',paste(gnm,'NMDS_Inc_sp_E_',plnm,'_',x,'.jpeg',sep=''))
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
      plot_rda <- ggplot(site_data,aes(x = RDA1, y = RDA2)) +
        geom_point(aes(color=categ,shape="UM"),size = 2, alpha = 0.8)+
        scale_color_manual(values = pal[c(1:nlvl)]) +
        geom_segment(data = rdavectors, aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
                     arrow = arrow(length = unit(0.2, "cm")),color="black") +
        geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
        geom_hline(yintercept = c(0), color = "grey70", linetype = 2) + #arrow = arrow(length = unit(0.2, "cm")),
        geom_point(data = species_data, aes(x=RDA1,y=RDA2, shape="Especies"), 
                   color = "olivedrab4",size=1) +
        geom_text_repel(data = species_lab, aes(x=RDA1,y=RDA2,label=rownames(species_lab)), 
                        color = "olivedrab4") +
        geom_text_repel(data = rdavectors, 
                        aes(x = RDA1, y = RDA2,label = rownames(rdavectors))) +
        scale_shape_manual(name='Símbolo',breaks=c('UM','Especies'),
                           values=c('UM'=19,'Especies'=17))+
        guides(shape = guide_legend(override.aes = list(pch = c(19,17), 
                                                        color = c("gray", "olivedrab4"))))+
        
        clean_background +
        labs(title = paste('RDA_',gnm,':',x,'_',plnm,' Var Exp=',vexp))
      print(plot_rda)
      f.nm<-file.path(WDOut,'RDA',paste('RDA_',gnm,'_',x,'_',plnm,'.jpeg',sep=''))
      jpeg(f.nm, width = 600, height = 480, quality=300)
      print(plot_rda)
      dev.off()
      return(plot_rda)}
    else{
      NULL
    }
  })
}
complete_cols<- function(BD_registros, BD_eventos, link, vector_cols){
  
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
Data.a.f<-function(catnm,fn="sum",scale=F){
  cov.1<-cov
  names(cov.1)[names(cov.1)==catnm]<-'categ'
  Data.r2<-Data.r%>%filter((!parentEventID%in%ompv)&(!eventID%in%ompv)&(!samplingProtocol%in%ommt))
  nsp<-unique(Data.r2$parentEventID)
  cov.1<-cov.1%>%dplyr::select(eventID,parentEventID,categ)%>%
    distinct(eventID,.keep_all=T)%>%
    filter(parentEventID%in%nsp)
  print(paste('length cov =',nrow(cov.1)))
  nsp<-length(nsp[!nsp%in%ompv])
  print(paste('length nsp =',nsp))
  
  if(fn=="sum"){
    Data.ee.o<-Data.r2%>%
      dplyr::select(eventID,parentEventID, samplingProtocol,organismQuantity,scientificName_2)%>%
      inner_join(.,cov.1, by="eventID")%>%dplyr::select(-eventID)%>%
      pivot_wider(names_from=parentEventID.x,values_from=organismQuantity,
                  values_fn=sum,values_fill=0,id_cols=c(scientificName_2,
                                                        categ,samplingProtocol))
    Data.ee.o <- Data.ee.o %>% 
      mutate(TotAbu=rowSums(.[,4:(length(Data.ee.o))]),.keep="unused")%>%
      dplyr::select(categ,samplingProtocol,scientificName_2,TotAbu)
  }else{
    Data.ee.o<-Data.r2%>%
      dplyr::select(eventID,samplingProtocol,organismQuantity,scientificName_2)%>%
      inner_join(.,cov.1, by="eventID")%>%dplyr::select(-eventID)%>%
      pivot_wider(names_from=parentEventID,values_from=organismQuantity,
                  values_fn=max,values_fill=0,id_cols=c(scientificName_2,
                                                        categ,samplingProtocol))
    Data.ee.o <- Data.ee.o %>%
      mutate(TotAbu=rowSums(.[,4:(length(Data.ee.o))]),.keep="unused")%>%
      dplyr::select(categ,samplingProtocol,scientificName_2,TotAbu)
  }
  if(scale==TRUE){
    Data.ee.o<-Data.ee.o%>%mutate_if(is.numeric,~ceiling(.x/min(.x[.x>0],na.rm=T)))%>%group_split(samplingProtocol)
  }else{
    Data.ee.o<-Data.ee.o%>%group_split(samplingProtocol)
  }
  names(Data.ee.o)<-levels(as.factor(Data.r2$samplingProtocol))
  Data.ee.oo<-map(Data.ee.o, function(x){
    y<-x%>%dplyr::select(categ,scientificName_2,TotAbu)%>%
      pivot_wider(names_from=categ,values_from=TotAbu,values_fill=0)%>%
      column_to_rownames(.,var="scientificName_2")
  })
  return(Data.ee.oo)
} #getslist by category with abundance
Data.i.f<-function(catnm){
  cov.1<-cov
  names(cov.1)[names(cov.1)==catnm]<-'categ'
  Data.r2<-Data.r%>%filter((!parentEventID%in%ompv)&(!eventID%in%ompv)&(!samplingProtocol%in%ommt))
  nsp<-unique(Data.r2$parentEventID)
  cov.1<-cov.1%>%dplyr::select(eventID,parentEventID,categ)%>%
    distinct(eventID,.keep_all=T)%>%
    filter(parentEventID%in%nsp)
  nsp<-length(nsp[!nsp%in%ompv])
  Data.ei.o<-Data.r2%>%
    dplyr::select(eventID,organismQuantity,scientificName_2)%>%
    inner_join(.,cov.1, by="eventID")%>%
    dplyr::select(-eventID)%>%
    mutate(categ=as.factor(categ))%>%
    pivot_wider(names_from=parentEventID,values_from=organismQuantity, 
                values_fn=sum,values_fill=0)%>%
    mutate_if(is.numeric,~1*(.>0))
  nmlvl<-levels(as.factor(as.character(Data.ei.o$categ)))
  print(nmlvl)
  Data.ei.o<-Data.ei.o%>% filter(!is.na(categ)) %>% 
    group_split(categ)
  names(Data.ei.o)<-nmlvl
  Data.ei.oo<-map(Data.ei.o, function(x){
    y<-x%>%dplyr::select(-categ)%>%
      column_to_rownames(.,var="scientificName_2")%>%
      dplyr::select(-which(colSums(.)==0))
  })
  return(Data.ei.oo)
} #gets list by category with incidence data

Data.a.MU<-function(DataP,evID,expPEID="^(ANH_[0-9]+)(_.*)$",fn="sum",scale=FALSE){
  cov.1<-cov%>%distinct(parentEventID,.keep_all=T)%>%
    dplyr::select('parentEventID',all_of(v.rec),all_of(v.pres),all_of(v.msite),all_of(cat.c))
  Data.r2<-DataP%>%filter((!parentEventID%in%ompv)&(!eventID%in%ompv)&(!samplingProtocol%in%ommt))
  names(Data.r2)[names(Data.r2)==evID]<-'evID'
  if(fn=="sum"){
    Data.rr.n<-Data.r2%>%
      dplyr::select(evID,organismQuantity,samplingProtocol,scientificName_2)%>%
      pivot_wider(names_from=evID,values_from=organismQuantity, values_fn=sum,values_fill=0)
  }else{
    Data.rr.n<-Data.r2%>%
      dplyr::select(evID,organismQuantity,samplingProtocol,scientificName_2)%>%
      pivot_wider(names_from=evID,values_from=organismQuantity, values_fn=max,values_fill=0)
  }
  if(scale==TRUE){
    Data.rr.n<-Data.rr.n%>%mutate_if(is.numeric,~ceiling(.x/min(.x[.x>0],na.rm=T)))%>%group_split(samplingProtocol)
  }else{
    Data.rr.n<-Data.rr.n%>%group_split(samplingProtocol)
  }
  names(Data.rr.n)<-levels(factor(Data.r2$samplingProtocol, exclude = NA))
  Data.ee.nn<-map(names(Data.rr.n), function(x) {
    xx<-Data.rr.n[[x]][,-1]%>%column_to_rownames("scientificName_2")%>%dplyr::select(-which(colSums(.)==0))
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

Data.a.pt<-function(grpp,Data.r2,evID,fn="sum",scale=FALSE){
  names(Data.r2)[names(Data.r2)==evID]<-'evID'
  yyy<-map(names(grpp), function(x) {
    y<-grpp[[x]]
    y.nm<-length(y)
    xx<-Data.r2%>%filter(samplingProtocol%in%y)%>%
      dplyr::select(parentEventID,samplingProtocol,organismQuantity)%>%
      pivot_wider(names_from=samplingProtocol, values_from=organismQuantity,
                  values_fn=sum,values_fill=0)%>%
      mutate_if(is.numeric,~1*(.>0))%>%
      mutate('t'=rowSums(dplyr::select(.,where(is.numeric))))%>%
      filter(t==y.nm)
    xxx<-Data.r2%>%
      dplyr::select(evID,parentEventID,organismQuantity,samplingProtocol,scientificName_2)%>%
      filter(parentEventID%in%xx$parentEventID&samplingProtocol%in%y)%>%
      group_split(parentEventID)
    names(xxx)<-lapply(xxx,function(x) unique(x$parentEventID))
    yy<-map(xxx,function(w) {
      if(fn=="sum"){
        v<-w%>%dplyr::select(-c(parentEventID,samplingProtocol))%>%
          pivot_wider(names_from=evID,
                      values_from=organismQuantity,
                      values_fn=sum,values_fill=0)
      }else{
        v<-w%>%dplyr::select(-c(parentEventID,samplingProtocol))%>%
          pivot_wider(names_from=evID,
                      values_from=organismQuantity,
                      values_fn=max,values_fill=0)
      }
      if(scale==TRUE){
        v<-v%>%mutate_if(is.numeric,~ceiling(.x/min(.x[.x>0], na.rm=T)))
      }
      return(v)
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
        filter(grp==z)%>%dplyr::select(ends_with(zz))%>%
        transmute('t'=rowSums(.))%>%unlist(.)
      Data.ee.sss$cmb_smpEf[Data.ee.sss$grp==z]<-zzz
    }
  }
  Data.ee.sss<-Data.ee.sss%>%dplyr::select(-ends_with(names(samEff.ttt)))
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
    ttt$cmb_smpEf<-rowSums(ttt[,selc])
  }else{
    ttt$cmb_smpEf<-ttt[,selc]
  }
  if(length(grpp)>1){
    sselc<-c(2:length(grpp))
    for(z in names(tt)[sselc]){
      zz<-grpp[[z]]
      zzz<-ttt%>%
        filter(grp==z)%>%dplyr::select(ends_with(zz))%>%
        transmute('t'=rowSums(.))%>%unlist(.)
      ttt$cmb_smpEf[ttt$grp==z]<-zzz
    }
  }
  ttt<-ttt%>%dplyr::select(-ends_with(names(samEf)))
} #gets final df with sampling effort

lm_eqn <- function(m){
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = sprintf(unname(coef(m)[1]), fmt="%.2f"),
                        b = sprintf(unname(coef(m)[2]), fmt="%.2f"),
                        r2 = sprintf(unname(summary(m)$r.squared), fmt="%.3f")))
  as.character(as.expression(eq))
}
plotCvar<-function(point.df,v,prfnm,x){
  if(nrow(point.df)>3){
    mp<-NULL
    eqex<-data.frame('order'=character(),'eqex'=character(),'x'=numeric(),'y'=numeric())
    for(i in levels(point.df$order)){
      point.df2<-point.df[point.df$order==i,]
      wtdlm <- lm(estimado ~ get(v), data =point.df2 , weights = samplEff.1)
      mpp <- as.data.frame(cbind(v = point.df2[,v],
                                 predict(wtdlm, interval = 'confidence')))
      mpp$order<-i
      eqex1<-data.frame('order'=i,'eqex'=lm_eqn(wtdlm),
                        'x'=0.8*max(mpp$v),'y'=0.9*max(point.df2$estimado))
      mp<-rbind(mp,mpp)
      eqex<-rbind(eqex,eqex1)
    }
    g<-ggplot(aes(y=estimado, x=get(v),colour=order), data=point.df)+
      geom_point()+
      geom_line(data = mp, aes(x = v, y = fit), size = 1, color = 'blue') +
      geom_line(data = mp, aes(x = v, y = lwr), color = 'gray80') +
      geom_line(data = mp, aes(x = v, y = upr), color = 'gray80') +
      geom_ribbon(data = mp, aes(x = v, y=fit, ymin = lwr, ymax = upr), 
                  alpha = 0.1)+
      geom_text(data=eqex,aes(x = x, y = y,label=eqex, color=order), parse = TRUE,
                inherit.aes=FALSE)+
      facet_wrap(~order, nrow=3, labeller=as_labeller(c(`Species richness` = "Riqueza", 
                                                        `Shannon diversity` = "Shannon",
                                                        `Simpson diversity` = "Simpson")),
                 scales="free")+
      theme_bw()+
      theme(legend.position = 'none')+
      labs(title=paste('Diversidad por UM para',x),
           y='Indice',x=v)
    print(g)
    f.nm<-file.path(WDOut,'Covariables_PCA',paste(prfnm,'.jpeg',sep=''))
    jpeg(f.nm, width = 800, height = 480, quality=300)
    print(g)
    dev.off()
  }
  else{
    print('not enough points')
  }} #plots diversity estimates against continuos variables
getMNDS_abu<-function(Data.ab,vcat,evenID){
  Data.ab2<-map(names(Data.ab),function(xx){
    print(xx)
    x<-Data.ab[[xx]]
    if(nrow(x)>1){
      y<-x[,-2]%>%column_to_rownames(.,var=evenID)%>%dplyr::select(-which(colSums(.)==0))
      colnames(y)<-gsub("^([[:alpha:]]{4}).*([[:space:]])([[:alpha:]]{4}).*$","\\1_\\3",colnames(y))
      cov1<-cov
      names(cov1)[names(cov1)==vcat]<-"categ"
      if(evenID=='parentEventID'){
        site_Cat<-cov1%>%dplyr::select(parentEventID,categ)%>%
          filter(parentEventID%in%rownames(y))%>%
          filter(!duplicated(parentEventID))%>%
          arrange(match(parentEventID,rownames(y)))
      }else{
        peID<-gsub('(^ANH_[[:digit:]]+)(_.*$)','\\1',rownames(y))
        site_Cat<-cov1%>%filter(parentEventID%in%peID)%>%
          dplyr::select(parentEventID,categ)
        site_Cat<-site_Cat[match(peID,site_Cat$parentEventID),]
        site_Cat[,evenID]<-rownames(y)
        site_Cat[,'Per']<-as.factor(gsub('(^ANH_[[:digit:]]+)_(.*$)','\\2',rownames(y)))
      }
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
getNMDS_DF<-function(Data.ord,evenID){
  Data.ord2<-map(names(Data.ord),function(xx){
    NMDS.t<-Data.ord[[xx]][["NMDS"]]
    if(!is.null(NMDS.t)){
      site_Cat<-Data.ord[[xx]][["site_cat"]]
      plot_df <- scores(NMDS.t, display = "sites") %>% 
        as.data.frame() %>% 
        rownames_to_column("site") %>% 
        inner_join(.,site_Cat, by = c("site"=evenID))
      varexp<-paste("stress: ", round(NMDS.t$stress,3),sep="")
      list("plot_df"=plot_df,"stress"=varexp)}
    else{
      list("plot_df"=NULL,"stress"=NULL)
    }
    
  })
  return(Data.ord2)
} #gets the info to plot NMDS
PlotNMDS<-function(Data.df,vcat,sfx,pcat,lab=FALSE){
  map(names(Data.df), function(x){
    xx<-Data.df[[x]][["plot_df"]]
    names(xx)[names(xx)%in%c('site','variable')]<-'site'
    if(!is.null(xx)){
      nlvl<-nlevels(xx$categ)
      names(xx)[which(names(xx)==pcat)]<-'pcat'
      plot_nmds <- ggplot(xx, aes(x = NMDS1, y = NMDS2, color = categ, shape = pcat)) +
        geom_point(size = 3, alpha = 0.8) +
        scale_color_manual(values = pal[1:nlvl]) +
        stat_ellipse(aes(x = NMDS1, y = NMDS2,color=categ),linetype = 2, size = 1,inherit.aes = F) +
        guides(colour=guide_legend(title=vcat), 
               fill=guide_legend(title=vcat), 
               shape=guide_legend(title=pcat))+
        clean_background +
        labs(title = paste(gnm,':',x,Data.df[[x]][["stress"]]))
      if(pcat=='Per'){
        plot_nmds<-plot_nmds+
          geom_text_repel(data = xx, aes(label = gsub('[[:alpha:]]|[[:punct:]]','',parentEventID)))
      }
      if(lab==TRUE){
        plot_nmds<-plot_nmds+
          geom_text_repel(data = xx, aes(label = gsub('[[:alpha:]]|[[:punct:]]','',site)))
      }
      print(plot_nmds)
      fcat<-vcat
      if(vcat!=pcat){fcat<paste(vcat,pcat,sep='_')}
      f.nm<-file.path(WDOut,'NMDS', paste('NMDS_',sfx,'_',gnm,'_',fcat,'_',x,'.jpeg',sep=''))
      jpeg(f.nm, width = 600, height = 480, quality=300)
      print(plot_nmds)
      dev.off()
    }
  })
  print("success")
} #plots NMDS by category
NMDS_Sp<-function(Data.ord,Data.ab,sfx,evenID){
  map(names(Data.ord),function(x){
    NMDS.t<-Data.ord[[x]][["NMDS"]]
    if(!is.null(NMDS.t)){
      Abu<-Data.ab[[x]][,-2]%>%column_to_rownames(.,var=evenID)
      colnames(Abu)<-gsub("^([[:alpha:]]{4}).*([[:space:]])([[:alpha:]]{4}).*$","\\1_\\3",
                          colnames(Abu))
      Abu<-Abu%>%dplyr::select(-which(colSums(Abu)==0))
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
      write.csv(fit_spp,file.path(WDOut,'NMDS',paste(gnm,'_SpFit_',sfx,'_',x,'.csv',sep='')))
      return(fit_spp)
    }else{
      return(NULL)
    }
  })
}
plotNMDS_sp<-function(Data.sp,Data.df,vcat,sfx,pcat,lab=FALSE){
  map(names(Data.df),function(x){
    plot_df<-Data.df[[x]][["plot_df"]]
    names(plot_df)[names(plot_df)%in%c('site','variable')]<-'site'
    if(!is.null(plot_df)){
      nlvl<-nlevels(plot_df$categ)
      fit_spp<-Data.sp[[x]]
      names(plot_df)[which(names(plot_df)==pcat)]<-'pcat'
      if (nrow(fit_spp)>0){
        # new plot
        nmds_plot_new <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2)) +
          coord_fixed() +
          geom_point(aes(color = categ, shape = pcat), size = 3, alpha = 0.8) +
          stat_ellipse(aes(color = categ)) +
          scale_color_manual(values = pal[1:nlvl]) +
          geom_segment(data = fit_spp, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
                       arrow = arrow(length = unit(0.25, "cm")),
                       col = "black") +
          guides(colour=guide_legend(title=vcat),
                 shape=guide_legend(title=pcat))+
          geom_text_repel(data = fit_spp, aes(label = variable)) +
          clean_background+labs(title = paste(gnm,':',x,vcat,' p-value=0.05'))
        if(pcat=='Per'){
          nmds_plot_new<-nmds_plot_new+
            geom_text_repel(data = plot_df, aes(label = gsub('[[:alpha:]]|[[:punct:]]','',parentEventID),color=categ))
        }
        if(lab==TRUE){
          nmds_plot_new<-nmds_plot_new+
            geom_text_repel(data = plot_df, aes(label = gsub('[[:alpha:]]|[[:punct:]]','',site)))
        }
        print(nmds_plot_new)
        fcat<-vcat
        if(vcat!=pcat){fcat<paste(vcat,pcat,sep='_')}
        f.nm<-file.path(WDOut,'NMDS',paste('NMDS_sp_',sfx,'_',gnm,'_',x,'_',fcat,'.jpeg',sep=''))
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
        inner_join(.,site_Cat)%>%dplyr::select(parentEventID,grr)%>%
        mutate(categ=as.factor(grr))%>%dplyr::select(parentEventID,categ)
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
}# joins group information to the dataframe of the ordination
NMDS_env<-function(Data.ord,Data.ab, v.selected,sufx=NULL){
  ord_env<-map(names(Data.ord),function(x){
    NMDS.t<-Data.ord[[x]][["NMDS"]]
    if(!is.null(NMDS.t)){
      evn<-Data.ab[[x]]$parentEventID
      cov.r<-cov%>%dplyr::select(all_of("parentEventID"),all_of(v.selected))%>%
        filter(parentEventID%in%evn)%>%
        filter(!duplicated(parentEventID))%>%remove_rownames(.)%>%
        column_to_rownames(.,var="parentEventID")
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
      write.csv(fit_var,file.path(WDOut,'NMDS',paste(gnm,'_Env_VarFit_',x,'_',sufx,'.csv',sep='')))
      return(fit_var)
    }else{
      return(NULL)
    }
  })
  return(ord_env)
}
getNMDS_i<-function(Data.ab,vcat,evenID){
  map(names(Data.ab),function(xx){
    x<-Data.ab[[xx]]
    if(nrow(x)>1){
      y<-x%>%column_to_rownames(.,var=evenID)%>%dplyr::select(-which(colSums(.)==0))
      colnames(y)<-gsub("^([[:alpha:]]{4}).*([[:space:]])([[:alpha:]]{4}).*$","\\1_\\3",colnames(y))
      
      cov1<-cov%>%dplyr::select(parentEventID,vcat)%>%
        distinct(parentEventID,.keep_all=T)
      names(cov1)[names(cov1)==vcat]<-"categ"
      if(evenID=='parentEventID'){
        site_Cat<-cov1%>%dplyr::select(parentEventID,categ)%>%
          filter(parentEventID%in%rownames(y))%>%
          filter(!duplicated(parentEventID))%>%
          arrange(match(parentEventID,rownames(y)))
      }else{
        peID<-gsub('(^ANH_[[:digit:]]+)(_.*$)','\\1',rownames(y))
        site_Cat<-cov1%>%filter(parentEventID%in%peID)%>%
          dplyr::select(parentEventID,categ)
        site_Cat<-site_Cat[match(peID,site_Cat$parentEventID),]
        site_Cat[,evenID]<-rownames(y)
        site_Cat[,'Per']<-as.factor(gsub('(^ANH_[[:digit:]]+)_(.*$)','\\2',rownames(y)))
      }
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
getNMDS_i_DF<-function(Data.ordi,evenID){
  map(names(Data.ordi),function(xx){
    NMDS.t<-Data.ordi[[xx]][["NMDS"]]
    if(!is.null(NMDS.t)){
      site_Cat<-Data.ordi[[xx]][["site_cat"]]
      plot_df <- scores(NMDS.t, display = "sites") %>% 
        as.data.frame() %>% 
        rownames_to_column("variable") %>% 
        inner_join(.,site_Cat, by = c("variable"=evenID))
      varexp<-paste("stress: ", round(NMDS.t$stress,3),sep="")
      list("plot_df"=plot_df,"stress"=varexp)}
    else{
      list("plot_df"=NULL,"stress"=NULL)
    }
  })
}
Data.a.r<-function(cnm,grnm,fn,scale=FALSE){
  cov.1<-cov
  names(cov.1)[names(cov.1)==cnm]<-'categ'
  Data.r2<-Data.r%>%filter((!parentEventID%in%ompv)&(!eventID%in%ompv)&(!samplingProtocol%in%ommt))
  nsp<-unique(Data.r2$parentEventID)
  cov.1<-cov.1%>%dplyr::select(parentEventID,categ)%>%distinct(parentEventID,.keep_all=T)%>%
    filter(parentEventID%in%nsp)
  nsp<-length(nsp[!nsp%in%ompv])
  if(fn=="sum"){
    Data.ee.o<-Data.r2%>%
      dplyr::select(parentEventID,samplingProtocol,organismQuantity,scientificName_2)%>%
      inner_join(.,cov.1, by="parentEventID")%>%
      dplyr::select(samplingProtocol,categ,organismQuantity,scientificName_2)%>%
      pivot_wider(names_from=scientificName_2,values_from=organismQuantity, 
                  values_fn=sum,values_fill=0)
  }else{
    Data.ee.o<-Data.r2%>%
      dplyr::select(parentEventID,samplingProtocol,organismQuantity,scientificName_2)%>%
      inner_join(.,cov.1, by="parentEventID")%>%
      dplyr::select(samplingProtocol,categ,organismQuantity,scientificName_2)%>%
      pivot_wider(names_from=scientificName_2,values_from=organismQuantity, 
                  values_fn=max,values_fill=0)
  }
  if(scale==TRUE){
    selc<-sapply(Data.ee.o,is.numeric)
    Data.ee.o2<-as.data.frame(t(as.matrix(Data.ee.o[,selc])))%>%mutate_if(is.numeric,~ceiling(.x/min(.x[.x>0], na.rm=T)))
    Data.ee.o3<-Data.ee.o
    Data.ee.o3[,selc]<-t(Data.ee.o2)
    Data.ee.o<-Data.ee.o3%>%group_split(samplingProtocol)
  }else{
    Data.ee.o<-Data.ee.o%>%group_split(samplingProtocol)
  }
  names(Data.ee.o)<-levels(as.factor(Data.r2$samplingProtocol))
  Data.ee.oo<-map(Data.ee.o, function(v){
    y<-v%>%dplyr::select(categ)%>%mutate(categ=droplevels(as.factor(categ)))%>%as.data.frame(.)
    x<-v%>%dplyr::select(-c(samplingProtocol,categ))%>%as.data.frame(.)
    RA.data<-rankabuncomp(x, y, factor='categ', 
                          return.data=TRUE, legend=FALSE)
    return(RA.data)
  })
  names(Data.ee.oo)<-levels(as.factor(Data.r2$samplingProtocol))
  Data.ee.o4<-Data.ee.oo%>%do.call(rbind,.)%>%
    rownames_to_column(var="samplingProtocol")%>%
    mutate(samplingProtocol=gsub('[[:punct:]]+[[:digit:]]+$','',samplingProtocol),.keep='all')
  write.csv(Data.ee.o4,file.path(WDOut,'CurvasRank',paste(grnm,'_RankAbu_',cnm,'.csv',sep='')))
  return(Data.ee.oo)
} #getslist by category with abundance
Data.a.rt<-function(Data.rr,cnm1,cnm2,grnm,fn,scale=FALSE){
  cov.1<-cov
  Data.r2<-Data.rr%>%filter((!parentEventID%in%ompv)&(!eventID%in%ompv)&(!samplingProtocol%in%ommt))
  nsp<-unique(Data.r2$parentEventID)
  cov.1<-cov.1%>%distinct(parentEventID,.keep_all=T)%>%
    filter(parentEventID%in%nsp)
  nsp<-length(nsp[!nsp%in%ompv])
  if(fn=="sum"){
    Data.ee.o<-Data.r2%>%
      inner_join(.,cov.1, by=c("parentEventID"))%>%mutate(categ=paste(get(cnm1),get(cnm2),sep='_'))%>%
      dplyr::select(samplingProtocol,categ,organismQuantity,scientificName_2)%>%
      pivot_wider(names_from=scientificName_2,values_from=organismQuantity, 
                  values_fn=sum,values_fill=0)
  }else{
    Data.ee.o<-Data.r2%>%
      inner_join(.,cov.1, by="parentEventID")%>%mutate(categ=paste(get(cnm1),get(cnm2),sep='_'))%>%
      dplyr::select(samplingProtocol,categ,organismQuantity,scientificName_2)%>%
      pivot_wider(names_from=scientificName_2,values_from=organismQuantity, 
                  values_fn=max,values_fill=0)
  }
  if(scale==TRUE){
    print(head(Data.ee.o))
    selc<-sapply(Data.ee.o,is.numeric)
    Data.ee.o2<-as.matrix(Data.ee.o[,selc])
    Data.ee.o4<-ceiling(Data.ee.o2/apply(Data.ee.o2,1,function(x){y<-min(x[x>0])}))
    Data.ee.o[,selc]<-Data.ee.o4
    print(head(Data.ee.o))
    Data.ee.o<-Data.ee.o%>%group_split(samplingProtocol)
  }else{
    Data.ee.o<-Data.ee.o%>%group_split(samplingProtocol)
  }
  names(Data.ee.o)<-levels(as.factor(Data.r2$samplingProtocol))
  Data.ee.oo<-map(Data.ee.o, function(v){
    y<-v%>%dplyr::select(categ)%>%mutate(categ=droplevels(as.factor(categ)))%>%as.data.frame(.)
    x<-v%>%dplyr::select(-c(samplingProtocol,categ))%>%as.data.frame(.)
    RA.data<-rankabuncomp(x, y, factor='categ', 
                          return.data=TRUE, legend=FALSE)
    return(RA.data)
  })
  names(Data.ee.oo)<-levels(as.factor(Data.r2$samplingProtocol))
  Data.ee.o3<-Data.ee.oo%>%do.call(rbind,.)%>%
    rownames_to_column(var="samplingProtocol")%>%
    mutate(samplingProtocol=gsub('[[:punct:]]+[[:digit:]]+$','',samplingProtocol),.keep='all')
  write.csv(Data.ee.o3,file.path(WDOut,'CurvasRank',paste(grnm,'_RankAbu_',cnm1,cnm2,'.csv',sep='')))
  return(Data.ee.oo)
} #getslist by category with abundance
plotRnkAb<-function(RA.data,grnm,cnm,x){
  BioR.theme <- theme(
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line("gray25"),
    text = element_text(size = 12, family="Arial"),
    axis.text = element_text(size = 10, colour = "gray25"),
    axis.title = element_text(size = 14, colour = "gray25"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.key = element_blank())
  RA.data$nms<-gsub("^([[:alpha:]]{4}).*([[:space:]])([[:alpha:]]{4}).*$","\\1_\\3",RA.data$species)
  nlev<-length(unique(RA.data$Grouping))
  plotgg2 <- ggplot(data=RA.data, aes(x = rank, y = logabun)) + 
    scale_x_continuous(expand=c(0, 1), sec.axis = dup_axis(labels=NULL, name=NULL)) +
    scale_y_continuous(expand=c(0, 1), sec.axis = dup_axis(labels=NULL, name=NULL)) +
    geom_line (size=1) +
    geom_point(size=3, alpha=0.7) +
    geom_text_repel(data=subset(RA.data, labelit == TRUE), 
                    aes(label=nms), 
                    nudge_x=1, nudge_y=1, show.legend=FALSE) +
    BioR.theme +
    facet_wrap(~ Grouping) +
    labs(x = "rank", y = "Log10_Abundancia", title = paste(cnm,x,sep=''))
  f.nm<-file.path(WDOut,'CurvasRank',paste('RnkAbuC_',grnm,'_',cnm,'_',x,'.jpeg',sep=''))
  jpeg(f.nm, width = 600, height = 480, quality=300)
  print(plotgg2)
  dev.off()
  print(plotgg2)
}
CompletEmpty<-function(Data.f,evenID,empMU,smplg){
  if(length(empMU)==0) return(Data.f)
  Data.e.x<-Data.et
  names(Data.e.x)[names(Data.e.x)==evenID]<-'evenID'
  Data.e.x<-Data.e.x%>%filter(evenID%in%empMU&samplingProtocol==smplg)%>%
    dplyr::select(evenID,samplingProtocol)
  if(nrow(Data.e.x)==0) return(Data.f)
  empMU<-Data.e.x$evenID
  cov.1<-cov
  names(cov.1)[names(cov.1)==evenID]<-'evenID'
  cov.1<-cov.1%>%distinct(evenID,.keep_all=T)%>%
    dplyr::select(evenID,all_of(v.rec),all_of(v.pres),all_of(v.msite),all_of(cat.c))%>%
    filter(evenID%in%empMU)
  names(Data.f)[names(Data.f)==evenID]<-'evenID'
  nmcol<-setdiff(names(Data.f),names(cov.1))
  Data.ff<-data.frame(matrix(data=0,nrow=length(empMU)*3,ncol=length(nmcol)))
  names(Data.ff)<-nmcol
  Data.ff$order<-as.factor(rep(levels(Data.f$order),length(empMU)))
  Data.ff$evenID<-rep(empMU,each=3)
  if(evenID=='eventID') Data.ff$parentEventID<-rep(gsub(pattern = "^(ANH_[0-9]+)(_.*)$", 
                                                        replacement = "\\1",empMU), each=3)
  Data.ff<-Data.ff%>%inner_join(.,cov.1,by='evenID')
  Data.f<-rbind(Data.f,Data.ff)
  names(Data.f)[names(Data.f)=='evenID']<-evenID
  return(Data.f)
}
find_slots <- function(a, b){
  slots = seq(a-minute(a)*60-second(a),
              b-minute(b)*60-second(b),
              "hour")
  
  dateseq = slots
  dateseq[1] = a
  r = c(dateseq, b)
  
  d = as.numeric(difftime(r[-1], r[-length(r)], unit = 'min'))
  
  data.frame(slot = slots, Q = d)
}

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
  
  return(tosave)
}

modify_event_label <- function(data, column = "eventID", n){
  require(stringr)
  original <- data[,column] 
  splitted <- original%>% str_split(., "_")
  index_vect <- lapply(X= splitted, FUN = function(X){length(X)>=n}) %>% unlist()
  
  if(sum(index_vect) != 0 ){
    print("modifying")
    modified <- lapply(X = splitted[index_vect], FUN = function(X){X[1:4]} %>% paste(., collapse = "_")) %>% unlist()
    original[index_vect] <- modified
    data[,column] <- original  
  }else{
    print("no changes")
  }
  
  return(data)
}
