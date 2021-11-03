# Graphical functions

# plots iNEXT three types of graphs

PrintggiNext <- function(fnm,iNxt){
  
  f.nm <- file.path(WDOut, paste(fnm,'_type1.jpeg',sep=''))
  jpeg(f.nm, width = 480, height = 480, quality=300) 
  g1 <-ggiNEXT(iNxt,type=1, facet.var = "order")+labs(title=paste('Diversidad Verdadera',fnm,sep=''), y='Diversidad extrapolada',x='Número de Individuos-bootstrap')+
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
  
}

# Plots asymptotic estimations by category

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
} 

# plots iNEXT three types of graphs with Incidence data

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
} 

#XXXXXXXX

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

# XXXXXXXXXXXX

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
  dev.off()
}

# XXXXXXXXXXXXXXXXx

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

# XXXXXXXXXXXXXXX

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

# XXXXXXXXXXXXXXX

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
  })
}

#plots diversity estimates against continuos variables

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
  }
} 

#plots NMDS by category

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
}

# XXXXXXXXXXXXXXXXXXx

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
