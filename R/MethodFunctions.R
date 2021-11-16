# Metodological functions


# XXXXXXXXXXXXXXX

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

# gets list by category with abundance

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
  +  return(Data.ee.oo)
} 

#gets list by category with incidence data

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
} 


#gets abundance by MU or sub MU. List by sampling protocol

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
}

#gets abundances merged by protocols using eventID or other column (except parenteventID).List by protocol groups 

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
}

#gets diversity estimates for incidence and adds sampling effort.DF with group protocols

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
}

# gets final df with sampling effort

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
} 

#Gets NMDS for each element in the list Data.ab

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
}

#gets the info to plot NMDS

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
} 

# XXXXXXXXXXXXXXXXx 

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

# XXXXXXXXXXXXX

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

# XXXXXXXXXXXXXXXXXX

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

# XXXXXXXXXXXXXXX

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

# XXXXXXXXXXXXXXXXXXX

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
