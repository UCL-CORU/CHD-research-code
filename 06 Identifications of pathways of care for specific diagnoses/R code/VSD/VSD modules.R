######################################################################################
#####################VSD inclusion and exclusion module
######################################################################################

VSD_inclusion_and_exclusion_module<-function(df){
  
  diagcodes1=read.csv(dir_diagcodes1,colClasses = "character") # direct diagnosis evidence of VSD
  diagcodes2="" #indirect diagnosis evidence of VSD.  NONE
  proccodes="" # we use special procedure algorithm
  VSD_SpecProcExc=read.csv(dir_VSD_SpecProcExccodes,colClasses = "character")
  exclusioncodes=read.csv(dir_exclusioncodes,colClasses = "character") #VSD exclusion codes
  
  df <- df %>% 
    mutate(SpecProcCode = (strtrim(as.character(gsub("[^Q0-9]", "", sp_allocation)), 2))) 
  
  #####################define functions used in VSD inclusion and exclusion#################################
  # function: search for all evidence used in inclusion and exclusion
  searchforevidence<-function(){
    # diag evidence 1
    usingcode=diagcodes1$shortcode
    for( j in 1:length(usingcode)){
      index=which(shortcode %in% usingcode[j])
      df$VSDdiagcodes1[index]=1
    }
    
    
    # exclusion 
    usingcode=exclusioncodes$shortcode
    for( j in 1:length(usingcode)){
      index=which(shortcode %in% usingcode[j])
      df$VSDexcludecodes[index]=1
    }
    return(df)
  }
  
  
  ######################search evidence in each fields###################################################
  
  # assign variables
  df$VSDdiagcodes1=0 # direct diagnosis evidence of VSD
  df$VSDdiagcodes2=0 #indirect diagnosis evidence of VSD.
  df$VSDproccodes=0 #procedure  evidence of VSD
  df$VSDexcludecodes=0 #VSD exclusion codes
  
  # search diagnosis filed
  for(i in 1:29){
    shortcode=df[[paste0("diagcode",i)]]
    df=searchforevidence()
  }
  
  # search morbidity filed
  for(i in 1:16){
    shortcode=df[[paste0("comorbidity",i)]]
    df=searchforevidence()
  }
  
  # search  proc filed
  for(i in 1:7){
    shortcode=df[[paste0("proccode",i)]]
    df=searchforevidence()
  }
  
  # search  prev proc filed
  for(i in 1:26){
    shortcode=df[[paste0("prevproccode",i)]]
    df=searchforevidence()
  }
  
  
  df<-df %>%
    mutate(VSDproccodes=ifelse(SpecProcCode %in% c("50","51","70"),1,VSDproccodes))
  
  df<-df %>%
    mutate(VSDexcludecodes=ifelse(SpecProcCode %in% VSD_SpecProcExc$SpecProcCode,1,VSDexcludecodes))
  
  
  ###########################################
  ##mark patients who had  evidence of VSD. 
  ###########################################
  df<-df %>%
    group_by(patid) %>%
    mutate(patVSDdiagcodes=max(VSDdiagcodes1))%>% 
    mutate(patVSDproccodes=max(VSDproccodes))%>% 
    mutate(patSuggestiveVSD=max(VSDdiagcodes2))%>% 
    ungroup()
  
  ###########################################
  ##mark patients with exclusion code of VSD
  ###########################################
  
  df<-df %>%
    group_by(patid) %>%
    mutate(patVSDexcludecodes=max(VSDexcludecodes))%>% 
    ungroup()
  
  ###########################################
  ##mark VSD patients 
  ###########################################
  df<-df %>%
    mutate(pat_diagVSD=ifelse(patVSDdiagcodes==1 | patSuggestiveVSD==1,1,0))%>% 
    mutate(pat_procVSD=ifelse(patVSDproccodes==1,1,0))
  
  
  df<-df %>%
    mutate(VSDpat=ifelse(( pat_diagVSD==1 | pat_procVSD==1  ) & patVSDexcludecodes==0,1,0))
  
  
  return(df)
}



######################################################################################
#####################VSD pathway module
######################################################################################
# VSD  is exclusively biventricular

VSD_pathway_module<-function(df){
  
  df<-df %>%
    mutate(ageatopchange=ifelse( is.na(ageatop) & !is.na(ageatdis), 1, 0)) 
  table(df$ageatopchange)
  # replace ageatop as ageatdis if ageatdis not missing, useful to estimate age at procedure as best as we can.
  df<-df %>%
    mutate(ageatop=ifelse( ageatopchange==1, ageatdis, ageatop))
  
  
  
  
  ########## identify stage 1
  # search  proc filed
  df$stage1=0

  for(i in 1:7){
    shortcode=df[[paste0("proccode",i)]]
    df=searchforevidence_generic(mark="stage1",usingcode=c("121402"))
  }
  
  df<-df %>%
    group_by(patid) %>%
    mutate(patStage1=max(stage1)) %>%
    ungroup()
  
  
  
  # identify patients who had heart transplant
  HTcode=c("123701","123702" ,"123703","123704","123706")
  df$HT=0
  for(i in 1:7){
    shortcode=df[[paste0("proccode",i)]]
    df=searchforevidence_generic(mark="HT",HTcode)
  }
  
  df<-df %>%
    group_by(patid) %>%
    mutate(patHT=max(HT))%>%
    ungroup()
  
  tabpatnum(df$patHT,df)
  
  
  ######################################################################################
  ########## identify  reparative procedure for VSD
  ######################################################################################
  
  
  df$VSDrepair=0  
  for(i in 1:7){
    shortcode=df[[paste0("proccode",i)]]
    df=searchforevidence_generic(mark="VSDrepair",usingcode=c("120801","120802","120803","120807","120816","120828","120901"))
  }
  
  
  df<-df %>%
    mutate(VSDpathway=ifelse( VSDrepair==1  ,1,0))  
  

  
  #################### assign CatProc for each record
  df<-df %>%
    mutate(CatProc=ifelse(HT==1,6,
                          ifelse(VSDrepair==1,4,
                                 ifelse(stage1==1,1,-999))))
                                        
                                        
                                        
  
  df$CatProc_raw=df$CatProc
  # mark the first occurrence
  
  
  df<-df %>%
    arrange(patid,patentry) %>%
    group_by(patid) %>%
    mutate(firstCatProc1 = ifelse(!is.na(which(CatProc==1)[1]) & which(CatProc==1)[1]==patentry,1,0)) %>%
    mutate(firstCatProc4 = ifelse(!is.na(which(CatProc==4)[1]) & which(CatProc==4)[1]==patentry,1,0)) %>%
    mutate(firstCatProc6 = ifelse(!is.na(which(CatProc==6)[1]) & which(CatProc==6)[1]==patentry,1,0)) %>%
    ungroup()
  
  df$firstCatProc_max<-NULL
  firstCatProc_cols<-df %>% select(starts_with("firstCatProc"))
  df$firstCatProc_max=apply(firstCatProc_cols,1,max)
  
  df<-df %>%
    mutate(CatProc=ifelse(firstCatProc_max==0,-999,CatProc))

  
  
  # age at each pathway 
  
  
  df$patAge2=df$patAge3=df$patAge5=NA
  df<-df %>%
    mutate(patAge1 = ifelse(CatProc==1,ageatop,-999)) %>%
    mutate(patAge4 = ifelse(CatProc==4,ageatop,-999)) %>%
    mutate(patAge6 = ifelse(CatProc==6,ageatop,-999)) 
  
  df<-df %>%
    group_by(patid) %>%
    mutate(patAge1 =ifelse(max(patAge1)>=0,max(patAge1),NA)) %>%
    mutate(patAge4 =ifelse(max(patAge4)>=0,max(patAge4),NA)) %>%
    mutate(patAge6 =ifelse(max(patAge6)>=0,max(patAge6),NA)) %>%
    ungroup()
  
  #check backward sequence. e.g., palliative first stage procedure after reparative procedure will be reintervention 
  
  df<-df %>%
    mutate(CatProc=ifelse(CatProc==1 & ageatop>patAge6 & !is.na(patAge6) & !is.na(ageatop),-999,CatProc))   %>%
    mutate(CatProc=ifelse(CatProc==1 & ageatop>patAge4 & !is.na(patAge4) & !is.na(ageatop),-999,CatProc))   %>%
    mutate(CatProc=ifelse(CatProc==4 & ageatop>patAge6 & !is.na(patAge6) & !is.na(ageatop),-999,CatProc))  
  
  
  
  # recompute age
  df<-df %>%
    mutate(patAge1 = ifelse(CatProc==1,ageatop,-999)) %>%
    mutate(patAge4 = ifelse(CatProc==4,ageatop,-999)) %>%
    mutate(patAge6 = ifelse(CatProc==6,ageatop,-999)) 
  
  df<-df %>%
    group_by(patid) %>%
    mutate(patAge1 =ifelse(max(patAge1)>=0,max(patAge1),NA)) %>%
    mutate(patAge4 =ifelse(max(patAge4)>=0,max(patAge4),NA)) %>%
    mutate(patAge6 =ifelse(max(patAge6)>=0,max(patAge6),NA)) %>%
    ungroup()
  
  table(df$CatProc,useNA="ifany")
  table(df$stage1,useNA="ifany")
  table(df$VSDpathway,useNA="ifany")
  
  df<-df %>%
    group_by(patid) %>%
    mutate(ProcSeq = paste(CatProc, collapse="")) %>%
    ungroup()
  
  df$ProcSeq<-str_replace_all(df$ProcSeq,"-999","")
  
  table(df$ProcSeq,useNA="ifany")
  
  
  # adjust the patentry order
  df<-df %>%
    mutate(patentry=ifelse(grepl("41",ProcSeq) & CatProc==1 & 
                             patAge1==patAge4 & !is.na(patAge1) & !is.na(patAge4),patentry-1,patentry))  %>%
    mutate(patentry=ifelse(grepl("41",ProcSeq) & CatProc==4 & 
                             patAge1==patAge4 & !is.na(patAge1) & !is.na(patAge4),patentry+1,patentry))  
  
  
  df<-df %>%
    arrange(patid,patentry) %>%
    group_by(patid) %>%
    mutate(ProcSeq = paste(CatProc, collapse="")) %>%
    ungroup()
  df$ProcSeq<-str_replace_all(df$ProcSeq,"-999","")
  
  table(df$ProcSeq[df$patentry==1],useNA="ifany")
  
  
  
  # mark reintervention and pre-pathway procedures
  df$minagepathway=apply(df[,grep("^patAge",names(df))],1,function(x) min(x,na.rm=T))
  df$minagepathway[is.infinite(df$minagepathway)]=NA
  
  
  
  df<-df %>%
    mutate(CatProc=ifelse(CatProc==-999 & InterType %in% c(1,2) & is.na(minagepathway),0,CatProc))   %>%
    mutate(CatProc=ifelse(CatProc==-999 & InterType %in% c(1,2) & ageatop<minagepathway & !is.na(minagepathway) & !is.na(ageatop),0,CatProc))   %>%
    mutate(CatProc=ifelse(CatProc==-999 & InterType %in% c(1,2) ,7,CatProc))   %>%
    mutate(CatProc=ifelse(CatProc==-999 & InterType %in% c(3) ,8,CatProc)) 
  
  # pathway marker
  df<-df %>%
    mutate(ispathway =ifelse(CatProc %in% c(1,2,3,4,5),1,0)) 
  
  
  
  CatProc_lab=c("0: Pre-pathway","1: Palliative first stage procedure","2: SV stage two","3: SV stage three (Fontan)",
                "4: Reparative procedure" ,"5: other pathway (none in VSD)","6: Heart transplant" ,
                "7: reintervention", "8: Excluded")
  df$CatProcLabel<-factor(df$CatProc,levels=c(0:8),labels=CatProc_lab,ordered=T)
  
  
  df$ProcSeq=as.character(df$ProcSeq)
  
  df<-df %>%
    arrange(patid, patentry) 
  
  return(df)
}



######################################################################################
#####################VSD diagnosis subgroup
######################################################################################
VSD_diagnosis_subtype_module<-function(df){
  
  df$multiVSD=0
  # search diagnosis filed
  for(i in 1:29){
    shortcode=df[[paste0("diagcode",i)]]
    df=searchforevidence_generic(mark="multiVSD",usingcode=c("071504","120816") )
  }
  
  # search morbidity filed
  for(i in 1:16){
    shortcode=df[[paste0("comorbidity",i)]]
    df=searchforevidence_generic(mark="multiVSD",usingcode=c("071504","120816") )
  }
  
  
  # search  proc filed
  for(i in 1:7){
    shortcode=df[[paste0("proccode",i)]]
    df=searchforevidence_generic(mark="multiVSD",usingcode=c("071504","120816") )
    }
  
  # search  prev proc filed
  for(i in 1:26){
    shortcode=df[[paste0("prevproccode",i)]]
    df=searchforevidence_generic(mark="multiVSD",usingcode=c("071504","120816") )
  }
  
  
  df<-df %>%
    group_by(patid) %>%
    mutate(patmultiVSD=max(multiVSD)) %>%
    ungroup()
  
  # assign diagnostic subgroup 
  
  df<-df %>%
    mutate(diagsubgroup=ifelse( patmultiVSD==1 ,1,2))
  
  
  
  diagsubgroup_lab=c("1:Muti-VSD","2: Isolated VSD")
  df$diagsubgroupLab<-factor(df$diagsubgroup,levels=c(1:2),labels=diagsubgroup_lab,ordered=T)
  
  
  return(df)
}


######################################################################################
########## VSD specific minor data missing or unusual records
######################################################################################

VSD_specific_minor_data_errors_module<-function(df){
  
  # patients who had pre-pathway only
  df<-df %>%
    mutate(Flagcentre_minor=ifelse(ProcSeq=="" & lastknownstatus==0 & patHT==0,1,Flagcentre_minor))  %>%
    mutate(Flagcentre_minorinf=ifelse(ProcSeq=="" & lastknownstatus==0 & patHT==0, paste0(Flagcentre_minorinf,"; patient had pre-pathway only and survived"),Flagcentre_minorinf)) 
  
  
  #need to replace minor data missing as 0 if have been flagged  as suspected missing/miscoded
  df<-df %>%
    mutate(Flagcentre_minor=ifelse(Flagcentre_missingdata==1 ,0,Flagcentre_minor))  %>%
    mutate(Flagcentre_minorinf=ifelse(Flagcentre_missingdata==1 ,"",Flagcentre_minorinf))  
  
  return(df)
  
}



