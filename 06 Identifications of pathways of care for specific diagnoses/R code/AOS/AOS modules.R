
######################################################################################
#####################AOS inclusion and exclusion module
######################################################################################

AOS_inclusion_and_exclusion_module<-function(df){
  
  diagcodes1=read.csv(dir_diagcodes1,colClasses = "character") # direct diagnosis evidence of AOS
  diagcodes2="" #indirect diagnosis evidence of AOS.  NONE
  proccodes="" # we use special procedure algorithm
  AOS_SpecProcExc=read.csv(dir_AOS_SpecProcExccodes,colClasses = "character")
  
  exclusioncodes=read.csv(dir_exclusioncodes,colClasses = "character") #AOS exclusion codes
  
  df <- df %>% 
    mutate(SpecProcCode = (strtrim(as.character(gsub("[^Q0-9]", "", sp_allocation)), 2))) 
  
  #####################define functions used in AOS inclusion and exclusion#################################
  # function: search for all evidence used in inclusion and exclusion
  searchforevidence<-function(){
    # diag evidence 1
    usingcode=diagcodes1$shortcode
    for( j in 1:length(usingcode)){
      index=which(shortcode %in% usingcode[j])
      df$AOSdiagcodes1[index]=1
    }
    
    
    # exclusion 
    usingcode=exclusioncodes$shortcode
    for( j in 1:length(usingcode)){
      index=which(shortcode %in% usingcode[j])
      df$AOSexcludecodes[index]=1
    }
    return(df)
  }
  
  
  ######################search evidence in each fields###################################################
  
  # assign variables
  df$AOSdiagcodes1=0 # direct diagnosis evidence of AOS
  df$AOSdiagcodes2=0 #indirect diagnosis evidence of AOS.
  df$AOSproccodes=0 #procedure  evidence of AOS
  df$AOSexcludecodes=0 #AOS exclusion codes
  
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
    mutate(AOSproccodes=ifelse(SpecProcCode %in% c("25","27","31","68"),1,AOSproccodes))
  
  df<-df %>%
    mutate(AOSexcludecodes=ifelse(SpecProcCode %in% AOS_SpecProcExc$SpecProcCode,1,AOSexcludecodes))
  
  
  ###########################################
  ##mark patients who had  evidence of AOS. 
  ###########################################
  df<-df %>%
    group_by(patid) %>%
    mutate(patAOSdiagcodes=max(AOSdiagcodes1))%>% 
    mutate(patAOSproccodes=max(AOSproccodes))%>% 
    mutate(patSuggestiveAOS=max(AOSdiagcodes2))%>% 
    ungroup()

  ###########################################
  ##mark patients with exclusion code of AOS
  ###########################################

  df<-df %>%
    group_by(patid) %>%
    mutate(patAOSexcludecodes=max(AOSexcludecodes))%>% 
    ungroup()
  
  ###########################################
  ##mark AOS patients 
  ###########################################
  df<-df %>%
    mutate(pat_diagAOS=ifelse(patAOSdiagcodes==1 | patSuggestiveAOS,1,0))%>% 
    mutate(pat_procAOS=ifelse(patAOSproccodes==1,1,0))
  
  
  df<-df %>%
    mutate(AOSpat=ifelse(( pat_diagAOS==1 | pat_procAOS==1  ) & patAOSexcludecodes==0,1,0))
  
  
  return(df)
}



######################################################################################
#####################AOS pathway module
######################################################################################
# AOS  is exclusively biventricular

AOS_pathway_module<-function(df){
  
  df<-df %>%
    mutate(ageatopchange=ifelse( is.na(ageatop) & !is.na(ageatdis), 1, 0)) 
  table(df$ageatopchange)
  # replace ageatop as ageatdis if ageatdis not missing, useful to estimate age at procedure as best as we can.
  df<-df %>%
    mutate(ageatop=ifelse( ageatopchange==1, ageatdis, ageatop))
  

  
  
  ########## identify stage 1
  # search  proc filed
  df$stage1=0
  df$stage1B=0
  df$stage1C=0

  
  for(i in 1:7){
    shortcode=df[[paste0("proccode",i)]]
    df=searchforevidence_generic(mark="stage1B",usingcode=c("121800","121801","121802","121803","121804","121810","121815","121827","121830","121817"))
    df=searchforevidence_generic(mark="stage1C",usingcode=c("121004","122020","122021"))
  }
  
  
  # assign stage 1 (raw data)
  df<-df %>%
    mutate(stage1 =ifelse((stage1B==1 | stage1C==1),1,0)) 
  
  
  #  subtypes
  df<-df %>%
    mutate(sub_stage1 =ifelse(stage1B==1,1,
                              ifelse(stage1C==1,2,-999))) 
  sub_stage1Lab<-c("1: palliative type Coarctation only","2: palliative type Hybrid","-999: NA")
  
  
  
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
  ########## indentify  reparative procedure for AOS
  ######################################################################################
  
  df$AoSrepair=0  

  df$AoSrepairA1=0  
  
  df$AoSrepairA2=0  
  
  df$AoSrepairB=0  
  
  df$AoSrepairC=0  
  
  for(i in 1:7){
    shortcode=df[[paste0("proccode",i)]]
    
    df=searchforevidence_generic(mark="AoSrepairA1",usingcode=c("121602","121604","121611"))
    df=searchforevidence_generic(mark="AoSrepairA2",usingcode=c("121625","121605"))
    df=searchforevidence_generic(mark="AoSrepairB",usingcode=c("121621","121622","121628","121629","121697","121650","121663","121664","121790","121633","121381","121777","121791","121635"))
    df=searchforevidence_generic(mark="AoSrepairC",usingcode=c("121630","121614","121662","121600"))
  }
  
  
  df<-df %>%
    mutate(AOSpathway=ifelse(
      AoSrepairA1==1 | AoSrepairA2==1 | AoSrepairB==1 | AoSrepairC==1 ,1,0))  
  
  
  # and AOS repair subtypes
  df<-df %>%
    mutate(sub_repair =ifelse(AoSrepairA1==1,1,
                              ifelse(AoSrepairA2==1,2,
                                     ifelse(AoSrepairB==1,3,
                                            ifelse(AoSrepairC==1,4,-999)))))
  
  sub_repairLab<-c("1: type A1 Reparative relief of AoS: surgical","2: type A2 Reparative relief of AoS: cathether",
                   "3: type B Reparative proc for AoS involving replacement proc","4: type C Complex and other types of reparative proc in AoS","-999: NA")
  
  df<-df %>%
    group_by(patid) %>%
    mutate(patAOSpathway=max(AOSpathway))%>%
    ungroup()
  

  
  #################### assign CatProc for each record 
  df$CatProc=ifelse(df$HT==1,6,
                           ifelse(df$AOSpathway==1,4,
                                         ifelse(df$stage1==1,1,-999)))
  df$CatProc_raw=df$CatProc
  
  # mark the first occurrence
  
  
  df<-df %>%
    group_by(patid) %>%
    arrange(patentry,.by_group=T) %>%
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
  falseifNA<-function(x){
    ifelse(is.na(x),F,x)
  }
  
  df$patAge2=df$patAge3=NA
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
  
  #check backward sequence. e.g., palliative first stage procedure after stage two/three procedure will be reintervention 
  
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
  table(df$AOSpathway,useNA="ifany")
  
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
  
  
  #mark sub type of treatment pathway in patient level
  
  
  df<-df %>%
    mutate(auxsub_stage1 =ifelse(CatProc==1,sub_stage1,-999)) %>%
    mutate(auxsub_repair =ifelse(CatProc==4,sub_repair,-999)) 
  
  df<-df %>%
    group_by(patid) %>%
    mutate(patsub_stage1 =ifelse(max(auxsub_stage1)>=0,max(auxsub_stage1),-999)) %>%
    mutate(patsub_BVpathway =ifelse(max(auxsub_repair)>=0,max(auxsub_repair),-999)) %>%
    ungroup()
  
  df$patsub_stage1Label<-factor(df$patsub_stage1,levels=c(1:2,-999),labels=sub_stage1Lab,ordered=T)
  df$patsub_BVpathwayLabel<-factor(df$patsub_BVpathway,levels=c(1:4,-999),labels=sub_repairLab,ordered=T)
  
  
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
                "4: Reparative procedure" ,"5: Other pathway (none in AOS)","6: Heart transplant" ,
                "7: reintervention", "8: Excluded")
  df$CatProcLabel<-factor(df$CatProc,levels=c(0:8),labels=CatProc_lab,ordered=T)
  
  
  df$ProcSeq=as.character(df$ProcSeq)
  
  df<-df %>%
    arrange(patid, patentry) 
  
  return(df)
}


######################################################################################
#####################AOS diagnosis subgroup
######################################################################################
AOS_diagnosis_subtype_module<-function(df){
  MLLHOevidencecode=read.csv(dir_AOS_MLLHOevidencecodes,colClasses = "character") 
  df$MLLHOevidence=0

  # search diagnosis filed
  for(i in 1:29){
    shortcode=df[[paste0("diagcode",i)]]
    
    usingcode=MLLHOevidencecode$shortcode
    df=searchforevidence_generic(mark="MLLHOevidence",usingcode)
    
  }
  
  # search morbidity filed
  for(i in 1:16){
    shortcode=df[[paste0("comorbidity",i)]]
    
    
    usingcode=MLLHOevidencecode$shortcode
    df=searchforevidence_generic(mark="MLLHOevidence",usingcode)
    
  }
  
  
  # search  proc filed
  for(i in 1:7){
    shortcode=df[[paste0("proccode",i)]]
    
    usingcode=MLLHOevidencecode$shortcode
    df=searchforevidence_generic(mark="MLLHOevidence",usingcode)
    
  }
  
  # search  prev proc filed
  for(i in 1:26){
    shortcode=df[[paste0("prevproccode",i)]]
    
    usingcode=MLLHOevidencecode$shortcode
    df=searchforevidence_generic(mark="MLLHOevidence",usingcode)
  }
  
  
  df<-df %>%
    group_by(patid) %>%
    mutate(patMLLHOevidence=max(MLLHOevidence)) %>%
    ungroup()
  
  # assign diagnostic subgroup 
  
  df<-df %>%
    mutate(diagsubgroup=ifelse( patMLLHOevidence==1 ,1,2))
  

  
  diagsubgroup_lab=c("1:AOS with multi level left heart obstruction","2: Isolated AoS")
  df$diagsubgroupLab<-factor(df$diagsubgroup,levels=c(1:2),labels=diagsubgroup_lab,ordered=T)
  
  
  return(df)
}

######################################################################################
########## AOS specific minor data missing or unusual records
######################################################################################

AOS_specific_minor_data_errors_module<-function(df){

  #paients who had  type B or C reparative proc as their first reparative pathway
  df<-df %>%
    mutate(Flagcentre_minor=ifelse(patsub_BVpathway %in% c(3,4),1,Flagcentre_minor))  %>%
    mutate(Flagcentre_minorinf=ifelse(patsub_BVpathway %in% c(3,4), paste0(Flagcentre_minorinf,";type B or C reparative proc"),Flagcentre_minorinf)) 
  
  
  
  #need to replace minor data missing as 0 if have been flagged  as suspected missing/miscoded
  df<-df %>%
    mutate(Flagcentre_minor=ifelse(Flagcentre_missingdata==1 ,0,Flagcentre_minor))  %>%
    mutate(Flagcentre_minorinf=ifelse(Flagcentre_missingdata==1 ,"",Flagcentre_minorinf))  
  
  return(df)
  
}

