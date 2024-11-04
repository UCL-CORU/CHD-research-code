######################################################################################
#####################TOF inclusion and exclusion module
######################################################################################


TOF_inclusion_and_exclusion_module<-function(df){
  
  diagcodes1=read.csv(dir_diagcodes1,colClasses = "character") # direct diagnosis evidence of TOF
  diagcodes2="" #indirect diagnosis evidence of TOF.  NONE
  proccodes=read.csv(dir_proccodes,colClasses = "character") #procedure  evidence of TOF
  exclusioncodes=read.csv(dir_exclusioncodes,colClasses = "character") #TOF exclusion codes
  
  #####################define functions used in TOF inclusion and exclusion#################################
  # function: search for all evidence used in inclusion and exclusion
  searchforevidence<-function(){
    # diag evidence 1
    usingcode=diagcodes1$shortcode
    for( j in 1:length(usingcode)){
      index=which(shortcode %in% usingcode[j])
      df$TOFdiagcodes1[index]=1
    }
    
    # proc evidence
    usingcode=proccodes$shortcode
    for( j in 1:length(usingcode)){
      index=which(shortcode %in% usingcode[j])
      df$TOFproccodes[index]=1
    }
    
    # exclusion 
    usingcode=exclusioncodes$shortcode
    for( j in 1:length(usingcode)){
      index=which(shortcode %in% usingcode[j])
      df$TOFexcludecodes[index]=1
    }
    return(df)
  }
  
  
  ######################search evidence in each fields###################################################
  
  # assign variables
  df$TOFdiagcodes1=0 # direct diagnosis evidence of TOF
  df$TOFdiagcodes2=0 #indirect diagnosis evidence of TOF.
  df$TOFproccodes=0 #procedure  evidence of TOF
  df$TOFexcludecodes=0 #TOF exclusion codes
  
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
  
  
  
  ###########################################
  ##mark patients who had  evidence of TOF. 
  ###########################################
  df<-df %>%
    group_by(patid) %>%
    mutate(patTOFdiagcodes=max(TOFdiagcodes1))%>% 
    mutate(patTOFproccodes=max(TOFproccodes))%>% 
    mutate(patSuggestiveTOF=max(TOFdiagcodes2))%>% 
    ungroup()
  
  ###########################################
  ##mark patients with exclusion code of TOF
  ###########################################
  df<-df %>%
    group_by(patid) %>%
    mutate(patTOFexcludecodes=max(TOFexcludecodes))%>% 
    ungroup()
  
  ###########################################
  ##mark TOF patients 
  ###########################################
  df<-df %>%
    mutate(pat_diagTOF=ifelse(patTOFdiagcodes==1 | patSuggestiveTOF,1,0))%>% 
    mutate(pat_procTOF=ifelse(patTOFproccodes==1,1,0))
  
  
  df<-df %>%
    mutate(TOFpat=ifelse(( pat_diagTOF==1 | pat_procTOF==1  ) & patTOFexcludecodes==0,1,0))

  
  return(df)
}


######################################################################################
#####################TOF pathway module
######################################################################################
# TOF   is exclusively biventricular

TOF_pathway_module<-function(df){
  
  df<-df %>%
    mutate(ageatopchange=ifelse( is.na(ageatop) & !is.na(ageatdis), 1, 0)) 
  table(df$ageatopchange)
  # replace ageatop as ageatdis if ageatdis not missing, useful to estimate age at procedure as best as we can.
  df<-df %>%
    mutate(ageatop=ifelse( ageatopchange==1, ageatdis, ageatop))
  
  stage1=read.csv(dir_stage1,colClasses = "character") #stage 1 code
  SVstage2=read.csv(dir_SVstage2,colClasses = "character") #stage 2 code
  SVstage3=read.csv(dir_SVstage3,colClasses = "character") #stage 3 code
  
  
  ########## identify stage 1 and SV  2 and 3
  # search  proc filed

  df$stage1D=0
  df$Glenn=0
  df$Fontan=0
  df$V15=0
  
  for(i in 1:7){
    shortcode=df[[paste0("proccode",i)]]
    
  
    
    df=searchforevidence_generic(mark="V15","120619")
    
    usingcode=stage1$shortcode[which(stage1$subtype=="D")]
    df=searchforevidence_generic(mark="stage1D",usingcode)
    
    
    usingcode=SVstage2$shortcode[which(SVstage2$subtype=="Glenn")]
    df=searchforevidence_generic(mark="Glenn",usingcode)
    
    
    usingcode=SVstage3$shortcode
    df=searchforevidence_generic(mark="Fontan",usingcode)
  }
  
 
  # assign stage 1, 2 and 3 (raw data)
  df<-df %>%
    mutate(stage1 =ifelse(stage1D==1,1,0))%>%
    mutate(stage2 =ifelse((V15==1 | Glenn==1),1,0)) %>%
    mutate(stage3 =ifelse(Fontan==1,1,0)) 
  
  
  #  subtypes
  df<-df %>%
    mutate(sub_stage1 =ifelse(stage1==1,1,-999))  %>%
    mutate(sub_stage2 =ifelse(Glenn==1,1,
                              ifelse(V15==1,2,-999))) 
    

  
  df<-df %>%
    group_by(patid) %>%
    mutate(patStage1=max(stage1))%>% 
    mutate(patStage2=max(stage2))%>% 
    mutate(patStage3=max(stage3))%>%
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
  ########## indentify  reparative procedure for TOF
  ######################################################################################
  
  #TOF and absent pulmonary valve repair
  df$TOFnoPV=0  
  TOFnoPVPVcode=c("122621")
  
  #TOF repair 
  df$TOFrepair=0 
  TOFrepaircode=c("122601","122613","122620","122701")
  
  
  #Other reparative proc
  df$TOFrepairOther=0 
  TOFrepairOthercode=c("120635","122920","122702","123601","121321","121322")
  
 
  df$CoB=0  #and with VSD code
  CoBcode=c("120641","120600","120626","121302","120821") 
  
  #VSD repair
  
  df$VSDrepair=0 
  VSDrepaircode=c("120801","120802","120803","120807","120828","120816")
  
  for(i in 1:7){
    shortcode=df[[paste0("proccode",i)]]
    
    df=searchforevidence_generic(mark="TOFnoPV",TOFnoPVPVcode)
    df=searchforevidence_generic(mark="TOFrepair",TOFrepaircode)
    df=searchforevidence_generic(mark="TOFrepairOther",TOFrepairOthercode)
    df=searchforevidence_generic(mark="CoB",CoBcode)
    df=searchforevidence_generic(mark="VSDrepair",VSDrepaircode)
  }
  

  df<-df %>%
    mutate(TOFpathway=ifelse(
      TOFnoPV==1 | TOFrepair==1 | TOFrepairOther==1 | VSDrepair==1 ,1,0))  
  
  
  # and TOF repair subtypes
  df<-df %>%
    mutate(sub_repair =ifelse(TOFnoPV==1,1,
                              ifelse(TOFrepair==1,2,
                                     ifelse(TOFrepairOther==1,3,
                                            ifelse(CoB==1 & VSDrepair==1,3,
                                                   ifelse(VSDrepair==1 ,4,-999))))))
  
  sub_repairLab<-c("1: Fallot and absent pulmonary valve repair","2: TOF repair",
                   "3: Other reparative procedures for TOF","4: VSD type repair","-999: NA")
  
  df<-df %>%
    group_by(patid) %>%
    mutate(patTOFpathway=max(TOFpathway))%>%
    ungroup()
  
  #indentify stage 2 subtypes.
  

  #################### assign CatProc for each record 
  df$CatProc=ifelse(df$HT==1,6,
                    ifelse(df$stage3==1,3,
                           ifelse(df$TOFpathway==1,4,
                                  ifelse(df$stage2==1,2,
                                         ifelse(df$stage1==1,1,-999)))))
  df$CatProc_raw=df$CatProc
  
  # mark the first occurrence
  
  
  df<-df %>%
    group_by(patid) %>%
    arrange(patentry,.by_group=T) %>%
    mutate(firstCatProc1 = ifelse(!is.na(which(CatProc==1)[1]) & which(CatProc==1)[1]==patentry,1,0)) %>%
    mutate(firstCatProc2 = ifelse(!is.na(which(CatProc==2)[1]) & which(CatProc==2)[1]==patentry,1,0)) %>%
    mutate(firstCatProc3 = ifelse(!is.na(which(CatProc==3)[1]) & which(CatProc==3)[1]==patentry,1,0)) %>%
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
  
  df<-df %>%
    mutate(patAge1 = ifelse(CatProc==1,ageatop,-999)) %>%
    mutate(patAge2 = ifelse(CatProc==2,ageatop,-999)) %>%
    mutate(patAge3 = ifelse(CatProc==3,ageatop,-999)) %>%
    mutate(patAge4 = ifelse(CatProc==4,ageatop,-999)) %>%
    mutate(patAge6 = ifelse(CatProc==6,ageatop,-999)) 
  
  df<-df %>%
    group_by(patid) %>%
    mutate(patAge1 =ifelse(max(patAge1)>=0,max(patAge1),NA)) %>%
    mutate(patAge2 =ifelse(max(patAge2)>=0,max(patAge2),NA)) %>%
    mutate(patAge3 =ifelse(max(patAge3)>=0,max(patAge3),NA)) %>%
    mutate(patAge4 =ifelse(max(patAge4)>=0,max(patAge4),NA)) %>%
    mutate(patAge6 =ifelse(max(patAge6)>=0,max(patAge6),NA)) %>%
    ungroup()
  
  #check backward sequence. e.g., palliative first stage procedure after stage two/three procedure will be off-pathway 
  
  df<-df %>%
    mutate(CatProc=ifelse(CatProc==1 & ageatop>patAge2 & !is.na(patAge2) & !is.na(ageatop),-999,CatProc))   %>%
    mutate(CatProc=ifelse(CatProc==1 & ageatop>patAge3 & !is.na(patAge3) & !is.na(ageatop),-999,CatProc))   %>%
    mutate(CatProc=ifelse(CatProc==1 & ageatop>patAge6 & !is.na(patAge6) & !is.na(ageatop),-999,CatProc))   %>%
    mutate(CatProc=ifelse(CatProc==1 & ageatop>patAge4 & !is.na(patAge4) & !is.na(ageatop),-999,CatProc))   %>%
    mutate(CatProc=ifelse(CatProc==2 & ageatop>patAge3 & !is.na(patAge3) & !is.na(ageatop),-999,CatProc))   %>%
    mutate(CatProc=ifelse(CatProc==4 & ageatop>patAge2 & !is.na(patAge2) & !is.na(ageatop),-999,CatProc))   %>%
    mutate(CatProc=ifelse(CatProc==4 & ageatop>patAge3 & !is.na(patAge3) & !is.na(ageatop),-999,CatProc))  %>%
    mutate(CatProc=ifelse(CatProc==4 & ageatop>patAge6 & !is.na(patAge6) & !is.na(ageatop),-999,CatProc))  
  
  
  
  # recompute age
  df<-df %>%
    mutate(patAge1 = ifelse(CatProc==1,ageatop,-999)) %>%
    mutate(patAge2 = ifelse(CatProc==2,ageatop,-999)) %>%
    mutate(patAge3 = ifelse(CatProc==3,ageatop,-999)) %>%
    mutate(patAge4 = ifelse(CatProc==4,ageatop,-999)) %>%
    mutate(patAge6 = ifelse(CatProc==6,ageatop,-999)) 
  
  df<-df %>%
    group_by(patid) %>%
    mutate(patAge1 =ifelse(max(patAge1)>=0,max(patAge1),NA)) %>%
    mutate(patAge2 =ifelse(max(patAge2)>=0,max(patAge2),NA)) %>%
    mutate(patAge3 =ifelse(max(patAge3)>=0,max(patAge3),NA)) %>%
    mutate(patAge4 =ifelse(max(patAge4)>=0,max(patAge4),NA)) %>%
    mutate(patAge6 =ifelse(max(patAge6)>=0,max(patAge6),NA)) %>%
    ungroup()
  
  table(df$CatProc,useNA="ifany")
  table(df$stage1,useNA="ifany")
  table(df$TOFpathway,useNA="ifany")
  
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
                             patAge1==patAge4 & !is.na(patAge1) & !is.na(patAge4),patentry+1,patentry))   %>%
    mutate(patentry=ifelse(grepl("21",ProcSeq) & CatProc==1 & 
                             patAge1==patAge1 & !is.na(patAge1) & !is.na(patAge2),patentry-1,patentry))   %>%
    mutate(patentry=ifelse(grepl("21",ProcSeq) & CatProc==2 & 
                             patAge1==patAge2 & !is.na(patAge1) & !is.na(patAge2),patentry+1,patentry))   %>%
    mutate(patentry=ifelse(grepl("31",ProcSeq) & CatProc==1 & 
                             patAge1==patAge3 & !is.na(patAge1) & !is.na(patAge3),patentry-1,patentry))  %>%
    mutate(patentry=ifelse(grepl("31",ProcSeq) & CatProc==3 & 
                             patAge1==patAge3 & !is.na(patAge1) & !is.na(patAge3),patentry+1,patentry))   
  
  
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
    mutate(auxsub_stage2 =ifelse(CatProc==2,sub_stage2,-999)) %>%
    mutate(auxsub_repair =ifelse(CatProc==4,sub_repair,-999)) 
  df<-df %>%
    group_by(patid) %>%
    mutate(patsub_stage1 =ifelse(max(auxsub_stage1)>=0,max(auxsub_stage1),-999)) %>%
    mutate(patsub_stage2 =ifelse(max(auxsub_stage2)>=0,max(auxsub_stage2),-999)) %>%
    mutate(patsub_BVpathway =ifelse(max(auxsub_repair)>=0,max(auxsub_repair),-999)) %>%
    ungroup()
  
  df$patsub_BVpathwayLabel<-factor(df$patsub_BVpathway,levels=c(1:4,-999),labels=sub_repairLab,ordered=T)
  
  
  # mark off-pathway and pre-pathway procedures
  df$minagepathway=apply(df[,grep("^patAge",names(df))],1,function(x) min(x,na.rm=T))
  df$minagepathway[is.infinite(df$minagepathway)]=NA
  
  
  
  df<-df %>%
    mutate(CatProc=ifelse(CatProc==-999 & InterType %in% c(1,2) & is.na(minagepathway),0,CatProc))   %>%
    mutate(CatProc=ifelse(CatProc==-999 & InterType %in% c(1,2) & ageatop<minagepathway & !is.na(minagepathway) & !is.na(ageatop),0,CatProc))   %>%
    mutate(CatProc=ifelse(CatProc==-999 & InterType %in% c(1,2) ,7,CatProc))   %>%
    mutate(CatProc=ifelse(CatProc==-999 & InterType %in% c(3) ,8,CatProc)) 
  
  # pathway marker
  df<-df %>%
    mutate(ispathway =ifelse(CatProc %in% c(1,2,3,4),1,0)) 
  
  
  
  CatProc_lab=c("0: Pre-pathway","1: Palliative first stage procedure","2: SV stage two","3: SV stage three (Fontan)",
                "4: Reparative procedure" ,"5: Other pathway (none in TOF)","6: Heart transplant" ,
                "7: Off-pathway", "8: Excluded")
  df$CatProcLabel<-factor(df$CatProc,levels=c(0:8),labels=CatProc_lab,ordered=T)
  
  
  df$ProcSeq=as.character(df$ProcSeq)
  
  df<-df %>%
    arrange(patid, patentry) 
  
  return(df)
}


######################################################################################
#####################TOF diagnosis subgroup
######################################################################################
TOF_diagnosis_subtype_module<-function(df){
  
  df$NOPV=0
  df$DORV=0
  
  # search diagnosis filed
  for(i in 1:29){
    shortcode=df[[paste0("diagcode",i)]]
    
    usingcode=c("090525" ,"122621")
    df=searchforevidence_generic(mark="NOPV",usingcode)
    
    usingcode=c("010117" ,"122701","122702")
    df=searchforevidence_generic(mark="DORV",usingcode)
    
  }
  
  # search morbidity filed
  for(i in 1:16){
    shortcode=df[[paste0("comorbidity",i)]]
    
    usingcode=c("090525" ,"122621")
    df=searchforevidence_generic(mark="NOPV",usingcode)
    
    usingcode=c("010117" ,"122701","122702")
    df=searchforevidence_generic(mark="DORV",usingcode)
  }
  
  
  # search  proc filed
  for(i in 1:7){
    shortcode=df[[paste0("proccode",i)]]
    
    usingcode=c("090525" ,"122621")
    df=searchforevidence_generic(mark="NOPV",usingcode)
    
    usingcode=c("010117" ,"122701","122702")
    df=searchforevidence_generic(mark="DORV",usingcode)
  }
  
  # search  prev proc filed
  for(i in 1:26){
    shortcode=df[[paste0("prevproccode",i)]]
    
    usingcode=c("090525" ,"122621")
    df=searchforevidence_generic(mark="NOPV",usingcode)
    
    usingcode=c("010117" ,"122701","122702")
    df=searchforevidence_generic(mark="DORV",usingcode)
    
  }
  
  
  df<-df %>%
    group_by(patid) %>%
    mutate(patNOPV=max(NOPV)) %>%
    mutate(patDORV=max(DORV)) %>%
    ungroup()
  
  # assign diagnostic subgroup 
  
  df<-df %>%
    mutate(diagsubgroup=ifelse( patNOPV==1 ,1,
                                ifelse(patDORV==1,2,3)))
  
  
  diagsubgroup_lab=c("1:TOF absent pulmonary valve","2: TOF with any evidence of double outlet right ventricle",
                     "3: Standard TOF")
  df$diagsubgroupLab<-factor(df$diagsubgroup,levels=c(1:3),labels=diagsubgroup_lab,ordered=T)
  
  
  return(df)
}

######################################################################################
########## TOF specific suspected  missing/miscoded data module
######################################################################################
TOF_specific_suspected_missing_miscoded_data_module<-function(df){
  
  #####################if SV pathway
  
  # min age at SV pathway 
  df$minageSVpathway=apply(df[,which(names(df) %in% c("patAge2","patAge3"))],1,function(x) min(x,na.rm=T))
  df$minageSVpathway[is.infinite(df$minageSVpathway)]=NA
  
  #in SV pathway but had no SV stage two/three by year 5
  
  df<-df %>%
    mutate(Flagcentre_missingdata=ifelse(diagsubgroup %in% c(1,2) & !is.na(minageSVpathway) & minageSVpathway>5 & !(patAge6<5 & !is.na(patAge6) ),1,Flagcentre_missingdata))  %>%
    mutate(Flagcentre_missingdatainf=ifelse(diagsubgroup %in% c(1,2) & !is.na(minageSVpathway) & minageSVpathway>5 & !(patAge6<5 & !is.na(patAge6)) ,
                                            paste0(Flagcentre_missingdatainf,"; in SV pathway but had no SV stage two/three by year 5"),Flagcentre_missingdatainf))  
  
  
  df<-df %>%
    mutate(Flagcentre_missingdata=ifelse(diagsubgroup %in% c(3) & !is.na(minageSVpathway) ,1,Flagcentre_missingdata))  %>%
    mutate(Flagcentre_missingdatainf=ifelse(diagsubgroup %in% c(3) & !is.na(minageSVpathway) ,
                                            paste0(Flagcentre_missingdatainf,"; Standrad tet-  had SV stage two/three"),Flagcentre_missingdatainf))  
  
  
  #####################if BV pathway
  #under BV pathway but had no reprative procedure in records and were surviving by 2 years old
  df<-df %>%
    mutate(Flagcentre_missingdata=ifelse(  is.na(minageSVpathway) & is.na(patAge4)  & ageatlastknownstatus>2  & !(patAge6<2 & !is.na(patAge6)),
                                          1,Flagcentre_missingdata))  %>%
    mutate(Flagcentre_missingdatainf=ifelse(is.na(minageSVpathway) & is.na(patAge4)  & ageatlastknownstatus>2  & !(patAge6<2 & !is.na(patAge6)),
                                            paste0(Flagcentre_missingdatainf,"; under BV pathway but had no reprative procedure whlist surviving by 2 years old (had reprative proc at later age allowed)"),Flagcentre_missingdatainf))  
  
  
  
  return(df)
}



######################################################################################
########## TOF specific minor data missing or unusual records: NONE
######################################################################################

TOF_specific_minor_data_errors_module<-function(df){
  #patients had stage three Fontan
  df<-df %>%
    mutate(Flagcentre_minor=ifelse(!is.na(patAge3),1,Flagcentre_minor))  %>%
    mutate(Flagcentre_minorinf=ifelse(!is.na(patAge3), paste(Flagcentre_minorinf,";TOF patients had stage three Fontan"),Flagcentre_minorinf)) 
  
  #patients with atypical/incompeting coding in reparative procedure
  df<-df %>%
    mutate(Flagcentre_minor=ifelse(patsub_BVpathway==3,1,Flagcentre_minor))  %>%
    mutate(Flagcentre_minorinf=ifelse(patsub_BVpathway==3, paste0(Flagcentre_minorinf,";patients with atypical/incompeting coding in reparative procedure"),Flagcentre_minorinf)) 
  
  
  
  #need to replace minor data missing as 0 if have been flagged  as suspected missing/miscoded
  df<-df %>%
    mutate(Flagcentre_minor=ifelse(Flagcentre_missingdata==1 ,0,Flagcentre_minor))  %>%
    mutate(Flagcentre_minorinf=ifelse(Flagcentre_missingdata==1 ,"",Flagcentre_minorinf))  
  
  return(df)
  
}
