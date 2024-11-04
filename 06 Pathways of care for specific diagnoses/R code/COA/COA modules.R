
######################################################################################
#####################COA inclusion and exclusion module
######################################################################################

COA_inclusion_and_exclusion_module<-function(df){
  
  diagcodes1=read.csv(dir_diagcodes1,colClasses = "character") # direct diagnosis evidence of COA
  diagcodes2="" #indirect diagnosis evidence of COA.  NONE
  proccodes="" # we use special procedure algorithm
  COA_SpecProcExc=read.csv(dir_COA_SpecProcExccodes,colClasses = "character")
  exclusioncodes=read.csv(dir_exclusioncodes,colClasses = "character") #COA exclusion codes
  
  df <- df %>% 
    mutate(SpecProcCode = (strtrim(as.character(gsub("[^Q0-9]", "", sp_allocation)), 2))) 
  
  #####################define functions used in COA inclusion and exclusion#################################
  # function: search for all evidence used in inclusion and exclusion
  searchforevidence<-function(){
    # diag evidence 1
    usingcode=diagcodes1$shortcode
    for( j in 1:length(usingcode)){
      index=which(shortcode %in% usingcode[j])
      df$COAdiagcodes1[index]=1
    }
    
    
    # exclusion 
    usingcode=exclusioncodes$shortcode
    for( j in 1:length(usingcode)){
      index=which(shortcode %in% usingcode[j])
      df$COAexcludecodes[index]=1
    }
    return(df)
  }
  
  
  ######################search evidence in each fields###################################################
  
  # assign variables
  df$COAdiagcodes1=0 # direct diagnosis evidence of COA
  df$COAdiagcodes2=0 #indirect diagnosis evidence of COA.
  df$COAproccodes=0 #procedure  evidence of COA
  df$COAexcludecodes=0 #COA exclusion codes
  
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
    mutate(COAproccodes=ifelse(SpecProcCode %in% c("47"),1,COAproccodes))
  
  df<-df %>%
    mutate(COAexcludecodes=ifelse(SpecProcCode %in% COA_SpecProcExc$SpecProcCode,1,COAexcludecodes))
  
  
  ###########################################
  ##mark patients who had  evidence of COA. 
  ###########################################
  df<-df %>%
    group_by(patid) %>%
    mutate(patCOAdiagcodes=max(COAdiagcodes1))%>% 
    mutate(patCOAproccodes=max(COAproccodes))%>% 
    mutate(patSuggestiveCOA=max(COAdiagcodes2))%>% 
    ungroup()
  
  ###########################################
  ##mark patients with exclusion code of COA
  ###########################################
  
  df<-df %>%
    group_by(patid) %>%
    mutate(patCOAexcludecodes=max(COAexcludecodes))%>% 
    ungroup()
  
  ###########################################
  ##mark COA patients 
  ###########################################
  df<-df %>%
    mutate(pat_diagCOA=ifelse(patCOAdiagcodes==1 | patSuggestiveCOA==1,1,0))%>% 
    mutate(pat_procCOA=ifelse(patCOAproccodes==1,1,0))
  
  
  df<-df %>%
    mutate(COApat=ifelse(( pat_diagCOA==1 | pat_procCOA==1  ) & patCOAexcludecodes==0,1,0))
  
  
  return(df)
}

######################################################################################
#####################COA pathway module
######################################################################################
# COA  is exclusively biventricular

COA_pathway_module<-function(df){
  
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
  ########## identify  reparative procedure for COA
  ######################################################################################
  

  df$COArepairS=0  
  
  df$COArepairC=0  
  
  df$VSDrepair=0  
  

  for(i in 1:7){
    shortcode=df[[paste0("proccode",i)]]
    
    df=searchforevidence_generic(mark="COArepairS",
                                 usingcode=c("121800","121801","121802","121803","121810","121815","121830"))
    df=searchforevidence_generic(mark="COArepairC",usingcode=c("121804","121808","121827","121817","121822","121848"))
    df=searchforevidence_generic(mark="VSDrepair",usingcode=c("120801","120802","120803","120807","120816","120828"))
  }
  
  
  df<-df %>%
    mutate(COArepair=ifelse( COArepairS==1 | COArepairC==1  ,1,0))   %>%
    mutate(COApathway=ifelse( COArepair==1  ,1,0))  
    
  
  # and COA repair subtypes
  df<-df %>%
    mutate(sub_repair =ifelse(COArepair==1 & VSDrepair==1,1,
                              ifelse(COArepair==1 & stage1==1,2,
                                     ifelse( COArepairS==1,3,
                                            ifelse(COArepairC==1,4,-999)))))
  
  sub_repairLab<-c("1: COA repair with VSD","2: COA repair plus PA band",
                   "3: COA repair in isolation (surgery)","4: COA repair in isolation (cathether)","-999: NA")
  
  df<-df %>%
    group_by(patid) %>%
    mutate(patCOApathway=max(COApathway))%>%
    ungroup()
  
  #mark first COA repair with VSD. used in later to identity VSD pathway
  df<-df %>%
    arrange(patid,patentry) %>%
    group_by(patid) %>%
    mutate(firstVSDrepair = ifelse(!is.na(which(VSDrepair==1)[1]) & which(VSDrepair==1)[1]==patentry,1,0)) %>%
    mutate(firstCOAVSDrepair = ifelse(!is.na(which(sub_repair==1)[1]) & which(sub_repair==1)[1]==patentry,1,0)) %>%
    ungroup()
  
  df<-df %>%
    mutate(ageatfirstCOAVSDrepair = ifelse(firstCOAVSDrepair==1,ageatop,-999)) 
  
  df<-df %>%
    group_by(patid) %>%
    mutate(ageatfirstCOAVSDrepair =ifelse(max(ageatfirstCOAVSDrepair)>=0,max(ageatfirstCOAVSDrepair),NA)) %>%
    ungroup()
  
  
  #################### assign CatProc for each record
  df<-df %>%
    mutate(CatProc=ifelse(HT==1,6,
                    ifelse(COApathway==1,4,
                           ifelse(VSDrepair==1 & firstVSDrepair==1,5,
                                  ifelse(stage1==1,1,-999)))))

  df$CatProc_raw=df$CatProc
    # mark the first occurrence

  
  df<-df %>%
    arrange(patid,patentry) %>%
    group_by(patid) %>%
    mutate(firstCatProc1 = ifelse(!is.na(which(CatProc==1)[1]) & which(CatProc==1)[1]==patentry,1,0)) %>%
    mutate(firstCatProc4 = ifelse(!is.na(which(CatProc==4)[1]) & which(CatProc==4)[1]==patentry,1,0)) %>%
    mutate(firstCatProc5 = ifelse(!is.na(which(CatProc==5)[1]) & which(CatProc==5)[1]==patentry,1,0)) %>%
    mutate(firstCatProc6 = ifelse(!is.na(which(CatProc==6)[1]) & which(CatProc==6)[1]==patentry,1,0)) %>%
    ungroup()
  
  df$firstCatProc_max<-NULL
  firstCatProc_cols<-df %>% select(starts_with("firstCatProc"))
  df$firstCatProc_max=apply(firstCatProc_cols,1,max)
  
  df<-df %>%
    mutate(CatProc=ifelse(firstCatProc_max==0,-999,CatProc))
  
  # mark more VSD pathway if it happened with COA repair but it was not the first occurrence of COA repair
 
   df<-df %>%
    mutate(CatProc=ifelse(firstVSDrepair==1 & CatProc_raw==4 & firstCatProc_max==0,5,CatProc))
  
  
   
  # age at each pathway 

  
  df$patAge2=df$patAge3=NA
  df<-df %>%
    mutate(patAge1 = ifelse(CatProc==1,ageatop,-999)) %>%
    mutate(patAge4 = ifelse(CatProc==4,ageatop,-999)) %>%
    mutate(patAge5 = ifelse(CatProc==5,ageatop,-999)) %>%
    mutate(patAge6 = ifelse(CatProc==6,ageatop,-999)) 
  
  df<-df %>%
    group_by(patid) %>%
    mutate(patAge1 =ifelse(max(patAge1)>=0,max(patAge1),NA)) %>%
    mutate(patAge4 =ifelse(max(patAge4)>=0,max(patAge4),NA)) %>%
    mutate(patAge5 =ifelse(max(patAge5)>=0,max(patAge5),NA)) %>%
    mutate(patAge6 =ifelse(max(patAge6)>=0,max(patAge6),NA)) %>%
    ungroup()
  
  #check backward sequence. e.g., palliative first stage procedure after reparative procedure will be reintervention 
  
  df<-df %>%
    mutate(CatProc=ifelse(CatProc==1 & ageatop>patAge6 & !is.na(patAge6) & !is.na(ageatop),-999,CatProc))   %>%
    mutate(CatProc=ifelse(CatProc==1 & ageatop>patAge4 & !is.na(patAge4) & !is.na(ageatop),-999,CatProc))   %>%
    mutate(CatProc=ifelse(CatProc==1 & ageatop>patAge5 & !is.na(patAge5) & !is.na(ageatop),-999,CatProc))   %>%
    mutate(CatProc=ifelse(CatProc==4 & ageatop>patAge6 & !is.na(patAge6) & !is.na(ageatop),-999,CatProc))  
  
  
  
  # recompute age
  df<-df %>%
    mutate(patAge1 = ifelse(CatProc==1,ageatop,-999)) %>%
    mutate(patAge4 = ifelse(CatProc==4,ageatop,-999)) %>%
    mutate(patAge5 = ifelse(CatProc==5,ageatop,-999)) %>%
    mutate(patAge6 = ifelse(CatProc==6,ageatop,-999)) 
  
  df<-df %>%
    group_by(patid) %>%
    mutate(patAge1 =ifelse(max(patAge1)>=0,max(patAge1),NA)) %>%
    mutate(patAge4 =ifelse(max(patAge4)>=0,max(patAge4),NA)) %>%
    mutate(patAge5 =ifelse(max(patAge5)>=0,max(patAge5),NA)) %>%
    mutate(patAge6 =ifelse(max(patAge6)>=0,max(patAge6),NA)) %>%
    ungroup()
  
  table(df$CatProc,useNA="ifany")
  table(df$stage1,useNA="ifany")
  table(df$COApathway,useNA="ifany")
  
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
    mutate(auxsub_repair =ifelse(CatProc==4,sub_repair,-999)) 
  
  df<-df %>%
    group_by(patid) %>%
    mutate(patsub_BVpathway =ifelse(max(auxsub_repair)>=0,max(auxsub_repair),-999)) %>%
    ungroup()
  
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
                "4: Reparative procedure" ,"5: VSD pathway","6: Heart transplant" ,
                "7: reintervention", "8: Excluded")
  df$CatProcLabel<-factor(df$CatProc,levels=c(0:8),labels=CatProc_lab,ordered=T)
  
  
  df$ProcSeq=as.character(df$ProcSeq)
  
  df<-df %>%
    arrange(patid, patentry) 
  
  return(df)
}

######################################################################################
#####################COA diagnosis subgroup
######################################################################################
COA_diagnosis_subtype_module<-function(df){
  COA_VSDevidence=read.csv(dir_COA_VSDevidencecodes,colClasses = "character") 
  
  df<-df %>%
    mutate(VSDevidence =ifelse(VSDrepair==1 ,1,0)) 
  
  # search diagnosis filed
  for(i in 1:29){
    shortcode=df[[paste0("diagcode",i)]]
    usingcode=COA_VSDevidence$shortcode
    df=searchforevidence_generic(mark="VSDevidence",usingcode)
  }
  
  # search morbidity filed
  for(i in 1:16){
    shortcode=df[[paste0("comorbidity",i)]]
    usingcode=COA_VSDevidence$shortcode
    df=searchforevidence_generic(mark="VSDevidence",usingcode)
    
  }
  
  
  # search  proc filed
  for(i in 1:7){
    shortcode=df[[paste0("proccode",i)]]
    usingcode=COA_VSDevidence$shortcode
    df=searchforevidence_generic(mark="VSDevidence",usingcode)
  }
  
  # search  prev proc filed
  for(i in 1:26){
    shortcode=df[[paste0("prevproccode",i)]]
    usingcode=COA_VSDevidence$shortcode
    df=searchforevidence_generic(mark="VSDevidence",usingcode)
  }
  
  
  df<-df %>%
    group_by(patid) %>%
    mutate(patVSDevidence=max(VSDevidence)) %>%
    ungroup()
  
  # assign diagnostic subgroup 
  
  df<-df %>%
    mutate(diagsubgroup=ifelse( patVSDevidence==1 ,1,2))
  
  
  
  diagsubgroup_lab=c("1:COA plus VSD","2: Isolated COA")
  df$diagsubgroupLab<-factor(df$diagsubgroup,levels=c(1:2),labels=diagsubgroup_lab,ordered=T)
  
  
  return(df)
}

######################################################################################
#####################COA_specific_violation_module
######################################################################################

COA_specific_violation_module<-function(df){
  
  df<-df %>%
    mutate(pat_violation=ifelse(!is.na(patAge5) & is.na(patAge4) & patHT==0,1,pat_violation)) %>%
    mutate(pat_violationinf=ifelse(!is.na(patAge5) & is.na(patAge4) & patHT==0,paste0(pat_violationinf,"; had VSD closure but no COA repair"),pat_violationinf))
  
  
  return(df)
}
######################################################################################
########## COA specific minor data missing or unusual records
######################################################################################

COA_specific_minor_data_errors_module<-function(df){
  
  #patients who had  VSD repair in isoaltion before CoA repair
  df<-df %>%
    mutate(Flagcentre_minor=ifelse(grepl("54",ProcSeq),1,Flagcentre_minor))  %>%
    mutate(Flagcentre_minorinf=ifelse(grepl("54",ProcSeq), paste0(Flagcentre_minorinf,"; VSD repair in isoaltion before CoA repair"),Flagcentre_minorinf)) 
  
  
  #patients who had PA band with no VSD evidence in records
  df<-df %>%
    mutate(Flagcentre_minor=ifelse(patStage1==1 & patVSDevidence==0 & patHT==0,1,Flagcentre_minor))  %>%
    mutate(Flagcentre_minorinf=ifelse(patStage1==1 & patVSDevidence==0 & patHT==0, paste0(Flagcentre_minorinf,"; had PA band with no VSD evidence in records"),Flagcentre_minorinf)) 
  
  df<-df %>%
    group_by(patid)%>%
    mutate(patVSDrepair=max(VSDrepair)) %>%
    ungroup()
  
  #plus VSD patients had no PA banding & VSD closure in records
  df<-df %>%
    mutate(Flagcentre_minor=ifelse(diagsubgroup==1 & patVSDrepair==0 & patStage1==0 & patHT==0,1,Flagcentre_minor))  %>%
    mutate(Flagcentre_minorinf=ifelse(diagsubgroup==1 & patVSDrepair==0 & patStage1==0 & patHT==0, paste0(Flagcentre_minorinf,"; CoA plus VSD patients had neither PA banding nor VSD closure in records"),Flagcentre_minorinf)) 
  
  
  
  #need to replace minor data missing as 0 if have been flagged  as suspected missing/miscoded
  df<-df %>%
    mutate(Flagcentre_minor=ifelse(Flagcentre_missingdata==1 ,0,Flagcentre_minor))  %>%
    mutate(Flagcentre_minorinf=ifelse(Flagcentre_missingdata==1 ,"",Flagcentre_minorinf))  
  
  return(df)
  
}