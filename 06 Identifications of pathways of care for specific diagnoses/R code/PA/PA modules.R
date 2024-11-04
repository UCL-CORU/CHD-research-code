######################################################################################
#####################PA inclusion and exclusion module
######################################################################################


PA_inclusion_and_exclusion_module<-function(df){
  
  diagcodes1=read.csv(dir_diagcodes1,colClasses = "character") # direct diagnosis evidence of PA
  diagcodes2=read.csv(dir_diagcodes2,colClasses = "character") #indirect diagnosis evidence of PA.  NONE
  proccodes=read.csv(dir_proccodes,colClasses = "character") #procedure  evidence of PA
  exclusioncodes=read.csv(dir_exclusioncodes,colClasses = "character") #PA exclusion codes
  
  #####################define functions used in PA inclusion and exclusion#################################
  # function: search for all evidence used in inclusion and exclusion
  searchforevidence<-function(){
    # diag evidence 1
    usingcode=diagcodes1$shortcode
    for( j in 1:length(usingcode)){
      index=which(shortcode %in% usingcode[j])
      df$PAdiagcodes1[index]=1
    }
    # diag evidence 2
    
    usingcode=diagcodes2$shortcode
    for( j in 1:length(usingcode)){
      index=which(shortcode %in% usingcode[j])
      df$PAdiagcodes2[index]=1
    }
    
    # proc evidence
    usingcode=proccodes$shortcode
    for( j in 1:length(usingcode)){
      index=which(shortcode %in% usingcode[j])
      df$PAproccodes[index]=1
    }
    
    # exclusion 
    usingcode=exclusioncodes$shortcode
    for( j in 1:length(usingcode)){
      index=which(shortcode %in% usingcode[j])
      df$PAexcludecodes[index]=1
    }
    return(df)
  }
  
  
  ######################search evidence in each fields###################################################
  
  # assign variables
  df$PAdiagcodes1=0 # direct diagnosis evidence of PA
  df$PAdiagcodes2=0 #indirect diagnosis evidence of PA.
  df$PAproccodes=0 #procedure  evidence of PA
  df$PAexcludecodes=0 #PA exclusion codes
  
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
  ##mark patients who had  evidence of PA. 
  ###########################################
  df<-df %>%
    group_by(patid) %>%
    mutate(patPAdiagcodes=max(PAdiagcodes1))%>% 
    mutate(patPAproccodes=max(PAproccodes))%>% 
    mutate(patSuggestivePA=max(PAdiagcodes2))%>% 
    ungroup()
  
  ###########################################
  ##mark patients with exclusion code of PA
  ###########################################
  df<-df %>%
    group_by(patid) %>%
    mutate(patPAexcludecodes=max(PAexcludecodes))%>% 
    ungroup()
  
  ###########################################
  ##mark PA patients 
  ###########################################
  df<-df %>%
    mutate(pat_diagPA=ifelse(patPAdiagcodes==1 | patSuggestivePA,1,0))%>% 
    mutate(pat_procPA=ifelse(patPAproccodes==1,1,0))
    

  df<-df %>%
    group_by(patid) %>%
    mutate(PApat=ifelse(( pat_diagPA==1 | pat_procPA==1  ) & patPAexcludecodes==0,1,0))%>% 
    ungroup()
  
  
  return(df)
}
######################################################################################
#####################PA pathway module
# PA  can be  managed by either single ventricle  or biventricular reparative pathway

PA_pathway_module<-function(df){
  
  df<-df %>%
    mutate(ageatopchange=ifelse( is.na(ageatop) & !is.na(ageatdis), 1, 0)) 
  table(df$ageatopchange)
  # replace ageatop as ageatdis if ageatdis not missing, useful to estimate age at procedure as best as we can.
  df<-df %>%
    mutate(ageatop=ifelse(ageatopchange==1, ageatdis, ageatop))
  
  
  SVstage2=read.csv(dir_SVstage2,colClasses = "character") #stage 2 code
  SVstage3=read.csv(dir_SVstage3,colClasses = "character") #stage 3 code
 
  
  ########## identify SV stage 1, 2 and 3
  # search  proc filed
  df$Glenn=0
  df$Fontan=0
  df$V15=0 
  
  df$stage1D=0 
  df$code120618=0 
  
  for(i in 1:7){
    shortcode=df[[paste0("proccode",i)]]
    
    usingcode=c("121014","123103","123104","123106","123130","123146")
    df=searchforevidence_generic(mark="stage1D",usingcode)
    
    df=searchforevidence_generic(mark="code120618",usingcode="120618")
    
    usingcode="120619"
    df=searchforevidence_generic(mark="V15",usingcode)
    
    
    usingcode=SVstage2$shortcode[which(SVstage2$subtype=="Glenn")]
    df=searchforevidence_generic(mark="Glenn",usingcode)
    
    usingcode=SVstage3$shortcode
    df=searchforevidence_generic(mark="Fontan",usingcode)
  }
  
  table(df$Fontan)

  # assign stage 1, 2 and 3 (raw data)
  df<-df %>%
    mutate(stage1 =ifelse(stage1D==1 | code120618==1,1,0)) %>%
    mutate(stage2 =ifelse(Glenn==1 | V15==1,1,0)) %>%
    mutate(stage3 =ifelse(Fontan==1,1,0)) 
  

    table(df$stage1)
    table(df$stage2)
    
  #  subtypes
  df<-df %>%
    mutate(sub_stage1 =ifelse(stage1==1,1,-999)) 

  #stage three refinement: If a patient has two stage 3 without stage 2, the first stage three under 1 year old will be stage two.
  
  
  df<-df %>%
    group_by(patid) %>%
    mutate(patStage1=max(stage1))%>% 
    mutate(patStage2=max(stage2))%>% 
    mutate(patStage3=max(stage3))%>%
    ungroup()
  
  df<-df %>%
    group_by(patid) %>%
    mutate(pat_stage3num=sum(stage3))%>%
    ungroup()
  
  tabpatnum(df$pat_stage3num,df)
  
  df<-df %>%
    arrange(patid,patentry) %>%
    group_by(patid) %>%
    mutate(firststage3 = ifelse(!is.na(which(stage3==1)[1]) & which(stage3==1)[1]==patentry,1,0))%>%
    ungroup()
  
  index_changetostage2=which(df$pat_stage3num>1 & df$firststage3==1 & df$patStage2==0 & df$ageatop<1 & !is.na(df$ageatop))
  
  df$stage3[index_changetostage2]=0
  df$stage2[index_changetostage2]=1
  df$Glenn[index_changetostage2]=1
  
  # recompute
  
  df<-df %>%
    group_by(patid) %>%
    mutate(patStage1=max(stage1))%>% 
    mutate(patStage2=max(stage2))%>% 
    mutate(patStage3=max(stage3))%>%
    ungroup()
  
  
  table(df$stage3)
  
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
  ##########indentify  reparative procedure for PA
  ######################################################################################
  
  #PA  with VSD repair
  df$PArepairVSD=0  
  PArepairVSDcode=c("122801","122811","122821","122822")
  
  #PA repair catheter
  df$PArepairC=0 
  PArepairCcode=c("121309","121305","120605")
  
  
  #Other reparative proc
  df$PArepairOther=0 
  PArepairOthercode=c("122500","121430","122920","122567","122587","122588")
  
  #Reparative  only involving pulmonary valve or pulmonary arteries: surgery
  df$PArepairPVPAS=0 
  PArepairPVPAScode=c("121300","121302","121321","121322","123601","120641","120600","120643","121420","121401","121421","121422","120612","120647")
 
  #incomplete coding  
  df$PArepairIC=0 
  PArepairICcode=c("123600","123640")

  
  df$PArepairexcludeIC=0 
  PArepairexcludeICcode=c("123103","123104","123130","123146") # exclusion code for incompleted coding 
  
  # Fallot type repair   
  df$PArepairFallot=0
  PArepairFallotcode=c("122601","122613","122620","122701") 
  
  # VSD ASD repair   
  df$PArepairvsdasd=0
  PArepairvsdasdcode=c("120101","120102","120103","120106","120110","120122","120801","120802","120803","120807","120828","120816") 
  
  
  
  for(i in 1:7){
    shortcode=df[[paste0("proccode",i)]]
    
    df=searchforevidence_generic(mark="PArepairVSD",PArepairVSDcode)
    df=searchforevidence_generic(mark="PArepairC",PArepairCcode)
    df=searchforevidence_generic(mark="PArepairOther",PArepairOthercode)
    df=searchforevidence_generic(mark="PArepairPVPAS",PArepairPVPAScode)
    df=searchforevidence_generic(mark="PArepairIC",PArepairICcode)
    df=searchforevidence_generic(mark="PArepairexcludeIC",PArepairexcludeICcode)
    df=searchforevidence_generic(mark="PArepairFallot",PArepairFallotcode)
    df=searchforevidence_generic(mark="PArepairvsdasd",PArepairvsdasdcode)
  }
  
  df<-df %>%
    mutate(PArepairIC=ifelse(PArepairexcludeIC==1 ,0,PArepairIC))  
  
  df<-df %>%
    mutate(PApathway=ifelse(
      PArepairVSD==1 | PArepairC==1 | PArepairOther==1 | PArepairPVPAS==1 | PArepairIC==1 
      | PArepairFallot==1 | PArepairvsdasd==1,1,0))  
  
  
  # and PA repair subtypes
  df<-df %>%
    mutate(sub_repair =ifelse(PArepairVSD==1,1,
                              ifelse(PArepairC==1,2,
                                     ifelse(PArepairOther==1,3,
                                            ifelse(PArepairPVPAS==1,4,
                                                   ifelse(PArepairIC==1,5,
                                                          ifelse(PArepairFallot==1,6,
                                                                 ifelse(PArepairvsdasd==1,7,-999)))))))) 
  
  sub_repairLab<-c("1: PA or PA with VSD surgical repair","2: PA repair catheter",
                   "3: Other reparative procedures for PA","4: Surgery: Reparative only involving pulmonary valve or pulmonary arteries","5: Reparative procedures with potentially incomplete coding",
                   "6: Fallot type reparative procedure","7: VSD/ASD closure","-999: NA")
  
  df<-df %>%
    group_by(patid) %>%
    mutate(patPApathway=max(PApathway))%>%
    ungroup()
  
  #indentify stage 2 subtypes.
  
  df<-df %>%
    mutate(sub_stage2 =ifelse(Glenn==1 & PApathway==1,1,
                              ifelse(Glenn==1,2,
                                     ifelse(V15==1,3,-999))))
                                            
  sub_stage2Lab<-c("1: Glenn with reparative procedure","2: Glenn in isolation",
                   "3: 1.5V type repair","-999: NA")                               
  
  #################### assign CatProc for each record 
  df$CatProc=ifelse(df$HT==1,6,
                    ifelse(df$stage3==1,3,
                           ifelse(df$stage2==1,2,
                                ifelse(df$PApathway==1,4,
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
  
  #check backward sequence. e.g., palliative first stage procedure after stage two/three procedure will be reintervention 
  
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
  table(df$PApathway,useNA="ifany")
  
  df<-df %>%
    group_by(patid) %>%
    mutate(ProcSeq = paste(CatProc, collapse="")) %>%
    ungroup()
  
  df$ProcSeq<-str_replace_all(df$ProcSeq,"-999","")
  
  table(df$ProcSeq,useNA="ifany")
  
  table(df$CatProc,useNA="ifany")
  
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
  
  df$patsub_stage2Label<-factor(df$patsub_stage2,levels=c(1:3,-999),labels=sub_stage2Lab,order=T)
  df$patsub_BVpathwayLabel<-factor(df$patsub_BVpathway,levels=c(1:7,-999),labels=sub_repairLab,ordered=T)
  
  
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
                "4: Reparative procedure" ,"5: Other pathway (none in PA)","6: Heart transplant" ,
                "7: reintervention", "8: Excluded")
  df$CatProcLabel<-factor(df$CatProc,levels=c(0:8),labels=CatProc_lab,ordered=T)
  
  # mark 1.5 V patients  
  
  df<-df %>%
    mutate(pat15V =ifelse(!is.na(patAge2) & !is.na(patAge4) & is.na(patAge3) ,1,0)) 
  
  
  df$ProcSeq=as.character(df$ProcSeq)
  
  df<-df %>%
    arrange(patid, patentry) 
  return(df)
}


######################################################################################
#####################PA diagnosis subgroup
######################################################################################
PA_diagnosis_subtype_module<-function(df){
  
  PAVSDcodes=read.csv(dir_PAVSD,colClasses = "character") 
  
  df$VSDcode=0
  df$IVScode=0
  # search diagnosis filed
  for(i in 1:29){
    shortcode=df[[paste0("diagcode",i)]]
    
    usingcode=PAVSDcodes$shortcode
    df=searchforevidence_generic(mark="VSDcode",usingcode)
    
    usingcode=c("010107" ,"060101" ,"072100")
    df=searchforevidence_generic(mark="IVScode",usingcode)
  }
  
  # search morbidity filed
  for(i in 1:16){
    shortcode=df[[paste0("comorbidity",i)]]
    
    usingcode=PAVSDcodes$shortcode
    df=searchforevidence_generic(mark="VSDcode",usingcode)
    
    usingcode=c("010107" ,"060101" ,"072100")
    df=searchforevidence_generic(mark="IVScode",usingcode)
  }
  
  
  # search  proc filed
  for(i in 1:7){
    shortcode=df[[paste0("proccode",i)]]
    
    usingcode=PAVSDcodes$shortcode
    df=searchforevidence_generic(mark="VSDcode",usingcode)
    
    usingcode=c("010107" ,"060101" ,"072100")
    df=searchforevidence_generic(mark="IVScode",usingcode)
  }
  
  # search  prev proc filed
  for(i in 1:26){
    shortcode=df[[paste0("prevproccode",i)]]
    
    usingcode=PAVSDcodes$shortcode
    df=searchforevidence_generic(mark="VSDcode",usingcode)
    
    usingcode=c("010107" ,"060101" ,"072100")
    df=searchforevidence_generic(mark="IVScode",usingcode)
    
  }
  
  
  df<-df %>%
    group_by(patid) %>%
    mutate(patVSDcode=max(VSDcode)) %>%
    mutate(patIVScode=max(IVScode)) %>%
    ungroup()
  
  
  # assign diagnostic subgroup 
  
  df<-df %>%
    mutate(diagsubgroup=ifelse( patVSDcode==1 ,1,2))
  
  diagsubgroup_lab=c("1: PA VSD (including DORV and Fallot MAPCA)","2: PA IVS")
  df$diagsubgroupLab<-factor(df$diagsubgroup,levels=c(1:2),labels=diagsubgroup_lab,ordered=T)
  
  
  return(df)
}
######################################################################################
########## PA specific minor data missing or unusual records
######################################################################################

PA_specific_minor_data_errors_module<-function(df){
  
  #if BV, had no reparative proc by 3 years old
 
  df<-df %>%
    mutate(Flagcentre_minor=ifelse((is.na(patAge4) | patAge4>3) & is.na(patAge2) & is.na(patAge3) & ageatlastknownstatus>3,1,Flagcentre_minor))  %>%
    mutate(Flagcentre_minorinf=ifelse((is.na(patAge4) | patAge4>3) & is.na(patAge2) & is.na(patAge3) & ageatlastknownstatus>3, paste0(Flagcentre_minorinf,"; No reparative proc by 3 years old"),Flagcentre_minorinf)) 
  
  
  #PA IVS patients without a relevant code
  df<-df %>%
    mutate(Flagcentre_minor=ifelse(diagsubgroup==2 & patIVScode==0 ,1,Flagcentre_minor))  %>%
    mutate(Flagcentre_minorinf=ifelse( diagsubgroup==2 & patIVScode==0 , paste0(Flagcentre_minorinf,"; PA IVS patients without relevant code"),Flagcentre_minorinf)) 
  
  #PA patients had atypical/incomplete coding in reparative proc
  df<-df %>%
    group_by(patid) %>%
    mutate(pataux=max(CatProc==4 & patsub_BVpathway %in% c(5,6,7))) %>%
    ungroup()
  
  df<-df %>%
    mutate(Flagcentre_minor=ifelse(pataux==1,1,Flagcentre_minor))  %>%
    mutate(Flagcentre_minorinf=ifelse( pataux==1, paste0(Flagcentre_minorinf,"; PA pathway incomplete/poor coding"),Flagcentre_minorinf)) 
  
  #patients who had Stent placement in RVOT as palliative first stage operation
  df<-df %>%
    group_by(patid) %>%
    mutate(pataux=max(CatProc==1 & code120618==1 & stage1D==0)) %>%
    ungroup()
  
  df<-df %>%
    mutate(Flagcentre_minor=ifelse(pataux==1,1,Flagcentre_minor))  %>%
    mutate(Flagcentre_minorinf=ifelse(pataux==1, paste(Flagcentre_minorinf,"; Stent placement in RVOT as palliative first stage operation"),Flagcentre_minorinf)) 
  
  
  #need to replace minor data missing as 0 if have been flagged  as suspected missing/miscoded
  df<-df %>%
    mutate(Flagcentre_minor=ifelse(Flagcentre_missingdata==1 ,0,Flagcentre_minor))  %>%
    mutate(Flagcentre_minorinf=ifelse(Flagcentre_missingdata==1 ,"",Flagcentre_minorinf))  
  
  return(df)
  
}
