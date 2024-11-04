

######################################################################################
######################HLHS inclusion and exclusion
######################################################################################

HLHS_inclusion_and_exclusion_module<-function(df){
  
  diagcodes1=read.csv(dir_diagcodes1,colClasses = "character") # direct diagnosis evidence of HLHS
  diagcodes2=read.csv(dir_diagcodes2,colClasses = "character") #indirect diagnosis evidence of HLHS.
  proccodes=read.csv(dir_proccodes,colClasses = "character") #procedure  evidence of HLHS
  exclusioncodes=read.csv(dir_exclusioncodes,colClasses = "character") #HLHS exclusion codes
  
  #####################define functions used in HLHS inclusion and exclusion#################################
  # function: search for all evidence used in inclusion and exclusion
  searchforevidence<-function(){
    # diag evidence 1
    usingcode=diagcodes1$shortcode
    for( j in 1:length(usingcode)){
      index=which(shortcode %in% usingcode[j])
      df$HLHSdiagcodes1[index]=1
    }
    
    # diag evidence 2
    usingcode=diagcodes2$shortcode
    for( j in 1:length(usingcode)){
      index=which(shortcode %in% usingcode[j])
      df$HLHSdiagcodes2[index]=1
    }
    
    # proc evidence
    usingcode=proccodes$shortcode
    for( j in 1:length(usingcode)){
      index=which(shortcode %in% usingcode[j])
      df$HLHSproccodes[index]=1
    }
    
    # exclusion 
    usingcode=exclusioncodes$shortcode
    for( j in 1:length(usingcode)){
      index=which(shortcode %in% usingcode[j])
      df$HLHSexcludecodes[index]=1
    }
    return(df)
  }
  
  
  ######################search evidence in each fields###################################################
  
  # assign variables
  df$HLHSdiagcodes1=0 # direct diagnosis evidence of HLHS
  df$HLHSdiagcodes2=0 #indirect diagnosis evidence of HLHS.
  df$HLHSproccodes=0 #procedure  evidence of HLHS
  df$HLHSexcludecodes=0 #HLHS exclusion codes
  
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
  

  
  # further identify procedure evidence: hybrid procedure by two parts of intervention within 4 weeks
  Stentcode="121014"
  Bandcode="121419"
  
  df$Stent=0
  df$Band=0
  
  for(i in 1:7){
    shortcode=df[[paste0("proccode",i)]]
    df$Stent[which(shortcode %in% Stentcode)]=1
    df$Band[which(shortcode %in% Bandcode)]=1
  }
  

  
  df<-df %>%
    group_by(patid) %>%
    mutate(numStent=sum(Stent)) %>%
    mutate(numBand=sum(Band))
  
  tabpatnum(df$numStent)
  tabpatnum(df$numBand)
  
  df$ageatop_Stent<-ifelse(df$Stent==1,df$ageatop,-999)
  df$ageatop_Band<-ifelse(df$Band==1,df$ageatop,-999)
  
  df<-df %>%
    group_by(patid) %>%
    mutate(patage_Stent=max(ageatop_Stent, na.rm=T)) %>%
    mutate(patage_Band=max(ageatop_Band, na.rm=T)) 
  
  
  df<-df %>%
    group_by(patid) %>%
    mutate(auxHybrid=ifelse((abs(patage_Stent-patage_Band) <= 4/52.1775 & (Stent==1 | Band==1)),1,0))
  
  
  # manually check/refine the case if more than one stent or band. maybe have better method to do it
  df$auxHybrid[which(df$launchesrecid %in% c("RSZLIXRD","RWUKVKHS"))]=0
  df$HLHSproccodes[which(df$launchesrecid %in% c("RSZLIXRD","RWUKVKHS"))]=0
  

  # further identify procedure evidence: Norwood procedure by the combination of 2 codes: DK and Comb_DK (shunt)
  DKcode="120903"
  Comb_DKcode=c("123103","123104","123106","123146","123130")
  
  df$DK=0
  df$Comb_DK=0
  
  for(i in 1:7){
    shortcode=df[[paste0("proccode",i)]]
    
    df$DK[which(shortcode %in% DKcode)]=1
    for(j in 1:length(Comb_DKcode)){
      df$Comb_DK[which(shortcode %in% Comb_DKcode[j])]=1
    }
  }
  

  df$HLHSproccodes[which(df$Comb_DK ==1 & df$DK==1)]=1
  
  
  #mark patients who had diagnosis evidence of HLHS, who had procedure evidence of HLHS and who had exclusion codes for HLHS
  
  df<-df %>%
    group_by(patid) %>%
    mutate(patdiagHLHScodes=max(HLHSdiagcodes1))%>% 
    mutate(patSuggestiveHLHS=max(HLHSdiagcodes2))%>% 
    mutate(patHLHSproccodes=max(HLHSproccodes))%>% 
    mutate(patHLHSexcludecodes=max(HLHSexcludecodes))
  
  df$pat_diagHLHS<-ifelse(df$patdiagHLHScodes==1 |df$patSuggestiveHLHS==1 ,1,0)
  df$HLHSpat<-ifelse(((df$pat_diagHLHS==1  | df$pat_procHLHS==1) & df$patHLHSexcludecodes==0),1,0)
  checkpatnum(df$HLHSpat,df)
  
  checkpatnum(df$HLHSpat,df)
  return(df)
}


######################################################################################
#####################HLHS pathway module
######################################################################################
# HLHS is exclusively single ventricle  

HLHS_pathway_module<-function(df){
  
  df<-df %>%
    mutate(ageatopchange=ifelse( is.na(ageatop) & !is.na(ageatdis), 1, 0)) 
  table(df$ageatopchange)
  # replace ageatop as ageatdis if ageatdis not missing, useful to estimate age at procedure as best as we can.
  df<-df %>%
    mutate(ageatop=ifelse( ageatopchange==1, ageatdis, ageatop))
  
  stage1=read.csv(dir_stage1,colClasses = "character") #stage 1 code
  SVstage2=read.csv(dir_SVstage2,colClasses = "character") #stage 2 code
  SVstage3=read.csv(dir_SVstage3,colClasses = "character") #stage 3 code
  

  ########## identify SV stage 1, 2 and 3
  # search  proc filed
  df$stage1A=0 
  df$stage1C_1=0 # by single hybrid proc
  df$stage1C_2=0 # by combination of stent and band
  df$CompStage2=0
  df$Glenn=0
  df$Fontan=0
  
  for(i in 1:7){
    shortcode=df[[paste0("proccode",i)]]
    
    usingcode=stage1$shortcode[which(stage1$subtype=="A")]
    df=searchforevidence_generic(mark="stage1A",usingcode)
    
    usingcode=stage1$shortcode[which(stage1$subtype=="C")]
    df=searchforevidence_generic(mark="stage1C_1",usingcode)
    
    usingcode=SVstage2$shortcode[which(SVstage2$subtype=="CompStage2")]
    df=searchforevidence_generic(mark="CompStage2",usingcode)
    
    usingcode=SVstage2$shortcode[which(SVstage2$subtype=="Glenn")]
    df=searchforevidence_generic(mark="Glenn",usingcode)
    
    
    usingcode=SVstage3$shortcode
    df=searchforevidence_generic(mark="Fontan",usingcode)
  }
  

  # further identify hybrid procedure (stage 1 C) by two parts of intervention within 4 weeks
  # now use a wider criteria because patients have been selected
  
  stentcode=c("121014","124511")
  bandcode=c("121419","121402")
  
  df$stent=0
  df$band=0
  
  for(i in 1:7){
    shortcode=df[[paste0("proccode",i)]]
    df=searchforevidence_generic(mark="stent",stentcode)
    df=searchforevidence_generic(mark="band",bandcode)
  }
  
  df<-df %>%
    group_by(patid) %>%
    mutate(numstent=sum(stent)) %>%
    mutate(numband=sum(band)) %>%
    ungroup()
  
  tabpatnum(df$numstent,df)
  tabpatnum(df$numband,df)
  
  df$ageatop_stent<-ifelse(df$stent==1,df$ageatop,-999)
  df$ageatop_band<-ifelse(df$band==1,df$ageatop,-999)
  
  df<-df %>%
    group_by(patid) %>%
    mutate(patage_stent=max(ageatop_stent, na.rm=T)) %>%
    mutate(patage_band=max(ageatop_band, na.rm=T)) %>%
    ungroup()
  
  
  df<-df %>%
    group_by(patid) %>%
    mutate(auxhybrid=ifelse((abs(patage_stent-patage_band) <= 4/52.1775 & (stent==1 | band==1) & 
                               numband==1 & numstent==1) ,1,0))%>%
    ungroup()
  
  
  # manually check/refine the case if more than one stent or band if needed
  
  
  df$stage1C_2[which(df$auxhybrid==1)]=1
  
  df<-df %>%
    mutate(stage1C =ifelse(stage1C_1==1|stage1C_2==1,1,0)) 
  table(df$stage1C)
  table(df$stage1C_1)
  table(df$stage1C_2)
  
  # assign stage 1, 2 and 3 (raw data)
  df<-df %>%
    mutate(stage1 =ifelse((stage1A==1 | stage1C==1),1,0))%>%
    mutate(stage2 =ifelse((CompStage2==1 | Glenn==1),1,0)) %>%
    mutate(stage3 =ifelse(Fontan==1,1,0)) 
  
  table(df$CompStage2)
  table(df$Glenn)
  

  # and subtypes
  df<-df %>%
    mutate(sub_stage1 =ifelse(stage1A==1,1,ifelse(stage1C==1,3,-999))) %>%
    mutate(sub_stage2 =ifelse((CompStage2==1 | (stage2==1 & stage1A==1)),1,ifelse(stage2==1,2,-999))) 
  
  table(df$sub_stage1)
  table(df$sub_stage2)
  
  
  sub_stage1Lab<-c("1: Norwood/sano/Damus","2: Coarctation/interrupted arch repair",
                   "3: HLHS hybrid","4: Securing pulmonary blood flow","5: PA banding","-999: NA")
  sub_stage2Lab<-c("1: Comprehensive Stage2","2: Glenn","-999: NA")
  
  
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
    group_by(patid) %>%
    arrange(patentry,.by_group=T) %>%
    mutate(firststage3 = ifelse(!is.na(which(stage3==1)[1]) & which(stage3==1)[1]==patentry,1,0))%>%
    ungroup()
  
  index_changetostage2=which(df$pat_stage3num>1 & df$firststage3==1 & df$patStage2==0 & df$ageatop<1 & !is.na(df$ageatop))
  df$stage2[index_changetostage2]=1
  df$sub_stage2[index_changetostage2]=2
  df$stage3[index_changetostage2]=0
  
  #stage 3 refinement. : if a patient has no stage three but it has the code "123027: Fenestration of Fontan type connection", then this indicates that a stage three occurred. 
  
  df$code123027=0
  for(i in 1:7){
    shortcode=df[[paste0("proccode",i)]]
    df=searchforevidence_generic(mark="code123027","123027")
  }
  index_changetostage3=which(df$code123027==1 & df$patStage3==0)
  df$stage3[index_changetostage3]=1
  
  
  # recompute
  
  df<-df %>%
    group_by(patid) %>%
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
  
  checkpatnum(df$patHT,df)
  
  
  #################### assign CatProc for each record 
  df$CatProc=ifelse(df$HT==1,6,
                    ifelse(df$stage3==1,3,
                           ifelse(df$stage2==1,2,
                                  ifelse(df$stage1==1,1,-999))))
  df$CatProc_raw=df$CatProc
  
  # mark the first occurrence
  
  
  df<-df %>%
    group_by(patid) %>%
    arrange(patentry,.by_group=T) %>%
    mutate(firstCatProc1 = ifelse(!is.na(which(CatProc==1)[1]) & which(CatProc==1)[1]==patentry,1,0)) %>%
    mutate(firstCatProc2 = ifelse(!is.na(which(CatProc==2)[1]) & which(CatProc==2)[1]==patentry,1,0)) %>%
    mutate(firstCatProc3 = ifelse(!is.na(which(CatProc==3)[1]) & which(CatProc==3)[1]==patentry,1,0)) %>%
    mutate(firstCatProc6 = ifelse(!is.na(which(CatProc==6)[1]) & which(CatProc==6)[1]==patentry,1,0)) %>%
    ungroup()
  
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
    mutate(patAge6 = ifelse(CatProc==6,ageatop,-999)) 
  
  df<-df %>%
    group_by(patid) %>%
    mutate(patAge1 =ifelse(max(patAge1)>=0,max(patAge1),NA)) %>%
    mutate(patAge2 =ifelse(max(patAge2)>=0,max(patAge2),NA)) %>%
    mutate(patAge3 =ifelse(max(patAge3)>=0,max(patAge3),NA)) %>%
    mutate(patAge6 =ifelse(max(patAge6)>=0,max(patAge6),NA)) %>%
    ungroup()
  
  #check backward sequence. e.g., palliative first stage procedure after stage two/three procedure will be reintervention 
  
  df<-df %>%
    mutate(CatProc=ifelse(CatProc==1 & ageatop>patAge2 & !is.na(patAge2) & !is.na(ageatop),-999,CatProc))   %>%
    mutate(CatProc=ifelse(CatProc==1 & ageatop>patAge3 & !is.na(patAge3) & !is.na(ageatop),-999,CatProc))   %>%
    mutate(CatProc=ifelse(CatProc==1 & ageatop>patAge6 & !is.na(patAge6) & !is.na(ageatop),-999,CatProc))   %>%
    mutate(CatProc=ifelse(CatProc==2 & ageatop>patAge3 & !is.na(patAge3) & !is.na(ageatop),-999,CatProc))  
  
  
  
  # recompute age
  df<-df %>%
    mutate(patAge1 = ifelse(CatProc==1,ageatop,-999)) %>%
    mutate(patAge2 = ifelse(CatProc==2,ageatop,-999)) %>%
    mutate(patAge3 = ifelse(CatProc==3,ageatop,-999)) %>%
    mutate(patAge6 = ifelse(CatProc==6,ageatop,-999)) 
  
  
  df<-df %>%
    group_by(patid) %>%
    mutate(patAge1 =ifelse(max(patAge1)>=0,max(patAge1),NA)) %>%
    mutate(patAge2 =ifelse(max(patAge2)>=0,max(patAge2),NA)) %>%
    mutate(patAge3 =ifelse(max(patAge3)>=0,max(patAge3),NA)) %>%
    mutate(patAge6 =ifelse(max(patAge6)>=0,max(patAge6),NA)) %>%
    ungroup()
  
  
  df<-df %>%
    arrange(patid,patentry) %>%
    group_by(patid) %>%
    mutate(ProcSeq = paste(CatProc, collapse="")) %>%
    ungroup()
  df$ProcSeq<-str_replace_all(df$ProcSeq,"-999","")
  
  table(df$ProcSeq,useNA="ifany")
  
  
  # adjust the patentry order
  
  df<-df %>%
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
  
  # manually change, 1 with missing age at operation data
  index_CatProcmanuallychange=which(df$ProcSeq=="231" & df$CatProc==1)
  df$CatProc[index_CatProcmanuallychange] =-999
  
  index_manuallychange=which(df$ProcSeq=="231")
  df$ProcSeq[index_manuallychange] ="23"
  
  table(df$ProcSeq,useNA="ifany")
  #mark sub type of treatment pathway in patient level
  
  df<-df %>%
    mutate(sub_stage1 =ifelse(stage1A==1,1,ifelse(stage1C==1,3,-999))) %>%
    mutate(sub_stage2 =ifelse((CompStage2==1 | (stage2==1 & stage1A==1)),1,ifelse(stage2==1,2,-999))) 
  
  df<-df %>%
    mutate(auxsub_stage1 =ifelse(CatProc==1,sub_stage1,-999)) %>%
    mutate(auxsub_stage2 =ifelse(CatProc==2,sub_stage2,-999)) %>%
    mutate(auxhybridbycomb =ifelse(CatProc==1 & sub_stage1==3 & stage1C_1==0 & stage1C_2==1 ,1,-999))
  
  df<-df %>%
    group_by(patid) %>%
    mutate(patsub_stage1 =ifelse(max(auxsub_stage1)>=0,max(auxsub_stage1),-999)) %>%
    mutate(patsub_stage2 =ifelse(max(auxsub_stage2)>=0,max(auxsub_stage2),-999)) %>%
    mutate(pathybridbycomb =ifelse(max(auxhybridbycomb)>=0,max(auxhybridbycomb),-999)) %>%
    ungroup()
  
  df$patsub_stage1Label<-factor(df$patsub_stage1,levels=c(1:5,-999),labels=sub_stage1Lab,order=T)
  
  df$patsub_stage2Label<-factor(df$patsub_stage2,levels=c(1:2,-999),labels=sub_stage2Lab,ordered=T)
  
  
  # mark reintervention and pre-pathway procedures
  df$minagepathway=apply(df[,grep("^patAge",names(df))],1,function(x) min(x,na.rm=T))
  df$minagepathway[is.infinite(df$minagepathway)]=NA
  subset=df[1:100,c("patid","patentry","patAge1","patAge2","patAge3","patAge6","minagepathway")]
  
  
  
  df<-df %>%
    mutate(CatProc=ifelse(CatProc==-999 & auxhybrid==1 & pathybridbycomb==1,5,CatProc)) %>% 
    mutate(CatProc=ifelse(CatProc==-999 & InterType %in% c(1,2) & is.na(minagepathway),0,CatProc))   %>%
    mutate(CatProc=ifelse(CatProc==-999 & InterType %in% c(1,2) & ageatop<minagepathway & !is.na(minagepathway) & !is.na(ageatop),0,CatProc))   %>%
    mutate(CatProc=ifelse(CatProc==-999 & InterType %in% c(1,2) ,7,CatProc))   %>%
    mutate(CatProc=ifelse(CatProc==-999 & InterType %in% c(3) ,8,CatProc)) 
  
  # pathway marker
  df<-df %>%
    mutate(ispathway =ifelse(CatProc %in% c(1,2,3,5),1,0)) 
  
  
  # add Labels
  # 
  # sub_stage1Lab<-c("1: Norwood/sano/Damus","2: Coarctation/interrupted arch repair",
  #                  "3: HLHS hybrid","4: Securing pulmonary blood flow","5: PA banding","-999: NA")
  # sub_stage2Lab<-c("1: Comprehensive Stage2","2: Glenn","-999: NA")
  # 
  df$patsub_stage1Label<-factor(df$patsub_stage1,levels=c(1:5,-999),labels=sub_stage1Lab,order=T)
  df$patsub_stage2Label<-factor(df$patsub_stage2,levels=c(1:2,-999),labels=sub_stage2Lab,ordered=T)
  
  
  CatProc_lab=c("0: Pre-pathway","1: Palliative first stage procedure","2: SV stage two","3: SV stage three (Fontan)",
                "4: Reparative procedure" ,"5: Other pathway (part of hybrid)","6: Heart transplant" ,
                "7: reintervention", "8: Excluded")
  df$CatProcLabel<-factor(df$CatProc,levels=c(0:8),labels=CatProc_lab,ordered=T)
  
  
  
  
  table(df$CatProcLabel)
  summary(df$CatProc)
  
  
  if(CHDtype=="HLHS"){df$patAge4=NA}
  df$ProcSeq=as.character(df$ProcSeq)
  
  df<-df %>%
    arrange(patid, patentry) 
  return(df)
}

######################################################################################
#####################HLHS diagnosis subgroup
######################################################################################

HLHS_diagnosis_subtype_module<-function(df){
  df$diagsubgroup=1 # HLHS only has 1 subgroup (HLHS)
  
  diagsubgroup_lab=c("1: HLHS")
  df$diagsubgroupLab<-factor(df$diagsubgroup,levels=c(1),labels=diagsubgroup_lab,ordered=T)
  
  return(df)
}



######################################################################################
#####################HLHS_specific_violation_module
######################################################################################

HLHS_specific_violation_module<-function(df){

  #HLHS  extra exclusion step c) and d) must be done after pathway analysis. here put it together with  violation rule
  
  #exclusion c)	Exclusion of patients with small left heart who go down a biventricular pathway
  stage1Bcode=c("121800","121801" ,"121802","121803","121804",
                "121810","121815" ,"121827","121830","122100")
  df$stage1B=0
  for(i in 1:7){
    shortcode=df[[paste0("proccode",i)]]
    df=searchforevidence_generic(mark="stage1B",stage1Bcode)
  }
  
  table(df$stage1B,useNA="ifany")
  
  df<-df %>%
    group_by(patid) %>%
    mutate(patstage1B=max(stage1B)) %>%
    ungroup
  
  df<-df %>%
    mutate(HLHSextraexclusion=ifelse((patstage1B==1&!(patsub_stage1==1 | !is.na(patAge2) | !is.na(patAge3))),1,0) )
  
  
  # exclusion d)	Exclusion of patients who are selected by diagnosis code 070700 Left ventricular hypoplasia only and have no procedures in HLHS pathway 
  
  otherdiagcode_code=c("010109","091503" ,"060201","091506","060202",
                       "060226","070842" )
  df$code070700=0
  df$otherdiagcode=0
  
  for(i in 1:29){
    shortcode=df[[paste0("diagcode",i)]]
    df=searchforevidence_generic(mark="code070700","070700")
    df=searchforevidence_generic(mark="otherdiagcode",otherdiagcode_code)
  }
  for(i in 1:16){
    shortcode=df[[paste0("comorbidity",i)]]
    df=searchforevidence_generic(mark="code070700","070700")
    df=searchforevidence_generic(mark="otherdiagcode",otherdiagcode_code)
  }
  for(i in 1:7){
    shortcode=df[[paste0("proccode",i)]]
    df=searchforevidence_generic(mark="code070700","070700")
    df=searchforevidence_generic(mark="otherdiagcode",otherdiagcode_code)
  }
  for(i in 1:26){
    shortcode=df[[paste0("prevproccode",i)]]
    df=searchforevidence_generic(mark="code070700","070700")
    df=searchforevidence_generic(mark="otherdiagcode",otherdiagcode_code)
  }
  
  
  df<-df %>%
    group_by(patid) %>%
    mutate(pat070700=max(code070700)) %>%
    mutate(patotherdiagcode=max(otherdiagcode)) %>%
    ungroup()
  
  df$ProcSeq=as.character(df$ProcSeq)
  
  df<-df %>%
    mutate(patcode070700only=ifelse(pat070700==1 & patotherdiagcode==0 & patHLHSproccodes==0 & ProcSeq=="",1,0)) %>%
    mutate(HLHSextraexclusion=ifelse(patcode070700only==1,1,HLHSextraexclusion) ) 
  
  table(df$patcode070700only,useNA="ifany")
  
  #exclusion e)	Exclusion of patients who had pre pathway only and an ASD closure code
  df$ASD=0
  for(i in 1:7){
    shortcode=df[[paste0("proccode",i)]]
    df=searchforevidence_generic(mark="ASD",c("120102","120103","120108","120189"))
  }
  df<-df %>%
    group_by(patid) %>%
    mutate(patASD=max(ASD)) %>%
    ungroup
  
  df<-df %>%
    mutate(HLHSextraexclusion=ifelse(patASD==1 & ProcSeq=="",1,HLHSextraexclusion) )  
  
  checkpatnum(df$HLHSextraexclusion,df)
  table(df$HLHSextraexclusion,useNA="ifany")
  
  #HLHS specific violation rule
  indexpat_violation1=which(df$pat_violation==1)
  #Patients who had no stage one for HLHS (Norwood or hybrid) by age three months and are surviving, except all those who had partial hybrid code (only allow the more specific codes here, 121419 application of bilateral bands OR 121014 stent of duct), or stage one type B Table F in place of stage one before the age of three months. 
  df<-df %>%
    group_by(patid) %>%
    mutate(pataux1=max(stage1B==1 & ageatop<1/4 & !is.na(ageatop))) %>%
    mutate(pataux2=max((Stent==1 & ageatop<1/4 & !is.na(ageatop)) |  (Band==1 & ageatop<1/4 & !is.na(ageatop)))) %>%
    ungroup()
  table(df$pataux2,useNA="ifany")
  
  df$pat_violation2=0
  df<-df %>%
    mutate(pat_violation2=ifelse(is.na(patAge1) & ageatlastknownstatus>=1/4 &   HLHSextraexclusion==0 & pataux1==0 & pataux2==0,1,pat_violation2) ) %>%
    mutate(pat_violation2=ifelse(!is.na(patAge1) & patAge1>=1/4 &   HLHSextraexclusion==0 & pataux1==0 & pataux2==0,1,pat_violation2) ) %>%
    mutate(pat_violation2=ifelse(is.na(patAge1) & !is.na(patAge2) &   patsub_stage2==2 & patAge2<1/4  & pataux1==0 & pataux2==0,1,pat_violation2) ) %>%
    mutate(pat_violation2=ifelse(!is.na(patAge6) & patAge6<1/4 ,0,pat_violation2) )  # allow heart transplant before age 3 months
  
  
  df<-df %>%
    mutate(pat_violation=ifelse(pat_violation2==1,1,pat_violation))
  
  aux=which(df$pat_violation2==1)
  df$pat_violationinf[aux]=paste(df$pat_violationinf[aux],";no stage HLHS one (or skipped stage one) by 3 months old whilst surviving")
  
  return(df)
}



######################################################################################
########## HLHS specific suspected  missing/miscoded data module
######################################################################################
#	Patients who had a stage one for HLHS (Norwood or hybrid) and no stage two/three by 18 months old whilst surviving.
HLHS_specific_suspected_missing_miscoded_data_module<-function(df){
  df<-df %>%
    mutate(Flagcentre_missingdata=ifelse( !is.na(patAge1) & !(patAge2<1.5 & !is.na(patAge2)) &   ageatlastknownstatus>=1.5 &  HLHSextraexclusion==0 & !(patAge6<1.5 & !is.na(patAge6) ),1,Flagcentre_missingdata))  %>%
    mutate(Flagcentre_missingdatainf=ifelse( !is.na(patAge1) & !(patAge2<1.5 & !is.na(patAge2)) &   ageatlastknownstatus>=1.5 &  HLHSextraexclusion==0 & !(patAge6<1.5 & !is.na(patAge6) ),paste(Flagcentre_missingdatainf,"; had stage one but no stage two/three by 18 months old and surviving"),Flagcentre_missingdatainf))  
  
  #	Patients who had a stage one (Norwood or hybrid) for HLHS and stage three, but no stage two by any age.
  df<-df %>%
    mutate(Flagcentre_missingdata=ifelse( !is.na(patAge1) & is.na(patAge2) & !is.na(patAge3) & pat_violation==0 & HLHSextraexclusion==0 ,1,Flagcentre_missingdata))  %>%
    mutate(Flagcentre_missingdatainf=ifelse( !is.na(patAge1) & is.na(patAge2) & !is.na(patAge3) & pat_violation==0 & HLHSextraexclusion==0 ,paste(Flagcentre_missingdatainf,";patients skipped stage two"),Flagcentre_missingdatainf))  
  
  
  
  
  #Patients who had a stage one (Norwood or hybrid) for HLHS and multiple two stage two but no stage three by surviving age more than 8 years old.
  df<-df %>%
    group_by(patid) %>%
    mutate(pat_stage2num=sum(stage2))%>%
    ungroup()
  
  df<-df %>%
    mutate(stage2redo=ifelse(pat_stage2num>1,1,0))%>%
    mutate(Flagcentre_missingdata=ifelse( !is.na(patAge1) & stage2redo==1 & !( !is.na(patAge3) & patAge3<8) &  pat_violation==0 & HLHSextraexclusion==0 & ageatlastknownstatus>8 & !(patAge6<8 & !is.na(patAge6) ) ,1,Flagcentre_missingdata))  %>%
    mutate(Flagcentre_missingdatainf=ifelse(!is.na(patAge1) & stage2redo==1 & !( !is.na(patAge3) & patAge3<8) &  pat_violation==0 & HLHSextraexclusion==0 & ageatlastknownstatus>8 & !(patAge6<8 & !is.na(patAge6) ),paste(Flagcentre_missingdatainf,";patients had multiple stage two but no stage three by 8 years old"),Flagcentre_missingdatainf))  
  
  
  #Patients who had a possibly incompletely coded hybrid (only allow the more specific hybrid codes here)- one of these codes 121419 application of bilateral bands OR 121014 stent of duct, before age three months, followed by stages two and or three depending on age of follow up. 
  
  df<-df %>%
    mutate(Flagcentre_missingdata=ifelse( ((ageatop_Stent<0.25 & ! is.na(ageatop_Stent)) | (ageatop_Band<0.25 & ! is.na(ageatop_Band))) & is.na(patAge1) & (!is.na(patAge2) | !is.na(patAge3)) &  pat_violation==0 & HLHSextraexclusion==0  ,1,Flagcentre_missingdata))  %>%
    mutate(Flagcentre_missingdatainf=ifelse(((ageatop_Stent<0.25 & ! is.na(ageatop_Stent)) | (ageatop_Band<0.25 & ! is.na(ageatop_Band))) & is.na(patAge1) & (!is.na(patAge2) | !is.na(patAge3)) &  pat_violation==0 & HLHSextraexclusion==0 ,paste(Flagcentre_missingdatainf,";possibly incompletely coded hybrid before 3 months followed by stage two or three"),Flagcentre_missingdatainf))  
  
  
  #Patients who had a stage one as type B table F followed by stages two and or three depending on age of follow up. 
  df<-df %>%
    mutate(aux=ifelse(stage1B==1 & !is.na(ageatop) & ageatop<0.25,1,0)) %>%
    mutate(aux=ifelse((ageatop>patAge2 & !is.na(patAge2)) | (ageatop>patAge3 & !is.na(patAge3)),0,aux))
  
  df<-df %>%
    group_by(patid) %>%
    mutate(pataux=max(aux)) %>%
    ungroup()
  
  
  df<-df %>%
    mutate(Flagcentre_missingdata=ifelse( is.na(patAge1) &  pataux==1 &  pat_violation==0 & HLHSextraexclusion==0 ,1,Flagcentre_missingdata))  %>%
    mutate(Flagcentre_missingdatainf=ifelse( is.na(patAge1) &  pataux==1 &  pat_violation==0 & HLHSextraexclusion==0  ,paste(Flagcentre_missingdatainf,"; stage one as type B followed by stage two or three"),Flagcentre_missingdatainf))  
  
  summary(df$Flagcentre_missingdata)
  
  df=df
  df<-df %>%
    mutate(Flagcentre_missingdata=ifelse( is.na(patAge1) &  pataux==1 &  pat_violation==0 & HLHSextraexclusion==0 ,1,Flagcentre_missingdata))  %>%
    mutate(Flagcentre_missingdatainf=ifelse( is.na(patAge1) &  pataux==1 &  pat_violation==0 & HLHSextraexclusion==0  ,paste(Flagcentre_missingdatainf,"; stage one as type B followed by stage two or three"),Flagcentre_missingdatainf))  
  summary(df$Flagcentre_missingdata)
  
  susbet=df[which(is.na(df$Flagcentre_missingdata)),c("patid","patAge1","pataux","pat_violation","HLHSextraexclusion","Flagcentre_missingdatainf")]
  
  return(df)
}



######################################################################################
########## HLHS specific minor data missing or unusual records: none
######################################################################################
#need to replace minor data missing as 0 if have been flagged  as suspected missing/miscoded

HLHS_specific_minor_data_errors_module<-function(df){
  
  df<-df %>%
    mutate(Flagcentre_minor=ifelse(Flagcentre_missingdata==1 & HLHSextraexclusion==0,0,Flagcentre_minor))  %>%
    mutate(Flagcentre_minorinf=ifelse(Flagcentre_missingdata==1 & HLHSextraexclusion==0,"",Flagcentre_minorinf))  
  
  return(df)
  
}
