
######################################################################################
#####################FUH inclusion and exclusion module
######################################################################################
FUH_inclusion_and_exclusion_module<-function(df){
  
  diagcodes1=read.csv(dir_diagcodes1,colClasses = "character") # direct diagnosis evidence of FUH
  diagcodes2=read.csv(dir_diagcodes2,colClasses = "character") #indirect diagnosis evidence of FUH.
  proccodes=read.csv(dir_proccodes,colClasses = "character") #procedure  evidence of FUH
  exclusioncodes=read.csv(dir_exclusioncodes,colClasses = "character") #FUH exclusion codes
  HLHSmalformation=read.csv(dir_HLHSmalformation,colClasses = "character") #HLHSmalformation, inclusion for FUH
  extraSVproc=read.csv(dir_FUHextraSVproc,colClasses = "character") #extra SV proc,  inclusion for FUH
  
  #####################define functions used in FUH inclusion and exclusion#################################
  # function: search for all evidence used in inclusion and exclusion
  searchforevidence<-function(){
    # diag evidence 1
    usingcode=diagcodes1$shortcode
    for( j in 1:length(usingcode)){
      index=which(shortcode %in% usingcode[j])
      df$FUHdiagcodes1[index]=1
    }
    
    # diag evidence 2
    usingcode=diagcodes2$shortcode
    for( j in 1:length(usingcode)){
      index=which(shortcode %in% usingcode[j])
      df$FUHdiagcodes2[index]=1
    }
    
    # proc evidence
    usingcode=proccodes$shortcode
    for( j in 1:length(usingcode)){
      index=which(shortcode %in% usingcode[j])
      df$FUHproccodes[index]=1
    }
    # HLHSmalformation 
    usingcode=HLHSmalformation$shortcode
    for( j in 1:length(usingcode)){
      index=which(shortcode %in% usingcode[j])
      df$HLHSmalformation[index]=1
    }  
    
    # extra SV proc 
    usingcode=extraSVproc$shortcode
    for( j in 1:length(usingcode)){
      index=which(shortcode %in% usingcode[j])
      df$FUHextraSVproc[index]=1
    }
    # exclusion 
    usingcode=exclusioncodes$shortcode
    for( j in 1:length(usingcode)){
      index=which(shortcode %in% usingcode[j])
      df$FUHexcludecodes[index]=1
    }
    return(df)
  }
  
  
  ######################search evidence in each fields###################################################
  
  # assign variables
  df$FUHdiagcodes1=0 # direct diagnosis evidence of FUH
  df$FUHdiagcodes2=0 #indirect diagnosis evidence of FUH.
  df$FUHproccodes=0 #procedure  evidence of FUH
  df$FUHexcludecodes=0 #FUH exclusion codes
  df$HLHSmalformation=0 #FUH inclusion codes
  df$FUHextraSVproc=0 #FUH extra inclusion codes
  
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
  
  # # further identify procedure evidence: Norwood procedure by the combination of 2 codes: DK and Comb_DK (shunt)
  # have computed in inclusion of HLHS
  # DKcode="120903"
  # Comb_DKcode=c("123103","123104","123106","123146","123130")
  # 
  # df$DK=0
  # df$Comb_DK=0
  # 
  # for(i in 1:7){
  #   shortcode=df[[paste0("proccode",i)]]
  #   
  #   df$DK[which(shortcode %in% DKcode)]=1
  #   for(j in 1:length(Comb_DKcode)){
  #     df$Comb_DK[which(shortcode %in% Comb_DKcode[j])]=1
  #   }
  # }

  df$FUHproccodes[which(df$Comb_DK ==1 & df$DK==1)]=1
  
  
  ###########################################
    ##mark patients who have HLHS related malformation
  ###########################################

  df<-df %>%
    group_by(patid) %>%
    mutate(patHLHSmalformationcodes=max(HLHSmalformation))%>% 
    mutate(pat_HLHSmalformation=ifelse(patHLHSmalformationcodes==1 & (pat_diagHLHS==1  | patHLHSproccodes==1),1,0)) %>%
    ungroup()
     
  ###########################################
    ##mark patients who had  evidence of FUH. 
  ###########################################
  df<-df %>%
    group_by(patid) %>%
    mutate(patFUHdiagcodes=max(FUHdiagcodes1))%>% 
    mutate(patFUHextraSVproc=max(FUHextraSVproc))%>% 
    mutate(patFUHproccodes=max(FUHproccodes))%>% 
    mutate(patSuggestiveFUH=max(FUHdiagcodes2))%>% 
    mutate(patSuggestiveFUH=ifelse(!(patFUHproccodes==1 | patFUHextraSVproc==1),0, patSuggestiveFUH )) %>%  
    ungroup()
  

  ###########################################
    ##mark patients with exlcusion code of FUH (BV proc)
  ###########################################
  df<-df %>%
    group_by(patid) %>%
    mutate(patFUHexcludecodes=max(FUHexcludecodes))%>% 
    ungroup()
  
  ###########################################
    ##mark FUH patients 
  ###########################################
  df<-df %>%
    group_by(patid) %>%
    mutate(FUHpat=ifelse((pat_HLHSmalformation==1 | patFUHdiagcodes==1 | patSuggestiveFUH==1  ) & patFUHexcludecodes==0 & HLHSpat==0,1,0))%>% 
    ungroup()
  
  
  
  checkpatnum(df$patFUHexcludecodes,df)

  return(df)
}



######################################################################################
#####################FUH pathway module
######################################################################################
# FUH is exclusively single ventricle  

FUH_pathway_module<-function(df){
  df<-df %>%
    mutate(ageatopchange=ifelse( is.na(ageatop) & !is.na(ageatdis), 1, 0)) 
  table(df$ageatopchange)
  # replace ageatop as ageatdis if ageatdis not missing, useful to estimate age at procedure as best as we can.
  df<-df %>%
    mutate(ageatop=ifelse(ageatopchange==1, ageatdis, ageatop))
  
  
  stage1=read.csv(dir_stage1,colClasses = "character") #stage 1 code
  SVstage2=read.csv(dir_SVstage2,colClasses = "character") #stage 2 code
  SVstage3=read.csv(dir_SVstage3,colClasses = "character") #stage 3 code

  
  ########## identify SV stage 1, 2 and 3
  # search  proc filed
  df$stage1A=0 
  df$stage1B=0
  df$stage1D=0
  df$stage1E=0
  
  df$stage1C_1=0 # by single hybrid proc
  df$stage1C_2=0 # by combination of stent and band
  df$CompStage2=0
  df$Glenn=0
  df$Fontan=0
  
  for(i in 1:7){
    shortcode=df[[paste0("proccode",i)]]
    
    usingcode=stage1$shortcode[which(stage1$subtype=="A")]
    df=searchforevidence_generic(mark="stage1A",usingcode)
    
    usingcode=stage1$shortcode[which(stage1$subtype=="B")]
    df=searchforevidence_generic(mark="stage1B",usingcode)
    
    usingcode=stage1$shortcode[which(stage1$subtype=="C")]
    df=searchforevidence_generic(mark="stage1C_1",usingcode)
    

    
    usingcode=stage1$shortcode[which(stage1$subtype=="D")]
    df=searchforevidence_generic(mark="stage1D",usingcode)
    
    usingcode=stage1$shortcode[which(stage1$subtype=="E")]
    df=searchforevidence_generic(mark="stage1E",usingcode)
    
    usingcode=SVstage2$shortcode[which(SVstage2$subtype=="CompStage2")]
    df=searchforevidence_generic(mark="CompStage2",usingcode)
    
    usingcode=SVstage2$shortcode[which(SVstage2$subtype=="Glenn")]
    df=searchforevidence_generic(mark="Glenn",usingcode)
    
    
    usingcode=SVstage3$shortcode
    df=searchforevidence_generic(mark="Fontan",usingcode)
  }
  
  table(df$Fontan)
  
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
  
  
  # if needed, manually check/refine the case if more than one stent or band. maybe have better method to do it  
  
  df$stage1C_2[which(df$auxhybrid==1)]=1
  
  df<-df %>%
    mutate(stage1C =ifelse(stage1C_1==1|stage1C_2==1,1,0)) 
  table(df$stage1C)
  table(df$stage1C_1)
  table(df$stage1C_2)
  
  # assign stage 1, 2 and 3 (raw data)
  df<-df %>%
    mutate(stage1 =ifelse((stage1A==1 |stage1B==1 | stage1C==1 | stage1D==1 | stage1E==1),1,0))%>%
    mutate(stage2 =ifelse((CompStage2==1 | Glenn==1),1,0)) %>%
    mutate(stage3 =ifelse(Fontan==1,1,0)) 
  
  table(df$CompStage2)
  table(df$Glenn)
  

  # and subtypes
  df<-df %>%
    mutate(sub_stage1 =ifelse(stage1A==1,1,
                              ifelse(stage1B==1,2,
                                     ifelse(stage1C==1,3,
                                            ifelse(stage1D==1,4,
                                                   ifelse(stage1E==1,5,-999)))))) %>%
    mutate(sub_stage2 =ifelse((CompStage2==1 | (stage2==1 & stage1A==1)),1,ifelse(stage2==1,2,-999))) 
  
  table(df$sub_stage1)
  table(df$sub_stage2)
  
  
  sub_stage1Lab<-c("1: Norwood/sano/Damus","2: Coarctation/interrupted arch repair",
                   "3: FUH hybrid","4: Securing pulmonary blood flow","5: PA banding","-999: NA")
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
  
  tabpatnum(df$patHT,df)
  
  
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
  
  table(df$CatProc,useNA="ifany")
  
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
  
  #mark sub type of treatment pathway in patient level
  
 
  df<-df %>%
    mutate(sub_stage1 =ifelse(stage1A==1,1,
                              ifelse(stage1B==1,2,
                                     ifelse(stage1C==1,3,
                                            ifelse(stage1D==1,4,
                                                   ifelse(stage1E==1,5,-999)))))) %>%
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
  #                  "3: FUH hybrid","4: Securing pulmonary blood flow","5: PA banding","-999: NA")
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
  
  
  if(CHDtype=="FUH"){df$patAge4=NA}
  df$ProcSeq=as.character(df$ProcSeq)
  
  df<-df %>%
    arrange(patid, patentry) 
  return(df)
}



######################################################################################
#####################FUH diagnosis subgroup
######################################################################################
# we identify 6 subgroup but will only work on DIV and Tricuspid atresia
FUH_diagnosis_subtype_module<-function(df){
  

  
  #################################
  df$codeTA=0
  df$codeAI=0
  df$codeDIV=0
  df$codeMA=0
  df$codeUAVSD=0
  

  codeDIV=c("010403","010404","010405","103452","103453" ,"103454","060311","060411","103412")
  codeTA=c("060101","060102")
  codeAI=c("030104","030105")
  codeMA=c("060201")
  codeUAVSD=c("060726","060609")
  
  
  # search diagnosis filed
  for(i in 1:29){
    shortcode=df[[paste0("diagcode",i)]]
    
    usingcode=codeDIV
    df=searchforevidence_generic(mark="codeDIV",usingcode)
    
    usingcode=codeTA
    df=searchforevidence_generic(mark="codeTA",usingcode)
    
    usingcode=codeAI
    df=searchforevidence_generic(mark="codeAI",usingcode)
    
    usingcode=codeMA
    df=searchforevidence_generic(mark="codeMA",usingcode)
    
    usingcode=codeUAVSD
    df=searchforevidence_generic(mark="codeUAVSD",usingcode)
    
    }
  
  # search morbidity filed
  for(i in 1:16){
    shortcode=df[[paste0("comorbidity",i)]]
    
    usingcode=codeDIV
    df=searchforevidence_generic(mark="codeDIV",usingcode)
    
    usingcode=codeTA
    df=searchforevidence_generic(mark="codeTA",usingcode)
    
    usingcode=codeAI
    df=searchforevidence_generic(mark="codeAI",usingcode)
    
    usingcode=codeMA
    df=searchforevidence_generic(mark="codeMA",usingcode)
    
    usingcode=codeUAVSD
    df=searchforevidence_generic(mark="codeUAVSD",usingcode)
  }
  
  
  # search  proc filed
  for(i in 1:7){
    shortcode=df[[paste0("proccode",i)]]
    
    usingcode=codeDIV
    df=searchforevidence_generic(mark="codeDIV",usingcode)
    
    usingcode=codeTA
    df=searchforevidence_generic(mark="codeTA",usingcode)
    
    usingcode=codeAI
    df=searchforevidence_generic(mark="codeAI",usingcode)
    
    usingcode=codeMA
    df=searchforevidence_generic(mark="codeMA",usingcode)
    
    usingcode=codeUAVSD
    df=searchforevidence_generic(mark="codeUAVSD",usingcode)
      }
  
  # search  prev proc filed
  for(i in 1:26){
    shortcode=df[[paste0("prevproccode",i)]]
    
    usingcode=codeDIV
    df=searchforevidence_generic(mark="codeDIV",usingcode)
    
    usingcode=codeTA
    df=searchforevidence_generic(mark="codeTA",usingcode)
    
    usingcode=codeAI
    df=searchforevidence_generic(mark="codeAI",usingcode)
    
    usingcode=codeMA
    df=searchforevidence_generic(mark="codeMA",usingcode)
    
    usingcode=codeUAVSD
    df=searchforevidence_generic(mark="codeUAVSD",usingcode)
    
  }

  
  df<-df %>%
    group_by(patid) %>%
    mutate(patcodeDIV=max(codeDIV)) %>%
    mutate(patcodeTA=max(codeTA)) %>%
    mutate(patcodeAI=max(codeAI)) %>%
    mutate(patcodeMA=max(codeMA)) %>%
    mutate(patcodeUAVSD=max(codeUAVSD)) %>%
    ungroup()
    


  df<-df %>%
    mutate(diagsubgroup=ifelse(patcodeAI==1,1,
                               ifelse(patcodeDIV==1,2,
                                      ifelse(patcodeTA==1,3,
                                             ifelse(patcodeMA==1,4,
                                                    ifelse(patcodeUAVSD==1,5,6))))))
                          
  
  table(df$diagsubgroup)


  diagsubgroup_lab=c("1: Atrial isomerism","2: DIV","3: Tricuspid atresia","4: Maitral atresia","5: Unbalanced AVSD","6: Complex FUH")
  df$diagsubgroupLab<-factor(df$diagsubgroup,levels=c(1:6),labels=diagsubgroup_lab,ordered=T)

  return(df)
}





######################################################################################
#####################FUH_specific_violation_module
######################################################################################

FUH_specific_violation_module<-function(df){

  
  #FUH  extra exclusion step c) and d) must be done after patwhay analysis. here put it together with  violation rule
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
    mutate(FUHextraexclusion=ifelse((patstage1B==1&!(patsub_stage1==1 | !is.na(patAge2) | !is.na(patAge3))),1,0) )
  
  
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
    mutate(FUHextraexclusion=ifelse(patcode070700only==1,1,FUHextraexclusion) ) 
  
  table(df$patcode070700only,useNA="ifany")

  
  #FUH specific violation rule: none

  return(df)
}


######################################################################################
########## FUH specific suspected  missing/miscoded data module
######################################################################################
#	Patients who had a stage one for FUH (Norwood or hybrid) and no stage two/three by 18 months old whilst surviving.
FUH_specific_suspected_missing_miscoded_data_module<-function(df){
  df<-df %>%
    mutate(Flagcentre_missingdata=ifelse( patsub_stage1%in%c(1,3) & !(patAge2<1.5 & !is.na(patAge2)) &   ageatlastknownstatus>=1.5 &  FUHextraexclusion==0 & !(patAge6<1.5 & !is.na(patAge6) ),1,Flagcentre_missingdata))  %>%
    mutate(Flagcentre_missingdatainf=ifelse( !is.na(patAge1) & !(patAge2<1.5 & !is.na(patAge2)) &   ageatlastknownstatus>=1.5 &  FUHextraexclusion==0 & !(patAge6<1.5 & !is.na(patAge6) ),paste(Flagcentre_missingdatainf,"; had stage one but no stage two/three by 18 months old and surviving"),Flagcentre_missingdatainf))  
  
  #	Patients who had a stage one (Norwood or hybrid) for FUH and stage three, but no stage two by any age.
  df<-df %>%
    mutate(Flagcentre_missingdata=ifelse( patsub_stage1%in%c(1,3) & is.na(patAge2) & !is.na(patAge3) & pat_violation==0 & FUHextraexclusion==0 ,1,Flagcentre_missingdata))  %>%
    mutate(Flagcentre_missingdatainf=ifelse( patsub_stage1%in%c(1,3) & is.na(patAge2) & !is.na(patAge3) & pat_violation==0 & FUHextraexclusion==0 ,paste(Flagcentre_missingdatainf,";patients skipped stage two"),Flagcentre_missingdatainf))  
  
  #Patients who had a stage one (Norwood or hybrid) for HLHS and multiple two stage two but no stage three by surviving age more than 8 years old.
  df<-df %>%
    group_by(patid) %>%
    mutate(pat_stage2num=sum(stage2))%>%
    ungroup()
  
  df<-df %>%
    mutate(stage2redo=ifelse(pat_stage2num>1,1,0))%>%
    mutate(Flagcentre_missingdata=ifelse( patsub_stage1%in%c(1,3) & stage2redo==1 & !( !is.na(patAge3) & patAge3<8) &  pat_violation==0 & FUHextraexclusion==0 & ageatlastknownstatus>8 & !(patAge6<8 & !is.na(patAge6) ) ,1,Flagcentre_missingdata))  %>%
    mutate(Flagcentre_missingdatainf=ifelse(patsub_stage1%in%c(1,3) & stage2redo==1 & !( !is.na(patAge3) & patAge3<8) &  pat_violation==0 & FUHextraexclusion==0 & ageatlastknownstatus>8 & !(patAge6<8 & !is.na(patAge6) ),paste(Flagcentre_missingdatainf,";patients had multiple stage two but no stage three by 8 years old"),Flagcentre_missingdatainf))  
  
  return(df)
}




######################################################################################
########## FUH specific minor data missing or unusual records: none
######################################################################################
#need to replace minor data missing as 0 if have been flagged  as suspected missing/miscoded

FUH_specific_suspected_missing_miscoded_data_module<-function(df){

  df<-df %>%
    mutate(Flagcentre_minor=ifelse(Flagcentre_missingdata==1 & FUHextraexclusion==0,0,Flagcentre_minor))  %>%
    mutate(Flagcentre_minorinf=ifelse(Flagcentre_missingdata==1 & FUHextraexclusion==0,"",Flagcentre_minorinf))  
  
  return(df)
  
}
