######################################################################################
#####################TGA inclusion and exclusion module
######################################################################################


TGA_inclusion_and_exclusion_module<-function(df){
  
  diagcodes1=read.csv(dir_diagcodes1,colClasses = "character") # direct diagnosis evidence of TGA
  diagcodes2="" #indirect diagnosis evidence of TGA.  NONE
  proccodes=read.csv(dir_proccodes,colClasses = "character") #procedure  evidence of TGA
  exclusioncodes=read.csv(dir_exclusioncodes,colClasses = "character") #TGA exclusion codes

  #####################define functions used in TGA inclusion and exclusion#################################
  # function: search for all evidence used in inclusion and exclusion
  searchforevidence<-function(){
    # diag evidence 1
    usingcode=diagcodes1$shortcode
    for( j in 1:length(usingcode)){
      index=which(shortcode %in% usingcode[j])
      df$TGAdiagcodes1[index]=1
    }
    

    # proc evidence
    usingcode=proccodes$shortcode
    for( j in 1:length(usingcode)){
      index=which(shortcode %in% usingcode[j])
      df$TGAproccodes[index]=1
    }
   
    # exclusion 
    usingcode=exclusioncodes$shortcode
    for( j in 1:length(usingcode)){
      index=which(shortcode %in% usingcode[j])
      df$TGAexcludecodes[index]=1
    }
    return(df)
  }
  
  
  ######################search evidence in each fields###################################################
  
  # assign variables
  df$TGAdiagcodes1=0 # direct diagnosis evidence of TGA
  df$TGAdiagcodes2=0 #indirect diagnosis evidence of TGA.
  df$TGAproccodes=0 #procedure  evidence of TGA
  df$TGAexcludecodes=0 #TGA exclusion codes

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
  ##mark patients who had  evidence of TGA. 
  ###########################################
  df<-df %>%
    group_by(patid) %>%
    mutate(patTGAdiagcodes=max(TGAdiagcodes1))%>% 
    mutate(patTGAproccodes=max(TGAproccodes))%>% 
    mutate(patSuggestiveTGA=max(TGAdiagcodes2))%>% 
    ungroup()
  
  df<-df %>%
    mutate(pat_diagTGA=ifelse((patdiagTGAcodes==1  | patSuggestiveTGA==1),1,0)) %>% 
    mutate(pat_procTGA=ifelse(patTGAproccodes==1,1,0))
  
  
  ###########################################
  ##mark patients with exclusion code of TGA (BV proc)
  ###########################################
  df<-df %>%
    group_by(patid) %>%
    mutate(patTGAexcludecodes=max(TGAexcludecodes))%>% 
    ungroup()
  
  ###########################################
  ##mark TGA patients 
  ###########################################
  df<-df %>%
    group_by(patid) %>%
    mutate(TGApat=ifelse(( pat_procTGA==1 | pat_diagTGA==1  ) & patTGAexcludecodes==0,1,0))%>% 
    ungroup()
  
  
  return(df)
}


######################################################################################
#####################TGA pathway module
######################################################################################
# TGA  can be  managed by either single ventricle  or biventricular reparative pathway

TGA_pathway_module<-function(df){

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
  df$stage1A=0 
  df$stage1B=0
  df$stage1C=0
  df$stage1D=0
  df$stage1E=0
  

  df$Glenn=0
  df$Fontan=0
  df$VSDrepair=0
  
  for(i in 1:7){
    shortcode=df[[paste0("proccode",i)]]
    
    usingcode=c("120643","121000","120903")
    df=searchforevidence_generic(mark="stage1A",usingcode)
    
    usingcode=c("121800","122100","121810","121801","121802","121803","121830")
    df=searchforevidence_generic(mark="stage1B",usingcode)
    
    usingcode="121004"
    df=searchforevidence_generic(mark="stage1C",usingcode)
    
    usingcode=c("121014","123103","123104","123106","123130","123146","123601")
    df=searchforevidence_generic(mark="stage1D",usingcode)
    
    usingcode="121402"
    df=searchforevidence_generic(mark="stage1E",usingcode)
    
    usingcode=c("120801","120802","120803","120807","120828","120816")
    df=searchforevidence_generic(mark="VSDrepair",usingcode)
    
    usingcode=SVstage2$shortcode[which(SVstage2$subtype=="Glenn")]
    df=searchforevidence_generic(mark="Glenn",usingcode)
    
    
    usingcode=SVstage3$shortcode
    df=searchforevidence_generic(mark="Fontan",usingcode)
    

  }
  

  df<-df %>%
    mutate(stage1B=ifelse(VSDrepair==1,0,stage1B)) %>%
    mutate(stage1=ifelse(stage1A==1 |  stage1B==1 | stage1C==1 | stage1D==1 | stage1E==1 ,1,0))     %>%
    mutate(stage2=ifelse(Glenn==1,1,0))  %>%
    mutate(stage3=ifelse(Fontan==1,1,0)) 

  
  # and subtypes
  df<-df %>%
    mutate(sub_stage1 =ifelse(stage1A==1,1,
                              ifelse(stage1B==1,2,
                                     ifelse(stage1C==1,3,
                                            ifelse(stage1D==1,4,
                                                   ifelse(stage1E==1,5,-999)))))) 
  
  table(df$sub_stage1)

  
  sub_stage1Lab<-c("1: Norwood/sano/Damus","2: Coarctation/interrupted arch repair",
                   "3:HLHS hybrid","4: Securing pulmonary blood flow","5: PA banding","-999: NA")

  
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
  df$stage3[index_changetostage2]=0
  
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
  
  
  ######################################################################################
    ##########indentify  reparative procedure for TGA
  ######################################################################################

 # TGA with PS repair

  df$TGAPSrepair=0 
  df$CoBcode1=0 
  df$CoBcode2=0 
  
  for(i in 1:7){
    shortcode=df[[paste0("proccode",i)]]
    usingcode=c("122911","122745","122778")
    df=searchforevidence_generic(mark="TGAPSrepair",usingcode)
    
    usingcode=c("122921")
    df=searchforevidence_generic(mark="CoBcode1",usingcode)
    
    usingcode=c("120713","120822","120701")
    df=searchforevidence_generic(mark="CoBcode2",usingcode)
  }
  
  df<-df %>%
    mutate(TGAPSrepair=ifelse(CoBcode1==1 & CoBcode2==1,1 ,TGAPSrepair))

  
  
 # complex TGA repair
  
  
  df$complexTGArepair=0 
  df$CoBcode1=0 
  df$CoBcode2=0 
  
  for(i in 1:7){
    shortcode=df[[paste0("proccode",i)]]
    usingcode=c("122940")
    df=searchforevidence_generic(mark="complexTGArepair",usingcode)
    
    usingcode=c("122921")
    df=searchforevidence_generic(mark="CoBcode1",usingcode)
    
    usingcode=c("122920","120801","120802","120803","120816","120828",
               "121800","121801","121802","121803","121810","121815","121830","122100") 
    df=searchforevidence_generic(mark="CoBcode2",usingcode)
  }
  df<-df %>%
    mutate(complexTGArepair=ifelse(CoBcode1==1 & CoBcode2==1,1 ,complexTGArepair))
  
  
table(df$complexTGArepair)



#TGA with IVS repair
df$TGAIVSrepair=0 

  for(i in 1:7){
    shortcode=df[[paste0("proccode",i)]]
    usingcode=c("122921")
    df=searchforevidence_generic(mark="TGAIVSrepair",usingcode)
    
  }

#Senning or Mustard
df$SMproc=0 

for(i in 1:7){
  shortcode=df[[paste0("proccode",i)]]
  usingcode=c("122901","122902")
  df=searchforevidence_generic(mark="SMproc",usingcode)
  
}
  
  

#other repair
df$otherrepair=0 

for(i in 1:7){
  shortcode=df[[paste0("proccode",i)]]
  usingcode=c("122702","122920")
  df=searchforevidence_generic(mark="otherrepair",usingcode)
  
}

  
df<-df %>%
  mutate(TGApathway=ifelse(TGAPSrepair==1  | complexTGArepair==1 | TGAIVSrepair==1  | SMproc==1 | otherrepair==1,1,0))  


# and TGA repair subtypes
df<-df %>%
  mutate(sub_repair =ifelse(TGAPSrepair==1,1,
                            ifelse(complexTGArepair==1,2,
                                   ifelse(TGAIVSrepair==1,3,
                                          ifelse(SMproc==1,4,
                                                 ifelse(otherrepair==1,5,-999)))))) 

sub_repairLab<-c("1: Complex TGA with pulmonary stenosis repair","2: Complex TGA repair",
                 "3:TGA with IVS repair","4: Senning or Mustard","5: Other reparative procedure linked to TGA","-999: NA")

df<-df %>%
  group_by(patid) %>%
  mutate(patTGApathway=max(TGApathway))%>%
  ungroup()
  
  #################### assign CatProc for each record 
  df$CatProc=ifelse(df$HT==1,6,
                    ifelse(df$stage3==1,3,
                           ifelse(df$TGApathway==1,4,
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
    mutate(auxsub_repair =ifelse(CatProc==4,sub_repair,-999)) 
  df<-df %>%
    group_by(patid) %>%
    mutate(patsub_stage1 =ifelse(max(auxsub_stage1)>=0,max(auxsub_stage1),-999)) %>%
    mutate(patsub_BVpathway =ifelse(max(auxsub_repair)>=0,max(auxsub_repair),-999)) %>%
    ungroup()
  
  df$patsub_stage1Label<-factor(df$patsub_stage1,levels=c(1:5,-999),labels=sub_stage1Lab,order=T)
  
  df$patsub_BVpathwayLabel<-factor(df$patsub_BVpathway,levels=c(1:5,-999),labels=sub_repairLab,ordered=T)
  
  
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
                "4: Reparative procedure" ,"5: Other pathway (none in TGA)","6: Heart transplant" ,
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
#####################TGA diagnosis subgroup
######################################################################################
TGA_diagnosis_subtype_module<-function(df){
  TGAPScodes=read.csv(dir_TGA_PScodes,colClasses = "character") #TGA with PS codes
  ComplexTGAcodes=read.csv(dir_TGA_Complexcodes,colClasses = "character") #complex TGA codes
  

  
  #################################
  df$TGAPS=0
  df$ComplexTGA=0
  
  
  # search diagnosis filed
  for(i in 1:29){
    shortcode=df[[paste0("diagcode",i)]]
    
    usingcode=TGAPScodes$shortcode
    df=searchforevidence_generic(mark="TGAPS",usingcode)
    
    usingcode=ComplexTGAcodes$shortcode
    df=searchforevidence_generic(mark="ComplexTGA",usingcode)
    
  }
  
  # search morbidity filed
  for(i in 1:16){
    shortcode=df[[paste0("comorbidity",i)]]
    
    usingcode=TGAPScodes$shortcode
    df=searchforevidence_generic(mark="TGAPS",usingcode)
    
    usingcode=ComplexTGAcodes$shortcode
    df=searchforevidence_generic(mark="ComplexTGA",usingcode)
  }
  
  
  # search  proc filed
  for(i in 1:7){
    shortcode=df[[paste0("proccode",i)]]
    
    usingcode=TGAPScodes$shortcode
    df=searchforevidence_generic(mark="TGAPS",usingcode)
    
    usingcode=ComplexTGAcodes$shortcode
    df=searchforevidence_generic(mark="ComplexTGA",usingcode)
  }
  
  # search  prev proc filed
  for(i in 1:26){
    shortcode=df[[paste0("prevproccode",i)]]
    
    usingcode=TGAPScodes$shortcode
    df=searchforevidence_generic(mark="TGAPS",usingcode)
    
    usingcode=ComplexTGAcodes$shortcode
    df=searchforevidence_generic(mark="ComplexTGA",usingcode)
    
  }
  

  # add evidence by subtype of stage 1 and repair
  df<-df %>%
    mutate(ComplexTGA=ifelse(sub_repair==2 | sub_repair==4 | sub_stage1==1 | sub_stage1==2,1,ComplexTGA)) %>%
    mutate(TGAPS=ifelse(sub_stage1==4 | sub_repair==1  ,1,TGAPS)) 
    

  
  df<-df %>%
    group_by(patid) %>%
    mutate(patPS=max(TGAPS)) %>%
    mutate(patComplexTGA=max(ComplexTGA)) %>%
    ungroup()
  

  df<-df %>%
    mutate(diagsubgroup=ifelse(patPS==1,1,
                               ifelse(patComplexTGA==1,2,3)))
  
  
  diagsubgroup_lab=c("1: Complex TGA with PS","2: Complex TGA without PS","3: TGA with intact ventricular spetum")
  df$diagsubgroupLab<-factor(df$diagsubgroup,levels=c(1:3),labels=diagsubgroup_lab,ordered=T)
  
  
  return(df)
}
######################################################################################
########## TGA specific suspected  missing/miscoded data module
######################################################################################
TGA_specific_suspected_missing_miscoded_data_module<-function(df){
 
#####################if SV pathway
  
  # min age at SV pathway 
  df$minageSVpathway=apply(df[,which(names(df) %in% c("patAge2","patAge3"))],1,function(x) min(x,na.rm=T))
  df$minageSVpathway[is.infinite(df$minageSVpathway)]=NA
  
  #TGA with IVS
  df<-df %>%
    mutate(Flagcentre_missingdata=ifelse( diagsubgroup==3 & !is.na(minageSVpathway), 1,Flagcentre_missingdata))  %>%
    mutate(Flagcentre_missingdatainf=ifelse( diagsubgroup==3 & !is.na(minageSVpathway) ,paste(Flagcentre_missingdatainf,";TGA with IVS - had SV stage 2/3"),Flagcentre_missingdatainf))  
  
   #complex TGA with and without PS patients no stage 2/3 by age 5 years old
  
   df<-df %>%
    mutate(Flagcentre_missingdata=ifelse(diagsubgroup %in% c(1,2) & !is.na(minageSVpathway) & minageSVpathway>5 & !(patAge6<5 & !is.na(patAge6) ),1,Flagcentre_missingdata))  %>%
    mutate(Flagcentre_missingdatainf=ifelse(diagsubgroup %in% c(1,2) & !is.na(minageSVpathway) & minageSVpathway>5 & !(patAge6<5 & !is.na(patAge6)) ,
                                          paste(Flagcentre_missingdatainf,"; complex TGA with/without PS - in SV pathway but had no SV stage two/three by year 5"),Flagcentre_missingdatainf))  
   
#####################if BV pathway
#complex TGA with PS  if had no reparative proc by  5 years old
df<-df %>%
  mutate(Flagcentre_missingdata=ifelse( diagsubgroup==1 & is.na(minageSVpathway) & (is.na(patAge4) | patAge4>5)  & ageatlastknownstatus>5  & !(patAge6<5 & !is.na(patAge6)),
                                          1,Flagcentre_missingdata))  %>%
  mutate(Flagcentre_missingdatainf=ifelse( diagsubgroup==1 & is.na(minageSVpathway) & (is.na(patAge4) | patAge4>5)  & ageatlastknownstatus>5  & !(patAge6<5 & !is.na(patAge6)),
                                        paste(Flagcentre_missingdatainf,";complex TGA with PS - under BV pathway but had no reprative procedure whlist surviving by 5 years old"),Flagcentre_missingdatainf))  

#complex TGA without PS  if had no reparative proc by  3 years old

df<-df %>%
  mutate(Flagcentre_missingdata=ifelse(diagsubgroup==2 & is.na(minageSVpathway) & (is.na(patAge4) | patAge4>3) & ageatlastknownstatus>3  & !(patAge6<3 & !is.na(patAge6)), 1,Flagcentre_missingdata))  %>%
  mutate(Flagcentre_missingdatainf=ifelse(diagsubgroup==2 & is.na(minageSVpathway) & (is.na(patAge4) | patAge4>3) & ageatlastknownstatus>3  & !(patAge6<3 & !is.na(patAge6)),
                                           paste(Flagcentre_missingdatainf,";complex TGA without PS - under BV pathway but had no reprative procedure whlist surviving by 3 years old"),Flagcentre_missingdatainf))  

#TGA IVS  if had no reparative proc by  4 months  old
df<-df %>%
  mutate(Flagcentre_missingdata=ifelse(diagsubgroup==3 & is.na(minageSVpathway) & (is.na(patAge4) | patAge4>1/3) & ageatlastknownstatus>1/3  & !(patAge6<1/3 & !is.na(patAge6)), 1,Flagcentre_missingdata))  %>%
  mutate(Flagcentre_missingdatainf=ifelse(diagsubgroup==3 & is.na(minageSVpathway) & (is.na(patAge4) | patAge4>1/3) & ageatlastknownstatus>1/3  & !(patAge6<1/3 & !is.na(patAge6)),
                                          paste(Flagcentre_missingdatainf,";TGA IVS - under BV pathway but had no reprative procedure whlist surviving by 4 months old"),Flagcentre_missingdatainf))  
 

  return(df)
}

######################################################################################
########## TGA specific minor data missing or unusual records
######################################################################################

TGA_specific_minor_data_errors_module<-function(df){

  #Patients who had incomplete coding in reparative procedure for TGA.   
  df<-df %>%
    group_by(patid) %>%
    mutate(pataux=max(CatProc==4 & sub_repair==5)) %>%
    ungroup()

  
  df<-df %>%
    mutate(Flagcentre_minor=ifelse(pataux==1,1,Flagcentre_minor))  %>%
    mutate(Flagcentre_minorinf=ifelse(pataux==1, paste(Flagcentre_minorinf,"; TGA pathway incomplete/poor coding"),Flagcentre_minorinf)) 
  
  
  
  #Patients who had stage one subtype A/C 
  df<-df %>%
    mutate(Flagcentre_minor=ifelse(patsub_stage1==1 | patsub_stage1==3,1,Flagcentre_minor))  %>%
    mutate(Flagcentre_minorinf=ifelse(patsub_stage1==1 | patsub_stage1==3, paste(Flagcentre_minorinf,"; Pallitive procedure type A/C (is coding complete and accurate?)"),Flagcentre_minorinf)) 
  
  
  #patients who had diagnostic code 010309/010510

  df$aux=0
  # search diagnosis filed
  for(i in 1:29){
    shortcode=df[[paste0("diagcode",i)]]
    usingcode=c("010309" , "010510")
    df=searchforevidence_generic(mark="aux",usingcode)
  }
  
  # search morbidity filed
  for(i in 1:16){
    shortcode=df[[paste0("comorbidity",i)]]
    usingcode=c("010309" , "010510")
    df=searchforevidence_generic(mark="aux",usingcode)
  }
  


  df<-df %>%
    group_by(patid) %>%
    mutate(pataux=max(aux)) %>%
    ungroup()
  
  df<-df %>%
    mutate(Flagcentre_minor=ifelse(pataux==1,1,Flagcentre_minor))  %>%
    mutate(Flagcentre_minorinf=ifelse(pataux==1, paste(Flagcentre_minorinf,"; diagnostic code 010309/010510"),Flagcentre_minorinf)) 
  

  #need to replace minor data missing as 0 if have been flagged  as suspected missing/miscoded
  df<-df %>%
    mutate(Flagcentre_minor=ifelse(Flagcentre_missingdata==1 ,0,Flagcentre_minor))  %>%
    mutate(Flagcentre_minorinf=ifelse(Flagcentre_missingdata==1 ,"",Flagcentre_minorinf))  
  
  return(df)
  
}
