######################################################################################
#####################AVSD inclusion and exclusion module
######################################################################################


AVSD_inclusion_and_exclusion_module<-function(df){
  
  diagcodes1=read.csv(dir_diagcodes1,colClasses = "character") # direct diagnosis evidence of AVSD
  diagcodes2=read.csv(dir_diagcodes2,colClasses = "character") #indirect diagnosis evidence of AVSD.  
  proccodes=read.csv(dir_proccodes,colClasses = "character") #procedure  evidence of AVSD
  exclusioncodes=read.csv(dir_exclusioncodes,colClasses = "character") #AVSD exclusion codes
  
  #####################define functions used in AVSD inclusion and exclusion#################################
  # function: search for all evidence used in inclusion and exclusion
  searchforevidence<-function(){
    # diag evidence 1
    usingcode=diagcodes1$shortcode
    for( j in 1:length(usingcode)){
      index=which(shortcode %in% usingcode[j])
      df$AVSDdiagcodes1[index]=1
    }
    # diag evidence 2
    
    usingcode=diagcodes2$shortcode
    for( j in 1:length(usingcode)){
      index=which(shortcode %in% usingcode[j])
      df$AVSDdiagcodes2[index]=1
    }
    
    # proc evidence
    usingcode=proccodes$shortcode
    for( j in 1:length(usingcode)){
      index=which(shortcode %in% usingcode[j])
      df$AVSDproccodes[index]=1
    }
    
    # exclusion 
    usingcode=exclusioncodes$shortcode
    for( j in 1:length(usingcode)){
      index=which(shortcode %in% usingcode[j])
      df$AVSDexcludecodes[index]=1
    }
    return(df)
  }
  
  
  ######################search evidence in each fields###################################################
  
  # assign variables
  df$AVSDdiagcodes1=0 # direct diagnosis evidence of AVSD
  df$AVSDdiagcodes2=0 #indirect diagnosis evidence of AVSD.
  df$AVSDproccodes=0 #procedure  evidence of AVSD
  df$AVSDexcludecodes=0 #AVSD exclusion codes
  
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
  ##mark patients who had  evidence of AVSD. 
  ###########################################
  df<-df %>%
    group_by(patid) %>%
    mutate(patAVSDdiagcodes=max(AVSDdiagcodes1))%>% 
    mutate(patAVSDproccodes=max(AVSDproccodes))%>% 
    mutate(patSuggestiveAVSD=max(AVSDdiagcodes2))%>% 
    ungroup()
  
  
 #step c) also inlcude tetralogy AVSD 



df$code010120=0
df$code010117010101=0
# search diagnosis filed
for(i in 1:29){
  shortcode=df[[paste0("diagcode",i)]]
  df=searchforevidence_generic(mark="code010120","010120")
  df=searchforevidence_generic(mark="code010117010101",c("010117" ,"010101"))
}

# search morbidity filed
for(i in 1:16){
  shortcode=df[[paste0("comorbidity",i)]]
  df=searchforevidence_generic(mark="code010120","010120")
  df=searchforevidence_generic(mark="code010117010101",c("010117" ,"010101"))
  }

# search  proc filed
for(i in 1:7){
  shortcode=df[[paste0("proccode",i)]]
  df=searchforevidence_generic(mark="code010120","010120")
  df=searchforevidence_generic(mark="code010117010101",c("010117" ,"010101"))
}

# search  prev proc filed
for(i in 1:26){
  shortcode=df[[paste0("prevproccode",i)]]
  df=searchforevidence_generic(mark="code010120","010120")
  df=searchforevidence_generic(mark="code010117010101",c("010117" ,"010101"))
}


df<-df %>%
  group_by(patid) %>%
  mutate(patcode010120=max(code010120)) %>%
  mutate(patcode010117010101=max(code010117010101)) %>%
  ungroup()

df<-df %>%
  mutate(AVSD_step1c=ifelse(patcode010120==1 | (patcode010117010101==1 & patdiagAVSDcodes==1) ,1,0)) 


# step d) also include unbalanced AVSD 


df$code060726=0
df$codeVH=0

# search diagnosis filed
for(i in 1:29){
  shortcode=df[[paste0("diagcode",i)]]
  df=searchforevidence_generic(mark="code060726","060726")
  df=searchforevidence_generic(mark="codeVH",c("070700" ,"070200"))
  
}

# search morbidity filed
for(i in 1:16){
  shortcode=df[[paste0("comorbidity",i)]]
  df=searchforevidence_generic(mark="code060726","060726")
  df=searchforevidence_generic(mark="codeVH",c("070700" ,"070200"))
  
  }

# search  proc filed
for(i in 1:7){
  shortcode=df[[paste0("proccode",i)]]
  df=searchforevidence_generic(mark="code060726","060726")
  df=searchforevidence_generic(mark="codeVH",c("070700" ,"070200"))
  
}

# search  prev proc filed
for(i in 1:26){
  shortcode=df[[paste0("prevproccode",i)]]
  df=searchforevidence_generic(mark="code060726","060726")
  df=searchforevidence_generic(mark="codeVH",c("070700" ,"070200"))
  
}
df<-df %>%
  group_by(patid) %>%
  mutate(patcode060726=max(code060726)) %>%
  mutate(patcodeVH=max(codeVH)) %>%
  ungroup()


df<-df %>%
  mutate(AVSD_step1d=ifelse(patcode060726==1 | (patcodeVH==1 & patdiagAVSDcodes==1) ,1,0)) 


   
df<-df %>%
    mutate(pat_diagAVSD=ifelse((patdiagAVSDcodes==1  | patSuggestiveAVSD==1 | AVSD_step1c==1  | AVSD_step1d==1),1,0)) %>% 
    mutate(pat_procAVSD=ifelse(patAVSDproccodes==1,1,0))
  
  
  ###########################################
  ##mark patients with exclusion code of AVSD
  ###########################################
  df<-df %>%
    group_by(patid) %>%
    mutate(patAVSDexcludecodes=max(AVSDexcludecodes))%>% 
    ungroup()
  
  ###########################################
  ##mark AVSD patients 
  ###########################################
  df<-df %>%
    group_by(patid) %>%
    mutate(AVSDpat=ifelse(( pat_procAVSD==1 | pat_diagAVSD==1  ) & patAVSDexcludecodes==0,1,0))%>% 
    ungroup()
  
  
  return(df)
}
######################################################################################
#####################AVSD pathway module
######################################################################################
# AVSD can be  managed by either single ventricle  or biventricular reparative pathway

AVSD_pathway_module<-function(df){
  
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
  
  
  # manually check/refine the case if more than one stent or band. maybe have better method to do it
  df$auxhybrid[which(df$launchesrecid %in% c("RJBXHMGU","RJBXEJFX"))]=1
  
  table(df$auxhybrid)
  
  
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
  
  # recompute
  
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
  ##########indentify  reparative procedure for AVSD
  ######################################################################################
  
  #  tetralogy AVSD repair
  
  df$TetAVSDproc=0 
  df$tetcombined1=0 
  df$tetcombined2=0 
  
  for(i in 1:7){
    shortcode=df[[paste0("proccode",i)]]
    TetAVSDproccode=c("120511" ,"122601","122613","122620","122701")
    df=searchforevidence_generic(mark="TetAVSDproc",TetAVSDproccode)
    
    tetcombinedcode1=c("120501" ,"120510","120401","120409","120400","120440","124801",
                "124802","120420","129001","120801","120571")
    df=searchforevidence_generic(mark="tetcombined1",tetcombinedcode1)
    
    tetcombinedcode2=c("120641","120600","123601")
    df=searchforevidence_generic(mark="tetcombined2",tetcombinedcode2)
  }
  
  df<-df %>%
    mutate(CombCode=ifelse(TetAVSDproc==0 & tetcombined1==1 & tetcombined2==1,1 ,0)) %>%
    mutate(TetAVSDproc=ifelse(CombCode==1,1 ,TetAVSDproc))
 
  
   # complete and partial AVSD repair
  df$CAVSDrepair=0 
  df$PAVSDrepair=0 

  for(i in 1:7){
    shortcode=df[[paste0("proccode",i)]]
    CAVSDrepaircode=c("120501","120510","120571")
    df=searchforevidence_generic(mark="CAVSDrepair",CAVSDrepaircode)
    
    PAVSDrepaircode=c("120401","120409")
    df=searchforevidence_generic(mark="PAVSDrepair",PAVSDrepaircode)

  }
  
  
  
# other reparative procedure linked to AVSD & ASD or VSD repair in AVSD
  
  df$OtherAVSDrepair=0 
  df$ASDVSDrepair=0 
  
  for(i in 1:7){
    shortcode=df[[paste0("proccode",i)]]
    OtherAVSDrepaircode=c("120400","120440","124801","124802","120420","129001","120303","120300","120304","120418","120311","120445")
    df=searchforevidence_generic(mark="OtherAVSDrepair",OtherAVSDrepaircode)
    
    ASDVSDrepaircode=c("120101","120102","120103","120106","120110","120122","120801","120802","120803","120807","120828","120816")
    df=searchforevidence_generic(mark="ASDVSDrepair",ASDVSDrepaircode)
    
  }

  df<-df %>%
    mutate(AVSDpathway=ifelse(
      TetAVSDproc==1 | CAVSDrepair==1 | PAVSDrepair==1 | OtherAVSDrepair==1 | ASDVSDrepair==1 ,1,0))  
  
  
  # and AVSD repair subtypes
  df<-df %>%
    mutate(sub_repair =ifelse(TetAVSDproc==1,1,
                              ifelse(CAVSDrepair==1,2,
                                     ifelse(PAVSDrepair==1,3,
                                            ifelse(OtherAVSDrepair==1,4,
                                                   ifelse(ASDVSDrepair==1,5,-999)))))) 
  
  sub_repairLab<-c("1: Tetralogy and AVSD repair","2: Complete AVSD repair",
                   "3:Partial AVSD repair","4: Other reparative procedure linked to AVSD","5: ASD or VSD repair","-999: NA")
  
  df<-df %>%
    group_by(patid) %>%
    mutate(patAVSDpathway=max(AVSDpathway))%>%
    ungroup()
  
  #################### assign CatProc for each record 
  df$CatProc=ifelse(df$HT==1,6,
                    ifelse(df$stage3==1,3,
                           ifelse(df$AVSDpathway==1,4,
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
  table(df$AVSDpathway,useNA="ifany")
  
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
    mutate(auxhybridbycomb =ifelse(CatProc==1 & sub_stage1==3 & stage1C_1==0 & stage1C_2==1 ,1,-999)) %>%
    mutate(auxsub_repair =ifelse(CatProc==4,sub_repair,-999)) 
  df<-df %>%
    group_by(patid) %>%
    mutate(patsub_stage1 =ifelse(max(auxsub_stage1)>=0,max(auxsub_stage1),-999)) %>%
    mutate(patsub_stage2 =ifelse(max(auxsub_stage2)>=0,max(auxsub_stage2),-999)) %>%
    mutate(pathybridbycomb =ifelse(max(auxhybridbycomb)>=0,max(auxhybridbycomb),-999)) %>%
    mutate(patsub_BVpathway =ifelse(max(auxsub_repair)>=0,max(auxsub_repair),-999)) %>%
    ungroup()
  
  df$patsub_stage1Label<-factor(df$patsub_stage1,levels=c(1:5,-999),labels=sub_stage1Lab,order=T)
  df$patsub_stage2Label<-factor(df$patsub_stage2,levels=c(1:2,-999),labels=sub_stage2Lab,order=T)
  df$patsub_BVpathwayLabel<-factor(df$patsub_BVpathway,levels=c(1:5,-999),labels=sub_repairLab,ordered=T)
  
  
  # mark reintervention and pre-pathway procedures
  df$minagepathway=apply(df[,grep("^patAge",names(df))],1,function(x) min(x,na.rm=T))
  df$minagepathway[is.infinite(df$minagepathway)]=NA
  
  
  
  df<-df %>%
    mutate(CatProc=ifelse(CatProc==-999 & auxhybrid==1 & pathybridbycomb==1,5,CatProc)) %>% 
    mutate(CatProc=ifelse(CatProc==-999 & InterType %in% c(1,2) & is.na(minagepathway),0,CatProc))   %>%
    mutate(CatProc=ifelse(CatProc==-999 & InterType %in% c(1,2) & ageatop<minagepathway & !is.na(minagepathway) & !is.na(ageatop),0,CatProc))   %>%
    mutate(CatProc=ifelse(CatProc==-999 & InterType %in% c(1,2) ,7,CatProc))   %>%
    mutate(CatProc=ifelse(CatProc==-999 & InterType %in% c(3) ,8,CatProc)) 
  
  # pathway marker
  df<-df %>%
    mutate(ispathway =ifelse(CatProc %in% c(1,2,3,4,5),1,0)) 
  
  
  
  CatProc_lab=c("0: Pre-pathway","1: Palliative first stage procedure","2: SV stage two","3: SV stage three (Fontan)",
                "4: Reparative procedure" ,"5: Other pathway (none in AVSD)","6: Heart transplant" ,
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
#####################AVSD diagnosis subgroup
######################################################################################
AVSD_diagnosis_subtype_module<-function(df){

  
  df$tetcodes=0
  df$PAVSDcodes=0
  df$CAVSDcodes=0
  
  
  # search diagnosis filed
  for(i in 1:29){
    shortcode=df[[paste0("diagcode",i)]]
    
    usingcode=c("010120","010117","010101")
    df=searchforevidence_generic(mark="tetcodes",usingcode)
    
    usingcode=c("060601","120401","120409")
    df=searchforevidence_generic(mark="PAVSDcodes",usingcode)
    
    usingcode=c("050601","060609","120501")
    df=searchforevidence_generic(mark="CAVSDcodes",usingcode)
  }
  
  # search morbidity filed
  for(i in 1:16){
    shortcode=df[[paste0("comorbidity",i)]]
    
    usingcode=c("010120","010117","010101")
    df=searchforevidence_generic(mark="tetcodes",usingcode)
    
    usingcode=c("060601","120401","120409")
    df=searchforevidence_generic(mark="PAVSDcodes",usingcode)
    
    usingcode=c("060609","120501")
    df=searchforevidence_generic(mark="CAVSDcodes",usingcode)
  }
  
  
  # search  proc filed
  for(i in 1:7){
    shortcode=df[[paste0("proccode",i)]]
    
    usingcode=c("010120","010117","010101")
    df=searchforevidence_generic(mark="tetcodes",usingcode)
    
    usingcode=c("060601","120401","120409")
    df=searchforevidence_generic(mark="PAVSDcodes",usingcode)
    
    usingcode=c("050601","060609","120501")
    df=searchforevidence_generic(mark="CAVSDcodes",usingcode)
  }
  
  # search  prev proc filed
  for(i in 1:26){
    shortcode=df[[paste0("prevproccode",i)]]
    
    usingcode=c("010120","010117","010101")
    df=searchforevidence_generic(mark="tetcodes",usingcode)
    
    usingcode=c("060601","120401","120409")
    df=searchforevidence_generic(mark="PAVSDcodes",usingcode)
    
    usingcode=c("050601","060609","120501")
    df=searchforevidence_generic(mark="CAVSDcodes",usingcode)
    
  }
  
  
  df<-df %>%
    group_by(patid) %>%
    mutate(pathasTetAVSDproc=max(TetAVSDproc)) %>%
    mutate(pathastetcodes=max(tetcodes)) %>%
    mutate(pathasPAVSDcodes=max(PAVSDcodes)) %>%
    mutate(pathasCAVSDcodes=max(CAVSDcodes)) %>%
    ungroup()
  
  
  
  # amb patients, who had both complete and partial AVSD codes, will be group in partial AVSD if had partial AVSD repair older than 2 years old.otherwise group in complete AVSD
  
  df<-df %>%
    mutate(aux=ifelse(ageatop>2 & sub_repair ==3 & !is.na(ageatop) & pathasCAVSDcodes==1 & pathasPAVSDcodes==1,1,0)) 
  
  df<-df %>%
    group_by(patid) %>%
    mutate(pataux=max(aux)) %>%
    ungroup()
  
  df<-df %>%
    mutate(patambCAVSD=ifelse(pataux==0 & pathasCAVSDcodes==1 & pathasPAVSDcodes==1,1,0)) 
  
  # assign diagnostic subgroup 
  
  df<-df %>%
    mutate(diagsubgroup=ifelse( AVSD_step1c==1 | pathasTetAVSDproc==1 | pathastetcodes==1,1,
                               ifelse(AVSD_step1d==1 | patStage2==1 | patStage3==1 | patsub_stage1==1| patsub_stage1==3,2,
                                      ifelse(pathasPAVSDcodes==1 & pathasCAVSDcodes==0 ,3,
                                             ifelse(pathasPAVSDcodes==1 & pathasCAVSDcodes==1 & !patambCAVSD==1,3,4)))))
  
  
  diagsubgroup_lab=c("1: Tetralogy AVSD","2: Unbalanced AVSD","3: Partial AVSD","4: Complete AVSD")
  df$diagsubgroupLab<-factor(df$diagsubgroup,levels=c(1:4),labels=diagsubgroup_lab,ordered=T)
  
  
  return(df)
}


######################################################################################
#####################AVSD_specific_violation_module
######################################################################################

AVSD_specific_violation_module<-function(df){
  #Exclude patients if had tetralogy diagnostic code (inlcusion step 1 c) and coaction code Table P
  
  df$coactioncode=0
  coactioncodelist=c("092901","121800","121801","121802","121803") 
  # search diagnosis filed
  for(i in 1:29){
    shortcode=df[[paste0("diagcode",i)]]
    df=searchforevidence_generic(mark="coactioncode",usingcode=coactioncodelist)
  }
  
  # search morbidity filed
  for(i in 1:16){
    shortcode=df[[paste0("comorbidity",i)]]
    df=searchforevidence_generic(mark="coactioncode",usingcode=coactioncodelist)
  }
  
  
  # search  proc filed
  for(i in 1:7){
    shortcode=df[[paste0("proccode",i)]]
    df=searchforevidence_generic(mark="coactioncode",usingcode=coactioncodelist)
  }
  
  # search  prev proc filed
  for(i in 1:26){
    shortcode=df[[paste0("prevproccode",i)]]
    df=searchforevidence_generic(mark="coactioncode",usingcode=coactioncodelist)
  }
  
  
  df<-df %>%
    group_by(patid) %>%
    mutate(patcoactioncode=max(coactioncode)) %>%
    ungroup()
  
  
  df<-df %>%
    mutate(pat_violation=ifelse(patcoactioncode==1 & AVSD_step1c==1,1,pat_violation)) %>%
    mutate(pat_violationinf=ifelse(patcoactioncode==1 & AVSD_step1c==1,paste(pat_violationinf,"; tetralogy with coaction code"),pat_violationinf)) 
    
  return(df)
}

######################################################################################
########## AVSD specific suspected  missing/miscoded data module
######################################################################################
AVSD_specific_suspected_missing_miscoded_data_module<-function(df){
  
  #####################if SV pathway
  
  # min age at SV pathway 
  df$minageSVpathway=apply(df[,which(names(df) %in% c("patAge2","patAge3"))],1,function(x) min(x,na.rm=T))
  df$minageSVpathway[is.infinite(df$minageSVpathway)]=NA
  
  #tetralogy AVSD or unbalanced AVSD: under SV pathway but had no SV pathway procedure before year 5

  df<-df %>%
    mutate(Flagcentre_missingdata=ifelse(diagsubgroup %in% c(1,2) & !is.na(minageSVpathway) & minageSVpathway>5 & !(patAge6<5 & !is.na(patAge6) ),1,Flagcentre_missingdata))  %>%
    mutate(Flagcentre_missingdatainf=ifelse(diagsubgroup %in% c(1,2) & !is.na(minageSVpathway) & minageSVpathway>5 & !(patAge6<5 & !is.na(patAge6)) ,
                                            paste(Flagcentre_missingdatainf,"; tetralogy and unbalanced AVSD - in SV pathway but had no SV stage two/three by year 5"),Flagcentre_missingdatainf))  

  #####################if BV pathway
  #Tetralogy AVSD, Unbalanced AVSD  and Complete AVSD patients had no reparative procedure by age 2 years old (note patients who only have a reparative procedure at older age can be included)
  df<-df %>%
    mutate(Flagcentre_missingdata=ifelse( diagsubgroup %in% c(1,2,4) & is.na(minageSVpathway) & is.na(patAge4)  & ageatlastknownstatus>2  & !(patAge6<2 & !is.na(patAge6)),
                                          1,Flagcentre_missingdata))  %>%
    mutate(Flagcentre_missingdatainf=ifelse(diagsubgroup %in% c(1,2,4) & is.na(minageSVpathway) & is.na(patAge4)  & ageatlastknownstatus>2  & !(patAge6<2 & !is.na(patAge6)),
                                             paste(Flagcentre_missingdatainf,";tetralogy, unbalanced and complete AVSD - under BV pathway but had no reprative procedure whlist surviving by 2 years old (had reprative proc at later age allowed)"),Flagcentre_missingdatainf))  
  
  #stage one D but no evidence of tetralogy AVSD
  
  df<-df %>%
    mutate(Flagcentre_missingdata=ifelse(diagsubgroup %in% c(2,3,4) & is.na(minageSVpathway)  & patsub_stage1==4 , 1,Flagcentre_missingdata))  %>%
    mutate(Flagcentre_missingdatainf=ifelse(diagsubgroup %in% c(2,3,4) & is.na(minageSVpathway)  & patsub_stage1==4 ,
                                            paste(Flagcentre_missingdatainf,";stage one D but no evidence of tetralogy AVSD"),Flagcentre_missingdatainf))  
  
 
  return(df)
}

######################################################################################
########## AVSD specific minor data missing or unusual records
######################################################################################

AVSD_specific_minor_data_errors_module<-function(df){
  
  #Patients are tetralogy AVSD in diagnosis subgroup but had  other types  reparative procedure for AVSD (not tetralogy AVSD repair).   
  
  df<-df %>%
    mutate(Flagcentre_minor=ifelse(diagsubgroup==1 & !patsub_BVpathway==1,1,Flagcentre_minor))  %>%
    mutate(Flagcentre_minorinf=ifelse(diagsubgroup==1 & !patsub_BVpathway==1, paste(Flagcentre_minorinf,"; Tet AVSD patients had other types of reparative procdure for AVSD (other than tet AVSD repair)"),Flagcentre_minorinf)) 
  
  
  
  #Patients who had their pathway procedure identified by combined codes 
  
  
  df<-df %>%
    group_by(patid) %>%
    mutate(pataux=max(aux)) %>%
    ungroup()
  
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
    mutate(pataux=max(CatProc==4 & CombCode==1)) %>%
    ungroup()
  
  df<-df %>%
    mutate(Flagcentre_minor=ifelse(pataux==1,1,Flagcentre_minor))  %>%
    mutate(Flagcentre_minorinf=ifelse(pataux==1, paste(Flagcentre_minorinf,"; patients had their tetralogy AVSD repair indentified by combined codes"),Flagcentre_minorinf)) 
  
  #Patients who had type iv and v reparative procedure for AVSD. 
  df<-df %>%
    mutate(Flagcentre_minor=ifelse(patsub_BVpathway %in% c(4,5),1,Flagcentre_minor))  %>%
    mutate(Flagcentre_minorinf=ifelse(pataux==1, paste(patsub_BVpathway %in% c(4,5),"; AVSD pathway incomplete/poor coding"),Flagcentre_minorinf)) 
  
  
  #need to replace minor data missing as 0 if have been flagged  as suspected missing/miscoded
  df<-df %>%
    mutate(Flagcentre_minor=ifelse(Flagcentre_missingdata==1 ,0,Flagcentre_minor))  %>%
    mutate(Flagcentre_minorinf=ifelse(Flagcentre_missingdata==1 ,"",Flagcentre_minorinf))  
  
  return(df)
  
}
