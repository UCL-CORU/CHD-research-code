# ALL generic functions 

tabpatnum<-function(x,main){
  x=x[which(main$patentry==1)]
  table(x,useNA="ifany")
}
falseifNA<-function(x){
  ifelse(is.na(x),F,x)
}
### load main dataset with correct format 

loadmaindata<-function(datadirectory){
  data=read.csv(datadirectory,colClasses = c(
    ProcSeq= "character",
    diagcode1 = "character",diagcode2="character",diagcode3 = "character",diagcode4="character",diagcode5 = "character",diagcode6="character",diagcode7="character",diagcode8="character",diagcode9="character",diagcode10="character",
    diagcode11 = "character",diagcode12="character",diagcode13 = "character",diagcode14="character",diagcode15 = "character",diagcode16="character",diagcode17="character",diagcode18="character",diagcode19="character",diagcode20="character",
    diagcode21 = "character",diagcode22="character",diagcode23 = "character",diagcode24="character",diagcode25 = "character",diagcode26="character",diagcode27="character",diagcode28="character",diagcode29="character",
    proccode1 = "character",proccode2="character",proccode3 = "character",proccode4="character",proccode5 = "character",proccode6="character",proccode7="character",
    comorbidity1 = "character",comorbidity2="character",comorbidity3 = "character",comorbidity4="character",comorbidity5 = "character",comorbidity6="character",comorbidity7="character",comorbidity8="character",comorbidity9="character",comorbidity10="character",
    comorbidity11 = "character",comorbidity12="character",comorbidity13 = "character",comorbidity14="character",comorbidity15 = "character",comorbidity16="character",
    prevproccode1 = "character",prevproccode2="character",prevproccode3 = "character",prevproccode4="character",prevproccode5 = "character",prevproccode6="character",prevproccode7="character",prevproccode8="character",prevproccode9="character",prevproccode10="character",
    prevproccode11 = "character",prevproccode12="character",prevproccode13 = "character",prevproccode14="character",prevproccode15 = "character",prevproccode16="character",prevproccode17="character",prevproccode18="character",prevproccode19="character",prevproccode20="character",
    prevproccode21 = "character",prevproccode22="character",prevproccode23 = "character",prevproccode24="character",prevproccode25 = "character",prevproccode26="character"))
  return(data)
}



##########  generic search code function
searchforevidence_generic<-function(mark,usingcode){
  for( j in 1:length(usingcode)){
    index=which(shortcode %in% usingcode[j]) # search whether shortcode contain the code in using code
    df[index,c(mark)]=1
  }
  return(df)
}
########## generic violation rule: remove patients with non-contributory records only
Generic_violation_module<-function(df){
  
  df<-df %>%
    mutate(aux=ifelse(CatProc==8,0,1)) 
  
  df<-df %>%
    group_by(patid) %>%
    mutate(pataux=max(aux))  %>%
    ungroup()
  
  df<-df %>%
    mutate(pat_violation=ifelse(pataux==0,1,0))  %>%
    mutate(pat_violationinf=ifelse(pataux==0,"non-contributory records only",""))  
  

  return(df)
  
}
########## Generic suspected data missing module:
#If single ventricle pathway then flag patients with suspected missing/miscoded data 
#  Patients recorded as having stage two at less than one month old 
#  Patients recorded as having stage three at less than six months old
######################################################################################
Generic_suspected_missing_miscoded_data_module<-function(df){
  df<-df %>%
    mutate(Flagcentre_missingdata=ifelse( patAge2<1/12 & !is.na(patAge2),1,0))  %>%
    mutate(Flagcentre_missingdatainf=ifelse( patAge2<1/12 & !is.na(patAge2),"age of SV stage 2 less than 1 month old",""))  %>%
    mutate(Flagcentre_missingdata=ifelse( patAge3<1/2 & !is.na(patAge3),1,Flagcentre_missingdata))  %>% 
    mutate(Flagcentre_missingdatainf=ifelse( patAge3<1/2 & !is.na(patAge3),paste(Flagcentre_missingdatainf,"; age of SV stage 2 less than 1 month old"),Flagcentre_missingdatainf))  
  
  return(df)
}  
######################################################################################
##########generic flag to centres: minor data error if patients had bypass surgery as pre-pathway
#######################################################################################

Generic_minor_data_errors_module<-function(df){
  
  df<-df %>%
    group_by(patid) %>%
    mutate(pataux=max(CatProc==0 & aa_allocation==1))  %>%
    ungroup()
  
  df<-df %>%
    mutate(Flagcentre_minor=ifelse(pataux==1,1,0))  %>%
    mutate(Flagcentre_minorinf=ifelse(pataux==1,"Bypass pre-pathway",""))  
  
  return(df)
  
}

######################################################################################
##########generic assign comorbidity
######################################################################################

Generic_assign_comorbidity<-function(df){
  
  #severity of illness map
  SOImap<-read.csv(dir_SOImap,colClasses = "character")
  #pre-term birth  map
  Premmap<-read.csv(dir_Premmap,colClasses = "character")
  
  #congenital comorbidity map
  CongComorbmap<-read.csv(dir_CongComorbmap,colClasses = "character")
  
  #additional cardiac risk  map
  ACRmap<-read.csv(dir_ACRmap,colClasses = "character")
  
  #acquired comorbidity map
  AcqComorbmap<-read.csv(dir_AcqComorbmap,colClasses = "character")
  
  #####################define functions #################################
  # function: search for  evidence of comorbidty
  searchforevidence_comorbid<-function(){
    # severity of illness evidence 
    usingcode=SOImap$shortcode
    for( j in 1:length(usingcode)){
      index=which(shortcode %in% usingcode[j])
      df$SOI[index]=1
    }
    # acquired comorbidity 
    usingcode=AcqComorbmap$shortcode
    for( j in 1:length(usingcode)){
      index=which(shortcode %in% usingcode[j])
      df$AcqComorb[index]=1
    }
    
    # pre-term birth  evidence 
    usingcode=Premmap$shortcode
    for( j in 1:length(usingcode)){
      index=which(shortcode %in% usingcode[j])
      df$Premat[index]=1
    }
    
    # congenital comorbidity evidence
    usingcode=CongComorbmap$shortcode
    for( j in 1:length(usingcode)){
      index=which(shortcode %in% usingcode[j])
      df$CongComorb[index]=1
    }
    
    # additional cardiac risk  evidence
    usingcode=ACRmap$shortcode
    for( j in 1:length(usingcode)){
      index=which(shortcode %in% usingcode[j])
      df$ACR[index]=1
    }
    
    # Downs
    usingcode="140102"
    for( j in 1:length(usingcode)){
      index=which(shortcode %in% usingcode[j])
      df$Downs[index]=1
    }
    return(df)
  }
  
  
  ######################search evidence in each fields###################################################
  
  
  # assign variables
  
  df$Premat=0 #pre-term birth   
  df$CongComorb=0 #congenital comorbidity
  df$Downs=0 #Downs
  
  # search diagnosis filed
  for(i in 1:29){
    shortcode=df[[paste0("diagcode",i)]]
    df=searchforevidence_comorbid()
  }
  
  # search morbidity filed
  for(i in 1:16){
    shortcode=df[[paste0("comorbidity",i)]]
    df=searchforevidence_comorbid()
  }
  
  # search  proc filed
  for(i in 1:7){
    shortcode=df[[paste0("proccode",i)]]
    df=searchforevidence_comorbid()
  }
  
  # search  prev proc filed
  for(i in 1:26){
    shortcode=df[[paste0("prevproccode",i)]]
    df=searchforevidence_comorbid()
  }
  
  
  
  #assign patient level comorbidty
  
  df<-df %>%
    group_by(patid) %>%
    mutate(patDowns =ifelse(max(Downs),1,0)) %>%
    mutate(patCongComorb =ifelse(max(CongComorb),1,0)) %>%
    mutate(patpremat =ifelse(max(Premat),1,0)) %>%
    ungroup()
  
  
  ######## mark first cardiac procedure
  
  df<-df %>%
    mutate(isCardiac =ifelse(!CatProc==8,1,0)) 
  
  df<-df %>%
    group_by(patid) %>%
    mutate(isFirstCardiac = ifelse(!is.na(which(isCardiac==1)[1]) & which(isCardiac==1)[1]==patentry,1,0))%>%
    ungroup()  
  
  # mark age and weight at index procedure
  df<-df %>%
    arrange(patid,isFirstCardiac) %>%
    group_by(patid) %>%
    mutate(AgeIndexProc = last(ageatop))%>%
    mutate(WeightIndexProc=last(weight))%>%
    ungroup()  
  
  df<-df %>%
    mutate(LWIndexProc = ifelse( !is.na(WeightIndexProc) & WeightIndexProc<2.5,1,
                                 ifelse(!is.na(WeightIndexProc) & WeightIndexProc>=2.5,0,2))) # low weight 
  
  
  LW_lab=c("0: weight at first cardiac procedure >2.5Kg","1:low weight at first cardiac procedure < 2.5Kg","2: missing data")
  df$LWIndexProcLabel<-factor(df$LWIndexProc,levels=c(0:2),labels=LW_lab,ordered=T)
  
  df<-df %>%
    arrange(patid, patentry) 
  
  return (df)
}


######################################################################################
##########generic compute reintervention status
# of note, the maximum follow-up time is up to the last NCHDA date
######################################################################################
generic_reintervention_status_module=function(df,my_condition){
  df<-df %>%
    mutate(Reint = ifelse(my_condition,1,0) )
  
  
  df<-df %>%
    group_by(patid) %>%
    mutate(numReint =sum(Reint) ) %>%
    mutate(isFirstReint = ifelse(!is.na(which(Reint==1)[1]) & which(Reint==1)[1]==patentry,1,0))%>%
    ungroup()  
  
  
  df<-df %>%
    arrange(patid,isFirstReint) %>%
    group_by(patid) %>%
    mutate(AgeatFirstReint = last(ageatop))%>%
    ungroup()  
  
  
  df<-df %>%
    group_by(patid) %>%
    mutate(Status_Reint = ifelse(numReint>=1,1,
                                 ifelse(numReint==0 & (lastknownstatus_NCHDA==1 |  patHT==1),2,0)))%>%
    ungroup()  
  
  Status_ReintLab<-c("0: Loss to follow-up","1: Occurance of the reintervention",
                     "2: Death or heart transplant without reintervention intervention")
  df$Status_ReintLab<-factor(df$Status_Reint,levels=c(0:2),labels=Status_ReintLab,order=T)
  
  
  # time of status (since birth)
  df<-df %>%
    mutate(timeofStatus_Reint = ifelse(Status_Reint==0,ageatlastknownstatus_NCHDA, #censoring
                                       ifelse(Status_Reint==1,AgeatFirstReint, #reintervention occurred
                                              ifelse(Status_Reint==2 & patHT==1,patAge6,ageatlastknownstatus_NCHDA))))  #age at heart transplant or age at death
  
  df<-df %>%
    mutate(timeofStatus_Reint = ifelse(timeofStatus_Reint==0,0.5/365.25,timeofStatus_Reint))
}                   



######################################################################################
##########generic descriptive analysis
######################################################################################
Generic_descriptive_analysis<-function(df){
  
  # assign comorbidity
  df=Generic_assign_comorbidity(df)
  
  ######### mark patients who had certain pathway procedure.
  ######### Note: depends on the condition, some may be 0 for all patients. for example, in BV conditions, none had SV stage three 
  df<-df %>%
    group_by(patid) %>%
    mutate(pathasStage1 = max(CatProc==1))%>%
    mutate(pathasSVStage2 = max(CatProc==2))%>%
    mutate(pathasSVStage3 = max(CatProc==3))%>%
    mutate(pathasBVpathway = max(CatProc==4))%>%
    mutate(pathasSVpathway = max(CatProc==2 | CatProc==3 ))%>%
    mutate(pathasPathway = max(ispathway==1)) %>%
    ungroup()  
  
  if(CHDtype == "AVSD"){
    df<-df %>%
      mutate(pathasSVpathway = ifelse(patsub_stage1==1,1,pathasSVpathway) )#AVSD: SV pathway if Norwood type stage 1 
  }
  ######### mark patients' pathway management route 
  # assign pathway type
  if(CHDtype %in% c("HLHS","FUH")){
    df$pathwaytype="single ventricle pathway"
  }else{
    df<-df %>%
      mutate(pathwaytype = ifelse(pathasSVpathway==1,"single ventricle pathway","biventricular reparative pathway"))
  }
  ######### compute the reintervention status
  df<-df[,!grepl("^Status_",names(df))]
  df<-df[,!grepl("^timeofStatus_",names(df))]
  df<-df[,!grepl("^num",names(df))]
  
  # all re intervention
  my_condition= df$CatProc==7 &  !is.na(df$ageatop) & !(df$ageatop>df$patAge6 & !is.na(df$patAge6)) 
  df=generic_reintervention_status_module(df,my_condition)
  
  table(df$Status_ReintLab)
  df <- df %>%  
    rename(Status_allReint=Status_Reint,Status_allReintLab=Status_ReintLab,timeofStatus_allReInt=timeofStatus_Reint,numallReint=numReint)
  
  table(df$Status_allReintLab)
  
  # surgical re intervention
  my_condition= df$CatProc==7 & df$InterType==1 & !is.na(df$ageatop) & !(df$ageatop>df$patAge6 & !is.na(df$patAge6)) 
  df=generic_reintervention_status_module(df,my_condition)
  
  table(df$Status_Reint[which(df$patentry==1)])
  
  df <- df %>%  
    rename(Status_HSReint=Status_Reint,Status_HSReintLab=Status_ReintLab,timeofStatus_HSReInt=timeofStatus_Reint,numHSReint=numReint)
  
  # catheter re intervention
  my_condition= df$CatProc==7 & df$InterType==2 & !is.na(df$ageatop) & !(df$ageatop>df$patAge6 & !is.na(df$patAge6)) 
  df=generic_reintervention_status_module(df,my_condition)
  
  table(df$Status_Reint[which(df$patentry==1)])
  
  df <- df %>%  
    rename(Status_CEReint=Status_Reint,Status_CEReintLab=Status_ReintLab,timeofStatus_CEReInt=timeofStatus_Reint,numCEReint=numReint)
  
  
  ######### number of intervention, pre-pathway and re intervention
  countbycondition<-function(condition){
    x=ifelse(condition,1,0)
    return(x)
  }
  df<-df %>%
    group_by(patid) %>%
    mutate(numPrePath = sum(countbycondition(CatProc==0)))%>%
    mutate(numIntby1 = sum(countbycondition(!CatProc==8) & ((ageatop<1 & !is.na(ageatop))| (ageatdis<1 & !is.na(ageatdis)))))%>% # number of total intervention by year 1
    mutate(numPathwayby1 = sum(countbycondition(ispathway==1) & ((ageatop<1 & !is.na(ageatop))| (ageatdis<1 & !is.na(ageatdis)))))%>% # number of pathway by year 1
    ungroup()  
  
  df<-df %>%
    mutate(SexMale = ifelse(is.na(gender),2,
                            ifelse(gender==1,1,0)))
  
  if((CHDtype=="AVSD")){
    df<-df %>%
      mutate(pat15V =ifelse(!diagsubgroup==1,0,pat15V)) #in AVSD, 1.5 V patients only exist in teratology AVSD
  }
  
  if(!CHDtype%in%c("AVSD","PA","TGA")){
    df$pat15V=0
  }
  
  
  df<-df %>%
    arrange(patid, patentry) 
  
  return(df)
}
######################################################################################
##########generic do file: save patient level data in both .dta and .csv
######################################################################################
Generic_save_patient_level_data_module<-function(df){
  
  index=which(df$patentry==1)
  if(CHDtype=="FUH"){
    index=which(df$patentry==1 & df$diagsubgroup %in% c(1,2)) #for FUH, we only work on DIV and Tricuspid atresia  
  }
  
  df1=df[index,]
  
  df1<-df1 %>%
    select(patid,dobyear,patCongComorb,patpremat,SexMale,pathwaytype,patHT,FinYr_birth,LWIndexProc,LWIndexProcLabel,
           AcqComorbIndexProc,SOIIndexProc,AcqComorbIndexProc,patACR,pat15V,patDowns,patCongComorb,
           starts_with("ageatlastknownstatus"),starts_with("lastknownstatus"),starts_with("num") ,starts_with("patsub"),starts_with("diagsubgroup"),
           starts_with("pathas"),starts_with("Status_"),starts_with("timeofStatus_"),starts_with("Flagcentre"),starts_with("ProcSeq"),starts_with("patAge"))
  
  return(df1)
}


######################################################################################
##########generic compute metrics:  survival and reintervention at year 1, 5 and 10 years
######################################################################################

### define functions  
# function: round the data with fixed decimal place
formatround<-function(data, decimal_point=1){
  data=as.data.frame(data)
  output<-matrix(NA,nrow(data),ncol(data))
  for(i in 1:nrow(data))
    for(j in 1:ncol(data)){{
      x=round(as.numeric(data[i,j]),decimal_point)
      output[i,j]<-format(x, nsmall = decimal_point)
      if(x==0){output[i,j]=0}
      if(x==100){output[i,j]=100}
      
    }}
  
  return(output)
}




# function: compute survival rates at certain age
survialrates<-function(df,ages=c(1,5,10),decimal_point=1 ){
  #df: dataset
  #CI: confidence interval 
  #digits: decimal points 
  fit=survfit(Surv(ageatlastknownstatus, lastknownstatus)~1, data=df)
  A=summary(fit, times=ages)
  surv=formatround(A$surv*100,decimal_point)
  lower=formatround(A$lower*100,decimal_point)
  upper=formatround(A$upper*100,decimal_point)
  
  
  Survivalrates=matrix(NA,1,length(ages))
  for(i in 1:length(ages)){
    if(A$surv[i]==1){    Survivalrates[1,i+1]="100%"}else{
      Survivalrates[1,i]=paste0(surv[i],"% (",lower[i],"%-",upper[i],"%)")
    }
  }
  colnames(Survivalrates)=c(paste0(ages,"-year"))
  
  return(Survivalrates)
}

# function: compute cumulative incidence of reintervention at certain age
Reintrates<-function(status,timeofstatus,ages=c(1,5,10)){
  #status: status of reintervention 0/censored 1/reintervention observed 2/competing events observed
  #timeofstatus: time of status
  
  maxtime=max(ages)
  fit=cuminc(timeofstatus,status,cencode=0)
  fipts.times <- timeofstatus
  
  fitpts.est <- timepoints( fit, fipts.times )$est
  time=as.numeric(colnames(fitpts.est))
  
  fitpts.var <- timepoints( fit, fipts.times )$var
  fitpts.CIdelta <- 1.96 * sqrt(fitpts.var) / (fitpts.est * log(fitpts.est) )
  fitpts.CIdeltaL = round(fitpts.est ^ exp( -fitpts.CIdelta ) * 100,2) # 2.5% quartile
  fitpts.CIdeltaR = round(fitpts.est ^ exp(  fitpts.CIdelta ) * 100, 2)  # 97.5% quartile
  fitpts.est <- round( 100*fitpts.est, 2) # estimates
  
  CIF<-matrix(NA,1,length(ages))
  
  for(i in 1:length(ages)){
    estimate=fitpts.est[1,which.min(abs(time-ages[i]))]
    M=formatround(estimate,1) # estimates
    Q25th=formatround(fitpts.CIdeltaL[1,which.min(abs(time-ages[i]))],1) # 2.5% quartile
    Q975th=formatround(fitpts.CIdeltaR[1,which.min(abs(time-ages[i]))],1) # 97.5% quartile
    if(estimate==0){CIF[1,i+1]="0%"}else{
      CIF[1,i]=paste0(M,"%"," (",Q25th,"%-",Q975th,"%)")
    }
    
  }
  colnames(CIF)=c(paste0(ages,"-year"))
  
  return(CIF)
}