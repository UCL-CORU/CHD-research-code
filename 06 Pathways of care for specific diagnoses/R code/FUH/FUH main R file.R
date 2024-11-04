###################################################################################################
## CHAMPION WS2: long term outcome by diagnosis. R Modular  programming
## Aug 2024 Qi Huang 
rm(list=ls())

# all CHD diagnoses in order of decreasing complexity. one need to identify patients in this order
allCHDs=c("HLHS","FUH","TGA","PA","AVSD","TOF","COA","AOS","VSD") 

#select CHD type
CHDtype="FUH"
dir_GenericRfile_champion= "S:/CHAMPION/Qi_folder/WS2 Modular programming in R/generic files"
source("S:/CHAMPION/Qi_folder/WS2 Modular programming in R/generic files/Generic global directory.R") # load directory of all dataset 
source("S:/CHAMPION/Qi_folder/WS2 Modular programming in R/generic files/Generic modules.R") # load all generic functions 

source(paste0(dir_FUHRfile_champion,"/", CHDtype," modules.R")) # load all FUH specific functions 


# select 
inclusionexlcusion=0 #set here to 1 if you want to select  patients with certain type of CHD from the NCHDA
pathway=0 #set here to 1 if you want to identify pathway for patients with certain type of CHD
FP=0 #set here to 1 if you want to do further processing after the pathway analysis (mark patients who met the violation rule, and patients with suspected missing data/minor data errors)
descriptiveanalysis=0 #set here to 1 if you want to perform descriptive analysis for patients with certain type of CHD
PatientLevelData=0   #set here to 1 if you want to save patient level data for patients with certain type of CHD
metrics=0 #set here to 1 if you want to compute metrics for patients with certain type of CHD

# inclusion and exclusion (select patients with certain CHD)
if(inclusionexlcusion==1){
  #load the main dataset
  NCHDAdata=loadmaindata(dir_NCHDAdataset)
  
  ########################### run inclusion and exclusion module 
  NCHDAdata= FUH_inclusion_and_exclusion_module(NCHDAdata)
  
  # derived variable.   marker of FUH patients
  table(NCHDAdata$FUHpat[which(NCHDAdata$patentry==1)],useNA="ifany")

  
  ########################### save the dataset
  write.csv(NCHDAdata,dir_NCHDAdataset,row.names = F)
  
  # save the FUH dataset (raw data)
  FUHdata_raw=NCHDAdata[which(NCHDAdata$FUHpat==1),]
  FUHdata_raw$CHDtype=CHDtype
  write.csv(FUHdata_raw,dir_data_raw,row.names = F)
}

#################################################
# Identify pathway and diagnostic subgroups and assign diagnostic subgroup
#################################################

if(pathway==1){
  #load the main dataset for FUH
  FUHdata_raw=loadmaindata(dir_data_raw)
  ########################### run pathway module
  FUHdata_raw=FUH_pathway_module(FUHdata_raw)
  
  # derived variable.
  
  # category of each procedure (record level)
  table(FUHdata_raw$CatProcLabel,useNA="ifany") # category of each procedure. in FUH, 4 is empty because FUH is exclusively SV
  # CatProc_lab=c("0: Pre-pathway","1: Palliative first stage procedure","2: SV stage two","3: SV stage three (Fontan)",
  #               "4: Reparative procedure (empty in FUH)" ,"5: Other pathway (part of hybrid)","6: Heart transplant" ,
  #               "7: Off-pathway", "8: Excluded")
  
  #Procedure sequence in patient level, for example 123 means patients had stage 1,2 and 3
  table(FUHdata_raw$ProcSeq[which(FUHdata_raw$patentry==1)],useNA="ifany")
  
  ########################### diagnosis subtype  

  FUHdata_raw=FUH_diagnosis_subtype_module(FUHdata_raw)
  # derived variable.  
  table(FUHdata_raw$diagsubgroupLab[which(FUHdata_raw$patentry==1)],useNA="ifany")
  
  ########################### save the dataset
  write.csv(FUHdata_raw,dir_data_raw,row.names = F)
}

#################################################
##Further processing after pathway analysis:
##1.violation rule, remove patients if they meet the violation rule
##2.flag for enquiry to centres, patients will be maintained in the dataset, but create a flag for centres
#################################################

if(FP==1){
  #load the main dataset for FUH
  FUHdata_raw=loadmaindata(dir_data_raw)
  
  ####violation: patients will be removed from the cohort
  
  #generic violation rule: remove patients with non-contributory records only
  FUHdata_raw=Generic_violation_module(FUHdata_raw)
  
  #FUH specific violation rule:
  #FUH  extra exclusion step c) and d) must be done after pathway analysis. here put it together with  violation rule
  FUHdata_raw=FUH_specific_violation_module(FUHdata_raw)
  
  #important var. FUHextraexclusion: FUH  extra exclusion step c) and d) must be done after pathway analysis. here put it together with  violation rule  tab FUHextraexclusion if patentry==1 //116
  table(FUHdata_raw$FUHextraexclusion[which(FUHdata_raw$patentry==1)],useNA="ifany")
  
  
  #important var. pat_violation: patients who need to removed according to the violation rule
  table(FUHdata_raw$pat_violation[which(FUHdata_raw$patentry==1)],useNA="ifany")
  table(FUHdata_raw$pat_violationinf[which(FUHdata_raw$patentry==1)])  # the reason for violation
  
  
  ####flag for enquiry to centres: patients will be maintained in the dataset, but create a flag for centres
  
  # flag patients with suspected missing/miscoded data
  # generic rule: mark patients if had SV stage two/three at a very young age
  
  FUHdata_raw=Generic_suspected_missing_miscoded_data_module(FUHdata_raw)
  
  
  #FUH specific suspected rule
  FUHdata_raw=FUH_specific_suspected_missing_miscoded_data_module(FUHdata_raw)
  
  table(FUHdata_raw$Flagcentre_missingdata[which(FUHdata_raw$patentry==1)],useNA="ifany")
  table(FUHdata_raw$Flagcentre_missingdatainf[which(FUHdata_raw$patentry==1)]) # the reason 
  
  #### minor data errors/unusual records 
  #generic rule:  minor data error if patients had bypass surgery as pre-pathway
  FUHdata_raw=Generic_minor_data_errors_module(FUHdata_raw)
  
  #FUH specific minor data errors rule:  
  FUHdata_raw=FUH_specific_suspected_missing_miscoded_data_module(FUHdata_raw)
  
  table(FUHdata_raw$Flagcentre_minor[which(FUHdata_raw$patentry==1)],useNA="ifany") # marker of Flagcentre_minor
  table(FUHdata_raw$Flagcentre_minorinf[which(FUHdata_raw$patentry==1)]) # the reason 
  
  
  ########################### save the dataset
  write.csv(FUHdata_raw,dir_data_raw,row.names = F)
  
  # remove patients who met the violation rule
  
  FUHdata_final=FUHdata_raw[-which(FUHdata_raw$FUHextraexclusion==1 | FUHdata_raw$pat_violation==1),]
  write.csv(FUHdata_final,dir_data_final,row.names = F)
  
}
######################################################################################
########## descriptive analysis, i.e., assign comorbidity and compute number of intervention including pathway, pre-pathway and reintervention 
######################################################################################
  
  
if(descriptiveanalysis==1){
  #load the main dataset for FUH
  FUHdata_final=loadmaindata(dir_data_final)
  
  FUHdata_final=Generic_descriptive_analysis(FUHdata_final)
  # derived variables
  # the following are all in patient level
  #  var. patpremat: pre-term birth 
  #  var. patCongComorb: congenital comorbidity
  #  var. patDowns: Downs syndrome
  #  var. pathas*: mark patients who had certain pathway
  #  var. numPrePath/numallReint/numHSReint/numCEReint: number of pre-pathway and number of re intervention (all, surgery and catheter)
  #  var. Status_allReint/Status_HSReint/Status_CEReint: the status of reinterventions: had reintervention/death or heart transplant without reintervention/censoring without reintervention
  #  var. numIntby1/numPathwayby1: number of intervention and pathway  by age 1 old
  #  var. pathwaytype whether the patient was managed under a single ventricle patwhay or a biventricular pathway. 
  

  write.csv(FUHdata_final,dir_data_final,row.names = F)
}


######################################################################################
########## save patient level data
######################################################################################

if(PatientLevelData==1){
  FUHdata_final=loadmaindata(dir_data_final)

  FUHpatientleveldata=Generic_save_patient_level_data_module(FUHdata_final)
  write.csv(FUHpatientleveldata,dir_dataPatientLevel,row.names = F)
  
}


#################################################
##Further processing after pathway analysis:
##1.violation rule, remove patients if they meet the violation rule
##2.flag for enquiry to centres, patients will be maintained in the dataset, but create a flag for centres
#################################################

if(FP==1){
  #load the main dataset for FUH
  FUHdata_raw=loadmaindata(dir_data_raw)
  
  ####violation: patients will be removed from the cohort
  
  #generic violation rule: remove patients with non-contributory records only
  FUHdata_raw=Generic_violation_module(FUHdata_raw)
  
  #FUH specific violation rule:
  #FUH  extra exclusion step c) and d) must be done after pathway analysis. here put it together with  violation rule
  FUHdata_raw=FUH_specific_violation_module(FUHdata_raw)
  
  #important var. FUH extraexclusion: FUH  extra exclusion step c) and d) must be done after pathway analysis. here put it together with  violation rule  tab FUHextraexclusion if patentry==1 //116
  table(FUHdata_raw$FUHextraexclusion[which(FUHdata_raw$patentry==1)],useNA="ifany")
  
  
  #important var. pat_violation: patients who need to removed according to the violation rule
  table(FUHdata_raw$pat_violation[which(FUHdata_raw$patentry==1)],useNA="ifany")
  table(FUHdata_raw$pat_violationinf[which(FUHdata_raw$patentry==1)])  # the reason for violation
  
  
  ####flag for enquiry to centres: patients will be maintained in the dataset, but create a flag for centres
  
  # flag patients with suspected missing/miscoded data
  # generic rule: mark patients if had SV stage two/three at a very young age
  
  FUHdata_raw=Generic_suspected_missing_miscoded_data_module(FUHdata_raw)
  
  
  #FUH specific suspected rule
  FUHdata_raw=FUH_specific_suspected_missing_miscoded_data_module(FUHdata_raw)
  
  table(FUHdata_raw$Flagcentre_missingdata[which(FUHdata_raw$patentry==1)],useNA="ifany")
  table(FUHdata_raw$Flagcentre_missingdatainf[which(FUHdata_raw$patentry==1)]) # the reason 
  
  #### minor data errors/unusual records 
  #generic rule:  minor data error if patients had bypass surgery as pre-pathway
  FUHdata_raw=Generic_minor_data_errors_module(FUHdata_raw)
  
  #FUH specific minor data errors rule:  
  FUHdata_raw=FUH_specific_minor_data_errors_module(FUHdata_raw)
  
  table(FUHdata_raw$Flagcentre_minor[which(FUHdata_raw$patentry==1)],useNA="ifany") # marker of Flagcentre_minor
  table(FUHdata_raw$Flagcentre_minorinf[which(FUHdata_raw$patentry==1)]) # the reason 
  
  
  ########################### save the dataset
  write.csv(FUHdata_raw,dir_data_raw,row.names = F)
  
  # remove patients who met the violation rule
  
  FUHdata_final=FUHdata_raw[-which(FUHdata_raw$FUHextraexclusion==1 | FUHdata_raw$pat_violation==1),]
  write.csv(FUHdata_final,dir_data_final,row.names = F)
  
}



######################################################################################
########## descriptive analysis, i.e., assign comorbidity and compute number of intervention including pathway, pre-pathway and reintervention 
######################################################################################


if(descriptiveanalysis==1){
  #load the main dataset for FUH
  FUHdata_final=loadmaindata(dir_data_final)
  
  FUHdata_final=Generic_descriptive_analysis(FUHdata_final)
  
  # the following are all in patient level
  # important var. patpremat: pre-term birth 
  # important var. patCongComorb: congenital comorbidity
  # important var. patDowns: Downs syndrome
  # important var. pathas*: mark patients who had certain pathway
  # important var. numPrePath/numReint/numallReint/numHSReint/numCEReint: number of pre-pathway and number of re intervention (all, surgery and catheter)
  # important var. Status_allReintLab/Status_HSReintLab/Status_CEReintLab: the status of reinterventions: had reintervention/death or heart transplant without reintervention/censoring without reintervention
  # important var. numIntby1/numPathwayby1: number of intervention and pathway  by age 1 old
  
  table(FUHdata_final$patpremat[which(FUHdata_final$patentry==1)],useNA="ifany") 
  table(FUHdata_final$Status_allReintLab[which(FUHdata_final$patentry==1)],useNA="ifany") 
  write.csv(FUHdata_final,dir_data_final,row.names = F)
}


######################################################################################
########## save patient level data
######################################################################################

if(PatientLevelData==1){
  FUHdata_final=loadmaindata(dir_data_final)

  FUHpatientleveldata=Generic_save_patient_level_data_module(FUHdata_final)
  write.csv(FUHpatientleveldata,dir_dataPatientLevel,row.names = F)
  
}


######################################################################################
########## compute metrics. 
######################################################################################
if(metrics==1){
  FUHpatientleveldata=read.csv(dir_dataPatientLevel,colClasses = c(ProcSeq= "character"))
  
  # compute survival cumulative incidence rates at age 1-year, 5-years and 10-years (using Kaplan-meier)
  survialrates(FUHpatientleveldata,ages=c(1,5,10))
  
  # compute reintervention cumulative incidence rates at age 1-year, 5-years and 10-years,
  #using competing risk analysis, taking  account death/had heart transplant without reintervention as competing events
  # all reintervention
  Reintrates(status=FUHpatientleveldata$Status_allReint,timeofstatus=FUHpatientleveldata$timeofStatus_allReInt)
  # of note, one can remove patients with suspected missing por miscoded data from the reintervention monitoring, using var. Flagcentre_missingdata 
  
  #one can also compute the CIF of surgery reintervention or catheter reintervention, using  Status_HSReint/Status_CEReint and timeofStatus_HSReInt/timeofStatus_CEReInt
}
