###################################################################################################
## CHAMPION WS2: long term outcome by diagnosis. R Modular  programming
## Aug 2024 Qi Huang 
rm(list=ls())

# all CHD diagnoses in order of decreasing complexity. one need to identify patients in this order
allCHDs=c("HLHS","FUH","TGA","PA","AVSD","TOF","COA","AOS","VSD") 

#select CHD type
CHDtype="HLHS"

dir_GenericRfile_champion= "S:/CHAMPION/Qi_folder/WS2 Modular programming in R/generic files"
source("S:/CHAMPION/Qi_folder/WS2 Modular programming in R/generic files/Generic global directory.R") # load directory of all data 
source("S:/CHAMPION/Qi_folder/WS2 Modular programming in R/generic files/Generic modules.R") # load all generic functions 

source(paste0(dir_HLHSRfile_champion,"/", CHDtype," modules.R")) # load all HLHS specific functions 


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
  NCHDAdata= HLHS_inclusion_and_exclusion_module(NCHDAdata)
  
  # derived variable.  
  table(NCHDAdata$HLSHpat[which(NCHDAdata$patentry==1)],useNA="ifany") #HLHS patient marker
  
  ########################### save the dataset
   write.csv(NCHDAdata,dir_NCHDAdataset,row.names = F)
  
   # save the HLHS dataset (raw data)
   HLHSdata_raw=NCHDAdata[which(NCHDAdata$HLHSpat==1),]
   HLHSdata_raw$CHDtype=CHDtype
   write.csv(HLHSdata_raw,dir_data_raw,row.names = F)
}
#################################################
# Identify pathway and diagnostic subgroups
#################################################

if(pathway==1){
  #load the main dataset for HLHS
  HLHSdata_raw=loadmaindata(dir_data_raw)
  ########################### run pathway module 
  HLHSdata_raw=HLHS_pathway_module(HLHSdata_raw)
  
  # derived variable.
  
  # category of each procedure (record level)
  table(HLHSdata_raw$CatProcLabel,useNA="ifany") # category of each procedure. in HLHS, 4 is empty because HLHS is exclusively SV
  # CatProc_lab=c("0: Pre-pathway","1: Palliative first stage procedure","2: SV stage two","3: SV stage three (Fontan)",
  #               "4: Reparative procedure" ,"5: Other pathway (part of hybrid)","6: Heart transplant" ,
  #               "7: reintervention", "8: Excluded")
  
  #Procedure sequence in patient level, for example 123 means patients had stage 1,2 and 3
  table(HLHSdata_raw$ProcSeq[which(HLHSdata_raw$patentry==1)],useNA="ifany")
  
  ########################### diagnosis subtype
  HLHSdata_raw=HLHS_diagnosis_subtype_module(HLHSdata_raw)
  # derived variable.  
  table(HLHSdata_raw$diagsubgroupLab[which(HLHSdata_raw$patentry==1)],useNA="ifany")
  
  
  ########################### save the dataset
  write.csv(HLHSdata_raw,dir_data_raw,row.names = F)
}
#################################################
##Further processing after pathway analysis:
##1.violation rule, remove patients if they meet the violation rule
##2.flag for enquiry to centres, patients will be maintained in the dataset, but create a flag for centres
#################################################

if(FP==1){
  #load the main dataset for HLHS
  HLHSdata_raw=loadmaindata(dir_data_raw)

  ####violation: patients will be removed from the cohort
  
  #generic violation rule: remove patients with non-contributory records only
  HLHSdata_raw=Generic_violation_module(HLHSdata_raw)
  
  #HLHS specific violation rule:
  #HLHS  extra exclusion step c) and d) must be done after pathway analysis. here put it together with  violation rule
  HLHSdata_raw=HLHS_specific_violation_module(HLHSdata_raw)
  
  #important var. HLHSextraexclusion: HLHS  extra exclusion step c) and d) must be done after pathway analysis. here put it together with  violation rule  tab HLHSextraexclusion if patentry==1 //116
  table(HLHSdata_raw$HLHSextraexclusion[which(HLHSdata_raw$patentry==1)],useNA="ifany")
  
  
  #important var. pat_violation: patients who need to removed according to the violation rule
  table(HLHSdata_raw$pat_violation[which(HLHSdata_raw$patentry==1)],useNA="ifany")
  table(HLHSdata_raw$pat_violationinf[which(HLHSdata_raw$patentry==1)])  # the reason for violation
  

  ####flag for enquiry to centres: patients will be maintained in the dataset, but create a flag for centres
  
  # flag patients with suspected missing/miscoded data
  # generic rule: mark patients if had SV stage two/three at a very young age

  HLHSdata_raw=Generic_suspected_missing_miscoded_data_module(HLHSdata_raw)
  

  #HLHS specific suspected rule
  HLHSdata_raw=HLHS_specific_suspected_missing_miscoded_data_module(HLHSdata_raw)
  
  table(HLHSdata_raw$Flagcentre_missingdata[which(HLHSdata_raw$patentry==1)],useNA="ifany")
  table(HLHSdata_raw$Flagcentre_missingdatainf[which(HLHSdata_raw$patentry==1)]) # the reason 
  
  #### minor data errors/unusual records 
  #generic rule:  minor data error if patients had bypass surgery as pre-pathway
  HLHSdata_raw=Generic_minor_data_errors_module(HLHSdata_raw)
  
  #HLHS specific minor data errors rule:  
  HLHSdata_raw=HLHS_specific_minor_data_errors_module(HLHSdata_raw)
  
  table(HLHSdata_raw$Flagcentre_minor[which(HLHSdata_raw$patentry==1)],useNA="ifany") # marker of Flagcentre_minor
  table(HLHSdata_raw$Flagcentre_minorinf[which(HLHSdata_raw$patentry==1)]) # the reason 
  
  
  ########################### save the dataset
  write.csv(HLHSdata_raw,dir_data_raw,row.names = F)
  
  # remove patients who met the violation rule
  
  HLHSdata_final=HLHSdata_raw[-which(HLHSdata_raw$HLHSextraexclusion==1 | HLHSdata_raw$pat_violation==1),]
  write.csv(HLHSdata_final,dir_data_final,row.names = F)
  
}

######################################################################################
########## descriptive analysis, i.e., assign comorbidity and compute number of intervention including pathway, pre-pathway and reintervention 
######################################################################################
  
if(descriptiveanalysis==1){
  #load the main dataset for HLHS
  HLHSdata_final=loadmaindata(dir_data_final)
  
  HLHSdata_final=Generic_descriptive_analysis(HLHSdata_final)

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
  
  write.csv(HLHSdata_final,dir_data_final,row.names = F)
}

######################################################################################
########## save patient level data
######################################################################################

if(PatientLevelData==1){
  HLHSdata_final=loadmaindata(dir_data_final)

  HLHSpatientleveldata=Generic_save_patient_level_data_module(HLHSdata_final)
  write.csv(HLHSpatientleveldata,dir_dataPatientLevel,row.names = F)
  
}

######################################################################################
########## compute metrics. 
######################################################################################
if(metrics==1){
  HLHSpatientleveldata=read.csv(dir_dataPatientLevel,colClasses = c(ProcSeq= "character"))
  
  # compute survival cumulative incidence rates at age 1-year, 5-years and 10-years (using Kaplan-meier)
  survialrates(HLHSpatientleveldata,ages=c(1,5,10))
  
  # compute reintervention cumulative incidence rates at age 1-year, 5-years and 10-years,
  #using competing risk analysis, taking  account death/had heart transplant without reintervention as competing events
  # all reintervention
  
  Reintrates(status=HLHSpatientleveldata$Status_allReint,timeofstatus=HLHSpatientleveldata$timeofStatus_allReInt)
  # of note, one can remove patients with suspected missing or miscoded data from the reintervention monitoring, using var. Flagcentre_missingdata 
  #one can also compute the CIF of surgery reintervention or catheter reintervention, using  Status_HSReint/Status_CEReint and timeofStatus_HSReInt/timeofStatus_CEReInt
}


