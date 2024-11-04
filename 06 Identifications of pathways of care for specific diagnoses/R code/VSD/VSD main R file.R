###################################################################################################
## CHAMPION WS2: long term outcome by diagnosis. R Modular  programming
## Aug 2024 Qi Huang 
rm(list=ls())

# all CHD diagnoses in order of decreasing complexity. one need to identify patients in this order
allCHDs=c("HLHS","FUH","TGA","PA","AVSD","TOF","AOS","VSD","VSD") 

#select CHD type
CHDtype="VSD"
dir_GenericRfile_champion= "S:/CHAMPION/Qi_folder/WS2 Modular programming in R/generic files"
source("S:/CHAMPION/Qi_folder/WS2 Modular programming in R/generic files/Generic global directory.R") # load directory of all dataset 
source("S:/CHAMPION/Qi_folder/WS2 Modular programming in R/generic files/Generic modules.R") # load all generic functions 

source(paste0(dir_VSDRfile_champion,"/", CHDtype," modules.R")) # load all VSD specific functions 


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
  
  NCHDAdata= VSD_inclusion_and_exclusion_module(NCHDAdata)
  
  # derived variable.   marker of VSD patients
  table(NCHDAdata$VSDpat[which(NCHDAdata$patentry==1)],useNA="ifany")
  
  
  ########################### save the NCHDA dataset
  write.csv(NCHDAdata,dir_NCHDAdataset,row.names = F)
  #  the VSD dataset (raw data)
  
  # further exclusion:  remove patients with more complex CHD which may not be fully captured by exclusion codes 
  VSDdata_raw=NCHDAdata[which(NCHDAdata$VSDpat==1 & NCHDAdata$TGApat==0  & NCHDAdata$PApat==0 & NCHDAdata$AVSDpat==0 & NCHDAdata$TOFpat==0 & NCHDAdata$AOSpat==0 & NCHDAdata$COApat==0),]
  # HLHS and FUH had extra exclusion, so we need to check their final patient id
  HLHS=read.csv("S:/CHAMPION/Qi_folder/Qi.data.CHAMPION/Excels/New data 2022 - Copy/HLHS/Champion_HLHS_final.csv")
  FUH=read.csv("S:/CHAMPION/Qi_folder/Qi.data.CHAMPION/Excels/New data 2022 - Copy/FUH/Champion_FUH_final.csv")
  
  VSDdata_raw=VSDdata_raw[which(!(VSDdata_raw$patid %in% HLHS$patid | VSDdata_raw$patid %in% FUH$patid)),]
  VSDdata_raw$CHDtype=CHDtype
  
  write.csv(VSDdata_raw,dir_data_raw,row.names = F)
}



#################################################
# Identify pathway and diagnostic subgroups and assign diagnostic subgroup
#################################################
if(pathway==1){
  #load the main dataset for VSD
  VSDdata_raw=loadmaindata(dir_data_raw)
  ########################### run pathway module
  VSDdata_raw=VSD_pathway_module(VSDdata_raw)
  # derived variable.  
  # category of each procedure (record level)
  table(VSDdata_raw$CatProcLabel,useNA="ifany") # category of each procedure in VSD. 2: SV stage two"and 3: SV stage three (Fontan) will be empty
  # CatProc_lab=c("0: Pre-pathway","1: Palliative first stage procedure","2: SV stage two","3: SV stage three (Fontan)",
  #               "4: Reparative procedure" ,"5: other pathway (none in VSD)","6: Heart transplant" ,
  #               "7: Off-pathway", "8: Excluded")
  

  #Procedure sequence in patient level, for example "14" means patient had stage 1 and reparative procedure
  table(VSDdata_raw$ProcSeq[which(VSDdata_raw$patentry==1)],useNA="ifany")
  
  
  ########################### diagnosis subtype  
  
  VSDdata_raw=VSD_diagnosis_subtype_module(VSDdata_raw)
  
  # derived variable.  
  table(VSDdata_raw$diagsubgroupLab[which(VSDdata_raw$patentry==1)],useNA="ifany")
  
  ########################### save the dataset
  write.csv(VSDdata_raw,dir_data_raw,row.names = F)
}

#################################################
##Further processing after pathway analysis:
##1.violation rule, remove patients if they meet the violation rule
##2.flag for enquiry to centres, patients will be maintained in the dataset, but create a flag for centres
#################################################


if(FP==1){
  #load the main dataset for VSD
  VSDdata_raw=loadmaindata(dir_data_raw)
  
  ####violation: patients will be removed from the cohort
  
  #generic violation rule: remove patients with non-contributory records only
  VSDdata_raw=Generic_violation_module(VSDdata_raw)
  
  #VSD specific violation rule: NONE

  #important var. pat_violation: patients who need to removed according to the violation rule
  table(VSDdata_raw$pat_violation[which(VSDdata_raw$patentry==1)],useNA="ifany")
  table(VSDdata_raw$pat_violationinf[which(VSDdata_raw$patentry==1)])  # the reason for violation
  
  
  ####flag for enquiry to centres: patients will be maintained in the dataset, but create a flag for centres
  
  # flag patients with suspected missing/miscoded data
  # generic rule: mark patients if had SV stage two/three at a very young age
  
  VSDdata_raw=Generic_suspected_missing_miscoded_data_module(VSDdata_raw)
  
  #VSD specific suspected rule:  NONE
  
  table(VSDdata_raw$Flagcentre_missingdata[which(VSDdata_raw$patentry==1)],useNA="ifany")
  table(VSDdata_raw$Flagcentre_missingdatainf[which(VSDdata_raw$patentry==1)]) # the reason 
  
  
  #### minor data errors/unusual records
  #generic rule:  minor data error if patients had bypass surgery as pre-pathway
  
  VSDdata_raw=Generic_minor_data_errors_module(VSDdata_raw)
  
  #VSD specific minor data errors rule:  
  
  VSDdata_raw=VSD_specific_minor_data_errors_module(VSDdata_raw)
  
  table(VSDdata_raw$Flagcentre_minor[which(VSDdata_raw$patentry==1)],useNA="ifany") # marker of Flagcentre_minor
  table(VSDdata_raw$Flagcentre_minorinf[which(VSDdata_raw$patentry==1)]) # the reason 
  
  ########################### save the dataset
  write.csv(VSDdata_raw,dir_data_raw,row.names = F)
  
  # remove patients who met the violation rule
  
  VSDdata_final=VSDdata_raw[-which(VSDdata_raw$pat_violation==1),]
  write.csv(VSDdata_final,dir_data_final,row.names = F)
}


######################################################################################
########## descriptive analysis, i.e., assign comorbidity and compute number of intervention including pathway, pre-pathway and reintervention 
######################################################################################


if(descriptiveanalysis==1){
  #load the main dataset for VSD
  VSDdata_final=loadmaindata(dir_data_final)
  
  VSDdata_final=Generic_descriptive_analysis(VSDdata_final)
  

  
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
  
  
  table(VSDdata_final$patpremat[which(VSDdata_final$patentry==1)],useNA="ifany") 

  write.csv(VSDdata_final,dir_data_final,row.names = F)
}


######################################################################################
########## save patient level data
######################################################################################

if(PatientLevelData==1){
  VSDdata_final=loadmaindata(dir_data_final)
  
  VSDpatientleveldata=Generic_save_patient_level_data_module(VSDdata_final)
  write.csv(VSDpatientleveldata,dir_dataPatientLevel,row.names = F)
  
}

######################################################################################
########## compute metrics. 
######################################################################################
if(metrics==1){
  VSDpatientleveldata=read.csv(dir_dataPatientLevel,colClasses = c(ProcSeq= "character"))
  
  # compute survival cumulative incidence rates at age 1-year, 5-years and 10-years (using Kaplan-meier)
  survialrates(VSDpatientleveldata,ages=c(1,5,10))
  
  # compute reintervention cumulative incidence rates at age 1-year, 5-years and 10-years,
  #using competing risk analysis, taking  account death/had heart transplant without reintervention as competing events
  # all reintervention
  Reintrates(status=VSDpatientleveldata$Status_allReint,timeofstatus=VSDpatientleveldata$timeofStatus_allReInt)
  # of note, one can remove patients with suspected missing por miscoded data from the reintervention monitoring, using var. Flagcentre_missingdata 
  
  #one can also compute the CIF of surgery reintervention or catheter reintervention, using  Status_HSReint/Status_CEReint and timeofStatus_HSReInt/timeofStatus_CEReInt
}
