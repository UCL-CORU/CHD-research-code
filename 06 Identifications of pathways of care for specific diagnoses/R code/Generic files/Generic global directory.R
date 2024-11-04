require("survival")
require( "cmprsk")
require("dplyr")
require("rlang")
require("openxlsx")
require("readxl")
require("stringr")
require("reshape2")
library("tidyr")
library("lubridate")

# load all generic/global variables 

# directory
dir_Excel_champion="S:/CHAMPION/Qi_folder/Qi.data.CHAMPION/Excels/New data 2022 - Copy"

# NCHDA dataset
dir_NCHDAdataset=paste0(dir_Excel_champion,"/NCHDA_after2000_rawdata.csv")

# procedure map
dir_Procmap=paste0(dir_Excel_champion,"/procedures_valid_nchda.csv")

# SV staged procedure list (well established, and we use it as generic code list)
dir_stage1=paste0(dir_Excel_champion,"/CHAMPION_Generic_stage1.csv")
dir_SVstage2=paste0(dir_Excel_champion,"/CHAMPION_Generic_SVstage2.csv")
dir_SVstage3=paste0(dir_Excel_champion,"/CHAMPION_Generic_SVstage3.csv")

#severity of illness map
dir_SOImap=paste0(dir_Excel_champion,"/SOImap.csv")

#pre-term birth  map
dir_Premmap=paste0(dir_Excel_champion,"/Premmap.csv")

#congenital comorbidity map
dir_CongComorbmap=paste0(dir_Excel_champion,"/CongComorbmap.csv")

#additional cardiac risk  map
dir_ACRmap=paste0(dir_Excel_champion,"/ACRmap.csv")

#acquired comorbidity map
dir_AcqComorbmap=paste0(dir_Excel_champion,"/AcqComorbmap.csv")


 # all CHD diagnoses in order of decreasing complexity. one need to identify patients in this order
 allCHDs=c("HLHS","FUH","TGA","PA","AVSD","TOF","COA","AOS","VSD") 
 
 if (CHDtype=="HLHS"){
   
   dir_HLHSRfile_champion= "S:/CHAMPION/Qi_folder/WS2 Modular programming in R/HLHS"
   
   dir_HLHSExcel_champion="S:/CHAMPION/Qi_folder/Qi.data.CHAMPION/Excels/New data 2022 - Copy/HLHS"
   
   
   ####### HLHS dataset
   # inclusion and exclusion code list 
   
   dir_diagcodes1<-paste0(dir_HLHSExcel_champion,"/HLHS_diagcodes1.csv")
   dir_diagcodes2<-paste0(dir_HLHSExcel_champion,"/HLHS_diagcodes2.csv")
   dir_proccodes<-paste0(dir_HLHSExcel_champion,"/HLHS_proccodes.csv")
   dir_exclusioncodes<-paste0(dir_HLHSExcel_champion,"/HLHS_exclusioncodes.csv")
   
   
   # procedure level dataset
   dir_data_raw=paste0(dir_HLHSExcel_champion,"/Champion_HLHS_raw.csv")
   dir_data_final=paste0(dir_HLHSExcel_champion,"/Champion_HLHS_final.csv")
   
   # patient level dataset
   dir_dataPatientLevel=paste0(dir_HLHSExcel_champion,"/Champion_HLHS_finalPatientLevel.csv")
 }
 
 if (CHDtype=="FUH"){
   
   dir_FUHRfile_champion= "S:/CHAMPION/Qi_folder/WS2 Modular programming in R/FUH"
   
   dir_FUHExcel_champion="S:/CHAMPION/Qi_folder/Qi.data.CHAMPION/Excels/New data 2022 - Copy/FUH"
   
   
   ####### FUH dataset
   # inclusion and exclusion code list 
   
   dir_diagcodes1<-paste0(dir_FUHExcel_champion,"/FUH_diagcodes1.csv")
   dir_diagcodes2<-paste0(dir_FUHExcel_champion,"/FUH_diagcodes2.csv")
   dir_proccodes<-paste0(dir_FUHExcel_champion,"/FUH_proccodes.csv")
   dir_exclusioncodes<-paste0(dir_FUHExcel_champion,"/FUH_exclusioncodes.csv")
   
   dir_HLHSmalformation<-paste0(dir_FUHExcel_champion,"/HLHSmalformation.csv")
   dir_FUHextraSVproc<-paste0(dir_FUHExcel_champion,"/FUH_extraSVproc.csv")
   
   
   # procedure level dataset
   dir_data_raw=paste0(dir_FUHExcel_champion,"/Champion_FUH_raw.csv")
   dir_data_final=paste0(dir_FUHExcel_champion,"/Champion_FUH_final.csv")
   
   # patient level dataset
   dir_dataPatientLevel=paste0(dir_FUHExcel_champion,"/Champion_FUH_finalPatientLevel.csv")

 }
 
 
 if (CHDtype=="TGA"){
   
   dir_TGARfile_champion= "S:/CHAMPION/Qi_folder/WS2 Modular programming in R/TGA"
   
   dir_TGAExcel_champion="S:/CHAMPION/Qi_folder/Qi.data.CHAMPION/Excels/New data 2022 - Copy/TGA"
   
   
   ####### TGA dataset
   # inclusion and exclusion code list 
   
   dir_diagcodes1<-paste0(dir_TGAExcel_champion,"/TGA_diagcodes1.csv")
   dir_proccodes<-paste0(dir_TGAExcel_champion,"/TGA_proccodes.csv")
   dir_exclusioncodes<-paste0(dir_TGAExcel_champion,"/TGA_exclusioncodes.csv")
   
   dir_TGA_PScodes<-paste0(dir_TGAExcel_champion,"/TGA_PScodes.csv")
   dir_TGA_Complexcodes<-paste0(dir_TGAExcel_champion,"/TGA_Complexcodes.csv")
   

   # procedure level dataset
   dir_data_raw=paste0(dir_TGAExcel_champion,"/Champion_TGA_raw.csv")
   dir_data_final=paste0(dir_TGAExcel_champion,"/Champion_TGA_final.csv")
   
   # patient level dataset
   dir_dataPatientLevel=paste0(dir_TGAExcel_champion,"/Champion_TGA_finalPatientLevel.csv")

 }
 
 if (CHDtype=="AVSD"){
   
   dir_AVSDRfile_champion= "S:/CHAMPION/Qi_folder/WS2 Modular programming in R/AVSD"
   
   dir_AVSDExcel_champion="S:/CHAMPION/Qi_folder/Qi.data.CHAMPION/Excels/New data 2022 - Copy/AVSD"
   
   
   ####### AVSD dataset
   # inclusion and exclusion code list 
   
   dir_diagcodes1<-paste0(dir_AVSDExcel_champion,"/AVSD_diagcodes1.csv")
   dir_diagcodes2<-paste0(dir_AVSDExcel_champion,"/AVSD_diagcodes2.csv")
   
   dir_proccodes<-paste0(dir_AVSDExcel_champion,"/AVSD_proccodes.csv")
   dir_exclusioncodes<-paste0(dir_AVSDExcel_champion,"/AVSD_exclusioncodes.csv")

   
   # procedure level dataset
   dir_data_raw=paste0(dir_AVSDExcel_champion,"/Champion_AVSD_raw.csv")
   dir_data_final=paste0(dir_AVSDExcel_champion,"/Champion_AVSD_final.csv")
   
   # patient level dataset
   dir_dataPatientLevel=paste0(dir_AVSDExcel_champion,"/Champion_AVSD_finalPatientLevel.csv")
   
 }
 
 if (CHDtype=="PA"){
   
   dir_PARfile_champion= "S:/CHAMPION/Qi_folder/WS2 Modular programming in R/PA"
   
   dir_PAExcel_champion="S:/CHAMPION/Qi_folder/Qi.data.CHAMPION/Excels/New data 2022 - Copy/PA"
   
   
   ####### PA dataset
   # inclusion and exclusion code list 
   
   dir_diagcodes1<-paste0(dir_PAExcel_champion,"/PA_diagcodes1.csv")
   dir_diagcodes2<-paste0(dir_PAExcel_champion,"/PA_diagcodes2.csv")
   
   dir_proccodes<-paste0(dir_PAExcel_champion,"/PA_proccodes.csv")
   dir_exclusioncodes<-paste0(dir_PAExcel_champion,"/PA_exclusioncodes.csv")
   dir_PAVSD<-paste0(dir_PAExcel_champion,"/PA_VSDcodes.csv")
   
   
   # procedure level dataset
   dir_data_raw=paste0(dir_PAExcel_champion,"/Champion_PA_raw.csv")
   dir_data_final=paste0(dir_PAExcel_champion,"/Champion_PA_final.csv")
   
   # patient level dataset
   dir_dataPatientLevel=paste0(dir_PAExcel_champion,"/Champion_PA_finalPatientLevel.csv")
   
 }
 
 
 
 if (CHDtype=="TOF"){
   
   dir_TOFRfile_champion= "S:/CHAMPION/Qi_folder/WS2 Modular programming in R/TOF"
   
   dir_TOFExcel_champion="S:/CHAMPION/Qi_folder/Qi.data.CHAMPION/Excels/New data 2022 - Copy/TOF"
   
   
   ####### TOF dataset
   # inclusion and exclusion code list 
   
   dir_diagcodes1<-paste0(dir_TOFExcel_champion,"/TOF_diagcodes1.csv")
   
   dir_proccodes<-paste0(dir_TOFExcel_champion,"/TOF_proccodes.csv")
   dir_exclusioncodes<-paste0(dir_TOFExcel_champion,"/TOF_exclusioncodes.csv")
   
   # procedure level dataset
   dir_data_raw=paste0(dir_TOFExcel_champion,"/Champion_TOF_raw.csv")
   dir_data_final=paste0(dir_TOFExcel_champion,"/Champion_TOF_final.csv")
   
   # patient level dataset
   dir_dataPatientLevel=paste0(dir_TOFExcel_champion,"/Champion_TOF_finalPatientLevel.csv")
   
 }
 
 
 if (CHDtype=="AOS"){
   
   dir_AOSRfile_champion= "S:/CHAMPION/Qi_folder/WS2 Modular programming in R/AOS"
   
   dir_AOSExcel_champion="S:/CHAMPION/Qi_folder/Qi.data.CHAMPION/Excels/New data 2022 - Copy/AOS"
   
   
   ####### AOS dataset
   # inclusion and exclusion code list 
   
   dir_diagcodes1<-paste0(dir_AOSExcel_champion,"/AOS_diagcodes1.csv")
   
   dir_exclusioncodes<-paste0(dir_AOSExcel_champion,"/AOS_exclusioncodes.csv")
   
   dir_AOS_SpecProcExccodes<-paste0(dir_AOSExcel_champion,"/AOS_SpecProcExc.csv")
   dir_AOS_MLLHOevidencecodes<-paste0(dir_AOSExcel_champion,"/AOS_MLLHOevidencecodes.csv")
   
   # procedure level dataset
   dir_data_raw=paste0(dir_AOSExcel_champion,"/Champion_AOS_raw.csv")
   dir_data_final=paste0(dir_AOSExcel_champion,"/Champion_AOS_final.csv")
   
   # patient level dataset
   dir_dataPatientLevel=paste0(dir_AOSExcel_champion,"/Champion_AOS_finalPatientLevel.csv")
   
 }
 
 
 
 if (CHDtype=="COA"){
   
   dir_COARfile_champion= "S:/CHAMPION/Qi_folder/WS2 Modular programming in R/COA"
   
   dir_COAExcel_champion="S:/CHAMPION/Qi_folder/Qi.data.CHAMPION/Excels/New data 2022 - Copy/COA"
   
   
   ####### COA dataset
   # inclusion and exclusion code list 
   
   dir_diagcodes1<-paste0(dir_COAExcel_champion,"/COA_diagcodes1.csv")
   
   dir_exclusioncodes<-paste0(dir_COAExcel_champion,"/COA_exclusioncodes.csv")
   
   dir_COA_SpecProcExccodes<-paste0(dir_COAExcel_champion,"/COA_SpecProcExc.csv")
   dir_COA_VSDevidencecodes<-paste0(dir_COAExcel_champion,"/COA_VSDevidencecodes.csv")
   
   # procedure level dataset
   dir_data_raw=paste0(dir_COAExcel_champion,"/Champion_COA_raw.csv")
   dir_data_final=paste0(dir_COAExcel_champion,"/Champion_COA_final.csv")
   
   # patient level dataset
   dir_dataPatientLevel=paste0(dir_COAExcel_champion,"/Champion_COA_finalPatientLevel.csv")
   
 }
 
 
 if (CHDtype=="VSD"){
   
   dir_VSDRfile_champion= "S:/CHAMPION/Qi_folder/WS2 Modular programming in R/VSD"
   
   dir_VSDExcel_champion="S:/CHAMPION/Qi_folder/Qi.data.CHAMPION/Excels/New data 2022 - Copy/VSD"
   
   
   ####### VSD dataset
   # inclusion and exclusion code list 
   
   dir_diagcodes1<-paste0(dir_VSDExcel_champion,"/VSD_diagcodes1.csv")
   
   dir_exclusioncodes<-paste0(dir_VSDExcel_champion,"/VSD_exclusioncodes.csv")
   
   dir_VSD_SpecProcExccodes<-paste0(dir_VSDExcel_champion,"/VSD_SpecProcExc.csv")
   
   # procedure level dataset
   dir_data_raw=paste0(dir_VSDExcel_champion,"/Champion_VSD_raw.csv")
   dir_data_final=paste0(dir_VSDExcel_champion,"/Champion_VSD_final.csv")
   
   # patient level dataset
   dir_dataPatientLevel=paste0(dir_VSDExcel_champion,"/Champion_VSD_finalPatientLevel.csv")
   
 }