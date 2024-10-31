# 02.01 NCHDA DATA CLEANING

## system clearing (comment out if you want to keep previous work)
  rm( list=ls()) #remove any previous R object
  gc()           #garbage collection of memory; It can be useful to call gc after a large object has been removed, as this may prompt R to return memory to the operating system.

#PARAMETERS:
## file_name, original data file
  in_file_name <- "S://CHAMPION/Data/NCHDA Data 2023/Dataset3_nolinebreak.csv" 
  #in_file_name <- "S://CHAMPION/Analysis/Results/CHD code curation/nchda_simulated_data.csv"
## out_file_name, output data file
  out_file_name <- "S://CHAMPION/Analysis/Results/CHD code curation/nchda_clean_data.csv"

# 1. We load the actual data

## the input file exists?
  file_exists = ifelse( file.exists( in_file_name ), TRUE, FALSE )
  if( !file_exists ) 
    stop( paste0("ERROR: cannot find the file ", in_file_name, "; please revise the source directory and file name" ) )
  rm( file_exists )
  
## we load the data as text
  nchda <- read.csv( in_file_name, colClasses="character" )

## then we convert to numeric the numeric fields (ages, times, IMD, and few other)
  numeric_fields <- names(nchda) [ grepl( "aop|aod|dob|IMD|Weight|Height|Time", names(nchda), ignore.case = T) ]
  nchda[ , numeric_fields ] <- sapply( numeric_fields, function(x) as.numeric(nchda[,x]) )

# 2. Revising NCHDA ages
# We used revised NCHDA ages: note that this processing was very specific to our projects (we could get revised a version of ages)
# yet there were some negative ages in the source data
  #nchda[ which(nchda$Raop < 0 | nchda$Raodis < 0) , c("Raop","Raodis", "RFLAGprocdis")]

  #revised record age at procedure
  sel = is.na(nchda$Raop_rev) & !is.na(nchda$Raop)
  sum( sel ) # not all ages were revised, only years up to 2016/17
  nchda$Raop_rev[ sel ] = nchda$Raop[ sel ]
  #revised record age at discharge
  sel = is.na(nchda$Raodis_rev) & !is.na(nchda$Raodis)
  nchda$Raodis_rev[ sel ] = nchda$Raodis[ sel ]
  #revised record age at death
  sel = is.na(nchda$Raod_rev) & !is.na(nchda$Raod)
  nchda$Raod_rev[ sel ] = nchda$Raod[ sel ]
  # Small changes for patient level fields (we used those for the project)
  nrow( nchda ) - sum( nchda$Paop_rev == nchda$Paop ) # 3
  nrow( nchda ) - sum( nchda$Paodis_rev == nchda$Paodis, na.rm=T ) - sum( is.na(nchda$Paodis_rev) & is.na(nchda$Paodis) ) # 1
  nrow( nchda ) - sum( nchda$Paod_rev == nchda$Paod, na.rm=T ) - sum( is.na(nchda$Paod_rev) & is.na(nchda$Paod) ) # 2
  nchda[ which( nchda$Paop_rev != nchda$Paop ), grepl( "aop|aodis", names(nchda), ignore.case = T) ]
  nchda[ which( (nchda$Paodis_rev != nchda$Paodis) | (!is.na(nchda$Paodis_rev) & is.na(nchda$Paodis))  ), grepl( "aop|aodis", names(nchda), ignore.case = T) ]
  nchda[ which( (nchda$Paod_rev != nchda$Paod) | (!is.na(nchda$Paod_rev) & is.na(nchda$Paod))  ), grepl( "aod", names(nchda), ignore.case = T) ]
  sel = nchda$Paop_rev != nchda$Paop
  nchda$Paop[ sel ] = nchda$Paop_rev[ sel ] 
  sel = which( (nchda$Paodis_rev != nchda$Paodis) | (!is.na(nchda$Paodis_rev) & is.na(nchda$Paodis)) )
  nchda$Paodis[ sel ] = nchda$Paodis_rev[ sel ] 
  sel = which( (nchda$Paod_rev != nchda$Paod) | (!is.na(nchda$Paod_rev) & is.na(nchda$Paod)) )
  nchda$Paod[ sel ] = nchda$Paod_rev[ sel ] 
  rm( sel )
  
  # we drop the revised variables
  to_drop =  setdiff( names(nchda)[ grepl( "_rev", names(nchda) ) ] , "LAUNCHESpatID_rev" )
  nchda[ , to_drop ] <- NULL 

  # difference between NCHDA patient-level and record-level ages
  nchda[ which( nchda$Paop != nchda$Raop ), grepl( "aop|aodis", names(nchda), ignore.case = T) ] #27 ages at procedure changed at patient level
  nchda[ which( (nchda$Paodis != nchda$Raodis) | (!is.na(nchda$Paodis) & is.na(nchda$Raodis))  ), grepl( "aop|aodis", names(nchda), ignore.case = T) ] # 26 age at discharge changed at patient level
  nchda[ which( (nchda$Paod != nchda$Raod) ), grepl( "aod", names(nchda), ignore.case = T) ] # 41 changes in deaths
  nchda[ which( (nchda$Paod != nchda$Raod) &  abs(nchda$Paod - nchda$Raod) > 1.0/365.25 ), grepl( "aod", names(nchda), ignore.case = T) ] # 32 changes in deaths are higher than 1 day
  nchda[ which( (!is.na(nchda$Paod) & is.na(nchda$Raod))  ), grepl( "aod", names(nchda), ignore.case = T) ] # 1593 allocated record deaths using patient level death information

  # in our projects, we used the patient ages
  nchda$Raop = nchda$Paop
  nchda$Raodis = nchda$Paodis
  nchda$Raod = nchda$Paod
  # for this legacy code, we just keep the record ages
  to_drop =  setdiff( names(nchda)[ grepl( "Pao", names(nchda) ) ] , "LAUNCHESpatID_rev" )
  nchda[ , to_drop ] <- NULL 
  
  

# 2. Deriving dates and IMD 

## DOB (at record level, derived from year and month of birth and assuming 15th as day of birth)
  nchda$Rdob <- as.Date( sprintf( "%d-%02d-15", nchda$dob.year, nchda$dob.month ), format="%Y-%m-%d" )
  summary( nchda$Rdob )

## Date of procedure (at record level)
  nchda$Rdop <- round( nchda$Rdob + nchda$Raop * 365.25 )
  nchda$Rdop.month <- as.numeric( format( nchda$Rdop, "%m" ) )
  nchda$Rdop.year  <- as.numeric( format( nchda$Rdop, "%Y" ) )
  
## Financial Year
  financial_start <- 4 #NCHDA financial year starts in April
  nchda$FinYr <- ifelse( nchda$Rdop.month >= financial_start,
                         nchda$Rdop.year, 
                         nchda$Rdop.year - 1 )
  
## IMD by Financial Year (defined as in HES)
  nchda$IMDdecile <- NA
  nchda$IMDdecile[ which(nchda$FinYr >= 2019) ] = nchda$IMD2019.decile[ which(nchda$FinYr >= 2019) ]
  nchda$IMDdecile[ which(nchda$FinYr <= 2018) ] = nchda$IMD2015.decile[ which(nchda$FinYr <= 2018) ]
  nchda$IMDdecile[ which(nchda$FinYr <= 2014) ] = nchda$IMD2010.decile[ which(nchda$FinYr <= 2014) ]
  nchda$IMDdecile[ which(nchda$FinYr <= 2009) ] = nchda$IMD2007.decile[ which(nchda$FinYr <= 2009) ]
  nchda$IMDdecile[ which(nchda$FinYr <= 2006) ] = nchda$IMD2004.decile[ which(nchda$FinYr <= 2006) ]

## tidy auxiliary variables
  rm( financial_start, numeric_fields )  
  gc( )

    
# 3. Cleaning the data
  
## Sex (NCHDA field X1.07.Gender)
  nchda$Rsex[ grepl( "1", nchda$X1.07.Gender) ] = "1. Male"
  nchda$Rsex[ grepl( "2", nchda$X1.07.Gender) ] = "2. Female"

## Ethnicity (NCHDA field X1.08.Ethnic) # two classification systems found in data, we respect those
  nchda$Rethnicity[ grepl( "1", nchda$X1.08.Ethnic ) & !grepl( "British", nchda$X1.08.Ethnic ) ] = "1. Caucassian"
  nchda$Rethnicity[ grepl( "2", nchda$X1.08.Ethnic ) ] = "2. Black"
  nchda$Rethnicity[ grepl( "3", nchda$X1.08.Ethnic ) ] = "3. Asian"
  nchda$Rethnicity[ grepl( "4", nchda$X1.08.Ethnic ) ] = "4. Oriental"
  nchda$Rethnicity[ grepl( "8", nchda$X1.08.Ethnic ) ] = "8. Other"
  nchda$Rethnicity[ grepl( "British", nchda$X1.08.Ethnic ) ] = "A. White - British"
  nchda$Rethnicity[ grepl( "Irish", nchda$X1.08.Ethnic ) ] = "B. White - Irish"
  nchda$Rethnicity[ grepl( "^C. White", nchda$X1.08.Ethnic ) ] = "C. White - Any other White background"
  nchda$Rethnicity[ grepl( "Pakistani", nchda$X1.08.Ethnic ) ] = "J. Asian - Pakistani"
  nchda$Rethnicity[ grepl( "^N. Black", nchda$X1.08.Ethnic ) ] = "N. Black - African"
  nchda$Rethnicity[ grepl( "^S. Other", nchda$X1.08.Ethnic ) ] = "S. Other - Any other ethnic group"
  nchda$Rethnicity[ grepl( "9|^Z", nchda$X1.08.Ethnic ) | is.na(nchda$X1.08.Ethnic) ] = NA #"Unknown" 
  
## Patient status (NCHDA field X1.09.Patient.Status)
  nchda$Rpatient_status[ grepl("1", nchda$X1.09.Patient.Status) ] = "1. NHS"
  nchda$Rpatient_status[ grepl("2", nchda$X1.09.Patient.Status) ] = "2. Private"
  nchda$Rpatient_status[ grepl("3", nchda$X1.09.Patient.Status) ] = "3. Amenity"
  nchda$Rpatient_status[ grepl("4", nchda$X1.09.Patient.Status) ] = "4. Overseas charity"
  nchda$Rpatient_status[ grepl("9", nchda$X1.09.Patient.Status) | is.na(nchda$X1.09.Patient.Status) ] = NA #"9. Unknown"

## Diagnosis codes (NCHDA fields X2.01.Diagnosis and Diagnosis.2 to Diagnosis.25..to.32)
  diagnosis_fields = names(nchda)[ grep( "Diagnosis", names(nchda) ) ]
  # we concatenate all diagnosis codes
  nchda$alldiags <- do.call( paste, c( nchda[ , diagnosis_fields ], sep = " " ) )
  # we extract codes from alldiags
  pattern = "(\\d{2}\\.\\d{2}\\.\\d{2}|Q\\d{5}|\\d{6})"
  nchda$alldiagcodes = sapply( 1:nrow(nchda),
                               function(x) unlist( gsub( " ", "", unlist( stringr::str_extract_all( nchda$alldiags[x], pattern ) ) ) )
                               )
  # number of diagnosis codes per record
  nchda$numdiags = sapply( nchda$alldiagcodes, length)
  # creating diagnosis code fields
  max_num_diags = max( nchda$numdiags )
  for( i in 1:max_num_diags ){
#    nchda[ , sprintf("diagcode%02d", i) ] = sapply( 1:nrow(nchda), function(x) ifelse(i<=nchda$numdiags[x], gsub( "\\.", "", nchda$alldiagcodes[x][[1]][i]), "" ) )
    nchda[ , sprintf("diagcode%d", i) ] = sapply( 1:nrow(nchda), function(x) ifelse(i<=nchda$numdiags[x], gsub( "\\.", "", nchda$alldiagcodes[x][[1]][i]), "" ) )
  }
  #cbind( nchda[ 230:233 , c(diagnosis_fields, sprintf( "diagcode%d", 1:7 ) ) ] )
  
## Previous procedure codes (NCHDA field X2.02.Previous.Procedure)
  # we extract codes from the NCHDA field
  pattern = "(\\d{2}\\.\\d{2}\\.\\d{2}|Q\\d{5}|\\d{6})"
  nchda$allprevproccodes = sapply( 1:nrow(nchda),
                               function(x) unlist( gsub( " ", "", unlist( stringr::str_extract_all( nchda$X2.02.Previous.Procedure[x], pattern ) ) ) )
  )
  # number of previous procedure codes per record
  nchda$numprevprocs = sapply( nchda$allprevproccodes, length)
  max_num_prevprocs = max( nchda$numprevprocs )
  #creating previous procedure fields
  for( i in 1:max_num_prevprocs ){
    nchda[ , sprintf("prevproccode%d", i) ] = sapply( 1:nrow(nchda), function(x) ifelse(i<=nchda$numprevprocs[x], gsub( "\\.", "", nchda$allprevproccodes[x][[1]][i]), "" ) )
  }

# Antenatal diagnosis (NCHDA field X2.04.AntenatalDx)
  nchda$antenataldiag <- 1.0 * (substr( nchda$X2.04.AntenatalDx, 1, 1) == 1)
  nchda$antenataldiag[ substr( nchda$X2.04.AntenatalDx, 1, 1) == 9 | nchda$X2.04.AntenatalDx==""] <- NA
  #table( nchda$X2.04.AntenatalDx, nchda$antenataldiag, useNA = "always")
  
# Pre-procedure seizures (NCHDA field X2.05.Pre.procedure.seizures)
  nchda$preproc_seizures = 1.0 * (substr( nchda$X2.05.Pre.procedure.seizures, 1, 1) == "1")
  nchda$preproc_seizures[ substr( nchda$X2.05.Pre.procedure.seizures, 1, 1) %in% c("9","U","x") | 
                            nchda$X2.05.Pre.procedure.seizures==""] <- NA
  #table( nchda$X2.05.Pre.procedure.seizures, nchda$preproc_seizures, useNA = "always" )
          
# Comorbidity present (NCHDA field X2.06b.Comorbidity.present)
  nchda$comorbidity_present = 1.0 * (substr( nchda$X2.06b.Comorbidity.present, 1, 1) == "1")
  nchda$comorbidity_present[ substr( nchda$X2.06b.Comorbidity.present, 1, 1) == 9 | nchda$X2.06b.Comorbidity.present==""] <- NA
  #table( nchda$X2.06b.Comorbidity.present, nchda$comorbidity_present, useNA = "always" )

  ## Comorbidity codes (NCHDA field X2.07.Comorbid.Conditions)
  # we extract codes from the NCHDA field
  pattern = "(\\d{2}\\.\\d{2}\\.\\d{2}|Q\\d{5}|\\d{6})"
  nchda$allcomorbcodes = sapply( 1:nrow(nchda),
                                   function(x) unlist( gsub( " ", "", unlist( stringr::str_extract_all( nchda$X2.07.Comorbid.Conditions[x], pattern ) ) ) )
  )
  # number of comorbidity codes per record
  nchda$numcomorbs = sapply( nchda$allcomorbcodes, length)
  max_num_comorbs = max( nchda$numcomorbs )
  #creating comorbidity code fields
  for( i in 1:max_num_comorbs ){
    nchda[ , sprintf("comorbidity%d", i) ] = sapply( 1:nrow(nchda), function(x) ifelse(i<=nchda$numcomorbs[x], gsub( "\\.", "", nchda$allcomorbcodes[x][[1]][i]), "" ) )
  }
  #cbind( nchda[ c(16,233,609,116185) , c("X2.07.Comorbid.Conditions", sprintf("comorbidity%d", 1:5) ) ] )

# Pre-procedural Systemic Ventricular Ejection Fraction (NCHDA field X2.08.PreProcSEVF)
  nchda$preprocsvef[ nchda$X2.08.PreProcSEVF %in% c("1","1. Good", "Good","55","63","65") ] = "1. Good"
  nchda$preprocsvef[ nchda$X2.08.PreProcSEVF %in% c("2","2. Moderate") ] = "2. Moderate"
  nchda$preprocsvef[ nchda$X2.08.PreProcSEVF %in% c("3","3. Poor") ] = "3. Poor"
  #table( nchda$X2.08.PreProcSEVF, nchda$preprocsvef, useNA="always" )   
  
# Pre-procedural Sub-pulmonary Ventricular Ejection Fraction (NCHDA field X2.09.PreProcSPSEVF)
  nchda$preprocspvef[ nchda$X2.09.PreProcSPSEVF %in% c("1","1. Good", "Good","55","63","65") ] = "1. Good"
  nchda$preprocspvef[ nchda$X2.09.PreProcSPSEVF %in% c("2","2. Moderate") ] = "2. Moderate"
  nchda$preprocspvef[ nchda$X2.09.PreProcSPSEVF %in% c("3","3. Poor") ] = "3. Poor"
  #table( nchda$X2.09.PreProcSPSEVF, nchda$preprocspvef, useNA="always" )   

# Procedure urgency (NCHDA field X3.01b)    
  nchda$procedureurgency[ nchda$X3.01b.Procedure.urgency %in% c("1","1. Elective", "Elective","elective") ] = "1. Elective"
  nchda$procedureurgency[ nchda$X3.01b.Procedure.urgency %in% c("2","2. Urgent", "Urgent", "urgent") ] = "2. Urgent"
  nchda$procedureurgency[ nchda$X3.01b.Procedure.urgency %in% c("3","3. Emergency", "Emergency", "emergency") ] = "3. Emergency"
  nchda$procedureurgency[ nchda$X3.01b.Procedure.urgency %in% c("4","4. Salvage", "Salvage", "salvage") ] = "4. Salvage"
  #table( nchda$X3.01b.Procedure.urgency, nchda$procedureurgency, useNA="always" )   

# Procedure type (NCHDA field X3.07.ProcedureType)
  #homogeneise the value description of the NCHDA field
  # 10.hybrid is an error and by looking at the first code it seems it should be 7. hybrid
  #nchda$X3.09[ nchda$X3.07.ProcedureType=="10. hybrid" ]
  nchda$X3.07.ProcedureType[ nchda$X3.07.ProcedureType=="10. hybrid" ] = "7. hybrid"
  # formatting change with the dash
  nchda$X3.07.ProcedureType <- gsub( "–", "-", nchda$X3.07.ProcedureType )
  # more detailed description of caths
  nchda$X3.07.ProcedureType[ nchda$X3.07.ProcedureType=="3. catheter" ] = "3. catheter intervention"
  nchda$X3.07.ProcedureType[ nchda$X3.07.ProcedureType=="5. diagnostic" ] = "5. diagnostic catheter"
  # void, 0 and 9 values to NA
  nchda$X3.07.ProcedureType[ nchda$X3.07.ProcedureType %in% c( "","0","9. unknown") ] <- NA
  
# Sternotomy sequence (NCHDA field X3.08.Sternotomy.Sequence)
  nchda$X3.08.Sternotomy.Sequence[ nchda$X3.08.Sternotomy.Sequence == c( "Fifth sternotomy") ] <- "5. Fifth sternotomy"
  nchda$X3.08.Sternotomy.Sequence[ nchda$X3.08.Sternotomy.Sequence == c( "1st intervention") ] <- "1. First sternotomy"
  nchda$X3.08.Sternotomy.Sequence[ nchda$X3.08.Sternotomy.Sequence == c( "2nd intervention") ] <- "2. Second sternotomy"
  nchda$X3.08.Sternotomy.Sequence[ nchda$X3.08.Sternotomy.Sequence %in% c( "3. Third Operation", "3rd intervention", "3. Third stenotomy") ] <- "3. Third sternotomy"
  nchda$X3.08.Sternotomy.Sequence[ nchda$X3.08.Sternotomy.Sequence == c( "4th intervention") ] <- "4. Fourth sternotomy"
  nchda$X3.08.Sternotomy.Sequence <- gsub( "Sternotomy", "sternotomy", nchda$X3.08.Sternotomy.Sequence ) 
  nchda$X3.08.Sternotomy.Sequence[ nchda$X3.08.Sternotomy.Sequence %in% c( "", "'", "xxxx", "9. Unknown", "9. Phrenic nerve injury requiring plication of diaphragm") ] <- NA

# Procedure codes (NCHDA fields X3.09.Operation.Performed, Procedure.2 to Procedure.7)    
  procedure_fields = names(nchda)[ grep( "Operation|Procedure\\.\\d", names(nchda) ) ]
  # we concatenate all procedure codes
  nchda$allprocs <- do.call( paste, c( nchda[ , procedure_fields ], sep = " " ) )
  # we extract codes from allprocs
  pattern = "(\\d{2}\\.\\d{2}\\.\\d{2}|Q\\d{5}|\\d{6})"
  nchda$allproccodes = sapply( 1:nrow(nchda),
                               function(x) unlist( gsub( " ", "", unlist( stringr::str_extract_all( nchda$allprocs[x], pattern ) ) ) )
  )
  # number of procedures codes per record
  nchda$numprocs = sapply( nchda$allproccodes, length)
  # creating procedure code fields
  max_num_procs = max( nchda$numprocs )
  for( i in 1:max_num_procs ){
    nchda[ , sprintf("proccode%d", i) ] = sapply( 1:nrow(nchda), function(x) ifelse(i<=nchda$numprocs[x], gsub( "\\.", "", nchda$allproccodes[x][[1]][i]), "" ) )
  }
  #cbind( nchda[ 230:233 , c(procedure_fields, sprintf("proccode%d", 1:9) ) ] )

# Discharge status (NCHDA field X4.03.Discharge.Status)
  # alive # I assume that "NSP" means no specific problem, and hence alive
  alive_values = "A. Alive|A.Alive|A;R|R. Reoperated|NSP"
  nchda$X4.03.Discharge.Status[ grep( alive_values, nchda$X4.03.Discharge.Status ) ] <- "A. Alive"
  # died in hospital
  died_values = "123331. Intraoperative death|123333. Death within 30 days of procedure|123334. Death unrelated to cardiac procedure|158264. Postprocedural brain death|159086. Death attributable to complication(s) following congenital cardiac procedure"
  nchda$X4.03.Discharge.Status[ grep( died_values, nchda$X4.03.Discharge.Status ) ] <- "D. Died in hospital"
  nchda$X4.03.Discharge.Status[ grep( "159086", nchda$X4.03.Discharge.Status ) ] <- "D. Died in hospital"
  nchda$X4.03.Discharge.Status <- gsub( "Hospital", "hospital", nchda$X4.03.Discharge.Status )
  # unknown
  nchda$X4.03.Discharge.Status[ grep( "Unknown|0", nchda$X4.03.Discharge.Status ) ] <- NA
  nchda$X4.03.Discharge.Status[ nchda$X4.03.Discharge.Status=="" ] <- NA

# Pre-procedural NYHA status (NCHDA field "X6.01.Pre.proc.NYHA.status" )  
  #1. No limitation of physical activity
  nchda$X6.01.Pre.proc.NYHA.status[ grep("1",nchda$X6.01.Pre.proc.NYHA.status)] <- "1. No limitation of physical activity"
  #2. Slight limitation of ordinary physical activity
  nchda$X6.01.Pre.proc.NYHA.status[ grep("2",nchda$X6.01.Pre.proc.NYHA.status)] <- "2. Slight limitation of ordinary physical activity"
  #3. Marked limitation of ordinary physical activity
  nchda$X6.01.Pre.proc.NYHA.status[ grep("3|Marked limitation",nchda$X6.01.Pre.proc.NYHA.status)] <- "3. Marked limitation of ordinary physical activity"
  #NA
  nchda$X6.01.Pre.proc.NYHA.status[ nchda$X6.01.Pre.proc.NYHA.status == "" ] <- NA
  
# Pre-procedural smoking status (NCHDA field X6.02.Pre.proc.smoking.status)
  #0. Never smoked
  nchda$X6.02.Pre.proc.smoking.status[ grep("0|Never",nchda$X6.02.Pre.proc.smoking.status) ] <- "0. Never smoked"
  #1. Ex smoker
  nchda$X6.02.Pre.proc.smoking.status[ grep("1|Ex",nchda$X6.02.Pre.proc.smoking.status) ] <- "1. Ex smoker"
  #2. Current smoker
  nchda$X6.02.Pre.proc.smoking.status[ grep("2|Current|3",nchda$X6.02.Pre.proc.smoking.status) ] <-"2. Current smoker"
  #NA
  nchda$X6.02.Pre.proc.smoking.status[ nchda$X6.02.Pre.proc.smoking.status %in% c("","9","9. Smoking status unknown") ] <- NA
  
# Pre-procedural diabetes (NCHDA field X6.03.Pre.proc.diabetes)  
  #0. Not diabetic
  nchda$X6.03.Pre.proc.diabetes[ grep("0|Not",nchda$X6.03.Pre.proc.diabetes) ] <- "0. Not diabetic"
  #1. Diet
  nchda$X6.03.Pre.proc.diabetes[ grep("1|Diet",nchda$X6.03.Pre.proc.diabetes) ] <- "1. Diet"
  #2. Oral therapy
  nchda$X6.03.Pre.proc.diabetes[ grep("2|Oral",nchda$X6.03.Pre.proc.diabetes) ] <-"2. Oral therapy"
  #3. Insulin
  nchda$X6.03.Pre.proc.diabetes[ grep("3|Insulin",nchda$X6.03.Pre.proc.diabetes) ] <- "3. Insulin"
  #NA
  nchda$X6.03.Pre.proc.diabetes[ nchda$X6.03.Pre.proc.diabetes %in% c("","9. Diabetes status unknown") ] <- NA  
  
# History of pulmonary disease (NCHDA field X6.04.History.of.pulmonary.disease)  
  #0. No pulmonary disease
  nchda$X6.04.History.of.pulmonary.disease[ grep("0|No",nchda$X6.04.History.of.pulmonary.disease) ] <- "0. No pulmonary disease"
  #1. COAD/emphysema or Asthma
  nchda$X6.04.History.of.pulmonary.disease[ grep("1|COAD|2",nchda$X6.04.History.of.pulmonary.disease) ] <- "1. COAD/emphysema or Asthma"
  #NA
  nchda$X6.04.History.of.pulmonary.disease[ nchda$X6.04.History.of.pulmonary.disease %in% c("","9. Unknown")  ] <- NA 

# Pre-procedural ischemic heart disease (NCHDA field X6.05.Pre.proc.ischaemic.heart.disease)
  #0. No history of ischaemic heart disease
  nchda$X6.05.Pre.proc.ischaemic.heart.disease[ grep( "0|No", nchda$X6.05.Pre.proc.ischaemic.heart.disease)] <- "0. No history of ischaemic heart disease"
  #1. History of Ischaemic heart disease
  nchda$X6.05.Pre.proc.ischaemic.heart.disease[ grep( "1|History", nchda$X6.05.Pre.proc.ischaemic.heart.disease)] <- "1. History of Ischaemic heart disease"
  #NA
  nchda$X6.05.Pre.proc.ischaemic.heart.disease[ nchda$X6.05.Pre.proc.ischaemic.heart.disease %in% c("","9. Unknown")] <- NA
  
  
## tidy auxiliary variables
  rm( diagnosis_fields, i, pattern, procedure_fields ) 
  rm( max_num_comorbs, max_num_diags, max_num_prevprocs, max_num_procs )
  rm( alive_values, died_values )
  nchda$allcomorbcodes <- NULL
  nchda$alldiagcodes <- NULL
  nchda$alldiags <- NULL
  nchda$allprevproccodes <- NULL
  nchda$allprocs <- NULL
  nchda$allproccodes <- NULL
  gc( )
  
  
  
# Data summary
  summary( nchda$Rdob ) # DOB
  table( nchda$FinYr, useNA = "always" ) #financial year
  table( nchda$country, useNA = "always" ) # country
  table( nchda$IMDdecile, useNA = "always" ) # IMD decile
  summary( nchda$Raop ) # Age at procedure (record)
  summary( nchda$Raodis ) # Age at discharge (record)
  summary( nchda$Raod ) # Age of death (record)
  table( nchda$orgid, useNA = "always" ) # Hospital code #project-specific
  table( nchda$Rsex, useNA = "always" ) # Sex (record)
  table( nchda$Rethnicity, useNA = "always" ) # Ethnicity (record)
  table( nchda$Rpatient_status, useNA = "always" ) # Patient status (record)
  table( nchda$numdiags, useNA = "always" ) #Number of diagnosis codes (record)
  table( nchda$numprevprocs, useNA = "always" ) # Number of previous procedure codes
  summary( nchda$X2.03.Weight ) # Weight (record)
  summary( nchda$X2.03b.Height ) # Height (record)
  table( nchda$antenataldiag, useNA = "always" ) # Antenatal diagnosis (record)
  table( nchda$preproc_seizures, useNA = "always" ) # Pre-procedure seizures
  table( nchda$comorbidity_present, useNA = "always" ) # Comorbidity present (record)
  table( nchda$preprocsvef, useNA="always" ) # Pre-procedural Systemic Ventricular Ejection Fraction
  table( nchda$preprocspvef, useNA="always" ) # Pre-procedural Sub-pulmonary Ventricular Ejection Fraction
  table( nchda$procedureurgency, useNA="always" ) # Procedure urgency 
  table( nchda$X3.07.ProcedureType, useNA = "always" ) # Procedure type
  table( nchda$X3.08.Sternotomy.Sequence, useNA = "always" ) # Sternotomy sequence
  table( nchda$numprocs, useNA = "always" ) # Number of procedures
  summary( nchda$X3.10.Bypass.Time ) #Bypass time
  summary( nchda$X3.11.Cross.Clamp.Time ) #Cross-clamp time
  summary( nchda$X3.12.Total.circulatory.arrest.time ) #Total circulatory arrest time
  table( nchda$X4.03.Discharge.Status, useNA = "always" ) # Discharge Status
  table( nchda$X6.01.Pre.proc.NYHA.status, useNA = "always" ) # Pre-procedural NYHA status
  table( nchda$X6.02.Pre.proc.smoking.status, useNA = "always" ) # Pre-procedural smoking status                             
  table( nchda$X6.03.Pre.proc.diabetes, useNA = "always" ) # Pre-procedural diabetes
  table( nchda$X6.04.History.of.pulmonary.disease, useNA = "always" ) # History of pulmonary disease                       
  table(nchda$X6.05.Pre.proc.ischaemic.heart.disease, useNA ="always") # Pre-procedural ischemic heart disease  
  
# 4. Saving the clean data into file
write.csv( nchda, file=out_file_name, row.names = FALSE )
