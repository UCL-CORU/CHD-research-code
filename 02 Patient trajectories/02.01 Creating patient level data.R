# 03.01 CREATING PATIENT LEVEL DATA

## system clearing (comment out if you want to keep previous work)
  rm( list=ls()) #remove any previous R object
  gc()           #garbage collection of memory; It can be useful to call gc after a large object has been removed, as this may prompt R to return memory to the operating system.

#PARAMETERS: 
## input data: files processed at record level (files were generated in Step 02)
  in_file_name_NCHDA <- "S://CHAMPION/Analysis/Results/CHD code curation/nchda_clean_data.csv" #NCHDA
  in_file_name_NCHDAaasp <- "S:/CHAMPION/Analysis/R Data files/nchda_processed_April_2000_March_22_2024-10-04.csv" #activ algo & spec proc 
  in_file_name_APC   <- "S://CHAMPION/Analysis/Results/CHD code curation/hes_apc_clean_data.csv" #HES inpatient
  in_file_name_AE  <- "S://CHAMPION/Analysis/Results/CHD code curation/hes_ae_clean_data.csv"
  in_file_name_OP  <- "S://CHAMPION/Analysis/Results/CHD code curation/hes_op_clean_data.csv"
  in_file_name_PICANetmatches <- "S://CHAMPION/Data/PICANet Data/LaunchesSent20191025v2_LaunchesPICANetMatches.txt"
  in_file_name_PICANet <- "S://CHAMPION/Analysis/Results/CHD code curation/picanet_clean_data.csv"
  
  ## out_file_name, output data file
  out_file_name <- "S://CHAMPION/Analysis/Results/CHD code curation/patient_level_linked_data.csv"

  ## the input files exist?
  for( in_file_name in c(in_file_name_NCHDA,in_file_name_NCHDAaasp,in_file_name_APC,in_file_name_AE,in_file_name_OP,in_file_name_PICANet,in_file_name_PICANetmatches) ) {  
    file_exists = ifelse( file.exists( in_file_name ), TRUE, FALSE )
    if( !file_exists ) 
      stop( paste0("ERROR: cannot find the file ", in_file_name, "; please revise the source directory and file name" ) )
    rm( file_exists )
  }  
  
  
# 1. NCHDA lookup of patients and records
  # loading the nchda data
  nchda <- read.csv( in_file_name_NCHDA, colClasses="character" )
  names( nchda ) <- tolower( names(nchda) )
  # lookup of patient and record ids
  nchda_pat_rec_lookup <- nchda[ , c("championpatid","launchesrecid")]
  sum( duplicated( nchda_pat_rec_lookup ) ) # 0 
  sum( duplicated( nchda_pat_rec_lookup$launchesrecid ) ) # 0 
  
# 2. Adding patient ID to all records (note that linkage was done at record level)
  ## patient id added to HES inpatient 
  hes_apc <-  read.csv( in_file_name_APC, colClasses="character" )
  aux <- merge( x = nchda_pat_rec_lookup, y = hes_apc, by = "launchesrecid", all.x = FALSE, all.y = FALSE )
  #some APC records get duplicated (were linked to multiple NCHDA records)
  sel = duplicated( aux[ , c("championpatid","encrypted_hesid","epikey","epiorder")] )
  nrow( aux ) - sum( sel ) #1046119
  aux <- aux[ -which( sel ) , ]
  #patient ID added and launches record ID deleted
  pat_hes_apc <- aux
  pat_hes_apc$launchesrecid <- NULL
  rm( hes_apc )
  gc()
  
  ## patient id added to HES A&E
  hes_ae <-  read.csv( in_file_name_AE, colClasses="character" )
  aux <- merge( x = nchda_pat_rec_lookup, y = hes_ae, by = "launchesrecid", all.x = FALSE, all.y = FALSE )
  #some AE records get duplicated (were linked to multiple NCHDA records)
  sel = duplicated( aux[ , c("championpatid","encrypted_hesid","aekey")] )
  nrow( aux ) - sum( sel ) #415677
  aux <- aux[ -which( sel ) , ]
  #patient ID added and launches record ID deleted
  pat_hes_ae <- aux
  pat_hes_ae$launchesrecid <- NULL
  rm( hes_ae )
  gc()
  
  ## patient id added to HES OP
  hes_op <-  read.csv( in_file_name_OP, colClasses="character" )
  aux <- merge( x = nchda_pat_rec_lookup, y = hes_op, by = "launchesrecid", all.x = FALSE, all.y = FALSE )
  #some OP records get duplicated (were linked to multiple NCHDA records)
  sel = duplicated( aux[ , c("championpatid","encrypted_hesid","attendkey")] )
  nrow( aux ) - sum( sel ) #4647750
  aux <- aux[ -which( sel ) , ]
  #patient ID added and launches record ID deleted
  pat_hes_op <- aux
  pat_hes_op$launchesrecid <- NULL
  rm( hes_op, aux )  
  gc()
  
  ## patient id added to PICANet
  picanet_matches <- read.csv( in_file_name_PICANetmatches, colClasses="character", sep = "\t", encoding = "ISO-8859-2" )
  picanet <-  read.csv( in_file_name_PICANet, colClasses="character" )
  # we link by record ID first the matched IDs
  names( picanet_matches ) <- tolower( names( picanet_matches ) )
  aux <- merge( x = nchda_pat_rec_lookup, y = picanet_matches, by = "launchesrecid", all.x = FALSE, all.y = FALSE )
  #some OP records get duplicated (were linked to multiple NCHDA records)
  sel = duplicated( aux[ , c("championpatid","picanetpatientid","picanetrecordid")] )
  nrow( aux ) - sum( sel ) #65801
  aux <- aux[ -which( sel ) , ]
  rm( sel )
  #patient ID added and launches record ID deleted
  pat_picanet_matches <- aux
  pat_picanet_matches$launchesrecid <- NULL
  rm( picanet_matches, aux )  
  gc()    
  # then we add the PICANet information
  aux <- merge( x = pat_picanet_matches, y = picanet, by = c("picanetpatientid","picanetrecordid"), all.x = TRUE, all.y = FALSE )
  pat_picanet <- aux
  rm( pat_picanet_matches, picanet, aux )  
  gc()

# 2. dropping some of the fields (note that fields can always be recovered from the original data given their record ID)
  #NCHDA IMD: we keep only the derived IMD decile
  todelete <- setdiff( names(nchda)[ grep("imd", names(nchda)) ], "imddecile" )
  nchda[ , todelete ] <- NULL
  #NCHDA diagnoses, procedures, previous procedures, and comorbidities: we keep only the derived code fields
  todelete <- setdiff( names(nchda)[ grep("diagnosis|x2.07|x3.09|x2.02|^procedure.", names(nchda)) ], "procedureurgency" )
  nchda[ , todelete ] <- NULL
  # NCHDA hospital, sex, ethnicity, patient_status, antenatal diagnosis: only the derived fields
  # NCHDA: we delete cath-related variables
  todelete <- names(nchda)[ grep("x1.0|x2.04|cath", names(nchda)) ]
  nchda[ , todelete ] <- NULL
  
  # HES OP: we drop diagnosis and procedure codes
  todelete <- names(pat_hes_op)[ grep("diag_|opertn_", names(pat_hes_op)) ]
  pat_hes_op[ , todelete ] <- NULL
  # comparing HES fields across data sets
  intersect( names(pat_hes_op), names(pat_hes_apc) ) #24 fields
  setdiff( names(pat_hes_op), names(pat_hes_apc) ) # 6 OP-specific fields ok
  setdiff( names(pat_hes_apc), names(pat_hes_op) ) # 7 APC-specific fields ok
  intersect( names(pat_hes_ae), names(pat_hes_apc) ) #17 fields
  setdiff( names(pat_hes_ae), names(pat_hes_apc) ) # 12 AE-specific fields ok? we end up dropping diagnoses
  # HES AE: we drop diagnosis codes
  todelete <- names(pat_hes_ae)[ grep("diag|daig2_", names(pat_hes_ae)) ]
  pat_hes_ae[ , todelete ] <- NULL
  setdiff( names(pat_hes_ae), names(pat_hes_apc) ) # 2 AE-specific fields ok
  
  #PICANet: we drop PIM and days by type of care
  todelete <- names(pat_picanet)[ grep("pim|_days", names(pat_picanet)) ]
  pat_picanet[ , todelete ] <- NULL
  #PICANet: we drop linkage quality details
  todelete <- names(pat_picanet)[ grep("linkageclass|linkagetype|qcode", names(pat_picanet)) ]
  pat_picanet[ , todelete ] <- NULL
  #PICANet: we drop diagnosis, gest, mult, previcuad
  todelete <- names(pat_picanet)[ grep("diagnosis|gest|mult|previcuad", names(pat_picanet)) ]
  pat_picanet[ , todelete ] <- NULL
  rm( todelete )
  gc()
  
# 3. putting together all files
  all_names = unique( tolower(c(names(nchda),names(pat_hes_apc),names(pat_hes_ae),names(pat_hes_op),names(pat_picanet)) ) )
  all_names = c( "dataSOURCE", all_names )
  
  # 01.NCHDA
  pat_data <- nchda
  names( pat_data ) <- tolower( names(pat_data) )
  pat_data$dataSOURCE = "01.NCHDA"  
  pat_data[ , setdiff(all_names,names(pat_data)) ] <- NA
  pat_data <- pat_data[ , all_names ]
  rm( nchda, nchda_pat_rec_lookup )
  gc()

  # 02. PICANet
  aux <- pat_picanet
  names(aux) <- tolower( names(aux) )
  aux$dataSOURCE = "02.PICANet"
  aux[ , setdiff(all_names,names(aux)) ] <- NA
  aux <- aux[ , all_names ]
  pat_data <- rbind( pat_data, aux )  
  rm( aux, pat_picanet )
  gc()
  table( pat_data$dataSOURCE, useNA = "always" )  
  
  # 03. reserved (old ICNARC-CMP)
  
  # 04. HES APC
  aux <- pat_hes_apc
  names(aux) <- tolower( names(aux) )
  aux$dataSOURCE = "04.APC"
  aux[ , setdiff(all_names,names(aux)) ] <- NA
  aux <- aux[ , all_names ]
  pat_data <- rbind( pat_data, aux )  
  rm( aux, pat_hes_apc )
  gc()
  table( pat_data$dataSOURCE, useNA = "always" )  
  
  # 05. HES OP
  aux <- pat_hes_op
  names(aux) <- tolower( names(aux) )
  aux$dataSOURCE = "05.OP"
  aux[ , setdiff(all_names,names(aux)) ] <- NA
  aux <- aux[ , all_names ]
  pat_data <- rbind( pat_data, aux )  
  rm( aux, pat_hes_op )
  gc()
  table( pat_data$dataSOURCE, useNA = "always" )  
  
  # 06. HES AE
  aux <- pat_hes_ae
  names(aux) <- tolower( names(aux) )
  aux$dataSOURCE = "06.AE"
  aux[ , setdiff(all_names,names(aux)) ] <- NA
  aux <- aux[ , all_names ]
  pat_data <- rbind( pat_data, aux )  
  rm( aux, pat_hes_ae )
  gc()
  table( pat_data$dataSOURCE ) #>6M records
  # 01.NCHDA 02.PICANet     04.APC      05.OP      06.AE 
  #   196438      65801    1046119    4647750     415677   
  length( unique( pat_data$championpatid ) ) #123973 patients

# 4. Unifying some fields
  #4.1. Record IDs
  sel = pat_data$dataSOURCE=="01.NCHDA"
  pat_data$recordid[sel] = pat_data$launchesrecid[sel] 
  pat_data$launchesrecid <- NULL
  sel = pat_data$dataSOURCE=="02.PICANet"
  pat_data$recordid[sel] = pat_data$picanetrecordid[sel] 
  pat_data$picanetrecordid <- NULL
  sel = pat_data$dataSOURCE=="04.APC"
  pat_data$recordid[sel] = pat_data$epikey[sel] 
  pat_data$epikey <- NULL
  sel = pat_data$dataSOURCE=="05.OP"
  pat_data$recordid[sel] = pat_data$attendkey[sel] 
  pat_data$attendkey <- NULL
  sel = pat_data$dataSOURCE=="06.AE"
  pat_data$recordid[sel] = pat_data$aekey[sel] 
  pat_data$aekey <- NULL
  rm( sel )
  # Patient identifiers: we only keep championpatid, picanetpatientid, encrypted_hes_id
  pat_data[ , c( "launchespatid", "launchespatid_rev" ) ] <- NULL
  gc()
  
  #4.2 Age at start (record level)
  sel = pat_data$dataSOURCE=="01.NCHDA"
  pat_data$ageatstart[sel] <- pat_data$raop[ sel ]
  pat_data$raop <- NULL
  sel = pat_data$dataSOURCE=="02.PICANet"
  pat_data$ageatstart[sel] = pat_data$ageyradmit[sel] 
  pat_data$ageyradmit <- NULL
  sel = pat_data$dataSOURCE=="04.APC"
  pat_data$ageatstart[sel] = pat_data$age_epistart[sel] 
  pat_data$age_epistart <- NULL
  sel = pat_data$dataSOURCE=="05.OP"
  pat_data$ageatstart[sel] = pat_data$age_appointment_op[sel] 
  pat_data$age_appointment_op <- NULL
  sel = pat_data$dataSOURCE=="06.AE"
  pat_data$ageatstart[sel] = pat_data$age_arrival_ae[sel] 
  pat_data$age_arrival_ae <- NULL
  rm( sel )
  pat_data$ageatstart <- as.numeric( pat_data$ageatstart )
  summary( pat_data$ageatstart, digits = 2 ) #only 113 NAs
  # Ages at procedure: we only keep record level ages
  pat_data[ , c( "paop", "raop_rev", "paop_rev" ) ] <- NULL
  names(pat_data)[ grep( "age", names(pat_data) ) ] # "age_admission"  "age_discharge"  "ageyrdischarge" "ageyrdeath"     "ageatstart"
  gc()

  #4.3 Age at discharge  
  sel = pat_data$dataSOURCE=="01.NCHDA"
  pat_data$ageatdis[sel] <- pat_data$raodis[ sel ]
  pat_data$raodis <- NULL
  sel = pat_data$dataSOURCE=="02.PICANet"
  pat_data$ageatdis[sel] = pat_data$ageyrdischarge[sel] 
  pat_data$ageyrdischarge <- NULL
  sel = pat_data$dataSOURCE=="04.APC"
  pat_data$ageatdis[sel] = pat_data$age_discharge[sel] 
  pat_data$age_discharge <- NULL
  rm( sel )
  pat_data$ageatdis <- as.numeric( pat_data$ageatdis )
  summary( pat_data$ageatdis, digits = 2 ) #NA for OP and AE records
  # age at discharge in APC is only meaningful at spel end
  pat_data$ageatdis[ pat_data$spelend == "N" ] = NA
  # Ages at discharge: we only keep record level ages
  pat_data[ , c( "paodis", "raodis_rev", "paodis_rev" ) ] <- NULL
  names(pat_data)[ grep( "dis", names(pat_data) ) ] 
  gc()

  # we reorder the dataset 
  pat_data <- pat_data[ with( pat_data, order( championpatid, ageatstart, epiorder, ageatdis, recordid ) ), ]
  row.names( pat_data ) <- NULL
  gc()

  #4.4. Patient entry
  pat_data$roworder = as.numeric( row.names( pat_data ) )
  pat_data = transform( pat_data, patentry = ave( roworder, championpatid, FUN = function(x) order(x, decreasing = FALSE ) ) )
  pat_data[ 200:220, c("roworder","patentry")]
  pat_data$roworder <- NULL
      
  #4.5 DOB month and year; record dob
  sel = which( pat_data$dataSOURCE %in% c("04.APC","05.OP","06.AE" ) )
  pat_data$dob.month[sel] = substr(pat_data$mydob[sel],1,2)
  pat_data$dob.year[sel]  = substr(pat_data$mydob[sel],3,6)
  table( pat_data$dob.year, useNA = "always" ) #missing in PICANet
  pat_data$mydob <- NULL
  pat_data$rdob <- as.Date( sprintf( "%d-%02d-15", as.integer(pat_data$dob.year), as.integer(pat_data$dob.month) ), format="%Y-%m-%d" )
  summary( pat_data$rdob )
  
  #4.6 patient dob (we take the mode of possible dobs)
  #function to calculate the mode
  modefun <- function(x) {
    x <- na.omit( x )
    unique_vals <- unique( x )
    unique_vals[ which.max( tabulate( match(x,unique_vals)) ) ]
  }
  #we apply it to each patient
  modes <- aggregate( pat_data$rdob, by = list(pat_data$championpatid), FUN=modefun)
  colnames( modes ) <- c( "championpatid", "dob")
  #we assign it to the dataset
  aux <- merge( pat_data, modes, by="championpatid")
  pat_data <- aux
  rm( aux )
  gc()
  
  #4.7. Date of start (at record level)
  pat_data$rdop <- as.Date( pat_data$rdop, format= "%Y-%m-%d" )
  sel = which( pat_data$dataSOURCE %in% c("04.APC","05.OP","06.AE" ) )
  pat_data$rdop[sel] <- round( pat_data$rdob[sel] + pat_data$ageatstart[sel] * 365.25 )
  sel = which( pat_data$dataSOURCE %in% c("02.PICANet" ) )
  pat_data$rdop[sel] <- round( pat_data$dob[sel] + pat_data$ageatstart[sel] * 365.25 )
  summary( pat_data$rdop )
  pat_data$rdop.month <- as.numeric( format( pat_data$rdop, "%m" ) )
  pat_data$rdop.year  <- as.numeric( format( pat_data$rdop, "%Y" ) )
  gc()
  
  #4.8. financial year for PICANEt
  sel = which( pat_data$dataSOURCE == "02.PICANet" )
  pat_data$finyr[ sel ] <- ifelse( pat_data$rdop.month[ sel ] >= 4,
                                   pat_data$rdop.year[ sel ], 
                                   pat_data$rdop.year[ sel ] - 1 )
  table( pat_data$finyr[sel] )  
  
  
  #4.9 Sex
  table( pat_data$rsex, pat_data$sex, useNA = "always")
  # we give preference to the NCHDA field
    #we apply the mode of rsex (NCHDA sex) to each patient
  modes <- aggregate( pat_data$rsex, by = list(pat_data$championpatid), FUN=modefun)
  colnames( modes ) <- c( "championpatid", "psex")
    #we assign it to the dataset
  aux <- merge( pat_data, modes, by="championpatid")
  pat_data <- aux
  table( pat_data$psex, pat_data$rsex, useNA = "always") #only misisng for 3k records
  # we calculate the equivalent using HES data
    #we apply the mode of sex (NCHDA sex) to each patient
  modes <- aggregate( pat_data$sex, by = list(pat_data$championpatid), FUN=modefun)
  colnames( modes ) <- c( "championpatid", "psexHES")
    #we assign it to the dataset
  aux <- merge( pat_data, modes, by="championpatid")
  pat_data <- aux
  table( pat_data$psexHES, pat_data$sex, useNA = "always") #missing for 5k records
  table( pat_data$psex[ pat_data$patentry==1 ], 
         pat_data$psexHES[ pat_data$patentry==1 ], useNA = "always") #few patients with missing psex
  # we can populate some of those values
  sel = is.na(pat_data$psex)
  pat_data$psex[ sel & pat_data$psexHES==1 ] = "1. Male"
  pat_data$psex[ sel & pat_data$psexHES==2 ] = "2. Female"
  pat_data$psexHES <- NULL
  table( pat_data$psex[ pat_data$patentry==1 ], useNA = "always") #few patients with missing psex
  
  #4.10 Ethnicity (White/Caucassian, Black, Asian, Mixed, Other)
  table( pat_data$rethnicity, pat_data$ethnos, useNA = "always")
  #first, we need to tidy the NCHDA record ethnicity (two versions over time)
  table( pat_data$rethnicity )
  pat_data$reth_unified <- pat_data$rethnicity
  pat_data[ grep( "White", pat_data$rethnicity ), "reth_unified" ] = "1. Caucassian"
  pat_data[ grep( "Black", pat_data$rethnicity ), "reth_unified" ] = "2. Black"
  pat_data[ grep( "Asian", pat_data$rethnicity ), "reth_unified" ] = "3. Asian"
  pat_data[ grep( "Oriental|Other", pat_data$rethnicity ), "reth_unified" ] = "8. Other"
  table( pat_data$reth_unified )
  # we give preference to the NCHDA field
  #we apply the mode of rethnicity (NCHDA ethnicity) to each patient
  modes <- aggregate( pat_data$reth_unified, by = list(pat_data$championpatid), FUN=modefun)
  colnames( modes ) <- c( "championpatid", "ethnicity")
  #we assign it to the dataset
  aux <- merge( pat_data, modes, by="championpatid")
  pat_data <- aux
  table( pat_data$ethnicity, pat_data$reth_unified, useNA = "always") # missing for 69k records
  gc()
  # we calculate the equivalent using HES data
  table( pat_data$ethnos, useNA = "always")
  #first, we need to tidy the HES record ethnicity (two versions over time)
   #first we use it as specified by the patients
  pat_data[ grep( "0|A|B|C", pat_data$ethnos ),  "HESeth" ] = "1. Caucassian"  
  pat_data[ grep( "1|2|3|M|N|P" , pat_data$ethnos ),  "HESeth" ] = "2. Black"
  pat_data[ grep( "4|5|6|7|H|J|K|L" , pat_data$ethnos ),  "HESeth" ] = "3. Asian"
  pat_data[ grep( "D|E|F|G|R|S!8" , pat_data$ethnos ),  "HESeth" ] = "8. Other" #includes Chinese (Chinese is R and appears as Other in HES)
  table( pat_data$HESeth )
   # then we overwrite it as specified by the centres
  pat_data[ grep( "0|A|B|C", pat_data$ethraw ),  "HESeth" ] = "1. Caucassian"  
  pat_data[ grep( "1|2|3|M|N|P" , pat_data$ethraw ),  "HESeth" ] = "2. Black"
  pat_data[ grep( "4|5|6|7|H|J|K|L" , pat_data$ethraw ),  "HESeth" ] = "3. Asian"
  pat_data[ grep( "D|E|F|G|R|S!8" , pat_data$ethraw ),  "HESeth" ] = "8. Other" #includes Chinese (Chinese is R and appears as Other in HES)
  table( pat_data$HESeth )
  #we apply the mode of HES ethnicity to each patient
  modes <- aggregate( pat_data$HESeth, by = list(pat_data$championpatid), FUN=modefun)
  colnames( modes ) <- c( "championpatid", "ethnicityHES")
  #we assign it to the dataset
  aux <- merge( pat_data, modes, by="championpatid")
  pat_data <- aux
  table( pat_data$ethnicityHES, pat_data$HESeth, useNA = "always") #missing for 10k records
  table( pat_data$ethnicity[ pat_data$patentry==1 ], 
         pat_data$ethnicityHES[ pat_data$patentry==1 ], useNA = "always") #many patients with missing ethnicity
  # we can populate some of those values
  sel = is.na(pat_data$ethnicity)
  pat_data$ethnicity[ sel ] = pat_data$ethnicityHES[sel]
  pat_data$HESeth <- NULL
  pat_data$ethnicityHES <- NULL
  pat_data$HESeth <- NULL
  rm( sel )
  rm( aux )
  rm( modes )
  table( pat_data$ethnicity[ pat_data$patentry==1 ], useNA = "always") #nearly 12k patients with missing psex
  gc()
    
# 5. Saving the data into file
  write.csv( pat_data, file=out_file_name, row.names = FALSE )
  