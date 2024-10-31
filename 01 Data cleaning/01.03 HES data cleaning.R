# 02.03 HES DATA CLEANING

## system clearing (comment out if you want to keep previous work)
  rm( list=ls()) #remove any previous R object
  gc()           #garbage collection of memory; It can be useful to call gc after a large object has been removed, as this may prompt R to return memory to the operating system.

#PARAMETERS:
## file_name, original data file
#  in_file_name <- "S://CHAMPION/Data/NCHDA Data 2023/Dataset3_nolinebreak.csv" 
  #in_file_name <- "S://CHAMPION/Analysis/Results/CHD code curation/nchda_simulated_data.csv"
## out_file_name, output data file
  out_file_name_APC <- "S://CHAMPION/Analysis/Results/CHD code curation/hes_apc_clean_data.csv"
  out_file_name_AE  <- "S://CHAMPION/Analysis/Results/CHD code curation/hes_ae_clean_data.csv"
  out_file_name_OP  <- "S://CHAMPION/Analysis/Results/CHD code curation/hes_op_clean_data.csv"
  
# 1. INPATIENT DATA

hes_apc <- c()  
for( i in 1998:2017 ) {
    print( i )
  
    in_file_name = paste0( "S://LAUNCHES/HES data/APC 1998-2017 v2/NIC234297_HES_APC_", i , "99.txt" )
    
    ## the input file exists?
    file_exists = ifelse( file.exists( in_file_name ), TRUE, FALSE )
    if( !file_exists ) 
      stop( paste0("ERROR: cannot find the file ", in_file_name, "; please revise the source directory and file name" ) )
    rm( file_exists )
    
    ## we load the data as text
    hes_apc_i <- read.csv( in_file_name, colClasses="character", sep = "|", encoding = "ISO-8859-2" ) 
    
    ## we create financial year and rename study_id to launchesrecid
    names( hes_apc_i ) <- tolower( names( hes_apc_i ) )
    hes_apc_i[ , "FinYr" ] = i
    names( hes_apc_i )[ which( names(hes_apc_i)=="study_id" ) ] <- "launchesrecid"
		## keep only 27 fields
    tokeep <- c( "encrypted_hesid", "epikey", "match_rank", "launchesrecid", "FinYr", "age_admission", "age_discharge", "epiorder", "spelend", "procode5",
		             "mydob", "disdest", # note that "discharge status is dead if disdest==79 & spelend=="Y"
		             "hrgnhs", "hrglate35", "sushrg", "resgor", "resgor_ons", "rururb_ind",
		             names(hes_apc_i)[ grep( pattern = "imd", names(hes_apc_i) ) ],
		             "provdist", "sitedist", 
            		 "age_epistart", "sex", "ethnos", "ethraw") #2022: added these vars
		hes_apc_i <- hes_apc_i[ , tokeep ]
		#we remove duplicated records
		to_delete <- which( duplicated( hes_apc_i[ , c("launchesrecid", "encrypted_hesid", "epikey", "FinYr", "age_admission", "age_epistart", "age_discharge", "epiorder", "spelend" ) ] ) ) #2022: added age_epistart
		if( length(to_delete) > 0 ) hes_apc_i <- hes_apc_i[ - to_delete, ]
		#we order the rows using record ID
		hes_apc_i <- hes_apc_i[ with(hes_apc_i, order(launchesrecid,encrypted_hesid,epikey )) , ]
		row.names( hes_apc_i ) <- NULL
		
    # we combine all years
		hes_apc <- rbind( hes_apc, hes_apc_i )

		#we order the rows using record ID
		hes_apc <- hes_apc[ with( hes_apc, order(launchesrecid,encrypted_hesid,epikey) ) , ]
		row.names( hes_apc ) <- NULL
		rm( hes_apc_i )
}	

# HRGcode definition (combining hrgnhs and hrglate35 versions)
hes_apc$HRGcode[ hes_apc$FinYr %in% c(2001,2002) ] = hes_apc$hrgnhs[ hes_apc$FinYr %in% c(2001,2002) ]
hes_apc$HRGcode[ hes_apc$FinYr %in% c(2003,2004,2005,2006,2007,2008,2009,2010,2011) ] = 
  hes_apc$hrglate35[ hes_apc$FinYr %in% c(2003,2004,2005,2006,2007,2008,2009,2010,2011) ]
hes_apc$hrgnhs <- NULL
hes_apc$hrglate35 <- NULL

## DEFINING isCARDIAC and isCOMPLICATION, as well as HRGdescription
# we load the lookup of HRG3 codes
hrg3_lookup <- read.csv( "S://CHAMPION/Analysis/R script files/CHD code curation/02 Data cleaning/Code lists and libs/HES_Data_Dictionary_LOOKUPS_FEP_HRG3.txt", colClasses="character", sep = "\t", encoding = "ISO-8859-2" ) 
sum( duplicated( hrg3_lookup$HRGcode ) ) #171 HRG codes duplicated because the description changed over years
# we use the HRG description corresponding to the data year
aux <- merge( x = hes_apc, y = hrg3_lookup, by.x = "HRGcode", by.y = "HRGcode", all.x = TRUE, all.y = FALSE )
for( HRGcode in unique( hrg3_lookup$HRGcode[ which( duplicated( hrg3_lookup$HRGcode ) ) ] ) ){
  for( FinYr in 2001:2011 ){
    sel = (aux$HRGcode == HRGcode) & (aux$FinYr==FinYr)
    if( sum( sel, na.rm = T) > 0 ){
      aux[ sel & aux[ , paste0( "Yr", substr( FinYr, 3,4 ) ) ]=="", "todelete" ] <- 1
    }
  }
}
# we remove the anachronistic hrg descriptions
table( aux$todelete, useNA = "always" )
aux <- aux[ -which( aux$todelete==1), ]
# we drop the indicators for hrg year of description
for( FinYr in c(1998,2000:2011) ){
  aux[ , paste0( "Yr", substr( FinYr, 3,4 ) ) ] <- NULL
}
aux$todelete <- NULL
# tidy the field names
names( aux )[ names(aux) %in% c("HRGcode","HRGdesc","Cardiac..0.No..1.Yes..2.Ambiguous.","ProcComplications..0.No..1.Yes.") ] <-
  c("HRG3code","HRG3desc","isCardiac","isComplication")
setdiff( names(aux) , names(hes_apc))
setdiff( names(hes_apc), names(aux) )#we will just loose HRGcode, but not really beacause it was saved as HRG3code
# update the dataset
hes_apc<- aux 
rm( aux )
rm( hrg3_lookup )
gc()
#hrg4 codes
hes_apc$HRGcode = toupper( hes_apc$sushrg ) # defined on years 2009 onwards
table( hes_apc$FinYr[ !is.na(hes_apc$HRGcode) & hes_apc$HRGcode!="" ] ) #we got 2008 onwards in data
# we load the lookup of HRG4 codes (2009 to 2016)
hrg4_lookup <- read.csv( "S://CHAMPION/Analysis/R script files/CHD code curation/02 Data cleaning/Code lists and libs/HES_Data_Dictionary_LOOKUPS_FEP_HRG4.txt", colClasses="character", sep = "\t", encoding = "ISO-8859-2" ) 
sum( duplicated( hrg4_lookup$HRGcode ) ) #640 HRG codes duplicated because the description changed over years
# we use the HRG description corresponding to the data year
aux <- merge( x = hes_apc, y = hrg4_lookup, by.x = "HRGcode", by.y = "HRGcode", all.x = TRUE, all.y = FALSE )
for( HRGcode in unique( hrg4_lookup$HRGcode[ which( duplicated( hrg4_lookup$HRGcode ) ) ] ) ){
  for( FinYr in 2009:2016 ){
    sel = (aux$HRGcode == HRGcode) & (aux$FinYr==FinYr)
    if( sum( sel, na.rm = T) > 0 ){
      aux[ sel & aux[ , paste0( "Yr", substr( FinYr, 3,4 ) ) ]=="", "todelete" ] <- 1
    }
  }
}
# we remove the anachronistic hrg descriptions
table( aux$todelete, useNA = "always" )
aux <- aux[ -which( aux$todelete==1), ]
# we drop the indicators for hrg year of description
for( FinYr in c(2009:2016) ){
  aux[ , paste0( "Yr", substr( FinYr, 3,4 ) ) ] <- NULL
}
aux$todelete <- NULL
#yet there are some duplicated records
sel = duplicated( aux[ , c("encrypted_hesid","epikey","launchesrecid")] )
sum( sel ) # 6089
table( aux[ sel , "FinYr" ], useNA = "always" )
# 2008 2017 
# 101 5988
aux <- aux[ -which( sel ), ]
names( aux )
# tidy the field names
names( aux )[ names(aux) %in% c("HRGcode","HRGdesc") ] <- c("HRG4code","HRG4desc")
aux[ which( is.na(aux$isCardiac) ), "isCardiac" ] = aux[ which( is.na(aux$isCardiac) ), "Cardiac..0.No..1.Yes..2.Ambiguous." ]
aux[ which( aux$isCardiac==0 ), "isCardiac" ] = aux[ which( aux$isCardiac==0 ), "Cardiac..0.No..1.Yes..2.Ambiguous." ]
aux[ which( is.na(aux$isComplication) ), "isComplication" ] = aux[ which( is.na(aux$isComplication) ), "ProcComplications..0.No..1.Yes." ]
aux[ which( aux$isComplication==0 ), "isComplication" ] = aux[ which( aux$isComplication==0 ), "ProcComplications..0.No..1.Yes." ]
table( aux$Cardiac..0.No..1.Yes..2.Ambiguous., aux$isCardiac, useNA = "always" )
table( aux$ProcComplications..0.No..1.Yes., aux$isComplication, useNA = "always" )
aux[ , c("Cardiac..0.No..1.Yes..2.Ambiguous.","ProcComplications..0.No..1.Yes.") ] <- NULL
setdiff( names(aux) , names(hes_apc))
setdiff( names(hes_apc), names(aux))
# update the dataset
hes_apc<- aux 
rm( aux )
gc()
# we load the lookup of HRG4 codes (2017)
hrg4_lookup <- read.csv( "S://CHAMPION/Analysis/R script files/CHD code curation/02 Data cleaning/Code lists and libs/HES_Data_Dictionary_LOOKUPS_FEP_HRG4_2017.txt", colClasses="character", sep = "\t", encoding = "ISO-8859-2" ) 
# we use the HRG description corresponding to 2017
aux <- merge( x = hes_apc, y = hrg4_lookup, by.x = "HRG4code", by.y = "HRGcode", all.x = TRUE, all.y = FALSE )
#replace the 2017 descriptions
sel = aux$FinYr == 2017
sum( sel ) # 92709
table( aux[ sel , "FinYr" ], useNA = "always" )
aux[ sel, "HRG4desc" ] <- aux[ sel, "HRGdesc" ]
# tidy the field names
aux[ which( is.na(aux$isCardiac) ), "isCardiac" ] = aux[ which( is.na(aux$isCardiac) ), "Cardiac..0.No..1.Yes..2.Ambiguous." ]
aux[ which( aux$isCardiac==0 ), "isCardiac" ] = aux[ which( aux$isCardiac==0 ), "Cardiac..0.No..1.Yes..2.Ambiguous." ]
aux[ which( is.na(aux$isComplication) ), "isComplication" ] = aux[ which( is.na(aux$isComplication) ), "ProcComplications..0.No..1.Yes." ]
aux[ which( aux$isComplication==0 ), "isComplication" ] = aux[ which( aux$isComplication==0 ), "ProcComplications..0.No..1.Yes." ]
table( aux$Cardiac..0.No..1.Yes..2.Ambiguous., aux$isCardiac, useNA = "always" )
table( aux$ProcComplications..0.No..1.Yes., aux$isComplication, useNA = "always" )
aux[ , c("Cardiac..0.No..1.Yes..2.Ambiguous.","ProcComplications..0.No..1.Yes.") ] <- NULL
aux[ , c("HRGdesc","Yr17") ] <- NULL
setdiff( names(aux) , names(hes_apc))
setdiff( names(hes_apc), names(aux))
# update the dataset
hes_apc<- aux 
rm( aux )
rm( hrg4_lookup )
gc()

## we convert to numeric the numeric fields (ages, epiorder)
numeric_fields <- names(hes_apc) [ grepl( "age|epiorder", names(hes_apc), ignore.case = T) ]
hes_apc[ , numeric_fields ] <- sapply( numeric_fields, function(x) as.numeric(hes_apc[,x]) )
gc()

## we export the processed APC data
write.csv( hes_apc, file=out_file_name_APC, row.names = FALSE )
rm( hes_apc )

# 2. A&E DATA
hes_ae <- c()  
for( i in 2007:2017 ) {
  print( i )
  
  in_file_name = paste0( "S://LAUNCHES/HES data/AE 2007-2017 v2/NIC234297_HES_AE_", i , "99.txt" )
  
  ## the input file exists?
  file_exists = ifelse( file.exists( in_file_name ), TRUE, FALSE )
  if( !file_exists ) 
    stop( paste0("ERROR: cannot find the file ", in_file_name, "; please revise the source directory and file name" ) )
  rm( file_exists )
  
  ## we load the data as text
  hes_ae_i <- read.csv( in_file_name, colClasses="character", sep = "|", encoding = "ISO-8859-2" ) 
  
  ## we create financial year and rename study_id to launchesrecid
  names( hes_ae_i ) <- tolower( names( hes_ae_i ) )
  hes_ae_i[ , "FinYr" ] = i
  names( hes_ae_i )[ which( names(hes_ae_i)=="study_id" ) ] <- "launchesrecid"
  ## keep only 27 fields
  tokeep <- c( "encrypted_hesid", "aekey", "match_rank", "launchesrecid", "FinYr", "age_arrival_ae", "procode5",
               "mydob", names(hes_ae_i)[ grep( pattern = "diag2", names(hes_ae_i) ) ],
               "resgor", "resgor_ons", "rururb_ind",
               names(hes_ae_i)[ grep( pattern = "imd", names(hes_ae_i) ) ],
               "provdist", "sitedist", 
               names(hes_ae_i)[ grep( pattern = "diag_", names(hes_ae_i) ) ], 
               "diagscheme", "sex", "ethraw") #2022: added these vars
  
  hes_ae_i <- hes_ae_i[ , tokeep ]
  #we remove duplicated records
  to_delete <- which( duplicated( hes_ae_i[ , c("launchesrecid", "encrypted_hesid", "aekey", "FinYr", "age_arrival_ae" ) ] ) ) #2022: added age_epistart
  if( length(to_delete) > 0 ) hes_ae_i <- hes_ae_i[ - to_delete, ]
  #we order the rows using record ID
  hes_ae_i <- hes_ae_i[ with(hes_ae_i, order(launchesrecid,encrypted_hesid,aekey )) , ]
  row.names( hes_ae_i ) <- NULL
  
  # we combine all years
  hes_ae <- rbind( hes_ae, hes_ae_i )
  
  #we order the rows using record ID
  hes_ae <- hes_ae[ with( hes_ae, order(launchesrecid,encrypted_hesid,aekey) ) , ]
  row.names( hes_ae ) <- NULL
  rm( hes_ae_i )
}	

#AE: DEFINING isCardiac and Diag2desc (After defining those fields, I only keep 3 out of 12 diagnosis fields)
# we load the lookup of diag2 codes
diag2_lookup <- read.csv( "S://CHAMPION/Analysis/R script files/CHD code curation/02 Data cleaning/Code lists and libs/HES_Data_Dictionary_LOOKUPS_FEP_AE_DIAG2.txt", colClasses="character", sep = "\t", encoding = "ISO-8859-2" ) 
names( diag2_lookup )[ names(diag2_lookup) %in% c("Cardiac..0.No..1.Yes..2.Ambiguous.") ] <- c("isCardiac")
# diagnosis fields description and whether cardiac (one flag per field)
for( i in 1:12 ){ #LOOP over the 12 diagnosis fields
  j = sprintf( "%02d", i )
  print( j )
  aux = merge( hes_ae, diag2_lookup, by.x=paste0( "diag2_", j ), by.y= "DIAG2_code", all.x = TRUE )
  names( aux )[ which(names(aux)=="DIAG2_desc" ) ] <- paste0( "diag2desc", j )
  names( aux )[ which(names(aux)=="isCardiac" ) ] <- paste0( "isCardiac", j )
  hes_ae <- aux
  rm( aux )
  gc()
}
# isCardiac at record level
hes_ae$isCardiac = 0
for( i in 1:12 ){ #LOOP over the 12 diagnosis fields
  j = sprintf( "%02d", i )
  hes_ae$isCardiac = pmax( hes_ae$isCardiac, hes_ae[ , paste0( "isCardiac", j ) ], na.rm = T )
}
table( hes_ae$isCardiac, useNA = "always" )
# removing the diagnosis=specific flags, and only keeping the first three diagnoses
hes_ae[ grep( "isCardiac0|isCardiac1", names(hes_ae)) ] <- NULL
for( i in 4:12 ){ #LOOP over the diagnosis fields
  j = sprintf( "%02d", i )
  print( j )
  hes_ae[ , paste0( "diag2desc", j ) ] <- NULL
  hes_ae[ , paste0( "diag2_", j ) ] <- NULL
  hes_ae[ , paste0( "diag_", j ) ] <- NULL
}
rm( diag2_lookup )
gc()

## we convert to numeric the numeric fields (ages)
numeric_fields <- names(hes_ae) [ grepl( "age", names(hes_ae), ignore.case = T) ]
hes_ae[ , numeric_fields ] <- as.numeric( hes_ae[,numeric_fields] ) # DANGER: we do not use "sapply" because there is only one age field
gc()

## we export the processed APC data
write.csv( hes_ae, file=out_file_name_AE, row.names = FALSE )
rm( hes_ae )
gc()

# 3. OUTPATIENT DATA

hes_op <- c()  
for( i in 2003:2017 ) {
  print( i )
  
  in_file_name = paste0( "S://LAUNCHES/HES data/OP 2003-2017 v2/NIC234297_HES_OP_", i , "99.txt" )
  
  ## the input file exists?
  file_exists = ifelse( file.exists( in_file_name ), TRUE, FALSE )
  if( !file_exists ) 
    stop( paste0("ERROR: cannot find the file ", in_file_name, "; please revise the source directory and file name" ) )
  rm( file_exists )
  
  ## we load the data as text
  hes_op_i <- read.csv( in_file_name, colClasses="character", sep = "|", encoding = "ISO-8859-2" ) 
  
  ## we create financial year and rename study_id to launchesrecid
  names( hes_op_i ) <- tolower( names( hes_op_i ) )
  hes_op_i[ , "FinYr" ] = i
  names( hes_op_i )[ which( names(hes_op_i)=="study_id" ) ] <- "launchesrecid"
  ## keep only 27 fields
  tokeep <- c( "encrypted_hesid", "attendkey", "match_rank", "launchesrecid", "FinYr", "age_appointment_op", "atentype", "attended", "procode5",
               "mydob", "hrgnhs", "sushrg", "tretspef", "resgor", "resgor_ons", "rururb_ind",
               names(hes_op_i)[ grep( pattern = "imd", names(hes_op_i) ) ],
               "provdist", "sitedist", 
               "sex", "ethnos", "ethraw",
               names(hes_op_i)[ grep( pattern = "diag_|opertn_", names(hes_op_i) ) ] ) #2022: added these vars
  hes_op_i <- hes_op_i[ , tokeep ]
  #we remove duplicated records
  to_delete <- which( duplicated( hes_op_i[ , c("launchesrecid", "encrypted_hesid", "attendkey", "FinYr", "age_appointment_op" ) ] ) ) #2022: added age_epistart
  if( length(to_delete) > 0 ) hes_op_i <- hes_op_i[ - to_delete, ]
  #we order the rows using record ID
  hes_op_i <- hes_op_i[ with(hes_op_i, order(launchesrecid,encrypted_hesid,attendkey )) , ]
  row.names( hes_op_i ) <- NULL
  
  # we combine all years
  hes_op <- rbind( hes_op, hes_op_i )
  
  #we order the rows using record ID
  hes_op <- hes_op[ with( hes_op, order(launchesrecid,encrypted_hesid,attendkey) ) , ]
  row.names( hes_op ) <- NULL
  rm( hes_op_i )
  gc()
}	

# HRGcode definition (hrgnhs version)
hes_op$HRGcode[ hes_op$FinYr %in% c(2003,2004,2005,2006,2007,2008,2009,2010,2011) ] = 
  hes_op$hrgnhs[ hes_op$FinYr %in% c(2003,2004,2005,2006,2007,2008,2009,2010,2011) ]
hes_op$hrgnhs <- NULL

## DEFINING isCARDIAC and isCOMPLICATION, as well as HRGdescription
# we load the lookup of HRG3 codes
hrg3_lookup <- read.csv( "S://CHAMPION/Analysis/R script files/CHD code curation/02 Data cleaning/Code lists and libs/HES_Data_Dictionary_LOOKUPS_FEP_HRG3.txt", colClasses="character", sep = "\t", encoding = "ISO-8859-2" ) 
sum( duplicated( hrg3_lookup$HRGcode ) ) #171 HRG codes duplicated because the description changed over years
# we use the HRG description corresponding to the data year
aux <- merge( x = hes_op, y = hrg3_lookup, by.x = "HRGcode", by.y = "HRGcode", all.x = TRUE, all.y = FALSE )
for( HRGcode in unique( hrg3_lookup$HRGcode[ which( duplicated( hrg3_lookup$HRGcode ) ) ] ) ){
  for( FinYr in 2003:2011 ){
    sel = (aux$HRGcode == HRGcode) & (aux$FinYr==FinYr)
    if( sum( sel, na.rm = T) > 0 ){
      aux[ sel & aux[ , paste0( "Yr", substr( FinYr, 3,4 ) ) ]=="", "todelete" ] <- 1
    }
  }
}
# we remove the anachronistic hrg descriptions (we need to be careful with a few records here the HRG was used out of the valid years)
table( aux$todelete, useNA = "always" ) # we would keep only 8371499 records < total OP records
 #we assign 0 to the non-deletable ones
aux$todelete[ is.na(aux$todelete) ] = 0
aux <- aux[ with(aux, order(encrypted_hesid,attendkey,launchesrecid, todelete) ), ]
sel = duplicated(aux[ , c("encrypted_hesid","attendkey","launchesrecid")])
aux <- aux[ -which( sel ), ]
# we drop the indicators for hrg year of description
for( FinYr in c(1998,2000:2011) ){
  aux[ , paste0( "Yr", substr( FinYr, 3,4 ) ) ] <- NULL
}
aux$todelete <- NULL
# tidy the field names
names( aux )[ names(aux) %in% c("HRGcode","HRGdesc","Cardiac..0.No..1.Yes..2.Ambiguous.","ProcComplications..0.No..1.Yes.") ] <-
  c("HRG3code","HRG3desc","isCardiac","isComplication")
setdiff( names(aux) , names(hes_op))
setdiff( names(hes_op), names(aux) )#we will just loose HRGcode, but not really because it was saved as HRG3code
# update the dataset
hes_op<- aux 
rm( aux )
rm( hrg3_lookup )
rm( sel )
gc()
#hrg4 codes
hes_op$HRGcode = toupper( hes_op$sushrg ) # defined on years 2009 onwards
table( hes_op$FinYr[ !is.na(hes_op$HRGcode) & hes_op$HRGcode!="" ] ) #we got 2009 onwards in data
# we load the lookup of HRG4 codes (2009 to 2016)
hrg4_lookup <- read.csv( "S://CHAMPION/Analysis/R script files/CHD code curation/02 Data cleaning/Code lists and libs/HES_Data_Dictionary_LOOKUPS_FEP_HRG4.txt", colClasses="character", sep = "\t", encoding = "ISO-8859-2" ) 
sum( duplicated( hrg4_lookup$HRGcode ) ) #640 HRG codes duplicated because the description changed over years
# we use the HRG description corresponding to the data year
aux <- merge( x = hes_op, y = hrg4_lookup, by.x = "HRGcode", by.y = "HRGcode", all.x = TRUE, all.y = FALSE )
for( HRGcode in unique( hrg4_lookup$HRGcode[ which( duplicated( hrg4_lookup$HRGcode ) ) ] ) ){
  for( FinYr in 2009:2016 ){
    sel = (aux$HRGcode == HRGcode) & (aux$FinYr==FinYr)
    if( sum( sel, na.rm = T) > 0 ){
      aux[ sel & aux[ , paste0( "Yr", substr( FinYr, 3,4 ) ) ]=="", "todelete" ] <- 1
    }
  }
}
gc()
# we remove the anachronistic hrg descriptions (we need to be careful with a few records here the HRG was used out of the valid years)
table( aux$todelete, useNA = "always" ) # we would keep 8886852 records > total OP records
#we assign 0 to the non-deletable ones
aux$todelete[ is.na(aux$todelete) ] = 0
aux <- aux[ with(aux, order(encrypted_hesid,attendkey,launchesrecid, todelete) ), ]
sel = duplicated(aux[ , c("encrypted_hesid","attendkey","attended","launchesrecid")])
sum(sel) + nrow(hes_op) == nrow(aux) # we avoid introducing duplicates
aux <- aux[ -which( sel ), ]
gc()
# we drop the indicators for hrg year of description
for( FinYr in c(2009:2016) ){
  aux[ , paste0( "Yr", substr( FinYr, 3,4 ) ) ] <- NULL
}
aux$todelete <- NULL
#there are no duplicated records left
sel = duplicated( aux[ , c("encrypted_hesid","attendkey","launchesrecid")] )
sum( sel ) # 0
names( aux )
# tidy the field names
names( aux )[ names(aux) %in% c("HRGcode","HRGdesc") ] <- c("HRG4code","HRG4desc")
aux[ which( is.na(aux$isCardiac) ), "isCardiac" ] = aux[ which( is.na(aux$isCardiac) ), "Cardiac..0.No..1.Yes..2.Ambiguous." ]
aux[ which( aux$isCardiac==0 ), "isCardiac" ] = aux[ which( aux$isCardiac==0 ), "Cardiac..0.No..1.Yes..2.Ambiguous." ]
aux[ which( is.na(aux$isComplication) ), "isComplication" ] = aux[ which( is.na(aux$isComplication) ), "ProcComplications..0.No..1.Yes." ]
aux[ which( aux$isComplication==0 ), "isComplication" ] = aux[ which( aux$isComplication==0 ), "ProcComplications..0.No..1.Yes." ]
table( aux$Cardiac..0.No..1.Yes..2.Ambiguous., aux$isCardiac, useNA = "always" )
table( aux$ProcComplications..0.No..1.Yes., aux$isComplication, useNA = "always" )
aux[ , c("Cardiac..0.No..1.Yes..2.Ambiguous.","ProcComplications..0.No..1.Yes.") ] <- NULL
setdiff( names(aux) , names(hes_op))
setdiff( names(hes_op), names(aux))
# update the dataset
hes_op<- aux 
rm( aux )
gc()
# we load the lookup of HRG4 codes (2017)
hrg4_lookup <- read.csv( "S://CHAMPION/Analysis/R script files/CHD code curation/02 Data cleaning/Code lists and libs/HES_Data_Dictionary_LOOKUPS_FEP_HRG4_2017.txt", colClasses="character", sep = "\t", encoding = "ISO-8859-2" ) 
# we use the HRG description corresponding to 2017
aux <- merge( x = hes_op, y = hrg4_lookup, by.x = "HRG4code", by.y = "HRGcode", all.x = TRUE, all.y = FALSE )
#replace the 2017 descriptions
sel = aux$FinYr == 2017
sum( sel ) # 770087
table( aux[ sel , "FinYr" ], useNA = "always" )
aux[ sel, "HRG4desc" ] <- aux[ sel, "HRGdesc" ]
# tidy the field names
aux[ which( is.na(aux$isCardiac) ), "isCardiac" ] = aux[ which( is.na(aux$isCardiac) ), "Cardiac..0.No..1.Yes..2.Ambiguous." ]
aux[ which( aux$isCardiac==0 ), "isCardiac" ] = aux[ which( aux$isCardiac==0 ), "Cardiac..0.No..1.Yes..2.Ambiguous." ]
aux[ which( is.na(aux$isComplication) ), "isComplication" ] = aux[ which( is.na(aux$isComplication) ), "ProcComplications..0.No..1.Yes." ]
aux[ which( aux$isComplication==0 ), "isComplication" ] = aux[ which( aux$isComplication==0 ), "ProcComplications..0.No..1.Yes." ]
table( aux$Cardiac..0.No..1.Yes..2.Ambiguous., aux$isCardiac, useNA = "always" )
table( aux$ProcComplications..0.No..1.Yes., aux$isComplication, useNA = "always" )
aux[ , c("Cardiac..0.No..1.Yes..2.Ambiguous.","ProcComplications..0.No..1.Yes.") ] <- NULL
aux[ , c("HRGdesc","Yr17") ] <- NULL
setdiff( names(aux) , names(hes_op))
setdiff( names(hes_op), names(aux))
# update the dataset
hes_op<- aux 
rm( aux )
rm( sel )
rm( hrg4_lookup )
gc()

## Additionally using Treatment speciality (TRETSPEF) to assign isCardiac
tretspef_lookup <- read.csv( "S://CHAMPION/Analysis/R script files/CHD code curation/02 Data cleaning/Code lists and libs/HES_Data_Dictionary_LOOKUPS_FEP_TRETSPEF_2004.txt", colClasses="character", sep = "\t", encoding = "ISO-8859-2" ) 
# we use the HRG description corresponding to 2017
aux <- merge( x = hes_op, y = tretspef_lookup, by.x = "tretspef", by.y = "TRETSPEF_code", all.x = TRUE, all.y = FALSE )
# assign isCardiac using TRETSPEF
table( aux$isCardiac, useNA="always" )
aux[ which( is.na(aux$isCardiac) ), "isCardiac" ] = aux[ which( is.na(aux$isCardiac) ), "Cardiac..0.No..1.Yes..2.Ambiguous." ]
aux[ which( aux$isCardiac==0 | aux$isCardiac=="" ), "isCardiac" ] = aux[ which( aux$isCardiac==0 | aux$isCardiac=="" ), "Cardiac..0.No..1.Yes..2.Ambiguous." ]
table( aux$Cardiac..0.No..1.Yes..2.Ambiguous., aux$isCardiac, useNA = "always" )
aux[ , c("Cardiac..0.No..1.Yes..2.Ambiguous.","X") ] <- NULL
setdiff( names(aux) , names(hes_op))
setdiff( names(hes_op), names(aux))
# update the dataset
hes_op<- aux 
rm( aux )
rm( tretspef_lookup )
gc()

## we convert to numeric the numeric fields (ages) #only one age
numeric_fields <- names(hes_op) [ grepl( "age", names(hes_op), ignore.case = T) ]
hes_op[ , numeric_fields ] <- as.numeric( hes_op[,numeric_fields] ) 
gc()

## we export the processed Outpatient data
write.csv( hes_op, file=out_file_name_OP, row.names = FALSE )
rm( hes_op )
