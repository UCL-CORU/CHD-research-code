# 06.02 MORTALITY OUTCOMES 

# In-hospital mortality as recorded in NCHDA, enhanced with ONS life status
# 30-day mortality

## system clearing (comment out if you want to keep previous work)
  rm( list=ls()) #remove any previous R object
  gc()           #garbage collection of memory; It can be useful to call gc after a large object has been removed, as this may prompt R to return memory to the operating system.

#PARAMETERS: 
## input data: files processed at record level (files were generated in Step 02)
  ## we only need NCHDA data
  in_file_name_nchda      <- "S://CHAMPION/Analysis/Results/CHD code curation/nchda_clean_data.csv"
  in_file_name_processed_lifestatus <- "S://CHAMPION/Analysis/Results/CHD code curation/nchda_mortality_and_life_status.csv"
  
  out_file_name <- "S://CHAMPION/Analysis/Results/CHD code curation/nchda_mortality_outcomes.csv"
  

# 1. We load the processed nchda and the ons life status data
  ## the input files exist?
  for( in_file_name in c(in_file_name_nchda,in_file_name_processed_lifestatus) ) {  
    file_exists = ifelse( file.exists( in_file_name ), TRUE, FALSE )
    if( !file_exists ) 
      stop( paste0("ERROR: cannot find the file ", in_file_name, "; please revise the source directory and file name" ) )
    rm( file_exists )
  }
  
  nchda_data       <- readr::read_csv( in_file_name_nchda , col_types = "c" )
  life_status_data <- readr::read_csv( in_file_name_processed_lifestatus , col_types = "c" )
  
  gc()

# 2. We only need a few fields from nchda: identifiers, ages, and processed life status
  names(nchda_data) <- tolower(names(nchda_data))
  nchda_data <- nchda_data[ , c("championpatid","launchesrecid","raop","raodis","raod","x4.03.discharge.status")]
  aux <- merge( nchda_data, life_status_data, by=c("championpatid","launchesrecid"), all.x=TRUE )
  nrow( aux ) == nrow( nchda_data ) # no extra rows introduced after merge
  nchda_data <- aux 
  rm( aux )  
  rm( life_status_data )
  gc( )
  
  #2.1. any negative ages set to NA
  nchda_data$raop[ which(nchda_data$raop<0) ] = NA #29
  summary( nchda_data$raop )
  summary( nchda_data$raodis ) #2593 missing
  length( which(nchda_data$raodis<0) ) #42 discharge ages being negative
  nchda_data$raodis[ which(nchda_data$raodis<0) ] = NA
  summary( nchda_data$raodis ) #2635 missing
  
  #2.2. max age in NCHDA by patient
  library( data.table )
  setDT(nchda_data)
  nchda_data[ , `:=` (
    pat_max_aop = max( raop, na.rm=TRUE ),
    pat_max_aodis = max( raodis, na.rm=TRUE )
  ), by= .(championpatid) ]
  
  nchda_data$pat_max_age_nchda = nchda_data$pat_max_aop
  sel = which( nchda_data$x4.03.discharge.status== "A. Alive" )
  length( sel )
  nchda_data$pat_max_age_nchda[ sel ] = pmax( nchda_data$pat_max_aop[ sel ], nchda_data$pat_max_aodis[ sel ], na.rm = TRUE )
  
  
# 3. In-hospital mortality
  summary( nchda_data$ageofdeath ) #note that this definition is at patient level
  
  # 3.1. we use NCHDA first
  nchda_data$death_inhosp <- NA
  nchda_data$death_inhosp[ !is.na(nchda_data$ageofdeath) ] = 1 
  nchda_data$death_inhosp[ which( is.na(nchda_data$death_inhosp) & is.na(nchda_data$ageofdeath) &  nchda_data$x4.03.discharge.status== "A. Alive") ] = 0
  nchda_data$death_inhosp[ which( is.na(nchda_data$death_inhosp) & !is.na(nchda_data$raodis) &  nchda_data$x4.03.discharge.status== "A. Alive") ] = 0
  nchda_data$death_inhosp[ which( is.na(nchda_data$death_inhosp) & !is.na(nchda_data$raodis) &  nchda_data$pat_max_age_nchda>nchda_data$raodis ) ] = 0
  table( nchda_data$death_inhosp, useNA = "always" )
  
  #3.2. we add ONS life status information (death overwrites the NCHDA alive status, but not the other way around)
  # ons dead
  sel = which(nchda_data$ons_life_status == "D" & !is.na( nchda_data$ons_age_life_status ) & !is.na(nchda_data$raodis) )
  sel2 = which(nchda_data$ons_age_life_status[ sel ] <= nchda_data$raodis[ sel ] )
  length( sel2 ) # 5,551
  table( nchda_data$death_inhosp[ sel[ sel2 ] ], useNA="always" ) #5247 (most of those) already well recorded
  nchda_data$death_inhosp[ sel[ sel2 ] ] <- 1
  # ons age at life status after discharge
  sel = which( is.na(nchda_data$death_inhosp) & !is.na( nchda_data$ons_age_life_status ) & !is.na(nchda_data$raodis) )
  sel2 = which(nchda_data$ons_age_life_status[ sel ] > nchda_data$raodis[ sel ] )
  length( sel2 ) # 596
  nchda_data$death_inhosp[ sel[ sel2 ] ] <- 0
  gc()
    
  table( nchda_data$death_inhosp, useNA = "always" ) # in-hospital death unknown for only 425 records
  
  length( unique( nchda_data$championpatid[ is.na(  nchda_data$death_inhosp )]) ) # those correspond to 384 patients 
  
# 4. 30-day mortality
  #we follow the same hierarchy: deaths have preference, then NCHDA and then ONS life status
  summary( nchda_data$ageofdeath ) #note that this definition is at patient level
  
  THIRTY_DAYS = 1.0 / 12.0 # a twelveth of year
  
  # 4.1. we use NCHDA first
  nchda_data$death_30d <- NA
  #nchda age of death before 30 days
  sel = which( nchda_data$ageofdeath <= THIRTY_DAYS + nchda_data$raop )
  length( sel ) #1556
  nchda_data$death_30d[ sel ] = 1
  #nchda discharge age before 30 days and discharge status D (Died in hospital)
  nchda_data$death_30d[ which( is.na(nchda_data$death_30d) & (nchda_data$raodis <= THIRTY_DAYS + nchda_data$raop) & nchda_data$x4.03.discharge.status== "D. Died in hospital" ) ] = 1
  # known to be zero otherwise if some ages after 30 days
  nchda_data$death_30d[ which( is.na(nchda_data$death_30d) & (nchda_data$ageofdeath>THIRTY_DAYS+ nchda_data$raop) ) ] = 0
  nchda_data$death_30d[ which( is.na(nchda_data$death_30d) & (nchda_data$raodis>THIRTY_DAYS+ nchda_data$raop) ) ] = 0
  nchda_data$death_30d[ which( is.na(nchda_data$death_30d) & (nchda_data$pat_max_age_nchda>THIRTY_DAYS+ nchda_data$raop) ) ] = 0
  table( nchda_data$death_30d, useNA = "always" ) #4869
  
  #4.2. we add ONS life status information (death overwrites the NCHDA alive status, but not the other way around)
  # ons dead only used if after procedure start (otherwise it makes no sense)
  sel = which(nchda_data$ons_life_status == "D" & !is.na( nchda_data$ons_age_life_status ) & !is.na(nchda_data$raop)  )
  sel2 = which(nchda_data$ons_age_life_status[ sel ] <= THIRTY_DAYS + nchda_data$raop[ sel ] & 
                 nchda_data$raop[ sel ] <= nchda_data$ons_age_life_status[ sel ] )
  length( sel2 ) # 4038
  table( nchda_data$death_30d[ sel[ sel2 ] ], useNA="always" ) #3810  (most of those) already well recorded
  nchda_data$death_30d[ sel[ sel2 ] ] <- 1
  # ons age at life status after 30 days
  sel = which( is.na(nchda_data$death_30d) & !is.na( nchda_data$ons_age_life_status ) & !is.na(nchda_data$raop) )
  sel2 = which(nchda_data$ons_age_life_status[ sel ] > THIRTY_DAYS + nchda_data$raop[ sel ] )
  length( sel2 ) # 111727 #it is really important to get life status and not only deaths
  nchda_data$death_30d[ sel[ sel2 ] ] <- 0
  gc()
  
  table( nchda_data$death_30d, useNA = "always" ) # 30-day death unknown for 10855 records
  
  length( unique( nchda_data$championpatid[ is.na(  nchda_data$death_30d )]) ) # those correspond to 10177 patients 

# 5. 90-day mortality
  #we follow the same hierarchy: deaths have preference, then NCHDA and then ONS life status
  NINETY_DAYS = 3.0 / 12.0 # a twelveth of year
  
  # 4.1. we use NCHDA first
  nchda_data$death_90d <- NA
  #nchda age of death before 90 days
  sel = which( nchda_data$ageofdeath <= NINETY_DAYS + nchda_data$raop )
  length( sel ) #6939
  nchda_data$death_90d[ sel ] = 1
  #nchda discharge age before 90 days and discharge status D (Died in hospital)
  nchda_data$death_90d[ which( is.na(nchda_data$death_90d) & (nchda_data$raodis <= NINETY_DAYS + nchda_data$raop) & nchda_data$x4.03.discharge.status== "D. Died in hospital" ) ] = 1
  # known to be zero otherwise if some ages after 90 days
  nchda_data$death_90d[ which( is.na(nchda_data$death_90d) & (nchda_data$ageofdeath>NINETY_DAYS+ nchda_data$raop) ) ] = 0
  nchda_data$death_90d[ which( is.na(nchda_data$death_90d) & (nchda_data$raodis>NINETY_DAYS+ nchda_data$raop) ) ] = 0
  nchda_data$death_90d[ which( is.na(nchda_data$death_90d) & (nchda_data$pat_max_age_nchda>NINETY_DAYS+ nchda_data$raop) ) ] = 0
  table( nchda_data$death_90d, useNA = "always" ) #6966
  
  #4.2. we add ONS life status information (death overwrites the NCHDA alive status, but not the other way around)
  # ons dead only used if after procedure start (otherwise it makes no sense)
  sel = which(nchda_data$ons_life_status == "D" & !is.na( nchda_data$ons_age_life_status ) & !is.na(nchda_data$raop)  )
  sel2 = which(nchda_data$ons_age_life_status[ sel ] <= NINETY_DAYS + nchda_data$raop[ sel ] & 
                 nchda_data$raop[ sel ] <= nchda_data$ons_age_life_status[ sel ] )
  length( sel2 ) # 6315
  table( nchda_data$death_90d[ sel[ sel2 ] ], useNA="always" ) #5715  (most of those) already well recorded
  nchda_data$death_90d[ sel[ sel2 ] ] <- 1
  # ons age at life status after 90 days
  sel = which( is.na(nchda_data$death_90d) & !is.na( nchda_data$ons_age_life_status ) & !is.na(nchda_data$raop) )
  sel2 = which(nchda_data$ons_age_life_status[ sel ] > NINETY_DAYS + nchda_data$raop[ sel ] )
  length( sel2 ) # 117430 #it is really important to get life status and not only deaths
  nchda_data$death_90d[ sel[ sel2 ] ] <- 0
  gc()
  
  table( nchda_data$death_90d, useNA = "always" ) # 90-day death unknown for 11519 records
  
  length( unique( nchda_data$championpatid[ is.na(  nchda_data$death_90d )]) ) # those correspond to 10453 patients 
  
    
# 6. Tidying data fields (we remove the all-code fields used to derive life status)
  todelete = names(nchda_data)[ !grepl( "id|death_", names( nchda_data ) )]
  nchda_data[ , (todelete):= NULL ]
  rm( sel, sel2, todelete )
  gc()
  
# 7. Saving the data into file (we adopt the data table approach again)
  #require( data.table )
  #setDT(pat_data)
  fwrite( x = nchda_data, file=out_file_name, row.names = FALSE )
gc()  
