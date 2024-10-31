# 06.01 PROCESSING MORTALITY AND LIFE STATUS

# In-hospital mortality is recorded in NCHDA (we will enhance it with ONS life status in another file)
# age of death / life status was available via linkage to ONS registry

## system clearing (comment out if you want to keep previous work)
  rm( list=ls()) #remove any previous R object
  gc()           #garbage collection of memory; It can be useful to call gc after a large object has been removed, as this may prompt R to return memory to the operating system.

#PARAMETERS: 
## input data: files processed at record level (files were generated in Step 02)
  ## we only need NCHDA data
  in_file_name_nchda          <- "S://CHAMPION/Analysis/Results/CHD code curation/nchda_clean_data.csv"
  in_file_name_ons_lifestatus <- "S://CHAMPION/Data/ONS Data 2023/FILE0179716_NIC234297_ONS_Mortality.txt"
  
  out_file_name <- "S://CHAMPION/Analysis/Results/CHD code curation/nchda_mortality_and_life_status.csv"
  

# 1. We load the processed nchda and the ons life status data
  ## the input files exist?
  for( in_file_name in c(in_file_name_nchda,in_file_name_ons_lifestatus) ) {  
    file_exists = ifelse( file.exists( in_file_name ), TRUE, FALSE )
    if( !file_exists ) 
      stop( paste0("ERROR: cannot find the file ", in_file_name, "; please revise the source directory and file name" ) )
    rm( file_exists )
  }
  
  nchda_data   <- readr::read_csv( in_file_name_nchda , col_types = "c" )
  ons_data <- readr::read_delim( in_file_name_ons_lifestatus , col_types = "c", delim="|" )
  gc()

# 2. We only need a few fields from nchda: identifiers, ages, and discharge status
  names(nchda_data) <- tolower(names(nchda_data))
  nchda_data <- nchda_data[ , c("championpatid","launchesrecid","raop","raodis","raod","x4.03.discharge.status")]
  
  #2.1. any negative ages set to NA
  nchda_data$raop[ which(nchda_data$raop<0) ] = NA #29
  summary( nchda_data$raop )
  summary( nchda_data$raodis ) #2593 missing
  length( which(nchda_data$raodis<0) ) #42 discharge ages being negative
  nchda_data$raodis[ which(nchda_data$raodis<0) ] = NA
  summary( nchda_data$raodis ) #2635 missing
  summary( nchda_data$raod )
  sum( !is.na(nchda_data$raod) ) #10609 death ages
  
  
# 3. Defining the NCHDA age of death (merging discharge information with the recorded age of death)
  #3.1. max age in NCHDA by patient
  library( data.table )
  setDT(nchda_data)
  nchda_data[ , `:=` (
               pat_max_aop = max( raop, na.rm=TRUE ),
               pat_max_aodis = max( raodis, na.rm=TRUE )
  ), by= .(championpatid) ]
  
  nchda_data$pat_max_age_nchda = pmax( nchda_data$pat_max_aop, nchda_data$pat_max_aodis )
  
  #3.2. NCHDA age at death
  nchda_data$ageofdeath = nchda_data$raod
  table( !is.na( nchda_data$ageofdeath ) ) # 10609 records
  
  # testing whether the definition is at patient level (and imposing it)
  nchda_data[ , aux := max( ageofdeath, na.rm=TRUE ), by= .(championpatid) ]
  table( !is.na( nchda_data$aux ), !is.na( nchda_data$ageofdeath ) ) # we need to add it to 497 records
  nchda_data$ageofdeath <- nchda_data$aux
  
  #compatibility with other NCHDA ages
  sum( nchda_data$ageofdeath < nchda_data$pat_max_age_nchda - 1/365.25 , na.rm=TRUE ) # 111 deaths occur before known ages
  
  sum( nchda_data$ageofdeath < nchda_data$pat_max_aop - 1/365.25 , na.rm=TRUE ) # only 6 occur before procedure ages
  sel = which( nchda_data$ageofdeath < nchda_data$pat_max_aop - 1/365.25 )
  nchda_data[ sel, c("championpatid", "launchesrecid","raop","raodis","raod","pat_max_aop","ageofdeath")]
  
  #3.3 NCHDA discharge age and status
  table( nchda_data$x4.03.discharge.status, useNA = "always" )

  sel = which( nchda_data$x4.03.discharge.status =="D. Died in hospital" & !is.na(nchda_data$raodis) )
  length( sel ) #5500 deaths at discharge with a valida age at discharge
  sum( nchda_data$raodis[ sel ] < nchda_data$pat_max_aop[ sel ] - 1/365.25, na.rm=TRUE ) ## 29 occur before procedure ages
  
  # any new death w.r.t. age of death?
  sel2 =  is.na( nchda_data$ageofdeath[ sel ] )
  table( sel2 ) # 61 extra deaths provided by discharge status
  nchda_data$ageofdeath[ sel[sel2] ] = nchda_data$raodis[ sel[sel2] ]
  
  # testing whether the definition is at patient level
  nchda_data[ , aux := max( ageofdeath, na.rm=TRUE ), by= .(championpatid) ]
  table( !is.na( nchda_data$aux ), !is.na( nchda_data$ageofdeath ) ) # we need to add it to 61 records
  nchda_data$ageofdeath <- nchda_data$aux # now death in 11,228 records

  length( unique( nchda_data$championpatid[ !is.na(nchda_data$ageofdeath) ] ) ) # 4,923 patients with known age of death
  
  
# 4.Processing ONS life status and mortality data set
  names( ons_data ) <- tolower( names( ons_data ) )
  
  # rename study_id as launchesrecid
  names( ons_data )[ grep("study_id", names( ons_data ) ) ] <- "launchesrecid"

  # some record had multiple identifiers, and were submitted multiple times as required (we need to recover the record ID and life status value)
  table( nchar( ons_data$launchesrecid ) ) # 835 records with a record id longer than 8 characters
  ons_data$launchesrecid2 <- substr(ons_data$launchesrecid, 1, 8)
  length( unique( ons_data$launchesrecid2 ) ) # 180542 unique record IDs
  
  # num entries per 8-character record ID
  ons_data$roworder = with( ons_data, order( launchesrecid2, launchesrecid ) )
  ons_data = transform( ons_data, numentry   = ave( roworder, launchesrecid2, FUN = function(x) order(x, decreasing = FALSE ) ) )
  ons_data = transform( ons_data, numEntries = ave( numentry, launchesrecid2, FUN = function(x) max(x, na.rm = TRUE ) ) )
  table( ons_data$numEntries, ons_data$numentry, useNA="always" ) # 828 duplicated record IDs
  
  # do those have the same values?
  ons_data = transform( ons_data, sameValues = ave( age_life_status, launchesrecid2, FUN = function(x) ifelse( length(x)==2, x[1] == x[2], NA ) ) )
  table( ons_data$sameValues )
  # one pair of duplicated record IDs has different life status for each copy
  ons_data[ which( ons_data$sameValues==0 ), ] # we wil keep the first entry since it provides date of death and it has match rank 1
  
  # we drop the duplicates
  ons_data <- ons_data[ - which( ons_data$numentry==2 ), ]
  row.names( ons_data ) <- NULL
  ons_data$roworder <- NULL
  ons_data$numentry <- NULL
  ons_data$numEntries <- NULL
  gc()
  
  # no ambiguities left: we update the record ID
  sum( duplicated( ons_data$launchesrecid2 ) ) # 0
  ons_data$launchesrecid <- ons_data$launchesrecid2
  ons_data$launchesrecid2 <- NULL
  gc()

# 5. Integrating the ONS life status into the nchda data (at patient level)  
  aux = merge( nchda_data, ons_data, by = "launchesrecid", all.x=T )
  nrow( aux ) == nrow( nchda_data ) # no extra rows introduced after merge
  nchda_data <- aux 
  rm( aux )  
  rm( ons_data )
  gc( )
    
  table( nchda_data$life_status, useNA = "always" )
  #      A      D   <NA> 
  # 161457  19085  15896 

  # Extending the information at patient level
  # life status
  nchda_data = transform( nchda_data, ons_life_status = ave( life_status, championpatid, FUN = function(x) max( x, na.rm = TRUE ) ) )
  table( nchda_data$ons_life_status, nchda_data$life_status, useNA = "always" ) #minor changes
  # age at life status
  #setDT(nchda_data)
  nchda_data[ , ons_age_life_status := max( age_life_status[ life_status == ons_life_status ], na.rm=TRUE ), by= .(championpatid) ]
  nchda_data$ons_age_life_status[ is.na(nchda_data$ons_life_status) ] <- NA
  summary( nchda_data$age_life_status )
  summary( nchda_data$ons_age_life_status )
  
  # we remove the life statuses with negative ages
  sel = which( nchda_data$ons_age_life_status < 0 )
  length( sel ) # 2
  nchda_data$ons_age_life_status[ sel ] = NA
  nchda_data$ons_life_status[ sel ] = NA

  table( nchda_data$ons_life_status, useNA = "always" )
  summary( nchda_data$ons_age_life_status )
  
# 6. Tidying data fields (we remove the all-code fields used to derive life status)
  to_delete = names( nchda_data )[ grep( "rao|pat_max|x4.03", names( nchda_data ) ) ]
  nchda_data[ , to_delete ] <- NULL
  nchda_data[ , c("aux", "hes_ons_match_rank") ] <- NULL  
  nchda_data[ , c( "life_status", "age_life_status", "age_death") ] <- NULL  
  nchda_data[ , c( "date_extract", "sameValues", "mydob") ] <- NULL 
  rm( sel, sel2, to_delete )
  gc()
  
# 7. Saving the data into file (we adopt the data table approach)
  #require( data.table )
  #setDT(pat_data)
  fwrite( x = nchda_data, file=out_file_name, row.names = FALSE )
gc()  
