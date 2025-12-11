# 04.03 PROCESSING COMPLICATIONS

# Complications are recorded in NCHDA since April 2015
# Adult composite outcome will contain any of the following:
 # unplanned need for a pacemaker, 
 # need for renal replacement therapy (including peritoneal dialysis), 
 # postprocedural requirement for tracheostomy, 
 # prolonged pleural drainage, 
 # use of extracorporeal life support, 
 # new neurological impairment (global or focal), 
 # surgical site infection requiring surgical intervention.  

## system clearing (comment out if you want to keep previous work)
  rm( list=ls()) #remove any previous R object
  gc()           #garbage collection of memory; It can be useful to call gc after a large object has been removed, as this may prompt R to return memory to the operating system.

#PARAMETERS: 
## input data: files processed at record level (files were generated in Step 02)
  ## we only need NCHDA data
  in_file_name_nchda                   <- "S://CHAMPION/Analysis/Results/CHD code curation/nchda_clean_data.csv"
  in_file_name_composite_complications <- "S://CHAMPION/Analysis/R script files/CHD code curation/06 Outcomes of interest/mapping files/composite_complications.txt"
  
  out_file_name <- "S://CHAMPION/Analysis/Results/CHD code curation/nchda_composite_complications.csv"
  

# 1. We load the processed nchda and the ons life status data
  ## the input files exist?
  for( in_file_name in c(in_file_name_nchda,in_file_name_composite_complications) ) {  
    file_exists = ifelse( file.exists( in_file_name ), TRUE, FALSE )
    if( !file_exists ) 
      stop( paste0("ERROR: cannot find the file ", in_file_name, "; please revise the source directory and file name" ) )
    rm( file_exists )
  }
  
  nchda_data          <- readr::read_csv( in_file_name_nchda , col_types = "c" )
  complications_codes <- readr::read_delim( in_file_name_composite_complications , col_types = "c", delim="\t" )
  gc()

# 2. We only need a few fields from nchda: identifiers, ages, and discharge status
  names(nchda_data) <- tolower(names(nchda_data))
  nchda_data <- nchda_data[ , c("championpatid","launchesrecid","raop","finyr","x4.08.post.operative.complications",
                                names(nchda_data)[ grep("^diagcode[0-9]", names(nchda_data)) ],
                                names(nchda_data)[ grep("^proccode[0-9]", names(nchda_data)) ],
                                names(nchda_data)[ grep("^comorbidity[0-9]", names(nchda_data)) ] 
                                )
                            ]

  #2.1. any negative ages set to NA
  length( which(nchda_data$raop<0) ) #a few ages at procedure being negative
  nchda_data$raop[ which(nchda_data$raop<0) ] = NA
  summary( nchda_data$raop )
  
# 3. Defining the NCHDA composite complication outcome
  
  #3.1. any complications code recorded in x4.08
  search_codes <- paste0( complications_codes$code, collapse = "|")
  sum( grepl( search_codes, nchda_data$x4.08.post.operative.complications, useBytes = TRUE ) )
  
  #3.2. any complications code recorded elsewhere
  nchda_data$all_diags <- do.call( paste, c( nchda_data[ grep("^diagcode[0-9]", names(nchda_data)) ], sep=" " ) )
  nchda_data$all_diags <- gsub( "  ", "", gsub( "NA", "", nchda_data$all_diags ) )
  nchda_data$all_comorbs <- do.call( paste, c( nchda_data[ grep("^comorbidity[0-9]", names(nchda_data)) ], sep=" " ) )
  nchda_data$all_comorbs <- gsub( "  ", "", gsub( "NA", "", nchda_data$all_comorbs ) )
  nchda_data$all_procs <- do.call( paste, c( nchda_data[ grep("^proccode[0-9]", names(nchda_data)) ], sep=" " ) )
  nchda_data$all_procs <- gsub( "  ", "", gsub( "NA", "", nchda_data$all_procs ) )
  
  nchda_data$all_codes <- do.call( paste, c( nchda_data[ c("all_diags","all_comorbs","all_procs")], sep=" " ) )
  nchda_data$all_diags <- NULL
  nchda_data$all_comorbs <- NULL
  nchda_data$all_procs <- NULL
  nchda_data$all_codes <- gsub( "  ", "", nchda_data$all_codes )
  
  sum( grepl( search_codes, nchda_data$all_codes, useBytes = TRUE ) )
  
  #3.3. any complications description recorded ##Note that complication descriptions were adapted to work with the grep() function in R
  search_descriptions <- paste0( complications_codes$description, collapse = "|")
  sum( grepl( search_descriptions, nchda_data$x4.08.post.operative.complications, useBytes = TRUE ) )
  
  #3.3 composite outcome (not taking death into account)
  nchda_data$any_complication = grepl( search_codes, nchda_data$x4.08.post.operative.complications, useBytes = TRUE ) |
    grepl( search_codes, nchda_data$all_codes, useBytes = TRUE ) |
    grepl( search_descriptions, nchda_data$x4.08.post.operative.complications, useBytes = TRUE )
  
  sum( nchda_data$any_complication ) 
  
  #3.4 not recorded officially before 2015
  nchda_data$any_complication[ which( (nchda_data$any_complication==0) & is.na( nchda_data$x4.08.post.operative.complications )) ] = NA #little recording pre-2015
  nchda_data$any_complication[ nchda_data$finyr < 2015 ] = NA
  
  table( nchda_data$finyr, nchda_data$any_complication, useNA = "always" )

  sum( nchda_data$any_complication, na.rm = TRUE )
  
  #in children (post-2015)
  table( nchda_data$any_complication[ nchda_data$raop < 18.0 & nchda_data$finyr >= 2015], useNA = "always" )
  
  #in adults  
  table( nchda_data$any_complication[ nchda_data$raop >= 18.0 & nchda_data$finyr >= 2015], useNA = "always" )


# 4. Tidying data fields (we remove the all-code fields used to derive the complications outcome)
  to_keep = names( nchda_data )[ grep( "champion|recid|any_complication", names( nchda_data ) ) ]
  nchda_data <- nchda_data[ , to_keep ]
  
  rm( list = setdiff( ls(), c("nchda_data","out_file_name") ) )
  gc()
  
# 7. Saving the data into file (we adopt the data table approach)
  require( data.table )
  fwrite( x = nchda_data, file=out_file_name, row.names = FALSE )
gc()  
