# 04.01 DIAGNOSTIC HIERARCHICAL GROUPING (definition at record level)

# Note that for children the resulting value is part of risk models.
# Note that for adults the resulting value was not considered of interest.

## system clearing (comment out if you want to keep previous work)
  rm( list=ls()) #remove any previous R object
  gc()           #garbage collection of memory; It can be useful to call gc after a large object has been removed, as this may prompt R to return memory to the operating system.

#PARAMETERS: 
## input data: files processed at record level (files were generated in Step 02)
  ## we only need NCHDA data
  in_file_name <- "S://CHAMPION/Analysis/Results/CHD code curation/nchda_clean_data.csv"
  in_file_namePRAIS <- "S://CHAMPION/Analysis/R script files/CHD code curation/04 Diagnostic and comorbidity mappings/mapping files/PRAIS_all_codes_ranking.txt"
  in_file_nameAdultCHD <- "S://CHAMPION/Analysis/R script files/CHD code curation/04 Diagnostic and comorbidity mappings/mapping files/AdultCHD_all_codes_ranking.txt"
  
  out_file_name <- "S://CHAMPION/Analysis/Results/CHD code curation/nchda_diagnostic_top_rank.csv"
  

# 1. We load the patient level data and the mappings
  ## the input files exist?
  for( file_name in c(in_file_name,in_file_namePRAIS,in_file_nameAdultCHD) ) {  
    file_exists = ifelse( file.exists( file_name ), TRUE, FALSE )
    if( !file_exists ) 
      stop( paste0("ERROR: cannot find the file ", file_name, "; please revise the source directory and file name" ) )
    rm( file_exists )
  }
  
  pat_data = read.csv( in_file_name, colClasses = "character", stringsAsFactors = FALSE )
  prais_map <- read.csv( in_file_namePRAIS, colClasses="character", sep = "\t", encoding = "ISO-8859-2" )
  achd_map <- read.csv( in_file_nameAdultCHD, colClasses="character", sep = "\t", encoding = "ISO-8859-2" )
  

# 2. field definitions to use:
  # age at operation
  pat_data$ageatop = as.numeric( pat_data$Raop )
  # is Adult? (flat)
  pat_data$isAdult = pat_data$ageatop >= 18.0
  # all diagnostic and comorbidity codes
  selection = setdiff( names( pat_data )[ grep("diagcode|comorbidity", names(pat_data)) ], "comorbidity_present" )
  pat_data$alldiagcomorb = do.call( paste, c( pat_data[ selection ], sep="_" ) )
    
  #3 we keep only the record ID, age at operation, and diagnostic codes
  pat_data <- pat_data[ , c("CHAMPIONpatID","LAUNCHESrecID","isAdult","alldiagcomorb")]
  gc()  

# 4. Paediatric records 
  sel = pat_data$isAdult == FALSE

  pat_data$toprank[ sel ] = 33
  for( i in 32:1 ){ #this implies looping over all 1,799 codes so it takes time
    print( i )
    codes = prais_map$code[ prais_map$NewCodeGroupRank == i ]

    for( code in codes ){
      code_found = grepl( code, pat_data$alldiagcomorb[ sel ] )
      pat_data$toprank[ sel ] = i * code_found + pat_data$toprank[ sel ] * ( 1 - code_found )
    }
  }  
  
  table( pat_data$toprank, useNA = "always" )
  gc()
  
# 5. Adult records
  sel = pat_data$isAdult == TRUE
  
  pat_data$toprank[ sel ] = 33
  for( i in 32:1 ){ #this implies looping over all 1,799 codes so it takes time
    print( i )
    codes = prais_map$code[ prais_map$NewCodeGroupRank == i ]
    
    for( code in codes ){
      code_found = grepl( code, pat_data$alldiagcomorb[ sel ] )
      pat_data$toprank[ sel ] = i * code_found + pat_data$toprank[ sel ] * ( 1 - code_found )
    }
  }  
  
  table( pat_data$toprank, useNA = "always" )
  

# 5. Saving the data into file (we adopt the data table approach again)
  require( data.table )
  setDT(pat_data)
  fwrite( x = pat_data, file=out_file_name, row.names = FALSE )
gc()  
