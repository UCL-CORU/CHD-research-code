# 04.02 COMORBIDITY ALGORITHM (definition at record level)

# Note that for the resulting value is part of risk models.

## system clearing (comment out if you want to keep previous work)
  rm( list=ls()) #remove any previous R object
  gc()           #garbage collection of memory; It can be useful to call gc after a large object has been removed, as this may prompt R to return memory to the operating system.

#PARAMETERS: 
## input data: files processed at record level (files were generated in Step 02)
  ## we only need NCHDA data
  in_file_name <- "S://CHAMPION/Analysis/Results/CHD code curation/nchda_clean_data.csv"
  in_file_namePRAIS <- "S://CHAMPION/Analysis/R script files/CHD code curation/04 Diagnostic and comorbidity mappings/mapping files/PRAIS_additional_risk_factors.txt"
  in_file_nameAdultCHD <- "S://CHAMPION/Analysis/R script files/CHD code curation/04 Diagnostic and comorbidity mappings/mapping files/AdultCHD_additional_risk_factors.txt"
  
  out_file_name <- "S://CHAMPION/Analysis/Results/CHD code curation/nchda_additional_risk_factors.csv"
  

# 1. We load the patient level data and the comorbidity mappings
  ## the input files exist?
  for( file_name in c(in_file_name,in_file_namePRAIS,in_file_nameAdultCHD) ) {  
    file_exists = ifelse( file.exists( file_name ), TRUE, FALSE )
    if( !file_exists ) 
      stop( paste0("ERROR: cannot find the file ", file_name, "; please revise the source directory and file name" ) )
    rm( file_exists )
  }
  
  pat_data <- readr::read_csv( in_file_name , col_types = "c" )
  prais_map <- read.csv( in_file_namePRAIS, colClasses="character", sep = "\t", encoding = "ISO-8859-2" )
  achd_map <- read.csv( in_file_nameAdultCHD, colClasses="character", sep = "\t", encoding = "ISO-8859-2" )
  
  #the mapping lookups have only two columns
  prais_map <- prais_map[ , c(1,2) ]
  achd_map  <- achd_map[ , c(1,2) ]
  gc()
    
# 2. field definitions to use:
  # age at operation
  pat_data$ageatop = as.numeric( pat_data$Raop )
  # is Adult? (flat)
  pat_data$isAdult = pat_data$ageatop >= 18.0
  # all diagnostic and comorbidity codes
  selection = setdiff( names( pat_data )[ grep("diagcode|comorbidity|proccode", names(pat_data)) ], "comorbidity_present" )
  pat_data$alldiagcomorbproc = do.call( paste, c( pat_data[ selection ], sep="_" ) )
    
  #3 we keep only the record ID, age at operation, and diagnostic codes
  pat_data <- pat_data[ , c("CHAMPIONpatID","LAUNCHESrecID","isAdult","alldiagcomorbproc")]
  gc()  

# 4. Paediatric records 
  sel = pat_data$isAdult == FALSE

  groups = unique( prais_map$AdditionalRiskFactorGroup_PRAIS3 )
  
  for( group in groups ){ #this implies looping over 155 codes so it takes time
    print( group )
    codes = prais_map$code[ prais_map$AdditionalRiskFactorGroup_PRAIS3 == group ]

    pat_data[ sel, group ] = 0
    
    for( code in codes ){
      code_found =  grep( code, pat_data$alldiagcomorbproc[ sel ] )
      pat_data[ which( sel )[ code_found ], group ] = 1
    }
  }  
  
  for( group in groups ){
    print( table( pat_data[ sel, group ] ) )
  }
  
# 5. Adult records
  sel = pat_data$isAdult == TRUE
  
  groups = unique( achd_map$AdditionalRiskFactorGroup_PRAIS3 )
  
  for( group in groups ){ #this implies looping over 155 codes so it takes time
    print( group )
    codes = achd_map$code[ achd_map$AdditionalRiskFactorGroup_PRAIS3 == group ]

    pat_data[ sel, group ] = 0
    
    for( code in codes ){
      code_found = grep( code, pat_data$alldiagcomorbproc[ sel ] )
      pat_data[ which( sel )[ code_found ], group ] = 1
    }
  }  
  
  for( group in groups ){
    print( table( pat_data[ sel, group ] ) )
  }  
  
# 5. Saving the data into file (we adopt the data table approach again)
  require( data.table )
  setDT(pat_data)
  fwrite( x = pat_data, file=out_file_name, row.names = FALSE )
gc()  
