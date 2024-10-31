# 04.03 COMBINING DIAGNOSTIC AND COMORBIDITY (definition at record level)

# Note that for the resulting values are used in risk models.

## system clearing (comment out if you want to keep previous work)
  rm( list=ls()) #remove any previous R object
  gc()           #garbage collection of memory; It can be useful to call gc after a large object has been removed, as this may prompt R to return memory to the operating system.

#PARAMETERS: 
## input data: files processed at record level (files were generated in Step 02)
  ## we only need NCHDA data
  in_file_name_diag   <- "S://CHAMPION/Analysis/Results/CHD code curation/nchda_diagnostic_top_rank.csv"
  in_file_name_comorb <- "S://CHAMPION/Analysis/Results/CHD code curation/nchda_additional_risk_factors.csv"
  
  out_file_name <- "S://CHAMPION/Analysis/Results/CHD code curation/nchda_diagnostic_and_comorbidity_categories.csv"
  

# 1. We load the patient level data and the comorbidity mappings
  ## the input files exist?
  for( in_file_name in c(in_file_name_diag,in_file_name_comorb) ) {  
    file_exists = ifelse( file.exists( in_file_name ), TRUE, FALSE )
    if( !file_exists ) 
      stop( paste0("ERROR: cannot find the file ", in_file_name, "; please revise the source directory and file name" ) )
    rm( file_exists )
  }
  
  diag_data   <- readr::read_csv( in_file_name_diag , col_types = "c" )
  comorb_data <- readr::read_csv( in_file_name_comorb , col_types = "c" )
  gc()
    
# 2. Combining the two data sets:
  names( diag_data )
  names( comorb_data )
  
  pat_data <- merge( diag_data, comorb_data, by= c("CHAMPIONpatID","LAUNCHESrecID","isAdult") )
  
  gc()  

# 3. Tidying data fields (we remove the all-code fields used to derive toprank and comorbidities)
  to_delete = names( pat_data )[ grep( "alldiag", names( pat_data ) ) ]
  pat_data[ , to_delete ] <- NULL
  
# 4. Saving the data into file (we adopt the data table approach again)
  require( data.table )
  setDT(pat_data)
  fwrite( x = pat_data, file=out_file_name, row.names = FALSE )
gc()  
