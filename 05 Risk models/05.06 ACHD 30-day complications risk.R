# 05.06 ACHD 30-DAY COMPLICATIONS RISK

## system clearing (comment out if you want to keep previous work)
  rm( list=ls()) #remove any previous R object
  gc()           #garbage collection of memory; It can be useful to call gc after a large object has been removed, as this may prompt R to return memory to the operating system.

#PARAMETERS: 
## input data: NCHDA processed data (files were generated in Step 07.03)
  ## we only need the NCHDA data processed for ACHD
  # Adult CHD data
  in_file_nameACHDdata <- "S://CHAMPION/Analysis/Results/CHD code curation/nchda_achd_data.csv"
  
  #ACHD model coefficients #DANGER: the intercept is the last coefficient (see data file for field names)
  in_file_name_ACHD_model <- "S://CHAMPION/Analysis/R script files/CHD code curation/07 Risk models/mapping files/ACHD_30d_complications_model_coefficients.txt"
  
  
# 1. We load the processed nchda and the ons life status data
  ## the input files exist?
  for( file_name in c(in_file_nameACHDdata, in_file_name_ACHD_model) ) {  
    file_exists = ifelse( file.exists( file_name ), TRUE, FALSE )
    if( !file_exists ) 
      stop( paste0("ERROR: cannot find the file ", file_name, "; please revise the source directory and file name" ) )
    rm( file_exists )
  }
  
  nchda <- read.csv( in_file_nameACHDdata, colClasses = "character", stringsAsFactors = FALSE )

  achd_model <- read.csv( in_file_name_ACHD_model, colClasses="character", sep = "\t", encoding = "ISO-8859-2" )
  
  
# 2. We format the numeric and flag fields (used in ACHD model)
  num_vars = c( "raop", "spgrp_compl", "proc_complexity", "non_elective", #"esccomplexity",
                "congenital.comorbidity", "acquired.comorbidity", 
                #"additional.cardiac.risk.factors", 
                "sternseq", "preprocnyhastatus_trans", "combined_evf_trans",
  "complics_30d" )
  
  for( var in num_vars ){
    nchda[ , var ] = as.numeric( nchda[ , var ] )
  }

  achd_model$coefficient = as.numeric( achd_model$coefficient )  
      
# 3. We delete those who died without a complication and those with missing recording of the NCHDA field 
  table( nchda$death_30d, nchda$complics_30d, useNA = "always" )
  todelete = which( (nchda$death_30d=="1" & nchda$complics_30d==0) | is.na(nchda$complics_30d) )
  nchda <- nchda[ -todelete, ]
  row.names( nchda ) <- NULL
  table( nchda$death_30d, nchda$complics_30d, useNA = "always" )  
  
# 10. ACHD model
  
  X = rbind( 
    (nchda$raop-18.0),            # Age in years since 18
#    nchda$spgrp_compl==1, # Procedure risk grouping 1 (lowest)
    nchda$spgrp_compl==2, # Procedure risk grouping 2
    nchda$spgrp_compl==3, # Procedure risk grouping 3
    nchda$spgrp_compl==4, # Procedure risk grouping 4
    nchda$spgrp_compl==5, # Procedure risk grouping 5
    nchda$proc_complexity == 1, # Complex procedure
    nchda$non_elective == 1, # Non-elective procedure
    nchda$congenital.comorbidity == 1, # Congenital comorbidity
    nchda$acquired.comorbidity == 1, # Acquired comorbidity
    nchda$sternseq == 1, # At least third sternotomy
    nchda$preprocnyhastatus_trans == 1, # NYHA Physical activity limitations
    nchda$combined_evf_trans == 1, # Poor ventricular ejection fraction
    rep( 1, nrow(nchda) ) # Constant
  )
  
  BETA = achd_model$coefficient
    
  nrow( X ) == length( BETA )
  
  nchda$achd_Z = t( X ) %*% BETA

  rm( X, BETA )
    
  # predicted probability of complications at 30 days
  nchda$achd_prob = 1 / (1 + exp(-nchda$achd_Z) )
  
  # predicted total number of complications at 30 days
  sum( nchda$achd_prob, na.rm = T )
  
  # actual total number of complications at 30 days
  sum( nchda$complics_30d, na.rm = T )
  
