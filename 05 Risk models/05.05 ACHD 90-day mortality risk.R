# 05.05 ACHD 90-DAY MORTALITY RISK

## system clearing (comment out if you want to keep previous work)
  rm( list=ls()) #remove any previous R object
  gc()           #garbage collection of memory; It can be useful to call gc after a large object has been removed, as this may prompt R to return memory to the operating system.

#PARAMETERS: 
## input data: NCHDA processed data (files were generated in Step 07.03)
  ## we only need the NCHDA data processed for ACHD
  # Adult CHD data
  in_file_nameACHDdata <- "S://CHAMPION/Analysis/Results/CHD code curation/nchda_achd_data.csv"
  
  #ACHD model coefficients #DANGER: the intercept is the last coefficient (see data file for field names)
  in_file_name_ACHD_model <- "S://CHAMPION/Analysis/R script files/CHD code curation/07 Risk models/mapping files/ACHD_90d_mortality_model_coefficients.txt"
  
  
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
  num_vars = c( "raop", "spgrp_death", "proc_complexity", #"non_elective", 
                "esccomplexity", "congenital.comorbidity", "acquired.comorbidity", 
                "additional.cardiac.risk.factors", "sternseq",
                "preprocnyhastatus_trans", "combined_evf_trans",
  "death_90d", "spell_90d" )
  
  for( var in num_vars ){
    nchda[ , var ] = as.numeric( nchda[ , var ] )
  }

  achd_model$coefficient = as.numeric( achd_model$coefficient )  

# 3. Selecting only 90-day episodes
  nchda <- nchda[ nchda$spell_90d==1, ]
  row.names( nchda ) <- NULL
          
# 4. ACHD model
  
  X = rbind( 
    (nchda$raop-18.0),            # Age in years since 18
#    nchda$spgrp_death==1, # Procedure risk grouping 1 (lowest)
    nchda$spgrp_death==2, # Procedure risk grouping 2 (highest)
    nchda$proc_complexity == 1, # Complex procedure
#    nchda$non_elective == 1, # Non-elective procedure
    nchda$esccomplexity == 2, # Moderate patient CHD complexity (ESC guideline definition)
    nchda$esccomplexity == 3, # Severe patient CHD complexity (ESC guideline definition)
    nchda$congenital.comorbidity == 1, # Congenital comorbidity
    nchda$acquired.comorbidity == 1, # Acquired comorbidity
    nchda$additional.cardiac.risk.factors == 1, # Additional cardiac risk factor
    nchda$sternseq == 1, # At least third sternotomy
    nchda$preprocnyhastatus_trans == 1, # NYHA Physical activity limitations
    nchda$combined_evf_trans == 1, # Poor ventricular ejection fraction
    rep( 1, nrow(nchda) ) # Constant
  )
  
  BETA = achd_model$coefficient
    
  nrow( X ) == length( BETA )
  
  nchda$achd_Z = t( X ) %*% BETA

  rm( X, BETA )
    
  # predicted probability of death at 90 days
  nchda$achd_prob = 1 / (1 + exp(-nchda$achd_Z) )
  
  # predicted total number of deaths at 90 days
  sum( nchda$achd_prob, na.rm = T )
  
  # actual total number of deaths at 90 days
  sum( nchda$death_90d, na.rm = T )
  
