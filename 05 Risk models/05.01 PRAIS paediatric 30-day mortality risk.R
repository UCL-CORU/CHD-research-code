# 07.02 PRAIS PAEDIATRIC 30-DAY MORTALITY RISK

## system clearing (comment out if you want to keep previous work)
  rm( list=ls()) #remove any previous R object
  gc()           #garbage collection of memory; It can be useful to call gc after a large object has been removed, as this may prompt R to return memory to the operating system.

#PARAMETERS: 
## input data: NCHDA processed data (files were generated in Step 07.01)
  ## we only need the NCHDA data processed for PRAIS
  # prais data
  in_file_namePRAISdata <- "S://CHAMPION/Analysis/Results/CHD code curation/nchda_prais_data.csv"
  
  #prais model coefficients #DANGER: the intercept is the last coefficient (see data file for field names)
  in_file_name_prais_model <- "S://CHAMPION/Analysis/R script files/CHD code curation/07 Risk models/mapping files/PRAIS_model_coefficients.txt"
  
  
# 1. We load the processed nchda and the ons life status data
  ## the input files exist?
  for( file_name in c(in_file_namePRAISdata, in_file_name_prais_model) ) {  
    file_exists = ifelse( file.exists( file_name ), TRUE, FALSE )
    if( !file_exists ) 
      stop( paste0("ERROR: cannot find the file ", file_name, "; please revise the source directory and file name" ) )
    rm( file_exists )
  }
  
  nchda <- read.csv( in_file_namePRAISdata, colClasses = "character", stringsAsFactors = FALSE )

  prais_model <- read.csv( in_file_name_prais_model, colClasses="character", sep = "\t", encoding = "ISO-8859-2" )
  
  
# 2. We format the numeric and flag fields (used in PRAIS)
  num_vars = c( "raop", "x2.03.weight", "fUVHflag",
  "acquired.comorbidity", "additional.cardiac.risk.factors", "congenital.comorbidity", "severity.of.illness", "premature",
  "death_30d" )
  
  for( var in num_vars ){
    nchda[ , var ] = as.numeric( nchda[ , var ] )
  }

  prais_model$coefficient = as.numeric( prais_model$coefficient )  
      
# 10. PRAIS model
  
  X = rbind( 
    sqrt( nchda$raop ),    # sqrt(age)
    nchda$raop,            # Age
    sqrt( nchda$x2.03.weight ), # sqrt(weight)
    nchda$x2.03.weight,    # Weight
    nchda$broad.diagnosis.grouping.used.in.prais2=="1", # Diagnosis grouping 1
    nchda$broad.diagnosis.grouping.used.in.prais2=="2", # Diagnosis grouping 2
    nchda$broad.diagnosis.grouping.used.in.prais2=="3", # Diagnosis grouping 3
    nchda$broad.diagnosis.grouping.used.in.prais2=="4", # Diagnosis grouping 4
    nchda$broad.diagnosis.grouping.used.in.prais2=="5", # Diagnosis grouping 5
    nchda$broad.diagnosis.grouping.used.in.prais2=="6", # Diagnosis grouping 6
    nchda$broad.diagnosis.grouping.used.in.prais2=="7", # Diagnosis grouping 7
    nchda$broad.diagnosis.grouping.used.in.prais2=="8", # Diagnosis grouping 8
    nchda$broad.diagnosis.grouping.used.in.prais2=="9", # Diagnosis grouping 9
    nchda$broad.diagnosis.grouping.used.in.prais2=="10", # Diagnosis grouping 10
    nchda$broad.diagnosis.grouping.used.in.prais2=="11", # Diagnosis grouping 11
    nchda$specproc_group == "01", # Specific procedure grouping 1
    nchda$specproc_group == "02", # Specific procedure grouping 2
    nchda$specproc_group == "03", # Specific procedure grouping 3
    nchda$specproc_group == "04", # Specific procedure grouping 4
    nchda$specproc_group == "05", # Specific procedure grouping 5
    nchda$specproc_group == "06", # Specific procedure grouping 6
    nchda$specproc_group == "07", # Specific procedure grouping 7
    nchda$specproc_group == "08", # Specific procedure grouping 8
    nchda$specproc_group == "09", # Specific procedure grouping 9
    nchda$specproc_group == "10", # Specific procedure grouping 10
    nchda$specproc_group == "11", # Specific procedure grouping 11
    nchda$specproc_group == "12", # Specific procedure grouping 12
    nchda$specproc_group == "13", # Specific procedure grouping 13
    nchda$specproc_group == "14", # Specific procedure grouping 14
    nchda$specproc_group == "15", # Specific procedure grouping 15
    nchda$specproc_group == "20", # Specific procedure grouping 20 (no sp)
    nchda$x3.07.proceduretype == "1. bypass", # Bypass procedure
    nchda$fUVHflag == 1, # Definite indication of univentricular heart
    nchda$additional.cardiac.risk.factors == 1, # Additional cardiac risk factor
    nchda$acquired.comorbidity == 1, # Acquired comorbidity
    nchda$congenital.comorbidity == 1, # Congenital comorbidity
    nchda$severity.of.illness == 1, # Severity of illness indicator
    nchda$premature == 1, # Prematurity
    rep( 1, nrow(nchda) ) # Constant
  )
  
  BETA = prais_model$coefficient
    
  nrow( X ) == length( BETA )
  
  nchda$prais_Z = t( X ) %*% BETA

  rm( X, BETA )
    
  # predicted probability of death at 30 days
  nchda$prais_prob = 1 / (1 + exp(-nchda$prais_Z) )
  
  # predicted total number of deaths at 30 days
  sum( nchda$prais_prob )
  
  # actual total number of deaths at 30 days
  sum( nchda$death_30d )
  
