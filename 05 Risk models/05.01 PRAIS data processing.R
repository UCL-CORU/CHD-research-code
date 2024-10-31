# 07.01 PRAIS DATA PROCESSING

## system clearing (comment out if you want to keep previous work)
  rm( list=ls()) #remove any previous R object
  gc()           #garbage collection of memory; It can be useful to call gc after a large object has been removed, as this may prompt R to return memory to the operating system.

#PARAMETERS: 
## input data: files processed at record level (files were generated in Step 02)
  ## we only need NCHDA data
  # NCHDA processed data
  in_file_name_nchda     <- "S://CHAMPION/Analysis/Results/CHD code curation/nchda_clean_data.csv"
  # NCHDA specific procedure
  in_file_name_sp        <- "S:/CHAMPION/Analysis/R Data files/nchda_processed_April_2000_March_22_2024-10-04.csv"
  # NCHDA diagnostic and comorbidity categories
  in_file_name_toprank_comorb <- "S://CHAMPION/Analysis/Results/CHD code curation/nchda_diagnostic_and_comorbidity_categories.csv"
  # mortality outcomes
  in_file_name_mortality <- "S://CHAMPION/Analysis/Results/CHD code curation/nchda_mortality_outcomes.csv"
  # prais broad procedure groups
  in_file_name_broad_proc <- "S://CHAMPION/Analysis/R script files/CHD code curation/07 Risk models/mapping files/PRAiS_broad_procedure_groups.txt"
  # prais broad diagnosis groups
  in_file_name_broad_diag <- "S://CHAMPION/Analysis/R script files/CHD code curation/07 Risk models/mapping files/PRAiS_broad_diagnosis_groups.txt"
  # prais ranking of codes
  in_file_namePRAIS <- "S://CHAMPION/Analysis/R script files/CHD code curation/04 Diagnostic and comorbidity mappings/mapping files/PRAIS_all_codes_ranking.txt"

  #Processed data for PRAIS
  out_file_namePRAISdata <- "S://CHAMPION/Analysis/Results/CHD code curation/nchda_prais_data.csv"

# 1. We load the processed nchda and the ons life status data
  ## the input files exist?
  for( file_name in c(in_file_name_nchda, in_file_name_sp, in_file_name_mortality, in_file_name_toprank_comorb, in_file_name_broad_diag, in_file_name_broad_proc, in_file_namePRAIS) ) {  
    file_exists = ifelse( file.exists( file_name ), TRUE, FALSE )
    if( !file_exists ) 
      stop( paste0("ERROR: cannot find the file ", file_name, "; please revise the source directory and file name" ) )
    rm( file_exists )
  }
  
  nchda_data    <- read.csv( in_file_name_nchda,          colClasses = "character", stringsAsFactors = FALSE )
  nchda_sp      <- read.csv( in_file_name_sp,             colClasses = "character", stringsAsFactors = FALSE )
  nchda_toprank <- read.csv( in_file_name_toprank_comorb, colClasses = "character", stringsAsFactors = FALSE )
  nchda_dead30d <- read.csv( in_file_name_mortality,      colClasses = "character", stringsAsFactors = FALSE )
  
  prais_proc_map <- read.csv( in_file_name_broad_proc, colClasses="character", sep = "\t", encoding = "ISO-8859-2" )
  prais_diag_map <- read.csv( in_file_name_broad_diag, colClasses="character", sep = "\t", encoding = "ISO-8859-2" )
  prais_fuvh_map <- read.csv( in_file_namePRAIS, colClasses="character", sep = "\t", encoding = "ISO-8859-2" )
  
  ## we process the procedure map
  names( prais_proc_map )
  prais_proc_map$specproc = as.numeric( substr( prais_proc_map$NumSpecProc_8_05, 1, 2 ) )
  prais_proc_map <- prais_proc_map[ !is.na(prais_proc_map$specproc ), ]
  row.names( prais_proc_map ) <- NULL
  prais_proc_map$specproc_group = substr( prais_proc_map$Specific.Procedure.Grouping, 20, 21 )
  prais_proc_map <- prais_proc_map[ , c( "specproc","specproc_group" ) ]
  
  # we process the FUVH map
  prais_fuvh_map <- prais_fuvh_map[ prais_fuvh_map$fUVHflag==1, c( "code", "fUVHflag" ) ] #67 codes
  row.names( prais_fuvh_map ) <- NULL
  
  gc()

# 2. We only need a few fields from each nchda file
  names(nchda_data) <- tolower(names(nchda_data))
  # we need to use allproccodes as part of the inclusion/exclusion, and to refine the spec proc groups
  nchda_data$allprocs = do.call( paste, c( nchda_data[ grep("^proccode", names(nchda_data)) ], sep="_" ) )
  # we also need all codes to assign the fUVH flag
  nchda_data$allcodes = do.call( paste, c( nchda_data[ grep("^proccode|^diagcode|^comorbidity", names(nchda_data)) ], sep="_" ) )
  
  nchda_data <- nchda_data[ , c("championpatid","launchesrecid", "finyr", "raop", "raodis", "x2.03.weight", "x3.07.proceduretype", "allprocs", "allcodes" )]
  
  names(nchda_sp) <- tolower(names(nchda_sp))
  nchda_sp <- nchda_sp[ , c("championpatid","launchesrecid", "sp_allocation", "aa_allocation" )] ## aa_allocation not needed for the risk model; added to double-check spec proc
  
  names(nchda_toprank) <- tolower(names(nchda_toprank))
  nchda_toprank <- nchda_toprank[ , c("championpatid","launchesrecid", "isadult", "toprank", 
                                      "acquired.comorbidity", "additional.cardiac.risk.factors", "congenital.comorbidity", "severity.of.illness", "premature" )]
  
  names(nchda_dead30d) <- tolower(names(nchda_dead30d))
  nchda_dead30d <- nchda_dead30d[ , c("championpatid","launchesrecid", "death_30d" )]
  gc()

# 3. We merge the files
  
  nchda <- merge( nchda_data, nchda_sp, by = c("championpatid","launchesrecid") )
  nchda <- merge( nchda, nchda_toprank, by = c("championpatid","launchesrecid") )
  nchda <- merge( nchda, nchda_dead30d, by = c("championpatid","launchesrecid") )
  
  nchda <- merge( nchda, prais_diag_map, by.x= c("toprank"), by.y= "NewDiagnosisGroupRank", all.x = TRUE )
  
  rm( nchda_data, nchda_sp, nchda_toprank, nchda_dead30d, prais_diag_map )
  gc( )
  
# 4. PRAIS INCLUSION: keep only children surgeries, etc. and format the numeric fields
  
  #4.1. keeping only children
  nchda <- nchda[ nchda$isadult==FALSE, ]
  row.names( nchda ) <- NULL
  nchda$isadult <- NULL
  
  #4.2. keeping only surgeries
  tokeep = which( nchda$x3.07.proceduretype %in% c("1. bypass","2. non-bypass","11. electrophysiology - surgery","7. hybrid") )
  table( nchda$x3.07.proceduretype[ tokeep ], useNA = "always" )
  nchda <- nchda[ tokeep, ]
  row.names( nchda ) <- NULL

  #4.3. only financial years from 2015 onwards
  nchda$finyr <- as.numeric( nchda$finyr )
  tokeep = which( nchda$finyr >= 2015 )
  nchda <- nchda[ tokeep, ]
  row.names( nchda ) <- NULL
  
  # 4.3. We exclude lung transplants
  todelete = which( nchda$sp_allocation == "03:lung_transplant" )
  nchda <- nchda[ - todelete, ]
  row.names( nchda ) <- NULL
  table( nchda$sp_allocation, useNA = "always" )

  #4.4. We exclude records having no code in group 27 (inclusion Procedure codes)
  # we first load the PRAIS mapping of codes to groups
  in_file_namePRAIS <- "S://CHAMPION/Analysis/R script files/CHD code curation/04 Diagnostic and comorbidity mappings/mapping files/PRAIS_all_codes_ranking.txt"
  prais_map <- read.csv( in_file_namePRAIS, colClasses="character", sep = "\t", encoding = "ISO-8859-2" )
  # we keep only the codes in group 27
  prais_map <- prais_map[ which( prais_map$NewCodeGroupRank=="27" ), ] # 316 procedure codes included in PRAIS
  # we then keep only records in that list of codes
  nchda$hasproc27 = 0
  for( code in prais_map$code ){
    code_found = grepl( code, nchda$allprocs )
    nchda$hasproc27[ code_found ] = 1
  }
  table( nchda$hasproc27, useNA = "always" ) #386 to delete 
  table( nchda$sp_allocation[ nchda$hasproc27 == 0], useNA = "always" ) # 197 with no qualifying codes; rest being mostly unallocated or ep_miscellaneous)
  nchda <- nchda[ - which( nchda$hasproc27 == 0 ), ]
  nchda$hasproc27 <- NULL
  rm( prais_map )
  row.names( nchda ) <- NULL
  gc()
  
  #4.4. formatting fields
  names( nchda ) <- tolower( names( nchda ) )
  for( numvar in c("raop","raodis","x2.03.weight", "toprank", "broad.diagnosis.grouping.used.in.prais2", "acquired.comorbidity", "additional.cardiac.risk.factors", 
                   "congenital.comorbidity", "severity.of.illness","premature","death_30d") ){
    nchda[ , numvar ] = as.numeric( nchda[ , numvar ] )
  }
  
  #any negative ages set to NA
  nchda$raop[ which(nchda$raop<0) ] = NA #0
  summary( nchda$raop )
  nchda$raodis[ which(nchda$raodis<0) ] = NA #2+20
  summary( nchda$raodis )
  
  #any negative or null weights set to NA
  nchda$x2.03.weight[ which(nchda$x2.03.weight<=0) ] = NA #3
  summary( nchda$x2.03.weight )
  
  # removing records with missing ages or weights
  todelete = which( is.na( nchda$raop ) | is.na( nchda$raodis ) | is.na( nchda$x2.03.weight ) )
  nchda <- nchda[ - todelete, ]
  row.names( nchda ) <- NULL

# 5. building 30-day episodes
  #5.1 Split the data by patient ID; We only need ages and patient IDs to create spells
  data_order = with( nchda, order( championpatid, raop, raodis, finyr,launchesrecid))
  nchda <- nchda[ data_order, ]
  row.names( nchda ) <- NULL
  pat_data_split <- split( nchda[ , c("championpatid","launchesrecid","raop","raodis")], nchda$championpatid )
  
  #5.2 We identify new spells by flagging the events that occur later than 30 days
  spell_fun <- function( data ){
    
    patentry = 1:nrow(data)
    
    flag = rep(0, nrow(data))
    max_age = rep(0, nrow(data))
    spellentry = rep(0, nrow(data))
    
    flag[1] = 1
    max_age[1] = max( data$raop[1], data$raodis[1], na.rm=T )
    spellentry[1] = 1
    
    if( nrow(data) > 1 ){
      for( i in 2:nrow(data) ){
        if( data$raop[i] <= max_age[i-1] + 30/365.25 ){
          flag[ i ] = 0
          max_age[ i ] = max( max_age[ i-1 ], data$raop[i], na.rm=T )
          max_age[ i ] = max( max_age[ i ],   data$raodis[i], na.rm=T )
          spellentry[ i ] = spellentry[ i-1 ] + 1
        } else {
          flag[ i ] = 1
          max_age[i] = max( data$raop[i], data$raodis[i], na.rm=T )
          spellentry[ i ] = 1
        }
      }
    }
    
    spellid = cumsum( flag )
    
    data.frame( patentry, flag, max_age, spellid, spellentry, stringsAsFactors = FALSE )
  }
  
  aux <- lapply( pat_data_split, spell_fun )
  rm( pat_data_split)
  rm( spell_fun )
  gc()
  
  aux2 <- do.call( rbind.data.frame, aux )
  rm( aux )
  gc()
  
  nchda <- cbind( nchda, aux2[ , -1 ] )
  row.names( nchda ) <- NULL
  rm( aux2 )
  gc()
  
  #5.3. we keep only the first record in 30-day episodes
  nchda <- nchda[ which( nchda$spellentry ==1 ), ]
  row.names( nchda ) <- NULL
  
  gc()

# 6. Outcome
  # We assigned alive life status at 30 days to records with missing ONS life status and NCHDA discharge alive before 30 days
  table( nchda$death_30d, useNA="always" )
  nchda$death_30d[ is.na( nchda$death_30d ) ] = 0

# 7. PRAIS Procedure groups  
  #7.1. Deriving a numeric specific procedure
  nchda$specproc <- as.numeric( substr( nchda$sp_allocation, 1, 2) )
  for( i in 22:32 ){
    sel = grep( i, nchda$sp_allocation )
    if( length(sel) >= 1 ){
      nchda$specproc[ sel ] = i
    }
  }
  table( nchda$specproc, useNA = "always")
  
  #7.2. SP groups
  nchda <- merge( nchda, prais_proc_map, by="specproc")
  rm( prais_proc_map )
  gc()
  ## Hybrid HLHS procedures are in group 1
  #“122020. Hypoplastic left heart syndrome hybrid approach (transcatheter and surgery) Stage 1”, 
  #“122021. Hypoplastic left heart syndrome hybrid approach (transcatheter & surgery)”, 
  #“121004. Application of bilateral pulmonary arterial bands & transcatheter placement of stent in arterial duct”; 
  #or if a record had the two codes “121014. Stent placement in arterial duct (PDA)” and “121419. Application of right & left pulmonary arterial bands”"
  isHLHS = grepl( "122020|122021|121004", nchda$allprocs )
  isHLHS = isHLHS | ( grepl( "121014", nchda$allprocs ) & grepl( "121419", nchda$allprocs ) )
  nchda[ which( isHLHS & (nchda$x3.07.proceduretype == "7. hybrid") ), "specproc_group" ] = "01"
  ## Tracheal procedures are in group 14
  nchda[ grep( "126420|126440", nchda$allprocs ), "specproc_group" ] = "14"
  table( nchda$specproc_group, useNA = "always" )
  # just checking that hybrid procedures go to either of groups (01,10,12,14,20) [coding to improve over time]
  table( nchda[ which( nchda$x3.07.proceduretype == "7. hybrid" ), "specproc_group" ] )
  table( nchda$specproc_group, useNA = "always")
  
# 8. PRAIS DIAGNOSIS and COMORBIDITY GROUPS 
  
  # missing/no diagnoses go to group 5
  sum( nchda$toprank >= 27 ) #86 records
  nchda$broad.diagnosis.grouping.used.in.prais2[ nchda$toprank >= 27 ] = 5
  
  table( nchda$toprank,
         nchda$broad.diagnosis.grouping.used.in.prais2 , useNA = "always" )
  table( nchda$congenital.comorbidity, useNA = "always" )
  table( nchda$acquired.comorbidity, useNA = "always" )
  table( nchda$additional.cardiac.risk.factors, useNA = "always" )
  table( nchda$severity.of.illness, useNA = "always" )
  table( nchda$premature, useNA = "always" )
  
# 9. FUVH flag
  nchda$fUVHflag = 0
  for( code in prais_fuvh_map$code ){
    code_found = grepl( code, nchda$allcodes )
    nchda$fUVHflag[ code_found ] = 1
  }
  rm( prais_fuvh_map )
  gc()
  table( nchda$fUVHflag, useNA = "always" ) #386 to delete 
  
  
# 10. We save the processed data for the PRAIS model
  write.csv( x = nchda, file=out_file_namePRAISdata, row.names = FALSE )
  gc()  
  
  