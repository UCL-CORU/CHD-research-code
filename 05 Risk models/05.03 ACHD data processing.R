# 05.03 ACHD DATA PROCESSING

# The ACHD risk models require:
# inclusion/exclusion of procedures
# 30-day and 90-day episodes (overlapping episodes not allowed) 
# defining risk factors

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
  # NCHDA diagnostic and comorbidity categories (adult lookups were used for ages 18+)
  in_file_name_toprank_comorb <- "S://CHAMPION/Analysis/Results/CHD code curation/nchda_diagnostic_and_comorbidity_categories.csv"
  # mortality outcomes
  in_file_name_mortality <- "S://CHAMPION/Analysis/Results/CHD code curation/nchda_mortality_outcomes.csv"
  # ACHD composite complications outcome
  in_file_name_complications <- "S://CHAMPION/Analysis/Results/CHD code curation/nchda_composite_complications.csv"
  # ACHD mortality broad procedure groups
  in_file_name_broad_proc_mortality     <- "S://CHAMPION/Analysis/R script files/CHD code curation/07 Risk models/mapping files/ACHD_broad_procedure_groups_mortality.txt"
  # ACHD complications broad procedure groups
  in_file_name_broad_proc_complications <- "S://CHAMPION/Analysis/R script files/CHD code curation/07 Risk models/mapping files/ACHD_broad_procedure_groups_complications.txt"
  # ACHD ranking of codes 
  in_file_nameACHD <- "S://CHAMPION/Analysis/R script files/CHD code curation/04 Diagnostic and comorbidity mappings/mapping files/AdultCHD_all_codes_ranking.txt"
  # ACHD included procedures
  in_file_name_includedACHD <- "S://CHAMPION/Analysis/R script files/CHD code curation/07 Risk models/mapping files/ACHD_included_procedures.txt"
  # ACHD excluded catheters for counting ACHD codes
  in_file_name_excludedACHD_cath <- "S://CHAMPION/Analysis/R script files/CHD code curation/07 Risk models/mapping files/ACHD_excluded_codes_diagnostic_catheter.txt"
  # ACHD excluded surgeries for counting ACHD codes
  in_file_name_excludedACHD_surg <- "S://CHAMPION/Analysis/R script files/CHD code curation/07 Risk models/mapping files/ACHD_excluded_codes_surgical.txt"
  
    
  #Processed data for ACHD
  out_file_nameACHDdata <- "S://CHAMPION/Analysis/Results/CHD code curation/nchda_achd_data.csv"

# 1. We load the processed nchda and the outcomes data
  ## the input files exist?
  for( file_name in c(in_file_name_nchda, in_file_name_sp, in_file_name_mortality, in_file_name_complications, in_file_name_toprank_comorb, in_file_name_broad_proc_mortality, in_file_name_broad_proc_complications, in_file_nameACHD) ) {  
    file_exists = ifelse( file.exists( file_name ), TRUE, FALSE )
    if( !file_exists ) 
      stop( paste0("ERROR: cannot find the file ", file_name, "; please revise the source directory and file name" ) )
    rm( file_exists )
  }
  
  nchda_data       <- read.csv( in_file_name_nchda,          colClasses = "character", stringsAsFactors = FALSE )
  nchda_sp         <- read.csv( in_file_name_sp,             colClasses = "character", stringsAsFactors = FALSE )
  nchda_toprank    <- read.csv( in_file_name_toprank_comorb, colClasses = "character", stringsAsFactors = FALSE )
  nchda_dead30d90d <- read.csv( in_file_name_mortality,      colClasses = "character", stringsAsFactors = FALSE )
  nchda_complic30d <- read.csv( in_file_name_complications,  colClasses = "character", stringsAsFactors = FALSE )
  
  achd_mortality_proc_map <- read.csv( in_file_name_broad_proc_mortality, colClasses="character", sep = "\t", encoding = "ISO-8859-2" )
  achd_complics_proc_map <- read.csv( in_file_name_broad_proc_complications, colClasses="character", sep = "\t", encoding = "ISO-8859-2" )
  achd_full_map <- read.csv( in_file_nameACHD, colClasses="character", sep = "\t", encoding = "ISO-8859-2" )
  achd_inclusion_map <- read.csv( in_file_name_includedACHD, colClasses="character", sep = "\t", encoding = "ISO-8859-2" )
  
  ## we process the procedure maps
  
  process_proc_mappings <- function( x ){
    print( names( x ) )
    x$specproc = as.numeric( substr( x$NumSpecProc_8_05, 1, 2 ) )
    x <- x[ !is.na(x$specproc ), ]
    row.names( x ) <- NULL
    x$specproc_group = substr( x$Specific.Procedure.Grouping, 20, 21 )
    x <- x[ , c( "specproc","specproc_group" ) ]
    return( x )
  }
  
  achd_mortality_proc_map <- process_proc_mappings( achd_mortality_proc_map )
  print( achd_mortality_proc_map )
  
  achd_complics_proc_map  <- process_proc_mappings( achd_complics_proc_map )
  print( achd_complics_proc_map )
  
  rm( process_proc_mappings )
  gc()

  # we need to tidy the nchda sp_allocation (unallocated valve procedures need to be reallocated)
  unallocated_to_change = grepl( "SP", nchda_sp$sp_allocation )
  table( nchda_sp$sp_allocation[ unallocated_to_change ] )
  table( substr(nchda_sp$sp_allocation, 18, 19)[ unallocated_to_change ] )
  nchda_sp$specproc = substr(nchda_sp$sp_allocation, 1, 2)
  nchda_sp$specproc[ unallocated_to_change ] = substr(nchda_sp$sp_allocation, 18, 19)[ unallocated_to_change ]
  rm( unallocated_to_change )
  table( nchda_sp$specproc )
  
  # 2. We merge the nchda files

  nchda <- merge( nchda_data, nchda_sp, by = c("CHAMPIONpatID","LAUNCHESrecID") )
  nchda <- merge( nchda, nchda_toprank, by = c("CHAMPIONpatID","LAUNCHESrecID") )
  names( nchda ) <- tolower( names( nchda ) )
  nchda <- merge( nchda, nchda_dead30d90d, by = c("championpatid","launchesrecid") )
  nchda <- merge( nchda, nchda_complic30d, by = c("championpatid","launchesrecid") )
  
  rm( nchda_data, nchda_sp, nchda_toprank, nchda_dead30d90d, nchda_complic30d )
  gc( )    
  
  nchda <- merge( nchda, achd_mortality_proc_map, by= c("specproc"), all.x = TRUE )
  nchda <- merge( nchda, achd_complics_proc_map,  by= c("specproc"), all.x = TRUE )
  rm( achd_mortality_proc_map, achd_complics_proc_map )
  names( nchda )[ names( nchda ) == "specproc_group.x" ] <- "spgrp_death"
  names( nchda )[ names( nchda ) == "specproc_group.y" ] <- "spgrp_compl"
  # we need to use allproccodes as part of the inclusion/exclusion, and to refine the spec proc groups
  nchda$allprocs = do.call( paste, c( nchda[ grep("^proccode", names(nchda)) ], sep="_" ) )
  
  #we tidy the complications outcome
  nchda$complics_30d = 1*(nchda$any_complication==TRUE)
  nchda$complics_30d[ nchda$any_complication == "" ] = NA
  table( nchda$any_complication, nchda$complics_30d, useNA = "always" )
  
  gc()
      
# 3. ACHD Inclusion/exclusion: keep only adult surgeries, etc. and format the numeric fields
  
  #3.1. keeping only adults
  nchda$raop = as.numeric( nchda$raop )
  nchda <- nchda[ nchda$raop >= 18.0, ]
  row.names( nchda ) <- NULL
  
  #3.2 only financial years from 2015 onwards
  nchda$finyr <- as.numeric( nchda$finyr )
  tokeep = which( nchda$finyr >= 2015 )
  nchda <- nchda[ tokeep, ]
  row.names( nchda ) <- NULL
  
  #3.3 keeping only bypass/non-bypass
  tokeep = which( nchda$type_procedure %in% c("1", "2", "1. bypass", "2. non-bypass") )
  table( nchda$x3.07.proceduretype[ tokeep ], useNA = "always" )
  table( nchda$type_procedure[ tokeep ], useNA = "always" )
  nchda <- nchda[ tokeep, ]
  row.names( nchda ) <- NULL
  rm( tokeep )
  gc()

  #3.4 We exclude transplants
  todelete = which( nchda$specproc %in% c("02:heart_transplant","03:lung_transplant","02","03") )
  nchda <- nchda[ - todelete, ]
  row.names( nchda ) <- NULL
  table( nchda$sp_allocation, useNA = "always" )

  #3.5. We exclude records having no code in group 27 (all allowed Procedure codes)
  # we first load the ACHD mapping of codes to groups
  #achd_full_map <- read.csv( in_file_nameACHD, colClasses="character", sep = "\t", encoding = "ISO-8859-2" )
  # we keep only the codes in group 27
  achd_map <- achd_full_map[ which( achd_full_map$NewDiagnosisGroupRank=="27" ), ] # 316 procedure codes included in PRAIS
  # we then keep only records in that list of codes
  nchda$hasproc27 = 0
  for( code in achd_map$code ){
    code_found = grepl( code, nchda$allprocs )
    nchda$hasproc27[ code_found ] = 1
  }
  table( nchda$hasproc27, useNA = "always" ) #few to delete 
  table( nchda$sp_allocation[ nchda$hasproc27 == 0], useNA = "always" ) # 103 with no qualifying codes; rest being mostly unallocated or ep_miscellaneous)
  nchda <- nchda[ - which( nchda$hasproc27 == 0 ), ]
  nchda$hasproc27 <- NULL
  rm( achd_map )
  row.names( nchda ) <- NULL
  gc()
  
  #3.6 excluding cath procedure codes (none found)
  # 121381. Transluminal aortic valvar insertion with stent mounted valve
  # 121351. Transluminal pulmonary valvar insertion with stent mounted valve 
  # 120106. Atrial septal defect (ASD) secundum closure with transluminal device
  # These codes should not be found in isolation:
  # 120153. PFO direct closure
  # 123203. Exploratory procedure to assess heart or PFO
  # 128701. Cardiac support procedure
  nchda$hasExludingCath = grepl( "121351|121381|120106", nchda$allprocs )
  table( nchda$hasExludingCath, useNA = "always" )
  # I also exclude records with only procedure codes being "120153 PFO direct closure"
  nchda$hasExcludingIsolated = FALSE
  for( code in c("120153", "123203", "128701" ) ){
    nchda$hasExcludingIsolated = grepl( code, nchda$allprocs ) & !grepl( "[0-9]", gsub( code, "", nchda$allprocs ) )  
  }
  table( nchda$hasExcludingIsolated, useNA = "always" )
  if( sum(nchda$hasExludingCath) + sum(  nchda$hasExcludingIsolated )){
    nchda <- nchda[ - which(nchda$hasExludingCath | nchda$hasExcludingIsolated ), ]
    row.names( nchda ) <- NULL
  }
  
  #3.7. SPEC PROC and AGE and BYPASS FILTERING
   #we process the lookup for use: we extract spec proc and proc codes from long description fields
   achd_inclusion_map$specproc = substr(achd_inclusion_map$Specific.Procedure.v8.05, 1, 2)
   achd_inclusion_map$proccode = substr(achd_inclusion_map$Procedure.code, 1, 6)
   achd_inclusion_map <- achd_inclusion_map[ - grep("Specific.Procedure.v8.05|Procedure.code", names(achd_inclusion_map))]
   nchda$inclusion = 0
   row.names( nchda ) <- NULL
   for( i in 1:nrow(achd_inclusion_map) ){
     code = achd_inclusion_map[ i, ]
     #- spec proc found or spec proc unallocated and proc code found
     code_found = ( nchda$specproc == code$specproc ) | ( (nchda$specproc == 99) & grepl(code$proccode,nchda$allprocs) ) 
     #- Age filter
     code_found = code_found & ( nchda$raop < ifelse(code$Ages.below.40 == 1, 40, 120) )
     #- Bypass filter
     code_found = code_found & ( nchda$type_procedure <= ifelse(code$Non.bypass == 1, 2 , 1 ) )
     
     nchda$inclusion[ which( code_found ) ] <- 1
   }
  table(nchda$inclusion )
  table( nchda$specproc[which(nchda$inclusion==1)] ) #this nearly matches Table 1 in the paper (episodes creation needed)
  
  rm( code, code_found )
  
  nchda <- nchda[ which(nchda$inclusion==1), ]
  row.names( nchda ) <- NULL
  
  # 4 Deriving new fields
  
  # 4.1. Complex procedure (we need to use p1 to p9, excluding Tables S3 and S4)
  
  names(nchda)[ grep("^p[0-9]", names(nchda)) ] #starting procedure codes
  nchda$allprocs = do.call( paste, c( nchda[ grep("^p[0-9]", names(nchda)) ], sep=" " ) )
  table( nchar( gsub( " ", "", nchda$allprocs ) ) / 6 ) # number of procedure codes
  
  # removing diagnostic catheter codes 
  excluded <- read.csv( in_file_name_excludedACHD_cath, colClasses="character", sep = "\t", encoding = "ISO-8859-2" ) 
  aux = gsub( paste0( excluded$code, collapse = "|" ), "", nchda$allprocs )
  # removing non-ACHD surgical codes
  excluded <- read.csv( in_file_name_excludedACHD_surg, colClasses="character", sep = "\t", encoding = "ISO-8859-2" ) 
  aux = gsub( paste0( excluded$code, collapse = "|" ), "", aux )
  # number of ACHD codes
  nchda$numACHDproccodes = nchar( gsub( " ", "", aux ) ) / 6
  
  table( nchda$numACHDproccodes ) # number of procedure codes
  
  #WARNING: THIS IS HARD-CODED; IT WOULD BE BETTER TO PLACE THE BINARISATION DEFINITION IN A LOOKUP FILE
  nchda$proc_complexity = ifelse( nchda$numACHDproccodes >=4, 1, 0 )
  
  table( nchda$numACHDproccodes, nchda$proc_complexity )
  table( nchda$proc_complexity, nchda$death_30d, useNA = "always" )
  
  # 4.2. Non-elective procedure
  table( nchda$procedureurgency, useNA = "always" )
  
  nchda$non_elective = 1
  nchda$non_elective[ nchda$procedureurgency=="1. Elective" ] = 0
  nchda$non_elective[ is.na(nchda$procedureurgency) ] = NA
  
  table( nchda$procedureurgency, nchda$non_elective, useNA = "always" )
  table( nchda$non_elective, nchda$death_30d, useNA = "always" )
  
  # 4.3 Patient complexity (ESC guidelines)
  #we will use diagnosis and comorbidity codes
  nchda$alldiags = paste( 
    do.call( paste, c( nchda[ grep("^d[0-9]", names(nchda)) ], sep=" " ) ),
    do.call( paste, c( nchda[ grep("^c[0-9]", names(nchda)) ], sep=" " ) )
  )
  severe_sp   = c(1,5,6,7,9,10,12,16,17,33,34,45,46)
  severe_diag  = c(1,2,3,4,5,7,8)
  moderate_sp = c(13,15,19,20,21,22,23,24,25,26,27,28,29,30,31,32,35,36,37,38,39,40,42,43,47,48,56,57,81,82,84)
  moderate_diag = c(6,10,11,12,13,14,15,16,17,18,19)
  #mild_sp = c(20,21,22,24)
  #mild_diag 
  
  # we extract the codes in severity or moderate diagnosis groups
  achd_map <- achd_full_map[ which( achd_full_map$NewDiagnosisGroupRank %in% severe_diag ), ]
  severe_diag_codes = achd_map$code
  achd_map <- achd_full_map[ which( achd_full_map$NewDiagnosisGroupRank %in% moderate_diag ), ]
  moderate_diag_codes = achd_map$code
  
  nchda$ESCcomplexity = 1 # mild
  # moderate
  nchda$ESCcomplexity[ as.numeric(nchda$specproc) %in% moderate_sp ] = 2
   #we need to search for codes in the unallocated (is_21==1 | is_22==1 | is_25==1 | is_26==1 | is_27==1 | is_28==1 | is_29==1 | is_30==1 | is_31==1 | is_32==1 | is_37==1 | is_47==1 )
  search = paste0( achd_inclusion_map[ which(achd_inclusion_map$specproc %in% moderate_sp), "proccode" ], collapse= "|" )
  aux = which( grepl(search, nchda$allprocs) & (nchda$specproc==99) )
  nchda$ESCcomplexity[ aux ] = 2
   #diagnosis codes
  aux = grep( paste0( moderate_diag_codes, collapse = "|"), nchda$alldiags )
  nchda$ESCcomplexity[ aux ] = 2
  # severe
  nchda$ESCcomplexity[ as.numeric(nchda$specproc) %in% severe_sp ] = 3
  aux = grep( paste0( severe_diag_codes, collapse = "|"), nchda$alldiags )
  nchda$ESCcomplexity[ aux ] = 3
  
  table( nchda$specproc, nchda$ESCcomplexity )
  table( nchda$ESCcomplexity, nchda$death_30d  )
  
  # 4.4 At least third sternotomy (use x3.08.sternotomy.sequence)
  table( nchda$x3.08.sternotomy.sequence, useNA = "always" )
  nchda$sternseq = as.numeric(substr(nchda$x3.08.sternotomy.sequence,1,1))
  table( nchda$sternseq, nchda$death_30d, useNA = "always" )
  nchda$sternseq[ nchda$sternseq <  3 ] = 0
  nchda$sternseq[ nchda$sternseq >= 3 ] = 1
  table( nchda$sternseq, nchda$death_30d, useNA = "always" )
  
  # 4.5 NYHA Physical activity limitations (use nchda$x6.01.pre.proc.nyha.status)
  table( nchda$x6.01.pre.proc.nyha.status, useNA = "always" )
  nchda$preprocnyhastatus_trans = substr( nchda$x6.01.pre.proc.nyha.status, 1, 1 )
  nchda$preprocnyhastatus_trans[ is.na(nchda$preprocnyhastatus_trans) ] = 1 #we attribute symptoms to those with missing value (after looking at data with cardiologists)
  # "1. No limitation" "2. Slight limiatation" vs "3. Marked limitation" "4. Symptoms at rest/minimal"
  nchda$preprocnyhastatus_trans[ nchda$preprocnyhastatus_trans < 3 ] = 0
  nchda$preprocnyhastatus_trans[ nchda$preprocnyhastatus_trans >= 3 ] = 1
  
  table( nchda$preprocnyhastatus_trans, nchda$death_30d, useNA = "always" )
  
  # 4.6. Poor ventricular ejection fraction (use "preprocsvef", "preprocspvef" )
  table( nchda$preprocspvef )
  table( nchda$preprocsvef )
  nchda$combined_evf_trans = pmax( as.numeric(substr(nchda$preprocspvef,1,1)),
                                  as.numeric(substr(nchda$preprocsvef,1,1)), na.rm = T ) #values 1,2,3
  table( nchda$combined_evf_trans, useNA = "always" )
  nchda$combined_evf_trans[ nchda$combined_evf_trans < 3 ] = 0
  nchda$combined_evf_trans[ nchda$combined_evf_trans >= 3 ] = 1
  nchda$combined_evf_trans[ is.na(nchda$combined_evf_trans) ] = 1 #decided after looking at data with cardiologists
  table( nchda$combined_evf_trans, nchda$death_30d, useNA = "always" )
  
  #5 selecting only the model fields and formatting fields
  
  names( nchda ) <- tolower( names( nchda ) )
  
  
  thenames = c("raop","raodis", "spgrp_death", "spgrp_compl", 
               "proc_complexity", "non_elective", "esccomplexity", "sternseq", "preprocnyhastatus_trans", "combined_evf_trans",
               "congenital.comorbidity", "acquired.comorbidity", "additional.cardiac.risk.factors",
               "death_30d", "death_90d", "complics_30d") #we will need orgid as well 
  thenames[ ! thenames %in% names( nchda ) ] ##all names in data
  
  for( numvar in thenames ){
    nchda[ , numvar ] = as.numeric( nchda[ , numvar ] )
  }
  
  thenames = c( "championpatid","launchesrecid","orgid","finyr",
               "raop","raodis", "spgrp_death", "spgrp_compl", 
               "proc_complexity", "non_elective", "esccomplexity", "sternseq", "preprocnyhastatus_trans", "combined_evf_trans",
               "congenital.comorbidity", "acquired.comorbidity", "additional.cardiac.risk.factors",
               "death_30d", "death_90d", "complics_30d") 
  thenames[ ! thenames %in% names( nchda ) ] ##all names in data
  nchda <- nchda[ , thenames]
  row.names( nchda ) <- NULL
  gc()
  
  summary( nchda$raop ) #ages above 18
  summary( nchda$raodis )
  nchda$raodis[ which(nchda$raodis<0) ] = NA #1
  summary( nchda$raodis )

  # removing records with missing ages
  todelete = which( is.na( nchda$raop ) | is.na( nchda$raodis ) ) 
  nchda <- nchda[ - todelete, ]
  row.names( nchda ) <- NULL

# 6. building 30-day episodes
  #6.1 Split the data by patient ID; We only need ages and patient IDs to create spells
  data_order = with( nchda, order( championpatid, raop, raodis, finyr,launchesrecid))
  nchda <- nchda[ data_order, ]
  row.names( nchda ) <- NULL
  pat_data_split <- split( nchda[ , c("championpatid","launchesrecid","raop","raodis")], nchda$championpatid )
  
  #6.2 We identify new spells by flagging the events that occur later than 30 days
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
        if( data$raop[i] < max_age[i-1] + 30/365.25 ){
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
  
  #6.3. we keep only the first record in 30-day episodes
  nchda <- nchda[ which( nchda$spellentry ==1 ), ] # x 30-day episodes left
  nchda$spellentry <- NULL
  row.names( nchda ) <- NULL

  #6.4 We identify new spells by flagging the events that occur later than 90 days
  pat_data_split <- split( nchda[ , c("championpatid","launchesrecid","raop","raodis")], nchda$championpatid )
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
        if( data$raop[i] < max_age[i-1] + 90/365.25 ){
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
  
  table( nchda$spellentry ) #this is a 90-day spell indicator
  nchda$spell_90d = 1*(nchda$spellentry==1)
  table( nchda$spell_90d, nchda$spellentry)  
  
  thenames = c( "championpatid","launchesrecid","orgid","spell_90d",
                "raop","raodis", "spgrp_death", "spgrp_compl", 
                "proc_complexity", "non_elective", "esccomplexity", "sternseq", "preprocnyhastatus_trans", "combined_evf_trans",
                "congenital.comorbidity", "acquired.comorbidity", "additional.cardiac.risk.factors",
                "death_30d", "death_90d", "complics_30d") 
  thenames[ ! thenames %in% names( nchda ) ] ##all names in data
  nchda <- nchda[ , thenames]
  row.names( nchda ) <- NULL
  gc()
  
  
  # we keep only the data and the name of the output file
  rm( list = setdiff( ls(), c("nchda", "out_file_nameACHDdata") ) )
  
  gc()

# 7. We save the processed data for the ACHD models
  write.csv( x = nchda, file=out_file_nameACHDdata, row.names = FALSE )
  gc()  
  
  