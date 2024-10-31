# 03.02 CREATING SPELLS OF CARE

## system clearing (comment out if you want to keep previous work)
  rm( list=ls()) #remove any previous R object
  gc()           #garbage collection of memory; It can be useful to call gc after a large object has been removed, as this may prompt R to return memory to the operating system.

#PARAMETERS: 
## input data: files processed at record level (files were generated in Step 02)
  in_file_name <- "S://CHAMPION/Analysis/Results/CHD code curation/patient_level_linked_data.csv"

  spell_gap_in_years = 1.0 / 365.25 # up one day of gap allowed to consider to consecutive episodes part of the same spell 

  out_file_name <- "S://CHAMPION/Analysis/Results/CHD code curation/patient_level_linked_data_spells.csv"
  

# 1. We load the patient level data (all linked datasets together)
  ## the input file exists?
  file_exists = ifelse( file.exists( in_file_name ), TRUE, FALSE )
  if( !file_exists ) 
    stop( paste0("ERROR: cannot find the file ", in_file_name, "; please revise the source directory and file name" ) )
  rm( file_exists )

  pat_data <- readr::read_csv( in_file_name , col_types = "c" )

  pat_data$ageatstart = as.numeric( pat_data$ageatstart )
  pat_data$ageatdis   = as.numeric( pat_data$ageatdis )
  
# 2. We remove outpatient cancelled and non-attended appointments (these can be added later on and are a big load that is not service utilisation)  
  table( pat_data$attended )
  # 2 = Appointment cancelled by, or on behalf of, the patient
  # 3 = Did not attend - no advance warning given 
  # 4 = Appointment cancelled or postponed by the Health Care Provider 
  # 5 = Seen, having attended on time or, if late, before the relevant care professional was ready to see the patient 
  # 6 = Arrived late, after the relevant care professional was ready to see the patient, but was seen 
  # 7 = Did not attend - patient arrived late and could not be seen 
  # 9 = Not known
  to_delete = pat_data$attended %in% c(2,3,4,7)
  sum( to_delete ) #997541
  pat_data <- pat_data[ -which(to_delete), ]
  rm( to_delete )
  gc()

  table( pat_data$atentype )    
  # 1 = Attended first appointment
  # 2 = Attended subsequent appointment
  # 3 = Attended but first/subsequent/tele unknown
  # 13 = Not known
  # 21 = Attended first tele consultation (from 2008-09)
  # 22 = Attended subsequent tele consultation (from 2008-09)
  
# 3. WARNING: Diagnostic procedures were only recorded in NCHDA from 2015 onwards. Hence if using pre-2015 years it is adviced to remove those.
  table( pat_data$finyr, pat_data$x3.07.proceduretype ) # if you spot any year with no data on some procedure type, then you need to adjust for that
  
# 4. Drop records with missing ages (we need ages/dates to create spells)
  to_delete = is.na(pat_data$ageatstart) 
  sum( to_delete ) #113
  table( pat_data$dataSOURCE[ to_delete ] ) # few PICANet and APC records, but no NCHDA records
  pat_data <- pat_data[ -which(to_delete), ]
  rm( to_delete )
  gc()

  #what about negative ages? we drop those ages below 1 day of age
  to_delete = pat_data$ageatstart < 0 - spell_gap_in_years
  sum( to_delete ) #55
  table( pat_data$dataSOURCE[ to_delete ] ) # few PICANet and APC and OP records, but also some NCHDA records
  table( pat_data$finyr[ to_delete ], pat_data$x3.07.proceduretype[ to_delete ], useNA = "always" )
  pat_data <- pat_data[ -which(to_delete), ]
  rm( to_delete )
  gc()
  
# 5. Spell creation
  #5.1 Split the data by patient ID; We only need ages and patient IDs to create spells
  pat_data_split <- split( pat_data[ , c("championpatid","patentry","ageatstart","ageatdis")], pat_data$championpatid )
  
  #5.3 We identify new spells by flagging the events that occur later than the allowed time gap
  spell_fun <- function( data ){

    flag = rep(0, nrow(data))
    max_age = rep(0, nrow(data))
    spellentry = rep(0, nrow(data))
    
    flag[1] = 1
    max_age[1] = max( data$ageatstart[1], data$ageatdis[1], na.rm=T )
    spellentry[1] = 1
    
    if( nrow(data) > 1 ){
      for( i in 2:nrow(data) ){
        if( data$ageatstart[i] <= max_age[i-1] + spell_gap_in_years ){
          flag[ i ] = 0
          max_age[ i ] = max( max_age[ i-1 ], data$ageatstart[i], na.rm=T )
          max_age[ i ] = max( max_age[ i ],   data$ageatdis[i], na.rm=T )
          spellentry[ i ] = spellentry[ i-1 ] + 1
        } else {
          flag[ i ] = 1
          max_age[i] = max( data$ageatstart[i], data$ageatdis[i], na.rm=T )
          spellentry[ i ] = 1
        }
      }
    }
    
    patentry=data$patentry
    
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
  
  pat_data <- cbind( pat_data, aux2[ , -1 ] )
  row.names( pat_data ) <- NULL
  rm( aux2 )
  gc()
          
# 6. Spell level definitions
  #6.1 Spell is Cardiac
  # we revise the record level flag for cardiac
  table( pat_data$dataSOURCE, pat_data$iscardiac, useNA = "always")
    # we consider any NCHDA record as cardiac
  pat_data[ pat_data$dataSOURCE=="01.NCHDA", "iscardiac" ] = 1
    # for APC records, I had only updated the iscardiac flag for cardiac records (i.e. missing is the same as non cardiac)
  pat_data[ pat_data$dataSOURCE=="04.APC" & is.na(pat_data$iscardiac), "iscardiac" ] = 0
  table( pat_data$dataSOURCE, pat_data$iscardiac, useNA = "always")
  # we then define take the max of cardiac flags for each spell
  iscardiac_spell <- aggregate( pat_data$iscardiac, list(pat_data$championpatid,pat_data$spellid), max, na.rm=FALSE )
  colnames( iscardiac_spell ) <- c("championpatid","spellid","spellcardiac")
  aux = merge( pat_data, iscardiac_spell, by=c("championpatid","spellid"), all=TRUE )
  pat_data <- aux
  rm( aux )
  rm( iscardiac_spell )
  gc()

  pat_data <- pat_data[ with(pat_data, order(championpatid,patentry) ),  ]
  
  #View( pat_data[ , c("championpatid","spellid","spellentry","ageatstart","ageatdis","dataSOURCE","epiorder","iscardiac","spellcardiac")])
  table( pat_data$spellcardiac[ pat_data$spellentry == 1 ] )

  #6.2 SpellNCHDA, SpellAPC, etc
  
  ## for further spell-level definitions we use a data.table approach
  library( data.table )
  # we convert to data table the subset of fields needed for the definition
  aux = data.table( pat_data[ , c("championpatid","spellid","spellentry","dataSOURCE") ] )

  # we implement the definitions at (patient,spell) level
  aux[ , `:=` (SpellhasNCHDA = max( dataSOURCE=="01.NCHDA" ),
               SpellhasAPC   = max( dataSOURCE=="04.APC" ),
               SpellhasOP    = max( dataSOURCE=="05.OP" )

    # ... (add definitions of interest here)
    # SpellhasPICANet     = max( dataSOURCE=="02.PICANet" )
    # SpellAPCnodaycase   = SpellhasAPC & max( data$classpat != 2 ) #for this, you need to add "CLASSPAT" when processing HES APC
    # SpellhasAE          = max( dataSOURCE=="06.AE" )

              ), by= .(championpatid,spellid) ]
  
  # definitions depending on the previous ones
  # e.g. spells with OP excluding the spells with APC
  aux$SpellhasOPnoAPC = pmin( aux$SpellhasOP, 1 - aux$SpellhasAPC )
  
  pat_data <- cbind( pat_data, aux[ , -(1:4) ] )
  row.names( pat_data ) <- NULL
  rm( aux )
  gc()
  
  # linkage of NCHDA to APC gave quite good results (see linkage paper for further analysis)
  with( pat_data, table( SpellhasNCHDA[spellentry==1], SpellhasAPC[spellentry==1] ) )
  gc()
        
# 5. Saving the data into file (we adopt the data table approach again)
  setDT(pat_data)
  fwrite( x = pat_data, file=out_file_name, row.names = FALSE )
gc()  
