# 02.04 PICANet DATA CLEANING

## system clearing (comment out if you want to keep previous work)
  rm( list=ls()) #remove any previous R object
  gc()           #garbage collection of memory; It can be useful to call gc after a large object has been removed, as this may prompt R to return memory to the operating system.

#PARAMETERS:
## file_name, original data files
  in_file_name_admission <- "S://CHAMPION/Data/PICANet Data/LaunchesSent20191025v2_AdmissionData.txt" 
  in_file_name_clinical <- "S://CHAMPION/Data/PICANet Data/LaunchesSent20191025v2_ClinicalData.txt" 
  in_file_name_interventions <- "S://CHAMPION/Data/PICANet Data/LaunchesSent20191025v2_InterventionsData.txt" 
  
  ## out_file_name, output data file
  out_file_name <- "S://CHAMPION/Analysis/Results/CHD code curation/picanet_clean_data.csv"

# 1. We load the actual data

  ## the input files exist?
  for( in_file_name in c(in_file_name_admission,in_file_name_clinical,in_file_name_interventions) ) {  
    file_exists = ifelse( file.exists( in_file_name ), TRUE, FALSE )
    if( !file_exists ) 
      stop( paste0("ERROR: cannot find the file ", in_file_name, "; please revise the source directory and file name" ) )
    rm( file_exists )
  }
  
  ## we load the data flies as text
  picanet_admission <- read.csv( in_file_name_admission, colClasses="character", sep = "\t", encoding = "ISO-8859-2" )
  picanet_clinical <- read.csv( in_file_name_clinical, colClasses="character", sep = "\t", encoding = "ISO-8859-2" )
  picanet_interventions <- read.csv( in_file_name_interventions, colClasses="character", sep = "\t", encoding = "ISO-8859-2" )
  
  ## we merge admission and interventions data
  picanet = merge( x = picanet_admission, y = picanet_interventions, by="PICANetRecordID", all = TRUE )
  rm( picanet_admission )
  rm( picanet_interventions )
  
#2. same recordID has multiple entries in clinical data; we ranked those (we defined a counter) but in the end only kept primary diagnosis
  picanet_clinical = picanet_clinical[ with( picanet_clinical, order(PICANetRecordID,ClinicalCodeType,Diagnosis)), ]
  row.names( picanet_clinical ) <- NULL
  picanet_clinical$roworder = as.numeric( row.names( picanet_clinical ) )
  picanet_clinical = transform( picanet_clinical, num_times_recordID = ave( roworder, PICANetRecordID, FUN = function(x) order(x, decreasing = FALSE ) ) )

  #   # we reshape the data from long to wide (DANGER: this results in 121 variables)
  # picanet_clinical_wide = reshape( data = picanet_clinical, idvar = "PICANetRecordID", timevar = "num_times_recordID", direction = "wide" )
  #   # DANGER: this increases the number of fields from 63 to 183
  # aux = merge( x=picanet, y=picanet_clinical_wide, by = "PICANetRecordID", all.x = TRUE )

  ## We just keep the primary diagnosis code and group
  table( picanet_clinical$ClinicalCodeType )
  # 1     2     3     6 
  # 65707 71181 49011 49047
  picanet_clinical <- picanet_clinical[ picanet_clinical$ClinicalCodeType==1, ]
  picanet_clinical[ , c("roworder", "num_times_recordID","ClinicalCodeType") ] <- NULL
  table( picanet_clinical$DiagnosticGroup ) #51464 Cardiovascular, 296 congenital

#3. Final data: we merge all files and tidy format
  aux = merge( x=picanet, y=picanet_clinical, by = "PICANetRecordID", all.x = TRUE )
  setdiff( names(aux), names(picanet) )
  picanet <- aux
  rm( picanet_clinical, aux )

  # low case field names
  names( picanet ) <- tolower( names( picanet ) ) 
      
## then we convert to numeric the numeric fields (Gest, Mult, PIMs, Ages, days, totals)
  numeric_fields <- names(picanet) [ grepl( "gest|mult|pim|age|day|total", names(picanet), ignore.case = T) ]
  picanet[ , numeric_fields ] <- sapply( numeric_fields, function(x) as.numeric(picanet[,x]) )

  ## tidy auxiliary variables
  rm( numeric_fields )  
  gc( )

# 4. Saving the clean data into file
write.csv( picanet, file=out_file_name, row.names = FALSE )
