###################################################################################################
## NCHDA data processing - AA - SP
## last updated 2020-07-20 John Stickley

library(tidyverse)
library(lubridate)
library(purrrlyr)

#################################################
algorithm_version <- "8.05"

data_dir <- "S:/CHAMPION/Data/NCHDA Data 2023/"
filename <- "Dataset3_nolinebreak.csv"

lib_dir <- "S:/CHAMPION/Analysis/R script files/CHD code curation/02 Data cleaning/Code lists and libs/"
code_lists_filename <- "02.nchda_aa_sp_shared_codes_v8.05.R"
aa_ecmo_functions_filename <- "04.activity_analysis_algorithm_v8.03.R"
spec_proc_functions_filename <- "05.specific_procedure_algorithm_v8.05.R"

out_dir <- "S:/CHAMPION/Analysis/R Data files/"
out_filename <- paste0("nchda_processed_April_2000_March_22_", Sys.Date(), ".csv")

#################################################
##  load data from default download from Qreg5
nchda_data <- read.csv(paste0(data_dir, filename), header = TRUE, stringsAsFactors = FALSE, sep = ",", colClasses = "character") # FEP: I add reading columns as character to avoid leading zeroes from codes being removed

# In 2019-22 audit, there is data issue in the field of 2.07.Comorbid.Conditions. String '\x92s' needs to be replaced by 's

nchda_data$X2.07.Comorbid.Conditions[(which(grepl("140105", nchda_data$X2.07.Comorbid.Conditions)))] <- gsub("\x92s", "s", nchda_data$X2.07.Comorbid.Conditions[(which(grepl("140105", nchda_data$X2.07.Comorbid.Conditions)))])

# In 2018-21 audit, due to the change of extra format, we need to remove extra columns provided.

# nchda_data <- nchda_data[, -c(76, 77, 78, 79, 80, 81)]

#   nchda_data <- read.csv("nchda_2019.csv", header = TRUE, stringsAsFactors = FALSE, sep = ",")


#################################################
## source other code groupings
source(paste0(lib_dir, code_lists_filename))

## Place all diagnoses or procedures in a single variable each

for (i in 2:25) {
  if (i < 25) varname <- paste0("Diagnosis.", i)
  if (i == 25) varname <- "Diagnosis.25..to.32"
  sel <- which(nchda_data[, varname] != "")
  nchda_data$X2.01.Diagnosis[sel] <- paste0(nchda_data$X2.01.Diagnosis[sel], ";", nchda_data[sel, varname])
  nchda_data[, varname] <- NULL
}

for (i in 2:7) {
  varname <- paste0("Procedure.", i)
  sel <- which(nchda_data[, varname] != "")
  nchda_data$X3.09.Operation.Performed[sel] <- paste0(nchda_data$X3.09.Operation.Performed[sel], ";", nchda_data[sel, varname])
  nchda_data[, varname] <- NULL
}

nchda_data$dob.day <- 15
nchda_data$procedure_date <- as.Date(paste(as.character(nchda_data$dob.day), as.character(nchda_data$dob.month), as.character(nchda_data$dob.year), sep = "-"), format = "%d-%m-%Y")
nchda_data$procedure_date <- as.Date(nchda_data$procedure_date + as.numeric(nchda_data$Paop_rev) * 365.25)

nchda_data[, "X3.04.First.operator.grade"] <- nchda_data$X3.04.Operator.1.Status
nchda_data$X3.04.Operator.1.Status <- NULL
nchda_data[, "X3.06.First.assistant.grade"] <- nchda_data$X3.06.Operator2Status
nchda_data$X3.06.Operator2Status <- NULL

####################################################
####################################################
################ Data Processing v8.02 #############

## preliminary data processing - author John Stickley
## last updated:
## 2021-02-02 JS - removed 'stat_allocation' and replaced with 'fuvh'
## added previous procedures as separate codes


#################################################
## number of separated fields for diagnosis, comorbidity, previous procedure and procedure codes
diagnosis_fields <- sprintf("diagnosis_%d", seq(1:10))
comorbidity_fields <- sprintf("comorbidity_%d", seq(1:10))
prev_procedure_fields <- sprintf("prev_procedure_%d", seq(1:20))
procedure_fields <- sprintf("procedure_%d", seq(1:10))


nchda_data <- nchda_data %>%
  mutate(hospital_code = strtrim(X1.01.Hospital, 3)) %>%
  # mutate(patient_identifier = X1.02.Local.Patient.Identifier) %>%
  mutate(patient_identifier = CHAMPIONpatID) %>%
  # mutate(X1.06.Birth.date = dmy(X1.06.Birth.date)) %>%

  ## this adds 00:00 to dates with missing times
  # mutate(X3.01.Date.of.Visit = (parse_date_time(X3.01.Date.of.Visit,
  #                                               orders = c("dmy", "dmy_HM")))) %>%
  # mutate(procedure_date_time = X3.01.Date.of.Visit) %>%
  # mutate(procedure_date = as_date(X3.01.Date.of.Visit)) %>%
  # mutate(X4.01.Date.of.Discharge = dmy(X4.01.Date.of.Discharge)) %>%
  # mutate(X4.02.Date.of.Death = dmy(X4.02.Date.of.Death)) %>%
  # mutate(Record.Created.On = (parse_date_time(Record.Created.On, orders = c("dmy", "dmy_HM"),
  #                                             truncated = 3, quiet = TRUE))) %>%


  # mutate(reporting_year = (ifelse((month(X3.01.Date.of.Visit) < 4),
  #   (year(X3.01.Date.of.Visit) - 1), (year(X3.01.Date.of.Visit))
  # ))) %>%
  mutate(algorithm_version = algorithm_version) %>%
  mutate(type_procedure = (as.integer(as.character(gsub("[^Q0-9]", "", X3.07.ProcedureType))))) %>%
  mutate(type_procedure = replace_na(type_procedure, 0)) %>%
  mutate(antenatal = (as.integer(as.character(gsub("[^Q0-9]", "", X2.04.AntenatalDx))))) %>%
  mutate(antenatal = replace_na(antenatal, 9)) %>%
  # mutate(nhsnumber =  X1.03.NHS.number) %>%


  ## create new fields for the codes
  separate(X2.01.Diagnosis, diagnosis_fields, sep = ";", convert = FALSE, extra = "drop", remove = FALSE) %>%
  separate(X2.07.Comorbid.Conditions, comorbidity_fields, sep = ";", convert = FALSE, extra = "drop", remove = FALSE) %>%
  separate(X3.09.Operation.Performed, procedure_fields, sep = ";", convert = FALSE, extra = "drop", remove = FALSE) %>%
  separate(X2.02.Previous.Procedure, prev_procedure_fields, sep = ";", convert = FALSE, extra = "drop", remove = FALSE) %>%
  ## ensure any other characters are removed and only the first 6 characters are kept
  mutate(d1 = (strtrim(as.character(gsub("[^Q0-9]", "", diagnosis_1)), 6))) %>%
  mutate(d2 = (strtrim(as.character(gsub("[^Q0-9]", "", diagnosis_2)), 6))) %>%
  mutate(d3 = (strtrim(as.character(gsub("[^Q0-9]", "", diagnosis_3)), 6))) %>%
  mutate(d4 = (strtrim(as.character(gsub("[^Q0-9]", "", diagnosis_4)), 6))) %>%
  mutate(d5 = (strtrim(as.character(gsub("[^Q0-9]", "", diagnosis_5)), 6))) %>%
  mutate(d6 = (strtrim(as.character(gsub("[^Q0-9]", "", diagnosis_6)), 6))) %>%
  mutate(d7 = (strtrim(as.character(gsub("[^Q0-9]", "", diagnosis_7)), 6))) %>%
  mutate(d8 = (strtrim(as.character(gsub("[^Q0-9]", "", diagnosis_8)), 6))) %>%
  mutate(d9 = (strtrim(as.character(gsub("[^Q0-9]", "", diagnosis_9)), 6))) %>%
  mutate(d10 = (strtrim(as.character(gsub("[^Q0-9]", "", diagnosis_10)), 6))) %>%
  mutate(c1 = (strtrim(as.character(gsub("[^Q0-9]", "", comorbidity_1)), 6))) %>%
  mutate(c2 = (strtrim(as.character(gsub("[^Q0-9]", "", comorbidity_2)), 6))) %>%
  mutate(c3 = (strtrim(as.character(gsub("[^Q0-9]", "", comorbidity_3)), 6))) %>%
  mutate(c4 = (strtrim(as.character(gsub("[^Q0-9]", "", comorbidity_4)), 6))) %>%
  mutate(c5 = (strtrim(as.character(gsub("[^Q0-9]", "", comorbidity_5)), 6))) %>%
  mutate(c6 = (strtrim(as.character(gsub("[^Q0-9]", "", comorbidity_6)), 6))) %>%
  mutate(c7 = (strtrim(as.character(gsub("[^Q0-9]", "", comorbidity_7)), 6))) %>%
  mutate(c8 = (strtrim(as.character(gsub("[^Q0-9]", "", comorbidity_8)), 6))) %>%
  mutate(c9 = (strtrim(as.character(gsub("[^Q0-9]", "", comorbidity_9)), 6))) %>%
  mutate(c10 = (strtrim(as.character(gsub("[^Q0-9]", "", comorbidity_10)), 6))) %>%
  mutate(p1 = (strtrim(as.character(gsub("[^Q0-9]", "", procedure_1)), 6))) %>%
  mutate(p2 = (strtrim(as.character(gsub("[^Q0-9]", "", procedure_2)), 6))) %>%
  mutate(p3 = (strtrim(as.character(gsub("[^Q0-9]", "", procedure_3)), 6))) %>%
  mutate(p4 = (strtrim(as.character(gsub("[^Q0-9]", "", procedure_4)), 6))) %>%
  mutate(p5 = (strtrim(as.character(gsub("[^Q0-9]", "", procedure_5)), 6))) %>%
  mutate(p6 = (strtrim(as.character(gsub("[^Q0-9]", "", procedure_6)), 6))) %>%
  mutate(p7 = (strtrim(as.character(gsub("[^Q0-9]", "", procedure_7)), 6))) %>%
  mutate(p8 = (strtrim(as.character(gsub("[^Q0-9]", "", procedure_8)), 6))) %>%
  mutate(p9 = (strtrim(as.character(gsub("[^Q0-9]", "", procedure_9)), 6))) %>%
  mutate(p10 = (strtrim(as.character(gsub("[^Q0-9]", "", procedure_10)), 6))) %>%
  mutate(pp1 = (strtrim(as.character(gsub("[^Q0-9]", "", prev_procedure_1)), 6))) %>%
  mutate(pp2 = (strtrim(as.character(gsub("[^Q0-9]", "", prev_procedure_2)), 6))) %>%
  mutate(pp3 = (strtrim(as.character(gsub("[^Q0-9]", "", prev_procedure_3)), 6))) %>%
  mutate(pp4 = (strtrim(as.character(gsub("[^Q0-9]", "", prev_procedure_4)), 6))) %>%
  mutate(pp5 = (strtrim(as.character(gsub("[^Q0-9]", "", prev_procedure_5)), 6))) %>%
  mutate(pp6 = (strtrim(as.character(gsub("[^Q0-9]", "", prev_procedure_6)), 6))) %>%
  mutate(pp7 = (strtrim(as.character(gsub("[^Q0-9]", "", prev_procedure_7)), 6))) %>%
  mutate(pp8 = (strtrim(as.character(gsub("[^Q0-9]", "", prev_procedure_8)), 6))) %>%
  mutate(pp9 = (strtrim(as.character(gsub("[^Q0-9]", "", prev_procedure_9)), 6))) %>%
  mutate(pp10 = (strtrim(as.character(gsub("[^Q0-9]", "", prev_procedure_10)), 6))) %>%
  mutate(pp11 = (strtrim(as.character(gsub("[^Q0-9]", "", prev_procedure_11)), 6))) %>%
  mutate(pp12 = (strtrim(as.character(gsub("[^Q0-9]", "", prev_procedure_12)), 6))) %>%
  mutate(pp13 = (strtrim(as.character(gsub("[^Q0-9]", "", prev_procedure_13)), 6))) %>%
  mutate(pp14 = (strtrim(as.character(gsub("[^Q0-9]", "", prev_procedure_14)), 6))) %>%
  mutate(pp15 = (strtrim(as.character(gsub("[^Q0-9]", "", prev_procedure_15)), 6))) %>%
  mutate(pp16 = (strtrim(as.character(gsub("[^Q0-9]", "", prev_procedure_16)), 6))) %>%
  mutate(pp17 = (strtrim(as.character(gsub("[^Q0-9]", "", prev_procedure_17)), 6))) %>%
  mutate(pp18 = (strtrim(as.character(gsub("[^Q0-9]", "", prev_procedure_18)), 6))) %>%
  mutate(pp19 = (strtrim(as.character(gsub("[^Q0-9]", "", prev_procedure_19)), 6))) %>%
  mutate(pp20 = (strtrim(as.character(gsub("[^Q0-9]", "", prev_procedure_20)), 6))) %>%
  ## add age and age group
  mutate(age_days = round(as.numeric(Paop_rev) * 365.25)) %>% ## (procedure_date - X1.06.Birth.date)) %>%
  # nchda_data <- nchda_data %>%
  # nchda_ecmo <- nchda_ecmo %>%
  mutate(age_years = (age_days / 365.25)) %>%
  mutate(age_group = (ifelse(age_days < 31, "neonate",
    ifelse(age_days %in% 31:364, "infant",
      ifelse(age_days %in% 365:5843, "child", "adult")
    )
  ))) %>%
  ## add other files for populating
  mutate(aa_allocation = "") %>%
  mutate(fuvh = "") %>%
  ## sort file > important for activity allocation (ecmo)
  # arrange(hospital_code, patient_identifier, procedure_date_time) %>%
  ## allocate row_id to a named column
  rowid_to_column()


###################################################################################################
styler:::style_active_file()




##########################################################################
##########################################################################
###################################################################################################
## NCHDA activity analysis algorithm v8.03

## source AA and ECMO allocation functions
source(paste0(lib_dir, aa_ecmo_functions_filename)) # Dual consultant operator code has been added into the AA algorithm


###################################################################################################
## AA allocation > then primary ecmo allocation
nchda_data <- aa_allocation(nchda_data)

###################################################################################################
## run and update primary ecmo
## subgroup of records - 1,2,4,6,7,11 (3 added for 2016-19 analysis)
nchda_ecmo <- subset(nchda_data, ((type_procedure %in% c(1, 2, 3, 4, 6, 7, 11) & (aa_allocation != "unallocated") & (aa_allocation != "no_valid_codes"))))
nchda_ecmo <- ecmo_allocation(nchda_ecmo)

## update nchda_analysis from nchda_ecmo
ecmo_records <- nchda_ecmo %>%
  filter(aa_allocation == "primary_ecmo") %>%
  select(rowid, primary_ecmo = aa_allocation)
nchda_data <- nchda_data %>%
  left_join(ecmo_records, by = c("rowid" = "rowid")) %>%
  mutate(aa_allocation = replace(aa_allocation, primary_ecmo == "primary_ecmo", "primary_ecmo"))

# The code was added on 1st Sept 2020 to deal with dual consultant operator
nchda_data <- plyr:::rename(nchda_data, c("X3.04.First.operator.grade" = "operator_grade_1", "X3.06.First.assistant.grade" = "operator_grade_2"))

nchda_data[, "operator_grade_1"] <- strtrim(as.character(gsub("[^Q0-9]", "", nchda_data[, "operator_grade_1"])), 1)
nchda_data[, "operator_grade_2"] <- strtrim(as.character(gsub("[^Q0-9]", "", nchda_data[, "operator_grade_2"])), 1)

index <- which(nchda_data$operator_grade_1 == 1 & nchda_data$operator_grade_2 == 1)

nchda_data$dual_consultant <- "no"
nchda_data$dual_consultant[index] <- "yes"

###################################################################################################
## run sp allocation processes

source(paste0(lib_dir, spec_proc_functions_filename))

## sp_algorithm(nchda_data[12, ]) ## single record
## if(nrow(nchda_data) > 0){
##    nchda_data[1:nrow(nchda_data),'sp_allocation'] <- sapply(1:nrow(nchda_data), function(i) sp_algorithm(nchda_data[i,]) )
## }

nchda_data <- nchda_data %>% by_row(sp_algorithm, .to = "sp_allocation")
nchda_data[, "sp_allocation"] <- unlist(nchda_data[, "sp_allocation"])

###################################################################################################
## write data to CSV file

tokeep <- c("LAUNCHESrecID", "LAUNCHESpatID_rev", "CHAMPIONpatID", "type_procedure", "antenatal", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9", "p10", "pp1", "pp2", "pp3", "pp4", "pp5", "pp6", "pp7", "pp8", "pp9", "pp10", "aa_allocation", "fuvh", "primary_ecmo", "dual_consultant", "sp_allocation")

write.csv(nchda_data[, tokeep], file = paste0(out_dir, out_filename), row.names = FALSE, na = "", quote = TRUE)


###################################################################################################
styler:::style_active_file()
