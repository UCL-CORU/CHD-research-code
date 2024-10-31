# CHD research code: Data Cleaning

**Ferran Espuny Pujol, John Stickley, Rodney CG Franklin, Katherine Brown, Jiaqiu Wang, Sonya Crowe, Christina Pagel**

## Description

R code used for research on Congenital Heart Disease (CHD) in the research projects 

* LAUNCHES QI - Linking audit and national datasets in congenital heart services for quality improvement;
* CHAMPION - Congenital heart audit: measuring progress in outcomes nationally (CHAMPION).

## Contents

01 Data Cleaning: R files and Code lists and libs

R files allow to process and clean the following data sets:

* National Congenital Heart Disease Audit (NCHDA)
  - R file 01.01 for CORU code
  - R file 01.02 for activity algorithm and specific procedure algorithms v8.05 by NICOR 
* Hospital Episode Statistics (HES) - R file 01.03
* Paediatric Intensive Care Network (PICANet) - R file 01.04

The folder [Code lists and libs](<Code lists and libs>) containts the R code from NICOR (John Stickley, Jiaqiu Wang) and HES data dictionary lookups gathered by Ferran Espuny Pujol.

## Details

As part of cleaning the NCHDA extract at COUR UCL, we revised ages, derived dates from ages and a single IMD decile field, tidied the fields sex, ethnicity, patient status, diagnosis codes, previous procedure codes, antenatal diagnosis, pre-procedure seizures, comorbidity codes, pre-procedural systemic ventricular ejection fraction, pre-procedural sub-pulmonary ventricular ejection fraction, procedure urgency, procedure type, sternotomy sequence, procedure codes, discharge status, pre-procedural NYHA status, pre-procedural smoking status, pre-procedural diabetes, history of pulmonary disease, pre-procedural ischemic heart disease.

For HES data, we processed the Health Research Group (HRG) and treatment Speciality (TRETSPEF) fields to derive a flag "isCardiac" indicating whether the hospital (inpatient, outpatient or A&E) episode was cardiac. 
