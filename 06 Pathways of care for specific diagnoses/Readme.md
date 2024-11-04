# CHD research code: Pathways of Care for Specific Diagnoses

**Katherine Brown, Qi Huang, Ferran Espuny Pujol, Christina Pagel, Rodney CG Franklin, Sonya Crowe**

## Description

R code used for research on Congenital Heart Disease (CHD) in the research projects 

* LAUNCHES QI - Linking audit and national datasets in congenital heart services for quality improvement;
* CHAMPION - Congenital heart audit: measuring progress in outcomes nationally (CHAMPION).

## Contents

06 Pathways of Care for Specific Diagnoses: R code and Word documents

There are 9 sentinel Congenital Heart Disease (CHD) diagnoses and one should run the programming for each CHD diagnosis in the following order (decreasing order of complexity):
*	Hypoplastic left heart syndrome (HLHS)
*	Functionally univentricular heart (FUH) 
*	Transposition of the great arteries (TGA)
*	Pulmonary atresia (PA)
*	Atrioventricular septal defect (AVSD)
*	Tetralogy of Fallot (TOF)
*	Aortic stenosis (AOS)
*	Coarctation (COA)
*	Ventricular septal defect (VSD)

For each CHD, the main R file is “CHDtype_main file.R”, which calls functions from "generic_modules.R" and "CHDtype_modules.R". 
Code list used in each CHD type are included in the corresponding sub folder. 

Before running the code, one needs to carefully read the rules for individual CHD, which include all code lists for inclusion and exclusion, assigning pathways & diagnosis subgroup, and handling data missing/errors, etc.
For enquiries, qi_huang@ucl.ac.uk

## Details

### Data sources
The core dataset is the National Congenital Heart Disease Audit (NCHDA) with ONS death registration data linked. 
NCHDA is run by the National Institute for Cardiovascular Outcomes Research (NICOR), which collects data to assess patient outcomes after therapeutic paediatric and congenital cardiovascular procedures (surgery, transcatheter, and electrophysiological interventions). 
CHD and paediatric cardiac operations are very diverse and complex, and are described in NCHDA using a special coding scheme from the European Paediatric Cardiac Code (EPCC), itself a derived [Short List of the International Paediatric and Congenital Cardiac Code](www.ipccc.net). 
One can download the NCHDA data manual and EPCC codes at [NICOR’s website](https://www.nicor.org.uk/datasets/supporting-data-set-documentation). 
To note, as the NCHDA is a procedure-based dataset, patients who did not undergo any surgical or interventional cardiac procedures do not appear in the dataset.

### Important variables 

We use the following important variables to assign the sentinel CHD diagnosis, subgroups, treatment pathway and suspected missing data. 

*	patid: patient-level identifier 
*	ageatop/ageatdis (record-level, derived): age at operation/discharge, which was computed by the difference between the date of operation/discharge and the birth date.
*	patentry (record-level, derived): one patient may have multiple procedure records. We order the entry of each record by age at operation, and if multiple records have the same age at operation, we use age at discharge to order the entry.
*	diagcode1-29 (record-level, derived): EPCC codes in diagnosis filed (cleaned, contain the 6 digits EPCC Code only)(code can be seen in 01 Data cleaning/01.02 NCHDA data processing from NICOR activity algorithm and specific procedure.R, marked as d1-d10. the maximum number of diagnosis codes depend on the exact data)
*	comorbidity1-16 (record-level, derived): codes in comorbidity filed (cleaned, contain the 6 digits EPCC Code only)(code can be seen in 01 Data cleaning/01.02 NCHDA data processing from NICOR activity algorithm and specific procedure.R, marked as c1-c10. the maximum number of comorbidity codes depend on the exact data)
*	proccode1-7 (record-level, derived):  codes in procedure filed (cleaned, contain the 6 digits EPCC Code only)(code can be seen in 01 Data cleaning/01.02 NCHDA data processing from NICOR activity algorithm and specific procedure.R, marked as p1-p10. the maximum number of procedure codes depend on the exact data)
*	prevproccode1-26 (record-level, derived):  codes in previous procedure filed (cleaned, contain the 6 digits EPCC Code only)(code can be seen in 01 Data cleaning/01.02 NCHDA data processing from NICOR activity algorithm and specific procedure.R, marked as pp1-pp10. the maximum number of procedure codes depend on the exact data)
*	sp_allocation (record-level, derived): specific procedure algorithm allocation of procedure type ([version 8.05, used in NICOR](code can be seen in 01 Data cleaning/01.02 NCHDA data processing from NICOR activity algorithm and specific procedure.R)
*	aa_allocation (record-level, derived):  activity algorithm allocation of procedure type. The algorithm is developed by NICOR (code can be seen in 01 Data cleaning/01.02 NCHDA data processing from NICOR activity algorithm and specific procedure.R)
*	InterType (record-level, derived):  intervention type derived from aa_allocation.
  The labels are:
    - 1: Surgery and hybrid (aa_allocation 1:bypass ,2:non-bypass,3:hybrid)
    - 2: Interventional catheters and ep (aa_allocation 6: icd:non-surgical, 7: pacemaker:non-surgical, 8: ep:non-surgical, 9: intervention:non-surgical)
    - 3: Excluded (aa_allocation 4: vad, 5: unallocated-ecmo, 10: diagnostic:non-surgical, 11: unallocated, 12: primary ecmo)
*	lastknownstatus/ageatlastknowstatus (patient-level, derived): vital status (0 censored/1 death) and age at vital status, which were computed from two data sources: NCHDA and ONS death registration.
  Patient vital status (dead or alive) was provided at the point of hospital discharge by NCHDA, who obtained this information from treating centres. The age at death for any patient who had died was taken from death certification data provided by the ONS. For surviving patients, we received from ONS their age when this status was confirmed. Any patients that were discharged alive and who had missing life status with ONS were deemed lost to follow up and were censored at their most recent discharge age provided by NCHDA. 
*	lastknownstatus_NCHDA/ageatlastknowstatus_NCHDA (patient-level): vital status (0 censored/1 death) and vital at survival status as of the last date of NCHDA dataset. 
There may be a time gap between the ONS confirmation date and the last date of the NCHDA dataset (due to the time required for regulatory processes), so procedures undertaken during this time gap may have been missed. We therefore used patients’ vital status as of the last date of NCHDA to calculate the follow-up time and status of procedures.

### Further details

For further details, you can consult the provided Word documents and the publication

> K.L Brown, Q. Huang, F. Espuny-Pujol, etal. Evaluating long-term outcomes of children undergoing surgical treatment for congenital heart disease for national audit in England and Wales. [J. Am. Heart Assoc. 2024](https://www.ahajournals.org/doi/10.1161/JAHA.124.035166).
