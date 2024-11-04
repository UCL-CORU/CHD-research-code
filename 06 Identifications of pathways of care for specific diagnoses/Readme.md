# CHD research code: Identification of Pathways of Care for Specific Diagnoses

**Katherine Brown, Qi Huang, Ferran Espuny Pujol, Christina Pagel, Rodney CG Franklin, Sonya Crowe**

## Description

R code used for research on Congenital Heart Disease (CHD) in the research projects 

* LAUNCHES QI - Linking audit and national datasets in congenital heart services for quality improvement;
* CHAMPION - Congenital heart audit: measuring progress in outcomes nationally (CHAMPION).

## Contents

06 Identification of Pathways of Care for Specific Diagnoses: R code and Word documents

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
CHD and paediatric cardiac operations are very diverse and complex, and are described in NCHDA using a special coding scheme from the European Paediatric Cardiac Code (EPCC), itself a derived Short List of the International Paediatric and Congenital Cardiac Code (www.ipccc.net). 
One can download the NCHDA data manual and EPCC codes at NICOR’s website (https://www.nicor.org.uk/datasets/supporting-data-set-documentation). 
To note, as the NCHDA is a procedure-based dataset, patients who did not undergo any surgical or interventional cardiac procedures do not appear in the dataset.
