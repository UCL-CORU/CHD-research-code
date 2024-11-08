# CHD research code: Patient trajectories

**Ferran Espuny Pujol, Rodney CG Franklin, Katherine Brown, Sonya Crowe, Christina Pagel**

## Description

R code used for research on Congenital Heart Disease (CHD) in the research projects 

* LAUNCHES QI - Linking audit and national datasets in congenital heart services for quality improvement;
* CHAMPION - Congenital heart audit: measuring progress in outcomes nationally (CHAMPION).

## Contents

02 Patient trajectories: R files 

R files to combine the following data sets into a patient data set (R file 02.01), where patients have multiple episodes, named "spells of care" (R file 02.02):

* National Congenital Heart Disease Audit (NCHDA)
* Hospital Episode Statistics (HES)
* Paediatric Intensive Care Network (PICANet)

Data was linked (ie record IDs pertaining to the same patient) external to CORU. 
CORU only received anonymised records with a study record ID that allowed us to identify linked records.
The creation of spells of care and the data linkage process was first described in:
> Espuny Pujol F, Pagel C, Brown KL, et al. Linkage of National Congenital Heart Disease Audit data to hospital, critical care and mortality national data sets to enable research focused on quality improvement. BMJ Open 2022;12:e057343. [doi: 10.1136/bmjopen-2021-057343](<https://bmjopen.bmj.com/content/12/5/e057343>)
