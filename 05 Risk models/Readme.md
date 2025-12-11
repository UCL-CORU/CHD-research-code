# CHD research code: Risk Models

**Ferran Espuny Pujol, Rodney CG Franklin, Katherine Brown, Sonya Crowe, Christina Pagel**

## Description

R code used for research on Congenital Heart Disease (CHD) in the research projects 

* LAUNCHES QI - Linking audit and national datasets in congenital heart services for quality improvement;
* CHAMPION - Congenital heart audit: measuring progress in outcomes nationally (CHAMPION).

The risk models being implemented are:

* A revised version of [PRAiS2](https://www.sciencedirect.com/science/article/pii/S0003497516318288), a 30-day post-surgical mortality model used for national audit of survival in children following heart surgery. This latest version of PRAiS is called PRAiS4.
* [New Models for Risk-Adjusted Monitoring of Postsurgical Complications and Mortality in Adult Congenital Heart Disease (ACHD) in England and Wales](https://www.sciencedirect.com/science/article/pii/S0003497525007519): 30-day and 90-day post-surgical mortality and 30-day post-surgical complications models.

The latest specification for PRAiS4, including a summary of changes from PRAiS2 and its performance, is given in the PDF.

## Contents

05 Risk Models: R files and Mapping files

R files to use all processed NCHDA data to:

* generate 30-day paediatric surgery episodes and derive all PRAi4S risk factors (R file 05.01) 
* run the PRAiS4 risk model (R file 05.02)
* generate 30-day and 90-day adult surgery episodes and derive all ACHD risk factors (R file 05.03)
* run the ACHD 30-day mortality risk model (R file 05.04)
* run the ACHD 90-day mortality risk model (R file 05.05)
* run the ACHD 30-day complications risk model (R file 05.06)

The folder [mapping files](<mapping files>) contains the PRAIS definitions of diagnosis and procedure broad risk groups, the actual coefficients for the PRAIS model, the ACHD inclusion and risk factor definition mappings, and the actual coefficients for all three ACHD risk models.
