# CHD research code: Risk Models

**Ferran Espuny Pujol, Rodney CG Franklin, Katherine Brown, Sonya Crowe, Christina Pagel**

## Description

R code used for research on Congenital Heart Disease (CHD) in the research projects 

* LAUNCHES QI - Linking audit and national datasets in congenital heart services for quality improvement;
* CHAMPION - Congenital heart audit: measuring progress in outcomes nationally (CHAMPION).

The risk models being implemented are:

* A revised version of [PRAiS2](https://www.sciencedirect.com/science/article/pii/S0003497516318288), a 30-day post-surgical mortality model used for national audit of survival in children following heart surgery. This latest version of PRAiS is called PRAiS4.

The latest specification for PRAiS4, including a summary of changes from PRAiS2 and its performance, is given in the PDF.

## Contents

05 Risk Models: R files and Mapping files

R files to use all processed NCHDA data to:

* generate 30-day episodes and derive all PRAi4S risk factors (R file 05.01), 
* run the PRAiS4 risk model (R file 05.02)

The folder [mapping files](<mapping files>) contains the PRAIS definitions of diagnosis and procedure broad risk groups, and the actual coefficients for the PRAIS model.
