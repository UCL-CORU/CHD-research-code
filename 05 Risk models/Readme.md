# CHD research code: Risk Models

**Ferran Espuny Pujol, John Stickley, Rodney CG Franklin, Katherine Brown, Jiaqiu Wang, Sonya Crowe, Christina Pagel**

## Description

R code used for research on Congenital Heart Disease (CHD) in the research projects 

* LAUNCHES QI - Linking audit and national datasets in congenital heart services for quality improvement;
* CHAMPION - Congenital heart audit: measuring progress in outcomes nationally (CHAMPION).

The risk models being implemented are:

* A revised version of [PRAiS2](https://www.sciencedirect.com/science/article/pii/S0003497516318288), a 30-day post-surgical mortality model used for national audit 

## Contents

05 Risk Models: R files and Mapping files

R files allow to use all processed NCHDA data to:

* generate 30-day episodes and derive all PRAIS risk factors (R file 05.01), 
* run the PRAIS risk model (R file 05.02)

The folder [mapping files](<mapping files>) contains the PRAIS definitions of diagnosis and procedure broad risk groups, and the actual coefficients for the PRAIS model.
