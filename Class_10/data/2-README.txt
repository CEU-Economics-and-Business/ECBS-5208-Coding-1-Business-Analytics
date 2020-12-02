****************************************************************
Prepared for Gabor's Data Analysis

Data Analysis for Business, Economics, and Policy
 by Gabor Bekes and  Gabor Kezdi
 Cambridge University Press 2021
 gabors-data-analysis.com 

Description of the 
share-health dataset

used in case study 11A Does smoking pose a health risk?


****************************************************************
Data source

Survey of Health, Aging and Retirement in Europe (SHARE)
http://www.share-project.org/home0.html

easySHARE dataset, release 7.1.0
http://www.share-project.org/special-data-sets/easyshare.html
"easySHARE is stored as a long format panel dataset covering respondents from all SHARE countries and hence it is very suitable for teaching longitudinal as well as country-comparative analyses."

****************************************************************
Data access and copyright

Registered SHARE users can download easySHARE from the SHARE Research Data Center. 
https://releases.sharedataportal.eu/users/login
You will receive login/password once you are registered

For details see 
./get-data/GET DATA.txt


IMPORTANT
Acces to the SHARE data means that you can use the data for your education. 
Nobody can distribute the original SHARE data or any data derived from it.
Therefore, we don't include it in our data_repo, 
neither the raw data nor the tidy data.
We distribute the code that cleans the data so you can create it for yourself.


****************************************************************
GET THE RAW FILES
****************************************************************

The process is simple
1. Login
2. We will need the archived 6.0.0 version. https://releases.sharedataportal.eu/releases?show_archived=1
3. Download the zip file `easySHARE_6.0.0_Stata.zip` 
4. Unzip the file, and copy the .dta file to the raw folder

Note for
* Stata users: `clean/share-health-cleaner.do` will import the raw data and prepare the clean version we use
* R users: `clean/share-health-cleaner.R` will import the raw data (in Stata format), and prepare the clean version (as .csv file) we use
* Python users: `clean/share-health-cleaner.py` will import the raw data (in Stata format), and prepare the clean version (as .csv file) we use

Note for all users
* The current version of EasyShare (7.1.0. in late 2020) will include what was in 6.0.0 and more. However, minor revisions may lead to small differences in the dataset. 

****************************************************************
Raw data tables
****************************************************************

easySHARE_rel6-0-0.dta
easySHARE dataset, release 6.0.0
Stata data file
long format panel data, 21 countries, 6 survey waves
observations person x survey wave, n=288,736
ID variables mergeid (individuals) wave (survey wave)
important variables 	country		country
			age		age
			female		gender (female=1 or 0)
			isced1997_r	education
			smoking		smokes 
			ever_smoked	smoked ever
			sphus		self-perceived health (US version)





*************
Tidy data tables
*************

share-health
derived from the easySHARE dataset, release 6.0.0
same as the original file with fewer variables
long format panel data, 21 countries, 6 survey waves
observations person x survey wave, n=288,736
ID variables mergeid (individuals) wave (survey wave)
important variables 	country		country
			age		age
			female		gender (female=1 or 0)
			isced1997_r	education
			smoking		smokes 
			ever_smoked	smoked ever
			sphus		self-perceived health (US version)
The data table is not included in the distributed data repo.
Instead, you need to run the share-health-cleaner code to create it
from the raw data (that you downloaded with appropriate permission)



********************************************************
easySHARE_rel7-1-0.dta
********************************************************

The latest version available in Oct 2020:
easySHARE dataset, release 7.1.0
Stata data file
long format panel data, 29 countries, 7 survey waves
observations person x survey wave, n=365,806
ID variables mergeid (individuals) wave (survey wave)
important variables 	country		country
			age		age
			female		gender (female=1 or 0)
			isced1997_r	education
			smoking		smokes 
			ever_smoked	smoked ever
			sphus		self-perceived health (US version)
The data table is not included in the distributed data repo.
Instead, you need to get permission from SHARE and download it yourself.

