clear all 
set more off

** set path **
cd "C:\Users\michs\Dropbox\_research\CzechElectionData\ced\ced\ced\donation_data\"

** define global **
global polparty "ano kscm kducsl ods pirati spd stan top09 cssd svobodni trikolora prisaha"
global years "2023 2022 2021 2020 2019 2018 2017"
global donationform "financial nonfinancial"


** create directories **
capture mkdir primary_data
capture mkdir data
capture mkdir intermediate_data


*****************************************
************	download  ***************
*****************************************
** data download ** 
* in: web 
* out: 
do dofiles\download_oofppm.do 

 
*****************************************
********** data handling  ***************
***************************************** 

** data handling **
* in: 
* out: 
do dofiles\handling_oofppm.do 


*****************************************
********** validation 	*****************
*****************************************
/*
* in: donation_data_clean.dta
* out: 
do 05_validation.do 


