clear all 
set more off

cd "C:\Users\michs\Dropbox\_research\CzechElectionData\ced\donation_data"

global polparty "kscm kducsl ods pirati spd stan top09 ano cssd"
global years "2023 2022 2021 2020 2019 2018 2017"

** data download ** 
* in: web 
* out: raw xls | dta
do 01_download.do 

** data merged **
* in: raw xls | dta
* out: donation_data_financial & donation_data_nonfinancial
do 02_merge.do 

** data handling **
* in: donation_data_financial.dta
* out: donation_data_financial_clean.dta
do 03a_handling.do 

** data handling **
* in: donation_data_nonfinancial.dta
* out: donation_data_nonfinancial_clean.dta
do 03b_handling.do 

** final merge **
* in: donation_data_financial_clean & donation_data_nonfinancial_clean
* out: data_donation_final
do 04_finalmerge.do


** data validation **
* in: donation_data_clean.dta
* out: 
do 05_validation.do 


