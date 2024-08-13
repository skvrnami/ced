foreach d of global odldata {

use "C:\Users\michs\Dropbox\_research\CzechElectionData\ced\ced\ced\donation_data\old_data\donation_`d'_edit.dta" 

gen polparty = .
replace polparty = 1 if Politickástrana == "KDUCSL"
replace polparty = 2 if Politickástrana == "KSCM"
replace polparty = 3 if Politickástrana == "ODS"
replace polparty = 7 if Politickástrana == "TOP09"
replace polparty = 8 if Politickástrana == "ANO11" | Politickástrana == "ANO2011"
replace polparty = 9 if Politickástrana == "CSSD"



}
