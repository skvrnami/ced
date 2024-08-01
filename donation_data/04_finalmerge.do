use donation_data_financial_clean.dta, clear

tempfile fileA
keep if financial_donation > 0
save `fileA'

use donation_data_financial_clean.dta, clear
keep if financial_donation == 0

sort polparty surname firstname birthdate year 
merge polparty surname firstname birthdate year using donation_data_nonfinancial_clean.dta

keep if _merge == 3

append using `fileA'

replace financial_donation = 0 if financial_donation == .
replace nonfinancial_donation = 0 if nonfinancial_donation == .
 
 
egen id = group(polparty surname firstname birthdate)

generate length_degree_bn = strlen(acadegree_bn)
replace length_degree_bn = 0 if length_degree_bn == 1
generate length_degree_an = strlen(acadegree_an)
replace length_degree_an = 0 if length_degree_an == 1

gen length = length_degree_an + length_degree_bn

gsort id -length

replace acadegree_bn = acadegree_bn[_n-1] if id[_n] == id[_n-1]
replace acadegree_an = acadegree_an[_n-1] if id[_n] == id[_n-1]

drop lengt* 
 
sort polparty surname firstname birthdate year

gen donation_all = financial_donation + nonfinancial_donation

gen i = 1

collapse (first) id acadegree_an acadegree_bn (sum) i donation_all financial_donation nonfinancial_donation, by(polparty surname firstname birthdate year)

save data_donation_final.dta, replace
 