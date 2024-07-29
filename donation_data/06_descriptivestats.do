use donation_data_clean.dta, clear

collapse (sum) i donated_value donation_all gratuitous_performance ,by(polparty year)

gen constant = 0
replace constant = donation_all if year == 2017
replace constant = constant[_n-1] if polparty[_n] == polparty[_n-1] & year[_n] == year[_n-1] + 1

gen relative_donation = donation_all/constant

*bysort polparty: egen average_donation_all = mean(donation_all)
*gen relative_donation = donation_all/average_donation_all 

keep relative_donation year polparty

reshape wide relative_donation, i(year) j(polparty)

twoway (connected relative_donation1 relative_donation2 relative_donation3 relative_donation3 relative_donation4 relative_donation5 relative_donation6 relative_donation7 relative_donation8 relative_donation9 year), legend(order(1 "kducsl" 2 "kscm" 3 "ods" 4 "pirati" 5 "spd" 6 "stan" 7 "top09" 8 "ano" 9 "cssd") row(3))


*label define polparty 1 "kducsl" 2 "kscm" 3 "ods" 4 "pirati" 5 "spd" 6 "stan" 7 "top09" 8 "ano" 9 "cssd"