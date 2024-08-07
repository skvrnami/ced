use data_donation_final.dta, clear

preserve

collapse (first) polparty firstname surname birthdate acadegree_an acadegree_bn (sum) donation_all financial_donation nonfinancial_donation , by(id)

order polparty id surname firstname birthdate acadegree_bn acadegree_an

generate day = day(birthdate)
hist day

gen n = 1 

collapse (sum) n, by(polparty day)

drop if day > 28

bysort polparty: egen average = mean(n)
gen dev = n-average


forvalues p = 1(1)9 {	
    twoway (bar dev day if polparty == `p', color(gs8) barw(0.8)), xtitle("Day of Month", size(small)) ytitle("Deviation from Average", size(small)) xlabel(1 14 28) title("Number of Donors")
	graph export "validation\birth_day_pol_`p'.png", as(png) name("Graph") replace
	
}


restore

preserve
collapse (first) polparty firstname surname birthdate acadegree_an acadegree_bn (sum) donation_all financial_donation nonfinancial_donation , by(id)

generate day = day(birthdate)
hist day

drop if day > 28

gen dob = 0
replace dob = runiformint(1, 28) if polparty == 1
replace dob = runiformint(1, 28) if polparty == 2
replace dob = runiformint(1, 28) if polparty == 3
replace dob = runiformint(1, 28) if polparty == 4
replace dob = runiformint(1, 28) if polparty == 5
replace dob = runiformint(1, 28) if polparty == 6
replace dob = runiformint(1, 28) if polparty == 7
replace dob = runiformint(1, 28) if polparty == 8
replace dob = runiformint(1, 28) if polparty == 9


tab day dob if polparty == 1, chi2
tab day dob if polparty == 2, chi2
tab day dob if polparty == 3, chi2
tab day dob if polparty == 4, chi2
tab day dob if polparty == 5, chi2
tab day dob if polparty == 6, chi2
tab day dob if polparty == 7, chi2
tab day dob if polparty == 8, chi2
tab day dob if polparty == 9, chi2

tab day dob, chi2
restore
