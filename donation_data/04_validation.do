use donation_data_clean.dta, clear

preserve

collapse (first) polparty firstname surname birthdate acadegree_an acadegree_bn (sum) i donated_value , by(id)

order polparty id surname firstname birthdate acadegree_bn acadegree_an

generate day = day(birthdate)
hist day

gen n = 1 

collapse (sum) n, by(polparty day)

drop if day == 31

bysort polparty: egen average = mean(n)
gen dev = n-average


forvalues p = 1(1)9 {	
    twoway (bar dev day if polparty == `p', color(gs8) barw(0.8)), xtitle("Day of Month", size(small)) ytitle("Deviation from Average", size(small)) xlabel(1 15 30) title("Number of Donors")
	graph export "validation\birth_day_pol_`p'.png", as(png) name("Graph") replace
}

restore

