clear all 
set more off

cd "C:\Users\michs\Dropbox\_research\CzechElectionData\ced\donation_data"

global polparty "kducsl kscm ods pirati spd stan top09 ano cssd"
global years "2023 2022 2021 2020 2019 2018 2017"


foreach p of global polparty {
    foreach y of global years {
	    
	capture: import excel "https://zpravy.udhpsh.cz/export/vfz`y'-`p'-penizefo.xls", first clear 
	sleep 100
	export excel  "pp_`p'\y_`y'\vfz`y'-`p'-penizefo.xls", replace
	save "pp_`p'\y_`y'\vfz`y'-`p'-penizefo.dta", replace
		
	sleep 100
	
	capture: import excel "https://zpravy.udhpsh.cz/export/vfz`y'-`p'-bupfo.xls", first clear 
	sleep 100
	export excel  "pp_`p'\y_`y'\vfz`y'-`p'-bupfo.xls", replace
	save "pp_`p'\y_`y'\vfz`y'-`p'-bupfo.dta", replace
	sleep 100
		}
	}
