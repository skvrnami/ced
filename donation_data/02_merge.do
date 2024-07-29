capture: erase donation_data_financial.dta

foreach p of global polparty {
    foreach y of global years {
		capture{
			use "pp_`p'\y_`y'\vfz`y'-`p'-penizefo.dta", clear
		}
		
		if _rc == 0 {
			quietly{
				rename příjmení surname
				rename jméno firstname 
				rename datumnarození birthdate
				rename obec municipality
			
				tostring titulpřed, replace 
				tostring titulza, replace 
				
				gen year = `y'
				gen polparty = . 
				replace polparty = 1 if "`p'" == "kducsl"
				replace polparty = 2 if "`p'" == "kscm"
				replace polparty = 3 if "`p'" == "ods"
				replace polparty = 4 if "`p'" == "pirati"
				replace polparty = 5 if "`p'" == "spd"
				replace polparty = 6 if "`p'" == "stan"
				replace polparty = 7 if "`p'" == "top09"
				replace polparty = 8 if "`p'" == "ano"
				replace polparty = 9 if "`p'" == "cssd"
			
						
				if "`p'"== "kscm" & `y' == 2023 {
				save donation_data_financial.dta, replace
				} 
			else {
				append using donation_data_financial.dta 
				save donation_data_financial.dta, replace
				sleep 50
				}	
			}
			}
		else {
		dis "vfz`y'-`p'-penizefo.dta not found"	
		}
	}
	
	clear 		
	}

	

capture: erase donation_data_nonfinancial.dta

foreach p of global polparty {
    foreach y of global years {
		capture{
			use "pp_`p'\y_`y'\vfz`y'-`p'-bupfo.dta", clear
		}
		
		if _rc == 0 {
			quietly{
				rename příjmení surname
				rename jméno firstname 
				rename datumnarození birthdate
				rename obec municipality
			
				tostring titulpřed, replace 
				tostring titulza, replace 
				
				gen year = `y'
				gen polparty = . 
				replace polparty = 1 if "`p'" == "kducsl"
				replace polparty = 2 if "`p'" == "kscm"
				replace polparty = 3 if "`p'" == "ods"
				replace polparty = 4 if "`p'" == "pirati"
				replace polparty = 5 if "`p'" == "spd"
				replace polparty = 6 if "`p'" == "stan"
				replace polparty = 7 if "`p'" == "top09"
				replace polparty = 8 if "`p'" == "ano"
				replace polparty = 9 if "`p'" == "cssd"
			
				capture: drop popisBÚP
						
				if "`p'"== "kscm" & `y' == 2023 {
				save donation_data_nonfinancial.dta, replace
				} 
			else {
				append using donation_data_nonfinancial.dta 
				save donation_data_nonfinancial.dta, replace
				sleep 50
				}	
			}
			}
		else {
		dis "vfz`y'-`p'-bupfo.dta not found"	
		}
	}
	
	clear 		
	}