foreach p of global polparty {
	
	capture mkdir primary_data\oofppm\pp_`p'
	
    foreach y of global years {
		
	capture mkdir primary_data\oofppm\pp_`p'\y_`y'
    
	capture: import excel "https://zpravy.udhpsh.cz/export/vfz`y'-`p'-penizefo.xls", first clear 
	sleep 100
	capture: export excel  "primary_data\oofppm\pp_`p'\y_`y'\vfz`y'-`p'-penizefo.xls", replace
	capture: tostring titulpřed, replace
	capture: tostring titulza, replace
	capture: replace titulza  = subinstr(titulza, `"""', `""', .)
	capture: replace titulpřed  = subinstr(titulpřed, `"""', `""', .)
	capture: replace titulpřed = "." if titulpřed == ""
	capture: replace titulza = "." if titulza == ""
	capture: save "primary_data\oofppm\pp_`p'\y_`y'\vfz`y'-`p'-penizefo.dta", replace
		
	sleep 100
	clear 
	
	capture: import excel "https://zpravy.udhpsh.cz/export/vfz`y'-`p'-bupfo.xls", first clear 
	sleep 100
	capture: export excel  "primary_data\oofppm\pp_`p'\y_`y'\vfz`y'-`p'-bupfo.xls", replace
	capture: tostring titulpřed, replace
	capture: tostring titulza, replace
	capture: replace titulza  = subinstr(titulza, `"""', `""', .)
	capture: replace titulpřed  = subinstr(titulpřed, `"""', `""', .)
	capture: replace titulpřed = "." if titulpřed == ""
	capture: replace titulza = "." if titulza == ""
	capture: save "primary_data\oofppm\pp_`p'\y_`y'\vfz`y'-`p'-bupfo.dta", replace
	sleep 100
	clear 
		}
	}

	
capture: erase intermediate_data\donation_data_financial.dta

foreach p of global polparty {
    foreach y of global years {
		capture{
			use "primary_data\oofppm\pp_`p'\y_`y'\vfz`y'-`p'-penizefo.dta", clear
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
				replace polparty = 10 if "`p'" == "svobodni"
				replace polparty = 11 if "`p'" == "trikolora"
				replace polparty = 12 if "`p'" == "prisaha"
			
						
				if "`p'"== "ano" & `y' == 2023 {
				save intermediate_data\donation_data_financial.dta, replace
				} 
			else {
				append using intermediate_data\donation_data_financial.dta
				save intermediate_data\donation_data_financial.dta, replace
				sleep 200
				}	
			}
			}
		else {
		dis "vfz`y'-`p'-penizefo.dta not found"	
		}
	}
	
	clear 		
	}

	

capture: erase intermediate_data\donation_data_nonfinancial.dta

foreach p of global polparty {
    foreach y of global years {
		capture{
			use "primary_data\oofppm\pp_`p'\y_`y'\vfz`y'-`p'-bupfo.dta", clear
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
				replace polparty = 10 if "`p'" == "svobodni"
				replace polparty = 11 if "`p'" == "trikolora"
				replace polparty = 12 if "`p'" == "prisaha"
			
				capture: drop popisBÚP
						
				if "`p'"== "ano" & `y' == 2023 {
				save intermediate_data\donation_data_nonfinancial.dta, replace
				} 
			else {
				append using intermediate_data\donation_data_nonfinancial.dta 
				save intermediate_data\donation_data_nonfinancial.dta, replace
				sleep 200
				}	
			}
			}
		else {
		dis "vfz`y'-`p'-bupfo.dta not found"	
		}
	}
	
	clear 		
	}