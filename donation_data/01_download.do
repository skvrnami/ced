
foreach p of global polparty {
	
	capture mkdir pp_`p'
	
    foreach y of global years {
		
	capture mkdir pp_`p'\y_`y'
    
	capture: import excel "https://zpravy.udhpsh.cz/export/vfz`y'-`p'-penizefo.xls", first clear 
	sleep 100
	capture: export excel  "pp_`p'\y_`y'\vfz`y'-`p'-penizefo.xls", replace
	capture: tostring titulpřed, replace
	capture: tostring titulza, replace
	capture: replace titulza  = subinstr(titulza, `"""', `""', .)
	capture: replace titulpřed  = subinstr(titulpřed, `"""', `""', .)
	capture: replace titulpřed = "." if titulpřed == ""
	capture: replace titulza = "." if titulza == ""
	capture: save "pp_`p'\y_`y'\vfz`y'-`p'-penizefo.dta", replace
		
	sleep 100
	
	capture: import excel "https://zpravy.udhpsh.cz/export/vfz`y'-`p'-bupfo.xls", first clear 
	sleep 100
	capture: export excel  "pp_`p'\y_`y'\vfz`y'-`p'-bupfo.xls", replace
	capture: tostring titulpřed, replace
	capture: tostring titulza, replace
	capture: replace titulza  = subinstr(titulza, `"""', `""', .)
	capture: replace titulpřed  = subinstr(titulpřed, `"""', `""', .)
	capture: replace titulpřed = "." if titulpřed == ""
	capture: replace titulza = "." if titulza == ""
	capture: save "pp_`p'\y_`y'\vfz`y'-`p'-bupfo.dta", replace
	sleep 100
	clear 
		}
	}
