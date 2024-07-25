clear all 
set more off

cd "C:\Users\michs\Dropbox\_research\CzechElectionData\ced\donation_data"

global polparty "kscm ods pirati spd stan top09 ano cssd kducsl"
global years "2023 2022 2021 2020 2019 2018 2017"


foreach p of global polparty {
    foreach y of global years {
		//capture{
			use "pp_`p'\y_`y'\vfz`y'-`p'-penizefo.dta", clear   
			append using "pp_`p'\y_`y'\vfz`y'-`p'-bupfo.dta"
					
			rename příjmení surname
			rename jméno firstname 
			rename datumnarození birthdate
			rename obec municipality
			
			gen donated_value = 0
			capture: replace donated_value = částkaKč 
			
			gen gratuitous_performance = 0
			capture: replace gratuitous_performance = hodnotaBÚPKč
			
			capture: tostring titulpřed, replace 
			capture: tostring titulza, replace 
			
			gen acadegree_an = "."
			replace acadegree_an = titulza 
			replace acadegree_an = "." if acadegree_an == ""
			
			gen acadegree_bn = "." 
			replace acadegree_bn = titulpřed if acadegree_bn != " "
			replace acadegree_bn = "." if acadegree_bn == ""
			
			capture: tostring acadegree_an, replace 
			capture: tostring acadegree_bn, replace 
				
			keep municipality acadegree_an acadegree_bn donated_value gratuitous_performance surname firstname birthdate
	
			*collapse (first) municipality acadegree_bn acadegree_an (sum) donated_value gratuitous_performance, by(surname firstname birthdate)
		
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
			
			
			*save "pp_`p'\y_`y'\all.dta", replace 
			
			if "`p'"== "kscm" & `y' == 2023 {
			save donation_data.dta, replace
			} 
		else {
			append using donation_data.dta 
			save donation_data.dta, replace
			sleep 100
		}
			
	}
}


label define polparty 1 "kducsl" 2 "kscm" 3 "ods" 4 "pirati" 5 "spd" 6 "stan" 7 "top09" 8 "ano" 9 "cssd"
label value polparty polparty



sort surname firstname birthdate polparty year 

replace acadegree_an = "." if acadegree_an == "JUDr. Ing." | acadegree_an == "JUDr." | acadegree_an == "brig. gen. v. v." | acadegree_an == "Belcredi" | acadegree_an == "RNDr." | acadegree_an == "s.r.o." | acadegree_an == "Liga spravedlnosti" | acadegree_an == "Bc." | acadegree_an == "Prchal" |  acadegree_an == "Ing. arch."
 
replace acadegree_an = "PhD." if acadegree_bn == "MVDr.  Ph.D." | acadegree_bn == "PhD." | acadegree_bn == "Mgr.et Mgr.,PhD." | acadegree_bn == "Ing.  PhD." | acadegree_bn == "Mgr. Ph.D." | acadegree_bn == "Doc. MUDr.  Ph" | acadegree_bn == "Ing. PhD." | acadegree_bn == "RNDr. Ph." | acadegree_bn == "MVDr.  Ph.D" | acadegree_bn == "Ing.  Ph." | acadegree_bn == "JUDr.  Ph.D." | acadegree_bn == "MUDr. Ph.D." | acadegree_bn == "Ing. Ph.D." | acadegree_bn == "Mgr. P.h.D." | acadegree_bn == "Ph. D." | acadegree_bn == "Ph.D." | acadegree_an == "Ph.D." | acadegree_an == "Mgr., Ph.D." | acadegree_an == "Ph.D" | acadegree_an == ", Ph.D." | acadegree_an == "Ph.D.,FICS" | acadegree_an == "Ph.D., FICS" | acadegree_an  == "Ph.D," | acadegree_an == "Ph.D.,ING-PAED IGIP" | acadegree_an == "PharmDr. Ph.D." | acadegree_an == "RNDr.,Ph.D." | acadegree_an == "PhD, MSI" 

replace acadegree_an = "MBA" if acadegree_bn == "PhDr.  MBA" | acadegree_bn == "Ing.  MBA" | acadegree_bn == "Ing. MBA" | acadegree_bn == "MUDr. MBA" |  acadegree_bn == "Ing., MBA" | acadegree_bn == "PhDr.- MBA" | acadegree_an == "MBA." | acadegree_an == ", MBA" | acadegree_an == "Ing.,MBA" | acadegree_an == "Mgr., MBA" | acadegree_an == "CFA, MBA"

replace acadegree_an = "MEPP" if acadegree_bn == "JUDr.  MEPP" | acadegree_an == "MEPP"
replace acadegree_an = "DiS." if acadegree_bn == "Dis." | acadegree_bn == "Bc. DiS." | acadegree_bn == "Mgr.  DiS." | acadegree_bn == "Dis" | acadegree_an == ", DiS." | acadegree_an == "DiS"
replace acadegree_an = "CSc." if acadegree_bn == "Ing. CSc." | acadegree_an == ", CSc." | acadegree_an == "prof. MUDr. CSc." | acadegree_an == "CSc," | acadegree_an == "CSc"
replace acadegree_an = "MSc." if acadegree_bn == "Bc. MSc." | acadegree_bn == "Bc,MSc." | acadegree_an == "MSc." | acadegree_an == "M.Sc."
replace acadegree_an = "M.A." if acadegree_bn == "M.A." | acadegree_an == "M.A." | acadegree_an == "MA"
replace acadegree_an = "B.A." if acadegree_bn == "B.A." | acadegree_an == "BA" | acadegree_an == "B.A."
replace acadegree_an = "BBA" if acadegree_bn == "BBA." | acadegree_an == "BBA"
replace acadegree_an = "MBA LL.M." if acadegree_an == "MBA, LLM" | acadegree_an == "MBA, LL.M." | acadegree_an == "LL.M., MBA" | acadegree_an == "MBA, L.L.M"
replace acadegree_an = "ThD." if acadegree_an == "Th.D."
replace acadegree_an = "LL.M." if acadegree_an == "LL.M." | acadegree_an == "LLM."
replace acadegree_an = "DrSc." if acadegree_an == "prof. MUDr., DrSc." | acadegree_an == ", DrSc." | acadegree_an == "DrSc., FICS" | acadegree_an == "DrSc."
replace acadegree_an = "MBA MSc." if acadegree_an == "MBA, MSc." 
replace acadegree_an = "PhD. LL.M." if acadegree_an == "Ph.D., LL.M." | acadegree_an == "MBA Ph.D."
replace acadegree_an = "PhD. MBA" if acadegree_an == ", Ph.D., MBA" | acadegree_an == "Ph.D., MBA" | acadegree_an == "Ph.D, MBA" | acadegree_an == "MBA, Ph.D."
replace acadegree_an = "LL.M. B.A." if acadegree_an == "BA.LLM" | acadegree_an == "BA, LL. M."
replace acadegree_an = "PhD. M.A." if acadegree_an == ", Ph.D., MA"
replace acadegree_an = "CSC. MBA" if acadegree_an == "CSc., MBA, EBIR"
replace acadegree_an = "PhD. LL.M. MBA" if acadegree_an == "Ph.D., MBA, LL.M."
replace acadegree_an = "EMLE" if acadegree_an == "E.M.L.E."
replace acadegree_an = "MPA" if acadegree_an == "MPA"
replace acadegree_an = "MBA DiS." if acadegree_an == "MBA, DIS"
replace acadegree_an = "MPA MBA" if acadegree_an == "MBA, MPA" | acadegree_an == "MPA,MBA" | acadegree_an == "MBA,MPA" | acadegree_an == "MBA, MPA" | acadegree_an == "MPA, MBE"
replace acadegree_an = "MHA" if acadegree_an == "MHA"
replace acadegree_an = "DBA" if acadegree_an == "DBA"
replace acadegree_an = "Dr." if acadegree_an == "Dr."



*Zahraniční a profesní tituly nikde soustavně popsány nejsou. Způsob psaní jejich zkratek není zcela jednotný, kolísá hlavně (ne)psaní teček a mezer po tečce. Ve většině případů tyto zkratky píšeme po čárce za příjmením. Z velkého množství zahraničních a profesních titulů zde uvádíme jen některé, např. BBA –⁠⁠⁠⁠⁠⁠⁠⁠⁠⁠ Bachelor of Business Administration (bakalář managementu), MBA –⁠⁠⁠⁠⁠⁠⁠⁠⁠⁠ Master of Business Administration (magistr managementu), BPA –⁠⁠⁠⁠⁠⁠⁠⁠⁠⁠ Bachelor of Public Administration (bakalář veřejné správy), MPA –⁠⁠⁠⁠⁠⁠⁠⁠⁠⁠ Master of Public Administration (magistr veřejné správy), B.Th. –⁠⁠⁠⁠⁠⁠⁠⁠⁠⁠ Bachelor of Theology (bakalář teologie), M.Th. –⁠⁠⁠⁠⁠⁠⁠⁠⁠⁠ Master of Theology (magistr teologie), B.A. –⁠⁠⁠⁠⁠⁠⁠⁠⁠⁠ Bachelor of Arts (bakalář humanitních věd), M.A. –⁠⁠⁠⁠⁠⁠⁠⁠⁠⁠ Master of Arts (magistr humanitních věd), BSc. –⁠⁠⁠⁠⁠⁠⁠⁠⁠⁠ Bachelor of Science (bakalář věd), MSc. –⁠⁠⁠⁠⁠⁠⁠⁠⁠⁠ Master of Science (magistr věd), BLaw, LL.B. –⁠⁠⁠⁠⁠⁠⁠⁠⁠⁠ Bachelor of Laws (bakalář práv), MLaw, LL.M. –⁠⁠⁠⁠⁠⁠⁠⁠⁠⁠ Master of Laws (magistr práv), MMed. –⁠⁠⁠⁠⁠⁠⁠⁠⁠⁠ Master of Medicine (magistr medicíny), M.D. –⁠⁠⁠⁠⁠⁠⁠⁠⁠⁠ Doctor of Medicine (doktor medicíny), Dipl.‑Ing. –⁠⁠⁠⁠⁠⁠⁠⁠⁠⁠ Diplom-Ingenieur (inženýr), EngD. –⁠⁠⁠⁠⁠⁠⁠⁠⁠⁠ Doctor of Engineering (doktor technických věd).











********************************

replace acadegree_bn = "." if acadegree_bn == "PhD." | acadegree_bn == "L." | acadegree_bn == "Paní" | acadegree_bn == "Ph. D." | acadegree_bn == "Ph.D." | acadegree_bn == "Dis" | acadegree_bn == "M.A." | acadegree_bn == "B.A."

replace acadegree_bn = "Bc." if acadegree_bn == "Bc. MSc." | acadegree_bn == "Bc. DiS." | acadegree_bn == "Bc,MSc." | acadegree_bn == "Bc" | acadegree_bn == "Bc," | acadegree_an == "Bc."

replace acadegree_bn = "Ing." if acadegree_bn == "Ing" | acadegree_bn == "Ing. CSc." | acadegree_bn == "Ing.  PhD." | acadegree_bn == "Ing.  MBA" | acadegree_bn == "Ing. PhD." | acadegree_bn == "Ing. MBA" | acadegree_bn == "Ing.  Ph." | acadegree_bn == "Ing. Ph.D."  | acadegree_bn == "Dipl. Ing." | acadegree_bn == "Ing. Lic." | acadegree_bn == "Ing., MBA" | acadegree_an == "Ing." |  acadegree_an == "Ph.D.,ING-PAED IGIP" | acadegree_an == "Ing.,MBA"
replace acadegree_bn = "Ing. Bc." if  acadegree_bn == "ing., Bc." | acadegree_bn == "Ing.Bc." | acadegree_bn == "ING-PAED IGIP Ing. Bc." 
replace acadegree_bn = "Ing. arch."  if acadegree_bn == "Ing.arch."
replace acadegree_bn = "Ing. Mgr." if acadegree_bn == "Mgr. Ing." | acadegree_bn == "Mgr.  Ing." | acadegree_bn == "Ing.  Mgr." | acadegree_bn == "Ing. Mgr." | acadegree_bn == "Ing.Mgr." | acadegree_bn == "Mgr.,Ing." | acadegree_an == "Ing. Mgr."
replace acadegree_bn = "Ing. et Ing." if acadegree_bn == "Ing. et. Ing."

replace acadegree_bn = "Mgr." if acadegree_bn == "Mgr" | acadegree_bn == "Mgr. Ph.D." | acadegree_bn == "Mgr.  DiS." | acadegree_bn == "Mgr. P.h.D." | acadegree_bn == "Mgr" | acadegree_bn == "brig.gen.v.v. Mgr." | acadegree_an == "Mgr., Ph.D." | acadegree_an == "Mgr." | acadegree_an == "Mgr., MBA"
replace acadegree_bn = "Mgr. et Mgr." if acadegree_bn == "Mgr.et Mgr.,PhD." | acadegree_bn == "Mgr.et Mgr." | acadegree_bn == "Mgr. et. Mgr."
replace acadegree_bn = "Mgr. Bc." if acadegree_bn == "Mgr, Bc." | acadegree_bn == "Mgr.Bc." | acadegree_bn == "Mgr.et.Bc." | acadegree_bn == "Mgr. Bc. (DH)"

replace acadegree_bn = "MgA." if acadegree_bn == "Mg.A."

replace acadegree_bn = "Ing. arch." if acadegree_an == "Ing. arch."

replace acadegree_bn = "Dr." if acadegree_bn == "Dr." | acadegree_bn == "Dr"
replace acadegree_bn = "Dr. Ing." if acadegree_bn == "Dr. Ing."
replace acadegree_bn = "Dr. Ing. Arch." if acadegree_bn == "Dr.Ing.Arch."

replace acadegree_bn = "MUDr." if acadegree_bn == "MUDr." | acadegree_bn == "MUDr. MBA" | acadegree_bn == "MUDr" | acadegree_an == "MUDr."
replace acadegree_bn = "MUDr. Mgr." if acadegree_bn == "MUDr.Mgr."
replace acadegree_bn = "MUDr. Ing." if acadegree_bn == "MUDr.Ing." | acadegree_bn == "MUDr. Ing." | acadegree_bn == "MUDr.  Ing."
replace acadegree_bn = "MUDr. Bc." if acadegree_bn == "MUDr. Bc."
replace acadegree_bn = "MUDr. ThMgr." if acadegree_bn == "MUDr.et ThMgr." | acadegree_bn == "MUDr. ThMgr."

replace acadegree_bn = "PaedDr." if acadegree_bn == "PaeDr." | acadegree_bn == "PaedDr"

replace acadegree_bn = "JUDr." if acadegree_bn == "JUDr" | acadegree_bn == "JUDr.  MEPP" | acadegree_bn == "JUDr.  Ph.D." | acadegree_bn == "JUDr" | acadegree_bn == "JUDr-" | acadegree_an == "JUDr."
replace acadegree_bn = "JUDr. Ing." if acadegree_bn == "JUDr.Ing." | acadegree_bn == "Ing.JUDr." | acadegree_an == "JUDr. Ing."
replace acadegree_bn = "JUDr. PhDr." if acadegree_bn == "JUDr.PhDr."
replace acadegree_bn = "JUDr. Mgr." if acadegree_bn == "JUDr. Mgr."

replace acadegree_bn = "RNDr." if acadegree_bn == "RNDr. Ph." | acadegree_bn == "Dr. rer. nat." | acadegree_an == "RNDr." | acadegree_an == "RNDr.,Ph.D."
replace acadegree_bn = "RNDr. Mgr." if acadegree_bn == "Mgr. RNDr" | acadegree_bn == "RNDr. Mgr." 

replace acadegree_bn = "PhDr." if acadegree_bn == "PhDr.  MBA" | acadegree_bn == "Ph.Dr." | acadegree_bn == "plk. gšt. v záloze PhDr." | acadegree_bn == "PhDr"  | acadegree_bn == "PhDr.- MBA"
replace acadegree_bn = "PhDr. Ing." if acadegree_bn == "PhDr. ,Ing." | acadegree_bn == "Ing.PhDr." | acadegree_bn == "PhDr. Ing." | acadegree_bn == "PhDr. ,Ing." | acadegree_bn == "Ing. PhDr."
replace acadegree_bn = "PhDr. Bc." if acadegree_bn == "PhDr.  Bc." | acadegree_bn == "PhDr. Bc."
replace acadegree_bn = "PhDr. Mgr." if acadegree_bn == "PHDr.,Mgr." | acadegree_bn == "PhDr., Mgr." | acadegree_bn == "PhDr. Mgr." | acadegree_bn == "PhDr. et Mgr."
replace acadegree_bn = "PhDr. BcA." if acadegree_bn == "PhDr.  BcA."

replace acadegree_bn = "RSDr." if acadegree_bn == "RSDr" | acadegree_bn == "RSDr."
replace acadegree_bn = "RSDr. Ing." if acadegree_bn == "RSDr Ing" | acadegree_bn == "RSDr. Ing." | acadegree_bn == "Ing. RSDr."
replace acadegree_bn = "RSDr. Mgr." if acadegree_bn == "RSDr. Mgr."

replace acadegree_bn = "PharmDr." if acadegree_bn == "PharmDr." | acadegree_an == "PharmDr. Ph.D."
replace acadegree_bn = "PharmDr. MUDr." if acadegree_bn == "PharmDr. MUDr."

replace acadegree_bn = "MVDr." if acadegree_bn == "MVDr.  Ph.D." | acadegree_bn == "MVDr.  Ph.D"

replace acadegree_bn = "MVDr." if acadegree_bn == "MVDr"

replace acadegree_bn = "doc. RNDr." if acadegree_bn == "Doc.RNDr." | acadegree_bn == "doc. RNDr."
replace acadegree_bn = "doc. MUDr." if acadegree_bn == "Doc. MUDr.  Ph" | acadegree_bn == "MUDr. Ph.D." | acadegree_bn == "Doc.MUDr."
replace acadegree_bn = "doc. PhDr." if acadegree_bn == "Doc.  PhDr." | acadegree_bn == "Doc.PhDr." | acadegree_bn == "Doc. PhDr."
replace acadegree_bn = "doc. Ing." if acadegree_bn == "doc., Ing." | acadegree_bn == "Doc. Ing."
replace acadegree_bn = "doc. Mgr." if acadegree_bn == "Doc.Mgr."
replace acadegree_bn = "doc. MgA." if acadegree_bn == "doc. MgA."
replace acadegree_bn = "doc. Dr. Ing." if acadegree_bn == "doc. Dr. Ing."
replace acadegree_bn = "doc. Dr." if acadegree_bn == "Doc. Dr."
 
replace acadegree_bn = "prof. MUDr." if acadegree_bn == "Prof.MUDr." | acadegree_bn == "prof. MUDr." | acadegree_an == "prof. MUDr., DrSc." | acadegree_an == "prof. MUDr. CSc."
replace acadegree_bn = "prof. PhDr." if acadegree_bn == "Prof, PhDr." | acadegree_bn == "prof. PhDr."  
replace acadegree_bn = "prof. Ing." if acadegree_bn == "prof. Ing."
replace acadegree_bn = "prof. PharmDr." if acadegree_bn == "Prof.PharmDr." | acadegree_bn == "prof. PharmDr."










