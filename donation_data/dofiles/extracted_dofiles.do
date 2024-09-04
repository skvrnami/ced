** chybi docistit ANO za rok 2020, TRIKOLORU 2021**

clear all 
set more off

global tituly_bn "Bc Ing.arch. Ing.Arch. Ing ThMgr Mgr PharmDr PaedDr Mgr PhDr RNDr PHDr MUDr JUDr Doc doc prof Prof MVDr"
global tituly_an "DiS MBA MSc Ph.D. CSc PH.D. PhD Ph.D" 

**** kducsl 2021 ****
import excel "C:\Users\michs\Dropbox\_research\CzechElectionData\ced\ced\ced\donation_data\primary_data_extracted\vfz2021-kducsl_edit.xlsx", sheet("Sheet1")

drop B C D E F G H I J K L M N O P Q S T U V W X Y Z AA AB

gen acadegree_an = "."
gen acadegree_bn = "." 

 foreach t of global tituly_bn {
    replace acadegree_bn = "`t'" if strpos(A, "`t'") > 0
  }

foreach t of global tituly_an {
    replace acadegree_an = "`t'" if strpos(A, "`t'") > 0
  } 
 
 
replace A = subinstr(A, " Bc.","", .)
replace A = subinstr(A, " MUDr.","", .)
replace A = subinstr(A, " JUDr.","", .)
replace A = subinstr(A, " PhDr.","", .)
replace A = subinstr(A, "Ing.arch.","", .)
replace A = subinstr(A, "Ing. arch.","", .)
replace A = subinstr(A, "Ing.Arch.","", .)
replace A = subinstr(A, "Ing. Arch.","", .)
replace A = subinstr(A, ", Ing.", "", .)
replace A = subinstr(A, " Ing.","", .)
replace A = subinstr(A, ".Ing.","", .)
replace A = subinstr(A, " Mgr.","", .)
replace A = subinstr(A, "Ing.arch.","", .)
replace A = subinstr(A, "Ing.PhD.","", .)
replace A = subinstr(A, " PH.D.","",.)
replace A = subinstr(A, " PharmDr.","",.)
replace A = subinstr(A, " PhDr. Ph.D.","",.)
replace A = subinstr(A, " Ph.D.","",.)
replace A = subinstr(A, " Dr.","",.)
replace A = subinstr(A, " Mgr.et Mgr.","",.)
replace A = subinstr(A, " Mgr-","",.)
replace A = subinstr(A, " doc. CSc","",.)
replace A = subinstr(A, " dipl.um.","",.)
replace A = subinstr(A, " PHDr.","",.)
replace A = subinstr(A, " RNDr.","",.)
replace A = subinstr(A, " doc.DDr.PdD.","",.)
replace A = subinstr(A, " Doc. JUDr. Ph.","",.)
replace A = subinstr(A, " Ing","",.)
replace A = subinstr(A, "-předseda","",.)
replace A = subinstr(A, ",, Ph.D","",.)
replace A = subinstr(A, ", Ph.D.","",.)
replace A = subinstr(A, ",,Mgr.,Ph.D","",.)
replace A = subinstr(A, ",, MBA","",.)
replace A = subinstr(A, ", Dis.","",.)
replace A = subinstr(A, ", Bc MBA","",.)
replace A = subinstr(A, ", DiS","",.)
replace A = subinstr(A, ", MBA","",.)
replace A = subinstr(A, "MBA","",.)
replace A = subinstr(A, "RNDr","",.)
replace A = subinstr(A, ",, IWE","",.)
replace A = subinstr(A, ", Dis.","",.)
replace A = subinstr(A, ", MVDr.","",.)
replace A = subinstr(A, ", Di","",.)
replace A = subinstr(A, ", RNDr.Ph.D.","",.)
replace A = subinstr(A, ", Prof.MUDr.","",.)
replace A = subinstr(A, ", ThMgr.","",.)
replace A = subinstr(A, " - krajský tajem","",.)
replace A = subinstr(A, ", et,","",.) 
replace A = subinstr(A, ", prof. PhDr.", "", .)
replace A = subinstr(A, ", RNDr.Ph.D.", "", .)
replace A = subinstr(A, ", doc. JUDr.", "" ,.)
replace A = subinstr(A, ", PhDr., BcA.", "",.)
replace A = subinstr(A, "PhD", "", .)
replace A = subinstr(A, " prof","",.)
replace A = subinstr(A, " RNDr","",.)
replace A = subinstr(A, " PaedDr", "", .)
replace A = subinstr(A, " doc", "", .)
replace A = subinstr(A, " Doc", "", .)
replace A = subinstr(A, " BcA","", .)
replace A = subinstr(A, ",Ph.D.", "", .)
replace A = subinstr(A, "  Mgr", "", .)
replace A = subinstr(A, "et","",.) if strpos(A,"Petr") == 0 & A != "Petra" & A != "Břetislav" & A != "Peter" & A != "Iveta" & A != "Iveta" & A != "Jetelinová" & A != "Kajetán" & A != "Žaneta" & A != "Bernadetta" & A != "Yveta" & A != "Aneta" & A != "Jeanette" & A != "Petros" & A != "Yvette" & A != "Elisabeth" & A != "Betty" & A != "Svetozar" & A != "Jiří Metod"  & A != "Elizabet" & A != "Petri" & A != "Petruše" & A != "Marketa"  & A != "Petr Felix" & A != "Yweta" & A != "Karel" & A != "Kvetoslava"
replace A = subinstr(A, "MUDr ","",.)
replace A = subinstr(A, "MUDr","",.)
replace A = subinstr(A, "Bc ","",.)
replace A = subinstr(A, "Mgr","",.)
replace A = subinstr(A, "Ing","",.)
replace A = subinstr(A, ".","", .)
replace A = subinstr(A, ",","", .)
replace A = subinstr(A, ",,","", .)
replace A = subinstr(A, "PhD", "", .)
replace A = subinstr(A, "Bc", "", .)
replace A = subinstr(A, "CSc", "", .)
replace A = subinstr(A, "MSc", "", .)
replace A = subinstr(A, ",","", .)
replace A = subinstr(A, ",,","", .)
replace A = subinstr(A, "MVDr", "", .)


gen financial = 0
replace financial = 1 if strpos(AC, "peněžitý dar") > 0
replace AC = subinstr(AC, "peněžitý dar","", .)
replace AC = subinstr(AC, ",00","", .)
replace AC = subinstr(AC, ",60","", .)
replace AC = subinstr(AC, " ","", .)
destring AC, replace 

split A, parse(" ") gen(name)

rename name1 surname 
rename name2 firstname

drop A name3 name4

rename R birthdate

gen birhtdate_f = 0
replace birhtdate_f = 1 if strpos(birthdate, ".") > 0

gen birthdate_aux = "." 
replace birthdate_aux = birthdate if birhtdate_f == 1
replace birthdate = "." if birhtdate_f == 1

destring birthdate, replace
replace birthdate = birthdate - 21916
format birthdate %td

gen birthdate_aux2 = date(birthdate_aux, "DMY")
replace birthdate = birthdate_aux2 if birhtdate_f == 1

gen financial_donation = 0
gen nonfinancial_donation = 0

replace financial_donation = AC if financial == 1
replace nonfinancial_donation = AC if financial == 0

drop birhtdate_f birthdate_aux birthdate_aux2 AC



gen i = 1
collapse (sum) i financial_donation nonfinancial_donation  ,by(surname firstname birthdate)

gen polparty = 1 
gen year = 2021

save "C:\Users\michs\Dropbox\_research\CzechElectionData\ced\ced\ced\donation_data\primary_data_extracted\vfz2021-kducsl.dta", replace 


**** spd 2018 ****
import excel "C:\Users\michs\Dropbox\_research\CzechElectionData\ced\ced\ced\donation_data\primary_data_extracted\vfz2018-spd_edit.xlsx", sheet("Sheet1") firstrow clear

drop B C D E F G H I J L M N O P Q R T U V W X Z AA AB AC AD AE

rename VvšeneněžitéhodaruvKč financial_donation
rename A firstname
rename K surname 
rename Datumnarozeníneboidentifikačn birthdate

gen nonfinancial_donation = 0
 
gen birhtdate_f = 0
replace birhtdate_f = 1 if strpos(birthdate, ".") > 0
gen birthdate_aux = "." 
replace birthdate_aux = birthdate if birhtdate_f == 1
replace birthdate = "." if birhtdate_f == 1

destring birthdate, replace
replace birthdate = birthdate - 21916
format birthdate %td

gen birthdate_aux2 = date(birthdate_aux, "DMY")
replace birthdate = birthdate_aux2 if birhtdate_f == 1

drop birhtdate_f birthdate_aux birthdate_aux2


replace firstname = "Patrik" if firstname == "Patrík"
replace surname = "Bojko" if surname == "Boiko"
replace surname = "Bojková" if surname == "Boiková"
replace firstname = "Darja" if firstname == "Daria"
replace surname = "Běrský"  if surname == "Běrskv" 
replace surname = "Hyťhová" if surname == "Hvťhová"
replace surname = "Hlaváč" if surname == "I-Ilaváč"
replace surname = "Krejcarová" if surname == "Kr icarová"
replace surname = "Krejcarová" if surname == "Kreicarová"
replace surname = "Mikulaj" if surname == "Mikulai"
replace firstname = "Richard" if firstname == "Ríchard"
replace surname = "Měchura" if surname == "Mt!chura"
replace surname = "Osmančíková" if surname == "Osmančiková"
replace surname = "Owczarzy"  if surname == "Owczarzv"
replace firstname = "Jarmila" if firstname == "Jannila"
replace surname = "Sukup" if surname == "Sukuo"
replace firstname = "Pynelopi" if firstname == "Pvnelooi"
replace firstname = "Michal" if firstname == "Míchal"
replace surname = "Vrzáň" if surname == "Vrz.áň"
replace surname = "Burčo" if surname == "burčo"
replace surname = "Pytlíková" if surname == "Pvtlíková"
replace surname = "Široký" if surname == "Širokv"
replace surname = "Fučík" if surname == "Fučile"
replace surname = "Koždoň" if surname == "Koždo11"
replace surname = "Pustějovský" if surname == "Pustěiovský" | surname == "Pust jovský"
replace firstname = "Robe1i" if surname == "Robert"
replace surname = "Holík" if surname == "Holik"
replace surname = "Fučík" if surname == "Fucik" | surname == "Fučik" | surname == "Fučlk"


gen i = 1
collapse (sum) i financial_donation nonfinancial_donation  ,by(surname firstname birthdate)

gen polparty = 5
gen year = 2018

save "C:\Users\michs\Dropbox\_research\CzechElectionData\ced\ced\ced\donation_data\primary_data_extracted\vfz2018-spd.dta", replace 


**** spd 2019 ****
import excel "C:\Users\michs\Dropbox\_research\CzechElectionData\ced\ced\ced\donation_data\primary_data_extracted\vfz2019-spd_edit.xlsx", sheet("Sheet1") clear

drop B C D F G H I J L M N O P Q R S T

rename A firstname
rename E surname
rename U financial_donation
rename K birthdate
gen nonfinancial_donation = 0


gen birhtdate_f = 0
replace birhtdate_f = 1 if strpos(birthdate, ".") > 0
gen birthdate_aux = "." 
replace birthdate_aux = birthdate if birhtdate_f == 1
replace birthdate = "." if birhtdate_f == 1

destring birthdate, replace
replace birthdate = birthdate - 21916
format birthdate %td

gen birthdate_aux2 = date(birthdate_aux, "DMY")
replace birthdate = birthdate_aux2 if birhtdate_f == 1

drop birhtdate_f birthdate_aux birthdate_aux2

replace firstname = "Patrik" if firstname == "Patrík"
replace surname = "Bojko" if surname == "Boiko"
replace surname = "Bojková" if surname == "Boiková"
replace firstname = "Darja" if firstname == "Daria"
replace surname = "Běrský"  if surname == "Běrskv" 
replace surname = "Hyťhová" if surname == "Hvťhová"
replace surname = "Hlaváč" if surname == "I-Ilaváč"
replace surname = "Krejcarová" if surname == "Kr icarová"
replace surname = "Krejcarová" if surname == "Kreicarová"
replace surname = "Mikulaj" if surname == "Mikulai"
replace firstname = "Richard" if firstname == "Ríchard"
replace surname = "Měchura" if surname == "Mt!chura"
replace surname = "Osmančíková" if surname == "Osmančiková"
replace surname = "Owczarzy"  if surname == "Owczarzv"
replace firstname = "Jarmila" if firstname == "Jannila"
replace surname = "Sukup" if surname == "Sukuo"
replace firstname = "Pynelopi" if firstname == "Pvnelooi"
replace firstname = "Michal" if firstname == "Míchal"
replace surname = "Vrzáň" if surname == "Vrz.áň"
replace surname = "Burčo" if surname == "burčo"
replace surname = "Pytlíková" if surname == "Pvtlíková"
replace surname = "Široký" if surname == "Širokv"
replace surname = "Fučík" if surname == "Fučile"
replace surname = "Koždoň" if surname == "Koždo11"
replace surname = "Pustějovský" if surname == "Pustěiovský" | surname == "Pust jovský"
replace firstname = "Robe1i" if surname == "Robert"
replace surname = "Fučík" if surname == "Fucik" | surname == "Fučik" | surname == "Fučlk"
replace surname = "Holík" if surname == "Holik"

gen i = 1
collapse (sum) i financial_donation nonfinancial_donation  ,by(surname firstname birthdate)


gen polparty = 5
gen year = 2019

save "C:\Users\michs\Dropbox\_research\CzechElectionData\ced\ced\ced\donation_data\primary_data_extracted\vfz2019-spd.dta", replace 



**** spd 2020 ****
import excel "C:\Users\michs\Dropbox\_research\CzechElectionData\ced\ced\ced\donation_data\primary_data_extracted\vfz2020-spd_edit.xlsx", sheet("Sheet1") clear

drop B C D E G H I J K M N 

rename A firstname
rename F surname
rename O financial_donation
rename L birthdate
gen nonfinancial_donation = 0


gen birhtdate_f = 0
replace birhtdate_f = 1 if strpos(birthdate, ".") > 0
gen birthdate_aux = "." 
replace birthdate_aux = birthdate if birhtdate_f == 1
replace birthdate = "." if birhtdate_f == 1

destring birthdate, replace
replace birthdate = birthdate - 21916
format birthdate %td

gen birthdate_aux2 = date(birthdate_aux, "DMY")
replace birthdate = birthdate_aux2 if birhtdate_f == 1

drop birhtdate_f birthdate_aux birthdate_aux2

replace firstname = "Patrik" if firstname == "Patrík"
replace surname = "Bojko" if surname == "Boiko"
replace surname = "Bojková" if surname == "Boiková"
replace firstname = "Darja" if firstname == "Daria"
replace surname = "Běrský"  if surname == "Běrskv" 
replace surname = "Hyťhová" if surname == "Hvťhová"
replace surname = "Hlaváč" if surname == "I-Ilaváč"
replace surname = "Krejcarová" if surname == "Kr icarová"
replace surname = "Krejcarová" if surname == "Kreicarová"
replace surname = "Mikulaj" if surname == "Mikulai"
replace firstname = "Richard" if firstname == "Ríchard"
replace surname = "Měchura" if surname == "Mt!chura"
replace surname = "Osmančíková" if surname == "Osmančiková"
replace surname = "Owczarzy"  if surname == "Owczarzv"
replace firstname = "Jarmila" if firstname == "Jannila"
replace surname = "Sukup" if surname == "Sukuo"
replace firstname = "Pynelopi" if firstname == "Pvnelooi"
replace firstname = "Michal" if firstname == "Míchal"
replace surname = "Vrzáň" if surname == "Vrz.áň"
replace surname = "Burčo" if surname == "burčo"
replace surname = "Pytlíková" if surname == "Pvtlíková"
replace surname = "Široký" if surname == "Širokv"
replace surname = "Fučík" if surname == "Fučile"
replace surname = "Koždoň" if surname == "Koždo11"
replace surname = "Pustějovský" if surname == "Pustěiovský" | surname == "Pust jovský"
replace firstname = "Robe1i" if surname == "Robert"
replace firstname = "Milada" if firstname == "Milada      '"

replace surname = "Holík" if surname == "Holik"
replace surname = "Fučík" if surname == "Fucik" | surname == "Fučik" | surname == "Fučlk"
replace birthdate = td(30nov1962) if birthdate == td(30jan1962) & surname == "Turoň"

sort surname firstname birthdate

gen i = 1
collapse (sum) i financial_donation nonfinancial_donation  ,by(surname firstname birthdate)

gen polparty = 5
gen year = 2020

save "C:\Users\michs\Dropbox\_research\CzechElectionData\ced\ced\ced\donation_data\primary_data_extracted\vfz2020-spd.dta", replace 


**** spd 2021 ****
import excel "C:\Users\michs\Dropbox\_research\CzechElectionData\ced\ced\ced\donation_data\primary_data_extracted\vfz2021-spd_edit.xlsx", sheet("Sheet1") firstrow  clear 

drop D E F G H I J L M N O P Q S T U V W X Y 

rename Jméno firstname
rename Příjmení surname
rename VýšepeněžitéhodaruvKč financial_donation
rename Datwnnarozeníneboidentifikačn birthdate
gen nonfinancial_donation = 0

replace financial_donation = "33333.33" if financial_donation == "33333,33" 
replace birthdate = birthdate - 21916
format birthdate %td
destring financial_donation, replace 

replace firstname = "llia" if firstname == "Ilja"
replace surname = "Fila" if surname == "Fiala" & birthdate == td(26dec1969)
replace surname = "Kunay" if surname == "Kunav"
replace firstname = "Miloslav" if firstname == "Miroslav" & birthdate == td(15jun1966)
replace surname = "Tykvart" if surname == "Tvkvart"

sort surname firstname birthdate

gen i = 1
collapse (sum) i financial_donation nonfinancial_donation  ,by(surname firstname birthdate)

gen polparty = 5
gen year = 2021

save "C:\Users\michs\Dropbox\_research\CzechElectionData\ced\ced\ced\donation_data\primary_data_extracted\vfz2021-spd.dta", replace 


**** spd 2022 ****
import excel "C:\Users\michs\Dropbox\_research\CzechElectionData\ced\ced\ced\donation_data\primary_data_extracted\vfz2022-spd_edit.xlsx", sheet("Sheet1") firstrow  clear 


keep Částka Jméno Příjmení Datumnarození 
drop if Částka == .

rename Jméno firstname
rename Příjmení surname
rename Částka financial_donation
rename Datumnarození birthdate
*gen nonfinancial_donation = 0


gen birhtdate_f = 0
replace birhtdate_f = 1 if strpos(birthdate, ".") > 0
gen birthdate_aux = "." 
replace birthdate_aux = birthdate if birhtdate_f == 1
replace birthdate = "." if birhtdate_f == 1

destring birthdate, replace
replace birthdate = birthdate - 21916
format birthdate %td

gen birthdate_aux2 = date(birthdate_aux, "DMY")
replace birthdate = birthdate_aux2 if birhtdate_f == 1

drop birhtdate_f birthdate_aux birthdate_aux2

replace firstname = "llia" if firstname == "Ilja"
replace birthdate = td(19jul1970) if birthdate == td(19oct1970)
replace firstname = "Hana" if firstname == "Benešová" & birthdate == td(07jul1965)
replace surname = "Stuhlík" if surname == "Stuchlík"
replace birthdate = td(29may1964) if birthdate == td(29may2022) & surname == "Zlínský"

sort surname firstname birthdate


gen i = 1
collapse (sum) i financial_donation  ,by(surname firstname birthdate)

gen polparty = 5

gen year = 2022

save "C:\Users\michs\Dropbox\_research\CzechElectionData\ced\ced\ced\donation_data\primary_data_extracted\vfz2022-spd_financial.dta", replace 


**** spd 2022 - non-financial donation ****
import excel "C:\Users\michs\Dropbox\_research\CzechElectionData\ced\ced\ced\donation_data\primary_data_extracted\vfz2022-spd_BEZUPLATNE_edit.xlsx", sheet("Sheet1")  clear 

keep E N V Z

rename E firstname
rename N surname
rename Z nonfinancial_donation
rename V birthdate
*gen financial_donation = 0


gen birhtdate_f = 0
replace birhtdate_f = 1 if strpos(birthdate, ".") > 0
gen birthdate_aux = "." 
replace birthdate_aux = birthdate if birhtdate_f == 1
replace birthdate = "." if birhtdate_f == 1

destring birthdate, replace
replace birthdate = birthdate - 21916
format birthdate %td

gen birthdate_aux2 = date(birthdate_aux, "DMY")
replace birthdate = birthdate_aux2 if birhtdate_f == 1

drop birhtdate_f birthdate_aux birthdate_aux2

gen i = 1

collapse (sum) i  nonfinancial_donation  ,by(surname firstname birthdate)

gen polparty = 5

save "C:\Users\michs\Dropbox\_research\CzechElectionData\ced\ced\ced\donation_data\primary_data_extracted\vfz2022-spd_nonfinancial.dta", replace 


merge 1:1 surname firstname birthdate using "C:\Users\michs\Dropbox\_research\CzechElectionData\ced\ced\ced\donation_data\primary_data_extracted\vfz2022-spd_financial.dta"

replace financial_donation = 0 if financial_donation == .
replace nonfinancial_donation = 0 if nonfinancial_donation == .

sort surname firstname birthdate

save "C:\Users\michs\Dropbox\_research\CzechElectionData\ced\ced\ced\donation_data\primary_data_extracted\vfz2022-spd.dta", replace 


**** spd 2023 ****
import excel "C:\Users\michs\Dropbox\_research\CzechElectionData\ced\ced\ced\donation_data\primary_data_extracted\vfz2023-spd_edit.xlsx", sheet("Sheet1")  clear 

keep F AC V M

rename M firstname
rename V surname
rename F financial_donation
rename AC birthdate
gen nonfinancial_donation = 0

drop if surname == ""

gen birhtdate_f = 0
replace birhtdate_f = 1 if strpos(birthdate, ".") > 0
gen birthdate_aux = "." 
replace birthdate_aux = birthdate if birhtdate_f == 1
replace birthdate = "." if birhtdate_f == 1

replace birthdate_aux = "02.10.1964" if birthdate_aux == "I 2.10.1964"
replace birthdate_aux = "18.01.1976" if birthdate_aux == "18.0l.1976"
replace birthdate_aux = "08.11.1958" if birthdate_aux == "08.l 1.1958"

destring birthdate, replace
replace birthdate = birthdate - 21916
format birthdate %td

gen birthdate_aux2 = date(birthdate_aux, "DMY")
replace birthdate = birthdate_aux2 if birhtdate_f == 1

drop birhtdate_f birthdate_aux birthdate_aux2

replace firstname = "Vladimíra" if firstname == "Vladirrúra"
replace surname = "Fučík" if surname == "Fučile"
replace surname = "Kopecký" if surname == "Kooecký"
replace firstname = "Jiří" if firstname == "Jiií"
replace surname = "Kysling" if surname == "Kvsling"
replace birthdate = td(12oct1964) if birthdate == td(02oct1964) & surname == "Melzerová"
replace surname = "Wisiorek" if surname == "Wísiorek"
replace firstname = "Michal" if firstname == "Míchal"


gen i = 1
collapse (sum) i financial_donation nonfinancial_donation  ,by(surname firstname birthdate)

gen polparty = 5
gen year = 2023

save "C:\Users\michs\Dropbox\_research\CzechElectionData\ced\ced\ced\donation_data\primary_data_extracted\vfz2023-spd.dta", replace 


**** ano 2021 ****
import excel "C:\Users\michs\Dropbox\_research\CzechElectionData\ced\ced\ced\donation_data\primary_data_extracted\vfz2021-ano_edit.xlsx", sheet("Sheet1") firstrow clear 

split Jménopříjmeníneboobchodnífi, parse(", ") gen(name)

keep name*  Jménopříjmeníneboobchodnífi Datumnarozeníneboidentifikač Výšepeněžitéhodaruneboobvykl T

rename Datumnarozeníneboidentifikač birthdate

gen financial_donation = Výšepeněžitéhodaruneboobvykl 
replace financial_donation = T if T != ""

replace financial_donation = subinstr(financial_donation, "peněžitý dar","", .)
replace financial_donation = subinstr(financial_donation, "peněžitý","", .)
replace financial_donation = subinstr(financial_donation, "dar","", .)
replace financial_donation = subinstr(financial_donation, "I","1", .)
replace financial_donation = subinstr(financial_donation, ",00","", .)
replace financial_donation = subinstr(financial_donation, "O","0", .)
replace financial_donation = subinstr(financial_donation, "       -","",.)
replace financial_donation = subinstr(financial_donation, "   ","",.)
replace financial_donation = subinstr(financial_donation, "  ","",.)
replace financial_donation = subinstr(financial_donation, " ","",.)

destring financial_donation, replace


gen birhtdate_f = 0
replace birhtdate_f = 1 if strpos(birthdate, ".") > 0
gen birthdate_aux = "." 
replace birthdate_aux = birthdate if birhtdate_f == 1
replace birthdate = "." if birhtdate_f == 1
replace birthdate = subinstr(birthdate, " ","",.)


destring birthdate, replace
replace birthdate = birthdate - 21916
format birthdate %td

gen birthdate_aux2 = date(birthdate_aux, "DMY")
replace birthdate = birthdate_aux2 if birhtdate_f == 1

drop birhtdate_f birthdate_aux birthdate_aux2 Výšepeněžitéhodaruneboobvykl T

rename name1 surname
rename name2 firstname
replace surname = subinstr(surname, "  "," ",.)
replace firstname = subinstr(firstname, "  "," ",.)

replace firstname = "Lubomír" if surname == "Brož, Lubomír                 "
replace surname = "Brož" if surname == "Brož, Lubomír                 " 

replace firstname = "Marcela" if surname == "Mrózková Heříková"
replace surname = "Heříková" if surname == "Mrózková Heříková"

drop if firstname == "spol. s r.o."
replace firstname = "Vladimír" if surname == "Slávka"
replace firstname = "Irena" if firstname == "Irena Pálková"
replace firstname = subinstr(firstname, " ","",.)
replace firstname = "Vasil Silvestr" if firstname == "VasilSilvestr"

gen nonfinancial_donation = 0

keep firstname surname birthdate financial_donation nonfinancial_donation


gen i = 1
collapse  (sum) i financial_donation nonfinancial_donation  ,by(surname firstname birthdate)

gen year = 2021
gen polparty = 8

save "C:\Users\michs\Dropbox\_research\CzechElectionData\ced\ced\ced\donation_data\primary_data_extracted\vfz2021-ano.dta", replace 


*** trikolora 2021 ***
import excel "C:\Users\michs\Dropbox\_research\CzechElectionData\ced\ced\ced\donation_data\primary_data_extracted\vfz2021-trikolora_edit.xlsx", sheet("Sheet1") firstrow clear 

gen acadegree_bn = "."
gen acadegree_an = "."

 foreach t of global tituly_bn {
    replace acadegree_bn = "`t'" if strpos(Jménopříjmeníneboobchodnífi, "`t'") > 0
	replace Jménopříjmeníneboobchodnífi = subinstr(Jménopříjmeníneboobchodnífi, "`t'","",.)
  }

foreach t of global tituly_an {
    replace acadegree_an = "`t'" if strpos(Jménopříjmeníneboobchodnífi, "`t'") > 0
	replace Jménopříjmeníneboobchodnífi = subinstr(Jménopříjmeníneboobchodnífi, "`t'","",.)
  } 
  
  split Jménopříjmeníneboobchodnífi, parse(", ") gen(name)
  
  
rename name1 surname
rename name2 firstname
replace surname = subinstr(surname, "  "," ",.)
replace firstname = subinstr(firstname, "  "," ",.)
replace surname = subinstr(surname, ",","",.)
replace firstname = subinstr(firstname, ",","",.)
replace surname = subinstr(surname, ".","",.)
replace firstname = subinstr(firstname, ".","",.)

drop B C D E F G H I J K L M N O P Q R S U V W X Y Z AA AB AC AD AE AF AG AH AI

gen financial = 0
replace financial = 1 if strpos(Výšepeněžitéhodaruneboobvykl, "peněžitý dar") > 0

gen financial_donation = Výšepeněžitéhodaruneboobvykl if financial == 1

replace financial_donation = subinstr(financial_donation, "peněžitý dar","", .)
replace financial_donation = subinstr(financial_donation, "peněžitý","", .)
replace financial_donation = subinstr(financial_donation, "dar","", .)
replace financial_donation = subinstr(financial_donation, "I","1", .)
replace financial_donation = subinstr(financial_donation, ",00","", .)
replace financial_donation = subinstr(financial_donation, "O","0", .)
replace financial_donation = subinstr(financial_donation, "       -","",.)
replace financial_donation = subinstr(financial_donation, "   ","",.)
replace financial_donation = subinstr(financial_donation, "  ","",.)
replace financial_donation = subinstr(financial_donation, " ","",.)
replace financial_donation = subinstr(financial_donation, ",",".",.)

destring financial_donation, replace

replace financial_donation = 0 if financial_donation == .

****
gen nonfinancial_donation = Výšepeněžitéhodaruneboobvykl if financial == 0

replace nonfinancial_donation = subinstr(nonfinancial_donation, "bezúplatné plnění","", .)
replace nonfinancial_donation = subinstr(nonfinancial_donation, "dar","", .)
replace nonfinancial_donation = subinstr(nonfinancial_donation, "I","1", .)
replace nonfinancial_donation = subinstr(nonfinancial_donation, ",00","", .)
replace nonfinancial_donation = subinstr(nonfinancial_donation, "O","0", .)
replace nonfinancial_donation = subinstr(nonfinancial_donation, "       -","",.)
replace nonfinancial_donation = subinstr(nonfinancial_donation, "   ","",.)
replace nonfinancial_donation = subinstr(nonfinancial_donation, "  ","",.)
replace nonfinancial_donation = subinstr(nonfinancial_donation, " ","",.)
replace nonfinancial_donation = subinstr(nonfinancial_donation, ",",".",.)

destring nonfinancial_donation, replace

replace nonfinancial_donation = 0 if nonfinancial_donation == .

replace surname = subinstr(surname, "  "," ",.)
replace firstname = subinstr(firstname, "  "," ",.)
replace firstname = subinstr(firstname, " ","",.)

rename Datumnarozeníneboidentifikačn birthdate

gen birhtdate_f = 0
replace birhtdate_f = 1 if strpos(birthdate, ".") > 0
gen birthdate_aux = "." 
replace birthdate_aux = birthdate if birhtdate_f == 1
replace birthdate = "." if birhtdate_f == 1


destring birthdate, replace
replace birthdate = birthdate - 21916
format birthdate %td

gen birthdate_aux2 = date(birthdate_aux, "DMY")
replace birthdate = birthdate_aux2 if birhtdate_f == 1

replace firstname = "Jitka" if firstname == "JIitka"
replace birthdate = td(27mar1971) if birthdate == td(27mar1981) & surname == "Cvejn"
replace surname = "Fošumová" if surname == "Fošunová" & birthdate == td(26nov1966)
replace surname = "Hanák" if surname == "Hanák3"
replace birthdate = td(21jul1979) if birthdate == td(27jul1979) & surname == "Hájek"
replace birthdate = td(14oct1970) if birthdate == td(14oct1971) & surname == "Jelínek"
replace surname = "Kříž" if surname == "KRIZ" 
replace firstname = "David" if firstname == "DAVID"
replace surname = "Kašný" if surname == "KAŠNÝ"
replace birthdate = td(12nov1984) if birthdate == td(11dec1984) & surname == "Křivka"
replace surname = "Lorenz" if surname == "LORENZ"


gen i = 1
collapse  (sum) i financial_donation nonfinancial_donation  ,by(surname firstname birthdate)

gen polparty = 11
gen year = 2021

save "C:\Users\michs\Dropbox\_research\CzechElectionData\ced\ced\ced\donation_data\primary_data_extracted\vfz2021-trikolora.dta", replace 


