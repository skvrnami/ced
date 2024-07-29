use donation_data.dta, clear 


label define polparty 1 "kducsl" 2 "kscm" 3 "ods" 4 "pirati" 5 "spd" 6 "stan" 7 "top09" 8 "ano" 9 "cssd"
label values polparty polparty

sort polparty surname firstname birthdate year

gen donated_value = 0
replace donated_value = částkaKč 

gen acadegree_an = "."
replace acadegree_an = titulza 
replace acadegree_an = "." if acadegree_an == ""

gen acadegree_bn = "." 
replace acadegree_bn = titulpřed if acadegree_bn != " "
replace acadegree_bn = "." if acadegree_bn == ""

tostring acadegree_an, replace 
tostring acadegree_bn, replace 

keep acadegree_an acadegree_bn donated_value surname year firstname birthdate polparty
	
				
*replace donated_value = 0 if donated_value == . 
*replace gratuitous_performance = 0 if gratuitous_performance == .

*g donation_all = donated_value + gratuitous_performance

*gen dropet = 0
*replace dropet = 1 if strpos(firstname, "et") > 0

replace firstname = "Jiří" if firstname == "jiří"
replace firstname = "Štěpánka" if firstname == "Štef."
replace firstname = "Ignác" if firstname == "ignác"
replace firstname = "Petr" if firstname == "petr"
replace firstname = "Petra" if firstname == "petra"
replace firstname = "Pavlína" if firstname == "Pavlína Šetková"
replace surname = "Németh" if firstname == "Németh" & birthdate == td(24aug1959)
replace firstname = "Karel" if surname == "Németh" & birthdate == td(24aug1959)
replace surname = "Cvetan" if firstname == "Cvetan" & birthdate == td(17sep1976)
replace firstname = "Martin" if surname == "Cvetan" & birthdate == td(17sep1976)
replace surname = "Tetík" if firstname == "Tetík" & birthdate == td(22jul1974)
replace firstname = "Rudolf" if surname ==  "Tetík" & birthdate == td(22jul1974)
replace surname = "Petira" if firstname == "Petira" & birthdate == td(20nov1941)
replace firstname = "Václav" if surname == "Petira" & birthdate == td(20nov1941)
replace surname = "Lačňák" if firstname == "Lačňák" & birthdate == td(22apr1971)
replace firstname = "Jan, Mgr., MBA" if surname == "Lačňák" & birthdate == td(22apr1971)
replace surname = "Poukar" if firstname == "Poukar" & birthdate == td(02jun1955)
replace firstname = "Jan" if surname == "Poukar" & birthdate == td(02jun1955)
replace surname = "Staněk" if firstname == "Staněk" & birthdate == td(06dec1940)
replace firstname = "Jan" if surname == "Staněk" & birthdate == td(06dec1940)
replace surname = "Zugar" if firstname == "Zugar" & birthdate == td(06apr1987)
replace firstname = "Jan" if surname == "Zugar" & birthdate == td(06apr1987)

drop if surname == "anonym" | surname == "test" | surname == "Test" | surname == "stojkov"

global tituly_bn "Bc Ing.arch. Ing.Arch. Ing ThMgr Mgr PharmDr PaedDr Mgr PhDr PHDr MUDr JUDr Doc doc prof Prof"
global tituly_an "MBA MSc Ph.D. PH.D."

 foreach t of global tituly_bn {
    replace acadegree_bn = "`t'" if strpos(firstname, "`t'") > 0
	replace acadegree_bn = "`t'" if strpos(surname, "`t'") > 0
  }

foreach t of global tituly_an {
    replace acadegree_an = "`t'" if strpos(firstname, "`t'") > 0
	replace acadegree_an = "`t'" if strpos(surname, "`t'") > 0
  } 
 
replace firstname = "Robert" if surname == "Huneš" & birthdate == td(09nov1968)
replace firstname = "Pavel" if firstname == "Pavel,MVDr., Ph.D, M"


replace firstname = subinstr(firstname, " Bc.","", .)
replace firstname = subinstr(firstname, " MUDr.","", .)
replace firstname = subinstr(firstname, " JUDr.","", .)
replace firstname = subinstr(firstname, " PhDr.","", .)
replace firstname = subinstr(firstname, "Ing.arch.","", .)
replace firstname = subinstr(firstname, "Ing. arch.","", .)
replace firstname = subinstr(firstname, "Ing.Arch.","", .)
replace firstname = subinstr(firstname, "Ing. Arch.","", .)
replace firstname = subinstr(firstname, ", Ing.", "", .)
replace firstname = subinstr(firstname, " Ing.","", .)
replace firstname = subinstr(firstname, ".Ing.","", .)
replace firstname = subinstr(firstname, " Mgr.","", .)
replace firstname = subinstr(firstname, "Ing.arch.","", .)
replace firstname = subinstr(firstname, "Ing.PhD.","", .)
replace firstname = subinstr(firstname, " PH.D.","",.)
replace firstname = subinstr(firstname, " PharmDr.","",.)
replace firstname = subinstr(firstname, " PhDr. Ph.D.","",.)
replace firstname = subinstr(firstname, " Ph.D.","",.)
replace firstname = subinstr(firstname, " Dr.","",.)
replace firstname = subinstr(firstname, " Mgr.et Mgr.","",.)
replace firstname = subinstr(firstname, " Mgr-","",.)
replace firstname = subinstr(firstname, " doc. CSc","",.)
replace firstname = subinstr(firstname, " dipl.um.","",.)
replace firstname = subinstr(firstname, " PHDr.","",.)
replace firstname = subinstr(firstname, " RNDr.","",.)
replace firstname = subinstr(firstname, " doc.DDr.PdD.","",.)
replace firstname = subinstr(firstname, " Doc. JUDr. Ph.","",.)
replace firstname = subinstr(firstname, " Ing","",.)
replace firstname = subinstr(firstname, "-předseda","",.)
replace firstname = subinstr(firstname, ",, Ph.D","",.)
replace firstname = subinstr(firstname, ", Ph.D.","",.)
replace firstname = subinstr(firstname, ",,Mgr.,Ph.D","",.)
replace firstname = subinstr(firstname, ",, MBA","",.)
replace firstname = subinstr(firstname, ", Dis.","",.)
replace firstname = subinstr(firstname, ", Bc MBA","",.)
replace firstname = subinstr(firstname, ", DiS","",.)
replace firstname = subinstr(firstname, ", MBA","",.)
replace firstname = subinstr(firstname, "MBA","",.)
replace firstname = subinstr(firstname, ",, IWE","",.)
replace firstname = subinstr(firstname, ", Dis.","",.)
replace firstname = subinstr(firstname, ", MVDr.","",.)
replace firstname = subinstr(firstname, ", Di","",.)
replace firstname = subinstr(firstname, ", RNDr.Ph.D.","",.)
replace firstname = subinstr(firstname, ", Prof.MUDr.","",.)
replace firstname = subinstr(firstname, ", ThMgr.","",.)
replace firstname = subinstr(firstname, " - krajský tajem","",.)
replace firstname = subinstr(firstname, ", et,","",.) 
replace firstname = subinstr(firstname, ", prof. PhDr.", "", .)
replace firstname = subinstr(firstname, ", RNDr.Ph.D.", "", .)
replace firstname = subinstr(firstname, ", doc. JUDr.", "" ,.)
replace firstname = subinstr(firstname, ", PhDr., BcA.", "",.)
replace firstname = subinstr(firstname, "PhD", "", .)
replace firstname = subinstr(firstname, " prof","",.)
replace firstname = subinstr(firstname, " RNDr","",.)
replace firstname = subinstr(firstname, " PaedDr", "", .)
replace firstname = subinstr(firstname, " doc", "", .)
replace firstname = subinstr(firstname, " Doc", "", .)
replace firstname = subinstr(firstname, " BcA","", .)
replace firstname = subinstr(firstname, ",Ph.D.", "", .)
replace firstname = subinstr(firstname, "  Mgr", "", .)
replace firstname = subinstr(firstname, "et","",.) if strpos(firstname,"Petr") == 0 & firstname != "Petra" & firstname != "Břetislav" & firstname != "Peter" & firstname != "Iveta" & firstname != "Iveta" & firstname != "Jetelinová" & firstname != "Kajetán" & firstname != "Žaneta" & firstname != "Bernadetta" & firstname != "Yveta" & firstname != "Aneta" & firstname != "Jeanette" & firstname != "Petros" & firstname != "Yvette" & firstname != "Elisabeth" & firstname != "Betty" & firstname != "Svetozar" & firstname != "Jiří Metod"  & firstname != "Elizabet" & firstname != "Petri" & firstname != "Petruše" & firstname != "Marketa"  & firstname != "Petr Felix" & firstname != "Yweta" & firstname != "Karel" & firstname != "Kvetoslava"
replace firstname = subinstr(firstname, "MUDr ","",.)
replace firstname = subinstr(firstname, "MUDr","",.)
replace firstname = subinstr(firstname, "Bc ","",.)
replace surname = subinstr(surname, " MUDr.", "", .)
replace surname = subinstr(surname, ", Ph.D.", "", .)
replace firstname = subinstr(firstname, "Mgr","",.)
replace firstname = subinstr(firstname, "Ing","",.)

replace firstname = subinstr(firstname, " ","", .) if surname == "Celý" | surname == "Carbolová" | surname == "Giacintov"
replace firstname = subinstr(firstname, ".","", .)
replace firstname = subinstr(firstname, ",","", .)
replace firstname = subinstr(firstname, ",,","", .)

replace firstname = "Vladimír" if firstname == "Vladimít"
replace surname = "Aulická Jírovcová" if surname == "Aulická -Jírovcová"
replace surname = "Hemzáčková" if surname == "Hemzáková"
replace firstname = "Vítězslav" if firstname == "Vítěslav" & surname == "Lapčík" & birthdate == td(21may1965)
replace surname = "Orszulik" if surname == "Orszulík" & birthdate == td(27feb1978)
replace surname = "Adamík" if surname == "ADAMÍK" & birthdate == td(16mar1962)
replace surname = "Bohutínský" if surname == "Bohutínský - soudní exekutor" & birthdate == td(23oct1966)
replace firstname = "Petr" if firstname == "PETR"
replace firstname = "Jan" if firstname == "JAN"
replace surname = "Hřebec" if surname == "HŘEBEC"
replace surname = "Kalabus" if surname == "KALABUS"
replace surname = "Kofroň" if surname == "KOFROŇ"
replace surname = "Kosubová" if surname == "KOSUBOVÁ"
replace surname = "Polášek" if surname == "POLÁŠEK"
replace firstname = "Miroslav" if firstname == "MIROSLAV"
replace surname = "Palyza" if surname == "Palyza st."
replace firstname = "Richard" if firstname == "RichaRD"
replace surname = "Stehno" if surname == "Stehno - ES TRANS"
replace surname = "Vavřinec" if surname == "VAVŘINEC"
replace surname = "Wanecki" if surname == "WANECKI-MANAP"
replace surname = "Ťulpík" if surname == "Ťulpík - WTC"
replace firstname = "Ján" if firstname == "Jan" & birthdate == td(17jun1980)
replace surname = "Ballek" if surname == "Baller" & birthdate == td(15may1997)
replace firstname = "Jakub Vojtěch" if surname == "Ballek" & birthdate == td(15may1997)
replace surname = "Friedlová" if surname == "Friedlova" & birthdate == td(05may1987)
replace firstname = "Eliška" if surname == "Friedlová" & birthdate == td(05may1987)
replace surname = "Homindová" if surname == "Homindova" & birthdate == td(20jul1974)
replace surname = "Beran" if firstname == "Beran" & birthdate == td(26may1986)
replace surname = "Nádvorník" if firstname == "Nádvorník" & birthdate == td(18jan1986)
replace surname = "Rýdl" if firstname == "Rýdl" & birthdate == td(08oct1979)
replace surname = "Střítecký" if firstname == "Střítecký" & birthdate == td(03jul1986)
replace surname = "Žůrek" if firstname == "Žůrek" & birthdate == td(23oct1984)
replace firstname = "Jan" if firstname == "Beran" & birthdate == td(26may1986)
replace firstname = "Jan" if firstname == "Nádvorník" & birthdate == td(18jan1986)
replace firstname = "Jan" if firstname == "Rýdl" & birthdate == td(08oct1979)
replace firstname = "Jan" if firstname == "Střítecký" & birthdate == td(03jul1986)
replace firstname = "Jan" if firstname == "Žůrek" & birthdate == td(23oct1984)
replace firstname = "Antonín" if firstname == "Antonin" & birthdate == td(16jun1996)
replace firstname = "Tomáš" if surname == "Kirš" & birthdate == td(18may1980)
replace surname = "Kohoutová" if surname == "Kohoutova" & birthdate == td(18oct1989)
replace firstname = "Magdalena" if firstname == "Magdalena Anna" & birthdate == td(18oct1989)
replace surname = "Komrsková" if surname == "Komrskova" & birthdate == td(08feb1978)
replace surname = "Krausová" if surname == "Krausova" & birthdate == td(08oct1992)
replace surname = "Krtičková" if surname == "Krtičková,"
replace surname = "Šotková"  if firstname == "Šotková" & birthdate ==  td(12apr1968)
replace firstname = "Lenka" if firstname == "Šotková" & birthdate ==  td(12apr1968)
replace surname = "Vachová" if firstname == "Vachova" & birthdate ==  td(25jun1979)
replace firstname = "Libuše" if firstname == "Vachova" & birthdate ==  td(25jun1979)
replace surname = "Lorovský" if surname == "Lorovsky" 
replace surname = "Rek" if firstname == "Rek" & birthdate ==  td(30mar1990)
replace firstname = "Lubomír" if firstname == "Rek" & birthdate ==  td(30mar1990)
replace surname = "Beránková" if firstname == "Beránková" & birthdate ==  td(01jun1992)
replace firstname = "Lucie" if firstname == "Beránková" & birthdate ==  td(01jun1992)
replace surname = "Sokolová" if firstname == "Sokolová" & birthdate ==  td(14jun2001)
replace firstname = "Lucie" if firstname == "Sokolová" & birthdate ==  td(14jun2001)
replace firstname = "Martina" if firstname == "Martina Jerie" & birthdate == td(06sep1971)
replace firstname = "Zdeněk" if firstname == "Zdenek" & birthdate == td(22aug1977) & surname == "Machek"
replace firstname = "Giuseppe" if firstname == "Giusepe" & birthdate == td(10jun1962) 
replace firstname = "Renata" if firstname == "Renáta" & birthdate == td(25jun1962)
replace firstname = "Tomáš" if firstname == "IngTomáš" & birthdate == td(18apr1985)
replace surname = "Chamrád" if firstname == "Chamrád" & birthdate == td(06apr2002)
replace firstname = "Michal" if firstname == "Chamrád" & birthdate == td(06apr2002)
replace surname = "Langer" if firstname == "Langer" & birthdate == td(01oct1999)
replace firstname = "Michal" if firstname == "Langer" & birthdate == td(01oct1999)
replace surname = "Mazík" if firstname == "Mazík" & birthdate == td(04sep1984)
replace firstname = "Michal" if firstname == "Mazík" & birthdate == td(04sep1984)
replace surname = "Trousil" if firstname == "Trousil" & birthdate == td(12mar1979)
replace firstname = "Michal" if firstname == "Trousil" & birthdate == td(12mar1979)
replace firstname = "Zdeněk" if (firstname == "Ing Zdeněk" & birthdate == td(27jul1966)) | (firstname == "IngZdeněk" & birthdate == td(27jul1966))
replace surname = "Ondráčková" if surname == "Ondrackova" & birthdate == td(22nov1993)
replace surname = "Bednář" if firstname == "Bednář" & birthdate == td(23nov1978)
replace firstname = "Ondřej" if firstname == "Bednář" & birthdate == td(23nov1978)
replace surname = "Haváč" if firstname == "Haváč" & birthdate == td(03sep1985)
replace firstname = "Ondřej" if firstname == "Haváč" & birthdate == td(03sep1985)
replace surname = "Kolínský" if firstname == "Kolínský" & birthdate == td(11mar1990)
replace firstname = "Ondřej" if firstname == "Kolínský" & birthdate == td(11mar1990)


replace surname = "Stalčík" if firstname == "stalčík" & birthdate == td(31oct1975)
replace firstname = "Adam" if firstname == "stalčík" & birthdate == td(31oct1975)
replace surname = "Balogová" if surname == "balogova" & birthdate == td(02sep1994)
replace surname = "De Bruyn" if surname == "de Bruyn" & birthdate == td(11jul1983)
replace surname = "Devátý" if firstname == "Jindrich" & birthdate == td(11jan1988)
replace firstname = "Jindřich" if firstname == "Jindrich" & birthdate == td(11jan1988)
replace surname = "Fafi" if surname == "fafi" & birthdate == td(03jul1990)
replace surname = "Fischer" if surname == "fischer" & birthdate == td(14oct1965)
replace surname = "Fojtek" if surname == "fojtek" & birthdate == td(07jan1969)
replace firstname = "Martin" if firstname == "martin" & birthdate == td(07jan1969)

replace surname = "Frauknecht" if surname == "frauknecht"
replace surname = "Gronych" if surname == "gronych" 
replace surname = "Hanzal" if surname == "hanzal" 
replace surname = "Holas" if surname == "holas" 
replace firstname = "Marin" if firstname == "marin" 
replace surname = "Hovorková" if surname == "hovorková" 
replace surname = "Jedlička" if surname == "jedlička" 
replace surname = "Junga" if surname == "junga" 
replace surname = "Kabelka" if surname == "kabelka"
replace surname = "Karas" if surname == "karas" 
replace firstname = "Zbyšek" if firstname == "zbyšek" 
replace surname = "Koubková" if surname == "koubkova" 
replace surname = "Karger" if surname == "karger"
replace surname = "Kindl" if surname == "kindl" 
replace surname = "Michailidu" if surname == "michailidu" 
replace firstname = "Janka" if firstname == "janka" 
replace surname = "Kolář" if surname == "kolář" 
replace firstname = "Miroslav" if firstname == "Miroslav"
replace surname = "Kopecká" if surname == "kopecká"
replace firstname = "Jana" if firstname == "jana"
replace surname = "Krištiak" if surname == "krištiak"
replace surname = "Kubovciak" if surname == "kubovciak"
replace surname = "Kuře" if surname == "kure"
replace surname = "Libich" if surname == "libich"
replace surname = "Lišková" if surname == "liskova"
replace surname = "Myslivec" if surname == "myslivec"
replace surname = "Pánek" if surname == "panek"
replace surname = "Novotný" if surname == "novotný"
replace firstname = "Simon" if firstname == "simon"
replace surname = "Selinger" if surname == "selinger" | surname == "selinger/knihkupectví"
replace firstname = "Josef" if firstname == "josef"
replace surname = "Pokorná" if surname == "pokorná"
replace surname = "Přečková" if surname == "přečková"
replace surname = "Rebl" if surname == "rebl"
replace surname = "Sebera" if surname == "sebera"
replace surname = "Svoboda" if surname == "svoboda"
replace surname = "Zecha" if surname == "zecha"
replace surname = "Sedlák" if surname == "sedlák"
replace surname = "Seguin" if surname == "seguin"
replace surname = "Tilinger" if surname == "tilinger"
replace surname = "Vlášek" if surname == "vlášek"
replace surname = "Wihan" if surname == "wihan"
replace surname = "Vobecká" if surname == "vobecká"
replace surname = "Hrůza" if firstname == "hrůza"
replace firstname = "Ladislav" if firstname == "hrůza"
replace surname = "Sklenka" if surname == "sklenka"
replace surname = "Skala" if surname == "skala" 
replace firstname = "Martin" if firstname == "Marri" 
replace surname = "Skovajsa" if surname == "skovajsa"
replace firstname = "Šimon" if firstname == "šimon"
replace surname = "Vedlová" if surname == "vendlova"
replace surname = "Miguele" if surname == "miguele"
replace surname = "Navrat" if surname == "navrat"
replace surname = "Pety" if surname == "pety"
replace firstname = "Tom" if surname == "tom"
replace surname = "Foltyn" if surname == "foltyn(bolsoncar)"
replace firstname = "Boleslav" if surname == "boleslav"
replace surname = "Řeha" if surname == "Ř?ha"
replace surname = "Špingl" if surname == "ŠPINGL"
replace surname = "Jánská" if firstname == "Jánská" & birthdate == td(04jun1963)
replace firstname = "Alena" if firstname == "Jánská" & birthdate == td(04jun1963)
replace surname = "Šeborová" if firstname == "Šeborová" & birthdate == td(15jan1957)
replace firstname = "Alena" if firstname == "Šeborová" & birthdate == td(15jan1957)
replace surname = "Mačková" if firstname == "Mačková" & birthdate == td(19sep1972)
replace firstname = "Andrea" if firstname == "Mačková" & birthdate == td(19sep1972)
replace surname = "Biersacková" if firstname == "Biersacková" & birthdate == td(06feb1946)
replace firstname = "Anna" if firstname == "Biersacková" & birthdate == td(06feb1946)
replace surname = "Dušek" if firstname == "Dušek" & birthdate == td(27feb1956)
replace firstname = "Antonín" if firstname == "Dušek" & birthdate == td(27feb1956)
replace surname = "Impseil" if firstname == "Impseil" & birthdate == td(19mar1995)
replace firstname = "Daniel" if firstname == "Impseil" & birthdate == td(19mar1995)
replace surname = "Šelešovský" if firstname == "Šelešovský" & birthdate == td(10feb1971)
replace firstname = "Daniel" if firstname == "Šelešovský" & birthdate == td(10feb1971)
replace surname = "Urbánková" if firstname == "Urbánková" & birthdate == td(10oct1952)
replace firstname = "Daniela" if firstname == "Urbánková" & birthdate == td(10oct1952)
replace surname = "Bojková" if firstname == "Bojková" & birthdate == td(24aug1963)
replace firstname = "Darja" if firstname == "Bojková" & birthdate == td(24aug1963)
replace surname = "Dvořák" if firstname == "Dvořák" & birthdate == td(03feb1991)
replace firstname = "David" if firstname == "Dvořák" & birthdate == td(03feb1991)
replace surname = "Ševčík" if firstname == "Ševčík" & birthdate == td(04jul1967)
replace firstname = "David" if firstname == "Ševčík" & birthdate == td(04jul1967)
replace surname = "Sklenka" if firstname == "Sklenka" & birthdate == td(13jul1986)
replace firstname = "David" if firstname == "Sklenka" & birthdate == td(13jul1986)
replace surname = "Pokorný" if firstname == "Pokorný" & birthdate == td(31jul1959)
replace firstname = "David" if firstname == "Pokorný" & birthdate == td(31jul1959)
replace surname = "Gerneš" if firstname == "Gerneš" & birthdate == td(25apr1991)
replace firstname = "David" if firstname == "Gerneš" & birthdate == td(25apr1991)
replace firstname = "Gogo" if firstname == "gogo"
replace surname = "Filipiecová" if surname == "Filipiecová Bc."
replace surname = "Blažek" if firstname == "Blažek" & birthdate == td(18feb1962)
replace firstname = "Igor" if firstname == "Blažek" & birthdate == td(18feb1962)
replace surname = "Jurásek" if firstname == "Jurásek" & birthdate == td(16jan1948)
replace firstname = "Igor" if firstname == "Jurásek" & birthdate == td(16jan1948)
replace surname = "Valenta" if surname == "Ing.Valenta" 
replace surname = "Dukátníková" if firstname == "Dukátníková" & birthdate == td(19feb1989)
replace firstname = "Irena" if firstname == "Dukátníková" & birthdate == td(19feb1989)
replace surname = "Nevludová" if firstname == "Nevludová" & birthdate == td(29jul1977)
replace firstname = "Ivana" if firstname == "Nevludová" & birthdate == td(29jul1977)
replace surname = "Ondrušová" if firstname == "Ondrušová" & birthdate == td(24jan1964)
replace firstname = "Ivana" if firstname == "Ondrušová" & birthdate == td(24jan1964)
replace surname = "Škurková" if firstname == "Škurková" & birthdate == td(18may1962)
replace firstname = "Iveta" if firstname == "Škurková" & birthdate == td(18may1962)
replace surname = "Švancarová" if firstname == "Švancarová" & birthdate == td(07apr1962)
replace firstname = "Iveta" if firstname == "Švancarová" & birthdate == td(07apr1962)
replace surname = "Jelínek" if firstname == "Jelínek" & birthdate == td(19jun1970)
replace firstname = "Ivo" if firstname == "Jelínek" & birthdate == td(19jun1970)
replace surname = "Havran" if firstname == "Havran" & birthdate == td(04aug1986)
replace firstname = "Jakub" if firstname == "Havran" & birthdate == td(04aug1986)
replace surname = "Morong" if firstname == "Morong" & birthdate == td(02oct1995)
replace firstname = "Jakub" if firstname == "Morong" & birthdate == td(02oct1995)
replace surname = "Bílý" if firstname == "Bílý" & birthdate == td(29aug1984)
replace firstname = "Jan" if firstname == "Bílý" & birthdate == td(29aug1984)
replace surname = "Frišhons" if firstname == "Frišhons" & birthdate == td(13dec1984)
replace firstname = "Jan" if firstname == "Frišhons" & birthdate == td(13dec1984)
replace surname = "Fučík" if firstname == "Fučík" & birthdate == td(21jun1975)
replace firstname = "Jan" if firstname == "Fučík" & birthdate == td(21jun1975)
replace surname = "Mašek" if firstname == "Mašek" & birthdate == td(18oct1984)
replace firstname = "Jan" if firstname == "Mašek" & birthdate == td(18oct1984)
replace surname = "Hrnčíř" if firstname == "Hrnčíř" & birthdate == td(01may1977)
replace firstname = "Jan" if firstname == "Hrnčíř" & birthdate == td(01may1977)
replace surname = "Hubený" if firstname == "Hubený" & birthdate == td(22mar1972)
replace firstname = "Jan" if firstname == "Hubený" & birthdate == td(22mar1972)
replace surname = "Ouzký" if firstname == "Ouzký" & birthdate == td(08jun1984)
replace firstname = "Jan" if firstname == "Ouzký" & birthdate == td(08jun1984)
replace surname = "Stöhr" if firstname == "Stöhr" & birthdate == td(03sep1989)
replace firstname = "Jan" if firstname == "Stöhr" & birthdate == td(03sep1989)
replace surname = "Čestický" if firstname == "Čestický" & birthdate == td(13may1967)
replace firstname = "Jan" if firstname == "Čestický" & birthdate == td(13may1967)
replace surname = "Štraub" if firstname == "Štraub" & birthdate == td(20may1971)
replace firstname = "Jan" if firstname == "Štraub" & birthdate == td(20may1971)
replace surname = "Žižka" if firstname == "Žižka" & birthdate == td(25sep1954)
replace firstname = "Jan" if firstname == "Žižka" & birthdate == td(25sep1954)
replace surname = "Žlebek" if firstname == "Žlebek" & birthdate == td(31jul1978)
replace firstname = "Jan" if firstname == "Žlebek" & birthdate == td(31jul1978)
replace surname = "Plevková" if firstname == "Plevková" & birthdate == td(25jul1988)
replace firstname = "Jana" if firstname == "Plevková" & birthdate == td(25jul1988)
replace surname = "Štablová" if firstname == "Štablová" & birthdate == td(25oct1969)
replace firstname = "Jana" if firstname == "Štablová" & birthdate == td(25oct1969)
replace surname = "Levová" if firstname == "Levová" & birthdate == td(17jun1977)
replace firstname = "Jana" if firstname == "Levová" & birthdate == td(17jun1977)
replace surname = "Valigurová" if firstname == "Valigurová" & birthdate == td(19dec1969)
replace firstname = "Jana" if firstname == "Valigurová" & birthdate == td(19dec1969)
replace surname = "Vávrová" if firstname == "Vávrová" & birthdate == td(08jul1961)
replace firstname = "Jana" if firstname == "Vávrová" & birthdate == td(08jul1961)
replace surname = "Dragounová" if firstname == "Dragounová" & birthdate == td(07nov1943)
replace firstname = "Jana" if firstname == "Dragounová" & birthdate == td(07nov1943)
replace surname = "Plevková" if firstname == "Plevková" & birthdate == td(04feb1959)
replace firstname = "Jarmila" if firstname == "Plevková" & birthdate == td(04feb1959)
replace surname = "Hendrychová" if firstname == "Hendrychová" & birthdate == td(27oct1960)
replace firstname = "Jarmila" if firstname == "Hendrychová" & birthdate == td(27oct1960)
replace surname = "Broulík" if firstname == "Broulík" & birthdate == td(13apr1970)
replace firstname = "Jaromír" if firstname == "Broulík" & birthdate == td(13apr1970)
replace surname = "Slíva" if firstname == "Slíva" & birthdate == td(07jan1977)
replace firstname = "Jaromír" if firstname == "Slíva" & birthdate == td(07jan1977)
replace surname = "Wesley" if firstname == "Wesley" & birthdate == td(14apr1954)
replace firstname = "Eva" if firstname == "Wesley" & birthdate == td(14apr1954)
replace surname = "Šťovíčková" if firstname == "Šťovíčková" & birthdate == td(25may1948)
replace firstname = "Eva" if firstname == "Šťovíčková" & birthdate == td(25may1948)
replace surname = "Klokočka" if firstname == "Klokočka" & birthdate == td(22mar1980)
replace firstname = "Evžen" if firstname == "Klokočka" & birthdate == td(22mar1980)
replace surname = "Gogo" if firstname == "Gogo" & birthdate == td(03jul1990)
replace firstname = "Fafi" if firstname == "Gogo" & birthdate == td(03jul1990)
replace surname = "Švancarová" if firstname == "Švancarová" & birthdate == td(07apr1967)
replace firstname = "Iveta" if firstname == "Švancarová" & birthdate == td(07apr1967)
replace surname = "Dvořák" if firstname == "Dvořák" & birthdate == td(04nov1957)
replace firstname = "Jaroslav" if firstname == "Dvořák" & birthdate == td(04nov1957)
replace surname = "Holík" if firstname == "Holík" & birthdate == td(29jun1953)
replace firstname = "Jaroslav" if firstname == "Holík" & birthdate == td(29jun1953)
replace surname = "Jána" if firstname == "Jána" & birthdate == td(07mar1971)
replace firstname = "Jaroslav" if firstname == "Jána" & birthdate == td(07mar1971)
replace surname = "Konečný" if firstname == "Konečný" & birthdate == td(03jul1956)
replace firstname = "Jaroslav" if firstname == "Konečný" & birthdate == td(03jul1956)
replace surname = "Nečas" if firstname == "Nečas" & birthdate == td(23mar1964)
replace firstname = "Jaroslav" if firstname == "Nečas" & birthdate == td(23mar1964)
replace surname = "Tyrala" if firstname == "Tyrala" & birthdate == td(18oct1981)
replace firstname = "Jaroslav" if firstname == "Tyrala" & birthdate == td(18oct1981)
replace surname = "Krejcarová" if firstname == "Krejcarová" & birthdate == td(29jun1991)
replace firstname = "Jitka" if firstname == "Krejcarová" & birthdate == td(29jun1991)
replace surname = "Dobešová" if firstname == "Dobešová" & birthdate == td(28may1957)
replace firstname = "Jiřina" if firstname == "Dobešová" & birthdate == td(28may1957)
replace surname = "Horáková" if firstname == "Horáková" & birthdate == td(16aug1949)
replace firstname = "Jiřina" if firstname == "Horáková" & birthdate == td(16aug1949)
replace surname = "Dvořák" if firstname == "Dvořák" & birthdate == td(15jul1977)
replace firstname = "Jiří" if firstname == "Dvořák" & birthdate == td(15jul1977)
replace surname = "Kysling" if firstname == "Kysling" & birthdate == td(14jul1979)
replace firstname = "Jiří" if firstname == "Kysling" & birthdate == td(14jul1979)
replace surname = "Strnad" if firstname == "Strnad" & birthdate == td(26sep1959) | birthdate == td(26jun1959)
replace firstname = "Jiří" if firstname == "Strnad" & birthdate == td(26sep1959) | birthdate == td(26jun1959)
replace surname = "Čejka" if firstname == "Čejka" & birthdate == td(30aug1983)
replace firstname = "Jiří" if firstname == "Čejka" & birthdate == td(30aug1983)
replace surname = "Soler" if firstname == "Soler" & birthdate == td(20apr1947)
replace firstname = "Jiří" if firstname == "Soler" & birthdate == td(20apr1947)
replace surname = "Nitsche" if firstname == "Nitsche" & birthdate == td(13apr1952)
replace firstname = "Jiří" if firstname == "Nitsche" & birthdate == td(13apr1952)
replace surname = "Kohoutek" if firstname == "Kohoutek" & birthdate == td(26nov1964)
replace firstname = "Jiří" if firstname == "Kohoutek" & birthdate == td(26nov1964)
replace surname = "Kocman" if firstname == "Kocman" & birthdate == td(01jun1955)
replace firstname = "Jiří" if firstname == "Kocman" & birthdate == td(01jun1955)
replace surname = "Kobza" if firstname == "Kobza" & birthdate == td(27dec1955)
replace firstname = "Jiří" if firstname == "Kobza" & birthdate == td(27dec1955)
replace surname = "Antuš" if firstname == "Antuš" & birthdate == td(16sep1973)
replace firstname = "Jiří" if firstname == "Antuš" & birthdate == td(16sep1973)
replace surname = "Pinkava" if firstname == "Pinkava" & birthdate == td(01oct1963)
replace firstname = "Jiří" if firstname == "Pinkava" & birthdate == td(01oct1963)
replace birthdate = td(26sep1959) if firstname == "Jiří" & surname == "Strnad" & birthdate == td(26jun1959)
replace surname = "Doležal" if firstname == "Doležal" & birthdate == td(18mar1949)
replace firstname = "John" if firstname == "Doležal" & birthdate == td(18mar1949)
replace surname = "Doležal" if firstname == "Doležal" & birthdate == td(18may1968)
replace firstname = "Josef" if firstname == "Doležal" & birthdate == td(18may1968)
replace surname = "Kodýtek" if firstname == "Kodýtek" & birthdate == td(04feb1998)
replace firstname = "Josef" if firstname == "Kodýtek" & birthdate == td(04feb1998)
replace surname = "Kopřiva" if firstname == "Kopřiva" & birthdate == td(10oct1978)
replace firstname = "Josef" if firstname == "Kopřiva" & birthdate == td(10oct1978)
replace surname = "Pleva" if firstname == "Pleva" & birthdate == td(22jun1961)
replace firstname = "Josef" if firstname == "Pleva" & birthdate == td(22jun1961)
replace surname = "Sukup" if firstname == "Sukup" & birthdate == td(04dec1959)
replace firstname = "Josef" if firstname == "Sukup" & birthdate == td(04dec1959)
replace surname = "Zakopčan" if firstname == "Zakopčan" & birthdate == td(21may1961)
replace firstname = "Josef" if firstname == "Zakopčan" & birthdate == td(21may1961)
replace surname = "Ondřej" if firstname == "Ondřej" & birthdate == td(28jan1958)
replace firstname = "Josef" if firstname == "Ondřej" & birthdate == td(28jan1958)
replace surname = "Kadlecová" if surname == "Kadlecova" & birthdate == td(30jul1990)
replace firstname = "Štěpánka" if firstname == "Stepanka" & birthdate == td(30jul1990)
replace surname = "Běrský" if firstname == "Běrský" & birthdate == td(16jul1958)
replace firstname = "Kamil" if firstname == "Běrský" & birthdate == td(16jul1958)
replace surname = "Kokolusová" if firstname == "Kokolusová" & birthdate == td(29may1967)
replace firstname = "Kamila" if firstname == "Kokolusová" & birthdate == td(29may1967)
replace surname = "Bák" if firstname == "Bák" & birthdate == td(04jun1978)
replace firstname = "Karel" if firstname == "Bák" & birthdate == td(04jun1978)
replace surname = "Fiala" if firstname == "Fiala" & birthdate == td(12jul1987)
replace firstname = "Karel" if firstname == "Fiala" & birthdate == td(12jul1987)
replace surname = "Kříž" if firstname == "Kříž" & birthdate == td(23mar1951)
replace firstname = "Karel" if firstname == "Kříž" & birthdate == td(23mar1951)
replace surname = "Oplt" if firstname == "Oplt" & birthdate == td(06sep1945)
replace firstname = "Karel" if firstname == "Oplt" & birthdate == td(06sep1945)
replace surname = "Ošťadnický" if firstname == "Ošťadnický" & birthdate == td(06oct1971)
replace firstname = "Karel" if firstname == "Ošťadnický" & birthdate == td(06oct1971)
replace surname = "Maříková" if firstname == "Maříková" & birthdate == td(19mar1981)
replace firstname = "Karel" if firstname == "Maříková" & birthdate == td(19mar1981)
replace surname = "Kolečkář" if surname == "Kolečk?ř" & birthdate == td(21jan1994)
replace surname = "Krňanský" if surname == "KrŇanský" & birthdate == td(04sep1944)
replace surname = "Maříková" if firstname == "Maříková" & birthdate == td(19mar1981)
replace firstname = "Karel" if firstname == "Maříková" & birthdate == td(19mar1981)
replace surname = "Hammer" if firstname == "Hammer" & birthdate == td(17mar1982)
replace firstname = "Ladislav" if firstname == "Hammer" & birthdate == td(17mar1982)
replace surname = "Hora" if firstname == "Hora" & birthdate == td(05jun1952)
replace firstname = "Ladislav" if firstname == "Hora" & birthdate == td(05jun1952) 
replace surname = "Ulrych" if firstname == "Ulrych" & birthdate == td(20nov1991)
replace firstname = "Ladislav" if firstname == "Ulrych" & birthdate == td(20nov1991) 
replace surname = "Vali" if firstname == "Vali" & birthdate == td(09jul1964)
replace firstname = "Libor" if firstname == "Vali" & birthdate == td(09jul1964) 
replace surname = "Volná" if firstname == "Volná" & birthdate == td(03jul1957)
replace firstname = "Libuše" if firstname == "Volná" & birthdate == td(03jul1957) 
replace surname = "Volný" if firstname == "Volný" & birthdate == td(03jul1973)
replace firstname = "Lubomír" if firstname == "Volný" & birthdate == td(03jul1973) 
replace surname = "Kapr" if firstname == "Kapr" & birthdate == td(09aug1979)
replace firstname = "Luboš" if firstname == "Kapr" & birthdate == td(09aug1979) 
replace surname = "Vitásek" if firstname == "Vitásek" & birthdate == td(24jun1952)
replace firstname = "Lubomír" if firstname == "Vitásek" & birthdate == td(24jun1952) 
replace surname = "Španěl" if firstname == "Španěl" & birthdate == td(01oct1955)
replace firstname = "Lubomír" if firstname == "Španěl" & birthdate == td(01oct1955) 
replace surname = "Burianová" if firstname == "Burianová" & birthdate == td(09oct1987)
replace firstname = "Lucie" if firstname == "Burianová" & birthdate == td(09oct1987) 
replace surname = "Šafránková" if firstname == "Šafránková" & birthdate == td(21may1987)
replace firstname = "Lucie" if firstname == "Šafránková" & birthdate == td(21may1987)  
replace surname = "Štěpánková" if firstname == "Štěpánková" & birthdate == td(06jan1988)
replace firstname = "Lucie" if firstname == "Štěpánková" & birthdate == td(06jan1988)   
replace surname = "Mičolová" if firstname == "Mičolová" & birthdate == td(17feb1945)
replace firstname = "Ludvika" if firstname == "Mičolová" & birthdate == td(17feb1945)     
replace surname = "Hromada" if firstname == "Hromada" & birthdate == td(14apr1971)
replace firstname = "Luděk" if firstname == "Hromada" & birthdate == td(14apr1971)    
replace surname = "Žídek" if firstname == "Žídek" & birthdate == td(11oct1950)
replace firstname = "Luděk" if firstname == "Žídek" & birthdate == td(11oct1950)    
replace surname = "Tomešová" if firstname == "Tomešová" & birthdate == td(04nov1968)
replace firstname = "Luďka" if firstname == "Tomešová" & birthdate == td(04nov1968)    
replace surname = "Skříčková" if firstname == "Skříčková" & birthdate == td(31jan1966)
replace firstname = "Marcela" if firstname == "Skříčková" & birthdate == td(31jan1966)    
replace surname = "Heydušek" if firstname == "Heydušek" & birthdate == td(02nov1965)
replace firstname = "Marcel" if firstname == "Heydušek" & birthdate == td(02nov1965)     
replace surname = "Vysocký" if firstname == "Vysocký" & birthdate == td(23feb1969)
replace firstname = "Marek" if firstname == "Vysocký" & birthdate == td(23feb1969)     
replace surname = "Žalman" if firstname == "Žalman" & birthdate == td(09dec1974)
replace firstname = "Marek" if firstname == "Žalman" & birthdate == td(09dec1974)      
replace birthdate = td(25jan1962) if birthdate == td(25dec1962) & firstname == "Bojko"
replace surname = "Bojko" if firstname == "Bojko" & birthdate == td(25jan1962)
replace firstname = "Marian" if firstname == "Bojko" & birthdate == td(25jan1962)      
replace surname = "Švec" if firstname == "Švec" & birthdate == td(10jul1992)
replace firstname = "Marian" if firstname == "Švec" & birthdate == td(10jul1992)  
replace surname = "Malcharová" if firstname == "Malcharová" & birthdate == td(03nov1963)
replace firstname = "Marie" if firstname == "Malcharová" & birthdate == td(03nov1963)  
replace surname = "Brož" if firstname == "Brož" & birthdate == td(28aug1981)
replace firstname = "Martin" if firstname == "Brož" & birthdate == td(28aug1981)  
replace surname = "Bojko" if firstname == "Bojko" & birthdate == td(01oct1982)
replace firstname = "Martin" if firstname == "Bojko" & birthdate == td(01oct1982)  
replace surname = "Mihulová" if firstname == "Mihulová" & birthdate == td(16feb1968)
replace firstname = "Marta" if firstname == "Mihulová" & birthdate == td(16feb1968)  
replace surname = "Vlčková" if firstname == "Vlčková" & birthdate == td(01jul1966)
replace firstname = "Markéta" if firstname == "Vlčková" & birthdate == td(01jul1966)  
replace surname = "Kovaříková" if firstname == "Kovaříková" & birthdate == td(29aug1947)
replace firstname = "Markéta" if firstname == "Kovaříková" & birthdate == td(29aug1947)  
replace surname = "Pešat" if firstname == "Pešat" & birthdate == td(13apr1985)
replace firstname = "Martin" if firstname == "Pešat" & birthdate == td(13apr1985)  
replace surname = "Franzová" if firstname == "Franzová" & birthdate == td(10sep1959)
replace firstname = "Martina" if firstname == "Franzová" & birthdate == td(10sep1959)  
replace surname = "Příhoda" if firstname == "Příhoda" & birthdate == td(01feb1979)
replace firstname = "Martin" if firstname == "Příhoda" & birthdate == td(01feb1979)    
replace surname = "Plojhar" if firstname == "Plojhar" & birthdate == td(16jun1978)
replace firstname = "Martin" if firstname == "Plojhar" & birthdate == td(16jun1978)   
replace surname = "Sokele" if firstname == "Sokele" & birthdate == td(10jun1984)
replace firstname = "Martin" if firstname == "Sokele" & birthdate == td(10jun1984)   
replace surname = "Řehák" if firstname == "Řehák" & birthdate == td(15mar1990)
replace firstname = "Martin" if firstname == "Řehák" & birthdate == td(15mar1990)   
replace surname = "Malášek" if firstname == "Malášek" & birthdate == td(16sep1970)
replace firstname = "Martin" if firstname == "Malášek" & birthdate == td(16sep1970)   
replace surname = "Homolová" if firstname == "Homolová" & birthdate == td(01jul1982)
replace firstname = "Martina" if firstname == "Homolová" & birthdate == td(01jul1982)  
replace surname = "Topičová" if firstname == "Topičová" & birthdate == td(07nov1964)
replace firstname = "Martina" if firstname == "Topičová" & birthdate == td(07nov1964)
replace surname = "Kachlík" if firstname == "Kachlík" & birthdate == td(21jan1987)
replace firstname = "Matěj" if firstname == "Kachlík" & birthdate == td(21jan1987)
replace surname = "Nastoupilová" if firstname == "Nastoupilová" & birthdate == td(24nov1989)
replace firstname = "Michaela" if firstname == "Nastoupilová" & birthdate == td(24nov1989)
replace birthdate = td(30nov1980) if birthdate == td(30nov1982) & firstname == "Dlouhý"
replace surname = "Dlouhý" if firstname == "Dlouhý" & birthdate == td(30nov1980)
replace firstname = "Michal" if firstname == "Dlouhý" & birthdate == td(30nov1980)
replace surname = "Kučera" if firstname == "Kučera" & birthdate == td(26feb1975)
replace firstname = "Michal" if firstname == "Kučera" & birthdate == td(26feb1975)
replace surname = "Tecl" if firstname == "Tecl" & birthdate == td(08dec1965)
replace firstname = "Michal" if firstname == "Tecl" & birthdate == td(08dec1965)
replace surname = "Wisiorek" if firstname == "Wisiorek" & birthdate == td(04may1977)
replace firstname = "Michal" if firstname == "Wisiorek" & birthdate == td(04may1977)
replace surname = "Imling" if firstname == "Imling" & birthdate == td(05dec1955)
replace firstname = "Mikuláš" if firstname == "Imling" & birthdate == td(05dec1955)
replace surname = "Kuchár" if firstname == "Kuchár" & birthdate == td(06jan1968)
replace firstname = "Milan" if firstname == "Kuchár" & birthdate == td(06jan1968)
replace surname = "Teuchner" if firstname == "Teuchner" & birthdate == td(08aug1947)
replace firstname = "Miloš" if firstname == "Teuchner" & birthdate == td(08aug1947)
replace surname = "Čech" if firstname == "Čech" & birthdate == td(01may1960)
replace firstname = "Miloslav" if firstname == "Čech" & birthdate == td(01may1960)
replace surname = "Rozner" if firstname == "Rozner" & birthdate == td(29mar1977)
replace firstname = "Miloslav" if firstname == "Rozner" & birthdate == td(29mar1977)
replace surname = "Vovesný" if firstname == "Vovesný" & birthdate == td(18dec1964)
replace firstname = "Milan" if firstname == "Vovesný" & birthdate == td(18dec1964)
replace surname = "Sek" if firstname == "Sek" & birthdate == td(12oct1961)
replace firstname = "Milan" if firstname == "Sek" & birthdate == td(12oct1961)
replace surname = "Palička" if firstname == "Palička" & birthdate == td(08sep1980)
replace firstname = "Milan" if firstname == "Palička" & birthdate == td(08sep1980)
replace surname = "Nevlud" if firstname == "Nevlud" & birthdate == td(16jun1965)
replace firstname = "Milan" if firstname == "Nevlud" & birthdate == td(16jun1965)
replace surname = "Křejčířová" if firstname == "Křejčířová" & birthdate == td(06apr1946)
replace firstname = "Miluše" if firstname == "Křejčířová" & birthdate == td(06apr1946)
replace surname = "Lošťák" if firstname == "Lošťák" & birthdate == td(23nov1954)
replace firstname = "Miroslav" if firstname == "Lošťák" & birthdate == td(23nov1954)
replace surname = "Volná" if firstname == "Volná" & birthdate == td(10jul1946)
replace firstname = "Miroslava" if firstname == "Volná" & birthdate == td(10jul1946)
replace surname = "Kleisner" if firstname == "Kleisner" & birthdate == td(15mar1984)
replace firstname = "Miroslav" if firstname == "Kleisner" & birthdate == td(15mar1984)
replace surname = "Šubrt" if firstname == "Šubrt" & birthdate == td(03oct1954)
replace firstname = "Miroslav" if firstname == "Šubrt" & birthdate == td(03oct1954)
replace surname = "Nápravníková" if surname == "Nápravnková" & birthdate == td(22feb1971)
replace surname = "Smejkal" if firstname == "Smejkal" & birthdate == td(07dec1944)
replace firstname = "Oldřich" if firstname == "Smejkal" & birthdate == td(07dec1944)
replace surname = "Černý" if firstname == "Černý" & birthdate == td(10sep1965)
replace firstname = "Oldřich" if firstname == "Černý" & birthdate == td(10sep1965)
replace surname = "Pech" if firstname == "Pech" & birthdate == td(17may1983)
replace firstname = "Ondřej" if firstname == "Pech" & birthdate == td(17may1983)
replace surname = "Farský" if firstname == "Farský" & birthdate == td(30aug1974)
replace firstname = "Patrik" if firstname == "Farský" & birthdate == td(30aug1974)
replace surname = "Klán" if firstname == "Klán" & birthdate == td(05feb1972)
replace firstname = "Patrik" if firstname == "Klán" & birthdate == td(05feb1972)
replace surname = "Bezucha" if firstname == "Bezucha" & birthdate == td(22sep1963)
replace firstname = "Pavel" if firstname == "Bezucha" & birthdate == td(22sep1963)
replace surname = "Domalíp" if firstname == "Domalíp" & birthdate == td(22feb1974)
replace firstname = "Pavel" if firstname == "Domalíp" & birthdate == td(22feb1974)
replace surname = "Jelínek" if firstname == "Jelínek" & birthdate == td(31oct1962)
replace firstname = "Pavel" if firstname == "Jelínek" & birthdate == td(31oct1962)
replace surname = "Kočí" if firstname == "Kočí" & birthdate == td(02jan1959)
replace firstname = "Pavel" if firstname == "Kočí" & birthdate == td(02jan1959)
replace surname = "Pecho" if firstname == "Pecho" & birthdate == td(15jun1974)
replace firstname = "Pavel" if firstname == "Pecho" & birthdate == td(15jun1974)
replace surname = "Popelka" if firstname == "Popelka" & birthdate == td(15dec1968)
replace firstname = "Pavel" if firstname == "Popelka" & birthdate == td(15dec1968)
replace surname = "Suchý" if firstname == "Suchý" & birthdate == td(04apr1991)
replace firstname = "Pavel" if firstname == "Suchý" & birthdate == td(04apr1991)
replace surname = "Vidlička" if firstname == "Vidlička" & birthdate == td(11feb1948)
replace firstname = "Pavel" if firstname == "Vidlička" & birthdate == td(11feb1948)
replace surname = "Šubrt" if firstname == "Šubrt" & birthdate == td(17apr1964)
replace firstname = "Pavel" if firstname == "Šubrt" & birthdate == td(17apr1964)
replace surname = "Peksová" if surname == "Peksova" 
replace surname = "Balcarová" if surname == "Balcarova" 
replace surname = "Bartoš" if surname == "Bartos"
replace surname = "Belšán" if surname == "Belšan"
replace firstname = "Hana" if firstname == "Bc Hana"
replace surname = "Čejková" if surname == "Cejkova" & birthdate == td(29mar1973)
replace firstname = "Václava" if firstname == "Vaclava" & birthdate == td(29mar1973)
replace surname = "Čech" if surname == "Cech" & birthdate == td(10mar1986)
replace surname = "Cholenská" if surname == "Cholenska"
replace surname = "Knot" if firstname == "Knot" & birthdate == td(30oct1994)
replace firstname = "Dominik" if firstname == "Knot" & birthdate == td(30oct1994)
replace surname = "Dvořák" if surname == "Dvorak" & birthdate == td(22jan1991)
replace surname = "Dvořáková" if surname == "Dvorakova" & birthdate == td(01jun1992)
replace firstname = "Adéla" if firstname == "Adela" & birthdate == td(01jun1992)
replace surname = "Havlová" if surname == "Havlova" 
replace surname = "Holubová" if surname == "Holubova"
replace firstname = "Jan" if firstname == "Jan09"
replace surname = "Hromadková" if surname == "Hromadkova"
replace firstname = "Vojtěch" if firstname == "Vojtech" & birthdate == td(22jul1987) | birthdate == td(19jun1985) | birthdate == td(05may1985)
replace surname = "Seidl" if firstname == "Seidl" & birthdate == td(02feb1993)
replace firstname = "Jakub" if firstname == "Seidl" & birthdate == td(02feb1993)
replace surname = "Jedelský" if surname == "Jedelsky" 
replace surname = "Jirásková" if surname == "Jiraskova"
replace firstname = "Zdeněk" if firstname == "Zdenek" & birthdate == td(21nov1955)
replace surname = "Řezníčková" if firstname == "Řezníčková" & birthdate == td(24sep1973)
replace firstname = "Jitka" if firstname == "Řezníčková" & birthdate == td(24sep1973)
replace surname = "Bartošová" if firstname == "Bartošová" & birthdate == td(30oct1986)
replace firstname = "Jiřina" if firstname == "Bartošová" & birthdate == td(30oct1986)
replace surname = "Ulrychová" if firstname == "Ulrychová" & birthdate == td(03apr1981)
replace firstname = "Jiřina" if firstname == "Ulrychová" & birthdate == td(03apr1981)
replace surname = "Bartoš" if firstname == "Bartoš" & birthdate == td(22feb1969)
replace firstname = "Jiří" if firstname == "Bartoš" & birthdate == td(22feb1969)
replace surname = "Růžička" if firstname == "Růžička" & birthdate == td(02feb1978)
replace firstname = "Jiří" if firstname == "Růžička" & birthdate == td(02feb1978)
replace surname = "Kalendová" if surname == "Kalendova"
replace surname = "Klepetář" if surname == "Kleptar"
replace surname = "Konečný" if surname == "Konecny"
replace surname = "Kopecká" if surname == "Kopecka" & birthdate == td(22sep1979)
replace firstname = "Vladimíra" if firstname == "Vladimira" & birthdate == td(22sep1979)
replace surname = "Kopecký" if surname == "Kopecky"
replace firstname = "Regina" if firstname == "Regína" & birthdate == td(17may1962)
replace surname = "Lepková" if surname == "LEPKOVÁ"
replace surname = "Nesnera" if firstname == "Nesnera" & birthdate == td(06oct1970)
replace firstname = "Ladislav" if firstname == "Nesnera" & birthdate == td(06oct1970)
replace surname = "Lažnovský" if (surname == "Laznovský" & birthdate == td(06mar1980)) | (surname == "Lazznovsky" & birthdate == td(06mar1980))
replace surname = "Mariánek" if surname == "Marianek"
replace surname = "Urbanová" if firstname == "Urbanová" & birthdate == td(20oct1982)
replace firstname = "Marta" if firstname == "Urbanová" & birthdate == td(20oct1982)
replace surname = "Havlíček" if firstname == "Havlíček" & birthdate == td(24apr2000)
replace firstname = "Martin" if firstname == "Havlíček" & birthdate == td(24apr2000)
replace surname = "Korčák" if firstname == "Korčák" & birthdate == td(30oct1974)
replace firstname = "Martin" if firstname == "Korčák" & birthdate == td(30oct1974)
replace surname = "Mayer" if surname == "Mayer Soukromé"
replace surname = "Moravová" if surname == "Moravova"
replace surname = "Nečas" if surname == "Necas"
replace surname = "Mrázová" if firstname == "Mrázová" & birthdate == td(02apr1984)
replace firstname = "Nikola" if firstname == "Mrázová" & birthdate == td(02apr1984)
replace surname = "Novák" if surname == "Novak"
replace surname = "Novotný" if surname == "Novotny"
replace firstname = "Olga" if firstname == "Olga Nováková" 
replace surname = "Oborný" if surname == "Oborny"
replace surname = "Oborský" if surname == "Oborsky"
replace firstname = "Tom" if firstname == "tom"
replace surname = "Pospíšil" if surname == "Pospisil"
replace firstname = "Antonín" if firstname == "Antonin"
replace surname = "Pírek" if surname == "PÍREK"
replace surname = "Kšicová" if firstname == "Kšicová" & birthdate == td(07jan1987)
replace firstname = "Radka" if firstname == "Kšicová" & birthdate == td(07jan1987)
replace firstname = "Jaromír" if firstname == "Jaromir" & surname == "Rain" & birthdate == td(08jan1958)
replace firstname = "Vojtěch" if firstname == "Rozsypal Vojtěch" & birthdate == td(24may2004)
replace surname = "Sabo" if surname == "Sabó"
replace surname = "Sellers-Zajíc" if surname == "Sellers-Zajic"
replace firstname = "Vojtěch" if firstname == "Vojtech" & surname == "Sidorin"
replace surname = "Siváček" if surname == "Sivacek" & birthdate == td(05sep1984)
replace firstname = "Ivo" if firstname == "Mgr Ivo"
replace surname = "Špaček" if surname == "Spacek" & firstname == "Lubomir"
replace firstname = "Lubomír" if surname == "Špaček" & firstname == "Lubomir"
replace surname = "Sedlák" if surname == "Sedlak"
replace surname = "Staněk" if surname == "Stanek"
replace surname = "Sýkora" if surname == "Sykora"
replace surname = "Tomášek" if surname == "Tomasek"
replace surname = "Tomková" if surname == "Tomkova"
replace surname = "Tomšíková" if surname == "Tomsikova"
replace surname = "Totzauerová" if surname == "Totuaurova" | surname == "Totzauerova"
replace surname = "Kosková Třísková" if surname == "Triskova Koskova"
replace surname = "Třešňáková" if surname == "Tresnakova"
replace firstname = "Markéta" if firstname == "Marketa"  | firstname == "markéta"
replace surname = "Turjanicová" if surname == "TurJanicová"
replace surname = "Tóth" if surname == "Toth"
replace surname = "Uretšlégr" if surname == "Urtešlégr"
replace surname = "Valenčíková" if surname == "Valencikova"
replace surname = "Volemanová" if surname == "Volemanova"
replace surname = "Wagenknecht" if surname == "Wageknecht" & polparty == 4
replace firstname = "David" if firstname == "David František"
replace surname = "Wirthová" if surname == "Wirthova"
replace surname = "Zahumenská" if surname == "Zahumenska"
replace surname = "Zamorová" if surname == "Zamorova"
replace surname = "Zemánek" if surname == "Zemanek"
replace surname = "Špryňarová Gottletová" if surname == "špryňarová gottletová"
replace firstname = "Kristina" if firstname == "kristina"
replace surname = "Hynar" if firstname == "Hynar" & birthdate == td(25may1991)
replace firstname = "Adam" if firstname == "Hynar" & birthdate == td(25may1991)
replace surname = "Krajča" if firstname == "Krajča" & birthdate == td(15jul1977)
replace firstname = "Adrian" if firstname == "Krajča" & birthdate == td(15jul1977) | firstname == "Adrián" & birthdate== td(15jul1977)
replace surname = "Pavelková" if firstname == "Pavelková" & birthdate == td(02dec1957)
replace firstname = "Anna" if firstname == "Pavelková" & birthdate == td(02dec1957)
replace surname = "Šilar" if firstname == "Šilar" & birthdate == td(22feb1961)
replace firstname = "Bohuslav" if firstname == "Šilar" & birthdate == td(22feb1961)
replace surname = "Dudzik" if firstname == "Dudzik" & birthdate == td(12feb1952)
replace firstname = "Bronislav" if firstname == "Dudzik" & birthdate == td(12feb1952)
replace surname = "Nevludová" if firstname == "Nevludová" & birthdate == td(16dec1963)
replace firstname = "Bronislava" if firstname == "Nevludová" & birthdate == td(16dec1963)
replace surname = "Marková" if firstname == "Marková" & birthdate == td(30jan1957)
replace firstname = "Dagmar" if firstname == "Marková" & birthdate == td(30jan1957)
replace surname = "Vlk" if firstname == "Vlk" & birthdate == td(16may1974)
replace firstname = "Dalibor" if firstname == "Vlk" & birthdate == td(16may1974)
replace surname = "Kabrna" if firstname == "Kabrna" & birthdate == td(31aug1950)
replace firstname = "František" if firstname == "Kabrna" & birthdate == td(31aug1950)
replace surname = "Kauer" if firstname == "Kauer" & birthdate == td(27dec1959)
replace firstname = "František" if firstname == "Kauer" & birthdate == td(27dec1959)
replace surname = "Klásek" if firstname == "Klásek" & birthdate == td(16dec1955)
replace firstname = "František" if firstname == "Klásek" & birthdate == td(16dec1955)
replace surname = "Konrád" if firstname == "Konrád" & birthdate == td(26jun1951)
replace firstname = "František" if firstname == "Konrád" & birthdate == td(26jun1951)
replace surname = "Kosner" if firstname == "Kosner" & birthdate == td(04jul1959)
replace firstname = "František" if firstname == "Kosner" & birthdate == td(04jul1959)
replace surname = "Kulhánek" if firstname == "Kulhánek" & birthdate == td(10dec1950)
replace firstname = "František" if firstname == "Kulhánek" & birthdate == td(10dec1950)
replace surname = "Roháček" if firstname == "Roháček" & birthdate == td(06aug1947)
replace firstname = "František" if firstname == "Roháček" & birthdate == td(06aug1947)
replace surname = "Wawrzacz" if firstname == "Wawrzacz" & birthdate == td(01dec1957)
replace firstname = "František" if firstname == "Wawrzacz" & birthdate == td(01dec1957)
replace surname = "Čada" if firstname == "Čada" & birthdate == td(15dec1979)
replace firstname = "František" if firstname == "Čada" & birthdate == td(15dec1979)
replace surname = "Vrátná" if firstname == "Vrátná" & birthdate == td(01mar1971)
replace firstname = "Gabriela" if firstname == "Vrátná" & birthdate == td(01mar1971)
replace surname = "Jára" if firstname == "Jára" & birthdate == td(27apr1974)
replace firstname = "Leoš" if firstname == "Jára" & birthdate == td(27apr1974)
replace surname = "Heitel" if firstname == "Heitel" & birthdate == td(20nov1986)
replace firstname = "Lukáš" if firstname == "Heitel" & birthdate == td(20nov1986)
replace surname = "Charousová" if firstname == "Charousová" & birthdate == td(23feb1974)
replace firstname = "Monika" if firstname == "Charousová" & birthdate == td(23feb1974)
replace surname = "Jarošová" if firstname == "Jarošová" & birthdate == td(30dec1970)
replace firstname = "Monika" if firstname == "Jarošová" & birthdate == td(30dec1970)
replace surname = "Owczarzy" if surname == "Owcarzy"
replace surname = "Brázda" if firstname == "Brázda" & birthdate == td(07jun1954)
replace firstname = "Petr" if firstname == "Brázda" & birthdate == td(07jun1954)
replace surname = "Doležel" if firstname == "Doležel" & birthdate == td(29oct1962)
replace firstname = "Petr" if firstname == "Doležel" & birthdate == td(29oct1962)
replace surname = "Dostálek" if firstname == "Dostálek" & birthdate == td(18dec1973)
replace firstname = "Petr" if firstname == "Dostálek" & birthdate == td(18dec1973)
replace surname = "Drbal" if firstname == "Drbal" & birthdate == td(12sep1978)
replace firstname = "Petr" if firstname == "Drbal" & birthdate == td(12sep1978)
replace surname = "Hercik" if firstname == "Hercik" & birthdate == td(18oct1969)
replace firstname = "Petr" if firstname == "Hercik" & birthdate == td(18oct1969)
replace surname = "Kovář" if firstname == "Kovář" & birthdate == td(16jun1983)
replace firstname = "Petr" if firstname == "Kovář" & birthdate == td(16jun1983)
replace surname = "Krejčiřík" if firstname == "Krejčiřík" & birthdate == td(04nov1967)
replace firstname = "Petr" if firstname == "Krejčiřík" & birthdate == td(04nov1967)
replace surname = "Laco" if firstname == "Laco" & birthdate == td(06feb1964)
replace firstname = "Petr" if firstname == "Laco" & birthdate == td(06feb1964)
replace surname = "Macek" if firstname == "Macek" & birthdate == td(19aug1991)
replace firstname = "Petr" if firstname == "Macek" & birthdate == td(19aug1991)
replace surname = "Mašíček" if firstname == "Mašíček" & birthdate == td(10feb1978)
replace firstname = "Petr" if firstname == "Mašíček" & birthdate == td(10feb1978)
replace surname = "Mrňavý" if firstname == "Mrňavý" & birthdate == td(08dec1980)
replace firstname = "Petr" if firstname == "Mrňavý" & birthdate == td(08dec1980)
replace surname = "Pech" if firstname == "Pech" & birthdate == td(07nov1996)
replace firstname = "Petr" if firstname == "Pech" & birthdate == td(07nov1996)
replace surname = "Sklenář" if firstname == "Sklenář" & birthdate == td(20may1949)
replace firstname = "Petr" if firstname == "Sklenář" & birthdate == td(20may1949)
replace surname = "Pustějovský" if firstname == "Pustějovský" & birthdate == td(09may1971)
replace firstname = "René" if firstname == "Pustějovský" & birthdate == td(09may1971)
replace surname = "Broda" if firstname == "Broda" & birthdate == td(08feb1979)
replace firstname = "Richard" if firstname == "Broda" & birthdate == td(08feb1979)
replace surname = "Michenka" if firstname == "Michenka" & birthdate == td(23aug1970)
replace firstname = "Richard" if firstname == "Michenka" & birthdate == td(23aug1970)
replace surname = "Měchura" if firstname == "Měchura" & birthdate == td(28jun1978)
replace firstname = "Richard" if firstname == "Měchura" & birthdate == td(28jun1978)
replace surname = "Staržec" if firstname == "Staržec" & birthdate == td(23oct1969)
replace firstname = "Richard" if firstname == "Staržec" & birthdate == td(23oct1969)
replace surname = "Vymola" if firstname == "Vymola" & birthdate == td(23mar1968)
replace firstname = "Robert" if firstname == "Vymola" & birthdate == td(23mar1968)
replace surname = "Čapkovič" if firstname == "Čapkovič" & birthdate == td(22dec1973)
replace firstname = "Robert" if firstname == "Čapkovič" & birthdate == td(22dec1973)
replace surname = "Široký" if firstname == "Široký" & birthdate == td(30aug1970)
replace firstname = "Robert" if firstname == "Široký" & birthdate == td(30aug1970)
replace surname = "Hába" if firstname == "Hába" & birthdate == td(02mar1986)
replace firstname = "Roman" if firstname == "Hába" & birthdate == td(02mar1986)
replace surname = "Owczarzy" if firstname == "Owczarzy" & birthdate == td(18feb1968)
replace firstname = "Roman" if firstname == "Owczarzy" & birthdate == td(18feb1968)
replace surname = "Razdíková" if firstname == "Razdíková" & birthdate == td(21aug1961)
replace firstname = "Růžena" if firstname == "Razdíková" & birthdate == td(21aug1961)
replace surname = "Toningerová" if firstname == "Toningerová" & birthdate == td(09jul1946)
replace firstname = "Růžena" if firstname == "Toningerová" & birthdate == td(09jul1946)
replace surname = "Sedlák" if surname == "Sedlák BTh."
replace surname = "Šatná" if firstname == "Šatná" & birthdate == td(12jul1972)
replace firstname = "Světlana" if firstname == "Šatná" & birthdate == td(12jul1972)
replace surname = "Okamura" if firstname == "Okamura" & birthdate == td(04jul1972)
replace firstname = "Tomio" if firstname == "Okamura" & birthdate == td(04jul1972)
replace surname = "Balcar" if firstname == "Balcar" & birthdate == td(02jun1969)
replace firstname = "Tomáš" if firstname == "Balcar" & birthdate == td(02jun1969)
replace surname = "Cikán" if firstname == "Cikán" & birthdate == td(23mar1969)
replace firstname = "Tomáš" if firstname == "Cikán" & birthdate == td(23mar1969)
replace surname = "Hauzner" if firstname == "Hauzner" & birthdate == td(18oct1962)
replace firstname = "Tomáš" if firstname == "Hauzner" & birthdate == td(18oct1962)
replace surname = "Hrnčárek" if firstname == "Hrnčárek" & birthdate == td(14jul1975)
replace firstname = "Tomáš" if firstname == "Hrnčárek" & birthdate == td(14jul1975)
replace surname = "Jelínek" if firstname == "Jelínek" & birthdate == td(14jul1966)
replace firstname = "Tomáš" if firstname == "Jelínek" & birthdate == td(14jul1966)
replace surname = "Kadlec" if firstname == "Kadlec" & birthdate == td(08may1984)
replace firstname = "Tomáš" if firstname == "Kadlec" & birthdate == td(08may1984)
replace surname = "Kulhánek" if firstname == "Kulhánek" & birthdate == td(19may1988)
replace firstname = "Tomáš" if firstname == "Kulhánek" & birthdate == td(19may1988)
replace surname = "Sabol" if firstname == "Sabol" & birthdate == td(25apr1968)
replace firstname = "Vladimír" if firstname == "Sabol" & birthdate == td(25apr1968)
replace surname = "Suchý" if firstname == "Suchý" & birthdate == td(14nov1950)
replace firstname = "Vladimír" if firstname == "Suchý" & birthdate == td(14nov1950)
replace surname = "Vaněk" if firstname == "Vaněk" & birthdate == td(19apr1955)
replace firstname = "Vladimír" if firstname == "Vaněk" & birthdate == td(19apr1955)
replace surname = "Pracnová" if firstname == "Pracnová" & birthdate == td(09jun1959)
replace firstname = "Vladimíra" if firstname == "Pracnová" & birthdate == td(09jun1959)
replace surname = "Čermák" if firstname == "Čermák" & birthdate == td(17aug1955)
replace firstname = "Vlastibor" if firstname == "Čermák" & birthdate == td(17aug1955)
replace surname = "Drábek" if firstname == "Drábek" & birthdate == td(07apr1973)
replace firstname = "Vlastimil" if firstname == "Drábek" & birthdate == td(07apr1973)
replace surname = "Mojžíšek" if firstname == "Mojžíšek" & birthdate == td(20jun1960)
replace firstname = "Vlastimil" if firstname == "Mojžíšek" & birthdate == td(20jun1960)
replace surname = "Protivínský" if firstname == "Protivínský" & birthdate == td(15sep1969)
replace firstname = "Vlastimil" if firstname == "Protivínský" & birthdate == td(15sep1969)
replace firstname = "Luboslav" if firstname == "luboslav"
replace surname = "Šimorda" if firstname == "Šimorda" & birthdate == td(07sep1957)
replace firstname = "Václav" if firstname == "Šimorda" & birthdate == td(07sep1957)
replace surname = "Šimík" if firstname == "Šimík" & birthdate == td(13mar1961)
replace firstname = "Václav" if firstname == "Šimík" & birthdate == td(13mar1961)
replace surname = "Novák" if firstname == "Novák" & birthdate == td(29aug1973)
replace firstname = "Vítězslav" if firstname == "Novák" & birthdate == td(29aug1973)
replace surname = "Strejcová" if firstname == "Strejcová" & birthdate == td(30mar1953)
replace firstname = "Věra" if firstname == "Strejcová" & birthdate == td(30mar1953)
replace birthdate = td(15oct1961) if birthdate == td(25oct1961) & firstname == "Čechová"
replace surname = "Čechová" if firstname == "Čechová" & birthdate == td(15oct1961)
replace firstname = "Věra" if firstname == "Čechová" & birthdate == td(15oct1961)
replace surname = "Růžičková" if firstname == "Růžičková" & birthdate == td(12jan1966)
replace firstname = "Yvona" if firstname == "Růžičková" & birthdate == td(12jan1966)
replace surname = "Škurková" if firstname == "Škurková" & birthdate == td(14nov1970)
replace firstname = "Yvona" if firstname == "Škurková" & birthdate == td(14nov1970)
replace surname = "Habrnal" if firstname == "Habrnal" & birthdate == td(01may1957)
replace firstname = "Zbyněk" if firstname == "Habrnal" & birthdate == td(01may1957)
replace surname = "Bříza" if firstname == "Bříza" & birthdate == td(08jul1978)
replace firstname = "Zdeněk" if firstname == "Bříza" & birthdate == td(08jul1978)
replace surname = "Cikrle" if firstname == "Cikrle" & birthdate == td(30sep1963)
replace firstname = "Zdeněk" if firstname == "Cikrle" & birthdate == td(30sep1963)
replace surname = "Podal" if firstname == "Podal" & birthdate == td(12apr1953)
replace firstname = "Zdeněk" if firstname == "Podal" & birthdate == td(12apr1953)
replace surname = "Polách" if firstname == "Polách" & birthdate == td(16jan1961)
replace firstname = "Zdeněk" if firstname == "Polách" & birthdate == td(16jan1961)
replace surname = "Urbanová" if firstname == "Urbanová" & birthdate == td(12mar1953)
replace firstname = "Zdeňka" if firstname == "Urbanová" & birthdate == td(12mar1953)
replace surname = "Frňková" if firstname == "Frňková" & birthdate == td(16jun1962)
replace firstname = "Zuzana" if firstname == "Frňková" & birthdate == td(16jun1962)
replace surname = "Schneiderová" if firstname == "Schneiderová" & birthdate == td(07oct1974)
replace firstname = "Zuzana" if firstname == "Schneiderová" & birthdate == td(07oct1974)
replace surname = "Čech" if surname == "Čech Ing."
replace surname = "Koller" if firstname == "Koller" & birthdate == td(08jun1961)
replace firstname = "Čestmír" if firstname == "Koller" & birthdate == td(08jun1961)
replace surname = "Nováková" if firstname == "Nováková" & birthdate == td(06nov1963)
replace firstname = "Šárka" if firstname == "Nováková" & birthdate == td(06nov1963)
replace surname = "Boublík" if surname == "Boublik"
replace birthdate = td(10sep1974) if birthdate == td(10sep1947) & surname == "Boublík"
replace firstname = "Pavel" if firstname == "Ing Pavel"
replace firstname = "Kateřina" if firstname == "Mgr Kateřina"
replace firstname = "Vlastislav" if firstname == "Vlastimil" & birthdate == td(05jan1955) & surname == "Málek"
replace firstname = "Pavel" if firstname == "Mgr Pavel"
replace firstname = "Miroslav" if firstname == "Ing Miroslav"
replace firstname = "Martin" if firstname == "Mgr Martin"
replace surname = "Kočí Palkovská" if surname == "Kočí  Palkovská"
replace surname = "Rozmajzl" if surname == "Rozmajzl st."
replace surname = "Dušková - Smrčková" if surname == "Dušková,Smrčková" | surname == "Dušková, Smrčková"

replace firstname = "Tomáš" if firstname == "Tomas" | firstname == "Tomás" | firstname == "Tomaš"
replace firstname = "Jiří" if firstname == "Jiri" | firstname == "Jiři" | firstname == "Jirí" | firstname == "jiri" | firstname == "JIří"
replace firstname = "Ondřej" if firstname == "Ondrej" | firstname == "Onřej" | firstname == "Ondra"
replace firstname = "Václav" if firstname == "Vaclav"
replace firstname = "Lukáš" if firstname == "Lukas" | firstname == "Lukás" | firstname == "Lukaš" | firstname == "lukas" | firstname == "lukáš"
replace firstname = "Klára" if firstname == "Klara"
replace firstname = "Přemysl" if firstname == "Premysl"
replace firstname = "Leoš" if firstname == "Leos"
replace firstname = "Štěpán" if firstname == "Stepan" | firstname == "Stěpán" | firstname == "Stepán" | firstname == "Štepan" | firstname == "Štepán" | firstname == "Štěpan"
replace firstname = "Kateřina" if firstname == "Katerina" | firstname == "kateřina" | firstname == "kateria"
replace firstname = "Magdaléna" if firstname == "Magdalena"
replace firstname = "Pavlína" if firstname == "Pavlina"
replace firstname = "Eliška" if firstname == "Eliska"
replace firstname = "Matěj" if firstname == "Matej" | firstname == "MATEJ" | firstname == "matej" 
replace firstname = "Michal" if firstname == "michal"
replace firstname = "Veronika" if firstname == "veronika"
replace firstname = "Zdeněk" if firstname == "zdeněk"
replace firstname = "Lucie" if firstname == "lucie"
replace firstname = "Jan" if firstname == "jan" | firstname == "Honza"
replace firstname = "Ivan" if firstname == "IVAN"
replace firstname = "Libor" if firstname == "Bc Libor"
replace firstname = "Šárka" if firstname == "Sarka" | firstname == "Šarka" | firstname == "Sárka" | firstname == "šarka"
replace firstname = "Boleslav" if firstname == "boleslav"
replace firstname = "Tadeáš" if firstname == "Tadeas"
replace firstname = "Aleš" if firstname == "Ales"
replace firstname = "Miluše" if firstname == "Miluse"
replace firstname = "Miroslav" if firstname == "miroslav"
replace firstname = "Martin" if firstname == "martin"
replace firstname = "Zuzana" if firstname == "zuzana"
replace firstname = "Anežka" if firstname == "Anezka" 
replace firstname = "Zbyšek" if firstname == "Zbysek"
replace firstname = "Tom" if firstname == "tom"
replace firstname = "Dagmar" if firstname == "DAGMAR"
replace firstname = "Karel" if firstname == "KAREL"
replace firstname = "František" if firstname == "františek" | firstname == "Frantisek" | firstname == "FRANTISEK" | firstname == "FRANTIŠEK"
replace firstname = "Františka" if firstname == "Frantiska"
replace firstname = "Božena" if firstname == "Bozena"
replace firstname = "René" if firstname == "Rene"
replace firstname = "Vítězslav" if firstname == "Víťa"
replace firstname = "Přemek" if firstname == "Premek"
replace firstname = "Martin" if firstname == "Martín"
replace firstname = "Jaroslava" if firstname == "Jarka"
replace firstname = "Jakub" if firstname == "jakub" | firstname == "Kuba" | firstname == "kuba"


 
order polparty year surname firstname birthdate acadegree_bn acadegree_an

replace acadegree_an = "." if acadegree_an == "JUDr. Ing." | acadegree_an == "JUDr." | acadegree_an == "brig. gen. v. v." | acadegree_an == "Belcredi" | acadegree_an == "RNDr." | acadegree_an == "s.r.o." | acadegree_an == "Liga spravedlnosti" | acadegree_an == "Bc." | acadegree_an == "Prchal" |  acadegree_an == "Ing. arch." | acadegree_an == "Ing." | acadegree_an == "Ing. Mgr." | acadegree_an == "Mgr." |  acadegree_an == "MUDr."
 
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

replace acadegree_bn = "." if acadegree_bn == "PhD." | acadegree_bn == "L." | acadegree_bn == "Paní" | acadegree_bn == "Ph. D." | acadegree_bn == "Ph.D." | acadegree_bn == "Dis" | acadegree_bn == "M.A." | acadegree_bn == "B.A." | acadegree_bn == "Dis."

replace acadegree_bn = "Bc." if acadegree_bn == "Bc. MSc." | acadegree_bn == "Bc. DiS." | acadegree_bn == "Bc,MSc." | acadegree_bn == "Bc" | acadegree_bn == "Bc," | acadegree_an == "Bc."

replace acadegree_bn = "Ing." if acadegree_bn == "Ing" | acadegree_bn == "Ing. CSc." | acadegree_bn == "Ing.  PhD." | acadegree_bn == "Ing.  MBA" | acadegree_bn == "Ing. PhD." | acadegree_bn == "Ing. MBA" | acadegree_bn == "Ing.  Ph." | acadegree_bn == "Ing. Ph.D."  | acadegree_bn == "Dipl. Ing." | acadegree_bn == "Ing. Lic." | acadegree_bn == "Ing., MBA" | acadegree_an == "Ing." |  acadegree_an == "Ph.D.,ING-PAED IGIP" | acadegree_an == "Ing.,MBA" | acadegree_bn == ".Ing."
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

gen i = 1

save donation_data_clean.dta, replace 

