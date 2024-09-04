

foreach d of global donationform {


use "intermediate_data\donation_data_`d'.dta", clear 

label define polparty 1 "kducsl" 2 "kscm" 3 "ods" 4 "pirati" 5 "spd" 6 "stan" 7 "top09" 8 "ano" 9 "cssd" 10 "svobodni" 11 "trikolora" 12 "prisaha"
label values polparty polparty

sort polparty surname firstname birthdate year

gen acadegree_an = "."
replace acadegree_an = titulza 
replace acadegree_an = "." if acadegree_an == ""

gen acadegree_bn = "." 
replace acadegree_bn = titulpřed if acadegree_bn != " "
replace acadegree_bn = "." if acadegree_bn == ""

tostring acadegree_an, replace 
tostring acadegree_bn, replace 

gen `d'_donation = .
capture: replace `d'_donation = částkaKč if částkaKč !=. 
capture: replace `d'_donation = hodnotaBÚPKč if hodnotaBÚPKč !=. 

keep surname firstname acadegree_an acadegree_bn birthdate year polparty `d'_donation

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



*** common typos in firstnames ***
replace firstname = "Aleš" if firstname == "Ales"
replace firstname = "Anežka" if firstname == "Anezka" 
replace firstname = "Antonín" if firstname == "Antonin"
replace firstname = "Alena" if firstname == "ALENA"
replace firstname = "Boleslav" if surname == "boleslav"
replace firstname = "Božena" if firstname == "Bozena"
replace firstname = "Dagmar" if firstname == "DAGMAR"
replace firstname = "David" if firstname == "DAVID" | firstname == "david"
replace firstname = "Daniel" if firstname == "Daniël"
replace firstname = "Eliška" if firstname == "Eliska" | firstname == "eliska"
replace firstname = "František" if firstname == "františek" | firstname == "Frantisek" | firstname == "FRANTISEK" | firstname == "FRANTIŠEK"  | firstname == "frantisek"
replace firstname = "Františka" if firstname == "Frantiska"
replace firstname = "Ivo" if firstname == "Mgr Ivo" | firstname == "IVO"
replace firstname = "Ignác" if firstname == "ignác"
replace firstname = "Ivan" if firstname == "IVAN"
replace firstname = "Ivana" if firstname == "ivana"
replace firstname = "Iva" if firstname == "iva"
replace firstname = "Igor" if firstname == "IGOR"
replace firstname = "Janka" if firstname == "janka" 
replace firstname = "Jana" if firstname == "jana"
replace firstname = "Josef" if firstname == "josef" | firstname == "JOsef" 
replace firstname = "Jiří" if firstname == "jiří" | firstname == "Jiri" | firstname == "Jiři" | firstname == "Jirí" | firstname == "jiri" | firstname == "JIří" | firstname == "JIŘÍ" | firstname == "Jaří" | firstname == "jiří" | firstname == "Jjiří"
replace firstname = "Jan" if firstname == " Jan" | firstname == "jan" | firstname == "Honza" | firstname == "JAN"
replace firstname = "Jeroným" if firstname == "Jeronym" 
replace firstname = "Jaroslava" if firstname == "Jarka" | firstname == "jaroslava"
replace firstname = "Jakub" if firstname == "jakub" | firstname == "Kuba" | firstname == "kuba" | firstname == "Jaku"
replace firstname = "Jindřich" if firstname == "Jindrich" | firstname == "jindřich" | firstname == "jindrich"
replace firstname = "Jaroslav" if firstname == "jaroslav" | firstname == "JAROSLAV"
replace firstname = "Kristina" if firstname == "kristina"
replace firstname = "Klára" if firstname == "Klara" | firstname == "klara"
replace firstname = "Kateřina" if firstname == "Katerina" | firstname == "kateřina" | firstname == "kateria"
replace firstname = "Karel" if firstname == "KAREL"
replace firstname = "Lukáš" if firstname == "Lukas" | firstname == "Lukás" | firstname == "Lukaš" | firstname == "lukas" | firstname == "lukáš"
replace firstname = "Leoš" if firstname == "Leos"
replace firstname = "Lucie" if firstname == "lucie"
replace firstname = "Libor" if firstname == "Bc Libor"
replace firstname = "Lubuše" if firstname == "LIBUŠE"
replace firstname = "Miroslav" if firstname == "MIROSLAV" | firstname == "miroslav"
replace firstname = "Marin" if firstname == "marin" 
replace firstname = "Martin" if firstname == "martin" | firstname == "Martín" 
replace firstname = "Markéta" if firstname == "Marketa"  | firstname == "markéta"
replace firstname = "Magdaléna" if firstname == "Magdalena" | firstname == "Magda"
replace firstname = "Matěj" if firstname == "Matej" | firstname == "MATEJ" | firstname == "matej" 
replace firstname = "Michal" if firstname == "michal"
replace firstname = "Miluše" if firstname == "Miluse"
replace firstname = "Marek" if firstname == "marek"
replace firstname = "Milan" if firstname == "MILAN"
replace firstname = "Naděžda" if firstname == "Naděža"
replace firstname = "Ota" if firstname == " Ota"
replace firstname = "Ondřej" if firstname == "Ondrej" | firstname == "Onřej" | firstname == "Ondra"
replace firstname = "Olga" if firstname == "olga" | firstname == "Olg"
replace firstname = "Petr" if firstname == "petr" | firstname == "Pertr" | firstname == "PETR" | firstname == "petr"
replace firstname = "Petra" if firstname == "petra"
replace firstname = "Přemysl" if firstname == "Premysl"
replace firstname = "Pavlína" if firstname == "Pavlina"
replace firstname = "Přemek" if firstname == "Premek"
replace firstname = "Pavol" if firstname == "PAVOL"
replace firstname = "Richard" if firstname == "RichaRD"
replace firstname = "René" if firstname == "Rene"
replace firstname = "Radka" if firstname == "RRadka"
replace firstname = "Rudolf" if firstname == "rudolf"
replace firstname = "Roman" if firstname == "ROMAN"
replace firstname = "Simon" if firstname == "simon"
replace firstname = "Šimon" if firstname == "šimon"
replace firstname = "Štěpánka" if firstname == "Štef." | firstname == "Stěpánka" | firstname == "Stepánka" | firstname == "Štepanka" | firstname == "Štepánka" | firstname == "stepanka"
replace firstname = "Štěpán" if firstname == "Stepan" | firstname == "Stěpán" | firstname == "Stepán" | firstname == "Štepan" | firstname == "Štepán" | firstname == "Štěpan"
replace firstname = "Šárka" if firstname == "Sarka" | firstname == "Šarka" | firstname == "Sárka" | firstname == "šarka" | firstname == "šárka"
replace firstname = "Štefan" if firstname == "Stefan"
replace firstname = "Tomáš" if firstname == "IngTomáš" | firstname == "Tomas" | firstname == "Tomás" | firstname == "Tomaš" | firstname == "tomas" | firstname == "tomáš" | firstname == "Tomá"
replace firstname = "Tom" if surname == "tom"
replace firstname = "Tadeáš" if firstname == "Tadeas"
replace firstname = "Vojtěch" if firstname == "Vojtech"
replace firstname = "Václav" if firstname == " Václav" | firstname == "Vaclav" | firstname == "VÁCLAV" | firstname == "vaclav" | firstname == "václav"
replace firstname = "Veronika" if firstname == "veronika"
replace firstname = "Vítězslav" if firstname == "Víťa"
replace firstname = "Vladislav" if firstname == "VLADISLAV"
replace firstname = "Zbyšek" if firstname == "zbyšek"  |  firstname == "Zbysek"
replace firstname = "Zdeněk" if firstname == "ZDENĚK" | firstname == "Zdaněk" | firstname == "zdeněk" | firstname == "Zdenek"
replace firstname = "Zuzana" if firstname == "zuzana"

**** switching firstname & surnames	 typos in surnames & others ****
replace firstname = "Robert" if surname == "Huneš" & birthdate == td(09nov1968)
replace firstname = "Pavel" if firstname == "Pavel,MVDr., Ph.D, M"
replace firstname = "Vladimír" if firstname == "Vladimít"
replace surname = "Aulická Jírovcová" if surname == "Aulická -Jírovcová"
replace surname = "Hemzáčková" if surname == "Hemzáková"
replace firstname = "Vítězslav" if firstname == "Vítěslav" & surname == "Lapčík" & birthdate == td(21may1965)
replace surname = "Orszulik" if surname == "Orszulík" & birthdate == td(27feb1978)
replace surname = "Adamík" if surname == "ADAMÍK" & birthdate == td(16mar1962)
replace surname = "Bohutínský" if surname == "Bohutínský - soudní exekutor" & birthdate == td(23oct1966)
replace surname = "Hřebec" if surname == "HŘEBEC"
replace surname = "Kalabus" if surname == "KALABUS"
replace surname = "Kofroň" if surname == "KOFROŇ"
replace surname = "Kosubová" if surname == "KOSUBOVÁ"
replace surname = "Polášek" if surname == "POLÁŠEK"
replace surname = "Palyza" if surname == "Palyza st."
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
replace surname = "Frauknecht" if surname == "frauknecht"
replace surname = "Gronych" if surname == "gronych" 
replace surname = "Hanzal" if surname == "hanzal" 
replace surname = "Holas" if surname == "holas" 
replace surname = "Hovorková" if surname == "hovorková" 
replace surname = "Jedlička" if surname == "jedlička" 
replace surname = "Junga" if surname == "junga" 
replace surname = "Kabelka" if surname == "kabelka"
replace surname = "Karas" if surname == "karas" 
replace surname = "Koubková" if surname == "koubkova" 
replace surname = "Karger" if surname == "karger"
replace surname = "Kindl" if surname == "kindl" 
replace surname = "Michailidu" if surname == "michailidu" 
replace surname = "Kolář" if surname == "kolář" 
replace surname = "Kopecká" if surname == "kopecká"
replace surname = "Krištiak" if surname == "krištiak"
replace surname = "Kubovciak" if surname == "kubovciak"
replace surname = "Kuře" if surname == "kure"
replace surname = "Libich" if surname == "libich"
replace surname = "Lišková" if surname == "liskova"
replace surname = "Myslivec" if surname == "myslivec"
replace surname = "Pánek" if surname == "panek"
replace surname = "Novotný" if surname == "novotný"
replace surname = "Selinger" if surname == "selinger" | surname == "selinger/knihkupectví"
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
replace surname = "Vedlová" if surname == "vendlova"
replace surname = "Miguele" if surname == "miguele"
replace surname = "Navrat" if surname == "navrat"
replace surname = "Pety" if surname == "pety"
replace surname = "Foltyn" if surname == "foltyn(bolsoncar)"
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
replace firstname = "Zdeněk" if firstname == "Zdenek" & birthdate == td(21nov1955)
replace surname = "Hromadková" if surname == "Hromadkova"
replace surname = "Seidl" if firstname == "Seidl" & birthdate == td(02feb1993)
replace firstname = "Jakub" if firstname == "Seidl" & birthdate == td(02feb1993)
replace surname = "Jedelský" if surname == "Jedelsky" 
replace surname = "Jirásková" if surname == "Jiraskova"
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
replace firstname = "Pavlína" if firstname == "Pavlína Šetková"
replace surname = "Sverák" if surname == "Śverák"
replace surname = "Antić" if surname == "ANTIĆ"
replace firstname = "Adam Zdeněk" if surname == "Adam Novák" & birthdate == td(01may1980)
replace surname = "Novák" if surname == "Adam Novák" & birthdate == td(01may1980)
replace surname = "Adamec" if surname == "Adamec ml."
replace firstname = "Luis Alberto Pinto" if surname == "Alberto Pinto Castillo" & birthdate == td(08apr1969)
replace surname = "Castillo" if surname == "Alberto Pinto Castillo" & birthdate == td(08apr1969)
replace surname = "Németh" if firstname == "Németh" & birthdate == td(24aug1959)
replace firstname = "Karel" if surname == "Németh" & birthdate == td(24aug1959)
replace surname = "Cvetan" if firstname == "Cvetan" & birthdate == td(17sep1976)
replace firstname = "Martin" if surname == "Cvetan" & birthdate == td(17sep1976)
replace surname = "Tetík" if firstname == "Tetík" & birthdate == td(22jul1974)
replace firstname = "Rudolf" if surname == "Tetík" & birthdate == td(22jul1974)
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
replace surname = "Bradáč" if firstname == "Bradáč" & birthdate == td(28aug1956)
replace firstname = "František" if firstname == "Bradáč" & birthdate == td(28aug1956)
replace surname = "Vávra" if firstname == "Vávra" & birthdate == td(26jan1967)
replace firstname = "Radek" if surname == "Vávra" & birthdate == td(26jan1967)
replace surname = "Vávra" if firstname == "Vávra" & birthdate == td(12sep1983)
replace firstname = "Radek" if surname == "Vávra" & birthdate == td(12sep1983)
replace surname = "Jošt" if surname == "Jost"
replace surname = "Julínková" if surname == "Julinkova"
replace surname = "Pecićová" if surname == "Pecicova"
replace surname = "Drastík" if surname == "Drastik" & birthdate == td(29aug1965)
replace surname = "Dželinová" if surname == "Dželiová" & birthdate == td(15mar1971)
replace surname = "Hauzner" if surname == "Hausner" & birthdate == td(15mar1975)
replace surname = "Kajprt" if surname == "Kajpr" & birthdate == td(27jun1969)
replace surname = "Kauzlaričová" if surname == "Kauzarovičková" & birthdate == td(28nov1961)
replace surname = "Kostelňák" if surname == "Kostelník" & birthdate == td(06oct1954)
replace surname = "Löwenthalová" if surname == "Löwenthálová" & birthdate == td(10dec1969)
replace surname = "Malchar" if surname == "Malcher" & birthdate == td(10oct1941)
replace surname = "Nácerová" if surname == "Nácalová" & birthdate == td(06jun1944)
replace surname = "Putlík" if surname == "Pytlík" & birthdate == td(26jul1956)
replace surname = "Stankayová" if surname == "Stankyová" & birthdate == td(26jan1979)
replace surname = "Topinková" if surname == "Topinkovvá" & birthdate == td(01jun1956)
replace surname = "Vondrášková" if surname == "Vondrášová" & birthdate == td(04mar1959)
replace surname = "Vycudiliková" if surname == "Vychodilová" & birthdate == td(10nov1950)
replace surname = "Čomová" if surname == "Čoková" & birthdate == td(25jun1938)
replace surname = "Zachariaš" if surname == "Zachariáš" & birthdate == td(28apr1993)
replace surname = "Břicháček" if surname == "Břichacek" & birthdate == td(05oct1976)
replace surname = "Frolík" if surname == "Frolik" & birthdate == td(04apr1963)
replace surname = "Slovák" if surname == "Slovak" & birthdate == td(08mar1959)
replace surname = "Chlupáček" if surname == "Chlupacek" & birthdate == td(07sep1986)
replace surname = "Davidová" if surname == "Davidova" & birthdate == td(07jul1990)
replace surname = "Dřevikovský" if surname == "Dřevíkovský" & birthdate == td(07jul1995)
replace surname = "Frýbortová" if surname == "Frýbortova" & birthdate == td(09apr1967)
replace surname = "Haisová" if surname == "Haisova" & birthdate == td(20feb1987)
replace surname = "Hanzlík" if surname == "Hanzlik" & birthdate == td(04mar1992)
replace surname = "Herbrichová" if surname == "Herbrichova" & birthdate == td(13jun1979)
replace surname = "Jechová" if surname == "Jechova" & birthdate == td(03dec1984)
replace surname = "Kadaník" if surname == "Kadanik" & birthdate == td(03aug1982)
replace surname = "Macinský" if surname == "Macinsky" & birthdate == td(07mar1984)
replace surname = "Naiclerová" if surname == "Naicerová" & birthdate == td(20jun1962)
replace surname = "Pavlovič" if surname == "Pavlovic" & birthdate == td(15may1984)
replace surname = "Popová" if surname == "Popova" & birthdate == td(13jun1976)
replace surname = "Poživilová Michailidu" if surname == "Poživilová" & birthdate == td(20aug1980)
replace surname = "Prokšová Zuská" if surname == "Prokšová" & birthdate == td(21jul1986)
replace surname = "Rozhoň" if surname == "Rozhon" & birthdate == td(03jan1996)
replace surname = "Rychtář" if surname == "Rychtar" & birthdate == td(14aug1990)
replace surname = "Saleh Salem" if surname == "Salem" & birthdate == td(17mar1987)
replace surname = "Strachotová" if surname == "Strachotova" & birthdate == td(17nov1991)
replace surname = "Svatuška" if surname == "Svatuska" & birthdate == td(12mar1986)
replace surname = "Tenzerová" if surname == "Tenzerova" & birthdate == td(30nov1980)
replace surname = "Ujfaluši" if surname == "Ujfalusi" & birthdate == td(17jul1979)
replace surname = "Vonešová" if surname == "Vonesova" & birthdate == td(27jun1991)
replace surname = "Bitomský" if surname == "Bitomký" & birthdate == td(06feb1981)
replace surname = "Břečťál" if surname == "Břešťál" & birthdate == td(05nov1950)
replace surname = "Burkhardová" if surname == "Burkhadová" & birthdate == td(13jul1953)
replace surname = "Kokolusová" if surname == "Kokolousová" & birthdate == td(29may1967)
replace surname = "Bukovanská" if surname == "Bukanovská" & birthdate == td(11sep1964)
replace surname = "Hanus" if surname == "Hanuš" & birthdate == td(22jul1971)
replace surname = "Hřebačková" if surname == "Hřebáčková" & birthdate == td(08mar1973)
replace surname = "Knauer Belcredi" if surname == "Knauer" & birthdate == td(19nov1982)
replace surname = "Vostradovský" if surname == "Vostadovský" & birthdate == td(22nov1970)
replace surname = "Švancara" if surname == "Švanacara" & birthdate == td(21sep1976)
replace surname = "Kmiećová" if surname == "Kmiecová" & birthdate == td(10jul1972)
replace surname = "Licehemer" if surname == "Licehamr" & birthdate == td(30jul1965)
replace surname = "Hromádka" if surname == "Hromádko" & birthdate == td(28nov1990)
replace surname = "Drož" if surname == "Drož Ml." & birthdate == td(03may1977)
replace surname = "Donatová" if surname == "Donátová" & birthdate == td(05dec1970)
replace surname = "Pláňava" if surname == "Plánava" & birthdate == td(30may1956)
replace surname = "David" if surname == "DAVID" & birthdate == td(03jan1983)
replace surname = "Uličný" if surname == "Daniel Uličný" & birthdate == td(17apr1976)
replace firstname = "Daniel" if surname == "Uličný" & birthdate == td(17apr1976)
replace surname = "Fanta" if surname == "FANTA" & birthdate == td(15apr1959)
replace surname = "Flekalová-Sláviková" if surname == "Flekalová" & birthdate == td(28aug1963)
replace surname = "Hrabě" if surname == "HRABĚ" & birthdate == td(03apr1964)
replace firstname = "Dominik" if firstname == "Dominik H" & birthdate == td(25oct1993)
replace firstname = "Jiři" if firstname == "" & birthdate == td(13nov1987)
replace surname = "Pešík" if surname == "Ing. Jiří Pešík" & birthdate == td(13nov1987)
replace firstname = "Michal" if firstname == "" & birthdate == td(02apr1970)
replace surname = "Vlačiha" if surname == "Ing. Michal Vlačiha" & birthdate == td(02apr1970)
replace surname = "Stránský" if surname == "Ing. Stránský" & birthdate == td(30oct1952)
replace firstname = "Pavel" if firstname == "Dr" & birthdate == td(23sep1971)
replace surname = "Pospíšil" if surname == "Ing. Pavel Pospíšil" & birthdate == td(23sep1971)
replace firstname = "Jan" if surname == "Jan Petržílka" & birthdate == td(18jun1971)
replace surname = "Petržílka" if surname == "Jan Petržílka" & birthdate == td(18jun1971)
replace surname = "Janeček" if surname == "Janeček Ing."
replace firstname = "Josef" if surname == "Josef Kubíček" & birthdate == td(22may1951)
replace surname = "Karlík" if surname == "KARLÍK"
replace surname = "Konopásek" if surname == "Konopásek, Ing." & birthdate == td(21jan1964)
replace firstname = "Kristýna" if surname == "Kristýna Hosáková" & birthdate == td(20jul1989)
replace surname = "Hosáková" if surname == "Kristýna Hosáková" & birthdate == td(20jul1989)
replace firstname = "Ladislava" if surname == "Ladislava Ondráčková" & birthdate == td(03dec1976)
replace surname = "Ondráčková" if surname == "Ladislava Ondráčková" & birthdate == td(03dec1976)
replace surname = "Lažanský" if surname == "LAŽANSKÝ"
replace firstname = "Lenka" if surname == "Lenka Wagnerová" & birthdate == td(15nov1960)
replace surname = "Wagnerová" if surname == "Lenka Wagnerová" & birthdate == td(15nov1960)
replace surname = "Macich" if surname == "Macich ml." 
replace firstname = "Martin" if surname == "Martin Bosák" & birthdate == td(26jul1991)
replace surname = "Bosák" if surname == "Martin Bosák" & birthdate == td(26jul1991)
replace firstname = "Miroslav" if surname == "Miroslav Nevřela" & birthdate == td(06sep1965) 
replace surname = "Nevřela"   if surname == "Miroslav Nevřela" & birthdate == td(06sep1965) 
replace firstname = "Ondřej" if surname == "Ondřej Musil" & birthdate == td(01oct1986) 
replace surname = "Musil"   if surname == "Ondřej Musil" & birthdate == td(01oct1986) 
replace surname = "Praum" if surname == "PRAUM" 
replace firstname = "Petr" if surname == "Petr Foch" & birthdate == td(01mar1946) 
replace surname = "Foch"   if surname == "Petr Foch" & birthdate == td(01mar1946) 
replace firstname = "Petr" if surname == "Petr Čermák" & birthdate == td(17mar1986) 
replace surname = "Čermák"   if surname == "Petr Čermák" & birthdate == td(17mar1986) 
replace firstname = "Philip" if firstname == "Phillip" & birthdate == td(05jul1980) 
replace surname = "Přibyl" if surname == "Pribyl" & birthdate == td(03jun1986)
replace surname = "Priesterová" if surname == "Priesterová Mgr." & birthdate == td(24nov1942)
replace surname = "Kubíček" if surname == "Josef Kubíček" & birthdate == td(22may1951)
replace firstname = "Josef" if surname == "Josef Kubíček" & birthdate == td(22may1951)
replace firstname = "Jakub Jan" if surname == "Jakub Ptáček" & birthdate == td(08oct1978)
replace surname = "Ptáček" if surname == "Jakub Ptáček" & birthdate == td(08oct1978)
replace firstname = "Radek" if surname == "Radek Boček" & birthdate == td(15mar1974)
replace surname = "Boček" if surname == "Radek Boček" & birthdate == td(15mar1974)
replace surname = "Skybová" if surname == "Skybova" & birthdate == td(05jul1973)
replace surname = "Staněk" if surname == "Stanek" & birthdate == td(04nov1955)
replace firstname = "František" if firstname == "Staněk" & birthdate == td(04nov1955)
replace surname = "Straková" if surname == "Strakova" & birthdate == td(30aug1987)
replace surname = "Syrový" if surname == "Syrovy" & birthdate == td(29jun1977)
replace surname = "Tlamka" if surname == "TLAMKA" 
replace surname = "Vagunda" if surname == "Vagunda-Drgáč" & birthdate == td(12nov1976)
replace firstname = "Petra Victoria" if surname == "Victoria Priester" & birthdate == td(06oct1987)
replace surname = "Priester" if surname == "Victoria Priester" & birthdate == td(06oct1987)
replace firstname = "Vítek" if surname == "Vítek Zažímal" & birthdate == td(23jun1987)
replace surname = "Zažímal" if surname == "Vítek Zažímal" & birthdate == td(23jun1987)
replace firstname = "Vít" if firstname == "ing Vít" & birthdate == td(03dec1956)
replace surname = "Beňová" if surname == "beňová" 
replace surname = "Bihary" if surname == "bihary"
replace surname = "Dušek" if surname == "dušek"
replace surname = "Havel" if surname == "havel"
replace surname = "Čermák" if surname == "ermák" & birthdate == td(17mar1986)
replace surname = "Jágrová" if surname == "jágrová"
replace surname = "Král" if surname == "král"
replace surname = "Kašparova" if surname == "kašparova"
replace surname = "Kubát" if surname == "kubat"
replace surname = "Kučera" if surname == "kučera"
replace surname = "Med" if surname == "med"
replace surname = "Palounek" if surname == "palounek"
replace surname = "Ptáček" if surname == "ptacek"
replace surname = "Veisová" if surname == "veisová"
replace firstname = "Markéta" if surname == "ta Karešová" & birthdate == td(06jun1972)
replace surname = "Karešová" if surname == "ta Karešová" & birthdate == td(06jun1972)
replace firstname = "Petr" if surname == "vidrman" & birthdate == td(20jun1975)
replace surname = "Vidrman" if surname == "vidrman" & birthdate == td(20jun1975)
replace surname = "Šrámková" if surname == "ŠRÁMKOVÁ"
replace surname = "Belžík" if surname == "BELŽÍK"
replace surname = "Beníšek" if surname == "BENÍŠEK"
replace firstname = "Petr" if firstname == "Peter" & birthdate == td(18jan1970)
replace surname = "Bedroň" if surname == "Bedron" & birthdate == td(01may1974)
replace surname = "Benešová" if surname == "Benesova" & birthdate == td(26jan1974)
replace surname = "Bernátek ml." if surname == "Bernátek" & birthdate == td(22aug1997)
replace surname = "Bečvář" if surname == "Bečvar" & birthdate == td(03jan2022)
replace surname = "Blažková-Šípková" if surname == "Blažková Šípková" & birthdate == td(12dec1955)
replace surname = "Brančík" if surname == "Brančík ml."
replace surname = "Brančík" if surname == "Brančík st."
replace surname = "Brávek" if surname == "Brávek, MVDr."
replace surname = "Bébar" if surname == "BÉBAR"
replace surname = "Novák" if surname == "C. Novák"
replace surname = "Cvachovec" if surname == "CVACHOVEC"
replace surname = "Gabaj" if surname == "GABAJ"
replace surname = "Gottwaldová" if surname == "GOTTWALDOVÁ"
replace firstname = "Miroslav" if firstname == "Miloslav" & birthdate == td(23may1952) & surname == "Gajdůšek"
replace firstname = "Miloš" if firstname == "MudrMiloš" & birthdate == td(16jan2022)
replace surname = "Gundackerová" if surname == "Gundackerova" & birthdate == td(22jan1977)
replace surname = "Göndör" if surname == "GÖNDÖR"
replace surname = "Humplík" if surname == "HUMPLÍK"
replace surname = "Hlinovský" if surname == "Hlinovsky" & birthdate == td(17sep1950)
replace surname = "Hlinovský" if surname == "Hlinovsky" & birthdate == td(17nov2000)
replace firstname = "Evžen" if firstname == "E" & birthdate == td(17nov2000)
replace surname = "Hrnčář" if surname == "Hrnčár" & birthdate == td(12sep1979)
replace firstname = "Andrea" if firstname == "Andrea Hrychová" & birthdate == td(09dec1973)
replace firstname = "Simona" if firstname == "Bc Simona" & birthdate == td(26jan1972)
replace firstname = "Elena" if firstname == "Elana" & birthdate == td(23apr1984)
replace surname = "Hábová" if surname == "Hábrová" & birthdate == td(15jul1975)
replace surname = "Hädler Stirská" if birthdate == td(26oct1954) & (surname == "Hädler" | surname == "Händlerová" )
replace surname = "Šustek" if surname == "JUDr. Šustek" & birthdate == td(14nov1965) | birthdate == td(05may1996)
replace firstname = "Jan" if surname == "Jan Fiala"
replace surname = "Fiala" if surname == "Jan Fiala"
replace surname = "Grác" if surname == "Jan Grác"
replace surname = "Šnek" if surname == "Jan Šnek"
replace surname = "Strnadel" if surname == "Josef Strnadel"
replace surname = "Fulková" if surname == "Josefína Fulková"
replace surname = "Karlík" if surname == "KARLÍK"
replace surname = "Koželuh" if surname == "KOŽELUH"
replace surname = "Konšel" if surname == "Konšel Ml." & birthdate == td(25jul1986)
replace surname = "Kozák" if surname == "Kozák Mgr. Bc."
replace surname = "Kuchař" if surname == "Kuchař Mgr"
replace surname = "Křížek" if surname == "KŘÍŽEK"
replace surname = "Lanz" if surname == "LANZ"
replace surname = "Lochner" if surname == "LOCHNER"
replace surname = "Lecjaks" if surname == "Lecjak" & birthdate == td(22apr1966)
replace birthdate = td(11oct1963) if birthdate == td(11jan1963) & surname == "Liška"
replace surname = "Luběník" if surname == "Lubenik" & (birthdate == td(29jul1963) | birthdate == td(12sep1975))
replace surname = "Malovec" if surname == "MALOVEC"
replace surname = "Melš" if surname == "MELŠ"
replace surname = "Majerová Zahradníková" if surname == "Majerová" & birthdate == td(28jun1972)
replace firstname = "Marian Anna" if surname == "Maria Brodinová" & birthdate == td(03jun1982)
replace surname = "Brodinová" if surname == "Maria Brodinová" & birthdate == td(03jun1982)
replace firstname = "Thomas Maria" if surname == "Maria Miklas" & birthdate == td(18nov1963)
replace surname = "Miklas" if surname == "Maria Miklas" & birthdate == td(18nov1963)
replace firstname = "Marie Dana" if surname == "Marie Černá" & birthdate == td(08dec1966)
replace surname = "Černá" if surname == "Marie Černá" & birthdate == td(08dec1966)
replace surname = "Míšek" if surname == "MÍŠEK"
replace surname = "Pevný" if surname == "PEVNÝ"
replace surname = "Pikart" if surname == "PIKART"
replace surname = "Pekárek" if surname == "Pekárek Ing."
replace firstname = "Radomír" if firstname == "Bc Radomír"
replace firstname = "Jaroslav" if firstname == "Jaroslav Prošek"
replace surname = "Reinbergová" if surname == "REINBERGOVÁ"
replace surname = "Ridzoň" if surname == "Ridzon"
replace surname = "Riško" if surname == "Risko"
replace firstname = "Miloslav" if firstname == "Miroslav" & birthdate == td(07dec1969) & surname == "Robotka"
replace surname = "Schwanzer" if surname == "SCHWANZER"
replace surname = "Spáčil-Elba" if surname == "SPÁČIL - ELBA"
replace surname = "Spudichová" if surname == "Spudichova"
replace surname = "Suralová" if surname == "Suralova"
replace surname = "Tesařík" if surname == "TESAŘÍK"
replace surname = "Troška" if surname == "Troska" & birthdate == td(26sep1968)
replace surname = "Tykadlová" if surname == "Tykalová" & birthdate == td(20aug1950)
replace surname = "Tůma" if surname == "Tůma, ing."
replace surname = "Tůmová" if surname == "Tůmová, dr."
replace surname = "Vajdík" if surname == "VAJDÍK"
replace surname = "Vašek" if surname == "VAŠEK"
replace surname = "Vojteková" if surname == "VOJTEKOVÁ"
replace surname = "Vycpálek" if surname == "VYCPÁLEK"
replace surname = "Winkler" if surname == "Winker"
replace surname = "Witozsz" if surname == "Witosz" 
replace surname = "Zejdová" if surname == "ZEJDOVÁ"
replace surname = "Zamorský" if surname == "Zamorsky"
replace firstname = "František Albert" if surname == "Zykán" & (firstname == "Albert" | firstname == "František-A")
replace surname = "Bohbot" if surname == "bohbot"
replace surname = "Brožek" if surname == "brožek"
replace surname = "Fraitová" if surname == "fraitová"
replace surname = "Jůna" if surname == "jůna"
replace surname = "Kalach" if surname == "kalach"
replace surname = "Klouček" if surname == "klouček"
replace surname = "Kocur" if surname == "kocur"
replace surname = "Kočvarová" if surname == "kočvarová"
replace surname = "Kučera" if surname == "kučera"
replace surname = "Šembera" if surname == "ŠEMBERA"
replace surname = "Škuliga" if surname == "ŠKULIGA"
replace surname = "Šrámek" if surname == "ŠRÁMEK"
replace surname = "Šarköziová" if surname == "Šarkoziova"
replace surname = "Švidran" if surname == "švidran"



**** birthdate change ****
replace birthdate = td(01jun1972) if birthdate == td(01jun1972) & surname == "Majringer"
replace birthdate = td(19sep1963) if birthdate == td(19sep1962) & surname == "Lejsková"
replace birthdate = td(10nov1955) if birthdate == td(11nov1955) & surname == "Landa"
replace birthdate = td(15jan1962) if birthdate == td(15feb1962) & surname == "Lameš"
replace birthdate = td(26feb1975) if birthdate == td(26feb2022) & surname == "Křížek"
replace birthdate = td(23aug1973) if birthdate == td(23aug1975) & surname == "Kyselý"
replace birthdate = td(09dec1977) if birthdate == td(07dec1979) & surname == "Kučera"
replace birthdate = td(05jul1948) if birthdate == td(05jul2022) & surname == "Kufrová"
replace birthdate = td(22may1951) if birthdate == td(07apr2020) & surname == "Kubíček"
replace birthdate = td(17feb1953) if birthdate == td(17feb2022) & surname == "Kubský"
replace birthdate = td(04aug1961) if birthdate == td(08apr1961) & surname == "Kubiena"
replace birthdate = td(24nov1951) if birthdate == td(24jan2022) & surname == "Krása"
replace birthdate = td(16nov1963) if birthdate == td(16sep2022) & surname == "Kronus"
replace birthdate = td(23oct1979) if birthdate == td(22oct1979) & surname == "Krause"
replace birthdate = td(01jan1972) if birthdate == td(01jan2022) & surname == "Kouřil"
replace birthdate = td(30jan1966) if birthdate == td(30jan2022) & surname == "Konšel"
replace birthdate = td(24jun1975) if birthdate == td(07apr2020) & surname == "Kolaříková"
replace birthdate = td(08jul1949) if birthdate == td(06jul1949) & surname == "Klejma"
replace birthdate = td(26oct1964) if birthdate == td(25oct1964) & surname == "John"
replace birthdate = td(05jun1952) if birthdate == td(06may1952) & surname == "Jirotka"
replace birthdate = td(14oct1970) if birthdate == td(14oct1971) & surname == "Jelínek"
replace birthdate = td(07oct1964) if birthdate == td(07may1964) & surname == "Janů"
replace birthdate = td(20dec1949) if birthdate == td(20jan2022) & surname == "Janderka"
replace birthdate = td(08feb1963) if birthdate == td(08mar2023) & surname == "Hájek"
replace birthdate = td(27jan1948) if birthdate == td(17apr2022) & surname == "Jalovec"
replace birthdate = td(12may1967) if birthdate == td(12jun1967) & surname == "Jech"
replace birthdate = td(28feb1978) if birthdate == td(23feb1978) & surname == "Babáčková" 
replace birthdate = td(01may1948) if birthdate == td(01may1948) & surname == "Balcarová"
replace birthdate = td(10dec1975) if birthdate == td(10dec1965) & surname == "Bačák"
replace birthdate = td(11dec1966) if birthdate == td(11dec1965) & surname == "Denk"
replace birthdate = td(13jun1975) if birthdate == td(19jun1975) & surname == "Dolanský"
replace birthdate = td(20mar1961) if birthdate == td(23mar1961) & surname == "Drbohlav"
replace birthdate = td(12jun1969) if birthdate == td(12jul1969) & surname == "Dvořák"
replace birthdate = td(28aug1956) if birthdate == td(26aug1956) & surname == "Bradáč"
replace birthdate = td(14nov1972) if birthdate == td(14nov1971) & surname == "Kladivová"
replace birthdate = td(05apr1967) if birthdate == td(05sep1967) & surname == "Felner"
replace birthdate = td(16mar1937) if birthdate == td(16jul1937) & surname == "Filák"
replace birthdate = td(02dec1961) if birthdate == td(02dec1962) & surname == "Flesar"
replace birthdate = td(10aug1968) if birthdate == td(10aug1967) & surname == "Fojt"
replace birthdate = td(17may1956) if birthdate == td(17may1955) & surname == "Forišková"
replace birthdate = td(26jul1969) if birthdate == td(26jul1967) & surname == "František"
replace birthdate = td(17dec1967) if birthdate == td(17nov1967) & surname == "Gogelová"
replace birthdate = td(11jun1984) if birthdate == td(01apr1984) & surname == "Grolich"
replace birthdate = td(29jul1988) if birthdate == td(29aug1988) & surname == "Hainc"
replace birthdate = td(01aug1960) if birthdate == td(04aug1960) & surname == "Kodrík"
replace birthdate = td(17may1956) if birthdate == td(13may1956) & surname == "Kollová"
replace birthdate = td(02dec1966) if birthdate == td(02dec1976) & surname == "Kozubík"
replace birthdate = td(12apr1966) if birthdate == td(14apr1966) & surname == "Krajíček"
replace birthdate = td(12jun1984) if birthdate == td(12jun1974) & surname == "Král"
replace birthdate = td(21mar1957) if birthdate == td(23mar1957) & surname == "Kuběna"
replace birthdate = td(05jun1972) if birthdate == td(05jun1974) & surname == "Kvitová"
replace birthdate = td(02jul1969) if birthdate == td(01jul1969) & surname == "Mikošková"
replace birthdate = td(21sep1982) if birthdate == td(21oct1982) & surname == "Netolický"
replace birthdate = td(21nov1978) if birthdate == td(21nov1976) & surname == "Novotný"
replace birthdate = td(02feb1959) if birthdate == td(01feb1956) & surname == "Pagáčova"
replace birthdate = td(02nov1951) if birthdate == td(02nov1950) & surname == "Poláčková"
replace birthdate = td(01mar1982) if birthdate == td(01mar1981) & surname == "Herot"
replace birthdate = td(14may1951) if birthdate == td(04may1951) & surname == "Hlisnikovský"
replace birthdate = td(18jun1984) if birthdate == td(18jun1983) & surname == "Horut"
replace birthdate = td(24jul1984) if birthdate == td(24jun1984) & surname == "Horák"
replace birthdate = td(17mar1977) if birthdate == td(04mar1977) & surname == "Hromek"
replace birthdate = td(09aug1968) if birthdate == td(09aug1966) & surname == "Hrubý"
replace birthdate = td(14may1960) if birthdate == td(15may1960) & surname == "Jastrzembská"
replace birthdate = td(27dec1989) if birthdate == td(27dec1985) & surname == "Jůnová"
replace birthdate = td(07oct1970) if birthdate == td(07jan1970) & surname == "Stejskal"
replace birthdate = td(17aug1952) if birthdate == td(27aug1952) & surname == "Trávníček"
replace birthdate = td(29dec1972) if birthdate == td(29dec1973) & surname == "Tučková"
replace birthdate = td(05feb1958) if birthdate == td(10feb1958) & surname == "Tvarůžek"
replace birthdate = td(21aug1972) if birthdate == td(12aug1972) & surname == "Tylš"
replace birthdate = td(05nov1949) if birthdate == td(05dec1949) & surname == "Urbánek"
replace birthdate = td(21aug1998) if birthdate == td(21aug1988) & surname == "Vodáková"
replace birthdate = td(23jun1971) if birthdate == td(23may1971) & surname == "Váša"
replace birthdate = td(21mar1963) if birthdate == td(21mar1964) & surname == "Změlíková" 
replace birthdate = td(28nov1986) if birthdate == td(28nov1968) & surname == "Černín"
replace birthdate = td(21feb1958) if birthdate == td(21jan1958) & surname == "Šlahúnková"
replace birthdate = td(05jul1961) if birthdate == td(05jul1964) & surname == "Chval"
replace birthdate = td(10may1964) if birthdate == td(19may1964) & surname == "Říha"
replace birthdate = td(10aug1980) if birthdate == td(01aug1980) & surname == "Šmídek"
replace birthdate = td(17nov1961) if birthdate == td(19nov1961) & surname == "Štefek"
replace birthdate = td(26mar1957) if birthdate == td(26may1957) & surname == "Štěpánek"
replace birthdate = td(29jun1952) if birthdate == td(26jun1952) & surname == "Švecová"
replace birthdate = td(01jun1950) if birthdate == td(06jan1950) & surname == "Halíková"
replace birthdate = td(30sep1978) if birthdate == td(30jun1978) & surname == "Janda"
replace birthdate = td(11feb1954) if birthdate == td(11mar1954) & surname == "Jirousová"
replace birthdate = td(09jan1981) if birthdate == td(01sep1981) & surname == "Kotala"
replace birthdate = td(02jul1955) if birthdate == td(07feb1955) & surname == "Kováčik"
replace birthdate = td(11mar1967) if birthdate == td(03nov1967) & surname == "Mackovík"
replace birthdate = td(19nov1953) if birthdate == td(09nov1953) & surname == "Mandát"
replace birthdate = td(18sep1947) if birthdate == td(18oct1947) & surname == "Muchová"
replace birthdate = td(08feb1947) if birthdate == td(02aug1947) & surname == "Pospíšil"
replace birthdate = td(01nov1964) if birthdate == td(01jan1964) & surname == "Sekeráková"
replace birthdate = td(06dec1964) if birthdate == td(16dec1964) & surname == "Vrzal"
replace birthdate = td(19aug1960) if birthdate == td(10aug1960) & surname == "Hlaváček"
replace birthdate = td(10apr1981) if birthdate == td(19apr1981) & surname == "Kinčl"
replace birthdate = td(26sep1989) if birthdate == td(29sep1989) & surname == "Pleskač"
replace birthdate = td(11jul1965) if birthdate == td(11jul1975) & surname == "Ruso"
replace birthdate = td(01sep1966) if birthdate == td(07sep1966) & surname == "Sovová"
replace birthdate = td(22oct1977) if birthdate == td(22oct1997) & surname == "Adamec" 
replace birthdate = td(08jul1979) if birthdate == td(07aug1979) & surname == "Binka"
replace birthdate = td(08jul1977) if birthdate == td(05jan1977) & surname == "Brůžek"
replace birthdate = td(16may1977) if birthdate == td(16may1997) & surname == "Bíbr"
replace birthdate = td(22apr1978) if birthdate == td(22apr2020) & surname == "Bohuňková"
replace birthdate = td(04jul1983) if birthdate == td(04jul1989) & surname == "Chaloupka"
replace birthdate = td(29jan1989) if birthdate == td(28jan1989) & surname == "Florián"
replace birthdate = td(03jul1976) if birthdate == td(07mar1976) & surname == "Galata"
replace birthdate = td(28jul1959) if birthdate == td(28jul1958) & surname == "Holubová"
replace birthdate = td(26jun1990) if birthdate == td(25jun1990) & surname == "Housa"
replace birthdate = td(30sep1983) if birthdate == td(01jan1900) & surname == "Hájek"
replace birthdate = td(22jan1954) if birthdate == td(01mar2023) & surname == "Janata"
replace birthdate = td(13apr1989) if birthdate == td(13apr1989) & surname == "Janek"
replace birthdate = td(05sep1988) if birthdate == td(08oct1988) & surname == "Janovský"
replace birthdate = td(09apr1987) if birthdate == td(04sep1987) & surname == "Jelínek"
replace birthdate = td(15dec1989) if birthdate == td(15dec1995) & surname == "Jokeš"
replace birthdate = td(08may1991) if birthdate == td(08may1988) & surname == "Karasaridis"
replace birthdate = td(24nov1994) if birthdate == td(24dec1994) & surname == "Kindl"
replace birthdate = td(14feb1972) if birthdate == td(14feb1989) & surname == "Kluka"
replace birthdate = td(26aug1968) if birthdate == td(26may1968) & surname == "Koumarová"
replace birthdate = td(31mar1960) if birthdate == td(24mar1960) & surname == "Kožich"
replace birthdate = td(04aug1989) if birthdate == td(04aug1988) & surname == "Lochmanová"
replace birthdate = td(06mar1964) if birthdate == td(12mar1964) & surname == "Mendl"
replace birthdate = td(07jun1955) if birthdate == td(06jul1955) & surname == "Mrázek"
replace birthdate = td(19jun1972) if birthdate == td(20jun1962) & surname == "Naiclerová" 
replace birthdate = td(01jan1984) if birthdate == td(12jul1984) & surname == "Nešpor"
replace birthdate = td(09jun1994) if birthdate == td(06sep1994) & surname == "Noha"
replace birthdate = td(28may1986) if birthdate == td(23may1986) & surname == "Novotná"
replace birthdate = td(10may1972) if birthdate == td(05oct1972) & surname == "Novák" 
replace birthdate = td(16jun1971) if birthdate == td(15jun1971) & surname == "Otto"
replace birthdate = td(18may1984) if birthdate == td(17apr1984) & surname == "Pařízek"
replace birthdate = td(29jun1983) if birthdate == td(26jun1983) & surname == "Paška"
replace birthdate = td(28feb1952) if birthdate == td(29feb1952) & surname == "Petr"
replace birthdate = td(03feb1987) if birthdate == td(03mar1987) & surname == "Pešán"
replace birthdate = td(28oct1991) if birthdate == td(28oct1995) & surname == "Podroužek"
replace birthdate = td(17aug1969) if birthdate == td(17aug1984) & surname == "Pravda"
replace birthdate = td(09mar1970) if birthdate == td(03sep1970) & surname == "Rendl"
replace birthdate = td(21aug1982) if birthdate == td(21aug1989) & surname == "Rozsypal"
replace birthdate = td(08may1977) if birthdate == td(08may1966) & surname == "Rusóová"
replace birthdate = td(28may1983) if birthdate == td(28may2020) & surname == "Sadílek"
replace birthdate = td(08apr1989) if birthdate == td(08sep1989) & surname == "Simkanič"
replace birthdate = td(05sep1984) if birthdate == td(05sep1983) & surname == "Suchanek"
replace birthdate = td(25oct1991) if birthdate == td(15oct1991) & surname == "Svrček"
replace birthdate = td(29sep1978) if birthdate == td(29sep2022) & surname == "Sáňková"
replace birthdate = td(18mar1983) if birthdate == td(18mar1971) & surname == "Síkora"
replace birthdate = td(05jun1991) if birthdate == td(06may1991) & surname == "Sýkora"
replace birthdate = td(09mar1972) if birthdate == td(03sep1972) & surname == "Tomis"
replace birthdate = td(05aug1984) if birthdate == td(08may1984) & surname == "Toušek"
replace birthdate = td(09jul1978) if birthdate == td(07sep1978) & surname == "Třešňáková"
replace birthdate = td(28nov1985) if birthdate == td(28nov1995) & surname == "Urbánek"
replace birthdate = td(19apr1998) if birthdate == td(19apr1994) & surname == "Vidláková" 
replace birthdate = td(24may1987) if birthdate == td(25may1987) & surname == "Vrba" 
replace birthdate = td(26jan1983) if birthdate == td(26jul1983) & surname == "Zach"
replace birthdate = td(03jun1962) if birthdate == td(30jun1962) & surname == "Zemánek"
replace birthdate = td(19sep1982) if birthdate == td(19sep1991) & surname == "Zámečník"
replace birthdate = td(28feb1976) if birthdate == td(29feb1976) & surname == "Čermáková"
replace birthdate = td(14feb1989) if birthdate == td(14feb1988) & surname == "Čáha"
replace birthdate = td(14jan1962) if birthdate == td(04jan1962) & surname == "Šemík"
replace birthdate = td(03nov1989) if birthdate == td(03nov1988) & surname == "Žák"
replace birthdate = td(25jun1976) if birthdate == td(25jun1973) & surname == "Bakošová"
replace birthdate = td(25dec1962) if birthdate == td(25jan1962) & surname == "Bojko"
replace birthdate = td(19mar1995) if birthdate == td(29mar1995) & surname == "Impseil"
replace birthdate = td(21jan1972) if birthdate == td(21jan1987) & surname == "Kachlík"
replace birthdate = td(29aug1948) if birthdate == td(28aug1948) & surname == "Moudrý"
replace birthdate = td(03sep1983) if birthdate == td(04sep1983) & surname == "Plevková"
replace birthdate = td(01mar1956) if birthdate == td(02mar1956) & surname == "Severová"
replace birthdate = td(12jan1957) if birthdate == td(01dec1957) & surname == "Wawrzacz"
replace birthdate = td(11may1960) if birthdate == td(01may1960) & surname == "Čech"
replace birthdate = td(25oct1961) if birthdate == td(15oct1961) & surname == "Čechová"
replace birthdate = td(07oct1963) if birthdate == td(10jul1963) & surname == "Štefcová"
replace birthdate = td(07apr1967) if birthdate == td(07apr1967) & surname == "Švancarová'"
replace birthdate = td(23feb1979) if birthdate == td(03feb1979) & surname == "Bouška"
replace birthdate = td(02oct1945) if birthdate == td(01oct1945) & surname == "Cetkovský"
replace birthdate = td(16sep1966) if birthdate == td(18sep1966) & surname == "Davídek"
replace birthdate = td(02may1976) if birthdate == td(20may1976) & surname == "Dohnal"
replace birthdate = td(06feb1985) if birthdate == td(05feb1985) & surname == "Drbohlav"
replace birthdate = td(12mar1988) if birthdate == td(13mar1988) & surname == "Flek"
replace birthdate = td(06jul1969) if birthdate == td(07jun1969) & surname == "Hejma"
replace birthdate = td(19mar1954) if birthdate == td(29mar1954) & surname == "Horník"
replace birthdate = td(14sep1968) if birthdate == td(14sep2020) & surname == "Horák"
replace birthdate = td(02feb1969) if birthdate == td(02feb1961) & surname == "Hrabě"
replace birthdate = td(01oct1980) if birthdate == td(11jan1980) & surname == "Jirovský"
replace birthdate = td(12may1977) if birthdate == td(12may2020) & surname == "Karásková"
replace birthdate = td(25jan1971) if birthdate == td(25oct1971) & surname == "Kuklová"
replace birthdate = td(17nov1962) if birthdate == td(07nov1962) & surname == "Marešová"
replace birthdate = td(28jan1976) if birthdate == td(27jan1976) & surname == "Bílová"
replace birthdate = td(28apr1966) if birthdate == td(29apr1966) & surname == "Müller"
replace birthdate = td(02sep1972) if birthdate == td(02aug1972) & surname == "Quittová"
replace birthdate = td(15nov1974) if birthdate == td(15nov1977) & surname == "Vávra"
replace birthdate = td(22jan1979) if birthdate == td(20jan1979) & surname == "Čejka"
replace birthdate = td(20oct1986) if birthdate == td(29oct1986) & surname == "Šperl"
replace birthdate = td(21sep1976) if birthdate == td(19sep1976) & surname == "Švancara"
replace birthdate = td(24aug1983) if birthdate == td(23aug1983) & surname == "Uzel"
replace birthdate = td(30apr1965) if birthdate == td(30apr1954) & surname == "Chvojka"
replace birthdate = td(22jul1973) if birthdate == td(21jul1977) & surname == "Janíček"
replace birthdate = td(30may1955) if birthdate == td(30apr1955) & surname == "Nedvědický"
replace birthdate = td(09mar1971) if birthdate == td(17mar1971) & surname == "Med"
replace birthdate = td(28jul1971) if birthdate == td(20jul1971) & surname == "Pavelek"
replace birthdate = td(05apr1954) if birthdate == td(03apr1954) & surname == "Procházková"
replace birthdate = td(03apr1970) if birthdate == td(04mar1970) & surname == "Tregner"
replace birthdate = td(22sep1982) if birthdate == td(22sep2020) & surname == "Netolický"
replace birthdate = td(28aug1988) if birthdate == td(25aug1988) & surname == "Polanský"
replace birthdate = td(17jan1984) if birthdate == td(17jan2021) & surname == "Eliáš"
replace birthdate = td(04jun1947) if birthdate == td(04apr1947) & surname == "Košek"
replace birthdate = td(29sep1970) if birthdate == td(29sep1950) & surname == "Snopek"
replace birthdate = td(12jun1962) if birthdate == td(12jun1975) & surname == "Stránský"
replace birthdate = td(22jul1971) if birthdate == td(22jul1977) & surname == "Šlachta"
replace birthdate = td(28jul1970) if birthdate == td(28oct1970) & surname == "Bravenec"
replace birthdate = td(26oct2001) if birthdate == td(16oct2001) & surname == "Knapovský"
replace birthdate = td(12aug1981) if birthdate == td(18aug1981) & surname == "Kufa"
replace birthdate = td(11jan1948) if birthdate == td(11oct1948) & surname == "Otevřel"
replace birthdate = td(23aug1980) if birthdate == td(23jul1980) & surname == "Pelán" 
replace birthdate = td(26mar1972) if birthdate == td(26sep2023) & surname == "Techlovský"
replace birthdate = td(01jul1970) if birthdate == td(27sep1970) & surname == "Urban"
replace birthdate = td(11mar1992) if birthdate == td(11mar1993) & surname == "Vorba"
replace birthdate = td(11dec1970) if birthdate == td(14dec1971) & surname == "Špitálský"
replace birthdate = td(20jan1957) if birthdate == td(21jan1957) & surname == "Andreska"
replace birthdate = td(27jan1965) if birthdate == td(27jan2022) & surname == "Bartošík"
replace birthdate = td(17feb1967) if birthdate == td(17feb2022) & surname == "Bednářová"
replace birthdate = td(13jun1969) if birthdate == td(12jul2022) & surname == "Bedrnová"
replace birthdate = td(01jan1965) if birthdate == td(17mar2022) & surname == "Bečán"
replace birthdate = td(04may1973) if birthdate == td(03jan2022) & surname == "Bečvář"
replace birthdate = td(12dec1955) if birthdate == td(12dec2000) & surname == "Blažková-Šípková"
replace birthdate = td(05jul1944) if birthdate == td(05jul2000) & surname == "Bárta"
replace birthdate = td(16dec1951) if birthdate == td(11dec2023) & surname == "Cholasta"
replace birthdate = td(27jan1965) if birthdate == td(11dec1965) & surname == "Císař" 
replace birthdate = td(17oct1959) if birthdate == td(11dec2023) & surname == "Dobrovolná"
replace birthdate = td(17nov2000) if birthdate == td(19nov2022) & surname == "Doležal"
replace birthdate = td(07jun1966) if birthdate == td(07jul1966) & surname == "Drbohlav"
replace birthdate = td(06aug1928) if birthdate == td(15apr2022) & surname == "Dupalová"
replace birthdate = td(07may1985) if birthdate == td(05jul1985) & surname == "Endrle"
replace birthdate = td(07may1971) if birthdate == td(07may2022) & surname == "Fendrych"
replace birthdate = td(03dec1944) if birthdate == td(03dec1994) & surname == "Fraitová"
replace birthdate = td(14jul1949) if birthdate == td(07jul2022) & surname == "Fuchs"
replace birthdate = td(05aug1957) if birthdate == td(29may2022) & surname == "Funioková"
replace birthdate = td(16jun1958) if birthdate == td(16jan2022) & surname == "Golian"
replace birthdate = td(30apr1949) if birthdate == td(30apr1948) & surname == "Hanuš"
replace birthdate = td(26sep1968) if birthdate == td(26sep2022) & surname == "Holub"
replace birthdate = td(10apr1950) if birthdate == td(10apr2022) & surname == "Novosád"
replace birthdate = td(28dec1953) if birthdate == td(31jul2023) & surname == "Neuman"
replace birthdate = td(18jan1969) if birthdate == td(18may1969) & surname == "Nejedlý"
replace birthdate = td(01sep1976) if birthdate == td(13apr1978) & surname == "Míkovcová Mališová"
replace birthdate = td(25sep1958) if birthdate == td(20nov2022) & surname == "Musílek"
replace birthdate = td(18mar1957) if birthdate == td(18mar2022) & surname == "Mišáková"
replace birthdate = td(22jan1960) if birthdate == td(20jan1960) & surname == "Mareš"
replace birthdate = td(13sep1948) if birthdate == td(13sep2022) & surname == "Mareš"
replace birthdate = td(22jul1974) if birthdate == td(07apr2020) & surname == "Makl"
replace birthdate = td(20mar1956) if birthdate == td(17nov2000) & surname == "Malý"
replace birthdate = td(11may1944) if birthdate == td(31jul2023) & surname == "Švarc"
replace birthdate = td(31jan1974) if birthdate == td(31jan2022) & surname == "Šustalová"
replace birthdate = td(02jan1958) if birthdate == td(02jan2022) & surname == "Štyrand"
replace birthdate = td(19may1978) if birthdate == td(31jul2023) & surname == "Špulák"
replace birthdate = td(11aug1979) if birthdate == td(11mar1979) & surname == "Špacír" 
replace birthdate = td(05feb1974) if birthdate == td(05feb1977) & surname == "Šimek"
replace birthdate = td(21dec1965) if birthdate == td(17jan2022) & surname == "Čada"
replace birthdate = td(02jul1958) if birthdate == td(02jul2022) & surname == "Zach"
replace birthdate = td(03may1960) if birthdate == td(03may2021) & surname == "Vraná"
replace birthdate = td(12jul1956) if birthdate == td(12jan2022) & surname == "Vorel"
replace birthdate = td(29oct1966) if birthdate == td(26oct1966) & surname == "Vepřek"
replace birthdate = td(29jun1978) if birthdate == td(28jun1978) & surname == "Vašíček"
replace birthdate = td(06aug1963) if birthdate == td(06may2022) & surname == "Vandasová"
replace birthdate = td(31mar1960) if birthdate == td(31mar1959) & surname == "Vajchr"
replace birthdate = td(15dec1973) if birthdate == td(15feb1973) & surname == "Uhlíř"
replace birthdate = td(28nov1968) if birthdate == td(11dec2023) & surname == "Trnková"
replace birthdate = td(07dec1955) if birthdate == td(01dec1959) & surname == "Soukup"
replace birthdate = td(29sep1976) if birthdate == td(11dec2023) & surname == "Smékal"
replace birthdate = td(17oct1976) if birthdate == td(17mar2022) & surname == "Sláma"
replace birthdate = td(07dec1967) if birthdate == td(07jan1967) & surname == "Sekera"
replace birthdate = td(11feb1972) if birthdate == td(11feb1973) & surname == "Sedlmajer"
replace birthdate = td(25jun1959) if birthdate == td(25jun1969) & surname == "Reiser"
replace birthdate = td(29jan1955) if birthdate == td(11dec2023) & surname == "Pšenička"
replace birthdate = td(08jun1947) if birthdate == td(08jun2023) & surname == "Pilich"
replace birthdate = td(27feb1975) if (birthdate == td(27may1975) | birthdate == td(27feb2022)) & surname == "Žáčková" 
replace birthdate = td(26mar1947) if (birthdate == td(26jan1957) | birthdate == td(26mar1957)) & surname == "Mužík" 
replace birthdate = td(14feb1969) if (birthdate == td(14feb2022) | birthdate == td(14may2022)) & surname == "Neliba"
replace birthdate = td(19dec1955) if (birthdate == td(19dec2000) | birthdate == td(19dec2021)) & surname == "Šoral"
replace birthdate = td(07jan1993) if (birthdate == td(07jan2022) | birthdate == td(21may2023)) & surname == "Zdobinský" 
replace birthdate = td(17nov1957) if (birthdate == td(09jun2023) | birthdate == td(17nov2023)) & surname == "Suralová" 
replace birthdate = td(19feb1975) if (birthdate == td(19feb2022) | birthdate == td(19feb2002)) & surname == "Spurný" 
replace birthdate = td(20apr1961) if (birthdate == td(31jul2023) | birthdate == td(20apr2022)) & surname == "Riedel" 
replace birthdate = td(15mar1975) if (birthdate == td(15mar2022) | birthdate == td(24nov2022)) & surname == "Frelová" 
replace birthdate = td(21oct1948) if (birthdate == td(06oct1948) | birthdate == td(02oct1948)) & surname == "Všetečka"
replace birthdate = td(04aug1986) if (birthdate == td(04aug1983) | birthdate == td(04jul1986)) & surname == "Havran"
replace birthdate = td(29jul1973) if (birthdate == td(29jul1989) | birthdate == td(29jul1993)) & surname == "Pešková"






 
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

collapse (sum) `d'_donation (first) acadegree_an acadegree_bn , by(polparty surname firstname birthdate year)

sort polparty surname firstname birthdate year
save "intermediate_data\donation_data_`d'_clean.dta", replace 

}

use intermediate_data\donation_data_financial_clean.dta, clear

tempfile fileA
keep if financial_donation > 0
save `fileA'

use intermediate_data\donation_data_financial_clean.dta, clear
keep if financial_donation == 0

sort polparty surname firstname birthdate year 
merge polparty surname firstname birthdate year using intermediate_data\donation_data_nonfinancial_clean.dta

keep if _merge == 3

append using `fileA'

replace financial_donation = 0 if financial_donation == .
replace nonfinancial_donation = 0 if nonfinancial_donation == .
 
 
egen id = group(polparty surname firstname birthdate)

generate length_degree_bn = strlen(acadegree_bn)
replace length_degree_bn = 0 if length_degree_bn == 1
generate length_degree_an = strlen(acadegree_an)
replace length_degree_an = 0 if length_degree_an == 1

gen length = length_degree_an + length_degree_bn

gsort id -length

replace acadegree_bn = acadegree_bn[_n-1] if id[_n] == id[_n-1]
replace acadegree_an = acadegree_an[_n-1] if id[_n] == id[_n-1]

drop lengt* id
 
sort polparty surname firstname birthdate year

gen donation_all = financial_donation + nonfinancial_donation


collapse (first) acadegree_an acadegree_bn (sum) donation_all financial_donation nonfinancial_donation, by(polparty surname firstname birthdate year)

gen birthyear = year(birthdate)

save data\data_donation_oofppm.dta, replace
 