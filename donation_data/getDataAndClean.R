rm(list=ls())
#install.packages("readxl")
#install.packages("writexl")
#install.packages("dplyr")
#install.packages("stringr")
#install.packages("haven")
#install.packages("lubridate")


# Load necessary libraries
library(readxl)
library(writexl)
library(dplyr)
library(stringr)
library(haven)
library(lubridate)

# Set working directory
setwd("/Users/vtitl/Documents/GitHub/ced/donation_data/")

# Define variables
polparty <- c("ano", "kscm", "kducsl", "ods", "pirati", "spd", "stan", "top09", "cssd", "svobodni", "trikolora", "prisaha")
years <- c("2023", "2022", "2021", "2020", "2019", "2018", "2017")
donationform <- c("financial", "nonfinancial")

# Create directories
dir.create("primary_data", showWarnings = FALSE)
dir.create("primary_data/oofppm", showWarnings = FALSE)
dir.create("data", showWarnings = FALSE)
dir.create("intermediate_data", showWarnings = FALSE)
dir.create("cleaned_data", showWarnings = FALSE)

# Loop through political parties and years to download and process files
for (p in polparty) {
  dir.create(paste0("primary_data/oofppm/pp_", p), showWarnings = FALSE)
  
  for (y in years) {
    
    if (!(p == "ano" && y == "2020") && !(p == "ano" && y == "2021") && !(p == "kducsl" && y == "2021")  && !(p == "spd" && y == "2017")  && !(p == "spd" && y == "2018")  && !(p == "spd" && y == "2019")   && !(p == "spd" && y == "2020")  && !(p == "spd" && y == "2021")  && !(p == "spd" && y == "2022") && !(p == "spd" && y == "2023")&& !(p == "trikolora" && y == "2017")&& !(p == "trikolora" && y == "2018") && !(p == "trikolora" && y == "2021") && !(p == "prisaha" && y == "2017") && !(p == "prisaha" && y == "2018") && !(p == "prisaha" && y == "2019") && !(p == "prisaha" && y == "2020")){
      dir.create(paste0("primary_data/oofppm/pp_", p, "/y_", y), showWarnings = FALSE)
      
      # Financial donations
      url <- paste0("https://zpravy.udhpsh.cz/export/vfz", y, "-", p, "-penizefo.xls")
      file_path <- paste0("primary_data/oofppm/pp_", p, "/y_", y, "/vfz", y, "-", p, "-penizefo.xls")
      download.file(url, file_path, mode = "wb")
      Sys.sleep(1) # Sleep for 1 second to avoid overloading the server
      
      # Read and clean the data
      df <- read_excel(file_path)
      
      # Rename rows
      df = df %>% 
        rename(
          firstname = "jméno",
          surname = "příjmení",
          municipality = "obec",
          částkakc = "částka Kč",
          titulpřed = "titul před",
          titulza = "titul za",
          birthdate = "datum narození"
        )
      
      df <- df %>%
        mutate(titulza = ifelse(titulza == "", ".", titulza),
               titulpřed = ifelse(titulpřed == "", ".", titulpřed))
      
      write_xlsx(df, file_path)
      
      saveRDS(df, paste0("primary_data/oofppm/pp_", p, "/y_", y, "/vfz", y, "-", p, "-penizefo.rds"))
      
      # Non-financial donations
      url <- paste0("https://zpravy.udhpsh.cz/export/vfz", y, "-", p, "-bupfo.xls")
      file_path <- paste0("primary_data/oofppm/pp_", p, "/y_", y, "/vfz", y, "-", p, "-bupfo.xls")
      download.file(url, file_path, mode = "wb")
      Sys.sleep(1)
      
      df <- read_excel(file_path)
      
      # Rename rows
      df = df %>% 
        rename(
          firstname = "jméno",
          surname = "příjmení",
          municipality = "obec",
          hodnotabúpkč = "hodnota BÚP Kč",
          popisbúp = "popis BÚP",
          titulpřed = "titul před",
          titulza = "titul za",
          birthdate = "datum narození"
        )
      
      df <- df %>%
        mutate(titulza = ifelse(titulza == "", ".", titulza),
               titulpřed = ifelse(titulpřed == "", ".", titulpřed))
      
      write_xlsx(df, file_path)
      
      saveRDS(df, paste0("primary_data/oofppm/pp_", p, "/y_", y, "/vfz", y, "-", p, "-bupfo.rds"))
    }
  }
}

# Process the financial donations data
financialData = "intermediate_data/donation_data_financial.rds"
if (file.exists(financialData)) {
  file.remove(financialData)
}
nonFinancialData = "intermediate_data/donation_data_nonfinancial.rds"
if (file.exists(nonFinancialData)) {
  file.remove(nonFinancialData)
}

df_old = data.frame()
df_old_nonfinancial = data.frame()

for (p in polparty) {
  for (y in years) {
    file_path <- paste0("primary_data/oofppm/pp_", p, "/y_", y, "/vfz", y, "-", p, "-penizefo.rds")
    
    
    if (file.exists(file_path)) {
      df <- readRDS(file_path) %>%
        mutate(year = y,
               polparty = case_when(
                 p == "kducsl" ~ 1,
                 p == "kscm" ~ 2,
                 p == "ods" ~ 3,
                 p == "pirati" ~ 4,
                 p == "spd" ~ 5,
                 p == "stan" ~ 6,
                 p == "top09" ~ 7,
                 p == "ano" ~ 8,
                 p == "cssd" ~ 9,
                 p == "svobodni" ~ 10,
                 p == "trikolora" ~ 11,
                 p == "prisaha" ~ 12,
                 TRUE ~ NA_real_
               ))
      
      if (!(p == "ano" && y == "2020") && !(p == "ano" && y == "2021") && !(p == "kducsl" && y == "2021")  && !(p == "spd" && y == "2017")  && !(p == "spd" && y == "2018")  && !(p == "spd" && y == "2019")   && !(p == "spd" && y == "2020")  && !(p == "spd" && y == "2021")  && !(p == "spd" && y == "2022") && !(p == "spd" && y == "2023")&& !(p == "trikolora" && y == "2017")&& !(p == "trikolora" && y == "2018") && !(p == "trikolora" && y == "2021") && !(p == "prisaha" && y == "2017") && !(p == "prisaha" && y == "2018") && !(p == "prisaha" && y == "2019") && !(p == "prisaha" && y == "2020")){
        if (file.exists(financialData)) {
          df_old <- readRDS(financialData)
        }
        df_combined <- bind_rows(df_old, df)
        saveRDS(df_combined, financialData)
        Sys.sleep(2)
      }
    } else {
      message(paste("vfz", y, "-", p, "-penizefo.rds not found"))
    }
    
    file_path_nonfinancial <- paste0("primary_data/oofppm/pp_", p, "/y_", y, "/vfz", y, "-", p, "-bupfo.rds")

    if (file.exists(file_path_nonfinancial)) {
      df_nonfinancial <- readRDS(file_path_nonfinancial) %>%
        mutate(year = y,
               polparty = case_when(
                 p == "kducsl" ~ 1,
                 p == "kscm" ~ 2,
                 p == "ods" ~ 3,
                 p == "pirati" ~ 4,
                 p == "spd" ~ 5,
                 p == "stan" ~ 6,
                 p == "top09" ~ 7,
                 p == "ano" ~ 8,
                 p == "cssd" ~ 9,
                 p == "svobodni" ~ 10,
                 p == "trikolora" ~ 11,
                 p == "prisaha" ~ 12,
                 TRUE ~ NA_real_
               ))
      
      if (!(p == "ano" && y == "2020") && !(p == "ano" && y == "2021") && !(p == "kducsl" && y == "2021")  && !(p == "spd" && y == "2017")  && !(p == "spd" && y == "2018")  && !(p == "spd" && y == "2019")   && !(p == "spd" && y == "2020")  && !(p == "spd" && y == "2021")  && !(p == "spd" && y == "2022") && !(p == "spd" && y == "2023")&& !(p == "trikolora" && y == "2017")&& !(p == "trikolora" && y == "2018") && !(p == "trikolora" && y == "2021") && !(p == "prisaha" && y == "2017") && !(p == "prisaha" && y == "2018") && !(p == "prisaha" && y == "2019") && !(p == "prisaha" && y == "2020")){
        if (file.exists(nonFinancialData)) {
          df_old_nonfinancial <- readRDS(nonFinancialData)
        }
        if(dim(df_nonfinancial)[1]!=0){ 
        df_combined_nonfinancial <- bind_rows(df_old_nonfinancial, df_nonfinancial)
        saveRDS(df_combined_nonfinancial, nonFinancialData)
        }
        Sys.sleep(2)
      }
    } else {
      message(paste("vfz", y, "-", p, "-bupfo.rds not found"))
    }
    
  }
}


tituly_bn <- c("Bc", "Ing.arch.", "Ing.Arch.", "Ing", "ThMgr", "Mgr", "PharmDr", "PaedDr", "PhDr", "PHDr", "MUDr", "JUDr", "Doc", "prof")
tituly_an <- c("MBA", "MSc", "Ph.D.", "PH.D.")

# Loop over each donation form
for (d in donationform) {
  
  # Load data
  data <- readRDS(paste0("intermediate_data/donation_data_", d, ".rds"))
  
  # Define the polparty variable as a factor with labels
  #data <- data %>%
  #  mutate(polparty = factor(polparty, levels = 1:12, labels = polparty))
  #data$polparty
  #ata$polparty=factor(data$polparty, labels = polparty, levels = 1:12
  data <- data %>%
    mutate(polparty = case_when(
      polparty == 1 ~ "kducsl",
      polparty == 2 ~ "kscm",
      polparty == 3 ~ "ods",
      polparty == 4 ~ "pirati",
      polparty == 5 ~ "spd",
      polparty == 6 ~ "stan",
      polparty == 7 ~ "top09",
      polparty == 8 ~ "ano",
      polparty == 9 ~ "cssd",
      polparty == 10 ~ "svobodni",
      polparty == 11 ~ "trikolora",
      polparty == 12 ~ "prisaha",
      TRUE ~ NA_character_
    ))             
  
  # Sort data
  data <- data %>%
    arrange(polparty, surname, firstname, birthdate, year)
  
  # Create and clean acadegree_an and acadegree_bn
  data <- data %>%
    mutate(acadegree_an = if_else(is.na(titulza) | titulza == "", ".", titulza),
           acadegree_bn = if_else(!is.na(titulpřed) & titulpřed != " ", titulpřed, ".")) %>%
    mutate(acadegree_an = as.character(acadegree_an),
           acadegree_bn = as.character(acadegree_bn))
  
  # Rename donation variable
  if (d=="financial"){
    
    
    data = data %>% 
      rename(
        "financial_donation" = "částkakc"
      )
    
    # Keep only the necessary variables
    data <- data %>%
      select(surname, firstname, acadegree_an, acadegree_bn, birthdate, year, polparty, financial_donation)
    
  } else{
    
    data = data %>% 
      rename(
        "nonfinancial_donation" = "hodnotabúpkč"
      )
    
    # Keep only the necessary variables
    data <- data %>%
      select(surname, firstname, acadegree_an, acadegree_bn, birthdate, year, polparty, nonfinancial_donation)
    
  }
  
  # Filter out specific surnames
  data <- data %>%
    filter(!surname %in% c("anonym", "test", "Test", "stojkov"))
  
  # Replace academic degrees in firstname and surname
  for (t in tituly_bn) {
    data$acadegree_bn[grepl(t, data$firstname)] <- t
    data$acadegree_bn[grepl(t, data$surname)] <- t
  }
  
  for (t in tituly_an) {
    data$acadegree_an[grepl(t, data$firstname)] <- t
    data$acadegree_an[grepl(t, data$surname)] <- t
  }
  
  # Clean up firstname by removing academic titles
  data <- data %>%
    mutate(firstname = str_remove_all(firstname, " Bc.| MUDr.| JUDr.| PhDr.|Ing.arch.|Ing. arch.|Ing.Arch.|Ing. Arch.|, Ing.| Ing.|.Ing.| Mgr.|Ing.arch.|Ing.PhD.| PH.D.| PharmDr.| PhDr. Ph.D.| Ph.D.| Dr.| Mgr.et Mgr.| Mgr-| doc. CSc| dipl.um.| PHDr.| RNDr.| doc.DDr.PdD.| Doc. JUDr. Ph.| Ing| -předseda|,, Ph.D|, Ph.D.|,,Mgr.,Ph.D|,, MBA|, Dis.|, Bc MBA|, DiS|, MBA|MBA|,, IWE|, Dis.|, MVDr.|, Di|, RNDr.Ph.D.|, Prof.MUDr.|, ThMgr.| - krajský tajem|, et,|, prof. PhDr.|, RNDr.Ph.D.|, doc. JUDr.|, PhDr., BcA.|PhD| prof| RNDr| PaedDr| doc| Doc| BcA|,Ph.D.|  Mgr")) %>%
    mutate(firstname = str_squish(firstname))  # Remove extra spaces and dots
  
  # Define the conditions for the first replacement
  excluded_names <- c("Petra", "Břetislav", "Peter", "Iveta", "Jetelinová", "Kajetán", "Žaneta", "Bernadetta", "Yveta", "Aneta", "Jeanette", "Petros", "Yvette", "Elisabeth", "Betty", "Svetozar", "Jiří Metod", "Elizabet", "Petri", "Petruše", "Marketa", "Petr Felix", "Yweta", "Karel", "Kvetoslava")
  
  # Replace "et" in firstname if conditions are met
  data$firstname <- ifelse(!grepl("Petr", data$firstname) & !data$firstname %in% excluded_names, gsub("et", "", data$firstname), data$firstname)
  
  # Other replacements in firstname
  data$firstname <- gsub("MUDr ", "", data$firstname)
  data$firstname <- gsub("MUDr", "", data$firstname)
  data$firstname <- gsub("Bc ", "", data$firstname)
  data$firstname <- gsub("Mgr", "", data$firstname)
  data$firstname <- gsub("Ing", "", data$firstname)
  
  # Replacements in surname
  data$surname <- gsub(" MUDr.", "", data$surname)
  data$surname <- gsub(", Ph.D.", "", data$surname)
  
  # Replace spaces in firstname if surname matches certain values
  data$firstname <- ifelse(data$surname %in% c("Celý", "Carbolová", "Giacintov"), gsub(" ", "", data$firstname), data$firstname)
  
  # Other replacements in firstname
  data$firstname <- gsub("\\.", "", data$firstname)
  data$firstname <- gsub(",", "", data$firstname)
  data$firstname <- gsub(",,", "", data$firstname)
  
  # Handle common typos in firstnames
  typos <- list(
    "Aleš" = "Ales", "Anežka" = "Anezka", "Antonín" = "Antonin", "Alena" = "ALENA", "Boleslav" = "boleslav",
    "Božena" = "Bozena", "Dagmar" = "DAGMAR", "David" = c("DAVID", "david"), "Daniel" = "Daniël",
    "Eliška" = c("Eliska", "eliska"), "František" = c("františek", "Frantisek", "FRANTISEK", "FRANTIŠEK", "frantisek"),
    "Františka" = "Frantiska", "Ivo" = c("Mgr Ivo", "IVO"), "Ignác" = "ignác", "Ivan" = "IVAN", "Ivana" = "ivana",
    "Iva" = "iva", "Igor" = "IGOR", "Janka" = "janka", "Jana" = "jana", "Josef" = c("josef", "JOsef"),
    "Jiří" = c("jiří", "Jiri", "Jiři", "Jirí", "jiri", "JIří", "JIŘÍ", "Jaří", "jiří", "Jjiří"),
    "Jan" = c(" Jan", "jan", "Honza", "JAN"), "Jeroným" = "Jeronym", "Jaroslava" = c("Jarka", "jaroslava"),
    "Jakub" = c("jakub", "Kuba", "kuba", "Jaku"), "Jindřich" = c("Jindrich", "jindřich", "jindrich"),
    "Jaroslav" = c("jaroslav", "JAROSLAV"), "Kristina" = "kristina", "Klára" = c("Klara", "klara"),
    "Kateřina" = c("Katerina", "kateřina", "kateria"), "Karel" = "KAREL", "Lukáš" = c("Lukas", "Lukás", "Lukaš", "lukas", "lukáš"),
    "Leoš" = "Leos", "Lucie" = "lucie", "Libor" = "Bc Libor", "Lubuše" = "LIBUŠE", "Miroslav" = c("MIROSLAV", "miroslav"),
    "Marin" = "marin", "Martin" = c("martin", "Martín"), "Markéta" = c("Marketa", "markéta"),
    "Magdaléna" = c("Magdalena", "Magda"), "Matěj" = c("Matej", "MATEJ", "matej"), "Michal" = "michal",
    "Miluše" = "Miluse", "Marek" = "marek", "Milan" = "MILAN", "Naděžda" = "Naděža", "Ota" = " Ota",
    "Ondřej" = c("Ondrej", "Onřej", "Ondra"), "Olga" = c("olga", "Olg"), "Petr" = c("petr", "Pertr", "PETR", "petr"),
    "Petra" = "petra", "Přemysl" = "Premysl", "Pavlína" = "Pavlina", "Přemek" = "Premek", "Pavol" = "PAVOL",
    "Richard" = "RichaRD", "René" = "Rene", "Radka" = "RRadka", "Rudolf" = "rudolf", "Roman" = "ROMAN",
    "Simon" = "simon", "Šimon" = "šimon", "Štěpánka" = c("Štef.", "Stěpánka", "Stepánka", "Štepanka", "Štepánka", "stepanka"),
    "Štěpán" = c("Stepan", "Stěpán", "Stepán", "Štepan", "Štepán", "Štěpan"), "Šárka" = c("Sarka", "Šarka", "Sárka", "šarka", "šárka"),
    "Štefan" = "Stefan", "Tomáš" = c("IngTomáš", "Tomas", "Tomás", "TOMÁŠ", "TOMAS", "tomas", "tomáš"),
    "Tomášek" = "Tomas", "Terezie" = c("Tereza", "tereza"), "Vendula" = "vendula", "Veronika" = c("Veronika", "veronika"),
    "Václav" = c("Václave", "Vaclav", "Václav", "vaclav", "VÁCLAV"), "Vlastimil" = "VLASTIMIL", "Zdeněk" = c("Zdenek", "zdeněk"),
    "Zdenka" = "zdenka", "Žaneta" = c("Žaneta", "zaneta"), "Jaroslav" = c("Jaro.", "JAROSLAV"), "Zuzana" = c("zuzana", "Zuzana"),
    "Zora" = "zora", "Vladimír" = c("Vladimir", "vladimir", "VLADIMIR"), "Věra" = c("Vera", "Věra", "Věda"),
    "Veronika" = c("veronika", "Veronika", "VERONIKA", "veronika")
  )
  
  # Apply the common typos corrections
  for (correct_name in names(typos)) {
    typo_patterns <- typos[[correct_name]]
    #if (is.character(typo_patterns)) {
    #  typo_patterns <- list(typo_patterns)
    #}
    for (typo_pattern in typo_patterns) {
      data <- data %>% mutate(
        firstname = ifelse(firstname == typo_pattern, correct_name, firstname)
        )
    }
  }
  
  
  data <- data %>%
    mutate(
      firstname = ifelse(surname == "Huneš" & birthdate == ymd("1968-11-09"), "Robert", firstname),
      
      firstname = ifelse(firstname == "Pavel,MVDr., Ph.D, M", "Pavel", firstname),
      firstname = ifelse(firstname == "Vladimít", "Vladimír", firstname),
      surname = ifelse(surname == "Aulická -Jírovcová", "Aulická Jírovcová", surname),
      surname = ifelse(surname == "Hemzáková", "Hemzáčková", surname),
      firstname = ifelse(firstname == "Vítěslav" & surname == "Lapčík" & birthdate == ymd("1965-05-21"), "Vítězslav", firstname),
      surname = ifelse(surname == "Orszulík" & birthdate == ymd("1978-02-27"), "Orszulik", surname),
      surname = ifelse(surname == "ADAMÍK" & birthdate == ymd("1962-03-16"), "Adamík", surname),
      surname = ifelse(surname == "Bohutínský - soudní exekutor" & birthdate == ymd("1966-10-23"), "Bohutínský", surname),
      surname = ifelse(surname == "HŘEBEC", "Hřebec", surname),
      surname = ifelse(surname == "KALABUS", "Kalabus", surname),
      surname = ifelse(surname == "KOFROŇ", "Kofroň", surname),
      surname = ifelse(surname == "KOSUBOVÁ", "Kosubová", surname),
      surname = ifelse(surname == "POLÁŠEK", "Polášek", surname),
      surname = ifelse(surname == "Palyza st.", "Palyza", surname),
      surname = ifelse(surname == "Stehno - ES TRANS", "Stehno", surname),
      surname = ifelse(surname == "VAVŘINEC", "Vavřinec", surname),
      surname = ifelse(surname == "WANECKI-MANAP", "Wanecki", surname),
      surname = ifelse(surname == "Ťulpík - WTC", "Ťulpík", surname),
      firstname = ifelse(firstname == "Jan" & birthdate == ymd("1980-06-17"), "Ján", firstname),
      surname = ifelse(surname == "Baller" & birthdate == ymd("1997-05-15"), "Ballek", surname),
      firstname = ifelse(surname == "Ballek" & birthdate == ymd("1997-05-15"), "Jakub Vojtěch", firstname),
      surname = ifelse(surname == "Friedlova" & birthdate == ymd("1987-05-05"), "Friedlová", surname),
      firstname = ifelse(surname == "Friedlová" & birthdate == ymd("1987-05-05"), "Eliška", firstname),
      surname = ifelse(surname == "Homindova" & birthdate == ymd("1974-07-20"), "Homindová", surname),
      surname = ifelse(firstname == "Beran" & birthdate == ymd("1986-05-26"), "Beran", surname),
      surname = ifelse(firstname == "Nádvorník" & birthdate == ymd("1986-01-18"), "Nádvorník", surname),
      surname = ifelse(firstname == "Rýdl" & birthdate == ymd("1979-10-08"), "Rýdl", surname),
      surname = ifelse(firstname == "Střítecký" & birthdate == ymd("1986-07-03"), "Střítecký", surname),
      surname = ifelse(firstname == "Žůrek" & birthdate == ymd("1984-10-23"), "Žůrek", surname),
      firstname = ifelse(firstname == "Beran" & birthdate == ymd("1986-05-26"), "Jan", firstname),
      firstname = ifelse(firstname == "Nádvorník" & birthdate == ymd("1986-01-18"), "Jan", firstname),
      firstname = ifelse(firstname == "Rýdl" & birthdate == ymd("1979-10-08"), "Jan", firstname),
      firstname = ifelse(firstname == "Střítecký" & birthdate == ymd("1986-07-03"), "Jan", firstname),
      firstname = ifelse(firstname == "Žůrek" & birthdate == ymd("1984-10-23"), "Jan", firstname),
      firstname = ifelse(surname == "Kirš" & birthdate == ymd("1980-05-18"), "Tomáš", firstname),
      surname = ifelse(surname == "Kohoutova" & birthdate == ymd("1989-10-18"), "Kohoutová", surname),
      firstname = ifelse(firstname == "Magdalena Anna" & birthdate == ymd("1989-10-18"), "Magdalena", firstname),
      surname = ifelse(surname == "Komrskova" & birthdate == ymd("1978-02-08"), "Komrsková", surname),
      surname = ifelse(surname == "Krausova" & birthdate == ymd("1992-10-08"), "Krausová", surname),
      surname = ifelse(surname == "Krtičková,", "Krtičková", surname),
      surname = ifelse(firstname == "Šotková" & birthdate == ymd("1968-04-12"), "Šotková", surname),
      firstname = ifelse(firstname == "Šotková" & birthdate == ymd("1968-04-12"), "Lenka", firstname),
      surname = ifelse(firstname == "Vachova" & birthdate == ymd("1979-06-25"), "Vachová", surname),
      firstname = ifelse(firstname == "Vachova" & birthdate == ymd("1979-06-25"), "Libuše", firstname),
      surname = ifelse(surname == "Lorovsky", "Lorovský", surname),
      surname = ifelse(firstname == "Rek" & birthdate == ymd("1990-03-30"), "Rek", surname),
      firstname = ifelse(firstname == "Rek" & birthdate == ymd("1990-03-30"), "Lubomír", firstname),
      surname = ifelse(firstname == "Beránková" & birthdate == ymd("1992-06-01"), "Beránková", surname),
      firstname = ifelse(firstname == "Beránková" & birthdate == ymd("1992-06-01"), "Lucie", firstname),
      surname = ifelse(firstname == "Sokolová" & birthdate == ymd("2001-06-14"), "Sokolová", surname),
      firstname = ifelse(firstname == "Sokolová" & birthdate == ymd("2001-06-14"), "Lucie", firstname),
      firstname = ifelse(firstname == "Martina Jerie" & birthdate == ymd("1971-09-06"), "Martina", firstname),
      firstname = ifelse(firstname == "Zdenek" & birthdate == ymd("1977-08-22") & surname == "Machek", "Zdeněk", firstname),
      firstname = ifelse(firstname == "Giusepe" & birthdate == ymd("1962-06-10"), "Giuseppe", firstname),
      firstname = ifelse(firstname == "Renáta" & birthdate == ymd("1962-06-25"), "Renata", firstname),
      surname = ifelse(firstname == "Chamrád" & birthdate == ymd("2002-04-06"), "Chamrád", surname),
      firstname = ifelse(firstname == "Chamrád" & birthdate == ymd("2002-04-06"), "Michal", firstname),
      surname = ifelse(firstname == "Langer" & birthdate == ymd("1999-10-01"), "Langer", surname),
      firstname = ifelse(firstname == "Langer" & birthdate == ymd("1999-10-01"), "Michal", firstname),
      surname = ifelse(firstname == "Mazík" & birthdate == ymd("1984-09-04"), "Mazík", surname),
      firstname = ifelse(firstname == "Mazík" & birthdate == ymd("1984-09-04"), "Michal", firstname),
      surname = ifelse(firstname == "Trousil" & birthdate == ymd("1979-03-12"), "Trousil", surname),
      
      
      firstname = ifelse(firstname == "Trousil" & birthdate == ymd("1979-03-12"), "Michal", firstname),
      firstname = ifelse((firstname == "Ing Zdeněk" & birthdate == ymd("1966-07-27")) | (firstname == "IngZdeněk" & birthdate == ymd("1966-07-27")), "Zdeněk", firstname),
      surname = ifelse(surname == "Ondrackova" & birthdate == ymd("1993-11-22"), "Ondráčková", surname),
      surname = ifelse(firstname == "Bednář" & birthdate == ymd("1978-11-23"), "Bednář", surname),
      firstname = ifelse(firstname == "Bednář" & birthdate == ymd("1978-11-23"), "Ondřej", firstname),
      surname = ifelse(firstname == "Haváč" & birthdate == ymd("1985-09-03"), "Haváč", surname),
      firstname = ifelse(firstname == "Haváč" & birthdate == ymd("1985-09-03"), "Ondřej", firstname),
      surname = ifelse(firstname == "Kolínský" & birthdate == ymd("1990-03-11"), "Kolínský", surname),
      firstname = ifelse(firstname == "Kolínský" & birthdate == ymd("1990-03-11"), "Ondřej", firstname),
      surname = ifelse(firstname == "stalčík" & birthdate == ymd("1975-10-31"), "Stalčík", surname),
      firstname = ifelse(firstname == "stalčík" & birthdate == ymd("1975-10-31"), "Adam", firstname),
      surname = ifelse(surname == "balogova" & birthdate == ymd("1994-09-02"), "Balogová", surname),
      surname = ifelse(surname == "de Bruyn" & birthdate == ymd("1983-07-11"), "De Bruyn", surname),
      surname = ifelse(firstname == "Jindrich" & birthdate == ymd("1988-01-11"), "Devátý", surname),
      firstname = ifelse(firstname == "Jindrich" & birthdate == ymd("1988-01-11"), "Jindřich", firstname),
      surname = ifelse(surname == "fafi" & birthdate == ymd("1990-07-03"), "Fafi", surname),
      surname = ifelse(surname == "fischer" & birthdate == ymd("1965-10-14"), "Fischer", surname),
      surname = ifelse(surname == "fojtek" & birthdate == ymd("1969-01-07"), "Fojtek", surname),
      surname = ifelse(surname == "frauknecht", "Frauknecht", surname),
      surname = ifelse(surname == "gronych", "Gronych", surname),
      surname = ifelse(surname == "hanzal", "Hanzal", surname),
      surname = ifelse(surname == "holas", "Holas", surname),
      surname = ifelse(surname == "hovorková", "Hovorková", surname),
      
      surname = ifelse(surname == "jedlička", "Jedlička", surname),
      surname = ifelse(surname == "junga", "Junga", surname),
      surname = ifelse(surname == "kabelka", "Kabelka", surname),
      surname = ifelse(surname == "karas", "Karas", surname),
      surname = ifelse(surname == "koubkova", "Koubková", surname),
      surname = ifelse(surname == "karger", "Karger", surname),
      surname = ifelse(surname == "kindl", "Kindl", surname),
      surname = ifelse(surname == "michailidu", "Michailidu", surname),
      surname = ifelse(surname == "kolář", "Kolář", surname),
      surname = ifelse(surname == "kopecká", "Kopecká", surname),
      surname = ifelse(surname == "krištiak", "Krištiak", surname),
      surname = ifelse(surname == "kubovciak", "Kubovciak", surname),
      surname = ifelse(surname == "kure", "Kuře", surname),
      surname = ifelse(surname == "libich", "Libich", surname),
      surname = ifelse(surname == "liskova", "Lišková", surname),
      surname = ifelse(surname == "myslivec", "Myslivec", surname),
      surname = ifelse(surname == "panek", "Pánek", surname),
      surname = ifelse(surname == "novotný", "Novotný", surname),
      surname = ifelse(surname == "selinger" | surname == "selinger/knihkupectví", "Selinger", surname),
      surname = ifelse(surname == "pokorná", "Pokorná", surname),
      surname = ifelse(surname == "přečková", "Přečková", surname),
      surname = ifelse(surname == "rebl", "Rebl", surname),
      surname = ifelse(surname == "sebera", "Sebera", surname),
      surname = ifelse(surname == "svoboda", "Svoboda", surname),
      surname = ifelse(surname == "zecha", "Zecha", surname),
      surname = ifelse(surname == "sedlák", "Sedlák", surname),
      surname = ifelse(surname == "seguin", "Seguin", surname),
      surname = ifelse(surname == "tilinger", "Tilinger", surname),
      surname = ifelse(surname == "vlášek", "Vlášek", surname),
      surname = ifelse(surname == "wihan", "Wihan", surname),
      surname = ifelse(surname == "vobecká", "Vobecká", surname),
      surname = ifelse(firstname == "hrůza", "Hrůza", surname),
      firstname = ifelse(firstname == "hrůza", "Ladislav", firstname),
      surname = ifelse(surname == "sklenka", "Sklenka", surname),
      surname = ifelse(surname == "skala", "Skala", surname),
      firstname = ifelse(firstname == "Marri", "Martin", firstname),
      surname = ifelse(surname == "skovajsa", "Skovajsa", surname),
      surname = ifelse(surname == "vendlova", "Vedlová", surname),
      surname = ifelse(surname == "miguele", "Miguele", surname),
      surname = ifelse(surname == "navrat", "Navrat", surname),
      surname = ifelse(surname == "pety", "Pety", surname),
      surname = ifelse(surname == "foltyn(bolsoncar)", "Foltyn", surname),
      surname = ifelse(surname == "Ř?ha", "Řeha", surname),
      surname = ifelse(surname == "ŠPINGL", "Špingl", surname),
      surname = ifelse(firstname == "Jánská" & birthdate == ymd("1963-06-04"), "Jánská", surname),
      firstname = ifelse(firstname == "Jánská" & birthdate == ymd("1963-06-04"), "Alena", firstname),
      surname = ifelse(firstname == "Šeborová" & birthdate == ymd("1957-01-15"), "Šeborová", surname),
      firstname = ifelse(firstname == "Šeborová" & birthdate == ymd("1957-01-15"), "Alena", firstname),
      surname = ifelse(firstname == "Mačková" & birthdate == ymd("1972-09-19"), "Mačková", surname),
      firstname = ifelse(firstname == "Mačková" & birthdate == ymd("1972-09-19"), "Andrea", firstname),
      surname = ifelse(firstname == "Biersacková" & birthdate == ymd("1946-02-06"), "Biersacková", surname),
      firstname = ifelse(firstname == "Biersacková" & birthdate == ymd("1946-02-06"), "Anna", firstname),
      surname = ifelse(firstname == "Dušek" & birthdate == ymd("1956-02-27"), "Dušek", surname),
      firstname = ifelse(firstname == "Dušek" & birthdate == ymd("1956-02-27"), "Antonín", firstname),
      surname = ifelse(firstname == "Impseil" & birthdate == ymd("1995-03-19"), "Impseil", surname),
      firstname = ifelse(firstname == "Impseil" & birthdate == ymd("1995-03-19"), "Daniel", firstname),
      surname = ifelse(firstname == "Šelešovský" & birthdate == ymd("1971-02-10"), "Šelešovský", surname),
      firstname = ifelse(firstname == "Šelešovský" & birthdate == ymd("1971-02-10"), "Daniel", firstname),
      surname = ifelse(firstname == "Urbánková" & birthdate == ymd("1952-10-10"), "Urbánková", surname),
      firstname = ifelse(firstname == "Urbánková" & birthdate == ymd("1952-10-10"), "Daniela", firstname),
      surname = ifelse(firstname == "Bojková" & birthdate == ymd("1963-08-24"), "Bojková", surname),
      firstname = ifelse(firstname == "Bojková" & birthdate == ymd("1963-08-24"), "Darja", firstname),
      
      
      surname = ifelse(firstname == "Dvořák" & birthdate == ymd("1991-02-03"), "Dvořák", surname),
      firstname = ifelse(firstname == "Dvořák" & birthdate == ymd("1991-02-03"), "David", firstname),
      surname = ifelse(firstname == "Ševčík" & birthdate == ymd("1967-07-04"), "Ševčík", surname),
      firstname = ifelse(firstname == "Ševčík" & birthdate == ymd("1967-07-04"), "David", firstname),
      surname = ifelse(firstname == "Sklenka" & birthdate == ymd("1986-07-13"), "Sklenka", surname),
      firstname = ifelse(firstname == "Sklenka" & birthdate == ymd("1986-07-13"), "David", firstname),
      surname = ifelse(firstname == "Pokorný" & birthdate == ymd("1959-07-31"), "Pokorný", surname),
      firstname = ifelse(firstname == "Pokorný" & birthdate == ymd("1959-07-31"), "David", firstname),
      surname = ifelse(firstname == "Gerneš" & birthdate == ymd("1991-04-25"), "Gerneš", surname),
      firstname = ifelse(firstname == "Gerneš" & birthdate == ymd("1991-04-25"), "David", firstname),
      firstname = ifelse(firstname == "gogo", "Gogo", firstname),
      surname = ifelse(surname == "Filipiecová Bc.", "Filipiecová", surname),
      surname = ifelse(firstname == "Blažek" & birthdate == ymd("1962-02-18"), "Blažek", surname),
      firstname = ifelse(firstname == "Blažek" & birthdate == ymd("1962-02-18"), "Igor", firstname),
      surname = ifelse(firstname == "Jurásek" & birthdate == ymd("1948-01-16"), "Jurásek", surname),
      firstname = ifelse(firstname == "Jurásek" & birthdate == ymd("1948-01-16"), "Igor", firstname),
      surname = ifelse(surname == "Ing.Valenta", "Valenta", surname),
      surname = ifelse(firstname == "Dukátníková" & birthdate == ymd("1989-02-19"), "Dukátníková", surname),
      firstname = ifelse(firstname == "Dukátníková" & birthdate == ymd("1989-02-19"), "Irena", firstname),
      surname = ifelse(firstname == "Nevludová" & birthdate == ymd("1977-07-29"), "Nevludová", surname),
      firstname = ifelse(firstname == "Nevludová" & birthdate == ymd("1977-07-29"), "Ivana", firstname),
      surname = ifelse(firstname == "Ondrušová" & birthdate == ymd("1964-01-24"), "Ondrušová", surname),
      firstname = ifelse(firstname == "Ondrušová" & birthdate == ymd("1964-01-24"), "Ivana", firstname),
      surname = ifelse(firstname == "Škurková" & birthdate == ymd("1962-05-18"), "Škurková", surname),
      firstname = ifelse(firstname == "Škurková" & birthdate == ymd("1962-05-18"), "Iveta", firstname),
      surname = ifelse(firstname == "Švancarová" & birthdate == ymd("1962-04-07"), "Švancarová", surname),
      firstname = ifelse(firstname == "Švancarová" & birthdate == ymd("1962-04-07"), "Iveta", firstname),
      surname = ifelse(firstname == "Jelínek" & birthdate == ymd("1970-06-19"), "Jelínek", surname),
      firstname = ifelse(firstname == "Jelínek" & birthdate == ymd("1970-06-19"), "Ivo", firstname),
      surname = ifelse(firstname == "Havran" & birthdate == ymd("1986-08-04"), "Havran", surname),
      firstname = ifelse(firstname == "Havran" & birthdate == ymd("1986-08-04"), "Jakub", firstname),
      surname = ifelse(firstname == "Morong" & birthdate == ymd("1995-10-02"), "Morong", surname),
      firstname = ifelse(firstname == "Morong" & birthdate == ymd("1995-10-02"), "Jakub", firstname),
      surname = ifelse(firstname == "Bílý" & birthdate == ymd("1984-08-29"), "Bílý", surname),
      firstname = ifelse(firstname == "Bílý" & birthdate == ymd("1984-08-29"), "Jan", firstname),
      surname = ifelse(firstname == "Frišhons" & birthdate == ymd("1984-12-13"), "Frišhons", surname),
      firstname = ifelse(firstname == "Frišhons" & birthdate == ymd("1984-12-13"), "Jan", firstname),
      surname = ifelse(firstname == "Fučík" & birthdate == ymd("1975-06-21"), "Fučík", surname),
      firstname = ifelse(firstname == "Fučík" & birthdate == ymd("1975-06-21"), "Jan", firstname),
      surname = ifelse(firstname == "Mašek" & birthdate == ymd("1984-10-18"), "Mašek", surname),
      firstname = ifelse(firstname == "Mašek" & birthdate == ymd("1984-10-18"), "Jan", firstname),
      surname = ifelse(firstname == "Hrnčíř" & birthdate == ymd("1977-05-01"), "Hrnčíř", surname),
      firstname = ifelse(firstname == "Hrnčíř" & birthdate == ymd("1977-05-01"), "Jan", firstname),
      surname = ifelse(firstname == "Hubený" & birthdate == ymd("1972-03-22"), "Hubený", surname),
      firstname = ifelse(firstname == "Hubený" & birthdate == ymd("1972-03-22"), "Jan", firstname),
      surname = ifelse(firstname == "Ouzký" & birthdate == ymd("1984-06-08"), "Ouzký", surname),
      firstname = ifelse(firstname == "Ouzký" & birthdate == ymd("1984-06-08"), "Jan", firstname),
      surname = ifelse(firstname == "Stöhr" & birthdate == ymd("1989-09-03"), "Stöhr", surname),
      firstname = ifelse(firstname == "Stöhr" & birthdate == ymd("1989-09-03"), "Jan", firstname),
      surname = ifelse(firstname == "Čestický" & birthdate == ymd("1967-05-13"), "Čestický", surname),
      firstname = ifelse(firstname == "Čestický" & birthdate == ymd("1967-05-13"), "Jan", firstname),
      surname = ifelse(firstname == "Štraub" & birthdate == ymd("1971-05-20"), "Štraub", surname),
      firstname = ifelse(firstname == "Štraub" & birthdate == ymd("1971-05-20"), "Jan", firstname),
      
      
      surname = ifelse(firstname == "Žižka" & birthdate == ymd("1954-09-25"), "Žižka", surname),
      firstname = ifelse(firstname == "Žižka" & birthdate == ymd("1954-09-25"), "Jan", firstname),
      surname = ifelse(firstname == "Žlebek" & birthdate == ymd("1978-07-31"), "Žlebek", surname),
      firstname = ifelse(firstname == "Žlebek" & birthdate == ymd("1978-07-31"), "Jan", firstname),
      surname = ifelse(firstname == "Plevková" & birthdate == ymd("1988-07-25"), "Plevková", surname),
      firstname = ifelse(firstname == "Plevková" & birthdate == ymd("1988-07-25"), "Jana", firstname),
      surname = ifelse(firstname == "Štablová" & birthdate == ymd("1969-10-25"), "Štablová", surname),
      firstname = ifelse(firstname == "Štablová" & birthdate == ymd("1969-10-25"), "Jana", firstname),
      surname = ifelse(firstname == "Levová" & birthdate == ymd("1977-06-17"), "Levová", surname),
      firstname = ifelse(firstname == "Levová" & birthdate == ymd("1977-06-17"), "Jana", firstname),
      surname = ifelse(firstname == "Valigurová" & birthdate == ymd("1969-12-19"), "Valigurová", surname),
      firstname = ifelse(firstname == "Valigurová" & birthdate == ymd("1969-12-19"), "Jana", firstname),
      surname = ifelse(firstname == "Vávrová" & birthdate == ymd("1961-07-08"), "Vávrová", surname),
      firstname = ifelse(firstname == "Vávrová" & birthdate == ymd("1961-07-08"), "Jana", firstname),
      surname = ifelse(firstname == "Dragounová" & birthdate == ymd("1943-11-07"), "Dragounová", surname),
      firstname = ifelse(firstname == "Dragounová" & birthdate == ymd("1943-11-07"), "Jana", firstname),
      surname = ifelse(firstname == "Plevková" & birthdate == ymd("1959-02-04"), "Plevková", surname),
      firstname = ifelse(firstname == "Plevková" & birthdate == ymd("1959-02-04"), "Jarmila", firstname),
      surname = ifelse(firstname == "Hendrychová" & birthdate == ymd("1960-10-27"), "Hendrychová", surname),
      firstname = ifelse(firstname == "Hendrychová" & birthdate == ymd("1960-10-27"), "Jarmila", firstname),
      surname = ifelse(firstname == "Broulík" & birthdate == ymd("1970-04-13"), "Broulík", surname),
      firstname = ifelse(firstname == "Broulík" & birthdate == ymd("1970-04-13"), "Jaromír", firstname),
      surname = ifelse(firstname == "Slíva" & birthdate == ymd("1977-01-07"), "Slíva", surname),
      firstname = ifelse(firstname == "Slíva" & birthdate == ymd("1977-01-07"), "Jaromír", firstname),
      surname = ifelse(firstname == "Wesley" & birthdate == ymd("1954-04-14"), "Wesley", surname),
      firstname = ifelse(firstname == "Wesley" & birthdate == ymd("1954-04-14"), "Eva", firstname),
      surname = ifelse(firstname == "Šťovíčková" & birthdate == ymd("1948-05-25"), "Šťovíčková", surname),
      firstname = ifelse(firstname == "Šťovíčková" & birthdate == ymd("1948-05-25"), "Eva", firstname),
      surname = ifelse(firstname == "Klokočka" & birthdate == ymd("1980-03-22"), "Klokočka", surname),
      firstname = ifelse(firstname == "Klokočka" & birthdate == ymd("1980-03-22"), "Evžen", firstname),
      surname = ifelse(firstname == "Gogo" & birthdate == ymd("1990-07-03"), "Gogo", surname),
      firstname = ifelse(firstname == "Gogo" & birthdate == ymd("1990-07-03"), "Fafi", firstname),
      surname = ifelse(firstname == "Švancarová" & birthdate == ymd("1967-04-07"), "Švancarová", surname),
      firstname = ifelse(firstname == "Švancarová" & birthdate == ymd("1967-04-07"), "Iveta", firstname),
      surname = ifelse(firstname == "Dvořák" & birthdate == ymd("1957-11-04"), "Dvořák", surname),
      firstname = ifelse(firstname == "Dvořák" & birthdate == ymd("1957-11-04"), "Jaroslav", firstname),
      surname = ifelse(firstname == "Holík" & birthdate == ymd("1953-06-29"), "Holík", surname),
      firstname = ifelse(firstname == "Holík" & birthdate == ymd("1953-06-29"), "Jaroslav", firstname),
      surname = ifelse(firstname == "Jána" & birthdate == ymd("1971-03-07"), "Jána", surname),
      firstname = ifelse(firstname == "Jána" & birthdate == ymd("1971-03-07"), "Jaroslav", firstname),
      surname = ifelse(firstname == "Konečný" & birthdate == ymd("1956-07-03"), "Konečný", surname),
      firstname = ifelse(firstname == "Konečný" & birthdate == ymd("1956-07-03"), "Jaroslav", firstname),
      surname = ifelse(firstname == "Nečas" & birthdate == ymd("1964-03-23"), "Nečas", surname),
      firstname = ifelse(firstname == "Nečas" & birthdate == ymd("1964-03-23"), "Jaroslav", firstname),
      surname = ifelse(firstname == "Tyrala" & birthdate == ymd("1981-10-18"), "Tyrala", surname),
      firstname = ifelse(firstname == "Tyrala" & birthdate == ymd("1981-10-18"), "Jaroslav", firstname),
      surname = ifelse(firstname == "Krejcarová" & birthdate == ymd("1991-06-29"), "Krejcarová", surname),
      firstname = ifelse(firstname == "Krejcarová" & birthdate == ymd("1991-06-29"), "Jitka", firstname),
      surname = ifelse(firstname == "Dobešová" & birthdate == ymd("1957-05-28"), "Dobešová", surname),
      firstname = ifelse(firstname == "Dobešová" & birthdate == ymd("1957-05-28"), "Jiřina", firstname),
      surname = ifelse(firstname == "Horáková" & birthdate == ymd("1949-08-16"), "Horáková", surname),
      firstname = ifelse(firstname == "Horáková" & birthdate == ymd("1949-08-16"), "Jiřina", firstname),
      surname = ifelse(firstname == "Dvořák" & birthdate == ymd("1977-07-15"), "Dvořák", surname),
      firstname = ifelse(firstname == "Dvořák" & birthdate == ymd("1977-07-15"), "Jiří", firstname),
      surname = ifelse(firstname == "Kysling" & birthdate == ymd("1979-07-14"), "Kysling", surname),
      firstname = ifelse(firstname == "Kysling" & birthdate == ymd("1979-07-14"), "Jiří", firstname),
      surname = ifelse(firstname == "Strnad" & (birthdate == ymd("1959-09-26") | birthdate == ymd("1959-06-26")), "Strnad", surname),
      firstname = ifelse(firstname == "Strnad" & (birthdate == ymd("1959-09-26") | birthdate == ymd("1959-06-26")), "Jiří", firstname),
      surname = ifelse(firstname == "Čejka" & birthdate == ymd("1983-08-30"), "Čejka", surname),
      firstname = ifelse(firstname == "Čejka" & birthdate == ymd("1983-08-30"), "Jiří", firstname),
      surname = ifelse(firstname == "Soler" & birthdate == ymd("1947-04-20"), "Soler", surname),
      firstname = ifelse(firstname == "Soler" & birthdate == ymd("1947-04-20"), "Jiří", firstname),
      surname = ifelse(firstname == "Nitsche" & birthdate == ymd("1952-04-13"), "Nitsche", surname),
      firstname = ifelse(firstname == "Nitsche" & birthdate == ymd("1952-04-13"), "Jiří", firstname),
      surname = ifelse(firstname == "Kohoutek" & birthdate == ymd("1964-11-26"), "Kohoutek", surname),
      firstname = ifelse(firstname == "Kohoutek" & birthdate == ymd("1964-11-26"), "Jiří", firstname),
      surname = ifelse(firstname == "Kocman" & birthdate == ymd("1955-06-01"), "Kocman", surname),
      firstname = ifelse(firstname == "Kocman" & birthdate == ymd("1955-06-01"), "Jiří", firstname),
      surname = ifelse(firstname == "Kobza" & birthdate == ymd("1955-12-27"), "Kobza", surname),
      firstname = ifelse(firstname == "Kobza" & birthdate == ymd("1955-12-27"), "Jiří", firstname),
      surname = ifelse(firstname == "Antuš" & birthdate == ymd("1973-09-16"), "Antuš", surname),
      firstname = ifelse(firstname == "Antuš" & birthdate == ymd("1973-09-16"), "Jiří", firstname),
      surname = ifelse(firstname == "Pinkava" & birthdate == ymd("1963-10-01"), "Pinkava", surname),
      firstname = ifelse(firstname == "Pinkava" & birthdate == ymd("1963-10-01"), "Jiří", firstname),    
      birthdate = ifelse(firstname == "Jiří" & surname == "Strnad" & birthdate == ymd("1959-06-26"), 
                         format(as.Date(ymd("1959-09-26"))), format(as.Date(birthdate),'%Y-%m-%d')),
      surname = ifelse(firstname == "Doležal" & birthdate == ymd("1949-03-18"), "Doležal", surname),
      firstname = ifelse(firstname == "Doležal" & birthdate == ymd("1949-03-18"), "John", firstname),
      surname = ifelse(firstname == "Doležal" & birthdate == ymd("1968-05-18"), "Doležal", surname),
      firstname = ifelse(firstname == "Doležal" & birthdate == ymd("1968-05-18"), "Josef", firstname),
      surname = ifelse(firstname == "Kodýtek" & birthdate == ymd("1998-02-04"), "Kodýtek", surname),
      firstname = ifelse(firstname == "Kodýtek" & birthdate == ymd("1998-02-04"), "Josef", firstname),
      surname = ifelse(firstname == "Kopřiva" & birthdate == ymd("1978-10-10"), "Kopřiva", surname),
      firstname = ifelse(firstname == "Kopřiva" & birthdate == ymd("1978-10-10"), "Josef", firstname),
      surname = ifelse(firstname == "Pleva" & birthdate == ymd("1961-06-22"), "Pleva", surname),
      firstname = ifelse(firstname == "Pleva" & birthdate == ymd("1961-06-22"), "Josef", firstname),
      surname = ifelse(firstname == "Sukup" & birthdate == ymd("1959-12-04"), "Sukup", surname),
      firstname = ifelse(firstname == "Sukup" & birthdate == ymd("1959-12-04"), "Josef", firstname),
      surname = ifelse(firstname == "Zakopčan" & birthdate == ymd("1961-05-21"), "Zakopčan", surname),
      firstname = ifelse(firstname == "Zakopčan" & birthdate == ymd("1961-05-21"), "Josef", firstname),
      surname = ifelse(firstname == "Ondřej" & birthdate == ymd("1958-01-28"), "Ondřej", surname),
      firstname = ifelse(firstname == "Ondřej" & birthdate == ymd("1958-01-28"), "Josef", firstname),
      surname = ifelse(surname == "Kadlecova" & birthdate == ymd("1990-07-30"), "Kadlecová", surname),
      firstname = ifelse(firstname == "Stepanka" & birthdate == ymd("1990-07-30"), "Štěpánka", firstname),
      surname = ifelse(firstname == "Běrský" & birthdate == ymd("1958-07-16"), "Běrský", surname),
      firstname = ifelse(firstname == "Běrský" & birthdate == ymd("1958-07-16"), "Kamil", firstname),
      surname = ifelse(firstname == "Kokolusová" & birthdate == ymd("1967-05-29"), "Kokolusová", surname),
      firstname = ifelse(firstname == "Kokolusová" & birthdate == ymd("1967-05-29"), "Kamila", firstname),
      surname = ifelse(firstname == "Bák" & birthdate == ymd("1978-06-04"), "Bák", surname),
      firstname = ifelse(firstname == "Bák" & birthdate == ymd("1978-06-04"), "Karel", firstname),
      surname = ifelse(firstname == "Fiala" & birthdate == ymd("1987-07-12"), "Fiala", surname),
      firstname = ifelse(firstname == "Fiala" & birthdate == ymd("1987-07-12"), "Karel", firstname),
      surname = ifelse(firstname == "Kříž" & birthdate == ymd("1951-03-23"), "Kříž", surname),
      firstname = ifelse(firstname == "Kříž" & birthdate == ymd("1951-03-23"), "Karel", firstname),
      surname = ifelse(firstname == "Oplt" & birthdate == ymd("1945-09-06"), "Oplt", surname),
      firstname = ifelse(firstname == "Oplt" & birthdate == ymd("1945-09-06"), "Karel", firstname),
      surname = ifelse(firstname == "Ošťadnický" & birthdate == ymd("1971-10-06"), "Ošťadnický", surname),
      firstname = ifelse(firstname == "Ošťadnický" & birthdate == ymd("1971-10-06"), "Karel", firstname),
      surname = ifelse(firstname == "Maříková" & birthdate == ymd("1981-03-19"), "Maříková", surname),
      firstname = ifelse(firstname == "Maříková" & birthdate == ymd("1981-03-19"), "Karel", firstname),
      surname = ifelse(surname == "Kolečk?ř" & birthdate == ymd("1994-01-21"), "Kolečkář", surname),
      surname = ifelse(surname == "KrŇanský" & birthdate == ymd("1944-09-04"), "Krňanský", surname), 
      
      surname = ifelse(firstname == "Maříková" & birthdate == ymd("1981-03-19"), "Maříková", surname),
      firstname = ifelse(firstname == "Maříková" & birthdate == ymd("1981-03-19"), "Karel", firstname),
      surname = ifelse(firstname == "Hammer" & birthdate == ymd("1982-03-17"), "Hammer", surname),
      firstname = ifelse(firstname == "Hammer" & birthdate == ymd("1982-03-17"), "Ladislav", firstname),
      surname = ifelse(firstname == "Hora" & birthdate == ymd("1952-06-05"), "Hora", surname),
      firstname = ifelse(firstname == "Hora" & birthdate == ymd("1952-06-05"), "Ladislav", firstname),
      surname = ifelse(firstname == "Ulrych" & birthdate == ymd("1991-11-20"), "Ulrych", surname),
      firstname = ifelse(firstname == "Ulrych" & birthdate == ymd("1991-11-20"), "Ladislav", firstname),
      surname = ifelse(firstname == "Vali" & birthdate == ymd("1964-07-09"), "Vali", surname),
      firstname = ifelse(firstname == "Vali" & birthdate == ymd("1964-07-09"), "Libor", firstname),
      surname = ifelse(firstname == "Volná" & birthdate == ymd("1957-07-03"), "Volná", surname),
      firstname = ifelse(firstname == "Volná" & birthdate == ymd("1957-07-03"), "Libuše", firstname),
      surname = ifelse(firstname == "Volný" & birthdate == ymd("1973-07-03"), "Volný", surname),
      firstname = ifelse(firstname == "Volný" & birthdate == ymd("1973-07-03"), "Lubomír", firstname),
      surname = ifelse(firstname == "Kapr" & birthdate == ymd("1979-08-09"), "Kapr", surname),
      firstname = ifelse(firstname == "Kapr" & birthdate == ymd("1979-08-09"), "Luboš", firstname),
      surname = ifelse(firstname == "Vitásek" & birthdate == ymd("1952-06-24"), "Vitásek", surname),
      firstname = ifelse(firstname == "Vitásek" & birthdate == ymd("1952-06-24"), "Lubomír", firstname),
      surname = ifelse(firstname == "Španěl" & birthdate == ymd("1955-10-01"), "Španěl", surname),
      firstname = ifelse(firstname == "Španěl" & birthdate == ymd("1955-10-01"), "Lubomír", firstname),
      surname = ifelse(firstname == "Burianová" & birthdate == ymd("1987-10-09"), "Burianová", surname),
      firstname = ifelse(firstname == "Burianová" & birthdate == ymd("1987-10-09"), "Lucie", firstname),
      surname = ifelse(firstname == "Šafránková" & birthdate == ymd("1987-05-21"), "Šafránková", surname),
      firstname = ifelse(firstname == "Šafránková" & birthdate == ymd("1987-05-21"), "Lucie", firstname),
      surname = ifelse(firstname == "Štěpánková" & birthdate == ymd("1988-01-06"), "Štěpánková", surname),
      firstname = ifelse(firstname == "Štěpánková" & birthdate == ymd("1988-01-06"), "Lucie", firstname),
      surname = ifelse(firstname == "Mičolová" & birthdate == ymd("1945-02-17"), "Mičolová", surname),
      firstname = ifelse(firstname == "Mičolová" & birthdate == ymd("1945-02-17"), "Ludvika", firstname),
      surname = ifelse(firstname == "Hromada" & birthdate == ymd("1971-04-14"), "Hromada", surname),
      firstname = ifelse(firstname == "Hromada" & birthdate == ymd("1971-04-14"), "Luděk", firstname),
      surname = ifelse(firstname == "Žídek" & birthdate == ymd("1950-10-11"), "Žídek", surname),
      firstname = ifelse(firstname == "Žídek" & birthdate == ymd("1950-10-11"), "Luděk", firstname),
      surname = ifelse(firstname == "Tomešová" & birthdate == ymd("1968-11-04"), "Tomešová", surname),
      firstname = ifelse(firstname == "Tomešová" & birthdate == ymd("1968-11-04"), "Luďka", firstname),
      surname = ifelse(firstname == "Skříčková" & birthdate == ymd("1966-01-31"), "Skříčková", surname),
      firstname = ifelse(firstname == "Skříčková" & birthdate == ymd("1966-01-31"), "Marcela", firstname),
      surname = ifelse(firstname == "Heydušek" & birthdate == ymd("1965-11-02"), "Heydušek", surname),
      firstname = ifelse(firstname == "Heydušek" & birthdate == ymd("1965-11-02"), "Marcel", firstname),
      surname = ifelse(firstname == "Vysocký" & birthdate == ymd("1969-02-23"), "Vysocký", surname),
      firstname = ifelse(firstname == "Vysocký" & birthdate == ymd("1969-02-23"), "Marek", firstname),
      surname = ifelse(firstname == "Žalman" & birthdate == ymd("1974-12-09"), "Žalman", surname),
      firstname = ifelse(firstname == "Žalman" & birthdate == ymd("1974-12-09"), "Marek", firstname),
      birthdate = ifelse(birthdate == ymd("1962-12-25") & firstname == "Bojko", format(as.Date(ymd("1962-01-25"))), format(as.Date(birthdate),'%Y-%m-%d')),
      surname = ifelse(firstname == "Bojko" & birthdate == ymd("1962-01-25"), "Bojko", surname),
      firstname = ifelse(firstname == "Bojko" & birthdate == ymd("1962-01-25"), "Marian", firstname),
      surname = ifelse(firstname == "Švec" & birthdate == ymd("1992-07-10"), "Švec", surname),
      firstname = ifelse(firstname == "Švec" & birthdate == ymd("1992-07-10"), "Marian", firstname),
      surname = ifelse(firstname == "Malcharová" & birthdate == ymd("1963-11-03"), "Malcharová", surname),
      firstname = ifelse(firstname == "Malcharová" & birthdate == ymd("1963-11-03"), "Marie", firstname),
      surname = ifelse(firstname == "Brož" & birthdate == ymd("1981-08-28"), "Brož", surname),
      firstname = ifelse(firstname == "Brož" & birthdate == ymd("1981-08-28"), "Martin", firstname),
      surname = ifelse(firstname == "Bojko" & birthdate == ymd("1982-10-01"), "Bojko", surname),
      firstname = ifelse(firstname == "Bojko" & birthdate == ymd("1982-10-01"), "Martin", firstname),
      surname = ifelse(firstname == "Mihulová" & birthdate == ymd("1968-02-16"), "Mihulová", surname),
      firstname = ifelse(firstname == "Mihulová" & birthdate == ymd("1968-02-16"), "Marta", firstname),
      surname = ifelse(firstname == "Vlčková" & birthdate == ymd("1966-07-01"), "Vlčková", surname),
      firstname = ifelse(firstname == "Vlčková" & birthdate == ymd("1966-07-01"), "Markéta", firstname),
      surname = ifelse(firstname == "Kovaříková" & birthdate == ymd("1947-08-29"), "Kovaříková", surname),
      firstname = ifelse(firstname == "Kovaříková" & birthdate == ymd("1947-08-29"), "Markéta", firstname),
      
      surname = ifelse(firstname == "Pešat" & birthdate == ymd("1985-04-13"), "Pešat", surname),
      firstname = ifelse(firstname == "Pešat" & birthdate == ymd("1985-04-13"), "Martin", firstname),
      surname = ifelse(firstname == "Franzová" & birthdate == ymd("1959-09-10"), "Franzová", surname),
      firstname = ifelse(firstname == "Franzová" & birthdate == ymd("1959-09-10"), "Martina", firstname),
      surname = ifelse(firstname == "Příhoda" & birthdate == ymd("1979-02-01"), "Příhoda", surname),
      firstname = ifelse(firstname == "Příhoda" & birthdate == ymd("1979-02-01"), "Martin", firstname),
      surname = ifelse(firstname == "Plojhar" & birthdate == ymd("1978-06-16"), "Plojhar", surname),
      firstname = ifelse(firstname == "Plojhar" & birthdate == ymd("1978-06-16"), "Martin", firstname),
      surname = ifelse(firstname == "Sokele" & birthdate == ymd("1984-06-10"), "Sokele", surname),
      firstname = ifelse(firstname == "Sokele" & birthdate == ymd("1984-06-10"), "Martin", firstname),
      surname = ifelse(firstname == "Řehák" & birthdate == ymd("1990-03-15"), "Řehák", surname),
      firstname = ifelse(firstname == "Řehák" & birthdate == ymd("1990-03-15"), "Martin", firstname),
      surname = ifelse(firstname == "Malášek" & birthdate == ymd("1970-09-16"), "Malášek", surname),
      firstname = ifelse(firstname == "Malášek" & birthdate == ymd("1970-09-16"), "Martin", firstname),
      surname = ifelse(firstname == "Homolová" & birthdate == ymd("1982-07-01"), "Homolová", surname),
      firstname = ifelse(firstname == "Homolová" & birthdate == ymd("1982-07-01"), "Martina", firstname),
      surname = ifelse(firstname == "Topičová" & birthdate == ymd("1964-11-07"), "Topičová", surname),
      firstname = ifelse(firstname == "Topičová" & birthdate == ymd("1964-11-07"), "Martina", firstname),
      surname = ifelse(firstname == "Kachlík" & birthdate == ymd("1987-01-21"), "Kachlík", surname),
      firstname = ifelse(firstname == "Kachlík" & birthdate == ymd("1987-01-21"), "Matěj", firstname),
      surname = ifelse(firstname == "Nastoupilová" & birthdate == ymd("1989-11-24"), "Nastoupilová", surname),
      firstname = ifelse(firstname == "Nastoupilová" & birthdate == ymd("1989-11-24"), "Michaela", firstname),
      birthdate = ifelse(birthdate == ymd("1982-11-30") & firstname == "Dlouhý", format(as.Date(ymd("1980-11-30"))), format(as.Date(birthdate),'%Y-%m-%d')),
      surname = ifelse(firstname == "Dlouhý" & birthdate == ymd("1980-11-30"), "Dlouhý", surname),
      firstname = ifelse(firstname == "Dlouhý" & birthdate == ymd("1980-11-30"), "Michal", firstname),
      surname = ifelse(firstname == "Kučera" & birthdate == ymd("1975-02-26"), "Kučera", surname),
      firstname = ifelse(firstname == "Kučera" & birthdate == ymd("1975-02-26"), "Michal", firstname),
      surname = ifelse(firstname == "Tecl" & birthdate == ymd("1965-12-08"), "Tecl", surname),
      firstname = ifelse(firstname == "Tecl" & birthdate == ymd("1965-12-08"), "Michal", firstname),
      surname = ifelse(firstname == "Wisiorek" & birthdate == ymd("1977-05-04"), "Wisiorek", surname),
      firstname = ifelse(firstname == "Wisiorek" & birthdate == ymd("1977-05-04"), "Michal", firstname),
      surname = ifelse(firstname == "Imling" & birthdate == ymd("1955-12-05"), "Imling", surname),
      firstname = ifelse(firstname == "Imling" & birthdate == ymd("1955-12-05"), "Mikuláš", firstname),
      surname = ifelse(firstname == "Kuchár" & birthdate == ymd("1968-01-06"), "Kuchár", surname),
      firstname = ifelse(firstname == "Kuchár" & birthdate == ymd("1968-01-06"), "Milan", firstname),
      surname = ifelse(firstname == "Teuchner" & birthdate == ymd("1947-08-08"), "Teuchner", surname),
      firstname = ifelse(firstname == "Teuchner" & birthdate == ymd("1947-08-08"), "Miloš", firstname),
      surname = ifelse(firstname == "Čech" & birthdate == ymd("1960-05-01"), "Čech", surname),
      firstname = ifelse(firstname == "Čech" & birthdate == ymd("1960-05-01"), "Miloslav", firstname),
      surname = ifelse(firstname == "Rozner" & birthdate == ymd("1977-03-29"), "Rozner", surname),
      firstname = ifelse(firstname == "Rozner" & birthdate == ymd("1977-03-29"), "Miloslav", firstname),
      surname = ifelse(firstname == "Vovesný" & birthdate == ymd("1964-12-18"), "Vovesný", surname),
      firstname = ifelse(firstname == "Vovesný" & birthdate == ymd("1964-12-18"), "Milan", firstname),
      surname = ifelse(firstname == "Sek" & birthdate == ymd("1961-10-12"), "Sek", surname),
      firstname = ifelse(firstname == "Sek" & birthdate == ymd("1961-10-12"), "Milan", firstname),
      surname = ifelse(firstname == "Palička" & birthdate == ymd("1980-09-08"), "Palička", surname),
      
      
      firstname = ifelse(firstname == "Palička" & birthdate == ymd("1980-09-08"), "Milan", firstname),
      surname = ifelse(firstname == "Nevlud" & birthdate == ymd("1965-06-16"), "Nevlud", surname),
      firstname = ifelse(firstname == "Nevlud" & birthdate == ymd("1965-06-16"), "Milan", firstname),
      surname = ifelse(firstname == "Křejčířová" & birthdate == ymd("1946-04-06"), "Křejčířová", surname),
      firstname = ifelse(firstname == "Křejčířová" & birthdate == ymd("1946-04-06"), "Miluše", firstname),
      surname = ifelse(firstname == "Lošťák" & birthdate == ymd("1954-11-23"), "Lošťák", surname),
      firstname = ifelse(firstname == "Lošťák" & birthdate == ymd("1954-11-23"), "Miroslav", firstname),
      surname = ifelse(firstname == "Volná" & birthdate == ymd("1946-07-10"), "Volná", surname),
      firstname = ifelse(firstname == "Volná" & birthdate == ymd("1946-07-10"), "Miroslava", firstname),
      surname = ifelse(firstname == "Kleisner" & birthdate == ymd("1984-03-15"), "Kleisner", surname),
      firstname = ifelse(firstname == "Kleisner" & birthdate == ymd("1984-03-15"), "Miroslav", firstname),
      surname = ifelse(firstname == "Šubrt" & birthdate == ymd("1954-10-03"), "Šubrt", surname),
      firstname = ifelse(firstname == "Šubrt" & birthdate == ymd("1954-10-03"), "Miroslav", firstname),
      surname = ifelse(surname == "Nápravnková" & birthdate == ymd("1971-02-22"), "Nápravníková", surname),
      surname = ifelse(firstname == "Smejkal" & birthdate == ymd("1944-12-07"), "Smejkal", surname),
      firstname = ifelse(firstname == "Smejkal" & birthdate == ymd("1944-12-07"), "Oldřich", firstname),
      surname = ifelse(firstname == "Černý" & birthdate == ymd("1965-09-10"), "Černý", surname),
      firstname = ifelse(firstname == "Černý" & birthdate == ymd("1965-09-10"), "Oldřich", firstname),
      surname = ifelse(firstname == "Pech" & birthdate == ymd("1983-05-17"), "Pech", surname),
      firstname = ifelse(firstname == "Pech" & birthdate == ymd("1983-05-17"), "Ondřej", firstname),
      surname = ifelse(firstname == "Farský" & birthdate == ymd("1974-08-30"), "Farský", surname),
      firstname = ifelse(firstname == "Farský" & birthdate == ymd("1974-08-30"), "Patrik", firstname),
      surname = ifelse(firstname == "Klán" & birthdate == ymd("1972-02-05"), "Klán", surname),
      firstname = ifelse(firstname == "Klán" & birthdate == ymd("1972-02-05"), "Patrik", firstname),
      surname = ifelse(firstname == "Bezucha" & birthdate == ymd("1963-09-22"), "Bezucha", surname),
      firstname = ifelse(firstname == "Bezucha" & birthdate == ymd("1963-09-22"), "Pavel", firstname),
      surname = ifelse(firstname == "Domalíp" & birthdate == ymd("1974-02-22"), "Domalíp", surname),
      firstname = ifelse(firstname == "Domalíp" & birthdate == ymd("1974-02-22"), "Pavel", firstname),
      surname = ifelse(firstname == "Jelínek" & birthdate == ymd("1962-10-31"), "Jelínek", surname),
      firstname = ifelse(firstname == "Jelínek" & birthdate == ymd("1962-10-31"), "Pavel", firstname),
      surname = ifelse(firstname == "Kočí" & birthdate == ymd("1959-01-02"), "Kočí", surname),
      firstname = ifelse(firstname == "Kočí" & birthdate == ymd("1959-01-02"), "Pavel", firstname),
      surname = ifelse(firstname == "Pecho" & birthdate == ymd("1974-06-15"), "Pecho", surname),
      firstname = ifelse(firstname == "Pecho" & birthdate == ymd("1974-06-15"), "Pavel", firstname),
      surname = ifelse(firstname == "Popelka" & birthdate == ymd("1968-12-15"), "Popelka", surname),
      firstname = ifelse(firstname == "Popelka" & birthdate == ymd("1968-12-15"), "Pavel", firstname),
      surname = ifelse(firstname == "Suchý" & birthdate == ymd("1991-04-04"), "Suchý", surname),
      firstname = ifelse(firstname == "Suchý" & birthdate == ymd("1991-04-04"), "Pavel", firstname),
      surname = ifelse(firstname == "Vidlička" & birthdate == ymd("1948-02-11"), "Vidlička", surname),
      firstname = ifelse(firstname == "Vidlička" & birthdate == ymd("1948-02-11"), "Pavel", firstname),
      surname = ifelse(firstname == "Šubrt" & birthdate == ymd("1964-04-17"), "Šubrt", surname),
      firstname = ifelse(firstname == "Šubrt" & birthdate == ymd("1964-04-17"), "Pavel", firstname),
      surname = ifelse(surname == "Peksova", "Peksová", surname),
      surname = ifelse(surname == "Balcarova", "Balcarová", surname),
      surname = ifelse(surname == "Bartos", "Bartoš", surname),
      surname = ifelse(surname == "Belšan", "Belšán", surname),
      firstname = ifelse(firstname == "Bc Hana", "Hana", firstname),
      
      surname = ifelse(surname == "Cejkova" & birthdate == ymd("1973-03-29"), "Čejková", surname),
      firstname = ifelse(firstname == "Vaclava" & birthdate == ymd("1973-03-29"), "Václava", firstname),
      surname = ifelse(surname == "Cech" & birthdate == ymd("1986-03-10"), "Čech", surname),
      surname = ifelse(surname == "Cholenska", "Cholenská", surname),
      surname = ifelse(firstname == "Knot" & birthdate == ymd("1994-10-30"), "Knot", surname),
      firstname = ifelse(firstname == "Knot" & birthdate == ymd("1994-10-30"), "Dominik", firstname),
      surname = ifelse(surname == "Dvorak" & birthdate == ymd("1991-01-22"), "Dvořák", surname),
      surname = ifelse(surname == "Dvorakova" & birthdate == ymd("1992-06-01"), "Dvořáková", surname),
      firstname = ifelse(firstname == "Adela" & birthdate == ymd("1992-06-01"), "Adéla", firstname),
      surname = ifelse(surname == "Havlova", "Havlová", surname),
      surname = ifelse(surname == "Holubova", "Holubová", surname),
      firstname = ifelse(firstname == "Jan09", "Jan", firstname),
      firstname = ifelse(firstname == "Zdenek" & birthdate == ymd("1955-11-21"), "Zdeněk", firstname),
      surname = ifelse(surname == "Hromadkova", "Hromadková", surname),
      surname = ifelse(firstname == "Seidl" & birthdate == ymd("1993-02-02"), "Seidl", surname),
      firstname = ifelse(firstname == "Seidl" & birthdate == ymd("1993-02-02"), "Jakub", firstname),
      surname = ifelse(surname == "Jedelsky", "Jedelský", surname),
      surname = ifelse(surname == "Jiraskova", "Jirásková", surname),
      surname = ifelse(firstname == "Řezníčková" & birthdate == ymd("1973-09-24"), "Řezníčková", surname),
      firstname = ifelse(firstname == "Řezníčková" & birthdate == ymd("1973-09-24"), "Jitka", firstname),
      surname = ifelse(firstname == "Bartošová" & birthdate == ymd("1986-10-30"), "Bartošová", surname),
      firstname = ifelse(firstname == "Bartošová" & birthdate == ymd("1986-10-30"), "Jiřina", firstname),
      surname = ifelse(firstname == "Ulrychová" & birthdate == ymd("1981-04-03"), "Ulrychová", surname),
      firstname = ifelse(firstname == "Ulrychová" & birthdate == ymd("1981-04-03"), "Jiřina", firstname),
      surname = ifelse(firstname == "Bartoš" & birthdate == ymd("1969-02-22"), "Bartoš", surname),
      firstname = ifelse(firstname == "Bartoš" & birthdate == ymd("1969-02-22"), "Jiří", firstname),
      surname = ifelse(firstname == "Růžička" & birthdate == ymd("1978-02-02"), "Růžička", surname),
      firstname = ifelse(firstname == "Růžička" & birthdate == ymd("1978-02-02"), "Jiří", firstname),
      surname = ifelse(surname == "Kalendova", "Kalendová", surname),
      surname = ifelse(surname == "Kleptar", "Klepetář", surname),
      surname = ifelse(surname == "Konecny", "Konečný", surname),
      surname = ifelse(surname == "Kopecka" & birthdate == ymd("1979-09-22"), "Kopecká", surname),
      firstname = ifelse(firstname == "Vladimira" & birthdate == ymd("1979-09-22"), "Vladimíra", firstname),
      surname = ifelse(surname == "Kopecky", "Kopecký", surname),
      firstname = ifelse(firstname == "Regína" & birthdate == ymd("1962-05-17"), "Regina", firstname),
      surname = ifelse(surname == "LEPKOVÁ", "Lepková", surname),
      surname = ifelse(firstname == "Nesnera" & birthdate == ymd("1970-10-06"), "Nesnera", surname),
      firstname = ifelse(firstname == "Nesnera" & birthdate == ymd("1970-10-06"), "Ladislav", firstname),
      surname = ifelse((surname == "Laznovský" & birthdate == ymd("1980-03-06")) | (surname == "Lazznovsky" & birthdate == ymd("1980-03-06")), "Lažnovský", surname),
      surname = ifelse(surname == "Marianek", "Mariánek", surname),
      
      surname = ifelse(firstname == "Urbanová" & birthdate == ymd("1982-10-20"), "Urbanová", surname),
      firstname = ifelse(firstname == "Urbanová" & birthdate == ymd("1982-10-20"), "Marta", firstname),
      surname = ifelse(firstname == "Havlíček" & birthdate == ymd("2000-04-24"), "Havlíček", surname),
      firstname = ifelse(firstname == "Havlíček" & birthdate == ymd("2000-04-24"), "Martin", firstname),
      surname = ifelse(firstname == "Korčák" & birthdate == ymd("1974-10-30"), "Korčák", surname),
      firstname = ifelse(firstname == "Korčák" & birthdate == ymd("1974-10-30"), "Martin", firstname),
      surname = ifelse(surname == "Mayer Soukromé", "Mayer", surname),
      surname = ifelse(surname == "Moravova", "Moravová", surname),
      surname = ifelse(surname == "Necas", "Nečas", surname),
      surname = ifelse(firstname == "Mrázová" & birthdate == ymd("1984-04-02"), "Mrázová", surname),
      firstname = ifelse(firstname == "Mrázová" & birthdate == ymd("1984-04-02"), "Nikola", firstname),
      surname = ifelse(surname == "Novak", "Novák", surname),
      surname = ifelse(surname == "Novotny", "Novotný", surname),
      firstname = ifelse(firstname == "Olga Nováková", "Olga", firstname),
      surname = ifelse(surname == "Oborny", "Oborný", surname),
      surname = ifelse(surname == "Oborsky", "Oborský", surname),
      firstname = ifelse(firstname == "tom", "Tom", firstname),
      surname = ifelse(surname == "Pospisil", "Pospíšil", surname),
      firstname = ifelse(firstname == "Antonin", "Antonín", firstname),
      
      surname = ifelse(surname == "PÍREK", "Pírek", surname),
      surname = ifelse(firstname == "Kšicová" & birthdate == ymd("1987-01-07"), "Kšicová", surname),
      firstname = ifelse(firstname == "Kšicová" & birthdate == ymd("1987-01-07"), "Radka", firstname),
      firstname = ifelse(firstname == "Jaromir" & surname == "Rain" & birthdate == ymd("1958-01-08"), "Jaromír", firstname),
      firstname = ifelse(firstname == "Rozsypal Vojtěch" & birthdate == ymd("2004-05-24"), "Vojtěch", firstname),
      surname = ifelse(surname == "Sabó", "Sabo", surname),
      surname = ifelse(surname == "Sellers-Zajic", "Sellers-Zajíc", surname),
      firstname = ifelse(firstname == "Vojtech" & surname == "Sidorin", "Vojtěch", firstname),
      surname = ifelse(surname == "Sivacek" & birthdate == ymd("1984-09-05"), "Siváček", surname),
      surname = ifelse(surname == "Spacek" & firstname == "Lubomir", "Špaček", surname),
      firstname = ifelse(surname == "Špaček" & firstname == "Lubomir", "Lubomír", firstname),
      surname = ifelse(surname == "Sedlak", "Sedlák", surname),
      surname = ifelse(surname == "Stanek", "Staněk", surname),
      surname = ifelse(surname == "Sykora", "Sýkora", surname),
      surname = ifelse(surname == "Tomasek", "Tomášek", surname),
      surname = ifelse(surname == "Tomkova", "Tomková", surname),
      surname = ifelse(surname == "Tomsikova", "Tomšíková", surname),
      surname = ifelse(surname == "Totuaurova" | surname == "Totzauerova", "Totzauerová", surname),
      surname = ifelse(surname == "Triskova Koskova", "Kosková Třísková", surname),
      surname = ifelse(surname == "Tresnakova", "Třešňáková", surname),
      surname = ifelse(surname == "TurJanicová", "Turjanicová", surname),
      surname = ifelse(surname == "Toth", "Tóth", surname),
      surname = ifelse(surname == "Urtešlégr", "Uretšlégr", surname),
      surname = ifelse(surname == "Valencikova", "Valenčíková", surname),
      surname = ifelse(surname == "Volemanova", "Volemanová", surname),
      surname = ifelse(surname == "Wageknecht" & polparty == "pirati", "Wagenknecht", surname),
      firstname = ifelse(firstname == "David František", "David", firstname),
      surname = ifelse(surname == "Wirthova", "Wirthová", surname),
      surname = ifelse(surname == "Zahumenska", "Zahumenská", surname),
      surname = ifelse(surname == "Zamorova", "Zamorová", surname),
      surname = ifelse(surname == "Zemanek", "Zemánek", surname),
      surname = ifelse(surname == "špryňarová gottletová", "Špryňarová Gottletová", surname),
      surname = ifelse(firstname == "Hynar" & birthdate == ymd("1991-05-25"), "Hynar", surname),
      firstname = ifelse(firstname == "Hynar" & birthdate == ymd("1991-05-25"), "Adam", firstname),
      surname = ifelse(firstname == "Krajča" & birthdate == ymd("1977-07-15"), "Krajča", surname),
      firstname = ifelse((firstname == "Krajča" & birthdate == ymd("1977-07-15")) | (firstname == "Adrián" & birthdate == ymd("1977-07-15")), "Adrian", firstname),
      surname = ifelse(firstname == "Pavelková" & birthdate == ymd("1957-12-02"), "Pavelková", surname),
      firstname = ifelse(firstname == "Pavelková" & birthdate == ymd("1957-12-02"), "Anna", firstname),
      surname = ifelse(firstname == "Šilar" & birthdate == ymd("1961-02-22"), "Šilar", surname),
      firstname = ifelse(firstname == "Šilar" & birthdate == ymd("1961-02-22"), "Bohuslav", firstname),
      surname = ifelse(firstname == "Dudzik" & birthdate == ymd("1952-02-12"), "Dudzik", surname),
      firstname = ifelse(firstname == "Dudzik" & birthdate == ymd("1952-02-12"), "Bronislav", firstname),
      surname = ifelse(firstname == "Nevludová" & birthdate == ymd("1963-12-16"), "Nevludová", surname),
      firstname = ifelse(firstname == "Nevludová" & birthdate == ymd("1963-12-16"), "Bronislava", firstname),
      surname = ifelse(firstname == "Marková" & birthdate == ymd("1957-01-30"), "Marková", surname),
      
      firstname = ifelse(firstname == "Marková" & birthdate == ymd("1957-01-30"), "Dagmar", firstname),
      surname = ifelse(firstname == "Vlk" & birthdate == ymd("1974-05-16"), "Vlk", surname),
      firstname = ifelse(firstname == "Vlk" & birthdate == ymd("1974-05-16"), "Dalibor", firstname),
      surname = ifelse(firstname == "Kabrna" & birthdate == ymd("1950-08-31"), "Kabrna", surname),
      firstname = ifelse(firstname == "Kabrna" & birthdate == ymd("1950-08-31"), "František", firstname),
      surname = ifelse(firstname == "Kauer" & birthdate == ymd("1959-12-27"), "Kauer", surname),
      firstname = ifelse(firstname == "Kauer" & birthdate == ymd("1959-12-27"), "František", firstname),
      surname = ifelse(firstname == "Klásek" & birthdate == ymd("1955-12-16"), "Klásek", surname),
      firstname = ifelse(firstname == "Klásek" & birthdate == ymd("1955-12-16"), "František", firstname),
      surname = ifelse(firstname == "Konrád" & birthdate == ymd("1951-06-26"), "Konrád", surname),
      firstname = ifelse(firstname == "Konrád" & birthdate == ymd("1951-06-26"), "František", firstname),
      surname = ifelse(firstname == "Kosner" & birthdate == ymd("1959-07-04"), "Kosner", surname),
      firstname = ifelse(firstname == "Kosner" & birthdate == ymd("1959-07-04"), "František", firstname),
      surname = ifelse(firstname == "Kulhánek" & birthdate == ymd("1950-12-10"), "Kulhánek", surname),
      firstname = ifelse(firstname == "Kulhánek" & birthdate == ymd("1950-12-10"), "František", firstname),
      surname = ifelse(firstname == "Roháček" & birthdate == ymd("1947-08-06"), "Roháček", surname),
      firstname = ifelse(firstname == "Roháček" & birthdate == ymd("1947-08-06"), "František", firstname),
      surname = ifelse(firstname == "Wawrzacz" & birthdate == ymd("1957-12-01"), "Wawrzacz", surname),
      firstname = ifelse(firstname == "Wawrzacz" & birthdate == ymd("1957-12-01"), "František", firstname),
      surname = ifelse(firstname == "Čada" & birthdate == ymd("1979-12-15"), "Čada", surname),
      firstname = ifelse(firstname == "Čada" & birthdate == ymd("1979-12-15"), "František", firstname),
      surname = ifelse(firstname == "Vrátná" & birthdate == ymd("1971-03-01"), "Vrátná", surname),
      firstname = ifelse(firstname == "Vrátná" & birthdate == ymd("1971-03-01"), "Gabriela", firstname),
      surname = ifelse(firstname == "Jára" & birthdate == ymd("1974-04-27"), "Jára", surname),
      firstname = ifelse(firstname == "Jára" & birthdate == ymd("1974-04-27"), "Leoš", firstname),
      surname = ifelse(firstname == "Heitel" & birthdate == ymd("1986-11-20"), "Heitel", surname),
      firstname = ifelse(firstname == "Heitel" & birthdate == ymd("1986-11-20"), "Lukáš", firstname),
      surname = ifelse(firstname == "Charousová" & birthdate == ymd("1974-02-23"), "Charousová", surname),
      firstname = ifelse(firstname == "Charousová" & birthdate == ymd("1974-02-23"), "Monika", firstname),
      surname = ifelse(firstname == "Jarošová" & birthdate == ymd("1970-12-30"), "Jarošová", surname),
      firstname = ifelse(firstname == "Jarošová" & birthdate == ymd("1970-12-30"), "Monika", firstname),
      surname = ifelse(surname == "Owcarzy", "Owczarzy", surname),
      surname = ifelse(firstname == "Brázda" & birthdate == ymd("1954-06-07"), "Brázda", surname),
      firstname = ifelse(firstname == "Brázda" & birthdate == ymd("1954-06-07"), "Petr", firstname),
      surname = ifelse(firstname == "Doležel" & birthdate == ymd("1962-10-29"), "Doležel", surname),
      firstname = ifelse(firstname == "Doležel" & birthdate == ymd("1962-10-29"), "Petr", firstname),
      surname = ifelse(firstname == "Dostálek" & birthdate == ymd("1973-12-18"), "Dostálek", surname),
      firstname = ifelse(firstname == "Dostálek" & birthdate == ymd("1973-12-18"), "Petr", firstname),
      surname = ifelse(firstname == "Drbal" & birthdate == ymd("1978-09-12"), "Drbal", surname),
      firstname = ifelse(firstname == "Drbal" & birthdate == ymd("1978-09-12"), "Petr", firstname),
      surname = ifelse(firstname == "Hercik" & birthdate == ymd("1969-10-18"), "Hercik", surname),
      firstname = ifelse(firstname == "Hercik" & birthdate == ymd("1969-10-18"), "Petr", firstname),
      surname = ifelse(firstname == "Kovář" & birthdate == ymd("1983-06-16"), "Kovář", surname),
      firstname = ifelse(firstname == "Kovář" & birthdate == ymd("1983-06-16"), "Petr", firstname),
      surname = ifelse(firstname == "Krejčiřík" & birthdate == ymd("1967-11-04"), "Krejčiřík", surname),
      firstname = ifelse(firstname == "Krejčiřík" & birthdate == ymd("1967-11-04"), "Petr", firstname),
      
      surname = ifelse(firstname == "Laco" & birthdate == ymd("1964-02-06"), "Laco", surname),
      firstname = ifelse(firstname == "Laco" & birthdate == ymd("1964-02-06"), "Petr", firstname),
      surname = ifelse(firstname == "Macek" & birthdate == ymd("1991-08-19"), "Macek", surname),
      firstname = ifelse(firstname == "Macek" & birthdate == ymd("1991-08-19"), "Petr", firstname),
      surname = ifelse(firstname == "Mašíček" & birthdate == ymd("1978-02-10"), "Mašíček", surname),
      firstname = ifelse(firstname == "Mašíček" & birthdate == ymd("1978-02-10"), "Petr", firstname),
      surname = ifelse(firstname == "Mrňavý" & birthdate == ymd("1980-12-08"), "Mrňavý", surname),
      firstname = ifelse(firstname == "Mrňavý" & birthdate == ymd("1980-12-08"), "Petr", firstname),
      surname = ifelse(firstname == "Pech" & birthdate == ymd("1996-11-07"), "Pech", surname),
      firstname = ifelse(firstname == "Pech" & birthdate == ymd("1996-11-07"), "Petr", firstname),
      surname = ifelse(firstname == "Sklenář" & birthdate == ymd("1949-05-20"), "Sklenář", surname),
      firstname = ifelse(firstname == "Sklenář" & birthdate == ymd("1949-05-20"), "Petr", firstname),
      surname = ifelse(firstname == "Pustějovský" & birthdate == ymd("1971-05-09"), "Pustějovský", surname),
      firstname = ifelse(firstname == "Pustějovský" & birthdate == ymd("1971-05-09"), "René", firstname),
      surname = ifelse(firstname == "Broda" & birthdate == ymd("1979-02-08"), "Broda", surname),
      firstname = ifelse(firstname == "Broda" & birthdate == ymd("1979-02-08"), "Richard", firstname),
      surname = ifelse(firstname == "Michenka" & birthdate == ymd("1970-08-23"), "Michenka", surname),
      firstname = ifelse(firstname == "Michenka" & birthdate == ymd("1970-08-23"), "Richard", firstname),
      surname = ifelse(firstname == "Měchura" & birthdate == ymd("1978-06-28"), "Měchura", surname),
      firstname = ifelse(firstname == "Měchura" & birthdate == ymd("1978-06-28"), "Richard", firstname),
      surname = ifelse(firstname == "Staržec" & birthdate == ymd("1969-10-23"), "Staržec", surname),
      firstname = ifelse(firstname == "Staržec" & birthdate == ymd("1969-10-23"), "Richard", firstname),
      surname = ifelse(firstname == "Vymola" & birthdate == ymd("1968-03-23"), "Vymola", surname),
      firstname = ifelse(firstname == "Vymola" & birthdate == ymd("1968-03-23"), "Robert", firstname),
      surname = ifelse(firstname == "Čapkovič" & birthdate == ymd("1973-12-22"), "Čapkovič", surname),
      firstname = ifelse(firstname == "Čapkovič" & birthdate == ymd("1973-12-22"), "Robert", firstname),
      surname = ifelse(firstname == "Široký" & birthdate == ymd("1970-08-30"), "Široký", surname),
      firstname = ifelse(firstname == "Široký" & birthdate == ymd("1970-08-30"), "Robert", firstname),
      surname = ifelse(firstname == "Hába" & birthdate == ymd("1986-03-02"), "Hába", surname),
      firstname = ifelse(firstname == "Hába" & birthdate == ymd("1986-03-02"), "Roman", firstname),
      surname = ifelse(firstname == "Owczarzy" & birthdate == ymd("1968-02-18"), "Owczarzy", surname),
      firstname = ifelse(firstname == "Owczarzy" & birthdate == ymd("1968-02-18"), "Roman", firstname),
      surname = ifelse(firstname == "Razdíková" & birthdate == ymd("1961-08-21"), "Razdíková", surname),
      firstname = ifelse(firstname == "Razdíková" & birthdate == ymd("1961-08-21"), "Růžena", firstname),
      surname = ifelse(firstname == "Toningerová" & birthdate == ymd("1946-07-09"), "Toningerová", surname),
      firstname = ifelse(firstname == "Toningerová" & birthdate == ymd("1946-07-09"), "Růžena", firstname),
      surname = ifelse(surname == "Sedlák BTh.", "Sedlák", surname),
      surname = ifelse(firstname == "Šatná" & birthdate == ymd("1972-07-12"), "Šatná", surname),
      firstname = ifelse(firstname == "Šatná" & birthdate == ymd("1972-07-12"), "Světlana", firstname),
      surname = ifelse(firstname == "Okamura" & birthdate == ymd("1972-07-04"), "Okamura", surname),
      firstname = ifelse(firstname == "Okamura" & birthdate == ymd("1972-07-04"), "Tomio", firstname),
      surname = ifelse(firstname == "Balcar" & birthdate == ymd("1969-06-02"), "Balcar", surname),
      firstname = ifelse(firstname == "Balcar" & birthdate == ymd("1969-06-02"), "Tomáš", firstname),
      surname = ifelse(firstname == "Cikán" & birthdate == ymd("1969-03-23"), "Cikán", surname),
      firstname = ifelse(firstname == "Cikán" & birthdate == ymd("1969-03-23"), "Tomáš", firstname),
      
      surname = ifelse(firstname == "Hauzner" & birthdate == ymd("1962-10-18"), "Hauzner", surname),
      firstname = ifelse(firstname == "Hauzner" & birthdate == ymd("1962-10-18"), "Tomáš", firstname),
      surname = ifelse(firstname == "Hrnčárek" & birthdate == ymd("1975-07-14"), "Hrnčárek", surname),
      firstname = ifelse(firstname == "Hrnčárek" & birthdate == ymd("1975-07-14"), "Tomáš", firstname),
      surname = ifelse(firstname == "Jelínek" & birthdate == ymd("1966-07-14"), "Jelínek", surname),
      firstname = ifelse(firstname == "Jelínek" & birthdate == ymd("1966-07-14"), "Tomáš", firstname),
      surname = ifelse(firstname == "Kadlec" & birthdate == ymd("1984-05-08"), "Kadlec", surname),
      firstname = ifelse(firstname == "Kadlec" & birthdate == ymd("1984-05-08"), "Tomáš", firstname),
      surname = ifelse(firstname == "Kulhánek" & birthdate == ymd("1988-05-19"), "Kulhánek", surname),
      firstname = ifelse(firstname == "Kulhánek" & birthdate == ymd("1988-05-19"), "Tomáš", firstname),
      surname = ifelse(firstname == "Sabol" & birthdate == ymd("1968-04-25"), "Sabol", surname),
      firstname = ifelse(firstname == "Sabol" & birthdate == ymd("1968-04-25"), "Vladimír", firstname),
      surname = ifelse(firstname == "Suchý" & birthdate == ymd("1950-11-14"), "Suchý", surname),
      firstname = ifelse(firstname == "Suchý" & birthdate == ymd("1950-11-14"), "Vladimír", firstname),
      surname = ifelse(firstname == "Vaněk" & birthdate == ymd("1955-04-19"), "Vaněk", surname),
      firstname = ifelse(firstname == "Vaněk" & birthdate == ymd("1955-04-19"), "Vladimír", firstname),
      surname = ifelse(firstname == "Pracnová" & birthdate == ymd("1959-06-09"), "Pracnová", surname),
      firstname = ifelse(firstname == "Pracnová" & birthdate == ymd("1959-06-09"), "Vladimíra", firstname),
      surname = ifelse(firstname == "Čermák" & birthdate == ymd("1955-08-17"), "Čermák", surname),
      firstname = ifelse(firstname == "Čermák" & birthdate == ymd("1955-08-17"), "Vlastibor", firstname),
      surname = ifelse(firstname == "Drábek" & birthdate == ymd("1973-04-07"), "Drábek", surname),
      firstname = ifelse(firstname == "Drábek" & birthdate == ymd("1973-04-07"), "Vlastimil", firstname),
      surname = ifelse(firstname == "Mojžíšek" & birthdate == ymd("1960-06-20"), "Mojžíšek", surname),
      firstname = ifelse(firstname == "Mojžíšek" & birthdate == ymd("1960-06-20"), "Vlastimil", firstname),
      surname = ifelse(firstname == "Protivínský" & birthdate == ymd("1969-09-15"), "Protivínský", surname),
      firstname = ifelse(firstname == "Protivínský" & birthdate == ymd("1969-09-15"), "Vlastimil", firstname),
      firstname = ifelse(firstname == "luboslav", "Luboslav", firstname),
      surname = ifelse(firstname == "Šimorda" & birthdate == ymd("1957-09-07"), "Šimorda", surname),
      firstname = ifelse(firstname == "Šimorda" & birthdate == ymd("1957-09-07"), "Václav", firstname),
      surname = ifelse(firstname == "Šimík" & birthdate == ymd("1961-03-13"), "Šimík", surname),
      firstname = ifelse(firstname == "Šimík" & birthdate == ymd("1961-03-13"), "Václav", firstname),
      surname = ifelse(firstname == "Novák" & birthdate == ymd("1973-08-29"), "Novák", surname),
      firstname = ifelse(firstname == "Novák" & birthdate == ymd("1973-08-29"), "Vítězslav", firstname),
      surname = ifelse(firstname == "Strejcová" & birthdate == ymd("1953-03-30"), "Strejcová", surname),
      firstname = ifelse(firstname == "Strejcová" & birthdate == ymd("1953-03-30"), "Věra", firstname),
      birthdate = ifelse(birthdate == ymd("1961-10-25") & firstname == "Čechová", format(as.Date(ymd("1961-10-15"))), format(as.Date(birthdate),'%Y-%m-%d')),
      surname = ifelse(firstname == "Čechová" & birthdate == ymd("1961-10-15"), "Čechová", surname),
      firstname = ifelse(firstname == "Čechová" & birthdate == ymd("1961-10-15"), "Věra", firstname),
      surname = ifelse(firstname == "Růžičková" & birthdate == ymd("1966-01-12"), "Růžičková", surname),
      firstname = ifelse(firstname == "Růžičková" & birthdate == ymd("1966-01-12"), "Yvona", firstname),
      surname = ifelse(firstname == "Škurková" & birthdate == ymd("1970-11-14"), "Škurková", surname),
      firstname = ifelse(firstname == "Škurková" & birthdate == ymd("1970-11-14"), "Yvona", firstname),
      surname = ifelse(firstname == "Habrnal" & birthdate == ymd("1957-05-01"), "Habrnal", surname),
      firstname = ifelse(firstname == "Habrnal" & birthdate == ymd("1957-05-01"), "Zbyněk", firstname),
      surname = ifelse(firstname == "Bříza" & birthdate == ymd("1978-07-08"), "Bříza", surname),
      
      firstname = ifelse(firstname == "Bříza" & birthdate == ymd("1978-07-08"), "Zdeněk", firstname),
      surname = ifelse(firstname == "Cikrle" & birthdate == ymd("1963-09-30"), "Cikrle", surname),
      firstname = ifelse(firstname == "Cikrle" & birthdate == ymd("1963-09-30"), "Zdeněk", firstname),
      surname = ifelse(firstname == "Podal" & birthdate == ymd("1953-04-12"), "Podal", surname),
      firstname = ifelse(firstname == "Podal" & birthdate == ymd("1953-04-12"), "Zdeněk", firstname),
      surname = ifelse(firstname == "Polách" & birthdate == ymd("1961-01-16"), "Polách", surname),
      firstname = ifelse(firstname == "Polách" & birthdate == ymd("1961-01-16"), "Zdeněk", firstname),
      surname = ifelse(firstname == "Urbanová" & birthdate == ymd("1953-03-12"), "Urbanová", surname),
      firstname = ifelse(firstname == "Urbanová" & birthdate == ymd("1953-03-12"), "Zdeňka", firstname),
      surname = ifelse(firstname == "Frňková" & birthdate == ymd("1962-06-16"), "Frňková", surname),
      firstname = ifelse(firstname == "Frňková" & birthdate == ymd("1962-06-16"), "Zuzana", firstname),
      surname = ifelse(firstname == "Schneiderová" & birthdate == ymd("1974-10-07"), "Schneiderová", surname),
      firstname = ifelse(firstname == "Schneiderová" & birthdate == ymd("1974-10-07"), "Zuzana", firstname),
      surname = ifelse(surname == "Čech Ing.", "Čech", surname),
      surname = ifelse(firstname == "Koller" & birthdate == ymd("1961-06-08"), "Koller", surname),
      firstname = ifelse(firstname == "Koller" & birthdate == ymd("1961-06-08"), "Čestmír", firstname),
      surname = ifelse(firstname == "Nováková" & birthdate == ymd("1963-11-06"), "Nováková", surname),
      firstname = ifelse(firstname == "Nováková" & birthdate == ymd("1963-11-06"), "Šárka", firstname),
      surname = ifelse(surname == "Boublik", "Boublík", surname),
      birthdate = ifelse(birthdate == ymd("1947-09-10") & surname == "Boublík", format(as.Date(ymd("1974-09-10"))), format(as.Date(birthdate),'%Y-%m-%d')),
      firstname = ifelse(firstname == "Ing Pavel", "Pavel", firstname),
      firstname = ifelse(firstname == "Mgr Kateřina", "Kateřina", firstname),
      firstname = ifelse(firstname == "Vlastimil" & birthdate == ymd("1955-01-05") & surname == "Málek", "Vlastislav", firstname),
      firstname = ifelse(firstname == "Mgr Pavel", "Pavel", firstname),
      firstname = ifelse(firstname == "Ing Miroslav", "Miroslav", firstname),
      firstname = ifelse(firstname == "Mgr Martin", "Martin", firstname),
      surname = ifelse(surname == "Kočí  Palkovská", "Kočí Palkovská", surname),
      surname = ifelse(surname == "Rozmajzl st.", "Rozmajzl", surname),
      surname = ifelse(surname == "Dušková,Smrčková" | surname == "Dušková, Smrčková", "Dušková - Smrčková", surname),
      firstname = ifelse(firstname == "Pavlína Šetková", "Pavlína", firstname),
      surname = ifelse(surname == "Śverák", "Sverák", surname),
      surname = ifelse(surname == "ANTIĆ", "Antić", surname),
      firstname = ifelse(surname == "Adam Novák" & birthdate == ymd("1980-05-01"), "Adam Zdeněk", firstname),
      surname = ifelse(surname == "Adam Novák" & birthdate == ymd("1980-05-01"), "Novák", surname),
      surname = ifelse(surname == "Adamec ml.", "Adamec", surname),
      firstname = ifelse(surname == "Alberto Pinto Castillo" & birthdate == ymd("1969-04-08"), "Luis Alberto Pinto", firstname),
      surname = ifelse(surname == "Alberto Pinto Castillo" & birthdate == ymd("1969-04-08"), "Castillo", surname),
      surname = ifelse(firstname == "Németh" & birthdate == ymd("1959-08-24"), "Németh", surname),
      firstname = ifelse(surname == "Németh" & birthdate == ymd("1959-08-24"), "Karel", firstname),
      surname = ifelse(firstname == "Cvetan" & birthdate == ymd("1976-09-17"), "Cvetan", surname),
      firstname = ifelse(surname == "Cvetan" & birthdate == ymd("1976-09-17"), "Martin", firstname),
      surname = ifelse(firstname == "Tetík" & birthdate == ymd("1974-07-22"), "Tetík", surname),
      firstname = ifelse(surname == "Tetík" & birthdate == ymd("1974-07-22"), "Rudolf", firstname),
      surname = ifelse(firstname == "Petira" & birthdate == ymd("1941-11-20"), "Petira", surname),
      firstname = ifelse(surname == "Petira" & birthdate == ymd("1941-11-20"), "Václav", firstname),
      
      surname = ifelse(firstname == "Lačňák" & birthdate == ymd("1971-04-22"), "Lačňák", surname),
      firstname = ifelse(surname == "Lačňák" & birthdate == ymd("1971-04-22"), "Jan, Mgr., MBA", firstname),
      surname = ifelse(firstname == "Poukar" & birthdate == ymd("1955-06-02"), "Poukar", surname),
      firstname = ifelse(surname == "Poukar" & birthdate == ymd("1955-06-02"), "Jan", firstname),
      surname = ifelse(firstname == "Staněk" & birthdate == ymd("1940-12-06"), "Staněk", surname),
      firstname = ifelse(surname == "Staněk" & birthdate == ymd("1940-12-06"), "Jan", firstname),
      surname = ifelse(firstname == "Zugar" & birthdate == ymd("1987-04-06"), "Zugar", surname),
      firstname = ifelse(surname == "Zugar" & birthdate == ymd("1987-04-06"), "Jan", firstname),
      surname = ifelse(firstname == "Bradáč" & birthdate == ymd("1956-08-28"), "Bradáč", surname),
      firstname = ifelse(firstname == "Bradáč" & birthdate == ymd("1956-08-28"), "František", firstname),
      surname = ifelse(firstname == "Vávra" & birthdate == ymd("1967-01-26"), "Vávra", surname),
      firstname = ifelse(surname == "Vávra" & birthdate == ymd("1967-01-26"), "Radek", firstname),
      surname = ifelse(firstname == "Vávra" & birthdate == ymd("1983-09-12"), "Vávra", surname),
      firstname = ifelse(surname == "Vávra" & birthdate == ymd("1983-09-12"), "Radek", firstname),
      surname = ifelse(surname == "Jost", "Jošt", surname),
      surname = ifelse(surname == "Julinkova", "Julínková", surname),
      surname = ifelse(surname == "Pecicova", "Pecićová", surname),
      surname = ifelse(surname == "Drastik" & birthdate == ymd("1965-08-29"), "Drastík", surname),
      surname = ifelse(surname == "Dželiová" & birthdate == ymd("1971-03-15"), "Dželinová", surname),
      surname = ifelse(surname == "Hausner" & birthdate == ymd("1975-03-15"), "Hauzner", surname),
      surname = ifelse(surname == "Kajpr" & birthdate == ymd("1969-06-27"), "Kajprt", surname),
      surname = ifelse(surname == "Kauzarovičková" & birthdate == ymd("1961-11-28"), "Kauzlaričová", surname),
      surname = ifelse(surname == "Kostelník" & birthdate == ymd("1954-10-06"), "Kostelňák", surname),
      surname = ifelse(surname == "Löwenthálová" & birthdate == ymd("1969-12-10"), "Löwenthalová", surname),
      surname = ifelse(surname == "Malcher" & birthdate == ymd("1941-10-10"), "Malchar", surname),
      surname = ifelse(surname == "Nácalová" & birthdate == ymd("1944-06-06"), "Nácerová", surname),
      surname = ifelse(surname == "Pytlík" & birthdate == ymd("1956-07-26"), "Putlík", surname),
      surname = ifelse(surname == "Stankyová" & birthdate == ymd("1979-01-26"), "Stankayová", surname),
      surname = ifelse(surname == "Topinkovvá" & birthdate == ymd("1956-06-01"), "Topinková", surname),
      surname = ifelse(surname == "Vondrášová" & birthdate == ymd("1959-03-04"), "Vondrášková", surname),
      surname = ifelse(surname == "Vychodilová" & birthdate == ymd("1950-11-10"), "Vycudiliková", surname),
      surname = ifelse(surname == "Čoková" & birthdate == ymd("1938-06-25"), "Čomová", surname),
      surname = ifelse(surname == "Zachariáš" & birthdate == ymd("1993-04-28"), "Zachariaš", surname),
      surname = ifelse(surname == "Břichacek" & birthdate == ymd("1976-10-05"), "Břicháček", surname),
      surname = ifelse(surname == "Frolik" & birthdate == ymd("1963-04-04"), "Frolík", surname),
      surname = ifelse(surname == "Slovak" & birthdate == ymd("1959-03-08"), "Slovák", surname),
      surname = ifelse(surname == "Chlupacek" & birthdate == ymd("1986-09-07"), "Chlupáček", surname),
      surname = ifelse(surname == "Davidova" & birthdate == ymd("1990-07-07"), "Davidová", surname),
      surname = ifelse(surname == "Dřevíkovský" & birthdate == ymd("1995-07-07"), "Dřevikovský", surname),
      surname = ifelse(surname == "Frýbortova" & birthdate == ymd("1967-04-09"), "Frýbortová", surname),
      surname = ifelse(surname == "Haisova" & birthdate == ymd("1987-02-20"), "Haisová", surname),
      surname = ifelse(surname == "Hanzlik" & birthdate == ymd("1992-03-04"), "Hanzlík", surname),
      surname = ifelse(surname == "Herbrichova" & birthdate == ymd("1979-06-13"), "Herbrichová", surname),
      surname = ifelse(surname == "Jechova" & birthdate == ymd("1984-12-03"), "Jechová", surname),
      
      surname = ifelse(surname == "Kadanik" & birthdate == ymd("1982-08-03"), "Kadaník", surname),
      surname = ifelse(surname == "Macinsky" & birthdate == ymd("1984-03-07"), "Macinský", surname),
      surname = ifelse(surname == "Naicerová" & birthdate == ymd("1962-06-20"), "Naiclerová", surname),
      surname = ifelse(surname == "Pavlovic" & birthdate == ymd("1984-05-15"), "Pavlovič", surname),
      surname = ifelse(surname == "Popova" & birthdate == ymd("1976-06-13"), "Popová", surname),
      surname = ifelse(surname == "Poživilová" & birthdate == ymd("1980-08-20"), "Poživilová Michailidu", surname),
      surname = ifelse(surname == "Prokšová" & birthdate == ymd("1986-07-21"), "Prokšová Zuská", surname),
      surname = ifelse(surname == "Rozhon" & birthdate == ymd("1996-01-03"), "Rozhoň", surname),
      surname = ifelse(surname == "Rychtar" & birthdate == ymd("1990-08-14"), "Rychtář", surname),
      surname = ifelse(surname == "Salem" & birthdate == ymd("1987-03-17"), "Saleh Salem", surname),
      surname = ifelse(surname == "Strachotova" & birthdate == ymd("1991-11-17"), "Strachotová", surname),
      surname = ifelse(surname == "Svatuska" & birthdate == ymd("1986-03-12"), "Svatuška", surname),
      surname = ifelse(surname == "Tenzerova" & birthdate == ymd("1980-11-30"), "Tenzerová", surname),
      surname = ifelse(surname == "Ujfalusi" & birthdate == ymd("1979-07-17"), "Ujfaluši", surname),
      surname = ifelse(surname == "Vonesova" & birthdate == ymd("1991-06-27"), "Vonešová", surname),
      surname = ifelse(surname == "Bitomký" & birthdate == ymd("1981-02-06"), "Bitomský", surname),
      surname = ifelse(surname == "Břešťál" & birthdate == ymd("1950-11-05"), "Břečťál", surname),
      surname = ifelse(surname == "Burkhadová" & birthdate == ymd("1953-07-13"), "Burkhardová", surname),
      surname = ifelse(surname == "Kokolousová" & birthdate == ymd("1967-05-29"), "Kokolusová", surname),
      surname = ifelse(surname == "Bukanovská" & birthdate == ymd("1964-09-11"), "Bukovanská", surname),
      surname = ifelse(surname == "Hanuš" & birthdate == ymd("1971-07-22"), "Hanus", surname),
      surname = ifelse(surname == "Hřebáčková" & birthdate == ymd("1973-03-08"), "Hřebačková", surname),
      surname = ifelse(surname == "Knauer" & birthdate == ymd("1982-11-19"), "Knauer Belcredi", surname),
      surname = ifelse(surname == "Vostadovský" & birthdate == ymd("1970-11-22"), "Vostradovský", surname),
      surname = ifelse(surname == "Švanacara" & birthdate == ymd("1976-09-21"), "Švancara", surname),
      surname = ifelse(surname == "Kmiecová" & birthdate == ymd("1972-07-10"), "Kmiećová", surname),
      surname = ifelse(surname == "Licehamr" & birthdate == ymd("1965-07-30"), "Licehemer", surname),
      surname = ifelse(surname == "Hromádko" & birthdate == ymd("1990-11-28"), "Hromádka", surname),
      surname = ifelse(surname == "Drož Ml." & birthdate == ymd("1977-05-03"), "Drož", surname),
      surname = ifelse(surname == "Donátová" & birthdate == ymd("1970-12-05"), "Donatová", surname),
      surname = ifelse(surname == "Plánava" & birthdate == ymd("1956-05-30"), "Pláňava", surname),
      surname = ifelse(surname == "DAVID" & birthdate == ymd("1983-01-03"), "David", surname),
      surname = ifelse(surname == "Daniel Uličný" & birthdate == ymd("1976-04-17"), "Uličný", surname),
      firstname = ifelse(surname == "Uličný" & birthdate == ymd("1976-04-17"), "Daniel", firstname),
      surname = ifelse(surname == "FANTA" & birthdate == ymd("1959-04-15"), "Fanta", surname),
      surname = ifelse(surname == "Flekalová" & birthdate == ymd("1963-08-28"), "Flekalová-Sláviková", surname),
      surname = ifelse(surname == "HRABĚ" & birthdate == ymd("1964-04-03"), "Hrabě", surname),
      firstname = ifelse(firstname == "Dominik H" & birthdate == ymd("1993-10-25"), "Dominik", firstname),
      firstname = ifelse(firstname == "" & birthdate == ymd("1987-11-13"), "Jiři", firstname),
      surname = ifelse(surname == "Ing. Jiří Pešík" & birthdate == ymd("1987-11-13"), "Pešík", surname),
      firstname = ifelse(firstname == "" & birthdate == ymd("1970-04-02"), "Michal", firstname),
      surname = ifelse(surname == "Ing. Michal Vlačiha" & birthdate == ymd("1970-04-02"), "Vlačiha", surname),
      surname = ifelse(surname == "Ing. Stránský" & birthdate == ymd("1952-10-30"), "Stránský", surname),
      firstname = ifelse(firstname == "Dr" & birthdate == ymd("1971-09-23"), "Pavel", firstname),
      surname = ifelse(surname == "Ing. Pavel Pospíšil" & birthdate == ymd("1971-09-23"), "Pospíšil", surname),
      
      firstname = ifelse(surname == "Jan Petržílka" & birthdate == ymd("1971-06-18"), "Jan", firstname),
      surname = ifelse(surname == "Jan Petržílka" & birthdate == ymd("1971-06-18"), "Petržílka", surname),
      surname = ifelse(surname == "Janeček Ing.", "Janeček", surname),
      firstname = ifelse(surname == "Josef Kubíček" & birthdate == ymd("1951-05-22"), "Josef", firstname),
      surname = ifelse(surname == "KARLÍK", "Karlík", surname),
      surname = ifelse(surname == "Konopásek, Ing." & birthdate == ymd("1964-01-21"), "Konopásek", surname),
      firstname = ifelse(surname == "Kristýna Hosáková" & birthdate == ymd("1989-07-20"), "Kristýna", firstname),
      surname = ifelse(surname == "Kristýna Hosáková" & birthdate == ymd("1989-07-20"), "Hosáková", surname),
      firstname = ifelse(surname == "Ladislava Ondráčková" & birthdate == ymd("1976-12-03"), "Ladislava", firstname),
      surname = ifelse(surname == "Ladislava Ondráčková" & birthdate == ymd("1976-12-03"), "Ondráčková", surname),
      surname = ifelse(surname == "LAŽANSKÝ", "Lažanský", surname),
      firstname = ifelse(surname == "Lenka Wagnerová" & birthdate == ymd("1960-11-15"), "Lenka", firstname),
      surname = ifelse(surname == "Lenka Wagnerová" & birthdate == ymd("1960-11-15"), "Wagnerová", surname),
      surname = ifelse(surname == "Macich ml.", "Macich", surname),
      firstname = ifelse(surname == "Martin Bosák" & birthdate == ymd("1991-07-26"), "Martin", firstname),
      surname = ifelse(surname == "Martin Bosák" & birthdate == ymd("1991-07-26"), "Bosák", surname),
      firstname = ifelse(surname == "Miroslav Nevřela" & birthdate == ymd("1965-09-06"), "Miroslav", firstname),
      surname = ifelse(surname == "Miroslav Nevřela" & birthdate == ymd("1965-09-06"), "Nevřela", surname),
      firstname = ifelse(surname == "Ondřej Musil" & birthdate == ymd("1986-10-01"), "Ondřej", firstname),
      surname = ifelse(surname == "Ondřej Musil" & birthdate == ymd("1986-10-01"), "Musil", surname),
      surname = ifelse(surname == "PRAUM", "Praum", surname),
      firstname = ifelse(surname == "Petr Foch" & birthdate == ymd("1946-03-01"), "Petr", firstname),
      surname = ifelse(surname == "Petr Foch" & birthdate == ymd("1946-03-01"), "Foch", surname),
      firstname = ifelse(surname == "Petr Čermák" & birthdate == ymd("1986-03-17"), "Petr", firstname),
      surname = ifelse(surname == "Petr Čermák" & birthdate == ymd("1986-03-17"), "Čermák", surname),
      firstname = ifelse(firstname == "Phillip" & birthdate == ymd("1980-07-05"), "Philip", firstname),
      surname = ifelse(surname == "Pribyl" & birthdate == ymd("1986-06-03"), "Přibyl", surname),
      surname = ifelse(surname == "Priesterová Mgr." & birthdate == ymd("1942-11-24"), "Priesterová", surname),
      surname = ifelse(surname == "Josef Kubíček" & birthdate == ymd("1951-05-22"), "Kubíček", surname),
      firstname = ifelse(surname == "Josef Kubíček" & birthdate == ymd("1951-05-22"), "Josef", firstname),
      firstname = ifelse(surname == "Jakub Ptáček" & birthdate == ymd("1978-10-08"), "Jakub Jan", firstname),
      surname = ifelse(surname == "Jakub Ptáček" & birthdate == ymd("1978-10-08"), "Ptáček", surname),
      firstname = ifelse(surname == "Radek Boček" & birthdate == ymd("1974-03-15"), "Radek", firstname),
      surname = ifelse(surname == "Radek Boček" & birthdate == ymd("1974-03-15"), "Boček", surname),
      surname = ifelse(surname == "Skybova" & birthdate == ymd("1973-07-05"), "Skybová", surname),
      surname = ifelse(surname == "Stanek" & birthdate == ymd("1955-11-04"), "Staněk", surname),
      firstname = ifelse(firstname == "Staněk" & birthdate == ymd("1955-11-04"), "František", firstname),
      surname = ifelse(surname == "Strakova" & birthdate == ymd("1987-08-30"), "Straková", surname),
      surname = ifelse(surname == "Syrovy" & birthdate == ymd("1977-06-29"), "Syrový", surname),
      surname = ifelse(surname == "TLAMKA", "Tlamka", surname),
      surname = ifelse(surname == "Vagunda-Drgáč" & birthdate == ymd("1976-11-12"), "Vagunda", surname),
      firstname = ifelse(surname == "Victoria Priester" & birthdate == ymd("1987-10-06"), "Petra Victoria", firstname),
      surname = ifelse(surname == "Victoria Priester" & birthdate == ymd("1987-10-06"), "Priester", surname),
      firstname = ifelse(surname == "Vítek Zažímal" & birthdate == ymd("1987-06-23"), "Vítek", firstname),
      
      
      surname = ifelse(surname == "Vítek Zažímal" & birthdate == ymd("1987-06-23"), "Zažímal", surname),
      firstname = ifelse(firstname == "ing Vít" & birthdate == ymd("1956-12-03"), "Vít", firstname),
      surname = ifelse(surname == "beňová", "Beňová", surname),
      surname = ifelse(surname == "bihary", "Bihary", surname),
      surname = ifelse(surname == "dušek", "Dušek", surname),
      surname = ifelse(surname == "havel", "Havel", surname),
      surname = ifelse(surname == "ermák" & birthdate == ymd("1986-03-17"), "Čermák", surname),
      surname = ifelse(surname == "jágrová", "Jágrová", surname),
      surname = ifelse(surname == "král", "Král", surname),
      surname = ifelse(surname == "kašparova", "Kašparova", surname),
      surname = ifelse(surname == "kubat", "Kubát", surname),
      surname = ifelse(surname == "kučera", "Kučera", surname),
      surname = ifelse(surname == "med", "Med", surname),
      surname = ifelse(surname == "palounek", "Palounek", surname),
      surname = ifelse(surname == "ptacek", "Ptáček", surname),
      surname = ifelse(surname == "veisová", "Veisová", surname),
      firstname = ifelse(surname == "ta Karešová" & birthdate == ymd("1972-06-06"), "Markéta", firstname),
      surname = ifelse(surname == "ta Karešová" & birthdate == ymd("1972-06-06"), "Karešová", surname),
      firstname = ifelse(surname == "vidrman" & birthdate == ymd("1975-06-20"), "Petr", firstname),
      surname = ifelse(surname == "vidrman" & birthdate == ymd("1975-06-20"), "Vidrman", surname),
      surname = ifelse(surname == "ŠRÁMKOVÁ", "Šrámková", surname),
      surname = ifelse(surname == "BELŽÍK", "Belžík", surname),
      surname = ifelse(surname == "BENÍŠEK", "Beníšek", surname),
      firstname = ifelse(firstname == "Peter" & birthdate == ymd("1970-01-18"), "Petr", firstname),
      surname = ifelse(surname == "Bedron" & birthdate == ymd("1974-05-01"), "Bedroň", surname),
      surname = ifelse(surname == "Benesova" & birthdate == ymd("1974-01-26"), "Benešová", surname),
      surname = ifelse(surname == "Bernátek" & birthdate == ymd("1997-08-22"), "Bernátek ml.", surname),
      surname = ifelse(surname == "Bečvar" & birthdate == ymd("2022-01-03"), "Bečvář", surname),
      surname = ifelse(surname == "Blažková Šípková" & birthdate == ymd("1955-12-12"), "Blažková-Šípková", surname),
      surname = ifelse(surname == "Brančík ml.", "Brančík", surname),
      surname = ifelse(surname == "Brančík st.", "Brančík", surname),
      surname = ifelse(surname == "Brávek, MVDr.", "Brávek", surname),
      surname = ifelse(surname == "BÉBAR", "Bébar", surname),
      surname = ifelse(surname == "C. Novák", "Novák", surname),
      surname = ifelse(surname == "CVACHOVEC", "Cvachovec", surname),
      surname = ifelse(surname == "GABAJ", "Gabaj", surname),
      surname = ifelse(surname == "GOTTWALDOVÁ", "Gottwaldová", surname),
      firstname = ifelse(firstname == "Miloslav" & birthdate == ymd("1952-05-23") & surname == "Gajdůšek", "Miroslav", firstname),
      firstname = ifelse(firstname == "MudrMiloš" & birthdate == ymd("2022-01-16"), "Miloš", firstname),
      surname = ifelse(surname == "Gundackerova" & birthdate == ymd("1977-01-22"), "Gundackerová", surname),
      surname = ifelse(surname == "GÖNDÖR", "Göndör", surname),
      surname = ifelse(surname == "HUMPLÍK", "Humplík", surname),
      surname = ifelse(surname == "Hlinovsky" & birthdate == ymd("1950-09-17"), "Hlinovský", surname),
      surname = ifelse(surname == "Hlinovsky" & birthdate == ymd("2000-11-17"), "Hlinovský", surname),
      
      firstname = ifelse(firstname == "E" & birthdate == ymd("2000-11-17"), "Evžen", firstname),
      surname = ifelse(surname == "Hrnčár" & birthdate == ymd("1979-09-12"), "Hrnčář", surname),
      firstname = ifelse(firstname == "Andrea Hrychová" & birthdate == ymd("1973-12-09"), "Andrea", firstname),
      firstname = ifelse(firstname == "Bc Simona" & birthdate == ymd("1972-01-26"), "Simona", firstname),
      firstname = ifelse(firstname == "Elana" & birthdate == ymd("1984-04-23"), "Elena", firstname),
      surname = ifelse(surname == "Hábrová" & birthdate == ymd("1975-07-15"), "Hábová", surname),
      surname = ifelse(birthdate == ymd("1954-10-26") & (surname == "Hädler" | surname == "Händlerová"), "Hädler Stirská", surname),
      surname = ifelse((surname == "JUDr. Šustek" & birthdate == ymd("1965-11-14")) | birthdate == ymd("1996-05-05"), "Šustek", surname),
      firstname = ifelse(surname == "Jan Fiala", "Jan", firstname),
      surname = ifelse(surname == "Jan Fiala", "Fiala", surname),
      surname = ifelse(surname == "Jan Grác", "Grác", surname),
      surname = ifelse(surname == "Jan Šnek", "Šnek", surname),
      surname = ifelse(surname == "Josef Strnadel", "Strnadel", surname),
      surname = ifelse(surname == "Josefína Fulková", "Fulková", surname),
      surname = ifelse(surname == "KARLÍK", "Karlík", surname),
      surname = ifelse(surname == "KOŽELUH", "Koželuh", surname),
      surname = ifelse(surname == "Konšel Ml." & birthdate == ymd("1986-07-25"), "Konšel", surname),
      surname = ifelse(surname == "Kozák Mgr. Bc.", "Kozák", surname),
      surname = ifelse(surname == "Kuchař Mgr", "Kuchař", surname),
      surname = ifelse(surname == "KŘÍŽEK", "Křížek", surname),
      surname = ifelse(surname == "LANZ", "Lanz", surname),
      surname = ifelse(surname == "LOCHNER", "Lochner", surname),
      surname = ifelse(surname == "Lecjak" & birthdate == ymd("1966-04-22"), "Lecjaks", surname),
      birthdate = ifelse(birthdate == ymd("1963-01-11") & surname == "Liška", format(as.Date(ymd("1963-10-11"))), format(as.Date(birthdate),'%Y-%m-%d')),
      surname = ifelse(surname == "Lubenik" & (birthdate == ymd("1963-07-29") | birthdate == ymd("1975-09-12")), "Luběník", surname),
      surname = ifelse(surname == "MALOVEC", "Malovec", surname),
      surname = ifelse(surname == "MELŠ", "Melš", surname),
      surname = ifelse(surname == "Majerová" & birthdate == ymd("1972-06-28"), "Majerová Zahradníková", surname),
      firstname = ifelse(surname == "Maria Brodinová" & birthdate == ymd("1982-06-03"), "Marian Anna", firstname),
      surname = ifelse(surname == "Maria Brodinová" & birthdate == ymd("1982-06-03"), "Brodinová", surname),
      firstname = ifelse(surname == "Maria Miklas" & birthdate == ymd("1963-11-18"), "Thomas Maria", firstname),
      surname = ifelse(surname == "Maria Miklas" & birthdate == ymd("1963-11-18"), "Miklas", surname),
      firstname = ifelse(surname == "Marie Černá" & birthdate == ymd("1966-12-08"), "Marie Dana", firstname),
      surname = ifelse(surname == "Marie Černá" & birthdate == ymd("1966-12-08"), "Černá", surname),
      surname = ifelse(surname == "MÍŠEK", "Míšek", surname),
      surname = ifelse(surname == "PEVNÝ", "Pevný", surname),
      surname = ifelse(surname == "PIKART", "Pikart", surname),
      surname = ifelse(surname == "Pekárek Ing.", "Pekárek", surname),
      firstname = ifelse(firstname == "Bc Radomír", "Radomír", firstname),
      firstname = ifelse(firstname == "Jaroslav Prošek", "Jaroslav", firstname),
      surname = ifelse(surname == "REINBERGOVÁ", "Reinbergová", surname),
      surname = ifelse(surname == "Ridzon", "Ridzoň", surname),
      surname = ifelse(surname == "Risko", "Riško", surname),
      firstname = ifelse(firstname == "Miroslav" & birthdate == ymd("1969-12-07") & surname == "Robotka", "Miloslav", firstname),
      surname = ifelse(surname == "SCHWANZER", "Schwanzer", surname),
      
      surname = ifelse(surname == "SPÁČIL - ELBA", "Spáčil-Elba", surname),
      surname = ifelse(surname == "Spudichova", "Spudichová", surname),
      surname = ifelse(surname == "Suralova", "Suralová", surname),
      surname = ifelse(surname == "TESAŘÍK", "Tesařík", surname),
      surname = ifelse(surname == "Troska" & birthdate == ymd("1968-09-26"), "Troška", surname),
      surname = ifelse(surname == "Tykalová" & birthdate == ymd("1950-08-20"), "Tykadlová", surname),
      surname = ifelse(surname == "Tůma, ing.", "Tůma", surname),
      surname = ifelse(surname == "Tůmová, dr.", "Tůmová", surname),
      surname = ifelse(surname == "VAJDÍK", "Vajdík", surname),
      surname = ifelse(surname == "VAŠEK", "Vašek", surname),
      surname = ifelse(surname == "VOJTEKOVÁ", "Vojteková", surname),
      surname = ifelse(surname == "VYCPÁLEK", "Vycpálek", surname),
      surname = ifelse(surname == "Winker", "Winkler", surname),
      surname = ifelse(surname == "Witosz", "Witozsz", surname),
      surname = ifelse(surname == "ZEJDOVÁ", "Zejdová", surname),
      surname = ifelse(surname == "Zamorsky", "Zamorský", surname),
      firstname = ifelse(surname == "Zykán" & (firstname == "Albert" | firstname == "František-A"), "František Albert", firstname),
      surname = ifelse(surname == "bohbot", "Bohbot", surname),
      surname = ifelse(surname == "brožek", "Brožek", surname),
      surname = ifelse(surname == "fraitová", "Fraitová", surname),
      surname = ifelse(surname == "jůna", "Jůna", surname),
      surname = ifelse(surname == "kalach", "Kalach", surname),
      surname = ifelse(surname == "klouček", "Klouček", surname),
      surname = ifelse(surname == "kocur", "Kocur", surname),
      surname = ifelse(surname == "kočvarová", "Kočvarová", surname),
      surname = ifelse(surname == "kučera", "Kučera", surname),
      surname = ifelse(surname == "ŠEMBERA", "Šembera", surname),
      surname = ifelse(surname == "ŠKULIGA", "Škuliga", surname),
      surname = ifelse(surname == "ŠRÁMEK", "Šrámek", surname),
      surname = ifelse(surname == "Šarkoziova", "Šarköziová", surname),
      surname = ifelse(surname == "švidran", "Švidran", surname)
    )
  
  
  
  data3 <- data %>%
    mutate(
      birthdate = ifelse(birthdate == ymd("1972-06-01") & surname == "Majringer", format(as.Date(ymd("1972-06-01"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1962-09-19") & surname == "Lejsková", format(as.Date(ymd("1963-09-19"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1955-11-11") & surname == "Landa", format(as.Date(ymd("1955-11-10"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1962-02-15") & surname == "Lameš", format(as.Date(ymd("1962-01-15"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2022-02-26") & surname == "Křížek", format(as.Date(ymd("1975-02-26"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1975-08-23") & surname == "Kyselý", format(as.Date(ymd("1973-08-23"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1979-12-07") & surname == "Kučera", format(as.Date(ymd("1977-12-09"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2022-07-05") & surname == "Kufrová", format(as.Date(ymd("1948-07-05"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2020-04-07") & surname == "Kubíček", format(as.Date(ymd("1951-05-22"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2022-02-17") & surname == "Kubský", format(as.Date(ymd("1953-02-17"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1961-04-08") & surname == "Kubiena", format(as.Date(ymd("1961-08-04"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2022-01-24") & surname == "Krása", format(as.Date(ymd("1951-11-24"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2022-09-16") & surname == "Kronus", format(as.Date(ymd("1963-11-16"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1979-10-22") & surname == "Krause", format(as.Date(ymd("1979-10-23"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2022-01-01") & surname == "Kouřil", format(as.Date(ymd("1972-01-01"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2022-01-30") & surname == "Konšel", format(as.Date(ymd("1966-01-30"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2020-04-07") & surname == "Kolaříková", format(as.Date(ymd("1975-06-24"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1949-07-06") & surname == "Klejma", format(as.Date(ymd("1949-07-08"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1964-10-25") & surname == "John", format(as.Date(ymd("1964-10-26"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1952-05-06") & surname == "Jirotka", format(as.Date(ymd("1952-06-05"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1971-10-14") & surname == "Jelínek", format(as.Date(ymd("1970-10-14"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1964-05-07") & surname == "Janů", format(as.Date(ymd("1964-10-07"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2022-01-20") & surname == "Janderka", format(as.Date(ymd("1949-12-20"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2023-03-08") & surname == "Hájek", format(as.Date(ymd("1963-02-08"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2022-04-17") & surname == "Jalovec", format(as.Date(ymd("1948-01-27"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1967-06-12") & surname == "Jech", format(as.Date(ymd("1967-05-12"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1978-02-23") & surname == "Babáčková", format(as.Date(ymd("1978-02-28"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1948-05-01") & surname == "Balcarová", format(as.Date(ymd("1948-05-01"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1965-12-10") & surname == "Bačák", format(as.Date(ymd("1975-12-10"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1965-12-11") & surname == "Denk", format(as.Date(ymd("1966-12-11"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1975-06-19") & surname == "Dolanský", format(as.Date(ymd("1975-06-13"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1961-03-23") & surname == "Drbohlav", format(as.Date(ymd("1961-03-20"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1969-07-12") & surname == "Dvořák", format(as.Date(ymd("1969-06-12"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1956-08-26") & surname == "Bradáč", format(as.Date(ymd("1956-08-28"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1971-11-14") & surname == "Kladivová", format(as.Date(ymd("1972-11-14"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1967-09-05") & surname == "Felner", format(as.Date(ymd("1967-04-05"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1937-07-16") & surname == "Filák", format(as.Date(ymd("1937-03-16"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1962-12-02") & surname == "Flesar", format(as.Date(ymd("1961-12-02"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1967-08-10") & surname == "Fojt", format(as.Date(ymd("1968-08-10"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1955-05-17") & surname == "Forišková", format(as.Date(ymd("1956-05-17"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1967-07-26") & surname == "František", format(as.Date(ymd("1969-07-26"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1967-11-17") & surname == "Gogelová", format(as.Date(ymd("1967-12-17"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1984-04-01") & surname == "Grolich", format(as.Date(ymd("1984-06-11"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1988-08-29") & surname == "Hainc", format(as.Date(ymd("1988-07-29"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1960-08-04") & surname == "Kodrík", format(as.Date(ymd("1960-08-01"))), format(as.Date(birthdate),'%Y-%m-%d')),
      
      birthdate = ifelse(birthdate == ymd("1956-05-13") & surname == "Kollová", format(as.Date(ymd("1956-05-17"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1976-12-02") & surname == "Kozubík", format(as.Date(ymd("1966-12-02"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1966-04-14") & surname == "Krajíček", format(as.Date(ymd("1966-04-12"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1974-06-12") & surname == "Král", format(as.Date(ymd("1984-06-12"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1957-03-23") & surname == "Kuběna", format(as.Date(ymd("1957-03-21"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1974-06-05") & surname == "Kvitová", format(as.Date(ymd("1972-06-05"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1969-07-01") & surname == "Mikošková", format(as.Date(ymd("1969-07-02"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1982-10-21") & surname == "Netolický", format(as.Date(ymd("1982-09-21"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1976-11-21") & surname == "Novotný", format(as.Date(ymd("1978-11-21"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1956-02-01") & surname == "Pagáčova", format(as.Date(ymd("1959-02-02"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1950-11-02") & surname == "Poláčková", format(as.Date(ymd("1951-11-02"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1981-03-01") & surname == "Herot", format(as.Date(ymd("1982-03-01"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1951-05-04") & surname == "Hlisnikovský", format(as.Date(ymd("1951-05-14"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1983-06-18") & surname == "Horut", format(as.Date(ymd("1984-06-18"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1984-06-24") & surname == "Horák", format(as.Date(ymd("1984-07-24"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1977-03-04") & surname == "Hromek", format(as.Date(ymd("1977-03-17"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1966-08-09") & surname == "Hrubý", format(as.Date(ymd("1968-08-09"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1960-05-15") & surname == "Jastrzembská", format(as.Date(ymd("1960-05-14"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1985-12-27") & surname == "Jůnová", format(as.Date(ymd("1989-12-27"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1970-01-07") & surname == "Stejskal", format(as.Date(ymd("1970-10-07"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1952-08-27") & surname == "Trávníček", format(as.Date(ymd("1952-08-17"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1973-12-29") & surname == "Tučková", format(as.Date(ymd("1972-12-29"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1958-02-10") & surname == "Tvarůžek", format(as.Date(ymd("1958-02-05"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1972-08-12") & surname == "Tylš", format(as.Date(ymd("1972-08-21"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1949-12-05") & surname == "Urbánek", format(as.Date(ymd("1949-11-05"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1988-08-21") & surname == "Vodáková", format(as.Date(ymd("1998-08-21"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1971-05-23") & surname == "Váša", format(as.Date(ymd("1971-06-23"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1964-03-21") & surname == "Změlíková", format(as.Date(ymd("1963-03-21"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1968-11-28") & surname == "Černín", format(as.Date(ymd("1986-11-28"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1958-01-21") & surname == "Šlahúnková", format(as.Date(ymd("1958-02-21"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1964-07-05") & surname == "Chval", format(as.Date(ymd("1961-07-05"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1964-05-19") & surname == "Říha", format(as.Date(ymd("1964-05-10"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1980-08-01") & surname == "Šmídek", format(as.Date(ymd("1980-08-10"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1961-11-19") & surname == "Štefek", format(as.Date(ymd("1961-11-17"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1957-05-26") & surname == "Štěpánek", format(as.Date(ymd("1957-03-26"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1952-06-26") & surname == "Švecová", format(as.Date(ymd("1952-06-29"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1950-01-06") & surname == "Halíková", format(as.Date(ymd("1950-06-01"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1978-06-30") & surname == "Janda", format(as.Date(ymd("1978-09-30"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1954-03-11") & surname == "Jirousová", format(as.Date(ymd("1954-02-11"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1981-09-01") & surname == "Kotala", format(as.Date(ymd("1981-01-09"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1955-02-07") & surname == "Kováčik", format(as.Date(ymd("1955-07-02"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1967-11-03") & surname == "Mackovík", format(as.Date(ymd("1967-03-11"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1953-11-09") & surname == "Mandát", format(as.Date(ymd("1953-11-19"))), format(as.Date(birthdate),'%Y-%m-%d')),
      
      birthdate = ifelse(birthdate == ymd("1947-10-18") & surname == "Muchová", format(as.Date(ymd("1947-09-18"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1947-08-02") & surname == "Pospíšil", format(as.Date(ymd("1947-02-08"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1964-01-01") & surname == "Sekeráková", format(as.Date(ymd("1964-11-01"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1964-12-16") & surname == "Vrzal", format(as.Date(ymd("1964-12-06"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1960-08-10") & surname == "Hlaváček", format(as.Date(ymd("1960-08-19"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1981-04-19") & surname == "Kinčl", format(as.Date(ymd("1981-04-10"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1989-09-29") & surname == "Pleskač", format(as.Date(ymd("1989-09-26"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1975-07-11") & surname == "Ruso", format(as.Date(ymd("1965-07-11"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1966-09-07") & surname == "Sovová", format(as.Date(ymd("1966-09-01"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1997-10-22") & surname == "Adamec", format(as.Date(ymd("1977-10-22"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1979-08-07") & surname == "Binka", format(as.Date(ymd("1979-07-08"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1977-01-05") & surname == "Brůžek", format(as.Date(ymd("1977-07-08"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1997-05-16") & surname == "Bíbr", format(as.Date(ymd("1977-05-16"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2020-04-22") & surname == "Bohuňková", format(as.Date(ymd("1978-04-22"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1989-07-04") & surname == "Chaloupka", format(as.Date(ymd("1983-07-04"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1989-01-28") & surname == "Florián", format(as.Date(ymd("1989-01-29"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1976-03-07") & surname == "Galata", format(as.Date(ymd("1976-07-03"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1958-07-28") & surname == "Holubová", format(as.Date(ymd("1959-07-28"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1990-06-25") & surname == "Housa", format(as.Date(ymd("1990-06-26"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1900-01-01") & surname == "Hájek", format(as.Date(ymd("1983-09-30"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2023-03-01") & surname == "Janata", format(as.Date(ymd("1954-01-22"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1989-04-13") & surname == "Janek", format(as.Date(ymd("1989-04-13"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1988-10-08") & surname == "Janovský", format(as.Date(ymd("1988-09-05"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1987-09-04") & surname == "Jelínek", format(as.Date(ymd("1987-04-09"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1995-12-15") & surname == "Jokeš", format(as.Date(ymd("1989-12-15"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1988-05-08") & surname == "Karasaridis", format(as.Date(ymd("1991-05-08"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1994-12-24") & surname == "Kindl", format(as.Date(ymd("1994-11-24"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1989-02-14") & surname == "Kluka", format(as.Date(ymd("1972-02-14"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1968-05-26") & surname == "Koumarová", format(as.Date(ymd("1968-08-26"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1960-03-24") & surname == "Kožich", format(as.Date(ymd("1960-03-31"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1988-08-04") & surname == "Lochmanová", format(as.Date(ymd("1989-08-04"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1964-03-12") & surname == "Mendl", format(as.Date(ymd("1964-03-06"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1955-07-06") & surname == "Mrázek", format(as.Date(ymd("1955-06-07"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1962-06-20") & surname == "Naiclerová", format(as.Date(ymd("1972-06-19"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1984-07-12") & surname == "Nešpor", format(as.Date(ymd("1984-01-01"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1994-09-06") & surname == "Noha", format(as.Date(ymd("1994-06-09"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1986-05-23") & surname == "Novotná", format(as.Date(ymd("1986-05-28"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1972-10-05") & surname == "Novák", format(as.Date(ymd("1972-05-10"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1971-06-15") & surname == "Otto", format(as.Date(ymd("1971-06-16"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1984-04-17") & surname == "Pařízek", format(as.Date(ymd("1984-05-18"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1983-06-26") & surname == "Paška", format(as.Date(ymd("1983-06-29"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1952-02-29") & surname == "Petr", format(as.Date(ymd("1952-02-28"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1987-03-03") & surname == "Pešán", format(as.Date(ymd("1987-02-03"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1995-10-28") & surname == "Podroužek", format(as.Date(ymd("1991-10-28"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1984-08-17") & surname == "Pravda", format(as.Date(ymd("1969-08-17"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1970-09-03") & surname == "Rendl", format(as.Date(ymd("1970-03-09"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1989-08-21") & surname == "Rozsypal", format(as.Date(ymd("1982-08-21"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1966-05-08") & surname == "Rusóová", format(as.Date(ymd("1977-05-08"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2020-05-28") & surname == "Sadílek", format(as.Date(ymd("1983-05-28"))), format(as.Date(birthdate),'%Y-%m-%d')),
      
      birthdate = ifelse(birthdate == ymd("1989-09-08") & surname == "Simkanič", format(as.Date(ymd("1989-04-08"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1983-09-05") & surname == "Suchanek", format(as.Date(ymd("1984-09-05"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1991-10-15") & surname == "Svrček", format(as.Date(ymd("1991-10-25"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2022-09-29") & surname == "Sáňková", format(as.Date(ymd("1978-09-29"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1971-03-18") & surname == "Síkora", format(as.Date(ymd("1983-03-18"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1991-05-06") & surname == "Sýkora", format(as.Date(ymd("1991-06-05"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1972-09-03") & surname == "Tomis", format(as.Date(ymd("1972-03-09"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1984-05-08") & surname == "Toušek", format(as.Date(ymd("1984-08-05"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1978-09-07") & surname == "Třešňáková", format(as.Date(ymd("1978-07-09"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1995-11-28") & surname == "Urbánek", format(as.Date(ymd("1985-11-28"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1994-04-19") & surname == "Vidláková", format(as.Date(ymd("1998-04-19"))), format(as.Date(birthdate),'%Y-%m-%d')),
      
      birthdate = ifelse(birthdate == ymd("1987-05-25") & surname == "Vrba", format(as.Date(ymd("1987-05-24"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1983-07-26") & surname == "Zach", format(as.Date(ymd("1983-01-26"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1962-06-30") & surname == "Zemánek", format(as.Date(ymd("1962-06-03"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1991-09-19") & surname == "Zámečník", format(as.Date(ymd("1982-09-19"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1976-02-29") & surname == "Čermáková", format(as.Date(ymd("1976-02-28"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1988-02-14") & surname == "Čáha", format(as.Date(ymd("1989-02-14"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1962-01-04") & surname == "Šemík", format(as.Date(ymd("1962-01-14"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1988-11-03") & surname == "Žák", format(as.Date(ymd("1989-11-03"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1973-06-25") & surname == "Bakošová", format(as.Date(ymd("1976-06-25"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1962-01-25") & surname == "Bojko", format(as.Date(ymd("1962-12-25"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1995-03-29") & surname == "Impseil", format(as.Date(ymd("1995-03-19"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1987-01-21") & surname == "Kachlík", format(as.Date(ymd("1972-01-21"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1948-08-28") & surname == "Moudrý", format(as.Date(ymd("1948-08-29"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1983-09-04") & surname == "Plevková", format(as.Date(ymd("1983-09-03"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1956-03-02") & surname == "Severová", format(as.Date(ymd("1956-03-01"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1957-12-01") & surname == "Wawrzacz", format(as.Date(ymd("1957-01-12"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1960-05-01") & surname == "Čech", format(as.Date(ymd("1960-05-11"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1961-10-15") & surname == "Čechová", format(as.Date(ymd("1961-10-25"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1963-07-10") & surname == "Štefcová", format(as.Date(ymd("1963-10-07"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1967-04-07") & surname == "Švancarová", format(as.Date(ymd("1967-04-07"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1979-02-03") & surname == "Bouška", format(as.Date(ymd("1979-02-23"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1945-10-01") & surname == "Cetkovský", format(as.Date(ymd("1945-10-02"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1966-09-18") & surname == "Davídek", format(as.Date(ymd("1966-09-16"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1976-05-20") & surname == "Dohnal", format(as.Date(ymd("1976-05-02"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1985-02-05") & surname == "Drbohlav", format(as.Date(ymd("1985-02-06"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1988-03-13") & surname == "Flek", format(as.Date(ymd("1988-03-12"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1969-06-07") & surname == "Hejma", format(as.Date(ymd("1969-07-06"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1954-03-29") & surname == "Horník", format(as.Date(ymd("1954-03-19"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2020-09-14") & surname == "Horák", format(as.Date(ymd("1968-09-14"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1961-02-02") & surname == "Hrabě", format(as.Date(ymd("1969-02-02"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1980-01-11") & surname == "Jirovský", format(as.Date(ymd("1980-10-01"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2020-05-12") & surname == "Karásková", format(as.Date(ymd("1977-05-12"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1971-10-25") & surname == "Kuklová", format(as.Date(ymd("1971-01-25"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1962-11-07") & surname == "Marešová", format(as.Date(ymd("1962-11-17"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1976-01-27") & surname == "Bílová", format(as.Date(ymd("1976-01-28"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1966-04-29") & surname == "Müller", format(as.Date(ymd("1966-04-28"))), format(as.Date(birthdate),'%Y-%m-%d')),
      
      birthdate = ifelse(birthdate == ymd("1972-08-02") & surname == "Quittová", format(as.Date(ymd("1972-09-02"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1977-11-15") & surname == "Vávra", format(as.Date(ymd("1974-11-15"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1979-01-20") & surname == "Čejka", format(as.Date(ymd("1979-01-22"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1986-10-29") & surname == "Šperl", format(as.Date(ymd("1986-10-20"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1976-09-19") & surname == "Švancara", format(as.Date(ymd("1976-09-21"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1983-08-23") & surname == "Uzel", format(as.Date(ymd("1983-08-24"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1954-04-30") & surname == "Chvojka", format(as.Date(ymd("1965-04-30"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1977-07-21") & surname == "Janíček", format(as.Date(ymd("1973-07-22"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1955-04-30") & surname == "Nedvědický", format(as.Date(ymd("1955-05-30"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1971-03-17") & surname == "Med", format(as.Date(ymd("1971-03-09"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1971-07-20") & surname == "Pavelek", format(as.Date(ymd("1971-07-28"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1954-04-03") & surname == "Procházková", format(as.Date(ymd("1954-04-05"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1970-03-04") & surname == "Tregner", format(as.Date(ymd("1970-03-03"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2020-09-22") & surname == "Netolický", format(as.Date(ymd("1982-09-22"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1988-08-25") & surname == "Polanský", format(as.Date(ymd("1988-08-28"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2021-01-17") & surname == "Eliáš", format(as.Date(ymd("1984-01-17"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1947-04-04") & surname == "Košek", format(as.Date(ymd("1947-06-04"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1950-09-29") & surname == "Snopek", format(as.Date(ymd("1970-09-29"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1975-06-12") & surname == "Stránský", format(as.Date(ymd("1962-06-12"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1977-07-22") & surname == "Šlachta", format(as.Date(ymd("1971-07-22"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1970-10-28") & surname == "Bravenec", format(as.Date(ymd("1970-07-28"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2001-10-16") & surname == "Knapovský", format(as.Date(ymd("2001-10-26"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1981-08-18") & surname == "Kufa", format(as.Date(ymd("1981-08-12"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1948-10-11") & surname == "Otevřel", format(as.Date(ymd("1948-01-11"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1980-07-23") & surname == "Pelán", format(as.Date(ymd("1980-08-23"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2023-09-26") & surname == "Techlovský", format(as.Date(ymd("1972-03-26"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1970-09-27") & surname == "Urban", format(as.Date(ymd("1970-07-01"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1993-03-11") & surname == "Vorba", format(as.Date(ymd("1992-03-11"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1971-12-14") & surname == "Špitálský", format(as.Date(ymd("1970-12-11"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1957-01-21") & surname == "Andreska", format(as.Date(ymd("1957-01-20"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2022-01-27") & surname == "Bartošík", format(as.Date(ymd("1965-01-27"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2022-02-17") & surname == "Bednářová", format(as.Date(ymd("1967-02-17"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2022-07-12") & surname == "Bedrnová", format(as.Date(ymd("1969-06-13"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2022-03-17") & surname == "Bečán", format(as.Date(ymd("1965-01-01"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2022-01-03") & surname == "Bečvář", format(as.Date(ymd("1973-05-04"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2000-12-12") & surname == "Blažková-Šípková", format(as.Date(ymd("1955-12-12"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2000-07-05") & surname == "Bárta", format(as.Date(ymd("1944-07-05"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2023-12-11") & surname == "Cholasta", format(as.Date(ymd("1951-12-16"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1965-12-11") & surname == "Císař", format(as.Date(ymd("1965-01-27"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2023-12-11") & surname == "Dobrovolná", format(as.Date(ymd("1959-10-17"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2022-11-19") & surname == "Doležal", format(as.Date(ymd("2000-11-17"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1966-07-07") & surname == "Drbohlav", format(as.Date(ymd("1966-06-07"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2022-04-15") & surname == "Dupalová", format(as.Date(ymd("1928-08-06"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1985-07-05") & surname == "Endrle", format(as.Date(ymd("1985-05-07"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2022-05-07") & surname == "Fendrych", format(as.Date(ymd("1971-05-07"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1994-12-03") & surname == "Fraitová", format(as.Date(ymd("1944-12-03"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2022-07-07") & surname == "Fuchs", format(as.Date(ymd("1949-07-14"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2022-05-29") & surname == "Funioková", format(as.Date(ymd("1957-08-05"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2022-05-29") & surname == "Funioková", format(as.Date(ymd("1957-08-05"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2022-01-16") & surname == "Golian", format(as.Date(ymd("1958-06-16"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1948-04-30") & surname == "Hanuš", format(as.Date(ymd("1949-04-30"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2022-09-26") & surname == "Holub", format(as.Date(ymd("1968-09-26"))), format(as.Date(birthdate),'%Y-%m-%d')),
      
      birthdate = ifelse(birthdate == ymd("2022-04-10") & surname == "Novosád", format(as.Date(ymd("1950-04-10"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2023-07-31") & surname == "Neuman", format(as.Date(ymd("1953-12-28"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1969-05-18") & surname == "Nejedlý", format(as.Date(ymd("1969-01-18"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1978-04-13") & surname == "Míkovcová Mališová", format(as.Date(ymd("1976-09-01"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2022-11-20") & surname == "Musílek", format(as.Date(ymd("1958-09-25"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2022-03-18") & surname == "Mišáková", format(as.Date(ymd("1957-03-18"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1960-01-20") & surname == "Mareš", format(as.Date(ymd("1960-01-22"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2022-09-13") & surname == "Mareš", format(as.Date(ymd("1948-09-13"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2020-04-07") & surname == "Makl", format(as.Date(ymd("1974-07-22"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2000-11-17") & surname == "Malý", format(as.Date(ymd("1956-03-20"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2023-07-31") & surname == "Švarc", format(as.Date(ymd("1944-05-11"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2022-01-31") & surname == "Šustalová", format(as.Date(ymd("1974-01-31"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2022-01-02") & surname == "Štyrand", format(as.Date(ymd("1958-01-02"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2023-07-31") & surname == "Špulák", format(as.Date(ymd("1978-05-19"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1979-03-11") & surname == "Špacír", format(as.Date(ymd("1979-08-11"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1977-02-05") & surname == "Šimek", format(as.Date(ymd("1974-02-05"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2022-01-17") & surname == "Čada", format(as.Date(ymd("1965-12-21"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2022-07-02") & surname == "Zach", format(as.Date(ymd("1958-07-02"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2021-05-03") & surname == "Vraná", format(as.Date(ymd("1960-05-03"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2022-01-12") & surname == "Vorel", format(as.Date(ymd("1956-07-12"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1966-10-26") & surname == "Vepřek", format(as.Date(ymd("1966-10-29"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1978-06-28") & surname == "Vašíček", format(as.Date(ymd("1978-06-29"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2022-05-06") & surname == "Vandasová", format(as.Date(ymd("1963-08-06"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1959-03-31") & surname == "Vajchr", format(as.Date(ymd("1960-03-31"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1973-02-15") & surname == "Uhlíř", format(as.Date(ymd("1973-12-15"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2023-12-11") & surname == "Trnková", format(as.Date(ymd("1968-11-28"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1959-12-01") & surname == "Soukup", format(as.Date(ymd("1955-12-07"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2023-12-11") & surname == "Smékal", format(as.Date(ymd("1976-09-29"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2022-03-17") & surname == "Sláma", format(as.Date(ymd("1976-10-17"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1967-01-07") & surname == "Sekera", format(as.Date(ymd("1967-12-07"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1973-02-11") & surname == "Sedlmajer", format(as.Date(ymd("1972-02-11"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("1969-06-25") & surname == "Reiser", format(as.Date(ymd("1959-06-25"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2023-12-11") & surname == "Pšenička", format(as.Date(ymd("1955-01-29"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse(birthdate == ymd("2023-06-08") & surname == "Pilich", format(as.Date(ymd("1947-06-08"))), format(as.Date(birthdate),'%Y-%m-%d')),
      
      birthdate = ifelse((birthdate == ymd("1975-05-27") | birthdate == ymd("2022-02-27")) & surname == "Žáčková", format(as.Date(ymd("1975-02-27"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse((birthdate == ymd("1957-01-26") | birthdate == ymd("1957-03-26")) & surname == "Mužík", format(as.Date(ymd("1947-03-26"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse((birthdate == ymd("2022-02-14") | birthdate == ymd("2022-05-14")) & surname == "Neliba", format(as.Date(ymd("1969-02-14"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse((birthdate == ymd("2000-12-19") | birthdate == ymd("2021-12-19")) & surname == "Šoral", format(as.Date(ymd("1955-12-19"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse((birthdate == ymd("2022-01-07") | birthdate == ymd("2023-05-21")) & surname == "Zdobinský", format(as.Date(ymd("1993-01-07"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse((birthdate == ymd("2023-06-09") | birthdate == ymd("2023-11-17")) & surname == "Suralová", format(as.Date(ymd("1957-11-17"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse((birthdate == ymd("2022-02-19") | birthdate == ymd("2002-02-19")) & surname == "Spurný", format(as.Date(ymd("1975-02-19"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse((birthdate == ymd("2023-07-31") | birthdate == ymd("2022-04-20")) & surname == "Riedel", format(as.Date(ymd("1961-04-20"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse((birthdate == ymd("2022-03-15") | birthdate == ymd("2022-11-24")) & surname == "Frelová", format(as.Date(ymd("1975-03-15"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse((birthdate == ymd("1948-10-06") | birthdate == ymd("1948-10-02")) & surname == "Všetečka", format(as.Date(ymd("1948-10-21"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse((birthdate == ymd("1983-08-04") | birthdate == ymd("1986-07-04")) & surname == "Havran", format(as.Date(ymd("1986-08-04"))), format(as.Date(birthdate),'%Y-%m-%d')),
      birthdate = ifelse((birthdate == ymd("1989-07-29") | birthdate == ymd("1993-07-29")) & surname == "Pešková", format(as.Date(ymd("1973-07-29"))), format(as.Date(birthdate),'%Y-%m-%d'))
    )
 
  
  
  if (d=="financial"){
    
    
    
    data <- data %>%
      group_by(polparty, surname, firstname, birthdate, year) %>%
      summarise(
        financial_donation = sum(financial_donation, na.rm = TRUE),
        acadegree_an_first = first(acadegree_an),
        acadegree_bn_first = first(acadegree_bn)
      )   
    
  } else{
    
    data <- data %>%
      group_by(polparty, surname, firstname, birthdate, year) %>%
      summarise(
        nonfinancial_donation = sum(nonfinancial_donation, na.rm = TRUE),
        acadegree_an_first = first(acadegree_an),
        acadegree_bn_first = first(acadegree_bn)
      )   
    
  }
  
  
  
  # Save the cleaned data to a new .dta file
  saveRDS(data, paste0("intermediate_data/donation_data_", d, "_clean.rds"))
}

#######################
# Load the data
data_financial <- readRDS("intermediate_data/donation_data_financial_clean.rds")
data_nonfinancial <- readRDS("intermediate_data/donation_data_nonfinancial_clean.rds")

# Filter and save the data where donation_sum > 0
financial_donation_nonzero <- data_financial %>% filter(financial_donation > 0)

# Filter only 0 donations
financial_data <- data_financial %>% filter(financial_donation == 0)
# Merge the datasets
merged_data <- merge(financial_data, data_nonfinancial, by = c("polparty", "surname", "firstname", "birthdate", "year"))

# Append the previously saved data
final_data <- bind_rows(merged_data, financial_donation_nonzero)

# Replace missing values with 0
final_data <- final_data %>%
  mutate(financial_donation = ifelse(is.na(financial_donation), 0, financial_donation),
         nonfinancial_donation = ifelse(is.na(nonfinancial_donation), 0, nonfinancial_donation))



# Sort the data
final_data <- final_data %>%
  arrange(polparty, surname, firstname, birthdate, year)
final_data = subset(final_data, select = c(polparty,surname,firstname,birthdate,year,financial_donation,nonfinancial_donation))


# Generate the total donation
final_data <- final_data %>%
  mutate(donation_all = financial_donation + nonfinancial_donation)

# Collapse the data
collapsed_data <- final_data %>%
  group_by(polparty, surname, firstname, birthdate, year) %>%
  summarise(donation_all = sum(donation_all),
            financial_donation = sum(financial_donation),
            nonfinancial_donation = sum(nonfinancial_donation))

# Generate birthyear
collapsed_data <- collapsed_data %>%
  mutate(birthyear = year(birthdate))


# Generate a unique ID
collapsed_data <- collapsed_data %>%
  group_by(polparty, surname, firstname, birthdate) %>%
  mutate(ID = cur_group_id())


# Save the final data
saveRDS(collapsed_data, "data/data_donation_oofppm.rds")
