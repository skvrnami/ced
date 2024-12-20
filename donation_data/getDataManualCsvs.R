rm(list=ls())
# Set working directory
setwd("/Users/vtitl/Documents/GitHub/ced/donation_data/")

# Load necessary libraries
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)

# Define academic titles
tituly_bn <- c("Bc", "Ing.arch.", "Ing.Arch.", "Ing", "ThMgr", "Mgr.", "Mgr", "PharmDr", "PaedDr", "PhDr", "RNDr", "PHDr", "MUDr", "JUDr", "Doc", "doc", "prof", "Prof", "MVDr")
tituly_an <- c("DiS", "MBA", "MSc", "Ph.D.", "CSc", "PH.D.", "PhD", "Ph.D")

# Import Excel file
data <- read_excel("primary_data_extracted/vfz2021-kducsl_edit.xlsx", sheet = "Sheet1", col_names=c("A","B","C"))


# Initialize new columns
data <- data %>% mutate(acadegree_an = NA, acadegree_bn = NA)

# Replace academic titles in the 'A' column
for (t in tituly_bn) {
  data <- data %>% mutate(acadegree_bn = ifelse(str_detect(A, t), t, acadegree_bn))
}

for (t in tituly_an) {
  data <- data %>% mutate(acadegree_an = ifelse(str_detect(A, t), t, acadegree_an))
}

# Remove academic titles from the 'A' column
titles_to_remove <- c(" Bc.", " MUDr.", "MVDr.", "Ph.D.", " JUDr.", " PhDr.", "Ing.arch.", "Ing. arch.", "Ing.Arch.", "Ing. Arch.", ", Ing.", " Ing.", ".Ing.", " Mgr.", "Ing.arch.", "Ing.PhD.", " PH.D.", " PharmDr.", " PhDr. Ph.D.", " Ph.D.", " Dr.", " Mgr.et Mgr.", " Mgr-", " doc. CSc", " dipl.um.", " PHDr.", " RNDr.", " doc.DDr.PdD.", " Doc. JUDr. Ph.", " Ing", "-předseda", ",, Ph.D", ", Ph.D.", ",,Mgr.,Ph.D", ",, MBA", ", Dis.", ", Bc MBA", ", DiS", ", MBA", "MBA", "RNDr", ",, IWE", ", Dis.", ", MVDr.", ", Di", ", RNDr.Ph.D.", ", Prof.MUDr.", ", ThMgr.", " - krajský tajem", ", et,", ", prof. PhDr.", ", RNDr.Ph.D.", ", doc. JUDr.", ", PhDr., BcA.", "PhD", " prof", " RNDr", " PaedDr", " doc", " Doc", " BcA", ",Ph.D.", "  Mgr", "et")
for (title in titles_to_remove) {
  data <- data %>% mutate(A = str_replace_all(A, fixed(title), ""))
}

# Additional replacements based on specific conditions
data <- data %>% mutate(A = ifelse(str_detect(A, "et") & !A %in% c("Petr", "Petra", "Břetislav", "Peter", "Iveta", "Jetelinová", "Kajetán", "Žaneta", "Bernadetta", "Yveta", "Aneta", "Jeanette", "Petros", "Yvette", "Elisabeth", "Betty", "Svetozar", "Jiří Metod", "Elizabet", "Petri", "Petruše", "Marketa", "Petr Felix", "Yweta", "Karel", "Kvetoslava"), str_replace_all(A, "et", ""), A))

# Remove additional unwanted characters
data <- data %>% mutate(A = str_replace_all(A, fixed("MUDr "), ""),
                        A = str_replace_all(A, fixed("MUDr"), ""),
                        A = str_replace_all(A, fixed("Bc "), ""),
                        A = str_replace_all(A, fixed("Mgr"), ""),
                        A = str_replace_all(A, fixed("Ing"), ""),
                        A = str_replace_all(A, fixed("."), ""),
                        A = str_replace_all(A, fixed(","), ""),
                        A = str_replace_all(A, fixed(",,,"), ""),
                        A = str_replace_all(A, fixed("PhD"), ""),
                        A = str_replace_all(A, fixed("Bc"), ""),
                        A = str_replace_all(A, fixed("CSc"), ""),
                        A = str_replace_all(A, fixed("MSc"), ""),
                        A = str_replace_all(A, fixed("MVDr"), ""),
                        A = str_replace_all(A, fixed("RNDr"), ""),
                        A = str_replace_all(A, fixed("doc"), ""),
                        A = str_replace_all(A, fixed("JUDr"), ""),
                        A = str_replace_all(A, fixed("arch"), ""),
                        A = str_replace_all(A, fixed("Ing."), ""),
                        A = str_replace_all(A, fixed("MBA"), ""),
                        A = str_replace_all(A, fixed("PhDr"), ""),
                        A = str_replace_all(A, fixed("PhDr."), ""),
                        A = str_replace_all(A, fixed("Mgr"), ""),
                        A = str_replace_all(A, fixed("Mgr."), ""),
                        A = str_replace_all(A, fixed("DiS"), ""),
                        A = str_replace_all(A, fixed("Th"), ""),
                        A = str_replace_all(A, fixed("arch"), ""),
                        A = str_replace_all(A, fixed("r "), ""),
                        A = str_replace_all(A, fixed("Dr"), ""),
                        A = str_replace_all(A, fixed("M "), ""))




# Assuming your dataframe is named df
data <- data %>%
  mutate(financial = ifelse(grepl("peněžitý dar", C), 1, 0),
         C = gsub("peněžitý dar", "", C),
         C = gsub(",00", "", C),
         C = gsub(",60", "", C),
         C = gsub(" ", "", C)) %>%
  mutate(C = as.numeric(C))

# Splitting column A into surname and firstname
data <- data %>%
  separate(A, into = c("surname", "firstname"), sep = " ", extra = "drop", fill = "right")

# Renaming column R to birthdate
data <- data %>%
  rename(donor_birthyear = B,
         donor_name = firstname,
         donor_lastname = surname
         )

data$donation_party = "KDU-CSL"
data$donation_year = 2021
data$donation_source = 2

data$donor_birthyear = as.numeric(data$donor_birthyear)
data$donor_birthyear[is.na(data$donor_birthyear)] <- 0
data$donor_birthyear = format(as.Date((data$donor_birthyear-2), origin = "1900-01-01"), "%Y")


data = subset(data, select = -c(acadegree_an,acadegree_bn) )

# Creating financial and nonfinancial donation columns
data <- data %>%
  mutate(donation_financial = ifelse(financial == 1, C, 0),
         donation_nonfinancial = ifelse(financial == 0, C, 0)) %>%
  select(-C,-financial)

data$donation_all = data$donation_financial + data$donation_nonfinancial

# Order of columns
data <- data[, c("donation_party", "donation_year", "donor_name", "donor_lastname", "donor_birthyear", "donation_all", "donation_financial", "donation_nonfinancial", "donation_source")]

# Saving the dataframe
saveRDS(data, "primary_data_extracted/vfz2021-kducsl.rds")


### SPD 2018
data <- read_excel("primary_data_extracted/vfz2018-spd_edit.xlsx", sheet = "Sheet1")


# Rename columns
data <- data %>%
  rename(
    donation_financial = "Vvše neněžitého daru v Kč",
    donor_name = ...1,
    donor_lastname = ...2,
    donor_birthyear = "Datum narození nebo identifikační číslo dárce"
  )


# Replace values in firstname and surname
data <- data %>%
  mutate(
    donor_name = case_when(
      donor_name == "Patrík" ~ "Patrik",
      donor_name == "Daria" ~ "Darja",
      donor_name == "Ríchard" ~ "Richard",
      donor_name == "Jannila" ~ "Jarmila",
      donor_name == "Pvnelooi" ~ "Pynelopi",
      donor_name == "Míchal" ~ "Michal",
      donor_name == "Robert" ~ "Robe1i",
      TRUE ~ donor_name
    ),
    donor_lastname = case_when(
      donor_lastname == "Boiko" ~ "Bojko",
      donor_lastname == "Boiková" ~ "Bojková",
      donor_lastname == "Běrskv" ~ "Běrský",
      donor_lastname == "Hvťhová" ~ "Hyťhová",
      donor_lastname == "I-Ilaváč" ~ "Hlaváč",
      donor_lastname == "Kr icarová" ~ "Krejcarová",
      donor_lastname == "Kreicarová" ~ "Krejcarová",
      donor_lastname == "Mikulai" ~ "Mikulaj",
      donor_lastname == "Mt!chura" ~ "Měchura",
      donor_lastname == "Osmančiková" ~ "Osmančíková",
      donor_lastname == "Owczarzv" ~ "Owczarzy",
      donor_lastname == "Sukuo" ~ "Sukup",
      donor_lastname == "Vrz.áň" ~ "Vrzáň",
      donor_lastname == "burčo" ~ "Burčo",
      donor_lastname == "Pvtlíková" ~ "Pytlíková",
      donor_lastname == "Širokv" ~ "Široký",
      donor_lastname == "Fučile" ~ "Fučík",
      donor_lastname == "Koždo11" ~ "Koždoň",
      donor_lastname == "Pustěiovský" | donor_lastname == "Pust jovský" ~ "Pustějovský",
      donor_lastname == "Holik" ~ "Holík",
      donor_lastname == "Fucik" | donor_lastname == "Fučik" | donor_lastname == "Fučlk" ~ "Fučík",
      TRUE ~ donor_lastname
    )
  )


data$donation_party = "SPD"
data$donation_year = 2018
data$donation_source = 2

data$donor_birthyear = as.numeric(data$donor_birthyear)
data$donor_birthyear[is.na(data$donor_birthyear)] <- 0
data$donor_birthyear = format(as.Date((data$donor_birthyear-2), origin = "1900-01-01"), "%Y")

data$donation_nonfinancial = 0
data$donation_all = data$donation_financial + data$donation_nonfinancial


data <- data[, c("donation_party", "donation_year", "donor_name", "donor_lastname", "donor_birthyear", "donation_all", "donation_financial", "donation_nonfinancial", "donation_source")]

saveRDS(data, "primary_data_extracted/vfz2018-spd.rds")

### SPD 2019
data <- read_excel("primary_data_extracted/vfz2019-spd_edit.xlsx", sheet = "Sheet1", col_names=c("A","B","C", "D"))


# Rename columns
data <- data %>%
  rename(
    donation_financial = D,
    donor_name = A,
    donor_lastname = B,
    donor_birthyear = C
  )


# Replace values in firstname and surname
data <- data %>%
  mutate(
    donor_name = case_when(
      donor_name == "Patrík" ~ "Patrik",
      donor_name == "Daria" ~ "Darja",
      donor_name == "Ríchard" ~ "Richard",
      donor_name == "Jannila" ~ "Jarmila",
      donor_name == "Pvnelooi" ~ "Pynelopi",
      donor_name == "Míchal" ~ "Michal",
      donor_name == "Robert" ~ "Robe1i",
      TRUE ~ donor_name
    ),
    donor_lastname = case_when(
      donor_lastname == "Boiko" ~ "Bojko",
      donor_lastname == "Boiková" ~ "Bojková",
      donor_lastname == "Běrskv" ~ "Běrský",
      donor_lastname == "Hvťhová" ~ "Hyťhová",
      donor_lastname == "I-Ilaváč" ~ "Hlaváč",
      donor_lastname == "Kr icarová" ~ "Krejcarová",
      donor_lastname == "Kreicarová" ~ "Krejcarová",
      donor_lastname == "Mikulai" ~ "Mikulaj",
      donor_lastname == "Mt!chura" ~ "Měchura",
      donor_lastname == "Osmančiková" ~ "Osmančíková",
      donor_lastname == "Owczarzv" ~ "Owczarzy",
      donor_lastname == "Sukuo" ~ "Sukup",
      donor_lastname == "Vrz.áň" ~ "Vrzáň",
      donor_lastname == "burčo" ~ "Burčo",
      donor_lastname == "Pvtlíková" ~ "Pytlíková",
      donor_lastname == "Širokv" ~ "Široký",
      donor_lastname == "Fučile" ~ "Fučík",
      donor_lastname == "Koždo11" ~ "Koždoň",
      donor_lastname == "Pustěiovský" | donor_lastname == "Pust jovský" ~ "Pustějovský",
      donor_lastname == "Holik" ~ "Holík",
      donor_lastname == "Fucik" | donor_lastname == "Fučik" | donor_lastname == "Fučlk" ~ "Fučík",
      TRUE ~ donor_lastname
    )
  )


data$donation_party = "SPD"
data$donation_year = 2019
data$donation_source = 2

data$donor_birthyear = as.numeric(data$donor_birthyear)
data$donor_birthyear[is.na(data$donor_birthyear)] <- 0
data$donor_birthyear = format(as.Date((data$donor_birthyear-2), origin = "1900-01-01"), "%Y")

data$donation_nonfinancial = 0
data$donation_all = data$donation_financial + data$donation_nonfinancial


data <- data[, c("donation_party", "donation_year", "donor_name", "donor_lastname", "donor_birthyear", "donation_all", "donation_financial", "donation_nonfinancial", "donation_source")]

saveRDS(data, "primary_data_extracted/vfz2019-spd.rds")



### SPD 2020
data <- read_excel("primary_data_extracted/vfz2020-spd_edit.xlsx", sheet = "Sheet1", col_names=c("A","B","C", "D"))


# Rename columns
data <- data %>%
  rename(
    donation_financial = D,
    donor_name = A,
    donor_lastname = B,
    donor_birthyear = C
  )


# Replace values in firstname and surname
data <- data %>%
  mutate(
    donor_name = case_when(
      donor_name == "Patrík" ~ "Patrik",
      donor_name == "Daria" ~ "Darja",
      donor_name == "Ríchard" ~ "Richard",
      donor_name == "Jannila" ~ "Jarmila",
      donor_name == "Pvnelooi" ~ "Pynelopi",
      donor_name == "Míchal" ~ "Michal",
      donor_name == "Robert" ~ "Robe1i",
      donor_name == "Milada      '" ~ "Milada",
      TRUE ~ donor_name
    ),
    donor_lastname = case_when(
      donor_lastname == "Boiko" ~ "Bojko",
      donor_lastname == "Boiková" ~ "Bojková",
      donor_lastname == "Běrskv" ~ "Běrský",
      donor_lastname == "Hvťhová" ~ "Hyťhová",
      donor_lastname == "I-Ilaváč" ~ "Hlaváč",
      donor_lastname == "Kr icarová" ~ "Krejcarová",
      donor_lastname == "Kreicarová" ~ "Krejcarová",
      donor_lastname == "Mikulai" ~ "Mikulaj",
      donor_lastname == "Mt!chura" ~ "Měchura",
      donor_lastname == "Osmančiková" ~ "Osmančíková",
      donor_lastname == "Owczarzv" ~ "Owczarzy",
      donor_lastname == "Sukuo" ~ "Sukup",
      donor_lastname == "Vrz.áň" ~ "Vrzáň",
      donor_lastname == "burčo" ~ "Burčo",
      donor_lastname == "Pvtlíková" ~ "Pytlíková",
      donor_lastname == "Širokv" ~ "Široký",
      donor_lastname == "Fučile" ~ "Fučík",
      donor_lastname == "Koždo11" ~ "Koždoň",
      donor_lastname == "Pustěiovský" | donor_lastname == "Pust jovský" ~ "Pustějovský",
      donor_lastname == "Holik" ~ "Holík",
      donor_lastname == "Fucik" | donor_lastname == "Fučik" | donor_lastname == "Fučlk" ~ "Fučík",
      TRUE ~ donor_lastname
    )
  )



data$donation_party = "SPD"
data$donation_year = 2020
data$donation_source = 2

data$donor_birthyear = as.numeric(data$donor_birthyear)
data$donor_birthyear[is.na(data$donor_birthyear)] <- 0
data$donor_birthyear = format(as.Date((data$donor_birthyear-2), origin = "1900-01-01"), "%Y")

data$donation_nonfinancial = 0
data$donation_all = data$donation_financial + data$donation_nonfinancial


data <- data[, c("donation_party", "donation_year", "donor_name", "donor_lastname", "donor_birthyear", "donation_all", "donation_financial", "donation_nonfinancial", "donation_source")]

saveRDS(data, "primary_data_extracted/vfz2020-spd.rds")



### SPD 2021
data <- read_excel("primary_data_extracted/vfz2021-spd_edit.xlsx", sheet = "Sheet1", col_names=c("A","B","C", "D"))


# Rename columns
data <- data %>%
  rename(
    donation_financial = A,
    donor_name = B,
    donor_lastname = C,
    donor_birthyear = D
  )


# Replace values in financial_donation
data <- data %>%
  mutate(donation_financial = ifelse(donation_financial == "33333,33", "33333.33", donation_financial))


data$donor_birthyear = as.numeric(data$donor_birthyear)
data$donor_birthyear[is.na(data$donor_birthyear)] <- 0
data$donor_birthyear = format(as.Date((data$donor_birthyear-2), origin = "1900-01-01"), "%Y")

# Replace values in firstname and surname
data <- data %>%
  mutate(
    donor_name = case_when(
      donor_name == "Ilja" ~ "llia",
      donor_name == "Miroslav" & donor_birthyear == "1966" ~ "Miloslav",
      TRUE ~ donor_name
    ),
    donor_lastname = case_when(
      donor_lastname == "Fiala" & donor_birthyear == "1969" ~ "Fila",
      donor_lastname == "Kunav" ~ "Kunay",
      donor_lastname == "Tvkvart" ~ "Tykvart",
      TRUE ~ donor_lastname
    )
  )


data$donation_party = "SPD"
data$donation_year = 2021
data$donation_source = 2

data$donation_nonfinancial = 0
data$donation_financial = as.numeric(data$donation_financial)
data$donation_all = data$donation_financial + data$donation_nonfinancial


data <- data[, c("donation_party", "donation_year", "donor_name", "donor_lastname", "donor_birthyear", "donation_all", "donation_financial", "donation_nonfinancial", "donation_source")]

saveRDS(data, "primary_data_extracted/vfz2021-spd.rds")



### SPD 2022
data <- read_excel("primary_data_extracted/vfz2022-spd_edit.xlsx", sheet = "Sheet1", col_names=c("A","B","C", "D"))

# Rename columns
data <- data %>%
  rename(
    donation_financial = A,
    donor_name = B,
    donor_lastname = C,
    donor_birthyear = D
  )

data$donor_birthyear_aux = as.numeric(data$donor_birthyear)
data$donor_birthyear_aux[is.na(data$donor_birthyear)] <- 0
data$donor_birthyear_aux = format(as.Date((data$donor_birthyear_aux-2), origin = "1900-01-01"), "%Y")


data$donor_birthyear_aux[is.na(data$donor_birthyear_aux)] = format(as.Date(data$donor_birthyear[is.na(data$donor_birthyear_aux)],"%d.%m.%Y"),"%Y")


data <- data %>%
  select(-donor_birthyear)

data <- data %>%
  rename(
    donor_birthyear = donor_birthyear_aux
  )

# Replace values in firstname, surname, and birthdate with conditions
data <- data %>%
  mutate(
    donor_name = case_when(
      donor_name == "Ilja" ~ "llia",
      donor_name == "Benešová" & donor_birthyear == "1965" ~ "Hana",
      TRUE ~ donor_name
    ),
    donor_lastname = case_when(
      donor_lastname == "Stuchlík" ~ "Stuhlík",
      TRUE ~ donor_lastname
    )
  )



data$donation_financial = as.numeric(data$donation_financial)

data <- data %>%
  group_by(donor_lastname, donor_name, donor_birthyear) %>%
  summarize(
    donation_financial = sum(donation_financial, na.rm = TRUE),
  )

data$donation_party = "SPD"
data$donation_year = 2022
data$donation_source = 2

data$donation_nonfinancial = 0
data$donation_all = data$donation_financial + data$donation_nonfinancial


data <- data[, c("donation_party", "donation_year", "donor_name", "donor_lastname", "donor_birthyear", "donation_all", "donation_financial", "donation_nonfinancial", "donation_source")]

saveRDS(data, "primary_data_extracted/vfz2022-spd-financial.rds")



### SPD 2022 - non-financial
data <- read_excel("primary_data_extracted/vfz2022-spd_BEZUPLATNE_edit.xlsx", sheet = "Sheet1", col_names=c("A","B","C", "D"))

# Rename columns
data <- data %>%
  rename(
    donation_nonfinancial = D,
    donor_name = A,
    donor_lastname = B,
    donor_birthyear = C
  )


data$donor_birthyear_aux = as.numeric(data$donor_birthyear)
data$donor_birthyear_aux[is.na(data$donor_birthyear)] <- 0
data$donor_birthyear_aux = format(as.Date((data$donor_birthyear_aux-2), origin = "1900-01-01"), "%Y")

data$donor_birthyear_aux[is.na(data$donor_birthyear_aux)] = format(as.Date(data$donor_birthyear[is.na(data$donor_birthyear_aux)],"%d.%m.%Y"),"%Y")


data <- data %>%
  select(-donor_birthyear)

data <- data %>%
  rename(
    donor_birthyear = donor_birthyear_aux
  )



data$donation_nonfinancial = as.numeric(data$donation_nonfinancial)

data <- data %>%
  group_by(donor_lastname, donor_name, donor_birthyear) %>%
  summarize(
    donation_nonfinancial = sum(donation_nonfinancial, na.rm = TRUE),
  )


data$donation_party = "SPD"
data$donation_year = 2022
data$donation_source = 2

data$donation_financial = 0
data$donation_all = data$donation_financial + data$donation_nonfinancial



data <- data[, c("donation_party", "donation_year", "donor_name", "donor_lastname", "donor_birthyear", "donation_all", "donation_financial", "donation_nonfinancial", "donation_source")]


saveRDS(data, "primary_data_extracted/vfz2022-spd-nonfinancial.rds")

### Merge 2022 . financial and non-financial
# Load financial data and merge
financial_data <- readRDS("primary_data_extracted/vfz2022-spd-financial.rds")
merged_data <- merge(data, financial_data, by = c("donor_lastname", "donor_name", "donor_birthyear"), all = TRUE)

# Merge columns
merged_data$donation_party = "SPD"
merged_data$donation_year = 2022
merged_data$donation_source = 2
merged_data$donation_financial.x[is.na(merged_data$donation_financial.x)] = 0
merged_data$donation_nonfinancial.x[is.na(merged_data$donation_nonfinancial.x)] = 0
merged_data$donation_financial.y[is.na(merged_data$donation_financial.y)] = 0
merged_data$donation_nonfinancial.y[is.na(merged_data$donation_nonfinancial.y)] = 0

merged_data$donation_financial = merged_data$donation_financial.x + merged_data$donation_financial.y
merged_data$donation_nonfinancial = merged_data$donation_nonfinancial.x + merged_data$donation_nonfinancial.y
merged_data$donation_all = merged_data$donation_financial + merged_data$donation_nonfinancial


merged_data <- merged_data %>%
  select(-donation_financial.x,-donation_financial.y,-donation_nonfinancial.x,-donation_nonfinancial.y,-donation_all.x,-donation_all.y,
         -donation_party.x,-donation_party.y,-donation_year.x,-donation_year.y,-donation_source.x,-donation_source.y)


merged_data <- merged_data[, c("donation_party", "donation_year", "donor_name", "donor_lastname", "donor_birthyear", "donation_all", "donation_financial", "donation_nonfinancial", "donation_source")]


# Save
saveRDS(merged_data, "primary_data_extracted/vfz2022-spd.rds")

### SPD 2023
data <- read_excel("primary_data_extracted/vfz2023-spd_edit.xlsx", sheet = "Sheet1", col_names=c("A","B","C", "D"))



# Rename columns
data <- data %>%
  rename(
    donation_financial = A,
    donor_name = B,
    donor_lastname = C,
    donor_birthyear = D
  )


# Replace specific birthdate_aux values
data <- data %>%
  mutate(
    donor_birthyear = case_when(
      donor_birthyear == "I 2.10.1964" ~ "02.10.1964",
      donor_birthyear == "18.0l.1976" ~ "18.01.1976",
      donor_birthyear == "08.l 1.1958" ~ "08.11.1958",
      TRUE ~ donor_birthyear
    )
  )

# Replace specific firstname and surname values
data <- data %>%
  mutate(
    donor_name = case_when(
      donor_name == "Vladirrúra" ~ "Vladimíra",
      donor_name == "Jiií" ~ "Jiří",
      donor_name == "Míchal" ~ "Michal",
      TRUE ~ donor_name
    ),
    donor_lastname = case_when(
      donor_lastname == "Fučile" ~ "Fučík",
      donor_lastname == "Kooecký" ~ "Kopecký",
      donor_lastname == "Kvsling" ~ "Kysling",
      donor_lastname == "Wísiorek" ~ "Wisiorek",
      TRUE ~ donor_lastname
    )
  )

data$donor_birthyear_aux = as.numeric(data$donor_birthyear)
data$donor_birthyear_aux[is.na(data$donor_birthyear)] <- 0
data$donor_birthyear_aux = format(as.Date((data$donor_birthyear_aux-2), origin = "1900-01-01"), "%Y")

data$donor_birthyear_aux[is.na(data$donor_birthyear_aux)] = format(as.Date(data$donor_birthyear[is.na(data$donor_birthyear_aux)],"%d.%m.%Y"),"%Y")


data <- data %>%
  select(-donor_birthyear)

data <- data %>%
  rename(
    donor_birthyear = donor_birthyear_aux
  )


data$donation_party = "SPD"
data$donation_year = 2023
data$donation_source = 2

data$donation_financial = as.numeric(data$donation_financial)
data$donation_nonfinancial = 0
data$donation_all = data$donation_financial + data$donation_nonfinancial



data <- data[, c("donation_party", "donation_year", "donor_name", "donor_lastname", "donor_birthyear", "donation_all", "donation_financial", "donation_nonfinancial", "donation_source")]

saveRDS(data, "primary_data_extracted/vfz2023-spd.rds")



### ANO 2020
data <- read_excel("primary_data_extracted/vfz2020-ano_edit.xlsx", sheet = "ano_2020", col_names=c("A","B","C","D"))


# Rename columns
data <- data %>%
  rename(
    donation_financial = D,
    donor_name = A,
    donor_lastname = B,
    donor_birthyear = C
  )


data$donor_birthyear_aux = as.numeric(data$donor_birthyear)
data$donor_birthyear_aux[is.na(data$donor_birthyear)] <- 0
data$donor_birthyear_aux = format(as.Date((data$donor_birthyear_aux-2), origin = "1900-01-01"), "%Y")

data$donor_birthyear_aux[is.na(data$donor_birthyear_aux)] = format(as.Date(data$donor_birthyear[is.na(data$donor_birthyear_aux)],"%d/%m/%Y"),"%Y")


data <- data %>%
  select(-donor_birthyear)

data <- data %>%
  rename(
    donor_birthyear = donor_birthyear_aux
  )


data$donation_party = "ANO2011"
data$donation_year = 2020
data$donation_source = 2

data$donation_financial = as.numeric(data$donation_financial)
data$donation_nonfinancial = 0
data$donation_all = data$donation_financial + data$donation_nonfinancial


data <- data[, c("donation_party", "donation_year", "donor_name", "donor_lastname", "donor_birthyear", "donation_all", "donation_financial", "donation_nonfinancial", "donation_source")]


saveRDS(data, "primary_data_extracted/vfz2020-ano.rds")


### ANO 2021
data <- read_excel("primary_data_extracted/vfz2021-ano_edit.xlsx", sheet = "Sheet1", col_names=c("A","B","C"))

data <- data %>%
  mutate(name = str_split(A, ", ", simplify = TRUE))

data$donor_lastname = data$name[,1]
data$donor_name = data$name[,2]

# Rename columns
data <- data %>%
  rename(
    donation_financial = C,
    donor_birthyear = B
  )


data <- data %>%
  select(-A,-name)

data <- data %>%
  mutate(
    donation_financial = str_replace_all(donation_financial, c(
      "peněžitý dar" = "",
      "peněžitý" = "",
      "dar" = "",
      "I" = "1",
      ",00" = "",
      "O" = "0",
      "       -" = "",
      "   " = "",
      "  " = "",
      " " = ""
    )))

data$donation_financial = as.numeric(data$donation_financial)

data <- data %>%
  mutate(donor_name = ifelse(donor_lastname == "Brož, Lubomír", "Lubomír", donor_name),
         donor_lastname = ifelse(donor_lastname == "Brož, Lubomír", "Brož", donor_lastname),
         donor_name = ifelse(donor_lastname == "Mrózková Heříková", "Marcela", donor_name),
         donor_lastname = ifelse(donor_lastname == "Mrózková Heříková", "Heříková", donor_lastname),
         donor_name = ifelse(donor_lastname == "Slávka", "Vladimír", donor_name),
         donor_name = ifelse(donor_name == "Irena Pálková", "Irena", donor_name),
         donor_name = ifelse(donor_name == "VasilSilvestr", "Vasil Silvestr", donor_name))

data <- data %>%
  mutate(donor_lastname = gsub("  ", " ", donor_lastname),
         donor_name = gsub("  ", " ", donor_name))


data$donor_birthyear_aux = as.numeric(data$donor_birthyear)
data$donor_birthyear_aux[is.na(data$donor_birthyear)] <- 0
data$donor_birthyear_aux = format(as.Date((data$donor_birthyear_aux-2), origin = "1900-01-01"), "%Y")

data$donor_birthyear_aux[is.na(data$donor_birthyear_aux)] = format(as.Date(data$donor_birthyear[is.na(data$donor_birthyear_aux)],"%d.%m.%Y"),"%Y")


data <- data %>%
  select(-donor_birthyear)

data <- data %>%
  rename(
    donor_birthyear = donor_birthyear_aux
  )

data <- data %>%
  filter(donor_name != "spol. s r.o.")

data$donation_party = "ANO2011"
data$donation_year = 2021
data$donation_source = 2

data$donation_financial = as.numeric(data$donation_financial)
data$donation_nonfinancial = 0
data$donation_all = data$donation_financial + data$donation_nonfinancial


data <- data[, c("donation_party", "donation_year", "donor_name", "donor_lastname", "donor_birthyear", "donation_all", "donation_financial", "donation_nonfinancial", "donation_source")]


saveRDS(data, "primary_data_extracted/vfz2021-ano.rds")


### Trikolora 2021
data <- read_excel("primary_data_extracted/vfz2021-trikolora_edit.xlsx", sheet = "Sheet1", col_names=c("A","B","C"))

data <- data %>%
  mutate(name = str_split(A, ", ", simplify = TRUE))

data$donor_lastname = data$name[,1]
data$donor_name = data$name[,2]

# Rename columns
data <- data %>%
  rename(
    donation_financial = C,
    donor_birthyear = B
  )

data <- data %>%
  select(-A,-name)

data <- data %>%
  mutate(financial = ifelse(grepl("peněžitý dar", donation_financial), 1, 0))

data <- data %>%
  mutate(donation_financial_aux = ifelse(financial == 1, donation_financial, NA))


data <- data %>%
  mutate(
    donation_financial_aux = str_replace_all(donation_financial_aux, c(
      "peněžitý dar" = "",
      "peněžitý" = "",
      "dar" = "",
      "I" = "1",
      ",00" = "",
      "O" = "0",
      "       -" = "",
      "   " = "",
      "  " = "",
      " " = ""
    )))

# Convert 'financial_donation' to numeric and replace NA with 0
data <- data %>%
  mutate(donation_financial_aux = as.numeric(donation_financial_aux),
         donation_financial_aux = ifelse(is.na(donation_financial_aux), 0, donation_financial_aux))

# Generate 'nonfinancial_donation' column based on 'financial' condition
data <- data %>%
  mutate(donation_nonfinancial = ifelse(financial == 0, donation_financial, NA))

# Replace specific substrings in 'nonfinancial_donation'
data <- data %>%
  mutate(donation_nonfinancial = gsub("bezúplatné plnění", "", donation_nonfinancial),
         donation_nonfinancial = gsub("dar", "", donation_nonfinancial),
         donation_nonfinancial = gsub("I", "1", donation_nonfinancial),
         donation_nonfinancial = gsub(",00", "", donation_nonfinancial),
         donation_nonfinancial = gsub("O", "0", donation_nonfinancial),
         donation_nonfinancial = gsub("       -", "", donation_nonfinancial),
         donation_nonfinancial = gsub("   ", "", donation_nonfinancial),
         donation_nonfinancial = gsub("  ", "", donation_nonfinancial),
         donation_nonfinancial = gsub(" ", "", donation_nonfinancial),
         donation_nonfinancial = gsub(",", ".", donation_nonfinancial))

# Convert 'nonfinancial_donation' to numeric and replace NA with 0
data <- data %>%
  mutate(donation_nonfinancial = as.numeric(donation_nonfinancial),
         donation_nonfinancial = ifelse(is.na(donation_nonfinancial), 0, donation_nonfinancial))

data <- data %>%
  mutate(donor_lastname = gsub("  ", " ", donor_lastname),
         donor_name = gsub("  ", " ", donor_name),
         donor_lastname = gsub(",", "", donor_lastname),
         donor_name = gsub(",", "", donor_name),
         donor_lastname = gsub("\\.", "", donor_lastname),
         donor_name = gsub("\\.", "", donor_name))

# Replace specific values in 'firstname' and 'birthdate'
data <- data %>%
  mutate(donor_name = ifelse(donor_name == "JIitka", "Jitka", donor_name),
         donor_lastname = ifelse(donor_lastname == "Fošunová" & donor_birthyear == "1966", "Fošumová", donor_lastname),
         donor_lastname = ifelse(donor_lastname == "Hanák3", "Hanák", donor_lastname),
         donor_lastname = ifelse(donor_lastname == "KRIZ", "Kříž", donor_lastname),
         donor_name = ifelse(donor_name == "DAVID", "David", donor_name),
         donor_lastname = ifelse(donor_lastname == "KAŠNÝ", "Kašný", donor_lastname),
         donor_lastname = ifelse(donor_lastname == "LORENZ", "Lorenz", donor_lastname))



data <- data %>%
  select(-donation_financial)
data$donation_financial = data$donation_financial_aux


data$donation_party = "Trikolora"
data$donation_year = 2021
data$donation_source = 2

data$donation_financial = as.numeric(data$donation_financial)
data$donation_nonfinancial = 0
data$donation_all = data$donation_financial + data$donation_nonfinancial

data <- data[, c("donation_party", "donation_year", "donor_name", "donor_lastname", "donor_birthyear", "donation_all", "donation_financial", "donation_nonfinancial", "donation_source")]

saveRDS(data, "primary_data_extracted/vfz2021-trikolora.rds")

### Merging
data = readRDS("data/data_donation_oofppm.rds")

data$polparty=as.character(data$polparty)
data <- data %>%
     mutate(polparty = ifelse(polparty == "ano", "ANO2011", polparty),
            polparty = ifelse(polparty == "kscm", "KSCM", polparty),
            polparty = ifelse(polparty == "kducsl", "KDU-CSL", polparty),
            polparty = ifelse(polparty == "pirati", "Pirati", polparty),
            polparty = ifelse(polparty == "spd", "SPD", polparty),
            polparty = ifelse(polparty == "stan", "STAN", polparty),
            polparty = ifelse(polparty == "top09", "TOP09", polparty),
            polparty = ifelse(polparty == "cssd", "CSSD", polparty),
            polparty = ifelse(polparty == "svobodni", "Svobodni", polparty),
            polparty = ifelse(polparty == "trikolora", "Trikolora", polparty),
            polparty = ifelse(polparty == "prisaha", "Prisaha", polparty),
            polparty = ifelse(polparty == "ods", "ODS", polparty)
                       )

data$donation_source = 1

data$year = as.numeric(data$year)

# Rename columns
data <- data %>%
  rename(
    donation_party = polparty,
    donation_year = year,
    donor_name = firstname,
    donor_lastname = surname,
    donation_financial = financial_donation,
    donation_nonfinancial = nonfinancial_donation,
    donor_birthyear = birthyear,
  )

data <- data[, c("donation_party", "donation_year", "donor_name", "donor_lastname", "donor_birthyear", "donation_all", "donation_financial", "donation_nonfinancial", "donation_source")]

### ANO
# 2020
data_ano_2020 = readRDS("primary_data_extracted/vfz2020-ano.rds")
data_ano_2020$donor_birthyear = as.numeric(data_ano_2020$donor_birthyear)
merged_data = rbind(data,data_ano_2020)

# 2021
data_ano_2021 = readRDS("primary_data_extracted/vfz2021-ano.rds")
data_ano_2021$donor_birthyear = as.numeric(data_ano_2021$donor_birthyear)
merged_data = rbind(merged_data,data_ano_2021)

### KDUCSL
#2021
data_kdu_2021 = readRDS("primary_data_extracted/vfz2021-kducsl.rds")
data_kdu_2021$donor_birthyear = as.numeric(data_kdu_2021$donor_birthyear)
merged_data = rbind(merged_data,data_kdu_2021)

### Trikolora
#2021
data_trikolora_2021 = readRDS("primary_data_extracted/vfz2021-trikolora.rds")
data_trikolora_2021$donor_birthyear = as.numeric(data_trikolora_2021$donor_birthyear)
merged_data = rbind(merged_data,data_trikolora_2021)

### SPD
#2018
data_spd_2018 = readRDS("primary_data_extracted/vfz2018-spd.rds")
data_spd_2018$donor_birthyear = as.numeric(data_spd_2018$donor_birthyear)
merged_data = rbind(merged_data,data_spd_2018)
#2019
data_spd_2019 = readRDS("primary_data_extracted/vfz2019-spd.rds")
data_spd_2019$donor_birthyear = as.numeric(data_spd_2019$donor_birthyear)
merged_data = rbind(merged_data,data_spd_2019)
#2020
data_spd_2020 = readRDS("primary_data_extracted/vfz2020-spd.rds")
data_spd_2020$donor_birthyear = as.numeric(data_spd_2020$donor_birthyear)
merged_data = rbind(merged_data,data_spd_2020)
#2021
data_spd_2021 = readRDS("primary_data_extracted/vfz2021-spd.rds")
data_spd_2021$donor_birthyear = as.numeric(data_spd_2021$donor_birthyear)
merged_data = rbind(merged_data,data_spd_2021)
#2022
data_spd_2022 = readRDS("primary_data_extracted/vfz2022-spd.rds")
data_spd_2022$donor_birthyear = as.numeric(data_spd_2022$donor_birthyear)
merged_data = rbind(merged_data,data_spd_2022)
#2023
data_spd_2023 = readRDS("primary_data_extracted/vfz2023-spd.rds")
data_spd_2023$donor_birthyear = as.numeric(data_spd_2023$donor_birthyear)
merged_data = rbind(merged_data,data_spd_2023)


### collapse and order
final_data <- merged_data %>%
  group_by(donation_party, donation_year, donor_name, donor_lastname, donor_birthyear, donation_source) %>%
  summarise(donation_all = sum(donation_all),
            donation_financial = sum(donation_financial),
            donation_nonfinancial = sum(donation_nonfinancial))

final_data=final_data %>% group_by(donation_party, donor_name, donor_lastname, donor_birthyear) %>% mutate(person_id = cur_group_id())

final_data <- final_data %>%
  rename(
    donor_surname = donor_lastname
  )


# Save
saveRDS(final_data, "data/finalDataset.rds")


final_data=final_data %>% group_by(donor_name, donor_surname, donor_birthyear) %>% mutate(id_xx = cur_group_id())
final_data=final_data %>% group_by(donation_party, donor_name, donor_surname, donor_birthyear) %>% mutate(id_xx_donorParty = cur_group_id())

### unique donors
print(max(final_data$id_xx))
### unique party donors
print(max(final_data$id_xx_donorParty))

# Some stats
summaryCounts=print.table(final_data %>% group_by(donation_party, donation_year) %>% count(donation_party))


summarySums=print.table(final_data %>% group_by(donation_party, donation_year) %>% count(donation_party))