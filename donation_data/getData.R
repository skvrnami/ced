#install.packages("readxl")
#install.packages("writexl")
#install.packages("dplyr")
#install.packages("stringr")
#install.packages("haven")

# Load necessary libraries
library(readxl)
library(writexl)
library(dplyr)
library(stringr)
library(haven)

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