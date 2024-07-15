# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.

tar_option_set(packages = c("dplyr", "rvest", "here", # "listr", 
                            "readxl", "readr", "reclin2"))

tar_source()

# TODO: kategorizace titulů
# divné tituly jako ak.arch nebo 
# prom.pedagog => Mgr. (https://cs.wikipedia.org/wiki/Promovan%C3%BD_pr%C3%A1vn%C3%ADk)

# Replace the target list below with your own:
psp_data <- list(
  tar_target(psp_1996, command = {
    cpp <- read_cpp(here("data", "PS1996", "CPPPS96.xlsx"), function(x) {x %>% rename(ZKRATKAP8 = ZKRATKAP)})
    read_excel(here("data", "PS1996", "PS96-RK.xlsx")) %>%
      clean_ps(., VSTRANA_MAP_96) %>%
      left_join(., cpp, by = "PSTRANA") %>%
      merge_and_recode_titles %>%
      mutate(row_id = row_number(), 
             ROK_NAROZENI = 1996 - VEK)
  }),
  
  tar_target(psp_1998, command = {
    cpp <- read_cpp(here("data", "PS1998", "CPPPS98.xlsx"), function(x) {x %>% rename(ZKRATKAP8 = ZKRATKAP)})
    read_excel(here("data", "PS1998", "PS98-RK.xlsx")) %>%
      clean_ps(., VSTRANA_MAP_98) %>%
      left_join(., cpp, by = "PSTRANA") %>%
      merge_and_recode_titles %>%
      mutate(row_id = row_number(), 
             ROK_NAROZENI = 1998 - VEK)
  }),
  
  tar_target(psp_2002, command = {
    cpp <- read_cpp(here("data", "PS2002", "CPPPS02.xlsx"))
    cns <- read_cns(here("data", "PS2002", "CNSPS02.xlsx"))
    read_excel(here("data", "PS2002", "PS02-RK.xlsx")) %>%
      rename(NAZEV_STRK = KSTRANA_NAZEV) %>%
      left_join(., cpp, by = "PSTRANA") %>%
      left_join(., cns, by = "NSTRANA") %>%
      merge_and_recode_titles %>%
      mutate(row_id = row_number(), 
             ROK_NAROZENI = 2002 - VEK)
  }),
  
  tar_target(psp_2006, command = {
    psp_parties <- read_parties(here("data", "PS2006", "PS2006reg2006", "PSRKL.xlsx"), 
                                clean_dbf_excel_colnames)
    cpp <- read_cpp(here("data", "PS2006", "PS2006ciselniky2006", "CPP.xlsx"), 
                    clean_dbf_excel_colnames)
    cns <- read_cns(here("data", "PS2006", "PS2006ciselniky2006", "CNS.xlsx"), 
                    clean_dbf_excel_colnames)
    read_candidates(here("data", "PS2006", "PS2006reg2006", "PSRK.xlsx"), 
                    psp_parties, cpp, cns, clean_dbf_excel_colnames)
  }),
  
  tar_target(psp_2010, command = {
    psp_parties <- read_parties(here("data", "PS2010", "PS2010reg2010", "PSRKL.xlsx"), 
                                clean_dbf_excel_colnames)
    cpp <- read_cpp(here("data", "PS2010", "PS2010ciselniky2010", "CPP.xlsx"), 
                    clean_dbf_excel_colnames)
    cns <- read_cns(here("data", "PS2010", "PS2010ciselniky2010", "CNS.xlsx"), 
                    clean_dbf_excel_colnames)
    read_candidates(here("data", "PS2010", "PS2010reg2010", "PSRK.xlsx"), 
                    psp_parties, cpp, cns, clean_dbf_excel_colnames)
  }),
  
  tar_target(psp_2013, command = {
    psp_parties <- read_parties(here("data", "PS2013", "PS2013reg20131026", "PSRKL.xlsx"), 
                                clean_dbf_excel_colnames)
    cpp <- read_cpp(here("data", "PS2013", "PS2013ciselniky20131021", "CPP.xlsx"), 
                    clean_dbf_excel_colnames)
    cns <- read_cns(here("data", "PS2013", "PS2013ciselniky20131021", "CNS.xlsx"), 
                    clean_dbf_excel_colnames)
    read_candidates(here("data", "PS2013", "PS2013reg20131026", "PSRK.xlsx"), 
                    psp_parties, cpp, cns, clean_dbf_excel_colnames)
  }),
  
  tar_target(psp_2017, command = {
    psp_parties <- read_parties(here("data", "PS2017NSS", "PS2017reg20171122_xlsx", "psrkl.xlsx"))
    cpp <- read_cpp(here("data", "PS2017NSS", "PS2017ciselniky20170905", "cpp.xlsx"))
    cns <- read_cns(here("data", "PS2017NSS", "PS2017ciselniky20170905", "cns.xlsx"))
    read_candidates(here("data", "PS2017NSS", "PS2017reg20171122_xlsx", "psrk.xlsx"), 
                    psp_parties, cpp, cns) %>%
      mutate(PLATNOST = ifelse(PLATNOST == "A", 0, 1), 
             MANDAT = ifelse(MANDAT == "A", 1, 0))
  }),
  
  tar_target(psp_2021, command = {
    psp_parties <- read_parties(here("data", "PS2021", "PS2021reg20211010_xlsx", "psrkl.xlsx"))
    cpp <- read_cpp(here("data", "PS2021", "PS2021ciselniky20210824", "cpp.xlsx"))
    cns <- read_cns(here("data", "PS2021", "PS2021ciselniky20210824", "cns.xlsx"))
    read_candidates(here("data", "PS2021", "PS2021reg20211010_xlsx", "psrk.xlsx"), 
                    psp_parties, cpp, cns) %>%
      mutate(PLATNOST = ifelse(PLATNOST == "A", 0, 1), 
             MANDAT = ifelse(MANDAT == "A", 1, 0))
  })
)

reg_data <- list(
  tar_target(reg_2000, command = {
    cpp <- read_cpp(here("data", "KZ2000", "CPPKZ00.xlsx"))
    cns <- cpp %>% rename(NSTRANA = PSTRANA, ZKRATKAN8 = ZKRATKAP8)
    list_path <- here("data", "KZ2000", "KZ00-Registr_kandidatu.xlsx")
    year <- as.numeric(stringr::str_extract(list_path, "[0-9]{4}"))
    read_excel(list_path) %>%
      left_join(., cpp, by = "PSTRANA") %>%
      left_join(., cns, by = "NSTRANA") %>%
      mutate(row_id = row_number(), 
             ROK_NAROZENI = year - VEK) %>%
      merge_and_recode_titles %>%
      rename(NAZEV_STRK = KSTRANA_NAZEV)
  }),
  
  tar_target(reg_2004, command = {
    cpp <- read_cpp(here("data", "KZ2004", "CPPKZ04.xlsx"))
    cns <- cpp %>% rename(NSTRANA = PSTRANA, ZKRATKAN8 = ZKRATKAP8)
    list_path <- here("data", "KZ2004", "KZ04-Registr_kandidatu.xlsx")
    year <- as.numeric(stringr::str_extract(list_path, "[0-9]{4}"))
    read_excel(list_path) %>%
      left_join(., cpp, by = "PSTRANA") %>%
      left_join(., cns, by = "NSTRANA") %>%
      mutate(row_id = row_number(), 
             ROK_NAROZENI = year - VEK) %>%
      merge_and_recode_titles %>%
      rename(NAZEV_STRK = KSTRANA_NAZEV)
  }),
  
  tar_target(reg_2008, command = {
    parties <- read_parties(here("data", "KZ2008", "kz2008_data_dbf", "KZRKL.xlsx"), 
                            clean_dbf_excel_colnames) %>% 
      unique
    cpp <- read_cpp(here("data", "KZ2008", "ciselnikyKZ2008", "CPP.xlsx"), 
                    clean_dbf_excel_colnames)
    cns <- read_cns(here("data", "KZ2008", "ciselnikyKZ2008", "CNS.xlsx"), 
                    clean_dbf_excel_colnames)
    read_candidates(here("data", "KZ2008", "kz2008_data_dbf", "KZRK.xlsx"), 
                    parties, cpp, cns, clean_dbf_excel_colnames)
  }),
  
  tar_target(reg_2012, command = {
    parties <- read_parties(here("data", "KZ2012", "kz2012_data_dbf", "KZRKL.xlsx"), 
                            clean_dbf_excel_colnames) %>% 
      unique
    cpp <- read_cpp(here("data", "KZ2012", "ciselniky20121010", "CPP.xlsx"), 
                    clean_dbf_excel_colnames)
    cns <- read_cns(here("data", "KZ2012", "ciselniky20121010", "CNS.xlsx"), 
                    clean_dbf_excel_colnames)
    read_candidates(here("data", "KZ2012", "kz2012_data_dbf", "KZRK.xlsx"), 
                    parties, cpp, cns, clean_dbf_excel_colnames)
  }),
  
  tar_target(reg_2016, command = {
    parties <- read_parties(here("data", "KZ2016", "KZ2016reg20161008_xlsx", "kzrkl.xlsx")) %>% 
      unique
    cpp <- read_cpp(here("data", "KZ2016", "KZ2016ciselniky20161007", "cpp.xlsx"))
    cns <- read_cns(here("data", "KZ2016", "KZ2016ciselniky20161007", "cns.xlsx"))
    read_candidates(here("data", "KZ2016", "KZ2016reg20161008_xlsx", "kzrk.xlsx"), 
                    parties, cpp, cns) %>%
      mutate(PLATNOST = ifelse(PLATNOST == "A", 0, 1), 
             MANDAT = ifelse(MANDAT == "A", 1, 0))
  }),
  
  tar_target(reg_2020, command = {
    parties <- read_parties(here("data", "KZ2020", "KZ2020reg20201004a_xlsx", "kzrkl.xlsx")) %>% 
      unique
    cpp <- read_cpp(here("data", "KZ2020", "KZ2020ciselniky20201002", "cpp.xlsx"))
    cns <- read_cns(here("data", "KZ2020", "KZ2020ciselniky20201002", "cns.xlsx"))
    read_candidates(here("data", "KZ2020", "KZ2020reg20201004a_xlsx", "kzrk.xlsx"), 
                    parties, cpp, cns) %>%
      mutate(PLATNOST = ifelse(PLATNOST == "A", 0, 1), 
             MANDAT = ifelse(MANDAT == "A", 1, 0))
  }), 
  
  tar_target(reg_panel, {
    reg00_04 <- match_reg_data(reg_2000, reg_2004)
    reg04_08 <- match_reg_data(reg_2004, reg_2008)
    
    reg00_08 <- match_reg_data(
      reg_2000 %>% 
        filter(!row_id %in% reg00_04$row_id.x),
      reg_2008 %>% 
        filter(!row_id %in% reg04_08$row_id.y)
    )
    
    reg08_12 <- match_reg_data(reg_2008, reg_2012)
    reg04_12 <- match_reg_data(
      reg_2004 %>% 
        filter(!row_id %in% reg04_08$row_id.x),
      reg_2012 %>% 
        filter(!row_id %in% reg08_12$row_id.y)
    )
    reg00_12 <- match_reg_data(
      reg_2000 %>% 
        filter(!row_id %in% reg00_04$row_id.x) %>% 
        filter(!row_id %in% reg00_08$row_id.x),
      reg_2012 %>% 
        filter(!row_id %in% reg08_12$row_id.y) %>% 
        filter(!row_id %in% reg04_12$row_id.y)
    )
    
    reg12_16 <- match_reg_data(reg_2012, reg_2016)
    reg08_16 <- match_reg_data(
      reg_2008 %>% 
        filter(!row_id %in% reg08_12$row_id.x),
      reg_2016 %>% 
        filter(!row_id %in% reg12_16$row_id.y)
    )
    reg04_16 <- match_reg_data(
      reg_2004 %>% 
        filter(!row_id %in% reg04_08$row_id.x) %>% 
        filter(!row_id %in% reg04_12$row_id.x),
      reg_2016 %>% 
        filter(!row_id %in% reg12_16$row_id.y) %>% 
        filter(!row_id %in% reg08_16$row_id.x)
    )
    reg00_16 <- match_reg_data(
      reg_2000 %>% 
        filter(!row_id %in% reg00_04$row_id.x) %>% 
        filter(!row_id %in% reg00_08$row_id.x) %>% 
        filter(!row_id %in% reg00_12$row_id.x),
      reg_2016 %>% 
        filter(!row_id %in% reg12_16$row_id.y) %>% 
        filter(!row_id %in% reg08_16$row_id.x) %>% 
        filter(!row_id %in% reg04_16$row_id.x)
    )
    
    reg16_20 <- match_reg_data(reg_2016, reg_2020)
    reg12_20 <- match_reg_data(
      reg_2012 %>% 
        filter(!row_id %in% reg12_16$row_id.x),
      reg_2020 %>% 
        filter(!row_id %in% reg16_20$row_id.y)
    )
    reg08_20 <- match_reg_data(
      reg_2008 %>% 
        filter(!row_id %in% reg08_12$row_id.x) %>% 
        filter(!row_id %in% reg08_16$row_id.x),
      reg_2020 %>% 
        filter(!row_id %in% reg16_20$row_id.y) %>% 
        filter(!row_id %in% reg12_20$row_id.y)
    )
    reg04_20 <- match_reg_data(
      reg_2004 %>% 
        filter(!row_id %in% reg04_08$row_id.x) %>% 
        filter(!row_id %in% reg04_12$row_id.x) %>% 
        filter(!row_id %in% reg04_16$row_id.x),
      reg_2020 %>% 
        filter(!row_id %in% reg16_20$row_id.y) %>% 
        filter(!row_id %in% reg12_20$row_id.y) %>% 
        filter(!row_id %in% reg08_20$row_id.x)
    )
    reg00_20 <- match_reg_data(
      reg_2000 %>% 
        filter(!row_id %in% reg00_04$row_id.x) %>% 
        filter(!row_id %in% reg00_08$row_id.x) %>% 
        filter(!row_id %in% reg00_12$row_id.x) %>% 
        filter(!row_id %in% reg00_16$row_id.x),
      reg_2020 %>% 
        filter(!row_id %in% reg16_20$row_id.y) %>% 
        filter(!row_id %in% reg12_20$row_id.y) %>% 
        filter(!row_id %in% reg08_20$row_id.x) %>% 
        filter(!row_id %in% reg04_20$row_id.x)
    )
    
    reg00_04 <- reg00_04 %>% select(row_id_2000 = row_id.x, row_id_2004 = row_id.y)
    reg00_08 <- reg00_08 %>% select(row_id_2000 = row_id.x, row_id_2008 = row_id.y)
    reg00_12 <- reg00_12 %>% select(row_id_2000 = row_id.x, row_id_2012 = row_id.y)
    reg00_16 <- reg00_16 %>% select(row_id_2000 = row_id.x, row_id_2016 = row_id.y)
    reg00_20 <- reg00_20 %>% select(row_id_2000 = row_id.x, row_id_2020 = row_id.y)
    
    reg04_08 <- reg04_08 %>% select(row_id_2004 = row_id.x, row_id_2008 = row_id.y)
    reg04_12 <- reg04_12 %>% select(row_id_2004 = row_id.x, row_id_2012 = row_id.y)
    reg04_16 <- reg04_16 %>% select(row_id_2004 = row_id.x, row_id_2016 = row_id.y)
    reg04_20 <- reg04_20 %>% select(row_id_2004 = row_id.x, row_id_2020 = row_id.y)
    
    reg08_12 <- reg08_12 %>% select(row_id_2008 = row_id.x, row_id_2012 = row_id.y)
    reg08_16 <- reg08_16 %>% select(row_id_2008 = row_id.x, row_id_2016 = row_id.y)
    reg08_20 <- reg08_20 %>% select(row_id_2008 = row_id.x, row_id_2020 = row_id.y)
    
    reg12_16 <- reg12_16 %>% select(row_id_2012 = row_id.x, row_id_2016 = row_id.y)
    reg12_20 <- reg12_20 %>% select(row_id_2012 = row_id.x, row_id_2020 = row_id.y)
    
    reg16_20 <- reg16_20 %>% select(row_id_2016 = row_id.x, row_id_2020 = row_id.y)
    
    pivot_table <- reg00_04 %>% 
      bind_rows(., reg_2000 %>% select(row_id_2000 = row_id) %>%
                  filter(!row_id_2000 %in% reg00_04$row_id_2000)) %>%
      full_join(.,
                reg04_08 %>%
                  bind_rows(., reg_2004 %>% select(row_id_2004 = row_id) %>%
                              filter(!row_id_2004 %in% reg04_08$row_id_2004))) %>%
      insert_nonconsecutive(., reg00_08, "row_id_2000", "row_id_2008") %>%
      full_join(., reg08_12 %>% 
                  bind_rows(., reg_2008 %>% 
                              select(row_id_2008 = row_id) %>% 
                              filter(!row_id_2008 %in% reg08_12$row_id_2008))) %>% 
      insert_nonconsecutive(., reg04_12, "row_id_2004", "row_id_2012") %>% 
      insert_nonconsecutive(., reg00_12, "row_id_2000", "row_id_2012") %>% 
      full_join(., reg12_16 %>% 
                  bind_rows(., reg_2012 %>% 
                              select(row_id_2012 = row_id) %>% 
                              filter(!row_id_2012 %in% reg12_16$row_id_2012))) %>% 
      insert_nonconsecutive(., reg08_16, "row_id_2008", "row_id_2016") %>% 
      insert_nonconsecutive(., reg04_16, "row_id_2004", "row_id_2016") %>% 
      insert_nonconsecutive(., reg00_16, "row_id_2000", "row_id_2016") %>% 
      full_join(., reg16_20 %>% 
                  bind_rows(., reg_2016 %>% 
                              select(row_id_2016 = row_id) %>% 
                              filter(!row_id_2016 %in% reg16_20$row_id_2016))) %>% 
      insert_nonconsecutive(., reg12_20, "row_id_2012", "row_id_2020") %>% 
      insert_nonconsecutive(., reg08_20, "row_id_2008", "row_id_2020") %>% 
      insert_nonconsecutive(., reg04_20, "row_id_2004", "row_id_2020") %>% 
      insert_nonconsecutive(., reg00_20, "row_id_2000", "row_id_2020") %>% 
      bind_rows(., reg_2020 %>% select(row_id_2020 = row_id) %>%
                  filter(!row_id_2020 %in% c(reg16_20$row_id_2020,
                                             reg12_20$row_id_2020,
                                             reg08_20$row_id_2020,
                                             reg04_20$row_id_2020,
                                             reg00_20$row_id_2020)))
    
    pivot_table_long <- pivot_table %>%
      mutate(person_id = paste0("REG", row_number())) %>% 
      tidyr::pivot_longer(., cols = 1:(ncol(.)-1), names_to = "year",
                          values_to = "row_id") %>%
      filter(!is.na(row_id)) %>%
      mutate(year = stringr::str_extract(year, "[0-9]{4}"))
    
    reg_candidates <- bind_rows(
      reg_2000 %>% mutate(year = "2000"),
      reg_2004 %>% mutate(year = "2004"),
      reg_2008 %>% mutate(year = "2008"),
      reg_2012 %>% mutate(year = "2012"),
      reg_2016 %>% mutate(year = "2016"),
      reg_2020 %>% mutate(year = "2020")
    )
    
    full_join(pivot_table_long, reg_candidates, by = c("row_id", "year")) 
    
  }),
  
  tar_target(
    reg_panel_check, 
    stopifnot(nrow(reg_panel) == (
      nrow(reg_2000) + nrow(reg_2004) + nrow(reg_2008) + nrow(reg_2012) + 
        nrow(reg_2016) + nrow(reg_2020) 
    ))
  )
)

mun_data <- list(
  tar_target(mun_1994, command = {
    parties <- read_excel(here("data", "KV1994", "CVSKV94.xlsx"))
    cpp <- read_cpp(here("data", "KV1994", "CPPKV94.xlsx"), function(x) 
    {x %>% rename(ZKRATKAP8 = ZKRAT)})
    cns <- read_cns(here("data", "KV1994", "CNSKV94.xlsx"), function(x) 
    {x %>% rename(ZKRATKAN8 = ZKRAT)})
    candidates_path <- here("data", "KV1994", "KV94_RK.xlsx")
    year <- as.numeric(stringr::str_extract(candidates_path, "[0-9]{4}"))
    read_excel(candidates_path) %>%
      mutate(JMENO = ifelse(JMENO == "Franišek", "František", JMENO)) %>%
      left_join(., parties, by = c("VSTRANA")) %>%
      left_join(., cpp, by = "PSTRANA") %>%
      left_join(., cns, by = "NSTRANA") %>%
      mutate(row_id = row_number(), 
             ROK_NAROZENI = year - VEK, 
             JMENO = ifelse(JMENO == toupper(JMENO), 
                            stringr::str_to_title(JMENO, locale = "cs"), 
                            JMENO),
             PRIJMENI = ifelse(PRIJMENI == toupper(PRIJMENI), 
                               stringr::str_to_title(PRIJMENI, locale = "cs"), 
                               PRIJMENI)) %>%
      extract_titles_from_last_name()
  }),
  
  tar_target(mun_1998, command = {
    parties <- read_excel(here("data", "KV1998", "CVSKV98.xlsx"))
    cpp <- read_cpp(here("data", "KV1998", "CPPKV98.xlsx"), function(x) 
    {x %>% rename(ZKRATKAP8 = ZKRATKAP)})
    cns <- read_cns(here("data", "KV1998", "CNSKV98.xlsx"))
    candidates_path <- here::here("data", "KV1998", "KV98_RK.xlsx")
    year <- as.numeric(stringr::str_extract(candidates_path, "[0-9]{4}"))
    read_excel(candidates_path) %>%
      left_join(., parties, by = c("VSTRANA")) %>%
      left_join(., cpp, by = "PSTRANA") %>%
      left_join(., cns, by = "NSTRANA") %>%
      rename(TITULY = TITUL) %>%
      mutate(row_id = row_number(), 
             ROK_NAROZENI = year - VEK, 
             TITUL_KATEGORIE = categorize_titles(TITULY), 
             JMENO = ifelse(JMENO == toupper(JMENO), 
                            stringr::str_to_title(JMENO, locale = "cs"), 
                            JMENO),
             PRIJMENI = ifelse(PRIJMENI == toupper(PRIJMENI), 
                               stringr::str_to_title(PRIJMENI, locale = "cs"), 
                               PRIJMENI))
  }),
  
  tar_target(mun_2002, command = {
    # parties <- read_excel(here("data", "KV2002", "CVSKV02.xlsx"))
    cpp <- read_cpp(here("data", "KV2002", "CPPKV02.xlsx"))
    cns <- read_cns(here("data", "KV2002", "CNSKV02.xlsx"))
    candidates_path <- here::here("data", "KV2002", "KV02_RK.xlsx")
    year <- as.numeric(stringr::str_extract(candidates_path, "[0-9]{4}"))
    read_excel(candidates_path) %>%
      # left_join(., parties, by = c("VSTRANA")) %>%
      left_join(., cpp, by = "PSTRANA") %>%
      left_join(., cns, by = "NSTRANA") %>%
      merge_and_recode_titles %>%
      mutate(row_id = row_number(), 
             ROK_NAROZENI = year - VEK, 
             JMENO = ifelse(JMENO == toupper(JMENO), 
                            stringr::str_to_title(JMENO, locale = "cs"), 
                            JMENO),
             PRIJMENI = ifelse(PRIJMENI == toupper(PRIJMENI), 
                               stringr::str_to_title(PRIJMENI, locale = "cs"), 
                               PRIJMENI))
  }),
  
  tar_target(mun_2006, command = {
    parties <- read_municipal_parties(here("data", "KV2006", "KV2006reg20140909_xlsx", "kvros.xlsx"))
    cpp <- read_cpp_xml(here("data", "KV2006", "KV2006ciselniky20140909", "cpp.xml"))
    cns <- read_cns_xml(here("data", "KV2006", "KV2006ciselniky20140909", "cns.xml"))
    read_municipal_candidates(here("data", "KV2006", "KV2006reg20140909_xlsx", "kvrk.xlsx"), 
                              parties, cpp, cns) %>%
      mutate(JMENO = ifelse(JMENO == toupper(JMENO), 
                            stringr::str_to_title(JMENO, locale = "cs"), 
                            JMENO),
             PRIJMENI = ifelse(PRIJMENI == toupper(PRIJMENI), 
                               stringr::str_to_title(PRIJMENI, locale = "cs"), 
                               PRIJMENI))
  }),
  
  tar_target(mun_2010, command = {
    parties <- read_municipal_parties(here("data", "KV2010", "KV2010reg20140903_xlsx", "kvros.xlsx"))
    cpp <- read_cpp_xml(here("data", "KV2010", "KV2010ciselniky20140903", "cpp.xml"))
    cns <- read_cns_xml(here("data", "KV2010", "KV2010ciselniky20140903", "cns.xml"))
    read_municipal_candidates(here("data", "KV2010", "KV2010reg20140903_xlsx", "kvrk.xlsx"), 
                              parties, cpp, cns) %>%
      mutate(JMENO = ifelse(JMENO == toupper(JMENO), 
                            stringr::str_to_title(JMENO, locale = "cs"), 
                            JMENO),
             PRIJMENI = ifelse(PRIJMENI == toupper(PRIJMENI), 
                               stringr::str_to_title(PRIJMENI, locale = "cs"), 
                               PRIJMENI))
  }),
  
  tar_target(mun_2014, command = {
    parties <- read_municipal_parties(here("data", "KV2014", "KV2014reg20141014_xlsx", "kvros.xlsx"))
    cpp <- read_cpp_xml(here("data", "KV2014", "KV2014ciselniky20141008", "cpp.xml"))
    cns <- read_cns_xml(here("data", "KV2014", "KV2014ciselniky20141008", "cns.xml"))
    read_municipal_candidates(here("data", "KV2014", "KV2014reg20141014_xlsx", "kvrk.xlsx"), 
                              parties, cpp, cns) %>%
      mutate(JMENO = ifelse(JMENO == toupper(JMENO), 
                            stringr::str_to_title(JMENO, locale = "cs"), 
                            JMENO),
             PRIJMENI = ifelse(PRIJMENI == toupper(PRIJMENI), 
                               stringr::str_to_title(PRIJMENI, locale = "cs"), 
                               PRIJMENI))
  }),
  
  tar_target(mun_2018, command = {
    parties <- read_municipal_parties(here("data", "KV2018", "KV2018reg20181008_xlsx", "kvros.xlsx"))
    cpp <- read_cpp(here("data", "KV2018", "KV2018ciselniky20181004", "cpp.xlsx"))
    cns <- read_cns(here("data", "KV2018", "KV2018ciselniky20181004", "cns.xlsx"))
    read_municipal_candidates(here("data", "KV2018", "KV2018reg20181008_xlsx", "kvrk.xlsx"), 
                              parties, cpp, cns) %>%
      mutate(PLATNOST = ifelse(PLATNOST == "A", 0, 1), 
             MANDAT = ifelse(MANDAT == "N", 0, 1), 
             JMENO = ifelse(JMENO == toupper(JMENO), 
                            stringr::str_to_title(JMENO, locale = "cs"), 
                            JMENO),
             PRIJMENI = ifelse(PRIJMENI == toupper(PRIJMENI), 
                               stringr::str_to_title(PRIJMENI, locale = "cs"), 
                               PRIJMENI))
  })
)

ep_data <- list(
  tar_target(ep_2004, command = {
    parties <- read_parties_xml(here("data", "EP2004", "EP2004reg", "eprkl.xml"), 
                                function(x) x %>% select(KSTRANA = ESTRANA, ZKRATKAK8 = ZKRATKAE8, NAZEV_STRK = NAZEV_STRE))
    cpp <- read_cpp_xml(here("data", "EP2004", "EP2004ciselniky", "cpp.xml"))
    cns <- read_cns_xml(here("data", "EP2004", "EP2004ciselniky", "cns.xml"))
    read_candidates_xml(here("data", "EP2004", "EP2004reg", "eprk.xml"), 
                        parties, cpp, cns, function(x) {x %>% rename(KSTRANA = ESTRANA)}) %>% 
      mutate(ROK_NAROZENI = 2004 - VEK)
  }),
  
  tar_target(ep_2009, command = {
    parties <- read_parties_xml(here("data", "EP2009", "EP2009reg", "eprkl.xml"), 
                                function(x) x %>% select(KSTRANA = ESTRANA, ZKRATKAK8 = ZKRATKAE8, NAZEV_STRK = NAZEV_STRE))
    cpp <- read_cpp_xml(here("data", "EP2009", "EP2009ciselniky", "cpp.xml"))
    cns <- read_cns_xml(here("data", "EP2009", "EP2009ciselniky", "cns.xml"))
    read_candidates_xml(here("data", "EP2009", "EP2009reg", "eprk.xml"), 
                        parties, cpp, cns, function(x) {x %>% rename(KSTRANA = ESTRANA)}) %>% 
      mutate(ROK_NAROZENI = 2009 - VEK)
  }),
  
  tar_target(ep_2014, command = {
    parties <- read_parties_xml(here("data", "EP2014", "EP2014reg20140525", "eprkl.xml"), 
                                function(x) x %>% select(KSTRANA = ESTRANA, ZKRATKAK8 = ZKRATKAE8, NAZEV_STRK = NAZEV_STRE))
    cpp <- read_cpp_xml(here("data", "EP2014", "EP2014ciselniky20140425", "cpp.xml"))
    cns <- read_cns_xml(here("data", "EP2014", "EP2014ciselniky20140425", "cns.xml"))
    read_candidates_xml(here("data", "EP2014", "EP2014reg20140525", "eprk.xml"), 
                        parties, cpp, cns, function(x) {x %>% rename(KSTRANA = ESTRANA)}) %>% 
      mutate(ROK_NAROZENI = 2014 - VEK)
  }),
  
  tar_target(ep_2019, command = {
    parties <- read_parties(here("data", "EP2019", "EP2019reg20190526_xlsx", "eprkl.xlsx"), 
                            function(x) x %>% select(KSTRANA = ESTRANA, ZKRATKAK8 = ZKRATKAE8, NAZEV_STRK = NAZEV_STRE))
    cpp <- read_cpp(here("data", "EP2019", "EP2019ciselniky20190513", "cpp.xlsx"))
    cns <- read_cns(here("data", "EP2019", "EP2019ciselniky20190513", "cns.xlsx"))
    read_candidates(here("data", "EP2019", "EP2019reg20190526_xlsx", "eprk.xlsx"), 
                    parties, cpp, cns, function(x) {x %>% rename(KSTRANA = ESTRANA) %>%
                        mutate(MANDAT = ifelse(MANDAT == "A", 1, 0))}) %>% 
      mutate(ROK_NAROZENI = 2019 - VEK)
  }), 
  
  tar_target(ep_2024, command = {
    parties <- read_parties(here("data", "EP2024", "EP2024reg20240609_xlsx", "eprkl.xlsx"), 
                            function(x) x %>% select(KSTRANA = ESTRANA, ZKRATKAK8 = ZKRATKAE8, NAZEV_STRK = NAZEV_STRE))
    cpp <- read_cpp(here("data", "EP2024", "EP2024ciselniky20240609", "cpp.xlsx"))
    cns <- read_cns(here("data", "EP2024", "EP2024ciselniky20240609", "cns.xlsx"))
    read_candidates(here("data", "EP2024", "EP2024reg20240609_xlsx", "eprk.xlsx"), 
                    parties, cpp, cns, function(x) {x %>% rename(KSTRANA = ESTRANA) %>%
                        mutate(MANDAT = ifelse(MANDAT == "A", 1, 0))}) %>% 
      mutate(ROK_NAROZENI = 2024 - VEK)
  }),
  
  tar_target(ep_panel, command = {
    ep04_09 <- match_data(ep_2004, ep_2009)
    ep09_14 <- match_data(ep_2009, ep_2014)
    
    ep04_14 <- match_data(
      ep_2004 %>% 
        filter(!row_id %in% ep04_09$row_id.x),
      ep_2014 %>% 
        filter(!row_id %in% ep09_14$row_id.y)
    )
    
    ep14_19 <- match_data(ep_2014, ep_2019)
    ep09_19 <- match_data(
      ep_2009 %>% 
        filter(!row_id %in% ep09_14$row_id.x),
      ep_2019 %>% 
        filter(!row_id %in% ep14_19$row_id.y)
    )
    ep04_19 <- match_data(
      ep_2004 %>% 
        filter(!row_id %in% ep04_09$row_id.x) %>% 
        filter(!row_id %in% ep04_14$row_id.x),
      ep_2019 %>% 
        filter(!row_id %in% ep14_19$row_id.y) %>% 
        filter(!row_id %in% ep09_19$row_id.y)
    )
    
    ep19_24 <- match_data(ep_2019, ep_2024) 
    ep14_24 <- match_data(
      ep_2014 %>% 
        filter(!row_id %in% ep14_19$row_id.x),
      ep_2024 %>% 
        filter(!row_id %in% ep19_24$row_id.y)
    )
    ep09_24 <- match_data(
      ep_2009 %>% 
        filter(!row_id %in% ep09_14$row_id.x) %>% 
        filter(!row_id %in% ep09_19$row_id.x),
      ep_2024 %>% 
        filter(!row_id %in% ep19_24$row_id.y) %>% 
        filter(!row_id %in% ep14_24$row_id.y)
    )
    ep04_24 <- match_data(
      ep_2004 %>% 
        filter(!row_id %in% ep04_09$row_id.x) %>% 
        filter(!row_id %in% ep04_14$row_id.x) %>% 
        filter(!row_id %in% ep04_19$row_id.x),
      ep_2024 %>% 
        filter(!row_id %in% ep19_24$row_id.y) %>% 
        filter(!row_id %in% ep14_24$row_id.y) %>% 
        filter(!row_id %in% ep09_24$row_id.y)
    )
    
    ep04_14 <- ep04_14 %>% select(row_id_2004 = row_id.x, row_id_2014 = row_id.y)
    ep04_19 <- ep04_19 %>% select(row_id_2004 = row_id.x, row_id_2019 = row_id.y)
    ep04_24 <- ep04_24 %>% select(row_id_2004 = row_id.x, row_id_2024 = row_id.y)
    ep09_19 <- ep09_19 %>% select(row_id_2009 = row_id.x, row_id_2019 = row_id.y)
    ep09_24 <- ep09_24 %>% select(row_id_2009 = row_id.x, row_id_2024 = row_id.y)
    ep14_24 <- ep14_24 %>% select(row_id_2014 = row_id.x, row_id_2024 = row_id.y)
    
    pivot_table <- ep04_09 %>% 
      select(row_id_2004 = row_id.x, row_id_2009 = row_id.y) %>%
      bind_rows(., ep_2004 %>% select(row_id_2004 = row_id) %>%
                  filter(!row_id_2004 %in% ep04_09$row_id.x)) %>%
      full_join(.,
                ep09_14 %>% select(row_id_2009 = row_id.x, row_id_2014 = row_id.y) %>%
                  bind_rows(., ep_2009 %>% select(row_id_2009 = row_id) %>%
                              filter(!row_id_2009 %in% ep09_14$row_id.x))) %>%
      insert_nonconsecutive(., ep04_14, "row_id_2004", "row_id_2014") %>%
      full_join(.,
                ep14_19 %>% select(row_id_2014 = row_id.x, row_id_2019 = row_id.y) %>%
                  bind_rows(., ep_2014 %>% select(row_id_2014 = row_id) %>%
                              filter(!row_id_2014 %in% ep14_19$row_id.x))) %>%
      insert_nonconsecutive(., ep04_19, "row_id_2004", "row_id_2019") %>%
      insert_nonconsecutive(., ep09_19, "row_id_2009", "row_id_2019") %>%
      full_join(.,
                ep19_24 %>% select(row_id_2019 = row_id.x, row_id_2024 = row_id.y) %>%
                  bind_rows(., ep_2019 %>% select(row_id_2019 = row_id) %>%
                              filter(!row_id_2019 %in% ep19_24$row_id.x)),
                by = "row_id_2019",
                na_matches = "never"
      ) %>%
      insert_nonconsecutive(., ep04_24, "row_id_2004", "row_id_2024") %>%
      insert_nonconsecutive(., ep09_24, "row_id_2009", "row_id_2024") %>%
      insert_nonconsecutive(., ep14_24, "row_id_2014", "row_id_2024") %>%
      bind_rows(., ep_2024 %>% select(row_id_2024 = row_id) %>%
                  filter(!row_id_2024 %in% ep19_24$row_id.y) %>%
                  filter(!row_id_2024 %in% c(ep04_24$row_id_2024,
                                             ep09_24$row_id_2024,
                                             ep14_24$row_id_2024))) %>%
      mutate(person_id = paste0("EP", row_number()))

    pivot_table_long <- pivot_table %>%
      tidyr::pivot_longer(., cols = 1:(ncol(.)-1), names_to = "year",
                          values_to = "row_id") %>%
      filter(!is.na(row_id)) %>%
      mutate(year = stringr::str_extract(year, "[0-9]{4}"))

    ep_candidates <- bind_rows(
      ep_2004 %>% mutate(year = "2004"),
      ep_2009 %>% mutate(year = "2009"),
      ep_2014 %>% mutate(year = "2014"),
      ep_2019 %>% mutate(year = "2019"),
      ep_2024 %>% mutate(year = "2024")
    )
    
    full_join(pivot_table_long, ep_candidates, by = c("row_id", "year"))
  }),
  
  tar_target(ep_panel_check, 
      stopifnot(nrow(ep_panel) == (
        nrow(ep_2004) + nrow(ep_2009) + nrow(ep_2014) + nrow(ep_2019) + nrow(ep_2024) 
      ))
  )
)

list(
  psp_data, 
  reg_data, 
  mun_data, 
  ep_data
)

