# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.

tar_option_set(packages = c("dplyr", "rvest", "here", # "listr", 
                            "readxl", "readr", "reclin2", 
                            "tidyr", "names.cze"))

tar_source()

# TODO: počet obyvatel obce
# TODO: vazba na ORP
# TODO: DiS. - https://cs.wikipedia.org/wiki/Diplomovan%C3%BD_specialista
# TODO: matchování žen => potřeba rozdělit matchování na muže/ženy

all_data <- list(
  tar_target(all_candidates, {
    bind_rows(
      m_1994 %>% select(JMENO, PRIJMENI),
      m_1998 %>% select(JMENO, PRIJMENI),
      m_2002 %>% select(JMENO, PRIJMENI),
      m_2006 %>% select(JMENO, PRIJMENI),
      m_2010 %>% select(JMENO, PRIJMENI),
      m_2014 %>% select(JMENO, PRIJMENI),
      m_2018 %>% select(JMENO, PRIJMENI),
      m_2022 %>% select(JMENO, PRIJMENI),
      mc_1994 %>% select(JMENO, PRIJMENI),
      mc_1998 %>% select(JMENO, PRIJMENI),
      mc_2002 %>% select(JMENO, PRIJMENI),
      mc_2006 %>% select(JMENO, PRIJMENI),
      mc_2010 %>% select(JMENO, PRIJMENI),
      mc_2014 %>% select(JMENO, PRIJMENI),
      mc_2018 %>% select(JMENO, PRIJMENI),
      mc_2022 %>% select(JMENO, PRIJMENI),
      reg_2000 %>% select(JMENO, PRIJMENI),
      reg_2004 %>% select(JMENO, PRIJMENI),
      reg_2008 %>% select(JMENO, PRIJMENI),
      reg_2012 %>% select(JMENO, PRIJMENI),
      reg_2016 %>% select(JMENO, PRIJMENI),
      reg_2020 %>% select(JMENO, PRIJMENI),
      psp_1996 %>% select(JMENO, PRIJMENI),
      psp_1998 %>% select(JMENO, PRIJMENI),
      psp_2002 %>% select(JMENO, PRIJMENI),
      psp_2006 %>% select(JMENO, PRIJMENI),
      psp_2010 %>% select(JMENO, PRIJMENI),
      psp_2013 %>% select(JMENO, PRIJMENI),
      psp_2017 %>% select(JMENO, PRIJMENI),
      psp_2021 %>% select(JMENO, PRIJMENI),
      senate_df %>% select(JMENO, PRIJMENI),
      ep_2004 %>% select(JMENO, PRIJMENI),
      ep_2009 %>% select(JMENO, PRIJMENI),
      ep_2014 %>% select(JMENO, PRIJMENI),
      ep_2019 %>% select(JMENO, PRIJMENI),
      ep_2024 %>% select(JMENO, PRIJMENI)
    ) %>% unique()
  }), 
  
  tar_target(
    multiple_last_names, {
      all_candidates %>% 
        filter(grepl("\\s|-", PRIJMENI))
    }
  )
)

# Chamber of Deputies ----------------------------------------
psp_data <- list(
  tar_target(kraje_do_2000, {
    tribble(
      ~VOLKRAJ, ~KRAJ_NAZEV,
      3100, "Hlavní město Praha",  
      3200, "Středočeský kraj",  
      3300, "Jihočeský kraj",  
      3400, "Západočeský kraj", 
      3500, "Severočeský kraj", 
      3600, "Východočeský kraj", 
      3700, "Jihomoravský kraj", 
      3800, "Severomoravský kraj")
  }),
  
  tar_target(kraje_po_2000, {
    tribble(
      ~VOLKRAJ, ~KRAJ_NAZEV,
      1, "Hlavní město Praha",
      2, "Středočeský kraj",
      3, "Jihočeský kraj",
      4, "Plzeňský kraj",
      5, "Karlovarský kraj",
      6, "Ústecký kraj",
      7, "Liberecký kraj",
      8, "Královéhradecký kraj",
      9, "Pardubický kraj",
      10, "Kraj Vysočina",
      11, "Jihomoravský kraj",
      12, "Olomoucký kraj",
      13, "Zlínský kraj",
      14, "Moravskoslezský kraj"
    )
  }),
  
  tar_target(psp_1996, command = {
    cpp <- read_cpp(here("data", "PS1996", "CPPPS96.xlsx"), function(x) {x %>% rename(ZKRATKAP8 = ZKRATKAP)})
    read_excel(here("data", "PS1996", "PS96-RK.xlsx")) %>%
      clean_ps(., VSTRANA_MAP_96) %>%
      left_join(., cpp, by = "PSTRANA") %>%
      merge_and_recode_titles %>%
      mutate(row_id = row_number(), 
             ROK_NAROZENI = 1996 - VEK) %>% 
      rename(VOLKRAJ = KRAJ) %>% 
      left_join(., kraje_do_2000, by = "VOLKRAJ") %>% 
      categorize_sex
  }),
  
  tar_target(psp_1998, command = {
    cpp <- read_cpp(here("data", "PS1998", "CPPPS98.xlsx"), function(x) {x %>% rename(ZKRATKAP8 = ZKRATKAP)})
    read_excel(here("data", "PS1998", "PS98-RK.xlsx")) %>%
      clean_ps(., VSTRANA_MAP_98) %>%
      left_join(., cpp, by = "PSTRANA") %>%
      merge_and_recode_titles %>%
      mutate(row_id = row_number(), 
             ROK_NAROZENI = 1998 - VEK) %>% 
      rename(VOLKRAJ = KRAJ) %>% 
      left_join(., kraje_do_2000, by = "VOLKRAJ") %>% 
      categorize_sex
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
             ROK_NAROZENI = 2002 - VEK) %>% 
      left_join(., kraje_po_2000, by = "VOLKRAJ") %>% 
      categorize_sex
  }),
  
  tar_target(psp_2006, command = {
    psp_parties <- read_parties(here("data", "PS2006", "PS2006reg2006", "PSRKL.xlsx"), 
                                clean_dbf_excel_colnames)
    cpp <- read_cpp(here("data", "PS2006", "PS2006ciselniky2006", "CPP.xlsx"), 
                    clean_dbf_excel_colnames)
    cns <- read_cns(here("data", "PS2006", "PS2006ciselniky2006", "CNS.xlsx"), 
                    clean_dbf_excel_colnames)
    read_candidates(here("data", "PS2006", "PS2006reg2006", "PSRK.xlsx"), 
                    psp_parties, cpp, cns, clean_dbf_excel_colnames) %>% 
      left_join(., kraje_po_2000, by = "VOLKRAJ") %>% 
      categorize_sex
  }),
  
  tar_target(psp_2010, command = {
    psp_parties <- read_parties(here("data", "PS2010", "PS2010reg2010", "PSRKL.xlsx"), 
                                clean_dbf_excel_colnames)
    cpp <- read_cpp(here("data", "PS2010", "PS2010ciselniky2010", "CPP.xlsx"), 
                    clean_dbf_excel_colnames)
    cns <- read_cns(here("data", "PS2010", "PS2010ciselniky2010", "CNS.xlsx"), 
                    clean_dbf_excel_colnames)
    read_candidates(here("data", "PS2010", "PS2010reg2010", "PSRK.xlsx"), 
                    psp_parties, cpp, cns, clean_dbf_excel_colnames) %>% 
      left_join(., kraje_po_2000, by = "VOLKRAJ") %>% 
      categorize_sex
  }),
  
  tar_target(psp_2013, command = {
    psp_parties <- read_parties(here("data", "PS2013", "PS2013reg20131026", "PSRKL.xlsx"), 
                                clean_dbf_excel_colnames)
    cpp <- read_cpp(here("data", "PS2013", "PS2013ciselniky20131021", "CPP.xlsx"), 
                    clean_dbf_excel_colnames)
    cns <- read_cns(here("data", "PS2013", "PS2013ciselniky20131021", "CNS.xlsx"), 
                    clean_dbf_excel_colnames)
    read_candidates(here("data", "PS2013", "PS2013reg20131026", "PSRK.xlsx"), 
                    psp_parties, cpp, cns, clean_dbf_excel_colnames) %>% 
      left_join(., kraje_po_2000, by = "VOLKRAJ") %>% 
      categorize_sex
  }),
  
  tar_target(psp_2017, command = {
    psp_parties <- read_parties(here("data", "PS2017NSS", "PS2017reg20171122_xlsx", "psrkl.xlsx"))
    cpp <- read_cpp(here("data", "PS2017NSS", "PS2017ciselniky20170905", "cpp.xlsx"))
    cns <- read_cns(here("data", "PS2017NSS", "PS2017ciselniky20170905", "cns.xlsx"))
    read_candidates(here("data", "PS2017NSS", "PS2017reg20171122_xlsx", "psrk.xlsx"), 
                    psp_parties, cpp, cns) %>%
      mutate(PLATNOST = ifelse(PLATNOST == "A", 0, 1), 
             MANDAT = ifelse(MANDAT == "A", 1, 0)) %>% 
      left_join(., kraje_po_2000, by = "VOLKRAJ") %>% 
      categorize_sex
  }),
  
  tar_target(psp_2021, command = {
    psp_parties <- read_parties(here("data", "PS2021", "PS2021reg20211010_xlsx", "psrkl.xlsx"))
    cpp <- read_cpp(here("data", "PS2021", "PS2021ciselniky20210824", "cpp.xlsx"))
    cns <- read_cns(here("data", "PS2021", "PS2021ciselniky20210824", "cns.xlsx"))
    read_candidates(here("data", "PS2021", "PS2021reg20211010_xlsx", "psrk.xlsx"), 
                    psp_parties, cpp, cns) %>%
      mutate(PLATNOST = ifelse(PLATNOST == "A", 0, 1), 
             MANDAT = ifelse(MANDAT == "A", 1, 0)) %>% 
      left_join(., kraje_po_2000, by = "VOLKRAJ") %>% 
      categorize_sex
  }), 
  
  tar_target(psp_panel, {
    psp_96_98 <- match_psp_data(psp_1996, psp_1998)
    
    psp_98_02 <- match_psp_data(psp_1998, psp_2002)
    
    psp_96_02 <- match_psp_data(
      psp_1996 %>% 
        filter(!row_id %in% psp_96_98$row_id.x),
      psp_2002 %>% 
        filter(!row_id %in% psp_98_02$row_id.y)
    )
    
    psp_02_06 <- match_psp_data(psp_2002, psp_2006)
    
    psp_98_06 <- match_psp_data(
      psp_1998 %>% 
        filter(!row_id %in% psp_98_02$row_id.x),
      psp_2006 %>% 
        filter(!row_id %in% psp_02_06$row_id.y)
    )
    
    psp_96_06 <- match_psp_data(
      psp_1996 %>% 
        filter(!row_id %in% psp_96_98$row_id.x) %>% 
        filter(!row_id %in% psp_96_02$row_id.x),
      psp_2006 %>% 
        filter(!row_id %in% psp_02_06$row_id.y) %>% 
        filter(!row_id %in% psp_98_06$row_id.y)
    )
    
    psp_06_10 <- match_psp_data(psp_2006, psp_2010)
    
    psp_02_10 <- match_psp_data(
      psp_2002 %>% 
        filter(!row_id %in% psp_02_06$row_id.x),
      psp_2010 %>% 
        filter(!row_id %in% psp_06_10$row_id.y)
    )
    
    psp_98_10 <- match_psp_data(
      psp_1998 %>% 
        filter(!row_id %in% psp_98_02$row_id.x) %>% 
        filter(!row_id %in% psp_98_06$row_id.x),
      psp_2010 %>% 
        filter(!row_id %in% psp_06_10$row_id.y) %>% 
        filter(!row_id %in% psp_02_10$row_id.y)
    )
    
    psp_96_10 <- match_psp_data(
      psp_1996 %>% 
        filter(!row_id %in% psp_96_98$row_id.x) %>% 
        filter(!row_id %in% psp_96_02$row_id.x) %>% 
        filter(!row_id %in% psp_96_06$row_id.x),
      psp_2010 %>% 
        filter(!row_id %in% psp_06_10$row_id.y) %>% 
        filter(!row_id %in% psp_02_10$row_id.y) %>% 
        filter(!row_id %in% psp_98_10$row_id.y)
    )
    
    psp_10_13 <- match_psp_data(psp_2010, psp_2013)
    
    psp_06_13 <- match_psp_data(
      psp_2006 %>% 
        filter(!row_id %in% psp_06_10$row_id.x), 
      psp_2013 %>% 
        filter(!row_id %in% psp_10_13$row_id.y)    
    )
    
    psp_02_13 <- match_psp_data(
      psp_2002 %>% 
        filter(!row_id %in% psp_02_06$row_id.x) %>% 
        filter(!row_id %in% psp_02_10$row_id.x), 
      psp_2013 %>% 
        filter(!row_id %in% psp_10_13$row_id.y) %>% 
        filter(!row_id %in% psp_06_13$row_id.y)
    )
    
    psp_98_13 <- match_psp_data(
      psp_1998 %>% 
        filter(!row_id %in% psp_98_02$row_id.x) %>% 
        filter(!row_id %in% psp_98_06$row_id.x) %>% 
        filter(!row_id %in% psp_98_10$row_id.x), 
      psp_2013 %>% 
        filter(!row_id %in% psp_10_13$row_id.y) %>% 
        filter(!row_id %in% psp_06_13$row_id.y) %>% 
        filter(!row_id %in% psp_02_13$row_id.y)
    )
    
    psp_96_13 <- match_psp_data(
      psp_1996 %>% 
        filter(!row_id %in% psp_96_98$row_id.x) %>% 
        filter(!row_id %in% psp_96_02$row_id.x) %>% 
        filter(!row_id %in% psp_96_06$row_id.x) %>% 
        filter(!row_id %in% psp_96_10$row_id.x), 
      psp_2013 %>% 
        filter(!row_id %in% psp_10_13$row_id.y) %>% 
        filter(!row_id %in% psp_06_13$row_id.y) %>% 
        filter(!row_id %in% psp_02_13$row_id.y) %>% 
        filter(!row_id %in% psp_98_13$row_id.y)
    )
    
    psp_13_17 <- match_psp_data(psp_2013, psp_2017)
    
    psp_10_17 <- match_psp_data(
      psp_2010 %>% 
        filter(!row_id %in% psp_10_13$row_id.x), 
      psp_2017 %>% 
        filter(!row_id %in% psp_13_17$row_id.y)
    )
    
    psp_06_17 <- match_psp_data(
      psp_2006 %>% 
        filter(!row_id %in% psp_06_10$row_id.x) %>% 
        filter(!row_id %in% psp_06_13$row_id.x), 
      psp_2017 %>% 
        filter(!row_id %in% psp_13_17$row_id.y) %>% 
        filter(!row_id %in% psp_10_17$row_id.y)
    )
    
    psp_02_17 <- match_psp_data(
      psp_2002 %>% 
        filter(!row_id %in% psp_02_06$row_id.x) %>% 
        filter(!row_id %in% psp_02_10$row_id.x) %>% 
        filter(!row_id %in% psp_02_13$row_id.x), 
      psp_2017 %>% 
        filter(!row_id %in% psp_13_17$row_id.y) %>% 
        filter(!row_id %in% psp_10_17$row_id.y) %>% 
        filter(!row_id %in% psp_06_17$row_id.y)
    )
    
    psp_98_17 <- match_psp_data(
      psp_1998 %>% 
        filter(!row_id %in% psp_98_02$row_id.x) %>% 
        filter(!row_id %in% psp_98_06$row_id.x) %>% 
        filter(!row_id %in% psp_98_10$row_id.x) %>% 
        filter(!row_id %in% psp_98_13$row_id.x), 
      psp_2017 %>% 
        filter(!row_id %in% psp_13_17$row_id.y) %>% 
        filter(!row_id %in% psp_10_17$row_id.y) %>% 
        filter(!row_id %in% psp_06_17$row_id.y) %>% 
        filter(!row_id %in% psp_02_17$row_id.y)
    )
    
    psp_96_17 <- match_psp_data(
      psp_1996 %>% 
        filter(!row_id %in% psp_96_98$row_id.x) %>% 
        filter(!row_id %in% psp_96_02$row_id.x) %>% 
        filter(!row_id %in% psp_96_06$row_id.x) %>% 
        filter(!row_id %in% psp_96_10$row_id.x) %>% 
        filter(!row_id %in% psp_96_13$row_id.x), 
      psp_2017 %>% 
        filter(!row_id %in% psp_13_17$row_id.y) %>% 
        filter(!row_id %in% psp_10_17$row_id.y) %>% 
        filter(!row_id %in% psp_06_17$row_id.y) %>% 
        filter(!row_id %in% psp_02_17$row_id.y) %>% 
        filter(!row_id %in% psp_98_17$row_id.y)
    )
    
    psp_17_21 <- match_psp_data(psp_2017, psp_2021)
    
    psp_13_21 <- match_psp_data(
      psp_2013 %>% 
        filter(!row_id %in% psp_13_17$row_id.x), 
      psp_2021 %>% 
        filter(!row_id %in% psp_17_21$row_id.y)
    )
    
    psp_10_21 <- match_psp_data(
      psp_2010 %>% 
        filter(!row_id %in% psp_10_17$row_id.x) %>% 
        filter(!row_id %in% psp_10_13$row_id.x),
      psp_2021 %>% 
        filter(!row_id %in% psp_17_21$row_id.y) %>% 
        filter(!row_id %in% psp_13_21$row_id.y)
    )
    
    psp_06_21 <- match_psp_data(
      psp_2006 %>% 
        filter(!row_id %in% psp_06_10$row_id.x) %>% 
        filter(!row_id %in% psp_06_13$row_id.x) %>% 
        filter(!row_id %in% psp_06_17$row_id.x),
      psp_2021 %>% 
        filter(!row_id %in% psp_17_21$row_id.y) %>% 
        filter(!row_id %in% psp_13_21$row_id.y) %>% 
        filter(!row_id %in% psp_10_21$row_id.y)
    )
    
    psp_02_21 <- match_psp_data(
      psp_2002 %>% 
        filter(!row_id %in% psp_02_17$row_id.x) %>% 
        filter(!row_id %in% psp_02_13$row_id.x) %>% 
        filter(!row_id %in% psp_02_10$row_id.x) %>% 
        filter(!row_id %in% psp_02_06$row_id.x),
      psp_2021 %>% 
        filter(!row_id %in% psp_17_21$row_id.y) %>% 
        filter(!row_id %in% psp_13_21$row_id.y) %>% 
        filter(!row_id %in% psp_10_21$row_id.y) %>% 
        filter(!row_id %in% psp_06_21$row_id.y)
    )
    
    psp_98_21 <- match_psp_data(
      psp_1998 %>% 
        filter(!row_id %in% psp_98_17$row_id.x) %>% 
        filter(!row_id %in% psp_98_13$row_id.x) %>% 
        filter(!row_id %in% psp_98_10$row_id.x) %>% 
        filter(!row_id %in% psp_98_06$row_id.x) %>% 
        filter(!row_id %in% psp_98_02$row_id.x),
      psp_2021 %>% 
        filter(!row_id %in% psp_17_21$row_id.y) %>% 
        filter(!row_id %in% psp_13_21$row_id.y) %>% 
        filter(!row_id %in% psp_10_21$row_id.y) %>% 
        filter(!row_id %in% psp_06_21$row_id.y) %>% 
        filter(!row_id %in% psp_02_21$row_id.y)
    )
    
    psp_96_21 <- match_psp_data(
      psp_1996 %>% 
        filter(!row_id %in% psp_96_17$row_id.x) %>% 
        filter(!row_id %in% psp_96_13$row_id.x) %>% 
        filter(!row_id %in% psp_96_10$row_id.x) %>% 
        filter(!row_id %in% psp_96_06$row_id.x) %>% 
        filter(!row_id %in% psp_96_02$row_id.x) %>% 
        filter(!row_id %in% psp_96_98$row_id.x),
      psp_2021 %>% 
        filter(!row_id %in% psp_17_21$row_id.y) %>% 
        filter(!row_id %in% psp_13_21$row_id.y) %>% 
        filter(!row_id %in% psp_10_21$row_id.y) %>% 
        filter(!row_id %in% psp_06_21$row_id.y) %>% 
        filter(!row_id %in% psp_02_21$row_id.y) %>% 
        filter(!row_id %in% psp_98_21$row_id.y)
    )
    
    ##
    psp_96_98b <- psp_96_98 %>% select(row_id_1996 = row_id.x, row_id_1998 = row_id.y)
    psp_96_02b <- psp_96_02 %>% select(row_id_1996 = row_id.x, row_id_2002 = row_id.y)
    psp_96_06b <- psp_96_06 %>% select(row_id_1996 = row_id.x, row_id_2006 = row_id.y)
    psp_96_10b <- psp_96_10 %>% select(row_id_1996 = row_id.x, row_id_2010 = row_id.y)
    psp_96_13b <- psp_96_13 %>% select(row_id_1996 = row_id.x, row_id_2013 = row_id.y)
    psp_96_17b <- psp_96_17 %>% select(row_id_1996 = row_id.x, row_id_2017 = row_id.y)
    psp_96_21b <- psp_96_21 %>% select(row_id_1996 = row_id.x, row_id_2021 = row_id.y)
    
    psp_98_02b <- psp_98_02 %>% select(row_id_1998 = row_id.x, row_id_2002 = row_id.y)
    psp_98_06b <- psp_98_06 %>% select(row_id_1998 = row_id.x, row_id_2006 = row_id.y)
    psp_98_10b <- psp_98_10 %>% select(row_id_1998 = row_id.x, row_id_2010 = row_id.y)
    psp_98_13b <- psp_98_13 %>% select(row_id_1998 = row_id.x, row_id_2013 = row_id.y)
    psp_98_17b <- psp_98_17 %>% select(row_id_1998 = row_id.x, row_id_2017 = row_id.y)
    psp_98_21b <- psp_98_21 %>% select(row_id_1998 = row_id.x, row_id_2021 = row_id.y)
    
    psp_02_06b <- psp_02_06 %>% select(row_id_2002 = row_id.x, row_id_2006 = row_id.y)
    psp_02_10b <- psp_02_10 %>% select(row_id_2002 = row_id.x, row_id_2010 = row_id.y)
    psp_02_13b <- psp_02_13 %>% select(row_id_2002 = row_id.x, row_id_2013 = row_id.y)
    psp_02_17b <- psp_02_17 %>% select(row_id_2002 = row_id.x, row_id_2017 = row_id.y)
    psp_02_21b <- psp_02_21 %>% select(row_id_2002 = row_id.x, row_id_2021 = row_id.y)
    
    psp_06_10b <- psp_06_10 %>% select(row_id_2006 = row_id.x, row_id_2010 = row_id.y)
    psp_06_13b <- psp_06_13 %>% select(row_id_2006 = row_id.x, row_id_2013 = row_id.y)
    psp_06_17b <- psp_06_17 %>% select(row_id_2006 = row_id.x, row_id_2017 = row_id.y)
    psp_06_21b <- psp_06_21 %>% select(row_id_2006 = row_id.x, row_id_2021 = row_id.y)
    
    psp_10_13b <- psp_10_13 %>% select(row_id_2010 = row_id.x, row_id_2013 = row_id.y)
    psp_10_17b <- psp_10_17 %>% select(row_id_2010 = row_id.x, row_id_2017 = row_id.y)
    psp_10_21b <- psp_10_21 %>% select(row_id_2010 = row_id.x, row_id_2021 = row_id.y)
    
    psp_13_17b <- psp_13_17 %>% select(row_id_2013 = row_id.x, row_id_2017 = row_id.y)
    psp_13_21b <- psp_13_21 %>% select(row_id_2013 = row_id.x, row_id_2021 = row_id.y)
    
    psp_17_21b <- psp_17_21 %>% select(row_id_2017 = row_id.x, row_id_2021 = row_id.y)
    
    pivot_table <- psp_96_98b %>% 
      bind_rows(., psp_1996 %>% select(row_id_1996 = row_id) %>%
                  filter(!row_id_1996 %in% psp_96_98b$row_id_1996)) %>%
      full_join(.,
                psp_98_02b %>%
                  bind_rows(., psp_1998 %>% select(row_id_1998 = row_id) %>%
                              filter(!row_id_1998 %in% psp_98_02b$row_id_1998))) %>%
      insert_nonconsecutive(., psp_96_02b, "row_id_1996", "row_id_2002") %>%
      full_join(., psp_02_06b %>% 
                  bind_rows(., psp_2002 %>% 
                              select(row_id_2002 = row_id) %>% 
                              filter(!row_id_2002 %in% psp_02_06b$row_id_2002))) %>% 
      insert_nonconsecutive(., psp_98_06b, "row_id_1998", "row_id_2006") %>% 
      insert_nonconsecutive(., psp_96_06b, "row_id_1996", "row_id_2006") %>% 
      full_join(., psp_06_10b %>% 
                  bind_rows(., psp_2006 %>% 
                              select(row_id_2006 = row_id) %>% 
                              filter(!row_id_2006 %in% psp_06_10b$row_id_2006))) %>% 
      insert_nonconsecutive(., psp_02_10b, "row_id_2002", "row_id_2010") %>% 
      insert_nonconsecutive(., psp_98_10b, "row_id_1998", "row_id_2010") %>% 
      insert_nonconsecutive(., psp_96_10b, "row_id_1996", "row_id_2010") %>% 
      full_join(., psp_10_13b %>% 
                  bind_rows(., psp_2010 %>% 
                              select(row_id_2010 = row_id) %>% 
                              filter(!row_id_2010 %in% psp_10_13b$row_id_2010))) %>% 
      insert_nonconsecutive(., psp_06_13b, "row_id_2006", "row_id_2013") %>% 
      insert_nonconsecutive(., psp_02_13b, "row_id_2002", "row_id_2013") %>% 
      insert_nonconsecutive(., psp_98_13b, "row_id_1998", "row_id_2013") %>% 
      insert_nonconsecutive(., psp_96_13b, "row_id_1996", "row_id_2013") %>% 
      full_join(., psp_13_17b %>% 
                  bind_rows(., psp_2013 %>% 
                              select(row_id_2013 = row_id) %>% 
                              filter(!row_id_2013 %in% psp_13_17b$row_id_2013))) %>% 
      insert_nonconsecutive(., psp_10_17b, "row_id_2010", "row_id_2017") %>% 
      insert_nonconsecutive(., psp_06_17b, "row_id_2006", "row_id_2017") %>% 
      insert_nonconsecutive(., psp_02_17b, "row_id_2002", "row_id_2017") %>% 
      insert_nonconsecutive(., psp_98_17b, "row_id_1998", "row_id_2017") %>% 
      insert_nonconsecutive(., psp_96_17b, "row_id_1996", "row_id_2017") %>% 
      full_join(., psp_17_21b %>% 
                  bind_rows(., psp_2017 %>% 
                              select(row_id_2017 = row_id) %>% 
                              filter(!row_id_2017 %in% psp_17_21b$row_id_2017))) %>% 
      insert_nonconsecutive(., psp_13_21b, "row_id_2013", "row_id_2021") %>%
      insert_nonconsecutive(., psp_10_21b, "row_id_2010", "row_id_2021") %>% 
      insert_nonconsecutive(., psp_06_21b, "row_id_2006", "row_id_2021") %>% 
      insert_nonconsecutive(., psp_02_21b, "row_id_2002", "row_id_2021") %>% 
      insert_nonconsecutive(., psp_98_21b, "row_id_1998", "row_id_2021") %>% 
      insert_nonconsecutive(., psp_96_21b, "row_id_1996", "row_id_2021") %>% 
      bind_rows(., psp_2021 %>% select(row_id_2021 = row_id) %>%
                  filter(!row_id_2021 %in% c(psp_17_21b$row_id_2021,
                                             psp_13_21b$row_id_2021,
                                             psp_10_21b$row_id_2021,
                                             psp_06_21b$row_id_2021,
                                             psp_02_21b$row_id_2021, 
                                             psp_98_21b$row_id_2021, 
                                             psp_96_21b$row_id_2021)))
    
    pivot_table_long <- pivot_table %>%
      mutate(person_id = paste0("PSP", row_number())) %>% 
      tidyr::pivot_longer(., cols = 1:(ncol(.)-1), names_to = "year",
                          values_to = "row_id") %>%
      filter(!is.na(row_id)) %>%
      mutate(year = stringr::str_extract(year, "[0-9]{4}"))
    
    psp_candidates <- bind_rows(
      psp_1996 %>% mutate(year = "1996"),
      psp_1998 %>% mutate(year = "1998"),
      psp_2002 %>% mutate(year = "2002"),
      psp_2006 %>% mutate(year = "2006"),
      psp_2010 %>% mutate(year = "2010"),
      psp_2013 %>% mutate(year = "2013"),
      psp_2017 %>% mutate(year = "2017"),
      psp_2021 %>% mutate(year = "2021")
    )
    
    full_join(pivot_table_long, psp_candidates, by = c("row_id", "year"))
  }), 
  
  tar_target(
    psp_panel_check, 
    stopifnot(nrow(psp_panel) == (
      nrow(psp_1996) + nrow(psp_1998) + nrow(psp_2002) + nrow(psp_2006) + 
        nrow(psp_2010) + nrow(psp_2013) + nrow(psp_2017) + nrow(psp_2021) 
    ))
  ), 
  
  tar_target(
    psp_panel_file_dta, 
    haven::write_dta(psp_panel, "output/psp_panel.dta")
  ),
  
  tar_target(
    psp_panel_file_rds, 
    saveRDS(psp_panel, "output/psp_panel.rds")
  )
)

# Regional elections --------------------------------------
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
      rename(NAZEV_STRK = KSTRANA_NAZEV) %>% 
      categorize_sex
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
      rename(NAZEV_STRK = KSTRANA_NAZEV) %>% 
      categorize_sex
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
                    parties, cpp, cns, clean_dbf_excel_colnames) %>% 
      categorize_sex %>% 
      select(-PORADIHAHR)
      
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
                    parties, cpp, cns, clean_dbf_excel_colnames) %>% 
      categorize_sex %>% 
      select(-PORADIHAHR)
  }),
  
  tar_target(reg_2016, command = {
    parties <- read_parties(here("data", "KZ2016", "KZ2016reg20161008_xlsx", "kzrkl.xlsx")) %>% 
      unique
    cpp <- read_cpp(here("data", "KZ2016", "KZ2016ciselniky20161007", "cpp.xlsx"))
    cns <- read_cns(here("data", "KZ2016", "KZ2016ciselniky20161007", "cns.xlsx"))
    read_candidates(here("data", "KZ2016", "KZ2016reg20161008_xlsx", "kzrk.xlsx"), 
                    parties, cpp, cns) %>%
      mutate(PLATNOST = ifelse(PLATNOST == "A", 0, 1), 
             MANDAT = ifelse(MANDAT == "A", 1, 0)) %>% 
      categorize_sex
  }),
  
  tar_target(reg_2020, command = {
    parties <- read_parties(here("data", "KZ2020", "KZ2020reg20201004a_xlsx", "kzrkl.xlsx")) %>% 
      unique
    cpp <- read_cpp(here("data", "KZ2020", "KZ2020ciselniky20201002", "cpp.xlsx"))
    cns <- read_cns(here("data", "KZ2020", "KZ2020ciselniky20201002", "cns.xlsx"))
    read_candidates(here("data", "KZ2020", "KZ2020reg20201004a_xlsx", "kzrk.xlsx"), 
                    parties, cpp, cns) %>%
      mutate(PLATNOST = ifelse(PLATNOST == "A", 0, 1), 
             MANDAT = ifelse(MANDAT == "A", 1, 0)) %>% 
      categorize_sex
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

# Municipal elections -------------------------------------
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
                               PRIJMENI), 
             POVOLANI = NA, 
             BYDLISTEN = NA) %>%
      extract_titles_from_last_name() %>% 
      mutate(
        TITUL_KATEGORIE = if_else(PRIJMENI == "Pernes.PhDr.", 
                                  factor("Doctor", levels = c("No title", "Bachelor", "Master",
                                                              "Doctor", "Associate Professor (docent)", "Professor"),
                                         ordered = TRUE), TITUL_KATEGORIE),
        TITULY = if_else(PRIJMENI == "Pernes.PhDr.", "PhDr.", TITULY),
        PRIJMENI = if_else(PRIJMENI == "Pernes.PhDr.", "Pernes", PRIJMENI)
      ) %>% 
      categorize_sex
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
                               PRIJMENI)) %>% 
      categorize_sex
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
                               PRIJMENI)) %>% 
      categorize_sex
  }),
  
  tar_target(mun_2006, command = {
    parties <- read_municipal_parties(here("data", "KV2006", "KV2006reg20140909_xlsx", "kvros.xlsx"))
    coco <- read_excel(here("data", "KV2006", "KV2006reg20140909_xlsx", "kvrzcoco.xlsx")) %>% 
      select(KODZASTUP, TYPZASTUP) %>% 
      unique()
    cpp <- read_cpp_xml(here("data", "KV2006", "KV2006ciselniky20140909", "cpp.xml"))
    cns <- read_cns_xml(here("data", "KV2006", "KV2006ciselniky20140909", "cns.xml"))
    read_municipal_candidates(here("data", "KV2006", "KV2006reg20140909_xlsx", "kvrk.xlsx"), 
                              parties, cpp, cns) %>%
      left_join(., coco, by = "KODZASTUP") %>% 
      mutate(JMENO = ifelse(JMENO == toupper(JMENO), 
                            stringr::str_to_title(JMENO, locale = "cs"), 
                            JMENO),
             PRIJMENI = ifelse(PRIJMENI == toupper(PRIJMENI), 
                               stringr::str_to_title(PRIJMENI, locale = "cs"), 
                               PRIJMENI)) %>% 
      categorize_sex
  }),
  
  tar_target(mun_2010, command = {
    parties <- read_municipal_parties(here("data", "KV2010", "KV2010reg20140903_xlsx", "kvros.xlsx"))
    cpp <- read_cpp_xml(here("data", "KV2010", "KV2010ciselniky20140903", "cpp.xml"))
    cns <- read_cns_xml(here("data", "KV2010", "KV2010ciselniky20140903", "cns.xml"))
    coco <- read_excel(here("data", "KV2010", "KV2010reg20140903_xlsx", "kvrzcoco.xlsx")) %>% 
      select(KODZASTUP, TYPZASTUP) %>% 
      unique()
    
    read_municipal_candidates(here("data", "KV2010", "KV2010reg20140903_xlsx", "kvrk.xlsx"), 
                              parties, cpp, cns) %>%
      left_join(., coco, by = "KODZASTUP") %>% 
      mutate(JMENO = ifelse(JMENO == toupper(JMENO), 
                            stringr::str_to_title(JMENO, locale = "cs"), 
                            JMENO),
             PRIJMENI = ifelse(PRIJMENI == toupper(PRIJMENI), 
                               stringr::str_to_title(PRIJMENI, locale = "cs"), 
                               PRIJMENI)) %>% 
      categorize_sex
  }),
  
  tar_target(mun_2014, command = {
    parties <- read_municipal_parties(here("data", "KV2014", "KV2014reg20141014_xlsx", "kvros.xlsx"))
    cpp <- read_cpp_xml(here("data", "KV2014", "KV2014ciselniky20141008", "cpp.xml"))
    cns <- read_cns_xml(here("data", "KV2014", "KV2014ciselniky20141008", "cns.xml"))
    coco <- read_excel(here("data", "KV2014", "KV2014reg20141014_xlsx", "kvrzcoco.xlsx")) %>% 
      select(KODZASTUP, TYPZASTUP) %>% 
      unique()
    
    read_municipal_candidates(here("data", "KV2014", "KV2014reg20141014_xlsx", "kvrk.xlsx"), 
                              parties, cpp, cns) %>%
      left_join(., coco, by = "KODZASTUP") %>% 
      mutate(JMENO = ifelse(JMENO == toupper(JMENO), 
                            stringr::str_to_title(JMENO, locale = "cs"), 
                            JMENO),
             PRIJMENI = ifelse(PRIJMENI == toupper(PRIJMENI), 
                               stringr::str_to_title(PRIJMENI, locale = "cs"), 
                               PRIJMENI)) %>% 
      categorize_sex
  }),
  
  tar_target(mun_2018, command = {
    parties <- read_municipal_parties(here("data", "KV2018", "KV2018reg20181008_xlsx", "kvros.xlsx"))
    cpp <- read_cpp(here("data", "KV2018", "KV2018ciselniky20181004", "cpp.xlsx"))
    cns <- read_cns(here("data", "KV2018", "KV2018ciselniky20181004", "cns.xlsx"))
    coco <- read_excel(here("data", "KV2018", "KV2018reg20181008_xlsx", "kvrzcoco.xlsx")) %>% 
      select(KODZASTUP, TYPZASTUP) %>% 
      unique()
    
    read_municipal_candidates(here("data", "KV2018", "KV2018reg20181008_xlsx", "kvrk.xlsx"), 
                              parties, cpp, cns) %>%
      left_join(., coco, by = "KODZASTUP") %>% 
      mutate(PLATNOST = ifelse(PLATNOST == "A", 0, 1), 
             MANDAT = ifelse(MANDAT == "N", 0, 1), 
             JMENO = ifelse(JMENO == toupper(JMENO), 
                            stringr::str_to_title(JMENO, locale = "cs"), 
                            JMENO),
             PRIJMENI = ifelse(PRIJMENI == toupper(PRIJMENI), 
                               stringr::str_to_title(PRIJMENI, locale = "cs"), 
                               PRIJMENI)) %>% 
      categorize_sex
  }), 
  
  # TODO: dodatečné volby
  tar_target(mun_2022, command = {
    parties <- read_excel(here("data", "KV2022", "KV2022reg20240623_xlsx", "kvros.xlsx")) %>% 
      filter(DATUMVOLEB == min(DATUMVOLEB)) %>% 
      select(KODZASTUP, COBVODU, POR_STR_HL, OSTRANA, NAZEVCELK)
    cpp <- read_cpp(here("data", "KV2022", "KV2022ciselniky20240623", "cpp.xlsx"))
    cns <- read_cns(here("data", "KV2022", "KV2022ciselniky20240623", "cns.xlsx"))
    coco <- read_excel(here("data", "KV2022", "KV2022reg20240623_xlsx", "kvrzcoco.xlsx")) %>% 
      select(KODZASTUP, TYPZASTUP) %>% 
      unique()
    
    read_excel(here("data", "KV2022", "KV2022reg20240623_xlsx", "kvrk.xlsx")) %>%
      filter(DATUMVOLEB == min(DATUMVOLEB)) %>% 
      left_join(., coco, by = "KODZASTUP") %>% 
      left_join(., parties, by = c("KODZASTUP", "COBVODU", "POR_STR_HL", "OSTRANA")) %>%
      left_join(., cpp, by = "PSTRANA") %>%
      left_join(., cns, by = "NSTRANA") %>%
      mutate(row_id = row_number(), 
             ROK_NAROZENI = 2022 - VEK, 
             PLATNOST = ifelse(PLATNOST == "A", 0, 1), 
             MANDAT = ifelse(MANDAT == "N", 0, 1)) %>%
      merge_and_recode_titles %>% 
      categorize_sex
  }),
  
  tar_target(district_municipalities_map, {
    
    missing_districts <- tibble::tribble(
      ~CITY_DISTRICT, ~MUNICIPALITY,
      408913,  563889 ,
      117820,  555134,
      141208,  555134,
      410608,  555134,
      95141,  505927,
      111872,  505927,
      124630,  505927,
      159182,  505927,
      177199,  505927,
      183601,  505927,
      193232,  505927,
      413917,  505927
    )
    
    read_csv(here("data", "mcast_obec.csv"), 
             locale = locale(encoding = "WINDOWS-1250")) %>%
      select(CITY_DISTRICT = CHODNOTA1, 
             MUNICIPALITY = CHODNOTA2) %>% 
      bind_rows(., missing_districts)
  }),
  
  ## City districts ------------------------------
  tar_target(mc_1994, filter_city_districts(mun_1994, district_municipalities_map)),
  tar_target(mc_1998, filter_city_districts(mun_1998, district_municipalities_map)),
  tar_target(mc_2002, filter_city_districts(mun_2002, district_municipalities_map)),
  tar_target(mc_2006, filter_city_districts(mun_2006, district_municipalities_map)),
  tar_target(mc_2010, filter_city_districts(mun_2010, district_municipalities_map)),
  tar_target(mc_2014, filter_city_districts(mun_2014, district_municipalities_map)),
  tar_target(mc_2018, filter_city_districts(mun_2018, district_municipalities_map)),
  tar_target(mc_2022, filter_city_districts(mun_2022, district_municipalities_map)),
  
  tar_target(mc_panel, {
    mc_94_98 <- match_mun_data(mc_1994, mc_1998)
    
    mc_98_02 <- match_mun_data(mc_1998, mc_2002)
    
    mc_94_02 <- match_mun_data(
      mc_1994 %>% 
        filter(!row_id %in% mc_94_98$row_id.x), 
      mc_2002 %>% 
        filter(!row_id %in% mc_98_02$row_id.y)
    )
    
    mc_02_06 <- match_mun_data(mc_2002, mc_2006)
    
    mc_98_06 <- match_mun_data(
      mc_1998 %>% 
        filter(!row_id %in% mc_98_02$row_id.x), 
      mc_2006 %>% 
        filter(!row_id %in% mc_02_06$row_id.y)
    )
    
    mc_94_06 <- match_mun_data(
      mc_1994 %>% 
        filter(!row_id %in% mc_94_98$row_id.x) %>% 
        filter(!row_id %in% mc_94_02$row_id.x), 
      mc_2006 %>% 
        filter(!row_id %in% mc_98_06$row_id.y) %>% 
        filter(!row_id %in% mc_02_06$row_id.y)
    )
    
    mc_06_10 <- match_mun_data(mc_2006, mc_2010)
    
    mc_02_10 <- match_mun_data(
      mc_2002 %>% 
        filter(!row_id %in% mc_02_06$row_id.x), 
      mc_2010 %>% 
        filter(!row_id %in% mc_06_10$row_id.y)
    )
    
    mc_98_10 <- match_mun_data(
      mc_1998 %>% 
        filter(!row_id %in% mc_98_02$row_id.x) %>% 
        filter(!row_id %in% mc_98_06$row_id.x), 
      mc_2010 %>% 
        filter(!row_id %in% mc_02_10$row_id.y) %>% 
        filter(!row_id %in% mc_06_10$row_id.y)
    )
    
    mc_94_10 <- match_mun_data(
      mc_1994 %>% 
        filter(!row_id %in% mc_94_98$row_id.x) %>% 
        filter(!row_id %in% mc_94_02$row_id.x) %>% 
        filter(!row_id %in% mc_94_06$row_id.x),
      mc_2010 %>% 
        filter(!row_id %in% mc_98_10$row_id.y) %>% 
        filter(!row_id %in% mc_02_10$row_id.y) %>% 
        filter(!row_id %in% mc_06_10$row_id.y)
    )
    
    mc_10_14 <- match_mun_data(mc_2010, mc_2014)
    
    mc_06_14 <- match_mun_data(
      mc_2006 %>% 
        filter(!row_id %in% mc_06_10$row_id.x), 
      mc_2014 %>% 
        filter(!row_id %in% mc_10_14$row_id.y)
    )
    
    mc_02_14 <- match_mun_data(
      mc_2002 %>% 
        filter(!row_id %in% mc_02_06$row_id.x) %>% 
        filter(!row_id %in% mc_02_10$row_id.x), 
      mc_2014 %>% 
        filter(!row_id %in% mc_06_14$row_id.y) %>% 
        filter(!row_id %in% mc_10_14$row_id.y)
    )
    
    mc_98_14 <- match_mun_data(
      mc_1998 %>% 
        filter(!row_id %in% mc_98_02$row_id.x) %>% 
        filter(!row_id %in% mc_98_06$row_id.x) %>% 
        filter(!row_id %in% mc_98_10$row_id.x), 
      mc_2014 %>% 
        filter(!row_id %in% mc_02_14$row_id.y) %>% 
        filter(!row_id %in% mc_06_14$row_id.y) %>% 
        filter(!row_id %in% mc_10_14$row_id.y)
    )
    
    mc_94_14 <- match_mun_data(
      mc_1994 %>% 
        filter(!row_id %in% mc_94_98$row_id.x) %>% 
        filter(!row_id %in% mc_94_02$row_id.x) %>% 
        filter(!row_id %in% mc_94_06$row_id.x) %>% 
        filter(!row_id %in% mc_94_10$row_id.x), 
      mc_2014 %>% 
        filter(!row_id %in% mc_98_14$row_id.y) %>% 
        filter(!row_id %in% mc_02_14$row_id.y) %>% 
        filter(!row_id %in% mc_06_14$row_id.y) %>% 
        filter(!row_id %in% mc_10_14$row_id.y)
    )
    
    mc_14_18 <- match_mun_data(mc_2014, mc_2018)
    
    mc_10_18 <- match_mun_data(
      mc_2010 %>% 
        filter(!row_id %in% mc_10_14$row_id.x), 
      mc_2018 %>% 
        filter(!row_id %in% mc_14_18$row_id.y)
    )
    
    mc_06_18 <- match_mun_data(
      mc_2006 %>% 
        filter(!row_id %in% mc_06_10$row_id.x) %>% 
        filter(!row_id %in% mc_06_14$row_id.x), 
      mc_2018 %>% 
        filter(!row_id %in% mc_10_18$row_id.y) %>% 
        filter(!row_id %in% mc_14_18$row_id.y)
    )
    
    mc_02_18 <- match_mun_data(
      mc_2002 %>% 
        filter(!row_id %in% mc_02_06$row_id.x) %>% 
        filter(!row_id %in% mc_02_10$row_id.x) %>% 
        filter(!row_id %in% mc_02_14$row_id.x), 
      mc_2018 %>% 
        filter(!row_id %in% mc_06_18$row_id.y) %>% 
        filter(!row_id %in% mc_10_18$row_id.y) %>% 
        filter(!row_id %in% mc_14_18$row_id.y)
    )
    
    mc_98_18 <- match_mun_data(
      mc_1998 %>% 
        filter(!row_id %in% mc_98_02$row_id.x) %>% 
        filter(!row_id %in% mc_98_06$row_id.x) %>% 
        filter(!row_id %in% mc_98_10$row_id.x) %>% 
        filter(!row_id %in% mc_98_14$row_id.x), 
      mc_2018 %>% 
        filter(!row_id %in% mc_02_18$row_id.y) %>% 
        filter(!row_id %in% mc_06_18$row_id.y) %>% 
        filter(!row_id %in% mc_10_18$row_id.y) %>% 
        filter(!row_id %in% mc_14_18$row_id.y)
    )
    
    mc_94_18 <- match_mun_data(
      mc_1994 %>% 
        filter(!row_id %in% mc_94_98$row_id.y) %>% 
        filter(!row_id %in% mc_94_02$row_id.x) %>% 
        filter(!row_id %in% mc_94_06$row_id.x) %>% 
        filter(!row_id %in% mc_94_10$row_id.x) %>% 
        filter(!row_id %in% mc_94_14$row_id.x), 
      mc_2018 %>% 
        filter(!row_id %in% mc_98_18$row_id.y) %>% 
        filter(!row_id %in% mc_02_18$row_id.y) %>% 
        filter(!row_id %in% mc_06_18$row_id.y) %>% 
        filter(!row_id %in% mc_10_18$row_id.y) %>% 
        filter(!row_id %in% mc_14_18$row_id.y)
    )
    
    mc_18_22 <- match_mun_data(mc_2018, mc_2022)
    
    mc_14_22 <- match_mun_data(
      mc_2014 %>% 
        filter(!row_id %in% mc_14_18$row_id.x),
      mc_2022 %>% 
        filter(!row_id %in% mc_18_22$row_id.y)
    )
    
    mc_10_22 <- match_mun_data(
      mc_2010 %>% 
        filter(!row_id %in% mc_10_14$row_id.x) %>% 
        filter(!row_id %in% mc_10_18$row_id.x),
      mc_2022 %>% 
        filter(!row_id %in% mc_14_22$row_id.y) %>% 
        filter(!row_id %in% mc_18_22$row_id.y)
    )
    
    mc_06_22 <- match_mun_data(
      mc_2006 %>% 
        filter(!row_id %in% mc_06_10$row_id.x) %>% 
        filter(!row_id %in% mc_06_14$row_id.x) %>% 
        filter(!row_id %in% mc_06_18$row_id.x),
      mc_2022 %>% 
        filter(!row_id %in% mc_10_22$row_id.y) %>% 
        filter(!row_id %in% mc_14_22$row_id.y) %>% 
        filter(!row_id %in% mc_18_22$row_id.y)
    )
    
    mc_02_22 <- match_mun_data(
      mc_2002 %>% 
        filter(!row_id %in% mc_02_06$row_id.x) %>% 
        filter(!row_id %in% mc_02_10$row_id.x) %>% 
        filter(!row_id %in% mc_02_14$row_id.x) %>% 
        filter(!row_id %in% mc_02_18$row_id.x),
      mc_2022 %>% 
        filter(!row_id %in% mc_06_22$row_id.y) %>% 
        filter(!row_id %in% mc_10_22$row_id.y) %>% 
        filter(!row_id %in% mc_14_22$row_id.y) %>% 
        filter(!row_id %in% mc_18_22$row_id.y)
    )
    
    mc_98_22 <- match_mun_data(
      mc_1998 %>% 
        filter(!row_id %in% mc_98_02$row_id.x) %>% 
        filter(!row_id %in% mc_98_06$row_id.x) %>% 
        filter(!row_id %in% mc_98_10$row_id.x) %>% 
        filter(!row_id %in% mc_98_14$row_id.x) %>% 
        filter(!row_id %in% mc_98_18$row_id.x),
      mc_2022 %>% 
        filter(!row_id %in% mc_02_22$row_id.y) %>% 
        filter(!row_id %in% mc_06_22$row_id.y) %>% 
        filter(!row_id %in% mc_10_22$row_id.y) %>% 
        filter(!row_id %in% mc_14_22$row_id.y) %>% 
        filter(!row_id %in% mc_18_22$row_id.y)
    )
    
    mc_94_22 <- match_mun_data(
      mc_1994 %>% 
        filter(!row_id %in% mc_94_98$row_id.x) %>% 
        filter(!row_id %in% mc_94_02$row_id.x) %>% 
        filter(!row_id %in% mc_94_06$row_id.x) %>% 
        filter(!row_id %in% mc_94_10$row_id.x) %>% 
        filter(!row_id %in% mc_94_14$row_id.x) %>% 
        filter(!row_id %in% mc_94_18$row_id.x),
      mc_2022 %>% 
        filter(!row_id %in% mc_98_22$row_id.y) %>% 
        filter(!row_id %in% mc_02_22$row_id.y) %>% 
        filter(!row_id %in% mc_06_22$row_id.y) %>% 
        filter(!row_id %in% mc_10_22$row_id.y) %>% 
        filter(!row_id %in% mc_14_22$row_id.y) %>% 
        filter(!row_id %in% mc_18_22$row_id.y)
    )
    
    mc_94_98b <- mc_94_98 %>% select(row_id_1994 = row_id.x, row_id_1998 = row_id.y)
    mc_94_02b <- mc_94_02 %>% select(row_id_1994 = row_id.x, row_id_2002 = row_id.y)
    mc_94_06b <- mc_94_06 %>% select(row_id_1994 = row_id.x, row_id_2006 = row_id.y)
    mc_94_10b <- mc_94_10 %>% select(row_id_1994 = row_id.x, row_id_2010 = row_id.y)
    mc_94_14b <- mc_94_14 %>% select(row_id_1994 = row_id.x, row_id_2014 = row_id.y)
    mc_94_18b <- mc_94_18 %>% select(row_id_1994 = row_id.x, row_id_2018 = row_id.y)
    mc_94_22b <- mc_94_22 %>% select(row_id_1994 = row_id.x, row_id_2022 = row_id.y)
    
    mc_98_02b <- mc_98_02 %>% select(row_id_1998 = row_id.x, row_id_2002 = row_id.y)
    mc_98_06b <- mc_98_06 %>% select(row_id_1998 = row_id.x, row_id_2006 = row_id.y)
    mc_98_10b <- mc_98_10 %>% select(row_id_1998 = row_id.x, row_id_2010 = row_id.y)
    mc_98_14b <- mc_98_14 %>% select(row_id_1998 = row_id.x, row_id_2014 = row_id.y)
    mc_98_18b <- mc_98_18 %>% select(row_id_1998 = row_id.x, row_id_2018 = row_id.y)
    mc_98_22b <- mc_98_22 %>% select(row_id_1998 = row_id.x, row_id_2022 = row_id.y)
    
    mc_02_06b <- mc_02_06 %>% select(row_id_2002 = row_id.x, row_id_2006 = row_id.y)
    mc_02_10b <- mc_02_10 %>% select(row_id_2002 = row_id.x, row_id_2010 = row_id.y)
    mc_02_14b <- mc_02_14 %>% select(row_id_2002 = row_id.x, row_id_2014 = row_id.y)
    mc_02_18b <- mc_02_18 %>% select(row_id_2002 = row_id.x, row_id_2018 = row_id.y)
    mc_02_22b <- mc_02_22 %>% select(row_id_2002 = row_id.x, row_id_2022 = row_id.y)
    
    mc_06_10b <- mc_06_10 %>% select(row_id_2006 = row_id.x, row_id_2010 = row_id.y)
    mc_06_14b <- mc_06_14 %>% select(row_id_2006 = row_id.x, row_id_2014 = row_id.y)
    mc_06_18b <- mc_06_18 %>% select(row_id_2006 = row_id.x, row_id_2018 = row_id.y)
    mc_06_22b <- mc_06_22 %>% select(row_id_2006 = row_id.x, row_id_2022 = row_id.y)
    
    mc_10_14b <- mc_10_14 %>% select(row_id_2010 = row_id.x, row_id_2014 = row_id.y)
    mc_10_18b <- mc_10_18 %>% select(row_id_2010 = row_id.x, row_id_2018 = row_id.y)
    mc_10_22b <- mc_10_22 %>% select(row_id_2010 = row_id.x, row_id_2022 = row_id.y)
    
    mc_14_18b <- mc_14_18 %>% select(row_id_2014 = row_id.x, row_id_2018 = row_id.y)
    mc_14_22b <- mc_14_22 %>% select(row_id_2014 = row_id.x, row_id_2022 = row_id.y)
    
    mc_18_22b <- mc_18_22 %>% select(row_id_2018 = row_id.x, row_id_2022 = row_id.y)
    
    (pivot_table <- mc_94_98b %>% 
        bind_rows(., mc_1994 %>% select(row_id_1994 = row_id) %>% 
                    filter(!row_id_1994 %in% mc_94_98b$row_id_1994)) %>% 
        full_join(., mc_98_02b %>% 
                    bind_rows(., mc_1998 %>% select(row_id_1998 = row_id) %>% 
                                filter(!row_id_1998 %in% mc_98_02b$row_id_1998))) %>% 
        insert_nonconsecutive(., mc_94_02b, "row_id_1994", "row_id_2002") %>% 
        full_join(., mc_02_06b %>% 
                    bind_rows(., mc_2002 %>% select(row_id_2002 = row_id) %>% 
                                filter(!row_id_2002 %in% mc_02_06b$row_id_2002))) %>% 
        insert_nonconsecutive(., mc_98_06b, "row_id_1998", "row_id_2006") %>% 
        insert_nonconsecutive(., mc_94_06b, "row_id_1994", "row_id_2006") %>% 
        full_join(., mc_06_10b %>% 
                    bind_rows(., mc_2006 %>% select(row_id_2006 = row_id) %>% 
                                filter(!row_id_2006 %in% mc_06_10b$row_id_2006))) %>% 
        insert_nonconsecutive(., mc_02_10b, "row_id_2002", "row_id_2010") %>% 
        insert_nonconsecutive(., mc_98_10b, "row_id_1998", "row_id_2010") %>% 
        insert_nonconsecutive(., mc_94_10b, "row_id_1994", "row_id_2010") %>% 
        full_join(., mc_10_14b %>% 
                    bind_rows(., mc_2010 %>% select(row_id_2010 = row_id) %>% 
                                filter(!row_id_2010 %in% mc_10_14b$row_id_2010))) %>% 
        insert_nonconsecutive(., mc_06_14b, "row_id_2006", "row_id_2014") %>% 
        insert_nonconsecutive(., mc_02_14b, "row_id_2002", "row_id_2014") %>% 
        insert_nonconsecutive(., mc_98_14b, "row_id_1998", "row_id_2014") %>% 
        insert_nonconsecutive(., mc_94_14b, "row_id_1994", "row_id_2014") %>% 
        full_join(., mc_14_18b %>% 
                    bind_rows(., mc_2014 %>% select(row_id_2014 = row_id) %>% 
                                filter(!row_id_2014 %in% mc_14_18b$row_id_2014))) %>% 
        insert_nonconsecutive(., mc_10_18b, "row_id_2010", "row_id_2018") %>% 
        insert_nonconsecutive(., mc_06_18b, "row_id_2006", "row_id_2018") %>% 
        insert_nonconsecutive(., mc_02_18b, "row_id_2002", "row_id_2018") %>% 
        insert_nonconsecutive(., mc_98_18b, "row_id_1998", "row_id_2018") %>% 
        insert_nonconsecutive(., mc_94_18b, "row_id_1994", "row_id_2018") %>% 
        full_join(., mc_18_22b %>% 
                    bind_rows(., mc_2018 %>% select(row_id_2018 = row_id) %>% 
                                filter(!row_id_2018 %in% mc_18_22b$row_id_2018))) %>% 
        insert_nonconsecutive(., mc_14_22b, "row_id_2014", "row_id_2022") %>% 
        insert_nonconsecutive(., mc_10_22b, "row_id_2010", "row_id_2022") %>% 
        insert_nonconsecutive(., mc_06_22b, "row_id_2006", "row_id_2022") %>% 
        insert_nonconsecutive(., mc_02_22b, "row_id_2002", "row_id_2022") %>% 
        insert_nonconsecutive(., mc_98_22b, "row_id_1998", "row_id_2022") %>% 
        insert_nonconsecutive(., mc_94_22b, "row_id_1994", "row_id_2022") %>% 
        bind_rows(., mc_2022 %>% select(row_id_2022 = row_id) %>%
                    filter(!row_id_2022 %in% c(mc_18_22b$row_id_2022, 
                                               mc_14_22b$row_id_2022, 
                                               mc_10_22b$row_id_2022, 
                                               mc_06_22b$row_id_2022, 
                                               mc_02_22b$row_id_2022, 
                                               mc_98_22b$row_id_2022, 
                                               mc_94_22b$row_id_2022)))
    )
    
    pivot_table_long <- pivot_table %>%
      mutate(person_id = paste0("MC", row_number())) %>% 
      tidyr::pivot_longer(., cols = 1:(ncol(.)-1), names_to = "year",
                          values_to = "row_id") %>%
      filter(!is.na(row_id)) %>%
      mutate(year = stringr::str_extract(year, "[0-9]{4}"))
    
    mc_candidates <- bind_rows(
      mc_1994 %>% mutate(year = "1994"),
      mc_1998 %>% mutate(year = "1998"),
      mc_2002 %>% mutate(year = "2002"),
      mc_2006 %>% mutate(year = "2006"),
      mc_2010 %>% mutate(year = "2010"),
      mc_2014 %>% mutate(year = "2014"),
      mc_2018 %>% mutate(year = "2018"),
      mc_2022 %>% mutate(year = "2022")
    )
    
    full_join(pivot_table_long, mc_candidates, by = c("row_id", "year"))
  }), 
  
  tar_target(
    mc_panel_check, 
    stopifnot(nrow(mc_panel) == (
      nrow(mc_1994) + nrow(mc_1998) + nrow(mc_2002) + nrow(mc_2006) +
        nrow(mc_2010) + nrow(mc_2014) + nrow(mc_2018) + nrow(mc_2022) 
    ))
  ),
  
  ## Municipalities ---------------------------------------
  tar_target(m_1994, filter_municipalities(mun_1994, district_municipalities_map) %>%
               mutate(PRIJMENI = gsub('"o', "ö", PRIJMENI) %>%
                        gsub('o"', "ö", .) %>%
                        gsub('u"', "ü", .)
               )),
  tar_target(m_1998, filter_municipalities(mun_1998, district_municipalities_map)),
  tar_target(m_2002, filter_municipalities(mun_2002, district_municipalities_map)),
  tar_target(m_2006, filter_municipalities(mun_2006, district_municipalities_map)),
  tar_target(m_2010, filter_municipalities(mun_2010, district_municipalities_map)),
  tar_target(m_2014, filter_municipalities(mun_2014, district_municipalities_map)),
  tar_target(m_2018, filter_municipalities(mun_2018, district_municipalities_map)),
  tar_target(m_2022, filter_municipalities(mun_2022, district_municipalities_map)),
  
  tar_target(
    m_panel, {
      m_94_98 <- match_mun_data(m_1994, m_1998)
      
      m_98_02 <- match_mun_data(m_1998, m_2002)
      
      m_94_02 <- match_mun_data(
        m_1994 %>% 
          filter(!row_id %in% m_94_98$row_id.x), 
        m_2002 %>% 
          filter(!row_id %in% m_98_02$row_id.y)
      )
      
      m_02_06 <- match_mun_data(m_2002, m_2006)
      
      m_98_06 <- match_mun_data(
        m_1998 %>% 
          filter(!row_id %in% m_98_02$row_id.x), 
        m_2006 %>% 
          filter(!row_id %in% m_02_06$row_id.y)
      )
      
      m_94_06 <- match_mun_data(
        m_1994 %>% 
          filter(!row_id %in% m_94_98$row_id.x) %>% 
          filter(!row_id %in% m_94_02$row_id.x), 
        m_2006 %>% 
          filter(!row_id %in% m_98_06$row_id.y) %>% 
          filter(!row_id %in% m_02_06$row_id.y)
      )
      
      m_06_10 <- match_mun_data(m_2006, m_2010)
      
      m_02_10 <- match_mun_data(
        m_2002 %>% 
          filter(!row_id %in% m_02_06$row_id.x), 
        m_2010 %>% 
          filter(!row_id %in% m_06_10$row_id.y)
      )
      
      m_98_10 <- match_mun_data(
        m_1998 %>% 
          filter(!row_id %in% m_98_02$row_id.x) %>% 
          filter(!row_id %in% m_98_06$row_id.x), 
        m_2010 %>% 
          filter(!row_id %in% m_02_10$row_id.y) %>% 
          filter(!row_id %in% m_06_10$row_id.y)
      )
      
      m_94_10 <- match_mun_data(
        m_1994 %>% 
          filter(!row_id %in% m_94_98$row_id.x) %>% 
          filter(!row_id %in% m_94_02$row_id.x) %>% 
          filter(!row_id %in% m_94_06$row_id.x),
        m_2010 %>% 
          filter(!row_id %in% m_98_10$row_id.y) %>% 
          filter(!row_id %in% m_02_10$row_id.y) %>% 
          filter(!row_id %in% m_06_10$row_id.y)
      )
      
      m_10_14 <- match_mun_data(m_2010, m_2014)
      
      m_06_14 <- match_mun_data(
        m_2006 %>% 
          filter(!row_id %in% m_06_10$row_id.x), 
        m_2014 %>% 
          filter(!row_id %in% m_10_14$row_id.y)
      )
      
      m_02_14 <- match_mun_data(
        m_2002 %>% 
          filter(!row_id %in% m_02_06$row_id.x) %>% 
          filter(!row_id %in% m_02_10$row_id.x), 
        m_2014 %>% 
          filter(!row_id %in% m_06_14$row_id.y) %>% 
          filter(!row_id %in% m_10_14$row_id.y)
      )
      
      m_98_14 <- match_mun_data(
        m_1998 %>% 
          filter(!row_id %in% m_98_02$row_id.x) %>% 
          filter(!row_id %in% m_98_06$row_id.x) %>% 
          filter(!row_id %in% m_98_10$row_id.x), 
        m_2014 %>% 
          filter(!row_id %in% m_02_14$row_id.y) %>% 
          filter(!row_id %in% m_06_14$row_id.y) %>% 
          filter(!row_id %in% m_10_14$row_id.y)
      )
      
      m_94_14 <- match_mun_data(
        m_1994 %>% 
          filter(!row_id %in% m_94_98$row_id.x) %>% 
          filter(!row_id %in% m_94_02$row_id.x) %>% 
          filter(!row_id %in% m_94_06$row_id.x) %>% 
          filter(!row_id %in% m_94_10$row_id.x), 
        m_2014 %>% 
          filter(!row_id %in% m_98_14$row_id.y) %>% 
          filter(!row_id %in% m_02_14$row_id.y) %>% 
          filter(!row_id %in% m_06_14$row_id.y) %>% 
          filter(!row_id %in% m_10_14$row_id.y)
      )
      
      m_14_18 <- match_mun_data(m_2014, m_2018)
      
      m_10_18 <- match_mun_data(
        m_2010 %>% 
          filter(!row_id %in% m_10_14$row_id.x), 
        m_2018 %>% 
          filter(!row_id %in% m_14_18$row_id.y)
      )
      
      m_06_18 <- match_mun_data(
        m_2006 %>% 
          filter(!row_id %in% m_06_10$row_id.x) %>% 
          filter(!row_id %in% m_06_14$row_id.x), 
        m_2018 %>% 
          filter(!row_id %in% m_10_18$row_id.y) %>% 
          filter(!row_id %in% m_14_18$row_id.y)
      )
      
      m_02_18 <- match_mun_data(
        m_2002 %>% 
          filter(!row_id %in% m_02_06$row_id.x) %>% 
          filter(!row_id %in% m_02_10$row_id.x) %>% 
          filter(!row_id %in% m_02_14$row_id.x), 
        m_2018 %>% 
          filter(!row_id %in% m_06_18$row_id.y) %>% 
          filter(!row_id %in% m_10_18$row_id.y) %>% 
          filter(!row_id %in% m_14_18$row_id.y)
      )
      
      m_98_18 <- match_mun_data(
        m_1998 %>% 
          filter(!row_id %in% m_98_02$row_id.x) %>% 
          filter(!row_id %in% m_98_06$row_id.x) %>% 
          filter(!row_id %in% m_98_10$row_id.x) %>% 
          filter(!row_id %in% m_98_14$row_id.x), 
        m_2018 %>% 
          filter(!row_id %in% m_02_18$row_id.y) %>% 
          filter(!row_id %in% m_06_18$row_id.y) %>% 
          filter(!row_id %in% m_10_18$row_id.y) %>% 
          filter(!row_id %in% m_14_18$row_id.y)
      )
      
      m_94_18 <- match_mun_data(
        m_1994 %>% 
          filter(!row_id %in% m_94_98$row_id.y) %>% 
          filter(!row_id %in% m_94_02$row_id.x) %>% 
          filter(!row_id %in% m_94_06$row_id.x) %>% 
          filter(!row_id %in% m_94_10$row_id.x) %>% 
          filter(!row_id %in% m_94_14$row_id.x), 
        m_2018 %>% 
          filter(!row_id %in% m_98_18$row_id.y) %>% 
          filter(!row_id %in% m_02_18$row_id.y) %>% 
          filter(!row_id %in% m_06_18$row_id.y) %>% 
          filter(!row_id %in% m_10_18$row_id.y) %>% 
          filter(!row_id %in% m_14_18$row_id.y)
      )
      
      m_18_22 <- match_mun_data(m_2018, m_2022)
      
      m_14_22 <- match_mun_data(
        m_2014 %>% 
          filter(!row_id %in% m_14_18$row_id.x),
        m_2022 %>% 
          filter(!row_id %in% m_18_22$row_id.y)
      )
      
      m_10_22 <- match_mun_data(
        m_2010 %>% 
          filter(!row_id %in% m_10_14$row_id.x) %>% 
          filter(!row_id %in% m_10_18$row_id.x),
        m_2022 %>% 
          filter(!row_id %in% m_14_22$row_id.y) %>% 
          filter(!row_id %in% m_18_22$row_id.y)
      )
      
      m_06_22 <- match_mun_data(
        m_2006 %>% 
          filter(!row_id %in% m_06_10$row_id.x) %>% 
          filter(!row_id %in% m_06_14$row_id.x) %>% 
          filter(!row_id %in% m_06_18$row_id.x),
        m_2022 %>% 
          filter(!row_id %in% m_10_22$row_id.y) %>% 
          filter(!row_id %in% m_14_22$row_id.y) %>% 
          filter(!row_id %in% m_18_22$row_id.y)
      )
      
      m_02_22 <- match_mun_data(
        m_2002 %>% 
          filter(!row_id %in% m_02_06$row_id.x) %>% 
          filter(!row_id %in% m_02_10$row_id.x) %>% 
          filter(!row_id %in% m_02_14$row_id.x) %>% 
          filter(!row_id %in% m_02_18$row_id.x),
        m_2022 %>% 
          filter(!row_id %in% m_06_22$row_id.y) %>% 
          filter(!row_id %in% m_10_22$row_id.y) %>% 
          filter(!row_id %in% m_14_22$row_id.y) %>% 
          filter(!row_id %in% m_18_22$row_id.y)
      )
      
      m_98_22 <- match_mun_data(
        m_1998 %>% 
          filter(!row_id %in% m_98_02$row_id.x) %>% 
          filter(!row_id %in% m_98_06$row_id.x) %>% 
          filter(!row_id %in% m_98_10$row_id.x) %>% 
          filter(!row_id %in% m_98_14$row_id.x) %>% 
          filter(!row_id %in% m_98_18$row_id.x),
        m_2022 %>% 
          filter(!row_id %in% m_02_22$row_id.y) %>% 
          filter(!row_id %in% m_06_22$row_id.y) %>% 
          filter(!row_id %in% m_10_22$row_id.y) %>% 
          filter(!row_id %in% m_14_22$row_id.y) %>% 
          filter(!row_id %in% m_18_22$row_id.y)
      )
      
      m_94_22 <- match_mun_data(
        m_1994 %>% 
          filter(!row_id %in% m_94_98$row_id.x) %>% 
          filter(!row_id %in% m_94_02$row_id.x) %>% 
          filter(!row_id %in% m_94_06$row_id.x) %>% 
          filter(!row_id %in% m_94_10$row_id.x) %>% 
          filter(!row_id %in% m_94_14$row_id.x) %>% 
          filter(!row_id %in% m_94_18$row_id.x),
        m_2022 %>% 
          filter(!row_id %in% m_98_22$row_id.y) %>% 
          filter(!row_id %in% m_02_22$row_id.y) %>% 
          filter(!row_id %in% m_06_22$row_id.y) %>% 
          filter(!row_id %in% m_10_22$row_id.y) %>% 
          filter(!row_id %in% m_14_22$row_id.y) %>% 
          filter(!row_id %in% m_18_22$row_id.y)
      )
      
      m_94_98b <- m_94_98 %>% select(row_id_1994 = row_id.x, row_id_1998 = row_id.y)
      m_94_02b <- m_94_02 %>% select(row_id_1994 = row_id.x, row_id_2002 = row_id.y)
      m_94_06b <- m_94_06 %>% select(row_id_1994 = row_id.x, row_id_2006 = row_id.y)
      m_94_10b <- m_94_10 %>% select(row_id_1994 = row_id.x, row_id_2010 = row_id.y)
      m_94_14b <- m_94_14 %>% select(row_id_1994 = row_id.x, row_id_2014 = row_id.y)
      m_94_18b <- m_94_18 %>% select(row_id_1994 = row_id.x, row_id_2018 = row_id.y)
      m_94_22b <- m_94_22 %>% select(row_id_1994 = row_id.x, row_id_2022 = row_id.y)
      
      m_98_02b <- m_98_02 %>% select(row_id_1998 = row_id.x, row_id_2002 = row_id.y)
      m_98_06b <- m_98_06 %>% select(row_id_1998 = row_id.x, row_id_2006 = row_id.y)
      m_98_10b <- m_98_10 %>% select(row_id_1998 = row_id.x, row_id_2010 = row_id.y)
      m_98_14b <- m_98_14 %>% select(row_id_1998 = row_id.x, row_id_2014 = row_id.y)
      m_98_18b <- m_98_18 %>% select(row_id_1998 = row_id.x, row_id_2018 = row_id.y)
      m_98_22b <- m_98_22 %>% select(row_id_1998 = row_id.x, row_id_2022 = row_id.y)
      
      m_02_06b <- m_02_06 %>% select(row_id_2002 = row_id.x, row_id_2006 = row_id.y)
      m_02_10b <- m_02_10 %>% select(row_id_2002 = row_id.x, row_id_2010 = row_id.y)
      m_02_14b <- m_02_14 %>% select(row_id_2002 = row_id.x, row_id_2014 = row_id.y)
      m_02_18b <- m_02_18 %>% select(row_id_2002 = row_id.x, row_id_2018 = row_id.y)
      m_02_22b <- m_02_22 %>% select(row_id_2002 = row_id.x, row_id_2022 = row_id.y)
      
      m_06_10b <- m_06_10 %>% select(row_id_2006 = row_id.x, row_id_2010 = row_id.y)
      m_06_14b <- m_06_14 %>% select(row_id_2006 = row_id.x, row_id_2014 = row_id.y)
      m_06_18b <- m_06_18 %>% select(row_id_2006 = row_id.x, row_id_2018 = row_id.y)
      m_06_22b <- m_06_22 %>% select(row_id_2006 = row_id.x, row_id_2022 = row_id.y)
      
      m_10_14b <- m_10_14 %>% select(row_id_2010 = row_id.x, row_id_2014 = row_id.y)
      m_10_18b <- m_10_18 %>% select(row_id_2010 = row_id.x, row_id_2018 = row_id.y)
      m_10_22b <- m_10_22 %>% select(row_id_2010 = row_id.x, row_id_2022 = row_id.y)
      
      m_14_18b <- m_14_18 %>% select(row_id_2014 = row_id.x, row_id_2018 = row_id.y)
      m_14_22b <- m_14_22 %>% select(row_id_2014 = row_id.x, row_id_2022 = row_id.y)
      
      m_18_22b <- m_18_22 %>% select(row_id_2018 = row_id.x, row_id_2022 = row_id.y)
      
      (pivot_table <- m_94_98b %>% 
          bind_rows(., m_1994 %>% select(row_id_1994 = row_id) %>% 
                      filter(!row_id_1994 %in% m_94_98b$row_id_1994)) %>% 
          full_join(., m_98_02b %>% 
                      bind_rows(., m_1998 %>% select(row_id_1998 = row_id) %>% 
                                  filter(!row_id_1998 %in% m_98_02b$row_id_1998))) %>% 
          insert_nonconsecutive(., m_94_02b, "row_id_1994", "row_id_2002") %>% 
          full_join(., m_02_06b %>% 
                      bind_rows(., m_2002 %>% select(row_id_2002 = row_id) %>% 
                                  filter(!row_id_2002 %in% m_02_06b$row_id_2002))) %>% 
          insert_nonconsecutive(., m_98_06b, "row_id_1998", "row_id_2006") %>% 
          insert_nonconsecutive(., m_94_06b, "row_id_1994", "row_id_2006") %>% 
          full_join(., m_06_10b %>% 
                      bind_rows(., m_2006 %>% select(row_id_2006 = row_id) %>% 
                                  filter(!row_id_2006 %in% m_06_10b$row_id_2006))) %>% 
          insert_nonconsecutive(., m_02_10b, "row_id_2002", "row_id_2010") %>% 
          insert_nonconsecutive(., m_98_10b, "row_id_1998", "row_id_2010") %>% 
          insert_nonconsecutive(., m_94_10b, "row_id_1994", "row_id_2010") %>% 
          full_join(., m_10_14b %>% 
                      bind_rows(., m_2010 %>% select(row_id_2010 = row_id) %>% 
                                  filter(!row_id_2010 %in% m_10_14b$row_id_2010))) %>% 
          insert_nonconsecutive(., m_06_14b, "row_id_2006", "row_id_2014") %>% 
          insert_nonconsecutive(., m_02_14b, "row_id_2002", "row_id_2014") %>% 
          insert_nonconsecutive(., m_98_14b, "row_id_1998", "row_id_2014") %>% 
          insert_nonconsecutive(., m_94_14b, "row_id_1994", "row_id_2014") %>% 
          full_join(., m_14_18b %>% 
                      bind_rows(., m_2014 %>% select(row_id_2014 = row_id) %>% 
                                  filter(!row_id_2014 %in% m_14_18b$row_id_2014))) %>% 
          insert_nonconsecutive(., m_10_18b, "row_id_2010", "row_id_2018") %>% 
          insert_nonconsecutive(., m_06_18b, "row_id_2006", "row_id_2018") %>% 
          insert_nonconsecutive(., m_02_18b, "row_id_2002", "row_id_2018") %>% 
          insert_nonconsecutive(., m_98_18b, "row_id_1998", "row_id_2018") %>% 
          insert_nonconsecutive(., m_94_18b, "row_id_1994", "row_id_2018") %>% 
          full_join(., m_18_22b %>% 
                      bind_rows(., m_2018 %>% select(row_id_2018 = row_id) %>% 
                                  filter(!row_id_2018 %in% m_18_22b$row_id_2018))) %>% 
          insert_nonconsecutive(., m_14_22b, "row_id_2014", "row_id_2022") %>% 
          insert_nonconsecutive(., m_10_22b, "row_id_2010", "row_id_2022") %>% 
          insert_nonconsecutive(., m_06_22b, "row_id_2006", "row_id_2022") %>% 
          insert_nonconsecutive(., m_02_22b, "row_id_2002", "row_id_2022") %>% 
          insert_nonconsecutive(., m_98_22b, "row_id_1998", "row_id_2022") %>% 
          insert_nonconsecutive(., m_94_22b, "row_id_1994", "row_id_2022") %>% 
          bind_rows(., m_2022 %>% select(row_id_2022 = row_id) %>%
                      filter(!row_id_2022 %in% c(m_18_22b$row_id_2022, 
                                                 m_14_22b$row_id_2022, 
                                                 m_10_22b$row_id_2022, 
                                                 m_06_22b$row_id_2022, 
                                                 m_02_22b$row_id_2022, 
                                                 m_98_22b$row_id_2022, 
                                                 m_94_22b$row_id_2022)))
      )
      
      pivot_table_long <- pivot_table %>%
        mutate(person_id = paste0("M", row_number())) %>% 
        tidyr::pivot_longer(., cols = 1:(ncol(.)-1), names_to = "year",
                            values_to = "row_id") %>%
        filter(!is.na(row_id)) %>%
        mutate(year = stringr::str_extract(year, "[0-9]{4}"))
      
      m_candidates <- bind_rows(
        m_1994 %>% mutate(year = "1994"),
        m_1998 %>% mutate(year = "1998"),
        m_2002 %>% mutate(year = "2002"),
        m_2006 %>% mutate(year = "2006"),
        m_2010 %>% mutate(year = "2010"),
        m_2014 %>% mutate(year = "2014"),
        m_2018 %>% mutate(year = "2018"),
        m_2022 %>% mutate(year = "2022")
      )
      
      full_join(pivot_table_long, m_candidates, by = c("row_id", "year"))
    }
  ),
  
  tar_target(
    m_panel_check, 
    stopifnot(nrow(m_panel) == (
      nrow(m_1994) + nrow(m_1998) + nrow(m_2002) + nrow(m_2006) +
        nrow(m_2010) + nrow(m_2014) + nrow(m_2018) + nrow(m_2022) 
    ))
  )
)

# EP elections --------------------------------------------
ep_data <- list(
  tar_target(ep_2004, command = {
    parties <- read_parties_xml(here("data", "EP2004", "EP2004reg", "eprkl.xml"), 
                                function(x) x %>% select(KSTRANA = ESTRANA, ZKRATKAK8 = ZKRATKAE8, NAZEV_STRK = NAZEV_STRE))
    cpp <- read_cpp_xml(here("data", "EP2004", "EP2004ciselniky", "cpp.xml"))
    cns <- read_cns_xml(here("data", "EP2004", "EP2004ciselniky", "cns.xml"))
    read_candidates_xml(here("data", "EP2004", "EP2004reg", "eprk.xml"), 
                        parties, cpp, cns, function(x) {x %>% rename(KSTRANA = ESTRANA)}) %>% 
      mutate(ROK_NAROZENI = 2004 - VEK) %>% 
      categorize_sex
  }),
  
  tar_target(ep_2009, command = {
    parties <- read_parties_xml(here("data", "EP2009", "EP2009reg", "eprkl.xml"), 
                                function(x) x %>% select(KSTRANA = ESTRANA, ZKRATKAK8 = ZKRATKAE8, NAZEV_STRK = NAZEV_STRE))
    cpp <- read_cpp_xml(here("data", "EP2009", "EP2009ciselniky", "cpp.xml"))
    cns <- read_cns_xml(here("data", "EP2009", "EP2009ciselniky", "cns.xml"))
    read_candidates_xml(here("data", "EP2009", "EP2009reg", "eprk.xml"), 
                        parties, cpp, cns, function(x) {x %>% rename(KSTRANA = ESTRANA)}) %>% 
      mutate(ROK_NAROZENI = 2009 - VEK) %>% 
      categorize_sex
  }),
  
  tar_target(ep_2014, command = {
    parties <- read_parties_xml(here("data", "EP2014", "EP2014reg20140525", "eprkl.xml"), 
                                function(x) x %>% select(KSTRANA = ESTRANA, ZKRATKAK8 = ZKRATKAE8, NAZEV_STRK = NAZEV_STRE))
    cpp <- read_cpp_xml(here("data", "EP2014", "EP2014ciselniky20140425", "cpp.xml"))
    cns <- read_cns_xml(here("data", "EP2014", "EP2014ciselniky20140425", "cns.xml"))
    read_candidates_xml(here("data", "EP2014", "EP2014reg20140525", "eprk.xml"), 
                        parties, cpp, cns, function(x) {x %>% rename(KSTRANA = ESTRANA)}) %>% 
      mutate(ROK_NAROZENI = 2014 - VEK) %>% 
      categorize_sex
  }),
  
  tar_target(ep_2019, command = {
    parties <- read_parties(here("data", "EP2019", "EP2019reg20190526_xlsx", "eprkl.xlsx"), 
                            function(x) x %>% select(KSTRANA = ESTRANA, ZKRATKAK8 = ZKRATKAE8, NAZEV_STRK = NAZEV_STRE))
    cpp <- read_cpp(here("data", "EP2019", "EP2019ciselniky20190513", "cpp.xlsx"))
    cns <- read_cns(here("data", "EP2019", "EP2019ciselniky20190513", "cns.xlsx"))
    read_candidates(here("data", "EP2019", "EP2019reg20190526_xlsx", "eprk.xlsx"), 
                    parties, cpp, cns, function(x) {x %>% rename(KSTRANA = ESTRANA) %>%
                        mutate(MANDAT = ifelse(MANDAT == "A", 1, 0))}) %>% 
      mutate(ROK_NAROZENI = 2019 - VEK) %>% 
      select(-DATNAR) %>% 
      categorize_sex
  }), 
  
  tar_target(ep_2024, command = {
    parties <- read_parties(here("data", "EP2024", "EP2024reg20240609_xlsx", "eprkl.xlsx"), 
                            function(x) x %>% select(KSTRANA = ESTRANA, ZKRATKAK8 = ZKRATKAE8, NAZEV_STRK = NAZEV_STRE))
    cpp <- read_cpp(here("data", "EP2024", "EP2024ciselniky20240609", "cpp.xlsx"))
    cns <- read_cns(here("data", "EP2024", "EP2024ciselniky20240609", "cns.xlsx"))
    read_candidates(here("data", "EP2024", "EP2024reg20240609_xlsx", "eprk.xlsx"), 
                    parties, cpp, cns, function(x) {x %>% rename(KSTRANA = ESTRANA) %>%
                        mutate(MANDAT = ifelse(MANDAT == "A", 1, 0))}) %>% 
      mutate(ROK_NAROZENI = 2024 - VEK) %>% 
      categorize_sex
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

# Senate elections ----------------------------------------
senate_data <- list(
  tar_target(
    senate_df, {
      candidates <- read_excel("data/SE1996_2022/serk.xlsx")
      
      cpp <- read_cpp("data/SE1996_2022/SENAT_cisel_20230227_xlsx/cpp.xlsx")
      cns <- read_cns("data/SE1996_2022/SENAT_cisel_20230227_xlsx/cns.xlsx")
      
      left_join(candidates, cpp, by = "PSTRANA") %>% 
        left_join(., cns, by = "NSTRANA") %>% 
        mutate(
          year = floor(DATUMVOLEB/10000),
          ROK_NAROZENI = year - VEK
        ) %>% 
        merge_and_recode_titles() %>% 
        categorize_sex
    }
  ), 
  
  tar_target(
    byelection_dates, {
      election_dates <- read_excel("data/SE1996_2022/SENAT_cisel_20230227_xlsx/sedatumvoleb.xlsx")
      
      election_dates %>% 
        filter(!is.na(POPIS_CZ)) %>% 
        pull(DATUMVOLEB)
    }
  ), 
  
  tar_target(
    # check that multiple by-election were not hold in the same Senate district
    byelection_districts_unique, {
      stopifnot(senate_df %>% 
        filter(DATUMVOLEB %in% byelection_dates) %>% 
        select(DATUMVOLEB, OBVOD) %>% 
        unique() %>% 
        count(OBVOD, sort = TRUE) %>% 
        pull(n) %>% 
        all(. == 1))
    }
  ),
  
  tar_target(
    regular_elections, {
      election_dates <- read_excel("data/SE1996_2022/SENAT_cisel_20230227_xlsx/sedatumvoleb.xlsx")
      
      election_dates %>% 
        filter(is.na(POPIS_CZ)) %>% 
        pull(DATUMVOLEB)
  }), 
  
  tar_target(
    sen_1996, {
      senate_df %>% 
        filter(DATUMVOLEB == 19961116)
    }
  ),
  
  tar_target(
    sen_1996a, {
      senate_df %>% 
        filter(DATUMVOLEB == 19961116) %>% 
        filter(OBVOD %in% sen_1998$OBVOD) %>% 
        mutate(row_id = row_number())
    }
  ),
  
  tar_target(
    sen_1996b, {
      senate_df %>% 
        filter(DATUMVOLEB == 19961116) %>% 
        filter(OBVOD %in% sen_2000$OBVOD) %>% 
        mutate(row_id = row_number())
    }
  ),
  
  tar_target(
    sen_1996c, {
      senate_df %>% 
        filter(DATUMVOLEB == 19961116) %>% 
        filter(OBVOD %in% sen_2002$OBVOD) %>% 
        mutate(row_id = row_number())
    }
  ),
  
  tar_target(
    sen_1998, {
      senate_df %>% 
        filter(DATUMVOLEB == 19981114) %>% 
        mutate(row_id = row_number())
    }
  ),
  
  tar_target(
    sen_2000, {
      senate_df %>% 
        filter(DATUMVOLEB == 20001112) %>% 
        mutate(row_id = row_number())
    }
  ),
  
  tar_target(
    sen_2002, {
      senate_df %>% 
        filter(DATUMVOLEB == 20021025) %>% 
        mutate(row_id = row_number())
    }
  ),
  
  tar_target(
    sen_2004, {
      senate_df %>% 
        filter(DATUMVOLEB == 20041105) %>% 
        mutate(row_id = row_number())
    }
  ),
  
  tar_target(
    sen_2006, {
      senate_df %>% 
        filter(DATUMVOLEB == 20061020) %>% 
        mutate(row_id = row_number())
    }
  ),
  
  tar_target(
    sen_2008, {
      senate_df %>% 
        filter(DATUMVOLEB == 20081017) %>% 
        mutate(row_id = row_number())
    }
  ),
  
  tar_target(
    sen_2010, {
      senate_df %>% 
        filter(DATUMVOLEB == 20101015) %>% 
        mutate(row_id = row_number())
    }
  ),
  
  tar_target(
    sen_2012, {
      senate_df %>% 
        filter(DATUMVOLEB == 20121012) %>% 
        mutate(row_id = row_number())
    }
  ),
  
  tar_target(
    sen_2014, {
      senate_df %>% 
        filter(DATUMVOLEB == 20141010) %>% 
        mutate(row_id = row_number())
    }
  ),
  
  tar_target(
    sen_2016, {
      senate_df %>% 
        filter(DATUMVOLEB == 20161007) %>% 
        mutate(row_id = row_number())
    }
  ),
  
  tar_target(
    sen_2018, {
      senate_df %>% 
        filter(DATUMVOLEB == 20181005) %>% 
        mutate(row_id = row_number())
    }
  ),
  
  tar_target(
    sen_2020, {
      senate_df %>% 
        filter(DATUMVOLEB == 20201002) %>% 
        mutate(row_id = row_number())
    }
  ),
  
  tar_target(
    sen_2022, {
      senate_df %>% 
        filter(DATUMVOLEB == 20220923) %>% 
        mutate(row_id = row_number())
    }
  ),
  
  tar_target(
    sen_panel, {
      # A districts
      sa_96_98 <- match_sen_data(sen_1996a, sen_1998)
      
      sa_98_04 <- match_sen_data(sen_1998, sen_2004)
      sa_96_04 <- match_sen_data(
        sen_1996a %>% 
          filter(!row_id %in% sa_96_98$row_id.x), 
        sen_2004 %>% 
          filter(!row_id %in% sa_98_04$row_id.y)
      )
      
      sa_04_10 <- match_sen_data(sen_2004, sen_2010)
      sa_98_10 <- match_sen_data(
        sen_1998 %>% 
          filter(!row_id %in% sa_98_04$row_id.x), 
        sen_2010 %>% 
          filter(!row_id %in% sa_04_10$row_id.y)
      )
      
      sa_96_10 <- match_sen_data(
        sen_1996a %>% 
          filter(!row_id %in% sa_96_98$row_id.x) %>% 
          filter(!row_id %in% sa_96_04$row_id.x), 
        sen_2010 %>% 
          filter(!row_id %in% sa_98_10$row_id.y) %>% 
          filter(!row_id %in% sa_04_10$row_id.y)
      )
      
      sa_10_16 <- match_sen_data(sen_2010, sen_2016)
      sa_04_16 <- match_sen_data(
        sen_2004 %>% 
          filter(!row_id %in% sa_04_10$row_id.x), 
        sen_2016 %>% 
          filter(!row_id %in% sa_10_16$row_id.y)
      )
      
      sa_98_16 <- match_sen_data(
        sen_1998 %>% 
          filter(!row_id %in% sa_98_04$row_id.x) %>% 
          filter(!row_id %in% sa_98_10$row_id.x), 
        sen_2016 %>% 
          filter(!row_id %in% sa_04_16$row_id.y) %>% 
          filter(!row_id %in% sa_10_16$row_id.y)
      )
      
      sa_96_16 <- match_sen_data(
        sen_1996a %>% 
          filter(!row_id %in% sa_96_98$row_id.x) %>% 
          filter(!row_id %in% sa_96_04$row_id.x) %>% 
          filter(!row_id %in% sa_96_10$row_id.x), 
        sen_2016 %>% 
          filter(!row_id %in% sa_98_16$row_id.y) %>% 
          filter(!row_id %in% sa_04_16$row_id.y) %>% 
          filter(!row_id %in% sa_10_16$row_id.y)
      )
      
      sa_16_22 <- match_sen_data(sen_2016, sen_2022)
      sa_10_22 <- match_sen_data(
        sen_2010 %>% 
          filter(!row_id %in% sa_10_16$row_id.x), 
        sen_2022 %>% 
          filter(!row_id %in% sa_16_22$row_id.y)
      )
      
      sa_04_22 <- match_sen_data(
        sen_2004 %>% 
          filter(!row_id %in% sa_04_10$row_id.x) %>% 
          filter(!row_id %in% sa_04_16$row_id.x), 
        sen_2022 %>% 
          filter(!row_id %in% sa_10_22$row_id.y) %>% 
          filter(!row_id %in% sa_16_22$row_id.y)
      )
      
      sa_98_22 <- match_sen_data(
        sen_1998 %>% 
          filter(!row_id %in% sa_98_04$row_id.x) %>% 
          filter(!row_id %in% sa_98_10$row_id.x) %>% 
          filter(!row_id %in% sa_98_16$row_id.x), 
        sen_2022 %>% 
          filter(!row_id %in% sa_04_22$row_id.y) %>% 
          filter(!row_id %in% sa_10_22$row_id.y) %>% 
          filter(!row_id %in% sa_16_22$row_id.y)
      )
      
      sa_96_22 <- match_sen_data(
        sen_1996a %>% 
          filter(!row_id %in% sa_96_98$row_id.x) %>% 
          filter(!row_id %in% sa_96_04$row_id.x) %>% 
          filter(!row_id %in% sa_96_10$row_id.x) %>% 
          filter(!row_id %in% sa_96_16$row_id.x), 
        sen_2022 %>% 
          filter(!row_id %in% sa_98_22$row_id.y) %>% 
          filter(!row_id %in% sa_04_22$row_id.y) %>% 
          filter(!row_id %in% sa_10_22$row_id.y) %>% 
          filter(!row_id %in% sa_16_22$row_id.y)
      )
      
      sa_96_98b <- sa_96_98 %>% select(row_id_1996 = row_id.x, row_id_1998 = row_id.y)
      sa_96_04b <- sa_96_04 %>% select(row_id_1996 = row_id.x, row_id_2004 = row_id.y)
      sa_96_10b <- sa_96_10 %>% select(row_id_1996 = row_id.x, row_id_2010 = row_id.y)
      sa_96_16b <- sa_96_16 %>% select(row_id_1996 = row_id.x, row_id_2016 = row_id.y)
      sa_96_22b <- sa_96_22 %>% select(row_id_1996 = row_id.x, row_id_2022 = row_id.y)
      
      sa_98_04b <- sa_98_04 %>% select(row_id_1998 = row_id.x, row_id_2004 = row_id.y)
      sa_98_10b <- sa_98_10 %>% select(row_id_1998 = row_id.x, row_id_2010 = row_id.y)
      sa_98_16b <- sa_98_16 %>% select(row_id_1998 = row_id.x, row_id_2016 = row_id.y)
      sa_98_22b <- sa_98_22 %>% select(row_id_1998 = row_id.x, row_id_2022 = row_id.y)
      
      sa_04_10b <- sa_04_10 %>% select(row_id_2004 = row_id.x, row_id_2010 = row_id.y)
      sa_04_16b <- sa_04_16 %>% select(row_id_2004 = row_id.x, row_id_2016 = row_id.y)
      sa_04_22b <- sa_04_22 %>% select(row_id_2004 = row_id.x, row_id_2022 = row_id.y)
      
      sa_10_16b <- sa_10_16 %>% select(row_id_2010 = row_id.x, row_id_2016 = row_id.y)
      sa_10_22b <- sa_10_22 %>% select(row_id_2010 = row_id.x, row_id_2022 = row_id.y)
      
      sa_16_22b <- sa_16_22 %>% select(row_id_2016 = row_id.x, row_id_2022 = row_id.y)
      
      
      (pivot_table_a <- sa_96_98b %>% 
          bind_rows(., sen_1996a %>% select(row_id_1996 = row_id) %>% 
                      filter(!row_id_1996 %in% sa_96_98b$row_id_1996)) %>% 
          full_join(., sa_98_04b %>% 
                      bind_rows(., sen_1998 %>% select(row_id_1998 = row_id) %>% 
                                  filter(!row_id_1998 %in% sa_98_04b$row_id_1998))) %>% 
          insert_nonconsecutive(., sa_96_04b, "row_id_1996", "row_id_2004") %>% 
          full_join(., sa_04_10b %>% 
                      bind_rows(., sen_2004 %>% select(row_id_2004 = row_id) %>% 
                                  filter(!row_id_2004 %in% sa_04_10b$row_id_2004))) %>% 
          insert_nonconsecutive(., sa_96_10b, "row_id_1996", "row_id_2010") %>% 
          insert_nonconsecutive(., sa_98_10b, "row_id_1998", "row_id_2010") %>% 
          full_join(., sa_10_16b %>% 
                      bind_rows(., sen_2010 %>% select(row_id_2010 = row_id) %>% 
                                  filter(!row_id_2010 %in% sa_10_16b$row_id_2010))) %>% 
          insert_nonconsecutive(., sa_96_16b, "row_id_1996", "row_id_2016") %>% 
          insert_nonconsecutive(., sa_98_16b, "row_id_1998", "row_id_2016") %>% 
          insert_nonconsecutive(., sa_04_16b, "row_id_2004", "row_id_2016") %>% 
          full_join(., sa_16_22b %>% 
                      bind_rows(., sen_2016 %>% select(row_id_2016 = row_id) %>% 
                                  filter(!row_id_2016 %in% sa_16_22b$row_id_2016))) %>% 
          insert_nonconsecutive(., sa_96_22b, "row_id_1996", "row_id_2022") %>% 
          insert_nonconsecutive(., sa_98_22b, "row_id_1998", "row_id_2022") %>% 
          insert_nonconsecutive(., sa_04_22b, "row_id_2004", "row_id_2022") %>% 
          insert_nonconsecutive(., sa_10_22b, "row_id_2010", "row_id_2022") %>% 
          bind_rows(., sen_2022 %>% select(row_id_2022 = row_id) %>%
                      filter(!row_id_2022 %in% c(sa_16_22b$row_id_2022, 
                                                 sa_10_22b$row_id_2022, 
                                                 sa_04_22b$row_id_2022, 
                                                 sa_98_22b$row_id_2022, 
                                                 sa_96_22b$row_id_2022)))
      )
      
      pivot_table_long_a <- pivot_table_a %>%
        mutate(person_id = paste0("SA", row_number())) %>% 
        tidyr::pivot_longer(., cols = 1:(ncol(.)-1), names_to = "year",
                            values_to = "row_id") %>%
        filter(!is.na(row_id)) %>%
        mutate(year = stringr::str_extract(year, "[0-9]{4}"))
      
      sa_candidates <- bind_rows(
        sen_1996a %>% mutate(year = "1996"),
        sen_1998 %>% mutate(year = "1998"),
        sen_2004 %>% mutate(year = "2004"),
        sen_2010 %>% mutate(year = "2010"),
        sen_2016 %>% mutate(year = "2016"),
        sen_2022 %>% mutate(year = "2022")
      )
      
      sen_panel_a <- full_join(pivot_table_long_a, sa_candidates, by = c("row_id", "year"))
      
      ## SB ----------------------------------------------------------------------
      sb_96_00 <- match_sen_data(sen_1996b, sen_2000)
      
      sb_00_06 <- match_sen_data(sen_2000, sen_2006)
      
      sb_96_06 <- match_sen_data(
        sen_1996b %>% 
          filter(!row_id %in% sb_96_00$row_id.x), 
        sen_2006 %>% 
          filter(!row_id %in% sb_00_06$row_id.y)
      )
      
      sb_06_12 <- match_sen_data(sen_2006, sen_2012)
      
      sb_00_12 <- match_sen_data(
        sen_2000 %>% 
          filter(!row_id %in% sb_00_06$row_id.x),
        sen_2012 %>% 
          filter(!row_id %in% sb_06_12$row_id.y)
      )
      
      sb_96_12 <- match_sen_data(
        sen_1996b %>% 
          filter(!row_id %in% sb_96_00$row_id.x) %>% 
          filter(!row_id %in% sb_96_06$row_id.x),
        sen_2012 %>% 
          filter(!row_id %in% sb_00_12$row_id.y) %>% 
          filter(!row_id %in% sb_06_12$row_id.y)
      )
      
      sb_12_18 <- match_sen_data(sen_2012, sen_2018)
      
      sb_06_18 <- match_sen_data(
        sen_2006 %>% 
          filter(!row_id %in% sb_06_12$row_id.x),
        sen_2018 %>% 
          filter(!row_id %in% sb_12_18$row_id.y)
      )
      
      sb_00_18 <- match_sen_data(
        sen_2000 %>% 
          filter(!row_id %in% sb_00_06$row_id.x) %>% 
          filter(!row_id %in% sb_00_12$row_id.x),
        sen_2018 %>% 
          filter(!row_id %in% sb_06_18$row_id.y) %>% 
          filter(!row_id %in% sb_12_18$row_id.y)
      )
      
      sb_96_18 <- match_sen_data(
        sen_1996b %>% 
          filter(!row_id %in% sb_96_00$row_id.x) %>% 
          filter(!row_id %in% sb_96_06$row_id.x) %>% 
          filter(!row_id %in% sb_96_12$row_id.x),
        sen_2018 %>% 
          filter(!row_id %in% sb_00_18$row_id.y) %>% 
          filter(!row_id %in% sb_06_18$row_id.y) %>% 
          filter(!row_id %in% sb_12_18$row_id.y)
      )
      
      sb_96_00b <- sb_96_00 %>% select(row_id_1996 = row_id.x, row_id_2000 = row_id.y)
      sb_96_06b <- sb_96_06 %>% select(row_id_1996 = row_id.x, row_id_2006 = row_id.y)
      sb_96_12b <- sb_96_12 %>% select(row_id_1996 = row_id.x, row_id_2012 = row_id.y)
      sb_96_18b <- sb_96_18 %>% select(row_id_1996 = row_id.x, row_id_2018 = row_id.y)
      
      sb_00_06b <- sb_00_06 %>% select(row_id_2000 = row_id.x, row_id_2006 = row_id.y)
      sb_00_12b <- sb_00_12 %>% select(row_id_2000 = row_id.x, row_id_2012 = row_id.y)
      sb_00_18b <- sb_00_18 %>% select(row_id_2000 = row_id.x, row_id_2018 = row_id.y)
      
      sb_06_12b <- sb_06_12 %>% select(row_id_2006 = row_id.x, row_id_2012 = row_id.y)
      sb_06_18b <- sb_06_18 %>% select(row_id_2006 = row_id.x, row_id_2018 = row_id.y)
      
      sb_12_18b <- sb_12_18 %>% select(row_id_2012 = row_id.x, row_id_2018 = row_id.y)
      
      (pivot_table_b <- sb_96_00b %>% 
          bind_rows(., sen_1996b %>% select(row_id_1996 = row_id) %>% 
                      filter(!row_id_1996 %in% sb_96_00b$row_id_1996)) %>% 
          full_join(., sb_00_06b %>% 
                      bind_rows(., sen_2000 %>% select(row_id_2000 = row_id) %>% 
                                  filter(!row_id_2000 %in% sb_00_06b$row_id_2000))) %>% 
          insert_nonconsecutive(., sb_96_06b, "row_id_1996", "row_id_2006") %>% 
          full_join(., sb_06_12b %>% 
                      bind_rows(., sen_2006 %>% select(row_id_2006 = row_id) %>% 
                                  filter(!row_id_2006 %in% sb_06_12b$row_id_2006))) %>% 
          insert_nonconsecutive(., sb_96_12b, "row_id_1996", "row_id_2012") %>% 
          insert_nonconsecutive(., sb_00_12b, "row_id_2000", "row_id_2012") %>% 
          full_join(., sb_12_18b %>% 
                      bind_rows(., sen_2012 %>% select(row_id_2012 = row_id) %>% 
                                  filter(!row_id_2012 %in% sb_12_18b$row_id_2012))) %>% 
          insert_nonconsecutive(., sb_96_18b, "row_id_1996", "row_id_2018") %>% 
          insert_nonconsecutive(., sb_00_18b, "row_id_2000", "row_id_2018") %>% 
          insert_nonconsecutive(., sb_06_18b, "row_id_2006", "row_id_2018") %>% 
          bind_rows(., sen_2018 %>% select(row_id_2018 = row_id) %>%
                      filter(!row_id_2018 %in% c(sb_12_18b$row_id_2018, 
                                                 sb_06_18b$row_id_2018, 
                                                 sb_00_18b$row_id_2018, 
                                                 sb_96_18b$row_id_2018)))
        
      )
      
      pivot_table_long_b <- pivot_table_b %>%
        mutate(person_id = paste0("SB", row_number())) %>% 
        tidyr::pivot_longer(., cols = 1:(ncol(.)-1), names_to = "year",
                            values_to = "row_id") %>%
        filter(!is.na(row_id)) %>%
        mutate(year = stringr::str_extract(year, "[0-9]{4}"))
      
      sb_candidates <- bind_rows(
        sen_1996b %>% mutate(year = "1996"),
        sen_2000 %>% mutate(year = "2000"),
        sen_2006 %>% mutate(year = "2006"),
        sen_2012 %>% mutate(year = "2012"),
        sen_2018 %>% mutate(year = "2018")
      )
      
      sen_panel_b <- full_join(pivot_table_long_b, sb_candidates, by = c("row_id", "year"))
      
      # SC ----------------------------------------------------------------------
      sc_96_02 <- match_sen_data(sen_1996c, sen_2002)
      
      sc_02_08 <- match_sen_data(sen_2002, sen_2008)
      
      sc_96_08 <- match_sen_data(
        sen_1996c %>% 
          filter(!row_id %in% sc_96_02$row_id.x),
        sen_2008 %>% 
          filter(!row_id %in% sc_02_08$row_id.y)
      )
      
      sc_08_14 <- match_sen_data(sen_2008, sen_2014)
      
      sc_02_14 <- match_sen_data(
        sen_2002 %>% 
          filter(!row_id %in% sc_02_08$row_id.x),
        sen_2014 %>% 
          filter(!row_id %in% sc_08_14$row_id.y)
      )
      
      sc_96_14 <- match_sen_data(
        sen_1996c %>% 
          filter(!row_id %in% sc_96_02$row_id.x) %>% 
          filter(!row_id %in% sc_96_08$row_id.x),
        sen_2014 %>% 
          filter(!row_id %in% sc_02_14$row_id.y) %>% 
          filter(!row_id %in% sc_08_14$row_id.y)
      )
      
      sc_14_20 <- match_sen_data(sen_2014, sen_2020)
      
      sc_08_20 <- match_sen_data(
        sen_2008 %>% 
          filter(!row_id %in% sc_08_14$row_id.x),
        sen_2020 %>% 
          filter(!row_id %in% sc_14_20$row_id.y)
      )
      
      sc_02_20 <- match_sen_data(
        sen_2002 %>% 
          filter(!row_id %in% sc_02_08$row_id.x) %>% 
          filter(!row_id %in% sc_02_14$row_id.x),
        sen_2020 %>% 
          filter(!row_id %in% sc_08_20$row_id.y) %>% 
          filter(!row_id %in% sc_14_20$row_id.y)
      )
      
      sc_96_20 <- match_sen_data(
        sen_1996c %>% 
          filter(!row_id %in% sc_96_02$row_id.x) %>% 
          filter(!row_id %in% sc_96_08$row_id.x) %>% 
          filter(!row_id %in% sc_96_14$row_id.x),
        sen_2020 %>% 
          filter(!row_id %in% sc_02_20$row_id.y) %>% 
          filter(!row_id %in% sc_08_20$row_id.y) %>% 
          filter(!row_id %in% sc_14_20$row_id.y)
      )
      
      sc_96_02b <- sc_96_02 %>% select(row_id_1996 = row_id.x, row_id_2002 = row_id.y)
      sc_96_08b <- sc_96_08 %>% select(row_id_1996 = row_id.x, row_id_2008 = row_id.y)
      sc_96_14b <- sc_96_14 %>% select(row_id_1996 = row_id.x, row_id_2014 = row_id.y)
      sc_96_20b <- sc_96_20 %>% select(row_id_1996 = row_id.x, row_id_2020 = row_id.y)
      
      sc_02_08b <- sc_02_08 %>% select(row_id_2002 = row_id.x, row_id_2008 = row_id.y)
      sc_02_14b <- sc_02_14 %>% select(row_id_2002 = row_id.x, row_id_2014 = row_id.y)
      sc_02_20b <- sc_02_20 %>% select(row_id_2002 = row_id.x, row_id_2020 = row_id.y)
      
      sc_08_14b <- sc_08_14 %>% select(row_id_2008 = row_id.x, row_id_2014 = row_id.y)
      sc_08_20b <- sc_08_20 %>% select(row_id_2008 = row_id.x, row_id_2020 = row_id.y)
      
      sc_14_20b <- sc_14_20 %>% select(row_id_2014 = row_id.x, row_id_2020 = row_id.y)
      
      (pivot_table_c <- sc_96_02b %>% 
          bind_rows(., sen_1996c %>% select(row_id_1996 = row_id) %>% 
                      filter(!row_id_1996 %in% sc_96_02b$row_id_1996)) %>% 
          full_join(., sc_02_08b %>% 
                      bind_rows(., sen_2002 %>% select(row_id_2002 = row_id) %>% 
                                  filter(!row_id_2002 %in% sc_02_08b$row_id_2002))) %>% 
          insert_nonconsecutive(., sc_96_08b, "row_id_1996", "row_id_2008") %>% 
          full_join(., sc_08_14b %>% 
                      bind_rows(., sen_2008 %>% select(row_id_2008 = row_id) %>% 
                                  filter(!row_id_2008 %in% sc_08_14b$row_id_2008))) %>% 
          insert_nonconsecutive(., sc_96_14b, "row_id_1996", "row_id_2014") %>% 
          insert_nonconsecutive(., sc_02_14b, "row_id_2002", "row_id_2014") %>% 
          full_join(., sc_14_20b %>% 
                      bind_rows(., sen_2014 %>% select(row_id_2014 = row_id) %>% 
                                  filter(!row_id_2014 %in% sc_14_20b$row_id_2014))) %>% 
          insert_nonconsecutive(., sc_96_20b, "row_id_1996", "row_id_2020") %>% 
          insert_nonconsecutive(., sc_02_20b, "row_id_2002", "row_id_2020") %>% 
          insert_nonconsecutive(., sc_08_20b, "row_id_2008", "row_id_2020") %>% 
          bind_rows(., sen_2020 %>% select(row_id_2020 = row_id) %>%
                      filter(!row_id_2020 %in% c(sc_14_20b$row_id_2020, 
                                                 sc_08_20b$row_id_2020, 
                                                 sc_02_20b$row_id_2020, 
                                                 sc_96_20b$row_id_2020)))
      )
      
      pivot_table_long_c <- pivot_table_c %>%
        mutate(person_id = paste0("SC", row_number())) %>% 
        tidyr::pivot_longer(., cols = 1:(ncol(.)-1), names_to = "year",
                            values_to = "row_id") %>%
        filter(!is.na(row_id)) %>%
        mutate(year = stringr::str_extract(year, "[0-9]{4}"))
      
      sc_candidates <- bind_rows(
        sen_1996c %>% mutate(year = "1996"),
        sen_2002 %>% mutate(year = "2002"),
        sen_2008 %>% mutate(year = "2008"),
        sen_2014 %>% mutate(year = "2014"),
        sen_2020 %>% mutate(year = "2020")
      )
      
      sen_panel_c <- full_join(pivot_table_long_c, sc_candidates, by = c("row_id", "year"))
      
      sen_byelection <- senate_df %>% 
        filter(DATUMVOLEB %in% byelection_dates) %>% 
        mutate(
          row_id = row_number(), 
          person_id = paste0("B", row_id)
        )
      
      sen_a_b <- match_sen_data(sen_panel_a, sen_panel_b, c("JMENO", "PRIJMENI"))
      sen_b_c <- match_sen_data(sen_panel_b, sen_panel_c, c("JMENO", "PRIJMENI"))
      sen_a_c <- match_sen_data(sen_panel_a, sen_panel_c, c("JMENO", "PRIJMENI"))
      
      sen_a_b_pivot <- sen_a_b %>% 
        select(person_id.x, person_id.y) %>% 
        unique() 
      
      sen_b_c_pivot <- sen_b_c %>% 
        select(person_id.x, person_id.y) %>% 
        unique() 
      
      sen_a_c_pivot <- sen_a_c %>% 
        select(person_id.x, person_id.y) %>% 
        unique() 
      
      tmp <- sen_a_b_pivot %>% 
        rename(sen_a_id = person_id.x, sen_b_id = person_id.y) %>% 
        bind_rows(., sen_panel_a %>% 
                    select(sen_a_id = person_id) %>% 
                    unique() %>% 
                    filter(!sen_a_id %in% sen_a_b_pivot$person_id.x)) %>% 
        full_join(., 
                  sen_b_c_pivot %>% 
                    rename(sen_b_id = person_id.x, sen_c_id = person_id.y) %>% 
                    bind_rows(., sen_panel_b %>% 
                                select(sen_b_id = person_id) %>% 
                                unique() %>% 
                                filter(!sen_b_id %in% sen_b_c_pivot$person_id.x))
        ) %>% 
        insert_nonconsecutive(., sen_a_c_pivot %>% rename(sen_a_id = person_id.x, 
                                                          sen_c_id = person_id.y), 
                              "sen_a_id", "sen_c_id") %>% 
        bind_rows(., sen_panel_c %>% 
                    select(sen_c_id = person_id) %>% 
                    unique() %>% 
                    filter(!sen_c_id %in% c(sen_b_c_pivot$person_id.y, 
                                            sen_a_c_pivot$person_id.y)))
      
      pivot_tmp_long <- tmp %>%
        mutate(person_id = paste0("SN", row_number())) %>% 
        tidyr::pivot_longer(., cols = 1:(ncol(.)-1), names_to = "panel",
                            values_to = "panel_id") %>%
        filter(!is.na(panel_id))
      
      sen_panel_candidates <- bind_rows(
        sen_panel_a %>% mutate(panel = "sen_a_id") %>% rename(panel_id = person_id),
        sen_panel_b %>% mutate(panel = "sen_b_id") %>% rename(panel_id = person_id),
        sen_panel_c %>% mutate(panel = "sen_c_id") %>% rename(panel_id = person_id)
      )
      
      senate_panel <- full_join(pivot_tmp_long, sen_panel_candidates, by = c("panel_id", "panel"))
      
      sen_byel <- match_sen_data(senate_panel, sen_byelection)
      
      sen_byel_b <- sen_byel %>% select(panel_id = person_id.x, byel_id = person_id.y)
      sen_byel_pivot_long <- sen_byel_b %>% 
        bind_rows(., senate_panel %>% 
                    select(panel_id = person_id) %>% 
                    unique() %>% 
                    filter(!panel_id %in% sen_byel_b$panel_id)) %>% 
        bind_rows(., sen_byelection %>% 
                    select(byel_id = person_id) %>% 
                    filter(!byel_id %in% sen_byel_b$byel_id)) %>% 
        mutate(person_id = paste0("SEN", row_number())) %>% 
        tidyr::pivot_longer(., cols = 1:(ncol(.)-1), names_to = "panel",
                            values_to = "panel_id") %>%
        filter(!is.na(panel_id))
      
      sen_all_candidates <- bind_rows(
        senate_panel %>% 
          mutate(panel = "panel_id") %>% 
          mutate(year = as.numeric(year)) %>% 
          select(-panel_id), 
        sen_byelection %>% 
          mutate(panel = "byel_id")
      )
      
      full_join(sen_byel_pivot_long, sen_all_candidates, by = c("panel_id"="person_id", "panel")) %>% 
        arrange(person_id, year) %>% 
        select(-c(panel, panel_id))
      
    }
  ),
  
  # check, že se nikdo neztratil
  tar_target(
    sen_panel_check, 
    stopifnot(nrow(senate_df) == nrow(sen_panel))
  ),
  
  # check, že nikdo nekandidoval 2x ve stejných volbách
  tar_target(
    sen_panel_check_unique_date_per_person, {
      stopifnot(sen_panel %>% 
                  count(person_id, DATUMVOLEB, sort = TRUE) %>% 
                  pull(n) %>% all(. == 1))
    }
  ),
  
  NULL
)

# Summary stats -------------------------------------------
summary_stats <- list(
  tar_target(n_candidates, {
    bind_rows(
      psp_panel %>% count(year) %>% 
        mutate(year = paste0("PS ", year)),
      reg_panel %>% count(year) %>% 
        mutate(year = paste0("Reg. ", year)),
      ep_panel %>% count(year) %>% 
        mutate(year = paste0("EP ", year)), 
      m_panel %>% 
        count(year) %>% 
        mutate(year = paste0("Municipal ", year)), 
      mc_panel %>% 
        count(year) %>% 
        mutate(year = paste0("City districts ", year)),
      sen_panel %>% 
        mutate(election = case_when(
          DATUMVOLEB %in% c(19961116, 19981114, 
                            20001112, 20021025,
                            20041105, 20061020, 
                            20081017, 20101015,
                            20121012, 20141010, 
                            20181005, 20201002,
                            20220923) ~ paste0("Senate Election ", year), 
          TRUE ~ paste0("Senate By-election ", year)
        )) %>% 
        count(year, election) %>% 
        select(year = election, n)
    )
  }),
  
  tar_target(n_candidates_xlsx, {
    writexl::write_xlsx(n_candidates, "figs/n_candidates.xlsx")
  }), 
  
  tar_target(n_candidates_tex, {
    knitr::kable(n_candidates, col.names = c("Election", "N of candidates"), 
                 format = "latex") %>% 
      writeLines(., "figs/n_candidates.tex")
  })
)

# Matching panels together --------------------------------
matched_panels <- list(
  ## Městské části - Obce  --------------------------------
  tar_target(
    mc_mun_panel, {
      mc_obec <- read_csv("data/mcast_obec.csv", locale = locale(encoding = "WINDOWS-1250")) %>% 
        select(MC_KODZASTUP = CHODNOTA1, KODZASTUP = CHODNOTA2)
      
      mc_panel_harm <- mc_panel %>% 
        rename(MC_KODZASTUP = KODZASTUP) %>% 
        left_join(mc_obec, by = "MC_KODZASTUP")
      
      match_m_mc <- match_mun_panel_data(m_panel, mc_panel_harm)
      
      x_multiple <- match_m_mc %>%
        select(person_id.x, person_id.y) %>%
        unique() %>% 
        count(person_id.x) %>% 
        filter(n > 1)
      
      y_multiple <- match_m_mc %>%
        select(person_id.x, person_id.y) %>%
        unique() %>% 
        count(person_id.y) %>% 
        filter(n > 1)
      
      stopifnot(match_m_mc %>% 
        select(person_id.x, person_id.y) %>%
        unique() %>% 
        filter(person_id.x %in% x_multiple & 
                 person_id.y %in% y_multiple) %>% 
        nrow() == 0)
      
      unique_ids <- match_m_mc %>%
        group_by(person_id.x, person_id.y) %>%
        summarise(score = max(score), .groups = "drop")
      
      x_unique <- unique_ids %>% 
        filter(person_id.x %in% x_multiple$person_id.x) %>% 
        group_by(person_id.x) %>% 
        summarise(person_id.y = list(person_id.y), 
                  score = list(score)) %>% 
        mutate(person_id.x = as.list(person_id.x))
      
      y_unique <- unique_ids %>% 
        filter(person_id.y %in% y_multiple$person_id.y) %>% 
        group_by(person_id.y) %>% 
        summarise(person_id.x = list(person_id.x), 
                  score = list(score)) %>% 
        mutate(person_id.y = as.list(person_id.y))
      
      both_unique <- unique_ids %>% 
        filter(!(person_id.y %in% y_multiple$person_id.y | 
                   person_id.x %in% x_multiple$person_id.x)) %>% 
        mutate(person_id.x = as.list(person_id.x), 
               person_id.y = as.list(person_id.y), 
               score = as.list(score))
      
      m_volby <- m_panel %>% 
        group_by(person_id) %>% summarise(year.x = list(year))
      mc_volby <- mc_panel %>% 
        group_by(person_id) %>% summarise(year.y = list(year))
      
      tmp <- bind_rows(
        x_unique, 
        y_unique, 
        both_unique
      ) %>% 
        # bind_rows(., missing_x) %>% 
        # bind_rows(., missing_y) %>% 
        mutate(panel_id = paste0("MM", row_number())) %>% 
        unnest(c(person_id.x, person_id.y, score)) %>% 
        left_join(., mc_volby, by = c("person_id.y"="person_id")) %>% 
        left_join(., m_volby, by = c("person_id.x"="person_id")) %>% 
        group_by(panel_id, person_id.x) %>% 
        mutate(n_y = length(unique(person_id.y))) %>% 
        group_by(panel_id, person_id.y) %>% 
        mutate(n_x = length(unique(person_id.x)))
      
      tmp_y <- tmp %>% filter(n_y > 1)
      y_keep <- tmp_y %>% 
        unnest(year.y) %>% 
        group_by(panel_id) %>% 
        mutate(
          n = n(), 
          unique_years = length(unique(year.y)), 
          intersection = n != unique_years,
          exclude = intersection & score != max(score)) %>%
        filter(!exclude) %>% 
        ungroup()
      
      tmp_x <- tmp %>% filter(n_x > 1)
      x_keep <- tmp_x %>% 
        anti_join(., y_keep, by = c("person_id.x", "person_id.y")) %>% 
        unnest(year.x) %>% 
        group_by(panel_id) %>% 
        mutate(
          n = n(), 
          unique_years = length(unique(year.x)), 
          intersection = n != unique_years,
          exclude = intersection & score != max(score)) %>%
        filter(!exclude) %>% 
        ungroup()
      
      all_unique <- bind_rows(
        x_keep %>% 
          select(-c(year.x, year.y)) %>% 
          select(person_id.x, person_id.y) %>% 
          unique() %>% 
          group_by(person_id.y) %>% 
          summarise(person_id.x = list(person_id.x), .groups = "drop") %>% 
          mutate(person_id.y = as.list(person_id.y)), 
        y_keep %>% 
          select(-c(year.x, year.y)) %>% 
          select(person_id.x, person_id.y) %>% 
          unique() %>% 
          group_by(person_id.x) %>% 
          summarise(person_id.y = list(person_id.y), .groups = "drop") %>% 
          mutate(person_id.x = as.list(person_id.x)), 
        tmp %>% 
          ungroup %>% 
          filter(n_x == 1, n_y == 1) %>% 
          select(person_id.x, person_id.y) %>% 
          mutate(across(everything(), as.list))
      )
      
      missing_x <- m_panel %>% 
        filter(!person_id %in% unlist(all_unique$person_id.x)) %>% 
        select(person_id.x = person_id) %>% 
        unique() %>% 
        mutate(person_id.x = as.list(person_id.x))
      
      missing_y <- mc_panel %>% 
        filter(!person_id %in% unlist(all_unique$person_id.y)) %>% 
        select(person_id.y = person_id) %>% 
        unique() %>% 
        mutate(person_id.y = as.list(person_id.y))
      
      pivot_table <- all_unique %>% 
        bind_rows(., missing_x) %>% 
        bind_rows(., missing_y) %>% 
        mutate(panel_id = paste0("MM", row_number())) %>% 
        unnest(c(person_id.x, person_id.y)) %>% 
        pivot_longer(., cols = c(person_id.x, person_id.y), names_to = "dataset", 
                     values_to = "person_id") %>% 
        select(-dataset) %>% 
        unique() %>% 
        filter(!is.na(person_id))
      
      all_candidates <- 
        bind_rows(
          m_panel %>% mutate(election = paste0("M", year)), 
          mc_panel_harm %>% mutate(election = paste0("MC", year))
        )
      
      duplicates <- pivot_table %>% 
        count(person_id, sort = TRUE) %>% 
        filter(n > 1)
      
      pivot_table2 <- pivot_table
      for(i in duplicates$person_id){
        check_ids <- pivot_table %>% 
          group_by(panel_id) %>% 
          filter(any(person_id == i))
        (check_df <- all_candidates %>% 
          filter(person_id %in% check_ids$person_id) %>% 
          select(year, election, JMENO, PRIJMENI, POVOLANI, ROK_NAROZENI, ZKRATKAN8, 
                 ZKRATKAP8))
        
        ok <- (anyDuplicated(check_df$election) == 0)
        if(ok){
          pivot_table2 <- pivot_table2 %>% 
            group_by(panel_id) %>% 
            mutate(change = any(person_id == i)) %>% 
            ungroup %>% 
            group_by(change) %>% 
            mutate(panel_id = if_else(change, head(panel_id, 1), panel_id))
        }else{
          stop("Error")
        }
      }
      
      pivot_table_final <- pivot_table2 %>% 
        ungroup %>% 
        select(panel_id, person_id) %>% 
        unique()
      
      # pivot_table_final      
      full_join(all_candidates, pivot_table_final, by = "person_id",
                  relationship = "many-to-one") %>%
        select(panel_id, person_id, everything())
    }
  ),
  
  tar_target(
    mc_mun_check, 
    stopifnot(
      nrow(mc_mun_panel) == (nrow(m_panel) + nrow(mc_panel))
    )
  ),
  
  tar_target(
    mun_panel_file_dta, 
    haven::write_dta(mc_mun_panel, "output/municipal_panel.dta")
  ),
  
  tar_target(
    mun_panel_file_rds, 
    saveRDS(mc_mun_panel, "output/municipal_panel.rds")
  ),
  
  ## Panel - Kraje ----------------------------------------
  tar_target(
    mun_reg_panel, {
      
      missing_obce <- tribble(
        ~KODZASTUP, ~KRAJ_NAZEV,
        530255, "Středočeský kraj",
        533131, "Středočeský kraj",
        537012, "Středočeský kraj",
        598534, "Středočeský kraj",
        542245, "Plzeňský kraj",
        542261, "Plzeňský kraj",
        542300, "Plzeňský kraj",
        557978, "Plzeňský kraj",
        559199, "Plzeňský kraj",
        544311, "Liberecký kraj",
        574414, "Pardubický kraj",
        589900, "Olomoucký kraj",
        549819, "Jihomoravský kraj",
        595942, "Jihomoravský kraj",
        596124, "Kraj Vysočina",
        598984, "Moravskoslezský kraj", # Havířov
        569763, "Olomoucký kraj",
        568252, "Moravskoslezský kraj",
        408913, "Liberecký kraj", # Vratislavice
        117820, "Pardubický kraj", # MO Pardubice
        141208, "Pardubický kraj", # MO Pardubice
        410608, "Pardubický kraj", # MO Pardubice
        574716, "Pardubický kraj",
        575020, "Pardubický kraj",
        95141, "Moravskoslezský kraj", # Opava
        111872, "Moravskoslezský kraj", # Opava
        124630, "Moravskoslezský kraj", # Opava
        159182, "Moravskoslezský kraj", # Opava
        177199, "Moravskoslezský kraj", # Opava
        183601, "Moravskoslezský kraj", # Opava
        193232, "Moravskoslezský kraj", # Opava
        413917, "Moravskoslezský kraj") # Opava
      
      mun_kraj <- read_csv("data/obec_kraj.csv", 
                           locale = locale(encoding = "WINDOWS-1250")) %>% 
        select(KODZASTUP = CHODNOTA1, 
               KRAJ_NAZEV = TEXT2) %>% 
        bind_rows(., missing_obce)
      
      kraje <- tribble(
        ~KRZAST, ~KRAJ_NAZEV,
        1, "Středočeský kraj",
        2, "Jihočeský kraj",
        3, "Plzeňský kraj",
        4, "Karlovarský kraj",
        5, "Ústecký kraj",
        6, "Liberecký kraj",
        7, "Královéhradecký kraj",
        8, "Pardubický kraj",
        9, "Kraj Vysočina",
        10, "Jihomoravský kraj",
        11,	"Olomoucký kraj",
        12,	"Zlínský kraj",
        13,	"Moravskoslezský kraj"
      )
      
      mun_panel_harm <- mc_mun_panel %>% 
        left_join(., mun_kraj, by = c("MUNICIPALITY"="KODZASTUP")) %>% 
        select(-person_id) %>% 
        rename(person_id = panel_id)
      reg_panel_harm <- reg_panel %>% 
        left_join(., kraje, by = "KRZAST") %>% 
        mutate(election = paste0("R", year))
      
      mun_reg_match <- match_mun_reg_panel(mun_panel_harm, reg_panel_harm)
      
      mun_reg_unique <- mun_reg_match %>% 
        group_by(person_id.x, person_id.y) %>% 
        summarise(score = max(score))
      
      x_multiple <- mun_reg_unique %>% 
        count(person_id.x, sort = TRUE) %>% 
        filter(n > 1)
      
      y_multiple <- mun_reg_unique %>% 
        count(person_id.y, sort = TRUE) %>% 
        filter(n > 1)
      
      stopifnot(mun_reg_unique %>% 
                  select(person_id.x, person_id.y) %>%
                  unique() %>% 
                  filter(person_id.x %in% x_multiple & 
                           person_id.y %in% y_multiple) %>% 
                  nrow() == 0)
      
      x_unique <- mun_reg_unique %>% 
        filter(person_id.x %in% x_multiple$person_id.x) %>% 
        group_by(person_id.x) %>% 
        summarise(person_id.y = list(person_id.y), 
                  score = list(score)) %>% 
        mutate(person_id.x = as.list(person_id.x))
      
      y_unique <- mun_reg_unique %>% 
        filter(person_id.y %in% y_multiple$person_id.y) %>% 
        group_by(person_id.y) %>% 
        summarise(person_id.x = list(person_id.x), 
                  score = list(score)) %>% 
        mutate(person_id.y = as.list(person_id.y))
      
      both_unique <- mun_reg_unique %>%
        filter(!(person_id.y %in% y_multiple$person_id.y |
                   person_id.x %in% x_multiple$person_id.x)) %>%
        mutate(person_id.x = as.list(person_id.x),
               person_id.y = as.list(person_id.y),
               score = as.list(score))
      
      m_volby <- mun_panel_harm %>%
        group_by(person_id) %>%
        summarise(election.x = list(election))
      reg_volby <- reg_panel_harm %>%
        group_by(person_id) %>% 
        summarise(election.y = list(election))
      
      tmp <- bind_rows(
        x_unique,
        y_unique,
        both_unique
      ) %>%
        mutate(panel_id = paste0("RM", row_number())) %>%
        unnest(c(person_id.x, person_id.y, score)) %>%
        left_join(., reg_volby, by = c("person_id.y"="person_id")) %>%
        left_join(., m_volby, by = c("person_id.x"="person_id")) %>%
        group_by(panel_id, person_id.x) %>%
        mutate(n_y = length(unique(person_id.y))) %>%
        group_by(panel_id, person_id.y) %>%
        mutate(n_x = length(unique(person_id.x)))
      
      tmp_y <- tmp %>% filter(n_y > 1)
      y_keep <- tmp_y %>% 
        unnest(election.y) %>% 
        group_by(panel_id) %>% 
        mutate(
          n = n(), 
          unique_years = length(unique(election.y)), 
          intersection = n != unique_years,
          exclude = intersection & score != max(score)) %>%
        filter(!exclude) %>% 
        ungroup()
      
      tmp_x <- tmp %>% filter(n_x > 1)
      x_keep <- tmp_x %>% 
        anti_join(., y_keep, by = c("person_id.x", "person_id.y")) %>% 
        unnest(election.x) %>% 
        group_by(panel_id) %>% 
        mutate(
          n = n(), 
          unique_years = length(unique(election.x)), 
          intersection = n != unique_years,
          exclude = intersection & score != max(score)) %>%
        filter(!exclude) %>% 
        ungroup()
      
      all_unique <- bind_rows(
        x_keep %>% 
          select(-c(election.x, election.y)) %>% 
          group_by(person_id.x, person_id.y) %>% 
          summarise(score = max(score)) %>% 
          ungroup() %>% 
          group_by(person_id.y) %>% 
          summarise(person_id.x = list(person_id.x), 
                    score = list(score), .groups = "drop") %>% 
          mutate(person_id.y = as.list(person_id.y)), 
        y_keep %>% 
          select(-c(election.x, election.y)) %>% 
          group_by(person_id.x, person_id.y) %>% 
          summarise(score = max(score)) %>% 
          ungroup() %>% 
          group_by(person_id.x) %>% 
          summarise(person_id.y = list(person_id.y), 
                    score = list(score), .groups = "drop") %>% 
          mutate(person_id.x = as.list(person_id.x)), 
        tmp %>% 
          ungroup %>% 
          filter(n_x == 1, n_y == 1) %>% 
          select(person_id.x, person_id.y, score) %>% 
          mutate(across(everything(), as.list))
      )
      
      missing_x <- mun_panel_harm %>% 
        filter(!person_id %in% unlist(all_unique$person_id.x)) %>% 
        select(person_id.x = person_id) %>% 
        unique() %>% 
        mutate(person_id.x = as.list(person_id.x))
      
      missing_y <- reg_panel_harm %>% 
        filter(!person_id %in% unlist(all_unique$person_id.y)) %>% 
        select(person_id.y = person_id) %>% 
        unique() %>% 
        mutate(person_id.y = as.list(person_id.y))
      
      pivot_table <- all_unique %>% 
        bind_rows(., missing_x) %>% 
        bind_rows(., missing_y) %>% 
        mutate(panel_id = paste0("RM", row_number())) %>% 
        unnest(c(person_id.x, person_id.y)) %>% 
        pivot_longer(., cols = c(person_id.x, person_id.y), names_to = "dataset", 
                     values_to = "person_id") %>% 
        select(-dataset) %>% 
        unique() %>% 
        filter(!is.na(person_id))
      
      all_candidates <- 
        bind_rows(
          mun_panel_harm, 
          reg_panel_harm
        )
      
      duplicates <- pivot_table %>% 
        count(person_id, sort = TRUE) %>% 
        filter(n > 1)
      
      pivot_table2 <- pivot_table
      for(i in duplicates$person_id){
        check_ids <- pivot_table %>% 
          group_by(panel_id) %>% 
          filter(any(person_id == i))
        (check_df <- all_candidates %>% 
            filter(person_id %in% check_ids$person_id) %>% 
            select(year, election, JMENO, PRIJMENI, POVOLANI, ROK_NAROZENI, ZKRATKAN8, 
                   ZKRATKAP8))
        
        ok <- (anyDuplicated(check_df$election) == 0)
        if(ok){
          pivot_table2 <- pivot_table2 %>% 
            group_by(panel_id) %>% 
            mutate(change = any(person_id == i)) %>% 
            ungroup %>% 
            group_by(change) %>% 
            mutate(panel_id = if_else(change, head(panel_id, 1), panel_id))
        }else{
          
          max_score <- pivot_table2 %>% 
            group_by(panel_id) %>% 
            filter(any(person_id == i)) %>% 
            pull(score) %>% unlist() %>% max()
          
          which_n <- which(duplicates$person_id == i)
          
          pivot_table2 <- pivot_table2 %>% 
            group_by(panel_id) %>% 
            # filter(any(person_id == i)) %>% 
            mutate(
              change = any(person_id == i), 
              has_max_score = purrr::map_lgl(score, function(x) max_score %in% x), 
              duplicated = person_id == i & !has_max_score
            ) %>% 
            filter(!duplicated) %>% 
            mutate(panel_id = case_when(
              change & has_max_score ~ panel_id, 
              change ~ paste0("NR", cur_group_id() + which_n),
              TRUE ~ panel_id
            ))
        }
      }
      
      pivot_table_final <- pivot_table2 %>% 
        ungroup %>% 
        select(panel_id, person_id) %>% 
        unique()
      
      # pivot_table_final      
      full_join(all_candidates, pivot_table_final, by = "person_id",
                relationship = "many-to-one") %>%
        select(panel_id, person_id, everything())
    }
  ),
  
  tar_target(
    mun_reg_check_nrow, {
      stopifnot(
        nrow(mun_reg_panel) == (nrow(mc_mun_panel) + nrow(reg_panel))
      )
    }
  ),
  
  ## Panel - PSP ------------------------------------------
  tar_target(
    mr_psp_panel, {
      mun_reg_panel_harm <- mun_reg_panel %>%
        select(-person_id) %>%
        rename(person_id = panel_id)

      psp_panel_harm <- psp_panel %>%
        mutate(election = paste("PSP", year))

      mr_psp_match <- match_mr_psp_panel(mun_reg_panel_harm, psp_panel_harm)

      # Unique matches between persons
      mr_psp_unique <- mr_psp_match %>%
        group_by(person_id.x, person_id.y) %>%
        summarise(score = max(score), .groups = "drop")
      
      # List of elections for each person
      mr_volby <- mun_reg_panel_harm %>%
        group_by(person_id) %>%
        summarise(election.x = list(election), .groups = "drop")
      psp_volby <- psp_panel_harm %>%
        group_by(person_id) %>%
        summarise(election.y = list(election), .groups = "drop")
      
      # Persons with multiple matches
      x_multiple <- mr_psp_unique %>%
        count(person_id.x, sort = TRUE) %>%
        filter(n > 1)
      
      y_multiple <- mr_psp_unique %>%
        count(person_id.y, sort = TRUE) %>%
        filter(n > 1)
      
      stopifnot(mr_psp_unique %>%
                  select(person_id.x, person_id.y) %>%
                  unique() %>%
                  filter(person_id.x %in% x_multiple &
                           person_id.y %in% y_multiple) %>%
                  nrow() == 0)
      
      # Single row for each
      x_unique <- mr_psp_unique %>%
        filter(person_id.x %in% x_multiple$person_id.x) %>%
        group_by(person_id.x) %>%
        summarise(person_id.y = list(person_id.y),
                  score = list(score)) %>%
        mutate(person_id.x = as.list(person_id.x))
      
      y_unique <- mr_psp_unique %>%
        filter(person_id.y %in% y_multiple$person_id.y) %>%
        group_by(person_id.y) %>%
        summarise(person_id.x = list(person_id.x),
                  score = list(score)) %>%
        mutate(person_id.y = as.list(person_id.y))
      
      both_unique <- mr_psp_unique %>%
        filter(!(person_id.y %in% y_multiple$person_id.y |
                   person_id.x %in% x_multiple$person_id.x)) %>%
        mutate(person_id.x = as.list(person_id.x),
               person_id.y = as.list(person_id.y),
               score = as.list(score))
      
      # Create TMP panel_id
      tmp <- bind_rows(
        x_unique,
        y_unique,
        both_unique
      ) %>%
        mutate(panel_id = paste0("PRM", row_number())) %>% 
        unnest(c(person_id.x, person_id.y, score)) %>%
        left_join(., mr_volby, by = c("person_id.x"="person_id")) %>%
        left_join(., psp_volby, by = c("person_id.y"="person_id")) %>%
        group_by(panel_id, person_id.x) %>%
        mutate(n_y = length(unique(person_id.y))) %>%
        group_by(panel_id, person_id.y) %>%
        mutate(n_x = length(unique(person_id.x))) %>% 
        ungroup()
      
      # multiple matches
      # exclude duplicates based on duplicated elections
      tmp_y <- tmp %>% filter(n_y > 1)
      y_keep <- tmp_y %>%
        unnest(election.y) %>%
        group_by(panel_id) %>%
        mutate(
          n = n(),
          unique_years = length(unique(election.y)),
          intersection = n != unique_years,
          exclude = intersection & score != max(score)) %>%
        filter(!exclude) %>%
        ungroup()
      
      tmp_x <- tmp %>% filter(n_x > 1)
      x_keep <- tmp_x %>%
        anti_join(., y_keep, by = c("person_id.x", "person_id.y")) %>%
        unnest(election.x) %>%
        group_by(panel_id) %>%
        mutate(
          n = n(),
          unique_years = length(unique(election.x)),
          intersection = n != unique_years,
          exclude = intersection & score != max(score)) %>%
        filter(!exclude) %>%
        ungroup()
      
      all_unique <- bind_rows(
        x_keep %>%
          select(-c(election.x, election.y)) %>%
          group_by(person_id.x, person_id.y) %>%
          summarise(score = max(score)) %>%
          ungroup() %>%
          group_by(person_id.y) %>%
          summarise(person_id.x = list(person_id.x),
                    score = list(score), .groups = "drop") %>%
          mutate(person_id.y = as.list(person_id.y)),
        y_keep %>%
          select(-c(election.x, election.y)) %>%
          group_by(person_id.x, person_id.y) %>%
          summarise(score = max(score)) %>%
          ungroup() %>%
          group_by(person_id.x) %>%
          summarise(person_id.y = list(person_id.y),
                    score = list(score), .groups = "drop") %>%
          mutate(person_id.x = as.list(person_id.x)),
        tmp %>%
          ungroup %>%
          filter(n_x == 1, n_y == 1) %>%
          select(person_id.x, person_id.y, score) %>%
          mutate(across(everything(), as.list))
      )
      
      # unmatched persons
      missing_x <- mun_reg_panel_harm %>%
        filter(!person_id %in% unlist(all_unique$person_id.x)) %>%
        select(person_id.x = person_id) %>%
        unique() %>%
        mutate(person_id.x = as.list(person_id.x))
      
      missing_y <- psp_panel_harm %>%
        filter(!person_id %in% unlist(all_unique$person_id.y)) %>%
        select(person_id.y = person_id) %>%
        unique() %>%
        mutate(person_id.y = as.list(person_id.y))
      
      # pivot table
      pivot_table <- all_unique %>%
        bind_rows(., missing_x) %>%
        bind_rows(., missing_y) %>%
        mutate(panel_id = paste0("PRM", row_number())) %>%
        unnest(c(person_id.x, person_id.y)) %>%
        pivot_longer(., cols = c(person_id.x, person_id.y), names_to = "dataset",
                     values_to = "person_id") %>%
        select(-dataset) %>%
        unique() %>%
        filter(!is.na(person_id)) %>% 
        select(panel_id, person_id) %>%
        unique()
      
      all_candidates <-
        bind_rows(
          mun_reg_panel_harm,
          psp_panel_harm
        )
      
      # pivot_table_final
      full_join(all_candidates, pivot_table, by = "person_id",
                relationship = "many-to-one") %>%
        select(panel_id, person_id, everything())

    }
  ),

  tar_target(
    mr_psp_check_nrow, {
      stopifnot(
        nrow(mr_psp_panel) == (nrow(mc_mun_panel) + nrow(reg_panel) +
                                 nrow(psp_panel))
      )
    }
  ),
  # TODO: checks
  
  ## Panel - Senát ----------------------------------------
  NULL
  
  # Panel - EP
)

validations <- list(
  tar_target(kouba_lysek_2023_tab1, {
    mc_mun_panel %>% 
      group_by(KODZASTUP, year) %>% 
      summarise(n_stran = max(POR_STR_HL)) %>% 
      group_by(year) %>% 
      summarise(pct = mean(n_stran == 1) * 100)
  })
)

list(
  psp_data, 
  reg_data, 
  mun_data, 
  ep_data, 
  senate_data,
  all_data, 
  matched_panels, 
  summary_stats, 
  validations
)

