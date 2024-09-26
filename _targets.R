# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.

tar_option_set(packages = c("dplyr", "rvest", "here", 
                            "readxl", "readr", "reclin2", 
                            "tidyr", "names.cze", "lubridate"))

tar_source()

# Limitations: 
# 90s were wild - there are inconsistencies how names are written
# mainly in municipal elections in the 90s where it depends on local bureaucracy
# and their ability to write on a computer
# some pre-processing aiming at standardisation was done 
# such as removing Jr., Sr. from last names 
# (and other words and symbols that are not a name, mainly academic titles in the 90s))
# TODO: replace ´ in last name with ' (or ľ)

# Possible improvements
# TODO: what would entail matching municipalities and city districts together?
# TODO: what if election-specific panel contains mismatch which could be resolved
# during deduplication (by clustering the candidates)
# TODO: by-elections in municipalities
# TODO: occupation word2vec distance (?)
# TODO: compare regions better (Východočeský matches with Královehradecký and Pardubický)
# TODO: DiS. - https://cs.wikipedia.org/wiki/Diplomovan%C3%BD_specialista

# Validace
# TODO: Kouba & Lysek
# TODO: Party people - připravit seznam kandidátů - noví, starí
# TODO: validace? = https://programydovoleb.cz/lide/profil/731#data

all_data <- list(
  tar_target(all_candidates, {
    bind_rows(
      m_1994 %>% select(candidate_name, candidate_surname),
      m_1998 %>% select(candidate_name, candidate_surname),
      m_2002 %>% select(candidate_name, candidate_surname),
      m_2006 %>% select(candidate_name, candidate_surname),
      m_2010 %>% select(candidate_name, candidate_surname),
      m_2014 %>% select(candidate_name, candidate_surname),
      m_2018 %>% select(candidate_name, candidate_surname),
      m_2022 %>% select(candidate_name, candidate_surname),
      mc_1994 %>% select(candidate_name, candidate_surname),
      mc_1998 %>% select(candidate_name, candidate_surname),
      mc_2002 %>% select(candidate_name, candidate_surname),
      mc_2006 %>% select(candidate_name, candidate_surname),
      mc_2010 %>% select(candidate_name, candidate_surname),
      mc_2014 %>% select(candidate_name, candidate_surname),
      mc_2018 %>% select(candidate_name, candidate_surname),
      mc_2022 %>% select(candidate_name, candidate_surname),
      reg_2000 %>% select(candidate_name, candidate_surname),
      reg_2004 %>% select(candidate_name, candidate_surname),
      reg_2008 %>% select(candidate_name, candidate_surname),
      reg_2012 %>% select(candidate_name, candidate_surname),
      reg_2016 %>% select(candidate_name, candidate_surname),
      reg_2020 %>% select(candidate_name, candidate_surname),
      psp_1996 %>% select(candidate_name, candidate_surname),
      psp_1998 %>% select(candidate_name, candidate_surname),
      psp_2002 %>% select(candidate_name, candidate_surname),
      psp_2006 %>% select(candidate_name, candidate_surname),
      psp_2010 %>% select(candidate_name, candidate_surname),
      psp_2013 %>% select(candidate_name, candidate_surname),
      psp_2017 %>% select(candidate_name, candidate_surname),
      psp_2021 %>% select(candidate_name, candidate_surname),
      senate_df %>% select(candidate_name, candidate_surname),
      ep_2004 %>% select(candidate_name, candidate_surname),
      ep_2009 %>% select(candidate_name, candidate_surname),
      ep_2014 %>% select(candidate_name, candidate_surname),
      ep_2019 %>% select(candidate_name, candidate_surname),
      ep_2024 %>% select(candidate_name, candidate_surname)
    ) %>% unique()
  }), 
  
  tar_target(
    multiple_last_names, {
      all_candidates %>% 
        filter(grepl("\\s|-", candidate_surname))
    }
  ),
  
  tar_target(
    unique_first_names, {
      # all_candidates %>% 
      # select(JMENO) %>% 
      # unique() %>% 
      # saveRDS(., "output/unique_names.rds")
      readRDS("output/unique_names.rds") %>% 
        categorize_by_first_name()
    }
  ),
  
  # FIXME: co dělat se složenými jmény "Rosa de Pauli"/"Tozzi di Angelo"
  tar_target(
    multiple_last_names_eligible, {
      split_names <- multiple_last_names %>% 
        filter(!grepl("^(Al|El|Van|Da|De|Del|Di|in|le|Abu|Abú)\\b", candidate_surname, ignore.case = TRUE)) %>% 
        pull(candidate_surname) %>% 
        strsplit(., "\\s|-") %>% 
        unlist() %>% 
        gsub("\\(|\\)", "", .) %>% 
        stringr::str_trim() %>% 
        unique()
      
      full_names <- multiple_last_names %>% 
        filter(!grepl("^(Al|El|Van|Da|De|Del|Di|in|le|Abu|Abú)\\b", candidate_surname, ignore.case = TRUE)) %>% 
        pull(candidate_surname)
      
      c(split_names, full_names)
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
  
  tar_target(ps_1996_results, {
    read_html("https://volby.cz/pls/ps1996/u5") %>% 
      html_node("table") %>% 
      html_table() %>% 
      janitor::clean_names() %>% 
      select(candidate_partyrun_fullname = politickestrany_a_hnuti_2, 
             type = politickestrany_a_hnuti_3, 
             kraj_praha, 
             kraj_stredocesky = stredo_ceskykraj, 
             kraj_jihocesky = jiho_ceskykraj, 
             kraj_zapadocesky = zapado_ceskykraj,
             kraj_severocesky = severo_ceskykraj, 
             kraj_vychodocesky = vychodo_ceskykraj, 
             kraj_jihomoravsky = jiho_moravskykraj,
             kraj_severomoravsky = severo_moravskykraj) %>% 
      filter(!candidate_partyrun_fullname %in% c(
        "Platné hlasy celkem", "Celkem okrsků", "Zpracováno okrsků",
        "Zpracované okrsky v %"
      )) %>% 
      tidyr::pivot_longer(., cols = 3:ncol(.), names_to = "kraj", values_to = "votes") %>% 
      tidyr::pivot_wider(., id_cols = c(candidate_partyrun_fullname, kraj), 
                         names_from = "type", values_from = "votes") %>% 
      rename(party_voteN = hlasy, 
             party_voteP = `%`) %>% 
      mutate(party_voteN = as.numeric(purrr::map_chr(party_voteN, remove_whitespace)), 
             party_voteP = as.numeric(party_voteP)) %>% 
      filter(!is.na(party_voteN)) %>% 
      mutate(electoral_region = case_when(
        kraj == "kraj_praha" ~ 3100, 
        kraj == "kraj_stredocesky" ~ 3200, 
        kraj == "kraj_jihocesky" ~ 3300, 
        kraj == "kraj_zapadocesky" ~ 3400,
        kraj == "kraj_severocesky" ~ 3500, 
        kraj == "kraj_vychodocesky" ~ 3600, 
        kraj == "kraj_jihomoravsky" ~ 3700, 
        kraj == "kraj_severomoravsky" ~ 3800, 
      )) %>% select(-kraj)
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
      categorize_sex(., unique_first_names) %>% 
      mutate(
        TITULY = case_when(
          PRIJMENI == "PaličkaCSc" ~ "Ing. CSc.",
          PRIJMENI == "Havel. CSc." ~ "Ing. CSc.",
          TRUE ~ TITULY
        ), 
        PRIJMENI = case_when(
          PRIJMENI == "PaličkaCSc" ~ "Palička",
          PRIJMENI == "Havel. CSc." ~ "Havel",
          TRUE ~ PRIJMENI
        ), 
        MANDAT = as.numeric(ZVOLEN1S != 0 | ZVOLEN2S != 0)
      ) %>% 
      select(-c(ZVOLEN1S, ZVOLEN2S)) %>% 
      filter(PRIJMENI != "vytištěného HL") %>% 
      filter(JMENO != "Náprava chybně") %>% 
      rename_variables() %>% 
      left_join(., ps_1996_results, by = c("electoral_region", "candidate_partyrun_fullname")) %>% 
      mutate(candidate_voteN = if_else(candidate_validity == 1, 0, candidate_voteN)) %>% 
      mutate(
        party_voteN = if_else(is.na(party_voteN), 0, party_voteN), 
        party_voteP = if_else(is.na(party_voteP), 0, party_voteP), 
        candidate_validity = if_else(party_voteP == 0, 1, candidate_validity),
        election_type = factor("Chamber of Deputies", 
                               levels = c("Municipal", "City district",
                                          "Regional", "Chamber of Deputies",
                                          "Senate", "European Parliament")),
        election_date = as.Date("1996-05-31", format = "%Y-%m-%d")
      )
  }),
  
  tar_target(ps_1998_results, command = {
    read_html("https://volby.cz/pls/ps1998/u5") %>% 
      html_node("table") %>% 
      html_table() %>% 
      janitor::clean_names() %>% 
      select(candidate_partyrun_fullname = politickestrany_a_hnuti_2, 
             type = politickestrany_a_hnuti_3, 
             kraj_praha, 
             kraj_stredocesky = stredo_ceskykraj, 
             kraj_jihocesky = jiho_ceskykraj, 
             kraj_zapadocesky = zapado_ceskykraj,
             kraj_severocesky = severo_ceskykraj, 
             kraj_vychodocesky = vychodo_ceskykraj, 
             kraj_jihomoravsky = jiho_moravskykraj,
             kraj_severomoravsky = severo_moravskykraj) %>% 
      filter(!candidate_partyrun_fullname %in% c(
        "Platné hlasy celkem", "Celkem okrsků", "Zpracováno okrsků",
        "Zpracované okrsky v %"
      )) %>% 
      tidyr::pivot_longer(., cols = 3:ncol(.), names_to = "kraj", values_to = "votes") %>% 
      tidyr::pivot_wider(., id_cols = c(candidate_partyrun_fullname, kraj), 
                         names_from = "type", values_from = "votes") %>% 
      rename(party_voteN = hlasy, 
             party_voteP = `%`) %>% 
      mutate(party_voteN = as.numeric(purrr::map_chr(party_voteN, remove_whitespace)), 
             party_voteP = as.numeric(party_voteP)) %>% 
      filter(!is.na(party_voteN)) %>% 
      mutate(electoral_region = case_when(
        kraj == "kraj_praha" ~ 3100, 
        kraj == "kraj_stredocesky" ~ 3200, 
        kraj == "kraj_jihocesky" ~ 3300, 
        kraj == "kraj_zapadocesky" ~ 3400,
        kraj == "kraj_severocesky" ~ 3500, 
        kraj == "kraj_vychodocesky" ~ 3600, 
        kraj == "kraj_jihomoravsky" ~ 3700, 
        kraj == "kraj_severomoravsky" ~ 3800, 
      )) %>% select(-kraj) 
  }),
  
  tar_target(psp_1998, command = {
    cpp <- read_cpp(here("data", "PS1998", "CPPPS98.xlsx"), function(x) {x %>% rename(ZKRATKAP8 = ZKRATKAP)})
    read_excel(here("data", "PS1998", "PS98-RK.xlsx")) %>%
      clean_ps(., VSTRANA_MAP_98) %>%
      left_join(., cpp, by = "PSTRANA") %>%
      mutate(row_id = row_number(), 
             ROK_NAROZENI = 1998 - VEK) %>% 
      rename(VOLKRAJ = KRAJ) %>% 
      left_join(., kraje_do_2000, by = "VOLKRAJ") %>% 
      categorize_sex(., unique_first_names) %>% 
      mutate(
        TITULZA = case_when(
          PRIJMENI == "Kaňák CSc." ~ "CSc.", 
          PRIJMENI == "Šindelářová CSc" ~ "CSc.", 
          TRUE ~ TITULZA
        ), 
        PRIJMENI = case_when(
          PRIJMENI == "Kaňák CSc." ~ "Kaňák", 
          PRIJMENI == "Šindelářová CSc" ~ "Šindelářová",
          TRUE ~ PRIJMENI
        ), 
        MANDAT = as.numeric(ZVOLEN1S != 0 | ZVOLEN2S != 0)
      ) %>% 
      select(-c(ZVOLEN1S, ZVOLEN2S)) %>% 
      merge_and_recode_titles %>% 
      filter(PRIJMENI != "HLASOVACÍHO LÍSTKU") %>% 
      rename_variables() %>% 
      left_join(., ps_1998_results, by = c("electoral_region", "candidate_partyrun_fullname")) %>% 
      mutate(candidate_voteN = if_else(candidate_validity == 1, 0, candidate_voteN)) %>% 
      mutate(
        party_voteN = if_else(is.na(party_voteN), 0, party_voteN), 
        party_voteP = if_else(is.na(party_voteP), 0, party_voteP), 
        candidate_validity = if_else(party_voteP == 0, 1, candidate_validity),
        election_type = factor("Chamber of Deputies", 
                               levels = c("Municipal", "City district",
                                          "Regional", "Chamber of Deputies",
                                          "Senate", "European Parliament")),
        election_date = as.Date("1998-06-19", format = "%Y-%m-%d")
      )
  }),
  
  tar_target(psp_2002, command = {
    kraje <- read_excel(here("data", "PS2002", "PS2002_cisel_20230224_xlsx", 
                             "psvolkr.xlsx")) %>% 
      select(KRAJ, VOLKRAJ)
    ps_vysledky <- read_excel(here("data", "PS2002", "PS2002_data_20230224_xlsx", 
                    "pst4p.xlsx")) %>% 
      # votes from abroad were added to Olomoucký kraj
      mutate(KRAJ = if_else(KRAJ == 9900, 6200, KRAJ)) %>% 
      group_by(KRAJ, KSTRANA) %>% 
      summarise(party_voteN = sum(POC_HLASU)) %>% 
      group_by(KRAJ) %>% 
      mutate(party_voteP = round(party_voteN / sum(party_voteN) * 100, 2)) %>% 
      ungroup() %>% 
      left_join(., kraje, by = "KRAJ") %>% 
      select(-KRAJ) %>% 
      rename_variables()
    
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
      categorize_sex(., unique_first_names) %>% 
      select(-c(POCPROC)) %>% 
      rename_variables() %>% 
      left_join(., ps_vysledky, by = c("candidate_partyrun_code", "electoral_region")) %>% 
      mutate(election_type = factor("Chamber of Deputies", 
                                    levels = c("Municipal", "City district",
                                               "Regional", "Chamber of Deputies",
                                               "Senate", "European Parliament")), 
             election_date = as.Date("2002-06-14", format = "%Y-%m-%d"))
  }),
  
  tar_target(psp_2006, command = {
    ps_vysledky <- read_html(here("data", "PS2006", "vysledky.xml")) %>% 
      parse_ps_results()
    psp_parties <- read_parties(here("data", "PS2006", "PS2006reg2006", "PSRKL.xlsx"), 
                                clean_dbf_excel_colnames)
    cpp <- read_cpp(here("data", "PS2006", "PS2006ciselniky2006", "CPP.xlsx"), 
                    clean_dbf_excel_colnames)
    cns <- read_cns(here("data", "PS2006", "PS2006ciselniky2006", "CNS.xlsx"), 
                    clean_dbf_excel_colnames)
    read_candidates(here("data", "PS2006", "PS2006reg2006", "PSRK.xlsx"), 
                    psp_parties, cpp, cns, clean_dbf_excel_colnames) %>% 
      left_join(., kraje_po_2000, by = "VOLKRAJ") %>% 
      categorize_sex(., unique_first_names) %>% 
      select(-c(POCPROC)) %>% 
      rename_variables() %>% 
      left_join(., ps_vysledky, by = c("electoral_region", "candidate_partyrun_code"), 
                relationship = "many-to-one") %>% 
      mutate(election_type = factor("Chamber of Deputies", 
                                    levels = c("Municipal", "City district",
                                               "Regional", "Chamber of Deputies",
                                               "Senate", "European Parliament")), 
             election_date = as.Date("2006-06-02", format = "%Y-%m-%d"))
  }),
  
  tar_target(psp_2010, command = {
    ps_vysledky <- read_html(here("data", "PS2010", "vysledky.xml")) %>% 
      parse_ps_results()
    psp_parties <- read_parties(here("data", "PS2010", "PS2010reg2010", "PSRKL.xlsx"), 
                                clean_dbf_excel_colnames)
    cpp <- read_cpp(here("data", "PS2010", "PS2010ciselniky2010", "CPP.xlsx"), 
                    clean_dbf_excel_colnames)
    cns <- read_cns(here("data", "PS2010", "PS2010ciselniky2010", "CNS.xlsx"), 
                    clean_dbf_excel_colnames)
    read_candidates(here("data", "PS2010", "PS2010reg2010", "PSRK.xlsx"), 
                    psp_parties, cpp, cns, clean_dbf_excel_colnames) %>% 
      left_join(., kraje_po_2000, by = "VOLKRAJ") %>% 
      categorize_sex(., unique_first_names) %>% 
      filter(JMENO != "Registrační úřad ponechal pozici volnou") %>% 
      select(-c(POCPROC)) %>% 
      rename_variables() %>% 
      left_join(., ps_vysledky, by = c("electoral_region", "candidate_partyrun_code"), 
                relationship = "many-to-one") %>% 
      mutate(election_type = factor("Chamber of Deputies", 
                                    levels = c("Municipal", "City district",
                                               "Regional", "Chamber of Deputies",
                                               "Senate", "European Parliament")), 
             election_date = as.Date("2010-05-28", format = "%Y-%m-%d"))
  }),
  
  tar_target(psp_2013, command = {
    ps_vysledky <- read_html(here("data", "PS2013", "vysledky.xml")) %>% 
      parse_ps_results()
    psp_parties <- read_parties(here("data", "PS2013", "PS2013reg20131026", "PSRKL.xlsx"), 
                                clean_dbf_excel_colnames)
    cpp <- read_cpp(here("data", "PS2013", "PS2013ciselniky20131021", "CPP.xlsx"), 
                    clean_dbf_excel_colnames)
    cns <- read_cns(here("data", "PS2013", "PS2013ciselniky20131021", "CNS.xlsx"), 
                    clean_dbf_excel_colnames)
    read_candidates(here("data", "PS2013", "PS2013reg20131026", "PSRK.xlsx"), 
                    psp_parties, cpp, cns, clean_dbf_excel_colnames) %>% 
      left_join(., kraje_po_2000, by = "VOLKRAJ") %>% 
      categorize_sex(., unique_first_names) %>% 
      filter(JMENO != "Registrační úřad ponechal pozici volnou") %>% 
      select(-c(POCPROC)) %>% 
      rename_variables() %>% 
      left_join(., ps_vysledky, by = c("electoral_region", "candidate_partyrun_code"), 
                relationship = "many-to-one") %>% 
      mutate(election_type = factor("Chamber of Deputies", 
                                    levels = c("Municipal", "City district",
                                               "Regional", "Chamber of Deputies",
                                               "Senate", "European Parliament")), 
             election_date = as.Date("2013-10-25", format = "%Y-%m-%d"))
  }),
  
  tar_target(psp_2017, command = {
    ps_vysledky <- read_html(here("data", "PS2017NSS", "vysledky.xml")) %>% 
      parse_ps_results()
    
    psp_parties <- read_parties(here("data", "PS2017NSS", "PS2017reg20171122_xlsx", "psrkl.xlsx"))
    cpp <- read_cpp(here("data", "PS2017NSS", "PS2017ciselniky20170905", "cpp.xlsx"))
    cns <- read_cns(here("data", "PS2017NSS", "PS2017ciselniky20170905", "cns.xlsx"))
    read_candidates(here("data", "PS2017NSS", "PS2017reg20171122_xlsx", "psrk.xlsx"), 
                    psp_parties, cpp, cns) %>%
      mutate(PLATNOST = ifelse(PLATNOST == "A", 0, 1), 
             MANDAT = ifelse(MANDAT == "A", 1, 0)) %>% 
      left_join(., kraje_po_2000, by = "VOLKRAJ") %>% 
      categorize_sex(., unique_first_names) %>% 
      remove_order_from_last_name() %>% 
      filter(JMENO != "Registrační úřad ponechal pozici volnou") %>% 
      select(-c(POCPROC, POHLAVI)) %>% 
      rename_variables() %>% 
      left_join(., ps_vysledky, by = c("electoral_region", "candidate_partyrun_code"), 
                relationship = "many-to-one") %>% 
      mutate(election_type = factor("Chamber of Deputies", 
                                    levels = c("Municipal", "City district",
                                               "Regional", "Chamber of Deputies",
                                               "Senate", "European Parliament")), 
             election_date = as.Date("2017-10-20", format = "%Y-%m-%d"))
  }),
  
  tar_target(psp_2021, command = {
    ps_vysledky <- read_html(here("data", "PS2021", "vysledky.xml")) %>% 
      parse_ps_results()
    
    psp_parties <- read_parties(here("data", "PS2021", "PS2021reg20211010_xlsx", "psrkl.xlsx"))
    cpp <- read_cpp(here("data", "PS2021", "PS2021ciselniky20210824", "cpp.xlsx"))
    cns <- read_cns(here("data", "PS2021", "PS2021ciselniky20210824", "cns.xlsx"))
    read_candidates(here("data", "PS2021", "PS2021reg20211010_xlsx", "psrk.xlsx"), 
                    psp_parties, cpp, cns) %>%
      mutate(PLATNOST = ifelse(PLATNOST == "A", 0, 1), 
             MANDAT = ifelse(MANDAT == "A", 1, 0)) %>% 
      left_join(., kraje_po_2000, by = "VOLKRAJ") %>% 
      categorize_sex(., unique_first_names) %>% 
      filter(JMENO != "Registrační úřad ponechal pozici volnou") %>% 
      select(-c(POHLAVI, SKRUTINIUM)) %>% 
      rename_variables() %>% 
      left_join(., ps_vysledky, by = c("electoral_region", "candidate_partyrun_code"), 
                relationship = "many-to-one") %>% 
      mutate(election_type = factor("Chamber of Deputies", 
                                    levels = c("Municipal", "City district",
                                               "Regional", "Chamber of Deputies",
                                               "Senate", "European Parliament")), 
             election_date = as.Date("2021-10-08", format = "%Y-%m-%d"))
  }), 
  
  tar_target(psp_panel, {
    psp_96_98 <- match_psp_data(psp_1996, psp_1998, 
                                multiple_last_names_eligible)
    
    psp_98_02 <- match_psp_data(psp_1998, psp_2002, 
                                multiple_last_names_eligible)
    
    psp_96_02 <- match_psp_data(
      psp_1996 %>% 
        filter(!row_id %in% psp_96_98$row_id.x),
      psp_2002 %>% 
        filter(!row_id %in% psp_98_02$row_id.y), 
      multiple_last_names_eligible
    )
    
    psp_02_06 <- match_psp_data(psp_2002, psp_2006, 
                                multiple_last_names_eligible)
    
    psp_98_06 <- match_psp_data(
      psp_1998 %>% 
        filter(!row_id %in% psp_98_02$row_id.x),
      psp_2006 %>% 
        filter(!row_id %in% psp_02_06$row_id.y), 
      multiple_last_names_eligible
    )
    
    psp_96_06 <- match_psp_data(
      psp_1996 %>% 
        filter(!row_id %in% psp_96_98$row_id.x) %>% 
        filter(!row_id %in% psp_96_02$row_id.x),
      psp_2006 %>% 
        filter(!row_id %in% psp_02_06$row_id.y) %>% 
        filter(!row_id %in% psp_98_06$row_id.y), 
      multiple_last_names_eligible
    )
    
    psp_06_10 <- match_psp_data(psp_2006, psp_2010, 
                                multiple_last_names_eligible)
    
    psp_02_10 <- match_psp_data(
      psp_2002 %>% 
        filter(!row_id %in% psp_02_06$row_id.x),
      psp_2010 %>% 
        filter(!row_id %in% psp_06_10$row_id.y), 
      multiple_last_names_eligible
    )
    
    psp_98_10 <- match_psp_data(
      psp_1998 %>% 
        filter(!row_id %in% psp_98_02$row_id.x) %>% 
        filter(!row_id %in% psp_98_06$row_id.x),
      psp_2010 %>% 
        filter(!row_id %in% psp_06_10$row_id.y) %>% 
        filter(!row_id %in% psp_02_10$row_id.y), 
      multiple_last_names_eligible
    )
    
    psp_96_10 <- match_psp_data(
      psp_1996 %>% 
        filter(!row_id %in% psp_96_98$row_id.x) %>% 
        filter(!row_id %in% psp_96_02$row_id.x) %>% 
        filter(!row_id %in% psp_96_06$row_id.x),
      psp_2010 %>% 
        filter(!row_id %in% psp_06_10$row_id.y) %>% 
        filter(!row_id %in% psp_02_10$row_id.y) %>% 
        filter(!row_id %in% psp_98_10$row_id.y), 
      multiple_last_names_eligible
    )
    
    psp_10_13 <- match_psp_data(psp_2010, psp_2013, 
                                multiple_last_names_eligible)
    
    psp_06_13 <- match_psp_data(
      psp_2006 %>% 
        filter(!row_id %in% psp_06_10$row_id.x), 
      psp_2013 %>% 
        filter(!row_id %in% psp_10_13$row_id.y), 
      multiple_last_names_eligible
    )
    
    psp_02_13 <- match_psp_data(
      psp_2002 %>% 
        filter(!row_id %in% psp_02_06$row_id.x) %>% 
        filter(!row_id %in% psp_02_10$row_id.x), 
      psp_2013 %>% 
        filter(!row_id %in% psp_10_13$row_id.y) %>% 
        filter(!row_id %in% psp_06_13$row_id.y), 
      multiple_last_names_eligible
    )
    
    psp_98_13 <- match_psp_data(
      psp_1998 %>% 
        filter(!row_id %in% psp_98_02$row_id.x) %>% 
        filter(!row_id %in% psp_98_06$row_id.x) %>% 
        filter(!row_id %in% psp_98_10$row_id.x), 
      psp_2013 %>% 
        filter(!row_id %in% psp_10_13$row_id.y) %>% 
        filter(!row_id %in% psp_06_13$row_id.y) %>% 
        filter(!row_id %in% psp_02_13$row_id.y), 
      multiple_last_names_eligible
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
        filter(!row_id %in% psp_98_13$row_id.y), 
      multiple_last_names_eligible
    )
    
    psp_13_17 <- match_psp_data(psp_2013, psp_2017,
                                multiple_last_names_eligible)
    
    psp_10_17 <- match_psp_data(
      psp_2010 %>% 
        filter(!row_id %in% psp_10_13$row_id.x), 
      psp_2017 %>% 
        filter(!row_id %in% psp_13_17$row_id.y), 
      multiple_last_names_eligible
    )
    
    psp_06_17 <- match_psp_data(
      psp_2006 %>% 
        filter(!row_id %in% psp_06_10$row_id.x) %>% 
        filter(!row_id %in% psp_06_13$row_id.x), 
      psp_2017 %>% 
        filter(!row_id %in% psp_13_17$row_id.y) %>% 
        filter(!row_id %in% psp_10_17$row_id.y), 
      multiple_last_names_eligible
    )
    
    psp_02_17 <- match_psp_data(
      psp_2002 %>% 
        filter(!row_id %in% psp_02_06$row_id.x) %>% 
        filter(!row_id %in% psp_02_10$row_id.x) %>% 
        filter(!row_id %in% psp_02_13$row_id.x), 
      psp_2017 %>% 
        filter(!row_id %in% psp_13_17$row_id.y) %>% 
        filter(!row_id %in% psp_10_17$row_id.y) %>% 
        filter(!row_id %in% psp_06_17$row_id.y), 
      multiple_last_names_eligible
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
        filter(!row_id %in% psp_02_17$row_id.y), 
      multiple_last_names_eligible
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
        filter(!row_id %in% psp_98_17$row_id.y), 
      multiple_last_names_eligible
    )
    
    psp_17_21 <- match_psp_data(psp_2017, psp_2021, 
                                multiple_last_names_eligible)
    
    psp_13_21 <- match_psp_data(
      psp_2013 %>% 
        filter(!row_id %in% psp_13_17$row_id.x), 
      psp_2021 %>% 
        filter(!row_id %in% psp_17_21$row_id.y), 
      multiple_last_names_eligible
    )
    
    psp_10_21 <- match_psp_data(
      psp_2010 %>% 
        filter(!row_id %in% psp_10_17$row_id.x) %>% 
        filter(!row_id %in% psp_10_13$row_id.x),
      psp_2021 %>% 
        filter(!row_id %in% psp_17_21$row_id.y) %>% 
        filter(!row_id %in% psp_13_21$row_id.y),
      multiple_last_names_eligible
    )
    
    psp_06_21 <- match_psp_data(
      psp_2006 %>% 
        filter(!row_id %in% psp_06_10$row_id.x) %>% 
        filter(!row_id %in% psp_06_13$row_id.x) %>% 
        filter(!row_id %in% psp_06_17$row_id.x),
      psp_2021 %>% 
        filter(!row_id %in% psp_17_21$row_id.y) %>% 
        filter(!row_id %in% psp_13_21$row_id.y) %>% 
        filter(!row_id %in% psp_10_21$row_id.y),
      multiple_last_names_eligible
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
        filter(!row_id %in% psp_06_21$row_id.y), 
      multiple_last_names_eligible
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
        filter(!row_id %in% psp_02_21$row_id.y), 
      multiple_last_names_eligible
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
        filter(!row_id %in% psp_98_21$row_id.y), 
      multiple_last_names_eligible
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
      tidyr::pivot_longer(., cols = 1:(ncol(.)-1), names_to = "election_year",
                          values_to = "row_id") %>%
      filter(!is.na(row_id)) %>%
      mutate(election_year = stringr::str_extract(election_year, "[0-9]{4}"))
    
    psp_candidates <- bind_rows(
      psp_1996 %>% mutate(election_year = "1996"),
      psp_1998 %>% mutate(election_year = "1998"),
      psp_2002 %>% mutate(election_year = "2002"),
      psp_2006 %>% mutate(election_year = "2006"),
      psp_2010 %>% mutate(election_year = "2010"),
      psp_2013 %>% mutate(election_year = "2013"),
      psp_2017 %>% mutate(election_year = "2017"),
      psp_2021 %>% mutate(election_year = "2021")
    )
    
    full_join(pivot_table_long, psp_candidates, by = c("row_id", "election_year"))
  }), 
  
  tar_target(
    psp_panel_check, 
    stopifnot(nrow(psp_panel) == (
      nrow(psp_1996) + nrow(psp_1998) + nrow(psp_2002) + nrow(psp_2006) + 
        nrow(psp_2010) + nrow(psp_2013) + nrow(psp_2017) + nrow(psp_2021) 
    ))
  ), 
  
  tar_target(
    psp_panel_rds, 
    saveRDS(psp_panel, "output/election_specific/ps_candidates_panel.rds")
  ),
  
  tar_target(
    psp_panel_csv, 
    write.csv(psp_panel, "output/election_specific/ps_candidates_panel.csv", 
              row.names = FALSE)
  ),
  
  NULL
)

# Regional elections --------------------------------------
reg_data <- list(
  tar_target(reg_2000, command = {
    krzast <- read_excel(here("data", "KZ2000", 
                              "KZ2000_cisel_20230223_xlsx", 
                              "kzciskr.xlsx")) %>% 
      select(KRZAST, KRAJ)
    reg_vysledky <- read_excel(here("data", "KZ2000", 
                                    "KZ2000_data_20230223_xlsx", 
                                    "kzt6p.xlsx")) %>% 
      group_by(KSTRANA, KRAJ) %>% 
      summarise(party_voteN = sum(POC_HLASU)) %>% 
      ungroup() %>% 
      group_by(KRAJ) %>% 
      mutate(party_voteP = round(party_voteN / sum(party_voteN) * 100, 2)) %>% 
      ungroup %>% 
      full_join(., krzast, by = "KRAJ") %>% 
      select(-KRAJ) %>% 
      rename_variables()
    
    parties <- read_excel(here("data", "KZ2000", "KZ2000_reg_20230223_xlsx", "kzrkl.xlsx")) %>% 
      select(KRZAST, KSTRANA, NAZEV_STRK)
    
    cpp <- read_cpp(here("data", "KZ2000", "CPPKZ00.xlsx"))
    cns <- cpp %>% rename(NSTRANA = PSTRANA, ZKRATKAN8 = ZKRATKAP8)
    list_path <- here("data", "KZ2000", "KZ2000_reg_20230223_xlsx", "kzrk.xlsx")
    year <- as.numeric(stringr::str_extract(list_path, "[0-9]{4}"))
    read_excel(list_path) %>%
      left_join(., cpp, by = "PSTRANA") %>%
      left_join(., cns, by = "NSTRANA") %>%
      left_join(., parties, by = c("KRZAST", "KSTRANA")) %>% 
      mutate(
        MANDAT = as.numeric(MANDAT == "A"),
        PLATNOST = as.numeric(PLATNOST != "A"),
        row_id = row_number(), 
        ROK_NAROZENI = year - VEK) %>%
      merge_and_recode_titles %>%
      categorize_sex(., unique_first_names) %>% 
      remove_order_from_last_name() %>% 
      select(-POCPROC) %>% rename_variables() %>% 
      left_join(., reg_vysledky, by = c("region_code", "candidate_partyrun_code")) %>% 
      mutate(election_type = factor("Regional", 
                                    levels = c("Municipal", "City district",
                                               "Regional", "Chamber of Deputies",
                                               "Senate", "European Parliament")), 
             election_date = as.Date("2000-11-12", format = "%Y-%m-%d"),
             candidate_voteN = if_else(is.na(candidate_voteN), 0, candidate_voteN),
             candidate_voteP = round(candidate_voteN / party_voteN * 100, 2))
  }),
  
  tar_target(reg_2004, command = {
    krzast <- read_excel(here("data", "KZ2004", 
                              "KZ2004_cisel_20230223_xlsx", 
                              "kzciskr.xlsx")) %>% 
      select(KRZAST, KRAJ)
    reg_vysledky <- read_excel(here("data", "KZ2004", 
                                    "KZ2004_data_20230223_xlsx", 
                                    "kzt6p.xlsx")) %>% 
      group_by(KSTRANA, KRAJ) %>% 
      summarise(party_voteN = sum(POC_HLASU)) %>% 
      ungroup() %>% 
      group_by(KRAJ) %>% 
      mutate(party_voteP = round(party_voteN / sum(party_voteN) * 100, 2)) %>% 
      ungroup %>% 
      full_join(., krzast, by = "KRAJ") %>% 
      select(-KRAJ) %>% 
      rename_variables()
    
    parties <- read_excel(here("data", "KZ2004", "KZ2004_reg_20230223_xlsx", "kzrkl.xlsx")) %>% 
      select(KRZAST, KSTRANA, NAZEV_STRK)
    
    cpp <- read_cpp(here("data", "KZ2004", "CPPKZ04.xlsx"))
    cns <- cpp %>% rename(NSTRANA = PSTRANA, ZKRATKAN8 = ZKRATKAP8)
    list_path <- here("data", "KZ2004", "KZ2004_reg_20230223_xlsx", "kzrk.xlsx")
    year <- as.numeric(stringr::str_extract(list_path, "[0-9]{4}"))
    read_excel(list_path) %>% 
      left_join(., cpp, by = "PSTRANA") %>%
      left_join(., cns, by = "NSTRANA") %>%
      left_join(., parties, by = c("KRZAST", "KSTRANA")) %>% 
      mutate(MANDAT = as.numeric(MANDAT == "A"), 
             PLATNOST = as.numeric(PLATNOST != "A")) %>% 
      mutate(row_id = row_number(), 
             ROK_NAROZENI = year - VEK, 
             MANDAT = if_else(is.na(MANDAT), 0, MANDAT)) %>%
      merge_and_recode_titles %>%
      categorize_sex(., unique_first_names) %>% 
      remove_order_from_last_name() %>% 
      select(-c(POCPROC)) %>% 
      rename_variables() %>% 
      left_join(., reg_vysledky, by = c("region_code", "candidate_partyrun_code")) %>% 
      mutate(election_type = factor("Regional", 
                                    levels = c("Municipal", "City district",
                                               "Regional", "Chamber of Deputies",
                                               "Senate", "European Parliament")), 
             election_date = as.Date("2004-11-05", format = "%Y-%m-%d"), 
             candidate_voteP = round(candidate_voteN / party_voteN * 100, 2))
  }),
  
  tar_target(reg_2008, command = {
    reg_vysledky <- read_html(here("data", "KZ2008", "vysledky.xml")) %>% 
      parse_kz_results()
    
    parties <- read_parties(here("data", "KZ2008", "KZ2008_reg_20230223_xlsx", "kzrkl.xlsx"), 
                            pass) %>% 
      unique
    cpp <- read_cpp(here("data", "KZ2008", "ciselnikyKZ2008", "CPP.xlsx"), 
                    clean_dbf_excel_colnames)
    cns <- read_cns(here("data", "KZ2008", "ciselnikyKZ2008", "CNS.xlsx"), 
                    clean_dbf_excel_colnames)
    read_candidates(here("data", "KZ2008", "KZ2008_reg_20230223_xlsx", "kzrk.xlsx"), 
                    parties, cpp, cns, cleanup_f = pass) %>% 
      categorize_sex(., unique_first_names) %>% 
      mutate(PRIJMENI = if_else(PRIJMENI == "Svoboda gen.", "Svoboda", PRIJMENI), 
             MANDAT = as.numeric(if_else(is.na(MANDAT), "N", MANDAT) == "A"), 
             PLATNOST = ifelse(PLATNOST == "A", 0, 1)) %>% 
      select(-c(POCPROC)) %>% 
      rename_variables() %>% 
      left_join(., reg_vysledky, by = c("region_code", "candidate_partyrun_code"), 
                relationship = "many-to-one") %>% 
      mutate(election_type = factor("Regional", 
                                    levels = c("Municipal", "City district",
                                               "Regional", "Chamber of Deputies",
                                               "Senate", "European Parliament")), 
             election_date = as.Date("2008-10-17", format = "%Y-%m-%d"), 
             candidate_voteP = round(candidate_voteN / party_voteN * 100, 2))
      
  }),
  
  tar_target(reg_2012, command = {
    reg_vysledky <- read_html(here("data", "KZ2012", "vysledky.xml")) %>% 
      parse_kz_results()
    
    parties <- read_parties(here("data", "KZ2012", "KZ2012_reg_20230223_xlsx", "kzrkl.xlsx")) %>% 
      unique
    cpp <- read_cpp(here("data", "KZ2012", "ciselniky20121010", "CPP.xlsx"), 
                    clean_dbf_excel_colnames)
    cns <- read_cns(here("data", "KZ2012", "ciselniky20121010", "CNS.xlsx"), 
                    clean_dbf_excel_colnames)
    read_candidates(here("data", "KZ2012", "KZ2012_reg_20230223_xlsx", "kzrk.xlsx"), 
                    parties, cpp, cns, pass) %>% 
      categorize_sex(., unique_first_names) %>% 
      remove_order_from_last_name() %>% 
      filter(JMENO != "Registrační úřad ponechal pozici volnou") %>% 
      mutate(MANDAT = as.numeric(if_else(is.na(MANDAT), "N", MANDAT) == "A"), 
             PLATNOST = ifelse(PLATNOST == "A", 0, 1)) %>% 
      select(-c(POCPROC)) %>%
      rename_variables() %>% 
      left_join(., reg_vysledky, by = c("region_code", "candidate_partyrun_code"), 
                relationship = "many-to-one") %>% 
      mutate(election_type = factor("Regional", 
                                    levels = c("Municipal", "City district",
                                               "Regional", "Chamber of Deputies",
                                               "Senate", "European Parliament")), 
             election_date = as.Date("2012-10-12", format = "%Y-%m-%d"), 
             candidate_voteP = round(candidate_voteN / party_voteN * 100, 2))
  }),
  
  tar_target(reg_2016, command = {
    reg_vysledky <- read_html(here("data", "KZ2016", "vysledky.xml")) %>% 
      parse_kz_results()
    
    parties <- read_parties(here("data", "KZ2016", "KZ2016reg20161008_xlsx", "kzrkl.xlsx")) %>% 
      unique
    cpp <- read_cpp(here("data", "KZ2016", "KZ2016ciselniky20161007", "cpp.xlsx"))
    cns <- read_cns(here("data", "KZ2016", "KZ2016ciselniky20161007", "cns.xlsx"))
    read_candidates(here("data", "KZ2016", "KZ2016reg20161008_xlsx", "kzrk.xlsx"), 
                    parties, cpp, cns) %>%
      mutate(PLATNOST = ifelse(PLATNOST == "A", 0, 1), 
             MANDAT = ifelse(MANDAT == "A", 1, 0)) %>% 
      categorize_sex(., unique_first_names) %>% 
      filter(JMENO != "Registrační úřad ponechal pozici volnou") %>% 
      select(-c(POCPROC, POHLAVI)) %>% 
      rename_variables() %>% 
      left_join(., reg_vysledky, by = c("region_code", "candidate_partyrun_code"), 
                relationship = "many-to-one") %>% 
      mutate(election_type = factor("Regional", 
                                    levels = c("Municipal", "City district",
                                               "Regional", "Chamber of Deputies",
                                               "Senate", "European Parliament")), 
             election_date = as.Date("2016-10-07", format = "%Y-%m-%d"), 
             candidate_voteP = round(candidate_voteN / party_voteN * 100, 2))
  }),
  
  tar_target(reg_2020, command = {
    reg_vysledky <- read_html(here("data", "KZ2020", "vysledky.xml")) %>% 
      parse_kz_results()
    
    parties <- read_parties(here("data", "KZ2020", "KZ2020reg20201004a_xlsx", "kzrkl.xlsx")) %>% 
      unique
    cpp <- read_cpp(here("data", "KZ2020", "KZ2020ciselniky20201002", "cpp.xlsx"))
    cns <- read_cns(here("data", "KZ2020", "KZ2020ciselniky20201002", "cns.xlsx"))
    read_candidates(here("data", "KZ2020", "KZ2020reg20201004a_xlsx", "kzrk.xlsx"), 
                    parties, cpp, cns) %>%
      mutate(PLATNOST = ifelse(PLATNOST == "A", 0, 1), 
             MANDAT = ifelse(MANDAT == "A", 1, 0)) %>% 
      categorize_sex(., unique_first_names) %>% 
      remove_order_from_last_name() %>% 
      select(-c(POCPROC, POHLAVI)) %>% 
      rename_variables() %>% 
      left_join(., reg_vysledky, by = c("region_code", "candidate_partyrun_code"), 
                relationship = "many-to-one") %>% 
      mutate(election_type = factor("Regional", 
                                    levels = c("Municipal", "City district",
                                               "Regional", "Chamber of Deputies",
                                               "Senate", "European Parliament")), 
             election_date = as.Date("2020-10-02", format = "%Y-%m-%d"), 
             candidate_voteP = round(candidate_voteN / party_voteN * 100, 2))
  }), 
  
  tar_target(reg_panel, {
    reg00_04 <- match_reg_data(reg_2000, reg_2004, 
                               multiple_last_names_eligible)
    reg04_08 <- match_reg_data(reg_2004, reg_2008, 
                               multiple_last_names_eligible)
    
    reg00_08 <- match_reg_data(
      reg_2000 %>% 
        filter(!row_id %in% reg00_04$row_id.x),
      reg_2008 %>% 
        filter(!row_id %in% reg04_08$row_id.y), 
      multiple_last_names_eligible
    )
    
    reg08_12 <- match_reg_data(reg_2008, reg_2012, 
                               multiple_last_names_eligible)
    reg04_12 <- match_reg_data(
      reg_2004 %>% 
        filter(!row_id %in% reg04_08$row_id.x),
      reg_2012 %>% 
        filter(!row_id %in% reg08_12$row_id.y), 
      multiple_last_names_eligible
    )
    reg00_12 <- match_reg_data(
      reg_2000 %>% 
        filter(!row_id %in% reg00_04$row_id.x) %>% 
        filter(!row_id %in% reg00_08$row_id.x),
      reg_2012 %>% 
        filter(!row_id %in% reg08_12$row_id.y) %>% 
        filter(!row_id %in% reg04_12$row_id.y), 
      multiple_last_names_eligible
    )
    
    reg12_16 <- match_reg_data(reg_2012, reg_2016, 
                               multiple_last_names_eligible)
    reg08_16 <- match_reg_data(
      reg_2008 %>% 
        filter(!row_id %in% reg08_12$row_id.x),
      reg_2016 %>% 
        filter(!row_id %in% reg12_16$row_id.y), 
      multiple_last_names_eligible
    )
    reg04_16 <- match_reg_data(
      reg_2004 %>% 
        filter(!row_id %in% reg04_08$row_id.x) %>% 
        filter(!row_id %in% reg04_12$row_id.x),
      reg_2016 %>% 
        filter(!row_id %in% reg12_16$row_id.y) %>% 
        filter(!row_id %in% reg08_16$row_id.x), 
      multiple_last_names_eligible
    )
    reg00_16 <- match_reg_data(
      reg_2000 %>% 
        filter(!row_id %in% reg00_04$row_id.x) %>% 
        filter(!row_id %in% reg00_08$row_id.x) %>% 
        filter(!row_id %in% reg00_12$row_id.x),
      reg_2016 %>% 
        filter(!row_id %in% reg12_16$row_id.y) %>% 
        filter(!row_id %in% reg08_16$row_id.x) %>% 
        filter(!row_id %in% reg04_16$row_id.x), 
      multiple_last_names_eligible
    )
    
    reg16_20 <- match_reg_data(reg_2016, reg_2020, 
                               multiple_last_names_eligible)
    reg12_20 <- match_reg_data(
      reg_2012 %>% 
        filter(!row_id %in% reg12_16$row_id.x),
      reg_2020 %>% 
        filter(!row_id %in% reg16_20$row_id.y), 
      multiple_last_names_eligible
    )
    reg08_20 <- match_reg_data(
      reg_2008 %>% 
        filter(!row_id %in% reg08_12$row_id.x) %>% 
        filter(!row_id %in% reg08_16$row_id.x),
      reg_2020 %>% 
        filter(!row_id %in% reg16_20$row_id.y) %>% 
        filter(!row_id %in% reg12_20$row_id.y), 
      multiple_last_names_eligible
    )
    reg04_20 <- match_reg_data(
      reg_2004 %>% 
        filter(!row_id %in% reg04_08$row_id.x) %>% 
        filter(!row_id %in% reg04_12$row_id.x) %>% 
        filter(!row_id %in% reg04_16$row_id.x),
      reg_2020 %>% 
        filter(!row_id %in% reg16_20$row_id.y) %>% 
        filter(!row_id %in% reg12_20$row_id.y) %>% 
        filter(!row_id %in% reg08_20$row_id.x), 
      multiple_last_names_eligible
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
        filter(!row_id %in% reg04_20$row_id.x), 
      multiple_last_names_eligible
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
      tidyr::pivot_longer(., cols = 1:(ncol(.)-1), names_to = "election_year",
                          values_to = "row_id") %>%
      filter(!is.na(row_id)) %>%
      mutate(election_year = stringr::str_extract(election_year, "[0-9]{4}"))
    
    reg_candidates <- bind_rows(
      reg_2000 %>% mutate(election_year = "2000"),
      reg_2004 %>% mutate(election_year = "2004"),
      reg_2008 %>% mutate(election_year = "2008"),
      reg_2012 %>% mutate(election_year = "2012"),
      reg_2016 %>% mutate(election_year = "2016"),
      reg_2020 %>% mutate(election_year = "2020")
    )
    
    full_join(pivot_table_long, reg_candidates, by = c("row_id", "election_year")) 
    
  }),
  
  tar_target(
    reg_panel_check, 
    stopifnot(nrow(reg_panel) == (
      nrow(reg_2000) + nrow(reg_2004) + nrow(reg_2008) + nrow(reg_2012) + 
        nrow(reg_2016) + nrow(reg_2020) 
    ))
  ),
  
  tar_target(
    reg_panel_rds, 
    saveRDS(reg_panel, "output/election_specific/reg_candidates_panel.rds")
  ),
  
  tar_target(
    reg_panel_csv, 
    write.csv(reg_panel, "output/election_specific/reg_candidates_panel.csv", 
              row.names = FALSE)
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
    candidates <- read_excel(candidates_path) %>%
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
        PRIJMENI = if_else(PRIJMENI == "Pernes.PhDr.", "Pernes", PRIJMENI) %>% 
          gsub(",$", "", .)
      ) %>% 
      extract_titles_from_first_name %>% 
      categorize_sex(., unique_first_names) %>% 
      select(-c(SLOZENI, VSTRANA, OKRES)) %>% 
      rename_variables() %>% 
      mutate(
        candidate_place_code = municipality_id, 
        candidate_place_name = municipality_name
      )
    
    party_results <- candidates %>% 
      group_by(municipality_id, electoral_district_no, party_rank) %>% 
      summarise(party_voteN = sum(candidate_voteN)) %>% 
      group_by(municipality_id, electoral_district_no) %>% 
      mutate(party_voteP = round(party_voteN / sum(party_voteN) * 100, 2) %>% 
               if_else(is.nan(.), 0, .)) %>% 
      ungroup()
    
    left_join(
      candidates, party_results, 
      by = c("municipality_id", "electoral_district_no", "party_rank"),
      relationship = "many-to-one"
    ) %>%
      mutate(
        election_date = as.Date("1994-11-18", format = "%Y-%m-%d"), 
        candidate_voteP = round(candidate_voteN / party_voteN * 100, 2) %>% 
          if_else(is.nan(.), 0, .))
  }),
  
  tar_target(mun_1998, command = {
    parties <- read_excel(here("data", "KV1998", "CVSKV98.xlsx"))
    cpp <- read_cpp(here("data", "KV1998", "CPPKV98.xlsx"), function(x) 
    {x %>% rename(ZKRATKAP8 = ZKRATKAP)})
    cns <- read_cns(here("data", "KV1998", "CNSKV98.xlsx"))
    candidates_path <- here::here("data", "KV1998", "KV98_RK.xlsx")
    year <- as.numeric(stringr::str_extract(candidates_path, "[0-9]{4}"))
    candidates <- read_excel(candidates_path) %>%
      left_join(., parties, by = c("VSTRANA")) %>%
      left_join(., cpp, by = "PSTRANA") %>%
      left_join(., cns, by = "NSTRANA") %>%
      rename(TITULY = TITUL) %>%
      extract_titles_from_last_name_1998 %>% 
      mutate(row_id = row_number(), 
             ROK_NAROZENI = year - VEK, 
             TITUL_KATEGORIE = categorize_titles(TITULY), 
             JMENO = ifelse(JMENO == toupper(JMENO), 
                            stringr::str_to_title(JMENO, locale = "cs"), 
                            JMENO) %>% 
               if_else(. %in% c("Jan ml.", "Jan st."), 
                       "Jan", .),
             PRIJMENI = ifelse(PRIJMENI == toupper(PRIJMENI), 
                               stringr::str_to_title(PRIJMENI, locale = "cs"), 
                               PRIJMENI) %>% 
               gsub(",$", "", .)) %>% 
      categorize_sex(., unique_first_names) %>% 
      mutate(PRIJMENI = if_else(PRIJMENI == "Viktora starší", "Viktora", PRIJMENI)) %>% 
      filter(PRIJMENI != "vytištěného HL") %>% 
      select(-c(SLOZENI, VSTRANA, ZKRATKAV30, OKRES)) %>% 
      rename_variables() %>% 
      mutate(
        candidate_place_code = municipality_id, 
        candidate_place_name = municipality_name
      )
    
    party_results <- candidates %>% 
      group_by(municipality_id, electoral_district_no, party_rank) %>% 
      summarise(party_voteN = sum(candidate_voteN)) %>% 
      group_by(municipality_id, electoral_district_no) %>% 
      mutate(party_voteP = round(party_voteN / sum(party_voteN) * 100, 2) %>% 
               if_else(is.nan(.), 0, .)) %>% 
      ungroup()
    
    left_join(
      candidates, party_results, 
      by = c("municipality_id", "electoral_district_no", "party_rank"),
      relationship = "many-to-one"
    ) %>% 
      mutate(election_date = as.Date("1998-11-13", format = "%Y-%m-%d"),
             candidate_voteP = round(candidate_voteN / party_voteN * 100, 2) %>% 
               if_else(is.nan(.), 0, .))
  }),
  
  tar_target(mun_2002, command = {
    # parties <- read_excel(here("data", "KV2002", "CVSKV02.xlsx"))
    cpp <- read_cpp(here("data", "KV2002", "CPPKV02.xlsx"))
    cns <- read_cns(here("data", "KV2002", "CNSKV02.xlsx"))
    candidates_path <- here::here("data", "KV2002", "KV02_RK.xlsx")
    year <- as.numeric(stringr::str_extract(candidates_path, "[0-9]{4}"))
    candidates <- read_excel(candidates_path) %>% 
      mutate(row_id = row_number()) %>% 
      mutate(
        JMENO = case_when(
          row_id == 156921 ~ "Miroslav",
          row_id == 163205 ~ "Vlasta",
          row_id == 1792 ~ "Robert", 
          row_id == 46438 ~ "Petr",
          row_id == 67714 ~ "Ladislav",
          row_id == 95679 ~ "Jana",
          row_id == 98561 ~ "Bohumil",
          row_id == 157279 ~ "Hana",
          JMENO == "Jitka Waschneko" ~ "Jitka",
          TRUE ~ JMENO
        ), 
        PRIJMENI = case_when(
          row_id == 156921 ~ "Procházka",
          row_id == 163205 ~ "Sršňová",
          row_id == 1792 ~ "Buchner", 
          row_id == 67714 ~ "Kohout",
          row_id == 95679 ~ "Jebavá",
          row_id == 98561 ~ "Černý",
          row_id == 157279 ~ "Lebedová",
          PRIJMENI == "Vá Mühlbauerová" ~ "Waschneková Mühlbauerová",
          TRUE ~ PRIJMENI
        ),
        TITULPRED = case_when(
          row_id == 156921 ~ "Mgr.",
          row_id == 163205 ~ "Mgr.",
          row_id == 1792 ~ "Ing.", 
          row_id == 46438 ~ "Ing.",
          row_id == 67714 ~ "Ing.",
          row_id == 95679 ~ "Ing.",
          row_id == 98561 ~ "Ing.",
          row_id == 157279 ~ "Ing.",
          TRUE ~ TITULPRED
        ),
        TITULZA = case_when(
          row_id == 156921 ~ NA_character_, 
          row_id == 163205 ~ NA_character_,
          row_id == 1792 ~ NA_character_, 
          row_id == 67714 ~ NA_character_,
          row_id == 95679 ~ NA_character_,
          row_id == 98561 ~ NA_character_,
          row_id == 157279 ~ NA_character_,
          TRUE ~ TITULZA
        )
      ) %>% 
      # left_join(., parties, by = c("VSTRANA")) %>%
      left_join(., cpp, by = "PSTRANA") %>%
      left_join(., cns, by = "NSTRANA") %>%
      merge_and_recode_titles %>%
      mutate(ROK_NAROZENI = year - VEK, 
             JMENO = ifelse(JMENO == toupper(JMENO), 
                            stringr::str_to_title(JMENO, locale = "cs"), 
                            JMENO),
             PRIJMENI = ifelse(PRIJMENI == toupper(PRIJMENI), 
                               stringr::str_to_title(PRIJMENI, locale = "cs"), 
                               PRIJMENI)) %>% 
      categorize_sex(., unique_first_names) %>% 
      remove_order_from_last_name %>% 
      mutate(
        PRIJMENI = gsub(",.*", "", PRIJMENI) %>% 
          gsub("\\(r\\.", "(", .) %>% 
          gsub("\\.$", "", .) %>% 
          stringr::str_trim(., "both")
      ) %>% 
      select(-c(OKRES, OSTRANA)) %>% 
      rename_variables() %>% 
      mutate(
        candidate_place_code = municipality_id, 
        candidate_place_name = municipality_name
      )
    
    party_results <- candidates %>% 
      group_by(municipality_id, electoral_district_no, party_rank) %>% 
      summarise(party_voteN = sum(candidate_voteN)) %>% 
      group_by(municipality_id, electoral_district_no) %>% 
      mutate(party_voteP = round(party_voteN / sum(party_voteN) * 100, 2)) %>% 
      ungroup()
    
    left_join(
      candidates, party_results, 
      by = c("municipality_id", "electoral_district_no", "party_rank"),
      relationship = "many-to-one"
    ) %>% 
      mutate(election_date = as.Date("2002-11-02", format = "%Y-%m-%d"))
  }),
  
  tar_target(mun_2006, command = {
    parties <- read_municipal_parties(here("data", "KV2006", "KV2006reg20140909_xlsx", "kvros.xlsx"))
    coco <- read_excel(here("data", "KV2006", "KV2006reg20140909_xlsx", "kvrzcoco.xlsx")) %>% 
      select(KODZASTUP, TYPZASTUP, municipality_name = NAZEVZAST) %>% 
      unique()
    cpp <- read_cpp_xml(here("data", "KV2006", "KV2006ciselniky20140909", "cpp.xml"))
    cns <- read_cns_xml(here("data", "KV2006", "KV2006ciselniky20140909", "cns.xml"))
    candidates <- read_municipal_candidates(here("data", "KV2006", "KV2006reg20140909_xlsx", "kvrk.xlsx"), 
                              parties, cpp, cns) %>%
      left_join(., coco, by = "KODZASTUP") %>% 
      mutate(JMENO = ifelse(JMENO == toupper(JMENO), 
                            stringr::str_to_title(JMENO, locale = "cs"), 
                            JMENO),
             PRIJMENI = ifelse(PRIJMENI == toupper(PRIJMENI), 
                               stringr::str_to_title(PRIJMENI, locale = "cs"), 
                               PRIJMENI), 
             MANDAT = if_else(is.na(MANDAT), 0, MANDAT)) %>% 
      categorize_sex(., unique_first_names) %>% 
      remove_order_from_last_name %>% 
      mutate(PRIJMENI = gsub(",\\sroz\\.", "", PRIJMENI) %>% 
               gsub("\\sroz\\.", "", .) %>% 
               gsub("\\(roz\\.[ ]*", "(", .)) %>% 
      select(-c(OKRES, OSTRANA, POHLAVI, DATNAR, 
                POCHL_PRES)) %>% 
      rename_variables() %>% 
      mutate(candidate_voteN = if_else(is.na(candidate_voteN), 0, 
                                       candidate_voteN), 
        candidate_place_code = municipality_id, 
        candidate_place_name = municipality_name
      )
    
    party_results <- candidates %>% 
      group_by(municipality_id, electoral_district_no, party_rank) %>% 
      summarise(party_voteN = sum(candidate_voteN)) %>% 
      group_by(municipality_id, electoral_district_no) %>% 
      mutate(party_voteP = round(party_voteN / sum(party_voteN) * 100, 2)) %>% 
      ungroup()
    
    left_join(
      candidates, party_results, 
      by = c("municipality_id", "electoral_district_no", "party_rank"),
      relationship = "many-to-one"
    ) %>% mutate(election_date = as.Date("2006-10-20", format = "%Y-%m-%d"), 
                 candidate_voteP = if_else(
                   is.na(candidate_voteP) & candidate_voteN == 0,
                   0, candidate_voteP))
      
  }),
  
  tar_target(mun_2010, command = {
    parties <- read_municipal_parties(here("data", "KV2010", "KV2010reg20140903_xlsx", "kvros.xlsx"))
    cpp <- read_cpp_xml(here("data", "KV2010", "KV2010ciselniky20140903", "cpp.xml"))
    cns <- read_cns_xml(here("data", "KV2010", "KV2010ciselniky20140903", "cns.xml"))
    coco <- read_excel(here("data", "KV2010", "KV2010reg20140903_xlsx", "kvrzcoco.xlsx")) %>% 
      select(KODZASTUP, TYPZASTUP, municipality_name = NAZEVZAST) %>% 
      unique()
    
    candidates <- read_municipal_candidates(here("data", "KV2010", "KV2010reg20140903_xlsx", "kvrk.xlsx"), 
                              parties, cpp, cns) %>% 
      left_join(., coco, by = "KODZASTUP") %>% 
      remove_order_from_last_name %>% 
      mutate(JMENO = ifelse(JMENO == toupper(JMENO), 
                            stringr::str_to_title(JMENO, locale = "cs"), 
                            JMENO),
             PRIJMENI = ifelse(PRIJMENI == toupper(PRIJMENI), 
                               stringr::str_to_title(PRIJMENI, locale = "cs"), 
                               PRIJMENI), 
             MANDAT = if_else(is.na(MANDAT), 0, MANDAT)) %>% 
      mutate(
        TITULZA = case_when(
          PRIJMENI == "Říčařová,CSc." ~ "CSc.",
          PRIJMENI == "Janderková DiS." ~ "DiS.",
          TRUE ~ TITULZA
        ), 
        PRIJMENI = case_when(
          PRIJMENI == "Říčařová,CSc." ~ "Říčařová", 
          PRIJMENI == "Kliková /roz. Hůlková/" ~ "Kliková (Hůlková)",
          PRIJMENI == "Janderková DiS." ~ "Janderková",
          TRUE ~ PRIJMENI
        ) %>% gsub(",$", "", .)
      ) %>% 
      merge_and_recode_titles() %>% 
      categorize_sex(., unique_first_names) %>% 
      mutate(PRIJMENI = gsub(",\\sroz\\.", "", PRIJMENI) %>% 
               gsub("\\sroz\\.", "", .) %>% 
               gsub("\\(roz\\.[ ]*", "(", .) %>% 
               gsub(",\\srozená", "", .) %>% 
               gsub(",roz\\.", "", .) %>% 
               gsub("\\(rozená\\s", "(", .)) %>% 
      filter(JMENO != "Registrační úřad ponechal pozici volnou") %>% 
      select(-c(OKRES, OSTRANA, POHLAVI, DATNAR, 
                POCHL_PRES)) %>% 
      rename_variables() %>% 
      mutate(candidate_voteN = if_else(is.na(candidate_voteN), 0, 
                                       candidate_voteN), 
        candidate_place_code = municipality_id, 
        candidate_place_name = municipality_name
      )
    
    party_results <- candidates %>% 
      group_by(municipality_id, electoral_district_no, party_rank) %>% 
      summarise(party_voteN = sum(candidate_voteN)) %>% 
      group_by(municipality_id, electoral_district_no) %>% 
      mutate(party_voteP = round(party_voteN / sum(party_voteN) * 100, 2)) %>% 
      ungroup()
    
    left_join(
      candidates, party_results, 
      by = c("municipality_id", "electoral_district_no", "party_rank"),
      relationship = "many-to-one"
    ) %>% mutate(election_date = as.Date("2010-10-15", format = "%Y-%m-%d"), 
                 candidate_voteP = if_else(
                   is.na(candidate_voteP) & candidate_voteN == 0,
                   0, candidate_voteP))
  }),
  
  tar_target(mun_2014, command = {
    parties <- read_municipal_parties(here("data", "KV2014", "KV2014reg20141014_xlsx", "kvros.xlsx"))
    cpp <- read_cpp_xml(here("data", "KV2014", "KV2014ciselniky20141008", "cpp.xml"))
    cns <- read_cns_xml(here("data", "KV2014", "KV2014ciselniky20141008", "cns.xml"))
    coco <- read_excel(here("data", "KV2014", "KV2014reg20141014_xlsx", "kvrzcoco.xlsx")) %>% 
      select(KODZASTUP, TYPZASTUP, municipality_name = NAZEVZAST) %>% 
      unique()
    
    candidates <- read_municipal_candidates(here("data", "KV2014", "KV2014reg20141014_xlsx", "kvrk.xlsx"), 
                              parties, cpp, cns) %>%
      left_join(., coco, by = "KODZASTUP") %>% 
      remove_order_from_last_name %>% 
      mutate(JMENO = ifelse(JMENO == toupper(JMENO), 
                            stringr::str_to_title(JMENO, locale = "cs"), 
                            JMENO),
             PRIJMENI = ifelse(PRIJMENI == toupper(PRIJMENI), 
                               stringr::str_to_title(PRIJMENI, locale = "cs"), 
                               PRIJMENI), 
             MANDAT = if_else(is.na(MANDAT), 0, MANDAT)) %>% 
      categorize_sex(., unique_first_names) %>% 
      mutate(PRIJMENI = gsub(",\\sroz\\.", "", PRIJMENI) %>% 
               gsub("\\sroz\\.", "", .) %>% 
               gsub("\\([ ]*roz\\.[ ]*", "(", .) %>% 
               gsub(",\\srozená", "", .) %>% 
               gsub(",roz\\.", "", .) %>% 
               gsub("\\(rozená\\s", "(", .) %>% 
               gsub(",\\s", " ", .)) %>% 
      filter(JMENO != "Registrační úřad ponechal pozici volnou") %>% 
      select(-c(OKRES, OSTRANA, POHLAVI, DATNAR, 
                POCHL_PRES)) %>% 
      rename_variables() %>% 
      mutate(candidate_voteN = if_else(is.na(candidate_voteN), 0, 
                                       candidate_voteN), 
        candidate_place_code = municipality_id, 
        candidate_place_name = municipality_name
      )
    
    party_results <- candidates %>% 
      group_by(municipality_id, electoral_district_no, party_rank) %>% 
      summarise(party_voteN = sum(candidate_voteN)) %>% 
      group_by(municipality_id, electoral_district_no) %>% 
      mutate(party_voteP = round(party_voteN / sum(party_voteN) * 100, 2)) %>% 
      ungroup()
    
    left_join(
      candidates, party_results, 
      by = c("municipality_id", "electoral_district_no", "party_rank"),
      relationship = "many-to-one"
    ) %>% 
      mutate(election_date = as.Date("2014-10-10", format = "%Y-%m-%d"), 
             candidate_voteP = if_else(
               is.na(candidate_voteP) & candidate_voteN == 0,
               0, candidate_voteP))
  }),
  
  tar_target(mun_2018, command = {
    parties <- read_municipal_parties(here("data", "KV2018", "KV2018reg20181008_xlsx", "kvros.xlsx"))
    cpp <- read_cpp(here("data", "KV2018", "KV2018ciselniky20181004", "cpp.xlsx"))
    cns <- read_cns(here("data", "KV2018", "KV2018ciselniky20181004", "cns.xlsx"))
    coco <- read_excel(here("data", "KV2018", "KV2018reg20181008_xlsx", "kvrzcoco.xlsx")) %>% 
      select(KODZASTUP, TYPZASTUP, municipality_name = NAZEVZAST) %>% 
      unique()
    
    candidates <- read_municipal_candidates(here("data", "KV2018", "KV2018reg20181008_xlsx", "kvrk.xlsx"), 
                              parties, cpp, cns) %>%
      left_join(., coco, by = "KODZASTUP") %>% 
      remove_order_from_last_name() %>% 
      mutate(PLATNOST = ifelse(PLATNOST == "A", 0, 1), 
             MANDAT = ifelse(MANDAT == "N", 0, 1), 
             JMENO = ifelse(JMENO == toupper(JMENO), 
                            stringr::str_to_title(JMENO, locale = "cs"), 
                            JMENO),
             PRIJMENI = ifelse(PRIJMENI == toupper(PRIJMENI), 
                               stringr::str_to_title(PRIJMENI, locale = "cs"), 
                               PRIJMENI)) %>% 
      categorize_sex(., unique_first_names) %>% 
      mutate(
        TITULZA = case_when(
          PRIJMENI == "McComb B.A. B.Ed." ~ "B.A. B.Ed.",
          TRUE ~ TITULZA
        ), 
        PRIJMENI = case_when(
          PRIJMENI == "McComb B.A. B.Ed." ~ "McComb",
          PRIJMENI == "Kristová roz. Krupičková" ~ "Kristová (Krupičková)",
          TRUE ~ PRIJMENI
        )
      ) %>% 
      mutate(PRIJMENI = gsub(",\\sroz\\.", "", PRIJMENI) %>% 
               gsub("\\([ ]*roz\\.[ ]*", "(", .) %>% 
               gsub("\\([ ]*býv\\.[ ]*", "(", .) %>% 
               gsub("\\.$", "", .)) %>% 
      filter(JMENO != "Registrační úřad ponechal pozici volnou") %>% 
      select(-c(OKRES, OSTRANA, POHLAVI, DATNAR, 
                POCHL_PRES)) %>% 
      rename_variables() %>% 
      mutate(
        candidate_voteN = if_else(is.na(candidate_voteN), 0, candidate_voteN), 
        candidate_voteP = if_else(is.na(candidate_voteP), 0, candidate_voteP),
        candidate_seat = if_else(is.na(candidate_seat), 0, candidate_seat),
        candidate_place_code = municipality_id, 
        candidate_place_name = municipality_name
      )
    
    party_results <- candidates %>% 
      group_by(municipality_id, electoral_district_no, party_rank) %>% 
      summarise(party_voteN = sum(candidate_voteN)) %>% 
      group_by(municipality_id, electoral_district_no) %>% 
      mutate(party_voteP = round(party_voteN / sum(party_voteN) * 100, 2)) %>% 
      ungroup()
    
    left_join(
      candidates, party_results, 
      by = c("municipality_id", "electoral_district_no", "party_rank"),
      relationship = "many-to-one"
    ) %>% mutate(election_date = as.Date("2018-10-05", format = "%Y-%m-%d"))
  }), 
  
  tar_target(mun_2022, command = {
    parties <- read_excel(here("data", "KV2022", "KV2022reg20240623_xlsx", "kvros.xlsx")) %>% 
      filter(DATUMVOLEB == min(DATUMVOLEB)) %>% 
      select(KODZASTUP, COBVODU, POR_STR_HL, OSTRANA, NAZEVCELK)
    cpp <- read_cpp(here("data", "KV2022", "KV2022ciselniky20240623", "cpp.xlsx"))
    cns <- read_cns(here("data", "KV2022", "KV2022ciselniky20240623", "cns.xlsx"))
    coco <- read_excel(here("data", "KV2022", "KV2022reg20240623_xlsx", "kvrzcoco.xlsx")) %>% 
      select(KODZASTUP, TYPZASTUP, municipality_name = NAZEVZAST) %>% 
      unique()
    
    candidates <- read_excel(here("data", "KV2022", "KV2022reg20240623_xlsx", "kvrk.xlsx")) %>%
      filter(DATUMVOLEB == min(DATUMVOLEB)) %>% 
      left_join(., coco, by = "KODZASTUP") %>% 
      left_join(., parties, by = c("KODZASTUP", "COBVODU", "POR_STR_HL", "OSTRANA")) %>%
      left_join(., cpp, by = "PSTRANA") %>%
      left_join(., cns, by = "NSTRANA") %>%
      mutate(row_id = row_number(), 
             ROK_NAROZENI = 2022 - VEK, 
             PLATNOST = ifelse(PLATNOST == "A", 0, 1), 
             MANDAT = ifelse(MANDAT == "N", 0, 1)) %>%
      mutate(
        TITULZA = case_when(
          PRIJMENI == "Lukášová, MBA" ~ "MBA",
          PRIJMENI == "Kohlíčková, M.A." ~ "M.A.",
          PRIJMENI == "Šena, dipl.technik" ~ "dipl. technik",
          TRUE ~ TITULZA
        ),
        PRIJMENI = case_when(
          PRIJMENI == "Lukášová, MBA" ~ "Lukášová",
          PRIJMENI == "Kohlíčková, M.A." ~ "Kohlíčková",
          PRIJMENI == "Šena, dipl.technik" ~ "Šena",
          TRUE ~ PRIJMENI
        )
      ) %>% 
      merge_and_recode_titles %>% 
      categorize_sex(., unique_first_names) %>% 
      remove_order_from_last_name() %>% 
      mutate(PRIJMENI = gsub(",\\sroz\\.", "", PRIJMENI) %>% 
               gsub("\\(č.p. [0-9]+\\)", "", .) %>% 
               gsub("\\([ ]*roz\\.[ ]*", "(", .) %>% 
               gsub("\\(rozená\\s", "(", .) %>% 
               gsub("\\([ ]*býv\\.[ ]*", "(", .) %>% 
               gsub("\\bml\\.$", "", .) %>% 
               gsub("\\bst\\.$", "", .) %>% 
               gsub("\\,$", "", .) %>% 
               stringr::str_trim() %>% 
               gsub("\\sml\\.$", "", .)) %>% 
      filter(JMENO != "Registrační úřad ponechal pozici volnou") %>% 
      select(-c(OKRES, OSTRANA, PRESKOCENI)) %>% 
      rename_variables() %>% 
      mutate(
        candidate_place_code = municipality_id, 
        candidate_place_name = municipality_name
      )
    
    party_results <- candidates %>% 
      group_by(municipality_id, electoral_district_no, party_rank) %>% 
      summarise(party_voteN = sum(candidate_voteN)) %>% 
      group_by(municipality_id, electoral_district_no) %>% 
      mutate(party_voteP = round(party_voteN / sum(party_voteN) * 100, 2)) %>% 
      ungroup()
    
    left_join(
      candidates, party_results, 
      by = c("municipality_id", "electoral_district_no", "party_rank"),
      relationship = "many-to-one"
    ) %>% mutate(election_date = as.Date(as.character(election_date), "%Y%m%d"))
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
  tar_target(mc_1994, filter_city_districts(mun_1994, district_municipalities_map) %>% 
               mutate(election_type = factor("City district", 
                                             levels = c("Municipal", "City district",
                                                        "Regional", "Chamber of Deputies",
                                                        "Senate", "European Parliament")))),
  tar_target(mc_1998, filter_city_districts(mun_1998, district_municipalities_map) %>% 
               mutate(election_type = factor("City district", 
                                             levels = c("Municipal", "City district",
                                                        "Regional", "Chamber of Deputies",
                                                        "Senate", "European Parliament")))),
  tar_target(mc_2002, filter_city_districts(mun_2002, district_municipalities_map) %>% 
               mutate(election_type = factor("City district", 
                                             levels = c("Municipal", "City district",
                                                        "Regional", "Chamber of Deputies",
                                                        "Senate", "European Parliament")))),
  tar_target(mc_2006, filter_city_districts(mun_2006, district_municipalities_map) %>% 
               mutate(election_type = factor("City district", 
                                             levels = c("Municipal", "City district",
                                                        "Regional", "Chamber of Deputies",
                                                        "Senate", "European Parliament")))),
  tar_target(mc_2010, filter_city_districts(mun_2010, district_municipalities_map) %>% 
               mutate(election_type = factor("City district", 
                                             levels = c("Municipal", "City district",
                                                        "Regional", "Chamber of Deputies",
                                                        "Senate", "European Parliament")))),
  tar_target(mc_2014, filter_city_districts(mun_2014, district_municipalities_map) %>% 
               mutate(election_type = factor("City district", 
                                             levels = c("Municipal", "City district",
                                                        "Regional", "Chamber of Deputies",
                                                        "Senate", "European Parliament")))),
  tar_target(mc_2018, filter_city_districts(mun_2018, district_municipalities_map) %>% 
               mutate(election_type = factor("City district", 
                                             levels = c("Municipal", "City district",
                                                        "Regional", "Chamber of Deputies",
                                                        "Senate", "European Parliament")))),
  tar_target(mc_2022, filter_city_districts(mun_2022, district_municipalities_map) %>% 
               mutate(election_type = factor("City district", 
                                             levels = c("Municipal", "City district",
                                                        "Regional", "Chamber of Deputies",
                                                        "Senate", "European Parliament")))),
  
  tar_target(mc_panel, {
    mc_94_98 <- match_mun_data(mc_1994, mc_1998, 
                               multiple_last_names_eligible)

    mc_98_02 <- match_mun_data(mc_1998, mc_2002, 
                               multiple_last_names_eligible)

    mc_94_02 <- match_mun_data(
      mc_1994 %>%
        filter(!row_id %in% mc_94_98$row_id.x),
      mc_2002 %>%
        filter(!row_id %in% mc_98_02$row_id.y), 
      multiple_last_names_eligible
    )

    mc_02_06 <- match_mun_data(mc_2002, mc_2006, 
                               multiple_last_names_eligible)

    mc_98_06 <- match_mun_data(
      mc_1998 %>%
        filter(!row_id %in% mc_98_02$row_id.x),
      mc_2006 %>%
        filter(!row_id %in% mc_02_06$row_id.y), 
      multiple_last_names_eligible
    )

    mc_94_06 <- match_mun_data(
      mc_1994 %>%
        filter(!row_id %in% mc_94_98$row_id.x) %>%
        filter(!row_id %in% mc_94_02$row_id.x),
      mc_2006 %>%
        filter(!row_id %in% mc_98_06$row_id.y) %>%
        filter(!row_id %in% mc_02_06$row_id.y), 
      multiple_last_names_eligible
    )

    mc_06_10 <- match_mun_data(mc_2006, mc_2010, 
                               multiple_last_names_eligible)

    mc_02_10 <- match_mun_data(
      mc_2002 %>%
        filter(!row_id %in% mc_02_06$row_id.x),
      mc_2010 %>%
        filter(!row_id %in% mc_06_10$row_id.y), 
      multiple_last_names_eligible
    )

    mc_98_10 <- match_mun_data(
      mc_1998 %>%
        filter(!row_id %in% mc_98_02$row_id.x) %>%
        filter(!row_id %in% mc_98_06$row_id.x),
      mc_2010 %>%
        filter(!row_id %in% mc_02_10$row_id.y) %>%
        filter(!row_id %in% mc_06_10$row_id.y), 
      multiple_last_names_eligible
    )

    mc_94_10 <- match_mun_data(
      mc_1994 %>%
        filter(!row_id %in% mc_94_98$row_id.x) %>%
        filter(!row_id %in% mc_94_02$row_id.x) %>%
        filter(!row_id %in% mc_94_06$row_id.x),
      mc_2010 %>%
        filter(!row_id %in% mc_98_10$row_id.y) %>%
        filter(!row_id %in% mc_02_10$row_id.y) %>%
        filter(!row_id %in% mc_06_10$row_id.y), 
      multiple_last_names_eligible
    )

    mc_10_14 <- match_mun_data(mc_2010, mc_2014, 
                               multiple_last_names_eligible)

    mc_06_14 <- match_mun_data(
      mc_2006 %>%
        filter(!row_id %in% mc_06_10$row_id.x),
      mc_2014 %>%
        filter(!row_id %in% mc_10_14$row_id.y), 
      multiple_last_names_eligible
    )

    mc_02_14 <- match_mun_data(
      mc_2002 %>%
        filter(!row_id %in% mc_02_06$row_id.x) %>%
        filter(!row_id %in% mc_02_10$row_id.x),
      mc_2014 %>%
        filter(!row_id %in% mc_06_14$row_id.y) %>%
        filter(!row_id %in% mc_10_14$row_id.y), 
      multiple_last_names_eligible
    )

    mc_98_14 <- match_mun_data(
      mc_1998 %>%
        filter(!row_id %in% mc_98_02$row_id.x) %>%
        filter(!row_id %in% mc_98_06$row_id.x) %>%
        filter(!row_id %in% mc_98_10$row_id.x),
      mc_2014 %>%
        filter(!row_id %in% mc_02_14$row_id.y) %>%
        filter(!row_id %in% mc_06_14$row_id.y) %>%
        filter(!row_id %in% mc_10_14$row_id.y), 
      multiple_last_names_eligible
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
        filter(!row_id %in% mc_10_14$row_id.y), 
      multiple_last_names_eligible
    )

    mc_14_18 <- match_mun_data(mc_2014, mc_2018, 
                               multiple_last_names_eligible)

    mc_10_18 <- match_mun_data(
      mc_2010 %>%
        filter(!row_id %in% mc_10_14$row_id.x),
      mc_2018 %>%
        filter(!row_id %in% mc_14_18$row_id.y), 
      multiple_last_names_eligible
    )

    mc_06_18 <- match_mun_data(
      mc_2006 %>%
        filter(!row_id %in% mc_06_10$row_id.x) %>%
        filter(!row_id %in% mc_06_14$row_id.x),
      mc_2018 %>%
        filter(!row_id %in% mc_10_18$row_id.y) %>%
        filter(!row_id %in% mc_14_18$row_id.y), 
      multiple_last_names_eligible
    )

    mc_02_18 <- match_mun_data(
      mc_2002 %>%
        filter(!row_id %in% mc_02_06$row_id.x) %>%
        filter(!row_id %in% mc_02_10$row_id.x) %>%
        filter(!row_id %in% mc_02_14$row_id.x),
      mc_2018 %>%
        filter(!row_id %in% mc_06_18$row_id.y) %>%
        filter(!row_id %in% mc_10_18$row_id.y) %>%
        filter(!row_id %in% mc_14_18$row_id.y), 
      multiple_last_names_eligible
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
        filter(!row_id %in% mc_14_18$row_id.y), 
      multiple_last_names_eligible
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
        filter(!row_id %in% mc_14_18$row_id.y), 
      multiple_last_names_eligible
    )

    mc_18_22 <- match_mun_data(mc_2018, mc_2022,
                               multiple_last_names_eligible)

    mc_14_22 <- match_mun_data(
      mc_2014 %>%
        filter(!row_id %in% mc_14_18$row_id.x),
      mc_2022 %>%
        filter(!row_id %in% mc_18_22$row_id.y), 
      multiple_last_names_eligible
    )

    mc_10_22 <- match_mun_data(
      mc_2010 %>%
        filter(!row_id %in% mc_10_14$row_id.x) %>%
        filter(!row_id %in% mc_10_18$row_id.x),
      mc_2022 %>%
        filter(!row_id %in% mc_14_22$row_id.y) %>%
        filter(!row_id %in% mc_18_22$row_id.y), 
      multiple_last_names_eligible
    )

    mc_06_22 <- match_mun_data(
      mc_2006 %>%
        filter(!row_id %in% mc_06_10$row_id.x) %>%
        filter(!row_id %in% mc_06_14$row_id.x) %>%
        filter(!row_id %in% mc_06_18$row_id.x),
      mc_2022 %>%
        filter(!row_id %in% mc_10_22$row_id.y) %>%
        filter(!row_id %in% mc_14_22$row_id.y) %>%
        filter(!row_id %in% mc_18_22$row_id.y), 
      multiple_last_names_eligible
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
        filter(!row_id %in% mc_18_22$row_id.y), 
      multiple_last_names_eligible
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
        filter(!row_id %in% mc_18_22$row_id.y), 
      multiple_last_names_eligible
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
        filter(!row_id %in% mc_18_22$row_id.y), 
      multiple_last_names_eligible
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
      tidyr::pivot_longer(., cols = 1:(ncol(.)-1), names_to = "election_year",
                          values_to = "row_id") %>%
      filter(!is.na(row_id)) %>%
      mutate(election_year = stringr::str_extract(election_year, "[0-9]{4}"))

    mc_candidates <- bind_rows(
      mc_1994 %>% mutate(election_year = "1994"),
      mc_1998 %>% mutate(election_year = "1998"),
      mc_2002 %>% mutate(election_year = "2002"),
      mc_2006 %>% mutate(election_year = "2006"),
      mc_2010 %>% mutate(election_year = "2010"),
      mc_2014 %>% mutate(election_year = "2014"),
      mc_2018 %>% mutate(election_year = "2018"),
      mc_2022 %>% mutate(election_year = "2022")
    )

    full_join(pivot_table_long, mc_candidates, by = c("row_id", "election_year"))
  }),

  tar_target(
    mc_panel_check,
    stopifnot(nrow(mc_panel) == (
      nrow(mc_1994) + nrow(mc_1998) + nrow(mc_2002) + nrow(mc_2006) +
        nrow(mc_2010) + nrow(mc_2014) + nrow(mc_2018) + nrow(mc_2022)
    ))
  ),
  
  tar_target(
    mc_panel_rds, 
    saveRDS(mc_panel, "output/election_specific/cd_candidates_panel.rds")
  ),
  
  tar_target(
    mc_panel_csv, 
    write.csv(mc_panel, "output/election_specific/cd_candidates_panel.csv", 
              row.names = FALSE)
  ),
  
  ## Municipalities ---------------------------------------
  tar_target(m_1994, filter_municipalities(mun_1994, district_municipalities_map) %>%
               mutate(candidate_surname = gsub('"o', "ö", candidate_surname) %>%
                        gsub('o"', "ö", .) %>%
                        gsub('u"', "ü", .)
               ) %>% 
               mutate(election_type = factor("Municipal", 
                                             levels = c("Municipal", "City district",
                                                        "Regional", "Chamber of Deputies",
                                                        "Senate", "European Parliament")))),
  tar_target(m_1998, filter_municipalities(mun_1998, district_municipalities_map) %>% 
               mutate(election_type = factor("Municipal", 
                                             levels = c("Municipal", "City district",
                                                        "Regional", "Chamber of Deputies",
                                                        "Senate", "European Parliament")))),
  tar_target(m_2002, filter_municipalities(mun_2002, district_municipalities_map) %>% 
               mutate(election_type = factor("Municipal", 
                                             levels = c("Municipal", "City district",
                                                        "Regional", "Chamber of Deputies",
                                                        "Senate", "European Parliament")))),
  tar_target(m_2006, filter_municipalities(mun_2006, district_municipalities_map) %>% 
               mutate(election_type = factor("Municipal", 
                                             levels = c("Municipal", "City district",
                                                        "Regional", "Chamber of Deputies",
                                                        "Senate", "European Parliament")))),
  tar_target(m_2010, filter_municipalities(mun_2010, district_municipalities_map) %>% 
               mutate(election_type = factor("Municipal", 
                                             levels = c("Municipal", "City district",
                                                        "Regional", "Chamber of Deputies",
                                                        "Senate", "European Parliament")))),
  tar_target(m_2014, filter_municipalities(mun_2014, district_municipalities_map) %>% 
               mutate(election_type = factor("Municipal", 
                                             levels = c("Municipal", "City district",
                                                        "Regional", "Chamber of Deputies",
                                                        "Senate", "European Parliament")))),
  tar_target(m_2018, filter_municipalities(mun_2018, district_municipalities_map) %>% 
               mutate(election_type = factor("Municipal", 
                                             levels = c("Municipal", "City district",
                                                        "Regional", "Chamber of Deputies",
                                                        "Senate", "European Parliament")))),
  tar_target(m_2022, filter_municipalities(mun_2022, district_municipalities_map) %>% 
               mutate(election_type = factor("Municipal", 
                                             levels = c("Municipal", "City district",
                                                        "Regional", "Chamber of Deputies",
                                                        "Senate", "European Parliament")))),
  
  tar_target(
    m_panel, {
      m_94_98 <- match_mun_data(m_1994, m_1998, 
                                multiple_last_names_eligible)

      m_98_02 <- match_mun_data(m_1998, m_2002, 
                                multiple_last_names_eligible)

      m_94_02 <- match_mun_data(
        m_1994 %>%
          filter(!row_id %in% m_94_98$row_id.x),
        m_2002 %>%
          filter(!row_id %in% m_98_02$row_id.y), 
        multiple_last_names_eligible
      )

      m_02_06 <- match_mun_data(m_2002, m_2006, 
                                multiple_last_names_eligible)

      m_98_06 <- match_mun_data(
        m_1998 %>%
          filter(!row_id %in% m_98_02$row_id.x),
        m_2006 %>%
          filter(!row_id %in% m_02_06$row_id.y), 
        multiple_last_names_eligible
      )

      m_94_06 <- match_mun_data(
        m_1994 %>%
          filter(!row_id %in% m_94_98$row_id.x) %>%
          filter(!row_id %in% m_94_02$row_id.x),
        m_2006 %>%
          filter(!row_id %in% m_98_06$row_id.y) %>%
          filter(!row_id %in% m_02_06$row_id.y), 
        multiple_last_names_eligible
      )

      m_06_10 <- match_mun_data(m_2006, m_2010, 
                                multiple_last_names_eligible)

      m_02_10 <- match_mun_data(
        m_2002 %>%
          filter(!row_id %in% m_02_06$row_id.x),
        m_2010 %>%
          filter(!row_id %in% m_06_10$row_id.y), 
        multiple_last_names_eligible
      )

      m_98_10 <- match_mun_data(
        m_1998 %>%
          filter(!row_id %in% m_98_02$row_id.x) %>%
          filter(!row_id %in% m_98_06$row_id.x),
        m_2010 %>%
          filter(!row_id %in% m_02_10$row_id.y) %>%
          filter(!row_id %in% m_06_10$row_id.y), 
        multiple_last_names_eligible
      )

      m_94_10 <- match_mun_data(
        m_1994 %>%
          filter(!row_id %in% m_94_98$row_id.x) %>%
          filter(!row_id %in% m_94_02$row_id.x) %>%
          filter(!row_id %in% m_94_06$row_id.x),
        m_2010 %>%
          filter(!row_id %in% m_98_10$row_id.y) %>%
          filter(!row_id %in% m_02_10$row_id.y) %>%
          filter(!row_id %in% m_06_10$row_id.y), 
        multiple_last_names_eligible
      )

      m_10_14 <- match_mun_data(m_2010, m_2014, 
                                multiple_last_names_eligible)

      m_06_14 <- match_mun_data(
        m_2006 %>%
          filter(!row_id %in% m_06_10$row_id.x),
        m_2014 %>%
          filter(!row_id %in% m_10_14$row_id.y),
        multiple_last_names_eligible
      )

      m_02_14 <- match_mun_data(
        m_2002 %>%
          filter(!row_id %in% m_02_06$row_id.x) %>%
          filter(!row_id %in% m_02_10$row_id.x),
        m_2014 %>%
          filter(!row_id %in% m_06_14$row_id.y) %>%
          filter(!row_id %in% m_10_14$row_id.y), 
        multiple_last_names_eligible
      )

      m_98_14 <- match_mun_data(
        m_1998 %>%
          filter(!row_id %in% m_98_02$row_id.x) %>%
          filter(!row_id %in% m_98_06$row_id.x) %>%
          filter(!row_id %in% m_98_10$row_id.x),
        m_2014 %>%
          filter(!row_id %in% m_02_14$row_id.y) %>%
          filter(!row_id %in% m_06_14$row_id.y) %>%
          filter(!row_id %in% m_10_14$row_id.y), 
        multiple_last_names_eligible
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
          filter(!row_id %in% m_10_14$row_id.y), 
        multiple_last_names_eligible
      )

      m_14_18 <- match_mun_data(m_2014, m_2018, 
                                multiple_last_names_eligible)

      m_10_18 <- match_mun_data(
        m_2010 %>%
          filter(!row_id %in% m_10_14$row_id.x),
        m_2018 %>%
          filter(!row_id %in% m_14_18$row_id.y), 
        multiple_last_names_eligible
      )

      m_06_18 <- match_mun_data(
        m_2006 %>%
          filter(!row_id %in% m_06_10$row_id.x) %>%
          filter(!row_id %in% m_06_14$row_id.x),
        m_2018 %>%
          filter(!row_id %in% m_10_18$row_id.y) %>%
          filter(!row_id %in% m_14_18$row_id.y), 
        multiple_last_names_eligible
      )

      m_02_18 <- match_mun_data(
        m_2002 %>%
          filter(!row_id %in% m_02_06$row_id.x) %>%
          filter(!row_id %in% m_02_10$row_id.x) %>%
          filter(!row_id %in% m_02_14$row_id.x),
        m_2018 %>%
          filter(!row_id %in% m_06_18$row_id.y) %>%
          filter(!row_id %in% m_10_18$row_id.y) %>%
          filter(!row_id %in% m_14_18$row_id.y), 
        multiple_last_names_eligible
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
          filter(!row_id %in% m_14_18$row_id.y), 
        multiple_last_names_eligible
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
          filter(!row_id %in% m_14_18$row_id.y),
        multiple_last_names_eligible
      )

      m_18_22 <- match_mun_data(m_2018, m_2022, 
                                multiple_last_names_eligible)

      m_14_22 <- match_mun_data(
        m_2014 %>%
          filter(!row_id %in% m_14_18$row_id.x),
        m_2022 %>%
          filter(!row_id %in% m_18_22$row_id.y), 
        multiple_last_names_eligible
      )

      m_10_22 <- match_mun_data(
        m_2010 %>%
          filter(!row_id %in% m_10_14$row_id.x) %>%
          filter(!row_id %in% m_10_18$row_id.x),
        m_2022 %>%
          filter(!row_id %in% m_14_22$row_id.y) %>%
          filter(!row_id %in% m_18_22$row_id.y), 
        multiple_last_names_eligible
      )

      m_06_22 <- match_mun_data(
        m_2006 %>%
          filter(!row_id %in% m_06_10$row_id.x) %>%
          filter(!row_id %in% m_06_14$row_id.x) %>%
          filter(!row_id %in% m_06_18$row_id.x),
        m_2022 %>%
          filter(!row_id %in% m_10_22$row_id.y) %>%
          filter(!row_id %in% m_14_22$row_id.y) %>%
          filter(!row_id %in% m_18_22$row_id.y), 
        multiple_last_names_eligible
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
          filter(!row_id %in% m_18_22$row_id.y), 
        multiple_last_names_eligible
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
          filter(!row_id %in% m_18_22$row_id.y), 
        multiple_last_names_eligible
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
          filter(!row_id %in% m_18_22$row_id.y), 
        multiple_last_names_eligible
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
        tidyr::pivot_longer(., cols = 1:(ncol(.)-1), names_to = "election_year",
                            values_to = "row_id") %>%
        filter(!is.na(row_id)) %>%
        mutate(election_year = stringr::str_extract(election_year, "[0-9]{4}"))

      m_candidates <- bind_rows(
        m_1994 %>% mutate(election_year = "1994"),
        m_1998 %>% mutate(election_year = "1998"),
        m_2002 %>% mutate(election_year = "2002"),
        m_2006 %>% mutate(election_year = "2006"),
        m_2010 %>% mutate(election_year = "2010"),
        m_2014 %>% mutate(election_year = "2014"),
        m_2018 %>% mutate(election_year = "2018"),
        m_2022 %>% mutate(election_year = "2022")
      )

      full_join(pivot_table_long, m_candidates, by = c("row_id", "election_year"))
    }
  ),

  tar_target(
    m_panel_check,
    stopifnot(nrow(m_panel) == (
      nrow(m_1994) + nrow(m_1998) + nrow(m_2002) + nrow(m_2006) +
        nrow(m_2010) + nrow(m_2014) + nrow(m_2018) + nrow(m_2022)
    ))
  ), 
  
  tar_target(
    m_panel_rds, 
    saveRDS(m_panel, "output/election_specific/m_candidates_panel.rds")
  ),
  
  tar_target(
    m_panel_csv, 
    write.csv(m_panel, "output/election_specific/m_candidates_panel.csv", 
              row.names = FALSE)
  )
)

# EP elections --------------------------------------------
ep_data <- list(
  tar_target(ep_2004, command = {
    ep_vysledky <- read_html(here("data", "EP2004", "vysledky.xml")) %>% 
      html_node("cr") %>% 
      html_nodes("hlasy_strana") %>% 
      parse_ep_hlasy_strana()
    
    parties <- read_parties_xml(here("data", "EP2004", "EP2004reg", "eprkl.xml"), 
                                function(x) x %>% select(KSTRANA = ESTRANA, ZKRATKAK8 = ZKRATKAE8, NAZEV_STRK = NAZEV_STRE))
    cpp <- read_cpp_xml(here("data", "EP2004", "EP2004ciselniky", "cpp.xml"))
    cns <- read_cns_xml(here("data", "EP2004", "EP2004ciselniky", "cns.xml"))
    read_candidates_xml(here("data", "EP2004", "EP2004reg", "eprk.xml"), 
                        parties, cpp, cns, function(x) {x %>% rename(KSTRANA = ESTRANA)}) %>% 
      mutate(ROK_NAROZENI = 2004 - VEK) %>% 
      categorize_sex(., unique_first_names) %>% 
      select(-c(POCPROC)) %>% 
      rename_variables() %>% 
      left_join(., ep_vysledky, by = "candidate_partyrun_code", 
                relationship = "many-to-one") %>% 
      mutate(election_type = factor("European Parliament", 
                                    levels = c("Municipal", "City district",
                                               "Regional", "Chamber of Deputies",
                                               "Senate", "European Parliament")), 
             election_date = as.Date("2004-06-11", format = "%Y-%m-%d"), 
             candidate_validity = as.numeric(candidate_validity))
  }),
  
  tar_target(ep_2009, command = {
    ep_vysledky <- read_html(here("data", "EP2009", "vysledky.xml")) %>% 
      html_node("cr") %>% 
      html_nodes("hlasy_strana") %>% 
      parse_ep_hlasy_strana()
    
    parties <- read_parties_xml(here("data", "EP2009", "EP2009reg", "eprkl.xml"), 
                                function(x) x %>% select(KSTRANA = ESTRANA, ZKRATKAK8 = ZKRATKAE8, NAZEV_STRK = NAZEV_STRE))
    cpp <- read_cpp_xml(here("data", "EP2009", "EP2009ciselniky", "cpp.xml"))
    cns <- read_cns_xml(here("data", "EP2009", "EP2009ciselniky", "cns.xml"))
    read_candidates_xml(here("data", "EP2009", "EP2009reg", "eprk.xml"), 
                        parties, cpp, cns, function(x) {x %>% rename(KSTRANA = ESTRANA)}) %>% 
      mutate(ROK_NAROZENI = 2009 - VEK) %>% 
      categorize_sex(., unique_first_names) %>% 
      filter(JMENO != "Registrační úřad ponechal pozici volnou") %>% 
      select(-c(POCPROC)) %>% 
      rename_variables() %>% 
      left_join(., ep_vysledky, by = "candidate_partyrun_code", 
                relationship = "many-to-one") %>% 
      mutate(election_type = factor("European Parliament", 
                                    levels = c("Municipal", "City district",
                                               "Regional", "Chamber of Deputies",
                                               "Senate", "European Parliament")), 
             election_date = as.Date("2009-06-05", format = "%Y-%m-%d"), 
             candidate_validity = as.numeric(candidate_validity))
  }),
  
  tar_target(ep_2014, command = {
    ep_vysledky <- read_html(here("data", "EP2014", "vysledky.xml")) %>% 
      html_node("cr") %>% 
      html_nodes("hlasy_strana") %>% 
      parse_ep_hlasy_strana()
    
    parties <- read_parties_xml(here("data", "EP2014", "EP2014reg20140525", "eprkl.xml"), 
                                function(x) x %>% select(KSTRANA = ESTRANA, ZKRATKAK8 = ZKRATKAE8, NAZEV_STRK = NAZEV_STRE))
    cpp <- read_cpp_xml(here("data", "EP2014", "EP2014ciselniky20140425", "cpp.xml"))
    cns <- read_cns_xml(here("data", "EP2014", "EP2014ciselniky20140425", "cns.xml"))
    read_candidates_xml(here("data", "EP2014", "EP2014reg20140525", "eprk.xml"), 
                        parties, cpp, cns, function(x) {x %>% rename(KSTRANA = ESTRANA)}) %>% 
      mutate(ROK_NAROZENI = 2014 - VEK) %>% 
      categorize_sex(., unique_first_names) %>% 
      filter(JMENO != "Registrační úřad ponechal pozici volnou") %>% 
      select(-c(POCPROC)) %>% 
      rename_variables() %>% 
      left_join(., ep_vysledky, by = "candidate_partyrun_code", 
                relationship = "many-to-one") %>% 
      mutate(election_type = factor("European Parliament", 
                                    levels = c("Municipal", "City district",
                                               "Regional", "Chamber of Deputies",
                                               "Senate", "European Parliament")), 
             election_date = as.Date("2014-05-23", format = "%Y-%m-%d"), 
             candidate_validity = as.numeric(candidate_validity))
  }),
  
  tar_target(ep_2019, command = {
    ep_vysledky <- read_html(here("data", "EP2019", "vysledky.xml")) %>% 
      html_node("cr") %>% 
      html_nodes("hlasy_strana") %>% 
      parse_ep_hlasy_strana()
    
    parties <- read_parties(here("data", "EP2019", "EP2019reg20190526_xlsx", "eprkl.xlsx"), 
                            function(x) x %>% select(KSTRANA = ESTRANA, ZKRATKAK8 = ZKRATKAE8, NAZEV_STRK = NAZEV_STRE))
    cpp <- read_cpp(here("data", "EP2019", "EP2019ciselniky20190513", "cpp.xlsx"))
    cns <- read_cns(here("data", "EP2019", "EP2019ciselniky20190513", "cns.xlsx"))
    read_candidates(here("data", "EP2019", "EP2019reg20190526_xlsx", "eprk.xlsx"), 
                    parties, cpp, cns, function(x) {x %>% rename(KSTRANA = ESTRANA) %>%
                        mutate(MANDAT = ifelse(MANDAT == "A", 1, 0))}) %>% 
      mutate(ROK_NAROZENI = 2019 - VEK) %>% 
      select(-c(DATNAR, POHLAVI)) %>% 
      categorize_sex(., unique_first_names) %>% 
      rename_variables() %>% 
      left_join(., ep_vysledky, by = "candidate_partyrun_code", 
                relationship = "many-to-one") %>% 
      mutate(election_type = factor("European Parliament", 
                                    levels = c("Municipal", "City district",
                                               "Regional", "Chamber of Deputies",
                                               "Senate", "European Parliament")), 
             election_date = as.Date("2019-05-24", format = "%Y-%m-%d"), 
             candidate_validity = as.numeric(candidate_validity == "N"))
  }), 
  
  tar_target(ep_2024, command = {
    ep_vysledky <- read_html(here("data", "EP2024", "vysledky.xml")) %>% 
      html_node("cr") %>% 
      html_nodes("hlasy_strana") %>% 
      parse_ep_hlasy_strana()
    
    parties <- read_parties(here("data", "EP2024", "EP2024reg20240609_xlsx", "eprkl.xlsx"), 
                            function(x) x %>% select(KSTRANA = ESTRANA, ZKRATKAK8 = ZKRATKAE8, NAZEV_STRK = NAZEV_STRE))
    cpp <- read_cpp(here("data", "EP2024", "EP2024ciselniky20240609", "cpp.xlsx"))
    cns <- read_cns(here("data", "EP2024", "EP2024ciselniky20240609", "cns.xlsx"))
    read_candidates(here("data", "EP2024", "EP2024reg20240609_xlsx", "eprk.xlsx"), 
                    parties, cpp, cns, function(x) {x %>% rename(KSTRANA = ESTRANA) %>%
                        mutate(MANDAT = ifelse(MANDAT == "A", 1, 0))}) %>% 
      mutate(ROK_NAROZENI = 2024 - VEK) %>% 
      select(-c(POHLAVI)) %>% 
      categorize_sex(., unique_first_names) %>% 
      rename_variables() %>% 
      left_join(., ep_vysledky, by = "candidate_partyrun_code", 
                relationship = "many-to-one") %>% 
      mutate(election_type = factor("European Parliament", 
                                    levels = c("Municipal", "City district",
                                               "Regional", "Chamber of Deputies",
                                               "Senate", "European Parliament")), 
             election_date = as.Date("2024-06-07", format = "%Y-%m-%d"), 
             candidate_validity = as.numeric(candidate_validity == "N"))
  }),
  
  tar_target(ep_panel, command = {
    ep04_09 <- match_data(ep_2004, ep_2009, 
                          multiple_last_names_eligible)
    ep09_14 <- match_data(ep_2009, ep_2014, 
                          multiple_last_names_eligible)
    
    ep04_14 <- match_data(
      ep_2004 %>% 
        filter(!row_id %in% ep04_09$row_id.x),
      ep_2014 %>% 
        filter(!row_id %in% ep09_14$row_id.y), 
      multiple_last_names_eligible
    )
    
    ep14_19 <- match_data(ep_2014, ep_2019, 
                          multiple_last_names_eligible)
    ep09_19 <- match_data(
      ep_2009 %>% 
        filter(!row_id %in% ep09_14$row_id.x),
      ep_2019 %>% 
        filter(!row_id %in% ep14_19$row_id.y), 
      multiple_last_names_eligible
    )
    ep04_19 <- match_data(
      ep_2004 %>% 
        filter(!row_id %in% ep04_09$row_id.x) %>% 
        filter(!row_id %in% ep04_14$row_id.x),
      ep_2019 %>% 
        filter(!row_id %in% ep14_19$row_id.y) %>% 
        filter(!row_id %in% ep09_19$row_id.y), 
      multiple_last_names_eligible
    )
    
    ep19_24 <- match_data(ep_2019, ep_2024, 
                          multiple_last_names_eligible) 
    ep14_24 <- match_data(
      ep_2014 %>% 
        filter(!row_id %in% ep14_19$row_id.x),
      ep_2024 %>% 
        filter(!row_id %in% ep19_24$row_id.y), 
      multiple_last_names_eligible
    )
    ep09_24 <- match_data(
      ep_2009 %>% 
        filter(!row_id %in% ep09_14$row_id.x) %>% 
        filter(!row_id %in% ep09_19$row_id.x),
      ep_2024 %>% 
        filter(!row_id %in% ep19_24$row_id.y) %>% 
        filter(!row_id %in% ep14_24$row_id.y), 
      multiple_last_names_eligible
    )
    ep04_24 <- match_data(
      ep_2004 %>% 
        filter(!row_id %in% ep04_09$row_id.x) %>% 
        filter(!row_id %in% ep04_14$row_id.x) %>% 
        filter(!row_id %in% ep04_19$row_id.x),
      ep_2024 %>% 
        filter(!row_id %in% ep19_24$row_id.y) %>% 
        filter(!row_id %in% ep14_24$row_id.y) %>% 
        filter(!row_id %in% ep09_24$row_id.y), 
      multiple_last_names_eligible
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
      tidyr::pivot_longer(., cols = 1:(ncol(.)-1), names_to = "election_year",
                          values_to = "row_id") %>%
      filter(!is.na(row_id)) %>%
      mutate(election_year = stringr::str_extract(election_year, "[0-9]{4}"))

    ep_candidates <- bind_rows(
      ep_2004 %>% mutate(election_year = "2004"),
      ep_2009 %>% mutate(election_year = "2009"),
      ep_2014 %>% mutate(election_year = "2014"),
      ep_2019 %>% mutate(election_year = "2019"),
      ep_2024 %>% mutate(election_year = "2024")
    )
    
    full_join(pivot_table_long, ep_candidates, by = c("row_id", "election_year"))
  }),
  
  tar_target(ep_panel_check, 
      stopifnot(nrow(ep_panel) == (
        nrow(ep_2004) + nrow(ep_2009) + nrow(ep_2014) + nrow(ep_2019) + nrow(ep_2024) 
      ))
  ), 
  
  tar_target(
    ep_panel_rds, 
    saveRDS(ep_panel, "output/election_specific/ep_candidates_panel.rds")
  ),
  
  tar_target(
    ep_panel_csv, 
    write.csv(ep_panel, "output/election_specific/ep_candidates_panel.csv", 
              row.names = FALSE)
  )
)

# Senate elections ----------------------------------------
senate_data <- list(
  tar_target(
    senate_df, {
      candidates <- read_excel("data/SE1996_2022/serk.xlsx") %>% 
        filter(!is.na(PRIJMENI))
      
      cpp <- read_cpp("data/SE1996_2022/SENAT_cisel_20230227_xlsx/cpp.xlsx")
      cns <- read_cns("data/SE1996_2022/SENAT_cisel_20230227_xlsx/cns.xlsx")
      
      left_join(candidates, cpp, by = "PSTRANA") %>% 
        left_join(., cns, by = "NSTRANA") %>% 
        mutate(
          year = floor(DATUMVOLEB/10000),
          ROK_NAROZENI = year - VEK
        ) %>% 
        mutate(
          TITULZA = case_when(
            grepl("\\sCSc\\.", PRIJMENI) ~ "CSc.", 
            grepl("\\sCSc$", PRIJMENI) ~ "CSc.",
            TRUE ~ TITULZA
          ), 
          PRIJMENI = gsub("\\sCSc\\.", "", PRIJMENI) %>% 
            if_else(. == "Štěpán CSc", "Štěpán", .) %>% 
            if_else(. == "Coolidge, rozená Hašková", "Coolidge (Hašková)", .)
        ) %>% 
        merge_and_recode_titles() %>% 
        categorize_sex(., unique_first_names) %>% 
        mutate(MANDAT = as.numeric(ZVOLEN_K1 == 1 | ZVOLEN_K2 == 1)) %>% 
        select(-c(VSTRANA, PROC_K1, PROC_K2, LOS_K1, LOS_K2, ZVOLEN_K1, 
                  ZVOLEN_K2)) %>% 
        rename_variables() %>% 
        mutate(election_type = factor("Senate", 
                                      levels = c("Municipal", "City district",
                                                 "Regional", "Chamber of Deputies",
                                                 "Senate", "European Parliament")), 
               election_date = as.Date(as.character(election_date), "%Y%m%d"))
    }
  ), 
  
  tar_target(
    byelection_dates, {
      election_dates <- read_excel("data/SE1996_2022/SENAT_cisel_20230227_xlsx/sedatumvoleb.xlsx")
      
      election_dates %>% 
        filter(!is.na(POPIS_CZ)) %>% 
        mutate(DATUMVOLEB = as.Date(as.character(DATUMVOLEB), "%Y%m%d")) %>% 
        pull(DATUMVOLEB) 
    }
  ), 
  
  tar_target(
    # check that multiple by-election were not hold in the same Senate district
    byelection_districts_unique, {
      stopifnot(senate_df %>% 
        filter(election_date %in% byelection_dates) %>% 
        select(election_date, senate_district) %>% 
        unique() %>% 
        count(senate_district, sort = TRUE) %>% 
        pull(n) %>% 
        all(. == 1))
    }
  ),
  
  tar_target(
    regular_elections, {
      election_dates <- read_excel("data/SE1996_2022/SENAT_cisel_20230227_xlsx/sedatumvoleb.xlsx")
      
      election_dates %>% 
        filter(is.na(POPIS_CZ)) %>% 
        mutate(DATUMVOLEB = as.Date(as.character(DATUMVOLEB), "%Y%m%d")) %>% 
        pull(DATUMVOLEB)
  }), 
  
  tar_target(
    sen_1996, {
      senate_df %>% 
        filter(election_date == "1996-11-16")
    }
  ),
  
  tar_target(
    sen_1996a, {
      senate_df %>% 
        filter(election_date == "1996-11-16") %>% 
        filter(senate_district %in% sen_1998$senate_district) %>% 
        mutate(row_id = row_number())
    }
  ),
  
  tar_target(
    sen_1996b, {
      senate_df %>% 
        filter(election_date == "1996-11-16") %>% 
        filter(senate_district %in% sen_2000$senate_district) %>% 
        mutate(row_id = row_number())
    }
  ),
  
  tar_target(
    sen_1996c, {
      senate_df %>% 
        filter(election_date == "1996-11-16") %>% 
        filter(senate_district %in% sen_2002$senate_district) %>% 
        mutate(row_id = row_number())
    }
  ),
  
  tar_target(
    sen_1998, {
      senate_df %>% 
        filter(election_date == "1998-11-14") %>% 
        mutate(row_id = row_number())
    }
  ),
  
  tar_target(
    sen_2000, {
      senate_df %>% 
        filter(election_date == "2000-11-12") %>% 
        mutate(row_id = row_number())
    }
  ),
  
  tar_target(
    sen_2002, {
      senate_df %>% 
        filter(election_date == "2002-10-25") %>% 
        mutate(row_id = row_number())
    }
  ),
  
  tar_target(
    sen_2004, {
      senate_df %>% 
        filter(election_date == "2004-11-05") %>% 
        mutate(row_id = row_number())
    }
  ),
  
  tar_target(
    sen_2006, {
      senate_df %>% 
        filter(election_date == "2006-10-20") %>% 
        mutate(row_id = row_number())
    }
  ),
  
  tar_target(
    sen_2008, {
      senate_df %>% 
        filter(election_date == "2008-10-17") %>% 
        mutate(row_id = row_number())
    }
  ),
  
  tar_target(
    sen_2010, {
      senate_df %>% 
        filter(election_date == "2010-10-15") %>% 
        mutate(row_id = row_number())
    }
  ),
  
  tar_target(
    sen_2012, {
      senate_df %>% 
        filter(election_date == "2012-10-12") %>% 
        mutate(row_id = row_number())
    }
  ),
  
  tar_target(
    sen_2014, {
      senate_df %>% 
        filter(election_date == "2014-10-10") %>% 
        mutate(row_id = row_number())
    }
  ),
  
  tar_target(
    sen_2016, {
      senate_df %>% 
        filter(election_date == "2016-10-07") %>% 
        mutate(row_id = row_number())
    }
  ),
  
  tar_target(
    sen_2018, {
      senate_df %>% 
        filter(election_date == "2018-10-05") %>% 
        mutate(row_id = row_number())
    }
  ),
  
  tar_target(
    sen_2020, {
      senate_df %>% 
        filter(election_date == "2020-10-02") %>% 
        mutate(row_id = row_number())
    }
  ),
  
  tar_target(
    sen_2022, {
      senate_df %>% 
        filter(election_date == "2022-09-23") %>% 
        mutate(row_id = row_number())
    }
  ),
  
  tar_target(
    sen_panel, {
      # A districts
      sa_96_98 <- match_sen_data(sen_1996a, sen_1998, 
                                 multiple_last_names_eligible)
      
      sa_98_04 <- match_sen_data(sen_1998, sen_2004, 
                                 multiple_last_names_eligible)
      sa_96_04 <- match_sen_data(
        sen_1996a %>% 
          filter(!row_id %in% sa_96_98$row_id.x), 
        sen_2004 %>% 
          filter(!row_id %in% sa_98_04$row_id.y), 
        multiple_last_names_eligible
      )
      
      sa_04_10 <- match_sen_data(sen_2004, sen_2010, 
                                 multiple_last_names_eligible)
      sa_98_10 <- match_sen_data(
        sen_1998 %>% 
          filter(!row_id %in% sa_98_04$row_id.x), 
        sen_2010 %>% 
          filter(!row_id %in% sa_04_10$row_id.y), 
        multiple_last_names_eligible
      )
      
      sa_96_10 <- match_sen_data(
        sen_1996a %>% 
          filter(!row_id %in% sa_96_98$row_id.x) %>% 
          filter(!row_id %in% sa_96_04$row_id.x), 
        sen_2010 %>% 
          filter(!row_id %in% sa_98_10$row_id.y) %>% 
          filter(!row_id %in% sa_04_10$row_id.y), 
        multiple_last_names_eligible
      )
      
      sa_10_16 <- match_sen_data(sen_2010, sen_2016, 
                                 multiple_last_names_eligible)
      sa_04_16 <- match_sen_data(
        sen_2004 %>% 
          filter(!row_id %in% sa_04_10$row_id.x), 
        sen_2016 %>% 
          filter(!row_id %in% sa_10_16$row_id.y), 
        multiple_last_names_eligible
      )
      
      sa_98_16 <- match_sen_data(
        sen_1998 %>% 
          filter(!row_id %in% sa_98_04$row_id.x) %>% 
          filter(!row_id %in% sa_98_10$row_id.x), 
        sen_2016 %>% 
          filter(!row_id %in% sa_04_16$row_id.y) %>% 
          filter(!row_id %in% sa_10_16$row_id.y), 
        multiple_last_names_eligible
      )
      
      sa_96_16 <- match_sen_data(
        sen_1996a %>% 
          filter(!row_id %in% sa_96_98$row_id.x) %>% 
          filter(!row_id %in% sa_96_04$row_id.x) %>% 
          filter(!row_id %in% sa_96_10$row_id.x), 
        sen_2016 %>% 
          filter(!row_id %in% sa_98_16$row_id.y) %>% 
          filter(!row_id %in% sa_04_16$row_id.y) %>% 
          filter(!row_id %in% sa_10_16$row_id.y), 
        multiple_last_names_eligible
      )
      
      sa_16_22 <- match_sen_data(sen_2016, sen_2022, 
                                 multiple_last_names_eligible)
      sa_10_22 <- match_sen_data(
        sen_2010 %>% 
          filter(!row_id %in% sa_10_16$row_id.x), 
        sen_2022 %>% 
          filter(!row_id %in% sa_16_22$row_id.y), 
        multiple_last_names_eligible
      )
      
      sa_04_22 <- match_sen_data(
        sen_2004 %>% 
          filter(!row_id %in% sa_04_10$row_id.x) %>% 
          filter(!row_id %in% sa_04_16$row_id.x), 
        sen_2022 %>% 
          filter(!row_id %in% sa_10_22$row_id.y) %>% 
          filter(!row_id %in% sa_16_22$row_id.y), 
        multiple_last_names_eligible
      )
      
      sa_98_22 <- match_sen_data(
        sen_1998 %>% 
          filter(!row_id %in% sa_98_04$row_id.x) %>% 
          filter(!row_id %in% sa_98_10$row_id.x) %>% 
          filter(!row_id %in% sa_98_16$row_id.x), 
        sen_2022 %>% 
          filter(!row_id %in% sa_04_22$row_id.y) %>% 
          filter(!row_id %in% sa_10_22$row_id.y) %>% 
          filter(!row_id %in% sa_16_22$row_id.y), 
        multiple_last_names_eligible
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
          filter(!row_id %in% sa_16_22$row_id.y), 
        multiple_last_names_eligible
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
        tidyr::pivot_longer(., cols = 1:(ncol(.)-1), names_to = "election_year",
                            values_to = "row_id") %>%
        filter(!is.na(row_id)) %>%
        mutate(election_year = stringr::str_extract(election_year, "[0-9]{4}"))
      
      sa_candidates <- bind_rows(
        sen_1996a %>% mutate(election_year = "1996"),
        sen_1998 %>% mutate(election_year = "1998"),
        sen_2004 %>% mutate(election_year = "2004"),
        sen_2010 %>% mutate(election_year = "2010"),
        sen_2016 %>% mutate(election_year = "2016"),
        sen_2022 %>% mutate(election_year = "2022")
      )
      
      sen_panel_a <- full_join(pivot_table_long_a, sa_candidates, 
                               by = c("row_id", "election_year"))
      
      ## SB ----------------------------------------------------------------------
      sb_96_00 <- match_sen_data(sen_1996b, sen_2000, 
                                 multiple_last_names_eligible)
      
      sb_00_06 <- match_sen_data(sen_2000, sen_2006, 
                                 multiple_last_names_eligible)
      
      sb_96_06 <- match_sen_data(
        sen_1996b %>% 
          filter(!row_id %in% sb_96_00$row_id.x), 
        sen_2006 %>% 
          filter(!row_id %in% sb_00_06$row_id.y), 
        multiple_last_names_eligible
      )
      
      sb_06_12 <- match_sen_data(sen_2006, sen_2012, 
                                 multiple_last_names_eligible)
      
      sb_00_12 <- match_sen_data(
        sen_2000 %>% 
          filter(!row_id %in% sb_00_06$row_id.x),
        sen_2012 %>% 
          filter(!row_id %in% sb_06_12$row_id.y), 
        multiple_last_names_eligible
      )
      
      sb_96_12 <- match_sen_data(
        sen_1996b %>% 
          filter(!row_id %in% sb_96_00$row_id.x) %>% 
          filter(!row_id %in% sb_96_06$row_id.x),
        sen_2012 %>% 
          filter(!row_id %in% sb_00_12$row_id.y) %>% 
          filter(!row_id %in% sb_06_12$row_id.y), 
        multiple_last_names_eligible
      )
      
      sb_12_18 <- match_sen_data(sen_2012, sen_2018, 
                                 multiple_last_names_eligible)
      
      sb_06_18 <- match_sen_data(
        sen_2006 %>% 
          filter(!row_id %in% sb_06_12$row_id.x),
        sen_2018 %>% 
          filter(!row_id %in% sb_12_18$row_id.y), 
        multiple_last_names_eligible
      )
      
      sb_00_18 <- match_sen_data(
        sen_2000 %>% 
          filter(!row_id %in% sb_00_06$row_id.x) %>% 
          filter(!row_id %in% sb_00_12$row_id.x),
        sen_2018 %>% 
          filter(!row_id %in% sb_06_18$row_id.y) %>% 
          filter(!row_id %in% sb_12_18$row_id.y), 
        multiple_last_names_eligible
      )
      
      sb_96_18 <- match_sen_data(
        sen_1996b %>% 
          filter(!row_id %in% sb_96_00$row_id.x) %>% 
          filter(!row_id %in% sb_96_06$row_id.x) %>% 
          filter(!row_id %in% sb_96_12$row_id.x),
        sen_2018 %>% 
          filter(!row_id %in% sb_00_18$row_id.y) %>% 
          filter(!row_id %in% sb_06_18$row_id.y) %>% 
          filter(!row_id %in% sb_12_18$row_id.y), 
        multiple_last_names_eligible
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
        tidyr::pivot_longer(., cols = 1:(ncol(.)-1), names_to = "election_year",
                            values_to = "row_id") %>%
        filter(!is.na(row_id)) %>%
        mutate(election_year = stringr::str_extract(election_year, "[0-9]{4}"))
      
      sb_candidates <- bind_rows(
        sen_1996b %>% mutate(election_year = "1996"),
        sen_2000 %>% mutate(election_year = "2000"),
        sen_2006 %>% mutate(election_year = "2006"),
        sen_2012 %>% mutate(election_year = "2012"),
        sen_2018 %>% mutate(election_year = "2018")
      )
      
      sen_panel_b <- full_join(pivot_table_long_b, sb_candidates, by = c("row_id", "election_year"))
      
      # SC ----------------------------------------------------------------------
      sc_96_02 <- match_sen_data(sen_1996c, sen_2002, 
                                 multiple_last_names_eligible)
      
      sc_02_08 <- match_sen_data(sen_2002, sen_2008, 
                                 multiple_last_names_eligible)
      
      sc_96_08 <- match_sen_data(
        sen_1996c %>% 
          filter(!row_id %in% sc_96_02$row_id.x),
        sen_2008 %>% 
          filter(!row_id %in% sc_02_08$row_id.y), 
        multiple_last_names_eligible
      )
      
      sc_08_14 <- match_sen_data(sen_2008, sen_2014, 
                                 multiple_last_names_eligible)
      
      sc_02_14 <- match_sen_data(
        sen_2002 %>% 
          filter(!row_id %in% sc_02_08$row_id.x),
        sen_2014 %>% 
          filter(!row_id %in% sc_08_14$row_id.y), 
        multiple_last_names_eligible
      )
      
      sc_96_14 <- match_sen_data(
        sen_1996c %>% 
          filter(!row_id %in% sc_96_02$row_id.x) %>% 
          filter(!row_id %in% sc_96_08$row_id.x),
        sen_2014 %>% 
          filter(!row_id %in% sc_02_14$row_id.y) %>% 
          filter(!row_id %in% sc_08_14$row_id.y), 
        multiple_last_names_eligible
      )
      
      sc_14_20 <- match_sen_data(sen_2014, sen_2020, 
                                 multiple_last_names_eligible)
      
      sc_08_20 <- match_sen_data(
        sen_2008 %>% 
          filter(!row_id %in% sc_08_14$row_id.x),
        sen_2020 %>% 
          filter(!row_id %in% sc_14_20$row_id.y), 
        multiple_last_names_eligible
      )
      
      sc_02_20 <- match_sen_data(
        sen_2002 %>% 
          filter(!row_id %in% sc_02_08$row_id.x) %>% 
          filter(!row_id %in% sc_02_14$row_id.x),
        sen_2020 %>% 
          filter(!row_id %in% sc_08_20$row_id.y) %>% 
          filter(!row_id %in% sc_14_20$row_id.y), 
        multiple_last_names_eligible
      )
      
      sc_96_20 <- match_sen_data(
        sen_1996c %>% 
          filter(!row_id %in% sc_96_02$row_id.x) %>% 
          filter(!row_id %in% sc_96_08$row_id.x) %>% 
          filter(!row_id %in% sc_96_14$row_id.x),
        sen_2020 %>% 
          filter(!row_id %in% sc_02_20$row_id.y) %>% 
          filter(!row_id %in% sc_08_20$row_id.y) %>% 
          filter(!row_id %in% sc_14_20$row_id.y), 
        multiple_last_names_eligible
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
        tidyr::pivot_longer(., cols = 1:(ncol(.)-1), names_to = "election_year",
                            values_to = "row_id") %>%
        filter(!is.na(row_id)) %>%
        mutate(election_year = stringr::str_extract(election_year, "[0-9]{4}"))
      
      sc_candidates <- bind_rows(
        sen_1996c %>% mutate(election_year = "1996"),
        sen_2002 %>% mutate(election_year = "2002"),
        sen_2008 %>% mutate(election_year = "2008"),
        sen_2014 %>% mutate(election_year = "2014"),
        sen_2020 %>% mutate(election_year = "2020")
      )
      
      sen_panel_c <- full_join(pivot_table_long_c, sc_candidates, by = c("row_id", "election_year"))
      
      sen_byelection <- senate_df %>% 
        filter(election_date %in% byelection_dates) %>% 
        mutate(
          row_id = row_number(), 
          person_id = paste0("B", row_id)
        ) %>% 
        mutate(election_year = year(election_date))
      
      sen_a_b <- match_sen_data(sen_panel_a, sen_panel_b, multiple_last_names_eligible, c("candidate_name", "candidate_surname"))
      sen_b_c <- match_sen_data(sen_panel_b, sen_panel_c, multiple_last_names_eligible, c("candidate_name", "candidate_surname"))
      sen_a_c <- match_sen_data(sen_panel_a, sen_panel_c, multiple_last_names_eligible, c("candidate_name", "candidate_surname"))
      
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
      
      sen_byel <- match_sen_data(senate_panel, sen_byelection, multiple_last_names_eligible)
      
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
          mutate(election_year = as.numeric(election_year)) %>% 
          select(-panel_id), 
        sen_byelection %>% 
          mutate(panel = "byel_id")
      )
      
      full_join(sen_byel_pivot_long, sen_all_candidates, by = c("panel_id"="person_id", "panel")) %>% 
        arrange(person_id, election_year) %>% 
        select(-c(panel, panel_id, year)) %>% 
        mutate(candidate_validity = if_else(candidate_validity == "A", 0, 1))
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
                  count(person_id, election_date, sort = TRUE) %>% 
                  pull(n) %>% all(. == 1))
    }
  ),
  
  tar_target(
    sen_panel_rds, 
    saveRDS(sen_panel, "output/election_specific/sen_candidates_panel.rds")
  ),
  
  tar_target(
    sen_panel_csv, 
    write.csv(sen_panel, "output/election_specific/sen_candidates_panel.csv", 
              row.names = FALSE)
  )
)

# Matching panels together --------------------------------
matched_panels <- list(
  ## Městské části - Obce  --------------------------------
  tar_target(
    mc_mun_panel, {
      mc_obec <- read_csv("data/mcast_obec.csv", locale = locale(encoding = "WINDOWS-1250")) %>% 
        select(city_district_id = CHODNOTA1, municipality_id = CHODNOTA2)
      
      m_panel_harm <- m_panel %>% 
        mutate(election = paste0("M", election_year))
      
      mc_panel_harm <- mc_panel %>% 
        rename(city_district_id = municipality_id) %>% 
        left_join(mc_obec, by = "city_district_id") %>% 
        select(-MUNICIPALITY) %>% 
        mutate(election = paste0("MC", election_year))
      
      match_m_mc <- match_mun_panel_data(m_panel_harm, mc_panel_harm, 
                                         multiple_last_names_eligible)
      
      
      mc_mun_unique <- match_m_mc %>% 
        group_by(person_id.x, person_id.y) %>% 
        summarise(score = max(score))
      
      x_multiple <- mc_mun_unique %>% 
        count(person_id.x, sort = TRUE) %>% 
        filter(n > 1)
      
      y_multiple <- mc_mun_unique %>% 
        count(person_id.y, sort = TRUE) %>% 
        filter(n > 1)
      
      stopifnot(mc_mun_unique %>% 
                  select(person_id.x, person_id.y) %>%
                  unique() %>% 
                  filter(person_id.x %in% x_multiple & 
                           person_id.y %in% y_multiple) %>% 
                  nrow() == 0)
      
      x_unique <- mc_mun_unique %>% 
        filter(person_id.x %in% x_multiple$person_id.x) %>% 
        group_by(person_id.x) %>% 
        summarise(person_id.y = list(person_id.y), 
                  score = list(score)) %>% 
        mutate(person_id.x = as.list(person_id.x))
      
      y_unique <- mc_mun_unique %>% 
        filter(person_id.y %in% y_multiple$person_id.y) %>% 
        group_by(person_id.y) %>% 
        summarise(person_id.x = list(person_id.x), 
                  score = list(score)) %>% 
        mutate(person_id.y = as.list(person_id.y))
      
      both_unique <- mc_mun_unique %>%
        filter(!(person_id.y %in% y_multiple$person_id.y |
                   person_id.x %in% x_multiple$person_id.x)) %>%
        mutate(person_id.x = as.list(person_id.x),
               person_id.y = as.list(person_id.y),
               score = as.list(score))
      
      m_volby <- m_panel_harm %>%
        group_by(person_id) %>%
        summarise(election.x = list(election))
      mc_volby <- mc_panel_harm %>%
        group_by(person_id) %>% 
        summarise(election.y = list(election))
      
      tmp <- bind_rows(
        x_unique,
        y_unique,
        both_unique
      ) %>%
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
      
      missing_x <- m_panel_harm %>% 
        filter(!person_id %in% unlist(all_unique$person_id.x)) %>% 
        select(person_id.x = person_id) %>% 
        unique() %>% 
        mutate(person_id.x = as.list(person_id.x))
      
      missing_y <- mc_panel_harm %>% 
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
          m_panel_harm, 
          mc_panel_harm
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
            select(election_year, election, candidate_name, candidate_surname, 
                   candidate_occupation, candidate_birthyear, candidate_partynom_name, 
                   candidate_partymem_name))
        
        ok <- (anyDuplicated(check_df$election) == 0)
        if(ok){
          pivot_table2 <- pivot_table2 %>% 
            group_by(panel_id) %>% 
            mutate(change = any(person_id == i)) %>% 
            ungroup %>% 
            group_by(change) %>% 
            mutate(panel_id = if_else(change, head(panel_id, 1), panel_id))
        }else{
          print(i)
          
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
    mc_mun_check, 
    stopifnot(
      nrow(mc_mun_panel) == (nrow(m_panel) + nrow(mc_panel))
    )
  ),
  
  # tar_target(
  #   mun_panel_file_dta, 
  #   haven::write_dta(mc_mun_panel, "output/municipal_panel.dta")
  # ),
  # 
  # tar_target(
  #   mun_panel_file_rds, 
  #   saveRDS(mc_mun_panel, "output/municipal_panel.rds")
  # ),
  
  ## Panel - Kraje ----------------------------------------
  tar_target(
    mun_reg_panel, {
      
      missing_obce <- tribble(
        ~KODZASTUP, ~region_name,
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
               region_name = TEXT2) %>% 
        bind_rows(., missing_obce)
      
      kraje <- tribble(
        ~KRZAST, ~region_name,
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
        left_join(., mun_kraj, by = c("municipality_id"="KODZASTUP")) %>% 
        select(-person_id) %>% 
        rename(person_id = panel_id)
      reg_panel_harm <- reg_panel %>% 
        left_join(., kraje, by = c("region_code"="KRZAST")) %>% 
        mutate(election = paste0("R", election_year))
      
      mun_reg_match <- match_mun_reg_panel(mun_panel_harm, reg_panel_harm, 
                                           multiple_last_names_eligible)
      
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
      
      pivot_table_final <- deduplicate_panel(pivot_table, all_candidates)
      
      stopifnot(
        pivot_table_final %>% 
          count(person_id) %>% 
          filter(n > 1) %>% 
          nrow() == 0
      )
      
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
        mutate(election = paste("PSP", election_year))

      mr_psp_match <- match_mr_psp_panel(mun_reg_panel_harm, psp_panel_harm, 
                                         multiple_last_names_eligible)

      mr_psp_unique <- mr_psp_match %>%
        group_by(person_id.x, person_id.y) %>%
        summarise(score = max(score))

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

      mr_volby <- mun_reg_panel_harm %>%
        group_by(person_id) %>%
        summarise(election.x = list(election))
      psp_volby <- psp_panel_harm %>%
        group_by(person_id) %>%
        summarise(election.y = list(election))

      tmp <- bind_rows(
        x_unique,
        y_unique,
        both_unique
      ) %>%
        mutate(panel_id = paste0("RP", row_number())) %>%
        unnest(c(person_id.x, person_id.y, score)) %>%
        left_join(., psp_volby, by = c("person_id.y"="person_id")) %>%
        left_join(., mr_volby, by = c("person_id.x"="person_id")) %>%
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

      pivot_table <- all_unique %>%
        bind_rows(., missing_x) %>%
        bind_rows(., missing_y) %>%
        mutate(panel_id = paste0("RP", row_number())) %>%
        unnest(c(person_id.x, person_id.y)) %>%
        pivot_longer(., cols = c(person_id.x, person_id.y), names_to = "dataset",
                     values_to = "person_id") %>%
        select(-dataset) %>%
        unique() %>%
        filter(!is.na(person_id))

      all_candidates <-
        bind_rows(
          mun_reg_panel_harm,
          psp_panel_harm
        )
      
      pivot_table_final <- deduplicate_panel(pivot_table, all_candidates)
      
      ## OLD
      # # Unique matches between persons
      # mr_psp_unique <- mr_psp_match %>%
      #   group_by(person_id.x, person_id.y) %>%
      #   summarise(score = max(score), .groups = "drop")
      # 
      # # List of elections for each person
      # mr_volby <- mun_reg_panel_harm %>%
      #   group_by(person_id) %>%
      #   summarise(election.x = list(election), .groups = "drop")
      # psp_volby <- psp_panel_harm %>%
      #   group_by(person_id) %>%
      #   summarise(election.y = list(election), .groups = "drop")
      # 
      # # Persons with multiple matches
      # x_multiple <- mr_psp_unique %>%
      #   count(person_id.x, sort = TRUE) %>%
      #   filter(n > 1)
      # 
      # y_multiple <- mr_psp_unique %>%
      #   count(person_id.y, sort = TRUE) %>%
      #   filter(n > 1)
      # 
      # stopifnot(mr_psp_unique %>%
      #             select(person_id.x, person_id.y) %>%
      #             unique() %>%
      #             filter(person_id.x %in% x_multiple &
      #                      person_id.y %in% y_multiple) %>%
      #             nrow() == 0)
      # 
      # # Single row for each
      # x_unique <- mr_psp_unique %>%
      #   filter(person_id.x %in% x_multiple$person_id.x) %>%
      #   group_by(person_id.x) %>%
      #   summarise(person_id.y = list(person_id.y),
      #             score = list(score)) %>%
      #   mutate(person_id.x = as.list(person_id.x))
      # 
      # y_unique <- mr_psp_unique %>%
      #   filter(person_id.y %in% y_multiple$person_id.y) %>%
      #   group_by(person_id.y) %>%
      #   summarise(person_id.x = list(person_id.x),
      #             score = list(score)) %>%
      #   mutate(person_id.y = as.list(person_id.y))
      # 
      # both_unique <- mr_psp_unique %>%
      #   filter(!(person_id.y %in% y_multiple$person_id.y |
      #              person_id.x %in% x_multiple$person_id.x)) %>%
      #   mutate(person_id.x = as.list(person_id.x),
      #          person_id.y = as.list(person_id.y),
      #          score = as.list(score))
      # 
      # # Create TMP panel_id
      # tmp <- bind_rows(
      #   x_unique,
      #   y_unique,
      #   both_unique
      # ) %>%
      #   mutate(panel_id = paste0("PRM", row_number())) %>% 
      #   unnest(c(person_id.x, person_id.y, score)) %>%
      #   left_join(., mr_volby, by = c("person_id.x"="person_id")) %>%
      #   left_join(., psp_volby, by = c("person_id.y"="person_id")) %>%
      #   group_by(panel_id, person_id.x) %>%
      #   mutate(n_y = length(unique(person_id.y))) %>%
      #   group_by(panel_id, person_id.y) %>%
      #   mutate(n_x = length(unique(person_id.x))) %>% 
      #   ungroup()
      # 
      # # multiple matches
      # # exclude duplicates based on duplicated elections
      # tmp_y <- tmp %>% filter(n_y > 1)
      # y_keep <- tmp_y %>%
      #   unnest(election.y) %>%
      #   group_by(panel_id) %>%
      #   mutate(
      #     n = n(),
      #     unique_years = length(unique(election.y)),
      #     intersection = n != unique_years,
      #     exclude = intersection & score != max(score)) %>%
      #   filter(!exclude) %>%
      #   ungroup()
      # 
      # tmp_x <- tmp %>% filter(n_x > 1)
      # x_keep <- tmp_x %>%
      #   anti_join(., y_keep, by = c("person_id.x", "person_id.y")) %>%
      #   unnest(election.x) %>%
      #   group_by(panel_id) %>%
      #   mutate(
      #     n = n(),
      #     unique_years = length(unique(election.x)),
      #     intersection = n != unique_years,
      #     exclude = intersection & score != max(score)) %>%
      #   filter(!exclude) %>%
      #   ungroup()
      # 
      # all_unique <- bind_rows(
      #   x_keep %>%
      #     select(-c(election.x, election.y)) %>%
      #     group_by(person_id.x, person_id.y) %>%
      #     summarise(score = max(score)) %>%
      #     ungroup() %>%
      #     group_by(person_id.y) %>%
      #     summarise(person_id.x = list(person_id.x),
      #               score = list(score), .groups = "drop") %>%
      #     mutate(person_id.y = as.list(person_id.y)),
      #   y_keep %>%
      #     select(-c(election.x, election.y)) %>%
      #     group_by(person_id.x, person_id.y) %>%
      #     summarise(score = max(score)) %>%
      #     ungroup() %>%
      #     group_by(person_id.x) %>%
      #     summarise(person_id.y = list(person_id.y),
      #               score = list(score), .groups = "drop") %>%
      #     mutate(person_id.x = as.list(person_id.x)),
      #   tmp %>%
      #     ungroup %>%
      #     filter(n_x == 1, n_y == 1) %>%
      #     select(person_id.x, person_id.y, score) %>%
      #     mutate(across(everything(), as.list))
      # )
      # 
      # # unmatched persons
      # missing_x <- mun_reg_panel_harm %>%
      #   filter(!person_id %in% unlist(all_unique$person_id.x)) %>%
      #   select(person_id.x = person_id) %>%
      #   unique() %>%
      #   mutate(person_id.x = as.list(person_id.x))
      # 
      # missing_y <- psp_panel_harm %>%
      #   filter(!person_id %in% unlist(all_unique$person_id.y)) %>%
      #   select(person_id.y = person_id) %>%
      #   unique() %>%
      #   mutate(person_id.y = as.list(person_id.y))
      # 
      # # pivot table
      # pivot_table <- all_unique %>%
      #   bind_rows(., missing_x) %>%
      #   bind_rows(., missing_y) %>%
      #   mutate(panel_id = paste0("PRM", row_number())) %>%
      #   unnest(c(person_id.x, person_id.y)) %>%
      #   pivot_longer(., cols = c(person_id.x, person_id.y), names_to = "dataset",
      #                values_to = "person_id") %>%
      #   select(-dataset) %>%
      #   unique() %>%
      #   filter(!is.na(person_id)) %>% 
      #   select(panel_id, person_id) %>%
      #   unique()
      # 
      # all_candidates <-
      #   bind_rows(
      #     mun_reg_panel_harm,
      #     psp_panel_harm
      #   )
      # 
      # duplicates <- pivot_table %>% 
      #   count(person_id) %>% 
      #   filter(n > 1)
      # 
      # # FIXME: compare all with all, cluster them by distance
      # pivot_table_final <- pivot_table %>% 
      #   group_by(person_id) %>% 
      #   filter(row_number() == 1) %>% 
      #   ungroup()
      
      stopifnot(
        (pivot_table_final %>% 
           count(person_id) %>% 
           filter(n > 1) %>% 
           nrow()) == 0
      )
      
      # pivot_table_final
      full_join(all_candidates, pivot_table_final, by = "person_id",
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
  
  ## Panel - Senát ----------------------------------------
  tar_target(
    psp_sen_panel, {
      mr_psp_panel_harm <- mr_psp_panel %>% 
        mutate(election_year = as.numeric(election_year)) %>% 
        select(-person_id) %>% 
        rename(person_id = panel_id)
      sen_panel_harm <- sen_panel %>% 
        mutate(candidate_validity = as.numeric(candidate_validity != "A"),
               election = paste0("S", election_year))
      
      psp_sen_match <- match_psp_sen_panel(mr_psp_panel_harm, sen_panel_harm, 
                                           multiple_last_names_eligible)
      
      # Unique matches between persons
      psp_sen_unique <- psp_sen_match %>%
        group_by(person_id.x, person_id.y) %>%
        summarise(score = max(score), .groups = "drop")
      
      # List of elections for each person
      psp_volby <- mr_psp_panel_harm %>%
        group_by(person_id) %>%
        summarise(election.x = list(election), .groups = "drop")
      sen_volby <- sen_panel_harm %>%
        group_by(person_id) %>%
        summarise(election.y = list(election), .groups = "drop")
      
      # Persons with multiple matches
      x_multiple <- psp_sen_unique %>%
        count(person_id.x, sort = TRUE) %>%
        filter(n > 1)
      
      y_multiple <- psp_sen_unique %>%
        count(person_id.y, sort = TRUE) %>%
        filter(n > 1)
      
      stopifnot(psp_sen_unique %>%
                  select(person_id.x, person_id.y) %>%
                  unique() %>%
                  filter(person_id.x %in% x_multiple &
                           person_id.y %in% y_multiple) %>%
                  nrow() == 0)
      
      # Single row for each
      x_unique <- psp_sen_unique %>%
        filter(person_id.x %in% x_multiple$person_id.x) %>%
        group_by(person_id.x) %>%
        summarise(person_id.y = list(person_id.y),
                  score = list(score)) %>%
        mutate(person_id.x = as.list(person_id.x))
      
      y_unique <- psp_sen_unique %>%
        filter(person_id.y %in% y_multiple$person_id.y) %>%
        group_by(person_id.y) %>%
        summarise(person_id.x = list(person_id.x),
                  score = list(score)) %>%
        mutate(person_id.y = as.list(person_id.y))
      
      both_unique <- psp_sen_unique %>%
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
        mutate(panel_id = paste0("PS", row_number())) %>% 
        unnest(c(person_id.x, person_id.y, score)) %>%
        left_join(., psp_volby, by = c("person_id.x"="person_id")) %>%
        left_join(., sen_volby, by = c("person_id.y"="person_id")) %>%
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
      missing_x <- mr_psp_panel_harm %>%
        filter(!person_id %in% unlist(all_unique$person_id.x)) %>%
        select(person_id.x = person_id) %>%
        unique() %>%
        mutate(person_id.x = as.list(person_id.x))
      
      missing_y <- sen_panel_harm %>%
        filter(!person_id %in% unlist(all_unique$person_id.y)) %>%
        select(person_id.y = person_id) %>%
        unique() %>%
        mutate(person_id.y = as.list(person_id.y))
      
      # pivot table
      pivot_table <- all_unique %>%
        bind_rows(., missing_x) %>%
        bind_rows(., missing_y) %>%
        mutate(panel_id = paste0("PS", row_number())) %>%
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
          mr_psp_panel_harm, 
          sen_panel_harm
        )
      
      pivot_table_final <- deduplicate_panel(pivot_table, all_candidates)
      
      stopifnot(pivot_table_final %>% 
        count(person_id) %>% 
        filter(n > 1) %>% 
        nrow() == 0)
      
      # pivot_table_final
      full_join(all_candidates, pivot_table_final, by = "person_id",
                relationship = "many-to-one") %>%
        select(panel_id, person_id, everything())
    }
  ),
  
  tar_target(
    psp_sen_check_nrow, {
      stopifnot(
        nrow(psp_sen_panel) == (nrow(mr_psp_panel) + nrow(sen_panel))
      )
    }
  ),
  
  ## Panel - EP -------------------------------------------
  tar_target(
    complete_panel, {
      psp_sen_panel_harm <- psp_sen_panel %>% 
        select(-person_id) %>% 
        rename(person_id = panel_id)
      
      ep_panel_harm <- ep_panel %>% 
        mutate(election_year = as.numeric(election_year), 
               election = paste0("EP", election_year))
      
      psp_ep_match <- match_psp_sen_panel(psp_sen_panel_harm, ep_panel_harm, 
                                          multiple_last_names_eligible)
      
      ## NEW
      psp_ep_unique <- psp_ep_match %>% 
        group_by(person_id.x, person_id.y) %>% 
        summarise(score = max(score))
      
      x_multiple <- psp_ep_unique %>% 
        count(person_id.x, sort = TRUE) %>% 
        filter(n > 1)
      
      y_multiple <- psp_ep_unique %>% 
        count(person_id.y, sort = TRUE) %>% 
        filter(n > 1)
      
      stopifnot(psp_ep_unique %>% 
                  select(person_id.x, person_id.y) %>%
                  unique() %>% 
                  filter(person_id.x %in% x_multiple & 
                           person_id.y %in% y_multiple) %>% 
                  nrow() == 0)
      
      x_unique <- psp_ep_unique %>% 
        filter(person_id.x %in% x_multiple$person_id.x) %>% 
        group_by(person_id.x) %>% 
        summarise(person_id.y = list(person_id.y), 
                  score = list(score)) %>% 
        mutate(person_id.x = as.list(person_id.x))
      
      y_unique <- psp_ep_unique %>% 
        filter(person_id.y %in% y_multiple$person_id.y) %>% 
        group_by(person_id.y) %>% 
        summarise(person_id.x = list(person_id.x), 
                  score = list(score)) %>% 
        mutate(person_id.y = as.list(person_id.y))
      
      both_unique <- psp_ep_unique %>%
        filter(!(person_id.y %in% y_multiple$person_id.y |
                   person_id.x %in% x_multiple$person_id.x)) %>%
        mutate(person_id.x = as.list(person_id.x),
               person_id.y = as.list(person_id.y),
               score = as.list(score))
      
      psp_volby <- psp_sen_panel_harm %>%
        group_by(person_id) %>%
        summarise(election.x = list(election))
      ep_volby <- ep_panel_harm %>%
        group_by(person_id) %>% 
        summarise(election.y = list(election))
      
      tmp <- bind_rows(
        x_unique,
        y_unique,
        both_unique
      ) %>%
        mutate(panel_id = paste0("RM", row_number())) %>%
        unnest(c(person_id.x, person_id.y, score)) %>%
        left_join(., ep_volby, by = c("person_id.y"="person_id")) %>%
        left_join(., psp_volby, by = c("person_id.x"="person_id")) %>%
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
      
      missing_x <- psp_sen_panel_harm %>% 
        filter(!person_id %in% unlist(all_unique$person_id.x)) %>% 
        select(person_id.x = person_id) %>% 
        unique() %>% 
        mutate(person_id.x = as.list(person_id.x))
      
      missing_y <- ep_panel_harm %>% 
        filter(!person_id %in% unlist(all_unique$person_id.y)) %>% 
        select(person_id.y = person_id) %>% 
        unique() %>% 
        mutate(person_id.y = as.list(person_id.y))
      
      pivot_table <- all_unique %>% 
        bind_rows(., missing_x) %>% 
        bind_rows(., missing_y) %>% 
        mutate(panel_id = paste0("PE", row_number())) %>% 
        unnest(c(person_id.x, person_id.y)) %>% 
        pivot_longer(., cols = c(person_id.x, person_id.y), names_to = "dataset", 
                     values_to = "person_id") %>% 
        select(-dataset) %>% 
        unique() %>% 
        filter(!is.na(person_id))
      
      all_candidates <- 
        bind_rows(
          psp_sen_panel_harm, 
          ep_panel_harm
        )
      
      duplicates <- pivot_table %>% 
        count(person_id, sort = TRUE) %>% 
        filter(n > 1)
      
      pivot_table2 <- pivot_table
      for(i in duplicates$person_id){
        # cat(which(i == duplicates$person_id), "\n")
        check_ids <- pivot_table %>% 
          group_by(panel_id) %>% 
          filter(any(person_id == i))
        (check_df <- all_candidates %>% 
            filter(person_id %in% check_ids$person_id) %>% 
            select(election_year, election, candidate_name, candidate_surname, 
                   candidate_occupation, candidate_birthyear, candidate_partynom_name, 
                   candidate_partymem_name))
        
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
        select(panel_id, person_id, everything()) %>% 
        select(-c(person_id, row_id, municipality, election)) %>% 
        rename(person_id = panel_id)
      
    }
  ), 
  
  tar_target(
    complete_panel_check_nrow, {
      stopifnot(
        nrow(complete_panel) == (nrow(psp_sen_panel) + nrow(ep_panel))
      )
    }
  ), 
  
  tar_target(
    complete_panel_rds, 
    saveRDS(complete_panel, "output/candidates_panel.rds")
  ),
  
  tar_target(
    complete_panel_csv, 
    write.csv(complete_panel, "output/candidates_panel.csv", row.names = FALSE)
  )
)

# Donations data ------------------------------------------
donations <- list(
  tar_target(
    donations_data, 
    readRDS("donation_data/data/finalDataset.rds")
  ), 
  
  tar_target(
    donations_matched, {
      complete_panel_harm <- complete_panel %>% 
        select(
          person_id, 
          name = candidate_name, 
          surname = candidate_surname, 
          birthyear = candidate_birthyear, 
          party = candidate_partynom_name, 
          everything()
        ) %>% 
        mutate(
          party = case_when(
            party == "Trikolóra" ~ "Trikolora",
            TRUE ~ party
          )
        )
      
      donations_data_harm <- donations_data %>% 
        rename(
          name = donor_name, 
          surname = donor_lastname,
          birthyear = donor_birthyear,
          party = donation_party
        ) %>% 
        mutate(
          party = case_when(
            party == "ANO2011" ~ "ANO 2011",
            party == "CSSD" ~ "ČSSD",
            party == "KDU-CSL" ~ "KDU-ČSL",
            party == "KSCM" ~ "KSČM",
            party == "Prisaha" ~ "Přísaha",
            party == "Svobodni" ~ "Svobodní",
            party == "TOP09" ~ "TOP 09",
            party == "ods" ~ "ODS", 
            party == "Pirati" ~ "Piráti",
            TRUE ~ party
          )
        )
      
      matched_donor <- match_donor_data(donations_data_harm, complete_panel_harm, 
                                        c("name", "surname", "party"))
      
      pivot <- matched_donor %>% 
        select(donor_person_id = person_id.x, 
               candidate_person_id = person_id.y) %>% 
        unique()
      
      pivot_unique <- pivot %>% 
        group_by(donor_person_id) %>% 
        mutate(donor_row = row_number()) %>% 
        ungroup %>% 
        group_by(candidate_person_id) %>% 
        mutate(candidate_row = row_number()) %>% 
        ungroup() %>% 
        filter(candidate_row == 1 & donor_row == 1) %>% 
        select(donor_person_id, candidate_person_id)
      
      donations_data_harm %>% 
        left_join(., pivot_unique, by = c("person_id"="donor_person_id"), 
                  relationship = "many-to-one") %>% 
        rename(
          donor_id = person_id,
          donor_name = name,
          donor_surname = surname, 
          donor_birthyear = birthyear,
          donation_party = party, 
          person_id = candidate_person_id
        ) %>% 
        ungroup %>% 
        mutate(
          person_id = if_else(is.na(person_id), 
                              paste0("D", donor_id), 
                              person_id)
        )
    }
  ), 
  
  tar_target(
    donations_matched_rds, 
    saveRDS(donations_matched, "output/donation_data.rds")
  ),
  
  tar_target(
    donations_matched_csv, 
    write.csv(donations_matched, "output/donation_data.csv", row.names = FALSE)
  )
)

# Validations ---------------------------------------------
validations <- list(
  tar_target(
    pref_votes_within_range, {
      pref_votes <- complete_panel %>% 
        filter(election_type != "Senate") %>% 
        pull(candidate_voteP)
      
      stopifnot(all(pref_votes >= 0 & pref_votes <= 100))
    }
  ),
  
  tar_target(
    abs_pref_votes_nonnegative, {
      pref_votes <- complete_panel %>% 
        filter(candidate_validity == 0) %>% 
        filter(election_type != "Senate") %>% 
        pull(candidate_voteN)
      stopifnot(all(pref_votes >= 0))
    }
  ),
  
  tar_target(
    abs_round1_votes_nonnegative, {
      round1_votes <- complete_panel %>% 
        filter(election_type == "Senate") %>% 
        pull(candidate_voteN_SR1) 
      stopifnot(all(round1_votes >= 0))
    }
  ),
  
  tar_target(
    pct_round1_votes_nonnegative, {
      round1_votes <- complete_panel %>% 
        filter(election_type == "Senate") %>% 
        pull(candidate_voteP_SR1) 
      stopifnot(all(round1_votes >= 0))
    }
  ),
  
  tar_target(
    party_abs_vote_not_missing, {
      stopifnot(complete_panel %>% 
        filter(election_type != "Senate") %>% 
        pull(party_voteN) %>% 
        is.na() %>% 
        sum() == 0)
    }
  ),
  
  # tar_target(
  #   party_pct_vote_not_missing, {
  #     stopifnot(complete_panel %>% 
  #                 filter(election_type != "Senate") %>% 
  #                 pull(party_voteP) %>% 
  #                 is.na() %>% 
  #                 sum() == 0)
  #   }
  # ),
  
  # tar_target(
  #   invalid_candidates_zero_votes, {
  #     pref_votes <- complete_panel %>%
  #       filter(election_type != "Senate") %>% 
  #       filter(candidate_validity == 1) %>% 
  #       pull(candidate_voteN)
  #     
  #     stopifnot(all(pref_votes == 0))
  #   }
  # ),
  
  tar_target(
    id_not_missing, {
      stopifnot(sum(is.na(complete_panel$person_id)) == 0)
    }
  ),
  
  tar_target(
    name_not_missing, {
      stopifnot(sum(is.na(complete_panel$candidate_name)) == 0)
    }
  ),
  
  tar_target(
    surname_not_missing, {
      stopifnot(sum(is.na(complete_panel$candidate_surname)) == 0)
    }
  ),
  
  tar_target(
    gender_not_missing, {
      stopifnot(sum(is.na(complete_panel$candidate_gender)) == 0)
    }
  ),
  
  tar_target(
    education_not_missing, {
      stopifnot(sum(is.na(complete_panel$candidate_education)) == 0)
    }
  ),
  
  tar_target(
    age_not_missing, {
      stopifnot(complete_panel %>% 
        filter(candidate_validity == 0) %>% 
        pull(candidate_age) %>% 
        is.na() %>% 
        sum() == 0)
    }
  ),
  
  tar_target(
    seat_not_missing, {
      stopifnot(sum(is.na(complete_panel$candidate_seat)) == 0)
    }
  ),
  
  tar_target(
    partymem_code_not_missing, {
      stopifnot(sum(is.na(complete_panel$candidate_partymem_code)) == 0)
    }
  ),
  
  tar_target(
    validity_not_missing, {
      stopifnot(sum(is.na(complete_panel$candidate_validity)) == 0)
    }
  ),
  
  tar_target(
    ranking_not_missing, {
      stopifnot(complete_panel %>% 
        filter(election_type != "Senate") %>% 
        pull(candidate_ranking) %>% 
        is.na() %>% 
        sum() == 0)
    }
  ),
  
  tar_target(
    partynom_code_not_missing, {
      stopifnot(complete_panel %>% 
                  filter(!(election_type == "Chamber of Deputies" &
                             election_year %in% c(1996, 1998))) %>% 
                  pull(candidate_partynom_code) %>% 
                  is.na() %>% 
                  sum() == 0)
    }
  ),
  
  tar_target(
    invalid_candidates, {
      invalid_cands_votes <- complete_panel %>% 
        filter(candidate_validity == 1) %>% 
        pull(candidate_voteN)
    }
  ),
  
  tar_target(clea_mandates, {
    # Subset of CLEA data containing Czech elections
    cze <- readRDS(here("validation_data", "clea_lc_20240419_r", "cze.rds")) %>% 
      filter(yr >= 1996) %>% 
      filter(can == "-990") %>% 
      select(yr, cst, cst_n, pty, pty_n, seat) %>% 
      filter(seat > 0) %>% 
      mutate(region_name = case_when(
        cst_n %in% c("praha kraj", "hlavní mesto praha", "Praha",
                     "Hlavni mesto Praha") ~ "Hlavní město Praha", 
        cst_n %in% c("stredo-ceský kraj", "Stredocesky") ~ "Středočeský kraj", 
        cst_n %in% c("jiho-ceský kraj", "Jihocesky") ~ "Jihočeský kraj", 
        cst_n %in% c("západo-ceský kraj") ~ "Západočeský kraj", 
        cst_n %in% c("severo-ceský") ~ "Severočeský kraj", 
        cst_n %in% c("východo-ceský kraj") ~ "Východočeský kraj", 
        cst_n %in% c("jiho-moravský kraj", "Jihomoravsky") ~ "Jihomoravský kraj", 
        cst_n %in% c("severo-moravský kraj") ~ "Severomoravský kraj", 
        cst_n %in% c("plzenský kraj", "Plzensky") ~ "Plzeňský kraj", 
        cst_n %in% c("karlovarský kraj", "Karlovarsky") ~ "Karlovarský kraj", 
        cst_n %in% c("Ústecký kraj", "Ustecky") ~ "Ústecký kraj", 
        cst_n %in% c("liberecký kraj", "Liberecky") ~ "Liberecký kraj", 
        cst_n %in% c("královéhradecký kraj", "Kralovehradecky") ~ "Královéhradecký kraj", 
        cst_n %in% c("pardubický kraj", "Pardubicky") ~ "Pardubický kraj", 
        cst_n %in% c("kraj vysocina", "Vysocina") ~ "Kraj Vysočina", 
        cst_n %in% c("olomoucký kraj", "Olomoucky") ~ "Olomoucký kraj", 
        cst_n %in% c("zlínský kraj", "Zlinsky") ~ "Zlínský kraj", 
        cst_n %in% c("moravskoslezský kraj", "Moravskoslezsky") ~ "Moravskoslezský kraj", 
      ), 
      candidate_partyrun_name = case_when(
        pty_n == "komunistická strana cech a moravy"  ~ "KSČM",
        pty_n == "sdružení pro republiku-republikánská (i)" ~ "SPR-RSČ",
        pty_n == "obcanská demokratická aliance"  ~ "ODA",
        pty_n == "ceskoslovenská sociální demokracie" ~ "ČSSD",
        pty_n == "krestanská a demokratická unie - ceskoslovenská strana lidová" ~ "KDU-ČSL",
        pty_n == "obcanská demokratická strana" ~ "ODS",
        pty_n == "unie svobody" ~ "US",
        pty_n == "koalice ku-csl, us-deu" ~ "Koalice",
        pty_n == "strana zelených"  ~ "SZ",
        pty_n == "TOP 09" ~ "TOP 09",
        pty_n == "VV" ~ "VV",
        pty_n == "ANO 2011" ~ "ANO 2011",
        pty_n == "Usvit" ~ "Úsvit"
      ))
    
    psp_seats <- psp_panel %>% 
      group_by(election_year, electoral_region, region_name, 
               candidate_partyrun_fullname, candidate_partyrun_name) %>% 
      summarise(seats = sum(candidate_seat), .groups = "drop") %>% 
      filter(seats > 0) %>% 
      mutate(election_year = as.numeric(election_year)) %>% 
      filter(election_year <= 2013) %>% 
      mutate(
        candidate_partyrun_name = case_when(
          candidate_partyrun_fullname == "Komunistická str.Čech a Moravy" ~ "KSČM",
          candidate_partyrun_fullname == "Křesť.a dem.unie-Čs.str.lid." ~ "KDU-ČSL",
          candidate_partyrun_fullname == "Občanská demokratická aliance" ~ "ODA",
          candidate_partyrun_fullname == "Občanská demokratická strana" ~ "ODS",
          candidate_partyrun_fullname == "Sdruž.pro rep.-Republ.str.Čsl." ~ "SPR-RSČ",
          candidate_partyrun_fullname == "Česká str.sociál.demokratická" ~ "ČSSD",
          candidate_partyrun_fullname == "Křesť.demokr.unie-Čs.str.lid." ~ "KDU-ČSL",
          candidate_partyrun_fullname == "Unie svobody" ~ "US",
          candidate_partyrun_fullname == "Česká str.sociálně demokrat." ~ "ČSSD",
          candidate_partyrun_fullname == "Koalice KDU-ČSL, US-DEU" ~ "Koalice",
          candidate_partyrun_fullname == "Komunistická strana Čech a Moravy" ~ "KSČM",
          candidate_partyrun_fullname == "Česká strana sociálně demokratická" ~ "ČSSD", 
          TRUE ~ candidate_partyrun_name
        )
      )
    
    stopifnot(all(psp_seats$candidate_partyrun_name %in% cze$candidate_partyrun_name))
    stopifnot(all(cze$candidate_partyrun_name %in% psp_seats$candidate_partyrun_name))
    stopifnot(all(psp_seats$region_name %in% cze$region_name))
    stopifnot(all(cze$region_name %in% psp_seats$region_name))
    
    clea_cpcd_data <- full_join(
      cze %>% select(year = yr, region_name, candidate_partyrun_name, seats_clea = seat), 
      psp_seats %>% select(year = election_year, region_name, candidate_partyrun_name, seats_cpcd = seats),
      by = c("year", "region_name", "candidate_partyrun_name"), 
      relationship = "one-to-one"
    ) %>% 
      mutate(across(where(is.numeric), ~if_else(is.na(.x), 0, .x)))
    
    clea_cpcd_data %>% 
      filter(seats_clea != seats_cpcd)
  }),
  
  tar_target(sikk_koker_wcn, {
    novelty_data <- read.csv("validation_data/sikk_koker/slate_novelty_dropout.csv") %>% 
      filter(country == "CZ") %>% 
      filter(year >= 1996)
    
    complete_panel2 <- complete_panel %>% 
      filter(election_type == "Chamber of Deputies") %>% 
      mutate(election = paste0("PSP ", election_year))
    
    # 2013
    new_candidates_2013 <- complete_panel2 %>% 
      find_new_candidates(., "PSP 2010", "PSP 2013")
    
    w_new_candidates_2013 <- new_candidates_2013 %>% 
      calculate_rankings()
    
    wcn_2013 <- w_new_candidates_2013 %>% 
      group_by(candidate_partyrun_fullname) %>% 
      summarise(wcn = Hmisc::wtd.mean(new_candidate, w = w)) %>% 
      arrange(wcn)
    
    wcn_2013_comparison <- novelty_data %>% 
      filter(year == 2013) %>% 
      mutate(candidate_partyrun_fullname = case_when(
        slate == "ANEO" ~ "Aktiv nezávislých občanů",
        slate == "ANO 2011" ~ "ANO 2011",
        slate == "CSSD" ~ "Česká strana sociálně demokratická",
        slate == "DSSS" ~ "Dělnická strana sociální spravedlnosti",
        slate == "HLVZHURU" ~ "HLAVU VZHŮRU - volební blok",
        slate == "KAN" ~ "Klub angažovaných nestraníků",
        slate == "KC" ~ "Koruna Česká (monarch.str.Čech, Moravy a Slezska)",
        slate == "KDU-CSL" ~ "Křesťan.a demokrat.unie-Českosl.strana lidová",
        slate == "KSCM" ~ "Komunistická strana Čech a Moravy",
        slate == "LEV 21" ~ "LEV 21 - Národní socialisté",
        slate == "OBC_2011" ~ "OBČANÉ 2011",
        slate == "ODS" ~ "Občanská demokratická strana",
        slate == "PB" ~ "Volte Pravý Blok www.cibulka.net",
        slate == "Pirati" ~ "Česká pirátská strana",
        slate == "RDS" ~ "Romská demokratická strana",
        slate == "SPOZ" ~ "Strana Práv Občanů ZEMANOVCI",
        slate == "SZ" ~ "Strana zelených",
        slate == "SsCR" ~ "Strana soukromníků České republiky",
        slate == "Suveren." ~ "Suverenita-Strana zdravého rozumu",
        slate == "Svobodni" ~ "Strana svobodných občanů",
        slate == "TOP 09" ~ "TOP 09",
        slate == "Usvit" ~ "Úsvit přímé demokracie Tomia Okamury",
        slate == "Zmena" ~ "politické hnutí Změna"
      )) %>% 
      rename(wcn_sk = wcn) %>% 
      left_join(., wcn_2013 %>% rename(wcn_cpcd = wcn))
    
    # plot(wcn_2013_comparison$wcn_sk, wcn_2013_comparison$wcn_cpcd)
    cor(wcn_2013_comparison$wcn_sk, wcn_2013_comparison$wcn_cpcd)
    
    # 2010
    new_candidates_2010 <- complete_panel2 %>% 
      find_new_candidates(., "PSP 2006", "PSP 2010")
    
    w_new_candidates_2010 <- new_candidates_2010 %>% 
      calculate_rankings()
    
    wcn_2010 <- w_new_candidates_2010 %>% 
      group_by(candidate_partyrun_fullname) %>% 
      summarise(wcn = Hmisc::wtd.mean(new_candidate, w = w)) %>% 
      arrange(wcn) %>% 
      rename(wcn_cpcd = wcn)
    
    wcn_2010_comparison <- novelty_data %>% 
      filter(year == 2010) %>% 
      mutate(candidate_partyrun_fullname = case_when(
        slate == "CPS" ~ "Česká pirátská strana",
        slate == "CSNS" ~ "Česká strana národně sociální",
        slate == "CSNS2005" ~ "Česká strana národně socialistická",
        slate == "CSSD" ~ "Česká strana sociálně demokratická",
        slate == "DSSS" ~ "Dělnická strana sociální spravedlnosti",
        slate == "ES" ~ "EVROPSKÝ STŘED",
        slate == "HS" ~ "Humanistická strana",
        slate == "KC" ~ "Koruna Česká (monarch.str.Čech, Moravy a Slezska)",
        slate == "KDU-CSL" ~ "Křesťan.a demokrat.unie-Českosl.strana lidová",
        slate == "KH" ~ "Klíčové hnutí",
        slate == "KONS" ~ "Konzervativní strana",
        slate == "KSCM" ~ "Komunistická strana Čech a Moravy",
        slate == "LIB" ~ "Liberálové.CZ",
        slate == "Moravane" ~ "Moravané",
        slate == "ODS" ~ "Občanská demokratická strana",
        slate == "Obcane" ~ "OBČANÉ.CZ",
        slate == "PB" ~ "Volte Pravý Blok www.cibulka.net",
        slate == "SPOZ" ~ "Strana Práv Občanů ZEMANOVCI",
        slate == "SPR-RSC" ~ "Sdružení pro republiku-Republikán.strana Českosl.",
        slate == "STOP" ~ "STOP",
        slate == "SZ" ~ "Strana zelených",
        slate == "Suveren." ~ "Suverenita-blok J.Bobošíkové,str.zdravého rozumu",
        slate == "Svobodni" ~ "Strana svobodných občanů",
        slate == "TOP 09" ~ "TOP 09",
        slate == "VV" ~ "Věci veřejné"
      )) %>% 
      rename(wcn_sk = wcn) %>% 
      left_join(., wcn_2010)
    
    # plot(wcn_2010_comparison$wcn_sk, wcn_2010_comparison$wcn_cpcd)
    cor(wcn_2010_comparison$wcn_sk, wcn_2010_comparison$wcn_cpcd)
    
    # 2006
    new_candidates_2006 <- complete_panel2 %>% 
      find_new_candidates(., "PSP 2002", "PSP 2006")
    
    w_new_candidates_2006 <- new_candidates_2006 %>% 
      calculate_rankings()
    
    wcn_2006 <- w_new_candidates_2006 %>% 
      group_by(candidate_partyrun_fullname) %>% 
      summarise(wcn = Hmisc::wtd.mean(new_candidate, w = w)) %>% 
      arrange(wcn) %>% 
      rename(wcn_cpcd = wcn)
    
    wcn_2006_comparison <- novelty_data %>% 
      filter(year == 2006) %>% 
      mutate(candidate_partyrun_fullname = case_when(
        slate == "4 VIZE" ~ "4 VIZE-www.4vize.cz",
        slate == "BPS" ~ "Balbínova poetická strana",
        slate == "CHNJ" ~ "České hnutí za národní jednotu",
        slate == "CSNS2006" ~ "Česká strana národně socialistická",
        slate == "CSSD" ~ "Česká strana sociálně demokratická",
        slate == "FiS" ~ "Folklor i Společnost",
        slate == "HOB" ~ "Helax-Ostrava se baví",
        slate == "HS" ~ "Humanistická strana",
        slate == "KC" ~ "Koruna Česká (monar.strana Čech, Moravy a Slezska)",
        slate == "KDU-CSL" ~ "Křesťan.a demokrat.unie-Českosl.strana lidová",
        slate == "KSCM" ~ "Komunistická strana Čech a Moravy",
        slate == "Koal_CR" ~ "Koalice pro Českou republiku",
        slate == "LiRA" ~ "Liberální reformní strana" ,
        slate == "M" ~ "Moravané",
        slate == "NEZ" ~ "NEZÁVISLÍ",
        slate == "NEZ/DEM" ~ "NEZÁVISLÍ DEMOKRATÉ (předseda V.Železný)",
        slate == "NS" ~ "Národní strana",
        slate == "ODS" ~ "Občanská demokratická strana",
        slate == "PB" ~ "Pravý Blok-str.za ODVOLATELNOST politiků,REFERENDA",
        slate == "PaS" ~ "Právo a Spravedlnost-ANO rodině,NE korupci a krim.",
        slate == "SNK ED" ~ "SNK Evropští demokraté" ,
        slate == "SRS" ~ "STRANA ROVNOST ŠANCÍ",
        slate == "SZ" ~ "Strana zelených",
        slate == "SZR" ~ "Strana zdravého rozumu" ,
        slate == "US-DEU" ~ "Unie svobody - Demokratická unie"
      )) %>% 
      rename(wcn_sk = wcn) %>% 
      left_join(., wcn_2006)
    
    # plot(wcn_2006_comparison$wcn_sk, wcn_2006_comparison$wcn_cpcd)
    cor(wcn_2006_comparison$wcn_sk, wcn_2006_comparison$wcn_cpcd)
    
    # 2002
    new_candidates_2002 <- complete_panel2 %>% 
      find_new_candidates(., "PSP 1998", "PSP 2002")
    
    w_new_candidates_2002 <- new_candidates_2002 %>% 
      calculate_rankings()
    
    wcn_2002 <- w_new_candidates_2002 %>% 
      group_by(candidate_partyrun_fullname) %>% 
      summarise(wcn = Hmisc::wtd.mean(new_candidate, w = w)) %>% 
      arrange(wcn) %>% 
      rename(wcn_cpcd = wcn)
    
    wcn_2002_comparison <- novelty_data %>% 
      filter(year == 2002) %>% 
      mutate(candidate_partyrun_fullname = case_when(
        slate == "AZSD" ~ "Akce za zrušení Senátu a proti vytun.důchod.fondů",
        slate == "BPS" ~ "Balbínova poetická strana",
        slate == "CP" ~ "Česká pravice",
        slate == "CSDH" ~ "České sociálně demokratické hnutí",
        slate == "CSNS" ~ "Česká strana národně sociální",
        slate == "CSSD" ~ "Česká strana sociálně demokratická",
        slate == "CZ" ~ "Cesta změny",
        slate == "DL" ~ "Demokratická liga",
        slate == "H.A." ~ "Humanistická aliance",
        slate == "KDU-CSL" ~ "Koalice KDU-ČSL, US-DEU",
        slate == "KSCM" ~ "Komunistická strana Čech a Moravy",
        slate == "MoDS" ~ "Moravská demokratická strana",
        slate == "N" ~ "Naděje",
        slate == "NDS" ~ "Národně demokratická strana",
        slate == "NH" ~ "Nové hnutí",
        slate == "ODA" ~ "Občanská demokratická aliance",
        slate == "ODS" ~ "Občanská demokratická strana",
        slate == "PB" ~ "Pravý Blok",
        slate == "REP" ~ "Republikáni",
        slate == "RMS" ~ "Republikáni Miroslava Sládka",
        slate == "ROI" ~ "Romská občanská iniciativa ČR",
        slate == "SDS" ~ "Strana demokratického socialismu",
        slate == "SNK" ~ "Sdružení nezávislých",
        slate == "SV SOS" ~ "Strana venkova - spojené občanské síly",
        slate == "SZ" ~ "Strana zelených",
        slate == "SZJ" ~ "Strana za životní jistoty",
        slate == "SZR" ~ "Strana zdravého rozumu",
        slate == "VPB" ~ "Volba pro budoucnost"
      )) %>% 
      rename(wcn_sk = wcn) %>% 
      left_join(., wcn_2002)
    
    # plot(wcn_2002_comparison$wcn_sk, wcn_2002_comparison$wcn_cpcd)
    cor(wcn_2002_comparison$wcn_sk, wcn_2002_comparison$wcn_cpcd)
    
    # 1998
    new_candidates_1998 <- complete_panel2 %>% 
      find_new_candidates(., "PSP 1996", "PSP 1998")
    
    w_new_candidates_1998 <- new_candidates_1998 %>% 
      calculate_rankings()
    
    wcn_1998 <- w_new_candidates_1998 %>% 
      group_by(candidate_partyrun_fullname) %>% 
      summarise(wcn = Hmisc::wtd.mean(new_candidate, w = w)) %>% 
      arrange(wcn) %>% 
      rename(wcn_cpcd = wcn)
    
    wcn_1998_comparison <- novelty_data %>% 
      filter(year == 1998) %>% 
      mutate(candidate_partyrun_fullname = case_when(
        slate == "A2001" ~ "Alternativa 2000",
        slate == "CAO" ~ "Celostátní aktiv občanů",
        slate == "CSNS" ~ "Česká strana národně sociální",
        slate == "CSSD" ~ "Česká str.sociálně demokrat.",
        slate == "DEU" ~ "Demokratická unie",
        slate == "DZJ" ~ "Důchodci za životní jistoty",
        slate == "KDU-CSL" ~ "Křesť.demokr.unie-Čs.str.lid.",
        slate == "KSCM" ~ "Komunistická str.Čech a Moravy",
        slate == "MODS" ~ "Moravská demokratická strana",
        slate == "NEZ" ~ "Nezávislí",
        slate == "ODA" ~ "Občanská demokratická aliance",
        slate == "ODS" ~ "Občanská demokratická strana",
        slate == "OK" ~ "Občanská koalice-Politic.klub",
        slate == "PB" ~ "Pravý blok",
        slate == "SDCR" ~ "Sdružení důchodců ČR",
        slate == "SPR-RSC" ~ "Sdruž.pro rep.-Republ.str.Čsl.",
        slate == "SZ" ~ "Strana zelených",
        slate == "US" ~ "Unie svobody"
      )) %>% 
      rename(wcn_sk = wcn) %>% 
      left_join(., wcn_1998)
    
    bind_rows(
      wcn_1998_comparison,
      wcn_2002_comparison,
      wcn_2006_comparison,
      wcn_2010_comparison,
      wcn_2013_comparison
    ) %>% 
      select(year, candidate_partyrun_fullname, slate, wcn_sk, wcn_cpcd)
  }), 
  
  tar_target(sikk_koker_wcn_cor_tab, {
    
    tibble(
      years = c(1998, 2002, 2006, 2010, 2013),
      r = c(
        round(correlate_year(sikk_koker_wcn, 1998), 2),
        round(correlate_year(sikk_koker_wcn, 2002), 2),
        round(correlate_year(sikk_koker_wcn, 2006), 2),
        round(correlate_year(sikk_koker_wcn, 2010), 2),
        round(correlate_year(sikk_koker_wcn, 2013), 2)
      ), 
      rmse = c(
        round(rmse_year(sikk_koker_wcn, 1998), 2),
        round(rmse_year(sikk_koker_wcn, 2002), 2),
        round(rmse_year(sikk_koker_wcn, 2006), 2),
        round(rmse_year(sikk_koker_wcn, 2010), 2),
        round(rmse_year(sikk_koker_wcn, 2013), 2)
      )
    )
  }), 
  
  tar_target(sikk_koker_wcn_tab_tex, {
    knitr::kable(sikk_koker_wcn %>% select(-candidate_partyrun_fullname), 
                 col.names = c("Year", "Party", "WCN (Sikk & Köker)", 
                               "WCN (CPCD)"), 
                 digits = 2,
                 format = "latex") %>% 
      writeLines(., "figs/wcn_tab.tex")
  }),
  
  tar_target(sikk_koker_wcn_cor_tab_tex, {
    knitr::kable(sikk_koker_wcn_cor_tab, 
                 col.names = c("Election year", "r", "RMSE"), 
                 format = "latex") %>% 
      writeLines(., "figs/wcn_validation.tex")
  }), 
  
  tar_target(comepelda_validation, {
    da <- read.csv(file="validation_data/comepelda/COMEPELDA_aggregate_v1.00.csv",
                   header=TRUE, encoding = "UTF-8", na.strings=".")
    dc <- read.csv(file="validation_data/comepelda/COMEPELDA_candidates_v1.00.csv",
                   header=TRUE, encoding = "UTF-8", na.strings=".") %>% 
      filter(cName == "Czech Republic") %>% 
      mutate(PrefVot = as.numeric(PrefVot))
    
    cz_comepelda <- merge(dc,
                          da[,c("IDAD","pName")],
                          by = "IDAD",
                          all.x=TRUE) %>% 
      select(ElYear, pName, Name, ListRankSel, PrefVot, Elected01) %>% 
      arrange(ElYear, pName, ListRankSel) %>% 
      mutate(pName2 = case_when(
        pName == "Komunistická strana Cech a Moravy" ~ "Komunistická strana Čech a Moravy",
        pName == "Křesťanská a demokratická unie – Československá strana lidová" ~ "Křesťan.a demokrat.unie-Českosl.strana lidová",
        pName == "Nezávislí" ~ "NEZÁVISLÍ",
        pName == "Občanská demokratická strana" ~ "Občanská demokratická strana",
        pName == "SNK Evropští demokraté" & ElYear == 2009 ~ "SNK Evropští demokraté",
        pName == "SNK Evropští demokraté" & ElYear == 2004 ~ "SNK sdružení nezávislých a Evropští demokraté",
        pName == "Unie svobody " ~ "Unie liberálních demokratů",
        pName == "Česká strana sociálně demokratická" ~ "Česká strana sociálně demokratická",
        pName == "Evropská demokratická strana" ~ "Evropská demokratická strana",
        pName == "Libertas.cz" ~ "Libertas.cz",
        pName ==  "Starostové a nezávislí" ~ "\"STAROSTOVÉ A NEZÁVISLÍ - VAŠE ALTERNATIVA\"",
        pName ==  "Suverenita – blok Jany Bobošíkové" ~ "Suverenita",
        pName ==  "Akce nespokojených občanů" ~ "ANO 2011",
        pName ==  "Strana svobodných občanů" ~ "Strana svobodných občanů",
        pName ==  "Tradice Odpovědnost Prosperita 09" ~ "Koalice TOP 09 a STAN"
      ))
    
    ep_data <- complete_panel %>% 
      filter(election_type == "European Parliament") %>% 
      filter(election_year %in% cz_comepelda$ElYear)
    
    n_candidates <- left_join(
      cz_comepelda %>% count(ElYear, pName2) %>% 
        rename(n_comepelda = n),
      ep_data %>% count(election_year, candidate_partyrun_fullname) %>% 
        rename(n_cpcd = n), 
      by = c("ElYear"="election_year", "pName2"="candidate_partyrun_fullname")
    )
    
    stopifnot(all(n_candidates$n_comepelda == n_candidates$n_cpcd))
    
    matched_candidates <- left_join(
      cz_comepelda %>% select(ElYear, pName2, Name, ListRankSel, PrefVot, Elected01),
      ep_data %>% select(election_year, candidate_partyrun_fullname, candidate_ranking, 
                         candidate_name, candidate_surname, candidate_voteN, 
                         candidate_seat, candidate_validity), 
      by = c("ElYear"="election_year", "pName2"="candidate_partyrun_fullname", 
             "ListRankSel"="candidate_ranking")
    ) %>% filter(candidate_validity == 0)
    
    stopifnot(all(matched_candidates$PrefVot == matched_candidates$candidate_voteN))  
    stopifnot(all(matched_candidates$Elected01 == matched_candidates$candidate_seat))  
      
  })
)

# Summary stats -------------------------------------------
summary_stats <- list(
  tar_target(n_candidates, {
    bind_rows(
      psp_panel %>% 
        mutate(election_year = paste0("PS ", election_year)) %>% 
        filter(candidate_validity == 0) %>% 
        summarise_election(),
      reg_panel %>% 
        filter(candidate_validity == 0) %>% 
        mutate(election_year = paste0("Reg. ", election_year)) %>% 
        summarise_election(),
      ep_panel %>% 
        filter(candidate_validity == 0) %>% 
        mutate(election_year = paste0("EP ", election_year)) %>% 
        summarise_election(), 
      m_panel %>% 
        filter(candidate_validity == 0) %>% 
        mutate(election_year = paste0("Municipal ", election_year)) %>% 
        summarise_election(), 
      mc_panel %>% 
        filter(candidate_validity == 0) %>% 
        mutate(election_year = paste0("City districts ", election_year)) %>% 
        summarise_election,
      sen_panel %>% 
        filter(candidate_validity == 0) %>% 
        mutate(election_year = case_when(
          election_date %in% c(19961116, 19981114, 
                               20001112, 20021025,
                               20041105, 20061020, 
                               20081017, 20101015,
                               20121012, 20141010, 
                               20161007, 20181005, 
                               20201002, 20220923) ~ paste0("Senate Election ", election_year), 
          TRUE ~ "Senate By-elections"
        )) %>% 
        summarise_election()
    )
  }),
  
  tar_target(n_candidates_xlsx, {
    writexl::write_xlsx(n_candidates, "figs/n_candidates.xlsx")
  }), 
  
  tar_target(n_unique_candidates, {
    length(unique(complete_panel$person_id))
  }),
  
  tar_target(n_unique_candidates_valid, {
    complete_panel %>% 
      filter(candidate_validity == 0) %>% 
      pull(person_id) %>% unique() %>% 
      length()
  }),
  
  tar_target(n_candidates_tex, {
    knitr::kable(n_candidates, col.names = c("Election", "N of candidates", 
                                             "Female candidates", 
                                             "Elected candidates", 
                                             "Elected female candidates", 
                                             "% elected"), 
                 format = "latex") %>% 
      writeLines(., "figs/n_candidates.tex")
  })
)

# All targets ---------------------------------------------
list(
  psp_data,
  reg_data,
  mun_data, 
  ep_data,
  senate_data,
  all_data,
  matched_panels,
  summary_stats,
  donations, 
  validations
)

