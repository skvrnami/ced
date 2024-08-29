pass <- function(x){x}

#' Clean colnames in Excel files exported from DBF files
#' 
#' colnames in files exported from .dbf have format 'variable,variable_type,number(,number)'
#' this function removes all text after the first comma
clean_dbf_excel_colnames <- function(df){
  n <- colnames(df)
  colnames(df) <- stringr::str_remove(n, ",[A-Z0-9,]+")
  df
}

categorize_titles <- function(x){
  x <- tolower(x)
  x <- dplyr::case_when(grepl("\\bprof\\b", x) ~ "Professor", # prof.
                        grepl("\\bdoc\\b", x) ~ "Associate Professor (docent)", # doc.
                        grepl("([a-z]+dr|ph\\.+d|phd|th\\.d|csc|drsc|dr)\\b", x) ~ "Doctor",
                        # https://cs.wikipedia.org/wiki/Promovan%C3%BD_pr%C3%A1vn%C3%ADk
                        grepl("\\b(ma|m[a-z]{2}|ing|prom|ak|akad)\\b", x) ~ "Master", #Mgr, MgA, MA, promovaní, akad.arch atd.
                        grepl("\\b(bc|ba|bsc)\\b", x) ~ "Bachelor", #Bc, BcA, BA, BSc
                        TRUE ~ "No title")
  factor(x, levels = c("No title", "Bachelor", "Master",
                       "Doctor", "Associate Professor (docent)", "Professor"),
         ordered = TRUE)
}

extract_titles_from_last_name <- function(x){
  REPLACEMENT_VECTOR <- c(
    "\\bDoc\\sIng\\b"="Doc. Ing.",
    "\\bDocent\\sCSc"="Doc. CSc.",
    "\\bProf\\sIng\\sDrSc\\b"="Prof. Ing. DrSc.",
    "\\bDocPhDrCSc\\b"="Doc. PhDr. CSc.",
    "\\bDocMUDrDrSc\\b"="Doc. MUDr. DrSc.",
    "\\bTh Lic P."="ThLic. P.",
    "\\bIng\\b"="Ing\\.", 
    "\\bing\\b"="Ing\\.",
    "\\bMUDr\\b"="MUDr\\.",
    "\\bMudr\\b"="MUDr.",
    "\\bPharmDr\\b"="PharmDr.",
    "\\bPhMr\\b"="PhMr.",
    "\\bMgr\\b"="Mgr.",
    "\\bMUC\\b"="MUC.",
    "\\bPharm\\sDr."="PharmDr.",
    "\\bJUDr\\b"="JUDr.",
    "\\bPhDr\\b"="PhDr.",
    "\\bMVDr\\b"="MVDr.",
    "\\bDr\\b"="Dr.",
    "\\bMGr\\b"="Mgr.",
    "\\bRnDr\\b"="RNDr.",
    "\\bRNDr\\b"="RNDr.",
    "\\bPaedDr\\b"="PaedDr.",
    "\\bPeadr\\b"="PaedDr.",
    "\\bRSDr\\b"="RSDr.", 
    "\\bRsDr\\b"="RSDr.",
    "\\bPaeDr\\b"="PaeDr.",
    "\\bBc\\b"="Bc.",
    "\\bTh,Mgr\\b"="ThMgr",
    "\\bRN Mgr\\b"="Mgr",
    "\\(ml\\.\\)"="ml.",
    "\\bml\\b"="ml.",
    "\\bst\\b"="st.",
    "\\bdpt\\b"=""
  )
  x %>%
    mutate(PRIJMENI = stringr::str_replace_all(PRIJMENI, REPLACEMENT_VECTOR)) %>%
    mutate(TITULY = paste(stringr::str_extract_all(PRIJMENI, "\\s[A-Za-z]+\\.[A-Ža-ž. ]*|,[^,]+"), 
                          sep = " "), 
           PRIJMENI = stringr::str_trim(
             stringr::str_remove_all(PRIJMENI, "\\s[A-Za-z]+\\.[A-Ža-ž. ]*|,[^,]+"))) %>%
    mutate(PRIJMENI = stringr::str_trim(gsub("\\.", "", PRIJMENI)), 
           TITULY = stringr::str_trim(gsub("\\.\\.", "\\.", TITULY))) %>%
    mutate(TITULY = ifelse(TITULY == "character(0)", NA_character_, TITULY), 
           TITUL_KATEGORIE = categorize_titles(TITULY))
}

extract_titles_from_first_name <- function(df){
  df %>% 
    mutate(
      TITULY_PRED = case_when(
        grepl("Ing\\.JUDr\\.", JMENO) ~ "Ing. JUDr.",
        grepl("Doc\\.Dr\\.", JMENO) ~ "Doc. Dr.",
        grepl("Doc\\.ing\\.", JMENO) ~ "Doc. Ing.",
        grepl("Mudr\\.", JMENO) ~ "MUDr.",
        grepl("MVDr\\.", JMENO) ~ "MVDr.",
        grepl("MUDr\\." , JMENO) ~ "MUDr.",
        grepl("Mgr\\."  , JMENO) ~ "Mgr.",
        grepl("RNDr\\." , JMENO) ~ "RNDr.",
        grepl("ThMgr\\."  , JMENO) ~ "ThMgr.",
        grepl("PharmDr\\.", JMENO) ~ "PharmDr.",
        grepl("JUDr\\." , JMENO) ~ "JUDr.",
        grepl("PhDr\\." , JMENO) ~ "PhDr.",
        grepl("Paed\\sDr\\."  , JMENO) ~ "PaedDr.",
        grepl("ing\\.", JMENO) ~ "Ing.",
        grepl("Dr\\." , JMENO) ~ "Dr.",
        grepl("PaedDr\\.", JMENO) ~ "PaedDr.",
        grepl("Ing\\.", JMENO) ~ "Ing.", 
        grepl("Ing\\s", JMENO) ~ "Ing."
      ), 
      JMENO = gsub(
        "^(Ing\\.JUDr\\.|Doc\\.Dr\\.|Doc\\.ing\\.|Mudr\\.|MVDr\\.|MUDr\\.|Mgr\\.|RNDr\\.|ThMgr\\.|PharmDr\\.|JUDr\\.|PhDr\\.|Paed\\sDr\\.|ing\\.|Dr\\.|PaedDr\\.|Ing\\.|Ing\\s)", 
        "", JMENO
      ) %>% stringr::str_trim(., "both")
    ) %>% 
    mutate(
      TITULY = case_when(
        is.na(TITULY) & !is.na(TITULY_PRED) ~ TITULY_PRED,
        !is.na(TITULY) & is.na(TITULY_PRED) ~ TITULY,
        !is.na(TITULY) & !is.na(TITULY_PRED) ~ paste0(TITULY_PRED, ", ", TITULY)
      ), 
      TITUL_KATEGORIE = categorize_titles(TITULY)
    ) %>% 
    select(-TITULY_PRED)
}

extract_titles_from_last_name_1998 <- function(x){
  REPLACEMENT_VECTOR <- c(
    "\\bDoc\\sIng\\b"="Doc. Ing.",
    "\\bDocent\\sCSc"="Doc. CSc.",
    "\\bProf\\sIng\\sDrSc\\b"="Prof. Ing. DrSc.",
    "\\bDocPhDrCSc\\b"="Doc. PhDr. CSc.",
    "\\bDocMUDrDrSc\\b"="Doc. MUDr. DrSc.",
    "\\bTh Lic P."="ThLic. P.",
    "\\bIng\\b"="Ing\\.", 
    "\\bing\\b"="Ing\\.",
    "\\bMUDr\\b"="MUDr\\.",
    "\\bMudr\\b"="MUDr.",
    "\\bPharmDr\\b"="PharmDr.",
    "\\bPhMr\\b"="PhMr.",
    "\\bMgr\\b"="Mgr.",
    "\\bMUC\\b"="MUC.",
    "\\bPharm\\sDr."="PharmDr.",
    "\\bJUDr\\b"="JUDr.",
    "\\bPhDr\\b"="PhDr.",
    "\\bMVDr\\b"="MVDr.",
    "\\bDr\\b"="Dr.",
    "\\bMGr\\b"="Mgr.",
    "\\bRnDr\\b"="RNDr.",
    "\\bRNDr\\b"="RNDr.",
    "\\bPaedDr\\b"="PaedDr.",
    "\\bPeadr\\b"="PaedDr.",
    "\\bRSDr\\b"="RSDr.", 
    "\\bRsDr\\b"="RSDr.",
    "\\bPaeDr\\b"="PaeDr.",
    "\\bBc\\b"="Bc.",
    "\\bTh,Mgr\\b"="ThMgr",
    "\\bRN Mgr\\b"="Mgr",
    "\\(ml\\.\\)"="ml.",
    "\\bml\\b"="ml.",
    "\\bst\\b"="st.",
    "\\bCSc\\b"="CSc.",
    "\\bdpt\\b"="",
    "\\bdipl.tech."=""
  )
  x %>%
    mutate(PRIJMENI = stringr::str_replace_all(PRIJMENI, REPLACEMENT_VECTOR)) %>%
    mutate(TITULY_NEW = 
      paste(stringr::str_extract_all(PRIJMENI, "\\s[A-Za-z]+\\.[A-Ža-ž. ]*|,[^,]+"), 
            sep = " "),
      PRIJMENI = stringr::str_trim(
        stringr::str_remove_all(PRIJMENI, "\\s[A-Za-z]+\\.[A-Ža-ž. ]*|,[^,]+"))) %>%
    mutate(
      TITULY_NEW = ifelse(TITULY_NEW == "character(0)", NA_character_, TITULY_NEW), 
      TITULY = if_else(!is.na(TITULY_NEW), paste0(TITULY, TITULY_NEW, sep = " "), 
                            TITULY)) %>% 
    mutate(PRIJMENI = stringr::str_trim(gsub("\\.", "", PRIJMENI)), 
           TITULY = stringr::str_trim(gsub("\\.\\.", "\\.", TITULY))) %>%
    mutate(TITULY = ifelse(TITULY == "character(0)", NA_character_, TITULY), 
           TITUL_KATEGORIE = categorize_titles(TITULY)) %>% 
    select(-TITULY_NEW)
}

merge_and_recode_titles <- function(df){
  df %>%
    mutate(TITULY = case_when(!is.na(TITULPRED) & !is.na(TITULZA) ~ paste(TITULPRED, ", ", TITULZA), 
                              !is.na(TITULPRED) ~ TITULPRED, 
                              !is.na(TITULZA) ~ TITULZA,
                              TRUE ~ NA_character_), 
           TITUL_KATEGORIE = categorize_titles(TITULY))
}

remove_order_from_last_name <- function(df){
  df %>% 
    mutate(PRIJMENI = gsub(
      "[,]*\\s(starší|st\\.|ml\\.|mladší|jr\\.|sen\\.|jun\\.|I\\.|II\\.|ST\\.|ML\\.|Ml\\.|ml|II|I)$", "", PRIJMENI) %>% 
        gsub(",(ml\\.|st\\.)", "", .) %>% 
        gsub(",[ ]*roč\\.[0-9]+", "", .) %>% 
        gsub("\\s\\((ml|mladší|st|starší)\\)", "", .))
}

categorize_sex <- function(df, unique_first_names){
  df %>% 
    left_join(., unique_first_names, by = "JMENO") %>% 
    mutate(SEX = case_when(
      !is.na(SEX_first_name) ~ SEX_first_name, 
      grepl("á$", PRIJMENI) | grepl("ka\\b", POVOLANI) ~ "female", 
      TRUE ~ "male"
    )) %>% 
    select(-SEX_first_name)
}

categorize_by_first_name <- function(df, threshold = 0.9){
  df %>% 
    mutate(SEX_first_name = purrr::map_chr(
      JMENO, function(x) {
        names <- quanteda::tokenize_word1(x) %>% 
          unlist() %>% 
          `[`(., !. %in% c(" ", "-")) %>% 
          tolower()
        
        tmp <- names.cze:::nameIndex %>% 
          filter(name %in% names) %>% 
          summarise(across(where(is.numeric), sum)) %>% 
          mutate(male_prop = (male_forename) / (male_forename + female_forename))
        
        case_when(
          tmp$male_prop >= threshold ~ "male", 
          1 - tmp$male_prop >= threshold ~ "female", 
          TRUE ~ NA_character_
        )
      }
    ))
}

extract_el_value <- function(x, el){
  x %>% html_node(el) %>% html_text()
}

extract_el_value_num <- function(x, el){
  extract_el_value(x, el) %>% as.numeric()
}


VSTRANA_MAP_96 <- c(
  "1" = "SD",
  "2" = "ČSSD",
  "3" = "ODS",
  "4" = "PB",
  "5" = "NEZ",
  "6" = "MNS-HSMS",
  "7" = "DEU",
  "8" = "ODA",
  "9" = "SČK",
  "10" = "KDU-ČSL",
  "11" = "DŽJ",
  "12" = "SDL",
  "13" = "ČMUS",
  "14" = "ČP",
  "15" = "KSČM",
  "16" = "SZ",
  "17" = "SPR-RSČ",
  "18" = "LB",
  "19" = "HSMSMNSJ",
  "20" = "CAO"
)

VSTRANA_MAP_98 <- c(
  "1"="KDU-ČSL", 
  "2" = "NEZ", 
  "3" ="CAO", 
  "4" = "DEU", 
  "5" ="ODS", 
  "6" = "OK", 
  "7" ="ČSSD", 
  "8"="MODS",
  "9" = "KSČM", 
  "10" ="SPR-RSČ",
  "11" = "US", 
  "12" ="DŽJ", 
  "13" = "ČSNS",
  "14" ="SDČR",
  "15" = "A2000", 
  "16" ="PB",
  "17" = "ODA", 
  "18" ="SZ"
)

clean_ps <- function(df, VSTRANA_MAP){
  df %>%
    mutate(ZKRATKAN8 = purrr::map_chr(VSTRANA, function(x) VSTRANA_MAP[as.character(x)]), 
           # KSTRANA = purrr::map_chr(VSTRANA, function(x) VSTRANA_MAP[as.character(x)]), 
           TITULZA = stringr::str_extract(PRIJMENI, ",[A-Za-z,\\. ]+") %>% gsub(",[ ]*", "", .), 
           PRIJMENI = stringr::str_remove(PRIJMENI, ",[A-Za-z,\\. ]+")
    ) %>%
    select(-VSTRANA) %>%
    rename(NAZEV_STRK = NAZEV_VSTRANA, 
           TITULPRED = TITUL, 
           POCPROCVSE = PROCHLASU)
  
}

# reading psrkl.xlsx etc (seznam kandidujících stran)
read_parties <- function(path, cleanup_f = pass){
  read_excel(path) %>%
    cleanup_f %>%
    select(KSTRANA, ZKRATKAK8, NAZEV_STRK)
}

read_municipal_parties <- function(path, cleanup_f = pass){
  read_excel(path) %>%
    cleanup_f %>%
    select(KODZASTUP, COBVODU, POR_STR_HL, OSTRANA, NAZEVCELK)
}

parse_ep_rkl_row <- function(x){
  tibble::tibble(
    ESTRANA = x %>% html_node("estrana") %>% html_text() %>% as.numeric(),
    VSTRANA = x %>% html_node("vstrana") %>% html_text() %>% as.numeric(),
    NAZEVCELK = x %>% html_node("nazevcelk") %>% html_text(), 
    NAZEV_STRE = x %>% html_node("nazev_stre") %>% html_text(), 
    ZKRATKAE30 = x %>% html_node("zkratkae30") %>% html_text(), 
    ZKRATKAE8 = x %>% html_node("zkratkae8") %>% html_text(), 
    POCSTRVKO = x %>% html_node("pocstrvko") %>% html_text() %>% as.numeric(), 
    SLOZENI = x %>% html_node("slozeni") %>% html_text(), 
    STAVREG = x %>% html_node("stavreg") %>% html_text() %>% as.numeric(), 
    PLATNOST = x %>% html_node("platnost") %>% html_text() %>% as.numeric(), 
    POCMANDCR = x %>% html_node("pocmandcr") %>% html_text() %>% as.numeric(), 
  )
}

# (here("data", "EP2004", "EP2004reg", "eprkl.xlsx"))
read_parties_xml <- function(path, cleanup_f = pass){
  read_html(path, encoding = "WINDOWS-1250") %>%
    html_nodes("ep_rkl_row") %>%
    purrr::map_df(., parse_ep_rkl_row) %>%
    cleanup_f
} 

# reading cpp.xlsx etc. (strany pro stranickou příslušnost kandidátů)
read_cpp <- function(path, cleanup_f = pass){
  read_excel(path) %>%
    cleanup_f %>%
    select(PSTRANA, ZKRATKAP8)
}

parse_cpp_row <- function(x){
  tibble::tibble(
    PSTRANA = x %>% html_node("pstrana") %>% html_text() %>% as.numeric(), 
    NAZEV_STRP = x %>% html_node("nazev_strp") %>% html_text(), 
    ZKRATKAP30 = x %>% html_node("zkratkap30") %>% html_text(),
    ZKRATKAP8 = x %>% html_node("zkratkap8") %>% html_text() 
  )
}

read_cpp_xml <- function(path){
  read_html(path, encoding = "WINDOWS-1250") %>%
    html_nodes("cpp_row") %>%
    purrr::map_df(., parse_cpp_row) %>%
    select(PSTRANA, ZKRATKAP8)
}

# reading cns.xlsx etc. (strany nominující kandidáty)
read_cns <- function(path, cleanup_f = pass){
  read_excel(path) %>%
    cleanup_f %>%
    select(NSTRANA, ZKRATKAN8)
}

parse_cns_row <- function(x){
  tibble::tibble(
    NSTRANA = x %>% html_node("nstrana") %>% html_text() %>% as.numeric(), 
    NAZEV_STRN = x %>% html_node("nazev_strn") %>% html_text(), 
    ZKRATKAN30 = x %>% html_node("zkratkan30") %>% html_text(),
    ZKRATKAN8 = x %>% html_node("zkratkan8") %>% html_text() 
  )
}

read_cns_xml <- function(path){
  read_html(path, encoding = "WINDOWS-1250") %>%
    html_nodes("cns_row") %>%
    purrr::map_df(., parse_cns_row) %>%
    select(NSTRANA, ZKRATKAN8)
}

read_candidates <- function(list_path, parties_df, cpp_df, cns_df, 
                            cleanup_f = pass){
  year <- as.numeric(stringr::str_extract(list_path, "[0-9]{4}"))
  read_excel(list_path) %>%
    cleanup_f %>%
    left_join(., parties_df, by = "KSTRANA") %>%
    left_join(., cpp_df, by = "PSTRANA") %>%
    left_join(., cns_df, by = "NSTRANA") %>%
    mutate(row_id = row_number(), 
           ROK_NAROZENI = year - VEK) %>%
    merge_and_recode_titles
}

read_municipal_candidates <- function(list_path, parties_df, cpp_df, cns_df, 
                                      cleanup_f = pass){
  year <- as.numeric(stringr::str_extract(list_path, "[0-9]{4}"))
  read_excel(list_path) %>%
    cleanup_f %>%
    left_join(., parties_df, by = c("KODZASTUP", "COBVODU", "POR_STR_HL", "OSTRANA")) %>%
    left_join(., cpp_df, by = "PSTRANA") %>%
    left_join(., cns_df, by = "NSTRANA") %>%
    mutate(row_id = row_number(), 
           ROK_NAROZENI = year - VEK) %>%
    merge_and_recode_titles
}

parse_ep_regkand_row <- function(x){
  tibble::tibble(
    ESTRANA = extract_el_value_num(x, "estrana"), 
    PORCISLO = extract_el_value_num(x, "porcislo"), 
    JMENO = extract_el_value(x, "jmeno"), 
    PRIJMENI = extract_el_value(x, "prijmeni"), 
    VEK = extract_el_value_num(x, "vek"),
    TITULPRED = extract_el_value(x, "titulpred"), 
    TITULZA = extract_el_value(x, "titulza"), 
    BYDLISTEN = extract_el_value(x, "bydlisten"), 
    BYDLISTEK = extract_el_value_num(x, "bydlistek"), 
    PSTRANA = extract_el_value_num(x, "pstrana"),
    NSTRANA = extract_el_value_num(x, "nstrana"),
    POCHLASU = extract_el_value_num(x, "pochlasu"),
    POCPROC = extract_el_value_num(x, "pocproc"),
    POCPROCVSE = extract_el_value_num(x, "pocprocvse"),
    MANDAT = extract_el_value_num(x, "mandat"),
    PORADIMAND = extract_el_value_num(x, "poradimand"),
    PORADINAHR = extract_el_value_num(x, "poradinahr"), 
    POVOLANI = extract_el_value(x, "povolani")
  )
}

read_candidates_xml <- function(list_path, parties_df, cpp_df, cns_df, 
                                cleanup_f = pass){
  year <- as.numeric(stringr::str_extract(list_path, "[0-9]{4}"))
  read_html(list_path, encoding = "WINDOWS-1250") %>%
    html_nodes("ep_regkand_row") %>%
    purrr::map_df(., parse_ep_regkand_row) %>%
    cleanup_f %>%
    left_join(., parties_df, by = "KSTRANA") %>%
    left_join(., cpp_df, by = "PSTRANA") %>%
    left_join(., cns_df, by = "NSTRANA") %>%
    mutate(row_id = row_number(), 
           ROK_NAROZENI = year - VEK) %>%
    merge_and_recode_titles
}

filter_city_districts <- function(df, district_map){
  df %>% 
    filter(municipality_type == 2) %>% 
    left_join(., district_map, by = c("municipality_id"="CITY_DISTRICT"))
}

filter_municipalities <- function(df, district_map){
  df %>%
    filter(municipality_type == 1) %>% 
    mutate(municipality = municipality_id)
}

cmp_within_1 <- function(){
  function(x, y){
    abs(x-y) <= 1
  }
}

cmp_last_names <- function(){
  function(x, y){
    x_split <- strsplit(gsub("\\(|\\)", "", x), "\\s|-")
    y_split <- strsplit(gsub("\\(|\\)", "", y), "\\s|-")
    purrr::map2_lgl(x_split, y_split, function(x, y) { any(x %in% y) })
  }
}

match_data <- function(d1, d2, blocking_vars = c("first_name", "last_name")){
  d1 <- d1 %>% 
    mutate(across(c(title_before_name, title_after_name), ~if_else(is.na(.x), "", .x)))
  d2 <- d2 %>% 
    mutate(across(c(title_before_name, title_after_name), ~if_else(is.na(.x), "", .x)))
  
  pairs <- pair_blocking(d1, d2, blocking_vars)
  pairs <- compare_pairs(pairs, on = c("first_name", "last_name", "birth_year", 
                                       "title_before_name", "title_after_name", 
                                       "party_membership_abbv", 
                                       "nominating_party_abbv", 
                                       "residence_name", "occupation"), 
                         comparators = list(
                           birth_year = cmp_within_1(),
                           occupation = cmp_jaccard()
                           # PRIJMENI = cmp_last_names()
                           # TODO: stejný nebo větší titul
                         ))
  
  scores <- score_simple(pairs, "score", 
                         on = c("first_name", "last_name", "birth_year", 
                                "title_before_name", "title_after_name", 
                                "party_membership_abbv", 
                                "nominating_party_abbv", "residence_name", "occupation"), 
                         w1 = c(first_name = 2, last_name = 2, birth_year = 2, 
                                title_before_name = 0.5, title_after_name = 0.5, 
                                party_membership_abbv = 0.5, nominating_party_abbv = 0.5, 
                                residence_name = 0.5, occupation = 0.5),
                         w0 = c(first_name = -5, last_name = -5, birth_year = -5,
                                title_before_name = -0.5, title_after_name = -0.5, 
                                party_membership_abbv = 0, nominating_party_abbv = 0, 
                                residence_name = -0.5, occupation = 0), 
                         wna = 0)
  
  selected_pairs_greedy <- select_greedy(scores, variable = "greedy", score = "score", threshold = 6)
  
  link(selected_pairs_greedy, selection = "greedy")
}

match_reg_data <- function(d1, d2, multiple_last_names, 
                           blocking_vars = c("first_name", "last_name", "region_code")){
  d1 <- d1 %>% 
    mutate(across(c(title_before_name, title_after_name), ~if_else(is.na(.x), "", .x)))
  d2 <- d2 %>% 
    mutate(across(c(title_before_name, title_after_name), ~if_else(is.na(.x), "", .x)))
  
  d1_multiple_names <- d1 %>% 
    filter(last_name %in% multiple_last_names & sex == "female")
  d2_multiple_names <- d2 %>% 
    filter(last_name %in% multiple_last_names & sex == "female")
  
  d1_no_change <- d1 %>% 
    filter(!row_id %in% d1_multiple_names$row_id)
  d2_no_change <- d2 %>% 
    filter(!row_id %in% d2_multiple_names$row_id)
  
  comparing_vars <- c("first_name", "last_name", "birth_year", 
                      "title_before_name", "title_after_name", 
                      "party_membership_abbv", 
                      "nominating_party_abbv", 
                      "residence_name", "occupation")
  w1_scores <- c(first_name = 2, last_name = 2, birth_year = 2, 
         title_before_name = 0.5, title_after_name = 0.5, 
         party_membership_abbv = 0.5, nominating_party_abbv = 0.5, 
         residence_name = 0.5, occupation = 0.5)
  w0_scores <- c(first_name = -5, last_name = -5, birth_year = -5,
         title_before_name = -0.5, title_after_name = -0.5, 
         party_membership_abbv = 0, nominating_party_abbv = 0, 
         residence_name = -0.5, occupation = 0)
  
  # Link women with multiple names
  blocking_vars_wo_last_name <- blocking_vars[blocking_vars != "last_name"]
  
  pairs_mn <- pair_blocking(d1_multiple_names, d2_multiple_names, blocking_vars_wo_last_name)
  pairs_mn <- compare_pairs(pairs_mn, on = comparing_vars,
                            comparators = list(
                              birth_year = cmp_within_1(),
                              occupation = cmp_jaccard(),
                              last_name = cmp_last_names()
                            ))
  
  scores_mn <- score_simple(pairs_mn, "score", 
                            on = comparing_vars, 
                            w1 = w1_scores,
                            w0 = w0_scores, 
                            wna = 0)
  
  selected_pairs_greedy_mn <- select_greedy(scores_mn, variable = "greedy", score = "score", threshold = 6)
  
  mn_linked <- link(selected_pairs_greedy_mn, selection = "greedy")
  
  # link the rest
  pairs <- pair_blocking(d1_no_change, d2_no_change, blocking_vars)
  pairs <- compare_pairs(pairs, on = comparing_vars, 
                         comparators = list(
                           birth_year = cmp_within_1(),
                           occupation = cmp_jaccard()
                         ))
  
  scores <- score_simple(pairs, "score", 
                         on = comparing_vars, 
                         w1 = w1_scores,
                         w0 = w0_scores, 
                         wna = 0)
  
  selected_pairs_greedy <- select_greedy(scores, variable = "greedy", score = "score", threshold = 6)
  
  linked_no_change <- link(selected_pairs_greedy, selection = "greedy")
  bind_rows(mn_linked, linked_no_change)
}

match_psp_data <- function(d1, d2, multiple_last_names, 
                           blocking_vars = c("first_name", "last_name")){
  d1 <- d1 %>% 
    mutate(across(c(title_before_name, title_after_name), ~if_else(is.na(.x), "", .x)))
  d2 <- d2 %>% 
    mutate(across(c(title_before_name, title_after_name), ~if_else(is.na(.x), "", .x)))
  
  d1_multiple_names <- d1 %>% 
    filter(last_name %in% multiple_last_names & sex == "female")
  d2_multiple_names <- d2 %>% 
    filter(last_name %in% multiple_last_names & sex == "female")
  
  d1_no_change <- d1 %>% 
    filter(!row_id %in% d1_multiple_names$row_id)
  d2_no_change <- d2 %>% 
    filter(!row_id %in% d2_multiple_names$row_id)
  
  comparing_vars <- c("first_name", "last_name", "birth_year", 
                      "title_before_name", "title_after_name", 
                      "party_membership_abbv", "nominating_party_abbv", 
                      "residence_name", "occupation", 
                      "region_name")
  w1_scores <- c(first_name = 2, last_name = 2, birth_year = 2, 
                 title_before_name = 0.5, title_after_name = 0.5, 
                 party_membership_abbv = 0.5, nominating_party_abbv = 0.5, 
                 residence_name = 0.5, occupation = 0.5, 
                 region_name = 0.5)
  w0_scores <- c(first_name = -5, last_name = -5, birth_year = -5,
                 title_before_name = -0.5, title_after_name = -0.5, 
                 party_membership_abbv = 0, nominating_party_abbv = 0, 
                 residence_name = -0.5, occupation = 0, 
                 region_name = 0)
  
  # Link women with multiple names
  blocking_vars_wo_last_name <- blocking_vars[blocking_vars != "last_name"]
  
  pairs_mn <- pair_blocking(d1_multiple_names, d2_multiple_names, blocking_vars_wo_last_name)
  pairs_mn <- compare_pairs(pairs_mn, on = comparing_vars,
                            comparators = list(
                              birth_year = cmp_within_1(),
                              occupation = cmp_jaccard(),
                              last_name = cmp_last_names()
                            ))
  
  scores_mn <- score_simple(pairs_mn, "score", 
                            on = comparing_vars, 
                            w1 = w1_scores,
                            w0 = w0_scores, 
                            wna = 0)
  
  selected_pairs_greedy_mn <- select_greedy(scores_mn, variable = "greedy", score = "score", threshold = 6)
  
  mn_linked <- link(selected_pairs_greedy_mn, selection = "greedy")
  
  # link the rest
  pairs <- pair_blocking(d1_no_change, d2_no_change, blocking_vars)
  pairs <- compare_pairs(pairs, on = comparing_vars, 
                         comparators = list(
                           birth_year = cmp_within_1(),
                           occupation = cmp_jaccard()
                         ))
  
  scores <- score_simple(pairs, "score", 
                         on = comparing_vars, 
                         w1 = w1_scores,
                         w0 = w0_scores, 
                         wna = 0)
  
  selected_pairs_greedy <- select_greedy(scores, variable = "greedy", score = "score", threshold = 6)
  
  linked_no_change <- link(selected_pairs_greedy, selection = "greedy")
  bind_rows(mn_linked, linked_no_change)
}

match_mun_data <- function(d1, d2, multiple_last_names, 
                           blocking_vars = c("first_name", "last_name", "municipality_id")){
  d1 <- d1 %>% 
    mutate(across(c(titles), ~if_else(is.na(.x), "", .x)))
  d2 <- d2 %>% 
    mutate(across(c(titles), ~if_else(is.na(.x), "", .x)))
  
  d1_multiple_names <- d1 %>% 
    filter(last_name %in% multiple_last_names & sex == "female")
  d2_multiple_names <- d2 %>% 
    filter(last_name %in% multiple_last_names & sex == "female")
  
  d1_no_change <- d1 %>% 
    filter(!row_id %in% d1_multiple_names$row_id)
  d2_no_change <- d2 %>% 
    filter(!row_id %in% d2_multiple_names$row_id)
  
  comparing_vars <- c("first_name", "last_name", "birth_year", 
                      "titles", "party_membership_abbv", 
                      "nominating_party_abbv", "residence_name", 
                      "occupation")
  
  w1_scores <- c(first_name = 2, last_name = 2, birth_year = 2, 
                 titles = 0.5, 
                 party_membership_abbv = 0.5, 
                 nominating_party_abbv = 0.5, 
                 residence_name = 0.5, occupation = 0.5)
  
  w0_scores <- c(first_name = -5, last_name = -5, birth_year = -5,
                 titles = -0.25, 
                 party_membership_abbv = 0, 
                 nominating_party_abbv = 0, 
                 residence_name = -0.25, occupation = 0)
  
  # Link women with multiple names
  blocking_vars_wo_last_name <- blocking_vars[blocking_vars != "last_name"]
  
  pairs_mn <- pair_blocking(d1_multiple_names, d2_multiple_names, blocking_vars_wo_last_name)
  pairs_mn <- compare_pairs(pairs_mn, on = comparing_vars, 
                            comparators = list(
                              birth_year = cmp_within_1(),
                              occupation = cmp_jaccard(), 
                              titles = cmp_jaccard(), 
                              residence_name = cmp_jaccard(),
                              last_name = cmp_last_names()
                            ))
  
  scores_mn <- score_simple(pairs_mn, "score", 
                            on = comparing_vars, 
                            w1 = w1_scores,
                            w0 = w0_scores, 
                            wna = 0)
  
  selected_pairs_greedy_mn <- select_greedy(scores_mn, variable = "greedy", score = "score", 
                                            threshold = 6)
  
  linked_mn <- link(selected_pairs_greedy_mn, selection = "greedy")
  
  # Link the rest
  pairs <- pair_blocking(d1_no_change, d2_no_change, blocking_vars)
  pairs <- compare_pairs(pairs, on = comparing_vars, 
                         comparators = list(
                           birth_year = cmp_within_1(),
                           occupation = cmp_jaccard(), 
                           titles = cmp_jaccard(), 
                           residence_name = cmp_jaccard()
                         ))
  
  scores <- score_simple(pairs, "score", 
                         on = comparing_vars, 
                         w1 = w1_scores,
                         w0 = w0_scores, 
                         wna = 0)
  
  selected_pairs_greedy <- select_greedy(scores, variable = "greedy", score = "score", 
                                         threshold = 6)
  
  linked_others <- link(selected_pairs_greedy, selection = "greedy")
  
  bind_rows(linked_mn, linked_others)
}

match_sen_data <- function(d1, d2, blocking_vars = c("first_name", "last_name")){
  d1 <- d1 %>% 
    mutate(across(c(title_before_name, title_after_name), ~if_else(is.na(.x), "", .x)))
  d2 <- d2 %>% 
    mutate(across(c(title_before_name, title_after_name), ~if_else(is.na(.x), "", .x)))
  
  pairs <- pair_blocking(d1, d2, blocking_vars)
  pairs <- compare_pairs(pairs, on = c("first_name", "last_name", "birth_year", 
                                       "title_before_name", "title_after_name", 
                                       "party_membership_abbv", 
                                       "nominating_party_abbv", 
                                       "residence_name", "occupation", 
                                       "senate_district"), 
                         comparators = list(
                           birth_year = cmp_within_1(),
                           occupation = cmp_jaccard()
                           # PRIJMENI = cmp_last_names()
                           # TODO: stejný nebo větší titul
                         ))
  
  scores <- score_simple(pairs, "score", 
                         on = c("first_name", "last_name", "birth_year", 
                                "title_before_name", "title_after_name", "party_membership_abbv", 
                                "nominating_party_abbv", "residence_name", "occupation", 
                                "senate_district"), 
                         w1 = c(first_name = 2, last_name = 2, birth_year = 2, 
                                title_before_name = 0.5, title_after_name = 0.5, 
                                party_membership_abbv = 0.5, nominating_party_abbv = 0.5, 
                                residence_name = 0.5, occupation = 0.5, 
                                senate_district = 2),
                         w0 = c(first_name = -5, last_name = -5, birth_year = -5,
                                title_before_name = -0.5, title_after_name = -0.5, 
                                party_membership_abbv = 0, nominating_party_abbv = 0, 
                                residence_name = -0.5, occupation = 0, 
                                senate_district = 0), 
                         wna = 0)
  
  selected_pairs_greedy <- select_greedy(scores, variable = "greedy", score = "score", threshold = 6)
  
  link(selected_pairs_greedy, selection = "greedy")
}

match_mun_panel_data <- function(d1, d2, multiple_last_names, 
                                 blocking_vars = c("first_name", "last_name", "municipality_id")){
  d1 <- d1 %>% 
    mutate(across(c(titles), ~if_else(is.na(.x), "", .x)))
  d2 <- d2 %>% 
    mutate(across(c(titles), ~if_else(is.na(.x), "", .x)))
  
  d1_multiple_names <- d1 %>% 
    filter(last_name %in% multiple_last_names & sex == "female")
  d2_multiple_names <- d2 %>% 
    filter(last_name %in% multiple_last_names & sex == "female")
  
  d1_no_change <- d1 %>% 
    filter(!row_id %in% d1_multiple_names$row_id)
  d2_no_change <- d2 %>% 
    filter(!row_id %in% d2_multiple_names$row_id)
  
  comparing_vars <- c("first_name", "last_name", "birth_year", 
                      "titles", "party_membership_abbv", 
                      "nominating_party_abbv", "residence_name", "occupation")
  
  w1_scores <- c(first_name = 2, last_name = 2, birth_year = 2, 
                 titles = 0.5, 
                 party_membership_abbv = 0.5, nominating_party_abbv = 0.5, 
                 residence_name = 0.5, occupation = 0.5)
  
  w0_scores <- c(first_name = -5, last_name = -5, birth_year = -5,
                 titles = -0.25, 
                 party_membership_abbv = 0, nominating_party_abbv = 0, 
                 residence_name = -0.25, occupation = 0)
  
  # Link women with multiple names
  blocking_vars_wo_last_name <- blocking_vars[blocking_vars != "last_name"]
  
  pairs_mn <- pair_blocking(d1_multiple_names, d2_multiple_names, blocking_vars_wo_last_name)
  pairs_mn <- compare_pairs(pairs_mn, on = comparing_vars, 
                            comparators = list(
                              birth_year = cmp_within_1(),
                              occupation = cmp_jaccard(), 
                              titles = cmp_jaccard(), 
                              residence_name = cmp_jaccard(),
                              last_name = cmp_last_names()
                            ))
  
  scores_mn <- score_simple(pairs_mn, "score", 
                            on = comparing_vars, 
                            w1 = w1_scores,
                            w0 = w0_scores, 
                            wna = 0)
  
  selected_pairs_greedy_mn <- select_greedy(scores_mn, variable = "greedy", score = "score", 
                                            threshold = 6)
  
  linked_mn <- link(selected_pairs_greedy_mn, selection = "greedy") %>% 
    left_join(., scores_mn %>% select(.x, .y, score), by = c(".x", ".y"))
  
  # Link the rest
  pairs <- pair_blocking(d1_no_change, d2_no_change, blocking_vars)
  pairs <- compare_pairs(pairs, on = comparing_vars, 
                         comparators = list(
                           birth_year = cmp_within_1(),
                           occupation = cmp_jaccard(), 
                           titles = cmp_jaccard(), 
                           residence_name = cmp_jaccard()
                         ))
  
  scores <- score_simple(pairs, "score", 
                         on = comparing_vars, 
                         w1 = w1_scores,
                         w0 = w0_scores, 
                         wna = 0)
  
  selected_pairs_greedy <- select_greedy(scores, variable = "greedy", score = "score", 
                                         threshold = 6)
  
  linked_others <- link(selected_pairs_greedy, selection = "greedy") %>% 
    left_join(., scores %>% select(.x, .y, score), by = c(".x", ".y"))
  
  bind_rows(linked_mn, linked_others)
}

match_mun_reg_panel <- function(d1, d2, multiple_last_names, 
                                blocking_vars = c("first_name", "last_name", "region_name")){
  d1 <- d1 %>% 
    mutate(across(c(titles), ~if_else(is.na(.x), "", .x)))
  d2 <- d2 %>% 
    mutate(across(c(titles), ~if_else(is.na(.x), "", .x)))
  
  d1_multiple_names <- d1 %>% 
    filter(last_name %in% multiple_last_names & sex == "female")
  d2_multiple_names <- d2 %>% 
    filter(last_name %in% multiple_last_names & sex == "female")
  
  d1_no_change <- d1 %>% 
    filter(!row_id %in% d1_multiple_names$row_id)
  d2_no_change <- d2 %>% 
    filter(!row_id %in% d2_multiple_names$row_id)
  
  comparing_vars <- c("first_name", "last_name", "birth_year", 
                      "titles", "party_membership_abbv", 
                      "nominating_party_abbv", "residence_name", 
                      "occupation", 
                      "region_name")
  
  w1_scores <- c(first_name = 2, last_name = 2, birth_year = 2, 
                 titles = 0.5, 
                 party_membership_abbv = 0.5, nominating_party_abbv = 0.5, 
                 residence_name = 0.5, occupation = 0.5, 
                 region_name = 1)
  
  w0_scores <- c(first_name = -5, last_name = -5, birth_year = -5,
                 titles = -0.25, 
                 party_membership_abbv = 0, nominating_party_abbv = 0, 
                 residence_name = -0.25, occupation = 0, 
                 region_name = -0.25)
  
  # Link women with multiple names
  blocking_vars_wo_last_name <- blocking_vars[blocking_vars != "last_name"]
  
  pairs_mn <- pair_blocking(d1_multiple_names, d2_multiple_names, blocking_vars_wo_last_name)
  pairs_mn <- compare_pairs(pairs_mn, on = comparing_vars, 
                            comparators = list(
                              birth_year = cmp_within_1(),
                              occupation = cmp_jaccard(), 
                              titles = cmp_jaccard(), 
                              residence_name = cmp_jaccard(),
                              last_name = cmp_last_names()
                            ))
  
  scores_mn <- score_simple(pairs_mn, "score", 
                            on = comparing_vars, 
                            w1 = w1_scores,
                            w0 = w0_scores, 
                            wna = 0)
  
  selected_pairs_greedy_mn <- select_greedy(scores_mn, variable = "greedy", score = "score", 
                                            threshold = 6)
  
  linked_mn <- link(selected_pairs_greedy_mn, selection = "greedy") %>% 
    left_join(., scores_mn %>% select(.x, .y, score), by = c(".x", ".y"))
  
  # Link the rest
  pairs <- pair_blocking(d1_no_change, d2_no_change, blocking_vars)
  pairs <- compare_pairs(pairs, on = comparing_vars, 
                         comparators = list(
                           birth_year = cmp_within_1(),
                           occupation = cmp_jaccard(), 
                           titles = cmp_jaccard(), 
                           residence_name = cmp_jaccard()
                         ))
  
  scores <- score_simple(pairs, "score", 
                         on = comparing_vars, 
                         w1 = w1_scores,
                         w0 = w0_scores, 
                         wna = 0)
  
  selected_pairs_greedy <- select_greedy(scores, variable = "greedy", score = "score", 
                                         threshold = 6)
  
  linked_others <- link(selected_pairs_greedy, selection = "greedy") %>% 
    left_join(., scores %>% select(.x, .y, score), by = c(".x", ".y"))
  
  bind_rows(linked_mn, linked_others)
}

match_mr_psp_panel <- function(d1, d2, multiple_last_names, blocking_vars = c("first_name", "last_name")){
  browser()
  d1 <- d1 %>% 
    mutate(across(c(titles), ~if_else(is.na(.x), "", .x)))
  d2 <- d2 %>% 
    mutate(across(c(titles), ~if_else(is.na(.x), "", .x)))
  
  d1_multiple_names <- d1 %>% 
    filter(last_name %in% multiple_last_names & sex == "female")
  d2_multiple_names <- d2 %>% 
    filter(last_name %in% multiple_last_names & sex == "female")
  
  d1_no_change <- d1 %>% 
    filter(!row_id %in% d1_multiple_names$row_id)
  d2_no_change <- d2 %>% 
    filter(!row_id %in% d2_multiple_names$row_id)
  
  comparing_vars <- c("first_name", "last_name", "birth_year", 
                      "titles", "party_membership_abbv", 
                      "nominating_party_abbv", "residence_name", "occupation", 
                      "region_name")
  
  w1_scores <- c(first_name = 2, last_name = 2, birth_year = 2, 
         titles = 0.5, 
         party_membership_abbv = 0.5, nominating_party_abbv = 0.5, 
         residence_name = 0.5, occupation = 0.5, 
         region_name = 1)
  w0_scores <- c(first_name = -5, last_name = -5, birth_year = -5,
         titles = -0.25, 
         party_membership_abbv = 0, nominating_party_abbv = 0, 
         residence_name = -0.25, occupation = 0, 
         region_name = -0.25)
  
  # Link women with multiple names
  blocking_vars_wo_last_name <- blocking_vars[blocking_vars != "last_name"]
  
  pairs_mn <- pair_blocking(d1_multiple_names, d2_multiple_names, blocking_vars_wo_last_name)
  pairs_mn <- compare_pairs(pairs_mn, on = comparing_vars, 
                            comparators = list(
                              birth_year = cmp_within_1(),
                              occupation = cmp_jaccard(), 
                              titles = cmp_jaccard(), 
                              residence_name = cmp_jaccard(),
                              last_name = cmp_last_names()
                            ))
  
  scores_mn <- score_simple(pairs_mn, "score", 
                            on = comparing_vars, 
                            w1 = w1_scores,
                            w0 = w0_scores, 
                            wna = 0)
  
  selected_pairs_greedy_mn <- select_greedy(scores_mn, variable = "greedy", score = "score", 
                                            threshold = 6.25)
  
  linked_mn <- link(selected_pairs_greedy_mn, selection = "greedy") %>% 
    left_join(., scores_mn %>% select(.x, .y, score), by = c(".x", ".y"))
  
  # Link the rest
  pairs <- pair_blocking(d1_no_change, d2_no_change, blocking_vars)
  pairs <- compare_pairs(pairs, on = comparing_vars, 
                         comparators = list(
                           birth_year = cmp_within_1(),
                           occupation = cmp_jaccard(), 
                           titles = cmp_jaccard(), 
                           residence_name = cmp_jaccard()
                         ))
  
  scores <- score_simple(pairs, "score", 
                         on = comparing_vars, 
                         w1 = w1_scores,
                         w0 = w0_scores, 
                         wna = 0)
  
  selected_pairs_greedy <- select_greedy(scores, variable = "greedy", score = "score", 
                                         threshold = 6.25)
  
  linked_others <- link(selected_pairs_greedy, selection = "greedy") %>% 
    left_join(., scores %>% select(.x, .y, score), by = c(".x", ".y"))
  
  bind_rows(linked_mn, linked_others)
}

match_psp_sen_panel <- function(d1, d2, blocking_vars = c("first_name", "last_name")){
  d1 <- d1 %>% 
    mutate(across(c(titles), ~if_else(is.na(.x), "", .x)))
  d2 <- d2 %>% 
    mutate(across(c(titles), ~if_else(is.na(.x), "", .x)))
  
  pairs <- pair_blocking(d1, d2, blocking_vars)
  pairs <- compare_pairs(pairs, on = c("first_name", "last_name", "birth_year", 
                                       "titles", "party_membership_abbv", 
                                       "nominating_party_abbv", "residence_name", "occupation"), 
                         comparators = list(
                           occupation = cmp_jaccard(), 
                           titles = cmp_jaccard(), 
                           residence_name = cmp_jaccard()
                           # last_name = cmp_last_names()
                         ))
  
  scores <- score_simple(pairs, "score", 
                         on = c("first_name", "last_name", "birth_year", 
                                "titles", "party_membership_abbv", 
                                "nominating_party_abbv", "residence_name", "occupation"), 
                         w1 = c(first_name = 2, last_name = 2, birth_year = 2, 
                                titles = 0.5, 
                                party_membership_abbv = 0.5, nominating_party_abbv = 0.5, 
                                residence_name = 0.5, occupation = 0.5),
                         w0 = c(first_name = -5, last_name = -5, birth_year = -5,
                                titles = -0.25, 
                                party_membership_abbv = 0, nominating_party_abbv = 0, 
                                residence_name = -0.25, occupation = 0), 
                         wna = 0)
  
  selected_pairs_greedy <- select_greedy(scores, variable = "greedy", score = "score", 
                                         threshold = 6.25)
  
  link(selected_pairs_greedy, selection = "greedy") %>% 
    left_join(., scores %>% select(.x, .y, score), by = c(".x", ".y"))
}

insert_nonconsecutive <- function(pivot_table, noncons, source, target){
  noncons <- as.data.frame(noncons) 
  pivot_table <- as.data.frame(pivot_table)
  running <- noncons %>% 
    arrange(.data[[source]])
  pivot_table <- pivot_table %>% arrange(., .data[[source]])
  pivot_table[pivot_table[[source]] %in% running[[source]], target] <- running[[target]]
  pivot_table
}

rename_variables <- function(df){
  vars_list <- c(
    election_date = "DATUMVOLEB",
    candidate_name = "JMENO",
    candidate_surname = "PRIJMENI",
    candidate_age = "VEK",
    candidate_birthyear = "ROK_NAROZENI",
    candidate_gender = "SEX",
    candidate_title_before = "TITULPRED",
    candidate_title_after = "TITULZA",
    candidate_title_both = "TITULY",
    candidate_education = "TITUL_KATEGORIE",
    candidate_occupation = "POVOLANI",
    candidate_place_name = "BYDLISTEN",
    candidate_place_code = "BYDLISTEK",
    candidate_ranking = "PORCISLO",
    candidate_partymem_code = "PSTRANA",
    candidate_partynom_code = "NSTRANA",
    candidate_partyrun_code = "KSTRANA",
    candidate_partynom_name = "ZKRATKAN8",
    candidate_partymem_name = "ZKRATKAP8",
    candidate_partyrun_name = "ZKRATKAK8",
    candidate_validity = "PLATNOST",
    candidate_voteN = "POCHLASU",
    candidate_voteP = "POCPROCVSE",
    candidate_voteP = "POCPROC",
    candidate_seat = "MANDAT",
    candidate_ranking_seat = "PORADIMAND",
    candidate_ranking_subs = "PORADINAHR",
    candidate_citizenship = "STATOBCAN",
    candidate_voteN_SR1 = "HLASY_K1",
    candidate_voteN_SR2 = "HLASY_K2",
    candidate_voteP_SR1 = "URIZ_PR_K1",
    candidate_voteP_SR2 = "URIZ_PR_K2",
    candidate_partyrun_fullname = "NAZEV_STRK",
    candidate_partyrun_fullname = "NAZEV_STRV",
    candidate_partyrun_fullname = "NAZEVVS",
    candidate_partyrun_fullname = "NAZEVCELK",
    candidate_partyrun_fullname = "NAZEV_VS",
    
    party_rank = "POR_STR_HL",
    
    electoral_region = "VOLKRAJ",
    region_name = "KRAJ_NAZEV",
    
    region_code = "KRZAST",
    municipality_id = "KODZASTUP", 
    municipality_name = "KODZASTUP_NAZEV",
    municipality_type = "TYPZASTUP",
    electoral_district_no = "COBVODU",
    
    senate_district = "OBVOD",
    senate_candidate_no = "CKAND"
  )
  
  rename(df, any_of(vars_list))
}
