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
        grepl("Ing\\.", JMENO) ~ "Ing."
      ), 
      JMENO = gsub(
        "^(Ing\\.JUDr\\.|Doc\\.Dr\\.|Doc\\.ing\\.|Mudr\\.|MVDr\\.|MUDr\\.|Mgr\\.|RNDr\\.|ThMgr\\.|PharmDr\\.|JUDr\\.|PhDr\\.|Paed\\sDr\\.|ing\\.|Dr\\.|PaedDr\\.|Ing\\.)", 
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
      "[,]*\\s(starší|st\\.|ml\\.|mladší|jr\\.|sen\\.|jun\\.|I\\.|II\\.|ST\\.|ML\\.|Ml\\.)$", "", PRIJMENI) %>% 
        gsub(",(ml\\.|st\\.)", "", .) %>% 
        gsub(",[ ]*roč\\.[0-9]+", "", .) %>% 
        gsub("\\s\\((ml|mladší|st|starší)\\)", "", .))
}

categorize_sex <- function(df){
  df %>% 
    mutate(FULL_NAME = paste0(JMENO, " ", PRIJMENI), 
           SEX = get_sex(stringr::word(FULL_NAME))) %>% 
    select(-FULL_NAME)
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
    filter(TYPZASTUP == 2) %>% 
    left_join(., district_map, by = c("KODZASTUP"="CITY_DISTRICT"))
}

filter_municipalities <- function(df, district_map){
  df %>%
    filter(TYPZASTUP == 1) %>% 
    mutate(MUNICIPALITY = KODZASTUP)
}

cmp_within_1 <- function(){
  function(x, y){
    abs(x-y) <= 1
  }
}

# cmp_tmp <- function(x, y){
#   if(grepl("\\s|-", x) || grepl("\\s|-", y)){
#     x_split <- unlist(strsplit(x, "\\s|-"))
#     y_split <- unlist(strsplit(y, "\\s|-"))
#     any(x_split) %in% y_split
#   }else{
#     x == y
#   }
# }

cmp_last_names <- function(){
  function(x, y){
    if(grepl("\\s|-", x) || grepl("\\s|-", y)){
      x_split <- unlist(strsplit(gsub("\\(|\\)", "", x), "\\s|-"))
      y_split <- unlist(strsplit(gsub("\\(|\\)", "", y), "\\s|-"))
      any(x_split %in% y_split)
    }else{
      x == y
    }
  }
}

match_data <- function(d1, d2, blocking_vars = c("JMENO", "PRIJMENI")){
  d1 <- d1 %>% 
    mutate(across(c(TITULPRED, TITULZA), ~if_else(is.na(.x), "", .x)))
  d2 <- d2 %>% 
    mutate(across(c(TITULPRED, TITULZA), ~if_else(is.na(.x), "", .x)))
  
  pairs <- pair_blocking(d1, d2, blocking_vars)
  pairs <- compare_pairs(pairs, on = c("JMENO", "PRIJMENI", "ROK_NAROZENI", 
                                       "TITULPRED", "TITULZA", "ZKRATKAP8", 
                                       "ZKRATKAN8", "BYDLISTEN", "POVOLANI"), 
                         comparators = list(
                           ROK_NAROZENI = cmp_within_1(),
                           POVOLANI = cmp_jaccard()
                           # PRIJMENI = cmp_last_names()
                           # TODO: stejný nebo větší titul
                         ))
  
  scores <- score_simple(pairs, "score", 
                         on = c("JMENO", "PRIJMENI", "ROK_NAROZENI", 
                                "TITULPRED", "TITULZA", "ZKRATKAP8", 
                                "ZKRATKAN8", "BYDLISTEN", "POVOLANI"), 
                         w1 = c(JMENO = 2, PRIJMENI = 2, ROK_NAROZENI = 2, 
                                TITULPRED = 0.5, TITULZA = 0.5, 
                                ZKRATKAP8 = 0.5, ZKRATKAN8 = 0.5, 
                                BYDLISTEN = 0.5, POVOLANI = 0.5),
                         w0 = c(JMENO = -5, PRIJMENI = -5, ROK_NAROZENI = -5,
                                TITULPRED = -0.5, TITULZA = -0.5, 
                                ZKRATKAP8 = 0, ZKRATKAN8 = 0, 
                                BYDLISTEN = -0.5, POVOLANI = 0), 
                         wna = 0)
  
  selected_pairs_greedy <- select_greedy(scores, variable = "greedy", score = "score", threshold = 6)
  
  link(selected_pairs_greedy, selection = "greedy")
}

match_reg_data <- function(d1, d2, blocking_vars = c("JMENO", "PRIJMENI", "KRZAST")){
  d1 <- d1 %>% 
    mutate(across(c(TITULPRED, TITULZA), ~if_else(is.na(.x), "", .x)))
  d2 <- d2 %>% 
    mutate(across(c(TITULPRED, TITULZA), ~if_else(is.na(.x), "", .x)))
  
  pairs <- pair_blocking(d1, d2, blocking_vars)
  pairs <- compare_pairs(pairs, on = c("JMENO", "PRIJMENI", "ROK_NAROZENI", 
                                       "TITULPRED", "TITULZA", "ZKRATKAP8", 
                                       "ZKRATKAN8", "BYDLISTEN", "POVOLANI"), 
                         comparators = list(
                           ROK_NAROZENI = cmp_within_1(),
                           POVOLANI = cmp_jaccard() 
                           # PRIJMENI = cmp_last_names()
                           # TODO: stejný nebo větší titul
                         ))
  
  scores <- score_simple(pairs, "score", 
                         on = c("JMENO", "PRIJMENI", "ROK_NAROZENI", 
                                "TITULPRED", "TITULZA", "ZKRATKAP8", 
                                "ZKRATKAN8", "BYDLISTEN", "POVOLANI"), 
                         w1 = c(JMENO = 2, PRIJMENI = 2, ROK_NAROZENI = 2, 
                                TITULPRED = 0.5, TITULZA = 0.5, 
                                ZKRATKAP8 = 0.5, ZKRATKAN8 = 0.5, 
                                BYDLISTEN = 0.5, POVOLANI = 0.5),
                         w0 = c(JMENO = -5, PRIJMENI = -5, ROK_NAROZENI = -5,
                                TITULPRED = -0.5, TITULZA = -0.5, 
                                ZKRATKAP8 = 0, ZKRATKAN8 = 0, 
                                BYDLISTEN = -0.5, POVOLANI = 0), 
                         wna = 0)
  
  selected_pairs_greedy <- select_greedy(scores, variable = "greedy", score = "score", threshold = 6)
  
  link(selected_pairs_greedy, selection = "greedy")
}

match_psp_data <- function(d1, d2, blocking_vars = c("JMENO", "PRIJMENI")){
  d1 <- d1 %>% 
    mutate(across(c(TITULPRED, TITULZA), ~if_else(is.na(.x), "", .x)))
  d2 <- d2 %>% 
    mutate(across(c(TITULPRED, TITULZA), ~if_else(is.na(.x), "", .x)))
  
  pairs <- pair_blocking(d1, d2, blocking_vars)
  pairs <- compare_pairs(pairs, on = c("JMENO", "PRIJMENI", "ROK_NAROZENI", 
                                       "TITULPRED", "TITULZA", "ZKRATKAP8", 
                                       "ZKRATKAN8", "BYDLISTEN", "POVOLANI", 
                                       "KRAJ_NAZEV"), 
                         comparators = list(
                           ROK_NAROZENI = cmp_within_1(),
                           POVOLANI = cmp_jaccard()
                           # PRIJMENI = cmp_last_names()
                           # TODO: stejný nebo větší titul
                         ))
  
  scores <- score_simple(pairs, "score", 
                         on = c("JMENO", "PRIJMENI", "ROK_NAROZENI", 
                                "TITULPRED", "TITULZA", "ZKRATKAP8", 
                                "ZKRATKAN8", "BYDLISTEN", "POVOLANI", 
                                "KRAJ_NAZEV"), 
                         w1 = c(JMENO = 2, PRIJMENI = 2, ROK_NAROZENI = 2, 
                                TITULPRED = 0.5, TITULZA = 0.5, 
                                ZKRATKAP8 = 0.5, ZKRATKAN8 = 0.5, 
                                BYDLISTEN = 0.5, POVOLANI = 0.5, 
                                KRAJ_NAZEV = 0.5),
                         w0 = c(JMENO = -5, PRIJMENI = -5, ROK_NAROZENI = -5,
                                TITULPRED = -0.5, TITULZA = -0.5, 
                                ZKRATKAP8 = 0, ZKRATKAN8 = 0, 
                                BYDLISTEN = -0.5, POVOLANI = 0, 
                                KRAJ_NAZEV = 0), 
                         wna = 0)
  
  selected_pairs_greedy <- select_greedy(scores, variable = "greedy", score = "score", threshold = 6)
  
  link(selected_pairs_greedy, selection = "greedy")
}

match_mun_data <- function(d1, d2, blocking_vars = c("JMENO", "PRIJMENI", "KODZASTUP")){
  d1 <- d1 %>% 
    mutate(across(c(TITULY), ~if_else(is.na(.x), "", .x)))
  d2 <- d2 %>% 
    mutate(across(c(TITULY), ~if_else(is.na(.x), "", .x)))
  
  pairs <- pair_blocking(d1, d2, blocking_vars)
  pairs <- compare_pairs(pairs, on = c("JMENO", "PRIJMENI", "ROK_NAROZENI", 
                                       "TITULY", "ZKRATKAP8", 
                                       "ZKRATKAN8", "BYDLISTEN", "POVOLANI"), 
                         comparators = list(
                           ROK_NAROZENI = cmp_within_1(),
                           POVOLANI = cmp_jaccard(), 
                           TITULY = cmp_jaccard(), 
                           BYDLISTEN = cmp_jaccard()
                           # PRIJMENI = cmp_last_names()
                           # TODO: stejný nebo větší titul
                         ))
  
  scores <- score_simple(pairs, "score", 
                         on = c("JMENO", "PRIJMENI", "ROK_NAROZENI", 
                                "TITULY", "ZKRATKAP8", 
                                "ZKRATKAN8", "BYDLISTEN", "POVOLANI"), 
                         w1 = c(JMENO = 2, PRIJMENI = 2, ROK_NAROZENI = 2, 
                                TITULY = 0.5, 
                                ZKRATKAP8 = 0.5, ZKRATKAN8 = 0.5, 
                                BYDLISTEN = 0.5, POVOLANI = 0.5),
                         w0 = c(JMENO = -5, PRIJMENI = -5, ROK_NAROZENI = -5,
                                TITULY = -0.25, 
                                ZKRATKAP8 = 0, ZKRATKAN8 = 0, 
                                BYDLISTEN = -0.25, POVOLANI = 0), 
                         wna = 0)
  
  selected_pairs_greedy <- select_greedy(scores, variable = "greedy", score = "score", 
                                         threshold = 6)
  
  link(selected_pairs_greedy, selection = "greedy")
}

match_sen_data <- function(d1, d2, blocking_vars = c("JMENO", "PRIJMENI")){
  d1 <- d1 %>% 
    mutate(across(c(TITULPRED, TITULZA), ~if_else(is.na(.x), "", .x)))
  d2 <- d2 %>% 
    mutate(across(c(TITULPRED, TITULZA), ~if_else(is.na(.x), "", .x)))
  
  pairs <- pair_blocking(d1, d2, blocking_vars)
  pairs <- compare_pairs(pairs, on = c("JMENO", "PRIJMENI", "ROK_NAROZENI", 
                                       "TITULPRED", "TITULZA", "ZKRATKAP8", 
                                       "ZKRATKAN8", "BYDLISTEN", "POVOLANI", 
                                       "OBVOD"), 
                         comparators = list(
                           ROK_NAROZENI = cmp_within_1(),
                           POVOLANI = cmp_jaccard()
                           # PRIJMENI = cmp_last_names()
                           # TODO: stejný nebo větší titul
                         ))
  
  scores <- score_simple(pairs, "score", 
                         on = c("JMENO", "PRIJMENI", "ROK_NAROZENI", 
                                "TITULPRED", "TITULZA", "ZKRATKAP8", 
                                "ZKRATKAN8", "BYDLISTEN", "POVOLANI", 
                                "OBVOD"), 
                         w1 = c(JMENO = 2, PRIJMENI = 2, ROK_NAROZENI = 2, 
                                TITULPRED = 0.5, TITULZA = 0.5, 
                                ZKRATKAP8 = 0.5, ZKRATKAN8 = 0.5, 
                                BYDLISTEN = 0.5, POVOLANI = 0.5, 
                                OBVOD = 2),
                         w0 = c(JMENO = -5, PRIJMENI = -5, ROK_NAROZENI = -5,
                                TITULPRED = -0.5, TITULZA = -0.5, 
                                ZKRATKAP8 = 0, ZKRATKAN8 = 0, 
                                BYDLISTEN = -0.5, POVOLANI = 0, 
                                OBVOD = 0), 
                         wna = 0)
  
  selected_pairs_greedy <- select_greedy(scores, variable = "greedy", score = "score", threshold = 6)
  
  link(selected_pairs_greedy, selection = "greedy")
}

match_mun_panel_data <- function(d1, d2, blocking_vars = c("JMENO", "PRIJMENI", "KODZASTUP")){
  d1 <- d1 %>% 
    mutate(across(c(TITULY), ~if_else(is.na(.x), "", .x)))
  d2 <- d2 %>% 
    mutate(across(c(TITULY), ~if_else(is.na(.x), "", .x)))
  
  pairs <- pair_blocking(d1, d2, blocking_vars)
  pairs <- compare_pairs(pairs, on = c("JMENO", "PRIJMENI", "ROK_NAROZENI", 
                                       "TITULY", "ZKRATKAP8", 
                                       "ZKRATKAN8", "BYDLISTEN", "POVOLANI"), 
                         comparators = list(
                           POVOLANI = cmp_jaccard(), 
                           TITULY = cmp_jaccard(), 
                           BYDLISTEN = cmp_jaccard()
                           # PRIJMENI = cmp_last_names()
                           # TODO: stejný nebo větší titul
                         ))
  
  scores <- score_simple(pairs, "score", 
                         on = c("JMENO", "PRIJMENI", "ROK_NAROZENI", 
                                "TITULY", "ZKRATKAP8", 
                                "ZKRATKAN8", "BYDLISTEN", "POVOLANI"), 
                         w1 = c(JMENO = 2, PRIJMENI = 2, ROK_NAROZENI = 2, 
                                TITULY = 0.5, 
                                ZKRATKAP8 = 0.5, ZKRATKAN8 = 0.5, 
                                BYDLISTEN = 0.5, POVOLANI = 0.5),
                         w0 = c(JMENO = -5, PRIJMENI = -5, ROK_NAROZENI = -5,
                                TITULY = -0.25, 
                                ZKRATKAP8 = 0, ZKRATKAN8 = 0, 
                                BYDLISTEN = -0.25, POVOLANI = 0), 
                         wna = 0)
  
  selected_pairs_greedy <- select_greedy(scores, variable = "greedy", score = "score", 
                                         threshold = 6)
  
  link(selected_pairs_greedy, selection = "greedy") %>% 
    left_join(., scores %>% select(.x, .y, score), by = c(".x", ".y"))
}

match_mun_reg_panel <- function(d1, d2, blocking_vars = c("JMENO", "PRIJMENI", "KRAJ_NAZEV")){
  d1 <- d1 %>% 
    mutate(across(c(TITULY), ~if_else(is.na(.x), "", .x)))
  d2 <- d2 %>% 
    mutate(across(c(TITULY), ~if_else(is.na(.x), "", .x)))
  
  pairs <- pair_blocking(d1, d2, blocking_vars)
  pairs <- compare_pairs(pairs, on = c("JMENO", "PRIJMENI", "ROK_NAROZENI", 
                                       "TITULY", "ZKRATKAP8", 
                                       "ZKRATKAN8", "BYDLISTEN", "POVOLANI", 
                                       "KRAJ_NAZEV"), 
                         comparators = list(
                           POVOLANI = cmp_jaccard(), 
                           TITULY = cmp_jaccard(), 
                           BYDLISTEN = cmp_jaccard()
                           # PRIJMENI = cmp_last_names()
                         ))
  
  scores <- score_simple(pairs, "score", 
                         on = c("JMENO", "PRIJMENI", "ROK_NAROZENI", 
                                "TITULY", "ZKRATKAP8", 
                                "ZKRATKAN8", "BYDLISTEN", "POVOLANI", 
                                "KRAJ_NAZEV"), 
                         w1 = c(JMENO = 2, PRIJMENI = 2, ROK_NAROZENI = 2, 
                                TITULY = 0.5, 
                                ZKRATKAP8 = 0.5, ZKRATKAN8 = 0.5, 
                                BYDLISTEN = 0.5, POVOLANI = 0.5, 
                                KRAJ_NAZEV = 1),
                         w0 = c(JMENO = -5, PRIJMENI = -5, ROK_NAROZENI = -5,
                                TITULY = -0.25, 
                                ZKRATKAP8 = 0, ZKRATKAN8 = 0, 
                                BYDLISTEN = -0.25, POVOLANI = 0, 
                                KRAJ_NAZEV = -0.25), 
                         wna = 0)
  
  selected_pairs_greedy <- select_greedy(scores, variable = "greedy", score = "score", 
                                         threshold = 6)
  
  link(selected_pairs_greedy, selection = "greedy") %>% 
    left_join(., scores %>% select(.x, .y, score), by = c(".x", ".y"))
}

match_mr_psp_panel <- function(d1, d2, blocking_vars = c("JMENO", "PRIJMENI")){
  d1 <- d1 %>% 
    mutate(across(c(TITULY), ~if_else(is.na(.x), "", .x)))
  d2 <- d2 %>% 
    mutate(across(c(TITULY), ~if_else(is.na(.x), "", .x)))
  
  pairs <- pair_blocking(d1, d2, blocking_vars)
  pairs <- compare_pairs(pairs, on = c("JMENO", "PRIJMENI", "ROK_NAROZENI", 
                                       "TITULY", "ZKRATKAP8", 
                                       "ZKRATKAN8", "BYDLISTEN", "POVOLANI", 
                                       "KRAJ_NAZEV"), 
                         comparators = list(
                           POVOLANI = cmp_jaccard(), 
                           TITULY = cmp_jaccard(), 
                           BYDLISTEN = cmp_jaccard()
                           # PRIJMENI = cmp_last_names()
                         ))
  
  scores <- score_simple(pairs, "score", 
                         on = c("JMENO", "PRIJMENI", "ROK_NAROZENI", 
                                "TITULY", "ZKRATKAP8", 
                                "ZKRATKAN8", "BYDLISTEN", "POVOLANI", 
                                "KRAJ_NAZEV"), 
                         w1 = c(JMENO = 2, PRIJMENI = 2, ROK_NAROZENI = 2, 
                                TITULY = 0.5, 
                                ZKRATKAP8 = 0.5, ZKRATKAN8 = 0.5, 
                                BYDLISTEN = 0.5, POVOLANI = 0.5, 
                                KRAJ_NAZEV = 1),
                         w0 = c(JMENO = -5, PRIJMENI = -5, ROK_NAROZENI = -5,
                                TITULY = -0.25, 
                                ZKRATKAP8 = 0, ZKRATKAN8 = 0, 
                                BYDLISTEN = -0.25, POVOLANI = 0, 
                                KRAJ_NAZEV = -0.25), 
                         wna = 0)
  
  selected_pairs_greedy <- select_greedy(scores, variable = "greedy", score = "score", 
                                         threshold = 6.25)
  
  link(selected_pairs_greedy, selection = "greedy") %>% 
    left_join(., scores %>% select(.x, .y, score), by = c(".x", ".y"))
}

match_psp_sen_panel <- function(d1, d2, blocking_vars = c("JMENO", "PRIJMENI")){
  d1 <- d1 %>% 
    mutate(across(c(TITULY), ~if_else(is.na(.x), "", .x)))
  d2 <- d2 %>% 
    mutate(across(c(TITULY), ~if_else(is.na(.x), "", .x)))
  
  pairs <- pair_blocking(d1, d2, blocking_vars)
  pairs <- compare_pairs(pairs, on = c("JMENO", "PRIJMENI", "ROK_NAROZENI", 
                                       "TITULY", "ZKRATKAP8", 
                                       "ZKRATKAN8", "BYDLISTEN", "POVOLANI"), 
                         comparators = list(
                           POVOLANI = cmp_jaccard(), 
                           TITULY = cmp_jaccard(), 
                           BYDLISTEN = cmp_jaccard()
                           # PRIJMENI = cmp_last_names()
                         ))
  
  scores <- score_simple(pairs, "score", 
                         on = c("JMENO", "PRIJMENI", "ROK_NAROZENI", 
                                "TITULY", "ZKRATKAP8", 
                                "ZKRATKAN8", "BYDLISTEN", "POVOLANI"), 
                         w1 = c(JMENO = 2, PRIJMENI = 2, ROK_NAROZENI = 2, 
                                TITULY = 0.5, 
                                ZKRATKAP8 = 0.5, ZKRATKAN8 = 0.5, 
                                BYDLISTEN = 0.5, POVOLANI = 0.5),
                         w0 = c(JMENO = -5, PRIJMENI = -5, ROK_NAROZENI = -5,
                                TITULY = -0.25, 
                                ZKRATKAP8 = 0, ZKRATKAN8 = 0, 
                                BYDLISTEN = -0.25, POVOLANI = 0), 
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
