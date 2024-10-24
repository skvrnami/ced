library(dplyr)
library(readxl)
library(targets)

tar_load(complete_panel)

sk_cssd1998 <- read_excel("validation_data/sikk_koker/CSSD1998 & SZ2013.xlsx", 
                       sheet = "CSSD1998") %>% 
  mutate(
    sk_new = is.na(ID_p)
  )

cssd1998_panel <- complete_panel %>% 
  group_by(person_id) %>% 
  filter(election_type == "Chamber of Deputies") %>% 
  filter(any(election_year == 1998 &
               candidate_partynom_name == "ČSSD"))

ced_cssd1998 <- cssd1998_panel %>% 
  filter(election_year %in% c(1996, 1998)) %>% 
  group_by(person_id) %>% 
  mutate(lag_year = lag(election_year)) %>% 
  select(lag_year, election_year, candidate_name, candidate_surname, 
         candidate_ranking, candidate_validity) %>% 
  filter(election_year == 1998) %>% 
  mutate(
    candidate_fullname = stringi::stri_trans_general(tolower(paste(candidate_name, candidate_surname)), 
                                                     id = "Latin-ASCII"), 
    ced_new = is.na(lag_year)
  )
  
table(sk_cssd1998$sk_new)
table(ced_cssd1998$ced_new)
ced_cssd1998 %>% 
  ungroup %>% 
  filter(candidate_validity == 0) %>% 
  select(ced_new) %>% 
  table()

cssd1998_comparison <- full_join(
  ced_cssd1998 %>% 
    select(candidate_fullname, ced_new, candidate_validity),
  sk_cssd1998,
  by = c("candidate_fullname"="name")
) %>% 
  select(ced_new, sk_new, everything())

cssd1998_comparison %>% 
  # filter(ced_new != sk_new) %>% 
  writexl::write_xlsx(., "validation_data/sikk_koker/cssd_1998_comparison.xlsx")

sk_sz2013 <- read_excel("validation_data/sikk_koker/CSSD1998 & SZ2013.xlsx", 
                     sheet = "SZ2013") %>% 
  mutate(
    sk_new = is.na(ID_p)
  )

sz2013_panel <- complete_panel %>% 
  group_by(person_id) %>% 
  filter(election_type == "Chamber of Deputies") %>% 
  filter(any(election_year == 2013 &
               candidate_partynom_name == "SZ"))

ced_sz2013 <- sz2013_panel %>% 
  filter(election_year %in% c(2010, 2013)) %>% 
  group_by(person_id) %>% 
  mutate(lag_year = lag(election_year)) %>% 
  select(lag_year, election_year, candidate_name, candidate_surname, 
         candidate_ranking, candidate_validity) %>% 
  filter(election_year == 2013) %>% 
  mutate(
    candidate_fullname = stringi::stri_trans_general(tolower(paste(candidate_name, candidate_surname)), 
                                                     id = "Latin-ASCII"), 
    ced_new = is.na(lag_year)
  )

table(sk_sz2013$sk_new)
table(ced_sz2013$ced_new)
ced_sz2013 %>% 
  ungroup %>% 
  filter(candidate_validity == 0) %>% 
  select(ced_new) %>% 
  table()

sz2013_comparison <- full_join(
  ced_sz2013 %>% 
    select(candidate_fullname, ced_new, candidate_ranking, candidate_validity),
  sk_sz2013,
  by = c("candidate_fullname"="name", 
         "candidate_ranking"="distr_p")
) %>% 
  select(ced_new, sk_new, everything())

sz2013_comparison %>% 
  writexl::write_xlsx(., "validation_data/sikk_koker/sz_2013_comparison.xlsx")
  
