library(targets)
library(reclin2)
library(dplyr)

tar_load(matches("ep"))

cmp_within_1 <- function(){
  function(x, y){
    abs(x-y) <= 1
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

# FIXME: nonconsecutive candidates
# FIXME: one-time candidates
insert_nonconsecutive <- function(pivot_table, noncons, source, target){
  noncons <- as.data.frame(noncons) 
  pivot_table <- as.data.frame(pivot_table)
  last_column <- ncol(noncons)
  running <- noncons[!is.na(noncons[, last_column]), ] %>% 
    arrange(.data[[source]])
  out <- pivot_table %>% arrange(., .data[[source]])
  pivot_table[pivot_table[[source]] %in% running[[source]], target] <- running[[target]]
  pivot_table
}

ep04_14 <- ep04_14 %>% select(row_id_2004 = row_id.x, row_id_2014 = row_id.y)
ep04_19 <- ep04_19 %>% select(row_id_2004 = row_id.x, row_id_2019 = row_id.y)
ep04_24 <- ep04_24 %>% select(row_id_2004 = row_id.x, row_id_2024 = row_id.y)
ep09_19 <- ep09_19 %>% select(row_id_2009 = row_id.x, row_id_2019 = row_id.y)
ep09_24 <- ep09_24 %>% select(row_id_2009 = row_id.x, row_id_2024 = row_id.y)
ep14_24 <- ep14_24 %>% select(row_id_2014 = row_id.x, row_id_2024 = row_id.y)

pivot_table <- ep04_09 %>% select(row_id_2004 = row_id.x, row_id_2009 = row_id.y) %>% 
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

ep_panel <- full_join(pivot_table_long, ep_candidates, by = c("row_id", "year")) 
nrow(ep_panel)
nrow(ep_2004) + nrow(ep_2009) + nrow(ep_2014) + nrow(ep_2019) + nrow(ep_2024)
