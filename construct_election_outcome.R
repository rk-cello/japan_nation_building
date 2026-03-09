# Construct election outcome data at both party and candidate levels

pacman::p_load("tidyverse", "ggplot2", "readxl")

# Load the data
load("data/raw/clea_lc_20251015.RData")
election_outcome_1946 <- read_excel("data/dev/election_outcome_1946.xlsx")
election_seats_1946 <- read_excel("data/dev/election_seats_1946.xlsx")
clea_candidate_names <- read_excel("data/raw/clea_20251015_candidate_names.xlsx", sheet = "Japan 1947-1993")
candidate_info <- read_csv("data/raw/Reed-Smith-JHRED-CANDIDATES.csv")

##### Party level election outcome ######
# Lower house 1947-1953 ####
clea <- clea_lc_20251015 %>% 
  filter(ctr_n == "Japan" & yr <= 1953 & yr >= 1945) 

# NOTES: sum(clea$cv2 != -990)
# [1] 0
# All elections didn't have runoff election. Can ignore second round variables. 

clea_2 <- clea %>% 
  select(id, can, yr, mn, cst_n, cst, mag, pty_n, pty, cv1, cvs1, seat) %>% 
  mutate(pref = substr(cst_n, 1, nchar(cst_n)-2)) %>% 
  select(pref, cst_n, everything()) 

clea_3 <- clea_2 %>% 
  select(-can) %>% 
  group_by(yr, pref, pty_n, pty) %>% 
  summarise(
    total_candidate_vote = sum(cv1),
    total_seats = sum(seat)
    ) %>% 
  ungroup()

clea_3 <- clea_3 %>% 
  group_by(yr, pref) %>% 
  mutate(
    vote_share = total_candidate_vote / sum(total_candidate_vote),
    seat_share = total_seats / sum(total_seats)
  ) %>% 
  ungroup()

# clea_4 <- clea_3 %>% 
#   group_by(yr, pref) %>% 
#   mutate(
#     check = sum(vote_share)
#   ) %>% 
#   ungroup()


# Lower house 1946 ####
election_outcome_1946_2 <- election_outcome_1946 %>% 
  pivot_longer(cols = -c(pref), names_to = "pty_n", values_to = "total_candidate_vote") %>%
  mutate(yr = 1946) 

election_outcome_1946_2 <- election_outcome_1946_2 %>% 
  group_by(pref) %>% 
  mutate(
    vote_share = total_candidate_vote / sum(total_candidate_vote)
  ) %>% 
  ungroup()

election_seats_1946_2 <- election_seats_1946 %>% 
  pivot_longer(cols = -c(prefecture), names_to = "pty_n", values_to = "total_seats") %>%
  rename(pref = prefecture) %>%
  mutate(yr = 1946)

election_seats_1946_2 <- election_seats_1946_2 %>% 
  group_by(pref) %>% 
  mutate(
    seat_share = total_seats / sum(total_seats)
  ) %>% 
  ungroup()

election_outcome_1946_3 <- left_join(election_outcome_1946_2, election_seats_1946_2, by = c("yr", "pref", "pty_n"))
  

# Combine the data ####
lower_election_outcome <- bind_rows(
  election_outcome_1946_3 %>% select(yr, pref, pty_n, total_candidate_vote, vote_share, total_seats, seat_share),
  clea_3
)
write_csv(lower_election_outcome, "data/dev/lower_election_outcome.csv")


##### Candidate level election outcome ######
# Candidate Information (available 1947~) ####
candidate_names <- clea_candidate_names %>% 
  select(-ctr_n) %>% 
  mutate(can = as.character(can)) %>% 
  rename(can_kanji = "can - native language")

candidate_info_2 <- candidate_info %>% 
  filter(year >= 1945 & year <= 1953) %>% 
  select(name_jp, inc, year, party_jp, party_en, 
         prewruns, prewwins, # prewar elections
         run1946, win1946, # 1946 election
         depurgee:generation, # background of candidates
         cabappt:vicespeaker)  # post-electoral appointment variables 

candidate_level_outcome <- clea_2 %>% 
  select(yr, cst_n, pty, pty_n, can, cv1, cvs1, seat) %>%
  rename(year = yr) %>% 
  left_join(candidate_names, by = c("can")) %>% 
  left_join(candidate_info_2, by = c("year", "can_kanji" = "name_jp")) 

write_csv(candidate_level_outcome, "data/dev/candidate_level_outcome.csv")
  

##### Combine with purge data #####
# 作業を続ける前に、1946年と1947年のパージデータを区別する必要がある
# outcome: election result for shimpoto in 1946を使うなら尚更

pty_lower_house_df <- df_purge_map %>% 
  mutate(prefecture = tolower(prefecture)) %>%
  left_join(lower_election_outcome, by = c("prefecture" = "pref"))


  
