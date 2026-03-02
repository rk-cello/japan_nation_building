pacman::p_load("tidyverse", "ggplot2")

# Load the data
load("data/raw/clea_lc_20251015.RData")


# Lower house 1947-1953 ####
clea <- clea_lc_20251015 %>% 
  filter(ctr_n == "Japan" & yr <= 1953 & yr >= 1945) 

# NOTES: sum(clea$cv2 != -990)
# [1] 0
# All elections didn't have runoff election. Can ignore second round variables. 

clea_2 <- clea %>% 
  select(id, yr, mn, cst_n, cst, mag, pty_n, pty, cv1, cvs1, seat) %>% 
  mutate(pref = substr(cst_n, 1, nchar(cst_n)-2)) %>% 
  select(pref, cst_n, everything()) 

clea_3 <- clea_2 %>% 
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

  
