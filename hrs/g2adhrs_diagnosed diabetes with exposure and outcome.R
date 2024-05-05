gc();rm(list=ls());source(".Rprofile")

library(srvyr)

hrs_all_exposure_available <- readRDS(paste0(path_g2a_depression_folder,"/working/g2adh01_analytic dataset.RDS")) %>% 
  dplyr::filter(!is.na(cesd)) %>% 
  mutate(followup = case_when(is.na(fu_control_joint) ~ 0,
                              TRUE ~ 1))

hrs_all <- hrs_all_exposure_available %>% 
  dplyr::filter(!is.na(fu_control_joint))

# Assessing counts ---------
hrs_all_exposure_available %>% 
  group_by(gender) %>%
  tally()

hrs_all_exposure_available %>% 
  distinct(hhidpn,gender) %>% 
  # group_by(gender) %>%
  tally()

hrs_all %>% 
  # group_by(gender) %>%
  tally()

hrs_all %>% 
  distinct(hhidpn,gender) %>% 
  # group_by(gender) %>%
  tally()

# Survey design ------------
hrs_all_diagnosed_svy <- hrs_all %>% 
  as_survey_design(ids = psu,
                   strata  = strata,
                   weight = normalizedweight,
                   nest = TRUE,
                   variance = "YG")

hrs_unique_df <- hrs_all %>% 
  group_by(hhidpn) %>% 
  dplyr::filter(wave == min(wave)) %>% 
  ungroup() 

hrs_unique_df %>% 
  group_by(gender) %>% 
  tally()

hrs_unique_svy <- hrs_unique_df %>% 
  as_survey_design(ids = psu,
                   strata  = strata,
                   weight = normalizedweight,
                   nest = TRUE,
                   variance = "YG")
