gc();rm(list=ls());source(".Rprofile")

library(srvyr)

elsa_all_exposure_available <- readRDS(paste0(path_g2a_depression_folder,"/working/g2ade02_analytic dataset.RDS"))  %>% 
  dplyr::filter(!is.na(cesd)) %>% 
  mutate(followup = case_when(is.na(fu_control_joint) ~ 0,
                              TRUE ~ 1))

elsa_all <- elsa_all_exposure_available %>% 
  dplyr::filter(!is.na(fu_control_joint))


# Assessing counts ---------
elsa_all_exposure_available %>% 
  # group_by(gender) %>%
  tally()

elsa_all_exposure_available %>% 
  distinct(personid,gender) %>% 
  # group_by(gender) %>%
  tally()

elsa_all %>% 
  # group_by(gender) %>%
  tally()

elsa_all %>% 
  distinct(personid,gender) %>% 
  # group_by(gender) %>%
  tally()

# Survey design ----------

elsa_all_diagnosed_svy <- elsa_all %>% 
  as_survey_design(
    # ids = psu,
    strata  = strata,
    weight = normalizedweight,
    nest = FALSE,
    variance = "YG")


# elsa_biomarkers_diagnosed %>% 
#   dplyr::filter(!is.na(diagnosed_dm)) %>% 
#   group_by(wave,age_category) %>% 
#   dplyr::summarize(m = mean(fall_any,na.rm=TRUE),
#             n = n(),
#             na_count = sum(is.na(fall_any))) %>% 
#   View()

elsa_unique_df <- elsa_all %>% 
  group_by(personid) %>% 
  dplyr::filter(wave == min(wave)) %>% 
  ungroup() 

# elsa_unique_df %>% 
#   group_by(gender) %>% 
#   tally()

elsa_unique_svy <- elsa_unique_df %>% 
  as_survey_design(ids = psu,
                   strata  = strata,
                   weight = normalizedweight,
                   nest = TRUE,
                   variance = "YG")

