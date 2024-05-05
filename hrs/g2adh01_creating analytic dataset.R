gc();rm(list=ls());source(".Rprofile")

g2adh01_processing <- function(df){
  
  df %>% 
    mutate(totalwordrecall = immediatewordrecall + delayedwordrecall) %>% 
    
    mutate(correct_countbackwards = case_when(countbackwards %in% c(1,2) ~ 1,
                                              countbackwards == 0 ~ 0,
                                              TRUE ~ NA_real_),
           correct_serial7subtract = case_when(serial7subtract %in% c(3:5) ~ 1,
                                               serial7subtract %in% c(0:2) ~ 0,
                                               TRUE ~ NA_real_),
           correct_immediatewordrecall = case_when(immediatewordrecall %in% c(5:10) ~ 1,
                                                   immediatewordrecall %in% c(0:4) ~ 0,
                                                   TRUE ~ NA_real_),
           correct_delayedwordrecall = case_when(delayedwordrecall %in% c(4:10) ~ 1,
                                                 delayedwordrecall %in% c(0:3) ~ 0,
                                                 TRUE ~ NA_real_)
    ) %>% 
    mutate(
      composite_cognition = rowSums(.[,c("correct_immediatewordrecall","correct_delayedwordrecall",
                                         "correct_countbackwards","correct_serial7subtract")],na.rm=TRUE),
      valid_cognition = rowSums(!is.na(.[,c("correct_immediatewordrecall","correct_delayedwordrecall",
                                            "correct_countbackwards","correct_serial7subtract")]),na.rm=TRUE),
      lengthmar = case_when(married == 0 ~ 0,
                            TRUE ~ lengthmar)) %>% 
    mutate(composite_cognition_impairment = case_when(valid_cognition < 4 ~ NA_real_,
                                                      composite_cognition %in% c(3,4) ~ 0,
                                                      composite_cognition %in% c(0:2) ~ 1,
                                                      TRUE ~ NA_real_)) %>% 
    
    mutate(polypharmacy = rowSums(.[(c("medication_bp","rxdiabi","rxdiabo",
                                       "rxstrok",
                                       "rxangina",
                                       "rxchf",
                                       "rxlung",
                                       "rxcancr",
                                       "rxhrtat",
                                       "rxheart",
                                       "rxhchol",
                                       "rxbreath",
                                       "rxstomach",
                                       "rxdepres"))],na.rm=TRUE),
           vision_impairment = case_when(sight_selfrated %in% c(5:6) ~ 1,
                                         sight_near %in% c(5:6) ~ 1,
                                         sight_selfrated %in% c(1:4) &
                                           sight_near %in% c(1:4) ~ 0,
                                         TRUE ~ NA_real_),
           balance_impairment = case_when(age <70 & fulltandem_seconds == 30 ~ 1,
                                          age >= 70 & fulltandem_seconds >= 10 ~ 1,
                                          !is.na(fulltandem_seconds) ~ 0,
                                          TRUE ~ NA_real_)) %>% 
    return(.)
  
}


analytic_depression <- function(baseline_wave = 13,followup_wave = 14){
  biomarkers <- readRDS(paste0(path_g2a_longitudinal_folder,"/working/hrs biomarkers.RDS")) 
  
  outcome_vars = c("age","a1c_adj","tc_adj","hdl_adj","sbp","dbp","polypharmacy","composite_cognition_impairment","iadl_some4")

  baseline <- bind_rows(
    readRDS(paste0(path_g2a_falls_folder,"/working/hrs/G2A HRS Falls Wave ",baseline_wave," male.RDS"))  %>% 
      dplyr::filter(!is.na(indsampleweight)),
    
    readRDS(paste0(path_g2a_falls_folder,"/working/hrs/G2A HRS Falls Wave ",baseline_wave," female.RDS"))  %>% 
      dplyr::filter(!is.na(indsampleweight))
    
  ) %>% 
    left_join(biomarkers %>% 
                dplyr::filter(wave == baseline_wave),
              by = c("hhid","pn")) %>% 
    g2adh01_processing(.)
  
  followup <- bind_rows(
    readRDS(paste0(path_g2a_falls_folder,"/working/hrs/G2A HRS Falls Wave ",followup_wave," male.RDS"))  %>% 
      dplyr::filter(!is.na(indsampleweight)),
    
    readRDS(paste0(path_g2a_falls_folder,"/working/hrs/G2A HRS Falls Wave ",followup_wave," female.RDS"))  %>% 
      dplyr::filter(!is.na(indsampleweight))
    
  ) %>% 
    left_join(biomarkers %>% 
                dplyr::filter(wave == followup_wave),
              by = c("hhid","pn")) %>% 
    dplyr::filter(wave == followup_wave) %>%
    g2adh01_processing(.) %>% 
    dplyr::select(hhid,pn,one_of(outcome_vars)) %>% 
    rename_with(~paste0("fu_",.x),one_of(outcome_vars))
  
  analytic_df = baseline %>% 
    left_join(followup,
              by=c("hhid","pn")) 
  
  return(analytic_df)
  
  
  
}

wave11 <- analytic_depression(11,13)
wave10 <- analytic_depression(10,12)
wave9 <- analytic_depression(9,11)
wave8 <- analytic_depression(8,10)

all_hrs <- bind_rows(wave8,
                     wave9,
                     wave10,
                     wave11) %>% 
  # blversion == 1 -->
  mutate(biowgtr = case_when(# wave == 13 & blversion == 1 ~ indsampleweight*2,
                             is.na(biowgtr) ~ indsampleweight*2,
                             TRUE ~ biowgtr)) 


hrs_with_cesd <- all_hrs %>% 
  dplyr::filter(!is.na(cesd),!is.na(blversion),age>=50,biowgtr > 0) %>% 
  group_by(wave) %>% 
  mutate(normalizedweight = biowgtr/sum(biowgtr)) %>% 
  mutate(normalizedweight = normalizedweight/n()) %>% 
  ungroup() %>% 
  mutate(normalizedweight = normalizedweight*n())  

hrs_with_cesd_diagnoseddm = hrs_with_cesd %>% 
  dplyr::filter(diagnosed_dm == 1) %>%  
  # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2585678/
  mutate(alzh_demen = case_when(wave == 8 & memoryproblem == 1 ~ 1,
                                alzh == 1 | dementia == 1 ~ 1,
                                TRUE ~ 0),
         cesd_ge3 = case_when(cesd >= 3 ~ 1,
                               TRUE ~ 0),
         
         cesd_ge5 = case_when(cesd >= 5 ~ 1,
                              TRUE ~ 0),
         
         cesd_ge6 = case_when(cesd >= 6 ~ 1,
                              TRUE ~ 0)
         )  %>% 
  mutate(nonhdl_adj = case_when(!is.na(tc_adj) & !is.na(hdl_adj) ~ tc_adj - hdl_adj,
                                TRUE ~ NA_real_),
         fu_nonhdl_adj = case_when(!is.na(fu_tc_adj) & !is.na(fu_hdl_adj) ~ fu_tc_adj - fu_hdl_adj,
                                TRUE ~ NA_real_)
         )     %>% 
  
  mutate(control_a1c = case_when(a1c_adj < 2.0 | a1c_adj >= 20.0 ~ NA_real_,
                                 age < 65 & a1c_adj < 7.0 ~ 1,
                                 age < 65 & a1c_adj >= 7.0 ~ 0,
                                 age >= 65 & polypharmacy >=3 & a1c_adj < 8.0 ~ 1,
                                 age >= 65 & iadl_some4 >= 2 & a1c_adj < 8.0 ~ 1,
                                 age >= 65 & composite_cognition_impairment == 1 & a1c_adj < 8.0 ~ 1,
                                 age>=65 & a1c_adj < 7.5 ~ 1,
                                 age>=65 & a1c_adj >= 7.5 ~ 0,
                                 TRUE ~ NA_real_),
         
         control_bp = case_when(sbp < 140 & dbp < 90 ~ 1,
                                sbp >= 140 | dbp >= 90 ~ 0,
                                TRUE ~ NA_real_),
         
         control_chol = case_when(nonhdl_adj <= 130 ~ 1,
                                  nonhdl_adj > 130 ~ 0,
                                  TRUE ~ NA_real_),
         
         control_joint = case_when(is.na(control_a1c) | is.na(control_bp) | is.na(control_chol) ~ NA_real_,
                                   control_a1c == 1 & control_bp == 1 & control_chol == 1 ~ 1,
                                   TRUE ~ 0),
         
         fu_control_a1c = case_when(fu_a1c_adj < 2.0 | fu_a1c_adj >= 20.0 ~ NA_real_,
                                    fu_age < 65 & fu_a1c_adj < 7.0 ~ 1,
                                    fu_age < 65 & fu_a1c_adj >= 7.0 ~ 0,
                                    fu_age >= 65 & fu_polypharmacy >=3 & fu_a1c_adj < 8.0 ~ 1,
                                    fu_age >= 65 & fu_iadl_some4 >= 2 & fu_a1c_adj < 8.0 ~ 1,
                                    fu_age >= 65 & fu_composite_cognition_impairment == 1 & fu_a1c_adj < 8.0 ~ 1,
                                    fu_age>=65 & fu_a1c_adj < 7.5 ~ 1,
                                    fu_age>=65 & fu_a1c_adj >= 7.5 ~ 0,
                                    TRUE ~ NA_real_),
         
         fu_control_bp = case_when(fu_sbp < 140 & fu_dbp < 90 ~ 1,
                                   fu_sbp >= 140 | fu_dbp >= 90 ~ 0,
                                   TRUE ~ NA_real_),
         
         fu_control_chol = case_when(fu_nonhdl_adj <= 130 ~ 1,
                                     fu_nonhdl_adj > 130 ~ 0,
                                     TRUE ~ NA_real_),
         
         fu_control_joint = case_when(is.na(fu_control_a1c) | is.na(fu_control_bp) | is.na(fu_control_chol) ~ NA_real_,
                                      fu_control_a1c == 1 & fu_control_bp == 1 & fu_control_chol == 1 ~ 1,
                                      TRUE ~ 0)
  )


hrs_with_cesd_diagnoseddm %>% 
  saveRDS(.,paste0(path_g2a_depression_folder,"/working/g2adh01_analytic dataset.RDS"))

# Checking counts -------


all_hrs %>% 
  # group_by(gender) %>%
  tally()

all_hrs %>% 
  distinct(hhidpn,gender) %>% 
  # group_by(gender) %>%
  tally()


hrs_with_cesd %>% 
  # group_by(gender) %>%
  tally()

hrs_with_cesd %>% 
  distinct(hhidpn,gender) %>% 
  # group_by(gender) %>%
  tally()


hrs_with_cesd_diagnoseddm %>% 
  # group_by(gender) %>%
  tally()

hrs_with_cesd_diagnoseddm %>% 
  distinct(hhidpn,gender) %>% 
  # group_by(gender) %>%
  tally()

