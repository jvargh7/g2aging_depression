gc();rm(list=ls());source(".Rprofile")

analytic_depression_elsa <- function(df,baseline_wave = 2, followup_wave = 4){
  
  
  biomarkers <- readRDS(paste0(path_g2a_longitudinal_folder,"/working/elsa biomarkers.RDS")) %>% 
    # Do not include wave 9 because we need to use it as the follow-up for wave8
    dplyr::filter(is.na(wavevisit)|wavevisit == 8)
  
  outcome_vars = c("age","hba1c","sbp","dbp","chol","ldl","hdl")
  
  baseline <- bind_rows(
    readRDS(paste0(path_g2a_falls_folder,"/working/elsa/G2A ELSA Falls Wave ",baseline_wave," male.RDS"))  %>% 
      dplyr::filter(sampleweight > 0),
    
    readRDS(paste0(path_g2a_falls_folder,"/working/elsa/G2A ELSA Falls Wave ",baseline_wave," female.RDS"))  %>% 
      dplyr::filter(sampleweight > 0)
    
  ) %>% 
    left_join(
      biomarkers %>% 
        dplyr::select(-year) %>% 
        dplyr::filter(wave == baseline_wave),
      by = c("personid")
      
    )
  
  followup <- bind_rows(
    readRDS(paste0(path_g2a_falls_folder,"/working/elsa/G2A ELSA Falls Wave ",followup_wave," male.RDS"))  %>% 
      dplyr::filter(sampleweight > 0),
    
    readRDS(paste0(path_g2a_falls_folder,"/working/elsa/G2A ELSA Falls Wave ",followup_wave," female.RDS"))  %>% 
      dplyr::filter(sampleweight > 0)
    
  ) %>% 
    left_join(
      biomarkers %>% 
        dplyr::select(-year) %>% 
        dplyr::filter(wave == followup_wave),
      by = c("personid")
      
    ) %>% 
    dplyr::select(personid,sampleweight,one_of(outcome_vars)) %>% 
    rename(fu_sampleweight = sampleweight) %>% 
    rename_with(~paste0("fu_",.x),one_of(outcome_vars))
  
  analytic_df = left_join(baseline,
                          followup,
                          by = c("personid"))
  
  return(analytic_df)
  
  
  
}


wave6 <- analytic_depression_elsa(6,8)
wave4 <- analytic_depression_elsa(4,6)
wave2 <- analytic_depression_elsa(2,4)


all_elsa <- bind_rows(wave2,
                      wave4,
                      wave6) 

elsa_with_cesd <- all_elsa %>% 
  dplyr::filter(!is.na(bldwt),!is.na(cesd),age>=50,bldwt > 0) %>% 
  group_by(wave) %>% 
  mutate(normalizedweight = bldwt/sum(bldwt)) %>% 
  mutate(normalizedweight = normalizedweight/n()) %>% 
  ungroup() %>% 
  mutate(normalizedweight = normalizedweight*n())  


elsa_with_cesd_diagnoseddm <- elsa_with_cesd %>% 
  mutate(
    # https://www.omnicalculator.com/health/cholesterol-units
    hdl = hdl*38.67,
    chol = chol*38.67,
    ldl = ldl*38.67,
    # https://heartcare.sydney/cholesterol-unit-conversion/
    trig = trig*88.57) %>% 
  dplyr::filter(diagnosed_dm == 1) %>%  
  # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2585678/
  mutate(cesd_ge3 = case_when(cesd >= 3 ~ 1,
                              TRUE ~ 0),
         
         cesd_ge5 = case_when(cesd >= 5 ~ 1,
                              TRUE ~ 0),
         
         cesd_ge6 = case_when(cesd >= 6 ~ 1,
                              TRUE ~ 0)
  ) %>%  
  mutate(iadl_any4 = case_when(iadl_any4 >= 0 ~ iadl_any4,
                               TRUE ~ NA_real_),
         fall_any  = case_when(fall_any >= 0 ~ as.numeric(fall_any),
                               TRUE ~ NA_real_),
         correct_countbackwards = case_when(countbackwards %in% c(1,2) ~ 1,
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
                                          "correct_countbackwards","correct_serial7subtract")]),na.rm=TRUE)) %>% 
  mutate(composite_cognition_impairment = case_when(wave %in% c(8,9) & valid_cognition < 4 ~ NA_real_,
                                                    wave %in% c(2,4,6) & valid_cognition < 2 ~ NA_real_,
                                                    composite_cognition %in% c(3,4) &  wave %in% c(8,9) ~ 0,
                                                    composite_cognition %in% c(2) &  wave %in% c(2,4,6) ~ 1,
                                                    composite_cognition %in% c(0:2) &  wave %in% c(8,9) ~ 1,
                                                    composite_cognition %in% c(0:1) &  wave %in% c(2,4,6) ~ 0,
                                                    TRUE ~ NA_real_)) %>% 
  mutate(nonhdl = case_when(!is.na(chol) & !is.na(hdl) ~ chol - hdl,
                                TRUE ~ NA_real_),
         fu_nonhdl = case_when(!is.na(fu_chol) & !is.na(fu_hdl) ~ fu_chol - fu_hdl,
                                   TRUE ~ NA_real_)
  ) %>%  
  mutate(control_a1c = case_when(hba1c < 2.0 | hba1c >= 20.0 ~ NA_real_,
                                 age < 65 & hba1c < 7.0 ~ 1,
                                 age < 65 & hba1c >= 7.0 ~ 0,
                                 age >= 65 & hba1c < 7.5 ~ 1,
                                 age >= 65 & hba1c >= 7.5 ~ 0,
                                 TRUE ~ NA_real_),
         
         control_bp = case_when(sbp < 140 & dbp < 90 ~ 1,
                                sbp >= 140 | dbp >= 90 ~ 0,
                                TRUE ~ NA_real_),
         
         control_chol = case_when(nonhdl <= 130 ~ 1,
                                  nonhdl > 130 ~ 0,
                                  TRUE ~ NA_real_),
         
         control_ldl = case_when(ldl <= 100 ~ 1,
                                 ldl > 100 ~ 0,
                                 TRUE ~ NA_real_),
         
         control_joint = case_when(is.na(control_a1c) | is.na(control_bp) | is.na(control_chol) ~ NA_real_,
                                   control_a1c == 1 & control_bp == 1 & control_chol == 1 ~ 1,
                                   TRUE ~ 0),
         control_joint2 = case_when(is.na(control_a1c) | is.na(control_bp) | is.na(control_ldl) ~ NA_real_,
                                    control_a1c == 1 & control_bp == 1 & control_ldl == 1 ~ 1,
                                    TRUE ~ 0),
         
         fu_control_a1c = case_when(fu_hba1c < 2.0 | fu_hba1c >= 20.0 ~ NA_real_,
                                    fu_age < 65 & fu_hba1c < 7.0 ~ 1,
                                    fu_age < 65 & fu_hba1c >= 7.0 ~ 0,
                                    fu_age>=65 & fu_hba1c < 7.5 ~ 1,
                                    fu_age>=65 & fu_hba1c >= 7.5 ~ 0,
                                    TRUE ~ NA_real_),
         
         fu_control_bp = case_when(fu_sbp < 140 & fu_dbp < 90 ~ 1,
                                   fu_sbp >= 140 | fu_dbp >= 90 ~ 0,
                                   TRUE ~ NA_real_),
         
         fu_control_ldl = case_when(fu_ldl <= 100 ~ 1,
                                     fu_ldl > 100 ~ 0,
                                     TRUE ~ NA_real_),
         
         fu_control_chol = case_when(fu_nonhdl <= 130 ~ 1,
                                     fu_nonhdl > 130 ~ 0,
                                     TRUE ~ NA_real_),
         
         fu_control_joint = case_when(is.na(fu_control_a1c) | is.na(fu_control_bp) | is.na(fu_control_chol) ~ NA_real_,
                                      fu_control_a1c == 1 & fu_control_bp == 1 & fu_control_chol == 1 ~ 1,
                                      TRUE ~ 0),
         
         fu_control_joint2 = case_when(is.na(fu_control_a1c) | is.na(fu_control_bp) | is.na(fu_control_ldl) ~ NA_real_,
                                       fu_control_a1c == 1 & fu_control_bp == 1 & fu_control_ldl == 1 ~ 1,
                                    TRUE ~ 0)
         
         
  ) %>% 
  mutate(polypharmacy = rowSums(.[(c("medication_bp","rxdiabi","rxdiabo",
                                     "rxlung",
                                     "rxasthma",
                                     "rxcancr",
                                     "rxbldthn",
                                     "rxhchol"))],na.rm=TRUE),
         vision_impairment = case_when(sight_selfrated %in% c(5:6) ~ 1,
                                       sight_near %in% c(5:6) ~ 1,
                                       sight_selfrated %in% c(1:4) &
                                         sight_near %in% c(1:4) ~ 0,
                                       TRUE ~ NA_real_),
         balance_impairment = case_when(age <70 & fulltandem_seconds == 30 ~ 1,
                                        age >= 70 & fulltandem_seconds >= 10 ~ 1,
                                        !is.na(fulltandem_seconds) ~ 0,
                                        TRUE ~ NA_real_),
         married = case_when(marital %in% c(1) ~ 1,
                             marital %in% c(3,4,5,7,8) ~ 0, # Partnered, Separated, Divorced, Widowed, Never Married
                             TRUE ~ NA_real_))

elsa_with_cesd_diagnoseddm %>% 
  saveRDS(.,paste0(path_g2a_depression_folder,"/working/g2ade02_analytic dataset.RDS"))


# Checking counts -------


all_elsa %>% 
  # group_by(gender) %>%
  tally()

all_elsa %>% 
  distinct(personid,gender) %>% 
  # group_by(gender) %>%
  tally()

elsa_with_cesd %>% 
  # group_by(gender) %>%
  tally()

elsa_with_cesd %>% 
  distinct(personid,gender) %>% 
  # group_by(gender) %>%
  tally()

elsa_with_cesd_diagnoseddm %>% 
  # group_by(gender) %>%
  tally()

elsa_with_cesd_diagnoseddm %>% 
  distinct(personid,gender) %>% 
  # group_by(gender) %>%
  tally()
