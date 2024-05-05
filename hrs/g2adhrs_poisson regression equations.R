
ind_covariates <- c("+ control_a1c + control_bp + control_chol + age + female + laborforce + iadl_any4 + moderate_pa + vigorous_pa +  heavydrinker + 
                    bmi + htn + raceeth + smoke + medication_dm + medication_bp + rxhchol + rxdepres + rxpsych + duration_dm")

hh_covariates <- c("+ hh_size + hh_wealthquintile + factor(wave)")

auxiliary_covariates <- c("+ adl_some6 + a1c_adj + sbp + dbp + tc_adj + hdl_adj + nfalls + polypharmacy + balance_impairment + vision_impairment +
                          lengthmar + fall_injury + fracture_hip + medication_bp")

m0 <- paste0("fu_control_joint ~ cesd_ge3") %>% as.formula()

m1 <- paste0("fu_control_joint ~ cesd_ge3",ind_covariates,hh_covariates) %>% as.formula()

m2 <- paste0("fu_control_joint ~ cesd_ge3*ge65",ind_covariates,hh_covariates) %>% str_replace(.,"\\+ age ","") %>% as.formula()

m3 <- paste0("fu_control_joint ~ cesd_ge3*female",ind_covariates,hh_covariates) %>% str_replace(.,"\\+ female ","") %>% as.formula()

m4 <- paste0("fu_control_joint ~ cesd_ge3*iadl_any4",ind_covariates,hh_covariates) %>% str_replace(.,"\\+ iadl_any4 ","") %>% as.formula()

m5 <- paste0("fu_control_joint ~ cesd_ge3*control_joint",ind_covariates,hh_covariates) %>% 
  str_replace_all(.,"\\+ control_a1c\\s\\+\\scontrol_bp\\s\\+\\scontrol_chol ","")  %>% as.formula()

m6 <- paste0("fu_control_joint ~ cesd_ge3*rxdepres",ind_covariates,hh_covariates) %>% str_replace(.,"\\+ rxdepres ","") %>% as.formula()

m7 <- paste0("fu_control_joint ~ cesd_ge3*medication_dm",ind_covariates,hh_covariates) %>% str_replace(.,"\\+ medication_dm ","") %>% as.formula()

m8 <- paste0("fu_control_joint ~ cesd_ge3*duration_dm_ge10",ind_covariates,hh_covariates) %>% str_replace(.,"\\+ duration_dm","") %>% as.formula()


ltfu1 <- paste0("followup ~ cesd_ge3", ind_covariates,hh_covariates,auxiliary_covariates) %>% as.formula()

a0 <- paste0("fu_control_a1c ~ cesd_ge3") %>% as.formula()
b0 <- paste0("fu_control_bp ~ cesd_ge3") %>% as.formula()
c0 <- paste0("fu_control_chol ~ cesd_ge3") %>% as.formula()

a1 <- paste0("fu_control_a1c ~ cesd_ge3",ind_covariates,hh_covariates) %>% as.formula()
b1 <- paste0("fu_control_bp ~ cesd_ge3",ind_covariates,hh_covariates) %>% as.formula()
c1 <- paste0("fu_control_chol ~ cesd_ge3",ind_covariates,hh_covariates) %>% as.formula()

a3 <- paste0("fu_control_a1c ~ cesd_ge3*female",ind_covariates,hh_covariates) %>% str_replace(.,"\\+ female ","") %>% as.formula()
b3 <- paste0("fu_control_bp ~ cesd_ge3*female",ind_covariates,hh_covariates) %>% str_replace(.,"\\+ female ","") %>% as.formula()
c3 <- paste0("fu_control_chol ~ cesd_ge3*female",ind_covariates,hh_covariates) %>% str_replace(.,"\\+ female ","") %>% as.formula()


a5 <- paste0("fu_control_a1c ~ cesd_ge3*control_a1c",ind_covariates,hh_covariates) %>% str_replace(.,"\\+ control_a1c ","") %>% as.formula()
b5 <- paste0("fu_control_bp ~ cesd_ge3*control_bp",ind_covariates,hh_covariates) %>% str_replace(.,"\\+ control_bp ","") %>% as.formula()
c5 <- paste0("fu_control_chol ~ cesd_ge3*control_chol",ind_covariates,hh_covariates) %>% str_replace(.,"\\+ control_chol ","") %>% as.formula()

# Lists for models --------

overall_m0 = list()
overall_m1 = list()
overall_m2 = list()
overall_m3 = list()
overall_m4 = list()
overall_m5 = list()
overall_m6 = list()
overall_m7 = list()
overall_m8 = list()

overall_a0 = list()
overall_b0 = list()
overall_c0 = list()
overall_a1 = list()
overall_b1 = list()
overall_c1 = list()
overall_a3 = list()
overall_b3 = list()
overall_c3 = list()
overall_a5 = list()
overall_b5 = list()
overall_c5 = list()

