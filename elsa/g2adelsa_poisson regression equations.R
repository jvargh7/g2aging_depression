
ind_covariates <- c("+ control_a1c + control_bp + control_chol + age + female + laborforce + iadl_any4 + fall_any + moderate_pa + vigorous_pa +  
                    heavydrinker + bmi + htn + race + smoke + rxdepres + medication_dm")

hh_covariates <- c("+ hh_size + hh_wealthquintile + wave")

auxiliary_covariates <- c("+ adl_some6 + hba1c + sbp + dbp + trig + hdl + ldl + chol + nfalls + 
                          lengthmar + polypharmacy + rxdiabi + balance_impairment + vision_impairment + fall_injury + fracture_hip")

m0 <- paste0("fu_control_joint ~ cesd_ge3") %>% as.formula()

m1 <- paste0("fu_control_joint ~ cesd_ge3",ind_covariates,hh_covariates) %>% as.formula()

m2 <- paste0("fu_control_joint ~ cesd_ge3*ge65",ind_covariates,hh_covariates) %>% str_replace(.,"\\+ age ","") %>% as.formula()

m3 <- paste0("fu_control_joint ~ cesd_ge3*female",ind_covariates,hh_covariates) %>% str_replace(.,"\\+ female ","") %>% as.formula()


m4 <- paste0("fu_control_joint ~ cesd_ge3*iadl_any4",ind_covariates,hh_covariates) %>% str_replace(.,"\\+ iadl_any4 ","") %>% as.formula()

m5 <- paste0("fu_control_joint ~ cesd_ge3*control_joint",ind_covariates,hh_covariates) %>% 
  str_replace_all(.,"\\+ control_a1c\\s\\+\\scontrol_bp\\s\\+\\scontrol_chol ","")  %>% as.formula()

m6 <- paste0("fu_control_joint ~ cesd_ge3*rxdepres",ind_covariates,hh_covariates) %>% str_replace(.,"\\+ rxdepres ","") %>% as.formula()

m7 <- paste0("fu_control_joint ~ cesd_ge3*medication_dm",ind_covariates,hh_covariates) %>% str_replace(.,"\\+ medication_dm ","") %>% as.formula()

ltfu1 <- paste0("followup ~ cesd_ge3", ind_covariates,hh_covariates,auxiliary_covariates) %>% as.formula()

a1 <- paste0("fu_control_a1c ~ cesd_ge3",ind_covariates,hh_covariates) %>% as.formula()
b1 <- paste0("fu_control_bp ~ cesd_ge3",ind_covariates,hh_covariates) %>% as.formula()
c1 <- paste0("fu_control_chol ~ cesd_ge3",ind_covariates,hh_covariates) %>% as.formula()
a3 <- paste0("fu_control_a1c ~ cesd_ge3*female",ind_covariates,hh_covariates) %>% str_replace(.,"\\+ female ","") %>% as.formula()
b3 <- paste0("fu_control_bp ~ cesd_ge3*female",ind_covariates,hh_covariates) %>% str_replace(.,"\\+ female ","") %>% as.formula()
c3 <- paste0("fu_control_chol ~ cesd_ge3*female",ind_covariates,hh_covariates) %>% str_replace(.,"\\+ female ","") %>% as.formula()

l1 <- paste0("fu_control_ldl ~ cesd_ge3",ind_covariates,hh_covariates) %>% as.formula()
l3 <- paste0("fu_control_ldl ~ cesd_ge3*female",ind_covariates,hh_covariates) %>% str_replace(.,"\\+ female ","") %>% as.formula()
j1 <- paste0("fu_control_joint2 ~ cesd_ge3",ind_covariates,hh_covariates) %>% as.formula()
j3 <- paste0("fu_control_joint2 ~ cesd_ge3*female",ind_covariates,hh_covariates) %>% str_replace(.,"\\+ female ","") %>% as.formula()
# Lists for models --------

overall_m0 = list()
overall_m1 = list()
overall_m2 = list()
overall_m3 = list()
overall_m4 = list()
overall_m5 = list()
overall_m6 = list()
overall_m7 = list()

overall_a1 = list()
overall_b1 = list()
overall_c1 = list()
overall_a3 = list()
overall_b3 = list()
overall_c3 = list()


overall_l1 = list()
overall_l3 = list()
overall_j1 = list()
overall_j3 = list()