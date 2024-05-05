gc();rm(list=ls());source(".Rprofile")

mi_dfs <- readRDS(paste0(path_g2a_depression_folder,"/working/g2adhrs_multiple imputation.RDS"))

continuous_vars = c("age","hh_size","hh_children","lengthmar",
                    "bmi","height","weight","waistcircumference",
                    "lengthmar","adl_some6", "iadl_any4",
                    "a1c_adj","sbp","dbp","tc_adj","hdl_adj","nonhdl_adj","nfalls","agediagnosed_dm",
                    "moderate_pa","vigorous_pa",
                    "polypharmacy","cesd",
                    "fu_age","fu_a1c_adj","fu_sbp","fu_dbp","fu_tc_adj","fu_hdl_adj","fu_nonhdl_adj"
)

proportion_vars = c("medication_bp","medication_dm","rxdiabi","rxdiabo",
                    "rxstrok","rxangina","rxchf","rxarthr",
                    "rxlung","rxpsych","rxcancr","rxhrtat","rxheart",
                    "rxmemry","rxhchol","rxbreath","rxstomach",
                    "rxdepres",
                    
                    "alcohol",
                    
                    "diagnosed_bp","htn",
                    "alzh_demen",
                    "bmi_underweight","bmi_overweight","bmi_obese",
                    "fall_any","fall_injury","fracture_hip",
                    "control_a1c","control_bp","control_chol","control_joint",
                    "fu_control_a1c","fu_control_bp","fu_control_chol","fu_control_joint",
                    "heavydrinker",
                    
                    "vision_impairment","balance_impairment","married","composite_cognition_impairment",
                    
                    "cesd_ge3","cesd_ge5","cesd_ge6"
)

grouped_vars = c(
  "age_category",
  "bmi_category",
  "education_h","hh_wealthquintile",
  "smoke","laborforce","raceeth","race","ethnicity"
)


total <- mi_dfs$data %>% 
  summarize(across(one_of(c(continuous_vars,proportion_vars,grouped_vars)),function(x) sum(is.na(x))))

total_by_gender <- mi_dfs$data %>% 
  group_by(gender) %>% 
  summarize(across(one_of(c(continuous_vars,proportion_vars,grouped_vars)),function(x) sum(is.na(x))))

bind_rows(total %>% mutate(gender = "Total"),
          total_by_gender) %>% 
  pivot_longer(-gender) %>% 
  pivot_wider(names_from=gender, values_from=value)  %>% 
  write_csv(.,"hrs/g2adhrs06_missing data in analytic sample.csv")
