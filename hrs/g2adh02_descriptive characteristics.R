# gc();rm(list=ls());source(".Rprofile") --

source("hrs/g2adhrs_diagnosed diabetes with exposure and outcome.R")
source("C:/code/external/functions/survey/svysummary.R")
source("C:/code/external/functions/survey/svysd.R")

continuous_vars = c("age","hh_size","hh_children","lengthmar",
                    "bmi","height","weight","waistcircumference",
                    "lengthmar","adl_some6", "iadl_some4",
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

hrs_sy <- svysummary(hrs_unique_svy,
                     c_vars = continuous_vars,
                     p_vars = proportion_vars,
                     g_vars = grouped_vars,
                     id_vars = "gender"
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"));

hrs_sy_total <- svysummary(hrs_unique_svy,
                           c_vars = continuous_vars,
                           p_vars = proportion_vars,
                           g_vars = grouped_vars
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"));

hrs_sy_cesd_ge3 <- svysummary(hrs_unique_svy,
                     c_vars = continuous_vars,
                     p_vars = proportion_vars,
                     g_vars = grouped_vars,
                     id_vars = "cesd_ge3"
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"));


svysd(hrs_unique_svy,
      c_vars = continuous_vars)

svysd(hrs_unique_svy,
      c_vars = continuous_vars,
      id_vars = "gender") %>% View()

bind_rows(hrs_sy_total %>% mutate(gender = "Total",cesd_ge3 = "Total"),
          hrs_sy %>% mutate(cesd_ge3 = "Total"),
          hrs_sy_cesd_ge3 %>% mutate(cesd_ge3 = case_when(cesd_ge3 == 1 ~ "Yes",
                                                          cesd_ge3 == 0 ~ "No",
                                                          TRUE ~ NA_character_)) 
          %>% mutate(gender = "Total")) %>% 
  write_csv(.,file = "hrs/g2adh02_descriptive characteristics.csv")

with(hrs_all,table(gender))
with(hrs_unique_df,table(gender))
with(hrs_all,table(fu_control_joint))
with(hrs_all,table(gender,fu_control_joint))
