# gc();rm(list=ls());source(".Rprofile") --

source("elsa/g2adelsa_diagnosed diabetes with exposure and outcome.R")
source("C:/code/external/functions/survey/svysummary.R")

continuous_vars = c("age","hh_size","children","lengthmar",
                    "bmi","height","weight","waistcircumference",
                    "lengthmar","adl_some6", "iadl_some4", "iadl_any4",
                    "hba1c","sbp","dbp","hdl","trig","ldl","chol","nonhdl",
                    "nfalls","fu_nfalls","agediagnosed_dm",
                    "moderate_pa","vigorous_pa","polypharmacy",
                    "cesd",
                    "fu_age","fu_a1c_adj","fu_sbp","fu_dbp","fu_tc_adj","fu_hdl_adj","fu_nonhdl_adj"
)

proportion_vars = c("medication_bp","medication_dm","rxdiabi","rxdiabo",
                    "rxlung","rxasthma","rxcancr","rxbldthn","rxhchol","rxdepres",
                    
                    "married",
                    "diagnosed_bp","htn",
                    "alzh_demen",
                    "bmi_underweight","bmi_overweight","bmi_obese",
                    "fall_any","fall_injury","fracture_hip",
                    "fu_fall_any","fu_fall_injury","fu_fracture_hip",
                    
                    "control_a1c","control_bp","control_chol","control_joint","control_ldl","control_joint2",
                    "fu_control_a1c","fu_control_bp","fu_control_chol","fu_control_joint","fu_control_ldl","fu_control_joint2",
                    
                    "heavydrinker",
                    
                    "vision_impairment","balance_impairment",
                    
                    "cesd_ge3","cesd_ge5","cesd_ge6"
                    
)

grouped_vars = c(
  "age_category",
  "bmi_category",
  "education_h","hh_wealthquintile",
  "smoke","laborforce","race"
)

elsa_sy <- svysummary(elsa_unique_svy,
                      c_vars = continuous_vars,
                      p_vars = proportion_vars,
                      g_vars = grouped_vars,
                      id_vars = "gender"
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"));

elsa_sy_total <- svysummary(elsa_unique_svy,
                            c_vars = continuous_vars,
                            p_vars = proportion_vars,
                            g_vars = grouped_vars
                            # id_vars = "gender"
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"));


bind_rows(elsa_sy_total %>% mutate(gender = "Total"),
          elsa_sy) %>% 
  write_csv(.,path = "elsa/g2ade02_descriptive characteristics.csv")

with(elsa_all,table(gender))
with(elsa_unique_df,table(gender))
with(elsa_all,table(fu_control_joint))
with(elsa_all,table(gender,fu_control_joint))