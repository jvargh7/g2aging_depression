gc();rm(list=ls());source(".Rprofile")

hrs_coef_unadjusted <- read_csv("hrs/g2adh04_coefficients for sensitivity poisson regression.csv") %>% 
  dplyr::filter(model %in% c("M0","A0","B0","C0")) %>% 
  dplyr::filter(str_detect(iv,"cesd")) %>% 
  rename(RR = OR) %>% 
  mutate(outcome = case_when(model == "M0" ~ "Overall",
                             model == "A0" ~ "HbA1c < 7%",
                             model == "B0" ~ "BP < 140/90 mmHg",
                             model == "C0" ~ "Non HDL < 130 mg/dL"),
         iv = case_when(model == "M0" ~ "Overall",
                        model == "A0" ~ "A1c + Overall",
                        model == "B0" ~ "BP + Overall",
                        model == "C0" ~ "Cholesterol + Overall"))



hrs_coef <- read_csv("hrs/g2adh04_coefficients for sensitivity poisson regression.csv") %>% 
  dplyr::filter(model %in% c("M1","A1","B1","C1")) %>% 
  dplyr::filter(str_detect(iv,"cesd")) %>% 
  rename(RR = OR) %>% 
  mutate(outcome = case_when(model == "M1" ~ "Overall",
                             model == "A1" ~ "HbA1c < 7%",
                             model == "B1" ~ "BP < 140/90 mmHg",
                             model == "C1" ~ "Non HDL < 130 mg/dL"),
         iv = case_when(model == "M1" ~ "Overall",
                        model == "A1" ~ "A1c + Overall",
                        model == "B1" ~ "BP + Overall",
                        model == "C1" ~ "Cholesterol + Overall"))



hrs_emm <- read_csv("hrs/g2adh04_contrasts for sensitivity poisson regression with multiple imputation.csv") %>% 
  dplyr::filter(iv != "Contrast 3") %>% 
  mutate(iv = case_when(iv == "Contrast 1" & model == "M2" ~ "Age < 65",
                        iv == "Contrast 2" & model == "M2" ~ "Age >= 65",
                        iv == "Contrast 1" & model == "M3" ~ "Male",
                        iv == "Contrast 2" & model == "M3" ~ "Female",
                        iv == "Contrast 1" & model == "M4" ~ "No Difficulty with IADL",
                        iv == "Contrast 2" & model == "M4" ~ "Difficulty with IADL",
                        iv == "Contrast 1" & model == "M5" ~ "ABC Uncontrolled",
                        iv == "Contrast 2" & model == "M5" ~ "ABC Controlled",
                        iv == "Contrast 1" & model == "M6" ~ "No Depression Medication",
                        iv == "Contrast 2" & model == "M6" ~ "Depression Medication",
                        iv == "Contrast 1" & model == "M7" ~ "No DM Medication",
                        iv == "Contrast 2" & model == "M7" ~ "DM Medication",
                        iv == "Contrast 1" & model == "M8" ~ "Duration < 10 years",
                        iv == "Contrast 2" & model == "M8" ~ "Duration >= 10 years",
                        iv == "Contrast 1" & model == "A3" ~ "A1c + Male",
                        iv == "Contrast 2" & model == "A3" ~ "A1c + Female",
                        iv == "Contrast 1" & model == "B3" ~ "BP + Male",
                        iv == "Contrast 2" & model == "B3" ~ "BP + Female",
                        iv == "Contrast 1" & model == "C3" ~ "Cholesterol + Male",
                        iv == "Contrast 2" & model == "C3" ~ "Cholesterol + Female",
                        iv == "Contrast 1" & model == "A5" ~ "A1c + A1c uncontrolled",
                        iv == "Contrast 2" & model == "A5" ~ "A1c + A1c controlled",
                        iv == "Contrast 1" & model == "B5" ~ "BP + BP uncontrolled",
                        iv == "Contrast 2" & model == "B5" ~ "BP + BP controlled",
                        iv == "Contrast 1" & model == "C5" ~ "Cholesterol + Cholesterol uncontrolled",
                        iv == "Contrast 2" & model == "C5" ~ "Cholesterol + Cholesterol controlled",
                        TRUE ~ NA_character_
  ))


table_df = bind_rows(hrs_coef %>% dplyr::select(iv,model,RR,lci,uci) %>% mutate(country = "USA"),
                     hrs_emm %>% dplyr::select(iv,model,RR,lci,uci) %>% mutate(country = "USA")
)  %>% 
  mutate(term = factor(iv,
                       levels=c("Overall",
                                "Age < 65",
                                "Age >= 65",
                                "Female",
                                "Male",
                                "No Difficulty with IADL",
                                "Difficulty with IADL",
                                "ABC Uncontrolled",
                                "ABC Controlled",
                                "No Depression Medication",
                                "Depression Medication",
                                "No DM Medication",
                                "DM Medication",
                                "Duration < 10 years",
                                "Duration >= 10 years",
                                "Joint ABC 2",
                                "A1c + Overall",
                                "A1c + Male",
                                "A1c + Female",
                                "BP + Overall",
                                "BP + Male",
                                "BP + Female",
                                "Cholesterol + Overall",
                                "Cholesterol + Male",
                                "Cholesterol + Female",
                                "A1c + A1c uncontrolled",
                                "A1c + A1c controlled",
                                "BP + BP uncontrolled",
                                "BP + BP controlled",
                                "Cholesterol + Cholesterol uncontrolled",
                                "Cholesterol + Cholesterol controlled"
                       ),
                       ordered = TRUE)) 


hrs_coef_unadjusted %>% 
  dplyr::select(outcome,RR) %>% 
  write_csv(.,"paper/table_sensitivity ipcw unadjusted risk of control.csv")


table_df %>% 
  dplyr::select(term,country,RR) %>% 
  pivot_wider(names_from = "country",values_from="RR") %>% 
  write_csv(.,"paper/table_sensitivity ipcw risk of control by effect modifier.csv")
