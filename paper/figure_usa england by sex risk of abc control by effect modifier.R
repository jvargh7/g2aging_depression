gc();rm(list=ls());source(".Rprofile")

hrs_coef_unadjusted <- read_csv("hrs/g2adh03_coefficients for poisson regression.csv") %>% 
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



hrs_coef <- read_csv("hrs/g2adh03_coefficients for poisson regression.csv") %>% 
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



hrs_emm <- read_csv("hrs/g2adh03_contrasts for poisson regression with multiple imputation.csv") %>% 
  dplyr::filter(iv != "Contrast 3") %>% 
  mutate(iv = case_when(iv == "Contrast 1" & model == "M2" ~ "Age < 65",
                        iv == "Contrast 2" & model == "M2" ~ "Age >= 65",
                        iv == "Contrast 1" & model == "M3" ~ "Male",
                        iv == "Contrast 2" & model == "M3" ~ "Female",
                        iv == "Contrast 1" & model == "M4" ~ "No Functional Limitations",
                        iv == "Contrast 2" & model == "M4" ~ "Functional Limitations",
                        iv == "Contrast 1" & model == "M5" ~ "ABC Uncontrolled",
                        iv == "Contrast 2" & model == "M5" ~ "ABC Controlled",
                        iv == "Contrast 1" & model == "M6" ~ "No Antidepressants",
                        iv == "Contrast 2" & model == "M6" ~ "Antidepressants",
                        iv == "Contrast 1" & model == "M7" ~ "No Diabetes Medication",
                        iv == "Contrast 2" & model == "M7" ~ "Diabetes Medication",
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
                                "No Functional Limitations",
                                "Functional Limitations",
                                "ABC Uncontrolled",
                                "ABC Controlled",
                                "No Antidepressants",
                                "Antidepressants",
                                "No Diabetes Medication",
                                "Diabetes Medication",
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
  write_csv(.,"paper/table_unadjusted risk of control.csv")
  

table_df %>% 
  dplyr::select(term,country,RR) %>% 
  pivot_wider(names_from = "country",values_from="RR") %>% 
  write_csv(.,"paper/table_risk of control by effect modifier.csv")

figure_risk = table_df %>% 
  dplyr::filter(term %in% c("Overall",
                            "Age < 65",
                            "Age >= 65",
                            "Female",
                            "Male",
                            "No Functional Limitations",
                            "Functional Limitations",
                            "ABC Uncontrolled",
                            "ABC Controlled",
                            "No Antidepressants",
                            "Antidepressants",
                            "No Diabetes Medication",
                            "Diabetes Medication",
                            "Duration < 10 years",
                            "Duration >= 10 years")) %>% 
  dplyr::filter(country == "USA") %>% 
  dplyr::select(term,country,RR) %>% 
  separate(RR,into=c("est","lci","uci"),sep="(\\t|\\,)") %>% 
  mutate(across(est:uci,.fns=function(x) as.numeric(str_replace(x,"(\\(|\\))","")))) %>% 
  ggplot(data=.,aes(x=est,xmin=lci,xmax=uci,y = term
                    # ,col=country
  )) +
  geom_point(position = position_dodge(width = 0.9)) +
  geom_errorbarh(position = position_dodge(width = 0.9),height = 0.1) +
  theme_bw() +
  scale_color_manual("",values=c(
    # rgb(1/255,160/255,138/255),
    rgb(114/255,148/255,212/255))) +
  xlab("") +
  ylab("") +
  scale_y_discrete(limits=rev) +
  geom_vline(xintercept = 1,col="red",linetype = 2) +
  theme(legend.position = "bottom") +
  theme(axis.text = element_text(size = 14),
        legend.text = element_text(size = 14))

figure_risk %>% 
  ggsave(.,filename=paste0(path_g2a_depression_folder,"/figures/figure_risk of depression by effect modifier.jpg"),width = 7,height = 8)


