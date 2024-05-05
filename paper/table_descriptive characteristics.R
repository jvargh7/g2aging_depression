gc();rm(list=ls());source(".Rprofile")

hrs_descriptives <-   read_csv("hrs/g2adh02_descriptive characteristics.csv") %>% 
  dplyr::select(variable,gender,cesd_ge3,group,est_ci) %>% 
  pivot_wider(names_from=c(gender,cesd_ge3),values_from=est_ci) %>% 
  mutate(variable = case_when(variable == "a1c_adj" ~ "hba1c",
                              variable == "hdl_adj" ~ "hdl",
                              variable == "tc_adj" ~ "chol",
                              TRUE ~ variable))



hrs_descriptives %>% 
  dplyr::select(variable,group,Total_Total,Female_Total, Male_Total,Total_No, Total_Yes) %>% 
  write_csv(.,"paper/table_descriptive characteristics.csv")


