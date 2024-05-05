gc();rm(list=ls());source(".Rprofile")

mi_dfs <- readRDS(paste0(path_g2a_depression_folder,"/working/g2adhrs_multiple imputation.RDS"))

library(mice)
library(geepack)

source("hrs/g2adhrs_poisson regression equations.R")

for(i in 1:mi_dfs$m){
  df = complete(mi_dfs,action = i) %>% 
    dplyr::select(-hhsampleweight,-spouseidpn,-spousepn) %>% 
    arrange(hhidpn,wave) %>% 
    mutate(raceeth = case_when(is.na(raceeth) ~ "NH White",
                               TRUE ~ raceeth)) %>% 
    mutate(duration_dm = age - agediagnosed_dm) %>% 
    mutate(duration_dm_ge10 = case_when(duration_dm > 10 ~ 1,
                                        TRUE ~ 0));
  
  
  overall_m0[[i]] = geeglm(formula = m0,
                           corstr = "exchangeable",
                           family=poisson(),
                           data = df,
                           weights = normalizedweight,
                           id = hhidpn);
  
  overall_m1[[i]] = geeglm(formula = m1,
                           corstr = "exchangeable",
                           family=poisson(),
                           data = df,
                           weights = normalizedweight,
                           id = hhidpn);
  
  overall_m2[[i]] = geeglm(formula = m2,
                           corstr = "exchangeable",
                           family=poisson(),
                           data = df,
                           weights = normalizedweight,
                           id = hhidpn);
  overall_m3[[i]] = geeglm(formula = m3,
                           corstr = "exchangeable",
                           family=poisson(),
                           data = df,
                           weights = normalizedweight,
                           id = hhidpn);
  overall_m4[[i]] = geeglm(formula = m4,
                           corstr = "exchangeable",
                           family=poisson(),
                           data = df,
                           weights = normalizedweight,
                           id = hhidpn);
  overall_m5[[i]] = geeglm(formula = m5,
                           corstr = "exchangeable",
                           family=poisson(),
                           data = df,
                           weights = normalizedweight,
                           id = hhidpn);
  overall_m6[[i]] = geeglm(formula = m6,
                           corstr = "exchangeable",
                           family=poisson(),
                           data = df,
                           weights = normalizedweight,
                           id = hhidpn);
  overall_m7[[i]] = geeglm(formula = m7,
                           corstr = "exchangeable",
                           family=poisson(),
                           data = df,
                           weights = normalizedweight,
                           id = hhidpn);
  overall_m8[[i]] = geeglm(formula = m8,
                           corstr = "exchangeable",
                           family=poisson(),
                           data = df,
                           weights = normalizedweight,
                           id = hhidpn);
  
  overall_a0[[i]] = geeglm(formula = a0,
                           corstr = "exchangeable",
                           family=poisson(),
                           data = df,
                           weights = normalizedweight,
                           id = hhidpn);
  overall_b0[[i]] = geeglm(formula = b0,
                           corstr = "exchangeable",
                           family=poisson(),
                           data = df,
                           weights = normalizedweight,
                           id = hhidpn);
  overall_c0[[i]] = geeglm(formula = c0,
                           corstr = "exchangeable",
                           family=poisson(),
                           data = df,
                           weights = normalizedweight,
                           id = hhidpn);
 
  
  overall_a1[[i]] = geeglm(formula = a1,
                           corstr = "exchangeable",
                           family=poisson(),
                           data = df,
                           weights = normalizedweight,
                           id = hhidpn);
  overall_b1[[i]] = geeglm(formula = b1,
                           corstr = "exchangeable",
                           family=poisson(),
                           data = df,
                           weights = normalizedweight,
                           id = hhidpn);
  overall_c1[[i]] = geeglm(formula = c1,
                           corstr = "exchangeable",
                           family=poisson(),
                           data = df,
                           weights = normalizedweight,
                           id = hhidpn);
  
  overall_a3[[i]] = geeglm(formula = a3,
                           corstr = "exchangeable",
                           family=poisson(),
                           data = df,
                           weights = normalizedweight,
                           id = hhidpn);
  overall_b3[[i]] = geeglm(formula = b3,
                           corstr = "exchangeable",
                           family=poisson(),
                           data = df,
                           weights = normalizedweight,
                           id = hhidpn);
  overall_c3[[i]] = geeglm(formula = c3,
                           corstr = "exchangeable",
                           family=poisson(),
                           data = df,
                           weights = normalizedweight,
                           id = hhidpn);
  
  overall_a5[[i]] = geeglm(formula = a5,
                           corstr = "exchangeable",
                           family=poisson(),
                           data = df,
                           weights = normalizedweight,
                           id = hhidpn);
  overall_b5[[i]] = geeglm(formula = b5,
                           corstr = "exchangeable",
                           family=poisson(),
                           data = df,
                           weights = normalizedweight,
                           id = hhidpn);
  overall_c5[[i]] = geeglm(formula = c5,
                           corstr = "exchangeable",
                           family=poisson(),
                           data = df,
                           weights = normalizedweight,
                           id = hhidpn);
  
  gc();rm(df);rm(svy_des)
}

# Please download the latest version ----

# https://github.com/jvargh7/functions/blob/main/imputation/adjusted_ci.R
# download.file("https://github.com/jvargh7/functions/blob/main/imputation/adjusted_ci.R",destfile = "")
source("C:/code/external/functions/imputation/adjusted_ci.R")
# https://github.com/jvargh7/functions/blob/main/imputation/clean_mi_conditionalregression.R
source("C:/code/external/functions/imputation/clean_mi_conditionalregression.R")

overall_m0_out = clean_mi_conditionalregression(overall_m0,link="geeglm log")
overall_m1_out = clean_mi_conditionalregression(overall_m1,link="geeglm log")
overall_m2_out = clean_mi_conditionalregression(overall_m2,link="geeglm log")
overall_m3_out = clean_mi_conditionalregression(overall_m3,link="geeglm log")
overall_m4_out = clean_mi_conditionalregression(overall_m4,link="geeglm log")
overall_m5_out = clean_mi_conditionalregression(overall_m5,link="geeglm log")
overall_m6_out = clean_mi_conditionalregression(overall_m6,link="geeglm log")
overall_m7_out = clean_mi_conditionalregression(overall_m7,link="geeglm log")
overall_m8_out = clean_mi_conditionalregression(overall_m8,link="geeglm log")
overall_a0_out = clean_mi_conditionalregression(overall_a0,link="geeglm log")
overall_b0_out = clean_mi_conditionalregression(overall_b0,link="geeglm log")
overall_c0_out = clean_mi_conditionalregression(overall_c0,link="geeglm log")
overall_a1_out = clean_mi_conditionalregression(overall_a1,link="geeglm log")
overall_b1_out = clean_mi_conditionalregression(overall_b1,link="geeglm log")
overall_c1_out = clean_mi_conditionalregression(overall_c1,link="geeglm log")
overall_a3_out = clean_mi_conditionalregression(overall_a3,link="geeglm log")
overall_b3_out = clean_mi_conditionalregression(overall_b3,link="geeglm log")
overall_c3_out = clean_mi_conditionalregression(overall_c3,link="geeglm log")
overall_a5_out = clean_mi_conditionalregression(overall_a3,link="geeglm log")
overall_b5_out = clean_mi_conditionalregression(overall_b3,link="geeglm log")
overall_c5_out = clean_mi_conditionalregression(overall_c3,link="geeglm log")


bind_rows(
  overall_m0_out %>% mutate(model = "M0"),
  overall_m1_out %>% mutate(model = "M1"),
  overall_m2_out %>% mutate(model = "M2"),
  overall_m3_out %>% mutate(model = "M3"),
  overall_m4_out %>% mutate(model = "M4"),
  overall_m5_out %>% mutate(model = "M5"),
  overall_m6_out %>% mutate(model = "M6"),
  overall_m7_out %>% mutate(model = "M7"),
  overall_m8_out %>% mutate(model = "M8"),
  
  overall_a0_out %>% mutate(model = "A0"),
  overall_b0_out %>% mutate(model = "B0"),
  overall_c0_out %>% mutate(model = "C0"),
  overall_a1_out %>% mutate(model = "A1"),
  overall_b1_out %>% mutate(model = "B1"),
  overall_c1_out %>% mutate(model = "C1"),
  overall_a3_out %>% mutate(model = "A3"),
  overall_b3_out %>% mutate(model = "B3"),
  overall_c3_out %>% mutate(model = "C3"),
  
  overall_a5_out %>% mutate(model = "A5"),
  overall_b5_out %>% mutate(model = "B5"),
  overall_c5_out %>% mutate(model = "C5")
  ) %>% 
  write_csv(.,"hrs/g2adh03_coefficients for poisson regression.csv")


# Contrasts ----------

source("C:/code/external/functions/imputation/clean_mi_contrasts.R")

# The coefficients are in column 'RR'. The rest of the columns are for pooling multiple imputated regressions

contrasts_m2_out = clean_mi_contrasts(model_list = overall_m2,link="geeglm log",modifier = "ge65",exposure = "cesd_ge3")
contrasts_m3_out = clean_mi_contrasts(model_list = overall_m3,link="geeglm log",modifier = "female",exposure = "cesd_ge3")
contrasts_m4_out = clean_mi_contrasts(model_list = overall_m4,link="geeglm log",modifier = "iadl_any4",exposure = "cesd_ge3")
contrasts_m5_out = clean_mi_contrasts(model_list = overall_m5,link="geeglm log",modifier = "control_joint",exposure = "cesd_ge3")
contrasts_m6_out = clean_mi_contrasts(model_list = overall_m6,link="geeglm log",modifier = "rxdepres",exposure = "cesd_ge3")
contrasts_m7_out = clean_mi_contrasts(model_list = overall_m7,link="geeglm log",modifier = "medication_dm",exposure = "cesd_ge3")
contrasts_m8_out = clean_mi_contrasts(model_list = overall_m8,link="geeglm log",modifier = "duration_dm_ge10",exposure = "cesd_ge3")

contrasts_a3_out = clean_mi_contrasts(model_list = overall_a3,link="geeglm log",modifier = "female",exposure = "cesd_ge3")
contrasts_b3_out = clean_mi_contrasts(model_list = overall_b3,link="geeglm log",modifier = "female",exposure = "cesd_ge3")
contrasts_c3_out = clean_mi_contrasts(model_list = overall_c3,link="geeglm log",modifier = "female",exposure = "cesd_ge3")


contrasts_a5_out = clean_mi_contrasts(model_list = overall_a5,link="geeglm log",modifier = "control_a1c",exposure = "cesd_ge3")
contrasts_b5_out = clean_mi_contrasts(model_list = overall_b5,link="geeglm log",modifier = "control_bp",exposure = "cesd_ge3")
contrasts_c5_out = clean_mi_contrasts(model_list = overall_c5,link="geeglm log",modifier = "control_chol",exposure = "cesd_ge3")

bind_rows(
  contrasts_m2_out %>% mutate(model = "M2"),
  contrasts_m3_out %>% mutate(model = "M3"),
  contrasts_m4_out %>% mutate(model = "M4"),
  contrasts_m5_out %>% mutate(model = "M5"),
  contrasts_m6_out %>% mutate(model = "M6"),
  contrasts_m7_out %>% mutate(model = "M7"),
  contrasts_m8_out %>% mutate(model = "M8"),
  contrasts_a3_out %>% mutate(model = "A3"),
  contrasts_b3_out %>% mutate(model = "B3"),
  contrasts_c3_out %>% mutate(model = "C3"),
  contrasts_a5_out %>% mutate(model = "A5"),
  contrasts_b5_out %>% mutate(model = "B5"),
  contrasts_c5_out %>% mutate(model = "C5")
  
) %>% 
  write_csv(.,"hrs/g2adh03_contrasts for poisson regression with multiple imputation.csv")
