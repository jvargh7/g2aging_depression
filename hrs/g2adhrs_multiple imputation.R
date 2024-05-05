source("hrs/g2adhrs_diagnosed diabetes with exposure and outcome.R")
rm(hrs_unique_df,hrs_unique_svy)

continuous_vars = c("age","hh_size","hh_children","lengthmar",
                    "bmi","height","weight","waistcircumference",
                    "lengthmar","adl_some6", "iadl_any4",
                    "a1c_adj","sbp","dbp","tc_adj","hdl_adj","nonhdl_adj","nfalls","agediagnosed_dm",
                    "moderate_pa","vigorous_pa",
                    "polypharmacy","cesd","fu_age",
                    "fu_a1c_adj","fu_sbp","fu_dbp","fu_tc_adj","fu_hdl_adj","fu_nonhdl_adj")

proportion_vars = c("medication_bp","medication_dm","rxdiabi","rxdiabo",
                    "rxstrok","rxangina","rxchf","rxarthr",
                    "rxlung","rxpsych","rxcancr","rxhrtat","rxheart",
                    "rxmemry","rxhchol","rxbreath","rxstomach",
                    "rxdepres",
                  
                    
                    "diagnosed_bp","htn",
                    "alzh_demen",
                    "bmi_underweight","bmi_overweight","bmi_obese",
                    "fall_any","fall_injury","fracture_hip",
                    
                    "heavydrinker",
                    
                    "vision_impairment","balance_impairment","composite_cognition_impairment",
                    
                    "control_a1c","control_bp","control_chol","control_joint",
                    "fu_control_a1c","fu_control_bp","fu_control_chol","fu_control_joint",
                    
                    "cesd_ge3","cesd_ge5","cesd_ge6"
)

grouped_vars = c(
  "age_category",
  "bmi_category",
  "education_h","hh_wealthquintile",
  "smoke","laborforce","raceeth",
  # "race","ethnicity" # We remove them because we are using race
  "gender"
)

id_vars = c("hhid","pn","hhidpn","spousepn","spouseidpn","strata","psu","coupleid","wave",
            "hhanalysisweight","hhsampleweight","indsampleweight","normalizedweight")


library(survey)
library(mice)

before_imputation <- hrs_all %>% 
  dplyr::select(one_of(id_vars),
                one_of(continuous_vars),one_of(proportion_vars),one_of(grouped_vars)) %>% 
  mutate_at(vars(fall_any,fall_injury,fracture_hip),~as.numeric(.)) %>%
  # Step 1: One hot encoding
  mutate(
    ge65 = case_when(age >= 65 ~ 1,
                     TRUE ~ 0),
    female = case_when(gender == "Female" ~ 1,
                       TRUE ~ 0)
    
  ) %>% 
  # Step 2: Modeling interactions
  mutate(cesd_ge3_control_joint = cesd_ge3*control_joint,
         
         cesd_ge3_control_a1c = cesd_ge3*control_a1c,
         cesd_ge3_control_bp = cesd_ge3*control_bp,
         cesd_ge3_control_chol = cesd_ge3*control_chol,
         

         cesd_ge3_iadl_any4 = cesd_ge3*iadl_any4,
        
         cesd_ge3_ge65 = cesd_ge3*ge65,
         
         cesd_ge3_female = cesd_ge3*female,
         
         cesd_ge3_rxdepres = cesd_ge3*rxdepres,
         
         cesd_ge3_medication_dm = cesd_ge3*medication_dm
         
  ) 

before_imputation %>% 
  dplyr::select(-one_of(id_vars),-gender) %>% 
  mutate(any_missing = rowSums(is.na(.))) %>% 
  summarize(prop = mean(any_missing>0))

interaction_terms <- c("cesd_ge3_control_joint",
                       "cesd_ge3_control_a1c",
                       "cesd_ge3_control_bp",
                       "cesd_ge3_control_chol",
                       "cesd_ge3_iadl_any4",
                       "cesd_ge3_ge65",
                       "cesd_ge3_female",
                       "cesd_ge3_rxdepres",
                       "cesd_ge3_medication_dm"
)

before_imputation %>% 
  summarize(across(one_of(interaction_terms),.fns=function(x) paste0(round(mean(x,na.rm=TRUE),1)," (",round(mean(!is.na(x)),1),")")))


mi_null <- mice(before_imputation,
                maxit = 0)

method = mi_null$method
pred = mi_null$predictorMatrix

pred[id_vars,] <- 0
pred[,id_vars] <- 0
method[id_vars] <- ""


# Impute via equation and do not use for imputation , --------
method["ge65"] <- "~I((age>=65)*1)"
pred[c("ge65"),] <- 0

method["control_a1c"] <- "~I(((a1c_adj<7.0 & age <65)|(a1c_adj<7.5 & age >=65))*1)"
pred[c("control_a1c"),] <- 0
method["fu_control_a1c"] <- "~I(((fu_a1c_adj<7.0 & fu_age <65)|(fu_a1c_adj<7.5 & fu_age >=65))*1)"
pred[c("fu_control_a1c"),] <- 0



method["control_bp"] <- "~I((sbp<140)*(dbp<90)*1)"
pred[c("control_bp"),] <- 0
method["fu_control_bp"] <- "~I((fu_sbp<140)*(fu_dbp<90)*1)"
pred[c("fu_control_bp"),] <- 0


method["nonhdl_adj"] <- "~I(tc_adj - hdl_adj)"
pred[c("nonhdl_adj"),] <- 0
method["fu_nonhdl_adj"] <- "~I(tc_adj - hdl_adj)"
pred[c("fu_nonhdl_adj"),] <- 0


method["control_chol"] <- "~I((nonhdl_adj<130)*1)"
pred[c("control_chol"),] <- 0
method["fu_control_chol"] <- "~I((fu_nonhdl_adj<130)*1)"
pred[c("fu_control_chol"),] <- 0


method["control_joint"] <- "~I(control_a1c*control_bp*control_chol*1)"
pred[c("control_joint"),] <- 0
method["fu_control_joint"] <- "~I(fu_control_a1c*fu_control_bp*fu_control_chol*1)"
pred[c("fu_control_joint"),] <- 0

# https://stackoverflow.com/questions/33865161/model-multiple-imputation-with-interaction-terms
# https://thestatsgeek.com/2014/05/10/multiple-imputation-with-interactions-and-non-linear-terms/

for(i_t in interaction_terms){
  print(i_t)
  exposure_term = str_extract(i_t,"^cesd_ge3")
  em_term = str_replace(i_t,pattern=paste0(exposure_term,"_"),replacement = "")
  method[i_t] = paste0("~I(",exposure_term,"*",em_term,")")
  
  # Do not use interaction terms for imputation of the source variables
  pred[c(exposure_term,em_term),i_t] <- 0
}


# Takes ~4h
mi_dfs <- mice(before_imputation,
               method = method,
               pred = pred,
               m=10,maxit=50,seed=500)

saveRDS(mi_dfs, paste0(path_g2a_depression_folder,"/working/g2adhrs_multiple imputation.RDS"))

