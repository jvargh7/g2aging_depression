source("hrs/g2adhrs_diagnosed diabetes with exposure and outcome.R")
source("C:/code/external/functions/survey/svysummary.R")
source("C:/code/external/functions/survey/svysd.R")

proportion_vars <- c("fu_control_a1c","fu_control_bp","fu_control_chol","fu_control_joint")

hrs_sy <- svysummary(hrs_all_diagnosed_svy,
                     # c_vars = continuous_vars,
                     p_vars = proportion_vars
                     # g_vars = grouped_vars,
                     # id_vars = "gender"
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))
