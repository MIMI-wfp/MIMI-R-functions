
fe_full_prob <- function(data, group1 = NULL, group2 = NULL, bio_avail = 10, hh_weight = NULL, strata = NULL, psu = NULL) {
  
  # Compute probability of inadequacy based on iron intake
  data <- data %>%
    mutate(prob_inad = case_when(
      bio_avail == 5 ~ case_when(
        fe_mg <= 15 ~ "1",
        fe_mg <= 16.7 & fe_mg > 15 ~ "0.96",
        fe_mg <= 18.7 & fe_mg > 16.7 ~ "0.93",
        fe_mg <= 21.4 & fe_mg > 18.7 ~ "0.85",
        fe_mg <= 23.6 & fe_mg > 21.4 ~ "0.75",
        fe_mg <= 25.7 & fe_mg > 23.6 ~ "0.65",
        fe_mg <= 27.8 & fe_mg > 25.7 ~ "0.55",
        fe_mg <= 30.2 & fe_mg > 27.8 ~ "0.45",
        fe_mg <= 33.2 & fe_mg > 30.2 ~ "0.35",
        fe_mg <= 37.3 & fe_mg > 33.2 ~ "0.25",
        fe_mg <= 45.0 & fe_mg > 37.3 ~ "0.15",
        fe_mg <= 53.5 & fe_mg > 45.0 ~ "0.08",
        fe_mg <= 63.0 & fe_mg > 53.5 ~ "0.04",
        fe_mg > 63 ~ "0"),
      bio_avail == 10 ~ case_when(
        fe_mg <= 7.5 ~ "1",
        fe_mg <= 8.4 & fe_mg > 7.5 ~ "0.96",
        fe_mg <= 9.4 & fe_mg > 8.4 ~ "0.93",
        fe_mg <= 10.7 & fe_mg > 9.4 ~ "0.85",
        fe_mg <= 11.8 & fe_mg > 10.7 ~ "0.75",
        fe_mg <= 12.9 & fe_mg > 11.8 ~ "0.65",
        fe_mg <= 13.9 & fe_mg > 12.9 ~ "0.55",
        fe_mg <= 15.1 & fe_mg > 13.9 ~ "0.45",
        fe_mg <= 16.6 & fe_mg > 15.1 ~ "0.35",
        fe_mg <= 18.7 & fe_mg > 16.6 ~ "0.25",
        fe_mg <= 22.5 & fe_mg > 18.7 ~ "0.15",
        fe_mg <= 26.7 & fe_mg > 22.5 ~ "0.08",
        fe_mg <= 31.5 & fe_mg > 26.7 ~ "0.04",
        fe_mg > 31.5 ~ "0"),
      bio_avail == 15 ~ case_when(
        fe_mg <= 5 ~ "1",
        fe_mg <= 5.6 & fe_mg > 5 ~ "0.96",
        fe_mg <= 6.2 & fe_mg > 5.6 ~ "0.93",
        fe_mg <= 7.1 & fe_mg > 6.2 ~ "0.85",
        fe_mg <= 7.9 & fe_mg > 7.1 ~ "0.75",
        fe_mg <= 8.6 & fe_mg > 7.9 ~ "0.65",
        fe_mg <= 9.3 & fe_mg > 8.6 ~ "0.55",
        fe_mg <= 10.1 & fe_mg > 9.3 ~ "0.45",
        fe_mg <= 11.1 & fe_mg > 10.1 ~ "0.35",
        fe_mg <= 12.4 & fe_mg > 11.1 ~ "0.25",
        fe_mg <= 15.0 & fe_mg > 12.4 ~ "0.15",
        fe_mg <= 17.8 & fe_mg > 15.0 ~ "0.08",
        fe_mg <= 21.0 & fe_mg > 17.8 ~ "0.04",
        fe_mg > 21.0 ~ "0")
    )) %>%
    mutate(prob_inad = as.numeric(prob_inad))
  
  # Assign weight column 
  if (!is.null(hh_weight) && hh_weight %in% colnames(data)) {
    data <- data %>% rename(weight = all_of(hh_weight))
  } else {
    data <- data %>% mutate(weight = 1)
  }
  
  # Create survey design
  survey_design <- data %>%
    as_survey_design(
      ids = !!rlang::sym(psu),
      strata = !!rlang::sym(strata),
      weights = weight,
      nest = TRUE
    )
  
  # Handle grouping
  grouping_vars <- c(group1, group2)
  
  if (length(grouping_vars) == 0 || all(sapply(grouping_vars, is.null))) {
    result <- survey_design %>%
      summarise(
        prev_inad = survey_mean(prob_inad, na.rm = TRUE, vartype = "se")*100
      ) %>%
      mutate(subpopulation = "Total population") %>%
      select(subpopulation, prev_inad, se)
  } else {
    result <- survey_design %>%
      group_by(across(all_of(grouping_vars))) %>%
      summarise(
        prev_inad = survey_mean(prob_inad, na.rm = TRUE, vartype = "se")*100,
        .groups = "drop"
      )
  }
  
  return(result)
}





# # Use function (sen_base_ai_fe df has hhid, adm1, adm2, survey_wgt, fe_mg ai)
# sen_fe_inadequacy_adm1 <- fe_full_prob(
#   all_intake,
#   group1 = "adm1",
#   bio_avail = 10,
#   hh_weight = "survey_wgt" ,
#   strata = "res",
#   psu = "ea"
# )

# print(sen_fe_inadequacy_adm1)

