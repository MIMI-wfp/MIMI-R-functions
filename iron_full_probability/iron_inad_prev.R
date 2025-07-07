## code to turn this chunk into a function

library(dplyr)
library(tidyr)

here::here()

#Probabistic apparent intake inadequacy

fe_full_prob <- function(data, group1 = NULL, group2 = NULL, bio_avail = 5, adjust_afe = FALSE){
  # Function that calculates the full probabilistic model for fe
  # Default, just calculates for full population
  # Add in parameters for group 1 and/or group 2 and it will calculate for
  # each sub population
  # The resultant data frame will have a column for each sub population with the 
  # first label corresponding to groups within group1, and the second label corresponding
  # to grousp within group2
  # Also can set the bioavailability to either 5%, 10% or 15%
  
  tryCatch(
  
  if(missing(group1)&missing(group2)){
    if(adjust_afe == TRUE){data <- data %>%mutate(ai_afe = fe_supply / afe)}
    data %>% 
      # mutate(ai_afe = fe_supply / afe) %>%                   # Generating apparent iron intake variable
      mutate(prob_inad = 
               case_when(
                 bio_avail == 5 ~
                   case_when(
                     # Dividing intake distribution into probability of inadequacy catagories 
                      ai_afe <= 15 ~ "1",
                      ai_afe <= 16.7 & ai_afe > 15 ~ "0.96",
                      ai_afe <= 18.7 & ai_afe > 16.7 ~ "0.93",
                      ai_afe <= 21.4 & ai_afe > 18.7 ~ "0.85",
                      ai_afe <= 23.6 & ai_afe > 21.4 ~ "0.75",
                      ai_afe <= 25.7 & ai_afe > 23.6 ~ "0.65",
                      ai_afe <= 27.8 & ai_afe > 25.7 ~ "0.55",
                      ai_afe <= 30.2 & ai_afe > 27.8 ~ "0.45",
                      ai_afe <= 33.2 & ai_afe > 30.2 ~ "0.35",
                      ai_afe <= 37.3 & ai_afe > 33.2 ~ "0.25",
                      ai_afe <= 45.0 & ai_afe > 37.3 ~ "0.15",
                      ai_afe <= 53.5 & ai_afe > 45.0 ~ "0.08",
                      ai_afe <= 63.0 & ai_afe > 53.5 ~ "0.04",
                      ai_afe > 63 ~ "0"),
                 bio_avail == 10 ~
                   case_when(                            # Dividing intake distribution into probability of inadequacy catagories 
                     ai_afe <= 7.5 ~ "1",
                     ai_afe <= 8.4 & ai_afe > 7.5 ~ "0.96",
                     ai_afe <= 9.4 & ai_afe > 8.4 ~ "0.93",
                     ai_afe <= 10.7 & ai_afe > 9.4 ~ "0.85",
                     ai_afe <= 11.8 & ai_afe > 10.7 ~ "0.75",
                     ai_afe <= 12.9 & ai_afe > 11.8 ~ "0.65",
                     ai_afe <= 13.9 & ai_afe > 12.9 ~ "0.55",
                     ai_afe <= 15.1 & ai_afe > 13.9 ~ "0.45",
                     ai_afe <= 16.6 & ai_afe > 15.1 ~ "0.35",
                     ai_afe <= 18.7 & ai_afe > 16.6 ~ "0.25",
                     ai_afe <= 22.5 & ai_afe > 18.7 ~ "0.15",
                     ai_afe <= 26.7 & ai_afe > 22.5 ~ "0.08",
                     ai_afe <= 31.5 & ai_afe > 26.7 ~ "0.04",
                     ai_afe > 31.5 ~ "0"),
                 bio_avail == 15 ~
                   case_when(                            # Dividing intake distribution into probability of inadequacy catagories 
                     ai_afe <= 5 ~ "1",
                     ai_afe <= 5.6 & ai_afe > 5 ~ "0.96",
                     ai_afe <= 6.2 & ai_afe > 5.6 ~ "0.93",
                     ai_afe <= 7.1 & ai_afe > 6.2 ~ "0.85",
                     ai_afe <= 7.9 & ai_afe > 7.1 ~ "0.75",
                     ai_afe <= 8.6 & ai_afe > 7.9 ~ "0.65",
                     ai_afe <= 9.3 & ai_afe > 8.6 ~ "0.55",
                     ai_afe <= 10.1 & ai_afe > 9.3 ~ "0.45",
                     ai_afe <= 11.1 & ai_afe > 10.1 ~ "0.35",
                     ai_afe <= 12.4 & ai_afe > 11.1 ~ "0.25",
                     ai_afe <= 15.0 & ai_afe > 12.4 ~ "0.15",
                     ai_afe <= 17.8 & ai_afe > 15.0 ~ "0.08",
                     ai_afe <= 21.0 & ai_afe > 17.8 ~ "0.04",
                     ai_afe > 21.0 ~ "0")
          )
        )%>% 
      group_by(prob_inad ) %>%     # Counting number of observations that fall into each probability category
      summarise(
        fe_prop = n()
      ) %>% 
      ungroup() %>% 
      summarise(   
        across(-prob_inad,
              ~ sum(.x*as.numeric(prob_inad))/sum(.x)*100
        )
      ) %>% 
      pivot_longer(cols = everything()) %>% 
      rename(subpopulation = name, prev_inad = value)
  }
  else{
    if(adjust_afe == TRUE){data <- data %>%mutate(ai_afe = fe_supply / afe)}
    
      data %>%
        # mutate(ai_afe = fe_supply / afe) %>%                   # Generating apparent iron intake variable
        mutate(prob_inad = case_when(
          bio_avail == 5 ~
            case_when(                            # Dividing intake distribution into probability of inadequacy catagories 
              ai_afe <= 15 ~ "1",
              ai_afe <= 16.7 & ai_afe > 15 ~ "0.96",
              ai_afe <= 18.7 & ai_afe > 16.7 ~ "0.93",
              ai_afe <= 21.4 & ai_afe > 18.7 ~ "0.85",
              ai_afe <= 23.6 & ai_afe > 21.4 ~ "0.75",
              ai_afe <= 25.7 & ai_afe > 23.6 ~ "0.65",
              ai_afe <= 27.8 & ai_afe > 25.7 ~ "0.55",
              ai_afe <= 30.2 & ai_afe > 27.8 ~ "0.45",
              ai_afe <= 33.2 & ai_afe > 30.2 ~ "0.35",
              ai_afe <= 37.3 & ai_afe > 33.2 ~ "0.25",
              ai_afe <= 45.0 & ai_afe > 37.3 ~ "0.15",
              ai_afe <= 53.5 & ai_afe > 45.0 ~ "0.08",
              ai_afe <= 63.0 & ai_afe > 53.5 ~ "0.04",
              ai_afe > 63 ~ "0"),
          bio_avail == 10 ~
            case_when(                            # Dividing intake distribution into probability of inadequacy catagories 
              ai_afe <= 7.5 ~ "1",
              ai_afe <= 8.4 & ai_afe > 7.5 ~ "0.96",
              ai_afe <= 9.4 & ai_afe > 8.4 ~ "0.93",
              ai_afe <= 10.7 & ai_afe > 9.4 ~ "0.85",
              ai_afe <= 11.8 & ai_afe > 10.7 ~ "0.75",
              ai_afe <= 12.9 & ai_afe > 11.8 ~ "0.65",
              ai_afe <= 13.9 & ai_afe > 12.9 ~ "0.55",
              ai_afe <= 15.1 & ai_afe > 13.9 ~ "0.45",
              ai_afe <= 16.6 & ai_afe > 15.1 ~ "0.35",
              ai_afe <= 18.7 & ai_afe > 16.6 ~ "0.25",
              ai_afe <= 22.5 & ai_afe > 18.7 ~ "0.15",
              ai_afe <= 26.7 & ai_afe > 22.5 ~ "0.08",
              ai_afe <= 31.5 & ai_afe > 26.7 ~ "0.04",
              ai_afe > 31.5 ~ "0"),
          bio_avail == 15 ~
            case_when(                            # Dividing intake distribution into probability of inadequacy catagories 
              ai_afe <= 5 ~ "1",
              ai_afe <= 5.6 & ai_afe > 5 ~ "0.96",
              ai_afe <= 6.2 & ai_afe > 5.6 ~ "0.93",
              ai_afe <= 7.1 & ai_afe > 6.2 ~ "0.85",
              ai_afe <= 7.9 & ai_afe > 7.1 ~ "0.75",
              ai_afe <= 8.6 & ai_afe > 7.9 ~ "0.65",
              ai_afe <= 9.3 & ai_afe > 8.6 ~ "0.55",
              ai_afe <= 10.1 & ai_afe > 9.3 ~ "0.45",
              ai_afe <= 11.1 & ai_afe > 10.1 ~ "0.35",
              ai_afe <= 12.4 & ai_afe > 11.1 ~ "0.25",
              ai_afe <= 15.0 & ai_afe > 12.4 ~ "0.15",
              ai_afe <= 17.8 & ai_afe > 15.0 ~ "0.08",
              ai_afe <= 21.0 & ai_afe > 17.8 ~ "0.04",
              ai_afe > 21.0 ~ "0")
        )
        ) %>%
        group_by(prob_inad, {{group1}},{{group2}}) %>%     # Counting number of observations that fall into each probability category
        summarise(
        fe_prop = n()
        )%>%
        ungroup() %>%
        pivot_wider(names_from = c({{group1}},{{group2}}), names_prefix = "", #this can be the name of the 'group by" group

                values_from = fe_prop)         %>%
        mutate(across(everything(),
              ~replace_na(.,0)))        %>%

        summarise(
          across(-prob_inad,
               ~sum(.x*as.numeric(prob_inad))/sum(.x)*100
          )
        ) %>%
        pivot_longer(cols = everything()) %>%
        rename(subpopulation = name, prev_inad = value)
    

  }
  )
    
}

#========================================================================= With Weights ===============================================================================================
# Full prob function
fe_full_prob <- function(data, group1 = NULL, group2 = NULL, bio_avail = 10, hh_weight = NULL) {
  
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
    ))
  
  # Assign weight column 
  if (!is.null(hh_weight) && hh_weight %in% colnames(data)) {
    data <- data %>% rename(weight = all_of(hh_weight))
  } else {
    data <- data %>% mutate(weight = 1)
  }
  
  # Compute prevalence of iron inadequacy
  if (missing(group1) & missing(group2)) {
    result <- data %>%
      group_by(prob_inad) %>%
      summarise(fe_weighted = sum(weight), .groups = "drop") %>%
      summarise(prev_inad = sum(fe_weighted * as.numeric(prob_inad)) / sum(fe_weighted) * 100) %>%
      pivot_longer(cols = everything(), names_to = "subpopulation", values_to = "prev_inad")
  } else {
    result <- data %>%
      group_by(prob_inad, {{group1}}, {{group2}}) %>%
      summarise(fe_weighted = sum(weight), .groups = "drop") %>%
      pivot_wider(names_from = {{group1}}, values_from = fe_weighted, values_fill = 0) %>%
      rename_with(~ gsub("^prev_inad_", "", .x)) %>%  # Remove prefix to maintain original names
      summarise(across(-prob_inad, ~ sum(.x * as.numeric(prob_inad)) / sum(.x) * 100, .names = "{.col}")) %>%
      pivot_longer(cols = everything(), names_to = "subpopulation", values_to = "fe_mg_prop")
  }
  
  return(result)
}


### Use case for Senegal
## sen_fe_inadequacy_adm1 <- fe_full_prob(sen_base_ai_fe, group1 = adm1, bio_avail = 10, hh_weight = "hh_weight")

## sen_fe_inadequacy_adm2 <- fe_full_prob(sen_base_ai_fe, group1 = adm2, bio_avail = 10, hh_weight = "hh_weight")
