# Custom color palettes - should be colorblind accessible  (https://jrnold.github.io/ggthemes/reference/colorblind.html)
pal = c('#000000','#d55e00', '#3DB7E9', '#F748A5', '#359B73','#2271B2', '#e69f00', '#f0e442')
pal2 = c(rgb(0,0,0), rgb(230/255, 159/255,0), rgb(86/255,180/255,233/255), rgb(204/255, 121/255, 167/255), rgb(0, 158/255, 115/255), rgb(0, 114/255, 178/255))
beta_prior = c(prior_string("student_t(3, 0, 10)", class = "b"))

# Separate baseline models of missing data using glmer (NOT USED IN MAIN ANALYSES-MODELS OFTEN DO NOT CONVERGE)
baseline_separate_models_with_missingness_glmer = function(df, model_formula_template, clinical_predictors, conf_level_corrected, bayes=FALSE){
  # convert to xtra long format
  df_long = df %>%
    pivot_longer(cols = all_of(clinical_predictors), names_to = 'name', values_to = 'scaled_predictor') %>%
    group_by(name) 
  
  # Nest a dataframe for each clinical predictor, then model for each one separately
  if(bayes ==FALSE){
  model_frame = df_long %>%
    nest() %>%
    mutate(model = purrr::map(data, ~lme4::glmer(data = ., formula = model_formula_template, family = binomial(link = 'logit'))))
  }
  else{
    beta_prior = c(prior_string("student_t(3, 0, 10)", class = "b"))
    model_frame = df_long %>%
      nest() %>%
      mutate(model = purrr::map(data, ~brms::brm(data = ., formula = model_formula_template, 
                                                 family = bernoulli(link = 'logit'),
                                                 cores = 4, chains = 4, iter = 2000, seed = 11291993,
                                                 prior = beta_prior)))
  }
  return(model_frame)
}

# Function for checking model convergence of many models nested in a df
check_convergences = function(model_df){
  model_df = model_df %>%
    mutate(convergence_message = purrr::map(model, ~summary(.)$optinfo$conv$lme4$messages))
  
  return(model_df)
}


# Pull out tidy coefficients and CIs with/without correction
pull_separate_model_coefs = function(model_df, conf_level_corrected, bayes=TRUE){
  model_df = model_df %>%
    mutate(coef_uncorrected = purrr::map(model, ~broom::tidy(., conf.int=TRUE, conf.level = 0.95)),
           coef_corrected = purrr::map(model, ~broom::tidy(., conf.int=TRUE, conf.level = conf_level_corrected)))
  
  coef_frame_uncorected = model_df %>%
    unnest(coef_uncorrected) %>%
    dplyr::select(-data, -model)  %>%
    mutate(coef_type = 'uncorrected')
  
  coef_frame_corrected = model_df %>%
    unnest(coef_corrected) %>%
    dplyr::select(-data, -model)  %>%
    mutate(coef_type = 'corrected')
  
  coef_frame = rbind(coef_frame_uncorected, coef_frame_corrected)
  
  return(coef_frame)
  
}


# Pull out tidy odds ratios with/without correction
pull_separate_model_odds_ratios = function(model_df, conf_level_corrected, bayes){
  
  if (bayes==FALSE){
  model_df = model_df %>%
    mutate(or_uncorrected = purrr::map(model, ~broom::tidy(., conf.int=TRUE, conf.level = 0.95, exponentiate=TRUE)),
           or_corrected = purrr::map(model, ~broom::tidy(., conf.int=TRUE, conf.level = conf_level_corrected, exponentiate=TRUE)))
  }else{
    model_df = model_df %>%
      mutate(or_uncorrected = purrr::map(model, ~fixef(., probs =c(.025, .975)) %>%
                                           exp() %>%
                                           as.data.frame() %>%
                                           rename_cols(., colnames = c('estimate', 'Est.Error', 'conf.low', 'conf.high')) %>%
                                           mutate(., term = row.names(.))),
             or_corrected = purrr::map(model, ~fixef(., probs =c((1-conf_level_corrected)/2, 
                                                                 1-(1-conf_level_corrected)/2)) %>%
                                         exp() %>%
                                         as.data.frame(.) %>%
                                         rename_cols(., colnames = c('estimate', 'Est.Error', 'conf.low', 'conf.high')) %>%
                                         mutate(., term = row.names(.))))
  }
  
  or_frame_uncorrected = model_df %>%
    unnest(or_uncorrected) %>%
    dplyr::select(-data, -model)  %>%
    mutate(or_type = 'uncorrected')

  or_frame_corrected = model_df %>%
    unnest(or_corrected) %>%
    dplyr::select(-data, -model)  %>%
    mutate(or_type = 'corrected')
  
  or_frame = rbind(or_frame_corrected, or_frame_uncorrected) %>%
    dplyr::select(-or_uncorrected, -or_corrected)
  
  return(or_frame)
}

# package previous functions into a pipeline
or_pipeline = function(df, model_formula_template, clinical_predictors, conf_level_corrected, bayes=FALSE){
  models_df = baseline_separate_models_with_missingness_glmer(df = df, 
                                                              model_formula_template = model_formula_template, 
                                                              clinical_predictors = clinical_predictors,
                                                              bayes=bayes)
  models_df = check_convergences(models_df)
  or_df = pull_separate_model_odds_ratios(models_df, conf_level_corrected = conf_level_corrected, bayes=bayes)
  return(list(models_df=models_df, or_df=or_df))
}


coef_pipeline = function(df, model_formula_template, clinical_predictors, conf_level_corrected, bayes=FALSE){
  models_df = baseline_separate_models_with_missingness_glmer(df = df, 
                                                              model_formula_template = model_formula_template, 
                                                              clinical_predictors = clinical_predictors,
                                                              bayes=bayes)
  models_df = check_convergences(models_df)
  coef_df = pull_separate_model_coefs(models_df, conf_level_corrected = conf_level_corrected, bayes=bayes)
  return(list(models_df=models_df, coef_df=coef_df))
}

# Create OR plot
baseline_predictors_missingness_or_plot = function(or_df, m, bayes=FALSE){
  
  # categorize & rename predictors
  or_df = or_df %>%
    mutate(variable_category = case_when(
      name %in% c('current_medication', 'current_therapy', 'hospitalization_pastyear', 
                      'visit_er_pastyear') ~ 'Service',
      name %in% c('SITBI_SB_ever', 'SITBI_sa_ever', 'SITBI_engaged_nssi_ever', 
                      'SITBI_si_ever') ~ 'SITBI',
      name %in% c('SHAPS_TOT_r_i', 'MFQ_i', 'SCARED_i', 'SSI19_i', 'INQ_burdenensomeness_i', 
                      'INQ_thwartedbelonging_i', 'comorbidity_current') ~ 'Psychopathology'),
      name = dplyr::recode(name, 
                           'interview_age_years'='Age (Years)',
                           'SITBI_engaged_nssi_ever'='Lifetime NSSI (SITBI)',
                           'MFQ_i'='Depression Symptoms (MFQ)',
                           'SITBI_sa_ever'='Lifetime Suicide Attempt (SITBI)', 
                           'SHAPS_TOT_r_i'='Anhedonia Symptoms (SHAPS)',
                           'comorbidity_current'='# Current Diagnoses\n(MINI-KID)',
                           'SITBI_si_ever'='Lifetime Suicidal Ideation (SITBI)', 
                           'hospitalization_pastyear' = 'Past Year Hospitalization',
                           'visit_er_pastyear'='Past Year ED Visit', 
                           'SITBI_SB_ever'='Lifetime Suicidal Behavior (SITBI)',
                           'SCARED_i'='Anxiety Symptoms (SCARED)', 
                           'SSI19_i'='Suicidal Ideation Severity (SSI)',
                           'INQ_burdenensomeness_i'='Perceived Burdensomeness (INQ)', 
                           'INQ_thwartedbelonging_i'='Thwarted Belonging (INQ)',
                           'hispanic'='Hispanic', 'sexMTRUE'='Assigned Male Sex',
                           'siteCUMC'='Site (NYC)', 
                           'current_therapy'='Current Therapy',
                           'current_medication'='Current Medication'),
      continuous = ifelse(variable_category == 'Psychopathology', '*', ''),
      name = paste0(name, continuous),
      missingness_type = ifelse(grepl('Survey', type), 'Survey', 'Passive'))
  
  
  if (bayes==FALSE){
    sig_frame = mutate(or_df, sig_star = case_when(
      p.value >= 0.05 ~ ' ',
      p.value < 0.05 & p.value >= 0.05/m ~ '+',
      p.value < 0.05/m ~ '#'))
  }else{
    sig_frame = or_df %>%
      ungroup() %>%
      mutate(., sig_star = case_when( 
        conf.low < 1 & conf.high <1 & or_type == 'corrected' ~ '#',
        conf.low > 1 & conf.high >1 & or_type == 'corrected' ~ '#',
        conf.low < 1 & conf.high <1 & or_type == 'uncorrected' ~ '+',
        conf.low > 1 & conf.high >1 & or_type == 'uncorrected' ~ '+',
        conf.low < 1 & conf.high >=1 ~ ' ',
        conf.low <=1 & conf.high >1 ~ ' '))
    
    sig_frame = pivot_wider(sig_frame,
                            id_cols = c('type', 'name', 'term'),
                            names_from = or_type,
                            values_from = sig_star)
    
    sig_frame = mutate(sig_frame, sig_star = case_when(
      corrected == '#' ~ corrected,
      corrected != '#' ~ uncorrected
    ))
    sig_frame = left_join(or_df, sig_frame, by = c('type', 'name', 'term')) %>%
      dplyr::filter(term == 'scaled_predictor')
  }
  
  # filter only for predictors of interest
  or_df = dplyr::filter(or_df, term == 'scaled_predictor')
  
  # Create plot
  mlm_or_plt = or_df %>%
    dplyr::filter(or_type == 'uncorrected') %>%
    ggplot(data=., aes(x = name, y = estimate)) +
    geom_point(position = position_dodge(0.6), aes(color = type)) +
      geom_errorbar(data = dplyr::filter(or_df, or_type == 'corrected'), 
                    aes(ymin = conf.low, ymax = conf.high, color = type), 
                    width = 0, position = position_dodge(0.6)) +
      geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = type), 
                    lwd = 1, width = 0, position = position_dodge(0.6)) +
      geom_text(data = dplyr::filter(sig_frame, or_type == 'corrected'), 
                aes(x = name, y = conf.high*1.2, label = sig_star, group = type, color = type),
                position = position_dodge(0.6), show.legend = FALSE) +
      coord_flip() +
      labs(x = NULL, y = NULL) +  
      geom_hline(yintercept = 1, lty = 2) +
      labs(color = NULL) +
      scale_color_manual(values= c(
                           "Accelerometer" = pal2[1], 
                           "Daily Mood Survey" = pal2[2], 
                           "GPS" = pal2[3], 
                           "Keyboard Input" = pal2[4], 
                           "Weekly SI Survey" = pal2[5]
                         ),
                         limits= c(
                           "Keyboard Input", 
                           "GPS", 
                           "Accelerometer",
                           "Weekly SI Survey",
                           "Daily Mood Survey"
                         )) +
      scale_y_log10() +
      facet_grid(rows=vars(variable_category), cols = vars(missingness_type), drop = TRUE, scales = 'free_y', space = 'free_y') +
      scale_x_discrete(limits = rev) +
      theme(strip.text.y = element_blank())

  return(mlm_or_plt)
}


# Sociodemographic model function
baseline_sociodemographic_model_with_missingness_glmer = function(model_formula, df){
  sociodem_model = df %>%
    lme4::glmer(data = ., formula = model_formula, family = binomial(link = 'logit'))
  return(sociodem_model)
}

baseline_sociodemographic_model_with_missingness_brm = function(model_formula, df){
  beta_prior = c(prior_string("student_t(3, 0, 10)", class = "b"))
  sociodem_model = df %>%
    brms::brm(data = ., formula = model_formula, family = bernoulli(link = 'logit'),
              cores = 4, chains = 4, iter = 2000, seed = 11291993,
              prior = beta_prior)
  return(sociodem_model)
}

# Extract ORs from sociodemographic model
baseline_sociodemographic_model_odds_ratio = function(sociodem_model, type, conf_level_corrected, bayes=FALSE){
  
  if (bayes==FALSE){
  or_uncorrected = broom.mixed::tidy(sociodem_model, conf.int=TRUE, exponentiate=TRUE) %>% 
    mutate(correction = 'uncorrected')
  
  or_corrected = broom.mixed::tidy(sociodem_model, conf.int=TRUE, conf.level = conf_level_corrected, exponentiate=TRUE) %>% 
    mutate(correction = 'corrected')
  }else{
    or_uncorrected = sociodem_model %>%
      fixef(., probs =c(.025, .975)) %>%
      exp() %>%
      as.data.frame() %>%
      rename_cols(., colnames = c('estimate', 'Est.Error', 'conf.low', 'conf.high')) %>%
      mutate(., term = row.names(.), correction = 'uncorrected')
    
    
    or_corrected = sociodem_model %>%
      fixef(., probs =c((1-conf_level_corrected)/2, 1-(1-conf_level_corrected)/2)) %>%
      exp() %>%
      as.data.frame(.) %>%
      rename_cols(., colnames = c('estimate', 'Est.Error', 'conf.low', 'conf.high')) %>%
      mutate(., term = row.names(.), correction = 'corrected')
  }
  
  or_df = rbind(or_uncorrected, or_corrected)
  or_df = mutate(or_df, type = type)
  
  return(or_df)
  
}

# Make OR plot for sociodemographic variables
make_sociodem_or_plot = function(sociodem_or_df, m, bayes=FALSE){
  sociodem_or_df = sociodem_or_df %>%
    dplyr::filter(!grepl('Intercept', term), !term %in% c('personal_time_z', 'weekend1', 'weekend'), !grepl('summer', term)) %>%
    mutate(term = dplyr::recode(term, 'interview_age_years'='Age (1 Year)*',
                                'SITBI_engaged_nssi_ever'='Lifetime NSSI (SITBI)',
                                'MFQ_i'='Depression Symptoms (MFQ)',
                                'SITBI_sa_ever'='Lifetime Suicide Attempt (SITBI)', 'SHAPS_TOT_r_i'='Anhedonia (SHAPS)','comorbidity_current'='# Current Diagnoses',
                                'SITBI_si_ever'='Lifetime Suicidal Ideaction (SITBI)', 'hospitalization_pastyear' = 'Past Year Hospitalization',
                                'visit_er_pastyear'='Past Year ED Visit', 'SITBI_SB_ever'='Lifetime Suicidal Behavior (SITBI)',
                                'SCARED_i'='Anxiety Symptoms (SCARED)', 'SSI19_i'='Suicidal Ideation Severity (SSI)',
                                'INQ_burdenensomeness_i'='Perceived Burdensomeness (INQ)', 'INQ_thwartedbelonging_i'='Thwarted Belonging (INQ)',
                                'hispanic'='Hispanic','income_ordinal'='Parental Income',
                                'cisgender'='Cisgender', 'sexMTRUE'='Assigned Male Sex', 'genderMTRUE'='Male Gender', 'heterosexual'='Heterosexual',
                                'siteCUMCTRUE'='Site (NYC)', 'parentscollege'='Parent Completed College', 'current_therapy'='Current Therapy',
                                'current_medication'='Current Medication',
                                'interview_date_numeric'='Interview Date*'),
           variable_category = case_when(
             term %in% c('White', 'Hispanic', 'Black', 'Asian') ~ 'Race/Ethnicity',
             term %in% c('Site (NYC)', 'iPhone', 'Interview Date*') ~ 'ZLogistic',
             term %in% c('Assigned Male Sex', 'Age (1 Year)*') ~ 'Demog'
           ),
           missingness_type = ifelse(grepl('Survey', type), 'Survey', 'Passive'))
  
  options(scipen = 999)
  
  
  if (bayes==FALSE){
    sig_frame = mutate(sociodem_or_df, sig_star = case_when(
      p.value >= 0.05 ~ ' ',
      p.value < 0.05 & p.value >= 0.05/m ~ '+',
      p.value < 0.05/m ~ '#'))
  }else{
    sig_frame = sociodem_or_df %>%
      ungroup() %>%
      mutate(., sig_star = case_when( 
        conf.low < 1 & conf.high <1 & correction == 'corrected' ~ '#',
        conf.low > 1 & conf.high >1 & correction == 'corrected' ~ '#',
        conf.low < 1 & conf.high <1 & correction == 'uncorrected' ~ '+',
        conf.low > 1 & conf.high >1 & correction == 'uncorrected' ~ '+',
        conf.low < 1 & conf.high >=1 ~ ' ',
        conf.low <=1 & conf.high >1 ~ ' '))
    
    print(sig_frame)
    sig_frame = pivot_wider(sig_frame,
                            id_cols = c('type', 'term'),
                            names_from = correction,
                            values_from = sig_star)
    
    sig_frame = mutate(sig_frame, sig_star = case_when(
      corrected == '#' ~ corrected,
      corrected != '#' ~ uncorrected
    ))
    sig_frame = left_join(sociodem_or_df, sig_frame, by = c('type', 'term')) 
  }
  
  sociodem_plot = sociodem_or_df %>%
    dplyr::filter(correction == 'uncorrected') %>%
    ggplot(data = ., aes(x = term, y = estimate, color = type)) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_point(position = position_dodge(0.4)) +
    geom_errorbar(data = dplyr::filter(sociodem_or_df, correction == 'corrected'),
                  aes(ymin = conf.low, ymax = conf.high), position = position_dodge(0.4), width = 0) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(0.4), lwd = 1, width = 0) +
    geom_text(data = dplyr::filter(sig_frame, correction == 'corrected'), 
              aes(x = term, y = conf.high*1.2, label = sig_star, group = type),
              position = position_dodge(0.6)) +
    coord_flip() +
    scale_color_manual(values= c(
      "Accelerometer" = pal2[1], 
      "Daily Mood Survey" = pal2[2], 
      "GPS" = pal2[3], 
      "Keyboard Input" = pal2[4], 
      "Weekly SI Survey" = pal2[5]
    ),
    limits= c(
      "Keyboard Input", 
      "GPS", 
      "Accelerometer",
      "Weekly SI Survey",
      "Daily Mood Survey"
    )) + 
    scale_y_log10(breaks = c(0.01, 0.1, 1, 10)) +
    labs(x = NULL, y = 'Association with Missingness\nAdjusted Odds Ratio') +
    theme(legend.position = 'none') +
    facet_grid(rows=vars(variable_category), cols = vars(missingness_type), drop = TRUE, scales = 'free_y', space = 'free_y') +
    scale_x_discrete(limits = rev) +
    theme(strip.text = element_blank())
  
  return(sociodem_plot)
  
}

# Turn column renaming (using column order) into a function
rename_cols = function(df, colnames){
  names(df) = colnames
  return(df)
}
