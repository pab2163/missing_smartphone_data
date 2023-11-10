# Identifying Factors Impacting Missingness within Smartphone-based Research: Implications for Intensive Longitudinal Studies of Adolescent Suicidal Thoughts and Behaviors 
 

Analyses of Missing Experience Sampling &amp; Passive Smartphone Sensor Data in a Study of Adolescents at Risk for Suicide

## Repository table of contents:

| Sub-directory      | Files | Contents |
| ----------- | ----------- |----------- |
| [0_setup](0_setup)      | `1a-d`| Aggregating individual participant files, cleaning of EMA and passive sensor data |
| [0_setup](0_setup)      | `2a_compile_daily_dataset.Rmd` | Aggregating and setting up data structures for analysis across modalities (experience sampling, passive sensing, self-report & interview measures).|
| [0_setup](0_setup)      | `3_consistency_reliability.Rmd` |  Calculations of internal consistency and reliability for self-report measures |
| [1_analysis](1_analysis)      | `0_descriptive_results.Rmd`| Descriptive analyses, including associations between missingness of  |
| [1_analysis](1_analysis)      | `1a-c`|   **Aim 1** analyses of time-varying associations between temporal factors (day of week, month, time since baseline) and missing data  |
| [1_analysis](1_analysis)      | `2a-h`|   **Aim 1** models of between-participant associations between clinical and sociodemographic variables and missing data |
| [1_analysis](1_analysis)      | `3_supplement_baseline_cox_models.Rmd`|  **Aim 1** Cox models looking at associations between baseline clinical & sociodemographic variables and time to drop out from smartphone data collection  |
| [1_analysis](1_analysis)      | `4a_clinical_selfreport_predict_missing_over_month.Rmd`|   **Aim 1** longitudinal analyses of whether changes in self-report clinical measures predict changes in missing data over the next 30 days |
| [1_analysis](1_analysis)      | `5a_daily_mood_weekly_si_autocor.Rmd`| Models of autocorrelation of daily mood, weekly suicidal ideation frequency, and their missingness|
| [1_analysis](1_analysis)      | `6a-d`|   **Aim 2** analyses of proximal passive smartphone sensor predictors of missing daily mood surveys |
| [1_analysis](1_analysis)      | `7_missing_predict_followup.Rmd`|   **Aim 3** analyses of associations between missing data during the study period and self-report clinical outcomes at 6-month follow-up|
| [1_analysis](1_analysis)      | `8_missing_over_month_predict_clinical_selfreport.Rmd`|   **Aim 3** longitudinal analyses of whether changes in missing data over 30-day periods predict subsequent changes in clinical self-report measures|
| [1_analysis](1_analysis)      | `9a-b`|   **Aim 3** analyses of whether changes in missingness precede or follow changes in weekly suicidal ideation frequency or daily mood |
| [1_analysis](1_analysis)      | `10_suicide_events_during_study.Rmd`|   **Aim 3** analyses of between-participant associations between missing data and suicidal events during the 6-month study period|
| [1_analysis](1_analysis)      | `11a_days_since_last_survey.Rmd` |   Supplemental analyses of missingness of passive smartphone sensor data as a function of time since the last observed daily mood survey response|
| [1_analysis](1_analysis)      | `11b_covid_school_closure.Rmd` |   Supplemental analyses of missing data as a function of local public high school closures due to Covid-19 |
| [1_analysis](1_analysis)      | `model_wrapper_functions.R` |  Custom helper functions used in modeling, plotting, and wrangling data across multiple analyses |
