# Identifying Factors Impacting Missingness within Smartphone-based Research: Implications for Intensive Longitudinal Studies of Adolescent Suicidal Thoughts and Behaviors 
 

Analyses of Missing Experience Sampling &amp; Passive Smartphone Sensor Data in a Study of Adolescents at Risk for Suicide

## Repository table of contents:

| Sub-directory      | Files | Contents |
| ----------- | ----------- |----------- |
| [0_setup](0_setup)      | | Aggregating and setting up data structures for analysis across modalities (experience sampling, passive sensing, self-report & interview measures). Also includes calculations of internal consistency and reliability |
| [1_analysis](1_analysis)      | `0_descriptive_results.Rmd`| Descriptive analyses, including associations between missingness of  |
| [1_analysis](1_analysis)      | `1a-e`|   **Aim 1** models of between-participant associations between clinical and sociodemographic variables and missing data  |
| [1_analysis](1_analysis)      | `2a-c`|   **Aim 1** plots and tables for models of between-participant associations between clinical and sociodemographic variables and missing data  |
| [1_analysis](1_analysis)      | `3a-c`|   **Aim 3** analyses of whether changes in missingness precede or follow changes in weekly suicidal ideation frequency or daily mood |
| [1_analysis](1_analysis)      | `4a-d`|   **Aim 2** analyses of proximal passive smartphone sensor predictors of missing daily mood surveys |
| [1_analysis](1_analysis)      | `5a-b`|   **Aim 1** analyses of time-varying associations between missingness and changes in clinical self-report measures|
| [1_analysis](1_analysis)      | `6a-d`|   **Aim 1** analyses of time-varying associations between temporal factors (day of week, month, time since baseline) and missing data|
| [1_analysis](1_analysis)      | `7`|   **Aim 3** analyses of between-participant associations between missing data and suicidal events during the 6-month study period|
| [1_analysis](1_analysis)      | `8` |   Supplemental analyses of missingness of passive smartphone sensor data as a function of time since the last observed daily mood survey response|
| [1_analysis](1_analysis)      | `9` |   Supplemental analyses of missing data as a function of local public high school closures due to Covid-19 |
| [1_analysis](1_analysis)      | `model_wrapper_functions.R` |  Custom helper functions used in modeling, plotting, and wrangling data across multiple analyses |