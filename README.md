# Nowcasting_covid19_hospitalizations

---

This is the public repository for the LMU Munich contribution (LMU_StaBLab-GAM_nowcast)
to the [German COVID-19 Hospitalization Nowcast Hub](https://covid19nowcasthub.de/).
The repository contains the code to perform the daily nowcast updates as well
as all daily nowcast results. Methodological details of the implemented nowcasting
approach are described in [Fritz et al. (2022)](https://journals.sagepub.com/doi/10.1177/1471082X221124628) and [Schneble et al. (2020)](https://onlinelibrary.wiley.com/doi/full/10.1002/bimj.202000143).


### Structure of zje repository:


### 1 Data

- `Data_RKI`: folder with preprocessed daily RKI data
- `population_age.rds`: german and federal state population by age
- `population_German.rds`: total population of the german federal states
- `realized7_after40d.csv`: realized hospitalizations of the last 7 days after 40 days

### 2 Code

Main code files:

- `Analysis_RKI_states.R`: code to perform nowcasting of hospitalizations on federal state level
- `Evaluation_RKI.R`: generate evaluation plots
- `update_RKI_nowcast_automatically.R`: script to automatically scrape new data and run computation of nowcasts

In folder *Functions*:

- `Fitting.R`: fitting functions
- `formatting_RKI_data.R`: data formatting functions
- `Intervals.R`: interval correction function
- `construct_realized7_after40d.R`: Script to generate dataset with realized hospitalizations after 40 days
- `Evaluation.R`: evaluation functions
- `formatting_nowcast_data.R`: data formatting functions
- `merge_corrections.R`: merging and formatting functions
- `retrospective_eval.R`: functions for retro evaluation

### 3 Results

A folder for the results of one day consists of:

- `...-LMU_StaBLab-GAM_nowcast.csv`: nowcasting results for all locations
- `coverage_correction_... .csv`: coverage corrected nowcast results by location
- `nowcasting_model_... .rds`: model object by location
- `nowcasting_results_... .csv`: uncorrected nowcasting results by location

