# post-processing-mos-point-analysis
This program contains MOS (Model Output Statistics) training code used at FMI

## Dependencies and directory
This repo should be cloned locally to directory point_data_analysis and run from there.
Dependency: Repository github/fmidev/post-processing-constant-lists needs to be cloned to directory ../constant_lists toghether with this code

## Usage
1. Set up variables that define how the training will behave. These can be found at the very beginning of the main program Postgre_query_point_data.R
* **timestamps_series** defines the time interval from which data is retrieved
* **modelobspairs_minimum_sample_size** Defines the minimum sample size used for a particular model
* **date_string** not sure if this is used at all
* **mos_label** the shortname abbreviation for the MOS training coefficients. Each coefficient set is identified based on this and should be created on the date when the training was begun (like "MOS_ECMWF_020320")
* **predictor_set** A "human-readable" naming for a specific predictor set
* **derived_variables** Derived variables (a list from all the available ones is at all_variable_lists$derived_variables_all) in addition to those DMO variables of "predictor_set" that should be added to the DMO predictor variables. These are calculated directly at retrieval script retrieve_data_MOS.R or at FetchData_season_analysis_time() (for those variables that only depend from the date and not the specific DMO parameter values of it [like "SOL_ANGLE", "DECLINATION"]))
* **station_list** Pre-defined station list name (possible alternatives can be found at those int list names from all_station_lists).
* **station_numbers** WMON station numbers used in training. If you want to use an arbitrary station number list not included in all_station_lists, assign the station numbers manually here
* **obs_interpolation_method** A few missing observations can be interpolated from the nearest ones. This defines the used interpolation method (used in retrieve_data_CLDB.R)
* **max_interpolate_gap** A few missing observations can be interpolated from the nearest ones. This defines the maximum length of the missing gap (used in retrieve_data_CLDB.R)
* **verif_stationtype** A parameter relevant only for the verif data
* **output_dir** Output directory where coefficients are stored to (see /data/statcal/results/MOS_coefficients/in_progress/"mos_label"/)
* **max_variables** Maximum number of predictors used in model training
* **fitting_algorithm** Fitting algorithm used
* **fitting_method** either purrr or glm
* By default, the used predictor variables are defined at rownames(all_variable_lists[["estimated_parameters"]]). Modify this list at the script ../constant_lists/list_generate_scripts/create_and_plot_lists.R if you want to add more predictand variables to read_in_variable_lists.Rdata. These variables are iterated through one by one at function MOS_training().
2. From command line, give the command
```console
R CMD Postgre_query_point_data.R Postgre_query_point_data.Rout
```
3. Implementation of the trained coefficients to operative environment is not currently automated, but is manually done like in (https://jira.fmi.fi/browse/STU-12862). New coefficients by default go to dev-version and it the verif results look good the current dev-version should be changed to operative.
4. Update Wiki-page https://wiki.fmi.fi/display/PAK/MOS-versionumeroinnit whenever changes in the coefficients take place.


## Call chain (simplified)
**Postgre_query_point_data.R -> MOS_training() (at functions_fitting.R) -> FitWithGlmnR1purrr() (at toMOSutils_functions_glm.R)**

## Files
* **load_libraries_tables_and_open_connections.R** Script opens all the needed libraries, mapping lists, subroutines and database connections
* **choose_variables.R** creates predictor sets based on some "human-readable" naming for it. These namings are needed e.g. in sensitivity tests (which are not included atm)
* **retrieve_data_all.R** calls the functions (**retrieve_data_MOS.R**, **retrieve_data_CLDB.R**, **retrieve_data_verif.R**) depending on what data is retrieved. aviation db retrieval function is not done atm (Nov2020).
* **toMOSutils_functions_glm.R** contains several fitting methods and helper functions
* **functions_fitting.R** contains the for loop structure that divides the whole training data to suitable sized training samples (a unique model is trained for each individual station/predictor/season/analysis_time/forecast_leadtime), calls the needed training functions (at file toMOSutils_functions_glm.R) and saves the trained coefficients of the specific model to a csv-file.

## Unnecessary files
* **functions_purrr.R** not known whether this contains unique information
* **functions.R** not known whether this contains unique information
* **main.R** some old test retrievals
* **main_purrr.R** some old test retrievals
* **Postgre_query_point_data_test_purrr.R** confusing, unlikely necessary at all
* **test_retrievals.R** should be removed

## Other documentation
* https://wiki.fmi.fi/display/PAK/MOS-data
* https://wiki.fmi.fi/display/PROD/MOS_ecflow
* https://wiki.fmi.fi/display/PROD/MOS_ecflow
* https://jira.fmi.fi/issues/?jql=project%20%3D%20POSSE%20AND%20statusCategory%20%3D%20Done%20order%20by%20updated%20DESC


## Some (but obviously not all!) development ideas
* files toMOSutils_functions_glm.R and functions_fitting could be combined in a neater way with each other
* purrr training method is currently implemented for model GlmR1 and not for any of the other methods available at toMOSutils_functions_glm.R
* minimum sample size check is only implemented in function FitWithGlmnR1purrr!
* Sensitivity tests comparing different modeling strategies (like those at https://jira.fmi.fi/browse/POSSE-220) are missing from this code. They only exist for the legacy code located at (devmos / vorlon_point_legacy / Postgre_query_point_data_MOS_crossvalidation.R)
* mos_trace_v is not supported atm
* aviation db retrievals are not supported atm
* new verification db retrievals are not supported atm
* fmisid numbers are not used atm, but only WMON numbers
* max_variables parameter is not passed on the training functions!
* fitting_algorithm is not passed on to function Train.Model(), but is manually defined in MOS_training()!
* Implementation of parallel retrievals to MOS training data (https://jira.fmi.fi/browse/PDTK-5)
* All CLDB retrievals are currently implemented through MOS db (liked table). These would need to be coded to retrieve_data_CLDB.R using Smartmet server interface.
* New MOS db retrievals (Specifications at https://jira.fmi.fi/browse/STU-11582, db retrieval ticket at https://jira.fmi.fi/browse/STU-12895) need to be implemented to retrieve_data_MOS.R
* db username+password is currently using Ylhäisi credentials from a local file at devmos (see load_libraries_tables_and_open_connections.R). A common group username should be used here.


