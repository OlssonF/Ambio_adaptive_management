# Ambio Adaptive management modelling and analysis
This repository includes the code to run GOTM modelling using a sensitivity approach to investigate how changes in flow will interact with air temeprature warming to impact lake water temperatures. 
The scripts to run the workflow are in the `Workflows` folder. 

The steps are 0. Install packages (run `install_packages.R`), 1. run GOTM scenarios; 2. run post-model analysis; and 3. Generate figures

## To rerun post-modelling analysis and reproduce figures
- Run `download_output.R`, this will download the GOTM output generated using the workflow below from the archive version on Zenodo (Olsson et al., 2023). This means if you don't have GOTM installed and setup you can run the analysis and figures.
- Run `Workflows/03a_calculate_metrics.R`. Calculates the water column stability and surface water temperatures from GOTM output. 
- Run `Workflows/03b_aggregate_to_timesteps.R`. Calculates the seasonal metrics.
- Run `Workflows/03c_calculate_change.R`. Calculates the change from baseline conditions (no air temperature or flow change).
- Run `Workflows/03d_variability_in_metrics.R`. Calculates the variability among years for each metric.
- Run `Workflows/04_airtemp_equivalents.R`. Estimates the air temperature equivalents and mitigation potential of each flow scenario. 
- Run `Workflows/05_plots_MS_SI.R`. Generates manuscript and supplementary figures. 
  
## To run the GOTM scenarios
- Run `Workflows/01_generate_inputs.R`. Generates each of the 81 flow/air-temperature scenario driving datasets.
- Run `Workflows/02_run_scenarios.R`. Runs GOTM with the relevent meteorology and inflow file (generated in 01) and saves the output. 
Then continue with workflow as above.

## GOTM folder
The `GOTM` folder contains all the configuration files needed to run GOTM as well as the baseline driving data. The worflow scripts above interact with the files in this directory as needed. There is no need to manually modify any files here. 

### GOTM/parsac
The configuration files for GOTM calibration using ParSAC are also provided in `GOTM/parsac` but not included in the above workflow. This workflow assumes that calibration/validation of GOTM parameter values has already occured. To run parsac a GOTM executable is needed in this folder. This is the same executable as is run for the experiments (included in the GOTMr installation). Within `Workflows` the script 00_parsac_calibration describes the generation of the configuration files used in the automated calibration routine. The final output of the parsac calibration is included for reference. 

## R folder

- `helper_functions.R`contains small functions to deal with date times and calculating changes
- `lake-inflow difference.R` contains code to calculate the average lake - inflow temperature difference across the modelling period and generate an SI plot of results
- `InflowR_from_AT.R` shows the method used to estimate inflow temperature when no observations were available. Uses raw data from the `Data` folder.

#

Olsson, F., Mackay, E., Spears, B., Philip, B., & Jones, I. D. (2023). Interacting impacts of hydrological changes and climate warming on lake temperatures highlight the potential for adaptive management: Model output [Data set]. Zenodo. https://doi.org/10.5281/zenodo.10309370
