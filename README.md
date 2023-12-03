# Ambio Adaptive management modelling and analysis
This repository includes the code to run GOTM modelling using a sensitivity approach to investigate how changes in flow will interact with air temeprature warming to impact lake water temperatures. 
The scripts to run the workflow are in the `Workflows` folder. The `GOTM` folder contains all the configuration files needed to run GOTM as well as the baseline driving data and the `R` folder contains additional functions to complete the workflow. 

The steps are 0. Install packages (run `install_packages.R`), 1. run GOTM scenarios; 2. run post-model analysis; and 3. Generate figures


## To rerun post-modelling analysis and reproduce figures
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

The configuration files for GOTM calibration using ParSAC are also provided but not included in the above workflow. This assumes that calibration/validation of GOTM parameter values has already occured. 
