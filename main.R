### The Main Program 
# This is the linear regression training program.
# We retieve the observation data from Climate Database, CLDB,  and the model data from Model Output Statistics, MOS.
# This script initializes interfaces to various databases that can be used to retrieve the observation and model data
# and also to implement the MOS training/analysis.
# test line

# Removing old lists
rm(list=ls())

# Set the working directory
setwd("/data/daniel/statcal/R_projects/ld_playground")# Initializing variables and data structures

# Reading in the required packages, mapping lists and functions
source("load_libraries_tables_and_open_connections.R")

# Set the working directory
setwd("/data/daniel/statcal/R_projects/ld_playground/lr_model_training")# Initializing variables and data structures


start_time_fetch_data_lr <- proc.time()

# User-defined variables
# timestamps_series <- define_time_series(begin_date = "2017-09-14 00:00:00 GMT", end_date = "2017-10-20 21:00:00 GMT", interval_in_hours = 3)
timestamps_series <- define_time_series() # default time series from 2011 to 2018 (present)
modelobspairs_minimum_sample_size <- 100 # Arbitrary number here, could in principle also depend on the number of predictor variables
mos_label <- paste("MOS_ECMWF_250416")
predictor_set <- "only_bestvars2" #"allmodelvars_1prec_noBAD_RH2"
derived_variables <- c("T2^2","T2^(1/2)","RH_SURF","RH_SURF^2","DECLINATION","SOL_ANGLE")
station_list <- "mos_stations_homogeneous_Europe" 
station_numbers <- eval(subs(all_station_lists[[station_list]])) 
obs_interpolation_method <- "spline_interp"
max_interpolate_gap <- 6
verif_stationtype <- "normal" 
fitting.method  <- "GlmnR1"
variable_list_predictors_all <- variable_list_predictors <- choose_variables(c(predictor_set,derived_variables),"previ_ecmos_narrow_v","MOS")
variable_list_predictands_all <- variable_list_predictands <- choose_variables("estimated_variables","both","CLDB")


# Defining running indices from lists
station_numbers_indices <- seq_len(length(station_numbers))
variable_indices <- seq_len(length(variable_list_predictands[["variable_name"]]))
#station_list_retrieved <- station_numbers[c(station_number_index)]

# Initialising the lists for station_id, analysis_time, forecast_period
analysis_times <- all_producer_lists$ECMWF$analysis_times
forecast_periods <- all_producer_lists$ECMWF$forecast_periods_hours
#fcast_periods <- forecast_periods[1]
max_variables <- 10
station_variables <- 4 # station_id, season, analysis_time, and forecast_time

# All helper functions for data manipulation and linear regression. These functions are to be integrated to MosPointUtils



source("functions.R")
station_list_retrieved <- c(2974)
#for (station_id in station_list_retrieved) {}
for (station_id in station_list_retrieved) { 
  
  # Initializing the training matrix for a season
  coefficients.station.season.atime.fperiod <- as.data.frame(matrix( NA, nrow=length(all_producer_lists$ECMWF$forecast_periods_hours), ncol=((length(variable_list_predictors$variable_name) + station_variables)+1)))
  colnames(coefficients.station.season.atime.fperiod ) <- c("station_id", "season", "analysis_time", "forecast_period", "Intercept", variable_list_predictors$variable_name)
  
  ### Retrieving predictand data
  function_arguments <- list(variable_list_predictands,station_id,timestamps_series)
  predictand_data <- do.call(retrieve_data_all,function_arguments)
  ### RETRIEVING PREDICTOR DATA ###
  function_arguments <- list(variable_list_predictors,station_id,timestamps_series)
  predictor_data <- do.call(retrieve_data_all,function_arguments)
  
  obsdata <- predictand_data$CLDB$observation_data_v1   # observation data is from observations_data_v1 of CLDB 
  mosdata <- predictor_data$MOS$previ_ecmos_narrow_v    # MOS data 
  
  # We split the mosdata and observation_data in to four seasons. The variable "data_seasons"
  # is  a list of mosdata and obsdata each of which contains the four seasaons data
  # data_seasons-> mosdata_seasons-> mos_winter, mos_spring, mos_summer, mos_autumn
  # data_seasons-> obsdata_seasons -> obs_winter, obs_spring, obs_summer, obs_autumn
    
  data_seasons <- SplitSeasons(station_id,mosdata, obsdata) 
  response <- variable_list_predictands[[1]][[1]]     # TA
  
   start_time_lr4 <-proc.time()
   for (ss in 1:4)  {   # Seasons winter: 1, spring: 2, Summer: 3 and autumn: 4
      mos_season_data <- data_seasons[[1]][[ss]]
      obs_season_data <- data_seasons[[2]][[ss]]
      obs_mos_matrix <- IntegrateObsMosData(station_id,mos_season_data, obs_season_data, response)
    
      for (aa in analysis_times) {  # Analysis times 00 UTC and 12 UTC 
        #aa <- as.integer(aa)
        i <- 1   ## This is an index to the coefficients.station.season.atime.fperiod ####TOBE CHANGED
        for (ff in forecast_periods) { # There are 64 forecast_periods
          start_time_lr <- proc.time()
          #ff <- as.integer(ff)
          data <- FetchDataFP(station_id, aa, ff, obs_mos_matrix)
          #### TO BE THOUGHT ABOUT Here is a temporary way to remove ^ and / from variable names
          names(data)[names(data) == "T2^2"] <- "T2_2"
          names(data)[names(data) == "T2^(1/2)"] <- "T2_0.5"
          names(data)[names(data) == "RH_SURF^2"] <- "RH_SURF_2"
          names(coefficients.station.season.atime.fperiod)[names(coefficients.station.season.atime.fperiod) == "T2^2"] <- "T2_2"
          names(coefficients.station.season.atime.fperiod)[names(coefficients.station.season.atime.fperiod) == "T2^(1/2)"] <- "T2_0.5"
          names(coefficients.station.season.atime.fperiod)[names(coefficients.station.season.atime.fperiod) == "RH_SURF^2"] <- "RH_SURF_2"
       
          data <- CleanData(data)
          results <- Train.Model(data, fitting.method)
          stn.season.atime.fperiod <-  c(station_id = station_id,
                                      season = ss,
                                      analysis_time = aa,
                                      forecast_period = ff
         )
       
        stn_coeffs <- c(stn.season.atime.fperiod, results$coefficients)
        coefficients.station.season.atime.fperiod[i,na.omit(match(names(stn_coeffs),names(coefficients.station.season.atime.fperiod)))] <- stn_coeffs
        i <- i + 1
        }
        coefficients.station.season.atime.fperiod[is.na(coefficients.station.season.atime.fperiod)] <- 0
        ## In the file the coefficients are saved  the name "forecast_period" is replaced by ""
        ## We take the transpose of the coefficient matrix and write to a file using write.table
        names(coefficients.station.season.atime.fperiod)[names(coefficients.station.season.atime.fperiod) == "forecast_period"] <- ""
        filename = paste0("Station_",station_id,"_",aa,"_season",ss,"_",response,"_level0_",fitting.method,"_MOS_maxvars10.csv")
        write.table(t(coefficients.station.season.atime.fperiod[, 4:ncol(coefficients.station.season.atime.fperiod)]), file=filename, sep=",", col.names=FALSE)
      }
      elapsed_time_lr = proc.time() - start_time_lr
    }
    elapsed_time_lr4 = proc.time() - start_time_lr4
}
elapsed_time_main = proc.time() - start_time_fetch_data_lr 
  
  
  
  


