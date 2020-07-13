# This script retrieves foreign station data from CLDB and prints out max precipitation values in the woule observation time period
rm(list=ls())
# Reading in the required packages, mapping lists and functions
source("load_libraries_tables_and_open_connections.R")

# User-defined variables
timestamps_series <- define_time_series(begin_date=as.POSIXct("2011-12-01 00:00:00 GMT",tz="GMT"),end_date=with_tz(round.POSIXt(Sys.time(),"hours"),tz="GMT"),interval_in_hours=3,interval_in_seconds=NA,even_hours=TRUE) # define_time_series(begin_date=as.POSIXct("2011-12-01 00:00:00 GMT",tz="GMT"),end_date=with_tz(round.POSIXt(Sys.time()+864000,"hours"),tz="GMT"),interval_in_hours=3,interval_in_seconds=NA,even_hours=TRUE)
modelobspairs_minimum_sample_size <- 100 # Arbitrary number here, could in principle also depend on the number of predictor variables
date_string <- format(Sys.time(), "%d%m%y")
mos_label <- paste0("MOS_ECMWF_020320") #paste0("MOS_ECMWF_",date_string) #
predictor_set <- "only_bestvars3" #"only_bestvars2_no_climatology_ensmean" #"NA" #"allmodelvars_1prec_noBAD_RH2"
derived_variables <- NA # c("Z_ORO","Z_850")  #c("RH_SURF","Z_850","GH_850")  # NA # c("DECLINATION")
station_list <- "mos_stations_homogeneous_Europe" # Possible pre-defined station lists are those names in all_station_lists. If you want to use an arbitrary station list, assign the station numbers manually to variable station_numbers
station_numbers <- eval(subs(all_station_lists[[station_list]])) # c(1406,2978) # Retrievals are generated and data is returned based on station wmon-numbers. If using a station list outside mos station list, define the wmon-numbers here.
obs_interpolation_method <- "spline_interp" # options repeat_previous (na.locf),linear_interp (na.approx),spline_interp (na.spline),no_interp (leave NA values to timeseries as they are). Continuous observations are interpolated, those which not are sublists in all_variable_lists
max_interpolate_gap <- 6 # This indicates the maximum time in hours to which observation interpolation is applied
verif_stationtype <- "normal" # In verif db, several stationgroups exist. "normal" assumes stations (2700 <= wmon <= 3000) belonging to stationgroup=1, and all other to stationgroup=9 (other stationgroups outside stationgroup=3 only have a small number of stations to them). Road weather station support needs to be coded later (this needs a road weather station list), currently this can be done manually by putting the stationgroup of interest here manually (e.g. ==3)
output_dir <- paste0("/data/statcal/results/MOS_coefficients/in_progress/",mos_label,"/") # output_dir check is done in the beginning of the function MOS_training
max_variables <- 10
fitting_algorithm  <- "GlmnR1"
fitting_method <- "purrr" # only purrr method is maintained

timestamps_series <- define_time_series(begin_date=as.POSIXct("2011-12-01 00:00:00 GMT",tz="GMT"),end_date=with_tz(round.POSIXt(Sys.time(),"hours"),tz="GMT"),interval_in_hours=1,interval_in_seconds=NA,even_hours=TRUE) # define_time_series(begin_date=as.POSIXct("2011-12-01 00:00:00 GMT",tz="GMT"),end_date=with_tz(round.POSIXt(Sys.time()+864000,"hours"),tz="GMT"),interval_in_hours=3,interval_in_seconds=NA,even_hours=TRUE)
variable_list_retrieved <- rbind(choose_variables(c("TA"),"weather_data_qc","CLDB")) # rbind(choose_variables(c("PR_6H"),"weather_data_qc","CLDB"))


# station_numbers <- station_numbers[station_numbers >= 7000 & station_numbers <= 10000]
temp2 <- NA
station_numbers <- station_numbers[(station_numbers>60350 & station_numbers<60800)]
for (station_list_retrieved in station_numbers) {
  function_arguments <- list(variable_list_retrieved,station_list_retrieved,timestamps_series)
  retrieved_data <- do.call(retrieve_data_all,function_arguments)
  temp1 <- retrieved_data$CLDB$weather_data_qc
  if (!length(temp1)==FALSE) {
    if (dim(temp1)[1]>1000) {
      temp1 <- temp1[(temp1$parameter == "TA"),]
      print(paste0("max TA value in time series at station wmon",station_list_retrieved," is ",max(temp1$value,na.rm=TRUE)," Kelvins!"))
      temp2 <- c(temp2,max(temp1$value,na.rm=TRUE))
      plot(temp1$value)
    }
  }
    rm(temp1)
}
