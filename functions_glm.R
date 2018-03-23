

# This is the linear regression training program using glmnet
# The observation data is retrived from Climate Database, CLDB,  and the model data is retrieved from Model Output Statistics, MOS.
# This script initializes interfaces to various databases that can be used to retrieve the observation and model data
# and also to implement the MOS training/analysis.

 
GlmnR1_training <- function(station_id, obsdata, mosdata, max_variables, fitting.method, station_type) { 
  # MOS linear regression training program using glmnet with Lasso regularization
  # Args
  #   station_id: station_id
  #   mosdata: MOS data from MOS database
  #   obsdata: Observation data from CLDB databas 
  #   response: the predictand for linear regression
  # 
  # 
  #   data: data frame in which the last  column is the response
  #         variable and the rest are independent variables
  #   timevector: a POSIXct vector with UTC-based datetimes of the observations  
  
  # Initialising the lists for station_id, analysis_time, forecast_period
  analysis_times <- all_producer_lists$ECMWF$analysis_times
  forecast_periods <- all_producer_lists$ECMWF$forecast_periods_hours
  
  # These functions are to be placed in MOSutils
  source("toMOSutils_functions_glm.R")
  # As a first thing, possibly included predictors TAMIN12H/TAMAX12H are interpolated to previous 11 hours so that further interpolation is avoided
  obsdata <- InterpolateMinMaxValues(station_id, obsdata, station_type)
  # Divide data into seasons
  data_seasons <- SplitSeasons(station_id, mosdata, obsdata)
  
  for (ss in 1:4)  {   # Seasons winter: 1, spring: 2, Summer: 3 and autumn: 4
    mos_season_data <- data_seasons[[1]][[ss]]
    obs_season_data <- data_seasons[[2]][[ss]]
    
    #Generate the MOS dataframe for a station, for two analysis_times (00 and 12 UTC) and for 64 forecast periods
    df_mos <- GenerateMOSDataFrame(station_id,mos_season_data)
  
    for (aa in analysis_times) {  # Analysis times 00 UTC and 12 UTC 
      # The predictands in the variable_list_predictands are checked so that at least some data exists
      for (rr in 1:length(variable_list_predictands$variable_name)) {
        response <- variable_list_predictands$variable_name[rr]
        
        # Initializing the training matrix for a season, for an analysis
        coefficients.station.season.atime.fperiod <- as.data.frame(matrix( NA, nrow=length(all_producer_lists$ECMWF$forecast_periods_hours), 
                                                                        ncol=((length(variable_list_predictors$variable_name))+1)))
        rownames(coefficients.station.season.atime.fperiod) <-   forecast_periods
        colnames(coefficients.station.season.atime.fperiod ) <- c("Intercept", variable_list_predictors$variable_name)
  
        for (ff in forecast_periods) { # There are 64 forecast_periods
          df_mos_fp <- FetchMOSDataFP(df_mos, aa, ff) 
          data_response <- FetchData_season_analysis_time(station_id, aa, df_mos_fp, obs_season_data, response, station_type)
          data_fit <- data_response # data_response$data  # data for a particular response variable
          # response <- data_response$response
          data_fit <- CleanData(data_fit)  # Cleaning the NAs
          # Training the regression model, checking that sample size is large enough
          if (dim(data_fit)[1] > modelobspairs_minimum_sample_size) {
            results <- Train.Model(data_fit, fitting.method)
            stn_coeffs <- results$coefficients
            coefficients.station.season.atime.fperiod[match(ff,forecast_periods),na.omit(match(names(stn_coeffs),colnames(coefficients.station.season.atime.fperiod)))] <- stn_coeffs
          }
        }
        rm(ff)
        coefficients.station.season.atime.fperiod[is.na(coefficients.station.season.atime.fperiod)] <- 0
        df <- t(coefficients.station.season.atime.fperiod)
        # Only saving coefficients into a file if enough forecast_periods are present in the data.
        # Only single consecutive forecast periods are allowed missing, not two or more
        chunks <- rle(colSums(df==0)==dim(df)[1])
        if (length(which(chunks$length>1 & chunks$values==TRUE))==0) {
          filename = paste0(output_dir,"station_",station_id,"_",aa,"_season",ss,"_",response,"_level0_",fitting.method,"_MOS_maxvars",max_variables,".csv")
          write.csv(df, file=filename)
          rm(filename)
        }
      }
      rm(rr)
    }
    rm(aa)
  }
  rm(ss)
} 

  
  
  


