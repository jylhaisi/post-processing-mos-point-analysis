

# This is the linear regression training program using glmnet
# The observation data is retrived from Climate Database, CLDB,  and the model data is retrieved from Model Output Statistics, MOS.
# This script initializes interfaces to various databases that can be used to retrieve the observation and model data
# and also to implement the MOS training/analysis.

 
  GlmnR1_training <- function(station_id, obsdata, mosdata) { 
    # MOS linear regression training program using glmnet with Lasso regularization
    # Args
    #   station_id: station_id
    #   mosdata: MOS data from MOS database
    #   obsdata: Observation data from CLDB databas 
    #   response: the predicatand for linear regression
    # 
    # 
    #   data: data frame in which the last  column is the response
    #         variable and the rest are independent variables
    #   timevector: a POSIXct vector with UTC-based datetimes of the observations  
    
  # Initialising the lists for station_id, analysis_time, forecast_period
  analysis_times <- all_producer_lists$ECMWF$analysis_times
  forecast_periods <- all_producer_lists$ECMWF$forecast_periods_hours
  
  max_variables <- 10
  fitting.method  <- "GlmnR1"
  
  
  # These functions are to be placed in MOSutils
  
  source("toMOSutils_functions_glm.R")
  data_seasons <- SplitSeasons(station_id,mosdata, obsdata) 
  
  
  for (ss in 1:4)  {   # Seasons winter: 1, spring: 2, Summer: 3 and autumn: 4
      mos_season_data <- data_seasons[[1]][[ss]]
      obs_season_data <- data_seasons[[2]][[ss]]
      
      #Generate the MOS dataframe for a station, for two analysis_times (00 and 12 UTC) and for 64 forecast periods
      df_mos <- GenerateMOSDataFrame(station_id,mos_season_data)  
    
      for (aa in analysis_times) {  # Analysis times 00 UTC and 12 UTC 
        
        for (rr in 1:length(variable_list_predictands$variable_name)) { # The predictands 
          response <- variable_list_predictands$variable_name[rr]
          #for (response in variable_list_predictands$variable_name) { # The predictands 
          
          # Initializing the training matrix for a season, for an analysis
          coefficients.station.season.atime.fperiod <- as.data.frame(matrix( NA, nrow=length(all_producer_lists$ECMWF$forecast_periods_hours), 
                                                                          ncol=((length(variable_list_predictors$variable_name))+1)))
          colnames(coefficients.station.season.atime.fperiod ) <- c("Intercept", variable_list_predictors$variable_name)
          i <- 1   ## This is an index to the coefficients.station.season.atime.fperiod 
        
          for (ff in forecast_periods) { # There are 64 forecast_periods
            df_mos_fp <- FetchMOSDataFP(station_id, aa, ff, df_mos) 
            data_response <- FetchData_season_analysis_time(station_id, aa, df_mos_fp, obs_season_data, response)
            data <- data_response$data  # data for a particular response variable
            response <- data_response$response
            data <- CleanData(data)  # Cleaning the NAs
            
            # Training the linear regression model with glmnet with Lasso regularization
            results <- Train.Model(data, fitting.method)
            stn_coeffs <- results$coefficients
            coefficients.station.season.atime.fperiod[i,na.omit(match(names(stn_coeffs),names(coefficients.station.season.atime.fperiod)))] <- stn_coeffs
            i <- i + 1
          }
        
          coefficients.station.season.atime.fperiod[is.na(coefficients.station.season.atime.fperiod)] <- 0
          df <- t(coefficients.station.season.atime.fperiod)
          colnames(df) <- c(all_producer_lists$ECMWF$forecast_periods_hours)
          filename = paste0(output_dir,"station_",station_id,"_",aa,"_season",ss,"_",response,"_level0_",fitting.method,"_MOS_maxvars10.csv")
          write.csv(df, file=filename)
          rm(filename)
        }
        }
        
      }
    } 

  
  
  


