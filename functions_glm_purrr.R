### The Main Program  with glmnet + purrr
# This is the linear regression training program.
# We retieve the observation data from Climate Database, CLDB,  and the model data from Model Output Statistics, MOS.
# This script initializes interfaces to various databases that can be used to retrieve the observation and model data
# and also to implement the MOS training/analysis.

source("toMOSutils_functions_glm.R")

GlmnR1_training_purrr <- function(station_id, station_type, obsdata, mosdata) { 

# All helper functions for data manipulation and linear regression. These functions are to be integrated to MosPointUtils



  # We split the mosdata and observation_data in to four seasons. The variable "data_seasons"
  # is  a list of mosdata and obsdata each of which contains the four seasaons data
  # data_seasons-> mosdata_seasons-> mos_winter, mos_spring, mos_summer, mos_autumn
  # data_seasons-> obsdata_seasons -> obs_winter, obs_spring, obs_summer, obs_autumn
  
  # Initialising the lists for station_id, analysis_time, forecast_period
  analysis_times <- all_producer_lists$ECMWF$analysis_times
  forecast_periods <- all_producer_lists$ECMWF$forecast_periods_hours
  
  max_variables <- 10
  fitting.method  <- "GlmnR1"
  
  data_seasons <- SplitSeasons(station_id, station_type, mosdata, obsdata) 
  
  
 
  #ss <- 1
  for (ss in 1:4)  {   # Seasons winter: 1, spring: 2, Summer: 3 and autumn: 4
   
    
    mos_season_data <- data_seasons[[1]][[ss]]
    obs_season_data <- data_seasons[[2]][[ss]]
    #Generate the MOS dataframe for a station, for two analysis_times (00 and 12 UTC) and for 64 forecast periods
    df_mos <- GenerateMOSDataFrame(station_id,mos_season_data)  
    
    for (aa in analysis_times) {  # Analysis times 00 UTC and 12 UTC 
    #aa <- "00"
      for (rr in 1:length(variable_list_predictands$variable_name)) { # The predictands 
        response <- variable_list_predictands$variable_name[rr] 
        
        coefficients.station.season.atime.fperiod <- as.data.frame(matrix( NA, nrow=length(all_producer_lists$ECMWF$forecast_periods_hours), 
                                                                           ncol=((length(variable_list_predictors$variable_name))+1)))
        colnames(coefficients.station.season.atime.fperiod ) <- c("Intercept", variable_list_predictors$variable_name)
        
        i <- 1   ## This is an index to the coefficients.station.season.atime.fperiod 
      
    
    # downloads data for a season
    #### FOR TESTING I USE THE FILE  cleaned_data_2794_winter.csv and change the forecast time to
    # POSIXct form by the command data$forecast_time <- as.POSIXct(data$forecast_time, tz = "UTC")
    
        
      df_mos_aa <- FetchMOSDataAA(df_mos, aa) 
      data <- FetchData_season_analysis_time(station_id, station_type, aa, df_mos_aa, obs_season_data, response)
    
    
    

    
   ###### purrr with glmnet
    
   #### data  have more NAs for some variables in different forecast periods. So we separate the data to forecast_period 00, forecast_period
  ### 01-144 and forecast_period 150-240
    
    data0 <- filter(data, as.integer(forecast_period) == 0)
    data0 <- CleanData(data0)
    data0$forecast_period <- as.integer(data0$forecast_period)
    
    data1 <- filter(data, as.integer(forecast_period) >= 03  & as.integer(forecast_period) <= 144)
    data1 <- CleanData(data1)
    data1$forecast_period <- as.integer(data1$forecast_period)
    
    data2 <- filter(data, as.integer(forecast_period) >= 150 & as.integer(forecast_period) <= 240)
    data2 <- CleanData(data2)
    data2$forecast_period <- as.integer(data2$forecast_period)
    
    
    
    glm0 <- (data0  %>%
               split(.$forecast_period) %>%
               map(FitWithGlmnR1purrr)
    )

    glm1 <- (data1  %>%
               split(.$forecast_period) %>%
               map(FitWithGlmnR1purrr )
    )
    
   
    
    
    glm2 <- (data2  %>%
               split(.$forecast_period) %>%
               map(FitWithGlmnR1purrr )
    )
    
    glm_model  <- do.call(c, list(glm0, glm1,glm2))   
    
   
    
   
    for (i in 1:length(glm_model)) { 
      fperiod <-  names(glm_model)[i]
      if (nchar(fperiod) == 1) {
        fperiod <- paste0("0", fperiod)
      }
      coefficients <- glm_model[[i]]$coefficients
      #names(coefficients)[1] <- "Intercept"
      stn_coeffs <-  coefficients
      coefficients.station.season.atime.fperiod[i,na.omit(match(names(stn_coeffs),names(coefficients.station.season.atime.fperiod)))] <- stn_coeffs
      i <- i + 1
    }
    coefficients.station.season.atime.fperiod[is.na(coefficients.station.season.atime.fperiod)] <- 0
    df <- t(coefficients.station.season.atime.fperiod)
    colnames(df) <- c(all_producer_lists$ECMWF$forecast_periods_hours)
    filename = paste0(output_dir,"station_",station_id,"_",aa,"_season",ss,"_",response,"_level0_",fitting.method,"_MOS_maxvars10_PURRR.csv")
    write.csv(df, file=filename)
    
      }
    }
    
  }
}