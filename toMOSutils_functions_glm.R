library(dplyr)
library(lubridate)
library(lattice)
library(ggplot2)
library(reshape2)
library(purrr)



# packages for different model selection algorithms

library(glmnet)  # glmnet


# Data retrieval and splitting
# We have to see how to use Jussi's function retieve data here retrieve  <- function(variable_list,station_list_retrieved,timestamps_series)
# function(variable_list,station_list_retrieved,timestamps_series)
# Above function brings both the observation and MOS data as lists 
# and we make the dataframes mosdata and observation_data

# Retrieve data from a .csv file
RetrieveDatafromFile <- function(mos_file, obs_file) {
  mosdata <- read.csv(mos_file)
  obsdata <- read.csv(mos_file)
  
  # This conversion is needed as the data is read from the .csv file where the forecast_time is saved as Factor 
  mosdata$forecast_time <- as.POSIXct(mosdata$forecast_time, tz = "UTC")
  obsdata$obstime <-  as.POSIXct(obsdata$obstime, tz = "UTC")
  result <- list("mosdata" = mosdata, "obsdata" = obsdata)
  return(result)
}
 



# We have to see how to use Jussi's function retieve data here retrieve  <- function(variable_list,station_list_retrieved,timestamps_series)
# function(variable_list,station_list_retrieved,timestamps_series)
# Above function brings both the observation and MOS data as lists 
# and we make the dataframes mosdata and observation_data

# We split the data based on seasons or years

SplitYears <- function(station_id,  mos_df, obs_df) {
  # 
  #  # Args:
  #   station_id: station_id
  #   mos_df: MOS data retrieved from postgre database 
  #   obs_df: Observation data from the CLDB
  #   response: the data of the observation variable
  # 
  # Returns:
  #   data.split: a list with mosdata and observation data split into years
  #               this list contains null data for mos and obs data for seasons


}



SplitSeasons <- function(station_id, mos_df, obs_df) { # function(station_id, station_type, mos_df, obs_df) {
 # 
  #  # Args:
  #   station_id: station_id
  #   season: season (winter-autumn 1-4)
  #   mos_df: MOS data retrieved from postgre database 
  #   obs_df: Observation data from the CLDB
  #   response: the data of the observation variable
  # 
  # Returns:
  #   data.split: a list with mosdata and observation data split into 4 seasons
  #   		  this list contains null data for yearly mos and obs data 
  
      
	    mos_winter <- (mos_df %>%
                 dplyr::filter(month(forecast_time) >= 11 | month(forecast_time) <= 3) %>%
                 dplyr::select(station_id, analysis_time, forecast_time, forecast_period, param_id, level_value, value))
	    
      obs_winter <- (obs_df %>%
                 dplyr::filter(month(obstime) >= 11 | month(obstime) <= 3) %>%
                   dplyr::select(everything())) 
               #  dplyr::select(station_id, obstime, measurand_id, value))

	    mos_spring <- (mos_df %>%
                 dplyr::filter(month(forecast_time) >= 2 & month(forecast_time) <= 6) %>%
                 dplyr::select(station_id, analysis_time, forecast_time, forecast_period, param_id, level_value, value))
	    obs_spring <- (obs_df  %>%
                 dplyr::filter(month(obstime) >= 2 & month(obstime) <= 6) %>%
                   dplyr::select(everything()))  
                 #dplyr::select(station_id, obstime, measurand_id, value))

	    mos_summer <- (mos_df %>%
                 dplyr::filter(month(forecast_time) >= 5 & month(forecast_time) <= 9) %>%
                 dplyr::select(station_id, analysis_time, forecast_time, forecast_period, param_id, level_value, value))
	    obs_summer <- (obs_df  %>%
                 dplyr::filter(month(obstime) >= 5 & month(obstime) <= 9) %>%
                  dplyr::select(everything())) 
                  #dplyr::select(station_id, obstime, measurand_id, value))

	    mos_autumn <- (mos_df %>%
                 dplyr::filter(month(forecast_time) >= 8 & month(forecast_time) <= 12) %>%
                 dplyr::select(station_id, analysis_time, forecast_time, forecast_period, param_id, level_value, value))
	    obs_autumn <- (obs_df  %>%
                 dplyr::filter(month(obstime) >= 8  & month(obstime) <= 12) %>%
                   dplyr::select(everything())) 
	    #dplyr::select(station_id, obstime,  measurand_id, value))

      mosdata_seasons  <- list("mos_winter" = mos_winter, "mos_spring" = mos_spring, "mos_summer" = mos_summer, "mos_autumn"= mos_autumn)

      obsdata_seasons <- list("obs_winter" = obs_winter, "obs_spring" = obs_spring, "obs_summer" = obs_summer, "obs_autumn" = obs_autumn)
      data_seasons <- list("mosdata_seasons" = mosdata_seasons, "obsdata_seasons" = obsdata_seasons)

 return(data_seasons)
}
 

SplitDataEvenly <- function(timevector, folds = 4) {
  # Splits data evenly into `folds` groups, default is four groups.
  #
  # Args:
  #   timevector: a vector
  #   folds: the number of groups
  #
  # Returns:
  #   Indices that define the first element in a new group. The last index is
  #   non-existent which is to be noted when using indices in a loop.
  
  no.of.points <- length(timevector)
  min.points <- no.of.points %/% folds
  add.points <- no.of.points %% folds
  point.vector <- rep(min.points, times = folds)
  if (add.points > 0) {
    point.vector[1:add.points] <- point.vector[1:add.points] + 1
  }
  indices <- rep(NA, times = (folds+1))
  indices[1] <- 1
  for (ii in 2:(folds+1)) {
    indices[ii] <- indices[ii-1] + point.vector[ii-1]
  }
  return(indices)
}


GenerateMOSDataFrame <- function(station_id,mos_season_data) { 
  # Generate MOS dataframe from the mosdata retrived for a particular season and for a particular analysis_time
  # This dataframe contains the MOS data for 00-64 forecast periods
  # station_id: station_id
  # atime: analysis_time can have values "00" and "12" UTC
  # mos_season_data: MOS data for a particular season
  # 
  # Returns:
  #   data: MOS data with independent variables of the linear regression
  #   timevector: a POSIXct vector with UTC-based datetimes of the observations 
  
  # param_id, level_id describes a particular parameter which is an independent variable of the MOS data
  param_list <- unique(mos_season_data[["param_id"]])
  level_list <- unique(mos_season_data[["level_value"]])
  analysis_times <- unique(mos_season_data[["analysis_time"]])
  forecast_periods <- unique(mos_season_data[["forecast_period"]])
  
  mos_vars <- all_variable_lists[["MOS"]]                     # Getting the mos variable names from all_variable_lists
  mos_D_vars <- all_variable_lists[["derived_variables_all"]] # Getting the derived variable names from all_variable_lists

  # Finding the parameter name and its values are added to the MOS dataframe
  # loop variable is zero for the first time when the first parameter value is added to the dataframe 
  loop1 <- 0  
  for (i in 1:length(param_list)) {
      for (j in 1:length(level_list)) {
        if (length(param_name <- filter(mos_vars, mos_vars$param_id == as.integer(param_list[i]) & mos_vars$level_value == level_list[j])[[1]]) == 0) { 
            if (length(param_name <- row.names(mos_D_vars)[which(suppressWarnings(mos_D_vars$derived_param_id) == param_list[i] & suppressWarnings(as.integer(mos_D_vars$MOS_previ_ecmos_narrow_v_level)) == level_list[j])]) == 0) {
              next
            }
        }
  
        df <- (mos_season_data %>%
            dplyr::filter(param_id == param_list[i], level_value == level_list[j]) %>%
            dplyr::select(station_id, analysis_time,  forecast_time, forecast_period, value))
    
        if (nrow(df) != 0) {
          names(df)[ncol(df)]<- eval(param_name)
          if (loop1 == 0) { 
            df_mos <- df
            loop1 <- 1
          } else {
          df_mos<- merge(df_mos, df,by.x=c("station_id", "analysis_time", "forecast_time", "forecast_period"), by.y=c("station_id","analysis_time","forecast_time", "forecast_period"), all.x = TRUE)
          }
        }
      }
  }
    
  
# The values of the 8-digit derived_param_id variables SOL_ANGLE and DECLINATION are calculated and added to the MOS dataframe

  if ("SOL_ANGLE" %in% variable_list_predictors$variable_name) { 
      declination <- -asin(0.39779*cos(0.98565/360*2*pi*((daydoy(df_mos$forecast_time)+
                          (as.numeric(format(df_mos$forecast_time,"%H"))/24)-1)+10)+1.914/360*2*pi*sin(0.98565/360*2*pi*((daydoy(df_mos$forecast_time)+
                          (as.numeric(format(df_mos$forecast_time,"%H"))/24)-1)-2))))*360/2/pi
  
      hour_angle <- ((as.numeric(format(df_mos$forecast_time,"%H"))+12)%%24)/24*2*pi
      # Edellinen on UTC-aikaa. Tämän vuoksi pitää lisätä tuntikulmalaskuun aseman longitudi (tarkemmin ottaen longitudin ja nollameridiaanin välinen kulma := longitudi)
      #tuntikulma <- (tuntikulma + station_idt_conversion$lon[which(station_idt_conversion$wmon==previ_ecmos_v_station_id[i])]/360*2*pi) %% (2*pi)
      hour_angle <- (hour_angle + station_idt_conversion$lon[which(station_idt_conversion$wmon==2974)]/360*2*pi) %% (2*pi)
      df_mos <- df_mos %>% mutate(SOL_ANGLE =  ((cos(hour_angle)*cos(declination/360*2*pi)*cos(2*pi/360*station_idt_conversion$lat[which(station_idt_conversion$wmon==2974)])) + 
                                              (sin(declination/360*2*pi)*sin(2*pi/360*station_idt_conversion$lat[which(station_idt_conversion$wmon==2974)]))))
  
      # Changing the negative values of SOL_ANGLE to zero
      df_mos$SOL_ANGLE[df_mos$SOL_ANGLE < 0] <- 0
  
  }

  if ("DECLINATION" %in% variable_list_predictors$variable_name) { 
      df_mos <- df_mos %>% mutate(DECLINATION = (-asin(0.39779*cos(0.98565/360*2*pi*((daydoy(df_mos$forecast_time-(3600*24*32))+
                                                (as.numeric(format(df_mos$forecast_time,"%H"))/24)-1)+10)+1.914/360*2*pi*sin(0.98565/360*2*pi*((daydoy(df_mos$forecast_time-(3600*24*32))+
                                                (as.numeric(format(df_mos$forecast_time,"%H"))/24)-1)-2))))*360/2/pi))}

  return(df_mos)
}

FetchMOSDataFP <- function(df_mos, atime, fperiod) {

  data <- (df_mos %>%
           dplyr::filter( analysis_time == atime, forecast_period == fperiod))
            
  return(data)         

}

FetchMOSDataAA <- function(df_mos, atime) {
  
  data <- (df_mos %>%
             dplyr::filter( analysis_time == atime))
  
  return(data)         
  
}

InterpolateMinMaxValues <- function(station_id, obsdata, station_type) {
  if (station_type == 1) {
    param_list <- unique(obsdata[["parameter"]]) 
    # param_ids <- rownames(all_variable_lists[["estimated_parameters"]])[match(param_list,unlist(all_variable_lists[["estimated_parameters"]]["CLDB_weather_data_qc"]))]
    for (param_id_extr in c("TAMAX12H","TAMIN12H")) {
      if (param_id_extr %in% param_list) {
        obsdata <- ReturnInterpolatedMinMaxValues(obsdata,"parameter",param_id_extr)
      }
    }
    rm(param_id_extr)
  } else {
    param_list <- unique(obsdata[["measurand_id"]])
    # param_ids <- rownames(all_variable_lists[["estimated_parameters"]])[match(param_list,unlist(all_variable_lists[["estimated_parameters"]]["CLDB_observation_data_v1"]))]
    for (param_id_extr in c("21","22")) {
      if (param_id_extr %in% param_list) {
        obsdata <- ReturnInterpolatedMinMaxValues(station_id, obsdata,"measurand_id",param_id_extr)
      }
    }
    rm(param_id_extr)
  }
  invisible(obsdata)
}

ReturnInterpolatedMinMaxValues <- function(station_id, obsdata, column_name, param_id_extr) {
  com_string <- paste0("df_obs <- subset(obsdata,",column_name,"==param_id_extr)")
  eval(parse(text=com_string))
  rm(com_string)
  start <- df_obs$obstime[1]
  end <- df_obs$obstime[dim(df_obs)[1]]
  obs_interp <-  as.data.frame(cbind(seq(start, end, by="1 hour"),NA))
  obs_interp[,1] <- seq(start, end, by="1 hour")
  rm(start)
  rm(end)
  colnames(obs_interp) <- c("obstime","value")
  
  for (rownumber in 1:dim(obs_interp)[1]) {
    differences_in_hours <- difftime(obs_interp$obstime[rownumber],df_obs$obstime,units="hours") #(obs_interp$obstime[rownumber] - df_obs$obstime)
    assigned_value <- df_obs$value[head(which((differences_in_hours <= 0) & (differences_in_hours > -12)),1)]
    if (!length(assigned_value)==FALSE) {
      obs_interp$value[rownumber] <- assigned_value
    }
    rm(assigned_value)
    rm(differences_in_hours)
  }
  rm(rownumber)
  df_obs <- data.frame(station_id,obs_interp[["obstime"]],param_id_extr,obs_interp[["value"]])
  colnames(df_obs) <- c("station_id","obstime",column_name,"value")
  com_string <- paste0("obsdata <- subset(obsdata,",column_name,"!=param_id_extr)")
  eval(parse(text=com_string))
  rm(com_string)
  obsdata <- rbind(obsdata,df_obs)
  return(obsdata)
}

FetchData_season_analysis_time <- function(station_id, atime, df_mos, obs_season_data, response, station_type) {
  if (station_type == 1) {
    param_list <- unique(obs_season_data[["parameter"]])
  } else {
    param_list <- unique(obs_season_data[["measurand_id"]])
  }
  obs_vars <- all_variable_lists[["estimated_parameters"]]
  for (pp in 1:length(param_list)) {
    if (rownames(obs_vars)[pp] != response) {
      next;
    } else { 
      if (station_type == 1) { 
        #param_name = obs_vars$CLDB_weather_data_qc[pp]
        param_name <- response
      } else { 
        #param_name <- variable_list_predictands$variable_name[pp]
        param_name <- response
        param_id  <- obs_vars$CLDB_observation_data_v1[pp]
      }
      break;
    }
  }
  rm(pp)
  if (station_type == 1) {
      df_obs <- dplyr::filter(obs_season_data, parameter == param_name)
      df_obs <-  dplyr::select(df_obs, -c(parameter))
  } else { 
      df_obs <- dplyr::filter(obs_season_data, measurand_id == param_id)
      df_obs <-  dplyr::select(df_obs, -c(measurand_id))
  }
  colnames(df_obs)[ncol(df_obs)] <- param_name
  df_mos_aa <- (df_mos %>% dplyr::filter(analysis_time == atime))
  df_mos_obs<- merge(df_mos_aa, df_obs,by.x=c("station_id", "forecast_time"), by.y=c("station_id","obstime"), all.x = TRUE)
  # results <- list("data" = df_mos_obs, "response" = param_name)
  return(df_mos_obs)
}


  
FetchData <- function(station_id, atime, fperiod, mos_season_data, obs_season_data, response) {
  # 
  # Combine mosdata and observation data for a particular season and for a particular forecast period
  # into a dataframe for applying linear regression
  #   station_id: station_id
  #   forecast.period: (1-65)
  #   mos_season_data: MOS data for a particular season
  #   obs_season_data: Observation data for a particular season
  #   response: the predicatand for linear regression
  # 
  # Returns:
  #   data: data frame in which the last  column is the response
  #         variable and the rest are independent variables
  #   timevector: a POSIXct vector with UTC-based datetimes of the observations 
 
 
  
  
  param_list <- unique(mos_season_data[["param_id"]])

  level_list <- unique(mos_season_data[["level_value"]])

  mos_vars <- all_variable_lists[["MOS"]]
  mos_D_vars <- all_variable_lists[["derived_variables_all"]]
 
  # loop1 variable is zero for the first time when the first parameter value is added to the dataframe 
  loop1 <- 0  

  for (i in 1:length(param_list)) {
      	 for (j in 1:length(level_list)) {
    	     if ( length(param_name <- filter(mos_vars, mos_vars$param_id == param_list[i] & mos_vars$level_value == level_list[j])[[1]]) == 0) { 
             	if (length(param_name <- row.names(mos_D_vars)[which(suppressWarnings(as.integer(mos_D_vars$derived_param_id)) == param_list[i] & suppressWarnings(as.integer(mos_D_vars$MOS_previ_ecmos_narrow_v_level)) == level_list[j])]) == 0) {
        	      next
             	}
    	     }
 
	        df <- (mos_season_data %>%
                dplyr::filter( analysis_time == atime, forecast_period == fperiod, param_id == param_list[i], level_value == level_list[j]) %>%
             	  dplyr::select(station_id, analysis_time,  forecast_time, forecast_period, value))
    
	        if (nrow(df) != 0) {
               names(df)[ncol(df)]<- eval(param_name)
               if (loop1 == 0){ 
               	  df_mos <- df
          	      loop1 <- 1
               } else {
               	  df_mos<- merge(df_mos, df,by.x=c("station_id", "analysis_time", "forecast_time", "forecast_period"), by.y=c("station_id","analysis_time","forecast_time", "forecast_period"), all.x = TRUE)
               }
	        }
      	 }
  }
  
  
  if ("SOL_ANGLE" %in% variable_list_predictors$variable_name) { 
        declination <- -asin(0.39779*cos(0.98565/360*2*pi*((daydoy(df_mos$forecast_time)+(as.numeric(format(df_mos$forecast_time,"%H"))/24)-1)+10)+1.914/360*2*pi*sin(0.98565/360*2*pi*((daydoy(df_mos$forecast_time)+
                        (as.numeric(format(df_mos$forecast_time,"%H"))/24)-1)-2))))*360/2/pi
        
        hour_angle <- ((as.numeric(format(df_mos$forecast_time,"%H"))+12)%%24)/24*2*pi
        # Edellinen on UTC-aikaa. Tämän vuoksi pitää lisätä tuntikulmalaskuun aseman longitudi (tarkemmin ottaen longitudin ja nollameridiaanin välinen kulma := longitudi)
        #tuntikulma <- (tuntikulma + station_idt_conversion$lon[which(station_idt_conversion$wmon==previ_ecmos_v_station_id[i])]/360*2*pi) %% (2*pi)
        hour_angle <- (hour_angle + station_idt_conversion$lon[which(station_idt_conversion$wmon==2974)]/360*2*pi) %% (2*pi)
        df_mos <- df_mos %>% mutate(SOL_ANGLE =  ((cos(hour_angle)*cos(declination/360*2*pi)*cos(2*pi/360*station_idt_conversion$lat[which(station_idt_conversion$wmon==2974)])) + 
                                                    (sin(declination/360*2*pi)*sin(2*pi/360*station_idt_conversion$lat[which(station_idt_conversion$wmon==2974)]))))
        
        #df_mos <- df_mos %>% mutate(SOL_ANGLE =  ((cos(tuntikulma)*cos(deklinaatio/360*2*pi)*cos(2*pi/360*station_idt_conversion$lat[which(station_idt_conversion$wmon==previ_ecmos_v_station_id[i])])) + 
                                                    #(sin(deklinaatio/360*2*pi)*sin(2*pi/360*station_idt_conversion$lat[which(station_idt_conversion$wmon==previ_ecmos_v_station_id[i])]))))
        # # Tämä tulee suoraan hienostuneemmasta funktiosta (samasta mitä NOAA käyttää), mutta tätä ei käytetä
        # havainnot_ja_mallidata[,dim(havainnot_ja_mallidata)[2]] <- 90-sunpos(sunvector(JD(havainnot_ja_mallidata$kohdehetki),station_idt_conversion$lat[which(station_idt_conversion$wmon==previ_ecmos_v_station_id[i])],station_idt_conversion$lon[which(station_idt_conversion$wmon==previ_ecmos_v_station_id[i])],0))[,2]
        # Muutetaan negatiiviset korkeuskulman arvot nollaksi
        df_mos$SOL_ANGLE[df_mos$SOL_ANGLE < 0] <- 0
        
     }
        
  # if ("DECLINATION" %in% variable_list_predictors$variable_name) { 
  #   df_mos <- df_mos %>% mutate(DECLINATION = (-asin(0.39779*cos(0.98565/360*2*pi*((daydoy(df_mos$forecast_time-(3600*24*32))+
  #                                                                                     (as.numeric(format(df_mos$forecast_time,"%H"))/24)-1)+10)+1.914/360*2*pi*sin(0.98565/360*2*pi*((daydoy(df_mos$forecast_time-(3600*24*32))+
  #                                                                                                                                                                                       (as.numeric(format(df_mos$forecast_time,"%H"))/24)-1)-2))))*360/2/pi))
  #   
  # }
    
   

  colnames(obs_season_data)[dim(obs_season_data)[2]] <- response
  obs_season_data_TA <-  dplyr::select(obs_season_data,station_id, obstime, TA) 
  df_mos_obs<- merge(df_mos, obs_season_data_TA,by.x=c("station_id", "forecast_time"), by.y=c("station_id","obstime"), all.x = TRUE)
  
return(df_mos_obs)
}
  



CleanData <- function(data) {
  # Cleans a data  from bad observations and variables and  NAs 
  # Args:
  #   data: data retrived from the database
  #  
  #
  # Returns:
  #   A cleaned data object of the same form as the input argument
  
  
  # Split the dataframe data  to two  (1) station_info, seasons, analysis_time, forecast_peroiod,  and timestamps 
  # and (2) station_data which consists of observations and ECMWF model data 
  # change the variable names in Finnish to English 
  
  #colnames(data)[c(1:6)] <- c("station", "analysis_time", "forecast_period", "timestamps", "T2Obs")
  
  station_info <- data[,c(1:4)]
  station_data <- data[,5:(ncol(data))]
  station_data <- station_data[,c(ncol(station_data),1:(ncol(station_data)-1))]
  
  no.of.obs <-  nrow(station_data)
  
  # first we will delete faulty variables with are NA most of the time
  
  # calculating the number of NAs in variables
  # the variables are removed only if all the values are NA
  no.of.na <- apply(X = station_data[,], MARGIN = 2, FUN = function(x) sum(is.na(x)))
  
  # na.tolerance is taken numer of observations
  #na.tolerance <- no.of.obs/ 3
  na.tolerance <- no.of.obs
  # taking only the variables that have no.of.na < na.tolerance
  good.variables <- which(no.of.na < na.tolerance)
  
  # removing the variables which that have na.tolerance > no.of.points/4
  # Two variables "FG10_3" (10 meter wind gust in the last 3 hours) and 
  # "MX2T3" (Maximum temperature at 2 meter  in the last 3 hours) are removed
  station_data <- station_data[, good.variables]
  
  # searching the complete cases and keeping only them
  # removing all incomplete observations
  complete.rows <- complete.cases(station_data)
  station_data <- station_data[complete.rows,]
  
  # removing constant variables
  #station_data <- station_data[,apply(X= station_data, MARGIN = 2, FUN = function(v) var(v) != 0)]
  
  # retaining only the corresponding complete.rows of station_data in station_info
  station_info <- station_info[complete.rows,]
  
  # returning the cleaned data by combining the new station_info dataframe and the new station_data dataframe
  data <- cbind.data.frame(station_info, station_data)
  
  return(data)
}

  
FitWithLM <- function(training.set) {
  # Estimates a full linear model and predicts with the model.
  #
  # Args:
  #   training.set:
  #
  # Returns:
  #   A list of coefficients.
  
  response <- colnames(training.set)[1]
  formula <- paste(response, " ~ .", sep = "")
  lm.model <- lm(formula = formula, data = training.set)
  names(lm.model$coefficients)[1] <- "Intercept"
  
  results <- list("coefficients" = lm.model$coefficients)
  return(results)
}


FitWithStep <- function(training.set,
                        object.formula, upper.formula, lower.formula,
                        direction, steps = 1000) {
  
  
  # Chooses a linear model by a stepwise algorithm and predicts a fit on an independant test data
  #
  # Args:
  #   training.set: a data matrix
  #   object.formula: a formula viable for a linear model
  #   upper.formula: a formula viable for a linear model
  #   lower.formula: a formula viable for a linear model
  #   direction: both/forward/backward
  #   steps: the number of steps the algorithm is allowed to take
  # 
  # Returns:
  #   A list of coefficients, residuals and fitted values.
  
  

  # generating the linear models
  object.lm <- lm(object.formula, data = training.set)
  upper.lm <- lm(upper.formula, data = training.set)
  lower.lm <- lm(lower.formula, data = training.set)
  
  # choosing the best model
  step.model <- step(object = object.lm,
                     scope = list(upper = upper.lm, lower = lower.lm),
                     direction = direction, trace = 0 , steps = steps)
  
  names(step.model$coefficients)[1] <- "Intercept"
  results <- list("coefficients" = step.model$coefficients)
  
  return(results)
}


FitWithRegsubsets <- function(training.set,
                              x, force.in, method) {
  # The regsubsets() function (part of the leaps library) performs best sub- set selection by identifying the best model that contains 
  # a given number of predictors, where best is quantified using RSS.
  #
  # Args:
  #   training.set:
  #   test.set:
  #   x: a formula
  #   force.in: the index of the variable that is forced in the model
  #   method: forward/backward/seqrep
  #
  # Returns:
  #   A list of coefficients, residuals and fitted values.
  
  # estimating the best models
  regsubsets.model <- regsubsets(x, data = training.set, nbest = 1, nvmax = max_variables,
                                 force.in = force.in, method = method)
  
  # the estimated coefficients of a model
  # a coefficient is zero if the variable is not in the model
  id <- dim(summary(regsubsets.model)$which)[1]
  coefficients <- coef(regsubsets.model, id)
 
  names(coefficients)[1] <- "Intercept"
  results <- list("coefficients" = coefficients)
  return(results)
}


FitWithGlmnR1 <- function(training.set) {
  # The Lasso regression with 11 predictors and lambda min
  #
  # Args:
  #   training.set:
  # glmnet takes the model data and observations as arguements
  #
  # Returns:
  #   A list of coefficients
  training.matrix <- as.matrix(training.set)
  training.x <- training.matrix[,-1]
  training.y <- training.matrix[,1]
  glmnet.model <- suppressWarnings(glmnet(training.x, training.y, family = "gaussian", alpha = 1, standardize = TRUE, pmax = max_variables+1))
  filter.for.folds <- IndexVectorToFilter(SplitDataEvenly(training.y))
  cv.glmnet.model <- suppressWarnings(cv.glmnet(training.x, training.y, alpha = 1, foldid = filter.for.folds, pmax =max_variables+1))
  best.lambda <- cv.glmnet.model$lambda.min
  # choosing the best coefficients
  all.coefficients <- coef(cv.glmnet.model, s = best.lambda)
  all.coef.names <- rownames(all.coefficients)
  nonzero.indices <- which(all.coefficients != 0)
  coefficients <- all.coefficients[nonzero.indices]
  names(coefficients) <- all.coef.names[nonzero.indices] 
  
  names(coefficients)[1] <- "Intercept"
  coefficients[is.na(coefficients)] <- round(0,digits=0)
  
  results <- list("coefficients" = coefficients)
  return(results)
}


FitWithGlmnR1purrr <- function(training.set) {
  # The Lasso regression with 11 predictors and lambda lse
  #
  # Args:
  #   training.set:
  # glmnet takes the model data and observations as arguements
  #
  # Returns:
  #   A list of coefficients
  max_variables = 10;
  training.matrix <- as.matrix(training.set[5:ncol(training.set)])
  glmnet.model <- suppressWarnings(glmnet(training.matrix[,-1], training.matrix[,1], family = "gaussian", alpha = 1, standardize = TRUE, pmax = max_variables+1))
  filter.for.folds <- IndexVectorToFilter(SplitDataEvenly(training.matrix[,1]))
  cv.glmnet.model <- suppressWarnings(cv.glmnet(training.matrix[,-1],training.matrix[,1], alpha = 1, foldid = filter.for.folds, pmax =max_variables+1))
  best.lambda <- cv.glmnet.model$lambda.min
  # choosing the best coefficients
  all.coefficients <- coef(cv.glmnet.model, s = best.lambda)
  all.coef.names <- rownames(all.coefficients)
  nonzero.indices <- which(all.coefficients != 0)
  coefficients <- all.coefficients[nonzero.indices]
  names(coefficients) <- all.coef.names[nonzero.indices] 
  
  names(coefficients)[1] <- "Intercept"
  results <- list("coefficients" = coefficients)
  return(results)
}

  
FitWithGlmnM1<- function(training.set) {
  # The Lasso regression which takes 11 predictors and lambda lse
  #
  # Args:
  #   training.set:
  # glmnet takes the model data and observations as arguements
  #
  # Returns:
  #   A list of coefficients
  training.matrix <- as.matrix(training.set)
  glmnet.model <- suppressWarnings(glmnet(training.matrix[,-1], training.matrix[,1], family = "gaussian", alpha = 1, standardize = TRUE, pmax = max_variables+1))
  filter.for.folds <- IndexVectorToFilter(SplitDataEvenly(training.matrix[,1]))
  cv.glmnet.model <- suppressWarnings(cv.glmnet(training.matrix[,-1],training.matrix[,1], alpha = 1, foldid = filter.for.folds, pmax =max_variables+1))
  best.lambda <- cv.glmnet.model$lambda.1se
  # choosing the best coefficients
  all.coefficients <- coef(cv.glmnet.model, s = best.lambda)
  all.coef.names <- rownames(all.coefficients)
  nonzero.indices <- which(all.coefficients != 0)
  coefficients <- all.coefficients[nonzero.indices]
  names(coefficients) <- all.coef.names[nonzero.indices]
  names(coefficients)[1] <- "Intercept"
  results <- list("coefficients" = coefficients)
  return(results)
}


FitWithPCR <- function(training.set,
                       formula, ncomp = (length(training.set)-1), scale = TRUE,
                       regulation = "none") {
  # Performs priciple component regression and predicts with the model
  #
  # Args:
  #   training.set:
  #   test.set:
  #   formula:
  #   ncomp: the number of principal components
  #   scale: TRUE/FALSE
  #   regulation: none/eigenvalues/variance
  #
  # Returns:
  #  A list of coefficients, residuals and fitted values
  
  pca <- prcomp(~., data = training.set[,-1],
                center = TRUE, scale. = TRUE, tol = NULL)
  
  variables <- switch(regulation,
                      eigenvalues = RegulatePCRUsingEigenvalues(pca),
                      variance = RegulatePCRUsingVariance(pca),
                      none = c("."))
  no.of.variables <- length(variables)
  if(variables[1] == ".") {
    no.of.variables <- ncomp
  }
  
  pcr.formula <- as.formula(paste(colnames(training.set)[1], " ~ ", 
                                  paste(variables, collapse = " + "), 
                                  sep = ""))
  
  pcr.model <- pcr(pcr.formula,  ncomp = no.of.variables, data = training.set,
                   scale = TRUE, validation = "none")
  
  
  coefficients.array <- coef(pcr.model, ncomp = no.of.variables,
                             intercept = TRUE)
  coefficients = as.numeric(coefficients.array)
  names(coefficients) <- rownames(coefficients.array)
  
  
  results <- list("coefficients" = coefficients)
  return(results)
}


RegulatePCRUsingEigenvalues <- function(prcomp.object) {
  # Removes the least important variables based on the eigenvalues
  # in principal component analysis
  #
  # Args:
  # prcomp.object: an object created by the function prcomp()
  #
  # Returns:
  # A vector of the names of the variables that are left in the model
  
  eigenvalues <- prcomp.object$sdev^2
  eigenvectors.abs <- abs(prcomp.object$rotation)
  
  # finding the eigenvalues that are smaller then 0.70 and saving their indices 
  # in the decreasing order (from least important to the most important)
  low.eigenvalues <- rev(which(eigenvalues < 0.70))
  
  # saving the initial variables, the variables which we are going to reduce
  variables <- rownames(eigenvectors.abs)
  
  for (i in low.eigenvalues) {
    # finding the largest coefficent
    deleted.variable <- which.max(eigenvectors.abs[,i])
    # deleting the variable corresponding to the largest coefficient from the 
    # eigenvector matrix and the variables
    eigenvectors.abs <- eigenvectors.abs[-deleted.variable,]
    variables <- variables[-deleted.variable]
  }
  return(variables)
}   


RegulatePCRVariance <- function(prcomp.object) {
  # Removes the least important for (i in c(638, 640, 652)) {  variables based on the explained variation
  # in the principal component analysis
  # Args:
  # prcomp.object: an object created by the function prcomp()
  #
  # Returns:
  # A vector that contains the variables that are left in the model
  
  cumulative.prop.of.var <- cumsum(prcomp.object$sdev^2 / sum(prcomp.object$sdev^2))
  eigenvectors.abs <- abs(prcomp.object$rotation)
  
  # finding how many PCs are needed to explain at least 80 % of the variance
  excess.components <- which(cumulative.prop.of.var > 0.80)
  excess.components <- excess.components[-1]
  excess.components <- rev(excess.components)
  
  # saving the initial variables
  variables <- rownames(eigenvectors.abs)
  
  for (i in excess.components) {
    deleted.variable <- which.max(eigenvectors.abs[,i])
    eigenvectors.abs <- eigenvectors.abs[-deleted.variable,]
    variables <- variables[-deleted.variable]
  } 
  return(variables)
}  

FitWithStepPCA <- function(training.set,
                           direction, steps = 1000) {
  # Selects a principal component regression model via the step-wise algorithm
  #
  # Args:
  #   training.set:
  #   direction: both/forward/backward
  #   steps:
  #
  # Returns:
  #
  #   a list of coefficients, residuals and fitted values
  
  response.variable <- colnames(training.set)[1]
  pca <- prcomp(~., data = training.set[,-1],
                center = TRUE, scale. = TRUE, tol = NULL)
  
  transformed.training.set <- data.frame(training.set[,1], pca$x)
  colnames(transformed.training.set)[1] <- colnames(training.set)[1]
  
  formula.lm.full <- as.formula(paste(response.variable, " ~ .", sep = ""))
  formula.lm.constant <- as.formula(paste(response.variable, " ~ 1", sep = ""))
  
  lm.full <- lm(formula = formula.lm.full, 
                data = transformed.training.set)
  lm.constant <- lm(formula = formula.lm.constant,
                    data = transformed.training.set)
  
  step.pca.model <- step(lm.constant,
                         scope = list(upper = lm.full, lower = lm.constant),
                         direction = direction, trace = 0, steps = steps)  
  
  coefficients = as.numeric(coefficients.array)
  names(coefficients) <- rownames(coefficients.array)
  
  
  results <- list("coefficients" = coefficients)
  return(results)
}


TransformToPCACoordinates <- function(x, prcomp.object) {
  # Transform a data matrix to principal coordinates
  #
  # Args:
  #   x:
  #   prcomp.object:
  #
  # Returns:
  #   A dataframe in principal coordinate system
  
  obs <- dim(x)[1]
  vars <- dim(x)[2]
  
  y <- x - matrix(rep(prcomp.object$center, each = obs), ncol = vars, nrow = obs)
  y <- y / matrix(rep(prcomp.object$scale, each = obs), ncol = vars, nrow = obs)
  y <- as.matrix(y) %*% prcomp.object$rotation
  y <- as.data.frame(y)
  
  colnames(y) <- paste("PC", 1:vars, sep = "")
}  



ModelsOfAlgorithms <- function(training.set) {
  # Models generated using different algorithms, currently with lm, lasso
  # 
  #
  # Args:
  #   training.set:
  #
  # Returns:
  # A list of elements ' coefficients', and 'response' variable
  
  # model details 
  response <- colnames(training.set)[1]
  fInitial <- as.formula(paste(response, " ~ 1", sep = ""))
  fFull <- as.formula(paste(response, " ~ .", sep = ""))
  
  # Create a list to store the model results for T2 and D2 observations
  coefficients.by.algorithm <- NULL
    
  # lm 
  tmp.time.result <- system.time(
    tmp.result <- FitWithLM(
      training.set)
  )
  ## If a predictor is not taken into linear regression becuase it is of constant value or due to some other reason, in the final results
  ## the coefficient of that predictor is given a value of zero
  tmp.result$coefficients[is.na(tmp.result$coefficients)] <- 0
  coefficients.by.algorithm$lm <- tmp.result$coefficients
 
  # lmstep
  tmp.time.result <- system.time(
    tmp.result <- FitWithStep(
      training.set, 
      fInitial, fFull, fInitial, 
      direction = "forward", steps = 1000)
  )
  ## If a predictor is not taken into linear regression becuase it is of constant value or due to some other reason, in the final results
  ## the coefficient of that predictor is given a value of zero
  tmp.result$coefficients[is.na(tmp.result$coefficients)] <- 0
  coefficients.by.algorithm$lmstep <- tmp.result$coefficients
  
  # regularized subsets
  tmp.time.result <- system.time(
    tmp.result <- FitWithRegsubsets(
      training.set, 
      fFull, NULL,"backward")
  )

  ## If a predictor is not taken into linear regression becuase it is of constant value or due to some other reason, in the final results
  ## the coefficient of that predictor is given a value of zero
  tmp.result$coefficients[is.na(tmp.result$coefficients)] <- 0
  coefficients.by.algorithm$regsub <- tmp.result$coefficients
  
  
  # lasso regularization with 11 predictors using lambda lse
  tmp.time.result <- system.time(
    tmp.result <- FitWithGlmnR1(
      training.set)
  )
  
  ## If a predictor is not taken into linear regression becuase it is of constant value or due to some other reason, in the final results
  ## the coefficient of that predictor is given a value of zero
  tmp.result$coefficients[is.na(tmp.result$coefficients)] <- 0
  coefficients.by.algorithm$glmnR1 <- tmp.result$coefficients
  
  
  
  # lasso regularization with maximum 11 predictors using lambda min
  tmp.time.result <- system.time(
    tmp.result <- FitWithGlmnM1(
      training.set)
  )
  
  ## If a predictor is not taken into linear regression becuase it is of constant value or due to some other reason, in the final results
  ## the coefficient of that predictor is given a value of zero
  tmp.result$coefficients[is.na(tmp.result$coefficients)] <- 0
  coefficients.by.algorithm$glmnM1 <- tmp.result$coefficients
  
  
  
  # pcr model 
  tmp.time.result <- system.time(
    tmp.result <- FitWithPCR(
      training.set, 
      formula = fFull, 
      ncomp = (length(training.set)-1), 
      scale = TRUE, regulation = "none" 
    )
  )
  ## If a predictor is not taken into linear regression becuase it is of constant value or due to some other reason, in the final results
  ## the coefficient of that predictor is given a value of zero
  tmp.result$coefficients[is.na(tmp.result$coefficients)] <- 0
  coefficients.by.algorithm$pcr <- tmp.result$coefficients
  
  # pcr model with 10 components 
  tmp.time.result <- system.time(
    tmp.result <- FitWithPCR10(
      training.set, 
      formula = fFull, 
      ncomp = 10, 
      scale = TRUE
    )
  )
  
  ## If a predictor is not taken into linear regression becuase it is of constant value or due to some other reason, in the final results
  ## the coefficient of that predictor is given a value of zero
  tmp.result$coefficients[is.na(tmp.result$coefficients)] <- 0
  coefficients.by.algorithm$pcr10 <- tmp.result$coefficients
  
  
  # pcr model with  variance 
  
  tmp.time.result <- system.time(
    tmp.result <- FitWithPCRVar(
      training.set, 
      test.set, 
      formula = fFull, 
      ncomp = (length(training.set)-1), 
      scale = TRUE, regulation = "variance" 
    )
  )
  
  ## If a predictor is not taken into linear regression becuase it is of constant value or due to some other reason, in the final results
  ## the coefficient of that predictor is given a value of zero
  tmp.result$coefficients[is.na(tmp.result$coefficients)] <- 0
  coefficients.by.algorithm$pcrvar <- tmp.result$coefficients
  
  results  <-  list(coefficients = coefficients.by.algorithm)
  return(results)
}



Train.Model <- function(data, fitting.method) { 
  # Train with LM, Glmnet Lasso and PCA models
  # Args:
  #   data: data retrieved from the database
  #   fitting.method: The type pf the regrssion model
  #
  # Returns:
  #   Trained coefficients 
  
  
  # Create a list to store the coefficients of the model
  coefficients.by.model <- NULL
  
  # Remove the station_info from data
  obs_model_data <- data[, 5:ncol(data)]
  
  # general datastructures for models
  training.set <- obs_model_data
  response <- colnames(training.set)[1]
  fInitial <- as.formula(paste(response, " ~ 1", sep = ""))
  fFull <- as.formula(paste(response, " ~ .", sep = ""))
  # Create a list to store the model results for T2 and D2 observations
  coefficients.by.algorithm <- NULL
  
  
  # Models for TA
  # Call the function ModelsOfAlgorithms() to get the regression models with all algorithms namely
  # lm, lmstep, regsub, GlmnR1, GlmnM1, pcr, pcr10 and pcrvar

  if (fitting.method == "All") { 
    results <- ModelsOfAlgorithms(obs_model_data)
    coefficients.lm     <- results$coefficients$lm
    coefficients.lmstep <- results$coefficients$lmstep
    coefficients.regsub <- results$coefficients$regsub
    coefficients.glmnR1 <- results$coefficients$glmnR1
    coefficients.glmnM1 <- results$coefficients$glmnM1
    coefficients.pcr    <- results$coefficients$pcr
    coefficients.pcr10  <- results$coefficients$pcr10
    coefficients.pcrvar <- results$coefficients$pcrvar
    
    results <- list(coefficients.lm = coefficients.lm,
                  coefficients.lmstep = coefficients.lmstep,
                  coefficients.regsub = coefficients.regsub,
                  coefficients.glmnR1 = coefficients.glmnR1,
                  coefficients.glmnM1 = coefficients.glmnM1,
                  coefficients.pcr    = coefficients.pcr,
                  coefficients.pcr10  = coefficients.pcr10,
                  coefficients.pcrvar = coefficients.pcrvar)
  } else { 
          switch(fitting.method,
                 "lm" = {
                     tmp.time.result <- system.time(
                     tmp.result <- FitWithLM(
                       training.set)
                   )
                   ## If a predictor is not taken into linear regression becuase it is of constant value or due to some other reason, in the final results
                   ## the coefficient of that predictor is given a value of zero
                   tmp.result$coefficients[is.na(tmp.result$coefficients)] <- 0
                   coefficients<- tmp.result$coefficients
                 },
                 "lmstep" = {
                   tmp.time.result <- system.time(
                     tmp.result <- FitWithLMStep(
                       training.set)
                   )
                   ## If a predictor is not taken into linear regression becuase it is of constant value or due to some other reason, in the final results
                   ## the coefficient of that predictor is given a value of zero
                   tmp.result$coefficients[is.na(tmp.result$coefficients)] <- 0
                   coefficients<- tmp.result$coefficients
                 },
                 "GlmnR1" = {
                   tmp.time.result <- system.time(
                     tmp.result <- FitWithGlmnR1(
                       training.set)
                   )
                   ## If a predictor is not taken into linear regression becuase it is of constant value or due to some other reason, in the final results
                   ## the coefficient of that predictor is given a value of zero
                   tmp.result$coefficients[is.na(tmp.result$coefficients)] <- 0
                   coefficients<- tmp.result$coefficients
                 },
                 "GlmnM1" = {
                   tmp.time.result <- system.time(
                     tmp.result <- FitWithGlmnM1(
                       training.set)
                   )
                   ## If a predictor is not taken into linear regression becuase it is of constant value or due to some other reason, in the final results
                   ## the coefficient of that predictor is given a value of zero
                   tmp.result$coefficients[is.na(tmp.result$coefficients)] <- 0
                   coefficients<- tmp.result$coefficients
                 },
                 "pcr" = {
                   tmp.time.result <- system.time(
                     tmp.result <- FitWithPCR(
                       training.set)
                   )
                   ## If a predictor is not taken into linear regression becuase it is of constant value or due to some other reason, in the final results
                   ## the coefficient of that predictor is given a value of zero
                   tmp.result$coefficients[is.na(tmp.result$coefficients)] <- 0
                   coefficients<- tmp.result$coefficients
                 },
                 "pcr10" = {
                   tmp.time.result <- system.time(
                     tmp.result <- FitWithPCR10(
                       training.set)
                   )
                   ## If a predictor is not taken into linear regression becuase it is of constant value or due to some other reason, in the final results
                   ## the coefficient of that predictor is given a value of zero
                   tmp.result$coefficients[is.na(tmp.result$coefficients)] <- 0
                   coefficients<- tmp.result$coefficients
                 },
                 "pcrvar" = {
                   tmp.time.result <- system.time(
                     tmp.result <- FitWithPCRVar(
                       training.set)
                   )
                   ## If a predictor is not taken into linear regression becuase it is of constant value or due to some other reason, in the final results
                   ## the coefficient of that predictor is given a value of zero
                   tmp.result$coefficients[is.na(tmp.result$coefficients)] <- 0
                   coefficients <- tmp.result$coefficients
                 }
          )
          results <- list(coefficients = coefficients, elapsed_time = tmp.time.result)  
  }
  
  return(results)
                 
}
  
 
 
#' A helper function that tests whether an object is either NULL _or_
#' a list of NULLs
#'
#' @keywords internal
is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))

#' @keywords internal
rmNullObs <- function(x) {
  x <- Filter(Negate(is.NullOb), x)
  lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
}

rbind.all.columns <- function(x, y) {
  
  x.diff <- setdiff(colnames(x), names(y))
  y.diff <- setdiff(names(y), colnames(x))
  
  x[, c(as.character(y.diff))] <- NA
  
  y[, c(as.character(x.diff))] <- NA
  
  return(rbind(x, y))
}
