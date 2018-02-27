### The Main Program  with glmnet + purrr
# This is the linear regression training program.
# We retieve the observation data from Climate Database, CLDB,  and the model data from Model Output Statistics, MOS.
# This script initializes interfaces to various databases that can be used to retrieve the observation and model data
# and also to implement the MOS training/analysis.


# Removing old lists
rm(list=ls())

setwd("/data/daniel/statcal/R_projects/ld_playground")# Initializing variables and data structures

# Reading in the required packages, mapping lists and functions
source("load_libraries_tables_and_open_connections.R")

# Set the working directory
setwd("/data/daniel/statcal/R_projects/ld_playground/lr_model_training")# Initializing variables and data structures


# start_time_fetch_data_lr <- proc.time()

# User-defined variables
# timestamps_series <- define_time_series(begin_date = "2017-09-14 00:00:00 GMT", end_date = "2017-10-20 21:00:00 GMT", interval_in_hours = 3)
timestamps_series <- define_time_series() # default time series from 2011 to 2018 (present)
modelobspairs_minimum_sample_size <- 100 # Arbitrary number here, could in principle also depend on the number of predictor variables
mos_label <- paste("MOS_ECMWF_250416")
predictor_set <- "only_bestvars2" #"allmodelvars_1prec_noBAD_RH2"
#derived_variables <- c("T2^2","T2^(1/2)","RH_SURF","RH_SURF^2","DECLINATION","SOL_ANGLE")
derived_variables <- NA
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


source("/data/daniel/statcal/R_projects/ld_playground/lr_model_training/functions_purrr.R")
station_list_retrieved <- c(2974)
#for (station_id in station_list_retrieved) {}
#for (station_id in station_list_retrieved) { 

start_retrieval <- proc.time()
  station_id <- station_list_retrieved
  # Initializing the training matrix for a season
  
  
  ### Retrieving predictand data
  function_arguments <- list(variable_list_predictands,station_id,timestamps_series)
  predictand_data <- do.call(retrieve_data_all,function_arguments)
  ### RETRIEVING PREDICTOR DATA ###
  function_arguments <- list(variable_list_predictors,station_id,timestamps_series)
  predictor_data <- do.call(retrieve_data_all,function_arguments)
  
  obsdata <- predictand_data$CLDB$observation_data_v1   # observation data is from observations_data_v1 of CLDB 
  mosdata <- predictor_data$MOS$previ_ecmos_narrow_v    # MOS data 
  
  elapsed_retrieval <- proc.time() - start_retrieval
  
  
  # We split the mosdata and observation_data in to four seasons. The variable "data_seasons"
  # is  a list of mosdata and obsdata each of which contains the four seasaons data
  # data_seasons-> mosdata_seasons-> mos_winter, mos_spring, mos_summer, mos_autumn
  # data_seasons-> obsdata_seasons -> obs_winter, obs_spring, obs_summer, obs_autumn
  
  start_time_glm_fetch <-proc.time()
  
  data_seasons <- SplitSeasons(station_id,mosdata, obsdata) 
  response <- variable_list_predictands[[1]][[1]]     # TA
  
 
  ss <- 1
 #for (ss in 1:4)  {   # Seasons winter: 1, spring: 2, Summer: 3 and autumn: 4
   
    
    mos_season_data <- data_seasons[[1]][[ss]]
    obs_season_data <- data_seasons[[2]][[ss]]
    aa <- "00"
    coefficients.station.season.atime.fperiod <- as.data.frame(matrix( NA, nrow=length(all_producer_lists$ECMWF$forecast_periods_hours), ncol=((length(variable_list_predictors$variable_name) + station_variables)+1)))
    colnames(coefficients.station.season.atime.fperiod ) <- c("station_id", "season", "analysis_time", "forecast_period", "Intercept", variable_list_predictors$variable_name)
    
    # downloads data for a season
    #### FOR TESTING I USE THE FILE  cleaned_data_2794_winter.csv and change the forecast time to
    # POSIXct form by the command data$forecast_time <- as.POSIXct(data$forecast_time, tz = "UTC")
    
    data <- FetchData_season_analysis_time(station_id, aa, mos_season_data, obs_season_data, response)
    #### TO BE THOUGHT ABOUT Here is a temporary way to remove ^ and / from variable names
    names(data)[names(data) == "T2^2"] <- "T2_2"
    names(data)[names(data) == "T2^(1/2)"] <- "T2_0.5"
    names(data)[names(data) == "RH_SURF^2"] <- "RH_SURF_2"
    names(coefficients.station.season.atime.fperiod)[names(coefficients.station.season.atime.fperiod) == "T2^2"] <- "T2_2"
    names(coefficients.station.season.atime.fperiod)[names(coefficients.station.season.atime.fperiod) == "T2^(1/2)"] <- "T2_0.5"
    names(coefficients.station.season.atime.fperiod)[names(coefficients.station.season.atime.fperiod) == "RH_SURF^2"] <- "RH_SURF_2"


    
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
    
    max_variables <- 10
    
    
    start_time_glm = proc.time()
    
    glm0 <- (data0  %>%
               split(.$forecast_period) %>%
               map(FitWithGlmnR1 )
    )

    glm1 <- (data1  %>%
               split(.$forecast_period) %>%
               map(FitWithGlmnR1 )
    )
    
   
    
    
    glm2 <- (data2  %>%
               split(.$forecast_period) %>%
               map(FitWithGlmnR1 )
    )
    
    glm_model  <- do.call(c, list(glm0, glm1,glm2))   
    
   
    elapsed_time_glm_purr = proc.time() - start_time_glm
    
    stn.season.atime <-  c(station_id = station_id,
                           season = ss,
                           analysis_time = aa
                           
    )
    for (i in 1:length(glm_model)) { 
      fperiod <-  names(glm_model)[i]
      if (nchar(fperiod) == 1) {
        fperiod <- paste0("0", fperiod)
      }
      coefficients <- glm_model[[i]]$coefficients
      #names(coefficients)[1] <- "Intercept"
      stn_coeffs <- c(stn.season.atime, forecast_period = fperiod, coefficients)
      coefficients.station.season.atime.fperiod[i,na.omit(match(names(stn_coeffs),names(coefficients.station.season.atime.fperiod)))] <- stn_coeffs
      i <- i + 1
    }
    coefficients.station.season.atime.fperiod[is.na(coefficients.station.season.atime.fperiod)] <- 0 
    ## In the file the coefficients are saved  the name "forecast_period" is replaced by ""
    ## We take the transpose of the coefficient matrix and write to a file using write.table
    names(coefficients.station.season.atime.fperiod)[names(coefficients.station.season.atime.fperiod) == "forecast_period"] <- ""
    filename = paste0("Station_",station_id,"_",aa,"_season",ss,"_",response,"_level0_",fitting.method,"_MOS_maxvars10_PURRR.csv")
    write.table(t(coefficients.station.season.atime.fperiod[, 4:ncol(coefficients.station.season.atime.fperiod)]), file=filename, sep=",", col.names=FALSE)
    
    elapsed_fetch_glm_purr <- proc.time() - start_time_glm_fetch
    
    elapsed_time_retrieval = proc.time() - start_retrieval
    
    
    
##########################    
################ glmnet purr ends here

    
    
    glm1 <- (data1  %>%
               split(.$forecast_period) %>%
               map(FitWithGlmnR1 )
    )
    
    glm13 <- (data13  %>%
               split(.$forecast_period) %>%
               map(FitWithGlmnR1 )
    )
    
    
    glm2 <- (data2  %>%
               split(.$forecast_period) %>%
               map(FitWithGlmnR1 )
    )
    
    glm12 <- do.call(c, list(glm1,glm2))   
    
    
    stn.season.atime <-  c(station_id = station_id,
                           season = ss,
                           analysis_time = aa
                           
    )
    for (i in 1:length(glm12)) { 
      fperiod <-  names(glm12)[i]
      coefficients <- glm12[[i]]$coefficients
      names(coefficients)[1] <- "Intercept"
      stn_coeffs <- c(stn.season.atime, forecast_period = fperiod, coefficients)
      coefficients.station.season.atime.fperiod[i,na.omit(match(names(stn_coeffs),names(coefficients.station.season.atime.fperiod)))] <- stn_coeffs
      i <- i + 1
    }
    coefficients.station.season.atime.fperiod[is.na(coefficients.station.season.atime.fperiod)] <- 0 
    
    
    # lm
    
    
    data0 <- filter(data, as.integer(forecast_period) == 0)
    data0 <- CleanData(data0)
    formula0<- paste("TA ~ ",names(data0)[6],sep="")
    for (l in (7:ncol(data0))) {
      formula0 <- paste(formula0, "+",names(data0)[l],sep="")
    }
    
    
    
    
    ### When using cleaned winter data is clean 
    
    data1 <- filter(data, as.integer(forecast_period) >= 03  & as.integer(forecast_period) <= 144)
    # data1 <- CleanData(data1)
    formula1 <- paste("TA ~ ",names(data1)[6],sep="")
    for (l in (7:ncol(data1))) {
      formula1 <- paste(formula1, "+",names(data1)[l],sep="")
    }
    
    
    
    
    
    data2 <- filter(data, as.integer(forecast_period) >= 150 & as.integer(forecast_period) <= 240)
    # data2 <- CleanData(data2)
    formula2 <- paste("TA ~ ",names(data2)[6],sep="")
    for (l in (7:ncol(data2))) {
      formula2 <- paste(formula2, "+",names(data2)[l],sep="")
    }
    
    
    
    lm0 <- lm(formula0, data0)


    lm1 <- (data1  %>%
                  split(.$forecast_period) %>%
                  map(~lm(formula = formula1, data =.)) 
                  )

  
    lm2 <- (data2  %>%
              split(.$forecast_period) %>%
              map(~lm(formula = formula2, data =.)) 
            )

    
    lm12 <- do.call(c, list(lm1,lm2))   
    
    stn.season.atime <-  c(station_id = station_id,
                           season = ss,
                           analysis_time = aa
     
    )
    for (i in 1:length(lm12)) { 
      fperiod <-  names(lm12)[i]
      coefficients <- lm12[[i]]$coefficients
      names(coefficients)[1] <- "Intercept"
      stn_coeffs <- c(stn.season.atime, forecast_period = fperiod, coefficients)
      coefficients.station.season.atime.fperiod[i,na.omit(match(names(stn_coeffs),names(coefficients.station.season.atime.fperiod)))] <- stn_coeffs
      i <- i + 1
    }
    coefficients.station.season.atime.fperiod[is.na(coefficients.station.season.atime.fperiod)] <- 0 
        
        
    for (i in 1:length(lm12)) { 
      fperiod <-  names(lm12)[i]
      coefficients <- as.data.frame(lm12[[i]][,1, drop=FALSE])
      coeff <- setNames(coefficients$Estimate, row.names(coefficients))
      stn_coeffs <- c(stn.season.atime, forecast_period = fperiod, coeff)
      coefficients.station.season.atime.fperiod[i,na.omit(match(names(stn_coeffs),names(coefficients.station.season.atime.fperiod)))] <- stn_coeffs
      i <- i + 1
    }
    coefficients.station.season.atime.fperiod[is.na(coefficients.station.season.atime.fperiod)] <- 0
   
    
    
    
    
     
    
  ##### Yet another way , from the summary extract the coefficients
    
    lm1 <- (data1  %>%
               split(.$forecast_period) %>%
               map(~lm(formula = formula1, data =.)) %>%
               map(summary) %>%
               map("coefficients"))
    
    
    lm2 <- (data2  %>%
               split(.$forecast_period) %>%
               map(~lm(formula = formula2, data =.)) %>%
               map(summary) %>%
               map("coefficients"))
    
    
  #### concatenating two lists and changing the name to Intercept
    lm12 <- do.call(c, list(lm1,lm2))   
    for (i in 1:length(lm12)) { 
      rownames(lm12[[i]])[1] <- "Intercept"  
    }
    
    
  lm12.df <- as.dataframe(lm12)
  coefficients <- t(lm12[[i]][,1, drop=FALSE])
  rownames(coefficients) <- NULL
  ## run for each analysis time
  
  stn.season.atime <-  c(station_id = station_id,
                                 season = ss,
                                 analysis_time = aa
                                 
                                
  )
  
  
  
  
  for (i in 1:length(lm12)) { 
    fperiod <-  names(lm12)[i]
    coefficients <- as.data.frame(lm12[[i]][,1, drop=FALSE])
    coeff <- setNames(coefficients$Estimate, row.names(coefficients))
    stn_coeffs <- c(stn.season.atime, forecast_period = fperiod, coeff)
    coefficients.station.season.atime.fperiod[i,na.omit(match(names(stn_coeffs),names(coefficients.station.season.atime.fperiod)))] <- stn_coeffs
    i <- i + 1
  }
  coefficients.station.season.atime.fperiod[is.na(coefficients.station.season.atime.fperiod)] <- 0
  ## In the file the coefficients are saved  the name "forecast_period" is replaced by ""
  ## We take the transpose of the coefficient matrix and write to a file using write.table
  names(coefficients.station.season.atime.fperiod)[names(coefficients.station.season.atime.fperiod) == "forecast_period"] <- ""
  filename = paste0("Station_",station_id,"_",aa,"_season",ss,"_",response,"_level0_",fitting.method,"_MOS_maxvars10_PURRR.csv") 
  write.table(t(coefficients.station.season.atime.fperiod[, 4:ncol(coefficients.station.season.atime.fperiod)]), file=filename, sep=",", col.names=FALSE)
 
    
    

  
  
  
####  Sample data60 and lm
  data60 <- filter(data, as.integer(data$forecast_period) == 60 )
  data60_obs_model <- data60[, 5:ncol(data60)]
  response <- colnames(training.set)[1]
  formula <- paste(response, " ~ .", sep = "")
  data60.lm.model <- lm(formula = formula, data = training.set)
  
 
  
  
  
  lm1C <- (data1  %>%
            split(.$forecast_period) %>%
            map(~lm(formula = formula1, data =.)) %>%
            map(summary) %>%
            map("coefficients"))
  
  
  lm2C <- (data2  %>%
            split(.$forecast_period) %>%
            map(~lm(formula = formula2, data =.)) %>%
            map(summary) %>%
            map("coefficients"))
  
  
    
    #----------------------------glmnet
    elapsed_time_lr_purr <- proc.time() - start_time_lr
    
  }
  elapsed_time_lr4_purr<- proc.time() -start_time_lr4 
}
elapse_time_retrival_lr_purr = proc.time() - start_time_fetch_data_lr

# to get the coefficients
lm2[[1]][,1]
rownames(lm2[[1]])[1] <- "Intercept"

data3 <- data0[6:ncol(data0)]
data3 <- data1
data3 <- data1[,4:ncol(data1)]
data3$forecast_period <- as.numeric(data3$forecast_period)

mod_form <- as.formula('TA ~ MSL+T2')
lm3 <- (data3  %>%
          nest(~(forecast_period)) %>%
          mutate(xx = map(data, ~ cv.glmnet(
          x = model.matrix(data = ., mod_form)[,-1],
          y = .$TA
          ))))
 

         
          
mod_test <- mtcars %>%
  nest(-vs) %>%
  mutate(cv_mod=map(data, ~ cv.glmnet(mpg ~ cyl + hp, data=.))) %>%
          map(summary) %>%
          map(coefficients)
for (i in 1:length(coeff[[1]])) {
  results <- list("coefficients" = coefficients)
}

mod_test <- mtcars %>%
  nest(-vs) %>%
  mutate(cv_mod = map(data, ~ cv.glmnet(
    x = model.matrix(data = ., .$mpg ~ .$cyl * .$hp)[,-1],
    y = .$mpg
  )))


library(Matrix)   
glm1 <- (data1  %>%
           split(.$forecast_period) %>%
           map(data, ~cv.glmnet( 
             x = as.matrix(data =.)[,-1],  alpha = 1, pmax = 4,
             y = as.matrix(data =.)[,1]))
)



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

