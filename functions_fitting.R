# This is the linear regression training program, with options to both purrr fitting and ordinary glm fitting using a for loop
# We retieve the observation data from Climate Database, CLDB,  and the model data from Model Output Statistics, MOS.
# This script initializes interfaces to various databases that can be used to retrieve the observation and model data
# and also to implement the MOS training/analysis.
# NOTE (040418): This code is very slow with ordinary glm fitting. The reason is in the Train.Model -function. Direct reference to FitWithGlmnR1purrr -function is much faster than to Train.Model -function. This part is now commented behind the current call. Correct this later in order to allow different fitting algorithms to be used.
# Other ways to make the code faster are: FetchMOSDataFP is not run in the innermost loop, also parallelisation is possible for each variable that is estimated.

 
MOS_training <- function(station_id, obsdata, mosdata, max_variables, fitting_method, fitting_algorithm, station_type, output_dir) { 
  # Args
  #   station_id: station_id
  #   mosdata: MOS data from MOS database
  #   obsdata: Observation data from CLDB databas 
  #   max_variables: max number of predictors in the fitted regression
  #   fitting_method: either purrr or glm
  #   fitting_algorithm: Those linear model options specified in the function Train.Model
  #   station_type: 1 for foreign, 0 for Finnish stations
  #   output_dir: the directory where the coefficients are stored
  
  # Creating output directory if it does not exist
  ifelse(!dir.exists(file.path(output_dir)), dir.create(file.path(output_dir)), FALSE)
  # Initialising the lists for station_id, analysis_time, forecast_period
  analysis_times <- all_producer_lists$ECMWF$analysis_times
  forecast_periods <- all_producer_lists$ECMWF$forecast_periods_hours
  
  # As a first thing, possibly included predictors TAMIN12H/TAMAX12H are interpolated to previous 11 hours so that further interpolation is avoided
  obsdata <- InterpolateMinMaxValues(station_id, obsdata, station_type)
  
  # Divide data into seasons
  data_seasons <- SplitSeasons(station_id, mosdata, obsdata)
  rm(obsdata)
  rm(mosdata)
  
  for (ss in 1:4)  {   # Seasons winter: 1, spring: 2, Summer: 3 and autumn: 4
    mos_season_data <- data_seasons[[1]][[ss]]
    obs_season_data <- data_seasons[[2]][[ss]]
    
    # Only proceed to training phase if there is both model and observation data available, at least some of it
    if (dim(obs_season_data)[1]!=0 & dim(mos_season_data)[1]!=0) {
      # Generate the MOS dataframe for a station, for two analysis_times (00 and 12 UTC) and for 64 forecast periods
      df_mos <- GenerateMOSDataFrame(station_id,mos_season_data)
      
      for (aa in analysis_times) {  # Analysis times 00 UTC and 12 UTC 
        # The predictands in the variable_list_predictands have been checked in the main programme so that at least some data exists
        for (rr in 1:length(variable_list_predictands$variable_name)) {
          response_name <- variable_list_predictands$variable_name[rr]
          if (station_type==1) {
            response_id <- variable_list_predictands$variable_name[rr]
          } else {
            if (variable_list_predictands$table_name[rr]=="observation_data_v1") {
              response_id <- variable_list_predictands$variable_name[rr]
            } else {
              response_id <- all_variable_lists$estimated_parameters$CLDB_observation_data_v1[match(variable_list_predictands$variable_name[rr],rownames(all_variable_lists$estimated_parameters))]
            }
          }
          
          # Initializing the training matrix for a season, for an analysis
          coefficients.station.season.atime.fperiod <- as.data.frame(matrix( NA, nrow=length(all_producer_lists$ECMWF$forecast_periods_hours), ncol=((length(variable_list_predictors$variable_name))+1)))
          rownames(coefficients.station.season.atime.fperiod) <- forecast_periods
          colnames(coefficients.station.season.atime.fperiod) <- c("Intercept", variable_list_predictors$variable_name)
          
          # Selecting either gl or purrr method. purrr is the default method here.
          if (fitting_method=="glm") {
            ##### ordinary glm fitting with a for loop
            for (ff in forecast_periods) { # There are 64 forecast_periods
              df_mos_aa_fp <- FetchMOSDataFP(df_mos, aa, ff) 
              data_response <- FetchData_season_analysis_time(station_id, df_mos_aa_fp, obs_season_data, response_name, response_id, station_type)
              data_fit <- data_response # data_response$data  # data for a particular response variable
              # PAUSE HERE IF YOU WANT TO ANALYZE STRANGE FUTURE PREDICTIONS!!!
              #print("pause to analyze data matrix")
              #Sys.sleep(10)
              # # READ IN COEFFICIENTS FROM THE FILE
              # station_coeffs <- ReadInMOScoefficientsFromFile(station_id,mos_label,forecast_periods,analysis_times,ss,response_name)
              # response <- data_response$response
              data_fit <- CleanData(data_fit)  # Cleaning the NAs
              # Training the regression model, checking that sample size is large enough
              if (dim(data_fit)[1] > modelobspairs_minimum_sample_size) {
                results <- FitWithGlmnR1purrr(training.set = data_fit, max_variables = max_variables) # Train.Model(data_fit, fitting_algorithm, max_variables = 10)
                stn_coeffs <- results$coefficients
                coefficients.station.season.atime.fperiod[match(ff,forecast_periods),na.omit(match(names(stn_coeffs),colnames(coefficients.station.season.atime.fperiod)))] <- stn_coeffs
              }
              # print(ff)
            }
            rm(ff)
          } else {
            ##### purrr fitting
            df_mos_aa <- FetchMOSDataAA(df_mos, aa) 
            data_response <- FetchData_season_analysis_time(station_id, df_mos_aa, obs_season_data, response_name, response_id, station_type)
            
            #### Different forecast periods have different number of predictors. Separate data accordingly.
            ### 00, 03-144 and 150-240
            data0 <- filter(data_response, as.integer(forecast_period) == 0)
            data0 <- CleanData(data0)
            data0$forecast_period <- as.integer(data0$forecast_period)
            data1 <- filter(data_response, as.integer(forecast_period) >= 03  & as.integer(forecast_period) <= 144)
            data1 <- CleanData(data1)
            data1$forecast_period <- as.integer(data1$forecast_period)
            data2 <- filter(data_response, as.integer(forecast_period) >= 150 & as.integer(forecast_period) <= 240)
            data2 <- CleanData(data2)
            data2$forecast_period <- as.integer(data2$forecast_period)
            # fit the models HERE THE MINIMUM SAMPLE SIZE NEEDS TO BE INCLUDED SO THAT MODEL IS NOT FIT UNLESS ENOUGH DATA!!!
            # (dim(data_fit)[1] > modelobspairs_minimum_sample_size)
            glm0 <- (data0 %>% split(.$forecast_period) %>% purrr::map(~FitWithGlmnR1purrr(training.set = ., max_variables = max_variables))) #purrr::map(~Train.Model(data = ., fitting_algorithm = fitting_algorithm)))
            glm1 <- (data1 %>% split(.$forecast_period) %>% purrr::map(~FitWithGlmnR1purrr(training.set = ., max_variables = max_variables))) #purrr::map(~Train.Model(data = ., fitting_algorithm = fitting_algorithm)))
            glm2 <- (data2 %>% split(.$forecast_period) %>% purrr::map(~FitWithGlmnR1purrr(training.set = ., max_variables = max_variables))) #purrr::map(~Train.Model(data = ., fitting_algorithm = fitting_algorithm)))
            glm_model <- do.call(c,list(glm0,glm1,glm2))
            if (length(glm_model)>0) {
              for (i in 1:length(glm_model)) { 
                ff <- names(glm_model)[i]
                if (nchar(ff) == 1) {
                  ff <- paste0("0", ff)
                }
                stn_coeffs <- glm_model[[i]]$coefficients
                coefficients.station.season.atime.fperiod[match(ff,forecast_periods),na.omit(match(names(stn_coeffs),colnames(coefficients.station.season.atime.fperiod)))] <- stn_coeffs
                rm(ff)
                rm(stn_coeffs)
              }
              rm(i)
            }
          }
          
          coefficients.station.season.atime.fperiod[is.na(coefficients.station.season.atime.fperiod)] <- 0
          df <- t(coefficients.station.season.atime.fperiod)
          # Only saving coefficients into a file if enough forecast_periods are present in the data.
          # Only single consecutive forecast periods are allowed missing, not two or more
          chunks <- rle(colSums(df==0)==dim(df)[1])
          if (length(which(chunks$lengths>1 & chunks$values==TRUE))==0) {
            filename = paste0(output_dir,"station_",station_id,"_",aa,"_season",ss,"_",response_name,"_level0_",fitting_algorithm,"_MOS_maxvars",max_variables,".csv")
            write.csv(df, file=filename)
            rm(filename)
          }
          rm(chunks)
        }
        rm(rr)
      }
      rm(aa)
    }
    
  }
  rm(ss)
} 



# This function removes all those variables from the variable lists which are not present in the actually retrieved data.
clean_variable_list <- function(variable_list_to_be_cleaned,db_data) {
  
  ### MOS -Previ_ecmos_narrow_v
  if (!is.null(db_data[["MOS"]])) {
    # Full variable list
    all_db_and_derived_vars <- setNames(all_variable_lists[["MOS"]][,c("variable_EC","param_id","level_value")],c("variable_name","param_id","level_value"))
    all_db_and_derived_vars <- rbind(all_db_and_derived_vars,setNames(cbind(rownames(all_variable_lists[["derived_variables_all"]]),all_variable_lists[["derived_variables_all"]][c("derived_param_id","MOS_previ_ecmos_narrow_v_level")]),c("variable_name","param_id","level_value"))[-1,])
    # These are actually retrieved
    retrieved_vars_db <- all_db_and_derived_vars[row.match(unique(db_data[["MOS"]][["previ_ecmos_narrow_v"]][,c("param_id","level_value")]),all_db_and_derived_vars[,c("param_id","level_value")]),]
    # Creating removed_vars -list by subtracting down from full variable list
    removed_vars <- all_db_and_derived_vars
    # Always include derived variables with 8 digit-param_numbers which are calculated later (remove them from removed_list)
    removed_vars <- removed_vars[sapply(removed_vars["param_id"],function (x) stri_count_regex(x,"[0-9]")<8),]
    # Narrow down removed_vars list to those variables which were initially in the list of retrieved variables
    removed_vars <- na.omit(removed_vars[match(as.matrix(subset(variable_list_to_be_cleaned,db=="MOS" & table_name=="previ_ecmos_narrow_v")["variable_name"]),removed_vars[["variable_name"]]),])
    # Narrow down removed_vars list to those which are NOT found among the retrieved vars
    removed_vars <- removed_vars[which(is.na(row.match(removed_vars,retrieved_vars_db))),]
    # Remove unfound variables
    if (dim(removed_vars)[1]>0) {
      variable_list_to_be_cleaned <- variable_list_to_be_cleaned[-which(variable_list_to_be_cleaned[["variable_name"]] %in% removed_vars[["variable_name"]] & variable_list_to_be_cleaned[["table_name"]]=="previ_ecmos_narrow_v" & variable_list_to_be_cleaned[["db"]]=="MOS"),]
    }
    rm(removed_vars)
    rm(all_db_and_derived_vars)
  }
  
  ### MOS -mos_trace_v...
  
  ### verif
  if (!is.null(db_data[["verif"]])) {
    # Full variable list (derived variables can also be fetched (NOT CODED AS IN 160817) and they're named as in rownames(all_variable_lists[["derived_variables_all"]]))
    all_db_and_derived_vars <- setNames(all_variable_lists[["verif"]][,c("id","id")],c("variable_name","id"))
    all_db_and_derived_vars <- rbind(all_db_and_derived_vars,setNames(cbind(rownames(all_variable_lists[["derived_variables_all"]]),all_variable_lists[["derived_variables_all"]]["derived_param_id"]),c("variable_name","id"))[-1,])
    # All these variables can be from any producer
    all_db_and_derived_vars <- all_db_and_derived_vars %>% tidyr::crossing(all_verif_lists[["producers"]][["id"]])
    dimnames(all_db_and_derived_vars)[[2]] <- c("variable_name","id","table_name")
    # These are actually retrieved
    retrieved_vars_db <- all_db_and_derived_vars[row.match(unique(db_data[["verif"]][,c("id","model")]),all_db_and_derived_vars[,c("id","table_name")]),]
    # Creating removed_vars -list by subtracting down from full variable list
    removed_vars <- all_db_and_derived_vars
    # Always include derived variables with 8 digit-param_numbers which are calculated later (remove them from removed_list)
    removed_vars <- removed_vars[sapply(removed_vars["id"],function (x) stri_count_regex(x,"[0-9]")<8),]
    # Narrow down removed_vars list to those variables which were initially in the list of retrieved variables
    removed_vars <- na.omit(removed_vars[row.match(subset(variable_list_to_be_cleaned,db=="verif")[,c("variable_name","table_name")],removed_vars[,c("id","table_name")]),])
    # Narrow down removed_vars list to those which are NOT found among the retrieved vars
    removed_vars <- removed_vars[which(is.na(row.match(removed_vars[,c("id","table_name")],retrieved_vars_db[,c("variable_name","table_name")]))),]
    # Remove unfound variables
    if (dim(removed_vars)[1]>0) {
      variable_list_to_be_cleaned <- variable_list_to_be_cleaned[-which(row.match(variable_list_to_be_cleaned[,c("variable_name","table_name")],removed_vars[,c("variable_name","table_name")]) & variable_list_to_be_cleaned[["db"]]=="verif"),]
    }
    rm(removed_vars)
    rm(all_db_and_derived_vars)
  }
  
  ### CLDB - weather_data_qc (these are retrieved based on variable names specified in rownames(all_variable_lists[["mapping_parameters_all"]]))
  if (!is.null(db_data[["CLDB"]][["weather_data_qc"]])) {
    # Full variable list (derived variables could also be derived for CLDB variables (NOT CODED AS IN 160817), using naming convention as defined in rownames(all_variable_lists[["derived_variables_all"]]). To data frame the data is stored using number all_variable_lists[["derived_variables_all"]][["derived_param_id"]])
    all_db_and_derived_vars <- setNames(as.data.frame(cbind(rownames(all_variable_lists[["mapping_parameters_all"]]),rownames(all_variable_lists[["mapping_parameters_all"]]))[-1,],stringsAsFactors = FALSE),c("variable_name","id"))
    all_db_and_derived_vars <- rbind(all_db_and_derived_vars,setNames(cbind(rownames(all_variable_lists[["derived_variables_all"]]),all_variable_lists[["derived_variables_all"]]["derived_param_id"]),c("variable_name","id"))[-1,])
    # These are actually retrieved (all which is retrieved from weather_data_qc, independent whether the table_name is "both" or "weather_data_qc")
    retrieved_vars_db <- all_db_and_derived_vars[match(unique(db_data[["CLDB"]][["weather_data_qc"]][,"parameter"]),all_db_and_derived_vars[,"id"]),]
    # Creating removed_vars -list by subtracting down from full variable list
    removed_vars <- all_db_and_derived_vars
    # Always include derived variables with 8 digit-param_numbers which are calculated later (remove them from removed_list)
    removed_vars <- removed_vars[sapply(removed_vars["id"],function (x) stri_count_regex(x,"[0-9]")<8),]
    # Narrow down removed_vars list to those variables which were initially in the list of retrieved variables (from the table of interest, table_name "both" is not included here so those variables cannot be removed later)
    removed_vars <- na.omit(removed_vars[match(as.matrix(subset(variable_list_to_be_cleaned,db=="CLDB" & table_name=="weather_data_qc")["variable_name"]),removed_vars[["variable_name"]]),])
    # Narrow down removed_vars list to those variables which are NOT found among the retrieved vars_db
    removed_vars <- removed_vars[which(is.na(row.match(removed_vars[,c("variable_name","id")],retrieved_vars_db[,c("variable_name","id")]))),]
    # Remove unfound variables
    if (dim(removed_vars)[1]>0) {
      variable_list_to_be_cleaned <- variable_list_to_be_cleaned[-which(!is.na(match(variable_list_to_be_cleaned[,c("variable_name")],removed_vars[,c("variable_name")])) & variable_list_to_be_cleaned[["table_name"]]=="weather_data_qc" & variable_list_to_be_cleaned[["db"]]=="CLDB"),]
    }
    rm(removed_vars)
    rm(all_db_and_derived_vars)
  }
  
  ### CLDB - observation_data_v1 (these are retrieved based on all_variable_lists$CLDB_observation_data_v1$measurand_id)
  if (!is.null(db_data[["CLDB"]][["observation_data_v1"]])) {
    # Full variable list (derived variables could also be derived for CLDB variables (NOT CODED AS IN 160817), using naming convention as defined in rownames(all_variable_lists[["derived_variables_all"]]). To data frame the data is stored using number all_variable_lists[["derived_variables_all"]][["derived_param_id"]])
    all_db_and_derived_vars <- setNames(as.data.frame(cbind(all_variable_lists$CLDB_observation_data_v1$measurand_code,all_variable_lists$CLDB_observation_data_v1$measurand_id),stringsAsFactors = FALSE),c("variable_name","measurand_id"))
    all_db_and_derived_vars <- rbind(all_db_and_derived_vars,setNames(cbind(rownames(all_variable_lists[["derived_variables_all"]]),all_variable_lists[["derived_variables_all"]]["derived_param_id"]),c("variable_name","measurand_id"))[-1,])
    # These are actually retrieved (all which is retrieved from observation_data_v1, independent whether the table_name is "both" or "observation_data_v1")
    retrieved_vars_db <- all_db_and_derived_vars[match(unique(db_data[["CLDB"]][["observation_data_v1"]][,"measurand_id"]),all_db_and_derived_vars[,"measurand_id"]),]
    # Creating removed_vars -list by subtracting down from full variable list
    removed_vars <- all_db_and_derived_vars
    # Always include derived variables with 8 digit-param_numbers which are calculated later (remove them from removed_list)
    removed_vars <- removed_vars[sapply(removed_vars["measurand_id"],function (x) stri_count_regex(x,"[0-9]")<8),]
    # Narrow down removed_vars list to those variables which were initially in the list of retrieved variables (from the table of interest, table_name "both" is not included here so those variables cannot be removed later)
    removed_vars <- na.omit(removed_vars[match(as.matrix(subset(variable_list_to_be_cleaned,db=="CLDB" & table_name=="observation_data_v1")["variable_name"]),removed_vars[["measurand_id"]]),])
    # Narrow down removed_vars list to those variables which are NOT found among the retrieved vars_db
    removed_vars <- removed_vars[which(is.na(row.match(removed_vars[,c("variable_name","measurand_id")],retrieved_vars_db[,c("variable_name","measurand_id")]))),]
    # Remove unfound variables
    if (dim(removed_vars)[1]>0) {
      variable_list_to_be_cleaned <- variable_list_to_be_cleaned[-which(!is.na(match(variable_list_to_be_cleaned[,c("variable_name")],removed_vars[,c("measurand_id")])) & variable_list_to_be_cleaned[["table_name"]]=="observation_data_v1" & variable_list_to_be_cleaned[["db"]]=="CLDB"),]
    }
    rm(removed_vars)
    rm(all_db_and_derived_vars)
  }
  
  ### CLDB - both (these are retrieved based on variable names specified in rownames(all_variable_lists[["mapping_parameters_all"]]), remove those variables that do not have any data from either Finnish or from foreign stations)
  if (!is.null(db_data[["CLDB"]])) {
    # Full variable list (derived variables could also be derived for CLDB variables (NOT CODED AS IN 160817), using naming convention as defined in rownames(all_variable_lists[["derived_variables_all"]]). To data frame the data is stored using number all_variable_lists[["derived_variables_all"]][["derived_param_id"]])
    all_db_and_derived_vars <- setNames(as.data.frame(cbind(rownames(all_variable_lists[["mapping_parameters_all"]]),rownames(all_variable_lists[["mapping_parameters_all"]]),all_variable_lists[["mapping_parameters_all"]][["CLDB_observation_data_v1"]])[-1,],stringsAsFactors = FALSE),c("variable_name","weather_data_qc","observation_data_v1"))
    all_db_and_derived_vars <- rbind(all_db_and_derived_vars,setNames(cbind(rownames(all_variable_lists[["derived_variables_all"]]),all_variable_lists[["derived_variables_all"]]["derived_param_id"],all_variable_lists[["derived_variables_all"]]["derived_param_id"]),c("variable_name","weather_data_qc","observation_data_v1"))[-1,])
    # These are actually retrieved (all which is retrieved from weather_data_qc, independent whether the table_name is "both" or "weather_data_qc")
    retrieved_vars_weather_data_qc <- all_db_and_derived_vars[match(unique(db_data[["CLDB"]][["weather_data_qc"]][,"parameter"]),all_db_and_derived_vars[,"weather_data_qc"]),]
    retrieved_vars_observation_data_v1 <- all_db_and_derived_vars[match(unique(db_data[["CLDB"]][["observation_data_v1"]][,"measurand_id"]),all_db_and_derived_vars[,"observation_data_v1"]),]
    retrieved_vars <- rbind(retrieved_vars_weather_data_qc,retrieved_vars_observation_data_v1)
    rm(retrieved_vars_weather_data_qc)
    rm(retrieved_vars_observation_data_v1)
    # Creating removed_vars -list by subtracting down from full variable list
    removed_vars <- all_db_and_derived_vars
    # Always include derived variables with 8 digit-param_numbers which are calculated later (remove them from removed_list)
    removed_vars <- removed_vars[sapply(removed_vars["observation_data_v1"],function (x) stri_count_regex(x,"[0-9]")<8),]
    # Narrow down removed_vars list to those variables which were initially in the list of retrieved variables (from the table of interest, table_name "both" is not included here so those variables cannot be removed later)
    removed_vars <- na.omit(removed_vars[match(as.matrix(subset(variable_list_to_be_cleaned,db=="CLDB" & table_name=="both")["variable_name"]),removed_vars[["variable_name"]]),])
    # Narrow down removed_vars list to those variables which are NOT found among the retrieved vars
    removed_vars <- removed_vars[which(is.na(row.match(removed_vars,retrieved_vars))),]
    
    # Remove unfound variables
    if (dim(removed_vars)[1]>0) {
      variable_list_to_be_cleaned <- variable_list_to_be_cleaned[-which(!is.na(match(variable_list_to_be_cleaned[,c("variable_name")],removed_vars[,c("variable_name")])) & variable_list_to_be_cleaned[["table_name"]]=="both" & variable_list_to_be_cleaned[["db"]]=="CLDB"),]
    }
    rm(removed_vars)
    rm(all_db_and_derived_vars)
  }
  
  invisible(variable_list_to_be_cleaned)
}