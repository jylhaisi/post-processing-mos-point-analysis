retrieve_data_MOS <- function(variable_list,station_list_retrieved,timestamps_series) {
  # This function retrieves data from MOS db (both tables previ_ecmos_narrow_v and mos_trace_v (not coded on 120717))
  
  # Checking if arguments are correct
  retrieved_vars <- subset(variable_list,db=="MOS")
  if (dim(retrieved_vars)[1] == 0) {
    stop("no MOS vars in variable list!")
  }
  retrieved_tables <- intersect(c("previ_ecmos_narrow_v","mos_trace_v"),unique(retrieved_vars[["table_name"]]))
  if (length(retrieved_tables) == 0) {
    stop("check table names!")
  }
  
  ECMWF <- all_variable_lists[["MOS"]]
  derived_variables_all <- all_variable_lists[["derived_variables_all"]]
  first_date <- timestamps_series[1]
  last_date <- tail(timestamps_series,1)
  all_retrieved_data <- vector("list",2)
  names(all_retrieved_data) <- c("previ_ecmos_narrow_v","mos_trace_v")
  
  # Generate variable list for previ_ecmos_narrow_v, which is actually retrieved.
  # Including those DMO variables which are needed for the calculation of derived variables:
  # -If previ_ecmos_narrow_v variable list contains RH_2/RH_3/RH_4/RH_5, make sure that RH from pressure levels are retrieved.
  # -If surface RH, surface T2 and D2 are both needed (escaping ^ would be //^)
  if (sum(retrieved_vars[["table_name"]] == "previ_ecmos_narrow_v")>0) {
    retrieved_vars_previ_ecmos_narrow_v <- subset(retrieved_vars,table_name=="previ_ecmos_narrow_v")
    retrieved_vars_previ_ecmos_narrow_v[["variable_name"]] <- gsub("RH_SURF","D2",retrieved_vars_previ_ecmos_narrow_v[["variable_name"]])
    retrieved_vars_previ_ecmos_narrow_v <- rbind(retrieved_vars_previ_ecmos_narrow_v,c("T2","previ_ecmos_narrow_v","MOS"))
    retrieved_vars_previ_ecmos_narrow_v[["variable_name"]] <- gsub("RH_2$","RH",retrieved_vars_previ_ecmos_narrow_v[["variable_name"]])
    retrieved_vars_previ_ecmos_narrow_v[["variable_name"]] <- gsub("RH_3$","RH",retrieved_vars_previ_ecmos_narrow_v[["variable_name"]])
    retrieved_vars_previ_ecmos_narrow_v[["variable_name"]] <- gsub("RH_4$","RH",retrieved_vars_previ_ecmos_narrow_v[["variable_name"]])
    retrieved_vars_previ_ecmos_narrow_v[["variable_name"]] <- gsub("RH_5$","RH",retrieved_vars_previ_ecmos_narrow_v[["variable_name"]])
  }
  # Unique retrieved variables using database/table-specific -names (so not including derived variables)
  retrieved_vars_previ_ecmos_narrow_v <- unique(subset(ECMWF,variable_EC %in% retrieved_vars_previ_ecmos_narrow_v[["variable_name"]])[c("param_id","level_value")])
  retrieved_vars_mos_trace_v <- subset(ECMWF,variable_EC %in% subset(retrieved_vars,table_name=="mos_trace_v")[["variable_name"]])[c("source_param_name","level_value","step_adjustment")]

  
  ### PREVI_ECMOS_NARROW_V ###
  
  # PARSING SQL QUERY USING FUNCTION PARAMETERS AND RETRIEVED VARIABLES LIST, NOT SORTING DATA IN DB QUERY AS IT IS FASTER TO SORT IN R
  sql_query <- paste0("select station_id, extract(hour from analysis_time) as analysis_time, ((analysis_time + interval '1 hour' * forecast_period) || ' UTC') as forecast_time, forecast_period, param_id, level_value, value from previ_ecmos_narrow_v where station_id in (",paste(station_list_retrieved,collapse=','),") and (analysis_time + interval '1 hour' * forecast_period) >= '",format(timestamps_series[1],'%Y-%m-%d %H:%M:%OS'),"' and (analysis_time + interval '1 hour' * forecast_period) <= '",format(tail(timestamps_series,1),'%Y-%m-%d %H:%M:%OS'),"' and param_id in (",paste(sort(unique(retrieved_vars_previ_ecmos_narrow_v[['param_id']])),collapse=','),") and level_value in (",paste(sort(unique(retrieved_vars_previ_ecmos_narrow_v[['level_value']])),collapse=','),") and forecast_period in (",paste(unlist(all_producer_lists[['ECMWF']]['forecast_periods_hours']),collapse=','),");")
  retrieved_data <- RPostgreSQL::dbGetQuery(con1, sql_query)
  rm(sql_query)
  # If no model data is available for station_list_retrieved, stop function
  if (dim(retrieved_data)[1]==0) {
    stop(paste0("no model data available for station_list_retrieved ",paste(station_list_retrieved,collapse=","),"!"))
  }
  # Converting integer-type forecast_period to character-type (for the sake of ordering)
  retrieved_data[["forecast_period"]] <- unlist(all_producer_lists[['ECMWF']]['forecast_periods_hours'])[match(retrieved_data$forecast_period,as.numeric(unlist(all_producer_lists[['ECMWF']]['forecast_periods_hours'])))]
  # Converting string-type timestamp to POSIXct-type
  retrieved_data$forecast_time <- as.POSIXct(as.character(retrieved_data$forecast_time),tz="GMT")
  
  # # SORTING OUT DATA HERE INSTEAD OF DOING IT INSIDE SQL-QUERY
  # retrieved_data <- retrieved_data[order(retrieved_data$station_id,retrieved_data$analysis_time,retrieved_data$forecast_time,retrieved_data$forecast_period,retrieved_data$param_id,retrieved_data$level_value),]
  
  
  
  
  
  # CONVERTING CUMULATIVE VARIABLES TO (SEMI-)INSTANTANEOUS
  # Cumulative variables in db have cumulative time period from the beginning of forecast analysis time. Translating these values to cumulative values over one time step. For forecast_period==0, no cumulative values exist. All cumulative variables are surface variables.
  all_cumulative_vars <- c(88,89,179,184,186,495,496,497,499,655,693,694,696)
  retrieved_cumulative_vars <- intersect(all_cumulative_vars,retrieved_vars_previ_ecmos_narrow_v[["param_id"]])
  rm(retrieved_vars_previ_ecmos_narrow_v)
  # Selecting all cumulative variable cells in the data
  previous_time_step <- retrieved_data[which(match(retrieved_data$forecast_period,unlist(all_producer_lists[['ECMWF']]['forecast_periods_hours'])) < length(unlist(all_producer_lists[['ECMWF']]['forecast_periods_hours'])) & ((retrieved_data$param_id) %in% c(retrieved_cumulative_vars))),]
  # Adding one forecast_step_length to forecast_period and forecast_time so that can be subtracted
  previous_time_step$forecast_period <- unlist(all_producer_lists[['ECMWF']]['forecast_periods_hours'])[(match(previous_time_step$forecast_period,unlist(all_producer_lists[['ECMWF']]['forecast_periods_hours']))+1)]
  previous_time_step$forecast_time[which(match(previous_time_step$forecast_period,unlist(all_producer_lists[['ECMWF']]['forecast_periods_hours']))<50)] <- previous_time_step$forecast_time[which(match(previous_time_step$forecast_period,unlist(all_producer_lists[['ECMWF']]['forecast_periods_hours']))<50)]  + (3*3600)
  previous_time_step$forecast_time[which(match(previous_time_step$forecast_period,unlist(all_producer_lists[['ECMWF']]['forecast_periods_hours']))>49)] <- previous_time_step$forecast_time[which(match(previous_time_step$forecast_period,unlist(all_producer_lists[['ECMWF']]['forecast_periods_hours']))>49)]  + (6*3600)
  # # Temporarily changing forecast_time type to character, so that they can be compared
  # retrieved_data$forecast_time <- as.character(retrieved_data$forecast_time,format="%Y-%m-%d %H:%M:%S")
  # previous_time_step$forecast_time <- as.character(previous_time_step$forecast_time,format="%Y-%m-%d %H:%M:%S")
  # Only keeping those rows in previous_time_step, which have a corresponding row in retrieved_data (do not have model data missing)
  previous_time_step <- previous_time_step[which(!is.na(row.match(previous_time_step[,c('station_id','analysis_time','forecast_time','forecast_period','param_id','level_value')],retrieved_data[,c('station_id','analysis_time','forecast_time','forecast_period','param_id','level_value')]))),]
  # Vice versa: removing those rows in retrieved_data which do not have a corresponding row in previous_time_step
  bare_cumulative_vars <- subset(retrieved_data, (param_id %in% retrieved_cumulative_vars & (match(retrieved_data$forecast_period,unlist(all_producer_lists[['ECMWF']]['forecast_periods_hours']))>2)))
  removed_rows <- bare_cumulative_vars[which(is.na(row.match(bare_cumulative_vars[,c('station_id','analysis_time','forecast_time','forecast_period','param_id','level_value')],previous_time_step[,c('station_id','analysis_time','forecast_time','forecast_period','param_id','level_value')]))),]
  if (dim(removed_rows)[1]>0) {
    retrieved_data <- retrieved_data[-row.match(removed_rows,retrieved_data),]
  }
  rm(removed_rows)
  rm(bare_cumulative_vars)
  # subtracting
  retrieved_data$value[row.match(previous_time_step[,c('station_id','analysis_time','forecast_time','forecast_period','param_id','level_value')],retrieved_data[,c('station_id','analysis_time','forecast_time','forecast_period','param_id','level_value')])] <- retrieved_data$value[row.match(previous_time_step[,c('station_id','analysis_time','forecast_time','forecast_period','param_id','level_value')],retrieved_data[,c('station_id','analysis_time','forecast_time','forecast_period','param_id','level_value')])] - previous_time_step$value
  rm(previous_time_step)
  rm(all_cumulative_vars)
  rm(retrieved_cumulative_vars)
  # # Changing forecast_time to POSIXct type
  # retrieved_data$forecast_time <- as.POSIXct(as.character(retrieved_data$forecast_time),tz="GMT")
  
  
  
  
  # SIMPLE VARIABLE CONVERSIONS NEEDED FOR THEIR OPERATIONAL USE
  # For all radiation parameters, variable conversion J/m2 -> W/m2 is made. Remembering here that [W] = [J/s].
  if (!length(retrieved_data$value[which(retrieved_data$param_id %in% c(88,89,495,496,497,499,696))])==FALSE) {
    retrieved_data$value[which(match(retrieved_data$forecast_period,unlist(all_producer_lists[['ECMWF']]['forecast_periods_hours']))<50 & retrieved_data$param_id %in% c(88,89,495,496,497,499,696))] <- retrieved_data$value[which(match(retrieved_data$forecast_period,unlist(all_producer_lists[['ECMWF']]['forecast_periods_hours']))<50 & retrieved_data$param_id %in% c(88,89,495,496,497,499,696))] / (3*3600)
    retrieved_data$value[which(match(retrieved_data$forecast_period,unlist(all_producer_lists[['ECMWF']]['forecast_periods_hours']))>49 & retrieved_data$param_id %in% c(88,89,495,496,497,499,696))] <- retrieved_data$value[which(match(retrieved_data$forecast_period,unlist(all_producer_lists[['ECMWF']]['forecast_periods_hours']))>49 & retrieved_data$param_id %in% c(88,89,495,496,497,499,696))] / (6*3600)
  }
  # Cumulative precipitation over forecast_step is divided with number of hours during forecast_step. So [mm] -> [mm/h]
  if (!length(retrieved_data$value[which(retrieved_data$param_id %in% c(179,184,186))])==FALSE) {
    retrieved_data$value[which(match(retrieved_data$forecast_period,unlist(all_producer_lists[['ECMWF']]['forecast_periods_hours']))<50 & retrieved_data$param_id %in% c(179,184,186))] <- retrieved_data$value[which(match(retrieved_data$forecast_period,unlist(all_producer_lists[['ECMWF']]['forecast_periods_hours']))<50 & retrieved_data$param_id %in% c(179,184,186))] / 3
    retrieved_data$value[which(match(retrieved_data$forecast_period,unlist(all_producer_lists[['ECMWF']]['forecast_periods_hours']))>49 & retrieved_data$param_id %in% c(179,184,186))] <- retrieved_data$value[which(match(retrieved_data$forecast_period,unlist(all_producer_lists[['ECMWF']]['forecast_periods_hours']))>49 & retrieved_data$param_id %in% c(179,184,186))] / 6
  }
  # Cloudiness (low, middle, high) is divided by 100, so 0...100 -> 0...1
  if (!length(retrieved_data$value[which(retrieved_data$param_id %in% c(196,197,198,187))])==FALSE) {
    retrieved_data$value[which(retrieved_data$param_id %in% c(196,197,198,187))] <- retrieved_data$value[which(retrieved_data$param_id %in% c(196,197,198,187))] / 100
  }
  
  
  
  
  
  # CALCULATING ALL DERIVED VARIABLES AND TIME-LAGGED VARIABLES
  derived_vars_previ_ecmos_narrow_v <- intersect(unlist(subset(retrieved_vars,table_name=="previ_ecmos_narrow_v")[["variable_name"]]),rownames(derived_variables_all))
  if (length(derived_vars_previ_ecmos_narrow_v)>0) {
    
    # ADDING TIME-LAGGED VARIABLES TO DATA (CURRENTLY ONLY SUPPORTS DIFFERENCE OF ONE IN FORECAST_PERIODS)
    derived_vars_previ_ecmos_narrow_v_time_lagged <- derived_vars_previ_ecmos_narrow_v[grep("_M",derived_vars_previ_ecmos_narrow_v)]
    if (length(derived_vars_previ_ecmos_narrow_v_time_lagged)>0) {
      previous_retrieved <- unique(derived_variables_all[match(derived_vars_previ_ecmos_narrow_v_time_lagged,rownames(derived_variables_all)),c("MOS_previ_ecmos_narrow_v_param_id","MOS_previ_ecmos_narrow_v_level","derived_param_id")])
      previous_time_step <- subset(retrieved_data, ((match(forecast_period,unlist(all_producer_lists[['ECMWF']]['forecast_periods_hours'])) < length(unlist(all_producer_lists[['ECMWF']]['forecast_periods_hours']))) & (param_id %in% as.integer(previous_retrieved[["MOS_previ_ecmos_narrow_v_param_id"]])) & (level_value %in% as.integer(previous_retrieved[["MOS_previ_ecmos_narrow_v_level"]]))))
  
      # Adding ONE forecast_step_length to forecast_period and forecast_time so that can be subtracted
      previous_time_step$forecast_period <- unlist(all_producer_lists[['ECMWF']]['forecast_periods_hours'])[(match(previous_time_step$forecast_period,unlist(all_producer_lists[['ECMWF']]['forecast_periods_hours']))+1)]
      previous_time_step$forecast_time[which(match(previous_time_step$forecast_period,unlist(all_producer_lists[['ECMWF']]['forecast_periods_hours']))<50)] <- previous_time_step$forecast_time[which(match(previous_time_step$forecast_period,unlist(all_producer_lists[['ECMWF']]['forecast_periods_hours']))<50)]  + (3*3600)
      previous_time_step$forecast_time[which(match(previous_time_step$forecast_period,unlist(all_producer_lists[['ECMWF']]['forecast_periods_hours']))>49)] <- previous_time_step$forecast_time[which(match(previous_time_step$forecast_period,unlist(all_producer_lists[['ECMWF']]['forecast_periods_hours']))>49)]  + (6*3600)
      # Replacing param_id with derived_param_id
      previous_time_step$param_id <- previous_retrieved[row.match(previous_time_step[,c("param_id","level_value")],previous_retrieved[,c("MOS_previ_ecmos_narrow_v_param_id","MOS_previ_ecmos_narrow_v_level")]),"derived_param_id"]
  
      # Binding time-lagged variables to retrieved_data
      retrieved_data <- rbind(retrieved_data,previous_time_step)
  
      rm(previous_time_step)
      rm(previous_retrieved)
    }
    rm(derived_vars_previ_ecmos_narrow_v_time_lagged)
    
    # ADDING DERIVED MODEL VARIABLES TO DATA (astronomical variables are calculated later as otherwise a lot of duplicate data values would need to be stored to data matrix)
    derived_vars_previ_ecmos_narrow_v_derived <- derived_vars_previ_ecmos_narrow_v[-grep("_M",derived_vars_previ_ecmos_narrow_v)]
    if (length(derived_vars_previ_ecmos_narrow_v_derived)>0) {
      derived_retrieved_all <- unique(derived_variables_all[match(derived_vars_previ_ecmos_narrow_v_derived,rownames(derived_variables_all)),c("MOS_previ_ecmos_narrow_v_param_id","MOS_previ_ecmos_narrow_v_level","derived_param_id")])
      for (derived_var_index in seq(derived_vars_previ_ecmos_narrow_v_derived)) {
        # If derived variable is based on one single variable
        if (derived_retrieved_all[derived_var_index,"MOS_previ_ecmos_narrow_v_param_id"] != "") {
          derived_retrieved <- subset(retrieved_data, ((param_id %in% as.integer(derived_retrieved_all[derived_var_index,"MOS_previ_ecmos_narrow_v_param_id"])) & (level_value %in% as.integer(derived_retrieved_all[derived_var_index,"MOS_previ_ecmos_narrow_v_level"]))))
        }
        # Relative humidity (clculated using the same method as in STU (method2 in https://github.com/fmidev/himan/blob/master/doc/plugin-relative_humidity.md). This gives ~0.1% smaller values compared to method below, with maximum of maximum 0.5% RH)
        if (length(grep("RH_SURF",rownames(derived_retrieved_all)[derived_var_index]))==1) {
          derived_retrieved <- subset(retrieved_data, (param_id==153) & (level_value==0))
          SURF_D2_matching <- subset(retrieved_data, (param_id==162) & (level_value==0))
          SURF_D2_matching <- SURF_D2_matching[row.match(derived_retrieved[,c("station_id","analysis_time","forecast_time","forecast_period","level_value")],SURF_D2_matching[,c("station_id","analysis_time","forecast_time","forecast_period","level_value")]),]
          derived_retrieved[,"value"] <- derived_retrieved[,"value"] - 273.15
          SURF_D2_matching[,"value"] <- SURF_D2_matching[,"value"] - 273.15
          derived_retrieved[,"value"] <- 100 * (exp(1.8+17.27*(SURF_D2_matching[,"value"] / (SURF_D2_matching[,"value"] + 237.3)))) / (exp(1.8 + 17.27 * (derived_retrieved[,"value"] / (derived_retrieved[,"value"] + 237.3))))
          # condensing_humidity <- 6.112*exp((17.62*derived_retrieved$value)/(derived_retrieved$value+243.12))
          # specific_humidity <- 6.112*exp((17.62*SURF_D2_matching$value)/(SURF_D2_matching$value+243.12))
          # derived_retrieved[,"value"] <- 100*(specific_humidity/condensing_humidity)
          # rm(condensing_humidity)
          # rm(specific_humidity)
          rm(SURF_D2_matching)
        }
        # if derived_param_id has six numbers, raise to the power
        if (nchar(derived_retrieved_all[derived_var_index,"derived_param_id"])==6) {
          # if first integer is 5 or less, raise to the power of first integer
          if (as.integer(substr(derived_retrieved_all[derived_var_index,"derived_param_id"],1,1))<6) {
            derived_retrieved[,"value"] <- derived_retrieved[,"value"]^(as.integer(substr(derived_retrieved_all[derived_var_index,"derived_param_id"],1,1)))
          }
          # if first integer is 8, take square root
          if (as.integer(substr(derived_retrieved_all[derived_var_index,"derived_param_id"],1,1))==8) {
            derived_retrieved[,"value"] <- derived_retrieved[,"value"]^0.5
          }
        }
        if (exists("derived_retrieved")) {
          # Changing derived variable number
          derived_retrieved[["param_id"]] <- derived_retrieved_all[derived_var_index,"derived_param_id"]
          # Binding derived variable to retrieved data
          retrieved_data <- rbind(retrieved_data,derived_retrieved)
          rm(derived_retrieved)
        }
      }
      rm(derived_var_index)
      rm(derived_retrieved_all)
    }
    rm(derived_vars_previ_ecmos_narrow_v_derived)
  }
  rm(derived_vars_previ_ecmos_narrow_v)
  
  # U/V wind components ae used as they are in linear model. Their transformation to speed+direction is commented below (in Finnish)
  
  #   Tuulen U- ja V-komponenttien muuttaminen nopeudeksi+(kategorisoiduksi) suuntakulmaksi. Muuttujat u ja v ovat ECMWF-parametriluettelossakin määriteltyinä nopeus (WS) + suuntakulma (WD)!
  #
  #   # Aluksi testataan, että U- ja V-komponenttidataa on saatavana yhtä paljon sekä näiden timestampit ovat keskenään synkassa.
  #   # Jos U- ja V- komponentteja ei ole TÄSMÄLLEEN yhtä paljon saatavilla ja timestampit TÄSMÄLLEEN samat, KESKEYTETÄÄN looppi ja siirrytään seuraavaan soluun!
  #   if ((length(which(eval(parse(text=retrieved_data))$param_id==174))!=length(which(eval(parse(text=retrieved_data))$param_id==171))) | sum(as.numeric(eval(parse(text=retrieved_data))$forecast_time[(which(eval(parse(text=retrieved_data))$param_id==171))]-eval(parse(text=retrieved_data))$forecast_time[(which(eval(parse(text=retrieved_data))$param_id==174))]))!=0) {
  #     # Hypätään loopissa seuraavaan ajanjaksoon, koska mallidataa ei ole.
  #     print(paste("tuulen U- ja V-komponenttien lukumäärä tai timestampit datalle --retrieved_data ",aseman_nimi,"-- eivät ole yhteneviä!",sep=""))
  #     # Otetaan pois mallidata
  #     com_string <- paste("rm(retrieved_data)",sep="")
  #     eval(parse(text=com_string))
  #     rm(com_string)
  #     rm(retrieved_data)
  #     next
  #   }
  #   if ((length(which(eval(parse(text=retrieved_data))$param_id==174))==length(which(eval(parse(text=retrieved_data))$param_id==171))) & sum(as.numeric(eval(parse(text=retrieved_data))$forecast_time[(which(eval(parse(text=retrieved_data))$param_id==171))]-eval(parse(text=retrieved_data))$forecast_time[(which(eval(parse(text=retrieved_data))$param_id==174))]))==0) {
  #     tuulen_komponentit=cbind(eval(parse(text=retrieved_data))$value[(which(eval(parse(text=retrieved_data))$param_id==171))],eval(parse(text=retrieved_data))$value[(which(eval(parse(text=retrieved_data))$param_id==174))])
  #
  #     # Tuulen nopeus tallennetaan alkuperäiseen U10-komponentin muuttujaan
  #     retrieved_data$value[which(retrieved_data$param_id==171)] <- round(sqrt(apply(tuulen_komponentit^2,1,sum)),digits=1)
  #
  #     # Tuulen suuntakulma tallennetaan alkuperäiseen V10-komponentin muuttujaan
  #     suuntakulma <- atan(tuulen_komponentit[,1]/tuulen_komponentit[,2])/pi*180
  #     # Jos tuulen V-komponentti>0, lisätään suuntakulmaan 180 astetta. Jos tuulen V-komponentti<0, lisätään suuntakulmaan 360 astetta ja otetaan jakojäännös 360:sta.
  #     suuntakulma[which(tuulen_komponentit[,2]>=0)] <- suuntakulma[which(tuulen_komponentit[,2]>=0)] + 180
  #     suuntakulma[which(tuulen_komponentit[,2]<0 & tuulen_komponentit[,1]>0)] <- (suuntakulma[which(tuulen_komponentit[,2]<0 & tuulen_komponentit[,1]>0)]+360)
  #     retrieved_data$value[which(retrieved_data$param_id==174)] <- round(suuntakulma,digits=1)
  #     rm(tuulen_komponentit)
  #     rm(suuntakulma)
  #   }
  

  # Removing rows which have "param_id" conversion chr -> num impossible (with is.na(as.numeric(x))==TRUE)
  removed_sum <- sum(is.na(as.numeric(retrieved_data$param_id)))
  if (removed_sum>0) {
    retrieved_data <- retrieved_data[-(which(is.na(as.numeric(retrieved_data$param_id)))),]
  }
  rm(removed_sum)
  # # Converting param_id chr -> num
  # retrieved_data$param_id <- as.numeric(retrieved_data$param_id)

  
  
  # # FOR DEVELOPMENT EXPERIMENT: REMOVE VARIABLES FROM THE DATA
  # retrieved_data <- subset(retrieved_data,param_id!=200153)
  # retrieved_data <- subset(retrieved_data,param_id!=174)
  # retrieved_data <- subset(retrieved_data,param_id!=171)
  # retrieved_data <- subset(retrieved_data,param_id!=163)
  
  
  
  # REMOVING UNNECESSARY ACTUALLY RETRIEVED DATA WHICH IS NOT IN THE VARIABLE LIST (is not needed now or later)
  # These variables were supposed to be retrieved and calculated
  retrieved_vars_previ_ecmos_narrow_v <- subset(retrieved_vars,table_name=="previ_ecmos_narrow_v")
  # These variables were actually retrieved
  all_mos_and_derived_variables <- setNames(all_variable_lists[["MOS"]][,c("variable_EC","param_id","level_value")],c("variable_name","param_id","level_value"))
  all_mos_and_derived_variables <- rbind(all_mos_and_derived_variables,setNames(cbind(rownames(all_variable_lists[["derived_variables_all"]]),all_variable_lists[["derived_variables_all"]][c("derived_param_id","MOS_previ_ecmos_narrow_v_level")]),c("variable_name","param_id","level_value")))
  actually_retrieved_vars_previ_ecmos_narrow_v <- all_mos_and_derived_variables[row.match(unique(retrieved_data[,c("param_id","level_value")]),all_mos_and_derived_variables[,c("param_id","level_value")]),]
  removed_vars <- actually_retrieved_vars_previ_ecmos_narrow_v[which(is.na(match(actually_retrieved_vars_previ_ecmos_narrow_v[["variable_name"]],retrieved_vars_previ_ecmos_narrow_v[["variable_name"]]))),]
  retrieved_data <- retrieved_data[is.na(row.match(retrieved_data[,c("param_id","level_value")],removed_vars[,c("param_id","level_value")])),]
  rm(removed_vars)
  
  # Reordering data according to variable and forecast_time
  retrieved_data <- retrieved_data[order(retrieved_data$station_id,retrieved_data$analysis_time,retrieved_data$forecast_time,retrieved_data$forecast_period,retrieved_data$param_id,retrieved_data$level_value),]
  retrieved_data$value <- round(retrieved_data$value,digits=2)
  # Changing analysis_time "0" to "00"
  retrieved_data$analysis_time[which(retrieved_data$analysis_time=="0")] <- "00"
  # Changing level_value from integer to character
  retrieved_data$level_value <- as.character(retrieved_data$level_value)
  
  all_retrieved_data[["previ_ecmos_narrow_v"]] <- retrieved_data
  rm(retrieved_data)
  
  # Returning
  invisible(all_retrieved_data)
}
