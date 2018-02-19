retrieve_data_CLDB <- function(variable_list,station_list_retrieved,timestamps_series) {
  # This function retrieves observations from
  # weather_data_qc @ Vorlon (CLDB, foreign stations) and observation_data_v1 @ Vorlon (CLDB, Finnish stations)
  # CONTINUOUS observations are interpolated in time as defined in global.env() -variable "interpolate_observations"

  # Checking if arguments are correct
  retrieved_vars <- subset(variable_list,db=="CLDB")
  if (dim(retrieved_vars)[1] == 0) {
    stop("no CLDB vars in variable list!")
  }
  retrieved_tables <- intersect(c("observation_data_v1","weather_data_qc","both"),unique(retrieved_vars[["table_name"]]))
  if (length(retrieved_tables) == 0) {
    stop("check table names!")
  }
  if (!length(tail(which((timestamps_series < Sys.time())==TRUE),1))==TRUE) {
    stop("all time stamps are in the future! Cannot retrieve future data!")
  }
  
  # This subfunction cleans up CLDB observation data and interpolates the timeseries. Function is defined inside retrieve_data_CLDB function as the input data columns are CLDB-specific. Neither does it need to be stored in the global environment after it has been executed.
  tidy_and_interpolate_CLDB_data <- function(CLDB_observation_data,CLDB_table) {
    # Input "CLDB_observation_data" has columns ["station_id","obstime","parameter","value"]
    # CLDB_table indicates wheter CLDB_observation_data is from observation_data_v1 or weather_data_qc
    
    # Sanity check for the parameters passed to the function
    if (dim(CLDB_observation_data)[1] == 0) {
      print("data is empty!")
    }
    if (CLDB_table %!in% c("weather_data_qc","observation_data_v1")) {
      print("check CLDB_table!")
    }
    # If you speficy this function in .GlobalEnv, the following lines need to be uncommented and env = parent.frame() needs to be given in the argument list.
    # # These variables could equally well be passed as parameters, but function calls are a bit simpler by doing this way. As this function is defined in .GlobalEnv (outside function retrieve_data_CLDB), it knows nothing about the variables in the calling environment without this.
    # retrieved_stations <- get("retrieved_stations", env)
    # first_date <- get("first_date", env)
    # last_date <- get("last_date", env)


    # TIDYING UP DATA
    # Renaming
    names(CLDB_observation_data) <- c("station_id","obstime","parameter","value")
    # Changing fmisid-numbers to wmon-numbers
    CLDB_observation_data[["station_id"]] <- retrieved_stations[match(CLDB_observation_data[["station_id"]],retrieved_stations[["fmisid"]]),"wmon"]
    CLDB_observation_data <- na.omit(CLDB_observation_data)
    # Removing missing values (some of these are also contained in the database)
    CLDB_observation_data <- CLDB_observation_data[!is.na(CLDB_observation_data$value),]
    CLDB_observation_data <- CLDB_observation_data[which(CLDB_observation_data$value!="nan"),]
    CLDB_observation_data <- CLDB_observation_data[which(CLDB_observation_data$value!="NaN"),]
    # Ordering with respect to station_id and obstime
    CLDB_observation_data <- CLDB_observation_data[order(CLDB_observation_data$station_id,CLDB_observation_data$parameter,CLDB_observation_data$obstime),]
    # Changing timestamp to POSIXct and rounding to the nearest hour
    CLDB_observation_data$obstime <- as.POSIXct(as.character(CLDB_observation_data$obstime),tz="GMT")
    CLDB_observation_data$obstime <- as.POSIXct(round.POSIXt(CLDB_observation_data$obstime, c("hours")))
    # Altering to numeric form
    CLDB_observation_data$value <- as.numeric(as.character(CLDB_observation_data$value))
    # Removing duplicate timestamps (preserving last if several exist: Data is temporally ordered so 19/20 to values are removed)
    while (sum(duplicated(CLDB_observation_data[,c("station_id","parameter","obstime")]))>0) {
      CLDB_observation_data <- CLDB_observation_data[-(which(duplicated(CLDB_observation_data[,c("station_id","parameter","obstime")]))-1),]
    }






    # INTERPOLATING MISSING VALUES
    # Initializing output data frame
    CLDB_observation_data_interpolated <- vector("list",1)
    # Table-specific variables which are not interpolated
    not_interpolated_variables <- eval(parse(text=paste0("all_variable_lists$CLDB_",CLDB_table,"_not_interpolated")))
    # Unique station_id/parameter -pairs in data
    unique_datas <- unique(CLDB_observation_data[,c("station_id","parameter")])
    # These station_id/parameter -pairs are interpolated
    interpolated_datas <- subset(unique_datas,parameter %!in% not_interpolated_variables)


    # Calculating average number of observations per day for those station_id/variable pairs which are interpolated (using dplyr package syntax)
    date_stamps <- CLDB_observation_data
    date_stamps$obstime <- as.POSIXct(trunc.POSIXt(date_stamps$obstime, units=c("days")))
    date_stamps <- na.omit(date_stamps)
    if (dim(date_stamps)[1]>0) {
      obs_per_day <- date_stamps %>% filter(station_id %in% interpolated_datas[["station_id"]],parameter %in% interpolated_datas[["parameter"]]) %>% group_by(station_id,obstime,parameter) %>% summarize(count = length(obstime)) %>% group_by(station_id,parameter) %>% summarize(count = mean(count))
    }
    rm(date_stamps)
    obs_per_day[["count"]]  <- round(obs_per_day[["count"]],digits=2)
    obs_per_day <- as.data.frame(obs_per_day)

    # Looping through all station_id/parameter -pairs in data (dplyr is not obvious to use in temporal data interpolation which really is needed here)
    for (unique_data in seq(dim(unique_datas)[1])) {
      interpolated_data <- subset(CLDB_observation_data,station_id %in% unique_datas[unique_data,"station_id"] & parameter %in% unique_datas[unique_data,"parameter"])[,c("obstime","value")]
      # Interpolate only if variable is continuous and instant
      if (unique_datas[unique_data,"parameter"] %!in% not_interpolated_variables) {
        # This is a crude check: Only interpolate the station_id/variable pair if it has ON AVERAGE more observations than 0.9*(24/max_interpolate_gap) (e.g. if 6 hour interpolation gap is allowed, 4 observations per day is needed and coefficient 0.9 takes into account that some days have observations missing)
        if (obs_per_day[row.match(unique_datas[unique_data,],obs_per_day[,1:2]),"count"] > (0.9*(24/max_interpolate_gap))) {
          interpolated_data_hourly <- seq(first_date, last_date, by="1 hour")
          interpolated_data_hourly <- cbind(interpolated_data_hourly,as.data.frame(matrix(NA,length(interpolated_data_hourly))))
          names(interpolated_data_hourly) <- c("obstime","value")
          interpolated_data_hourly[match(interpolated_data$obstime,interpolated_data_hourly$obstime),"value"] <- interpolated_data[,"value"]
          interpolated_data_hourly[,"value"] <- round(MOSpointutils::interpolate_NA_values(interpolated_data_hourly[,"value"],obs_interpolation_method,maxgap=max_interpolate_gap,na.rm=FALSE),digits=2)
          interpolated_data <- na.omit(interpolated_data_hourly)
          rm(interpolated_data_hourly)
        }
      }
      # store interpolated/non-interpolated data to output data frame
      added_data <- interpolated_data[,c(1,1,2,2)]
      names(added_data) <- names(CLDB_observation_data)
      added_data[,1] <- unique_datas[unique_data,"station_id"]
      added_data[,3] <- unique_datas[unique_data,"parameter"]
      CLDB_observation_data_interpolated <- rbind(CLDB_observation_data_interpolated,added_data)
      rm(added_data)
      rm(interpolated_data)
    }
    rm(unique_data)

    # Removing obsolete variables
    rm(not_interpolated_variables)
    rm(unique_datas)
    rm(interpolated_datas)
    rm(obs_per_day)
    rm(CLDB_observation_data)
    # Renaming Finnish data parameter to measurand_id
    if (CLDB_table=="observation_data_v1") {
      names(CLDB_observation_data_interpolated) <- c("station_id","obstime","measurand_id","value")
    }
    return(CLDB_observation_data_interpolated)
  }

  
  
  
  
  # Define empty list where data from individual databases is stored and returned at the end of the function
  all_retrieved_obs_data <- vector("list",2)
  names(all_retrieved_obs_data) <- c("weather_data_qc","observation_data_v1")

  CLDB_observation_data_v1 <- all_variable_lists[["CLDB_observation_data_v1"]]
  CLDB_weather_data_qc_foreign <- all_variable_lists[["CLDB_weather_data_qc_foreign"]]
  CLDB_weather_data_qc_roadweather <- all_variable_lists[["CLDB_weather_data_qc_roadweather"]]
  CLDB_mapping_parameters_all <- all_variable_lists[["mapping_parameters_all"]]
  derived_variables_all <- all_variable_lists[["derived_variables_all"]]
  first_date <- timestamps_series[1]
  last_date <- timestamps_series[tail(which((timestamps_series < Sys.time())==TRUE),1)]

  # SORTING RETRIEVED STATIONS TO FOREIGN/FINNISH
  if (sum(station_list_retrieved == "all_stations")>0) {
    weather_data_qc_stations <- station_list_retrieved
    observation_data_v1_stations <- station_list_retrieved
  } else {
    weather_data_qc_stations <- station_list_retrieved[station_list_retrieved<2700 | station_list_retrieved>3000]
    observation_data_v1_stations <- subset(station_list_retrieved,station_list_retrieved %!in% weather_data_qc_stations)
  }
  

  # Q <- dbGetQuery(con3,"select lpnn,dayx,tday,rrday from daily_qc where lpnn=301 order by lpnn,dayx")

  # WEATHER_DATA_QC
  # This view has foreign observations but also some finnish ones. Only retrieve foreign data. In view WEATHER_QC (and WEATHER_DATA_QC) flags >=6 are not shown in retrievals. So there's no need to use conditional retrieval of the observations based on "suitable" flags.
  if ((sum(!is.na(match(retrieved_tables,c("weather_data_qc","both"))))>0) & (length(weather_data_qc_stations)>0)) {
    # These station numbers are retrieved (generating string which is then incorporated to sql_query)
    if (sum(weather_data_qc_stations != "all_stations")>0) {
      retrieved_stations <- subset(station_idt_conversion, wmon %in% weather_data_qc_stations)
      fmisids <- paste(retrieved_stations[["fmisid"]],collapse=",")
      fmisids <- paste0("fmisid in (",fmisids,") and ")
    } else {
      # Include all stations for later inspection, only remove those duplicate fmisid numbers. With the expection of stations 2879 and 2932, all duplicate wmon numbers are those Finnish ones with 5xxx number, so always remove the later one (the duplicate)
      retrieved_stations <- station_idt_conversion
      if (sum(retrieved_stations$wmon %in% c(2148,2172))>0) {
        retrieved_stations <- retrieved_stations[-which(retrieved_stations$wmon %in% c(2148,2172)),]
      }
      while (sum(duplicated(retrieved_stations$fmisid))>0) {
        retrieved_stations <- retrieved_stations[-which(duplicated(retrieved_stations$fmisid)),]
      }
      fmisids <- paste0("")
    }
    

    # For retrieved_vars with table_name %in% c("weather_data_qc","both"), db specific variable names are taken from rownames(CLDB_mapping_parameters_all). Also retrieve variables needed for the calculation of derived variables (pick by hand here those that cannot be derived from one single variable [have no number in derived_variables_all[["CLDB_weather_data_qc"]]])
    retrieved_vars_weather_data_qc <- as.character(na.omit(CLDB_mapping_parameters_all[match(subset(retrieved_vars,table_name %in% c("weather_data_qc","both"))[["variable_name"]],rownames(CLDB_mapping_parameters_all)),"CLDB_weather_data_qc"]))
    retrieved_vars_weather_data_qc <- c(retrieved_vars_weather_data_qc,na.omit(derived_variables_all[match(subset(retrieved_vars,table_name %in% c("weather_data_qc","both"))[["variable_name"]],rownames(derived_variables_all)),"CLDB_weather_data_qc"]))
    retrieved_vars_weather_data_qc <- paste(retrieved_vars_weather_data_qc,collapse="','")

    # Direct retrievals from CLDB (Oracle)

    # Older retrievals from MOS db (PostgreSQL)
    # This query groups results based on even-hour -rounded obstime (even-hour observation is average of all values which are rounded to that even-hour)
    sql_query <- paste0("select fmisid as station_id, ((date_trunc('day',obstime) + interval '1 hour' * round( (date_part('hour',obstime) + date_part('minute',obstime)/60) / 1)) || ' UTC') as nearest_1, parameter, round(avg(value)::DECIMAL,1) as avg_value from weather_data_qc where ",fmisids,"parameter in ('",retrieved_vars_weather_data_qc,"') and obstime>='",first_date,"' and obstime<='",last_date,"' group by fmisid,parameter,nearest_1 order by fmisid,parameter,nearest_1;")
    # # This query rounds obstime to nearest hour and calculates difference of obstime to it
    # max_diff_in_minutes <- 30
    # sql_query <- paste0("select (obstime || ' UTC') as obstime, ((date_trunc('day',obstime) + interval '1 hour' * round( (date_part('hour',obstime) + date_part('minute',obstime)/60) / 1)) || 'UTC') as nearest_1, (@extract(epoch from(obstime - (date_trunc('day',obstime) + interval '1 hour' * round( (date_part('hour',obstime) + date_part('minute',obstime)/60) / 1))))/60) as abs_diff_in_minutes, value from weather_data_qc where fmisid in (",fmisids,") and parameter in ('",retrieved_vars_weather_data_qc,"') and obstime>='",first_date,"' and obstime<='",last_date,"' and (@extract(epoch from(obstime - (date_trunc('day',obstime) + interval '1 hour' * round( (date_part('hour',obstime) + date_part('minute',obstime)/60) / 1))))/60) < ",max_diff_in_minutes," order by obstime, nearest_1, abs_diff_in_minutes;")
    # # The oldest query, fetching only the data which has 3-hourly hour component to it
    # sql_query <- paste0("select (obstime || ' UTC') as obstime, value from weather_data_qc where fmisid in (",fmisids,") and parameter in ('",retrieved_vars_weather_data_qc,"') and obstime>='",first_date,"' and obstime<='",last_date,"' and (extract(hour from obstime) :: bigint) in (0,3,6,9,12,15,18,21) order by obstime;")
    # # This fetches all data
    # sql_query <- paste0("select (obstime || ' UTC') as obstime, value from weather_data_qc where fmisid in (",fmisids,") and parameter in ('",retrieved_vars_weather_data_qc,"') and obstime>='",first_date,"' and obstime<='",last_date,"' order by obstime;")
    # retrieved_data <- ROracle::dbGetQuery(con3, sql_query)
    retrieved_data <- RPostgreSQL::dbGetQuery(con1, sql_query)
    rm(sql_query)

    # Tidy up + interpolate data and assign to list returned at the end of the function
    if (!length(retrieved_data)==FALSE) {
      retrieved_data <- tidy_and_interpolate_CLDB_data(retrieved_data,"weather_data_qc")
      all_retrieved_obs_data[["weather_data_qc"]] <- retrieved_data
    }
    rm(retrieved_data)

    rm(retrieved_stations)
    rm(fmisids)
    rm(retrieved_vars_weather_data_qc)
  }



  # FINNISH DATA (observation_data_v1)
  if (sum(!is.na(match(retrieved_tables,c("observation_data_v1","both"))))>0 & (length(observation_data_v1_stations)>0)) {
    # These station numbers are retrieved (generating string which is then incorporated to sql_query)
    if (sum(observation_data_v1_stations != "all_stations")>0) {
      retrieved_stations <- subset(station_idt_conversion, wmon %in% observation_data_v1_stations)
      fmisids <- paste(retrieved_stations[["fmisid"]],collapse=",")
      fmisids <- paste0("station_id in (",fmisids,") and ")
    } else {
      # Include all stations for later inspection, only remove those duplicate fmisid numbers. With the expection of stations 2879 and 2932, all duplicate wmon numbers are those Finnish ones with 5xxx number, so always remove the later one (the duplicate)
      retrieved_stations <- station_idt_conversion
      if (sum(retrieved_stations$wmon %in% c(2148,2172))>0) {
        retrieved_stations <- retrieved_stations[-which(retrieved_stations$wmon %in% c(2148,2172)),]
      }
      while (sum(duplicated(retrieved_stations$fmisid))>0) {
        retrieved_stations <- retrieved_stations[-which(duplicated(retrieved_stations$fmisid)),]
      }
      fmisids <- paste0("")
    }
    
    

    # Defining retrieved variable numbers using database/table-specific -names
    # If data is retrieved from both tables, use variable numbers mapped from CLDB_mapping_parameters_all[["CLDB_observation_data_v1"]]
    # For derived variables derived from one single variable, take parameter numbers from derived_variables_all[["CLDB_observation_data_v1"]]
    # If using observation_data_v1, retrieved_vars has db-specific variable numbers. Retrieving those which match CLDB_observation_data_v1[["measurand_id"]]
    retrieved_vars_observation_data_v1 <- as.character(na.omit(CLDB_mapping_parameters_all[match(subset(retrieved_vars,table_name %in% c("both"))[["variable_name"]],rownames(CLDB_mapping_parameters_all)),"CLDB_observation_data_v1"]))
    retrieved_vars_observation_data_v1 <- c(retrieved_vars_observation_data_v1,na.omit(derived_variables_all[match(subset(retrieved_vars,table_name %in% c("observation_data_v1","both"))[["variable_name"]],rownames(derived_variables_all)),"CLDB_observation_data_v1"]))
    retrieved_vars_observation_data_v1 <- c(retrieved_vars_observation_data_v1,subset(retrieved_vars,table_name %in% c("observation_data_v1") & variable_name %in% CLDB_observation_data_v1[["measurand_id"]])[["variable_name"]])

  #       # Brainstorm is not used anymore for Finnish stations as the retrieval times are very comparable to those of table observation_data_v1
  #       # Brainstorm fmi -> weather_qc (CLDB). Includes both manual and auto-observations. These cannot be distinguished from each other in time series.
  #       com_string <- paste("temp1 <- as.character(scrape(url=\"http://brainstormgw.fmi.fi/observe?param=utctime,tz,",sel_obsplugin,"&format=ascii&timeformat=sql&separator=,&tz=UTC&stationtype=fmi&starttime=",format(timestamps_series[1],'%Y%m%d%H%M'),"&endtime=",format(timestamps_series[length(timestamps_series)],'%Y%m%d%H%M'),"&timestep=180&fmisid=",fmisid,"\",parse=FALSE))",sep="")
  #       eval(parse(text=com_string))
  #       rm(com_string)
  #       if (temp1!="") {
  #         retrieved_data <- temp1
  #         retrieved_data <- unlist(strsplit(retrieved_data,"\n"))
  #         retrieved_data <- unlist(strsplit(retrieved_data,","))
  #         retrieved_data <- as.data.frame(matrix(retrieved_data,nrow=(length(retrieved_data)/3),byrow=T))
  #         retrieved_data[,1] <- paste(retrieved_data[,1],retrieved_data[,2])
  #         retrieved_data <- retrieved_data[,-2]
  #         names(retrieved_data) <- c("obstime","value")
  #       }
  #       rm(temp1)

    # observation_data_v1 (CLDB)
    retrieved_hours1 <- paste(sort((unique(as.numeric(format(timestamps_series,"%H")))+24)%%24),collapse=",")
    retrieved_hours2 <- paste(sort((unique(as.numeric(format(timestamps_series,"%H")))+24-1)%%24),collapse=",")
    # If the variable of interest is instantaneous temperature
    if ("1" %in% retrieved_vars_observation_data_v1) {
      # Fetching observations done both at sharp and 20 to observation hours. Some manual stations report 20to observations 19to.
      sql_query <- paste0("select station_id, (data_time || ' UTC') as obstime, measurand_id, round(AVG(CASE WHEN measurand_id in (1) THEN data_value END)::DECIMAL,1) AS value from observation_data_v1 where ",fmisids,"measurand_id in ('1') and measurand_no=1 and data_time>='",first_date,"' and data_time<='",last_date,"' and ((extract(hour from data_time) :: bigint) in (",retrieved_hours1,") and (extract(minute from data_time) :: bigint) in (0) or (extract(hour from data_time) :: bigint) in (",retrieved_hours2,") and (extract(minute from data_time) :: bigint) in (40,41)) group by station_id,measurand_id,obstime order by station_id,measurand_id,obstime;")
      # # This query below is the old one. It returns the same data as the above query.
      # sql_query <- paste0("retrieved_data_Finnish <- dbSendQuery(con1, \"select (data_time || ' UTC') as obstime, data_value as value from observation_data_v1 where station_id in (",station_idt_conversion$fmisid[which(station_idt_conversion$wmon==previ_ecmos_v_station_id[i])],") and measurand_id in (",sel_CLDB,") and measurand_no=1 and data_time>='",first_date,"' and data_time<='",last_date,"' and ((extract(hour from data_time) :: bigint) in (",retrieved_hours1,") and (extract(minute from data_time) :: bigint) in (0) or (extract(hour from data_time) :: bigint) in (",retrieved_hours2,") and (extract(minute from data_time) :: bigint) in (40,41)) order by obstime, data_value;\")",sep="")
      retrieved_data <- dbGetQuery(con1, sql_query)
      rm(sql_query)
      # Tidy up data and assign to list returned at the end of the function
      if (!length(retrieved_data)==FALSE) {
        retrieved_data <- tidy_and_interpolate_CLDB_data(retrieved_data,"observation_data_v1")
        all_retrieved_obs_data[["observation_data_v1"]] <- rbind(all_retrieved_obs_data[["observation_data_v1"]],retrieved_data)
      }
      rm(retrieved_data)
    }
    # If variable list contains variables which cannot be interpolated (is not observed for every hour)
    if (sum(!is.na(match(as.numeric(retrieved_vars_observation_data_v1),all_variable_lists[["CLDB_observation_data_v1_not_interpolated"]])))>0) {
      retrieved_variables <- subset(retrieved_vars_observation_data_v1,as.numeric(retrieved_vars_observation_data_v1) %in% all_variable_lists[["CLDB_observation_data_v1_not_interpolated"]])
      retrieved_variables <- paste(retrieved_variables,collapse="','")
      # This query groups values for every hour by taking the average of all values of that specific hour
      sql_query <- paste0("select station_id, (data_time || ' UTC') as obstime, measurand_id, round(AVG(CASE WHEN measurand_id in ('",retrieved_variables,"') THEN data_value END)::DECIMAL,1) AS value from observation_data_v1 where ",fmisids,"measurand_id in ('",retrieved_variables,"') and measurand_no=1 and data_time>='",first_date,"' and data_time<='",last_date,"' and (extract(minute from data_time) :: bigint) in (0) group by station_id,measurand_id,obstime order by station_id,measurand_id,obstime;")
      rm(retrieved_variables)
      retrieved_data <- dbGetQuery(con1, sql_query)
      rm(sql_query)
      # Tidy up data and assign to list returned at the end of the function
      if (!length(retrieved_data)==FALSE) {
        retrieved_data <- tidy_and_interpolate_CLDB_data(retrieved_data,"observation_data_v1")
        all_retrieved_obs_data[["observation_data_v1"]] <- rbind(all_retrieved_obs_data[["observation_data_v1"]],retrieved_data)
      }
      rm(retrieved_data)
    }
    # If the variable of interest can be interpolated and is not instantaneous temperature
    if (sum(is.na(match(as.numeric(subset(retrieved_vars_observation_data_v1,as.numeric(retrieved_vars_observation_data_v1)!=1)),all_variable_lists[["CLDB_observation_data_v1_not_interpolated"]])))>0) {
      retrieved_variables <- subset(retrieved_vars_observation_data_v1,as.numeric(retrieved_vars_observation_data_v1) %!in% all_variable_lists[["CLDB_observation_data_v1_not_interpolated"]] & as.numeric(retrieved_vars_observation_data_v1)!=1)
      retrieved_variables <- paste(retrieved_variables,collapse="','")

      # This query groups values for every hour by taking the average of all values of that specific hour
      sql_query <- paste0("select station_id, (data_time || ' UTC') as obstime, measurand_id, round(AVG(CASE WHEN measurand_id in ('",retrieved_variables,"') THEN data_value END)::DECIMAL,1) AS value from observation_data_v1 where ",fmisids,"measurand_id in ('",retrieved_variables,"') and measurand_no=1 and data_time>='",first_date,"' and data_time<='",last_date,"' and (extract(minute from data_time) :: bigint) in (0) group by station_id,measurand_id,obstime order by station_id,measurand_id,obstime;")
      # # This query fetches the minimum value of the instantaneous values of the previous hour (these do not capture actual observed minimums as instantaneous values are sampled only every 10mins in the database) So: DON'T USE THIS!
      # sql_query <- paste("retrieved_data_Finnish <- dbSendQuery(con1, \"select ((date_trunc('day',data_time) + interval '1 hour' * ceil( (date_part('hour',data_time) + date_part('minute',data_time)/60) / 1)) || ' UTC') as obstime, round(min(data_value)::DECIMAL,1) AS value from observation_data_v1 where station_id in (",station_idt_conversion$fmisid[which(station_idt_conversion$wmon==previ_ecmos_v_station_id[i])],") and measurand_id in (1) and measurand_no=1 and data_time>='",first_date,"' and data_time<='",last_date,"' group by obstime order by obstime;\")",sep="")
      # # This query fetches the minimum value of the minimum hourly values of the same day (1801UTC ... 1800UTC). This NEATLY corresponds to daily min values in MOST cases. So: USE THIS IF YOU WANT!
      # sql_query <- paste("retrieved_data_Finnish <- dbSendQuery(con1, \"select ((date_trunc('day',data_time) + interval '1 hour' * ceil( (date_part('hour',data_time) + date_part('minute',data_time)/60 - 18.0166) / 24)*24) || ' UTC') as obstime, round(min(data_value)::DECIMAL,1) AS value from observation_data_v1 where station_id in (",station_idt_conversion$fmisid[which(station_idt_conversion$wmon==previ_ecmos_v_station_id[i])],") and measurand_id in (",sel_CLDB,") and measurand_no=1 and data_time>='",first_date,"' and data_time<='",last_date,"' group by obstime order by obstime;\")",sep="")
      # # This query fetches the maximum value of the instantaneous values of the previous hour (these do not capture actual observed maximums as instantaneous values are sampled only every 10mins in the database) So: DON'T USE THIS!
      # sql_query <- paste("retrieved_data_Finnish <- dbSendQuery(con1, \"select ((date_trunc('day',data_time) + interval '1 hour' * ceil( (date_part('hour',data_time) + date_part('minute',data_time)/60) / 1)) || ' UTC') as obstime, round(max(data_value)::DECIMAL,1) AS value from observation_data_v1 where station_id in (",station_idt_conversion$fmisid[which(station_idt_conversion$wmon==previ_ecmos_v_station_id[i])],") and measurand_id in (1) and measurand_no=1 and data_time>='",first_date,"' and data_time<='",last_date,"' group by obstime order by obstime;\")",sep="")
      # # This query fetches the maximum value of the maximum hourly values of the same day (1801UTC ... 1800UTC). This NEATLY corresponds to daily max values in MOST cases. So: USE THIS IF YOU WANT!
      # sql_query <- paste("retrieved_data_Finnish <- dbSendQuery(con1, \"select ((date_trunc('day',data_time) + interval '1 hour' * ceil( (date_part('hour',data_time) + date_part('minute',data_time)/60 - 18.0166) / 24)*24) || ' UTC') as obstime, round(max(data_value)::DECIMAL,1) AS value from observation_data_v1 where station_id in (",station_idt_conversion$fmisid[which(station_idt_conversion$wmon==previ_ecmos_v_station_id[i])],") and measurand_id in (",sel_CLDB,") and measurand_no=1 and data_time>='",first_date,"' and data_time<='",last_date,"' group by obstime order by obstime;\")",sep="")
      rm(retrieved_variables)
      retrieved_data <- dbGetQuery(con1, sql_query)
      rm(sql_query)
      # Tidy up data and assign to list returned at the end of the function
      if (!length(retrieved_data)==FALSE) {
        retrieved_data <- tidy_and_interpolate_CLDB_data(retrieved_data,"observation_data_v1")
        all_retrieved_obs_data[["observation_data_v1"]] <- rbind(all_retrieved_obs_data[["observation_data_v1"]],retrieved_data)
      }
      rm(retrieved_data)
    }
    rm(retrieved_hours1)
    rm(retrieved_hours2)

    rm(retrieved_stations)
    rm(fmisids)
    rm(retrieved_vars_observation_data_v1)
  }








  # SIMPLE VARIABLE CONVERSIONS
  # Converting temperature to Kelvins
  if (!is.null(all_retrieved_obs_data[["weather_data_qc"]])) {
    all_retrieved_obs_data[["weather_data_qc"]][(which(as.matrix(all_retrieved_obs_data[["weather_data_qc"]]["parameter"]) %in% CLDB_mapping_parameters_all[["CLDB_weather_data_qc"]][CLDB_mapping_parameters_all[["unit"]] == "°C"])),"value"] <- all_retrieved_obs_data[["weather_data_qc"]][(which(as.matrix(all_retrieved_obs_data[["weather_data_qc"]]["parameter"]) %in% CLDB_mapping_parameters_all[["CLDB_weather_data_qc"]][CLDB_mapping_parameters_all[["unit"]] == "°C"])),"value"] + 273.15
  }
  if (!is.null(all_retrieved_obs_data[["observation_data_v1"]])) {
    all_retrieved_obs_data[["observation_data_v1"]][(which(as.matrix(all_retrieved_obs_data[["observation_data_v1"]]["measurand_id"]) %in% CLDB_mapping_parameters_all[["CLDB_observation_data_v1"]][CLDB_mapping_parameters_all[["unit"]] == "°C"])),"value"] <- all_retrieved_obs_data[["observation_data_v1"]][(which(as.matrix(all_retrieved_obs_data[["observation_data_v1"]]["measurand_id"]) %in% CLDB_mapping_parameters_all[["CLDB_observation_data_v1"]][CLDB_mapping_parameters_all[["unit"]] == "°C"])),"value"] + 273.15
  }




  # CALCULATING DERIVED VARIABLES
  # not done at the moment...



  # Changing level_value from integer to character
  if (!is.null(all_retrieved_obs_data[["weather_data_qc"]])) {
    all_retrieved_obs_data$weather_data_qc$parameter <- as.character(all_retrieved_obs_data$weather_data_qc$parameter)
  }
  if (!is.null(all_retrieved_obs_data[["observation_data_v1"]])) {
    all_retrieved_obs_data$observation_data_v1$measurand_id <- as.character(all_retrieved_obs_data$observation_data_v1$measurand_id)
  }
  invisible(all_retrieved_obs_data)
}