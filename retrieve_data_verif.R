retrieve_data_verif <- function(variable_list,station_list_retrieved,timestamps_series) {
  # This function retrieves data from verif db
  
  # Checking if arguments are correct
  retrieved_vars <- subset(variable_list,db=="verif")
  if (dim(retrieved_vars)[1] == 0) {
    stop("no verif vars in variable list!")
  }
  retrieved_producers <- intersect(all_verif_lists[["producers"]][,"id"],unique(retrieved_vars[["table_name"]]))
  if (length(retrieved_producers) == 0) {
    stop("check table names!")
  }
  rm(retrieved_producers)
  
  first_date <- timestamps_series[1]
  last_date <- tail(timestamps_series,1)
  retrieved_stations <- subset(station_idt_conversion, wmon %in% station_list_retrieved)
  
  
  # Define empty list where data from individual databases is stored and returned at the end of the function
  all_retrieved_verif_data <- vector("list",1)
  names(all_retrieved_verif_data) <- c("verif")
  for (l2 in 1:dim(retrieved_stations)[1]) {
    # "normal" assumes Finnish stations (2700 <= wmon <= 3000) belonging to stationgroup=1, and all other to stationgroup=9.
    if (verif_stationtype=="normal") {
      if ((retrieved_stations[l2,"wmon"]<2700) | (retrieved_stations[l2,"wmon"]>=3000)) {
        stationtype <- 9
      } else {
        stationtype <- 1
      }
    } else {
      stationtype <- verif_stationtype
    }
    # Foreign stations are sorted in the db according to fmisid numbers, instead of wmon numbers
    if (stationtype==9) {
      stationnumber <- subset(station_idt_conversion, wmon %in% station_list_retrieved[l2])[["fmisid"]]
    } else {
      stationnumber <- subset(station_idt_conversion, wmon %in% station_list_retrieved[l2])[["wmon"]]
    }
    for (l1 in 1:dim(retrieved_vars)[1]) {
      sql_query <- paste0("select count (relname) as a from pg_class where relname like('",retrieved_vars[l1,"table_name"],"_",stationnumber,"_",stationtype,"');")
      is_data <- RPostgreSQL::dbGetQuery(con2, sql_query)
      rm(sql_query)
      if (is_data$a[1]>0) {
        ### RETRIEVING DATA
        sql_query <- paste0("select (date || ' UTC') as date, atime, fdate, ROUND(((EXTRACT(EPOCH FROM fdate AT TIME ZONE 'UTC') - 3600 * atime)::INTEGER % 86400) / 3600.0) as leadtime_add, leadtime, paramvalue from ",retrieved_vars$table_name[l1],"_",stationnumber,"_",stationtype," where date>='",format(first_date,'%Y-%m-%d %H:%M:%OS'),"' and date <='",format(last_date,'%Y-%m-%d %H:%M:%OS'),"' and paramid='",retrieved_vars$variable_name[l1],"' order by date;")
        retrieved_data <- RPostgreSQL::dbGetQuery(con2, sql_query)
        rm(sql_query)
        if ((dim(retrieved_data)[1]>0)==FALSE) {
          rm(retrieved_data)
          next
        }
        
        
        ### TIDYING UP DATA
        # Changing date to POSIXct type
        retrieved_data$date <- as.POSIXct(as.character(retrieved_data$date),tz="GMT")
        # leadtime_add is the difference between analysis_time and sending/receiving_time abs(fdate-atime). This must be added to forecast lead time.
        retrieved_data$leadtime <- retrieved_data$leadtime + retrieved_data$leadtime_add
        # Removing column leadtime_add
        retrieved_data <- retrieved_data[,-which(names(retrieved_data)=="leadtime_add")]
        # For edited data ("pal"), changing forecast lead time so that it is comparable with the models:
        # Selecting main analysis times as c(00,06,12,18). Changing all atimes to first following analysis time within the five-hour window (e.g. atime 02 is changed to 06). Forecast lead times are shortened equal amount to what atimes are moved forward.
        if (retrieved_vars[l1,"table_name"]=="pal") {
          retrieved_data$leadtime[which(retrieved_data$atime %in% c(19,20,21,22,23))] <- retrieved_data$leadtime[which(retrieved_data$atime %in% c(19,20,21,22,23))] - abs(retrieved_data$atime[which(retrieved_data$atime %in% c(19,20,21,22,23))]-24)
          retrieved_data$leadtime[which(retrieved_data$atime %in% c(1,2,3,4,5))] <- retrieved_data$leadtime[which(retrieved_data$atime %in% c(1,2,3,4,5))] - abs(retrieved_data$atime[which(retrieved_data$atime %in% c(1,2,3,4,5))]-6)
          retrieved_data$leadtime[which(retrieved_data$atime %in% c(7,8,9,10,11))] <- retrieved_data$leadtime[which(retrieved_data$atime %in% c(7,8,9,10,11))] - abs(retrieved_data$atime[which(retrieved_data$atime %in% c(7,8,9,10,11))]-12)
          retrieved_data$leadtime[which(retrieved_data$atime %in% c(13,14,15,16,17))] <- retrieved_data$leadtime[which(retrieved_data$atime %in% c(13,14,15,16,17))] - abs(retrieved_data$atime[which(retrieved_data$atime %in% c(13,14,15,16,17))]-18)
          retrieved_data$atime[which(retrieved_data$atime %in% c(19,20,21,22,23))] <- 0
          retrieved_data$atime[which(retrieved_data$atime %in% c(1,2,3,4,5))] <- 6
          retrieved_data$atime[which(retrieved_data$atime %in% c(7,8,9,10,11))] <- 12
          retrieved_data$atime[which(retrieved_data$atime %in% c(13,14,15,16,17))] <- 18
        }
        # ROUND((... -sql retrieval rounds leadtime_add -values upwards whereas they should be rounded downwards (fdate-atime -difference in these cases is systematically one hour greater). In these cases one hour is subtracted from forecast lead time, so that all forecast lead times (all else than <24h forecasts which are done for every hour) correspond to lead times used in edited data. 
        retrieved_data$leadtime[which(as.numeric(format(retrieved_data$fdate,"%M"))==30)] <- (retrieved_data$leadtime[which(as.numeric(format(retrieved_data$fdate,"%M"))==30)] - 1)
        
        
        
        # Changing column names so that data looks like MOS data [analysis_time, forecast_time, forecast_period, param_id, value]
        retrieved_data <- retrieved_data[,c(2,1,4,3,5)]
        names(retrieved_data) <- c("analysis_time","forecast_time","forecast_period","fdate","value")
        retrieved_data[,"analysis_time"] <- (retrieved_data$forecast_time - retrieved_data$forecast_period*3600)
        # Ordering
        retrieved_data <- retrieved_data[order(retrieved_data$analysis_time,retrieved_data$forecast_time,retrieved_data$forecast_period,retrieved_data$fdate),]
        # Removing duplicate values. Preserving the latest forecast (the one at the bottom)
        is_duplicated <- which(duplicated(retrieved_data[,c("analysis_time","forecast_time","forecast_period")]))
        while (length(is_duplicated)>0) {
          retrieved_data <- retrieved_data[-(is_duplicated-1),]
          is_duplicated <- which(duplicated(retrieved_data[,c("analysis_time","forecast_time","forecast_period")]))
        }
        rm(is_duplicated)
        # Removing negative forecast lengths
        negative_lead_times <- which(retrieved_data$forecast_period<0)
        if (length(negative_lead_times)>0) {
          retrieved_data <- retrieved_data[-negative_lead_times,]
        }
        rm(negative_lead_times)
        # Changing forecast time zone to GMT
        retrieved_data$forecast_time <- as.POSIXct(as.character(retrieved_data$forecast_time),tz="GMT")
        # Rounding to nearest hour
        retrieved_data$forecast_time <- as.POSIXct(round.POSIXt(retrieved_data$forecast_time, c("hours")))
        # Changing to numeric if not yet
        retrieved_data$value <- as.numeric(as.character(retrieved_data$value))
        
        # Changing column fdate name and content to id
        names(retrieved_data)[which(names(retrieved_data)=="fdate")] <- "id"
        retrieved_data[,which(names(retrieved_data)=="id")] <- as.numeric(retrieved_vars[l1,"variable_name"])
        # Add table_name (model) as the first column and station_id as the first column
        retrieved_data <- retrieved_data[,c(1,1,seq(1,dim(retrieved_data)[2]))]
        names(retrieved_data)[1:3] <- c("model","station_id","analysis_time")
        retrieved_data[,"model"] <- retrieved_vars[l1,"table_name"]
        retrieved_data[,"station_id"] <- station_list_retrieved[l2]
        
        ### SIMPLE VARIABLE CONVERSIONS
        # Changing all "°C" -unit variables to Kelvin
        if (all_verif_lists$parameters[match(retrieved_vars[l1,"variable_name"],all_verif_lists$parameters[,"id"]),"unit"]=="°C") {
          retrieved_data$value <- retrieved_data$value + 273.15
        }
        
        
        all_retrieved_verif_data <- rbind(all_retrieved_verif_data,retrieved_data)
        rm(retrieved_data)
      }
      rm(is_data)
    }
    rm(l1)
    rm(stationtype)
    rm(stationnumber)
  }
  rm(l2)
  
  rm(retrieved_vars)
  rm(first_date)
  rm(last_date)
  rm(retrieved_stations)
  
  # Changing level_value from integer to character
  if (!is.null(all_retrieved_verif_data)) {
    all_retrieved_verif_data$forecast_period <- as.character(all_retrieved_verif_data$forecast_period)
    all_retrieved_verif_data$id <- as.character(all_retrieved_verif_data$id)
  }
  
  invisible(all_retrieved_verif_data)
}