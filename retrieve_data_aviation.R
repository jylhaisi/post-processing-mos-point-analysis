retrieve_data_aviation <- function(variable_list,station_list_retrieved,timestamps_series) {
  # # THIS PART IS ONLY FOR RETRIEVING AVIATION DATA...
  # retrieved_data <- as.data.frame(matrix(NA,0,0))
  # # Haetaan Teron AQU-tietokannasta METAR-havaintoja lentokentille ja tallennetaan tämä tieto muuttujaan retrieved_data
  # # HUOM! MessagetypeId=1 ottaa huomioon vain manuaaliset METARIT, auto-METARIt ovat arvolla 8!
  # sql_query <- paste("temp1 <- as.character(scrape(url=\"http://lentosaa.fmi.fi/nordictafverif/tafverif/aqu.php?stationId=",station_id,"&messageTypeId=1&startTime=",format(timestamps_series[1],'%Y%m%d%H%M'),"&endTime=",format(timestamps_series[length(timestamps_series)],'%Y%m%d%H%M'),"&paramId=",sel_AQU,"&timeStep=180&missingValue=NaN\",parse=FALSE))",sep="")
  # eval(parse(text=sql_query))
  # rm(sql_query)
  # if (temp1!="\n") {
  #   retrieved_data <- temp1
  #   retrieved_data <- unlist(strsplit(retrieved_data,"\n"))
  #   retrieved_data <- unlist(strsplit(retrieved_data,";"))
  #   retrieved_data <- as.data.frame(matrix(retrieved_data,nrow=(length(retrieved_data)/2),byrow=T))
  #   retrieved_data[,1] <- paste(retrieved_data[,1]," UTC")
  #   names(retrieved_data) <- c("obstime","value")
  #   if (!length(which(retrieved_data$value=="nan"))==FALSE) {
  #     retrieved_data <- retrieved_data[-which(retrieved_data$value=="nan"),]
  #   }
  #   if (!length(which(retrieved_data$value=="NaN"))==FALSE) {
  #     retrieved_data <- retrieved_data[-which(retrieved_data$value=="NaN"),]
  #   }
  # }
  # rm(temp1)
  
  
}