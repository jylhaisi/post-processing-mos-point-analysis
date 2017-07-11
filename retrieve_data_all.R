retrieve_data_all <- function(variable_list,station_list_retrieved,timestamps_series) {
  
  # Define empty list where data from individual databases is stored and returned at the end of the function
  all_retrieved_data <- vector("list",4)
  names(all_retrieved_data) <- c("MOS","verif","CLDB","aviation")
  
  # These unique data sources are in predictor list
  data_sources <- as.list(intersect(c("MOS","verif","CLDB","aviation"),unique(variable_list[["db"]])))
  
  if (length(data_sources)==0 | sum(is.numeric(station_list_retrieved))==0 | length(timestamps_series)==0) {
    stop("check arguments, no data to be fetched!")
  }
  
  # Retrieving data from all different sources using db-specific retrieval scripts
  for (data_source in seq(data_sources)) {
    eval(parse(text=paste0("all_retrieved_data[[\"",data_sources[data_source],"\"]] <- retrieve_data_",data_sources[data_source],"(variable_list,station_list_retrieved,timestamps_series)")))
  }

  # # Data from several sources can be retrieved using something like below...
  # if (!length(data_sources)==FALSE) {
  #   retrievals <- paste0("retrieve_data_",data_sources,"(variable_list,station_list_retrieved,timestamps_series)")
  #   assigns <- paste0("retrieved_data_",data_sources)
  #   equation <- paste(paste(assigns, retrievals, sep=" <- "), collapse=";")
  #   print(equation)
  #   eval(parse(text=equation))
  #   rm(list = c("retrievals","assigns","equation"))
  #   all_retrieved_data <- eval(parse(text=paste0("list(",paste(assigns,collapse = ","),")")))
  #   names(all_retrieved_data) <- data_sources
  # }
  # rm(data_sources)

  
  print("Terve!")
  invisible(all_retrieved_data)
}
