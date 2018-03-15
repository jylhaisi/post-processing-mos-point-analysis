# This script initializes interfaces to various databases, can be used to read in data and in MOS training/analysis.
rm(list=ls())
# Reading in the required packages, mapping lists and functions
source("load_libraries_tables_and_open_connections.R")

# WHEN MAKING THIS SCRIPT INTO A FUNCTION, MAKE POSSIBLE TO GIVE STATION NUMBERS OR STATION LISTS AS ARGUMENTS
# DERIVED VARIABLES: IS ONLY SUPPORTED CURRENTLY FOR MOS VARIABLES. CLDB/VERIF VARIABLES CANNOT BE DERIVED ATM. DERIVED VARIABLES WITH 8-DIGIT PARAMETER NUMBER (E.G. ASTRONOMICAL VARIABLES) ARE FORMED LATER IN EXECUTION WHEN FORMING MODEL-OBSPAIRS AND ARE NOT STORED IN MOS DATA FRAMES!
# KNOWN ISSUES: TAMINDAILY -RETRIEVAL AND OTHER DERIVED VARS NEED TO BE CHANGED SO THAT DERIVED_VARIABLE NAME IS CHANGED INSTEAD OF DB-SPECIFIC NAME
# CREATE A FUNCTION THAT "MELTS" "observation_data_v1" and "weather_data_qc" FROM CLDB LIST INTO A COMMON DATA FRAME!


# User-defined variables
timestamps_series <- define_time_series(begin_date = "2010-10-14 00:00:00 GMT", end_date = "2017-10-20 21:00:00 GMT", interval_in_hours = 3)
modelobspairs_minimum_sample_size <- 100 # Arbitrary number here, could in principle also depend on the number of predictor variables
mos_label <- paste("MOS_ECMWF_250416")
predictor_set <- "only_bestvars2" #"allmodelvars_1prec_noBAD_RH2"
derived_variables <- c("T2_2","T2_0.5","RH_SURF","RH_SURF_2","DECLINATION","SOL_ANGLE")
station_list <- "mos_stations_homogeneous_Europe" # Possible pre-defined station lists are those names in all_station_lists. If you want to use an arbitrary station list, assign the station numbers manually to variable station_numbers
station_numbers <- eval(subs(all_station_lists[[station_list]])) # Retrievals are generated and data is returned based on station wmon-numbers. If using a station list outside mos station list, define the wmon-numbers here.
obs_interpolation_method <- "spline_interp" # options repeat_previous (na.locf),linear_interp (na.approx),spline_interp (na.spline),no_interp (leave NA values to timeseries as they are). Continuous observations are interpolated, those which not are sublists in all_variable_lists
max_interpolate_gap <- 6 # This indicates the maximum time in hours to which observation interpolation is applied
verif_stationtype <- "normal" # In verif db, several stationgroups exist. "normal" assumes stations (2700 <= wmon <= 3000) belonging to stationgroup=1, and all other to stationgroup=9 (other stationgroups outside stationgroup=3 only have a small number of stations to them). Road weather station support needs to be coded later (this needs a road weather station list), currently this can be done manually by putting the stationgroup of interest here manually (e.g. ==3)
output_dir <- "/data/daniel/statcal/R_projects/ld_playground/lr_model_training/test" 
max_variables <- 10
fitting.method  <- "GlmnR1"

# Defining used variable_lists
# First column indicates specific variable name in the database table indicated by the second column, third column is the database name.
# Varibles shortnames are database-specific, except for MOS db (it uses pre-defined variable set names and derived variables) and CLDB_both (foreign and finnish observations are both fetched and they have different variable names).
# For MOS db table MOS_trace_v, data can be fetched either from only the target_param_name (use table_name MOS_trace_v) or also from source_param_name and weight (use table_name MOS_trace_v_all).
# If you want to combine variables from several databases, run choose_variables several times with different parameters and combine the output.
variable_list_predictors_all <- variable_list_predictors <- choose_variables(c(predictor_set,derived_variables),"previ_ecmos_narrow_v","MOS")
variable_list_predictands_all <- variable_list_predictands <- choose_variables("estimated_variables","both","CLDB")
# variable_list_predictors_all <- variable_list_predictors <- rbind(choose_variables(c(predictor_set,derived_variables),"previ_ecmos_narrow_v","MOS"),choose_variables("1",c("ecmwf","pal","kalmanecmwf","hirlam"),"verif"),choose_variables("5",c("ecmwf","pal","kalmanecmwf","hirlam"),"verif"))
# variable_list_predictands_all <- variable_list_predictands <- rbind(choose_variables("estimated_variables","both","CLDB"),choose_variables(c("56","73"),"observation_data_v1","CLDB"),choose_variables(c("PSEA","WS","WD"),"weather_data_qc","CLDB"))





# ##### EXAMPLE FOR RETRIEVING ONLY TEMPERATURE FROM VERIF DB (DMO) + CLDB (OBS)
# # variable_list_retrieved <- rbind(choose_variables("TA","both","CLDB"),choose_variables("1",c("ecmwf","hirlam","gfs","meps","mosecmwf"),"verif"))
# variable_list_predictands_all <- variable_list_predictands <- choose_variables(c("56","73"),"observation_data_v1","CLDB")
# variable_list_retrieved <- rbind(choose_variables(c("270"),"observation_data_v1","CLDB"))
# station_list_retrieved <- 2974 #all_station_lists[["all_stations_realtime"]]
# function_arguments <- list(variable_list_retrieved,station_list_retrieved,timestamps_series)
# retrieved_data <- do.call(retrieve_data_all,function_arguments)
# 
# ##### EXAMPLE FOR RETRIEVING ONLY WIND FROM CLDB
# variable_list_retrieved <- rbind(choose_variables("TA","both","CLDB"),choose_variables(c("2","13"),"ecmwf","verif"))
# station_list_retrieved <- 2974 #all_station_lists[["all_stations_realtime"]]
# station_list_retrieved <- station_list_retrieved
# station_list_retrieved <- station_list_retrieved$station_id
# function_arguments <- list(variable_list_retrieved,station_list_retrieved,timestamps_series)
# retrieved_data <- do.call(retrieve_data_all,function_arguments)

# ##### EXAMPLE FOR RETRIEVING ONLY PRECIPITATION FROM CLDB
# variable_list_retrieved <- rbind(choose_variables(c("1","61","62","63","64","65","175","315","368"),"observation_data_v1","CLDB"))
# station_list_retrieved <- 2876 #all_station_lists[["all_stations_realtime"]]
# station_list_retrieved <- station_list_retrieved
# station_list_retrieved <- station_list_retrieved$station_id
# function_arguments <- list(variable_list_retrieved,station_list_retrieved,timestamps_series)
# retrieved_data <- do.call(retrieve_data_all,function_arguments)

# Defining running indices from lists
station_numbers_indices <- seq_len(length(station_numbers))
variable_indices <- seq_len(length(variable_list_predictands[["variable_name"]]))

for (station_number_index in station_numbers_indices) {
  
  
  
  
  # # DEFINE THIS RESULT PART LATER, INSIDE THE FUNCTION THAT PRODUCES ACTUAL MOS COEFFICIENTS TO A FILE
  # # This matrix contains metadata related to MOS training
  # station_numbers_data_status <- station_numbers
  # station_numbers_data_status <- cbind(station_numbers_data_status, as.data.frame(matrix(0,ncol=35,nrow=length(station_numbers_data_status))))
  # names(station_numbers_data_status) <- c("wmon","havaintojen_pituus","havaintoja_paivassa","mallidatan_pituus","season1_00_ennustejaksoja","season1_00_min_otoskoko","season1_00_max_otoskoko","season1_00_mean_otoskoko","season1_12_ennustejaksoja","season1_12_min_otoskoko","season1_12_max_otoskoko","season1_12_mean_otoskoko","season2_00_ennustejaksoja","season2_00_min_otoskoko","season2_00_max_otoskoko","season2_00_mean_otoskoko","season2_12_ennustejaksoja","season2_12_min_otoskoko","season2_12_max_otoskoko","season2_12_mean_otoskoko","season3_00_ennustejaksoja","season3_00_min_otoskoko","season3_00_max_otoskoko","season3_00_mean_otoskoko","season3_12_ennustejaksoja","season3_12_min_otoskoko","season3_12_max_otoskoko","season3_12_mean_otoskoko","season4_00_ennustejaksoja","season4_00_min_otoskoko","season4_00_max_otoskoko","season4_00_mean_otoskoko","season4_12_ennustejaksoja","season4_12_min_otoskoko","season4_12_max_otoskoko","season4_12_mean_otoskoko")
  # # Proceed if coefficients for that particular MOS version have not been calculated before this 
  # komento <- paste("tiedostot <- list.files(\"/media/ylhaisi/PURE/R_projects/vorlon_point_data/processed/MOS_coefficients/valmiit/",mos_label,"/\")",sep="")
  # eval(parse(text=komento))
  # rm(komento)
  # tiedostot <- as.character(matrix(tiedostot,nrow=(length(tiedostot)),byrow=T))
  # komento <- paste("tallennetut_kertoimet <- tiedostot[grep(\"^station_",station_numbers[station_number_index],"_\",tiedostot)]",sep="")
  # eval(parse(text=komento))
  # rm(komento)
  # if (length(tallennetut_kertoimet)<10) {
    
    ### RETRIEVING PREDICTAND DATA ###
    # WMO-numbers of stations which are retrieved (any wmon station list based on some criteria [such as geographical distance] can be defined here)
    # station_list_retrieved <- station_numbers[c(station_number_index,station_number_index+1)]
    # station_list_retrieved <- c(2974)
    station_list_retrieved <- station_numbers[c(station_number_index)]
    function_arguments <- list(variable_list_predictands,station_list_retrieved,timestamps_series)
    predictand_data <- do.call(retrieve_data_all,function_arguments)
    ### RETRIEVING PREDICTOR DATA ###
    function_arguments <- list(variable_list_predictors,station_list_retrieved,timestamps_series)
    predictor_data <- do.call(retrieve_data_all,function_arguments)
    # ### RETRIEVING ALL DATA ###
    # function_arguments <- list(rbind(variable_list_predictors,variable_list_predictands),station_list_retrieved,timestamps_series)
    # all_data <- do.call(retrieve_data_all,function_arguments)
    
    # # For this example, remove some variables from the predictand data (some MOS predictor data was already removed in retrieve_data_MOS.R)
    # predictand_data[["CLDB"]][["weather_data_qc"]] <- subset(predictand_data[["CLDB"]][["weather_data_qc"]],parameter!="TAMIN12H")
    # predictand_data[["CLDB"]][["weather_data_qc"]] <- subset(predictand_data[["CLDB"]][["weather_data_qc"]],parameter!="WD")
    # predictand_data[["CLDB"]][["observation_data_v1"]] <- subset(predictand_data[["CLDB"]][["observation_data_v1"]],measurand_id!="73")
    # predictor_data[["verif"]] <- subset(predictor_data[["verif"]],model!="kalmanecmwf")
    # unique(predictand_data[["CLDB"]][["weather_data_qc"]][,"parameter"])
    # rownames(all_variable_lists$mapping_parameters_all)[match(unique(predictand_data[["CLDB"]][["observation_data_v1"]][,"measurand_id"]),all_variable_lists$mapping_parameters_all$CLDB_observation_data_v1)]
    # 
    # Removing all unnecessary variables in variable_list_predictors which are not present in the data
    # EDIT THIS A LOT MORE STILL...
    # MOS: Remove those variables from variable_list which are not found in the database
    # COUNT THE NUMBER OF DIGITS IN STRING! nchar(x)<8 is not ENOUGH AS THERE IS e.g. TAMAX12H!!
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
        removed_vars <- removed_vars[sapply(removed_vars["param_id"],function (x) nchar(x)<8),]
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
        removed_vars <- removed_vars[sapply(removed_vars["id"],function (x) nchar(x)<8),]
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
        removed_vars <- removed_vars[sapply(removed_vars["id"],function (x) nchar(x)<8),]
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
        removed_vars <- removed_vars[sapply(removed_vars["measurand_id"],function (x) nchar(x)<8),]
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
        removed_vars <- removed_vars[sapply(removed_vars["weather_data_qc"],function (x) nchar(x)<8),]
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
    # Use above function like this
    variable_list_predictors <- clean_variable_list(variable_list_predictors,predictor_data)
    variable_list_predictands <- clean_variable_list(variable_list_predictands,predictand_data)
    
    
    
    
    
    # obsdata <- predictand_data$CLDB$weather_data_qc   # observation data is from observations_data_v1 of CLDB foreign sttaions
    obsdata <- predictand_data$CLDB$observation_data_v1
    mosdata <- predictor_data$MOS$previ_ecmos_narrow_v    # MOS data 
    #station_numbers[station_number_index]
    
    source("functions_glm.R")
    
    GlmnR1_training(station_list_retrieved, obsdata, mosdata)
    
#     # Tarkistetaan löytyykö mallidataa vai ei
#     if (length(eval(parse(text=mallidatamatriisi))[,1])<ennuste_havaintoparien_minimimaara) {
#       # Hypätään loopissa seuraavaan ajanjaksoon, koska mallidataa ei ole.
#       print(paste("mallidataa --",aseman_nimi,"-- ei löydy!",sep=""))
#       # Otetaan pois mallidata
#       komento <- paste("rm(",mallidatamatriisi,")",sep="")
#       eval(parse(text=komento))
#       rm(komento)
#       rm(mallidatamatriisi)
#       # Otetaan pois havaintodata
#       rm(havainnot)
#       rm(station_id)
#       rm(aseman_nimi)
#     } else {
#       for (sel in variable_indices) {
#         
#         ### RETREIVING PREDICTAND DATA ###
#         source("hae_havainnot_GTS_Brainstorm_AQU.R")
#         fmisid <- station_idt_conversion$fmisid[which(TRUE==(station_idt_conversion$wmon==station_numbers[station_number_index]))]
#         parameter <- selitettavat_muuttujat[sel]
#         sel_obsplugin <- selitettavat_muuttujat_obsplugin_koodit[sel]
#         sel_AQU <- selitettavat_muuttujat_AQU_koodit[sel]
#         sel_CLDB <- selitettavat_muuttujat_CLDB_koodit[sel]
#         station_id <- station_numbers[station_number_index]
#         aseman_nimi <- station_idt_conversion$station_name[which(station_idt_conversion$wmon==station_numbers[station_number_index])]
#         
#         havainnot <- hae_havainnot_GTS_Brainstorm_AQU(fmisid,parameter,sel_obsplugin,sel_AQU,sel_CLDB,station_id,aseman_nimi,onko_lentosaahavaintoja)      
#         rm(fmisid)
#         rm(parameter)
#         rm(sel_obsplugin)
#         rm(sel_AQU)
#         rm(sel_CLDB)
#         
#         # Tarkistetaan löytyykö havaintoja vai ei
#         # Tähän asti havaintoja voidaan hakea, vaikkei niitä tietokannassa olisikaan. Jos aseman osalta ei ole ollenkaan havaintoja (tai on naurettavan vähän), hypätään for-loopissa seuraavaan indeksiin.
#         if (length(havainnot[,1])<ennuste_havaintoparien_minimimaara) {
#           # Hypätään loopissa seuraavaan ajanjaksoon, koska havaintodataa ei ole.
#           print(paste("havaintodataa --wmon_",station_id," ",aseman_nimi,"-- ei löydy!",sep=""))
#           # Otetaan pois havaintodata
#           rm(havainnot)
#           rm(station_id)
#           rm(aseman_nimi)
#           
#         } else {
#           rm(station_id)
#           rm(aseman_nimi)
#           # SAVE DATA LENGTH LATER IN SCRIPT
#           # Tallennetaan jälki, että havaintoja löytyy ainakin jotain
#           station_numbers_data_status$havaintojen_pituus[station_number_index] <- dim(havainnot)[1]
#           havaintodata_paivastampit <- na.omit(as.POSIXct(trunc.POSIXt(havainnot$obstime, units=c("days"))))
#           havaintodata_paivastampit <- aggregate(data.frame(count=havaintodata_paivastampit),list(value=havaintodata_paivastampit),length)
#           station_numbers_data_status$havaintoja_paivassa[station_number_index] <- round(mean(havaintodata_paivastampit$count),digits=2)
#           rm(havaintodata_paivastampit)
#           # Tallennetaan jälki, että mallidataa löytyy. Sitähän pitäisi olla rutkasti enemmän kuin havaintoja.
#           station_numbers_data_status$mallidatan_pituus[station_number_index] <- dim(eval(parse(text=mallidatamatriisi)))[1]
#           
#           for (k in c(1)) {
#             
#       #       ### MALLIKENTTIEN HAKU MOS-TIETOKANNASTA ###
#       #       station_id <- station_numbers[station_number_index]
#       #       aseman_nimi <- station_idt_conversion$station_name[which(station_idt_conversion$wmon==station_numbers[station_number_index])]
#       #       analyysiaika <- analyysiajat[k]
#       #       mallidatamatriisi <- paste("station_",station_id,"_",analyysiaika,sep="")
#       #       
#       #       source("hae_mallidata_MOS_for_one_analysis_hour.R")
#       #       komento <- paste(mallidatamatriisi," <- hae_mallidata_MOS_for_one_analysis_hour(station_id,aseman_nimi,analyysiaika,mallidatamatriisi)",sep="") 
#       #       eval(parse(text=komento))
#       #       rm(komento)
#       #       rm(station_id)
#       #       rm(aseman_nimi)
#       #       rm(analyysiaika)
#             
#       #       source("hae_mallidata_MOS_individual_analysis_times.R")
#       #       komento <- paste(mallidatamatriisi,"_1kerrallaan <- hae_mallidata_MOS_individual_analysis_times(station_id,aseman_nimi,analyysiaika,mallidatamatriisi,sarja)",sep="") 
#       #       eval(parse(text=komento))
#       #       rm(komento)
#         
#       
#         
#       #       station_2974_00 <- station_2974_00[order(station_2974_00$forecast_time,station_2974_00$forecast_period,station_2974_00$param_id,station_2974_00$level_value),]
#       #       station_2974_00_1kerrallaan <- station_2974_00_1kerrallaan[order(station_2974_00_1kerrallaan$forecast_time,station_2974_00_1kerrallaan$forecast_period,station_2974_00_1kerrallaan$param_id,station_2974_00_1kerrallaan$level_value),]
#       #       jape <- station_2974_00$param_id[1:551157]-station_2974_00_1kerrallaan$param_id[1:551157]
#         
#         
#       
#             
#             
#       #     ennustusjaksot <- c(1,3,5,7,9,11,13,15,17,19,21,23,25,29,33,37,41,45,49,51,53,55,57,59,61,63,65)
#       #     ennustusjaksot <- (1:length(forecast_periods))
#       #     ennustusjaksot <- c(9,17,25,33,41,49,53,57,61,65) # 1 per päivä
#             
#             
#             for (kausi in c(1)) { # Kaudet järjestyksessä talvi,kevät,kesä,syksy
#               # Talvi: (sovellusjakso Joulu-Helmi 12-02), koulutusjakso 11-03
#               if (kausi==1) {
#                 sarja_rajattu <- sarja[as.numeric(format(sarja, "%m"))<4 | as.numeric(format(sarja, "%m"))>10]
#               }
#               # Kevät: (sovellusjakso Maalis-Touko 03-05), koulutusjakso 02-06
#               if (kausi==2) {
#                 sarja_rajattu <- sarja[as.numeric(format(sarja, "%m"))<7 & as.numeric(format(sarja, "%m"))>1]
#               }
#               # Kesä: (sovellusjakso Kesä-Elo 06-08), koulutusjakso 05-09
#               if (kausi==3) {
#                 sarja_rajattu <- sarja[as.numeric(format(sarja, "%m"))<10 & as.numeric(format(sarja, "%m"))>4]
#               }
#               # Syksy: (sovellusjakso Syys-Marras 09-11), koulutusjakso 08-12
#               if (kausi==4) {
#                 sarja_rajattu <- sarja[as.numeric(format(sarja, "%m"))>7]
#               }
#               
#               
#               # Tallennetaan tietokannasta löytyvät selittävät muuttujat omaan vektoriinsa, koska eri ennustejaksoille on saatavilla eri määrä muuttujia (lähinnä analyysi vs. ennusteet)
#               selittavat_muuttujat_tietokanta <- selittavat_muuttujat
#               # # Ennustusjaksot tässä järjestyksessä sen vuoksi, koska analyysihetkellä on vähemmän tietokannassa muuttujia kuin ennustusajanhetkillä.
#               # # ennustusjaksot <- c(5,1,length(forecast_periods))
#               # ennustusjaksot <- c(5,4,3,2,1,6:length(forecast_periods))
#               # ennustusjaksot <- c(1:length(forecast_periods))
#               # Tässä analysoidaan ennusteet ainoastaan 6h välein olevat ennusteet. Vaihdetaan samalla ensimmäisen ja toisen alkion paikkaa.
#               ennustusjaksot <- which((as.numeric(forecast_periods_hours) %% 6)==0)
#               ennustusjaksot <- c(ennustusjaksot[2],ennustusjaksot[1],ennustusjaksot[3:length(ennustusjaksot)])
#               for (j in ennustusjaksot) { # c(9,17,33,49,57,65)) { # päivät 1,2,4,6,8,10 1 ennuste per päivä c(9,17,33,49,57,65)) { # päivät 1,2,4,6,8,10 # 1 ennuste per päivä c(9,17,25,33,41,49,53,57,61,65)) { # kaikki mahdolliset 1:length(forecast_periods)) {
#                 selittavat_muuttujat <- selittavat_muuttujat_tietokanta
#                 
#                 if (j<35) {
#                   ennustusjakso_tunteina <- substr(forecast_periods[j],1,2)
#                 } else if (j>34) {
#                   ennustusjakso_tunteina <- substr(forecast_periods[j],1,3)
#                 }
#                 
#                 ### YHDISTÄMINEN SAMAAN MATRIISIIN ###
#                 source("integrate_MOS_obs_verif_matrices.R")
#                 ### KÄYTETTÄVISSÄ ON NYT TÄYSI MATRIISI (havainnot + verif-data + mallidata + apumuuttujat) JOS DATAA ON. ###
#                 
#                 # KOSKA DATA VOI MYÖS PUUTTUA, TÄYTYY VIELÄ ERIKSEEN TESTATA LÖYTYYKÖ SITÄ
#                 # Mikäli havainnot_ja_mallidata -data framen pituus on alle ennuste_havaintoparien_minimimaaran, siirrytään loopissa seuraavaan alkioon. Tällaiseen data frameen ei nimittäin ole mitään mieltä fitata lineaarisia malleja kun ne vain huonontaisivat ennustetta.
#                 if (exists("havainnot_ja_mallidata")==TRUE) {
#                   if (length(havainnot_ja_mallidata[,1])>=ennuste_havaintoparien_minimimaara) {
#                 
#                     # Lasketaan tilastollisen mallin herkkyys ennustaville muuttujille sekä verifiointijakson blokin pituudelle. Tallennetaan tulokset tiedostoon.
#                     # source("Postgre_query_point_data_check_MOS_sensitivity_lm_quantilemapping.R") VANHA PASKA, MUTTA TÄSTÄ SAAT lm_quantilemapping -ALGORITMIN KUN JOSKUS KIRJOITAT SEN UUDESTAAN
#                     
#                     # Piirretään MOS:n tuottamia arvoja scatterploteina vs. havainnot.
#                     # source("Postgre_query_point_data_scatterplot_MOS_vs_obs.R")
#                     
# #                     # Tallennetaan havainnot_ja_mallidata -matriisi tiedostoon omalla nimellään
# #                     tallennettava_matriisi <- paste("station_",station_numbers[station_number_index],"_",analyysiajat[k],"_",ennustusjakso_tunteina,"_season",kausi,"_",selitettavat_muuttujat_ECMWF_koodit[sel],"_level",ECMWF_muuttujat$level_value[match(selitettavat_muuttujat_ECMWF_koodit[sel],ECMWF_muuttujat$muuttuja_EC)],sep="")
# #                     komento <- paste(tallennettava_matriisi," <- havainnot_ja_mallidata",sep="")
# #                     eval(parse(text=komento))
# #                     rm(komento)
# #                     komento <- paste("write.csv(",tallennettava_matriisi,", file=\"/media/ylhaisi/PURE/R_projects/vorlon_point_data/processed/testisetteja/kesatyontekijalle/data/",tallennettava_matriisi,".csv\")",sep="")
# #                     eval(parse(text=komento))
# #                     rm(komento)
# #                     komento <- paste("rm(",tallennettava_matriisi,")",sep="")
# #                     eval(parse(text=komento))
# #                     rm(komento)
# #                     rm(tallennettava_matriisi)
#                   }
#                 }
#                 
#                 # MYÖS NÄISSÄ SKRIPTEISSÄ TESTATAAN SISÄLLÄ ONKO DATAA. JOS EI OLE, NIIN AINOASTAAN TALLENNETAAN VIIMEISEN ENNUSTEJAKSON AJANHETKELLÄ KOKO MATRIISIN SISÄLTÖ.
#                 
# #                 # Koulutetaan lineaarinen malli ja tallennetaan kertoimet tiedostoon
# #                 source("Postgre_query_point_data_train_and_save_lm_MOS.R")
#         
#                 
# #                 # NÄITÄ SKRIPTEJÄ VOIDAAN AJAA ILMAN havainnot_ja_mallidata -matriisiakin
# #         
#                 # Vertaillaan verifiointistatistiikkoja eri aineistojen välillä ja piirretään niistä kuvia
#                 source("Postgre_query_point_data_fcst_MOS_verification_all_timesteps_all_stations.R")
#                 
#             
#                 print(j)
#                 if (exists("havainnot_ja_mallidata")) {
#                   rm(havainnot_ja_mallidata)
#                 }
#                 if (exists("l2")) {
#                   rm(l2)
#                 }
#                 rm(ennustusjakso_tunteina)
#               }
#               rm(j)
#               rm(ennustusjaksot)
#               
#               # Ennustusjaksot on käyty läpi. Palautetaan selittäviksi muuttujiksi ne, joille löytyy edes jotain dataa MOS-tietokannasta (ei välttämättä kaikille ennustepituuksille)
#               selittavat_muuttujat <- selittavat_muuttujat_tietokanta
#               rm(selittavat_muuttujat_tietokanta)
#               rm(sarja_rajattu)
#             }
#             rm(kausi)
#           }
#           rm(k)
#           
#           
#           # Otetaan pois havainto, verifdb- ja mallidatat
#           if (exists("havainnot")) {
#             rm(havainnot)
#           }
#           temp1=ls()
#           temp1 <- temp1[grep("^verifdb_",temp1)]
#           rm(list=temp1)
#           rm(temp1)
#         }
#         
#         
#         
#         #   rm(station_numbers_indices)
#         #   # Tallennetaan valvontamatriisi, jossa on lueteltu MOS:n kouluttamiseen käytetty data asemittain
#         #   pvm <- format(as.POSIXct(as.character(Sys.time()),tz="GMT"),"%d%m%Y")
#         #   komento <- paste("write.csv(station_numbers_data_status, file=\"/media/ylhaisi/PURE/R_projects/vorlon_point_data/processed/MOS_coefficients/keskeneraiset/",mos_label,"/koulutukseen_kaytetty_data/",selitettavat_muuttujat_ECMWF_koodit[sel],"_level",ECMWF_muuttujat$level_value[match(selitettavat_muuttujat_ECMWF_koodit[sel],ECMWF_muuttujat$muuttuja_EC)],"_lm_",mos_label,"_",muuttujajoukko,"_",pvm,".csv\", quote = TRUE)",sep="")
#         #   eval(parse(text=komento))
#         #   rm(komento)
#         #   rm(pvm)
#         rm(station_numbers_data_status)
#       }
#       rm(sel)
#       
#       # Poistetaan kaikki mallikama (kaiken mallidatan sisältävä mallidatamatriisi, sekä mahdolliset kerroinmatriisit)
#       temp1=ls()
#       komento <- paste("temp1 <- temp1[grep(\"^station_",station_numbers[station_number_index],"\",temp1)]",sep="")
#       eval(parse(text=komento))
#       rm(komento)
#       rm(list=temp1)
#       rm(temp1)
#       rm(mallidatamatriisi)
#     }
#   }
#   rm(tallennetut_kertoimet)
#   rm(tiedostot)
#   print(i)
}
rm(station_number_index)



rm(con1)
rm(con2)
rm(con3)
rm(con4)
rm(drv_psql)
rm(drv_ora)