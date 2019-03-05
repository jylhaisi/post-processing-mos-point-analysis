choose_variables <- function (variable_list,table_name,db) {
  
  # HERE PUT ERROR CHECK WHETHER FUNCTION ARGUMENTS ARE PROPER OR NOT!
  # AVIATION DB PARAMETERS HAVE NOT BEEN CODED AT ALL YET TO THIS SCRIPT
  
  derived_variables_all <- rownames(all_variable_lists$derived_variables_all)[-1]
  
  # With this function the most common uses for creating variable lists are defined:
  # -predictor variable_list for MOS training (data retrieved from MOS table previ_ecmos_narrow_v). Used derived variables are included in the first argument
  # example: choose_variables(c("allmodelvars_1prec_derived_noBAD_noZW_RH2","T2_2","DECLINATION"),"previ_ecmos_narrow_v","MOS")
  # -predictand variable_list for MOS training (data retrieved from CLDB tables observation_data_v1 and weather_data_qc, option "both" retrieves data from both tables for the station list at question). In case of table_name %in% c("weather_data_qc","both"), give variable names according to rownames(all_variable_lists[["mapping_parameters_all"]])! If you retrieve data from Finnish stations and from variables outside mapping_parameters_all (observation_data_v1), use db-specific variable numbers all_variable_lists$CLDB_observation_data_v1$measurand_id!
  # example: choose_variables("estimated_variables","both","CLDB")
  # -DMO data from verif db (data retrieved from verif db tables "producer_station", second argument of tables indicate producers which are used)
  # example: choose_variables("estimated_variables",c("ecmwf","gem","kalmanecmwf","hirlam"),"verif")
  # -all coefficients and results from MOS_trace_v (options to choose all variables in the regression equation "all_vars" or only the variable which is estimated, "only_estimated_var"). In case table_name==MOS_trace_v and db==MOS, an additional column is added to parameter table_name which indicates the name of the MOS version which is returned. If no parameter is given (no second column exists to the argument), then all versions are returned
  # example: choose_variables("only_estimated_vars","mos_trace_v","MOS")
  
  # Pre-defined variable lists which are used primarily for MOS db (also other can be used in specific cases) IF parameter variable_list matches these variable lists, they are returned
    ## allmodelvars
    # all ECMWF model variables (covering full MOS training period from the year 2011) in database are used (including time-lagged). Same as in file ECMWF_narrow_variable_list_explained.csv
    ## IN TOGETHER with variable list "allmodelvars" the following appendices are defined (e.g "allmodelvars_1prec_derived_noZW")
    ## _1prec
    # Precipitation variables are combined (TP = CP + LSP).
    ## _noBAD
    # No surface parameters having problems to them
    ## _noRAD
    # No radiation parameters
    ## _noZW
    # No pressure level Z/W
    ## _RH2
    # Transforming model RH to RH_2
    ## _RH3
    # Transforming model RH to RH_3
    ## _RH4
    # Transforming model RH to RH_4
    ## _RH5
    # Transforming model RH to RH_5
    ## _surface
    # Only surface variables
    ## _pressurelevels
    # Only pressule level variables
    ## MOSconstant_9a
    # First operative MOS version
    ## MOSconstant_9b
    # First operative MOS version, with time-lagged variables added
    ## only_bestvars
    # Second operative MOS version
    ## only_bestvars2
    # Second operative MOS version, including Tmax/Tmin
    ## only_surfT2
    # only surface temperature
    ## estimated_variables
    # all variables which are defined in all_variable_lists$estimated_variables, the names of the returned parameters correspond to those of the parameter db (in case of CLDB variable name in first column is used)
  
    # IF variable_list doesn't correspond to any of these, the variables in the parameter "variable_list" are returned as typed in it
  
  # pre-defined table_name/db combinations that can be used
    ## previ_ecmos_narrow_v/MOS 
    # the most common combination that is used when downloading model data for MOS training
    ## mos_trace_v/MOS
    # data from MOS db table_name mos_trace_v (parameters can be either "all_vars" or "only_estimated_vars")
    ## observation_data_v1/CLDB
    # Finnish observations
    ## weather_data_qc/CLDB
    # Foreign and road weather observations
    ## both/CLDB
    # Both Finnish and foreign observations for a specific variable
    ## {producer list}/verif
    # point data from verif db from from a list of producers
  
  
  # By default variable_list is manually defined in the first function argument (variable names specific to which is used in db)
  variable_name <- variable_list
  
  # Those variables that are estimated
  if (variable_list[1]=="estimated_variables") {
    if (db %in% c("aviation","verif")) {
      variable_name <- eval(subs(all_variable_lists[["estimated_parameters"]][[db]]))
    }
    if (db=="MOS") {
      variable_name <- eval(subs(all_variable_lists[["estimated_parameters"]][[paste0(db,"_",table_name)]]))
    }
    if (db=="CLDB") {
      # if both Finnish and foreign CLDB data is retrieved a universal field_name is returned
      if (table_name[1]=="both") {
        variable_name <- rownames(all_variable_lists[["estimated_parameters"]])
      }
      if (table_name[1] %in% c("observation_data_v1","weather_data_qc")) {
        variable_name <- eval(subs(all_variable_lists[["estimated_parameters"]][[paste0(db,"_",table_name)]]))
      }
    }
  }
  
  # VARIABLE LISTS FOR MOS TRAINING - based on ECMWF_narrow_variable_list
  if (db=="MOS" & table_name[1]=="previ_ecmos_narrow_v") {
    if (length(grep("allmodelvars",variable_list[1]))>0) {
      # DEFINING BASIS FOR ALLMODELVARS: ALL THESE VARIABLES ARE AVAILABLE FOR THE WHOLE MOS TRAINING PERIOD IN MOS DB
      variable_name <- read.csv("../constant_lists/variable_lists/ECMWF_narrow_variable_list.csv",stringsAsFactors = FALSE)[[1]]
      # Removing constant variables
      variable_name <- variable_name[-grep("LSM|Z_ORO",variable_name)]
      # Removing unnecessary variables (TP is duplicate, STRD is not needed as net component is found as well)
      variable_name <- variable_name[-grep("TP|STRD",variable_name)]
      # adding time-lagged variables
      variable_name <- c(variable_name,"T2_M1","T_925_M1","T_950_M1")
      
      # NARROWING DOWN ALLMODELVARS BASED ON APPENDICES GIVEN IN THE FIRST ARGUMENT
      # Convective and large-scale rain are combined: using only TP instead of both LSP and CP
      if (length(grep("1prec",variable_list[1]))>0) {
        variable_name <- variable_name[-grep("LSP",variable_name)]
        variable_name <- variable_name[-grep("CP",variable_name)]
        variable_name <- c(variable_name,"TP")
      }
      # Not including those variables that are poorly simulated/contain erroneous values or outliers/are sensitive to model version changes/would need a variable conversion to them
      if (length(grep("noBAD",variable_list[1]))>0) {
        # REMEMBER: DO NOT USE GH FOR PRESSURE LEVELS, USE Z!
        # These variables have some unresolved outliers to their training time series, which prohibits their use
        variable_name <- variable_name[-grep("RSN|SSTK|U_100M|V_100M",variable_name)]
        # CBH has missing values in the training data.
        variable_name <- variable_name[-grep("CBH",variable_name)]
        # These are not meaningful for practically any estimated variable
        variable_name <- variable_name[-grep("CAPE|CIN",variable_name)]
        # These would demand some variable conversions to them or some non-linear modeling approaches
        variable_name <- variable_name[-grep("SD",variable_name)]
      }
      # Not including radiation variables
      if (length(grep("noRAD",variable_list[1]))>0) {
        # These variables have some unresolved outliers to their training time series, which prohibits their use
        variable_name <- variable_name[-grep("SLHF|SSHF|SSR|STR",variable_name)]
      }
      # No heights of pressure levels or vertical velocity
      if (length(grep("noZW",variable_list[1]))>0) {
        variable_name <- variable_name[-grep("Z_",variable_name)]
        variable_name <- variable_name[-grep("W_",variable_name)]
      }
      # Transforming RH to RH_2
      if (length(grep("RH2",variable_list[1]))>0) {
        replaceable <- variable_name[grep("RH_",variable_name)]
        variable_name[grep("RH_",variable_name)] <- gsub("RH","RH_2",replaceable)
        rm(replaceable)
      }
      # Transforming RH to RH_3
      if (length(grep("RH3",variable_list[1]))>0) {
        replaceable <- variable_name[grep("RH_",variable_name)]
        variable_name[grep("RH_",variable_name)] <- gsub("RH","RH_3",replaceable)
        rm(replaceable)
      }
      # Transforming RH to RH_4
      if (length(grep("RH4",variable_list[1]))>0) {
        replaceable <- variable_name[grep("RH_",variable_name)]
        variable_name[grep("RH_",variable_name)] <- gsub("RH","RH_4",replaceable)
        rm(replaceable)
      }
      # All surface variables, no pressure level variables
      if (length(grep("surface",variable_list[1]))>0) {
        variable_name <- variable_name[-grep("_950|_925|_850|_700|_500",variable_name)]
      }
      # All pressure level variables, no surface variables
      if (length(grep("pressurelevels",variable_list[1]))>0) {
        variable_name <- variable_name[grep("_950|_925|_850|_700|_500",variable_name)]
      }
    }
    # First operative MOS version, with a constant predictor variable list
    if (variable_list[1]=="MOSconstant_9a") {
      variable_name <- c("MSL","T2","D2","LCC","MCC","RH_925","T_925","RH_500","T_500")
    }
    # First operative MOS version, with time-lagged predictors, with a constant predictor variable list
    if (variable_list[1]=="MOSconstant_9b") {
      variable_name <- c("MSL","T2","D2","LCC","MCC","RH_925","T_925","RH_500","T_500","T2_M1","T_925_M1")
    }
    # Second operative MOS version
    if (variable_list[1]=="only_bestvars") {
      variable_name <- c("MSL","T2","D2","SKT","U10","V10","LCC","MCC","T_925","T2_M1","T_925_M1","DECLINATION")
    }
    # Second operative MOS version (first variable list going to Tmax/Tmin estimation)
    if (variable_list[1]=="only_bestvars2") {
      variable_name <- c("MSL","T2","D2","SKT","U10","V10","LCC","MCC","MN2T3","MX2T3","T_925","T2_M1","T_925_M1","DECLINATION")
    }
    # Declination completely replaced with ensmean
    if (variable_list[1]=="only_bestvars2_no_climatology_ensmean") {
      variable_name <- c("MSL","T2","D2","SKT","U10","V10","LCC","MCC","MN2T3","MX2T3","T2_ENSMEAN","T_925","T2_M1","T_925_M1")
    }
    
    
    # Only surface temperature
    if (variable_list[1]=="only_surfT2") {
      variable_name <- c("T2")
    }
    
    # Combining user-defined (derived, time-lagged or plain model) variables with pre-defined variable lists
    variable_name <- unique(c(variable_name,variable_list))
    
    
    ### REORDERING MOS VARIABLES ###
    # Order is: surface vars, pressure level vars, time_lagged vars, derived vars.
    
    # First taking all vars among variable_name which are not non_timelagged surfacevars. Adding derived_vars to this list
    non_surfacevars <- unique(c(variable_name[grep("_950|_925|_850|_700|_500|_M",variable_name)],derived_variables_all))
    # Surface vars is everything else
    if (length(which(variable_name %in% non_surfacevars))>0) {
      surfacevars <- variable_name[-which(variable_name %in% non_surfacevars)]
    } else {
      surfacevars <- variable_name
    }
    # Ordering to same order as variables in list ECMWF_muuttujat
    surfacevars <- all_variable_lists$MOS$variable_EC[which(all_variable_lists$MOS$variable_EC %in% surfacevars)]
    
    # Pick out pressure level variables, time-lagged variables and derived variables.
    pressurelevelvars1 <- all_variable_lists$MOS$variable_EC[which(all_variable_lists$MOS$variable_EC %in% (variable_name[grep("_950$",variable_name)]))]
    pressurelevelvars2 <- all_variable_lists$MOS$variable_EC[which(all_variable_lists$MOS$variable_EC %in% (variable_name[grep("_925$",variable_name)]))]
    pressurelevelvars3 <- all_variable_lists$MOS$variable_EC[which(all_variable_lists$MOS$variable_EC %in% (variable_name[grep("_850$",variable_name)]))]
    pressurelevelvars4 <- all_variable_lists$MOS$variable_EC[which(all_variable_lists$MOS$variable_EC %in% (variable_name[grep("_700$",variable_name)]))]
    pressurelevelvars5 <- all_variable_lists$MOS$variable_EC[which(all_variable_lists$MOS$variable_EC %in% (variable_name[grep("_500$",variable_name)]))]
    time_laggedvars <- derived_variables_all[which(derived_variables_all %in% (variable_name[grep("_M",variable_name)]))]
    derived_vars <- derived_variables_all[which(derived_variables_all %in% variable_name & derived_variables_all %!in% (variable_name[grep("_M",variable_name)]))]
    # Concatenating these variable lists and removing inermediate results
    variable_name <- c(surfacevars,pressurelevelvars1,pressurelevelvars2,pressurelevelvars3,pressurelevelvars4,pressurelevelvars5,time_laggedvars,derived_vars)
    rm(non_surfacevars)
    rm(surfacevars)
    rm(pressurelevelvars1)
    rm(pressurelevelvars2)
    rm(pressurelevelvars3)
    rm(pressurelevelvars4)
    rm(pressurelevelvars5)
    rm(time_laggedvars)
    rm(derived_vars)
    
    # If variable list has RH_SURF or RH_SURF_2, remove D2 from variable list
    if (sum(c("RH_SURF","RH_SURF_2") %in% variable_name) > 0) {
      variable_name <- variable_name[variable_name!="D2"]
    }
  }
  
  # VARIABLE LISTS FOR mos_trace_v THINK THIS THROUGH AGAIN LATER...
  if (db=="MOS" & table_name[1]=="mos_trace_v") {
    if (variable_list[1]=="only_estimated_vars") {
      variable_name <- all_variable_lists[["estimated_parameters"]][["MOS_mos_trace_v"]]
    }
    if (variable_list[1]=="all_vars") {
      variable_name <- "all"
    }
  }
  
  # If not verification db, always retrieve from same table
  if (db!="verif") {
    variable_name <- cbind.data.frame(variable_name,table_name,db,stringsAsFactors=FALSE)
  } else {
    variable_name <- cbind.data.frame(expand.grid(variable_name=variable_name,table_name=table_name,stringsAsFactors = FALSE),db,stringsAsFactors=FALSE)
  }
  

  # Returning
  variable_name
}
