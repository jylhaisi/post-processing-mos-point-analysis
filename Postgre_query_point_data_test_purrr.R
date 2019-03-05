# This script initializes interfaces to various databases, can be used to read in data and in MOS training/analysis.
rm(list=ls())
# Reading in the required packages, mapping lists and functions
source("load_libraries_tables_and_open_connections.R")

# WHEN MAKING THIS SCRIPT INTO A FUNCTION, MAKE POSSIBLE TO GIVE STATION NUMBERS OR STATION LISTS AS ARGUMENTS
# DERIVED VARIABLES: IS ONLY SUPPORTED CURRENTLY FOR MOS VARIABLES. CLDB/VERIF VARIABLES CANNOT BE DERIVED ATM. DERIVED VARIABLES WITH 8-DIGIT PARAMETER NUMBER (E.G. ASTRONOMICAL VARIABLES) ARE FORMED LATER IN EXECUTION WHEN FORMING MODEL-OBSPAIRS AND ARE NOT STORED IN MOS DATA FRAMES!
# KNOWN ISSUES: TAMINDAILY -RETRIEVAL AND OTHER DERIVED VARS NEED TO BE CHANGED SO THAT DERIVED_VARIABLE NAME IS CHANGED INSTEAD OF DB-SPECIFIC NAME
# CREATE A FUNCTION THAT "MELTS" "observation_data_v1" and "weather_data_qc" FROM CLDB LIST INTO A COMMON DATA FRAME!
# For TAMAX12H/TAMIN12H values InterpolateMinMaxValues generates multiple NA-values for those 12h timeslots that have missing value for the variable.
# Improve data retrieval scripts:
# mos_trace_v: there's never really a need to download just a specific source_param_name, but the retrieval is based on target_param_name. As an additional definition in the retrieval it has to be defined whether 1) all corresponding source_values 2) just the one source_value of the same variable 3) no source_values are retrieved at the same time
# Smartmet server: Generate mapping tables for this data source.

# User-defined variables
timestamps_series <- define_time_series(begin_date = trunc.POSIXt(with_tz(Sys.time(),tz="GMT"),"days")-(3*24*3600), end_date = (trunc.POSIXt(with_tz(Sys.time(),tz="GMT"),"days")))  # define_time_series(begin_date = "2011-12-01 00:00:00 GMT", end_date = Sys.time())
modelobspairs_minimum_sample_size <- 100 # Arbitrary number here, could in principle also depend on the number of predictor variables
date_string <- format(Sys.time(), "%d%m%y")
mos_label <- paste0("MOS_ECMWF_",date_string)
predictor_set <- "only_bestvars2" #"NA" #"allmodelvars_1prec_noBAD_RH2"
derived_variables <- NA # c("Z_ORO","Z_850")  #c("RH_SURF","Z_850","GH_850")  # NA # c("DECLINATION")
station_list <- "mos_stations_homogeneous_Europe" # Possible pre-defined station lists are those names in all_station_lists. If you want to use an arbitrary station list, assign the station numbers manually to variable station_numbers
station_numbers <- c(1406,2978,4124,4165) # eval(subs(all_station_lists[[station_list]])) # c(1406,2978) # Retrievals are generated and data is returned based on station wmon-numbers. If using a station list outside mos station list, define the wmon-numbers here.
obs_interpolation_method <- "spline_interp" # options repeat_previous (na.locf),linear_interp (na.approx),spline_interp (na.spline),no_interp (leave NA values to timeseries as they are). Continuous observations are interpolated, those which not are sublists in all_variable_lists
max_interpolate_gap <- 6 # This indicates the maximum time in hours to which observation interpolation is applied
verif_stationtype <- "normal" # In verif db, several stationgroups exist. "normal" assumes stations (2700 <= wmon <= 3000) belonging to stationgroup=1, and all other to stationgroup=9 (other stationgroups outside stationgroup=3 only have a small number of stations to them). Road weather station support needs to be coded later (this needs a road weather station list), currently this can be done manually by putting the stationgroup of interest here manually (e.g. ==3)
output_dir <- paste0("/data/statcal/results/MOS_coefficients/in_progress/test_purrr_210219/") # paste0("/data/statcal/results/MOS_coefficients/in_progress/",mos_label,"/")
max_variables <- 10
fitting_algorithm  <- "GlmnR1"
fitting_method <- "purrr" # either purrr or glm

# Defining used variable_lists
# First column indicates specific variable name in the database table indicated by the second column, third column is the database name.
# Varibles shortnames are database-specific, except for MOS db (it uses pre-defined variable set names and derived variables) and CLDB_both (foreign and finnish observations are both fetched and they have different variable names).
# For MOS db table MOS_trace_v, data can be fetched either from only the target_param_name (use table_name MOS_trace_v) or also from source_param_name and weight (use table_name MOS_trace_v_all).
# If you want to combine variables from several databases, run choose_variables several times with different parameters and combine the output.
# variable_list_predictors_all <- variable_list_predictors <- choose_variables(c(predictor_set,derived_variables),"previ_ecmos_narrow_v","MOS")
# variable_list_predictands_all <- variable_list_predictands <- choose_variables("estimated_variables","both","CLDB")

variable_list_predictors_all <- variable_list_predictors <- choose_variables(c(predictor_set,derived_variables),"previ_ecmos_narrow_v","MOS")
variable_list_predictands_all <- variable_list_predictands <- choose_variables("TA","both","CLDB") # choose_variables("estimated_variables","both","CLDB")
# variable_list_predictors_all <- variable_list_predictors <- rbind(choose_variables(c(predictor_set,derived_variables),"previ_ecmos_narrow_v","MOS"),choose_variables("1",c("ecmwf","pal","kalmanecmwf","hirlam"),"verif"),choose_variables("5",c("ecmwf","pal","kalmanecmwf","hirlam"),"verif"))
# variable_list_predictands_all <- variable_list_predictands <- rbind(choose_variables("estimated_variables","both","CLDB"),choose_variables(c("56","73"),"observation_data_v1","CLDB"),choose_variables(c("PSEA","WS","WD"),"weather_data_qc","CLDB"))

station_list_retrieved <- "all_stations" # station_numbers[c(station_number_index)]
function_arguments <- list(variable_list_predictands,station_list_retrieved,timestamps_series)
predictand_data <- do.call(retrieve_data_all,function_arguments)

save.image("/data/ylhaisi/mos_trace_v_obs_retrieval_tests/new_retrievals.RData")

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

# # Defining running indices from lists
# station_numbers_indices <- seq_len(length(station_numbers))
# # station_numbers_indices <- station_numbers_indices[-(1:163)]
# variable_indices <- seq_len(length(variable_list_predictands[["variable_name"]]))
# 
# for (station_number_index in station_numbers_indices) {
#   
#   print(station_numbers[c(station_number_index)])
#   
#   # Assign original predictors/predictands
#   variable_list_predictors <- variable_list_predictors_all
#   variable_list_predictands <- variable_list_predictands_all
#   
#   # # DEFINE THIS RESULT PART LATER, INSIDE THE FUNCTION THAT PRODUCES ACTUAL MOS COEFFICIENTS TO A FILE
#   # # This matrix contains metadata related to MOS training
#   # station_numbers_data_status <- station_numbers
#   # station_numbers_data_status <- cbind(station_numbers_data_status, as.data.frame(matrix(0,ncol=35,nrow=length(station_numbers_data_status))))
#   # names(station_numbers_data_status) <- c("wmon","havaintojen_pituus","havaintoja_paivassa","mallidatan_pituus","season1_00_ennustejaksoja","season1_00_min_otoskoko","season1_00_max_otoskoko","season1_00_mean_otoskoko","season1_12_ennustejaksoja","season1_12_min_otoskoko","season1_12_max_otoskoko","season1_12_mean_otoskoko","season2_00_ennustejaksoja","season2_00_min_otoskoko","season2_00_max_otoskoko","season2_00_mean_otoskoko","season2_12_ennustejaksoja","season2_12_min_otoskoko","season2_12_max_otoskoko","season2_12_mean_otoskoko","season3_00_ennustejaksoja","season3_00_min_otoskoko","season3_00_max_otoskoko","season3_00_mean_otoskoko","season3_12_ennustejaksoja","season3_12_min_otoskoko","season3_12_max_otoskoko","season3_12_mean_otoskoko","season4_00_ennustejaksoja","season4_00_min_otoskoko","season4_00_max_otoskoko","season4_00_mean_otoskoko","season4_12_ennustejaksoja","season4_12_min_otoskoko","season4_12_max_otoskoko","season4_12_mean_otoskoko")
#   # # Proceed if coefficients for that particular MOS version have not been calculated before this 
#   # komento <- paste("tiedostot <- list.files(\"/media/ylhaisi/PURE/R_projects/vorlon_point_data/processed/MOS_coefficients/valmiit/",mos_label,"/\")",sep="")
#   # eval(parse(text=komento))
#   # rm(komento)
#   # tiedostot <- as.character(matrix(tiedostot,nrow=(length(tiedostot)),byrow=T))
#   # komento <- paste("tallennetut_kertoimet <- tiedostot[grep(\"^station_",station_numbers[station_number_index],"_\",tiedostot)]",sep="")
#   # eval(parse(text=komento))
#   # rm(komento)
#   # if (length(tallennetut_kertoimet)<10) {
#     
#   ### RETRIEVING PREDICTAND DATA ###
#   # WMO-numbers of stations which are retrieved (any wmon station list based on some criteria [such as geographical distance] can be defined here)
#   # station_list_retrieved <- station_numbers[c(station_number_index,station_number_index+1)]
#   # station_list_retrieved <- c(2974)
#   station_list_retrieved <- station_numbers[c(station_number_index)]
#   function_arguments <- list(variable_list_predictands,station_list_retrieved,timestamps_series)
#   predictand_data <- do.call(retrieve_data_all,function_arguments)
#   # ### RETRIEVING PREDICTOR DATA ###
#   function_arguments <- list(variable_list_predictors,station_list_retrieved,timestamps_series)
#   predictor_data <- do.call(retrieve_data_all,function_arguments)
#   # # ### RETRIEVING ALL DATA ###
#   # function_arguments <- list(rbind(variable_list_predictors,variable_list_predictands),station_list_retrieved,timestamps_series)
#   # all_data <- do.call(retrieve_data_all,function_arguments)
#   
#   # Exit if either obsdata or mosdata is completely NULL, proceed to next index
#   if (is.null(unlist(predictand_data)) | is.null(unlist(predictor_data))) {
#     print(paste0("station number ",station_numbers[c(station_number_index)]," has NULL values for either obs or model data!"))
#     next
#   }
#   
#   # # For this example, remove some variables from the predictand data (some MOS predictor data was already removed in retrieve_data_MOS.R)
#   # predictand_data[["CLDB"]][["weather_data_qc"]] <- subset(predictand_data[["CLDB"]][["weather_data_qc"]],parameter!="TAMIN12H")
#   # predictand_data[["CLDB"]][["weather_data_qc"]] <- subset(predictand_data[["CLDB"]][["weather_data_qc"]],parameter!="WD")
#   # predictand_data[["CLDB"]][["observation_data_v1"]] <- subset(predictand_data[["CLDB"]][["observation_data_v1"]],measurand_id!="73")
#   # predictor_data[["verif"]] <- subset(predictor_data[["verif"]],model!="kalmanecmwf")
#   # unique(predictand_data[["CLDB"]][["weather_data_qc"]][,"parameter"])
#   # rownames(all_variable_lists$mapping_parameters_all)[match(unique(predictand_data[["CLDB"]][["observation_data_v1"]][,"measurand_id"]),all_variable_lists$mapping_parameters_all$CLDB_observation_data_v1)]
#   #
#   # Remove those variables from variable_list which are not found in the database
#   variable_list_predictors <- clean_variable_list(variable_list_predictors,predictor_data)
#   variable_list_predictands <- clean_variable_list(variable_list_predictands,predictand_data)
#   
#   if (!(is.null(predictand_data$CLDB$weather_data_qc))) {  # observation data from foreign station
#     obsdata <- predictand_data$CLDB$weather_data_qc
#     station_type <- 1
#   } else {
#     obsdata <- predictand_data$CLDB$observation_data_v1  # observation data from Finnish stations
#     station_type <- 0
#   }
#   mosdata <- predictor_data$MOS$previ_ecmos_narrow_v    # MOS data 
#   
#   # Training linear model and saving coefficients to a file
#   MOS_training(station_list_retrieved, obsdata, mosdata, max_variables, fitting_method, fitting_algorithm, station_type, output_dir)
#     
# 
# #       for (sel in variable_indices) {
# #           # SAVE DATA LENGTH LATER IN SCRIPT
# #           # Tallennetaan jälki, että havaintoja löytyy ainakin jotain
# #           station_numbers_data_status$havaintojen_pituus[station_number_index] <- dim(havainnot)[1]
# #           havaintodata_paivastampit <- na.omit(as.POSIXct(trunc.POSIXt(havainnot$obstime, units=c("days"))))
# #           havaintodata_paivastampit <- aggregate(data.frame(count=havaintodata_paivastampit),list(value=havaintodata_paivastampit),length)
# #           station_numbers_data_status$havaintoja_paivassa[station_number_index] <- round(mean(havaintodata_paivastampit$count),digits=2)
# #           rm(havaintodata_paivastampit)
# #           # Tallennetaan jälki, että mallidataa löytyy. Sitähän pitäisi olla rutkasti enemmän kuin havaintoja.
# #           station_numbers_data_status$mallidatan_pituus[station_number_index] <- dim(eval(parse(text=mallidatamatriisi)))[1]
# #           
# #           for (k in c(1)) {
# #             for (kausi in c(1)) { # Kaudet järjestyksessä talvi,kevät,kesä,syksy
# #               # Talvi: (sovellusjakso Joulu-Helmi 12-02), koulutusjakso 11-03
# #               if (kausi==1) {
# #                 sarja_rajattu <- sarja[as.numeric(format(sarja, "%m"))<4 | as.numeric(format(sarja, "%m"))>10]
# #               }
# #               # Kevät: (sovellusjakso Maalis-Touko 03-05), koulutusjakso 02-06
# #               if (kausi==2) {
# #                 sarja_rajattu <- sarja[as.numeric(format(sarja, "%m"))<7 & as.numeric(format(sarja, "%m"))>1]
# #               }
# #               # Kesä: (sovellusjakso Kesä-Elo 06-08), koulutusjakso 05-09
# #               if (kausi==3) {
# #                 sarja_rajattu <- sarja[as.numeric(format(sarja, "%m"))<10 & as.numeric(format(sarja, "%m"))>4]
# #               }
# #               # Syksy: (sovellusjakso Syys-Marras 09-11), koulutusjakso 08-12
# #               if (kausi==4) {
# #                 sarja_rajattu <- sarja[as.numeric(format(sarja, "%m"))>7]
# #               }
# #               
# #               
# #               # Tallennetaan tietokannasta löytyvät selittävät muuttujat omaan vektoriinsa, koska eri ennustejaksoille on saatavilla eri määrä muuttujia (lähinnä analyysi vs. ennusteet)
# #               selittavat_muuttujat_tietokanta <- selittavat_muuttujat
# #               # # Ennustusjaksot tässä järjestyksessä sen vuoksi, koska analyysihetkellä on vähemmän tietokannassa muuttujia kuin ennustusajanhetkillä.
# #               # # ennustusjaksot <- c(5,1,length(forecast_periods))
# #               # ennustusjaksot <- c(5,4,3,2,1,6:length(forecast_periods))
# #               # ennustusjaksot <- c(1:length(forecast_periods))
# #               # Tässä analysoidaan ennusteet ainoastaan 6h välein olevat ennusteet. Vaihdetaan samalla ensimmäisen ja toisen alkion paikkaa.
# #               ennustusjaksot <- which((as.numeric(forecast_periods_hours) %% 6)==0)
# #               ennustusjaksot <- c(ennustusjaksot[2],ennustusjaksot[1],ennustusjaksot[3:length(ennustusjaksot)])
# #               for (j in ennustusjaksot) { # c(9,17,33,49,57,65)) { # päivät 1,2,4,6,8,10 1 ennuste per päivä c(9,17,33,49,57,65)) { # päivät 1,2,4,6,8,10 # 1 ennuste per päivä c(9,17,25,33,41,49,53,57,61,65)) { # kaikki mahdolliset 1:length(forecast_periods)) {
# #                 selittavat_muuttujat <- selittavat_muuttujat_tietokanta
# #                 
# #                 if (j<35) {
# #                   ennustusjakso_tunteina <- substr(forecast_periods[j],1,2)
# #                 } else if (j>34) {
# #                   ennustusjakso_tunteina <- substr(forecast_periods[j],1,3)
# #                 }
# #                 
# #                 ### YHDISTÄMINEN SAMAAN MATRIISIIN ###
# #                 source("integrate_MOS_obs_verif_matrices.R")
# #                 ### KÄYTETTÄVISSÄ ON NYT TÄYSI MATRIISI (havainnot + verif-data + mallidata + apumuuttujat) JOS DATAA ON. ###
# #                 
# #                 # KOSKA DATA VOI MYÖS PUUTTUA, TÄYTYY VIELÄ ERIKSEEN TESTATA LÖYTYYKÖ SITÄ
# #                 # Mikäli havainnot_ja_mallidata -data framen pituus on alle ennuste_havaintoparien_minimimaaran, siirrytään loopissa seuraavaan alkioon. Tällaiseen data frameen ei nimittäin ole mitään mieltä fitata lineaarisia malleja kun ne vain huonontaisivat ennustetta.
# #                 if (exists("havainnot_ja_mallidata")==TRUE) {
# #                   if (length(havainnot_ja_mallidata[,1])>=ennuste_havaintoparien_minimimaara) {
# #                 
# #                     # Lasketaan tilastollisen mallin herkkyys ennustaville muuttujille sekä verifiointijakson blokin pituudelle. Tallennetaan tulokset tiedostoon.
# #                     # source("Postgre_query_point_data_check_MOS_sensitivity_lm_quantilemapping.R") VANHA PASKA, MUTTA TÄSTÄ SAAT lm_quantilemapping -ALGORITMIN KUN JOSKUS KIRJOITAT SEN UUDESTAAN
# #                     
# #                     # Piirretään MOS:n tuottamia arvoja scatterploteina vs. havainnot.
# #                     # source("Postgre_query_point_data_scatterplot_MOS_vs_obs.R")
# #                     
# # #                     # Tallennetaan havainnot_ja_mallidata -matriisi tiedostoon omalla nimellään
# # #                     tallennettava_matriisi <- paste("station_",station_numbers[station_number_index],"_",analyysiajat[k],"_",ennustusjakso_tunteina,"_season",kausi,"_",selitettavat_muuttujat_ECMWF_koodit[sel],"_level",ECMWF_muuttujat$level_value[match(selitettavat_muuttujat_ECMWF_koodit[sel],ECMWF_muuttujat$muuttuja_EC)],sep="")
# # #                     komento <- paste(tallennettava_matriisi," <- havainnot_ja_mallidata",sep="")
# # #                     eval(parse(text=komento))
# # #                     rm(komento)
# # #                     komento <- paste("write.csv(",tallennettava_matriisi,", file=\"/media/ylhaisi/PURE/R_projects/vorlon_point_data/processed/testisetteja/kesatyontekijalle/data/",tallennettava_matriisi,".csv\")",sep="")
# # #                     eval(parse(text=komento))
# # #                     rm(komento)
# # #                     komento <- paste("rm(",tallennettava_matriisi,")",sep="")
# # #                     eval(parse(text=komento))
# # #                     rm(komento)
# # #                     rm(tallennettava_matriisi)
# #                   }
# #                 }
# #                 
# #                 # MYÖS NÄISSÄ SKRIPTEISSÄ TESTATAAN SISÄLLÄ ONKO DATAA. JOS EI OLE, NIIN AINOASTAAN TALLENNETAAN VIIMEISEN ENNUSTEJAKSON AJANHETKELLÄ KOKO MATRIISIN SISÄLTÖ.
# #                 
# # #                 # Koulutetaan lineaarinen malli ja tallennetaan kertoimet tiedostoon
# # #                 source("Postgre_query_point_data_train_and_save_lm_MOS.R")
# #         
# #                 
# # #                 # NÄITÄ SKRIPTEJÄ VOIDAAN AJAA ILMAN havainnot_ja_mallidata -matriisiakin
# # #         
# #                 # Vertaillaan verifiointistatistiikkoja eri aineistojen välillä ja piirretään niistä kuvia
# #                 source("Postgre_query_point_data_fcst_MOS_verification_all_timesteps_all_stations.R")
# #                 
# #             
# #                 print(j)
# #                 if (exists("havainnot_ja_mallidata")) {
# #                   rm(havainnot_ja_mallidata)
# #                 }
# #                 if (exists("l2")) {
# #                   rm(l2)
# #                 }
# #                 rm(ennustusjakso_tunteina)
# #               }
# #               rm(j)
# #               rm(ennustusjaksot)
# #               
# #               # Ennustusjaksot on käyty läpi. Palautetaan selittäviksi muuttujiksi ne, joille löytyy edes jotain dataa MOS-tietokannasta (ei välttämättä kaikille ennustepituuksille)
# #               selittavat_muuttujat <- selittavat_muuttujat_tietokanta
# #               rm(selittavat_muuttujat_tietokanta)
# #               rm(sarja_rajattu)
# #             }
# #             rm(kausi)
# #           }
# #           rm(k)
# #           
# #           
# #           # Otetaan pois havainto, verifdb- ja mallidatat
# #           if (exists("havainnot")) {
# #             rm(havainnot)
# #           }
# #           temp1=ls()
# #           temp1 <- temp1[grep("^verifdb_",temp1)]
# #           rm(list=temp1)
# #           rm(temp1)
# #         }
# #         
# #         
# #         
# #         #   rm(station_numbers_indices)
# #         #   # Tallennetaan valvontamatriisi, jossa on lueteltu MOS:n kouluttamiseen käytetty data asemittain
# #         #   pvm <- format(as.POSIXct(as.character(Sys.time()),tz="GMT"),"%d%m%Y")
# #         #   komento <- paste("write.csv(station_numbers_data_status, file=\"/media/ylhaisi/PURE/R_projects/vorlon_point_data/processed/MOS_coefficients/keskeneraiset/",mos_label,"/koulutukseen_kaytetty_data/",selitettavat_muuttujat_ECMWF_koodit[sel],"_level",ECMWF_muuttujat$level_value[match(selitettavat_muuttujat_ECMWF_koodit[sel],ECMWF_muuttujat$muuttuja_EC)],"_lm_",mos_label,"_",muuttujajoukko,"_",pvm,".csv\", quote = TRUE)",sep="")
# #         #   eval(parse(text=komento))
# #         #   rm(komento)
# #         #   rm(pvm)
# #         rm(station_numbers_data_status)
# #       }
# #       rm(sel)
# #       
# #       # Poistetaan kaikki mallikama (kaiken mallidatan sisältävä mallidatamatriisi, sekä mahdolliset kerroinmatriisit)
# #       temp1=ls()
# #       komento <- paste("temp1 <- temp1[grep(\"^station_",station_numbers[station_number_index],"\",temp1)]",sep="")
# #       eval(parse(text=komento))
# #       rm(komento)
# #       rm(list=temp1)
# #       rm(temp1)
# #       rm(mallidatamatriisi)
# #     }
# #   }
# #   rm(tallennetut_kertoimet)
# #   rm(tiedostot)
# #   print(i)
# }
# rm(station_number_index)



rm(con1)
rm(con2)
rm(con3)
rm(con4)
rm(drv_psql)
rm(drv_ora)