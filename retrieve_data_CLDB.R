retrieve_data_CLDB <- function(variable_list,station_list_retrieved,timestamps_series) {
  
  # For these variables, the time stamps between the data points are not interpolated
  non_interpolated_observations <- c(21,22,23,24,25,26)
  
  if (sel_CLDB %notin% non_interpolated_observations) {
    interpoloidaanko <- 1
  } else {
    interpoloidaanko <- 0
  }
  
  max_diff_in_minutes <- 30
  
    
  # This script fetches observations from databases. If no aviation observations, the order is
  # weather_data_qc @ Vorlon (CLDB, foreign stations)
  # Observation_data_v1 @ Vorlon (CLDB, Finnish stations)
  
  
  if (onko_lentosaahavaintoja==0) {
    
    # weather_data_qc (Mostly foreign observations but also some finnish ones). Do not fetch finnish station data.
    if (station_id<2700 | station_id>3000) {
      # In view WEATHER_QC (and WEATHER_DATA_QC) flags >=6 are not shown in retrievals. So there's no need to use conditional retrieval of the observations based on "suitable" flags.
      # This query groups results based on even-hour -rounded obstime (even-hour observation is average of all values which are rounded to that even-hour)
      komento <- paste("haku <- dbSendQuery(con1, \"select ((date_trunc('day',obstime) + interval '1 hour' * round( (date_part('hour',obstime) + date_part('minute',obstime)/60) / 1)) || ' UTC') as nearest_1, round(avg(value)::DECIMAL,1) as avg_value from weather_data_qc where fmisid=",fmisid," and parameter='",parameter,"' and obstime>='",format(sarja[1],'%Y-%m-%d %H:%M:%OS'),"' and obstime<='",format(sarja[length(sarja)],'%Y-%m-%d %H:%M:%OS'),"' group by nearest_1 order by nearest_1;\")",sep="")
      # # This query rounds obstime to nearest hour and calculates difference of obstime to it
      # komento <- paste("haku <- dbSendQuery(con1, \"select (obstime || ' UTC') as obstime, ((date_trunc('day',obstime) + interval '1 hour' * round( (date_part('hour',obstime) + date_part('minute',obstime)/60) / 1)) || 'UTC') as nearest_1, (@extract(epoch from(obstime - (date_trunc('day',obstime) + interval '1 hour' * round( (date_part('hour',obstime) + date_part('minute',obstime)/60) / 1))))/60) as abs_diff_in_minutes, value from weather_data_qc where fmisid=",fmisid," and parameter='",parameter,"' and obstime>='",format(sarja[1],'%Y-%m-%d %H:%M:%OS'),"' and obstime<='",format(sarja[length(sarja)],'%Y-%m-%d %H:%M:%OS'),"' and (@extract(epoch from(obstime - (date_trunc('day',obstime) + interval '1 hour' * round( (date_part('hour',obstime) + date_part('minute',obstime)/60) / 1))))/60) < ",max_diff_in_minutes," order by obstime, nearest_1, abs_diff_in_minutes;\")",sep="")
      # # The oldest query, fetching only the data which has 3-hourly hour component to it
      # komento <- paste("haku <- dbSendQuery(con1, \"select (obstime || ' UTC') as obstime, value from weather_data_qc where fmisid=",fmisid," and parameter='",parameter,"' and obstime>='",format(sarja[1],'%Y-%m-%d %H:%M:%OS'),"' and obstime<='",format(sarja[length(sarja)],'%Y-%m-%d %H:%M:%OS'),"' and (extract(hour from obstime) :: bigint) in (0,3,6,9,12,15,18,21) order by obstime;\")",sep="")
      # # This fetches all data
      # komento <- paste("haku <- dbSendQuery(con1, \"select (obstime || ' UTC') as obstime, value from weather_data_qc where fmisid=",fmisid," and parameter='",parameter,"' and obstime>='",format(sarja[1],'%Y-%m-%d %H:%M:%OS'),"' and obstime<='",format(sarja[length(sarja)],'%Y-%m-%d %H:%M:%OS'),"' order by obstime;\")",sep="")
      eval(parse(text=komento))
      rm(komento)
      havainnot <- fetch(haku)
      if (TRUE==dbClearResult(haku)) {
        rm(haku)
      }
      if (dim(havainnot)[1]>0) {
        names(havainnot) <- c("obstime","value")
        
        # Removing missing values (some of these are also contained in the database)
        havainnot <- havainnot[!is.na(havainnot$value),]
        
        
        # Tidying up timestamp and rounding to the nearest hour
        havainnot$obstime <- as.POSIXct(as.character(havainnot$obstime),tz="GMT")
        havainnot$obstime <- as.POSIXct(round.POSIXt(havainnot$obstime, c("hours")))
        # Altering to numeric form
        havainnot$value <- as.numeric(as.character(havainnot$value))
        
        # Removing duplicate timestamps. Mean of all available values is preserved.
        # Temporal sorting of values at first
        havainnot <- havainnot[order(havainnot$obstime),]
        # havainnot <- havainnot[order(havainnot$obstime,havainnot$abs_diff_in_minutes),]
        
        # Primary method (used when diff_time_in_minutes is fetched): Removing all duplicate values for each obstime (while loop has passed, only the obs with minimum abs_diff_in_minutes is left, if e.g. both 20 to and 20 over values are available, to values are left because of obstime-based ordering in sql-query
        while (sum(duplicated(havainnot$obstime))>0) {
          havainnot <- havainnot[-which(duplicated(havainnot$obstime)),]
        }
        #       # Old method (used if no diff_time_in_minutes is fetched): Taking the mean from all duplicate values. Rounding to one decimal precision.
        #       if (sum(duplicated(havainnot$obstime))>0) {
        #         for (l in 2:dim(havainnot)[1]) {
        #           if (l <= dim(havainnot)[1]) {
        #             if (havainnot$obstime[l]==havainnot$obstime[l-1]) {
        #               alkurivi <- (l-1)
        #               loppurivi <- tail(which(havainnot$obstime==havainnot$obstime[l-1]),n=1)
        #               # Assigning mean of all observations to last row. Removing all values prior to last value.
        #               havainnot$value[loppurivi] <- round(mean(havainnot$value[alkurivi:loppurivi],na.rm=T),digits=1)
        #               havainnot$value[(alkurivi:(loppurivi-1))] <- NA
        #               rm(alkurivi)
        #               rm(loppurivi)
        #             }
        #           }
        #         }
        #         rm(l)
        #       }
        # Removing missing values and removing column abs_diff_in_minutes
        if (dim(havainnot)[1]>0) {
          havainnot <- havainnot[!is.na(havainnot$value),c("obstime","value")]
        }
        
        
        
      }
      
    } else {
      # Brainstorm is not used anymore for Finnish stations as the retrieval times are very comparable to those of table observation_data_v1 
      
#       # Brainstorm fmi -> weather_qc (CLDB). Includes both manual and auto-observations. These cannot be distinguished from each other in time series.
#       komento <- paste("temp1 <- as.character(scrape(url=\"http://brainstormgw.fmi.fi/observe?param=utctime,tz,",sel_obsplugin,"&format=ascii&timeformat=sql&separator=,&tz=UTC&stationtype=fmi&starttime=",format(sarja[1],'%Y%m%d%H%M'),"&endtime=",format(sarja[length(sarja)],'%Y%m%d%H%M'),"&timestep=180&fmisid=",fmisid,"\",parse=FALSE))",sep="")
#       eval(parse(text=komento))
#       rm(komento)
#       if (temp1!="") {
#         havainnot <- temp1
#         havainnot <- unlist(strsplit(havainnot,"\n"))
#         havainnot <- unlist(strsplit(havainnot,","))
#         havainnot <- as.data.frame(matrix(havainnot,nrow=(length(havainnot)/3),byrow=T))
#         havainnot[,1] <- paste(havainnot[,1],havainnot[,2])
#         havainnot <- havainnot[,-2]
#         names(havainnot) <- c("obstime","value")
#       }
#       rm(temp1)
      
      # observation_data_v1 (CLDB)
      haettavat_tunnit1 <- paste(sort((unique(as.numeric(format(sarja,"%H")))+24)%%24),collapse=",")
      haettavat_tunnit2 <- paste(sort((unique(as.numeric(format(sarja,"%H")))+24-1)%%24),collapse=",")
      # If the variable of interest is instantaneous temperature
      if (sel_CLDB %in% non_interpolated_observations[1]) {
        # Fetching observations done both at sharp and 20 to observation hours. Some manual stations report 20to observations 19to.
        komento <- paste("haku <- dbSendQuery(con1, \"select (data_time || ' UTC') as obstime, round(AVG(CASE WHEN measurand_id = ",sel_CLDB," THEN data_value END)::DECIMAL,1) AS value from observation_data_v1 where station_id in (",station_idt_conversion$fmisid[which(station_idt_conversion$wmon==previ_ecmos_v_station_id[i])],") and measurand_id in (",sel_CLDB,") and measurand_no=1 and data_time>='",format(sarja[1],'%Y-%m-%d %H:%M:%OS'),"' and data_time<='",format(sarja[length(sarja)],'%Y-%m-%d %H:%M:%OS'),"' and ((extract(hour from data_time) :: bigint) in (",haettavat_tunnit1,") and (extract(minute from data_time) :: bigint) in (0) or (extract(hour from data_time) :: bigint) in (",haettavat_tunnit2,") and (extract(minute from data_time) :: bigint) in (40,41)) group by obstime order by obstime;\")",sep="")
        # # This query below is the old one. It returns the same data as the above query.
        # komento <- paste("haku <- dbSendQuery(con1, \"select (data_time || ' UTC') as obstime, data_value as value from observation_data_v1 where station_id in (",station_idt_conversion$fmisid[which(station_idt_conversion$wmon==previ_ecmos_v_station_id[i])],") and measurand_id in (",sel_CLDB,") and measurand_no=1 and data_time>='",format(sarja[1],'%Y-%m-%d %H:%M:%OS'),"' and data_time<='",format(sarja[length(sarja)],'%Y-%m-%d %H:%M:%OS'),"' and ((extract(hour from data_time) :: bigint) in (",haettavat_tunnit1,") and (extract(minute from data_time) :: bigint) in (0) or (extract(hour from data_time) :: bigint) in (",haettavat_tunnit2,") and (extract(minute from data_time) :: bigint) in (40,41)) order by obstime, data_value;\")",sep="")
      }
      # If the variable of interest is not interpolated (is not observed for every hour)
      if (sel_CLDB %in% non_interpolated_observations[-1]) {
        # This query groups values for every hour by taking the average of all values of that specific hour
        komento <- paste("haku <- dbSendQuery(con1, \"select (data_time || ' UTC') as obstime, round(AVG(CASE WHEN measurand_id = ",sel_CLDB," THEN data_value END)::DECIMAL,1) AS value from observation_data_v1 where station_id in (",station_idt_conversion$fmisid[which(station_idt_conversion$wmon==previ_ecmos_v_station_id[i])],") and measurand_id in (",sel_CLDB,") and measurand_no=1 and data_time>='",format(sarja[1],'%Y-%m-%d %H:%M:%OS'),"' and data_time<='",format(sarja[length(sarja)],'%Y-%m-%d %H:%M:%OS'),"' and (extract(minute from data_time) :: bigint) in (0) group by obstime order by obstime;\")",sep="")
      }
      # If the variable of interest can be interpolated
      if (sel_CLDB %notin% non_interpolated_observations) {
        # This query groups values for every hour by taking the average of all values of that specific hour
        komento <- paste("haku <- dbSendQuery(con1, \"select (data_time || ' UTC') as obstime, round(AVG(CASE WHEN measurand_id = ",sel_CLDB," THEN data_value END)::DECIMAL,1) AS value from observation_data_v1 where station_id in (",station_idt_conversion$fmisid[which(station_idt_conversion$wmon==previ_ecmos_v_station_id[i])],") and measurand_id in (",sel_CLDB,") and measurand_no=1 and data_time>='",format(sarja[1],'%Y-%m-%d %H:%M:%OS'),"' and data_time<='",format(sarja[length(sarja)],'%Y-%m-%d %H:%M:%OS'),"' and (extract(minute from data_time) :: bigint) in (0) group by obstime order by obstime;\")",sep="")
        # # This query fetches the minimum value of the instantaneous values of the previous hour (these do not capture actual observed minimums as instantaneous values are sampled only every 10mins in the database) So: DON'T USE THIS!
        # komento <- paste("haku <- dbSendQuery(con1, \"select ((date_trunc('day',data_time) + interval '1 hour' * ceil( (date_part('hour',data_time) + date_part('minute',data_time)/60) / 1)) || ' UTC') as obstime, round(min(data_value)::DECIMAL,1) AS value from observation_data_v1 where station_id in (",station_idt_conversion$fmisid[which(station_idt_conversion$wmon==previ_ecmos_v_station_id[i])],") and measurand_id in (1) and measurand_no=1 and data_time>='",format(sarja[1],'%Y-%m-%d %H:%M:%OS'),"' and data_time<='",format(sarja[length(sarja)],'%Y-%m-%d %H:%M:%OS'),"' group by obstime order by obstime;\")",sep="")
        # # This query fetches the minimum value of the minimum hourly values of the same day (1801UTC ... 1800UTC). This NEATLY corresponds to daily min values in MOST cases. So: USE THIS IF YOU WANT!
        # komento <- paste("haku <- dbSendQuery(con1, \"select ((date_trunc('day',data_time) + interval '1 hour' * ceil( (date_part('hour',data_time) + date_part('minute',data_time)/60 - 18.0166) / 24)*24) || ' UTC') as obstime, round(min(data_value)::DECIMAL,1) AS value from observation_data_v1 where station_id in (",station_idt_conversion$fmisid[which(station_idt_conversion$wmon==previ_ecmos_v_station_id[i])],") and measurand_id in (",sel_CLDB,") and measurand_no=1 and data_time>='",format(sarja[1],'%Y-%m-%d %H:%M:%OS'),"' and data_time<='",format(sarja[length(sarja)],'%Y-%m-%d %H:%M:%OS'),"' group by obstime order by obstime;\")",sep="")
        # # This query fetches the maximum value of the instantaneous values of the previous hour (these do not capture actual observed maximums as instantaneous values are sampled only every 10mins in the database) So: DON'T USE THIS!
        # komento <- paste("haku <- dbSendQuery(con1, \"select ((date_trunc('day',data_time) + interval '1 hour' * ceil( (date_part('hour',data_time) + date_part('minute',data_time)/60) / 1)) || ' UTC') as obstime, round(max(data_value)::DECIMAL,1) AS value from observation_data_v1 where station_id in (",station_idt_conversion$fmisid[which(station_idt_conversion$wmon==previ_ecmos_v_station_id[i])],") and measurand_id in (1) and measurand_no=1 and data_time>='",format(sarja[1],'%Y-%m-%d %H:%M:%OS'),"' and data_time<='",format(sarja[length(sarja)],'%Y-%m-%d %H:%M:%OS'),"' group by obstime order by obstime;\")",sep="")
        # # This query fetches the maximum value of the maximum hourly values of the same day (1801UTC ... 1800UTC). This NEATLY corresponds to daily max values in MOST cases. So: USE THIS IF YOU WANT!
        # komento <- paste("haku <- dbSendQuery(con1, \"select ((date_trunc('day',data_time) + interval '1 hour' * ceil( (date_part('hour',data_time) + date_part('minute',data_time)/60 - 18.0166) / 24)*24) || ' UTC') as obstime, round(max(data_value)::DECIMAL,1) AS value from observation_data_v1 where station_id in (",station_idt_conversion$fmisid[which(station_idt_conversion$wmon==previ_ecmos_v_station_id[i])],") and measurand_id in (",sel_CLDB,") and measurand_no=1 and data_time>='",format(sarja[1],'%Y-%m-%d %H:%M:%OS'),"' and data_time<='",format(sarja[length(sarja)],'%Y-%m-%d %H:%M:%OS'),"' group by obstime order by obstime;\")",sep="")
      }
      rm(haettavat_tunnit1)
      rm(haettavat_tunnit2)
      eval(parse(text=komento))
      rm(komento)
      komento <- paste("havainnot <- fetch(haku)",sep="")
      eval(parse(text=komento))
      rm(komento)
      if (TRUE==dbClearResult(haku)) {
        rm(haku)
      }
      # Removing missing values
      if (!length(which(havainnot$value=="nan"))==FALSE) {
        havainnot <- havainnot[-which(havainnot$value=="nan"),]
      }
      if (!length(which(havainnot$value=="NaN"))==FALSE) {
        havainnot <- havainnot[-which(havainnot$value=="NaN"),]
      }
      # Tidying up timestamp and rounding to the nearest hour
      havainnot$obstime <- as.POSIXct(as.character(havainnot$obstime),tz="GMT")
      havainnot$obstime <- as.POSIXct(round.POSIXt(havainnot$obstime, c("hours")))
      # Altering to numeric form
      havainnot$value <- as.numeric(as.character(havainnot$value))
      # Removing those 20 and 19 to values which have also their equivalent observation made at sharp (observations are temporally ordered)
      if (sum(duplicated(havainnot$obstime))>0) {
        havainnot <- havainnot[-(which(duplicated(havainnot$obstime))-1),]
      }
      if (sum(duplicated(havainnot$obstime))>0) {
        havainnot <- havainnot[-(which(duplicated(havainnot$obstime))-1),]
      }
    }
    
  } else {
    havainnot <- as.data.frame(matrix(NA,0,0))
    # Haetaan Teron AQU-tietokannasta METAR-havaintoja lentokentille ja tallennetaan tämä tieto muuttujaan havainnot
    # HUOM! MessagetypeId=1 ottaa huomioon vain manuaaliset METARIT, auto-METARIt ovat arvolla 8!
    komento <- paste("temp1 <- as.character(scrape(url=\"http://lentosaa.fmi.fi/nordictafverif/tafverif/aqu.php?stationId=",station_id,"&messageTypeId=1&startTime=",format(sarja[1],'%Y%m%d%H%M'),"&endTime=",format(sarja[length(sarja)],'%Y%m%d%H%M'),"&paramId=",sel_AQU,"&timeStep=180&missingValue=NaN\",parse=FALSE))",sep="")
    eval(parse(text=komento))
    rm(komento)
    if (temp1!="\n") {
      havainnot <- temp1
      havainnot <- unlist(strsplit(havainnot,"\n"))
      havainnot <- unlist(strsplit(havainnot,";"))
      havainnot <- as.data.frame(matrix(havainnot,nrow=(length(havainnot)/2),byrow=T))
      havainnot[,1] <- paste(havainnot[,1]," UTC")
      names(havainnot) <- c("obstime","value")
      if (!length(which(havainnot$value=="nan"))==FALSE) {
        havainnot <- havainnot[-which(havainnot$value=="nan"),]
      }
      if (!length(which(havainnot$value=="NaN"))==FALSE) {
        havainnot <- havainnot[-which(havainnot$value=="NaN"),]
      }
    }
    rm(temp1)
  }
  
  
  # Tähän asti havaintoja voidaan hakea, vaikkei niitä tietokannassa olisikaan. Jos aseman osalta ei ole ollenkaan havaintoja, skipataan skriptin jatkokäsittely.
  if (dim(havainnot)[1]!=0) {
#     # Removing values which do not correspond to sarja
#     if (length(which(is.na(match(havainnot$obstime,sarja))))>0) {
#       havainnot <- havainnot[-(which(is.na(match(havainnot$obstime,sarja)))),]
#     }
    # Converting to Kelvins
    if (onko_lentosaahavaintoja==0 & (sel_CLDB %in% which(CLDB_kaikki_muuttujat$measurand_code=="TA"))) {
      havainnot$value <- havainnot$value +273.15
    }
    
    
    
    
    if (interpoloidaanko==1) {
      ######### INTERPOLOIDAAN VÄLISTÄ PUUTTUVIA HAVAINTOJA YKSINKERTAISELLA KESKIARVOISTUSMENETELMÄLLÄ ###########
    
      # Lasketaan saatavilla olevien havaintojen keskimääräinen lukumäärä per päivä (niille päiville jolloin edes jotain havaintoja on tehty.)
      havaintodata_paivastampit <- havainnot
      havaintodata_paivastampit$obstime <- as.POSIXct(trunc.POSIXt(havaintodata_paivastampit$obstime, units=c("days")))
      havaintodata_paivastampit <- na.omit(havaintodata_paivastampit)
      if (dim(havaintodata_paivastampit)[1]>0) {
        havaintojen_lukumaara_paivassa <- aggregate(data.frame(count=havaintodata_paivastampit$obstime),list(value=havaintodata_paivastampit$obstime),length)
        havaintojen_lukumaara_paivassa  <- round(mean(havaintojen_lukumaara_paivassa$count),digits=2)
      }
      rm(havaintodata_paivastampit)
      
      # Koska alla sallitaan kuuden tunnin mittainen pätkä havaintoja, täytyy asemalla tahdä vähintään neljä havaintoa päivässä. Joinain päivinä havaintoja tottakai puuttuu, joten keskiarvoistettuna havaintoja on vähemmän
      
      if (havaintojen_lukumaara_paivassa > 3) {
    #     jape <- havainnot
    #     jape <- jape[-(550:650),]
    #     jape <- jape[-(2550:2650),]
    #     jape <- jape[-(1820:1880),]
    #     jape <- jape[-(555:556),]
    #     jape <- jape[-(900),]
    #     jape <- jape[-(2:3),]
    #     jape <- jape[-(12),]
    #     jape <- jape[-(32),]
        
  #       # VANHA ALGORITMI: TÄSSÄ HYÄDYNNETÄÄN AINOASTAAN NIITÄ HAVAINTOJA, JOTKA OSUVAT YKSIIN MUUTTUJAN sarja KANSSA.
  #       nama_sarja_alkiot_loytyvat <- !is.na(match(sarja,havainnot$obstime))
  #       temp1 <- rle(nama_sarja_alkiot_loytyvat)
  #       
  #       # Lisätään väleihin ainoastaan yksittäisiä puuttuvia arvoja (jotka interpoloidaan lineaarisesti ympäröivien arvojen avulla)
  #       lisattavat_alkiot <- which(temp1$lengths==1 & temp1$values==FALSE)
  #       # Jos aikasarjan alussa tai lopussa on puuttuvia arvo, ei käsitellä niitä
  #       lisattavat_alkiot <- lisattavat_alkiot[which(lisattavat_alkiot!=1)]
  #       lisattavat_alkiot <- lisattavat_alkiot[which(lisattavat_alkiot!=length(temp1$lengths))]
  #       
  #       if (length(lisattavat_alkiot) > 0) {
  #         for (lisattava in length(lisattavat_alkiot):1) {
  #           alun_viimeinen <- tail(which((havainnot$obstime<sarja[sum(temp1$lengths[1:(lisattavat_alkiot[lisattava])])])==TRUE),1)
  #           lopun_ensimmainen <- alun_viimeinen + 1
  #           interpoloitava <- havainnot[alun_viimeinen,]
  #           interpoloitava[,1] <- as.POSIXct(round.POSIXt(sarja[sum(temp1$lengths[1:(lisattavat_alkiot[lisattava])])]),tz="GMT")
  #           interpoloitava[,2] <- mean(havainnot$value[alun_viimeinen:lopun_ensimmainen])
  #           havainnot <- rbind(havainnot[(1:alun_viimeinen),],interpoloitava,havainnot[(lopun_ensimmainen:dim(havainnot)[1]),] )
  #           rm(alun_viimeinen)
  #           rm(lopun_ensimmainen)
  #           rm(interpoloitava)
  #         }
  #         rm(lisattava)
  #       }
  #       rm(lisattavat_alkiot)
  #       rm(nama_sarja_alkiot_loytyvat)
  #       rm(temp1)
        
        
        
        # UUSI ALGORITMI: TÄMÄ HYÖDYNTÄÄ KAIKKEA MAHDOLLISTA DATAA JA TEKEE INTERPOLOINNIN JOKA TUNNILLE
        
        alku <- as.POSIXct(format(sarja[1],"%Y-%m-%d %H:%M:%OS"),tz="GMT")
        loppu <- as.POSIXct(format(sarja[length(sarja)],"%Y-%m-%d %H:%M:%OS"),tz="GMT")
        sarja_tunneittain <- seq(alku, loppu, by="1 hour")
        rm(alku)
        rm(loppu)
        
        # Nyt kun havaintodata on pyöristetty tunnin tarkkuuteen, pitäisi jokaiselle havaintodatan alkiolle löytyä vastaava arvo sarja_tunneittain -matriisista (koska haut tehdään sarja-matriisin perusteella)
        if (sum(is.na(match(havainnot$obstime,sarja_tunneittain)))==0) {
          
          
          # Ensin luodaan tyhja matriisi, jossa aikasarja on tunnin resoluutiolla ja kopioidaan siihen alkiot jotka löytyvät oikeastikin
          havainnot_tunneittain <- sarja_tunneittain
          havainnot_tunneittain <- cbind(havainnot_tunneittain,as.data.frame(matrix(NA,length(havainnot_tunneittain))))
          names(havainnot_tunneittain) <- c("obstime","value")
          havainnot_tunneittain[match(havainnot$obstime,havainnot_tunneittain$obstime),] <- havainnot
          
          nama_sarja_alkiot_loytyvat <- !is.na(match(sarja_tunneittain,havainnot$obstime))
          temp1 <- rle(nama_sarja_alkiot_loytyvat)
          
          # Yli kuuden tunnin mittaisia gappeja ei interpoloida, puuttuvat arvot interpoloidaan lineaarisesti ympäröivien arvojen avulla.
          lisattavat_alkiot <- which(temp1$lengths<6 & temp1$values==FALSE)
          # Jos aikasarjan alussa tai lopussa on puuttuvia arvo, ei käsitellä niitä
          lisattavat_alkiot <- lisattavat_alkiot[which(lisattavat_alkiot!=1)]
          lisattavat_alkiot <- lisattavat_alkiot[which(lisattavat_alkiot!=length(temp1$lengths))]
          
          if (length(lisattavat_alkiot) > 0) {
            for (lisattava in length(lisattavat_alkiot):1) {
              gapin_ensimmainen <- sum(temp1$lengths[1:(lisattavat_alkiot[lisattava]-1)])+1
              gapin_viimeinen <- sum(temp1$lengths[1:(lisattavat_alkiot[lisattava])])
              # Tästä välistä sitten interpoloidaan puuttuvat arvot. Otetaan yksinkertainen lineaarinen keskiarvo ensimmäisestä ja viimeisestä arvosta.
              for (interpoloitava in (gapin_ensimmainen:gapin_viimeinen)) {
                koko_etaisyys <- (gapin_viimeinen-gapin_ensimmainen)+2
                alku_etaisyys <- interpoloitava-gapin_ensimmainen+1
                loppu_etaisyys <- gapin_viimeinen-interpoloitava+1
                # Painotetaan havaintoja sen mukaan, kuinka kaukana päistä ne ovat.
                havainnot_tunneittain$value[interpoloitava] <- round((loppu_etaisyys/koko_etaisyys)*havainnot_tunneittain$value[(gapin_ensimmainen-1)]+(alku_etaisyys/koko_etaisyys)*havainnot_tunneittain$value[(gapin_viimeinen+1)],digits=2)
                rm(koko_etaisyys)
                rm(alku_etaisyys)
                rm(loppu_etaisyys)
              }
              rm(interpoloitava)
              rm(gapin_ensimmainen)
              rm(gapin_viimeinen)
            }
            rm(lisattava)
          }
          rm(lisattavat_alkiot)
          rm(nama_sarja_alkiot_loytyvat)
          rm(temp1)
          
          havainnot <- havainnot_tunneittain
          rm(havainnot_tunneittain)
          rm(sarja_tunneittain)
          
          # Kavennetaan havainnot-vektoria sarja-vektorissa olevan aikaresoluution perusteella
          havainnot <- havainnot[match(sarja,havainnot$obstime),]
          # Poistetaan puuttuvat arvot
          havainnot <- havainnot[!is.na(havainnot$value),]
        }
      }
      rm(havaintojen_lukumaara_paivassa)
    }
    
    # If 12min values, then calculate these...
    
    # Siistitään havaintomatriisin sarakkeet kunnolliseen muotoon
    havainnot$obstime <- as.POSIXct(as.character(havainnot$obstime),tz="GMT")
    # Pyöristetään havainnot lähimpään tuntiin.
    havainnot$obstime <- as.POSIXct(round.POSIXt(havainnot$obstime, c("hours")))
    # Muutetaan varmuuden vuoksi vielä numeeriseen muotoon (Brainstormista haettavat eivät sitä ole)
    havainnot$value <- as.numeric(as.character((havainnot$value)))
    # Pyöristetään havainnot yhden desimaalin tarkkuuteen
    havainnot$value <- round(havainnot$value,digits=1)
  }

  rm(non_interpolated_observations)
  
  havainnot
}