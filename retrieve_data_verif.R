### Ladataan verif-kannasta dataa sisään siinä järjestyksessä kun aineistot ovat verif_aineistot -vektorissa ###

# if (onko_lentosaahavaintoja==0) {
#   for (l in 1:length(verif_aineistot)) {        
#     komento <- paste("haku <- dbSendQuery(con2, \"select count (relname) as a from pg_class where relname like('",verif_aineistot[l],"_",previ_ecmos_v_station_id[i],"_1');\")",sep="")
#     eval(parse(text=komento))
#     rm(komento)
#     komento <- paste("verifdb_",verif_aineistot[l],"_pituus <- fetch(haku)",sep="")
#     eval(parse(text=komento))
#     rm(komento)
#     
#     if (TRUE==dbClearResult(haku)) {
#       rm(haku)
#     }
#     if (eval(parse(text=(paste("verifdb_",verif_aineistot[l],"_pituus$a[1]>0",sep=""))))) {
#       # komento <- paste("haku <- dbSendQuery(con2, \"select (date || ' UTC') as date, atime, extract(hour from date_trunc('hour', fdate + interval '30 minute')) as saapumistunti, leadtime, paramvalue from ",verif_aineistot[l],"_",previ_ecmos_v_station_id[i],"_1 where date>'2011-11-30 23:00:00' and paramid='",selitettavat_muuttujat_verif_koodit[sel],"' and atime='",analyysiajat[k],"' order by date;\")",sep="")
#       # komento <- paste("haku <- dbSendQuery(con2, \"select (date || ' UTC') as date, atime, extract(hour from date_trunc('hour', fdate + interval '30 minute')) as saapumistunti, leadtime, paramvalue from ",verif_aineistot[l],"_",previ_ecmos_v_station_id[i],"_1 where date>'2011-11-30 23:00:00' and paramid='",selitettavat_muuttujat_verif_koodit[sel],"' order by date;\")",sep="")
#       komento <- paste("haku <- dbSendQuery(con2, \"select (date || ' UTC') as date, atime, fdate, ROUND(((EXTRACT(EPOCH FROM fdate AT TIME ZONE 'UTC') - 3600 * atime)::INTEGER % 86400) / 3600.0) as leadtime_add, leadtime, paramvalue from ",verif_aineistot[l],"_",previ_ecmos_v_station_id[i],"_1 where date>='",format(sarja[1],'%Y-%m-%d %H:%M:%OS'),"' and date <='",format(sarja[length(sarja)],'%Y-%m-%d %H:%M:%OS'),"' and paramid='",selitettavat_muuttujat_verif_koodit[sel],"' order by date;\")",sep="")
#       
#       eval(parse(text=komento))
#       rm(komento)
#       komento <- paste("verifdb_",verif_aineistot[l]," <- fetch(haku)",sep="")
#       eval(parse(text=komento))
#       rm(komento)
#       if (TRUE==dbClearResult(haku)) {
#         rm(haku)
#       }
#       if (eval(parse(text=(paste("!length(verifdb_",verif_aineistot[l],")==FALSE",sep=""))))) {
#         
#         # Ensin muutetaan aikaleima POSIXct-formaattiin
#         komento <- paste("verifdb_",verif_aineistot[l],"$date <- as.POSIXct(as.character(verifdb_",verif_aineistot[l],"$date),tz=\"GMT\")",sep="")
#         eval(parse(text=komento))
#         rm(komento)
#         
#         # leadtime_add on analyysihetken ja lähetys/saapumishetken välinen ero abs(fdate-atime). Tämä pitää siis lisätä leadtimeen, jotta saadaan todelliset ennustetunnit.
#         komento <- paste("verifdb_",verif_aineistot[l],"$leadtime <- verifdb_",verif_aineistot[l],"$leadtime + verifdb_",verif_aineistot[l],"$leadtime_add",sep="")
#         eval(parse(text=komento))
#         rm(komento)
#         # Otetaan pois leadtime_add
#         komento <- paste("verifdb_",verif_aineistot[l]," <- verifdb_",verif_aineistot[l],"[,-which(names(verifdb_",verif_aineistot[l],")==\"leadtime_add\")]",sep="")
#         eval(parse(text=komento))
#         rm(komento)
#         
#         # Saapumisaikaperuste editoidulle datalle laitetaan vertailukelpoiseksi mallien kanssa:
#         # analyysitunteja c(00,06,12,18) edeltävän viiden tunnin aikana olevat analyysitunnit asetetaan näille pääanalyysiajoille, ennustepituuksia lyhennetään yhtä paljon kuin analyysitunteja siirretään eteenpäin.
#         if (length(grep("pal",verif_aineistot[l]))>0) {
#           verifdb_pal$leadtime[which(verifdb_pal$atime %in% c(19,20,21,22,23))] <- verifdb_pal$leadtime[which(verifdb_pal$atime %in% c(19,20,21,22,23))] - abs(verifdb_pal$atime[which(verifdb_pal$atime %in% c(19,20,21,22,23))]-24)
#           verifdb_pal$leadtime[which(verifdb_pal$atime %in% c(1,2,3,4,5))] <- verifdb_pal$leadtime[which(verifdb_pal$atime %in% c(1,2,3,4,5))] - abs(verifdb_pal$atime[which(verifdb_pal$atime %in% c(1,2,3,4,5))]-6)
#           verifdb_pal$leadtime[which(verifdb_pal$atime %in% c(7,8,9,10,11))] <- verifdb_pal$leadtime[which(verifdb_pal$atime %in% c(7,8,9,10,11))] - abs(verifdb_pal$atime[which(verifdb_pal$atime %in% c(7,8,9,10,11))]-12)
#           verifdb_pal$leadtime[which(verifdb_pal$atime %in% c(13,14,15,16,17))] <- verifdb_pal$leadtime[which(verifdb_pal$atime %in% c(13,14,15,16,17))] - abs(verifdb_pal$atime[which(verifdb_pal$atime %in% c(13,14,15,16,17))]-18)
#           verifdb_pal$atime[which(verifdb_pal$atime %in% c(19,20,21,22,23))] <- 0
#           verifdb_pal$atime[which(verifdb_pal$atime %in% c(1,2,3,4,5))] <- 6
#           verifdb_pal$atime[which(verifdb_pal$atime %in% c(7,8,9,10,11))] <- 12
#           verifdb_pal$atime[which(verifdb_pal$atime %in% c(13,14,15,16,17))] <- 18
#         }
#         
#         # ROUND((... -sql haku pyöristää leadtime_add -arvot ylöspäin vaikka pitäisi pyöristää alaspäin (fdate-atime -erotus on näissä tapauksissa systemaattisesti tunnin suurempi). Vähennetään tällaisista ennustepituuksista yksi tunti, jolloin saadaan kaikille ennustepituuksille (pl. <24h ennusteet, jotka tehdään joka tunnille) kolmella jaollisia ennustepituuksia tunteina.
#         komento <- paste("verifdb_",verif_aineistot[l],"$leadtime[which(as.numeric(format(verifdb_",verif_aineistot[l],"$fdate,\"%M\"))==30)] <- (verifdb_",verif_aineistot[l],"$leadtime[which(as.numeric(format(verifdb_",verif_aineistot[l],"$fdate,\"%M\"))==30)] - 1)",sep="")
#         eval(parse(text=komento))
#         rm(komento)
#         
#         # Sortataan aika-, analyysitunti- ja ennustepituusjärjestykseen.
#         komento <- paste("verifdb_",verif_aineistot[l]," <- verifdb_",verif_aineistot[l],"[order(verifdb_",verif_aineistot[l],"$date,verifdb_",verif_aineistot[l],"$atime,verifdb_",verif_aineistot[l],"$leadtime,verifdb_",verif_aineistot[l],"$fdate),]",sep="")
#         eval(parse(text=komento))
#         rm(komento)
#         
#         
#         # Joillekin havaintohetkille voi löytyä duplikaattiarvoja, etenkin pal-datalle. Jätetään jäljelle tuorein ennuste (alimpana samanlaisten alkioiden joukossa)
#         komento <- paste("duplikaatit <- which(duplicated(verifdb_",verif_aineistot[l],"[,c(\"date\",\"atime\",\"leadtime\")]))",sep="")
#         eval(parse(text=komento))
#         rm(komento)
#         # Koska
#         # 1) data on sortattu järjestykseen duplikaatit-matriisissa määritettyjen kriterien perusteella
#         # 2) duplicated-funktio ei palauta sellaisia duplikaatteja, jotka esiintyvät matriisissa ensimmäisen kerran
#         # voidaan duplikaatit poistaa näinkin helposti
#         if (length(duplikaatit)>0) {
#           duplikaatit <- duplikaatit-1
#           komento <- paste("verifdb_",verif_aineistot[l]," <- verifdb_",verif_aineistot[l],"[-duplikaatit,]",sep="")
#           eval(parse(text=komento))
#           rm(komento)
#         }
#         rm(duplikaatit)
#         
#         # Otetaan negatiiviset ennustepituudet pois (näitäkin voi löytyä, etenkin pal-datalle)
#         komento <- paste("negatiiviset_pituudet <- which(verifdb_",verif_aineistot[l],"$leadtime<0)",sep="")
#         eval(parse(text=komento))
#         rm(komento)
#         if (length(negatiiviset_pituudet)>0) {
#           komento <- paste("verifdb_",verif_aineistot[l]," <- verifdb_",verif_aineistot[l],"[-negatiiviset_pituudet,]",sep="")
#           eval(parse(text=komento))
#           rm(komento)
#         }
#         rm(negatiiviset_pituudet)
#         
#         
#         
#         # Otetaan pois sarake fdate
#         komento <- paste("verifdb_",verif_aineistot[l]," <- verifdb_",verif_aineistot[l],"[,-which(names(verifdb_",verif_aineistot[l],")==\"fdate\")]",sep="")
#         eval(parse(text=komento))
#         rm(komento)
#         
#         # Muutetaan ennusteen aikaleima GMT-aikavyöhykkeelle
#         komento <- paste("verifdb_",verif_aineistot[l],"$date <- as.POSIXct(as.character(verifdb_",verif_aineistot[l],"$date),tz=\"GMT\")",sep="")
#         eval(parse(text=komento))
#         rm(komento)
#         # Pyöristetään havainnot lähimpään tuntiin.
#         komento <- paste("verifdb_",verif_aineistot[l],"$date <- as.POSIXct(round.POSIXt(verifdb_",verif_aineistot[l],"$date, c(\"hours\")))",sep="")
#         eval(parse(text=komento))
#         rm(komento)
#         # Muutetaan varmuuden vuoksi vielä numeeriseen muotoon
#         komento <- paste("verifdb_",verif_aineistot[l],"$paramvalue <- as.numeric(as.character(verifdb_",verif_aineistot[l],"$paramvalue))",sep="")
#         eval(parse(text=komento))
#         rm(komento)
#         
#         # Jos on kyse lämpötilasta, muutetaan Kelvineiksi
#         if (sel==1) {
#           komento <- paste("verifdb_",verif_aineistot[l],"$paramvalue <- verifdb_",verif_aineistot[l],"$paramvalue + 273.15",sep="")
#           eval(parse(text=komento))
#           rm(komento)
#         }
#       } else {
#         komento <- paste("rm(verifdb_",verif_aineistot[l],")",sep="")
#         eval(parse(text=komento))
#         rm(komento)
#       }
#     }
#     komento <- paste("rm(verifdb_",verif_aineistot[l],"_pituus)",sep="")
#     eval(parse(text=komento))
#     rm(komento)
#   }
#   rm(l)
# }