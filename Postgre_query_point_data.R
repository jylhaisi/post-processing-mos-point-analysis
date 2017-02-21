# This script initializes interfaces to various databases, can be used to read in data and in MOS training/analysis.
rm(list=ls())
#aa
# # Käytetäänkö lentosäähavaintoja vai jotain muuta
# onko_lentosaahavaintoja=0
# Kuinka monta ennuste-havaintoparia täytyy olla tilastollisten mallien fittaamiseen
ennuste_havaintoparien_minimimaara <- 100 # Normaalisti 100 mutta nyt vain 30 kun on pal-data mukana
# Välttämättömät asema- ja muuttujalistat yms.
source("load_libraries_tables_and_open_connections.R")

# ASEMAJOUKKO
# # Eurooppalaiset asemat
# previ_ecmos_v_station_id <- station_idt_conversion$wmon
# asemajoukko <- paste("kaikki",sep="")
# previ_ecmos_v_station_id <- station_idt_conversion$wmon
# previ_ecmos_v_station_id <- previ_ecmos_v_station_id[seq(1,length(previ_ecmos_v_station_id),by=2)]
# asemajoukko <- paste("kaikki_puolet",sep="")
# previ_ecmos_v_station_id <- asemalista_lehtisaa$wmon
# asemajoukko <- paste("lehtisaa",sep="")
# previ_ecmos_v_station_id <- asemalista_rannikko$wmon
# asemajoukko <- paste("rannikko",sep="")
# previ_ecmos_v_station_id <- asemalista_sisamaa$wmon
# asemajoukko <- paste("sisamaa",sep="")
# previ_ecmos_v_station_id <- asemalista_MOSulkopuoliset1$wmon
# asemajoukko <- paste("MOSulkopuoliset1",sep="")
# previ_ecmos_v_station_id <- asemalista_MOSulkopuoliset2$wmon
# asemajoukko <- paste("MOSulkopuoliset2",sep="")
# previ_ecmos_v_station_id <- asemalista_MOSulkopuoliset3$wmon
# asemajoukko <- paste("MOSulkopuoliset3",sep="")
# previ_ecmos_v_station_id <- asemalista_withheldMOS$wmon
# asemajoukko <- paste("withheldMOS",sep="")
# previ_ecmos_v_station_id <- asemalista_vuoristoasemat$wmon
# asemajoukko <- paste("vuoristoasemat",sep="")
# previ_ecmos_v_station_id <- asemalista_lehtisaa_ja_suomalaiset$wmon
# asemajoukko <- paste("lehtisaa_ja_suomalaiset",sep="")

# # Suomalaiset asemat
# previ_ecmos_v_station_id <- asemalista_suomalaiset$wmon
# asemajoukko <- paste("suomalaiset",sep="")
# previ_ecmos_v_station_id <- asemalista_suomalaiset2$wmon
# asemajoukko <- paste("suomalaiset2",sep="")
# previ_ecmos_v_station_id <- asemalista_suomalaiset3$wmon
# asemajoukko <- paste("suomalaiset3",sep="")
# previ_ecmos_v_station_id <- asemalista_suomalaiset4$wmon
# asemajoukko <- paste("suomalaiset4",sep="")
# previ_ecmos_v_station_id <- asemalista_suomalaiset_lentokentat$wmon
# asemajoukko <- paste("suomalaiset_lentokentat",sep="")
  
  
  
# Käytetyn MOS-mallin versio
mos_label <- paste("MOS_ECMWF_250416")
# mos_label <- paste("MOS_ECMWF_040416")
# mos_label <- paste("MOS_ECMWF_290216")
# mos_label <- paste("MOS_ECMWF_9a_281215",sep="")
# mos_label <- paste("MOS_ECMWF_r144",sep="")

# VALITAAN MUUTTUJAT (TARKEMMAT MUUTTUJAJOUKKO-LISTAT ITSE SKRIPTISSÄ)
muuttujajoukko <- paste("allmodelvars_1prec_noZW_noSD",sep="")
source("choose_variables.R")

for (sel in c(1)) { # :length(selitettavat_muuttujat)) {    # :length(selitettavat_muuttujat)) { # (s in 1:length(selitettavat_muuttujat))
  
  # Luodaan matriisi, jonne tallennetaan tieto, mikä on ko. havaintoaseman tilanne MOS-kertoimia tallennettaessa
  previ_ecmos_v_station_id_data_status <- previ_ecmos_v_station_id
  previ_ecmos_v_station_id_data_status <- cbind(previ_ecmos_v_station_id_data_status, as.data.frame(matrix(0,ncol=35,nrow=length(previ_ecmos_v_station_id_data_status))))
  names(previ_ecmos_v_station_id_data_status) <- c("wmon","havaintojen_pituus","havaintoja_paivassa","mallidatan_pituus","season1_00_ennustejaksoja","season1_00_min_otoskoko","season1_00_max_otoskoko","season1_00_mean_otoskoko","season1_12_ennustejaksoja","season1_12_min_otoskoko","season1_12_max_otoskoko","season1_12_mean_otoskoko","season2_00_ennustejaksoja","season2_00_min_otoskoko","season2_00_max_otoskoko","season2_00_mean_otoskoko","season2_12_ennustejaksoja","season2_12_min_otoskoko","season2_12_max_otoskoko","season2_12_mean_otoskoko","season3_00_ennustejaksoja","season3_00_min_otoskoko","season3_00_max_otoskoko","season3_00_mean_otoskoko","season3_12_ennustejaksoja","season3_12_min_otoskoko","season3_12_max_otoskoko","season3_12_mean_otoskoko","season4_00_ennustejaksoja","season4_00_min_otoskoko","season4_00_max_otoskoko","season4_00_mean_otoskoko","season4_12_ennustejaksoja","season4_12_min_otoskoko","season4_12_max_otoskoko","season4_12_mean_otoskoko")
  havaintoasemat <- c(1:length(previ_ecmos_v_station_id)) # c(1:length(previ_ecmos_v_station_id))
  # havaintoasemat <- c(559:561)
  # havaintoasemat <- c(383,2839,4993,1394) # Ulkomaiset asemat (järjestyksessä Smolensk, Tenerife Sur, Funchal)
  # havaintoasemat <- c(655,619,572,501,634) # Suomalaiset asemat (järjestyksessä Kumpula, Jyväskylä, Sodankylä, Jomala, Lappeenranta)
  # havaintoasemat <- c(15:17,655,634,776) #:778,1099:1101)
  for (i in 1:length(havaintoasemat)) { #for (i in c(5,640,2861)) # 1:length(havaintoasemat)) {   # havaintoasemat) { # 7:length(previ_ecmos_v_station_id)) { # c(663,2906,5108)) { # c(663,2906,1444)) { (i in c(663,2906,5108)) { # Nyt haettavina asemina ovat "2460",114369,390,"Stockholm-Arlanda", "3839",114639,795,"Exeter Airport" ja "27612",117549,2980,"Moskva"              647 on Hki-Vantaan lentoasema  Kaisaniemi, Jyväskylä ja Sodankylä: c(649,624,576) ## kaikki mallipisteet: 1:length(previ_ecmos_v_station_id) {
  
  #   # Venäjältä joku tästä listasta:
  #   station_idt_conversion[which(TRUE==(station_idt_conversion$lon>30 & station_idt_conversion$lon<40 & station_idt_conversion$lat>40 & station_idt_conversion$lat<50)),]
  #   # Tähän kohtaan pitää rakentaa myöhäisemmässä vaiheessa skripti, joka hakee havaintodatan suoraan tietokannasta.
  #   # Toistaiseksi ladataan kuitenkin vain suoraan tiedostosta.
  #   havainnot <- read.csv("/media//ylhaisi//PURE/MOS_project_related/postgres_hakuja/havaintodataa/Kaisaniemi_yhdistetyt_havainnot.csv",stringsAsFactors = FALSE)
  #   havainnot <- havainnot[,-1]
  #   havainnot[,1] <- as.POSIXct(havainnot[,1],tz="GMT")
    
    # Testataan, onko asemajoukkoluettelossa sellaista asemaa, jonka wmon-numerointi vastaa previ_ecmos_v -vektorin indeksiä i (eli onko samalta asemalta saatavilla homogeenisuusvaatimukset täyttävää havaintodataa).
    if (!length(which(station_idt_conversion$wmon==previ_ecmos_v_station_id[i]))) {
      # Hypätään loopissa seuraavaan asemaan, koska tälle ei ole lainkaan dataa
      print(paste("asemalle --wmon ",previ_ecmos_v_station_id[i],"-- ei löydy havaintotietokannasta homogeenista aikasarjaa!",sep=""))
      next
    }
    
    
    
    # Jos MOS-kertoimia ei ole vielä tallennettu tiedostoon, vain siinä tapauksessa käsitellään ko. asema läpi.
    
    komento <- paste("tiedostot <- list.files(\"/media/ylhaisi/PURE/R_projects/vorlon_point_data/processed/MOS_coefficients/valmiit/",mos_label,"/\")",sep="")
    eval(parse(text=komento))
    rm(komento)
    tiedostot <- as.character(matrix(tiedostot,nrow=(length(tiedostot)),byrow=T))
    komento <- paste("tallennetut_kertoimet <- tiedostot[grep(\"^station_",previ_ecmos_v_station_id[i],"_\",tiedostot)]",sep="")
    eval(parse(text=komento))
    rm(komento)
    if (length(tallennetut_kertoimet)<10) {
    
      ### HAVAINTODATAN HAKU ###
      source("hae_havainnot_GTS_Brainstorm_AQU.R")
      fmisid <- station_idt_conversion$fmisid[which(TRUE==(station_idt_conversion$wmon==previ_ecmos_v_station_id[i]))]
      parameter <- selitettavat_muuttujat[sel]
      sel_obsplugin <- selitettavat_muuttujat_obsplugin_koodit[sel]
      sel_AQU <- selitettavat_muuttujat_AQU_koodit[sel]
      sel_CLDB <- selitettavat_muuttujat_CLDB_koodit[sel]
      station_id <- previ_ecmos_v_station_id[i]
      aseman_nimi <- station_idt_conversion$station_name[which(station_idt_conversion$wmon==previ_ecmos_v_station_id[i])]
      
      havainnot <- hae_havainnot_GTS_Brainstorm_AQU(fmisid,parameter,sel_obsplugin,sel_AQU,sel_CLDB,station_id,aseman_nimi,onko_lentosaahavaintoja)      
      rm(fmisid)
      rm(parameter)
      rm(sel_obsplugin)
      rm(sel_AQU)
      rm(sel_CLDB)
      
      # Tarkistetaan löytyykö havaintoja vai ei
      # Tähän asti havaintoja voidaan hakea, vaikkei niitä tietokannassa olisikaan. Jos aseman osalta ei ole ollenkaan havaintoja (tai on naurettavan vähän), hypätään for-loopissa seuraavaan indeksiin.
      if (length(havainnot[,1])<ennuste_havaintoparien_minimimaara) {
        # Hypätään loopissa seuraavaan ajanjaksoon, koska havaintodataa ei ole.
        print(paste("havaintodataa --wmon_",station_id," ",aseman_nimi,"-- ei löydy!",sep=""))
        # Otetaan pois havaintodata
        rm(havainnot)
        rm(station_id)
        rm(aseman_nimi)
        
      } else {
        rm(station_id)
        rm(aseman_nimi)
        # Tallennetaan jälki, että havaintoja löytyy ainakin jotain
        previ_ecmos_v_station_id_data_status$havaintojen_pituus[i] <- dim(havainnot)[1]
        havaintodata_paivastampit <- na.omit(as.POSIXct(trunc.POSIXt(havainnot$obstime, units=c("days"))))
        havaintodata_paivastampit <- aggregate(data.frame(count=havaintodata_paivastampit),list(value=havaintodata_paivastampit),length)
        previ_ecmos_v_station_id_data_status$havaintoja_paivassa[i] <- round(mean(havaintodata_paivastampit$count),digits=2)
        rm(havaintodata_paivastampit)
        
      
      
  
#         ### VERIFIOINTITIETOKANNAN DATAN HAKU ###
#         source("hae_verifdb.R")
    
    
        ### MALLIKENTTIEN HAKU MOS-TIETOKANNASTA ###
        source("hae_mallidata_MOS.R")
        station_id <- previ_ecmos_v_station_id[i]
        aseman_nimi <- station_idt_conversion$station_name[which(station_idt_conversion$wmon==previ_ecmos_v_station_id[i])]
        mallidatamatriisi <- paste("station_",station_id,sep="")
        
        komento <- paste(mallidatamatriisi," <- hae_mallidata_MOS(station_id,aseman_nimi,mallidatamatriisi)",sep="") 
        eval(parse(text=komento))
        rm(komento)
        # Tarkistetaan löytyykö mallidataa vai ei
        if (length(eval(parse(text=mallidatamatriisi))[,1])<ennuste_havaintoparien_minimimaara) {
          # Hypätään loopissa seuraavaan ajanjaksoon, koska mallidataa ei ole.
          print(paste("mallidataa --",aseman_nimi,"-- ei löydy!",sep=""))
          # Otetaan pois mallidata
          komento <- paste("rm(",mallidatamatriisi,")",sep="")
          eval(parse(text=komento))
          rm(komento)
          rm(mallidatamatriisi)
          # Otetaan pois havaintodata
          rm(havainnot)
          rm(station_id)
          rm(aseman_nimi)
        } else {
          rm(station_id)
          rm(aseman_nimi)
          # Tallennetaan jälki, että mallidataa löytyy. Sitähän pitäisi olla rutkasti enemmän kuin havaintoja.
          previ_ecmos_v_station_id_data_status$mallidatan_pituus[i] <- dim(eval(parse(text=mallidatamatriisi)))[1]
    
          for (k in c(1)) {
            
      #       ### MALLIKENTTIEN HAKU MOS-TIETOKANNASTA ###
      #       station_id <- previ_ecmos_v_station_id[i]
      #       aseman_nimi <- station_idt_conversion$station_name[which(station_idt_conversion$wmon==previ_ecmos_v_station_id[i])]
      #       analyysiaika <- analyysiajat[k]
      #       mallidatamatriisi <- paste("station_",station_id,"_",analyysiaika,sep="")
      #       
      #       source("hae_mallidata_MOS_for_one_analysis_hour.R")
      #       komento <- paste(mallidatamatriisi," <- hae_mallidata_MOS_for_one_analysis_hour(station_id,aseman_nimi,analyysiaika,mallidatamatriisi)",sep="") 
      #       eval(parse(text=komento))
      #       rm(komento)
      #       rm(station_id)
      #       rm(aseman_nimi)
      #       rm(analyysiaika)
            
      #       source("hae_mallidata_MOS_individual_analysis_times.R")
      #       komento <- paste(mallidatamatriisi,"_1kerrallaan <- hae_mallidata_MOS_individual_analysis_times(station_id,aseman_nimi,analyysiaika,mallidatamatriisi,sarja)",sep="") 
      #       eval(parse(text=komento))
      #       rm(komento)
        
      
        
      #       station_2974_00 <- station_2974_00[order(station_2974_00$forecast_time,station_2974_00$forecast_period,station_2974_00$param_id,station_2974_00$level_value),]
      #       station_2974_00_1kerrallaan <- station_2974_00_1kerrallaan[order(station_2974_00_1kerrallaan$forecast_time,station_2974_00_1kerrallaan$forecast_period,station_2974_00_1kerrallaan$param_id,station_2974_00_1kerrallaan$level_value),]
      #       jape <- station_2974_00$param_id[1:551157]-station_2974_00_1kerrallaan$param_id[1:551157]
        
        
      
            
            
      #     ennustusjaksot <- c(1,3,5,7,9,11,13,15,17,19,21,23,25,29,33,37,41,45,49,51,53,55,57,59,61,63,65)
      #     ennustusjaksot <- (1:length(forecast_periods))
      #     ennustusjaksot <- c(9,17,25,33,41,49,53,57,61,65) # 1 per päivä
            
            
            for (kausi in c(1)) { # Kaudet järjestyksessä talvi,kevät,kesä,syksy
              # Talvi: (sovellusjakso Joulu-Helmi 12-02), koulutusjakso 11-03
              if (kausi==1) {
                sarja_rajattu <- sarja[as.numeric(format(sarja, "%m"))<4 | as.numeric(format(sarja, "%m"))>10]
              }
              # Kevät: (sovellusjakso Maalis-Touko 03-05), koulutusjakso 02-06
              if (kausi==2) {
                sarja_rajattu <- sarja[as.numeric(format(sarja, "%m"))<7 & as.numeric(format(sarja, "%m"))>1]
              }
              # Kesä: (sovellusjakso Kesä-Elo 06-08), koulutusjakso 05-09
              if (kausi==3) {
                sarja_rajattu <- sarja[as.numeric(format(sarja, "%m"))<10 & as.numeric(format(sarja, "%m"))>4]
              }
              # Syksy: (sovellusjakso Syys-Marras 09-11), koulutusjakso 08-12
              if (kausi==4) {
                sarja_rajattu <- sarja[as.numeric(format(sarja, "%m"))>7]
              }
              
              
              # Tallennetaan tietokannasta löytyvät selittävät muuttujat omaan vektoriinsa, koska eri ennustejaksoille on saatavilla eri määrä muuttujia (lähinnä analyysi vs. ennusteet)
              selittavat_muuttujat_tietokanta <- selittavat_muuttujat
              # # Ennustusjaksot tässä järjestyksessä sen vuoksi, koska analyysihetkellä on vähemmän tietokannassa muuttujia kuin ennustusajanhetkillä.
              # # ennustusjaksot <- c(5,1,length(forecast_periods))
              # ennustusjaksot <- c(5,4,3,2,1,6:length(forecast_periods))
              # ennustusjaksot <- c(1:length(forecast_periods))
              # Tässä analysoidaan ennusteet ainoastaan 6h välein olevat ennusteet. Vaihdetaan samalla ensimmäisen ja toisen alkion paikkaa.
              ennustusjaksot <- which((as.numeric(forecast_periods_hours) %% 6)==0)
              ennustusjaksot <- c(ennustusjaksot[2],ennustusjaksot[1],ennustusjaksot[3:length(ennustusjaksot)])
              for (j in ennustusjaksot) { # c(9,17,33,49,57,65)) { # päivät 1,2,4,6,8,10 1 ennuste per päivä c(9,17,33,49,57,65)) { # päivät 1,2,4,6,8,10 # 1 ennuste per päivä c(9,17,25,33,41,49,53,57,61,65)) { # kaikki mahdolliset 1:length(forecast_periods)) {
                selittavat_muuttujat <- selittavat_muuttujat_tietokanta
                
                if (j<35) {
                  ennustusjakso_tunteina <- substr(forecast_periods[j],1,2)
                } else if (j>34) {
                  ennustusjakso_tunteina <- substr(forecast_periods[j],1,3)
                }
                
                ### YHDISTÄMINEN SAMAAN MATRIISIIN ###
                source("integrate_MOS_obs_verif_matrices.R")
                ### KÄYTETTÄVISSÄ ON NYT TÄYSI MATRIISI (havainnot + verif-data + mallidata + apumuuttujat) JOS DATAA ON. ###
                
                # KOSKA DATA VOI MYÖS PUUTTUA, TÄYTYY VIELÄ ERIKSEEN TESTATA LÖYTYYKÖ SITÄ
                # Mikäli havainnot_ja_mallidata -data framen pituus on alle ennuste_havaintoparien_minimimaaran, siirrytään loopissa seuraavaan alkioon. Tällaiseen data frameen ei nimittäin ole mitään mieltä fitata lineaarisia malleja kun ne vain huonontaisivat ennustetta.
                if (exists("havainnot_ja_mallidata")==TRUE) {
                  if (length(havainnot_ja_mallidata[,1])>=ennuste_havaintoparien_minimimaara) {
                
                    # Lasketaan tilastollisen mallin herkkyys ennustaville muuttujille sekä verifiointijakson blokin pituudelle. Tallennetaan tulokset tiedostoon.
                    # source("Postgre_query_point_data_check_MOS_sensitivity_lm_quantilemapping.R") VANHA PASKA, MUTTA TÄSTÄ SAAT lm_quantilemapping -ALGORITMIN KUN JOSKUS KIRJOITAT SEN UUDESTAAN
                    
                    # Piirretään MOS:n tuottamia arvoja scatterploteina vs. havainnot.
                    # source("Postgre_query_point_data_scatterplot_MOS_vs_obs.R")
                    
#                     # Tallennetaan havainnot_ja_mallidata -matriisi tiedostoon omalla nimellään
#                     tallennettava_matriisi <- paste("station_",previ_ecmos_v_station_id[i],"_",analyysiajat[k],"_",ennustusjakso_tunteina,"_season",kausi,"_",selitettavat_muuttujat_ECMWF_koodit[sel],"_level",ECMWF_muuttujat$level_value[match(selitettavat_muuttujat_ECMWF_koodit[sel],ECMWF_muuttujat$muuttuja_EC)],sep="")
#                     komento <- paste(tallennettava_matriisi," <- havainnot_ja_mallidata",sep="")
#                     eval(parse(text=komento))
#                     rm(komento)
#                     komento <- paste("write.csv(",tallennettava_matriisi,", file=\"/media/ylhaisi/PURE/R_projects/vorlon_point_data/processed/testisetteja/kesatyontekijalle/data/",tallennettava_matriisi,".csv\")",sep="")
#                     eval(parse(text=komento))
#                     rm(komento)
#                     komento <- paste("rm(",tallennettava_matriisi,")",sep="")
#                     eval(parse(text=komento))
#                     rm(komento)
#                     rm(tallennettava_matriisi)
                  }
                }
                
                # MYÖS NÄISSÄ SKRIPTEISSÄ TESTATAAN SISÄLLÄ ONKO DATAA. JOS EI OLE, NIIN AINOASTAAN TALLENNETAAN VIIMEISEN ENNUSTEJAKSON AJANHETKELLÄ KOKO MATRIISIN SISÄLTÖ.
                
#                 # Koulutetaan lineaarinen malli ja tallennetaan kertoimet tiedostoon
#                 source("Postgre_query_point_data_train_and_save_lm_MOS.R")
        
                
#                 # NÄITÄ SKRIPTEJÄ VOIDAAN AJAA ILMAN havainnot_ja_mallidata -matriisiakin
#         
                # Vertaillaan verifiointistatistiikkoja eri aineistojen välillä ja piirretään niistä kuvia
                source("Postgre_query_point_data_fcst_MOS_verification_all_timesteps_all_stations.R")
                
            
                print(j)
                if (exists("havainnot_ja_mallidata")) {
                  rm(havainnot_ja_mallidata)
                }
                if (exists("l2")) {
                  rm(l2)
                }
                rm(ennustusjakso_tunteina)
              }
              rm(j)
              rm(ennustusjaksot)
              
              # Ennustusjaksot on käyty läpi. Palautetaan selittäviksi muuttujiksi ne, joille löytyy edes jotain dataa MOS-tietokannasta (ei välttämättä kaikille ennustepituuksille)
              selittavat_muuttujat <- selittavat_muuttujat_tietokanta
              rm(selittavat_muuttujat_tietokanta)
              rm(sarja_rajattu)
            }
            rm(kausi)
          }
          rm(k)
          
          # Poistetaan kaikki mallikama (kaiken mallidatan sisältävä mallidatamatriisi, sekä mahdolliset kerroinmatriisit)
          temp1=ls()
          komento <- paste("temp1 <- temp1[grep(\"^station_",previ_ecmos_v_station_id[i],"\",temp1)]",sep="")
          eval(parse(text=komento))
          rm(komento)
          rm(list=temp1)
          rm(temp1)
          rm(mallidatamatriisi)
        }
        # Otetaan pois havainto, verifdb- ja mallidatat
        if (exists("havainnot")) {
          rm(havainnot)
        }
        temp1=ls()
        temp1 <- temp1[grep("^verifdb_",temp1)]
        rm(list=temp1)
        rm(temp1)
      }
    }
    rm(tallennetut_kertoimet)
    rm(tiedostot)
    print(i)
  }
  rm(i)
#   rm(havaintoasemat)
#   # Tallennetaan valvontamatriisi, jossa on lueteltu MOS:n kouluttamiseen käytetty data asemittain
#   pvm <- format(as.POSIXct(as.character(Sys.time()),tz="GMT"),"%d%m%Y")
#   komento <- paste("write.csv(previ_ecmos_v_station_id_data_status, file=\"/media/ylhaisi/PURE/R_projects/vorlon_point_data/processed/MOS_coefficients/keskeneraiset/",mos_label,"/koulutukseen_kaytetty_data/",selitettavat_muuttujat_ECMWF_koodit[sel],"_level",ECMWF_muuttujat$level_value[match(selitettavat_muuttujat_ECMWF_koodit[sel],ECMWF_muuttujat$muuttuja_EC)],"_lm_",mos_label,"_",muuttujajoukko,"_",pvm,".csv\", quote = TRUE)",sep="")
#   eval(parse(text=komento))
#   rm(komento)
#   rm(pvm)
  rm(previ_ecmos_v_station_id_data_status)
}
rm(sel)


rm(con1)
rm(con2)
rm(drv)
