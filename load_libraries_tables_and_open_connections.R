# Loading needed libraries
library(RPostgreSQL)
library(car)
library(MASS)
require(mgcv)
library(xlsx)
library(verification)
library(scrapeR)
library(prodlim)
library(insol)
library(abind)
library(insol)
library(ggplot2)
library(reshape)
library(gridExtra)
library(abind)
library(LatticeKrig)
library(maps)
library(mapproj)
library(maptools)
library(RColorBrewer)
library(classInt)
library(gpclib)
library(mapdata)
library(lattice)
library(reshape2)
# packages for different model selection algorithms
library(leaps)   # regsubsets
library(glmnet)  # glmnet
library(pls)     # pcr and plsr
# library(Hmisc)

# Some needed functions
`%notin%` <- function(x,y) !(x %in% y)
# Very useful functions of Johanna Piipponen
source("functions_Johanna.R")




# Selitettävät, havaitut pintamuuttujat on listattu vectoriin "selitettavat_muuttujat"
# IL:n käyttämän param_id:n perusteella sortattuna järjestyksessä (lämpötila, kastepiste, sademäärä viimeisen 6h aikana, tuulen nopeus, tuulen suuntakulma).
CLDB_muuttujat <- read.xlsx("../constant_lists/lists_legacy/CLDB_variable_names.xls", 1,header = TRUE,stringsAsFactors = FALSE)
ECMWF_muuttujat <- read.xlsx("../constant_lists/lists_legacy/ECMWF_variable_names_and_numbers.xls", 1,header = TRUE,stringsAsFactors = FALSE)
# # Poistetaan ylimääräiset rivit ECMWF_muuttujat -muuttujasta
# ECMWF_muuttujat <- ECMWF_muuttujat[,-(5:6)]
verif_muuttujat <- read.xlsx("../constant_lists/lists_legacy/verif_variable_names_and_numbers.xls", 1,header = TRUE,stringsAsFactors = FALSE)
AQU_muuttujat <- read.xlsx("../constant_lists/lists_legacy/AQU_variable_names_and_numbers.xls", 1,header = TRUE,stringsAsFactors = FALSE)

CLDB_kaikki_muuttujat <- read.csv("../constant_lists/lists_legacy/measurand_id_uniikit_csv.txt",header = TRUE,stringsAsFactors = FALSE,sep=";")
selitettavat_muuttujat_CLDB_koodit <- c(1,22,21,32,62,321,322,37)


# Huomaa, että verifiointikannasta haetaan KORJATUT tuulen nopeudet, mutta ilmastotietokannassa tätä muuttujaa ei ole! Myös muiden muuttujien osalta on epäkonsistenssia, nämä pitää katsoa kuntoon sitä mukaa kun muuttujat tulevat käsittelyyn.
selitettavat_muuttujat_ECMWF_koodit=c(ECMWF_muuttujat$muuttuja_EC[19],ECMWF_muuttujat$muuttuja_EC[63],ECMWF_muuttujat$muuttuja_EC[62],ECMWF_muuttujat$muuttuja_EC[21],ECMWF_muuttujat$muuttuja_EC[26],"WS","WD",ECMWF_muuttujat$muuttuja_EC[15])
selitettavat_muuttujat_verif_koodit=c(verif_muuttujat$id[1],verif_muuttujat$id[8],verif_muuttujat$id[9],verif_muuttujat$id[4],verif_muuttujat$id[5],verif_muuttujat$id[19],verif_muuttujat$id[13],verif_muuttujat$id[45])
# selittavat_muuttujat_apumuuttujat=c("SOL_ANGLE","R_SURF","R_SURF2","T2_D2_DIFF","T2_T950_DIFF","T2_T925_DIFF","T2_T850_DIFF","T2_T700_DIFF","T2_T500_DIFF","GH_500_700_DIFF","GH_700_850_DIFF","SHEARVEL_950","SHEARVEL_925","SHEARVEL_850","SHEARVEL_700","SHEARVEL_500","SHEARDIR_950","SHEARDIR_925","SHEARDIR_850","SHEARDIR_700","SHEARDIR_500","SHEAR_950","SHEAR_925","SHEAR_850","SHEAR_700","SHEAR_500","VO_950_500_DIFF","VO_925_500_DIFF","SW_NET","LW_NET","RAD_NET","WATER_ACC","H","E_SURF_NET")
selittavat_muuttujat_apumuuttujat=c("SOL_ANGLE","SIN_SOL_ANGLE","DECLINATION","DECLINATION_LAG1MON","R_SURF","R_SURF2","SHEARVEL_950","SHEARVEL_925","SHEARVEL_850","SHEARVEL_700","SHEARVEL_500","SHEARDIR_950","SHEARDIR_925","SHEARDIR_850","SHEARDIR_700","SHEARDIR_500","SHEAR_950","SHEAR_925","SHEAR_850","SHEAR_700","SHEAR_500")

# Tähän vektoriin tallennetaan aivan kaikki muuttujat, joita voidaan käyttää selittävinä muuttujina
selittavat_muuttujat_kaikki <- c(ECMWF_muuttujat$muuttuja_EC,selittavat_muuttujat_apumuuttujat)

# Tässä kohtaa listataan selitettävien havaintomuuttujien lyhenteet
if (onko_lentosaahavaintoja==0) {
  selitettavat_muuttujat=c(CLDB_muuttujat$koodi[32],CLDB_muuttujat$koodi[35],CLDB_muuttujat$koodi[33],CLDB_muuttujat$koodi[37],CLDB_muuttujat$koodi[25],CLDB_muuttujat$koodi[44],CLDB_muuttujat$koodi[42],CLDB_muuttujat$koodi[28])
  selitettavat_muuttujat_obsplugin_koodit=c(verif_muuttujat$newbase_name[1],verif_muuttujat$newbase_name[8],verif_muuttujat$newbase_name[9],verif_muuttujat$newbase_name[4],verif_muuttujat$newbase_name[5],verif_muuttujat$newbase_name[3],verif_muuttujat$newbase_name[13],verif_muuttujat$newbase_name[45])
  selitettavat_muuttujat_AQU_koodit=c(AQU_muuttujat$paramId[2],AQU_muuttujat$paramId[2],AQU_muuttujat$paramId[2],AQU_muuttujat$paramId[3],"NA",AQU_muuttujat$paramId[17],AQU_muuttujat$paramId[4],AQU_muuttujat$paramId[1])
} else { # Lentosääparametrit {
  selitettavat_muuttujat=c(CLDB_muuttujat$koodi[1],CLDB_muuttujat$koodi[6],CLDB_muuttujat$koodi[39],"VERT")
  selitettavat_muuttujat_obsplugin_koodit=c("CH1_AWS","N_AWS","VIS","VERT")
  selitettavat_muuttujat_AQU_koodit=c(AQU_muuttujat$paramId[6],AQU_muuttujat$paramId[5],AQU_muuttujat$paramId[15],AQU_muuttujat$paramId[16])
  # Asetetaan pelkät lämpötilat näihin, sillä näitä käytetään esim. lm()-skriptissä kn yritetään selittää näkyvyyttä. Lämpötila on paras ainoaksi selittäväksi muuttujaksi.
  selitettavat_muuttujat_ECMWF_koodit=c(ECMWF_muuttujat$muuttuja_EC[19],ECMWF_muuttujat$muuttuja_EC[19],ECMWF_muuttujat$muuttuja_EC[19],ECMWF_muuttujat$muuttuja_EC[19])
  selitettavat_muuttujat_verif_koodit=c(verif_muuttujat$id[1],verif_muuttujat$id[1],verif_muuttujat$id[1],verif_muuttujat$id[1])
}

# Näitä aineistoja ladataan verifiointitietokannasta
verif_aineistot <- c("ecmwf","gfs","hirlam","harmonie","kalmanecmwf","dwdmosmix","pal")

# Ladataan mallin asemajoukkolista (wmon) ja otetaan pois sen viimeinen rivi (footer).
previ_ecmos_v_station_id <- read.csv("../constant_lists/lists_legacy/previ_ecmos_v_uniikit_station_id.txt",header = TRUE,stringsAsFactors = FALSE)
previ_ecmos_v_station_id <- previ_ecmos_v_station_id[-(dim(previ_ecmos_v_station_id)[1]),]

# Ladataan ennusteajanjaksot ja otetaan pois sen viimeinen rivi (footer).
forecast_periods <- read.csv("../constant_lists/lists_legacy/previ_ecmos_v_uniikit_forecast_period.txt",header = TRUE)
forecast_periods <- forecast_periods[-(dim(forecast_periods)[1]),]
forecast_periods <- as.character(forecast_periods)
forecast_periods_hours <- unlist(strsplit(forecast_periods,split=':00:00',fixed=TRUE))

# 00 ja 12 UTC ajot käsitellään MOS:ssa erikseen
analyysiajat=c("00","12")

# Ladataan perustellulla tavalla typistetty wmostations-taulu, jotta voidaan tehdä konversio wmon -> fmisid (Havainnot ovat fmisid-tunnisteella). Tässä listassa olevien havaintoasemien asianmukaisuus on suodatettu läpi skriptissä (crop_wmostations_list_to_homogeneous.R)
# TÄMÄ TAULU SISÄLTÄÄ KURANTIN KÄYTETTÄVISSÄ OLEVAN DATAN, JOLLE HAVAINTOASEMIEN SIJAINNIT MOS:N INTERPOLOINTIAJANHETKELLÄ OVAT YHTENEVÄISET NIIDEN ASEMASIJAINTIEN KANSSA, JOTKA OVAT KOKO MOS-KOULUTUSAJANJAKSOLLA HOMOGEENISET.
station_idt_conversion <- read.csv("../constant_lists/lists_legacy/wmostations_cropped_station_list.csv",header = TRUE,stringsAsFactors = FALSE)
station_idt_conversion <- station_idt_conversion[,-1 ,drop= FALSE]


# Näille asemille on ECMWF:n päästä ladattuna dataa. Tässä listassa olevat havaintoasemat on tarkastettu yhtenevien wmostations-ecmwf sijaintien ja ecmwf-taulun wmo-numeroiden yksikäsitteisyyden suhteen
asemat_ECMWF <- read.csv("../constant_lists/lists_legacy/ECMWF_cropped_station_list.csv",header = TRUE,stringsAsFactors = FALSE)
asemat_ECMWF <- asemat_ECMWF[,-1 ,drop= FALSE]
# asemat_ECMWF <- read.csv("../constant_lists/lists_legacy/ecmwf_bufr_v_uniikit_shortened_csv.txt",header = TRUE,stringsAsFactors = FALSE)

# EUROOPPALAISET ASEMALISTAT
# Asemat rannikolla (lsm lähellä nollaa) ja orografiaerotus mallihilan ja todellisuuden välillä on kohtuullisen suuri.
asemalista_rannikko_vuoristo <- read.csv("../constant_lists/lists_legacy/rannikko_vuoristo.csv",header=TRUE)
asemalista_rannikko_vuoristo <- asemalista_rannikko_vuoristo[,-1]
# Sisamaassa (lsm 100) ja orografiaerotus mallihilan ja todellisuuden välillä on olematon.
asemalista_sisamaa_tasainen <- read.csv("../constant_lists/lists_legacy/sisamaa_tasainen.csv",header=TRUE)
asemalista_sisamaa_tasainen <- asemalista_sisamaa_tasainen[,-1]
# Asemat rannikolla (lsm lähellä nollaa) ja orografiaerotus mallihilan ja todellisuuden välillä on kohtuullisen suuri.
asemalista_rannikko <- read.csv("../constant_lists/lists_legacy/rannikko.csv",header=TRUE)
asemalista_rannikko <- asemalista_rannikko[,-1]
# Sisamaassa (lsm 100) ja orografiaerotus mallihilan ja todellisuuden välillä on olematon.
asemalista_sisamaa <- read.csv("../constant_lists/lists_legacy/sisamaa.csv",header=TRUE)
asemalista_sisamaa <- asemalista_sisamaa[,-1]
# MOS-pisteiden ulkopuoliset asemat 1 (uniform-laatikko sekä h/dist-riippuvuudet)
asemalista_MOSulkopuoliset1 <- read.csv("../constant_lists/lists_legacy/MOSulkopuolisetasemat1.csv")
asemalista_MOSulkopuoliset1 <- asemalista_MOSulkopuoliset1[,-1]
# MOS-pisteiden ulkopuoliset asemat 2 (uniform-laatikon ulkopuolella olevat h/dist-riippuvuudet)
asemalista_MOSulkopuoliset2 <- read.csv("../constant_lists/lists_legacy/MOSulkopuolisetasemat2.csv")
asemalista_MOSulkopuoliset2 <- asemalista_MOSulkopuoliset2[,-1]
# MOS-pisteiden ulkopuoliset asemat 3 (pelkkä uniform-laatikko, ts. ulkopuoliset1 - ulkopuoliset2 = uniform-laatikko)
asemalista_MOSulkopuoliset3 <- read.csv("../constant_lists/lists_legacy/MOSulkopuolisetasemat3.csv")
asemalista_MOSulkopuoliset3 <- asemalista_MOSulkopuoliset3[,-1]
# Hilautuksesta pidätetyt MOS-asemat
asemalista_withheldMOS <- read.csv("../constant_lists/lists_legacy/withheldMOS.csv")
asemalista_withheldMOS <- asemalista_withheldMOS[,-1]
# Vuoristoasemat (maailmalta > 2000m ja MOS-pisteiltä > 1500m)
asemalista_vuoristoasemat <- read.csv("../constant_lists/lists_legacy/vuoristoasemat.csv")
asemalista_vuoristoasemat <- asemalista_vuoristoasemat[,-1]
# verif_kannan lehtisaa-asemalista
asemalista_lehtisaa <- read.csv("../constant_lists/lists_legacy/lehtisaaasemat.csv",header=TRUE)
asemalista_lehtisaa <- asemalista_lehtisaa[,-1]
# verif_kannan lehtisaa-asemalista sekä suomalaiset asemat1
asemalista_lehtisaa_ja_suomalaiset <- read.csv("../constant_lists/lists_legacy/lehtisaaasemat_ja_suomalaiset.csv",header=TRUE)
asemalista_lehtisaa_ja_suomalaiset <- asemalista_lehtisaa_ja_suomalaiset[,-1]


# SUOMALAISET ASEMALISTAT
# Metkujen kontrollipisteet, yhteensä 39 kpl (järjestyksessä Jomala Jomalaby, Hanko Tulliniemi, Helsinki Kaisaniemi, Vantaa Helsinki-Vantaan lentoasema, Turku Artukainen, Jokioinen Jokioisten observatorio, Kotka Rankki, Pori Tahkoluoto, Pirkkala Tampere-Pirkkala lentoasema, Lahti Laune, Kouvola Utti lentoasema, Lappeenranta lentoasema, Vaasa Klemettilä, Ähtäri Myllymäki, Jyväskylä lentoasema, Mikkeli lentoasema, Savonlinna lentoasema, Kokkola Tankar, Ylivieska lentokenttä, Viitasaari Haapaniemi, Siilinjärvi Kuopio lentoasema, Liperi Joensuu lentoasema, Ilomantsi Mekrijärvi, Oulu Oulunsalo Pellonpää, Kajaani lentoasema, Kuhmo Kalliojoki, Tornio Torppi, Pudasjärvi lentokenttä, Kuusamo lentoasema, Pello kk museotie, Rovaniemi lentoasema, Salla kk Myllytie, Muonio Alamuonio, Kittilä Pokka, Sodankylä Lapin ilmatieteellinen tutkimuskeskus, Salla Värriötunturi, Enontekiö Kilpisjärvi kyläkeskus, Utsjoki Kevo, Inari Ivalo lentoasema)
asemalista_suomalaiset <- read.csv("../constant_lists/lists_legacy/suomalaiset_asemat.csv",header=TRUE)
asemalista_suomalaiset <- asemalista_suomalaiset[,-1]
# Lista asemista, joille on saatavilla dwdmosmix-ennusteita
asemalista_suomalaiset2 <- read.csv("../constant_lists/lists_legacy/suomalaiset_asemat2.csv",header=TRUE)
asemalista_suomalaiset2 <- asemalista_suomalaiset2[,-1]
# Itse keksitty lista suomalaisia asemia, yhteensä 19 kpl (järjestyksessä Kumpula, Jyväskylä, Sodankylä, Jomala, Lappeenranta, Kuopio, Liperi, Rovaniemi, Oulu, Vaasa, Inari, Salla, Hyytiälä, Turku, Kauhajoki, Enontekiö, Kokkola, Suomussalmi, Vieremä)
asemalista_suomalaiset3 <- read.csv("../constant_lists/lists_legacy/suomalaiset_asemat3.csv",header=TRUE)
asemalista_suomalaiset3 <- asemalista_suomalaiset3[,-1]
# alea iacta est -asemat (Hki-Vantaa, Tampere Pirkkala, Sodankylä)
asemalista_suomalaiset4 <- read.csv("../constant_lists/lists_legacy/suomalaiset_asemat4.csv",header=TRUE)
asemalista_suomalaiset4 <- asemalista_suomalaiset4[,-1]
# Lentokentat
asemalista_suomalaiset_lentokentat <- read.csv("../constant_lists/lists_legacy/suomalaiset_lentokentat.csv",header=TRUE)
asemalista_suomalaiset_lentokentat <- asemalista_suomalaiset_lentokentat[,-1]


# RAJATAAN HAVAINTOASEMAJOUKKOA AINOASTAAN EUROOPALLE MOS-MÄÄRITTELYJEN MUKAISESTI (lat 25...90, lon -25...40)
station_idt_conversion <- station_idt_conversion[station_idt_conversion$lon >= -25 & station_idt_conversion$lon <= 40 & station_idt_conversion$lat >=25 & station_idt_conversion$lat <= 90,]
asemat_ECMWF <- asemat_ECMWF[asemat_ECMWF$lon >= -25 & asemat_ECMWF$lon <= 40 & asemat_ECMWF$lat >=25 & asemat_ECMWF$lat <= 90,]
temp1=ls()
temp1 <- temp1[grep("^asemalista_",temp1)]
for (station in 1:length(temp1)) {
  komento <- paste(temp1[station]," <- ",temp1[station],"[",temp1[station],"$lon >= -25 & ",temp1[station],"$lon <= 40 & ",temp1[station],"$lat >=25 & ",temp1[station],"$lat <= 90,]",sep="")
}
rm(station)
rm(temp1)

# Koulutetaan MOS ainoastaan niille havaintoasemille, joille on saatavilla asiallista havaintodataa
# Oletusarvoisesti koulutetaan kaikki ne asemat, jotka ovat MOS-listassakin
previ_ecmos_v_station_id <- station_idt_conversion$wmon

# Otetaan yhteys PostgreSQL-tietokantaan
# Alustetaan ajuri, rec. on max. rivimäärä joka voidaan hakea
drv <- dbDriver("PostgreSQL",fetch.default.rec=100000000)
# Otetaan yhteys mos-tietokantaan.
con1 <- dbConnect(drv, host="vorlon.fmi.fi", user="mos_ro", password="3ltpU9RHghstRU", dbname="mos")
# con1 <- dbConnect(drv, host="vorlon.fmi.fi", user="mos_rw", password="VfwsP1x4wDwde1", dbname="mos")
con2 <- dbConnect(drv, host="vorlon.fmi.fi", user="ylhaisi", password="6Mgk9fe3Hsz7", dbname="verif")
con3 <- dbConnect(drv, host="vorlon.fmi.fi", user="ylhaisi", password="6Mgk9fe3Hsz7", dbname="aviation")


# Tämä toimii pohjana matriisille, johon tallennetaan kaikki havainto+mallidata
# MOS-ajanjakso
alku=as.POSIXct("2011-12-01 00:00:00 GMT",tz="GMT")
loppu <- as.POSIXct(as.character(Sys.time()),tz="GMT")
loppu <- as.POSIXct(round.POSIXt(loppu, c("hours")))
komento <- paste("loppu <- loppu - ",(as.numeric(format(round.POSIXt(loppu, c("hours")),'%H'))%%3)*3600,sep="")
eval(parse(text=komento))
rm(komento)

# # Entinen MOS-koulutusajanjakson loppuajankohta
# loppu=as.POSIXct("2015-10-01 21:00:00 GMT",tz="GMT")
# # Vertailuun käytetty ajanjakso
# alku=as.POSIXct("2013-10-11 00:00:00 GMT",tz="GMT")
# # loppu=as.POSIXct("2013-10-10 21:00:00 GMT",tz="GMT")
# loppu=as.POSIXct("2014-03-31 21:00:00 GMT",tz="GMT")

# loppu=as.POSIXct(as.character(Sys.time()),tz="GMT")
sarja=seq(alku, loppu, by="1 hour")
rm(alku)
rm(loppu)
