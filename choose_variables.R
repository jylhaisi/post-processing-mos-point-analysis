# Tämä skripti valitsee ennustavat muuttujat muuttujajoukko-stringin perusteella.
# Seuraavia voi valita (jossain vaiheessa voi poistaa määritteen _1prec ja sisällyttää sen osaksi allmodelvars- määritettä):

# # Kaikki mallimuuttujat ml. edellisen ajanhetken muuttujat. Ei mitään johdettuja muuttujia (auringon korkeuskulmaa yms.)
# muuttujajoukko <- paste("allmodelvars",sep="")
# # Sama kuin edellinen, mutta laaja-alainen ja konvektiivinen sade on yhdistetty yhteen (TP = CP + LSP). Lisäksi on selittävät apumuuttujat mukana (eli deklinaatio)
# muuttujajoukko <- paste("allmodelvars_1prec",sep="")
# # Sama kuin allmodelvars_1prec, mutta mukaan otetaan myös mallin constant-kentät z_oro ja lsm. Mukana ei ole edellisen ennustehetken muuttujia eikä apumuuttujia
# muuttujajoukko <- paste("allmodelvars_1prec_and_lsm_z_oro",sep="")
# # Sama kuin allmodelvars_1prec, mutta ihan vaan tietokantahakujen nopeuttamiseksi ei haeta painepinnoilta muuttujia Z ja W. 02/2016 ei-rutiinissa olevista muuttujista vain SKT ja MX2T3 sisällytetään ennustavien muuttujien joukkoon. Mukana on myös DECLINATION. Tällainen muuttujajoukko tehtiin helmikuussa 2016 ihan vaan siksi että saataisiin vielä kuluvaa talvea verifioitua isommalla muuttujajoukolla.
# muuttujajoukko <- paste("allmodelvars_1prec_noZW_no950_rutiiniextended1",sep="")
# # Sama kuin edellinen, mutta 950hPa sisällytetään mukaan. Ei sisällytetä säteilysuureita eikä suuretta SD.
# muuttujajoukko <- paste("allmodelvars_1prec_noZW_noSD_noRAD",sep="")
# # Sama kuin allmodelvars_1prec, mutta 950hPa sisällytetään mukaan. Ei sisällytetä painepinnoilta Z/W:tä eikä muitakaan huonoja pintasuureita joissa on todistetusti aikasarjassa virheitä
# muuttujajoukko <- paste("allmodelvars_1prec_noZW_noSD",sep="")
# # Sama kuin allmodelvars_1prec, mutta ainoastaan tietokannassa huonoa dataa sisältävät muuttujat (SD,RSN,SSTK,U_100M,V_100M) on otettu pois.
# muuttujajoukko <- paste("allmodelvars_1prec_noSD")
# # Kaikki mallimuuttujat ml. edellisen ajanhetken muuttujat. Vain ne muuttujat, jotka sisältyivät rutiiniin 12/2015.
# muuttujajoukko <- paste("allmodelvars_rutiini1",sep="")
# # Kaikki pintamuuttujat, ei mallipintamuuttujia
# muuttujajoukko <- paste("allmodelvars_1prec_surface")
# # Kaikki mallipintamuuttujat, ei pintamuuttujia
# muuttujajoukko <- paste("allmodelvars_1prec_pressurelevels")
# # Ensimmäinen operatiivinen versio MOS:sta
# muuttujajoukko <- paste("MOSconstant_9a",sep="")
# # Ensimmäinen operatiivinen versio MOS:sta sekä edellisen ajanhetken muuttujat
# muuttujajoukko <- paste("MOSconstant_9b",sep="")
# # Pelkkä pintalämpötila
# muuttujajoukko <- paste("only_surfT2",sep="")


# 1) nk. "Perussetti": Käytetään koulutukseen ainoastaan niitä mallimuuttujia, joista tulee globaalit kentät rutiinilla taloon sisälle (08/2015) JA joille löytyy jotain MOS-dataa tietokannasta
selittavat_muuttujat <- ECMWF_muuttujat[grep("T2|D2|U10|V10|LSP|CP|TP|MSL|TCC|LCC|MCC|HCC|FG10_3|SSRD|STR|STRD|SSHF|CAPE|CIN|SD|Z_|T_|RH_|W_|U_|V_",ECMWF_muuttujat$muuttuja_EC),1]
if (length(grep("_M1",selittavat_muuttujat))>0) {
  selittavat_muuttujat <- selittavat_muuttujat[-grep("_M1",selittavat_muuttujat)]
}
if (length(grep("PV_",selittavat_muuttujat))>0) {
  selittavat_muuttujat <- selittavat_muuttujat[-grep("PV_",selittavat_muuttujat)]
}
if (length(grep("ISSRD",selittavat_muuttujat))>0) {
  selittavat_muuttujat <- selittavat_muuttujat[-grep("ISSRD",selittavat_muuttujat)]
}
if (length(grep("_950",selittavat_muuttujat))>0) {
  selittavat_muuttujat <- selittavat_muuttujat[-grep("_950",selittavat_muuttujat)]
}
if (length(grep("_100M",selittavat_muuttujat))>0) {
  selittavat_muuttujat <- selittavat_muuttujat[-grep("_100M",selittavat_muuttujat)]
}
if (length(grep("Z_ORO",selittavat_muuttujat))>0) {
  selittavat_muuttujat <- selittavat_muuttujat[-grep("Z_ORO",selittavat_muuttujat)]
}
# Näitä operatiivisessa striimissä + MOS-tietokannassa olevia muuttujia on 44 kpl.


# 2) Lisätään ei-rutiinimuuttujia, jotka löytyvät vuosille 2011-2013 MOS-tietokannasta.
# Tässä listassa rajattu muuttujasetti, joka ladataan vuosille 2014-2015
jape <- read.csv("../constant_lists/lists_legacy/ECMWF_variable_list_loaded_variables_and_others_muuttujat_talvikausiksi_2013_2015.csv",stringsAsFactors = FALSE)
# Tässä listassa kaikki muuttujat, numeroon 86 asti olevat muuttujat on ladattuna vuosille 2011-2013.
jape2 <- read.csv("../constant_lists/lists_legacy/ECMWF_variable_list_loaded_variables_and_others.csv",stringsAsFactors = FALSE)
# Tässä listattuna kaikki ne muuttujat, jotka eivät ole selittavien muuttujien joukossa vielä. Muuttujaan D_950 asti lisätään puuttuvat muuttujat listaan.
lisattavat_muuttujat <- jape2$muuttuja_EC[which(is.na(match(jape2$muuttuja_EC,selittavat_muuttujat)))]
lisattavat_muuttujat <- lisattavat_muuttujat[1:42]
selittavat_muuttujat <- c(selittavat_muuttujat,lisattavat_muuttujat)
rm(lisattavat_muuttujat)
rm(jape)
rm(jape2)
# Nyt selittavia muuttujia on 86 kpl, eli kaikki ne mitä on ladattu vuosille 2011-2013.



# 3) Rajataan muuttujasettiä ottamalla pois osa selittavista muuttujista (Z_ORO + LSM + VO + PV + D)
# Z_ORO ja LSM eivät muutu, VO, PV ja D ladattiin alunperinkin vain kahdeksi vuodeksi.
if (length(grep("Z_ORO",selittavat_muuttujat))>0) {
  selittavat_muuttujat <- selittavat_muuttujat[-grep("Z_ORO",selittavat_muuttujat)]
}
if (length(grep("LSM",selittavat_muuttujat))>0) {
  selittavat_muuttujat <- selittavat_muuttujat[-grep("LSM",selittavat_muuttujat)]
}
if (length(grep("VO_",selittavat_muuttujat))>0) {
  selittavat_muuttujat <- selittavat_muuttujat[-grep("VO_",selittavat_muuttujat)]
}
if (length(grep("PV_",selittavat_muuttujat))>0) {
  selittavat_muuttujat <- selittavat_muuttujat[-grep("PV_",selittavat_muuttujat)]
}
if (length(grep("D_",selittavat_muuttujat))>0) {
  selittavat_muuttujat <- selittavat_muuttujat[-grep("D_",selittavat_muuttujat)]
}
# Nyt selittaviä muuttuja on 69 kpl, mikä on oikein. 69 + Z_ORO + LSM + 5*VO + 5*PV + 5*D = 86

# Operatiivisten muuttujien päälle lisättiin siis nämä muuttujat
# c("TCWV","SLHF","CI","U_100M","V_100M","FAL","SKT","CBH","SSTK","SSR","DEG0","BLH","E","MX2T3","MN2T3","SRO","SSRO","FDIR","RSN","T_950","Z_950","RH_950","W_950","U_950","V_950")



# 4) Otetaan pois muuttujat, joita ei löydy vuosille 2014-2015 tietokannasta
jape <- read.csv("../constant_lists/lists_legacy/ECMWF_variable_list_loaded_variables_and_others_muuttujat_talvikausiksi_2013_2015.csv",stringsAsFactors = FALSE)
poistettavat_muuttujat <- selittavat_muuttujat[which(is.na(match(selittavat_muuttujat,jape$muuttuja_EC)))]
selittavat_muuttujat <- selittavat_muuttujat[-(which(selittavat_muuttujat %in% poistettavat_muuttujat))]
rm(jape)
rm(poistettavat_muuttujat)
# # Tällainen lista siis otettiin pois selittavia muuttujia
# c("TCC","SSRD","U_925","V_925","U_850","V_850","U_700","V_700","U_500","V_500","CI","U_950","V_950","FAL","E","SRO","SSRO","FDIR")
# Nyt selittavia muuttujia on mukana 51 kpl, niinkuin STU:n listassakin. (51 kpl + Z_ORO + LSM = 53 kpl.)

### TÄSSÄ KOHDASSA LISTA ON SAMA KUIN ECMWF_variable_list_loaded_variables_and_others_muuttujat_talvikausiksi_2013_2015 ###




# 5) Otetaan pois sellaiset muuttujat, joita ei ole mitään järkeä säilyttää (ovat duplikaatteja, epäfysikaalisia tms.)

if (length(grep("TP|STRD",selittavat_muuttujat))>0) {
  selittavat_muuttujat <- selittavat_muuttujat[-grep("TP|STRD",selittavat_muuttujat)]
}


# 6) Mukaan otetaan myös edellisajanhetken mallimuuttujia (näiden perässä on "_M1")
selittavat_muuttujat <- c(selittavat_muuttujat,"T2_M1","T_925_M1","T_950_M1")
# selittavat_muuttujat <- c(selittavat_muuttujat,"T2_M1","T_925_M1","SOL_ANGLE","R_SURF2","jape_M1","OBS_T2_M1","GH_500_M1")

##### JOS muuttujajoukko=="allmodelvars", KÄSITTELY PÄÄTTYY TÄHÄN #####




















# Tässä laaja-alainen ja konvektiivinen sade on yhdistetty yhteen (TP = CP + LSP). Lisäksi on selittävät apumuuttujat mukana (eli deklinaatio).
if (muuttujajoukko=="allmodelvars_1prec") {
  selittavat_muuttujat <- selittavat_muuttujat[-grep("LSP",selittavat_muuttujat)]
  selittavat_muuttujat <- selittavat_muuttujat[-grep("CP",selittavat_muuttujat)]
  selittavat_muuttujat <- c(selittavat_muuttujat,"TP")
  selittavat_muuttujat <- c(selittavat_muuttujat,"DECLINATION")
}

# Tässä lisäksi mukana mallin vakiokentät z_oro ja lsm.
if (muuttujajoukko=="allmodelvars_1prec_and_lsm_z_oro") {
  selittavat_muuttujat <- selittavat_muuttujat[-grep("LSP",selittavat_muuttujat)]
  selittavat_muuttujat <- selittavat_muuttujat[-grep("CP",selittavat_muuttujat)]
  selittavat_muuttujat <- selittavat_muuttujat[-grep("T2_M1",selittavat_muuttujat)]
  selittavat_muuttujat <- selittavat_muuttujat[-grep("T_925_M1",selittavat_muuttujat)]
  selittavat_muuttujat <- selittavat_muuttujat[-grep("T_950_M1",selittavat_muuttujat)]
  selittavat_muuttujat <- c(selittavat_muuttujat,"TP")
  selittavat_muuttujat <- c(selittavat_muuttujat,"Z_ORO")
  selittavat_muuttujat <- c(selittavat_muuttujat,"LSM")
}

# Otetaan pois painepinnan 950 muuttujat sekä kaikilta painepinnoilta Z ja W. 02/2016 ei-rutiinissa olevista muuttujista vain SKT ja MX2T3 sisällytetään ennustavien muuttujien joukkoon.
if (muuttujajoukko=="allmodelvars_1prec_noZW_no950_rutiiniextended1") {
  selittavat_muuttujat <- selittavat_muuttujat[-grep("LSP",selittavat_muuttujat)]
  selittavat_muuttujat <- selittavat_muuttujat[-grep("CP",selittavat_muuttujat)]
  selittavat_muuttujat <- c(selittavat_muuttujat,"TP")
  selittavat_muuttujat <- c(selittavat_muuttujat,"DECLINATION")
  # 950 hPa pois
  if (length(grep("_950",selittavat_muuttujat))>0) {
    selittavat_muuttujat <- selittavat_muuttujat[-grep("_950",selittavat_muuttujat)]
  }
  # Painepinnoilta Z ja W pois
  if (length(grep("Z_",selittavat_muuttujat))>0) {
    selittavat_muuttujat <- selittavat_muuttujat[-grep("Z_",selittavat_muuttujat)]
  }
  if (length(grep("W_",selittavat_muuttujat))>0) {
    selittavat_muuttujat <- selittavat_muuttujat[-grep("W_",selittavat_muuttujat)]
  }
  # Ei-rutiinimuuttujista säästetään vain SKT ja MX2T3
  selittavat_muuttujat <- selittavat_muuttujat[-grep("TCW|SSR|T_950|RH_950|RSN|BLH|DEG0|SLHF|CBH|U_100M|V_100M|SSTK|MN2T3|Z_950|W_950",selittavat_muuttujat)]
  # Lämpötilan ennustamisen kannalta otetaan muutamia aivan turhia muuttujia pois
  selittavat_muuttujat <- selittavat_muuttujat[-grep("CAPE|CIN",selittavat_muuttujat)]
}

# Sama kuin edellinen, mutta otetaan mukaan painepinta 950. Poistetaan lisäksi säteilysuureet SSHF+STR sekä SD.
if (muuttujajoukko=="allmodelvars_1prec_noZW_noSD_noRAD") {
  selittavat_muuttujat <- selittavat_muuttujat[-grep("LSP",selittavat_muuttujat)]
  selittavat_muuttujat <- selittavat_muuttujat[-grep("CP",selittavat_muuttujat)]
  selittavat_muuttujat <- c(selittavat_muuttujat,"TP")
  selittavat_muuttujat <- c(selittavat_muuttujat,"DECLINATION")
  # Painepinnoilta Z ja W pois
  if (length(grep("Z_",selittavat_muuttujat))>0) {
    selittavat_muuttujat <- selittavat_muuttujat[-grep("Z_",selittavat_muuttujat)]
  }
  if (length(grep("W_",selittavat_muuttujat))>0) {
    selittavat_muuttujat <- selittavat_muuttujat[-grep("W_",selittavat_muuttujat)]
  }
  # Ei-rutiinimuuttujista säästetään vain SKT ja MX2T3
  selittavat_muuttujat <- selittavat_muuttujat[-grep("TCW|SSR|RSN|BLH|DEG0|SLHF|CBH|U_100M|V_100M|SSTK|MN2T3",selittavat_muuttujat)]
  # Lämpötilan ennustamisen kannalta otetaan muutamia muuttujia pois (joko turhia tai malliversion muutoksille alttiita)
  selittavat_muuttujat <- selittavat_muuttujat[-grep("CAPE|CIN|SD|SSHF|STR",selittavat_muuttujat)]
}

# Sama kuin allmodelvars_1prec, mutta tietokannassa huonoa dataa sisältävät muuttujat (SD,RSN,SSTK,U_100M, V_100M) sekä painepintojen Z+W on otettu pois.
if (muuttujajoukko=="allmodelvars_1prec_noZW_noSD") {
  selittavat_muuttujat <- selittavat_muuttujat[-grep("LSP",selittavat_muuttujat)]
  selittavat_muuttujat <- selittavat_muuttujat[-grep("CP",selittavat_muuttujat)]
  selittavat_muuttujat <- c(selittavat_muuttujat,"TP")
  selittavat_muuttujat <- c(selittavat_muuttujat,"DECLINATION")
  # Painepinnoilta Z ja W pois
  if (length(grep("Z_",selittavat_muuttujat))>0) {
    selittavat_muuttujat <- selittavat_muuttujat[-grep("Z_",selittavat_muuttujat)]
  }
  if (length(grep("W_",selittavat_muuttujat))>0) {
    selittavat_muuttujat <- selittavat_muuttujat[-grep("W_",selittavat_muuttujat)]
  }
  # Poistetaan epäkelvot muuttujat
  selittavat_muuttujat <- selittavat_muuttujat[-grep("SD|RSN|SSTK|U_100M|V_100M",selittavat_muuttujat)]
  # Lämpötilan ennustamisen kannalta otetaan muutamia muuttujia pois
  selittavat_muuttujat <- selittavat_muuttujat[-grep("CAPE|CIN",selittavat_muuttujat)]
}



# Kaikki mallimuuttujat ml. edellisen ajanhetken muuttujat. Vain ne muuttujat, jotka sisältyivät rutiiniin 12/2015.
if (muuttujajoukko=="allmodelvars_rutiini1") {
  selittavat_muuttujat <- selittavat_muuttujat[-grep("SKT|MX2T3|TCW|SSR|T_950|RH_950|RSN|BLH|DEG0|SLHF|CBH|U_100M|V_100M|SSTK|MN2T3|Z_950|W_950",selittavat_muuttujat)]
}

# Kaikki pintamuuttujat, ei mallipintamuuttujia
if (muuttujajoukko=="allmodelvars_1prec_surface") {
  selittavat_muuttujat <- selittavat_muuttujat[-grep("_950|_925|_850|_700|_500",selittavat_muuttujat)]
}

# Kaikki mallipintamuuttujat, ei pintamuuttujia
if (muuttujajoukko=="allmodelvars_1prec_pressurelevels") {
  selittavat_muuttujat <- selittavat_muuttujat[grep("_950|_925|_850|_700|_500",selittavat_muuttujat)]
}

# Ensimmäinen operatiivinen MOS-versio, jossa ennustavien muuttujien joukko on vakio.
if (muuttujajoukko=="MOSconstant_9a") {
  selittavat_muuttujat <- c("MSL","T2","D2","LCC","MCC","RH_925","T_925","RH_500","T_500")
}

# Ensimmäinen operatiivinen MOS-versio, jossa ennustavien muuttujien joukko on vakio. Mukaanlukien edellisen ajanhetken muuttujat.
if (muuttujajoukko=="MOSconstant_9b") {
  selittavat_muuttujat <- c("MSL","T2","D2","LCC","MCC","RH_925","T_925","RH_500","T_500","T2_M1","T_925_M1")
}

# Pelkkä pintalämpötila
if (muuttujajoukko=="only_surfT2") {
  selittavat_muuttujat <- c("T2")
}













### JÄRJESTETÄÄN SELITTÄVÄT MUUTTUJAT OIKEAAN JÄRJESTYKSEEN ###
# Järjestetään selittavat muuttujat järjestykseen: pintamuuttujat, painepinnat, _M1, apumuuttujat
# Muuttujien järjestys on sama kuin ECMWF_muuttujat -matriisissa

# Ensin poimitaan selittävien muuttujien joukosta erilleen kaikki muut paitsi saman ajanhetken pintamuuttujat, sekä laitetaan näiden perään apumuuttujat joita ei edes ole välttämättä lainkaan selittavien muuttujien joukossa
ei_pintamuuttujat <- c(selittavat_muuttujat[grep("_950|_925|_850|_700|_500|_M1",selittavat_muuttujat)],selittavat_muuttujat_apumuuttujat)
# Jos selittävien muuttujien joukossa on muitakin kuin saman ajanhetken pintamuuttujia, poimitaan kaikki muut paitsi ne. Muussa tapauksessa kaikkien selittävien muuttujien oletetaan olevan saman ajanhetken pintamuuttujia.
if (length(which(selittavat_muuttujat %in% ei_pintamuuttujat))>0) {
  pintamuuttujat <- selittavat_muuttujat[-which(selittavat_muuttujat %in% ei_pintamuuttujat)]
} else {
  pintamuuttujat <- selittavat_muuttujat
}
# Ordering to same order as variables in list ECMWF_muuttujat
pintamuuttujat <- ECMWF_muuttujat$muuttuja_EC[which(ECMWF_muuttujat$muuttuja_EC %in% pintamuuttujat)]

# Pick out pressure level variables, time-lagged variables and derived variables.
painepintamuuttujat1 <- ECMWF_muuttujat$muuttuja_EC[which(ECMWF_muuttujat$muuttuja_EC %in% (selittavat_muuttujat[grep("_950$",selittavat_muuttujat)]))]
painepintamuuttujat2 <- ECMWF_muuttujat$muuttuja_EC[which(ECMWF_muuttujat$muuttuja_EC %in% (selittavat_muuttujat[grep("_925$",selittavat_muuttujat)]))]
painepintamuuttujat3 <- ECMWF_muuttujat$muuttuja_EC[which(ECMWF_muuttujat$muuttuja_EC %in% (selittavat_muuttujat[grep("_850$",selittavat_muuttujat)]))]
painepintamuuttujat4 <- ECMWF_muuttujat$muuttuja_EC[which(ECMWF_muuttujat$muuttuja_EC %in% (selittavat_muuttujat[grep("_700$",selittavat_muuttujat)]))]
painepintamuuttujat5 <- ECMWF_muuttujat$muuttuja_EC[which(ECMWF_muuttujat$muuttuja_EC %in% (selittavat_muuttujat[grep("_500$",selittavat_muuttujat)]))]
edellisjakson_muuttujat <- ECMWF_muuttujat$muuttuja_EC[which(ECMWF_muuttujat$muuttuja_EC %in% (selittavat_muuttujat[grep("_M1$",selittavat_muuttujat)]))]
apumuuttujat <- selittavat_muuttujat_apumuuttujat[which(selittavat_muuttujat_apumuuttujat %in% selittavat_muuttujat)]

selittavat_muuttujat <- c(pintamuuttujat,painepintamuuttujat1,painepintamuuttujat2,painepintamuuttujat3,painepintamuuttujat4,painepintamuuttujat5,edellisjakson_muuttujat,apumuuttujat)

rm(ei_pintamuuttujat)
rm(pintamuuttujat)
rm(painepintamuuttujat1)
rm(painepintamuuttujat2)
rm(painepintamuuttujat3)
rm(painepintamuuttujat4)
rm(painepintamuuttujat5)
rm(edellisjakson_muuttujat)
rm(apumuuttujat)

### TALLENNETAAN OMAAN MUUTTUJAANSA selittavat_muuttujat.
### Myöhemmin skriptissä selittavat_muuttujat -vektoria rajataan sen perusteella, kuinka monta mallimuuttujaa löytyy MOS-tietokannasta (mille hyvänsä ennustejaksolle).
### -> tallennetaan tämä selittavat_muuttujat_tietokanta -vektoriin myöhemmin
### -> jokaiselle ennustepituudelle rajataan selittavat_muuttujat -vektoria vielä sen perusteella, kuinka monta selittavaa muuttujaa juuri ko. ennustepituudelle löytyy.
selittavat_muuttujat_kaikki <- selittavat_muuttujat
