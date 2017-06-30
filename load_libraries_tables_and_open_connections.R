### Loading needed libraries ###
# Libraries for data retrieval and handling
library(RPostgreSQL)
library(xlsx)
library(scrapeR)
library(abind)
library(reshape)
library(reshape2)
library(zoo)
library(pryr)
library(operator.tools)
# Graphical packages
library(ggplot2)
library(gridExtra)
library(lattice)
library(LatticeKrig)
library(maps)
library(mapproj)
library(maptools)
library(mapdata)
library(RColorBrewer)
library(gpclib)
# Statistical packages and different model selection algorithms
library(mgcv)
library(verification)
library(MASS)
library(prodlim)
library(car)
library(leaps)   # regsubsets
library(glmnet)  # glmnet
library(pls)     # pcr and plsr
# Other
library(insol)
library(classInt)
library(lubridate)
library(smooth)
# library(Hmisc)
# User defined data analysis and plotting functions
library(MOSpointutils)

# All various station, variable and producer lists are in these lists
load(file="../constant_lists/station_lists/read_in_station_lists.Rdata")
load(file="../constant_lists/variable_lists/read_in_variable_lists.Rdata")
load(file="../constant_lists/variable_lists/read_in_producer_lists.Rdata")

# All producers in verif db
verif_producers <- read.csv("../constant_lists/database_tables/verif/producers_distinct_csv.txt",sep=";",header = TRUE,stringsAsFactors = FALSE)

# These are the all unique station numbers in MOS db
previ_ecmos_v_station_id <- (read.csv("../constant_lists/database_tables/mos/previ_ecmos_narrow_v_distinct_station_id_csv.txt"))[[1]]

# Initializing connections to different databases. Establishing db driver, defining max number of rows which can be fetched
drv <- dbDriver("PostgreSQL",fetch.default.rec=100000000)
passwords <- read.csv("/data/statcal/infofiles_etc/passwords.txt",header=TRUE,stringsAsFactors = FALSE)
# Read-only MOS
con1 <- dbConnect(drv, host = passwords[1,][1], user = passwords[1,][2], password = passwords[1,][3], dbname = passwords[1,][4])
# Read-write MOS
# # con1 <- dbConnect(drv, host = passwords[2,][1], user = passwords[2,][2], password = passwords[2,][3], dbname = passwords[2,][4])
# verif
con2 <- dbConnect(drv, host = passwords[3,][1], user = passwords[3,][2], password = passwords[3,][3], dbname = passwords[3,][4])
# aviation
con3 <- dbConnect(drv, host = passwords[4,][1], user = passwords[4,][2], password = passwords[4,][3], dbname = passwords[4,][4])
rm(passwords)

source("choose_variables.R")
source("retrieve_data_all.R")
source("retrieve_data_MOS.R")
source("retrieve_data_verif.R")
source("retrieve_data_CLDB.R")
