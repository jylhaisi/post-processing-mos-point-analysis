### Loading needed libraries ###
# Libraries for data retrieval and handling
library(RPostgreSQL)
library(ROracle)
library(xlsx)
library(scrapeR)
library(abind)
library(reshape)
library(reshape2)
library(zoo)
library(pryr)
library(operator.tools)
library(stringr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringi)
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
library(outliers) # outlier detection
# Other
library(insol)
library(classInt)
library(lubridate)
library(smooth)
library(pryr)
require(geosphere)
# library(Hmisc)
# User defined data analysis and plotting functions
library(MOSpointutils)

# All various station, variable and producer lists are in these lists
load(file="../constant_lists/station_lists/read_in_station_lists.Rdata")
load(file="../constant_lists/variable_lists/read_in_variable_lists.Rdata")
load(file="../constant_lists/variable_lists/read_in_producer_lists.Rdata")
load(file="../constant_lists/variable_lists/read_in_verif_lists.Rdata")
load(file="../constant_lists/variable_lists/read_in_aviation_lists.Rdata")


# Loading updateable station location history table defining fmisid-wmon conversion. Removing all duplicate wmo numbers (preserving only the first (latest) station location). -> wmon-numbers are unique, but several fmisids can be found. Most of these stations are Finnish wmon5xxx changes to newer wmon2xxx stations (duplicated() picks older stations, match() picks newer stations)
# This station list is not checked for location consistency between mos station locations! It is narrowed down based on requirements of pre-defined station lists.
station_idt_conversion <- read.csv("../constant_lists/station_lists/station_idt_conversion.csv",header = TRUE,stringsAsFactors = FALSE)
station_idt_conversion <- station_idt_conversion[!duplicated(station_idt_conversion$wmon),]


# Initializing connections to different databases. Establishing db driver, defining max number of rows which can be fetched
drv_psql <- dbDriver("PostgreSQL",fetch.default.rec=100000000, max.con=25)
drv_ora <- dbDriver("Oracle")
passwords <- read.csv("/data/statcal/infofiles_etc/passwords.txt",header=TRUE,stringsAsFactors = FALSE)
# Read-only MOS
con1 <- RPostgreSQL::dbConnect(drv_psql, host = passwords[1,][1], user = passwords[1,][2], password = passwords[1,][3], dbname = passwords[1,][4])
# Read-write MOS
# # con1 <- RPostgreSQL::dbConnect(drv, host = passwords[2,][1], user = passwords[2,][2], password = passwords[2,][3], dbname = passwords[2,][4])
# verif
con2 <- RPostgreSQL::dbConnect(drv_psql, host = passwords[3,][1], user = passwords[3,][2], password = passwords[3,][3], dbname = passwords[3,][4])
# CLDB
connect.string <- paste("(DESCRIPTION=","(ADDRESS=(PROTOCOL=tcp)(HOST=", passwords[4,][1], ")(PORT=1521))","(CONNECT_DATA=(SERVICE_NAME=", passwords[4,][4], ")))", sep = "")
con3 <- ROracle::dbConnect(drv_ora, username = passwords[4,][2], password = passwords[4,][3], dbname = connect.string)
rm(connect.string)
# aviation
con4 <- RPostgreSQL::dbConnect(drv_psql, host = passwords[5,][1], user = passwords[5,][2], password = passwords[5,][3], dbname = passwords[5,][4])
rm(passwords)


source("../point_data_analysis/choose_variables.R")
source("../point_data_analysis/retrieve_data_all.R")
source("../point_data_analysis/retrieve_data_MOS.R")
source("../point_data_analysis/retrieve_data_verif.R")
source("../point_data_analysis/retrieve_data_CLDB.R")
source("../point_data_analysis/functions_fitting.R")
source("../point_data_analysis/toMOSutils_functions_glm.R")