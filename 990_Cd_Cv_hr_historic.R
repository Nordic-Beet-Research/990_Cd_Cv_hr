# RUN THROUGH WITH CHANGES TO YEAR. START YEAR IS 2021. STOP AT 2021.


############################################
############################################
##
## NBR Weather data - import from Lantmet
##
## This project uses R 4.1.2 
## with snapshot date 2021-11-01
##
############################################
############################################

# Setup

{
  # -------------------------------------------
  snapshot_date = "2021-11-01"
  options("repos" = paste0("https://mran.revolutionanalytics.com/snapshot/", snapshot_date))
  # -------------------------------------------
  
  # -------------------------------------------
  # sink options
  options(width = 150)
  # rJava memory option
  options(java.parameters = "-Xmx8000m")
  # -------------------------------------------
  
  # R packages
  # -------------------------------------------
  Rpackages_version = c("data.table_1.14.2",
                        "dplyr_1.0.7",
                        "ggplot2_3.3.5",
                        "tidyr_1.1.4",
                        "writexl_1.4.0",
                        "rdrop2_0.8.2.1",
                        "lubridate_1.8.0",
                        "httr_1.4.2",
                        "stringr_1.4.0",
                        "vctrs_0.3.8",
                        "png_0.1-7"#,
                        #"readxl_1.3.1"
  )
  path_Rpackages = "C:/R packages_412"
  # -------------------------------------------
  
  # version check and load packages
  # -------------------------------------------
  # R version check
  if(sessionInfo()$R.version$version.string != "R version 4.1.2 (2021-11-01)") stop("R.version must be 4.1.2 (2021-11-01)")
  
  # install packages
  Rpack = sapply(strsplit(Rpackages_version, "_", fixed = T), FUN = function(x) x[1])
  Rpack_version = sapply(strsplit(Rpackages_version, "_", fixed = T), FUN = function(x) x[2])
  if(!all(Rpack %in% list.files(path_Rpackages))){
    loadRpackages <- Rpack[!Rpack %in% list.files(path_Rpackages)]
    for(i in loadRpackages) install.packages(i, lib = path_Rpackages, repos = options("repos"), dependencies = T)
  }
  
  # load packages
  for(i in Rpack) eval(parse(text = paste0("library(", i, ", lib.loc = '", path_Rpackages, "')")))
}

############################################
############################################
# Definitions
## Data to collect from LantMet and Meteometics
### TIME AND TIME ZONES
time_zone <- "Europe/Copenhagen"
startdate <- ISOdatetime(year = as.integer("2021",'%Y'),
                         month = as.integer("01",'%m'),
                         day = as.integer("01",'%d'),
                         hour = 00, min = 00, sec = 00, tz = "UTC") #DK format
startDate <- date(startdate)                                        #SE format
enddate <- ISOdatetime(year = as.integer("2021",'%Y'),
                       month = as.integer("08",'%m'),
                       day = as.integer("31",'%d'),
                       hour = 00, min = 00, sec = 00, tz = "UTC")   #DK format
endDate <- date(enddate) - 1                                        #SE format

### LOCATIONS
station_id <- c(20947, 40007, 40141, 40143, 40144, 40145, 41105) # NBRs stations = 40141, 40142, 40143, 40144, 40145
station_names <-c("Borgeby","Sandby gård","Gretelund","Lovisero","Tofta","Hviderup", "Jonstorp")
station_land <- c("SE","SE","SE","SE","SE","SE","SE")
station_lat <- c(55.7530, 55.4395, 55.8864, 55.3772, 55.8935, 55.7779, 56.2295)
station_long <- c(13.0396, 14.1827, 14.0355, 13.4048, 12.9146, 13.3217, 12.6535)
stations <- data.frame("WSTNID" = station_id, "WSTN_namn" = station_names, "land" = station_land,"lat" = station_lat, "long" = station_long)

### DATA AND INTERVAL
#### SE / LANTMET
logInterval <- 1                                # 1= hourly, 2 = daily
elementMeasurement <- "TM"

## Calculation and reporting of degree days and vernalisation hours
stations_rep <- c(20947, 40007, 40141, 40143, 40144, 40145, 41105)
dates <- c("2021-03-02", "2021-03-14", "2021-03-21") # Sow dates. Cd and Vern-hours are calculated from these dates
date_report <- Sys.Date()-1
Cd_base <- 3  # base temperature for degree day calculations.

## Overview tables
summ_stations <- data.frame(station_id, station_names)
dates_all <- c("2021-01-01", dates)
summ_dates <- data.frame(seq(0:length(dates)), dates_all)
names(summ_dates) <- c("såtid","datum")

############################################
############################################
# Get data 
## SWEDEN
### create matrix of all urls = startDate x stations x elements
len_startDate <- length(startDate)
len_stations <- nrow(stations[which(stations$land == "SE"),])
len_ele <- length(elementMeasurement)
len_matrix <- len_startDate*len_stations*len_ele
startDate_c <- rep(startDate, len_matrix/len_startDate)
endDate_c <- rep(endDate, len_matrix)
stations_c <- rep(stations$WSTNID[which(stations$land == "SE")], each=len_matrix/len_stations)
logInterval_c <- rep(logInterval, len_matrix)
elementMeasurement_c <- rep(elementMeasurement, each=len_matrix/len_ele)
urlBase <- "https://www.ffe.slu.se/lm/json/downloadJS.cfm?outputType=CSV&AddID=1"

### create url
for (i in 1:len_matrix){
  urlStation <- paste0("weatherStationID=",stations_c[i])
  urlStart <- paste0("startDate=",startDate_c[i])
  urlEnd <- paste0("endDate=",endDate_c[i])
  urlLog <- paste0("LogIntervalID=",logInterval_c[i])
  urlEle <- paste0("elementMeasurementTypeList=",elementMeasurement_c[i])
  url <- paste(urlBase,urlStation,urlStart,urlEnd,urlLog,urlEle,sep="&")
  
  # Import from Lantmet. Need to add a time-out and re-run to this...
  dat_in_SE_i <- data.frame(fread(url))
  dat_in_SE_i <- dat_in_SE_i %>%
    mutate_at(elementMeasurement, as.numeric)
  
  if(i==1L) dat_in_SE <- dat_in_SE_i else dat_in_SE <- rbind(dat_in_SE, dat_in_SE_i)
}

dat_in <- dat_in_SE %>%
  left_join(stations, by = "WSTNID")

############################################
############################################
# Calculate degree days and vernalisation hours

dat <- dat_in %>%
  group_by(WSTNID) %>%
  mutate(Cd_timvis = ifelse(TM < Cd_base, 0, TM-Cd_base)/24) %>% # Degree days
  mutate(Cv_timvis = -1.256 + (1.26 + 0.131*TM)*0.9357^TM) %>% ## Vernalisation per hour
  mutate(Cv_timvis = ifelse(Cv_timvis < 0, 0, Cv_timvis)) %>% # remove negative Cvs
  mutate(Cd_0 = cumsum(Cd_timvis)) %>% ## Cumulative values
  mutate(Cv_0 = cumsum(Cv_timvis)) %>% ## Cumulative values
  ungroup()

dat_sum <- dat %>%
  filter(HOUR == 23) %>%
  filter(DAY > "2021-03-12") %>%
  group_by(WSTNID) %>%
  mutate(Cv = -1*(Cv_0- max(Cv_0))) %>%
  filter(DAY < "2021-08-31") %>%
  ungroup()

write_xlsx(list(sum_2021 = dat_sum, full_2021 = dat), 
           "Cd_Cv_hist.xlsx")
