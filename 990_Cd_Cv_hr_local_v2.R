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
                        "png_0.1-7",
                        "readxl_1.3.1",
                        "grid_4.1.2"
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
startdate <- ISOdatetime(year = as.integer("2022",'%Y'),
                         month = as.integer("01",'%m'),
                         day = as.integer("01",'%d'),
                         hour = 00, min = 00, sec = 00, tz = "UTC") #DK format
startDate <- date(startdate)                                        #SE format
enddate <- ISOdatetime(year = as.integer(strftime(today(),'%Y')),
                       month = as.integer(strftime(today(),'%m')),
                       day = as.integer(strftime(today(),'%d')),
                       hour = 00, min = 00, sec = 00, tz = "UTC")   #DK format
endDate <- date(enddate) - 1                                        #SE format

### LOCATIONS
station_id <- c(20947, 40007, 40141, 40143, 40144, 40145, 41105, 45001, 45002, 45003, 45004, 45005, 45006) # NBRs stations = 40141, 40142, 40143, 40144, 40145
station_names <-c("Borgeby","Sandby gård","Gretelund","Lovisero","Tofta","Hviderup", "Jonstorp", "Horslunde", "Lungholm", "Eskilstrup", "Snesere", "Glumsø", "Odense S")
station_land <- c("SE","SE","SE","SE","SE","SE","SE","DK","DK","DK","DK","DK", "DK")
station_lat <- c(55.7530, 55.4395, 55.8864, 55.3772, 55.8935, 55.7779, 56.2295, 54.912747, 54.667524, 54.842805, 55.150173, 55.359515, 55.335240)
station_long <- c(13.0396, 14.1827, 14.0355, 13.4048, 12.9146, 13.3217, 12.6535, 11.199119, 11.450440, 11.916009, 11.917179, 11.751473, 10.356779)
stations <- data.frame("WSTNID" = station_id, "WSTN_namn" = station_names, "land" = station_land,"lat" = station_lat, "long" = station_long)

lat_dk <- pull(filter(stations, land=="DK"),lat)
long_dk <- pull(filter(stations, land=="DK"),long)
coordinate <- paste0(c(rbind(lat_dk, ",", long_dk, "+")), collapse = "")
coordinate <- substr(coordinate,1,nchar(coordinate)-1)

### DATA AND INTERVAL
#### SE / LANTMET
logInterval <- 1                                # 1= hourly, 2 = daily
elementMeasurement <- "TM"
#### DK / METEOMETICS
interval <- "PT1H"
parameters <- "t_2m:C"



## Calculation and reporting of degree days and vernalisation hours
stations_rep <- c(20947, 40007, 40141, 40143, 40144, 40145, 41105)
dates <- c("2022-03-02", "2022-03-14", "2022-03-21") # Sow dates. Cd and Vern-hours are calculated from these dates
date_report <- Sys.Date()-1
Cd_base <- 3  # base temperature for degree day calculations.

## Overview tables
summ_stations <- data.frame(station_id, station_names)
dates_all <- c("2022-01-01", dates)
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


## DENMARK
source('R_query_api.R')

### Account data
access <-read.csv2("www/access.csv")
username <- unlist(access["meteo_user"])
password <- unlist(access["meteo_pass"])
 
dat_in_DK <- timeseries(startdate, enddate, interval, parameters, coordinate)
# write_xlsx(list(full_data = dat_in_DK), "Cd_Cv_2022_DK.xlsx")

# dat_in_DK <- read_xlsx("Cd_Cv_2022_DK.xlsx")


## COMBINE
### SWEDEN
dat_in_SE <- dat_in_SE %>%
  left_join(stations, by = "WSTNID")
### DENMARK
dat_in_DK <- dat_in_DK %>%
  left_join(stations, by = "lat") %>%
  rename(TM = "t_2m:C") %>%
  mutate(lon = NULL) %>%
  mutate(HOUR = hour(ymd_hms(validdate))) %>%
  mutate(DAY = date(ymd_hms(validdate)))
### COMBINED
dat_in <- dat_in_SE %>%
  bind_rows(dat_in_DK)

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
  filter(DAY > "2022-03-12") %>%
  group_by(WSTNID) %>%
  mutate(Cv = -1*(Cv_0- max(Cv_0))) %>%
  filter(DAY < "2022-04-05") %>%
  ungroup()


############################################
############################################
# Add in historic
dat_in_SE_hist <- read_xlsx("Cd_Cv_year_aves.xlsx")

dat_in_SE_hist <- dat_in_SE_hist %>%
  transmute(DAY = as.IDate(DAY), Cv = All_2018, WSTN_namn = "2016-21", land = "SE") %>%
  mutate(Cv = Cv - Cv[which(DAY == endDate)]) %>%
  filter(DAY < "2022-04-05")

dat_sum <- dat_sum %>%
  bind_rows(dat_in_SE_hist) 

############################################
############################################
# Data for 118

############################################
############################################
# IMAGE FOR WEBSITE
img = readPNG("NBR_RGB.png")

## SWEDEN
dat_sum_SE <- dat_sum %>%
  filter(land == "SE") %>%
  rename(Väderstationer = WSTN_namn)

xmax = as.Date(max(dat_sum_SE$DAY))
xmin = as.Date(max(dat_sum_SE$DAY))-3
ymax = max(dat_sum_SE$Cv)
ymin = max(dat_sum_SE$Cv)-8
ytext = max(dat_sum_SE$Cv[which(dat_sum_SE$Väderstationer == "2016-21")])-5

Cv_SE_jpg <- ggplot(dat_sum_SE, aes(x=DAY, y=Cv, group = Väderstationer))+
  geom_line(aes(color = Väderstationer, linetype = Väderstationer), size = 1) + 
  ggtitle(paste("VERNALISATIONSTIMMAR. Uppdaterad senast: ", endDate)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45),
        legend.position="bottom") +
  labs(x="Sådatum", y= "Vernalisationstimmar", group = "Väderstationer") +
  scale_x_date(date_labels="%d %b",date_breaks  ="1 day") +
  scale_y_continuous(breaks=seq(0,200,5)) +
  scale_linetype_manual(values = c("solid","solid","twodash","dashed","solid","twodash","dashed","solid")) +
  annotation_custom(rasterGrob(img), xmax = xmax, xmin = xmin, ymax = ymax, ymin = ymin) +
  geom_text(x = as.IDate("2022-03-16"), y = ytext, label = "Medel 2016-2021", angle = -28, size = 3)

# PUT IN A LINE AT 120 AND 140 Cv WHEN IT IS TIME....
#Cv_SE_jpg

jpeg("C:/Dropbox/Sockerbetor NBR/Sockerbetor 2022/Weather_data/Cv_SE.jpeg", units = "in", width = 7, height = 5, res = 700)
Cv_SE_jpg
dev.off()

jpeg("Cv_SE.jpeg", units = "in", width = 7, height = 5, res = 700)
Cv_SE_jpg
dev.off()

## DENMARK
dat_sum_DK <- dat_sum %>%
  filter(land == "DK") %>%
  rename(Vejrstationer = WSTN_namn)

xmax = as.Date(max(dat_sum_DK$DAY))
xmin = as.Date(max(dat_sum_DK$DAY))-3
ymax = max(dat_sum_DK$Cv)
ymin = max(dat_sum_DK$Cv)-8

Cv_DK_jpg <- ggplot(dat_sum_DK, aes(x=DAY, y=Cv, group = Vejrstationer))+
  geom_line(aes(color = Vejrstationer, linetype = Vejrstationer), size = 1) + 
  ggtitle(paste("VERNALISATIONSTIMER. Sidst opdateret d.: ", endDate)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45),
        legend.position="bottom") +
  labs(x="Sådato", y= "Vernalisationstimer", group = "Vejrstationer") +
  scale_x_date(date_labels="%d %b",date_breaks  ="1 day") +
  scale_y_continuous(breaks=seq(0,200,5)) +
  scale_linetype_manual(values = c("solid","twodash","dashed","solid","twodash","dashed")) +
  annotation_custom(rasterGrob(img), xmax = xmax, xmin = xmin, ymax = ymax, ymin = ymin)

# PUT IN A LINE AT 120 AND 140 Cv WHEN IT IS TIME....
# Cv_DK_jpg

jpeg("C:/Dropbox/Sockerbetor NBR/Sockerbetor 2022/Weather_data/Cv_DK.jpeg", units = "in", width = 7, height = 5, res = 700)
Cv_DK_jpg
dev.off()

jpeg("Cv_DK.jpeg", units = "in", width = 7, height = 5, res = 700)
Cv_DK_jpg
dev.off()

############################################
############################################

write_xlsx(list(summary = dat_sum, full_data = dat, summ_stations = summ_stations, summ_sådatum = summ_dates, summ_report_date = data.frame(date_report)), 
           "C:/Dropbox/Sockerbetor NBR/Sockerbetor 2022/Weather_data/Cd_Cv_2022.xlsx")
write_xlsx(list(summary = dat_sum, full_data = dat, summ_stations = summ_stations, summ_sådatum = summ_dates, summ_report_date = data.frame(date_report)), 
           "Cd_Cv_2022.xlsx")

##############################

# drop_auth(rdstoken = "www/token.rds")
# drop_upload("Cd_Cv_2022.xlsx", path = "/Sockerbetor NBR/Sockerbetor 2022/Weather_data")

# token <- drop_auth()
# saveRDS(token, file = "www/token.rds")
# # I THINK THAT IT'S THEN BEST TO DELETE THE .HTTR.TOKEN FILE (OR WHATEVER IT'S CALLED)
# token <- readRDS("www/token.rds")
# drop_upload("Cd_Cv_2022.xlsx", path = "/Sockerbetor NBR/Sockerbetor 2022/Weather_data", dtoken = token)

##############################