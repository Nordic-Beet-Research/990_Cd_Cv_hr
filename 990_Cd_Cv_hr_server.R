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
                        "rdrop2_0.8.2.1"
  )
  path_Rpackages = "/home/william/R/package_412"
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
## Data to collect from LantMet
startDate <- c("2022-01-01")                        # read this from previous download?
endDate <- Sys.Date() - 1
stations <- c(20947, 40141, 40142, 40143, 40144, 40145) # NBRs stations = 40141, 40142, 40143, 40144, 40145
stations_names <-c("Borgeby","Gretelund","Valterslund","Lovisero","Tofta","Hviderup")
logInterval <- 1                                # 1= hourly, 2 = daily
elementMeasurement <- "TM"

## Calculation and reporting of degree days and vernalisation hours
stations_rep <- c(20947, 40141, 40142, 40143, 40144, 40145)
dates <- c("2022-01-26","2022-02-10") # Sow dates. Cd and Vern-hours are calculated from these dates
date_report <- Sys.Date()-1
Cd_base <- 3  # base temperature for degree day calculations.

## Overview tables
summ_stations <- data.frame(stations, stations_names)
dates_all <- c("2022-01-01", dates)
summ_dates <- data.frame(seq(0:length(dates)), dates_all)
names(summ_dates) <- c("såtid","datum")

############################################
############################################
# Get data
## create matrix of all urls = startDate x stations x elements
len_startDate <- length(startDate)
len_stations <- length(stations)
len_ele <- length(elementMeasurement)
len_matrix <- len_startDate*len_stations*len_ele
startDate_c <- rep(startDate, len_matrix/len_startDate)
endDate_c <- rep(endDate, len_matrix)
stations_c <- rep(stations, each=len_matrix/len_stations)
logInterval_c <- rep(logInterval, len_matrix)
elementMeasurement_c <- rep(elementMeasurement, each=len_matrix/len_ele)
urlBase <- "https://www.ffe.slu.se/lm/json/downloadJS.cfm?outputType=CSV&AddID=1"

## create url
for (i in 1:len_matrix){
  urlStation <- paste0("weatherStationID=",stations_c[i])
  urlStart <- paste0("startDate=",startDate_c[i])
  urlEnd <- paste0("endDate=",endDate_c[i])
  urlLog <- paste0("LogIntervalID=",logInterval_c[i])
  urlEle <- paste0("elementMeasurementTypeList=",elementMeasurement_c[i])
  url <- paste(urlBase,urlStation,urlStart,urlEnd,urlLog,urlEle,sep="&")
  
  # Import from Lantmet. Need to add a time-out and re-run to this...
  dat_in_i <- data.frame(fread(url))
  dat_in_i <- dat_in_i %>%
    mutate_at(elementMeasurement, as.numeric)
  
  if(i==1L) dat_in <- dat_in_i else dat_in <- rbind(dat_in, dat_in_i)
}

############################################
# QA on the hourly data from 2021
#dat_in$ROW <- as.numeric(row.names(dat_in))
#ggplot(dat=dat_in[which(dat_in$WSTNID==40145),], aes(x=ROW, y=TM))+
#  geom_line()
## 40142 has issues in Jan and Apr... Leave out for now.

############################################
# Calculate degree days and vernalisation hours

for (j in 1:length(stations_rep)){
  dat_j <- dat_in[which(dat_in$WSTNID == stations_rep[j]),]
  ## get positives only, and make them 1/24 of a day
  dat_j$Cd_timvis <- ifelse(dat_j$TM < Cd_base, 0, dat_j$TM-Cd_base)/24
  ## Vernalisation per hour
  dat_j$Cv_timvis <- -1.256 + (1.26 + 0.131*dat_j$TM)*0.9357^dat_j$TM
  dat_j$Cv_timvis <- ifelse(dat_j$Cv_timvis < 0, 0, dat_j$Cv_timvis)
  ## Cumulative values
  dat_j$Cd_0 <- cumsum(dat_j$Cd_timvis)
  dat_j$Cv_0 <- cumsum(dat_j$Cv_timvis)
  
  for (k in 1:length(dates)){
    ## Cd from sowing date
    Cd_k <- min(dat_j$Cd_0[which(dat_j$DAY == as.IDate(dates[k]))])
    dat_j$Cd_k <- dat_j$Cd_0 - Cd_k
    dat_j$Cd_k <- ifelse(dat_j$Cd_k < 0, 0, dat_j$Cd_k)
    colnames(dat_j)[which(names(dat_j) == "Cd_k")] <- paste0("Cd_",k)
    
    ## Vernalisation 
    ### The model is: calculated hourly, not rebased. 
    Cv_k <- min(dat_j$Cv_0[which(dat_j$DAY == as.IDate(dates[k]))])
    dat_j$Cv_k <- dat_j$Cv_0 - Cv_k
    dat_j$Cv_k <- ifelse(dat_j$Cv_k < 0, 0, dat_j$Cv_k)
    colnames(dat_j)[which(names(dat_j) == "Cv_k")] <- paste0("Cv_",k)
    
  }
  if(j==1) dat_Cdv <- dat_j else dat_Cdv <- rbind(dat_Cdv, dat_j)
}

dat_sum <- dat_Cdv %>%
  filter(DAY == as.IDate(date_report)) %>%
  filter(HOUR == 23) %>%
  pivot_longer(cols= 7:ncol(dat_Cdv),names_to = "metric", values_to = "val") %>%
  select(WSTNID, metric, val) %>%
  separate(metric, sep="_", into = c("C","såtid")) %>%
  pivot_wider(names_from = C, values_from = val)

# lazy replace of values
dates_summ <- rep(dates_all, length(stations))
WSTNID_summ <- rep(stations_names, each = length(dates)+1)
dat_sum$WSTN_namn <- WSTNID_summ
dat_sum$såtid <- dates_summ
dat_sum <- dat_sum %>%
  relocate(WSTNID, WSTN_namn, såtid, Cd, Cv)

write_xlsx(list(summary = dat_sum, full_data = dat_Cdv, summ_stations = summ_stations, summ_sådatum = summ_dates, summ_report_date = data.frame(date_report)), "/home/william/R/temp_dat/Cdv_2022.xlsx")

token <- readRDS("/home/william/R/script/token.rds")
drop_upload("/home/william/R/temp_dat/Cdv_2022.xlsx", path = "/Sockerbetor NBR/Sockerbetor 2022/Weather_data", dtoken = token)

#token <- drop_auth()
#saveRDS(token, file = "token.rds")
#token <- readRDS("token.rds")
#drop_upload("Cdv_2022.xlsx", path = "/Sockerbetor NBR/Sockerbetor 2022/Weather_data", dtoken = token)
