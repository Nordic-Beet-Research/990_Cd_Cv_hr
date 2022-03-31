############################################
############################################
##
## NBR Weather data - Shiny App
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
                        "shiny_1.7.1",
                        "shinyWidgets_0.6.2",
                        "plotly_4.10.0",
                        "readxl_1.3.1",
                        "RCurl_1.98-1.5"
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
# source data from Dropbox
# token <- drop_auth()
# saveRDS(token, file = "www/token.rds")
# drop_auth()
# token <- readRDS("www/token.rds")
# drop_download(path = "/Sockerbetor NBR/Sockerbetor 2022/Weather_data/Cdv_2022.xlsx", overwrite = T, dtoken = token)
# Or just download this straight from ssh
# x = scp(host = "192.168.130.212", path = "/home/william/R/temp_dat/Cdv_2022.xlsx", keypasswd = "PASSWORD", user = "william")

# import data to R
dat_raw <- read_xlsx("Cd_Cv_2022.xlsx", sheet = "full_data")
dat_stations <- read_xlsx("Cd_Cv_2022.xlsx",sheet="summ_stations")
dat_stations <- rename(dat_stations, WSTNID = stations, WSTNNM = stations_names)
dat_raw <-left_join(dat_raw, dat_stations, by="WSTNID")
dat_sadatum <- read_xlsx("Cd_Cv_2022.xlsx",sheet="summ_sådatum")
dat_report_max <- max(unique(dat_raw$DAY))

ui <- fluidPage(
  titlePanel("Cumulative temperatures"),
  sidebarLayout(
    sidebarPanel(
      # define parameters for summary table
      checkboxGroupInput("in_station", h4("Weather stations"), choices = dat_stations$WSTNNM, selected = dat_stations$WSTNNM[1]),
      airDatepickerInput("in_sådatum", h4("Sådatum"), value = c("2022-01-01", "2022-02-10"), multiple = T),
      airDatepickerInput("in_report_date", h4("Report date"), value = dat_report_max),
      checkboxGroupInput("in_metric", h4("Metrics"), choices = list("Daggrader" = "Cd", "Vernalisation timmar" = "Cv"), selected = "Cv"),
      sliderInput("ref_hr", h4("Reference hour"), min = 0, max = 23, value = 12)
    ),
    mainPanel(tableOutput("tab_summ"))
  )
)
#list("Borgeby" = 20947, "Gretelund" = 40141)

server <- function(input, output){
  dat_summ <- reactive({
    in_metric_p <- input$in_metric
    in_report_date_p <- input$in_report_date
    in_station_p <- input$in_station
    in_sådatum_p <- input$in_sådatum
    ref_hr_p <- input$ref_hr
    
    dat_summ <- expand.grid(report_date = in_report_date_p, sådatum = in_sådatum_p, station = in_station_p, metric = in_metric_p)
    
    for(i in 1:nrow(dat_summ)){
      dat_summ$val_0[i] <- dat_raw %>% 
        filter(DAY == as.POSIXct(dat_summ$report_date[i], tz = "UTC"), HOUR == ref_hr_p, WSTNNM == dat_summ$station[i]) %>%
        select(paste0(dat_summ$metric[i],"_0")) %>%
        unlist
    }
    
    for(i in 1:nrow(dat_summ)){
      dat_summ$val_1[i] <- dat_raw %>% 
        filter(DAY == as.POSIXct(dat_summ$sådatum[i], tz = "UTC"), HOUR == ref_hr_p, WSTNNM == dat_summ$station[i]) %>%
        select(paste0(dat_summ$metric[i],"_0")) %>%
        unlist
    }
    
    dat_summ <- dat_summ %>%
      mutate(val = val_0 - val_1, val_0 = NULL, val_1 = NULL) %>%
      pivot_wider(names_from = "metric", values_from = "val") %>%
      relocate(station, sådatum) %>%
      mutate_at(c("report_date","sådatum"), as.character) %>%
      rename_with(toupper)
    
    dat_summ
  })
  
  output$tab_summ <- renderTable(dat_summ())
}

shinyApp(ui = ui, server = server)

## build a matrix of values


# summary table
# val_0 <- unlist(subset(dat_raw, DAY == as.POSIXct(dat_summ$sådatum[3], tz = "UTC") & HOUR == ref_hr & 
#                                  WSTNID == dat_summ$station[3])[paste0(dat_summ$metric[3],"_0")])


#########################
## DEBUG

in_metric_p <- c("Cd","Cv")
in_report_date_p <- dat_report_max
in_station_p <- c(20947,40141)
in_sådatum_p <- c("2022-01-01", "2022-01-24")
ref_hr_p <- 12
