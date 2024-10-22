install.packages("fredr")

library(csodata)
library(tidyverse)
library(onsr)
library(zoo)
library(pxweb)
library(oenb)
library(danstat)
library(insee)
library(readsdmx)
library(BFS)
library(readxl)
library(fredr)

install.packages(file.choose(), repos = NULL)

datalist_raw = list()

############################### 1) Ireland (CSO) ###############################
ireland_load <- cso_get_data(
                            "CPM01",
                            pivot_format = "tall",
                            use_dates = TRUE,
                          ) 

ie_transform <- function(data, start_date){
  data <- data %>%
    mutate(Month = as_date(Month)) %>%
    filter(Month >= start_date,
           Statistic == 'Consumer Price Index (Base Dec 2016=100)') %>%
    mutate(Source = 'CSO',
           Name = paste0('CPI-', Commodity.Group),
           Value = value,
           `Indicator | Country | Source` = paste0(Name, ' | CSO | IE'))
  .GlobalEnv$datalist_raw$ireland <-data
}

ie_transform(ireland_load, '2017-01-01')

############################### 2) UK (ONS) ####################################

uk_load <- read.csv('https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindices/current/mm23.csv')

uk_transform <- function(data, start_date){
  data <- data %>%
    mutate(Month = as.Date(as.yearmon(Time, "%B-%y"))) %>%
    mutate(Name = case_when(
      grepl("\\d", Aggregate) ~ sub(".*? ", "", Aggregate),
      TRUE ~ Aggregate),
           Source = 'ONS',
          `Indicator | Country | Source` = paste0(Name, ' | ONS | UK'),
      Value = v4_0) %>%
    filter(Month > start_date)
  .GlobalEnv$datalist_raw$uk <-data
}

uk_transform(uk_load, '2017-01-01')

############################### Austria (SA) ################################
# 1) List of datasets
toc <- oenb_toc()

# 2) Series contained in above dataset
overview <- oenb_dataset(id = "6")

# 3) List of attributes of series
attrib <- oenb_attributes(id = "6", pos = "VDBPLVPIG15")

# 4) Download series
series <- oenb_data(id = "6", pos = "VDBPLVPIG15")

############################### Belgium (SB) ################################

be <- read.csv("https://bestat.statbel.fgov.be/bestat/api/views/208b69bd-05c5-4947-b7f9-2d2300f517b8/result/CSV")

############################### Denmark (StatBank) ################################
get_subjects()
subj <- get_subjects(subjects = c("2","3"))
subsubjects <- subj$subjects %>% bind_rows()
subsubjects
tables <- get_tables(subjects = c("3436")) 

vars_cpi <- get_table_metadata(table_id = "PRIS113", variables_only = TRUE) 

vars_cpi %>%
  select(id, text)

variable_codes <- vars_cpi$id[c(1,2)]
variable_values <- list(NA)

variable_input <- purrr::map2(.x = variable_codes, .y = variable_values, .f = ~list(code = .x, values = .y))

cpi <- get_data("PRIS113", variables = variable_input)


############################### Finland (SA) ################################

fl_query <- list("Kuukausi" = c("2023M12","2024M01"),
                 "Hyödyke" = c("0"),
                 "Tiedot" = c("indeksipisteluku"))

fl_request <- pxweb_get(url="https://statfin.stat.fi:443/PxWeb/api/v1/en/StatFin/khi/statfin_khi_pxt_11xb.px",
                        query = fl_query)

fl_load <- as.data.frame(fl_request, column.name.type = "text", variable.value.type = "text")

############################### France (Insee) #################################

fr <- get_insee_idbank("001769682")

############################### Italy (Istat) ##################################

it_url <- paste("https://esploradati.istat.it/SDMXWS/rest/data/IT1,167_744_DF_DCSP_NIC1B2015_1,1.0/M..../ALL/?detail=full&startPeriod=2023-01-31&endPeriod=2024-01-31&dimensionAtObservation=TIME_PERIOD",
             sep = "")

it_load <- read_sdmx(it_url)

############################### Norway (SSB) ################################

no_query <- list("ContentsCode"=c("KpiIndMnd"),
                 "Tid"=c("2023M12","2024M01"))



no_request <- pxweb_get(url="https://data.ssb.no/api/v0/en/table/03013",
                        query = no_query)

no_load <- as.data.frame(no_request, column.name.type = "text", variable.value.type = "text")

############################### Sweden (SCB) ################################

se_query = list("ContentsCode"=c("000004VU"),
             "Tid"=c("2023M12","2024M01"))

se_request <- pxweb_get(url="https://api.scb.se/OV0104/v1/doris/en/ssd/PR/PR0101/PR0101A/KPItotM",
                  query = se_query)

se_load <- as.data.frame(se_request, column.name.type = "text", variable.value.type = "text")

############################### Switzerland (FSO) ################################
catalog_data_en <- bfs_get_catalog_data(language = "en")

bfs_get_data(number_bfs = "px-x-1502040100_131", language = "en")

############################### FRED (USA) ################################
fredr_set_key("396439e001b287331e3dff8b33b4d61c")
fredr_has_key()

fred_test <- fredr(series_id = "LCWRTT01AUQ661N")

ireland <- fredr_request(
  endpoint = "tags/series", 
  tag_names = "ireland"
)

switzerland <- fredr_request(
  endpoint = "tags/series", 
  tag_names = "switzerland; construction"
)

write.csv(switzerland, 'fred_switzerland_series.csv', row.names = FALSE)

write.csv(ireland, 'fred_ireland_series.csv', row.names = FALSE)

##### We need to use FRED to query against country and type of indicator and then look at the csv lists


############################### Merge Tables ####################################
select_cols <- function(data){
  data <- data %>%
    select(Month, Source, `Indicator | Country | Source`, Value)
}

### Select columns from created dataframes per API and combine into single dataframe
datalist <- lapply(datalist_raw, select_cols) %>%
  bind_rows()

write.csv(datalist, 'indicators.csv', row.names = FALSE)




