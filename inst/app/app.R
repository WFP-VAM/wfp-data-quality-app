############################
## Data Quality Check App
#This is a data quality check tool for food security assessment in near real time.
#The app was developed by WFP WACARO RAM Team with support from all COs.
#The app is structure in 3 main points: User Interface (UI), Server Code and Report Template.
############################
library(devtools)
library(dplyr)
library(DT)
library(forcats)
library(ggplot2)
library(glue)
library(haven)
library(htmltools)
library(httr)
library(jsonlite)
library(kableExtra)
library(labelled)
library(lubridate)
library(openxlsx)
library(plotly)
library(purrr)
library(rlang)
library(rmarkdown)
library(rstatix)
library(scales)
library(shiny)
library(shinydashboard)
library(tidyr)
library(tidyverse)
library(treemapify) # for treemap
library(writexl)

source("ui.R")
source("server.R")
shinyApp(ui = ui, server = server)
