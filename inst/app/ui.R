# Shiny related
library(shiny)
library(shinydashboard)
library(htmltools)

# File handling
library(openxlsx)
library(writexl)
library(jsonlite)

# Utilities
library(lubridate)
library(scales)
library(glue)
library(rlang)
library(httr)
library(kableExtra)
library(rmarkdown)
library(rstatix)

# Custom UI modules
my_path <- c("modules/ui/") # set your path
source_files <- list.files(my_path, "*.R$") # locate all .R files
map(paste0(my_path, source_files), source)

# Global options
options(shiny.maxRequestSize = 500 * 1024^2) 

# UI 
ui <- dashboardPage(
    dashboardHeader(title = "WFP Data Quality Check"),
    dashboardSidebar(
        sidebarMenu(
            id = "tabs",
            menuItem("Home", tabName = "home", icon = icon("home")),
            menuItem("Data Upload", tabName = "upload", icon = icon("upload")),
            menuItem("Survey Progress", tabName = "survey", icon = icon("chart-line")),
            menuItem("FCS", tabName = "fcs", icon = icon("utensils")),
            menuItem("HDDS", tabName = "hdds", icon = icon("leaf")),
            menuItem("rCSI", tabName = "rcsi", icon = icon("heartbeat")),
            menuItem("HHS", tabName = "hhs", icon = icon("home")),
            menuItem("MATRIX", tabName = "matrix", icon = icon("table")),
            menuItem("Livelihood Coping (LCS)", tabName = "lcs", icon = icon("users")),
            menuItem("FES", tabName = "fes", icon = icon("dollar")),
            menuItem("CARI", tabName = "cari", icon = icon("clipboard-check")),
            menuItem("Report", tabName = "report", icon = icon("file-alt"))
        )
    ),
    dashboardBody(
        tags$head(
            includeCSS("www/custom.css")
        ),
        tabItems(
            tabItem( tabName = "home", homeUI() ),
            tabItem( tabName = "upload", dataUploadUI() ),
            tabItem( tabName = "survey", surveyProgressUI() ),
            tabItem( tabName = "fcs", fcsUI() ),
            tabItem( tabName = "hdds", hhdsUI() ),
            tabItem( tabName = "rcsi", rcsiUI() ),
            tabItem( tabName = "hhs", hhsUI() ),
            tabItem( tabName = "matrix", fewsnetMatrixUI() ),
            tabItem( tabName = "lcs", lcsUI() ),
            tabItem( tabName = "fes", fesUI() ),
            tabItem( tabName = "cari", cariUI() ),
            tabItem( tabName = "report", reportUI()
            )
        ) # end tabItems
    )
)
