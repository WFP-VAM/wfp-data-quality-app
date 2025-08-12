############################
## Data Quality Check App
#This is a data quality check tool for food security assessment in near real time.
#The app was originally developed by WFP West Africa Regional RAM team with support from all COs.
#The app is structured in 3 main parts: User Interface (UI), Server Code and Report Template.
############################

source("ui.R")
source("server.R")


shinyApp(ui = ui, server = server)
