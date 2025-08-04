############################
## Data Quality Check App
#This is a data quality check tool for food security assessment in near real time.
#The app was developed by WFP WACARO RAM Team with support from all COs.
#The app is structure in 3 main points: User Interface (UI), Server Code and Report Template.
############################


source("ui.R")
source("server.R")
shinyApp(ui = ui, server = server)
