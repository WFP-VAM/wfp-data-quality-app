

# Custom UI modules
my_path <- c("modules/ui/") # set your path
source_files <- list.files(my_path, "*.R$") # locate all .R files
map(paste0(my_path, source_files), source)



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
            tabItem( tabName = "home", homeUI("home") ),
            tabItem( tabName = "upload", dataUploadUI("upload") ),
            tabItem( tabName = "survey", surveyProgressUI("survey_progress") ),
            tabItem( tabName = "fcs", fcsUI("fcs") ),
            tabItem( tabName = "hdds", hhdsUI("hdds") ),
            tabItem( tabName = "rcsi", rcsiUI("rcsi") ),
            tabItem( tabName = "hhs", hhsUI("hhs") ),
            tabItem( tabName = "matrix", fewsnetMatrixUI("matrix") ),
            tabItem( tabName = "lcs", lcsUI("lcs") ),
            tabItem( tabName = "fes", fesUI("fes") ),
            tabItem( tabName = "cari", cariUI("cari") ),
            tabItem( tabName = "report", reportUI("report")
            )
        ) # end tabItems
    )
)
