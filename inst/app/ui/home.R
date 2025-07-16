

homeUI <- function() {
  tagList(
    fluidRow(
      box(
        title = "Guidance",
        status = "primary", 
        solidHeader = TRUE, 
        width = 12,
        tags$ol(
          tags$li(
            strong("Data Upload Tab"), ": upload your data in ", 
            strong("SPSS (.sav)"), " format. Max size 200 MB.",
            tags$ul(
              tags$li(
                strong("Upload .sav file"), ": Select ", 
                strong("Upload .sav file"), 
                " under ", 
                strong("Choose data source"), 
                " dropdown list to browse your file."
              )
            ),
            "Once loaded, you'll see a preview of your data."
          ),
          tags$li(
            "Ensure your dataset contains ", 
            strong("all variables in the table below"), 
            ", plus the ", 
            strong("standard variable names of the key indicators"), ": ",
            make_link("FCS", "https://docs.wfp.org/api/documents/WFP-0000134782/download/"), ", ",
            make_link("HDDS", "https://docs.wfp.org/api/documents/WFP-0000136453/download/"), ", ",
            make_link("HHS", "https://docs.wfp.org/api/documents/WFP-0000109062/download/"), ", ",
            make_link("rCSI", "https://docs.wfp.org/api/documents/WFP-0000109756/download/"), ", ",
            make_link("LCS-FS", "https://docs.wfp.org/api/documents/WFP-0000134094/download/"), " ",
            make_link("(List of strategies)", "https://docs.wfp.org/api/documents/WFP-0000147820/download/"), " and ",
            make_link("FES", "https://docs.wfp.org/api/documents/WFP-0000134243/download/"), ".",
            strong("This is mandatory for the app to work.")
          ),
          tags$li(
            "If a variable (e.g. supervisor's name) is missing, create an empty column named ",
            tags$code("EnuSupervisorName"), "."
          ),
          tags$li(
            "If you encounter issues,",
            tags$a(href = "https://github.com/WFP-VAM/wfp-data-quality-app/issues", "please open an Issue on the main GitHub repository")
          )
        )
      )
    ),
    fluidRow(
      box(
        title = "Required Variables",
        status = "info", solidHeader = TRUE, width = 12,
        HTML(
          "<table class='table table-striped'>\
            <thead>\
              <tr>\
                <th>Variable name</th><th>Label</th>\
              </tr>\
            </thead>\
            <tbody>\
              <tr><td>RESPConsent</td><td>Consent form respondent to do the survey (1 = Yes, 0 = No)</td></tr>\
              <tr><td>ADMIN1Name</td><td>Admin1 area (region/wilaya)</td></tr>\
              <tr><td>ADMIN2Name</td><td>Admin2 area (department/cerlce/district)</td></tr>\
              <tr><td>ADMIN4Name</td><td>Community/village/disaggregated area</td></tr>\
              <tr><td>EnuSupervisorName</td><td>Name of supervisor</td></tr>\
              <tr><td>EnuName</td><td>Name of Enumerator</td></tr>\
              <tr><td>HHSizeCalc</td><td>Household size (Total number of household members)</td></tr>\
            </tbody>\
          </table>"
        )
      )
    )
  )
}

