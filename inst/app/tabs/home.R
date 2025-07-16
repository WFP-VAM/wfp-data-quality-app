homeUI <- function() {
  tagList(
    fluidRow(
                          box(
                            title = "Guidance",
                            status = "primary", solidHeader = TRUE, width = 12,
                            HTML(
                              "<ol>\
            <li><strong>Data Upload Tab</strong>: upload your data in <strong>SPSS (.sav)</strong> format. Max size 200 MB.<br>\
              <ul>\
                <li><strong>Upload .sav file</strong>: Select <strong>Upload .sav file</strong> under <strong>Choose data source</strong> dropdown list to browse your file.</li>\
              </ul>\
              Once loaded, you’ll see a preview of your data.\
            </li>\
            <li>Ensure your dataset contains <strong>all variables in the table below</strong>, plus the <strong>standard variable names of the key indicators</strong>: \
              <strong><a href='https://docs.wfp.org/api/documents/WFP-0000134782/download/' target='_blank'>FCS</a></strong>, \
              <strong><a href='https://docs.wfp.org/api/documents/WFP-0000136453/download/' target='_blank'>HDDS</a></strong>, \
              <strong><a href='https://docs.wfp.org/api/documents/WFP-0000109062/download/' target='_blank'>HHS</a></strong>, \
              <strong><a href='https://docs.wfp.org/api/documents/WFP-0000109756/download/' target='_blank'>rCSI</a></strong>, \
              <strong><a href='https://docs.wfp.org/api/documents/WFP-0000134094/download/' target='_blank'>LCS-FS</a></strong> \
              <strong><a href='https://docs.wfp.org/api/documents/WFP-0000147820/download//' target='_blank'>(List of strategies)</a></strong> and \
              <strong><a href='https://docs.wfp.org/api/documents/WFP-0000134243/download/' target='_blank'>FES</a></strong>.\
              <strong>This is mandatory for the app to work.</strong>\
            </li>\
            <li>If a variable (e.g. supervisor’s name) is missing, create an empty column named <code>EnuSupervisorName</code>.</li>\
            <li>If you encounter issues, please contact \
              <strong><a href='mailto:rbd.ram@wfp.org'>WACARO RAM</a></strong> or \
              <strong><a href='mailto:alioubadara.samake@wfp.org'>Aliou Badara SAMAKE</a></strong>.\
            </li>\
          </ol>"
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

