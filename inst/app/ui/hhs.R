hhsUI <- function() {
tagList(
     fluidRow(
         tabBox(
             id = "hhsTabs", width = 12,

             ## Sub-tab: HHS by Admin1
             tabPanel(
                 "HHS by Admin1",
                 fluidRow(
                     box(
                         title = "HHS (CH phases) by Admin1", width = 12, status = "primary",
                         plotlyOutput("plotHHSadm1")
                     )
                 )
             ),

             ## Sub-tab: HHS by Admin2
             tabPanel(
                 "HHS by Admin2",
                 fluidRow(
                     column(
                         4,
                         box(
                             title = "Filters", width = 12, status = "warning", solidHeader = TRUE,
                             # Force user to pick exactly one Admin1
                             selectInput("admin1FilterHHS2", "Admin1:",
                                 choices = character(0), # we'll populate from server
                                 selected = NULL
                             ),
                         )
                     ),
                     column(
                         8,
                         box(
                             title = "HHS (CH phases) by Admin2", width = 12, status = "primary", solidHeader = TRUE,
                             plotlyOutput("plotHHSadm2")
                         )
                     )
                 )
             ),

             ## Sub-tab: HHS by Admin1 & Enumerator
             tabPanel(
                 "HHS by Admin1/2 & Enumerator",
                 fluidRow(
                     column(
                         4,
                         box(
                             title = "Filters", width = 12, status = "warning", solidHeader = TRUE,
                             # Force user to pick exactly one Admin1
                             selectInput("admin1FilterEnumHHS", "Admin1:",
                                 choices = character(0),
                                 selected = NULL
                             ),
                             # Admin2 filter with "All" option
                             selectInput("admin2FilterEnumHHS", "Admin2:",
                                 choices = c("All"), # we'll populate from server
                                 selected = "All"
                             ),
                         )
                     ),
                     column(
                         8,
                         box(
                             title = "HHS (CH phases) by Admin1/2 & Enumerator", width = 12, status = "primary", solidHeader = TRUE,
                             plotlyOutput("plotHHSadm1Enum")
                         )
                     )
                 )
             ),
         ) # end tabBox
     )
                   
    )
}