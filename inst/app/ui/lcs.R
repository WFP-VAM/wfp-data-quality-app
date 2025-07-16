lcsUI <- function() {
    tagList(
        # insert here code
                        fluidRow(
                            tabBox(
                                id = "lcsTabs", width = 12,

                                ## Sub-tab: "Dynamic LCS" to select strategies
                                tabPanel(
                                    "Dynamic LCS",
                                    fluidRow(
                                        column(
                                            4,
                                            box(
                                                title = "Select Strategies", status = "warning", solidHeader = TRUE, width = 12,

                                                # 1) Stress strategies
                                                selectInput("stressVars", "Pick EXACTLY 4 Stress Strategies:",
                                                    multiple = TRUE,
                                                    # No default 'selected' – starts empty
                                                    choices = c(
                                                        "Lcs_stress_Saving", "Lcs_stress_DomAsset", "Lcs_stress_ConsActive",
                                                        "Lcs_stress_SellFoodRation", "Lcs_stress_SellNFIRation", "Lcs_stress_EatOut",
                                                        "Lcs_stress_BorrowCash", "Lcs_stress_Pawn", "Lcs_stress_LessSchool",
                                                        "Lcs_stress_Utilities", "Lcs_stress_Edu", "Lcs_stress_BorrowFood",
                                                        "Lcs_stress_MoreLabour", "Lcs_stress_HHSeparation", "Lcs_stress_Housing",
                                                        "Lcs_stress_LessSchool", "LcsR_stress_Animals", "LcsR_stress_BorrowCash",
                                                        "LcsR_stress_Pawn", "LcsR_stress_DomAsset", "LcsR_stress_EatOut",
                                                        "LcsR_stress_LessSchool", "LcsR_stress_Saving", "LcsR_stress_HHSeparation",
                                                        "LcsR_stress_ConsActive", "LcsR_stress_SellFoodRation", "LcsR_stress_DomMigration",
                                                        "LcsR_stress_Housing", "LcsR_stress_SellNFIRation", "LcsR_stress_Edu"
                                                    )
                                                ),

                                                # 2) Crisis strategies
                                                selectInput("crisisVars", "Pick EXACTLY 3 Crisis Strategies:",
                                                    multiple = TRUE,
                                                    choices = c(
                                                        "Lcs_crisis_ProdAssets", "Lcs_crisis_Barter", "Lcs_crisis_Health",
                                                        "Lcs_crisis_Housing", "Lcs_crisis_HHSeparation", "Lcs_crisis_OutSchool",
                                                        "Lcs_crisis_Migration", "Lcs_crisis_DomMigration", "Lcs_crisis_ChildWork",
                                                        "Lcs_crisis_Edu_Health", "Lcs_crisis_Barter", "Lcs_crisis_ConsActive",
                                                        "Lcs_crisis_Edu", "Lcs_crisis_Health", "Lcs_crisis_Marriage", "Lcs_crisis_Utilities",
                                                        "LcsR_crisis_AgriCare", "LcsR_crisis_ImmCrops",
                                                        "LcsR_crisis_Seed", "LcsR_crisis_Animals", "LcsR_crisis_Health",
                                                        "LcsR_crisis_Edu", "LcsR_crisis_ProdAssets", "LcsR_crisis_Housing",
                                                        "LcsR_crisis_HHSeparation", "LcsR_crisis_Barter", "LcsR_crisis_Migration",
                                                        "LcsR_crisis_ChildWork", "LcsR_crisis_Marriage", "LcsR_crisis_ConsActive",
                                                        "LcsR_crisis_OutSchool", "LcsR_crisis_DomMigration"
                                                    )
                                                ),

                                                # 3) Emergency strategies
                                                selectInput("emergencyVars", "Pick EXACTLY 3 Emergency Strategies:",
                                                    multiple = TRUE,
                                                    choices = c(
                                                        "Lcs_em_ChildMigration", "Lcs_em_IllegalAct", "Lcs_em_Begged",
                                                        "Lcs_em_Marriage", "Lcs_em_ResAsset", "Lcs_em_Migration", "Lcs_em_ChildWork",
                                                        "Lcs_em_OutSchool", "LcsR_em_FemAnimal", "LcsR_em_WildFood", "LcsR_em_Seed",
                                                        "LcsR_em_OutSchool", "LcsR_em_Migration", "LcsR_em_ChildWork", "LcsR_em_Marriage",
                                                        "LcsR_em_ResAsset", "LcsR_em_Begged", "LcsR_em_IllegalAct", "LcsR_em_ChildMigration"
                                                    )
                                                )
                                            )
                                        ),
                                        column(
                                            8,
                                            box(
                                                title = "Dynamic LCS Results", status = "primary", solidHeader = TRUE, width = 12,
                                                # We'll display a plot (or any other output) here
                                                plotlyOutput("plotDynamicLCS"),
                                                # Also display an info/warning text if there's a problem
                                                textOutput("dynamicLCSWarning")
                                            )
                                        )
                                    )
                                ),

                                ## Sub-tab: LCS by Admin1
                                tabPanel(
                                    "LCS by Admin1",
                                    fluidRow(
                                        box(
                                            title = "LCS by Admin1", width = 12, status = "primary",
                                            plotlyOutput("plotLHCSadm1")
                                        )
                                    )
                                ),

                                ## Sub-tab: LCS by Admin2
                                tabPanel(
                                    "LCS by Admin2",
                                    fluidRow(
                                        column(
                                            4,
                                            box(
                                                title = "Filters", width = 12, status = "warning", solidHeader = TRUE,
                                                # Force user to pick exactly one Admin1
                                                selectInput("admin1FilterLCS2", "Admin1:",
                                                    choices = character(0), # we'll populate from server
                                                    selected = NULL
                                                ),
                                            )
                                        ),
                                        column(
                                            8,
                                            box(
                                                title = "LCS by Admin2", width = 12, status = "primary", solidHeader = TRUE,
                                                plotlyOutput("plotLHCSadm2")
                                            )
                                        )
                                    )
                                ),

                                ## Sub-tab: LCS by Admin1 & Enumerator
                                tabPanel(
                                    "LCS by Admin1/2 & Enumerator",
                                    fluidRow(
                                        column(
                                            4,
                                            box(
                                                title = "Filters", width = 12, status = "warning", solidHeader = TRUE,
                                                # Force user to pick exactly one Admin1
                                                selectInput("admin1FilterEnumLCS", "Admin1:",
                                                    choices = character(0),
                                                    selected = NULL
                                                ),
                                                # Admin2 filter with "All" option
                                                selectInput("admin2FilterEnumLCS", "Admin2:",
                                                    choices = c("All"), # we'll populate from server
                                                    selected = "All"
                                                ),
                                            )
                                        ),
                                        column(
                                            8,
                                            box(
                                                title = "LCS by Admin1/2 & Enumerator", width = 12, status = "primary", solidHeader = TRUE,
                                                plotlyOutput("plotLHCSadm1Enum")
                                            )
                                        )
                                    )
                                ),

                                # Sub-tab: LCS by Strategy
                                tabPanel(
                                    "LCS by Strategy",
                                    fluidRow(
                                        column(
                                            4,
                                            box(
                                                title = "Filters", status = "warning", solidHeader = TRUE, width = 12,
                                                selectInput("lcsStrategyAdmin1", "Select Admin1:",
                                                    choices = character(0), selected = NULL
                                                ),
                                                selectInput("lcsStrategyAdmin2", "Select Admin2:",
                                                    choices = c("All"), selected = "All"
                                                ),
                                                selectInput("lcsStrategyEnu", "Select Enumerator:",
                                                    choices = c("All"), selected = "All"
                                                )
                                            )
                                        ),
                                        column(
                                            8,
                                            box(
                                                title = "Breakdown of Livelihood Coping Strategies (LCS)", status = "primary", solidHeader = TRUE, width = 12,
                                                plotlyOutput("plotLCSStrategy")
                                            )
                                        )
                                    )
                                ),
                            ) # end tabBox
                        )
    )
}