process_data <- function(input, processed_data) {
    reactive({
        df <- processed_data()
        req(df) # make sure we have data

        # your entire pipeline in one place

        # Example debug: check structure + labels of one LCS variable
        str(df$Lcs_stress_DomAsset)
        print(attr(df$Lcs_stress_DomAsset, "labels"))

        # 1) Zap labels on LCS columns and HDDS so we see numeric codes (10,20,30,9999, etc.)
        df$Lcs_stress_DomAsset <- haven::zap_labels(df$Lcs_stress_DomAsset)
        df$Lcs_stress_Saving <- haven::zap_labels(df$Lcs_stress_Saving)
        df$Lcs_stress_EatOut <- haven::zap_labels(df$Lcs_stress_EatOut)
        df$Lcs_stress_BorrowCash <- haven::zap_labels(df$Lcs_stress_BorrowCash)
        df$Lcs_stress_ConsActive <- haven::zap_labels(df$Lcs_stress_ConsActive)
        df$Lcs_stress_SellFoodRation <- haven::zap_labels(df$Lcs_stress_SellFoodRation)
        df$Lcs_stress_SellNFIRation <- haven::zap_labels(df$Lcs_stress_SellNFIRation)
        df$Lcs_stress_Pawn <- haven::zap_labels(df$Lcs_stress_Pawn)
        df$Lcs_stress_LessSchool <- haven::zap_labels(df$Lcs_stress_LessSchool)
        df$Lcs_stress_Utilities <- haven::zap_labels(df$Lcs_stress_Utilities)
        df$Lcs_stress_Edu <- haven::zap_labels(df$Lcs_stress_Edu)
        df$Lcs_stress_BorrowFood <- haven::zap_labels(df$Lcs_stress_BorrowFood)
        df$Lcs_stress_MoreLabour <- haven::zap_labels(df$Lcs_stress_MoreLabour)
        df$Lcs_stress_LessSchool <- haven::zap_labels(df$Lcs_stress_LessSchool)
        df$Lcs_stress_Housing <- haven::zap_labels(df$Lcs_stress_Housing)
        df$Lcs_stress_MoreLabour <- haven::zap_labels(df$Lcs_stress_MoreLabour)
        df$LcsR_stress_Animals <- haven::zap_labels(df$LcsR_stress_Animals)
        df$LcsR_stress_BorrowCash <- haven::zap_labels(df$LcsR_stress_BorrowCash)
        df$LcsR_stress_Pawn <- haven::zap_labels(df$LcsR_stress_Pawn)
        df$LcsR_stress_DomAsset <- haven::zap_labels(df$LcsR_stress_DomAsset)
        df$LcsR_stress_EatOut <- haven::zap_labels(df$LcsR_stress_EatOut)
        df$LcsR_stress_LessSchool <- haven::zap_labels(df$LcsR_stress_LessSchool)
        df$LcsR_stress_Saving <- haven::zap_labels(df$LcsR_stress_Saving)
        LcsR_stress_HHSeparation <- haven::zap_labels(df$LcsR_stress_HHSeparation)
        df$LcsR_stress_ConsActive <- haven::zap_labels(df$LcsR_stress_ConsActive)
        df$LcsR_stress_SellFoodRation <- haven::zap_labels(df$LcsR_stress_SellFoodRation)
        df$LcsR_stress_DomMigration <- haven::zap_labels(df$LcsR_stress_DomMigration)
        df$LcsR_stress_Housing <- haven::zap_labels(df$LcsR_stress_Housing)
        df$LcsR_stress_SellNFIRation <- haven::zap_labels(df$LcsR_stress_SellNFIRation)
        df$LcsR_stress_Edu <- haven::zap_labels(df$LcsR_stress_Edu)

        df$Lcs_crisis_ProdAssets <- haven::zap_labels(df$Lcs_crisis_ProdAssets)
        df$Lcs_crisis_Health <- haven::zap_labels(df$Lcs_crisis_Health)
        df$Lcs_crisis_OutSchool <- haven::zap_labels(df$Lcs_crisis_OutSchool)
        df$Lcs_crisis_Barter <- haven::zap_labels(df$Lcs_crisis_Barter)
        df$Lcs_crisis_Housing <- haven::zap_labels(df$Lcs_crisis_Housing)
        df$Lcs_crisis_HHSeparation <- haven::zap_labels(df$Lcs_crisis_HHSeparation)
        df$Lcs_crisis_Migration <- haven::zap_labels(df$Lcs_crisis_Migration)
        df$Lcs_crisis_DomMigration <- haven::zap_labels(df$Lcs_crisis_DomMigration)
        df$Lcs_crisis_ChildWork <- haven::zap_labels(df$Lcs_crisis_ChildWork)
        df$Lcs_crisis_Edu_Health <- haven::zap_labels(df$Lcs_crisis_Edu_Health)
        df$Lcs_crisis_Barter <- haven::zap_labels(df$Lcs_crisis_Barter)
        df$Lcs_crisis_ConsActive <- haven::zap_labels(df$Lcs_crisis_ConsActive)
        df$Lcs_crisis_Edu <- haven::zap_labels(df$Lcs_crisis_Edu)
        df$Lcs_crisis_Health <- haven::zap_labels(df$Lcs_crisis_Health)
        df$Lcs_crisis_Marriage <- haven::zap_labels(df$Lcs_crisis_Marriage)
        df$Lcs_crisis_Utilities <- haven::zap_labels(df$Lcs_crisis_Utilities)
        df$LcsR_crisis_AgriCare <- haven::zap_labels(df$LcsR_crisis_AgriCare)
        df$LcsR_crisis_ImmCrops <- haven::zap_labels(df$LcsR_crisis_ImmCrops)
        df$LcsR_crisis_Seed <- haven::zap_labels(df$LcsR_crisis_Seed)
        df$LcsR_crisis_Animals <- haven::zap_labels(df$LcsR_crisis_Animals)
        df$LcsR_crisis_Health <- haven::zap_labels(df$LcsR_crisis_Health)
        df$LcsR_crisis_Edu <- haven::zap_labels(df$LcsR_crisis_Edu)
        df$LcsR_crisis_ProdAssets <- haven::zap_labels(df$LcsR_crisis_ProdAssets)
        df$LcsR_crisis_Housing <- haven::zap_labels(df$LcsR_crisis_Housing)
        df$LcsR_crisis_HHSeparation <- haven::zap_labels(df$LcsR_crisis_HHSeparation)
        df$LcsR_crisis_Barter <- haven::zap_labels(df$LcsR_crisis_Barter)
        df$LcsR_crisis_Migration <- haven::zap_labels(df$LcsR_crisis_Migration)
        df$LcsR_crisis_ChildWork <- haven::zap_labels(df$LcsR_crisis_ChildWork)
        df$LcsR_crisis_Marriage <- haven::zap_labels(df$LcsR_crisis_Marriage)
        df$LcsR_crisis_ConsActive <- haven::zap_labels(df$LcsR_crisis_ConsActive)
        df$LcsR_crisis_OutSchool <- haven::zap_labels(df$LcsR_crisis_OutSchool)
        df$LcsR_crisis_DomMigration <- haven::zap_labels(df$LcsR_crisis_DomMigration)

        df$Lcs_em_ResAsset <- haven::zap_labels(df$Lcs_em_ResAsset)
        df$Lcs_em_Begged <- haven::zap_labels(df$Lcs_em_Begged)
        df$Lcs_em_IllegalAct <- haven::zap_labels(df$Lcs_em_IllegalAct)
        df$Lcs_em_ChildMigration <- haven::zap_labels(df$Lcs_em_ChildMigration)
        df$Lcs_em_Marriage <- haven::zap_labels(df$Lcs_em_Marriage)
        df$Lcs_em_Migration <- haven::zap_labels(df$Lcs_em_Migration)
        df$Lcs_em_ChildWork <- haven::zap_labels(df$Lcs_em_ChildWork)
        df$Lcs_em_OutSchool <- haven::zap_labels(df$Lcs_em_OutSchool)
        df$LcsR_em_FemAnimal <- haven::zap_labels(df$LcsR_em_FemAnimal)
        df$LcsR_em_WildFood <- haven::zap_labels(df$LcsR_em_WildFood)
        df$LcsR_em_Seed <- haven::zap_labels(df$LcsR_em_Seed)
        df$LcsR_em_OutSchool <- haven::zap_labels(df$LcsR_em_OutSchool)
        df$LcsR_em_Migration <- haven::zap_labels(df$LcsR_em_Migration)
        df$LcsR_em_ChildWork <- haven::zap_labels(df$LcsR_em_ChildWork)
        df$LcsR_em_Marriage <- haven::zap_labels(df$LcsR_em_Marriage)
        df$LcsR_em_ResAsset <- haven::zap_labels(df$LcsR_em_ResAsset)
        df$LcsR_em_Begged <- haven::zap_labels(df$LcsR_em_Begged)
        df$LcsR_em_IllegalAct <- haven::zap_labels(df$LcsR_em_IllegalAct)
        df$LcsR_em_ChildMigration <- haven::zap_labels(df$LcsR_em_ChildMigration)

        df$HDDSStapCer <- haven::zap_labels(df$HDDSStapCer)
        df$HDDSStapRoot <- haven::zap_labels(df$HDDSStapRoot)
        df$HDDSPulse <- haven::zap_labels(df$HDDSPulse)
        df$HDDSDairy <- haven::zap_labels(df$HDDSDairy)
        df$HDDSPrMeatF <- haven::zap_labels(df$HDDSPrMeatF)
        df$HDDSPrMeatO <- haven::zap_labels(df$HDDSPrMeatO)
        df$HDDSPrMeat <- haven::zap_labels(df$HDDSPrMeat)
        df$HDDSPrFish <- haven::zap_labels(df$HDDSPrFish)
        df$HDDSPrEggs <- haven::zap_labels(df$HDDSPrEggs)
        df$HDDSVeg <- haven::zap_labels(df$HDDSVeg)
        df$HDDSFruit <- haven::zap_labels(df$HDDSFruit)
        df$HDDSFat <- haven::zap_labels(df$HDDSFat)
        df$HDDSSugar <- haven::zap_labels(df$HDDSSugar)
        df$HDDSCond <- haven::zap_labels(df$HDDSCond)

        df$EnuSupervisorName <- haven::zap_labels(df$EnuSupervisorName)

        # Example freq table
        table(df$Lcs_stress_DomAsset, useNA = "ifany")

        # 2) Convert enumerator and supervisor name to character (exclude value labels)
        df <- df %>%
            set_value_labels(EnuName = NULL) %>%
            to_factor()
        df$EnuName <- as.character(df$EnuName)

        df <- df %>%
            set_value_labels(EnuSupervisorName = NULL) %>%
            to_factor()
        df$EnuSupervisorName <- as.character(df$EnuSupervisorName)

        # Convert ADMIN1Name & ADMIN2Name to text
        df <- df %>%
            set_value_labels(ADMIN2Name = NULL) %>%
            to_factor() %>%
            mutate(
                ADMIN2Name = as.character(ADMIN2Name),
                ADMIN1Name = as.character(ADMIN1Name)
            )

        # If dataset has "@_submission_time", split date & hour
        if ("@_submission_time" %in% names(df)) {
            df <- df %>%
                separate(`@_submission_time`, c("Survey_date", "survey_hour"), sep = " ")
            df$Survey_date <- as.Date(df$Survey_date)
        } else {
            df$Survey_date <- Sys.Date()
        }

        # Indicators calculation Steps

        # ------------------------------------------------------------------
        # FCS Calculation
        # ------------------------------------------------------------------
        df <- df %>%
            mutate(
                FCS = (2 * FCSStap) + (3 * FCSPulse) + (4 * FCSPr) + FCSVeg + FCSFruit +
                    (4 * FCSDairy) + (0.5 * FCSFat) + (0.5 * FCSSugar),
                FCSCat21 = case_when(
                    FCS <= 21 ~ "Poor",
                    between(FCS, 21.5, 35) ~ "Borderline",
                    FCS > 35 ~ "Acceptable"
                ),
                FCSCat28 = case_when(
                    FCS <= 28 ~ "Poor",
                    between(FCS, 28, 42) ~ "Borderline",
                    FCS > 42 ~ "Acceptable"
                )
            )

        # ------------------------------------------------------------------
        # HDDS
        # ------------------------------------------------------------------
        # 1) Create composite meat field if needed
        if (all(c("HDDSPrMeatF", "HDDSPrMeatO") %in% names(df))) {
            df <- df %>%
                mutate(
                    # zap and test both F and O
                    HDDSPrMeat = if_else(
                        haven::zap_labels(HDDSPrMeatF) == 1 |
                            haven::zap_labels(HDDSPrMeatO) == 1,
                        1, 0, missing = 0
                    )
                )
        } else if ("HDDSPrMeat" %in% names(df)) {
            df <- df %>%
                mutate(
                    HDDSPrMeat = if_else(
                        haven::zap_labels(HDDSPrMeat) == 1,
                        1, 0, missing = 0
                    )
                )
        }

        # 2) List all the “core” HDDS vars we might recode
        hdds_core <- c(
            "HDDSStapCer", "HDDSStapRoot", "HDDSVeg", "HDDSFruit",
            "HDDSPrEggs", "HDDSPrFish", "HDDSPulse",
            "HDDSDairy", "HDDSFat", "HDDSSugar", "HDDSCond"
        )

        # 3) Restrict to those actually present
        present_core <- intersect(names(df), hdds_core)

        # 4) Zap labels + binarize each present core var
        df <- df %>%
            mutate(
                across(
                    all_of(present_core),
                    ~ if_else(haven::zap_labels(.x) == 1, 1, 0, missing = 0)
                )
            )

        # 5) Now sum everything that’s present (including HDDSPrMeat if created)
        to_sum <- intersect(
            names(df),
            c(present_core, "HDDSPrMeat")
        )

        df <- df %>%
            mutate(
                HDDS = rowSums(across(all_of(to_sum)), na.rm = TRUE),
                HDDS_CH = case_when(
                    HDDS >= 5 ~ "Phase 1 (>=5 Food Groups)",
                    HDDS == 4 ~ "Phase 2 (=4 Food Groups)",
                    HDDS == 3 ~ "Phase 3 (=3 Food Groups)",
                    HDDS == 2 ~ "Phase 4 (=2 Food Groups)",
                    HDDS < 2 ~ "Phase 5 (<=1 Food Groups)"
                )
            )

        # ------------------------------------------------------------------
        # rCSI
        # ------------------------------------------------------------------
        df <- df %>%
            mutate(
                rCSI = rCSILessQlty + (2 * rCSIBorrow) + rCSIMealSize +
                    (3 * rCSIMealAdult) + rCSIMealNb,
                rCSI_CH = case_when(
                    rCSI <= 3 ~ "Phase 1 (0-3)",
                    between(rCSI, 4, 18) ~ "Phase 2 (4-18)",
                    rCSI >= 19 ~ "Phase 3 (>=19)"
                )
            )

        # ------------------------------------------------------------------
        # HHS
        # ------------------------------------------------------------------
        df <- df %>%
            set_value_labels(
                HHSNoFood_FR = NULL, HHSBedHung_FR = NULL, HHSNotEat_FR = NULL
            ) %>%
            mutate(
                HHSNoFood_FR  = as.numeric(HHSNoFood_FR),
                HHSBedHung_FR = as.numeric(HHSBedHung_FR),
                HHSNotEat_FR  = as.numeric(HHSNotEat_FR)
            ) %>%
            mutate(
                HHhSNoFood_FR_r = case_when(
                    HHSNoFood_FR == 1 ~ 1,
                    HHSNoFood_FR == 2 ~ 1,
                    HHSNoFood_FR == 3 ~ 2,
                    TRUE ~ 0
                ),
                HHhSBedHung_FR_r = case_when(
                    HHSBedHung_FR == 1 ~ 1,
                    HHSBedHung_FR == 2 ~ 1,
                    HHSBedHung_FR == 3 ~ 2,
                    TRUE ~ 0
                ),
                HHhSNotEat_FR_r = case_when(
                    HHSNotEat_FR == 1 ~ 1,
                    HHSNotEat_FR == 2 ~ 1,
                    HHSNotEat_FR == 3 ~ 2,
                    TRUE ~ 0
                )
            ) %>%
            mutate(
                HHS = HHhSNoFood_FR_r + HHhSBedHung_FR_r + HHhSNotEat_FR_r,
                HHS_CH = case_when(
                    HHS == 0 ~ "Phase 1 (=0)",
                    HHS == 1 ~ "Phase 2 (=1)",
                    HHS %in% c(2, 3) ~ "Phase 3 (2-3)",
                    HHS == 4 ~ "Phase 4 (=4)",
                    HHS >= 5 ~ "Phase 5 (5-6)"
                )
            )

        # ------------------------------------------------------------------
        # FES
        # ------------------------------------------------------------------
        # A) Utility function for 7-day => monthly
        convert_to_monthly <- function(df2, columns, recall_period) {
            if (recall_period == 7) {
                df2 <- df2 %>% mutate(across(all_of(columns), ~ . * (30 / 7)))
            }
            df2
        }

        # B) We'll pick the user-chosen recall from the sub-tab or default to 30
        recall_choice <- input$fesRecall
        if (is.null(recall_choice)) {
            recall_choice <- 30
        }

        # If user picks 7 => do 7-day logic, else 1-month
        if (recall_choice == 7) {
            df <- df %>%
                mutate(
                    HHExp_Food_Purch_MN_1M   = rowSums(select(., starts_with("HHExpF") & ends_with("_Purch_MN_7D")), na.rm = TRUE),
                    HHExp_Food_GiftAid_MN_1M = rowSums(select(., starts_with("HHExpF") & ends_with("_GiftAid_MN_7D")), na.rm = TRUE),
                    HHExp_Food_Own_MN_1M     = rowSums(select(., starts_with("HHExpF") & ends_with("_Own_MN_7D")), na.rm = TRUE)
                )
            df <- convert_to_monthly(
                df,
                c("HHExp_Food_Purch_MN_1M", "HHExp_Food_GiftAid_MN_1M", "HHExp_Food_Own_MN_1M"),
                7
            )
        } else {
            # 1-month logic
            df <- df %>%
                mutate(
                    HHExp_Food_Purch_MN_1M   = rowSums(select(., starts_with("HHExpF") & ends_with("_Purch_MN_1M")), na.rm = TRUE),
                    HHExp_Food_GiftAid_MN_1M = rowSums(select(., starts_with("HHExpF") & ends_with("_GiftAid_MN_1M")), na.rm = TRUE),
                    HHExp_Food_Own_MN_1M     = rowSums(select(., starts_with("HHExpF") & ends_with("_Own_MN_1M")), na.rm = TRUE)
                )
        }

        # 3) Overall monthly food expenditure
        df <- df %>%
            mutate(
                HHExp_Food_MN_1M = rowSums(select(., starts_with("HHExpF") & ends_with("_MN_1M")), na.rm = TRUE)
            )

        # 4) total non-food (1-month)
        df <- df %>%
            mutate(
                HHExpNFTotal_Purch_MN_30D   = rowSums(select(., starts_with("HHExpNF") & ends_with("_Purch_MN_1M")), na.rm = TRUE),
                HHExpNFTotal_GiftAid_MN_30D = rowSums(select(., starts_with("HHExpNF") & ends_with("_GiftAid_MN_1M")), na.rm = TRUE)
            )

        # 5) Convert 6-month recall => monthly
        df <- df %>%
            mutate(
                HHExpNFTotal_Purch_MN_6M   = rowSums(select(., starts_with("HHExpNF") & ends_with("_Purch_MN_6M")), na.rm = TRUE) / 6,
                HHExpNFTotal_GiftAid_MN_6M = rowSums(select(., starts_with("HHExpNF") & ends_with("_GiftAid_Mn_6M")), na.rm = TRUE) / 6
            )

        # 6) Sum up total non-food monthly
        df <- df %>%
            mutate(
                HHExpNFTotal_Purch_MN_1M   = HHExpNFTotal_Purch_MN_30D + HHExpNFTotal_Purch_MN_6M,
                HHExpNFTotal_GiftAid_MN_1M = HHExpNFTotal_GiftAid_MN_30D + HHExpNFTotal_GiftAid_MN_6M
            )

        # 7) total food & non-food
        df <- df %>%
            mutate(
                HHExpF_1M  = HHExp_Food_Purch_MN_1M + HHExp_Food_GiftAid_MN_1M + HHExp_Food_Own_MN_1M,
                HHExpNF_1M = HHExpNFTotal_Purch_MN_1M + HHExpNFTotal_GiftAid_MN_1M
            )

        # 8) FES (food expenditure share)
        df <- df %>%
            mutate(
                FES = HHExpF_1M / (HHExpF_1M + HHExpNF_1M)
            )

        # 9) FES 4-pt categories
        df <- df %>%
            mutate(
                Foodexp_4pt = case_when(
                    FES < 0.50 ~ 1,
                    FES >= 0.50 & FES < 0.65 ~ 2,
                    FES >= 0.65 & FES < 0.75 ~ 3,
                    FES >= 0.75 ~ 4,
                    TRUE ~ NA_real_
                )
            )
        df$Foodexp_4pt <- factor(df$Foodexp_4pt,
            levels = c(1, 2, 3, 4),
            labels = c("<50%", "50-65%", "65-75%", ">75%")
        )

        # Return final processed data
        df
    })
}