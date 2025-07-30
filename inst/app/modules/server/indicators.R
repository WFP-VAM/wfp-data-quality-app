get_lcs_variables <- function(input, df){
    reactive({
    
    req(input$stressVars, input$crisisVars, input$emergencyVars)
  
    # 1) Gather user picks
    chosen_stress <- input$stressVars
    chosen_crisis <- input$crisisVars
    chosen_emerg <- input$emergencyVars
  
    # 2) Validate EXACT counts
    if (length(chosen_stress) != 4) {
        shiny::validate(
            shiny::need(
                FALSE,
                paste("Must pick EXACTLY 4 stress strategies. Currently:", length(chosen_stress))
            )
        )
    }
    if (length(chosen_crisis) != 3) {
        shiny::validate(
            shiny::need(
                FALSE,
                paste("Must pick EXACTLY 3 crisis strategies. Currently:", length(chosen_crisis))
            )
        )
    }
    if (length(chosen_emerg) != 3) {
        shiny::validate(
            shiny::need(
                FALSE,
                paste("Must pick EXACTLY 3 emergency strategies. Currently:", length(chosen_emerg))
            )
        )
    }
  
    # 3) Check all exist in df
    missingStress <- setdiff(chosen_stress, names(df))
    missingCrisis <- setdiff(chosen_crisis, names(df))
    missingEmerg <- setdiff(chosen_emerg, names(df))
    allMissing <- c(missingStress, missingCrisis, missingEmerg)
    if (length(allMissing) > 0) {
        shiny::validate(
            shiny::need(
                FALSE,
                paste("The following selected LCS variables are not in your dataset:", paste(allMissing, collapse = ", "))
            )
        )
    }

    calculate_lcs(df, chosen_stress, chosen_crisis, chosen_emerg)

  })}


calculate_lcs <- function(df, chosen_stress, chosen_crisis, chosen_emerg){
    # 4) If picks are valid, compute dynamic coping
    df <- df %>%
        mutate(
            stress_coping = if_else(
                rowSums(across(all_of(chosen_stress), ~ . %in% c(20, 30))) > 0,
                1,
                0
            ),
            crisis_coping = if_else(
                rowSums(across(all_of(chosen_crisis), ~ . %in% c(20, 30))) > 0,
                1,
                0
            ),
            emergency_coping = if_else(
                rowSums(across(all_of(chosen_emerg), ~ . %in% c(20, 30))) > 0,
                1,
                0
            ),
            LhCSICat = case_when(
                emergency_coping == 1 ~ 4,
                crisis_coping == 1 ~ 3,
                stress_coping == 1 ~ 2,
                TRUE ~ 1
            )
        ) %>%
        mutate(
            LhCSICat = factor(
                LhCSICat,
                levels = c(1, 2, 3, 4),
                labels = c(
                    "NoStrategies",
                    "StressStrategies",
                    "CrisisStrategies",
                    "EmergencyStrategies"
                )
            )
        )

    df
}


calculate_cari <- function(input, dynamicLCS){
    reactive({
        cat("Entered dynamicCARI()...\n")
        df_lcs <- dynamicLCS() # This reactive should return df with LhCSICat

        req(input$cariFcsThreshold) # Expecting "FCSCat21" or "FCSCat28"
        fcsVar <- input$cariFcsThreshold
        cat("User picked FCS threshold =>", fcsVar, "\n")

        # Recode FCS_4pt based on the chosen threshold
        df_lcs <- df_lcs %>%
            mutate(
                FCS_4pt = case_when(
                    .data[[fcsVar]] == "Poor" ~ 4,
                    .data[[fcsVar]] == "Borderline" ~ 3,
                    .data[[fcsVar]] == "Acceptable" ~ 1,
                    TRUE ~ NA_real_
                )
            ) %>%
            # Mandatory adjustment: if rCSI >= 4 and FCS_4pt equals 1, then set FCS_4pt to 2
            mutate(
                FCS_4pt = case_when(
                    rCSI >= 4 & FCS_4pt == 1 ~ 2,
                    TRUE ~ FCS_4pt
                )
            )

        df_lcs <- df_lcs %>%
            mutate(
                FCS_4pt = factor(
                    FCS_4pt,
                    levels = c(1, 2, 3, 4),
                    labels = c(
                        "Acceptable",
                        "Acceptable and rCSI>4",
                        "Borderline",
                        "Poor"
                    )
                ),
                FCS_4pt = as.numeric(FCS_4pt)
            )

        cat("Summary of FCS_4pt:\n")
        print(table(df_lcs$FCS_4pt, useNA = "ifany"))

        # Convert dynamic LCS factor to a numeric variable (LCS_4pt)
        df_lcs <- df_lcs %>%
            mutate(
                LCS_4pt = case_when(
                    LhCSICat == "NoStrategies" ~ 1,
                    LhCSICat == "StressStrategies" ~ 2,
                    LhCSICat == "CrisisStrategies" ~ 3,
                    LhCSICat == "EmergencyStrategies" ~ 4,
                    TRUE ~ NA_real_
                )
            )

        # Convert Foodexp_4pt (already calculated in FES section) from factor to numeric.
        # Adjust the mapping as needed.
        df_lcs <- df_lcs %>%
            mutate(
                Foodexp_4pt_num = case_when(
                    Foodexp_4pt == "<50%" ~ 1,
                    Foodexp_4pt == "50-65%" ~ 2,
                    Foodexp_4pt == "65-75%" ~ 3,
                    Foodexp_4pt == ">75%" ~ 4,
                    TRUE ~ NA_real_
                )
            )

        # Compute row means to derive the coping capacity and final CARI_FES:
        df_lcs <- df_lcs %>%
            mutate(
                Mean_coping_capacity_FES = rowMeans(
                    cbind(as.numeric(LCS_4pt), as.numeric(Foodexp_4pt_num)),
                    na.rm = TRUE
                ),
                CARI_unrounded_FES = rowMeans(
                    cbind(Mean_coping_capacity_FES, as.numeric(FCS_4pt)),
                    na.rm = TRUE
                ),
                CARI_FES = round_half_up(CARI_unrounded_FES)
            )

        # Label final CARI_FES
        df_lcs$CARI_FES <- factor(
            df_lcs$CARI_FES,
            levels = 1:4,
            labels = c(
                "Food secure",
                "Marginally food secure",
                "Moderately food insecure",
                "Severely food insecure"
            )
        )

        cat("LCS_4pt summary:\n")
        print(summary(df_lcs$LCS_4pt))
        cat("Foodexp_4pt_num summary:\n")
        print(summary(df_lcs$Foodexp_4pt_num))
        cat("FCS_4pt summary:\n")
        print(summary(df_lcs$FCS_4pt))
        cat("Mean coping capacity:\n")
        print(summary(df_lcs$Mean_coping_capacity_FES))
        cat("Unrounded CARI:\n")
        print(summary(df_lcs$CARI_unrounded_FES))
        cat("Dynamic CARI done. Summary of CARI_FES:\n")
        print(table(df_lcs$CARI_FES, useNA = "ifany"))

        df_lcs
    })
}