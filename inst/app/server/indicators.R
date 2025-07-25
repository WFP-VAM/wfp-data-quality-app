calculate_lcs <- function(input, df){
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

    # 4) If picks are valid, compute dynamic coping
    df <- df %>%
        mutate(
            stress_coping = if_else(
                rowSums(across(all_of(chosen_stress), ~ . %in% c(20, 30))) > 0, 1, 0
            ),
            crisis_coping = if_else(
                rowSums(across(all_of(chosen_crisis), ~ . %in% c(20, 30))) > 0, 1, 0
            ),
            emergency_coping = if_else(
                rowSums(across(all_of(chosen_emerg), ~ . %in% c(20, 30))) > 0, 1, 0
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
                labels = c("NoStrategies", "StressStrategies", "CrisisStrategies", "EmergencyStrategies")
            )
        )

    df
})

}