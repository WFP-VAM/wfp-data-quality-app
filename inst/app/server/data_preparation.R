data_preparation <- function(input, spss_data, moda_data){
  reactive({
    # 1) Pull in the raw data (from SPSS upload or MoDa)
    df <- switch(input$data_source,
      spss = spss_data(),
      moda = moda_data()
    )

    # --- 1) List every variable your app expects for FCS, HDDS, rCSI, HHS, etc. ---
    calc_vars <- c(
      # FCS components
      "FCSStap", "FCSPulse", "FCSPr", "FCSVeg", "FCSFruit",
      "FCSDairy", "FCSFat", "FCSSugar", "FCS",
      # HDDS components
      "HDDSStapCer", "HDDSStapRoot", "HDDSVeg", "HDDSFruit",
      "HDDSPrEggs", "HDDSPrFish", "HDDSPulse", "HDDSDairy",
      "HDDSFat", "HDDSSugar", "HDDSCond", "HDDS",
      # rCSI components
      "rCSILessQlty", "rCSIBorrow", "rCSIMealSize",
      "rCSIMealAdult", "rCSIMealNb", "rCSI",
      # HHS components
      "HHSNoFood_FR", "HHSBedHung_FR", "HHSNotEat_FR",
      "HHS",
      # Food expenditure components
      "HHExpFCer_Purch_MN_7D", "HHExpFCer_GiftAid_MN_7D", "HHExpFCer_Own_MN_7D",
      "HHExpFTub_Purch_MN_7D", "HHExpFTub_GiftAid_MN_7D", "HHExpFTub_Own_MN_7D",
      "HHExpFPuls_Purch_MN_7D", "HHExpFPuls_GiftAid_MN_7D", "HHExpFPuls_Own_MN_7D",
      "HHExpFVeg_Purch_MN_7D", "HHExpFVeg_GiftAid_MN_7D", "HHExpFVeg_Own_MN_7D",
      "HHExpFFrt_Purch_MN_7D", "HHExpFFrt_GiftAid_MN_7D", "HHExpFFrt_Own_MN_7D",
      "HHExpFAnimMeat_Purch_MN_7D", "HHExpFAnimMeat_GiftAid_MN_7D", "HHExpFAnimMeat_Own_MN_7D",
      "HHExpFAnimFish_Purch_MN_7D", "HHExpFAnimFish_GiftAid_MN_7D", "HHExpFAnimFish_Own_MN_7D",
      "HHExpFFats_Purch_MN_7D", "HHExpFFats_GiftAid_MN_7D", "HHExpFFats_Own_MN_7D",
      "HHExpFDairy_Purch_MN_7D", "HHExpFDairy_GiftAid_MN_7D", "HHExpFDairy_Own_MN_7D",
      "HHExpFEgg_Purch_MN_7D", "HHExpFEgg_GiftAid_MN_7D", "HHExpFEgg_Own_MN_7D",
      "HHExpFSgr_Purch_MN_7D", "HHExpFSgr_GiftAid_MN_7D", "HHExpFSgr_Own_MN_7D",
      "HHExpFCond_Purch_MN_7D", "HHExpFCond_GiftAid_MN_7D", "HHExpFCond_Own_MN_7D",
      "HHExpFBev_Purch_MN_7D", "HHExpFBev_GiftAid_MN_7D", "HHExpFBev_Own_MN_7D",
      "HHExpFOut_Purch_MN_7D", "HHExpFOut_GiftAid_MN_7D", "HHExpFOut_Own_MN_7D",
      "HHExpFCer_Purch_MN_1M", "HHExpFCer_GiftAid_MN_1M", "HHExpFCer_Own_MN_1M",
      "HHExpFTub_Purch_MN_1M", "HHExpFTub_GiftAid_MN_1M", "HHExpFTub_Own_MN_1M",
      "HHExpFPuls_Purch_MN_1M", "HHExpFPuls_GiftAid_MN_1M", "HHExpFPuls_Own_MN_1M",
      "HHExpFVeg_Purch_MN_1M", "HHExpFVeg_GiftAid_MN_1M", "HHExpFVeg_Own_MN_1M",
      "HHExpFFrt_Purch_MN_1M", "HHExpFFrt_GiftAid_MN_1M", "HHExpFFrt_Own_MN_1M",
      "HHExpFAnimMeat_Purch_MN_1M", "HHExpFAnimMeat_GiftAid_MN_1M", "HHExpFAnimMeat_Own_MN_1M",
      "HHExpFAnimFish_Purch_MN_1M", "HHExpFAnimFish_GiftAid_MN_1M", "HHExpFAnimFish_Own_MN_1M",
      "HHExpFFats_Purch_MN_1M", "HHExpFFats_GiftAid_MN_1M", "HHExpFFats_Own_MN_1M",
      "HHExpFDairy_Purch_MN_1M", "HHExpFDairy_GiftAid_MN_1M", "HHExpFDairy_Own_MN_1M",
      "HHExpFEgg_Purch_MN_1M", "HHExpFEgg_GiftAid_MN_1M", "HHExpFEgg_Own_MN_1M",
      "HHExpFSgr_Purch_MN_1M", "HHExpFSgr_GiftAid_MN_1M", "HHExpFSgr_Own_MN_1M",
      "HHExpFCond_Purch_MN_1M", "HHExpFCond_GiftAid_MN_1M", "HHExpFCond_Own_MN_1M",
      "HHExpFBev_Purch_MN_1M", "HHExpFBev_GiftAid_MN_1M", "HHExpFBev_Own_MN_1M",
      "HHExpFOut_Purch_MN_1M", "HHExpFOut_GiftAid_MN_1M", "HHExpFOut_Own_MN_1M"
    )

    # --- 2) For any of these that already exist, zap labels & coerce numeric;
    #        for any that are missing, fill in a column of zeros. ---
    for (v in calc_vars) {
      if (v %in% names(df)) {
        # drop SPSS labels and make sure it’s numeric
        df[[v]] <- as.numeric(haven::zap_labels(df[[v]]))
      } else {
        # create a zero‐column so sums/etc. don’t break
        df[[v]] <- 0
      }
    }


    req(df) # ensure we have a data.frame

    # 2) If RESPConsent is present, zap its labels and keep only consenters
    if ("RESPConsent" %in% names(df)) {
      # turn any labelled vector into its numeric codes
      df$RESPConsent <- haven::zap_labels(df$RESPConsent)

      # keep only rows where RESPConsent == 1
      df <- df[df$RESPConsent == 1, , drop = FALSE]
    }

    # 3) Return (filtered or not) for downstream processing
    df
  })
}