
spss_import <- function(input, output, session) {
  spss_data <- reactive({
    req(input$data_source == "spss", input$file)
    ext <- tools::file_ext(input$file$name)
    shiny::validate(
      shiny::need(ext == "sav", "Please upload an SPSS .sav file")
    )
    haven::read_sav(input$file$datapath)
  })

  output$fileUploadedMessage <- renderPrint({
    req(spss_data())
    str(spss_data())
  })

  return(list(spss_data = spss_data))
}

moda_import <- function(input, output, session) {
  moda_error <- reactiveVal(NULL)

  moda_data <- eventReactive(input$moda_load, {
    req(input$data_source == "moda", input$moda_formid, input$moda_token)
    moda_error(NULL)

    tryCatch({
      init_url <- sprintf(
        "https://api.moda.wfp.org/api/v1/forms/%s/export_async?format=savzip",
        input$moda_formid
      )
      init_resp <- httr::GET(
        init_url,
        httr::add_headers(Authorization = paste("Token", input$moda_token))
      )
      httr::stop_for_status(init_resp, "initializing export")

      init_json <- httr::content(init_resp, as = "parsed", type = "application/json")

      if (!is.null(init_json$export_url) && isTRUE(init_json$job_status == "SUCCESS")) {
        export_url <- init_json$export_url
      } else if (!is.null(init_json$job_uuid) && isTRUE(init_json$job_status == "PENDING")) {
        job_uuid <- init_json$job_uuid

        repeat {
          poll_url <- sprintf(
            "https://api.moda.wfp.org/api/v1/forms/%s/export_async?job_uuid=%s",
            input$moda_formid,
            job_uuid
          )
          status_resp <- httr::GET(
            poll_url,
            httr::add_headers(Authorization = paste("Token", input$moda_token))
          )
          httr::stop_for_status(status_resp, "polling export job")
          status_json <- httr::content(status_resp, as = "parsed", type = "application/json")

          if (identical(status_json$job_status, "SUCCESS")) {
            export_url <- status_json$export_url
            break
          }
          if (identical(status_json$job_status, "FAILED")) {
            stop("Export failed: ", status_json$error_message)
          }
          Sys.sleep(2)
        }
      } else {
        stop("Unexpected init response: status = ", init_json$job_status)
      }

      tmpzip <- tempfile(fileext = ".zip")
      httr::GET(
        export_url,
        httr::add_headers(Authorization = paste("Token", input$moda_token)),
        httr::write_disk(tmpzip, overwrite = TRUE)
      ) %>%
        httr::stop_for_status("downloading ZIP")

      zip_list <- utils::unzip(tmpzip, list = TRUE)$Name
      if (!"data.sav" %in% zip_list) {
        stop("`data.sav` not found in ZIP archive")
      }

      tmpdir <- tempfile()
      utils::unzip(tmpzip, files = "data.sav", exdir = tmpdir)
      sav_path <- file.path(tmpdir, "data.sav")

      haven::read_sav(sav_path)
    },
    error = function(e) {
      moda_error(e$message)
      showNotification(
        paste("Error loading data from MoDa:", e$message),
        type = "error",
        duration = 10
      )
      NULL
    })
  })

  output$moda_status <- renderText({
    req(input$moda_load)
    if (!is.null(moda_error())) {
      paste("❌", moda_error())
    } else if (is.null(moda_data()) || nrow(moda_data()) == 0) {
      "⚠️ No rows returned from MoDa."
    } else {
      paste("✅ Loaded", nrow(moda_data()), "rows from MoDa.")
    }
  })

  return(list(moda_data = moda_data, moda_error = moda_error))
}

