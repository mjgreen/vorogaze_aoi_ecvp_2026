# Export UI and Shiny wiring ----

vorogaze_summary_enabled <- function() {
  vorogaze_env_flag("VOROGAZE_ENABLE_SUMMARY", default = TRUE)
}

vorogaze_release_revision <- function() {
  revision <- trimws(Sys.getenv("VOROGAZE_RELEASE_REVISION", unset = ""))
  if (nzchar(revision)) revision else "development"
}

aoi_exports_badge <- function(label, style = c("recommended", "beta")) {
  style <- match.arg(style)
  colour <- if (identical(style, "recommended")) "text-bg-success" else "text-bg-warning"
  shiny::span(class = paste("badge", colour), label)
}

aoi_export_annotation_ready <- function(item) {
  is.list(item) &&
    is.data.frame(item$data) &&
    nrow(item$data) > 0L
}

aoi_export_download_control <- function(item) {
  if (!aoi_export_annotation_ready(item)) return(NULL)

  shiny::downloadButton(
    "download_aoi_annotated_fixations",
    "Download annotated fixation report",
    class = "btn-success"
  )
}

aoi_exports_panel <- function() {
  summary_panel <- if (vorogaze_summary_enabled()) {
    bslib::nav_panel(
      "Participant summary",
      bslib::layout_columns(
        col_widths = c(4, 8),
        bslib::card(
          bslib::card_header(
            shiny::span("Researcher-configured participant summary "),
            aoi_exports_badge("Beta", "beta")
          ),
          bslib::card_body(
            shiny::p(
              "There is no universally correct participant-level aggregation. ",
              "Your selections determine how this summary is calculated."
            ),
            shiny::p(
              class = "text-muted",
              "This narrow tool supports fixation-level reports with identifiable ",
              "participants, trials, AOIs and fixation durations. It does not ",
              "determine the correct analysis."
            ),
            shiny::uiOutput("aoi_summary_config_ui")
          )
        ),
        bslib::navset_card_tab(
          full_screen = TRUE,
          title = "Reviewed output",
          bslib::nav_panel(
            "Specification",
            shiny::uiOutput("aoi_summary_review_status"),
            shiny::div(
              class = "dt-table-output",
              DT::DTOutput("aoi_summary_specification")
            )
          ),
          bslib::nav_panel(
            "Participant summary",
            shiny::div(
              class = "dt-table-output",
              DT::DTOutput("aoi_summary_preview")
            )
          )
        )
      )
    )
  } else {
    bslib::nav_panel(
      "Participant summary",
      bslib::card(
        bslib::card_header("Researcher-configured participant summary"),
        bslib::card_body("The Beta summary generator is disabled for this deployment.")
      )
    )
  }

  bslib::nav_panel(
    "Exports",
    bslib::navset_card_tab(
      full_screen = TRUE,
      bslib::nav_panel(
        "Annotated fixations",
        bslib::layout_columns(
          col_widths = c(4, 8),
          bslib::card(
            bslib::card_header(
              shiny::span("Annotated fixation report "),
              aoi_exports_badge("Recommended", "recommended")
            ),
            bslib::card_body(
              shiny::p(
                "Preserves every supplied row and variable, then appends AOI ",
                "assignment fields without aggregation."
              ),
              shiny::uiOutput("aoi_export_prepared_fixture_ui"),
              shiny::uiOutput("aoi_export_status_ui"),
              shiny::uiOutput("aoi_export_download_ui")
            )
          ),
          bslib::navset_card_tab(
            full_screen = TRUE,
            title = "Annotated output",
            bslib::nav_panel(
              "Preview",
              shiny::div(
                class = "dt-table-output",
                DT::DTOutput("aoi_export_preview")
              )
            ),
            bslib::nav_panel(
              "Assignment coverage",
              shiny::div(
                class = "dt-table-output",
                DT::DTOutput("aoi_export_coverage")
              )
            ),
            bslib::nav_panel(
              "Constructed names",
              shiny::div(
                class = "dt-table-output",
                DT::DTOutput("aoi_export_name_mapping")
              )
            )
          )
        )
      ),
      summary_panel
    )
  )
}

aoi_summary_specification <- function(
    config,
    annotation,
    summary,
    source_filename,
    source_sha256,
    generated_at,
    all_scope_keys
) {
  rows <- list()
  add <- function(section, setting, value, explanation = "") {
    rows[[length(rows) + 1L]] <<- data.frame(
      section = section,
      setting = setting,
      selected_rule = paste(value, collapse = ", "),
      explanation = explanation,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  add("provenance", "annotated report", source_filename)
  add("provenance", "annotated report SHA-256", source_sha256)
  add("provenance", "annotated report rows", nrow(annotation$data))
  add("provenance", "VoroGaze revision", vorogaze_release_revision())
  add("provenance", "generated UTC", generated_at)
  add("design", "participant variable", config$participant_col)
  add("design", "trial key", config$trial_key_cols)
  add(
    "design",
    "retained grouping variables",
    if (length(config$group_cols) == 0L) "(none)" else config$group_cols
  )
  add("design", "recipe", config$recipe)
  add("design", "measures", config$measures)
  add(
    "design",
    "empty-AOI denominator",
    if (identical(config$recipe, "equal_trial")) {
      config$empty_aoi_denominator
    } else {
      "not applicable to pooled-fixation recipe"
    }
  )
  add("design", "output layout", config$layout)
  add(
    "rules",
    "eligible fixation rows",
    paste0(config$status_col, " == assigned"),
    "Outside-image, invalid, missing-coordinate and uncommitted rows remain in the annotated report but do not contribute to AOI measures."
  )
  add(
    "rules",
    "summed fixation duration",
    "sum of valid fixation durations",
    "This output is not labelled dwell time."
  )
  add(
    "rules",
    "zero and missing values",
    "zero count/sum for included eligible no-hit trials; mean duration missing",
    "An AOI absent from a face definition is unavailable, not zero."
  )
  add("output", "participant-summary rows", nrow(summary))
  add("output", "participant-summary columns", ncol(summary))

  for (scope in config$scope_keys) {
    add("included scope", scope, "included")
  }
  for (scope in setdiff(all_scope_keys, config$scope_keys)) {
    add("excluded scope", scope, "excluded")
  }
  for (i in seq_len(nrow(annotation$name_mapping))) {
    item <- annotation$name_mapping[i, ]
    add(
      "annotation name",
      item$constructed_name,
      item$export_name,
      if (isTRUE(item$collision_renamed)) {
        "Renamed to preserve a supplied variable of the same name."
      } else {
        "No supplied-variable collision."
      }
    )
  }
  for (i in seq_len(nrow(config$summary_name_mapping))) {
    item <- config$summary_name_mapping[i, ]
    add(
      "summary name",
      item$constructed_name,
      item$export_name,
      if (isTRUE(item$collision_renamed)) {
        "Renamed to preserve an existing output variable of the same name."
      } else {
        "No output-name collision."
      }
    )
  }

  wide_mapping <- attr(summary, "wide_name_mapping", exact = TRUE)
  if (!is.null(wide_mapping) && nrow(wide_mapping) > 0L) {
    for (i in seq_len(nrow(wide_mapping))) {
      item <- wide_mapping[i, ]
      add(
        "wide name",
        paste(item$value_name, item$which_aoi, sep = " \u00d7 "),
        item$export_name,
        paste0("Constructed from ", item$constructed_name, ".")
      )
    }
  }

  do.call(rbind, rows)
}

aoi_exports_server <- function(
    input,
    output,
    session,
    raw_fixrep,
    standardised_fixrep,
    fixrep_map,
    face_files,
    screen_params,
    active_fixrep_path,
    aoi_state
) {
  annotation <- shiny::reactive({
    aoi_export_annotate(
      raw = raw_fixrep(),
      standardised = standardised_fixrep(),
      assignments = aoi_state$session_assignments(),
      defs = aoi_state$session_defs(),
      face_files = face_files(),
      screen = screen_params(),
      image_origin = input$image_origin %||% "center",
      use_screen_center = isTRUE(input$aoi_workbench_use_screen_center)
    )
  })

  annotation_result <- shiny::reactive({
    tryCatch(
      list(item = annotation(), error = NULL),
      error = function(error) {
        message("Annotated export unavailable: ", conditionMessage(error))
        list(item = NULL, error = conditionMessage(error))
      }
    )
  })

  annotation_value <- shiny::reactive({
    result <- annotation_result()
    shiny::req(is.null(result$error))
    shiny::req(aoi_export_annotation_ready(result$item))
    result$item
  })

  prepared_fixture_dir <- shiny::reactive({
    aoi_export_fixture_dir(active_fixrep_path())
  })

  prepared_fixture_ready <- shiny::reactive({
    if (is.null(prepared_fixture_dir())) return(FALSE)

    dat <- standardised_fixrep()
    expected <- unique(aoi_summary_scope_key(dat$FACE, dat$CONDITION))
    completed <- unique(aoi_state$session_defs()$commit_key)
    length(expected) > 0L && all(expected %in% completed)
  })

  output$aoi_export_prepared_fixture_ui <- shiny::renderUI({
    fixture_dir <- prepared_fixture_dir()
    if (is.null(fixture_dir)) return(NULL)

    if (prepared_fixture_ready()) {
      return(shiny::div(
        class = "alert alert-success py-2",
        "Prepared demonstration AOIs are loaded for this browser session."
      ))
    }

    shiny::tagList(
      shiny::div(
        class = "alert alert-info py-2",
        "This synthetic fixture includes prepared Face Research Lab AOI centres."
      ),
      shiny::actionButton(
        "aoi_export_load_prepared",
        if (nrow(aoi_state$session_defs()) > 0L) {
          "Replace current session with prepared demonstration AOIs"
        } else {
          "Load prepared demonstration AOIs"
        },
        class = "btn-primary"
      )
    )
  })

  shiny::observeEvent(input$aoi_export_load_prepared, {
    fixture_dir <- prepared_fixture_dir()
    shiny::req(fixture_dir)

    result <- shiny::withProgress(
      message = "Calculating prepared AOI assignments",
      value = 0.2,
      {
        loaded <- aoi_export_load_prepared_fixture(
          fixture_dir = fixture_dir,
          fixrep = standardised_fixrep(),
          face_files = face_files(),
          screen = screen_params(),
          image_origin = input$image_origin %||% "center",
          aoi_state = aoi_state
        )
        shiny::incProgress(0.8)
        loaded
      }
    )

    shiny::showNotification(
      sprintf(
        "Loaded %d completed scopes and assigned %d fixation rows.",
        result$scopes,
        result$assigned_rows
      ),
      type = "message",
      duration = 4
    )
  })

  output$aoi_export_status_ui <- shiny::renderUI({
    result <- annotation_result()
    if (!is.null(result$error)) {
      return(shiny::div(
        class = "alert alert-danger py-2",
        "The annotated report is unavailable. Check the fixation report, face ",
        "images, screen settings and committed AOIs."
      ))
    }
    item <- result$item
    assigned <- sum(
      item$status_counts$fixation_rows[
        item$status_counts$assignment_status == "assigned"
      ]
    )
    total <- nrow(item$data)
    completed_scopes <- length(unique(aoi_state$session_defs()$commit_key))
    all_scopes <- unique(data.frame(
      FACE = standardised_fixrep()$FACE,
      CONDITION = standardised_fixrep()$CONDITION,
      stringsAsFactors = FALSE
    ))

    shiny::div(
      class = "loaded-file-box",
      shiny::tags$ul(
        shiny::tags$li(sprintf("Source fixation rows preserved: %d", total)),
        shiny::tags$li(sprintf("Rows assigned to an AOI: %d", assigned)),
        shiny::tags$li(sprintf(
          "Completed face/condition scopes: %d of %d",
          completed_scopes,
          nrow(all_scopes)
        ))
      )
    )
  })

  output$aoi_export_download_ui <- shiny::renderUI({
    aoi_export_download_control(annotation_result()$item)
  })

  output$aoi_export_preview <- DT::renderDT({
    aoi_workbench_dt(annotation_value()$data, page_length = 10)
  })
  output$aoi_export_coverage <- DT::renderDT({
    aoi_workbench_dt(annotation_value()$status_counts, page_length = 10)
  })
  output$aoi_export_name_mapping <- DT::renderDT({
    aoi_workbench_dt(annotation_value()$name_mapping, page_length = 10)
  })

  annotated_filename <- function() {
    paste0("vorogaze_annotated_fixations_", Sys.Date(), ".csv")
  }

  output$download_aoi_annotated_fixations <- shiny::downloadHandler(
    filename = annotated_filename,
    content = function(file) {
      item <- annotation_value()
      shiny::req(aoi_export_annotation_ready(item))
      readr::write_csv(item$data, file, na = "")
    }
  )

  if (!vorogaze_summary_enabled()) {
    return(invisible(NULL))
  }

  completed_scope_table <- shiny::reactive({
    defs <- aoi_state$session_defs()
    if (nrow(defs) == 0L) {
      return(data.frame(
        scope_key = character(),
        label = character(),
        stringsAsFactors = FALSE
      ))
    }
    out <- unique(data.frame(
      scope_key = defs$commit_key,
      label = paste(defs$FACE, defs$CONDITION, sep = " \u2013 "),
      stringsAsFactors = FALSE
    ))
    out[order(out$label), , drop = FALSE]
  })

  output$aoi_summary_config_ui <- shiny::renderUI({
    scopes <- completed_scope_table()
    if (nrow(scopes) == 0L) {
      return(shiny::div(
        class = "alert alert-warning",
        "Commit AOIs, or load the prepared demonstration AOIs, before configuring a participant summary."
      ))
    }

    map <- fixrep_map()
    raw_names <- names(raw_fixrep())
    excluded <- unique(c(
      map$participant,
      map$trial,
      map$fix_x,
      map$fix_y,
      map$fix_dur,
      map$image_position
    ))
    candidates <- setdiff(raw_names, excluded)

    shiny::tagList(
      shiny::div(
        class = "loaded-file-box",
        shiny::tags$ul(
          shiny::tags$li(paste("Participant:", map$participant)),
          shiny::tags$li(paste("Mapped trial:", map$trial)),
          shiny::tags$li(paste("Mapped face:", map$face)),
          shiny::tags$li(paste("Mapped condition:", map$condition))
        )
      ),
      shiny::selectInput(
        "aoi_summary_scope_mode",
        "Completed AOI scope",
        choices = c(
          "Choose a scope rule" = "",
          "All completed face/condition scopes" = "all",
          "Select completed scopes" = "selected"
        ),
        selected = ""
      ),
      shiny::conditionalPanel(
        "input.aoi_summary_scope_mode == 'selected'",
        shiny::selectizeInput(
          "aoi_summary_scopes",
          "Included completed scopes",
          choices = stats::setNames(scopes$scope_key, scopes$label),
          multiple = TRUE,
          selected = character()
        )
      ),
      shiny::selectizeInput(
        "aoi_summary_trial_extra",
        "Additional columns needed to identify a trial",
        choices = candidates,
        multiple = TRUE,
        selected = character()
      ),
      shiny::selectizeInput(
        "aoi_summary_groups",
        "Retained grouping variables",
        choices = candidates,
        multiple = TRUE,
        selected = character()
      ),
      shiny::checkboxInput(
        "aoi_summary_groups_confirmed",
        "I confirm that these are the grouping variables to retain",
        value = FALSE
      ),
      shiny::checkboxGroupInput(
        "aoi_summary_measures",
        "Participant-summary measures",
        choices = c(
          "Fixation count" = "fixation_count",
          "Summed fixation duration" = "summed_fixation_duration",
          "Mean fixation duration" = "mean_fixation_duration"
        ),
        selected = character()
      ),
      shiny::selectInput(
        "aoi_summary_recipe",
        "Aggregation recipe",
        choices = c(
          "Choose a recipe" = "",
          "Equal trial weighting" = "equal_trial",
          "Pool fixation rows" = "pooled_fixations"
        ),
        selected = ""
      ),
      shiny::conditionalPanel(
        "input.aoi_summary_recipe == 'equal_trial'",
        shiny::selectInput(
          "aoi_summary_empty_denominator",
          "Trials with no fixation in an available AOI",
          choices = c(
            "Choose a denominator" = "",
            "Include all eligible trials as zero count and summed duration" = "all_trials",
            "Use AOI-hit trials only" = "hit_trials"
          ),
          selected = ""
        )
      ),
      shiny::selectInput(
        "aoi_summary_layout",
        "Output layout",
        choices = c(
          "Choose a layout" = "",
          "Long: one participant/group/AOI row" = "long",
          "Wide: AOIs in constructed column names" = "wide"
        ),
        selected = ""
      ),
      shiny::actionButton(
        "aoi_summary_review",
        "Review aggregation specification",
        class = "btn-primary"
      )
    )
  })

  shiny::observeEvent(input$aoi_summary_groups, {
    shiny::updateCheckboxInput(
      session,
      "aoi_summary_groups_confirmed",
      value = FALSE
    )
  }, ignoreInit = TRUE)

  build_summary_config <- function() {
    map <- fixrep_map()
    scopes <- completed_scope_table()
    scope_mode <- input$aoi_summary_scope_mode %||% ""
    selected_scopes <- if (identical(scope_mode, "all")) {
      scopes$scope_key
    } else if (identical(scope_mode, "selected")) {
      input$aoi_summary_scopes %||% character()
    } else {
      character()
    }

    if (length(selected_scopes) == 0L) {
      stop("Choose at least one completed AOI scope.", call. = FALSE)
    }
    if (!isTRUE(input$aoi_summary_groups_confirmed)) {
      stop("Confirm the retained grouping-variable selection.", call. = FALSE)
    }
    measures <- input$aoi_summary_measures %||% character()
    if (length(measures) == 0L) {
      stop("Choose at least one participant-summary measure.", call. = FALSE)
    }
    recipe <- input$aoi_summary_recipe %||% ""
    if (!recipe %in% c("equal_trial", "pooled_fixations")) {
      stop("Choose an aggregation recipe.", call. = FALSE)
    }
    empty_denominator <- if (identical(recipe, "equal_trial")) {
      input$aoi_summary_empty_denominator %||% ""
    } else {
      NA_character_
    }
    if (identical(recipe, "equal_trial") &&
        !empty_denominator %in% c("all_trials", "hit_trials")) {
      stop("Choose how no-fixation AOI trials enter the denominator.", call. = FALSE)
    }
    layout <- input$aoi_summary_layout %||% ""
    if (!layout %in% c("long", "wide")) {
      stop("Choose a long or wide output layout.", call. = FALSE)
    }

    annotation_item <- annotation_value()
    config <- list(
      participant_col = map$participant,
      trial_key_cols = unique(c(
        map$participant,
        map$trial,
        input$aoi_summary_trial_extra %||% character()
      )),
      face_col = map$face,
      condition_col = map$condition,
      duration_col = map$fix_dur,
      aoi_col = aoi_export_name(annotation_item$name_mapping, "which_aoi"),
      status_col = aoi_export_name(
        annotation_item$name_mapping,
        "aoi_assignment_status"
      ),
      group_cols = input$aoi_summary_groups %||% character(),
      scope_keys = selected_scopes,
      excluded_scope_keys = setdiff(scopes$scope_key, selected_scopes),
      measures = measures,
      recipe = recipe,
      empty_aoi_denominator = empty_denominator,
      layout = layout,
      limits = list(
        intermediate_rows = 1000000L,
        result_rows = 250000L,
        wide_columns = 1000L
      )
    )
    aoi_summary_configure_names(config)
  }

  reviewed <- shiny::eventReactive(input$aoi_summary_review, {
    tryCatch({
      config <- build_summary_config()
      annotation_item <- annotation_value()
      defs <- aoi_state$session_defs()
      selected_defs <- defs[defs$commit_key %in% config$scope_keys, , drop = FALSE]
      summary <- aoi_summary_calculate(
        annotation_item$data,
        config,
        selected_defs
      )

      annotated_temp <- tempfile(fileext = ".csv")
      on.exit(unlink(annotated_temp), add = TRUE)
      readr::write_csv(annotation_item$data, annotated_temp, na = "")
      source_sha256 <- digest::digest(
        annotated_temp,
        algo = "sha256",
        file = TRUE
      )
      generated_at <- format(
        Sys.time(),
        "%Y-%m-%dT%H:%M:%SZ",
        tz = "UTC"
      )
      specification <- aoi_summary_specification(
        config = config,
        annotation = annotation_item,
        summary = summary,
        source_filename = annotated_filename(),
        source_sha256 = source_sha256,
        generated_at = generated_at,
        all_scope_keys = completed_scope_table()$scope_key
      )
      signature <- digest::digest(
        list(config, annotation_item$data, selected_defs),
        algo = "sha256",
        serialize = TRUE
      )

      list(
        config = config,
        annotation = annotation_item,
        defs = selected_defs,
        summary = summary,
        specification = specification,
        source_filename = annotated_filename(),
        source_sha256 = source_sha256,
        generated_at = generated_at,
        signature = signature
      )
    }, error = function(error) {
      shiny::showNotification(
        conditionMessage(error),
        type = "error",
        duration = 6
      )
      NULL
    })
  }, ignoreInit = TRUE)

  review_valid <- shiny::reactive({
    item <- reviewed()
    if (is.null(item)) return(FALSE)
    current_config <- tryCatch(build_summary_config(), error = function(error) NULL)
    if (is.null(current_config)) return(FALSE)
    current_defs <- aoi_state$session_defs()
    current_defs <- current_defs[
      current_defs$commit_key %in% current_config$scope_keys,
      ,
      drop = FALSE
    ]
    current_signature <- digest::digest(
      list(current_config, annotation_value()$data, current_defs),
      algo = "sha256",
      serialize = TRUE
    )
    identical(current_signature, item$signature)
  })

  output$aoi_summary_review_status <- shiny::renderUI({
    item <- reviewed()
    if (is.null(item)) {
      return(shiny::div(
        class = "alert alert-info",
        "Complete the required decisions and review the aggregation specification."
      ))
    }
    if (!review_valid()) {
      return(shiny::div(
        class = "alert alert-warning",
        "The data or a selected decision changed. Review the specification again."
      ))
    }
    shiny::tagList(
      shiny::div(
        class = "alert alert-success",
        "The reviewed specification is current."
      ),
      shiny::downloadButton(
        "download_aoi_participant_summary",
        "Download workbook and R script",
        class = "btn-warning"
      )
    )
  })

  output$aoi_summary_specification <- DT::renderDT({
    shiny::req(review_valid())
    aoi_workbench_dt(reviewed()$specification, page_length = 15)
  })
  output$aoi_summary_preview <- DT::renderDT({
    shiny::req(review_valid())
    aoi_workbench_dt(reviewed()$summary, page_length = 15)
  })

  output$download_aoi_participant_summary <- shiny::downloadHandler(
    filename = function() {
      paste0("vorogaze_participant_summary_", Sys.Date(), ".zip")
    },
    content = function(file) {
      shiny::req(review_valid())
      item <- reviewed()
      aoi_summary_write_bundle(
        file = file,
        specification = item$specification,
        summary = item$summary,
        config = item$config,
        aoi_definitions = item$defs,
        annotated_filename = item$source_filename,
        expected_sha256 = item$source_sha256,
        expected_rows = nrow(item$annotation$data)
      )
    },
    contentType = "application/zip"
  )

  invisible(list(annotation = annotation, reviewed_summary = reviewed))
}
