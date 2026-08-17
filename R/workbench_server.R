vorogaze_server <- function(input, output, session) {
  aoi_demo_server(input, output, session)
  public_upload_store <- if (vorogaze_public_mode()) {
    new_public_session_upload_store(session)
  } else {
    NULL
  }

  # Fixation report tab ----

  default_fixrep <- default_fixrep_path()

  current_fixrep_path <- reactive({
    if (vorogaze_public_mode()) {
      uploaded_path <- stage_public_fixrep_upload(input$upload_fixrep, public_upload_store)
      return(uploaded_path %||% default_fixrep)
    }

    active_fixrep_path(input$upload_fixrep, default_fixrep)
  })

  output$download_bundled_fixrep <- downloadHandler(
    filename = function() {
      basename(default_fixrep %||% "fixations_synthetic.csv")
    },
    content = function(file) {
      copy_bundled_file(default_fixrep, file)
    }
  )

  output$fixrep_file_display <- renderText({
    fixrep_source_label(input$upload_fixrep, default_fixrep)
  })

  fixrep_raw <- reactive({
    path <- current_fixrep_path()
    req(path)
    read_fixrep(path)
  })

  fixrep_for_standardisation <- reactive({
    prepare_fixrep_for_standardisation(fixrep_raw())
  })

  fixrep_read_mode <- reactive({
    if (is.null(current_fixrep_path())) {
      return(NULL)
    }

    tryCatch(
      attr(fixrep_raw(), "read_mode", exact = TRUE),
      error = function(e) NULL
    )
  })

  fixrep_map <- reactive({
    req_fixrep_map(input)
  })

  screen_params <- reactive({
    screen_params_from_input(input)
  })

  fixrep <- reactive({
    raw <- fixrep_for_standardisation()
    map <- fixrep_map()
    req_fixrep_map_matches_cols(raw, map)

    standardise_fixrep(
      raw = raw,
      map = map,
      screen = screen_params()
    )
  })

  screen_fixrep <- reactive({
    if (is.null(current_fixrep_path())) {
      return(NULL)
    }

    tryCatch(
      fixrep(),
      error = function(e) NULL
    )
  })

  output$fixrep_mapping_ui <- renderUI({
    cols <- tryCatch(
      names(fixrep_for_standardisation()),
      error = function(e) character(0)
    )

    make_fixrep_mapping_ui(cols)
  })

  output$fixrep_raw_preview <- DT::renderDT({
    DT::datatable(
      format_table_int(fixrep_raw()),
      rownames = FALSE,
      filter = "top",
      extensions = c("FixedColumns"),
      options = list(
        dom = "tip",
        searching = TRUE,
        paging = TRUE,
        pageLength = 5,
        lengthChange = FALSE,
        scrollX = TRUE,
        fixedColumns = list(leftColumns = 1),
        autoWidth = TRUE,
        columnDefs = list(
          list(targets = "_all", className = "dt-nowrap")
        )
      ),
      class = "compact stripe hover nowrap"
    )
  })

  output$fixrep_preview <- DT::renderDT({
    DT::datatable(
      format_table_int(fixrep()),
      rownames = FALSE,
      filter = "top",
      extensions = c("FixedColumns"),
      options = list(
        dom = "tip",
        searching = TRUE,
        paging = TRUE,
        pageLength = 5,
        lengthChange = FALSE,
        scrollX = TRUE,
        fixedColumns = list(leftColumns = 1),
        autoWidth = TRUE,
        columnDefs = list(
          list(targets = "_all", className = "dt-nowrap")
        )
      ),
      class = "compact stripe hover nowrap"
    )
  })

  output$fixrep_summary <- DT::renderDT({
    DT::datatable(
      fixrep_summary_table(fixrep()),
      rownames = FALSE,
      options = list(
        dom = "t",
        searching = FALSE,
        paging = FALSE,
        ordering = FALSE,
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(
          list(targets = "_all", className = "dt-nowrap")
        )
      ),
      class = "compact stripe hover nowrap"
    )
  })

  output$fixrep_validation_summary <- renderUI({
    fixrep_validation_summary_ui(
      raw = fixrep_for_standardisation(),
      map = fixrep_map(),
      standardised = fixrep()
    )
  })

  # Faces tab ----

  bundled_face_dir <- bundled_face_dir_path()

  output$download_bundled_face_dir <- downloadHandler(
    filename = function() {
      paste0(basename(bundled_face_dir %||% "faces_300x350"), ".zip")
    },
    content = function(file) {
      zip_bundled_face_dir(bundled_face_dir, file)
    },
    contentType = "application/zip"
  )

  output$face_dir_display <- renderText({
    face_source_label(input$upload_face_dir, bundled_face_dir)
  })

  face_files <- reactive({
    if (vorogaze_public_mode()) {
      uploaded_files <- stage_public_face_uploads(input$upload_face_dir, public_upload_store)

      if (length(uploaded_files) > 0) {
        return(uploaded_files)
      }

      return(list_face_image_files(bundled_face_dir))
    }

    uploaded_files <- uploaded_face_image_files(input$upload_face_dir)

    if (length(uploaded_files) > 0) {
      return(uploaded_files)
    }

    list_face_image_files(bundled_face_dir)
  })

  output$face_file_ui <- renderUI({
    make_face_file_ui(face_files())
  })

  face_image_path <- reactive({
    resolve_face_file_selection(input$selected_face_file, face_files())
  })

  sanity_face_image_path <- reactive({
    if (is.null(input$sanity_face)) {
      return(NULL)
    }

    find_face_file(
      face = input$sanity_face,
      files = face_files()
    )
  })

  output$view_face <- renderPlot({
    req(input$image_origin)

    if (identical(input$image_origin, "other")) {
      plot_message(
        title = "This image origin is not supported yet.",
        subtitle = "Choose Top left or Centre to continue."
      )
      return(invisible(NULL))
    }

    plot_face_image(
      face_image_path = face_image_path(),
      image_origin = input$image_origin
    )
  })

  aoi_state <- aoi_workbench_server(
    input = input,
    output = output,
    session = session,
    fixrep = fixrep,
    face_files = face_files,
    screen_params = screen_params
  )

  aoi_exports_server(
    input = input,
    output = output,
    session = session,
    raw_fixrep = fixrep_for_standardisation,
    standardised_fixrep = fixrep,
    fixrep_map = fixrep_map,
    face_files = face_files,
    screen_params = screen_params,
    active_fixrep_path = current_fixrep_path,
    aoi_state = aoi_state
  )

  # Screen tab ----

  output$screen_bounds_display <- renderUI({
    screen_bounds_summary_ui(screen_params())
  })

  output$view_screen <- renderPlot({
    req(input$screen_origin)

    if (identical(input$screen_origin, "other")) {
      plot_message(
        title = "Other screen origins are not supported yet.",
        subtitle = "Choose Top left or Centre to continue."
      )
      return(invisible(NULL))
    }

    screen <- screen_params()

    plot_screen(
      fixrep = screen_fixrep(),
      fix_x = "FIX_X",
      fix_y = "FIX_Y",
      screen_left = screen$left,
      screen_right = screen$right,
      screen_top = screen$top,
      screen_bottom = screen$bottom,
      screen_origin = "top_left",
      tick_by = 100,
      fixation_pad = 50
    )
  })

  output$view_screen_hover_label <- renderUI({
    hover_coordinate_label(input$view_screen_hover)
  })

  # Sanity tab ----

  output$sanity_filter_ui <- renderUI({
    req(fixrep())
    make_sanity_filter_ui(fixrep())
  })

  sanity_fixrep <- reactive({
    if (is.null(current_fixrep_path())) {
      return(NULL)
    }

    dat <- tryCatch(
      fixrep(),
      error = function(e) NULL
    )

    filter_sanity_fixrep(
      dat = dat,
      face = input$sanity_face,
      condition = input$sanity_condition
    )
  })

  invalid_image_position_use_center <- reactiveVal(FALSE)
  invalid_image_position_dismissed <- reactiveVal(FALSE)

  observeEvent(
    list(input$upload_fixrep, input$sanity_face, input$sanity_condition, input$image_origin),
    {
      invalid_image_position_use_center(FALSE)
      invalid_image_position_dismissed(FALSE)
    },
    ignoreInit = TRUE
  )

  observeEvent(input$dismiss_invalid_image_position, {
    invalid_image_position_dismissed(TRUE)
    removeModal()
  })

  observeEvent(input$use_center_for_invalid_image_position, {
    invalid_image_position_use_center(TRUE)
    removeModal()
  })

  sanity_image_position_info <- reactive({
    if (is.null(current_fixrep_path())) {
      return(missing_image_position_info())
    }

    req(input$sanity_face, input$sanity_condition)

    image_position_values(
      fixrep = sanity_fixrep(),
      img_x = "IMG_X",
      img_y = "IMG_Y"
    )
  })

  output$view_sanity <- renderPlot({
    req(input$screen_origin, input$image_origin)

    if (identical(input$screen_origin, "other")) {
      plot_message(
        title = "Other screen origins are not supported yet.",
        subtitle = "Choose Top left or Centre to continue."
      )
      return(invisible(NULL))
    }

    if (identical(input$image_origin, "other")) {
      plot_message(
        title = "Other image origins are not supported yet.",
        subtitle = "Choose Top left or Centre to continue."
      )
      return(invisible(NULL))
    }

    screen <- screen_params()
    has_fixrep <- !is.null(current_fixrep_path())

    if (isTRUE(has_fixrep)) {
      req(input$sanity_face, input$sanity_condition)
    }

    image_position_info <- if (isTRUE(has_fixrep)) {
      sanity_image_position_info()
    } else {
      missing_image_position_info()
    }

    if (
      identical(image_position_info$status, "invalid") &&
      !isTRUE(invalid_image_position_use_center())
    ) {
      if (!isTRUE(invalid_image_position_dismissed())) {
        show_invalid_image_position_modal()
      }

      plot_message(
        title = "Image placement is ambiguous.",
        subtitle = "Choose how to place the image in the warning dialog.",
        title_cex = 1.1
      )
      return(invisible(NULL))
    }

    plot_sanity(
      fixrep = sanity_fixrep(),
      face_image_path = sanity_face_image_path(),
      image_position = if (isTRUE(invalid_image_position_use_center())) "screen_center" else "auto",
      show_image = isTRUE(has_fixrep),
      fix_x = "FIX_X",
      fix_y = "FIX_Y",
      img_x = "IMG_X",
      img_y = "IMG_Y",
      face_col = "FACE",
      condition_col = "CONDITION",
      screen_left = screen$left,
      screen_right = screen$right,
      screen_top = screen$top,
      screen_bottom = screen$bottom,
      screen_origin = "top_left",
      image_origin = input$image_origin
    )
  })

  output$view_sanity_hover_label <- renderUI({
    hover_coordinate_label(input$view_sanity_hover)
  })

}
