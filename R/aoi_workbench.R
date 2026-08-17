# AOI workbench helpers and Shiny wiring ----

aoi_workbench_face_key <- function(x) {
  x <- as.character(x)
  out <- tolower(tools::file_path_sans_ext(basename(x)))
  out[is.na(x)] <- NA_character_
  out
}

aoi_workbench_empty_centres <- function() {
  tibble::tibble(
    face_key = character(),
    face = character(),
    aoi_id = character(),
    aoi_name = character(),
    deldir_id = character(),
    x = numeric(),
    y = numeric()
  )
}

aoi_workbench_empty_assignments <- function() {
  tibble::tibble(
    commit_key = character(),
    face_key = character(),
    .VOROGAZE_SOURCE_ROW = integer(),
    SUBJECT = character(),
    FACE = character(),
    CONDITION = character(),
    TRIAL_ID = character(),
    FIX_X = numeric(),
    FIX_Y = numeric(),
    FIX_DUR = numeric(),
    IMG_X = numeric(),
    IMG_Y = numeric(),
    FIX_X_IMG = numeric(),
    FIX_Y_IMG = numeric(),
    AOI_ASSIGNED = character(),
    AOI_ID = character(),
    AOI_NAME = character(),
    AOI_LABEL = character()
  )
}

aoi_workbench_empty_defs <- function() {
  tibble::tibble(
    commit_key = character(),
    face_key = character(),
    FACE = character(),
    CONDITION = character(),
    AOI_ASSIGNED = character(),
    AOI_ID = character(),
    AOI_NAME = character(),
    AOI_LABEL = character(),
    x = numeric(),
    y = numeric()
  )
}

aoi_workbench_empty_metrics <- function() {
  tibble::tibble(
    SUBJECT = character(),
    FACE = character(),
    CONDITION = character(),
    AOI_ASSIGNED = character(),
    AOI_ID = character(),
    AOI_NAME = character(),
    AOI_LABEL = character(),
    N_FIX = integer(),
    TOTAL_FIX_DUR = numeric(),
    MEAN_FIX_DUR = numeric()
  )
}

aoi_workbench_filter_centres <- function(centres, face_key) {
  if (is.null(centres) || nrow(centres) == 0 || is.null(face_key) || is.na(face_key)) {
    return(aoi_workbench_empty_centres())
  }

  centres[centres$face_key == face_key, , drop = FALSE]
}

aoi_workbench_next_id <- function(centres, face_key) {
  pts <- aoi_workbench_filter_centres(centres, face_key)
  nums <- suppressWarnings(as.integer(sub("^AOI_", "", pts$aoi_id)))
  nums <- nums[is.finite(nums)]
  sprintf("AOI_%03d", if (length(nums) == 0) 1L else max(nums) + 1L)
}

aoi_workbench_upsert_centre <- function(centres, face, label, x, y) {
  if (is.null(centres)) {
    centres <- aoi_workbench_empty_centres()
  }

  face <- as.character(face)[1]
  face_key <- aoi_workbench_face_key(face)
  label <- trimws(as.character(label %||% ""))
  label <- if (nzchar(label)) label else NA_character_

  idx <- integer(0)
  if (!is.na(label)) {
    idx <- which(
      centres$face_key == face_key &
        (centres$aoi_name == label | centres$deldir_id == label)
    )
  }

  if (length(idx) > 0) {
    i <- idx[1]
    centres$x[i] <- as.numeric(x)
    centres$y[i] <- as.numeric(y)
    centres$aoi_name[i] <- label
    centres$deldir_id[i] <- label
    return(tibble::as_tibble(centres))
  }

  aoi_id <- aoi_workbench_next_id(centres, face_key)
  deldir_id <- if (!is.na(label)) label else aoi_id

  tibble::as_tibble(rbind(
    centres,
    tibble::tibble(
      face_key = face_key,
      face = face,
      aoi_id = aoi_id,
      aoi_name = label,
      deldir_id = deldir_id,
      x = as.numeric(x),
      y = as.numeric(y)
    )
  ))
}

aoi_workbench_prepare_centres <- function(centres, face_key = NULL) {
  if (is.null(centres) || nrow(centres) == 0) {
    return(aoi_workbench_empty_centres())
  }

  out <- tibble::as_tibble(centres)

  if (!is.null(face_key)) {
    out <- out[out$face_key == face_key, , drop = FALSE]
  }

  out$x <- suppressWarnings(as.numeric(out$x))
  out$y <- suppressWarnings(as.numeric(out$y))
  out$deldir_id <- trimws(as.character(out$deldir_id))
  out <- out[is.finite(out$x) & is.finite(out$y) & nzchar(out$deldir_id), , drop = FALSE]
  out <- out[!duplicated(out$deldir_id), , drop = FALSE]
  out <- out[!duplicated(out[c("x", "y")]), , drop = FALSE]
  rownames(out) <- NULL
  out
}

aoi_workbench_delete_nearest <- function(centres, face_key, x, y) {
  pts <- aoi_workbench_prepare_centres(centres, face_key)

  if (nrow(pts) == 0) {
    return(centres)
  }

  d2 <- (pts$x - x)^2 + (pts$y - y)^2
  delete_id <- pts$aoi_id[which.min(d2)]

  out <- centres[!(centres$face_key == face_key & centres$aoi_id == delete_id), , drop = FALSE]
  tibble::as_tibble(out)
}

aoi_workbench_point_in_image <- function(click, width, height) {
  !is.null(click) &&
    is.finite(click$x) &&
    is.finite(click$y) &&
    click$x >= 0 &&
    click$x <= width &&
    click$y >= 0 &&
    click$y <= height
}

aoi_workbench_deldir <- function(centres, width, height) {
  pts <- aoi_workbench_prepare_centres(centres)

  if (nrow(pts) < 2 || !requireNamespace("deldir", quietly = TRUE)) {
    return(NULL)
  }

  res <- tryCatch(
    deldir::deldir(
      x = pts$x,
      y = pts$y,
      rw = c(0, width, 0, height),
      id = pts$deldir_id
    ),
    error = function(e) NULL
  )

  if (is.null(res)) {
    return(NULL)
  }

  list(input_pts = pts, res = res)
}

aoi_workbench_screen_centre <- function(screen) {
  if (is.null(screen)) {
    return(c(x = NA_real_, y = NA_real_))
  }

  vals <- c(screen$left, screen$right, screen$top, screen$bottom)
  if (!all(is.finite(vals))) {
    return(c(x = NA_real_, y = NA_real_))
  }

  c(
    x = mean(c(screen$left, screen$right)),
    y = mean(c(screen$top, screen$bottom))
  )
}

aoi_workbench_image_geometry <- function(
    fixrep,
    width,
    height,
    screen,
    image_origin = c("center", "top_left"),
    use_screen_center = FALSE
) {
  image_origin <- match.arg(image_origin)
  pos <- image_position_values(fixrep, img_x = "IMG_X", img_y = "IMG_Y")
  centre <- c(x = NA_real_, y = NA_real_)
  status <- pos$status

  if (identical(pos$status, "valid")) {
    if (identical(image_origin, "top_left")) {
      centre <- c(x = pos$x + width / 2, y = pos$y + height / 2)
    } else {
      centre <- c(x = pos$x, y = pos$y)
    }
    status <- "valid"
  } else if (isTRUE(use_screen_center)) {
    centre <- aoi_workbench_screen_centre(screen)
    if (all(is.finite(centre))) {
      status <- "screen_center"
    }
  }

  if (!all(is.finite(centre))) {
    return(list(
      status = status,
      message = "Image placement is missing or ambiguous.",
      left = NA_real_,
      right = NA_real_,
      top = NA_real_,
      bottom = NA_real_,
      centre_x = NA_real_,
      centre_y = NA_real_
    ))
  }

  list(
    status = status,
    message = if (identical(status, "screen_center")) "Using screen centre." else "Using report image position.",
    left = centre[["x"]] - width / 2,
    right = centre[["x"]] + width / 2,
    top = centre[["y"]] - height / 2,
    bottom = centre[["y"]] + height / 2,
    centre_x = centre[["x"]],
    centre_y = centre[["y"]]
  )
}

aoi_workbench_fixations_image_space <- function(fixrep, geometry) {
  if (
    is.null(fixrep) ||
      nrow(fixrep) == 0 ||
      is.null(geometry) ||
      !all(is.finite(c(geometry$left, geometry$top)))
  ) {
    return(tibble::tibble())
  }

  out <- tibble::as_tibble(fixrep)
  out$FIX_X_IMG <- out$FIX_X - geometry$left
  out$FIX_Y_IMG <- out$FIX_Y - geometry$top
  out
}

aoi_workbench_assign_fixations <- function(fixrep, centres, width, height, geometry) {
  pts <- aoi_workbench_prepare_centres(centres)

  if (nrow(pts) < 2) {
    return(aoi_workbench_empty_assignments()[0, setdiff(names(aoi_workbench_empty_assignments()), c("commit_key", "face_key", "AOI_ID", "AOI_NAME", "AOI_LABEL")), drop = FALSE])
  }

  fx <- aoi_workbench_fixations_image_space(fixrep, geometry)

  if (nrow(fx) == 0) {
    return(aoi_workbench_empty_assignments()[0, setdiff(names(aoi_workbench_empty_assignments()), c("commit_key", "face_key", "AOI_ID", "AOI_NAME", "AOI_LABEL")), drop = FALSE])
  }

  keep <- is.finite(fx$FIX_X_IMG) &
    is.finite(fx$FIX_Y_IMG) &
    fx$FIX_X_IMG >= 0 &
    fx$FIX_X_IMG <= width &
    fx$FIX_Y_IMG >= 0 &
    fx$FIX_Y_IMG <= height

  fx <- fx[keep, , drop = FALSE]
  if (nrow(fx) == 0) {
    return(aoi_workbench_empty_assignments()[0, setdiff(names(aoi_workbench_empty_assignments()), c("commit_key", "face_key", "AOI_ID", "AOI_NAME", "AOI_LABEL")), drop = FALSE])
  }

  assign_one <- function(px, py) {
    d2 <- (pts$x - px)^2 + (pts$y - py)^2
    pts$deldir_id[which.min(d2)]
  }

  fx$AOI_ASSIGNED <- vapply(
    seq_len(nrow(fx)),
    function(i) assign_one(fx$FIX_X_IMG[[i]], fx$FIX_Y_IMG[[i]]),
    character(1)
  )

  for (col in intersect(c("SUBJECT", "FACE", "CONDITION", "TRIAL_ID"), names(fx))) {
    fx[[col]] <- as.character(fx[[col]])
  }

  keep_cols <- c(
    ".VOROGAZE_SOURCE_ROW",
    "SUBJECT", "FACE", "CONDITION", "TRIAL_ID",
    "FIX_X", "FIX_Y", "FIX_DUR",
    "IMG_X", "IMG_Y",
    "FIX_X_IMG", "FIX_Y_IMG",
    "AOI_ASSIGNED"
  )

  tibble::as_tibble(fx[, intersect(keep_cols, names(fx)), drop = FALSE])
}

aoi_workbench_aoi_defs <- function(centres, face_key, face, condition, commit_key) {
  pts <- aoi_workbench_prepare_centres(centres, face_key)

  if (nrow(pts) == 0) {
    return(aoi_workbench_empty_defs())
  }

  tibble::tibble(
    commit_key = commit_key,
    face_key = face_key,
    FACE = as.character(face),
    CONDITION = as.character(condition),
    AOI_ASSIGNED = pts$deldir_id,
    AOI_ID = pts$aoi_id,
    AOI_NAME = pts$aoi_name,
    AOI_LABEL = ifelse(!is.na(pts$aoi_name) & nzchar(pts$aoi_name), pts$aoi_name, pts$aoi_id),
    x = pts$x,
    y = pts$y
  )
}

aoi_workbench_annotate_assignments <- function(assignments, defs, face_key, commit_key) {
  if (is.null(assignments) || nrow(assignments) == 0) {
    return(aoi_workbench_empty_assignments())
  }

  out <- tibble::as_tibble(assignments)
  out$commit_key <- commit_key
  out$face_key <- face_key

  out |>
    dplyr::left_join(
      defs[, c("commit_key", "face_key", "AOI_ASSIGNED", "AOI_ID", "AOI_NAME", "AOI_LABEL"), drop = FALSE],
      by = c("commit_key", "face_key", "AOI_ASSIGNED")
    ) |>
    dplyr::select(dplyr::any_of(names(aoi_workbench_empty_assignments())))
}

aoi_workbench_metric_totals <- function(assignments) {
  assignments |>
    dplyr::group_by(.data$commit_key, .data$SUBJECT, .data$FACE, .data$CONDITION, .data$AOI_ASSIGNED) |>
    dplyr::summarise(
      N_FIX = dplyr::n(),
      TOTAL_FIX_DUR = sum(.data$FIX_DUR, na.rm = TRUE),
      MEAN_FIX_DUR = mean(.data$FIX_DUR, na.rm = TRUE),
      .groups = "drop"
    )
}

aoi_workbench_metrics_unaggregated <- function(assignments, defs) {
  if (is.null(assignments) || nrow(assignments) == 0 || is.null(defs) || nrow(defs) == 0) {
    return(aoi_workbench_empty_metrics())
  }

  groups <- assignments |>
    dplyr::distinct(.data$commit_key, .data$SUBJECT, .data$FACE, .data$CONDITION)

  def_cols <- defs |>
    dplyr::select(dplyr::all_of(c(
      "commit_key",
      "FACE",
      "CONDITION",
      "AOI_ASSIGNED",
      "AOI_ID",
      "AOI_NAME",
      "AOI_LABEL"
    ))) |>
    dplyr::distinct()

  grid <- dplyr::left_join(
    groups,
    def_cols,
    by = c("commit_key", "FACE", "CONDITION"),
    relationship = "many-to-many"
  )

  totals <- aoi_workbench_metric_totals(assignments)

  out <- grid |>
    dplyr::left_join(
      totals,
      by = c("commit_key", "SUBJECT", "FACE", "CONDITION", "AOI_ASSIGNED")
    ) |>
    dplyr::mutate(
      N_FIX = dplyr::coalesce(.data$N_FIX, 0L),
      TOTAL_FIX_DUR = dplyr::coalesce(.data$TOTAL_FIX_DUR, 0),
      MEAN_FIX_DUR = dplyr::if_else(.data$N_FIX == 0L, NA_real_, .data$MEAN_FIX_DUR)
    ) |>
    dplyr::arrange(.data$SUBJECT, .data$FACE, .data$CONDITION, .data$AOI_LABEL) |>
    dplyr::select(-dplyr::all_of("commit_key"))

  tibble::as_tibble(out)
}

aoi_workbench_mean_or_na <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  mean(x, na.rm = TRUE)
}

aoi_workbench_metrics_over_subjects <- function(metrics) {
  if (is.null(metrics) || nrow(metrics) == 0) {
    return(aoi_workbench_empty_metrics()[0, setdiff(names(aoi_workbench_empty_metrics()), "SUBJECT"), drop = FALSE])
  }

  metrics |>
    dplyr::group_by(.data$FACE, .data$CONDITION, .data$AOI_ASSIGNED, .data$AOI_ID, .data$AOI_NAME, .data$AOI_LABEL) |>
    dplyr::summarise(
      N_FIX = mean(.data$N_FIX, na.rm = TRUE),
      TOTAL_FIX_DUR = mean(.data$TOTAL_FIX_DUR, na.rm = TRUE),
      MEAN_FIX_DUR = aoi_workbench_mean_or_na(.data$MEAN_FIX_DUR),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$FACE, .data$CONDITION, .data$AOI_LABEL) |>
    tibble::as_tibble()
}

aoi_workbench_metrics_over_faces <- function(metrics_over_subjects) {
  if (is.null(metrics_over_subjects) || nrow(metrics_over_subjects) == 0) {
    return(aoi_workbench_empty_metrics()[0, setdiff(names(aoi_workbench_empty_metrics()), c("SUBJECT", "FACE")), drop = FALSE])
  }

  metrics_over_subjects |>
    dplyr::group_by(.data$CONDITION, .data$AOI_ASSIGNED, .data$AOI_ID, .data$AOI_NAME, .data$AOI_LABEL) |>
    dplyr::summarise(
      N_FIX = mean(.data$N_FIX, na.rm = TRUE),
      TOTAL_FIX_DUR = mean(.data$TOTAL_FIX_DUR, na.rm = TRUE),
      MEAN_FIX_DUR = aoi_workbench_mean_or_na(.data$MEAN_FIX_DUR),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$CONDITION, .data$AOI_LABEL) |>
    tibble::as_tibble()
}

aoi_workbench_plot <- function(face_path, centres, fixrep_image = NULL, assignments = NULL) {
  if (is.null(face_path) || !file.exists(face_path)) {
    plot_message("Face image not found.", "Check the Faces tab directory selection.")
    return(invisible(NULL))
  }

  face <- read_face_image(face_path)
  width <- face$width
  height <- face$height
  pts <- aoi_workbench_prepare_centres(centres)
  dd <- aoi_workbench_deldir(pts, width, height)

  op <- par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  on.exit(par(op), add = TRUE)

  plot(
    NA,
    NA,
    type = "n",
    xlim = c(0, width),
    ylim = c(height, 0),
    asp = 1,
    axes = FALSE,
    ann = FALSE
  )

  rasterImage(face$rast, xleft = 0, ybottom = height, xright = width, ytop = 0)

  if (!is.null(dd) && !is.null(dd$res$dirsgs) && nrow(dd$res$dirsgs) > 0) {
    segs <- dd$res$dirsgs
    segments(segs$x1, segs$y1, segs$x2, segs$y2, lwd = 2, col = "#0099B8")
  }

  fx <- if (!is.null(assignments) && nrow(assignments) > 0) assignments else fixrep_image
  if (!is.null(fx) && nrow(fx) > 0 && all(c("FIX_X_IMG", "FIX_Y_IMG") %in% names(fx))) {
    points(fx$FIX_X_IMG, fx$FIX_Y_IMG, pch = 21, cex = 1, lwd = 1, bg = "#F7D060", col = "#4A2A00")
  }

  if (nrow(pts) > 0) {
    points(pts$x, pts$y, pch = 4, cex = 1.7, lwd = 2.3, col = "#007C91")
    text(
      pts$x,
      pts$y,
      labels = ifelse(!is.na(pts$aoi_name) & nzchar(pts$aoi_name), pts$aoi_name, pts$aoi_id),
      pos = 3,
      cex = 0.8,
      font = 2,
      col = "#005466"
    )
  }

  invisible(NULL)
}

aoi_workbench_dt <- function(data, page_length = 6) {
  DT::datatable(
    data,
    rownames = FALSE,
    filter = "top",
    options = list(
      dom = "tip",
      searching = TRUE,
      paging = TRUE,
      pageLength = page_length,
      lengthChange = FALSE,
      scrollX = TRUE,
      autoWidth = TRUE,
      columnDefs = list(list(targets = "_all", className = "dt-nowrap"))
    ),
    class = "compact stripe hover nowrap"
  )
}

aoi_workbench_write_csv <- function(data, file, empty_message) {
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    readr::write_csv(tibble::tibble(message = empty_message), file)
  } else {
    readr::write_csv(data, file)
  }
}

aoi_workbench_panel <- function() {
  bslib::nav_panel(
    "AOI Workbench",
    bslib::layout_columns(
      col_widths = c(3, 6, 3),
      height = "100%",
      bslib::card(
        bslib::card_header("input"),
        bslib::card_body(
          shiny::uiOutput("aoi_workbench_filter_ui"),
          shiny::textInput("aoi_workbench_label", "AOI label", value = "left_eye"),
          shiny::checkboxInput("aoi_workbench_use_screen_center", "Use screen centre if image position is ambiguous", value = FALSE),
          shiny::div(
            class = "aoi-workbench-button-row",
            shiny::actionButton("aoi_workbench_assign", "Assign fixations", class = "btn-primary"),
            shiny::actionButton("aoi_workbench_commit", "Commit/replace face", class = "btn-success"),
            shiny::actionButton("aoi_workbench_reset", "Reset session", class = "btn-outline-danger")
          ),
          shiny::uiOutput("aoi_workbench_status"),
          shiny::tags$hr(),
          shiny::downloadButton("download_aoi_workbench_current", "Download current assignments"),
          shiny::downloadButton("download_aoi_workbench_session", "Download session assignments"),
          shiny::downloadButton("download_aoi_workbench_aois", "Download AOIs"),
          shiny::downloadButton("download_aoi_workbench_metrics", "Download legacy metrics")
        )
      ),
      bslib::card(
        bslib::card_header("image AOIs"),
        bslib::card_body(
          shiny::plotOutput(
            "aoi_workbench_plot",
            click = "aoi_workbench_click",
            dblclick = "aoi_workbench_dblclick",
            height = "70vh"
          )
        )
      ),
      bslib::navset_card_tab(
        full_screen = TRUE,
        title = "outputs",
        bslib::nav_panel("AOIs", shiny::div(class = "dt-table-output", DT::DTOutput("aoi_workbench_centres"))),
        bslib::nav_panel("Current", shiny::div(class = "dt-table-output", DT::DTOutput("aoi_workbench_current"))),
        bslib::nav_panel("Session", shiny::div(class = "dt-table-output", DT::DTOutput("aoi_workbench_session"))),
        bslib::nav_panel(
          "Legacy metrics",
          shiny::tagList(
            shiny::p(
              class = "text-muted",
              "Legacy convention: pool fixations across trials within participant, face, condition and AOI."
            ),
            shiny::div(class = "dt-table-output", DT::DTOutput("aoi_workbench_metrics_unaggregated"))
          )
        ),
        bslib::nav_panel("Legacy mean subjects", shiny::div(class = "dt-table-output", DT::DTOutput("aoi_workbench_metrics_subjects"))),
        bslib::nav_panel("Legacy mean faces", shiny::div(class = "dt-table-output", DT::DTOutput("aoi_workbench_metrics_faces")))
      )
    )
  )
}

aoi_workbench_server <- function(input, output, session, fixrep, face_files, screen_params) {
  centres <- shiny::reactiveVal(aoi_workbench_empty_centres())
  current_assignments <- shiny::reactiveVal(aoi_workbench_empty_assignments())
  current_defs <- shiny::reactiveVal(aoi_workbench_empty_defs())
  session_assignments <- shiny::reactiveVal(aoi_workbench_empty_assignments())
  session_defs <- shiny::reactiveVal(aoi_workbench_empty_defs())

  shiny::observeEvent(
    list(
      fixrep(),
      screen_params(),
      input$image_origin,
      input$aoi_workbench_use_screen_center
    ),
    {
      centres(aoi_workbench_empty_centres())
      current_assignments(aoi_workbench_empty_assignments())
      current_defs(aoi_workbench_empty_defs())
      session_assignments(aoi_workbench_empty_assignments())
      session_defs(aoi_workbench_empty_defs())
    },
    ignoreInit = TRUE
  )

  selected_face <- shiny::reactive({
    dat <- fixrep()
    faces <- sort(unique(stats::na.omit(as.character(dat$FACE))))
    shiny::validate(shiny::need(length(faces) > 0, "No FACE values are available."))
    input$aoi_workbench_face %||% faces[[1]]
  })

  selected_condition <- shiny::reactive({
    dat <- fixrep()
    face <- selected_face()
    conditions <- sort(unique(stats::na.omit(as.character(dat$CONDITION[as.character(dat$FACE) == face]))))
    if (length(conditions) == 0) {
      conditions <- sort(unique(stats::na.omit(as.character(dat$CONDITION))))
    }
    shiny::validate(shiny::need(length(conditions) > 0, "No CONDITION values are available."))
    input$aoi_workbench_condition %||% conditions[[1]]
  })

  active_face_key <- shiny::reactive({
    aoi_workbench_face_key(selected_face())
  })

  active_commit_key <- shiny::reactive({
    paste(active_face_key(), selected_condition(), sep = "|")
  })

  output$aoi_workbench_filter_ui <- shiny::renderUI({
    dat <- fixrep()
    faces <- sort(unique(stats::na.omit(as.character(dat$FACE))))
    selected <- input$aoi_workbench_face %||% faces[[1]]
    selected <- if (selected %in% faces) selected else faces[[1]]

    conditions <- sort(unique(stats::na.omit(as.character(dat$CONDITION[as.character(dat$FACE) == selected]))))
    if (length(conditions) == 0) {
      conditions <- sort(unique(stats::na.omit(as.character(dat$CONDITION))))
    }

    condition_selected <- input$aoi_workbench_condition %||% conditions[[1]]
    condition_selected <- if (condition_selected %in% conditions) condition_selected else conditions[[1]]

    shiny::tagList(
      shiny::selectInput("aoi_workbench_face", "Face", choices = faces, selected = selected),
      shiny::selectInput("aoi_workbench_condition", "Condition", choices = conditions, selected = condition_selected)
    )
  })

  face_path <- shiny::reactive({
    find_face_file(selected_face(), face_files())
  })

  face_info <- shiny::reactive({
    path <- face_path()
    shiny::validate(shiny::need(!is.null(path), "No matching face image found."))
    read_face_image(unname(path))
  })

  workbench_fixrep <- shiny::reactive({
    filter_sanity_fixrep(
      dat = fixrep(),
      face = selected_face(),
      condition = selected_condition()
    )
  })

  active_centres <- shiny::reactive({
    aoi_workbench_filter_centres(centres(), active_face_key())
  })

  geometry <- shiny::reactive({
    if (identical(input$image_origin %||% "center", "other")) {
      return(list(status = "unsupported", message = "Choose Top left or Centre image origin."))
    }

    info <- face_info()
    aoi_workbench_image_geometry(
      fixrep = workbench_fixrep(),
      width = info$width,
      height = info$height,
      screen = screen_params(),
      image_origin = input$image_origin %||% "center",
      use_screen_center = isTRUE(input$aoi_workbench_use_screen_center)
    )
  })

  fixrep_image <- shiny::reactive({
    aoi_workbench_fixations_image_space(workbench_fixrep(), geometry())
  })

  shiny::observeEvent(
    list(selected_face(), selected_condition(), centres(), input$image_origin, input$aoi_workbench_use_screen_center),
    {
      current_assignments(aoi_workbench_empty_assignments())
      current_defs(aoi_workbench_empty_defs())
    },
    ignoreInit = TRUE
  )

  shiny::observeEvent(input$aoi_workbench_click, {
    info <- face_info()
    click <- input$aoi_workbench_click

    if (!aoi_workbench_point_in_image(click, info$width, info$height)) {
      shiny::showNotification("Click on the image, not the padding.", type = "message", duration = 1.5)
      return()
    }

    centres(aoi_workbench_upsert_centre(
      centres = centres(),
      face = selected_face(),
      label = input$aoi_workbench_label,
      x = click$x,
      y = click$y
    ))
  })

  shiny::observeEvent(input$aoi_workbench_dblclick, {
    info <- face_info()
    click <- input$aoi_workbench_dblclick

    if (!aoi_workbench_point_in_image(click, info$width, info$height)) {
      shiny::showNotification("Double-click on the image, not the padding.", type = "message", duration = 1.5)
      return()
    }

    centres(aoi_workbench_delete_nearest(
      centres = centres(),
      face_key = active_face_key(),
      x = click$x,
      y = click$y
    ))
  })

  shiny::observeEvent(input$aoi_workbench_assign, {
    info <- face_info()
    geom <- geometry()
    pts <- active_centres()

    if (!identical(geom$status, "valid") && !identical(geom$status, "screen_center")) {
      shiny::showNotification(geom$message %||% "Image placement is not usable.", type = "error", duration = 3)
      return()
    }

    if (nrow(aoi_workbench_prepare_centres(pts)) < 2) {
      shiny::showNotification("Add at least two AOI centres before assigning.", type = "message", duration = 2)
      return()
    }

    defs <- aoi_workbench_aoi_defs(
      centres = centres(),
      face_key = active_face_key(),
      face = selected_face(),
      condition = selected_condition(),
      commit_key = active_commit_key()
    )

    assigned <- aoi_workbench_assign_fixations(
      fixrep = workbench_fixrep(),
      centres = pts,
      width = info$width,
      height = info$height,
      geometry = geom
    )

    current_defs(defs)
    current_assignments(aoi_workbench_annotate_assignments(
      assignments = assigned,
      defs = defs,
      face_key = active_face_key(),
      commit_key = active_commit_key()
    ))

    shiny::showNotification(
      sprintf("Assigned %d fixation%s.", nrow(current_assignments()), if (nrow(current_assignments()) == 1) "" else "s"),
      type = "message",
      duration = 2
    )
  })

  shiny::observeEvent(input$aoi_workbench_commit, {
    cur <- current_assignments()
    defs <- current_defs()

    if (nrow(defs) == 0) {
      shiny::showNotification("Assign fixations before committing this face.", type = "message", duration = 2)
      return()
    }

    key <- active_commit_key()
    old_session <- session_assignments()
    old_defs <- session_defs()

    session_assignments(dplyr::bind_rows(
      old_session[old_session$commit_key != key, , drop = FALSE],
      cur
    ))
    session_defs(dplyr::bind_rows(
      old_defs[old_defs$commit_key != key, , drop = FALSE],
      defs
    ))

    shiny::showNotification("Committed this face/condition to the session table.", type = "message", duration = 2)
  })

  shiny::observeEvent(input$aoi_workbench_reset, {
    current_assignments(aoi_workbench_empty_assignments())
    current_defs(aoi_workbench_empty_defs())
    session_assignments(aoi_workbench_empty_assignments())
    session_defs(aoi_workbench_empty_defs())
    shiny::showNotification("AOI workbench session cleared.", type = "message", duration = 2)
  })

  session_metrics_unaggregated <- shiny::reactive({
    aoi_workbench_metrics_unaggregated(session_assignments(), session_defs())
  })

  session_metrics_subjects <- shiny::reactive({
    aoi_workbench_metrics_over_subjects(session_metrics_unaggregated())
  })

  session_metrics_faces <- shiny::reactive({
    aoi_workbench_metrics_over_faces(session_metrics_subjects())
  })

  output$aoi_workbench_status <- shiny::renderUI({
    geom <- geometry()
    shiny::div(
      class = "loaded-file-box aoi-workbench-status",
      shiny::tags$ul(
        shiny::tags$li(sprintf("Face fixations: %d", nrow(workbench_fixrep()))),
        shiny::tags$li(sprintf("AOIs: %d", nrow(active_centres()))),
        shiny::tags$li(sprintf("Placement: %s", geom$message %||% geom$status))
      )
    )
  })

  output$aoi_workbench_plot <- shiny::renderPlot({
    aoi_workbench_plot(
      face_path = unname(face_path()),
      centres = active_centres(),
      fixrep_image = fixrep_image(),
      assignments = current_assignments()
    )
  }, res = 72)

  output$aoi_workbench_centres <- DT::renderDT({
    aoi_workbench_dt(active_centres())
  })

  output$aoi_workbench_current <- DT::renderDT({
    aoi_workbench_dt(current_assignments())
  })

  output$aoi_workbench_session <- DT::renderDT({
    aoi_workbench_dt(session_assignments())
  })

  output$aoi_workbench_metrics_unaggregated <- DT::renderDT({
    aoi_workbench_dt(session_metrics_unaggregated())
  })

  output$aoi_workbench_metrics_subjects <- DT::renderDT({
    aoi_workbench_dt(session_metrics_subjects())
  })

  output$aoi_workbench_metrics_faces <- DT::renderDT({
    aoi_workbench_dt(session_metrics_faces())
  })

  output$download_aoi_workbench_current <- shiny::downloadHandler(
    filename = function() paste0("vorogaze_current_assignments_", Sys.Date(), ".csv"),
    content = function(file) aoi_workbench_write_csv(current_assignments(), file, "No current AOI assignments.")
  )

  output$download_aoi_workbench_session <- shiny::downloadHandler(
    filename = function() paste0("vorogaze_session_assignments_", Sys.Date(), ".csv"),
    content = function(file) aoi_workbench_write_csv(session_assignments(), file, "No session AOI assignments.")
  )

  output$download_aoi_workbench_aois <- shiny::downloadHandler(
    filename = function() paste0("vorogaze_aoi_definitions_", Sys.Date(), ".csv"),
    content = function(file) aoi_workbench_write_csv(session_defs(), file, "No committed AOI definitions.")
  )

  output$download_aoi_workbench_metrics <- shiny::downloadHandler(
    filename = function() paste0("vorogaze_aoi_metrics_", Sys.Date(), ".csv"),
    content = function(file) aoi_workbench_write_csv(session_metrics_unaggregated(), file, "No AOI metrics.")
  )

  list(
    centres = centres,
    current_assignments = current_assignments,
    current_defs = current_defs,
    session_assignments = session_assignments,
    session_defs = session_defs
  )
}
