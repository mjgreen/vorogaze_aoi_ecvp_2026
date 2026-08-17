# AOI demo helpers and UI ----

aoi_demo_fixture_dir <- function() {
  configured <- Sys.getenv("VOROGAZE_EXAMPLE_FIXTURE_DIR", unset = "")
  path <- if (nzchar(configured)) {
    configured
  } else {
    file.path(getwd(), "demo", "worked_example")
  }

  normalizePath(
    path,
    winslash = "/",
    mustWork = FALSE
  )
}

aoi_demo_face_path <- function() {
  file.path(aoi_demo_fixture_dir(), "faces", "001_03.jpg")
}

aoi_demo_fixrep_path <- function() {
  file.path(aoi_demo_fixture_dir(), "fixrep_demo_synthetic.csv")
}

aoi_demo_default_landmarks <- function() {
  data.frame(
    label = c("left_eye", "right_eye", "nose", "mouth", "chin"),
    x = c(130, 230, 170, 174, 177),
    y = c(224, 224, 277, 325, 388),
    stringsAsFactors = FALSE
  )
}

aoi_demo_read_fixrep <- function(path = aoi_demo_fixrep_path()) {
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

aoi_demo_prepare_landmarks <- function(landmarks) {
  if (is.null(landmarks) || nrow(landmarks) == 0) {
    return(data.frame(label = character(), x = numeric(), y = numeric()))
  }

  out <- data.frame(
    label = trimws(as.character(landmarks$label)),
    x = suppressWarnings(as.numeric(landmarks$x)),
    y = suppressWarnings(as.numeric(landmarks$y)),
    stringsAsFactors = FALSE
  )

  out <- out[nzchar(out$label) & is.finite(out$x) & is.finite(out$y), , drop = FALSE]
  out <- out[!duplicated(out$label), , drop = FALSE]
  out <- out[!duplicated(out[c("x", "y")]), , drop = FALSE]
  rownames(out) <- NULL
  out
}

aoi_demo_upsert_landmark <- function(landmarks, label, x, y) {
  label <- trimws(as.character(label %||% ""))

  if (!nzchar(label)) {
    label <- sprintf("landmark_%02d", nrow(landmarks) + 1L)
  }

  next_row <- data.frame(
    label = label,
    x = as.numeric(x),
    y = as.numeric(y),
    stringsAsFactors = FALSE
  )

  landmarks <- landmarks[landmarks$label != label, , drop = FALSE]
  rownames(landmarks) <- NULL
  rbind(landmarks, next_row)
}

aoi_demo_delete_nearest_landmark <- function(landmarks, x, y) {
  landmarks <- aoi_demo_prepare_landmarks(landmarks)

  if (nrow(landmarks) == 0) {
    return(landmarks)
  }

  d2 <- (landmarks$x - x)^2 + (landmarks$y - y)^2
  landmarks[-which.min(d2), , drop = FALSE]
}

aoi_demo_point_in_image <- function(click, width, height) {
  !is.null(click) &&
    is.finite(click$x) &&
    is.finite(click$y) &&
    click$x >= 0 &&
    click$x <= width &&
    click$y >= 0 &&
    click$y <= height
}

aoi_demo_deldir <- function(landmarks, width, height) {
  landmarks <- aoi_demo_prepare_landmarks(landmarks)

  if (nrow(landmarks) < 2 || !requireNamespace("deldir", quietly = TRUE)) {
    return(NULL)
  }

  res <- tryCatch(
    deldir::deldir(
      x = landmarks$x,
      y = landmarks$y,
      rw = c(0, width, 0, height),
      id = landmarks$label
    ),
    error = function(e) NULL
  )

  if (is.null(res)) {
    return(NULL)
  }

  list(input_pts = landmarks, res = res)
}

aoi_demo_fixations_image_space <- function(fixrep, width, height) {
  img_x <- unique(stats::na.omit(fixrep$IMG_X))
  img_y <- unique(stats::na.omit(fixrep$IMG_Y))

  if (length(img_x) != 1 || length(img_y) != 1) {
    stop("The bundled AOI fixture must have one image centre.")
  }

  image_left <- img_x[[1]] - width / 2
  image_top <- img_y[[1]] - height / 2
  fixrep$FIX_X_IMG <- fixrep$FIX_X - image_left
  fixrep$FIX_Y_IMG <- fixrep$FIX_Y - image_top
  fixrep
}

aoi_demo_assign_fixations <- function(fixrep, landmarks, width, height) {
  landmarks <- aoi_demo_prepare_landmarks(landmarks)

  if (nrow(landmarks) < 2) {
    return(data.frame())
  }

  fixrep <- aoi_demo_fixations_image_space(fixrep, width, height)
  in_bounds <- is.finite(fixrep$FIX_X_IMG) &
    is.finite(fixrep$FIX_Y_IMG) &
    fixrep$FIX_X_IMG >= 0 &
    fixrep$FIX_X_IMG <= width &
    fixrep$FIX_Y_IMG >= 0 &
    fixrep$FIX_Y_IMG <= height

  fixrep <- fixrep[in_bounds, , drop = FALSE]

  if (nrow(fixrep) == 0) {
    return(data.frame())
  }

  assign_one <- function(px, py) {
    d2 <- (landmarks$x - px)^2 + (landmarks$y - py)^2
    landmarks$label[[which.min(d2)]]
  }

  fixrep$AOI <- vapply(
    seq_len(nrow(fixrep)),
    function(i) assign_one(fixrep$FIX_X_IMG[[i]], fixrep$FIX_Y_IMG[[i]]),
    character(1)
  )

  fixrep
}

aoi_demo_metrics <- function(assignments, landmarks = NULL) {
  if (is.null(assignments) || nrow(assignments) == 0) {
    return(data.frame())
  }

  landmarks <- aoi_demo_prepare_landmarks(landmarks)
  if (nrow(landmarks) == 0) {
    aoi_levels <- sort(unique(assignments$AOI))
  } else {
    aoi_levels <- landmarks$label
  }

  n_fix <- stats::aggregate(FIX_DUR ~ AOI, assignments, length)
  total <- stats::aggregate(FIX_DUR ~ AOI, assignments, sum)
  mean_dur <- stats::aggregate(FIX_DUR ~ AOI, assignments, mean)

  out <- data.frame(AOI = aoi_levels, stringsAsFactors = FALSE)
  out <- merge(out, n_fix, by = "AOI", all.x = TRUE, sort = FALSE)
  out <- merge(out, total, by = "AOI", all.x = TRUE, sort = FALSE, suffixes = c("_n", "_total"))
  out <- merge(out, mean_dur, by = "AOI", all.x = TRUE, sort = FALSE)
  names(out) <- c("AOI", "N_FIX", "TOTAL_FIX_DUR", "MEAN_FIX_DUR")
  out$N_FIX[is.na(out$N_FIX)] <- 0L
  out$MEAN_FIX_DUR <- round(out$MEAN_FIX_DUR, 1)
  out[match(aoi_levels, out$AOI), , drop = FALSE]
}

aoi_demo_fixture_summary <- function(face_path, fixrep, face_info) {
  data.frame(
    item = c("face", "fixations", "subjects", "image_size"),
    value = c(
      basename(face_path),
      nrow(fixrep),
      length(unique(fixrep$SUBJECT)),
      sprintf("%s x %s", face_info$width, face_info$height)
    ),
    stringsAsFactors = FALSE
  )
}

aoi_demo_plot <- function(face_path, fixrep, landmarks, assignments = NULL) {
  face <- read_face_image(face_path)
  width <- face$width
  height <- face$height
  fixrep <- aoi_demo_fixations_image_space(fixrep, width, height)
  landmarks <- aoi_demo_prepare_landmarks(landmarks)
  dd <- aoi_demo_deldir(landmarks, width, height)

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

  if (!is.null(dd)) {
    segs <- dd$res$dirsgs

    if (!is.null(segs) && nrow(segs) > 0) {
      segments(segs$x1, segs$y1, segs$x2, segs$y2, lwd = 2, col = "#0099B8")
    }
  }

  if (nrow(fixrep) > 0) {
    points(
      fixrep$FIX_X_IMG,
      fixrep$FIX_Y_IMG,
      pch = 21,
      cex = 1.2,
      lwd = 1.1,
      bg = "#F7D060",
      col = "#4A2A00"
    )
  }

  if (nrow(landmarks) > 0) {
    points(landmarks$x, landmarks$y, pch = 4, cex = 1.7, lwd = 2.3, col = "#007C91")
    text(
      landmarks$x,
      landmarks$y,
      labels = landmarks$label,
      pos = 3,
      cex = 0.85,
      font = 2,
      col = "#005466"
    )
  }

  invisible(NULL)
}

aoi_demo_panel <- function() {
  bslib::nav_panel(
    "Worked Example",
    bslib::layout_columns(
      col_widths = c(2, 4, 6),
      height = "100%",
      bslib::card(
        bslib::card_header("landmarks"),
        bslib::card_body(
          shiny::textInput("aoi_demo_label", "Current label", value = "left_eye"),
          shiny::div(
            class = "aoi-demo-button-row",
            shiny::actionButton("aoi_demo_reset", "Reset examples"),
            shiny::actionButton("aoi_demo_clear", "Clear landmarks"),
            shiny::actionButton("aoi_demo_assign", "Assign fixations", class = "btn-primary")
          ),
          shiny::div(
            class = "compact-table-output",
            shiny::tableOutput("aoi_demo_landmarks")
          )
        )
      ),
      bslib::card(
        class = "aoi-demo-image-card",
        fill = FALSE,
        full_screen = TRUE,
        bslib::card_header("image AOIs"),
        bslib::card_body(
          shiny::div(
            class = "aoi-demo-plot-frame",
            shiny::plotOutput(
              "aoi_demo_plot",
              click = "aoi_demo_plot_click",
              dblclick = shiny::dblclickOpts(id = "aoi_demo_plot_dblclick"),
              width = "100%",
              height = "620px"
            )
          )
        )
      ),
      bslib::card(
        class = "aoi-demo-metrics-card",
        bslib::card_header("metrics"),
        bslib::card_body(
          shiny::div(
            class = "compact-table-output aoi-demo-metrics-output",
            shiny::tableOutput("aoi_demo_metrics")
          ),
          shiny::div(
            class = "compact-table-output aoi-demo-fixture-output",
            shiny::tableOutput("aoi_demo_fixture")
          )
        )
      )
    )
  )
}

aoi_demo_server <- function(input, output, session) {
  fixrep <- aoi_demo_read_fixrep()
  face_path <- aoi_demo_face_path()
  face_info <- read_face_image(face_path)
  default_landmarks <- aoi_demo_default_landmarks()
  landmarks <- shiny::reactiveVal(default_landmarks)
  assignments <- shiny::reactiveVal(
    aoi_demo_assign_fixations(fixrep, default_landmarks, face_info$width, face_info$height)
  )

  recompute_assignments <- function() {
    assignments(
      aoi_demo_assign_fixations(fixrep, landmarks(), face_info$width, face_info$height)
    )
  }

  observeEvent(input$aoi_demo_plot_click, {
    click <- input$aoi_demo_plot_click

    if (!aoi_demo_point_in_image(click, face_info$width, face_info$height)) {
      return()
    }

    landmarks(aoi_demo_upsert_landmark(landmarks(), input$aoi_demo_label, click$x, click$y))
    assignments(data.frame())
  })

  observeEvent(input$aoi_demo_plot_dblclick, {
    click <- input$aoi_demo_plot_dblclick

    if (!aoi_demo_point_in_image(click, face_info$width, face_info$height)) {
      return()
    }

    landmarks(aoi_demo_delete_nearest_landmark(landmarks(), click$x, click$y))
    assignments(data.frame())
  })

  observeEvent(input$aoi_demo_reset, {
    landmarks(aoi_demo_default_landmarks())
    recompute_assignments()
  })

  observeEvent(input$aoi_demo_clear, {
    landmarks(data.frame(label = character(), x = numeric(), y = numeric()))
    assignments(data.frame())
  })

  observeEvent(input$aoi_demo_assign, {
    recompute_assignments()
  })

  output$aoi_demo_plot <- renderPlot({
    aoi_demo_plot(face_path, fixrep, landmarks(), assignments())
  })

  output$aoi_demo_landmarks <- renderTable({
    out <- aoi_demo_prepare_landmarks(landmarks())

    if (nrow(out) == 0) {
      return(NULL)
    }

    out
  }, digits = 0)

  output$aoi_demo_metrics <- renderTable({
    out <- aoi_demo_metrics(assignments(), landmarks())

    if (nrow(out) == 0) {
      return(NULL)
    }

    out
  }, digits = 1)

  output$aoi_demo_fixture <- renderTable({
    aoi_demo_fixture_summary(face_path, fixrep, face_info)
  }, digits = 0)
}
