# Checks whether the current data has one usable IMG_X/IMG_Y placement.
image_position_values <- function(fixrep, img_x = "IMG_X", img_y = "IMG_Y") {
  if (
    is.null(fixrep) ||
    nrow(fixrep) == 0 ||
    !all(c(img_x, img_y) %in% names(fixrep))
  ) {
    return(list(status = "missing", x = NA_real_, y = NA_real_))
  }
  
  image_x_value <- unique(stats::na.omit(fixrep[[img_x]]))
  image_y_value <- unique(stats::na.omit(fixrep[[img_y]]))
  
  if (
    length(image_x_value) != 1 ||
    length(image_y_value) != 1 ||
    !is.finite(image_x_value) ||
    !is.finite(image_y_value)
  ) {
    return(list(status = "invalid", x = NA_real_, y = NA_real_))
  }
  
  list(status = "valid", x = image_x_value, y = image_y_value)
}

# Draws the combined screen, face image, and fixation sanity-check plot.
plot_sanity <- function(
    fixrep = NULL,
    face_image_path = NULL,
    selected_face = NULL,
    selected_condition = NULL,
    fix_x = "FIX_X",
    fix_y = "FIX_Y",
    img_x = "IMG_X",
    img_y = "IMG_Y",
    face_col = "FACE",
    condition_col = "CONDITION",
    screen_left = 0,
    screen_right = 1600,
    screen_top = 0,
    screen_bottom = 900,
    screen_origin = c("top_left", "center"),
    image_origin = c("center", "top_left"),
    image_position = c("auto", "screen_center"),
    show_image = TRUE,
    tick_by = 100,
    fixation_pad = 50,
    placeholder_width = 600,
    placeholder_height = 800
) {
  screen_origin <- match.arg(screen_origin)
  image_origin <- match.arg(image_origin)
  image_position <- match.arg(image_position)
  
  screen_width <- screen_right - screen_left
  screen_height <- abs(screen_bottom - screen_top)
  
  if (identical(screen_origin, "center")) {
    screen_left <- -screen_width / 2
    screen_right <- screen_width / 2
    screen_top <- -screen_height / 2
    screen_bottom <- screen_height / 2
  }
  
  if (!is.null(fixrep)) {
    if (!is.null(selected_face) && face_col %in% names(fixrep)) {
      fixrep <- fixrep[fixrep[[face_col]] == selected_face, , drop = FALSE]
    }
    
    if (!is.null(selected_condition) && condition_col %in% names(fixrep)) {
      fixrep <- fixrep[fixrep[[condition_col]] == selected_condition, , drop = FALSE]
    }
  }
  
  fixations <- fixation_values(fixrep, fix_x, fix_y)
  fix_x_values <- fixations$x
  fix_y_values <- fixations$y
  
  if (is.null(face_image_path)) {
    face_image <- NULL
    image_width <- placeholder_width
    image_height <- placeholder_height
  } else {
    face_image <- read_face_image(face_image_path)
    image_width <- face_image$width
    image_height <- face_image$height
  }
  
  screen_centre_x <- mean(c(screen_left, screen_right))
  screen_centre_y <- mean(c(screen_bottom, screen_top))
  
  if (identical(image_position, "screen_center")) {
    image_centre_x <- screen_centre_x
    image_centre_y <- screen_centre_y
  } else {
    image_position_info <- image_position_values(fixrep, img_x, img_y)
    
    if (identical(image_position_info$status, "missing")) {
      image_centre_x <- screen_centre_x
      image_centre_y <- screen_centre_y
    } else if (identical(image_position_info$status, "invalid")) {
      plot_message(
        title = "Selected data does not have one unique IMG_X and IMG_Y.",
        subtitle = "Check FACE, CONDITION, IMG_X, and IMG_Y.",
        title_cex = 1.1
      )
      return(invisible(NULL))
    } else {
      if (identical(image_origin, "top_left")) {
        image_centre_x <- image_position_info$x + image_width / 2
        image_centre_y <- image_position_info$y + image_height / 2
      } else {
        image_centre_x <- image_position_info$x
        image_centre_y <- image_position_info$y
      }
    }
  }
  
  image_left <- image_centre_x - image_width / 2
  image_right <- image_centre_x + image_width / 2
  image_top <- image_centre_y - image_height / 2
  image_bottom <- image_centre_y + image_height / 2
  
  if (isTRUE(show_image)) {
    base_left <- min(screen_left, image_left)
    base_right <- max(screen_right, image_right)
    base_top <- min(screen_top, image_top)
    base_bottom <- max(screen_bottom, image_bottom)
  } else {
    base_left <- screen_left
    base_right <- screen_right
    base_top <- screen_top
    base_bottom <- screen_bottom
  }
  
  plot_bounds <- expand_plot_bounds(
    left = base_left,
    right = base_right,
    top = base_top,
    bottom = base_bottom,
    fix_x_values = fix_x_values,
    fix_y_values = fix_y_values,
    fixation_pad = fixation_pad
  )
  
  plot_left <- plot_bounds$left
  plot_right <- plot_bounds$right
  plot_top <- plot_bounds$top
  plot_bottom <- plot_bounds$bottom
  
  x_ticks <- seq(
    floor(plot_left / tick_by) * tick_by,
    ceiling(plot_right / tick_by) * tick_by,
    by = tick_by
  )
  
  y_ticks <- seq(
    floor(plot_top / tick_by) * tick_by,
    ceiling(plot_bottom / tick_by) * tick_by,
    by = tick_by
  )
  
  par(
    mar = c(1.5, 4, 3, 1),
    xaxs = "i",
    yaxs = "i"
  )
  
  plot(
    NA,
    NA,
    type = "n",
    xlim = c(plot_left, plot_right),
    ylim = c(plot_bottom, plot_top),
    asp = 1,
    axes = FALSE,
    ann = FALSE
  )
  
  axis(side = 3, pos = plot_top, at = x_ticks)
  axis(side = 2, pos = plot_left, at = y_ticks, las = 1)
  
  rect(
    xleft = screen_left,
    ybottom = screen_bottom,
    xright = screen_right,
    ytop = screen_top,
    border = "black",
    lwd = 2
  )
  
  if (!isTRUE(show_image)) {
    # Draw only the screen and fixation overlay.
  } else if (is.null(face_image)) {
    rect(
      xleft = image_left,
      ybottom = image_bottom,
      xright = image_right,
      ytop = image_top,
      border = "grey50",
      lty = 2
    )
    
    text(
      x = image_centre_x,
      y = image_centre_y,
      labels = "?",
      cex = 8,
      font = 2
    )
  } else {
    rasterImage(
      image = face_image$rast,
      xleft = image_left,
      ybottom = image_bottom,
      xright = image_right,
      ytop = image_top
    )
  }
  
  if (length(fix_x_values) > 0) {
    points(
      x = fix_x_values,
      y = fix_y_values,
      pch = 4,
      cex = 0.9,
      lwd = 2,
      col = "#FF0000"
    )
  }
  
  invisible(NULL)
}
