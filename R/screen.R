# Extracts complete, finite fixation x/y pairs from a standardised fixation report.
fixation_values <- function(fixrep, fix_x = "FIX_X", fix_y = "FIX_Y") {
  if (is.null(fixrep) || nrow(fixrep) == 0) {
    return(list(x = numeric(0), y = numeric(0)))
  }
  
  x_values <- fixrep[[fix_x]]
  y_values <- fixrep[[fix_y]]
  
  if (is.null(x_values) || is.null(y_values)) {
    return(list(x = numeric(0), y = numeric(0)))
  }
  
  keep <- is.finite(x_values) & is.finite(y_values)
  
  list(
    x = x_values[keep],
    y = y_values[keep]
  )
}

# Expands plot limits when fixations fall outside the screen or image area.
expand_plot_bounds <- function(
    left,
    right,
    top,
    bottom,
    fix_x_values,
    fix_y_values,
    fixation_pad = 50
) {
  if (length(fix_x_values) == 0) {
    return(list(left = left, right = right, top = top, bottom = bottom))
  }
  
  fix_min_x <- min(fix_x_values, na.rm = TRUE)
  fix_max_x <- max(fix_x_values, na.rm = TRUE)
  fix_min_y <- min(fix_y_values, na.rm = TRUE)
  fix_max_y <- max(fix_y_values, na.rm = TRUE)
  
  list(
    left = if (fix_min_x < left) fix_min_x - fixation_pad else left,
    right = if (fix_max_x > right) fix_max_x + fixation_pad else right,
    top = if (fix_min_y < top) fix_min_y - fixation_pad else top,
    bottom = if (fix_max_y > bottom) fix_max_y + fixation_pad else bottom
  )
}

# Draws the screen coordinate system and overlays any available fixation points.
plot_screen <- function(
    fixrep = NULL,
    fix_x = "FIX_X",
    fix_y = "FIX_Y",
    screen_left = 0,
    screen_right = 1600,
    screen_top = 0,
    screen_bottom = 900,
    screen_origin = c("top_left", "center"),
    tick_by = 100,
    fixation_pad = 50
) {
  screen_origin <- match.arg(screen_origin)
  
  screen_width <- screen_right - screen_left
  screen_height <- abs(screen_bottom - screen_top)
  
  if (identical(screen_origin, "center")) {
    screen_left <- -screen_width / 2
    screen_right <- screen_width / 2
    screen_top <- -screen_height / 2
    screen_bottom <- screen_height / 2
  }
  
  fixations <- fixation_values(fixrep, fix_x, fix_y)
  fix_x_values <- fixations$x
  fix_y_values <- fixations$y
  
  plot_bounds <- expand_plot_bounds(
    left = screen_left,
    right = screen_right,
    top = screen_top,
    bottom = screen_bottom,
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
    ylim = c(plot_bottom, plot_top), # reversed y-axis: y increases downward
    xaxs = "i",
    yaxs = "i",
    asp = 1,
    axes = FALSE,
    ann = FALSE
  )
  
  axis(
    side = 3,
    pos = plot_top,
    at = x_ticks
  )
  
  axis(
    side = 2,
    pos = plot_left,
    at = y_ticks,
    las = 1
  )
  
  rect(
    xleft = screen_left,
    ybottom = screen_bottom,
    xright = screen_right,
    ytop = screen_top,
    border = "black"
  )
  
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
