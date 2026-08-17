args <- commandArgs(trailingOnly = TRUE)

# This disposable process handles untrusted image bytes. Keep the elapsed-time
# limit here rather than on the long-lived Shiny process.
Sys.setenv(MAGICK_TIME_LIMIT = "60")

status <- tryCatch({
  stopifnot(length(args) == 3)
  source_path <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
  target_path <- normalizePath(args[[2]], winslash = "/", mustWork = FALSE)
  dimension_limit <- suppressWarnings(as.integer(args[[3]]))
  stopifnot(is.finite(dimension_limit), dimension_limit > 0)

  suppressPackageStartupMessages(library(magick))
  image <- magick::image_read(source_path)
  stopifnot(length(image) == 1)

  info <- magick::image_info(image)
  stopifnot(
    nrow(info) == 1,
    toupper(info$format[[1]]) %in% c("PNG", "JPEG", "JPG"),
    is.finite(info$width[[1]]),
    is.finite(info$height[[1]]),
    info$width[[1]] >= 1,
    info$height[[1]] >= 1,
    info$width[[1]] <= dimension_limit,
    info$height[[1]] <= dimension_limit
  )

  image <- magick::image_orient(image)
  image <- magick::image_strip(image)
  magick::image_write(image, path = target_path, format = "png")

  output_info <- magick::image_info(magick::image_read(target_path))
  stopifnot(nrow(output_info) == 1, toupper(output_info$format[[1]]) == "PNG")
  0L
}, error = function(error) {
  1L
})

quit(save = "no", status = status, runLast = FALSE)
