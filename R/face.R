# Image helpers ----

# Finds the image file whose basename or stem matches the FACE value in the data.
find_face_file <- function(face, files) {
  if (is.null(face) || length(face) == 0 || length(files) == 0) {
    return(NULL)
  }
  
  face <- as.character(face)[1]
  
  file_labels <- names(files)
  if (is.null(file_labels)) {
    file_labels <- files
  } else {
    missing_labels <- is.na(file_labels) | file_labels == ""
    file_labels[missing_labels] <- files[missing_labels]
  }
  
  file_base <- basename(file_labels)
  file_stem <- tools::file_path_sans_ext(file_base)
  file_label_stem <- tools::file_path_sans_ext(file_labels)
  face_stem <- tools::file_path_sans_ext(basename(face))
  
  match_idx <- which(
    file_labels == face |
      file_label_stem == face |
      file_base == face |
      file_stem == face |
      file_stem == face_stem
  )
  
  if (length(match_idx) == 0) {
    return(NULL)
  }
  
  files[match_idx[1]]
}


# Reads an image, applies orientation metadata, and returns raster plus dimensions.
read_face_image <- function(path) {
  img <- magick::image_read(path) |> magick::image_orient()
  if (length(img) > 1) img <- img[1]
  
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  
  magick::image_write(img, path = tmp, format = "png")
  
  rast <- png::readPNG(tmp, native = TRUE)
  list(
    rast = rast,
    width = ncol(rast),
    height = nrow(rast)
  )
}

# Reads orientation-aware dimensions without decoding the image into a raster.
read_face_dimensions <- function(path) {
  img <- magick::image_read(path) |> magick::image_orient()
  if (length(img) > 1) img <- img[1]

  info <- magick::image_info(img)
  width <- as.integer(info$width[[1]])
  height <- as.integer(info$height[[1]])
  if (!is.finite(width) || !is.finite(height) || width < 1L || height < 1L) {
    stop("Face image dimensions are unavailable: ", basename(path), call. = FALSE)
  }

  list(width = width, height = height)
}


# Draws the selected face image in either top-left or centre-origin coordinates.
plot_face_image <- function(
    face_image_path = NULL,
    image_origin = c("top_left", "center"),
    placeholder_width = 600,
    placeholder_height = 800
) {
  
  image_origin <- match.arg(image_origin)
  
  if (is.null(face_image_path)) {
    image_width <- placeholder_width
    image_height <- placeholder_height
    face_image <- NULL
  } else {
    face_image <- read_face_image(face_image_path)
    image_width <- face_image$width
    image_height <- face_image$height
  }
  
  if (image_origin == "top_left") {
    image_left <- 0
    image_right <- image_width
    image_top <- 0
    image_bottom <- image_height
  } else {
    image_left <- -image_width / 2
    image_right <- image_width / 2
    image_top <- -image_height / 2
    image_bottom <- image_height / 2
  }
  
  my_xlim <- c(image_left, image_right)
  my_ylim <- c(image_bottom, image_top)
  
  x_ticks <- pretty(my_xlim)
  y_ticks <- pretty(c(image_top, image_bottom))
  
  plot(
    x = 0,
    y = 0,
    type = "n",
    xlim = my_xlim,
    ylim = my_ylim,
    xaxs = "i",
    yaxs = "i",
    asp = 1,
    axes = FALSE,
    ann = FALSE
  )
  
  axis(
    side = 3,
    pos = image_top,
    at = x_ticks
  )
  
  axis(
    side = 2,
    pos = image_left,
    at = y_ticks,
    las = 1
  )
  
  if (is.null(face_image)) {
    text(
      x = mean(c(image_left, image_right)),
      y = mean(c(image_top, image_bottom)),
      labels = "?",
      cex = 8,
      font = 2
    )
    
    return(invisible(NULL))
  }
  
  rasterImage(
    image = face_image$rast,
    xleft = image_left,
    ybottom = image_bottom,
    xright = image_right,
    ytop = image_top
  )
  
  invisible(NULL)
}
