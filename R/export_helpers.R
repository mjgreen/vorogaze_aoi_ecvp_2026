# Row-preserving annotation and narrow participant-summary helpers ----

aoi_export_constructed_bases <- c(
  "vorogaze_source_row",
  "which_aoi",
  "aoi_id",
  "aoi_assignment_status",
  "fix_x_image",
  "fix_y_image",
  "aoi_definition_id"
)

aoi_export_resolve_names <- function(source_names, constructed_names) {
  used <- as.character(source_names)
  actual <- character(length(constructed_names))

  for (i in seq_along(constructed_names)) {
    candidate <- constructed_names[[i]]
    while (candidate %in% used) {
      candidate <- paste0(candidate, "_constructed_export_name")
    }
    actual[[i]] <- candidate
    used <- c(used, candidate)
  }

  data.frame(
    constructed_name = constructed_names,
    export_name = actual,
    collision_renamed = constructed_names != actual,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

aoi_export_name <- function(mapping, constructed_name) {
  hit <- mapping$export_name[mapping$constructed_name == constructed_name]
  if (length(hit) != 1L) {
    stop("Constructed export name is unavailable: ", constructed_name, call. = FALSE)
  }
  hit[[1]]
}

aoi_export_definition_ids <- function(defs) {
  if (is.null(defs) || nrow(defs) == 0) {
    return(data.frame(
      commit_key = character(),
      aoi_definition_id = character(),
      stringsAsFactors = FALSE
    ))
  }

  requireNamespace("digest", quietly = TRUE)
  defs <- as.data.frame(defs, stringsAsFactors = FALSE)
  split_defs <- split(defs, defs$commit_key)

  do.call(
    rbind,
    lapply(names(split_defs), function(key) {
      item <- split_defs[[key]]
      item <- item[
        order(item$AOI_LABEL, item$AOI_ID, item$x, item$y, na.last = TRUE),
        c("FACE", "CONDITION", "AOI_ID", "AOI_NAME", "AOI_LABEL", "x", "y"),
        drop = FALSE
      ]
      data.frame(
        commit_key = key,
        aoi_definition_id = paste0(
          "sha256:",
          digest::digest(item, algo = "sha256", serialize = TRUE)
        ),
        stringsAsFactors = FALSE
      )
    })
  )
}

aoi_export_append_constructed <- function(raw, constructed) {
  raw <- as.data.frame(raw, stringsAsFactors = FALSE, check.names = FALSE)
  mapping <- aoi_export_resolve_names(names(raw), names(constructed))
  out <- raw

  for (i in seq_len(nrow(mapping))) {
    out[[mapping$export_name[[i]]]] <- constructed[[mapping$constructed_name[[i]]]]
  }

  list(
    data = tibble::as_tibble(out, .name_repair = "minimal"),
    name_mapping = mapping
  )
}

aoi_export_annotate <- function(
    raw,
    standardised,
    assignments,
    defs,
    face_files,
    screen,
    image_origin = "center",
    use_screen_center = FALSE
) {
  if (is.null(raw) || is.null(standardised) || nrow(raw) != nrow(standardised)) {
    stop("The imported and standardised fixation reports do not align.", call. = FALSE)
  }

  standardised <- as.data.frame(
    standardised,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  n <- nrow(standardised)
  source_row <- standardised$.VOROGAZE_SOURCE_ROW

  status <- rep("aoi_not_committed", n)
  which_aoi <- rep(NA_character_, n)
  aoi_id <- rep(NA_character_, n)
  fix_x_image <- rep(NA_real_, n)
  fix_y_image <- rep(NA_real_, n)
  definition_id <- rep(NA_character_, n)

  defs <- if (is.null(defs)) aoi_workbench_empty_defs() else as.data.frame(defs)
  assignments <- if (is.null(assignments)) {
    aoi_workbench_empty_assignments()
  } else {
    as.data.frame(assignments)
  }

  definition_ids <- aoi_export_definition_ids(defs)
  scope_keys <- paste(
    aoi_workbench_face_key(standardised$FACE),
    as.character(standardised$CONDITION),
    sep = "|"
  )
  assignment_match <- match(
    paste(source_row, scope_keys, sep = "\036"),
    paste(
      assignments$.VOROGAZE_SOURCE_ROW,
      assignments$commit_key,
      sep = "\036"
    )
  )

  unique_scopes <- unique(scope_keys)

  for (scope_key in unique_scopes) {
    idx <- which(scope_keys == scope_key)
    scope <- standardised[idx, , drop = FALSE]
    face <- as.character(scope$FACE[[1]])
    condition <- as.character(scope$CONDITION[[1]])
    committed <- scope_key %in% defs$commit_key

    missing_coordinates <- !is.finite(scope$FIX_X) | !is.finite(scope$FIX_Y)
    status[idx[missing_coordinates]] <- "missing_coordinates"

    face_path <- find_face_file(face, face_files)
    if (is.null(face_path) || !file.exists(unname(face_path))) {
      usable <- idx[!missing_coordinates]
      status[usable] <- "invalid_image_geometry"
      next
    }

    info <- read_face_dimensions(unname(face_path))
    geometry <- aoi_workbench_image_geometry(
      fixrep = scope,
      width = info$width,
      height = info$height,
      screen = screen,
      image_origin = image_origin,
      use_screen_center = use_screen_center
    )

    geometry_valid <- identical(geometry$status, "valid") ||
      identical(geometry$status, "screen_center")

    if (!geometry_valid) {
      usable <- idx[!missing_coordinates]
      status[usable] <- "invalid_image_geometry"
      next
    }

    image_space <- aoi_workbench_fixations_image_space(scope, geometry)
    fix_x_image[idx] <- image_space$FIX_X_IMG
    fix_y_image[idx] <- image_space$FIX_Y_IMG

    if (!committed) {
      next
    }

    id_hit <- definition_ids$aoi_definition_id[
      definition_ids$commit_key == scope_key
    ]
    if (length(id_hit) == 1L) {
      definition_id[idx] <- id_hit[[1]]
    }

    in_image <- !missing_coordinates &
      is.finite(image_space$FIX_X_IMG) &
      is.finite(image_space$FIX_Y_IMG) &
      image_space$FIX_X_IMG >= 0 &
      image_space$FIX_X_IMG <= info$width &
      image_space$FIX_Y_IMG >= 0 &
      image_space$FIX_Y_IMG <= info$height

    status[idx[!missing_coordinates & !in_image]] <- "outside_image"
    candidate_idx <- idx[in_image]
    assignment_idx <- assignment_match[candidate_idx]
    found <- !is.na(assignment_idx)

    status[candidate_idx[found]] <- "assigned"
    which_aoi[candidate_idx[found]] <- assignments$AOI_LABEL[assignment_idx[found]]
    aoi_id[candidate_idx[found]] <- assignments$AOI_ID[assignment_idx[found]]
    status[candidate_idx[!found]] <- "invalid_image_geometry"
  }

  constructed <- list(
    vorogaze_source_row = source_row,
    which_aoi = which_aoi,
    aoi_id = aoi_id,
    aoi_assignment_status = status,
    fix_x_image = fix_x_image,
    fix_y_image = fix_y_image,
    aoi_definition_id = definition_id
  )

  appended <- aoi_export_append_constructed(raw, constructed)
  appended$status_counts <- as.data.frame(
    table(status, useNA = "ifany"),
    stringsAsFactors = FALSE
  )
  names(appended$status_counts) <- c("assignment_status", "fixation_rows")
  appended$definition_ids <- definition_ids
  appended
}

aoi_export_fixture_dir <- function(active_fixrep_path) {
  if (is.null(active_fixrep_path) || length(active_fixrep_path) != 1L) {
    return(NULL)
  }
  path <- normalizePath(active_fixrep_path, mustWork = FALSE)
  if (!identical(basename(path), "fixations_synthetic.csv")) {
    return(NULL)
  }
  fixture_dir <- dirname(path)
  required <- c(
    file.path(fixture_dir, "aoi_definitions.csv"),
    file.path(fixture_dir, "faces")
  )
  if (file.exists(required[[1]]) && dir.exists(required[[2]])) fixture_dir else NULL
}

aoi_export_load_prepared_fixture <- function(
    fixture_dir,
    fixrep,
    face_files,
    screen,
    image_origin,
    aoi_state
) {
  prepared <- read.csv(
    file.path(fixture_dir, "aoi_definitions.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  centres <- tibble::tibble(
    face_key = aoi_workbench_face_key(prepared$face_stimulus),
    face = prepared$face_stimulus,
    aoi_id = prepared$aoi_id,
    aoi_name = prepared$aoi_name,
    deldir_id = prepared$aoi_name,
    x = as.numeric(prepared$x),
    y = as.numeric(prepared$y)
  )

  dat <- as.data.frame(fixrep, stringsAsFactors = FALSE)
  scopes <- unique(data.frame(
    FACE = as.character(dat$FACE),
    CONDITION = as.character(dat$CONDITION),
    stringsAsFactors = FALSE
  ))
  all_assignments <- vector("list", nrow(scopes))
  all_defs <- vector("list", nrow(scopes))

  for (i in seq_len(nrow(scopes))) {
    face <- scopes$FACE[[i]]
    condition <- scopes$CONDITION[[i]]
    face_key <- aoi_workbench_face_key(face)
    commit_key <- paste(face_key, condition, sep = "|")
    subset <- dat[
      as.character(dat$FACE) == face &
        as.character(dat$CONDITION) == condition,
      ,
      drop = FALSE
    ]
    face_path <- find_face_file(face, face_files)
    if (is.null(face_path) || !file.exists(unname(face_path))) {
      stop("Prepared fixture face is unavailable: ", face, call. = FALSE)
    }

    info <- read_face_dimensions(unname(face_path))
    geometry <- aoi_workbench_image_geometry(
      fixrep = subset,
      width = info$width,
      height = info$height,
      screen = screen,
      image_origin = image_origin,
      use_screen_center = FALSE
    )
    if (!identical(geometry$status, "valid")) {
      stop("Prepared fixture geometry is invalid for ", face, call. = FALSE)
    }

    face_centres <- aoi_workbench_filter_centres(centres, face_key)
    defs <- aoi_workbench_aoi_defs(
      centres = centres,
      face_key = face_key,
      face = face,
      condition = condition,
      commit_key = commit_key
    )
    assigned <- aoi_workbench_assign_fixations(
      fixrep = subset,
      centres = face_centres,
      width = info$width,
      height = info$height,
      geometry = geometry
    )
    all_assignments[[i]] <- aoi_workbench_annotate_assignments(
      assignments = assigned,
      defs = defs,
      face_key = face_key,
      commit_key = commit_key
    )
    all_defs[[i]] <- defs
  }

  combined_assignments <- dplyr::bind_rows(all_assignments)
  combined_defs <- dplyr::bind_rows(all_defs)

  aoi_state$centres(centres)
  aoi_state$current_assignments(aoi_workbench_empty_assignments())
  aoi_state$current_defs(aoi_workbench_empty_defs())
  aoi_state$session_assignments(combined_assignments)
  aoi_state$session_defs(combined_defs)

  list(
    fixation_rows = nrow(dat),
    assigned_rows = nrow(combined_assignments),
    scopes = nrow(scopes),
    aoi_definitions = nrow(combined_defs)
  )
}

aoi_summary_make_key <- function(data, columns) {
  if (length(columns) == 0L) {
    return(rep("", nrow(data)))
  }
  values <- lapply(data[columns], function(x) {
    out <- as.character(x)
    out[is.na(out)] <- "<NA>"
    out
  })
  do.call(paste, c(values, sep = "\034"))
}

aoi_summary_scope_key <- function(face, condition) {
  paste(
    aoi_workbench_face_key(as.character(face)),
    as.character(condition),
    sep = "|"
  )
}

aoi_summary_measure_bases <- function(recipe, measures) {
  prefixes <- if (identical(recipe, "equal_trial")) {
    c(
      fixation_count = "mean_fixation_count_per_trial",
      summed_fixation_duration = "mean_summed_fixation_duration_ms_per_trial",
      mean_fixation_duration = "mean_of_trial_mean_fixation_duration_ms"
    )
  } else {
    c(
      fixation_count = "pooled_fixation_count",
      summed_fixation_duration = "pooled_summed_fixation_duration_ms",
      mean_fixation_duration = "pooled_mean_fixation_duration_ms"
    )
  }

  c(
    unname(prefixes[measures]),
    "eligible_trial_count",
    "aoi_hit_trial_count",
    "valid_duration_count"
  )
}

aoi_summary_configure_names <- function(config) {
  bases <- aoi_summary_measure_bases(config$recipe, config$measures)
  used <- unique(c(config$participant_col, config$group_cols, config$aoi_col))
  mapping <- aoi_export_resolve_names(used, bases)
  config$summary_name_mapping <- mapping
  config$summary_names <- stats::setNames(
    mapping$export_name,
    mapping$constructed_name
  )
  config
}

aoi_summary_calculate <- function(annotated, config, aoi_definitions) {
  mean_or_na <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    if (length(x) == 0L || all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  }
  sum_or_na <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    if (length(x) == 0L || all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
  }
  make_key <- function(data, columns) {
    if (length(columns) == 0L) return(rep("", nrow(data)))
    values <- lapply(data[columns], function(x) {
      out <- as.character(x)
      out[is.na(out)] <- "<NA>"
      out
    })
    do.call(paste, c(values, sep = "\034"))
  }
  face_key <- function(x) {
    out <- tolower(tools::file_path_sans_ext(basename(as.character(x))))
    out[is.na(x)] <- NA_character_
    out
  }
  unique_constructed <- function(base, used) {
    candidate <- base
    while (candidate %in% used) {
      candidate <- paste0(candidate, "_constructed_export_name")
    }
    candidate
  }
  long_to_wide <- function(data) {
    id_cols <- unique(c(config$participant_col, config$group_cols))
    aoi_col <- config$aoi_col
    value_cols <- setdiff(names(data), c(id_cols, aoi_col))
    ids <- unique(data[id_cols])
    ids$.wide_id <- make_key(ids, id_cols)
    data$.wide_id <- make_key(data, id_cols)
    aoi_values <- sort(unique(as.character(data[[aoi_col]])))
    sanitised <- tolower(gsub("[^A-Za-z0-9]+", "_", aoi_values))
    sanitised <- gsub("^_+|_+$", "", sanitised)
    sanitised[!nzchar(sanitised)] <- "aoi"
    sanitised <- make.unique(sanitised, sep = "_")
    used <- id_cols
    mapping_rows <- list()

    for (aoi_index in seq_along(aoi_values)) {
      aoi <- aoi_values[[aoi_index]]
      subset <- data[as.character(data[[aoi_col]]) == aoi, , drop = FALSE]
      hit <- match(ids$.wide_id, subset$.wide_id)
      for (value_col in value_cols) {
        base <- paste0(value_col, "__", sanitised[[aoi_index]])
        actual <- unique_constructed(base, used)
        ids[[actual]] <- subset[[value_col]][hit]
        used <- c(used, actual)
        mapping_rows[[length(mapping_rows) + 1L]] <- data.frame(
          which_aoi = aoi,
          value_name = value_col,
          constructed_name = base,
          export_name = actual,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      }
    }
    ids$.wide_id <- NULL
    attr(ids, "wide_name_mapping") <- do.call(rbind, mapping_rows)
    ids
  }

  required <- unique(c(
    config$participant_col,
    config$trial_key_cols,
    config$face_col,
    config$condition_col,
    config$duration_col,
    config$aoi_col,
    config$status_col,
    config$group_cols
  ))
  missing_columns <- setdiff(required, names(annotated))
  if (length(missing_columns) > 0L) {
    stop(
      "Annotated report is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  data <- as.data.frame(annotated, stringsAsFactors = FALSE, check.names = FALSE)
  data$.scope_key <- paste(
    face_key(data[[config$face_col]]),
    as.character(data[[config$condition_col]]),
    sep = "|"
  )
  data <- data[data$.scope_key %in% config$scope_keys, , drop = FALSE]
  if (nrow(data) == 0L) {
    stop("The selected completed scope contains no fixation rows.", call. = FALSE)
  }

  data$.trial_key <- make_key(data, config$trial_key_cols)
  meta_cols <- unique(c(
    config$participant_col,
    config$face_col,
    config$condition_col,
    config$group_cols
  ))
  trial_splits <- split(seq_len(nrow(data)), data$.trial_key)
  trial_rows <- vector("list", length(trial_splits))
  split_names <- names(trial_splits)

  for (i in seq_along(trial_splits)) {
    idx <- trial_splits[[i]]
    for (column in meta_cols) {
      values <- as.character(data[[column]][idx])
      values[is.na(values)] <- "<NA>"
      if (length(unique(values)) != 1L) {
        stop(
          "Trial key is ambiguous because ",
          column,
          " varies within trial ",
          split_names[[i]],
          ".",
          call. = FALSE
        )
      }
    }
    row <- data[idx[[1]], meta_cols, drop = FALSE]
    row$.trial_key <- split_names[[i]]
    trial_rows[[i]] <- row
  }
  trial_meta <- do.call(rbind, trial_rows)
  rownames(trial_meta) <- NULL

  defs <- as.data.frame(aoi_definitions, stringsAsFactors = FALSE)
  defs$.scope_key <- paste(
    face_key(defs$FACE),
    as.character(defs$CONDITION),
    sep = "|"
  )
  defs <- defs[defs$.scope_key %in% config$scope_keys, , drop = FALSE]
  defs <- unique(defs[c("FACE", "CONDITION", "AOI_LABEL", ".scope_key")])

  estimated_cells <- 0L
  grid_rows <- vector("list", nrow(trial_meta))
  for (i in seq_len(nrow(trial_meta))) {
    key <- paste(
      face_key(trial_meta[[config$face_col]][[i]]),
      as.character(trial_meta[[config$condition_col]][[i]]),
      sep = "|"
    )
    available <- defs[defs$.scope_key == key, , drop = FALSE]
    estimated_cells <- estimated_cells + nrow(available)
    if (estimated_cells > config$limits$intermediate_rows) {
      stop("The selected trial \u00d7 AOI grid exceeds the supported limit.", call. = FALSE)
    }
    if (nrow(available) == 0L) next
    row <- trial_meta[rep(i, nrow(available)), , drop = FALSE]
    row$.aoi <- as.character(available$AOI_LABEL)
    grid_rows[[i]] <- row
  }
  grid_rows <- grid_rows[!vapply(grid_rows, is.null, logical(1))]
  grid <- do.call(rbind, grid_rows)
  rownames(grid) <- NULL
  if (nrow(grid) == 0L) {
    stop("No AOIs are available in the selected completed scope.", call. = FALSE)
  }

  assigned <- data[
    as.character(data[[config$status_col]]) == "assigned" &
      !is.na(data[[config$aoi_col]]),
    ,
    drop = FALSE
  ]
  assigned$.cell_key <- paste(
    assigned$.trial_key,
    as.character(assigned[[config$aoi_col]]),
    sep = "\035"
  )
  grid$.cell_key <- paste(grid$.trial_key, grid$.aoi, sep = "\035")
  cell_splits <- split(seq_len(nrow(assigned)), assigned$.cell_key)
  cell_rows <- vector("list", length(cell_splits))

  if (length(cell_splits) > 0L) {
    for (i in seq_along(cell_splits)) {
      idx <- cell_splits[[i]]
      durations <- suppressWarnings(as.numeric(assigned[[config$duration_col]][idx]))
      valid_duration_count <- sum(!is.na(durations))
      cell_rows[[i]] <- data.frame(
        .cell_key = names(cell_splits)[[i]],
        .n_fix = length(idx),
        .sum_duration = if (valid_duration_count == 0L) NA_real_ else sum(durations, na.rm = TRUE),
        .mean_duration = if (valid_duration_count == 0L) NA_real_ else mean(durations, na.rm = TRUE),
        .valid_duration_count = valid_duration_count,
        stringsAsFactors = FALSE
      )
    }
    cells <- do.call(rbind, cell_rows)
    hit <- match(grid$.cell_key, cells$.cell_key)
    grid$.n_fix <- cells$.n_fix[hit]
    grid$.sum_duration <- cells$.sum_duration[hit]
    grid$.mean_duration <- cells$.mean_duration[hit]
    grid$.valid_duration_count <- cells$.valid_duration_count[hit]
  } else {
    grid$.n_fix <- NA_integer_
    grid$.sum_duration <- NA_real_
    grid$.mean_duration <- NA_real_
    grid$.valid_duration_count <- NA_integer_
  }

  empty <- is.na(grid$.n_fix)
  grid$.n_fix[empty] <- 0L
  grid$.sum_duration[empty] <- 0
  grid$.valid_duration_count[empty] <- 0L

  output_id_cols <- unique(c(config$participant_col, config$group_cols))
  grid$.output_key <- make_key(
    cbind(grid[output_id_cols], data.frame(.aoi = grid$.aoi, check.names = FALSE)),
    c(output_id_cols, ".aoi")
  )
  output_splits <- split(seq_len(nrow(grid)), grid$.output_key)
  output_rows <- vector("list", length(output_splits))
  names_map <- config$summary_names

  for (i in seq_along(output_splits)) {
    idx <- output_splits[[i]]
    row <- grid[idx[[1]], output_id_cols, drop = FALSE]
    row[[config$aoi_col]] <- grid$.aoi[idx[[1]]]
    hit_rows <- grid$.n_fix[idx] > 0L
    eligible_trial_count <- length(unique(grid$.trial_key[idx]))
    aoi_hit_trial_count <- length(unique(grid$.trial_key[idx[hit_rows]]))
    valid_duration_count <- sum(grid$.valid_duration_count[idx])

    if (identical(config$recipe, "equal_trial")) {
      denominator_rows <- if (identical(config$empty_aoi_denominator, "all_trials")) {
        rep(TRUE, length(idx))
      } else {
        hit_rows
      }
      count_value <- if (any(denominator_rows)) {
        mean(grid$.n_fix[idx][denominator_rows])
      } else {
        NA_real_
      }
      sum_value <- if (any(denominator_rows)) {
        mean_or_na(grid$.sum_duration[idx][denominator_rows])
      } else {
        NA_real_
      }
      mean_value <- mean_or_na(grid$.mean_duration[idx])
    } else {
      count_value <- sum(grid$.n_fix[idx])
      if (count_value == 0L) {
        sum_value <- 0
        mean_value <- NA_real_
      } else {
        sum_value <- sum_or_na(grid$.sum_duration[idx])
        mean_value <- if (valid_duration_count == 0L) {
          NA_real_
        } else {
          sum_value / valid_duration_count
        }
      }
    }

    if ("fixation_count" %in% config$measures) {
      base <- aoi_summary_measure_bases(config$recipe, "fixation_count")[[1]]
      row[[names_map[[base]]]] <- count_value
    }
    if ("summed_fixation_duration" %in% config$measures) {
      base <- aoi_summary_measure_bases(config$recipe, "summed_fixation_duration")[[1]]
      row[[names_map[[base]]]] <- sum_value
    }
    if ("mean_fixation_duration" %in% config$measures) {
      base <- aoi_summary_measure_bases(config$recipe, "mean_fixation_duration")[[1]]
      row[[names_map[[base]]]] <- mean_value
    }
    row[[names_map[["eligible_trial_count"]]]] <- eligible_trial_count
    row[[names_map[["aoi_hit_trial_count"]]]] <- aoi_hit_trial_count
    row[[names_map[["valid_duration_count"]]]] <- valid_duration_count
    output_rows[[i]] <- row
  }

  output <- do.call(rbind, output_rows)
  rownames(output) <- NULL
  if (nrow(output) > config$limits$result_rows) {
    stop("The participant summary exceeds the supported row limit.", call. = FALSE)
  }
  ordering <- do.call(
    order,
    lapply(output[c(output_id_cols, config$aoi_col)], function(x) as.character(x))
  )
  output <- output[ordering, , drop = FALSE]

  if (identical(config$layout, "wide")) {
    output <- long_to_wide(output)
    if (ncol(output) > config$limits$wide_columns) {
      stop("The participant summary exceeds the supported wide-column limit.", call. = FALSE)
    }
  }

  output
}

aoi_summary_script_text <- function(
    config,
    aoi_definitions,
    annotated_filename,
    expected_sha256,
    expected_rows
) {
  config_text <- capture.output(dput(config))
  defs_text <- capture.output(dput(as.data.frame(aoi_definitions, stringsAsFactors = FALSE)))
  measure_function_text <- deparse(aoi_summary_measure_bases, width.cutoff = 100L)
  function_text <- deparse(aoi_summary_calculate, width.cutoff = 100L)

  c(
    "# Reproduce the VoroGaze participant summary.",
    "# Place this script beside the separately downloaded annotated fixation report.",
    "",
    paste0("annotated_filename <- ", deparse(annotated_filename)),
    paste0("expected_sha256 <- ", deparse(expected_sha256)),
    paste0("expected_rows <- ", as.integer(expected_rows), "L"),
    "config <- ",
    config_text,
    "aoi_definitions <- ",
    defs_text,
    "",
    "aoi_summary_measure_bases <-",
    measure_function_text,
    "",
    "aoi_summary_calculate <-",
    function_text,
    "",
    "args <- commandArgs(trailingOnly = TRUE)",
    "annotated_path <- if (length(args) > 0L) args[[1]] else annotated_filename",
    "if (!file.exists(annotated_path)) {",
    "  stop(\"Annotated fixation report not found: \", annotated_path, call. = FALSE)",
    "}",
    "if (requireNamespace(\"digest\", quietly = TRUE)) {",
    "  actual_sha256 <- digest::digest(annotated_path, algo = \"sha256\", file = TRUE)",
    "  if (!identical(actual_sha256, expected_sha256)) {",
    "    stop(\"Annotated report SHA-256 does not match this specification.\", call. = FALSE)",
    "  }",
    "} else {",
    "  warning(\"Install the digest package to verify the annotated report SHA-256.\", call. = FALSE)",
    "}",
    "annotated <- read.csv(annotated_path, stringsAsFactors = FALSE, check.names = FALSE)",
    "if (nrow(annotated) != expected_rows) {",
    "  stop(\"Annotated report row count does not match this specification.\", call. = FALSE)",
    "}",
    "participant_summary <- aoi_summary_calculate(annotated, config, aoi_definitions)",
    "write.csv(",
    "  participant_summary,",
    "  \"participant_summary_reproduced.csv\",",
    "  row.names = FALSE,",
    "  na = \"\"",
    ")",
    "message(\"Wrote participant_summary_reproduced.csv\")"
  )
}

aoi_summary_write_bundle <- function(
    file,
    specification,
    summary,
    config,
    aoi_definitions,
    annotated_filename,
    expected_sha256,
    expected_rows
) {
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("The writexl package is required to create the summary workbook.", call. = FALSE)
  }

  bundle_dir <- tempfile("vorogaze-summary-")
  dir.create(bundle_dir, recursive = TRUE)
  on.exit(unlink(bundle_dir, recursive = TRUE, force = TRUE), add = TRUE)

  workbook_path <- file.path(bundle_dir, "participant_summary.xlsx")
  script_path <- file.path(bundle_dir, "reproduce_participant_summary.R")

  writexl::write_xlsx(
    list(
      aggregation_spec = specification,
      participant_summary = as.data.frame(
        summary,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    ),
    path = workbook_path
  )
  writeLines(
    aoi_summary_script_text(
      config = config,
      aoi_definitions = aoi_definitions,
      annotated_filename = annotated_filename,
      expected_sha256 = expected_sha256,
      expected_rows = expected_rows
    ),
    script_path,
    useBytes = TRUE
  )

  zip::zipr(
    zipfile = normalizePath(file, mustWork = FALSE),
    files = c("participant_summary.xlsx", "reproduce_participant_summary.R"),
    root = bundle_dir
  )

  invisible(file)
}
