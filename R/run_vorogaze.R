#' Run the VoroGaze Research Workbench
#'
#' Starts the complete workbench as a local Shiny application. The default
#' address is available only from the computer on which it is running.
#'
#' @param host Character scalar passed to [shiny::runApp()].
#' @param port Integer TCP port passed to [shiny::runApp()].
#' @param launch.browser Whether to open the app in a browser.
#'
#' @return This function does not return until the Shiny application stops.
#' @export
run_vorogaze <- function(
    host = "127.0.0.1",
    port = getOption("shiny.port", 3838L),
    launch.browser = interactive()
) {
  stopifnot(is.character(host), length(host) == 1L, !is.na(host), nzchar(host))
  port <- as.integer(port)
  stopifnot(length(port) == 1L, !is.na(port), port >= 1L, port <= 65535L)

  fixture_dir <- system.file("extdata", "worked_example", package = "VoroGaze", mustWork = TRUE)
  fixture_csv <- file.path(fixture_dir, "fixrep_demo_synthetic.csv")
  face_dir <- file.path(fixture_dir, "faces")
  worker <- system.file(
    "workers",
    "public_face_normalise_worker.R",
    package = "VoroGaze",
    mustWork = TRUE
  )

  environment_names <- c(
    "VOROGAZE_PUBLIC_MODE",
    "VOROGAZE_ENABLE_DEVELOPER",
    "VOROGAZE_ENABLE_SUMMARY",
    "VOROGAZE_RELEASE_REVISION",
    "VOROGAZE_EXAMPLE_FIXTURE_DIR",
    "VOROGAZE_DEFAULT_FIXREP",
    "VOROGAZE_BUNDLED_FACE_DIR",
    "VOROGAZE_PUBLIC_FACE_NORMALISER"
  )
  old_environment <- Sys.getenv(environment_names, unset = NA_character_)
  old_options <- options(
    shiny.maxRequestSize = VOROGAZE_MAX_REQUEST_BYTES,
    shiny.sanitize.errors = TRUE
  )

  on.exit({
    options(old_options)
    present <- !is.na(old_environment)
    if (any(present)) {
      do.call(Sys.setenv, as.list(old_environment[present]))
    }
    Sys.unsetenv(environment_names[!present])
  }, add = TRUE)

  configured_environment <- c(
    VOROGAZE_PUBLIC_MODE = "true",
    VOROGAZE_ENABLE_DEVELOPER = "false",
    VOROGAZE_ENABLE_SUMMARY = "true",
    VOROGAZE_RELEASE_REVISION = as.character(utils::packageVersion("VoroGaze")),
    VOROGAZE_EXAMPLE_FIXTURE_DIR = fixture_dir,
    VOROGAZE_DEFAULT_FIXREP = fixture_csv,
    VOROGAZE_BUNDLED_FACE_DIR = face_dir,
    VOROGAZE_PUBLIC_FACE_NORMALISER = worker
  )
  do.call(Sys.setenv, as.list(configured_environment))

  shiny::runApp(
    appDir = vorogaze_app(),
    host = host,
    port = port,
    launch.browser = launch.browser
  )
}
