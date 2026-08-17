VOROGAZE_MAX_REQUEST_BYTES <- 60 * 1024^2

# Reads a conventional true/false environment flag.
vorogaze_env_flag <- function(name, default = FALSE) {
  value <- trimws(tolower(Sys.getenv(name, unset = "")))

  if (!nzchar(value)) {
    return(isTRUE(default))
  }

  value %in% c("1", "true", "yes", "on")
}

# Public mode enables the bounded, session-only upload boundary.
vorogaze_public_mode <- function() {
  vorogaze_env_flag("VOROGAZE_PUBLIC_MODE", default = FALSE)
}

# Public deployments return generic errors; local development keeps full errors.

# The internal app keeps its Developer tab; public images explicitly disable it.
vorogaze_developer_enabled <- function() {
  configured <- Sys.getenv("VOROGAZE_ENABLE_DEVELOPER", unset = "")

  if (nzchar(configured)) {
    return(vorogaze_env_flag("VOROGAZE_ENABLE_DEVELOPER"))
  }

  !vorogaze_public_mode()
}

# Returns the fallback value when the first value is NULL.
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Draws a simple message inside a plot area when there is nothing to plot.
plot_message <- function(title, subtitle = NULL, title_cex = 1.2) {
  plot.new()
  box()
  text(
    x = 0.5,
    y = 0.55,
    labels = title,
    cex = title_cex,
    font = 2
  )
  
  if (!is.null(subtitle)) {
    text(
      x = 0.5,
      y = 0.45,
      labels = subtitle,
      cex = 1
    )
  }
  
  invisible(NULL)
}

# Returns the package-owned browser assets.
vorogaze_assets <- function() {
  htmltools::htmlDependency(
    name = "vorogaze",
    version = "0.1.0",
    src = c(file = system.file("www", package = "VoroGaze", mustWork = TRUE)),
    stylesheet = c("styles.css", "public-workbench.css")
  )
}

# Builds the full public Research Workbench UI.
public_workbench_ui <- function() {
  bslib::page_fillable(
    title = "VoroGaze Research Workbench",
    theme = app_theme(),
    vorogaze_assets(),
    public_workbench_notice(),
    bslib::navset_card_pill(
      aoi_demo_panel(),
      fixations_panel(),
      screen_panel(),
      faces_panel(),
      sanity_panel(),
      aoi_workbench_panel(),
      aoi_exports_panel()
    )
  )
}

# Constructs the package-local Shiny application object.
vorogaze_app <- function() {
  shiny::shinyApp(ui = public_workbench_ui(), server = vorogaze_server)
}
