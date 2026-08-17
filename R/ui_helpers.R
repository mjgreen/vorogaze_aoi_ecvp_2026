# UI helper functions ----

# Builds the Bootstrap theme used by every page in the app.
app_theme <- function() {
  bslib::bs_theme(
    version = 5,
    base_font_size = "0.9rem",
    "input-font-size" = "0.8rem",
    "input-padding-y" = "0.15rem",
    "input-padding-x" = "0.4rem"
  )
}

# Adds app-wide browser assets, currently just the stylesheet in www/.
app_head <- function() {
  shiny::tags$head(
    shiny::tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "styles.css"
    )
  )
}

# Wraps a plot and hover label in the positioned container Shiny needs.
hover_plot_box <- function(plot_id, hover_id, label_id, style = NULL, label_style = NULL) {
  shiny::div(
    style = style %||% "position: relative; width: 100%;",
    shiny::plotOutput(
      plot_id,
      hover = shiny::hoverOpts(
        id = hover_id,
        delay = 0,
        delayType = "throttle",
        clip = TRUE
      )
    ),
    shiny::uiOutput(label_id, style = label_style)
  )
}

# Creates the upload and mapping controls for the fixation report tab.
fixations_input_card <- function() {
  bslib::card(
    bslib::card_header("input"),
    bslib::card_body(
      shiny::div(
        class = "fixrep-upload-box",
        shiny::downloadButton(
          "download_bundled_fixrep",
          if (vorogaze_public_mode()) {
            "Download bundled DeBruine et al fixation report"
          } else {
            "Download bundled fixation report"
          }
        ),
        shiny::fileInput(
          "upload_fixrep",
          "Upload fixation report",
          accept = c(".csv", ".tsv", ".txt", ".xls", ".xlsx"),
          width = "100%"
        ),
        shiny::div(
          class = "loaded-file-box",
          shiny::span(class = "loaded-file-label", "Fixation report:"),
          shiny::textOutput("fixrep_file_display", inline = TRUE)
        )
      ),
      shiny::div(
        class = "fixrep-mapping-ui",
        shiny::uiOutput("fixrep_mapping_ui")
      ),
      shiny::uiOutput("fixrep_validation_summary")
    )
  )
}

# Creates one compact DT card for either raw or processed fixation data.
fixrep_preview_card <- function(title, output_id) {
  bslib::card(
    full_screen = TRUE,
    bslib::card_header(title),
    bslib::card_body(
      shiny::div(
        class = "dt-table-output",
        DT::DTOutput(output_id)
      )
    )
  )
}

standardised_fixrep_tabs <- function() {
  bslib::navset_card_tab(
    full_screen = TRUE,
    title = "Standardised fixation report",
    bslib::nav_panel(
      "Table",
      shiny::div(
        class = "dt-table-output",
        DT::DTOutput("fixrep_preview")
      )
    ),
    bslib::nav_panel(
      "Summary",
      shiny::div(
        class = "dt-table-output fixrep-summary-output",
        DT::DTOutput("fixrep_summary")
      )
    )
  )
}

# Assembles the fixation report upload and preview tab.
fixations_panel <- function() {
  bslib::nav_panel(
    "Fixations",
    bslib::layout_columns(
      col_widths = c(4, 8),
      height = "100%",
      fixations_input_card(),
      bslib::layout_columns(
        col_widths = 12,
        height = "100%",
        fixrep_preview_card("Raw view of the uploaded fixation report", "fixrep_raw_preview"),
        standardised_fixrep_tabs()
      )
    )
  )
}

# Creates the four numeric screen boundary inputs.
# These custom bounds currently assume y increases downward: top must be less than bottom.
screen_boundary_inputs <- function() {
  shiny::tagList(
    shiny::numericInput("screen_left", "Screen left", value = 0, step = 1),
    shiny::numericInput("screen_right", "Screen right", value = 1600, step = 1),
    shiny::numericInput("screen_top", "Screen top", value = 0, step = 1),
    shiny::numericInput("screen_bottom", "Screen bottom", value = 900, step = 1)
  )
}

# Creates the screen dimension preset selector.
screen_dimension_input <- function() {
  shiny::selectInput(
    "screen_dimensions",
    "Screen dimensions",
    choices = c(
      "1600 x 900" = "1600x900",
      "1920 x 1280" = "1920x1280",
      "Other" = "other"
    ),
    selected = "1600x900"
  )
}

# Assembles the screen controls and coordinate preview plot.
screen_panel <- function() {
  bslib::nav_panel(
    "Screen",
    bslib::layout_columns(
      col_widths = c(4, 8),
      bslib::card(
        bslib::card_header("input"),
        bslib::card_body(
          origin_radio_buttons("screen_origin", "Screen origin", selected = "top_left"),
          screen_dimension_input(),
          shiny::conditionalPanel(
            condition = "input.screen_dimensions == 'other'",
            screen_boundary_inputs()
          ),
          shiny::uiOutput("screen_bounds_display")
        )
      ),
      bslib::card(
        bslib::card_header("view"),
        bslib::card_body(
          hover_plot_box("view_screen", "view_screen_hover", "view_screen_hover_label")
        )
      )
    )
  )
}

# Creates a shared origin selector for screen and image coordinate systems.
origin_radio_buttons <- function(input_id, label, selected) {
  shiny::radioButtons(
    input_id,
    label,
    choices = c(
      "Top left" = "top_left",
      "Centre" = "center",
      "Other" = "other"
    ),
    selected = selected
  )
}

# Assembles the face controls and image preview plot.
faces_panel <- function() {
  bslib::nav_panel(
    "Faces",
    bslib::layout_columns(
      col_widths = c(4, 8),
      bslib::card(
        bslib::card_header("input"),
        bslib::card_body(
          shiny::downloadButton(
            "download_bundled_face_dir",
            if (vorogaze_public_mode()) {
              "Download bundled DeBruine et al face"
            } else {
              "Download bundled face folder"
            }
          ),
          shiny::fileInput(
            "upload_face_dir",
            "Browse face directory",
            multiple = TRUE,
            accept = c(".png", ".jpg", ".jpeg"),
            width = "100%",
            buttonLabel = "Browse...",
            placeholder = "No directory selected"
          ),
          shiny::tags$script(
            shiny::HTML(
              paste(
                "document.addEventListener('DOMContentLoaded', function() {",
                "  var input = document.getElementById('upload_face_dir');",
                "  if (input) {",
                "    input.setAttribute('webkitdirectory', '');",
                "    input.setAttribute('directory', '');",
                "  }",
                "});"
              )
            )
          ),
          shiny::div(
            class = "loaded-file-box",
            shiny::span(class = "loaded-file-label", "Face directory:"),
            shiny::textOutput("face_dir_display", inline = TRUE)
          ),
          shiny::uiOutput("face_file_ui"),
          origin_radio_buttons("image_origin", "Image origin", selected = "center")
        )
      ),
      bslib::card(
        bslib::card_header("view"),
        bslib::card_body(shiny::plotOutput("view_face"))
      )
    )
  )
}

# Assembles the sanity-check filters and combined image/fixation preview.
sanity_panel <- function() {
  bslib::nav_panel(
    "Sanity",
    bslib::layout_columns(
      col_widths = c(4, 8),
      bslib::card(
        bslib::card_header("verify params"),
        bslib::card_body(shiny::uiOutput("sanity_filter_ui"))
      ),
      bslib::card(
        bslib::card_header("view everything"),
        bslib::card_body(
          hover_plot_box(
            "view_sanity",
            "view_sanity_hover",
            "view_sanity_hover_label",
            style = "position: relative; width: 100%; padding: 0; margin: 0;",
            label_style = paste(
              "position: absolute;",
              "left: 0;",
              "top: 0;",
              "width: 0;",
              "height: 0;",
              "overflow: visible;",
              "pointer-events: none;"
            )
          )
        )
      )
    )
  )
}

# Shows the upload boundary and privacy guidance in the public workbench.
public_workbench_notice <- function() {
  shiny::div(
    class = "public-workbench-notice",
    shiny::tags$strong("Temporary uploads"),
    shiny::span(
      paste(
        "Files are processed only for this browser session and are removed when it ends.",
        "Do not upload identifiable or sensitive participant data.",
        "Limits: one fixation report up to 25 MiB; up to 25 PNG/JPEG faces,",
        "5 MiB each and 50 MiB total, with a maximum size of 4096 x 4096 pixels."
      )
    )
  )
}
