#' Run the Repbox Debug Report addin
#'
#' Opens the Repbox Debug Report Shiny application in the RStudio Viewer pane.
#'
#' @return Invisibly returns the result of [run_sr_app()].
#' @export
repbox_debug_report_addin = function() {
  if (!rstudioapi::isAvailable()) {
    stop("The Repbox Debug Report addin must be run inside RStudio.")
  }

  parent_dir = getOption(
    "repboxReportSummary.parent_dir",
    "/home/rstudio/repbox/projects"
  )

  invisible(run_sr_app(
    parent_dir = parent_dir,
    in_viewer = TRUE,
    as_job = FALSE
  ))
}
