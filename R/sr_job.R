run_sr_as_job = function(
    parent_dir = NULL,
    project_dirs = NULL,
    in_viewer = FALSE,
    output_dir = "/home/rstudio/repbox/reports",
    job_name = "repbox report summary",
    ...) {

  app_args = c(
    list(
      parent_dir = parent_dir,
      project_dirs = project_dirs,
      in_viewer = in_viewer,
      as_job = FALSE,
      output_dir = output_dir,
      job_name = job_name
    ),
    list(...)
  )

  run_shiny_app_as_job(
    app_id = "repbox-report-summary",
    fun = "run_sr_app",
    args = app_args,
    package = "repboxReportSummary",
    output_dir = output_dir,
    job_name = job_name,
    working_dir = getwd(),
    replace = TRUE
  )
}




# Optional manual controls:

stop_sr_app = function(
    output_dir = "/home/rstudio/repbox/reports",
    job_name = "repbox report summary"
) {
  stop_shiny_app_job(
    app_id = "repbox-report-summary",
    output_dir = output_dir,
    job_name = job_name
  )
}


sr_app_job_status = function(
    output_dir = "/home/rstudio/repbox/reports"
) {
  shiny_app_job_status(
    app_id = "repbox-report-summary",
    output_dir = output_dir
  )
}
