example = function() {
  library(repboxReportSummary)
  parent_dir = "/home/rstudio/repbox/projects"
  sr_summary_report(parent_dir = parent_dir)
  report_file = "/home/rstudio/repbox/reports/projects_summary.html"
  browseURL(report_file)

}

