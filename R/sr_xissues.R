# UI pane for XIssues
sr_xissue_pane_ui = function() {
  shiny::tabPanel("xissues",
    shiny::fluidRow(
      shiny::column(12,
        shiny::textAreaInput("xissue_yaml_text", "YAML Text", value = "", rows = 12, width = "100%"),
        shiny::actionButton("btn_add_xissue", "Add Xissue", class = "btn-primary"),
        shiny::hr(),
        shiny::div(class = "table-wrapper", DT::DTOutput("xissues_table"))
      )
    )
  )
}

# Create a YAML template from an issue
sr_make_xissue_template = function(issue, project) {
  cmd = if (!is.null(issue$cmd)) issue$cmd else ""
  if (length(cmd) > 1) cmd = cmd[1]
  if (is.na(cmd)) cmd = ""

  pid = if (!is.null(issue$runid)) issue$runid else issue$pid
  if (is.null(pid)) pid = ""
  if (length(pid) > 1) pid = pid[1]
  if (is.na(pid)) pid = ""

  cat = issue$issue_category
  if (is.null(cat)) cat = ""
  if (is.na(cat)) cat = ""

  failure_cat = ""
  if (grepl("sb_raw, sb and rb failed", cat)) failure_cat = "sb_raw-sb-rb"
  else if (grepl("sb and rb failed", cat)) failure_cat = "sb-rb"
  else if (grepl("rb failed", cat)) failure_cat = "rb"
  else if (grepl("coefs don't match|coeffs don't match", cat, ignore.case = TRUE)) failure_cat = "coefs"
  else if (grepl("sb and so coefs", cat)) failure_cat = "so"

  artid = project
  if (is.null(artid)) artid = ""

  li = list(
    where = "reg",
    cmd = as.character(cmd),
    fixed_pattern = "",
    failure_cat = failure_cat,
    descr = "",
    artids = as.character(artid),
    pids = as.character(pid),
    time = format(Sys.time()),
    rx_pattern = ""
  )

  yaml::as.yaml(li)
}
