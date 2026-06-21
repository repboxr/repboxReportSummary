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

# Map issue category strings consistently to short failure categories
sr_issue_to_failure_cat = function(cat) {
  restore.point("sr_issue_to_failure_cat")
  if (is.null(cat) || length(cat) == 0) return(character(0))
  cat[is.na(cat)] = ""

  failure_cat = rep("", length(cat))
  failure_cat[grepl("sb_raw, sb and rb failed", cat)] = "sb_raw-sb-rb"
  failure_cat[grepl("sb and rb failed", cat) & failure_cat == ""] = "sb-rb"
  failure_cat[grepl("rb failed", cat) & failure_cat == ""] = "rb"
  failure_cat[grepl("coefs don't match|coeffs don't match", cat, ignore.case = TRUE) & failure_cat == ""] = "coefs"
  failure_cat[grepl("sb and so coefs", cat) & failure_cat == ""] = "so"

  failure_cat
}

# Match issues dataframe against known xissues dataframe
sr_match_xissues = function(df_issues, xi_df) {
  restore.point("sr_match_xissues")
  res_xid = rep("", nrow(df_issues))
  if (NROW(xi_df) == 0 || NROW(df_issues) == 0) return(res_xid)

  df_fail_cat = sr_issue_to_failure_cat(df_issues$issue_category)

  df_cmd = stringi::stri_trim_both(as.character(df_issues$cmd))
  df_cmd[is.na(df_cmd)] = ""

  df_cmdline = as.character(df_issues$cmdline)
  df_cmdline[is.na(df_cmdline)] = ""

  for (i in seq_len(nrow(xi_df))) {
    xi = xi_df[i, ]
    if (is.null(xi$xid) || is.na(xi$xid) || !nzchar(xi$xid)) next

    # 1. Failure mode match
    match_mask = (df_fail_cat == xi$failure_cat)

    # 2. Cmd match (if specified)
    if (!is.null(xi$cmd) && nzchar(xi$cmd)) {
      xi_cmds = stringi::stri_split_fixed(xi$cmd, ",")[[1]]
      xi_cmds = stringi::stri_trim_both(xi_cmds)
      xi_cmds = xi_cmds[nzchar(xi_cmds)]
      if (length(xi_cmds) > 0) {
        match_mask = match_mask & (df_cmd %in% xi_cmds)
      }
    }

    # 3. Fixed pattern match (if specified)
    if (!is.null(xi$fixed_pattern) && nzchar(xi$fixed_pattern)) {
      rx = fixed_terms_to_regex(xi$fixed_pattern, space_to_ws = TRUE)
      match_mask = match_mask & stringi::stri_detect_regex(df_cmdline, rx)
    }

    # 4. Regex pattern match (if specified)
    if (!is.null(xi$rx_pattern) && nzchar(xi$rx_pattern)) {
      match_mask = match_mask & stringi::stri_detect_regex(df_cmdline, xi$rx_pattern)
    }

    # Update matches that have not been assigned an xid yet (first match wins)
    update_idx = match_mask & (res_xid == "")
    res_xid[update_idx] = xi$xid
  }

  res_xid
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

  failure_cat = sr_issue_to_failure_cat(cat)

  artid = project
  if (is.null(artid)) artid = ""

  li = list(
    xid = "",
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
