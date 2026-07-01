Here are the modifications to `repboxReportSummary` to properly show timeout problems in the main issues list.

We will inject any detected `TIMEOUT` issues directly into the `sr_get_project_issues` output, prefixing the `issue_category` with `"TIMEOUT: "` and appending the timeout messages to the `problem` details column so they show up when the row is selected in the UI. We also adapt `xissues` so that you can categorize timeouts appropriately.

!MODIFICATION sr_get_project_issues repboxReportSummary/R/sr_data.R
scope = "function"
file = "/home/rstudio/repbox/repboxReportSummary/R/sr_data.R"
function_name = "sr_get_project_issues"
description = "Update `sr_get_project_issues` to detect timeout problems and prepend 'TIMEOUT: ' to issue_category, or create a new timeout row if no other issues exist."
---
```r
#' Get and categorize issues for a single project based on fine-tuned categories
sr_get_project_issues = function(project_dir, parcels=list()) {
  restore.point("sr_get_project_issues")
  library(repboxDB)

  parcels = repdb_load_parcels(project_dir,c("regcheck", "reg"))
  regcheck = parcels$regcheck
  reg = parcels$reg
  
  df_rc = NULL
  
  if (!is.null(regcheck) && NROW(regcheck) > 0) {
    df_rc = as.data.frame(regcheck)
    if (!is.null(reg) && NROW(reg) > 0) {
      df_rc = dplyr::left_join(df_rc, reg %>% dplyr::select(runid, cmd, cmdline), by="runid")
    } else {
      df_rc$cmd = NA_character_
      df_rc$cmdline = NA_character_
    }
    df_rc = df_rc %>% dplyr::mutate(line=NA_integer_, file_path=NA_character_)

    # Check if run worked
    reg_ok = sr_v_bool(df_rc$reg_ok, FALSE)

    # Identify variables safely
    so_exists = sr_v_bool(df_rc$so_raw_did_run, FALSE) | sr_v_bool(df_rc$so_did_run, FALSE)
    sb_raw_exists = sr_v_bool(df_rc$sb_raw_did_run, FALSE)
    sb_failed = !sr_v_bool(df_rc$sb_did_run, FALSE)
    rb_failed = !sr_v_bool(df_rc$rb_did_run, FALSE)

    sb_so_diff = !sr_v_bool(df_rc$sb_so_identical, TRUE)
    share_same = sr_v_num(df_rc$rb_sb_share_coeff_same, 1)
    coef_mismatch_share = 1 - share_same

    cmd_col = tolower(as.character(df_rc$cmd))
    cmd_col[is.na(cmd_col)] = ""
    is_logit_probit = grepl("logit|probit", cmd_col)

    cat = rep("8. Other issues", NROW(df_rc))

    # 1. sb_raw, sb and rb failed but so exists
    mask1 = !sb_raw_exists & sb_failed & rb_failed & so_exists
    # 2. sb and rb failed but sb_raw exists
    mask2 = sb_raw_exists & sb_failed & rb_failed
    # 3. rb failed
    mask3 = !mask1 & !mask2 & rb_failed
    # 4. > 20% coeffs don't match between sb and rb and not a logit or probit command
    mask4 = !rb_failed & (coef_mismatch_share > 0.2) & !is_logit_probit
    # 5. sb and so coefs don't match
    mask5 = !rb_failed & !mask4 & sb_so_diff
    # 6. < 20% coeffs don't match (i.e. > 0 and <= 0.2)
    mask6 = !rb_failed & !mask4 & !mask5 & (coef_mismatch_share > 0 & coef_mismatch_share <= 0.2) & !is_logit_probit
    # 7. coefs don't match but logit or probit
    mask7 = !rb_failed & (coef_mismatch_share > 0) & is_logit_probit

    # Apply masks to category (later masks overwrite earlier ones if overlapping, though constructed to be mostly exclusive)
    cat[mask7] = "7. Coefs don't match but logit/probit"
    cat[mask6] = "6. < 20% coeffs don't match"
    cat[mask5] = "5. sb and so coefs don't match"
    cat[mask4] = "4. > 20% coeffs don't match (not logit/probit)"
    cat[mask3] = "3. rb failed"
    cat[mask2] = "2. sb and rb failed but sb_raw exists"
    cat[mask1] = "1. sb_raw, sb and rb failed but so exists"

    df_rc$issue_category = cat
    
    # Filter only actual issues
    is_issue = !reg_ok | (cat != "8. Other issues") | (!is.na(df_rc$problem) & nzchar(as.character(df_rc$problem)))
    df_rc = df_rc[is_issue, , drop = FALSE]
  }

  # Add Timeouts
  probs = sr_get_project_problems(project_dir)
  timeout_probs = data.frame()
  if (!is.null(probs) && NROW(probs) > 0) {
    timeout_probs = probs[probs$problem_type %in% c("stata_reproduction_timeout", "stata_reproduction_global_timeout", "mrb_stata_timeout"), , drop = FALSE]
  }
  
  if (NROW(timeout_probs) > 0) {
    timeout_msgs = paste0(timeout_probs$problem_descr, collapse = "\n\n")
    
    if (!is.null(df_rc) && NROW(df_rc) > 0) {
      df_rc$issue_category = paste0("TIMEOUT: ", df_rc$issue_category)
      if (!"problem" %in% names(df_rc)) df_rc$problem = ""
      df_rc$problem = paste0(ifelse(is.na(df_rc$problem) | df_rc$problem == "", "", paste0(df_rc$problem, "\n\n")), "TIMEOUT DETAILS:\n", timeout_msgs)
    } else {
      df_rc = data.frame(
        runid = NA_integer_,
        cmd = "TIMEOUT",
        cmdline = NA_character_,
        issue_category = "TIMEOUT",
        problem = paste0("TIMEOUT DETAILS:\n", timeout_msgs),
        stringsAsFactors = FALSE
      )
    }
  }

  if (is.null(df_rc) || NROW(df_rc) == 0) return(NULL)

  if (!"artid" %in% names(df_rc)) {
    df_rc$artid = basename(project_dir)
  }
  df_rc$project_dir = project_dir

  df_rc
}
```
!END_MODIFICATION


!MODIFICATION xissue_failure_cats repboxReportSummary/R/xissues.R
scope = "function"
file = "/home/rstudio/repbox/repboxReportSummary/R/xissues.R"
function_name = "xissue_failure_cats"
description = "Add 'timeout' to xissue failure categories."
---
```r
xissue_failure_cats = function() {
  c("rb", "sb-rb", "sb_raw-sb-rb", "coefs","so", "timeout", "")
}
```
!END_MODIFICATION


!MODIFICATION sr_issue_to_failure_cat repboxReportSummary/R/sr_xissues.R
scope = "function"
file = "/home/rstudio/repbox/repboxReportSummary/R/sr_xissues.R"
function_name = "sr_issue_to_failure_cat"
description = "Handle 'TIMEOUT' in issue category."
---
```r
# Map issue category strings consistently to short failure categories
sr_issue_to_failure_cat = function(cat) {
  restore.point("sr_issue_to_failure_cat")
  if (is.null(cat) || length(cat) == 0) return(character(0))
  cat[is.na(cat)] = ""

  failure_cat = rep("", length(cat))
  failure_cat[grepl("TIMEOUT", cat)] = "timeout"
  failure_cat[grepl("sb_raw, sb and rb failed", cat) & failure_cat == ""] = "sb_raw-sb-rb"
  failure_cat[grepl("sb and rb failed", cat) & failure_cat == ""] = "sb-rb"
  failure_cat[grepl("rb failed", cat) & failure_cat == ""] = "rb"
  failure_cat[grepl("coefs don't match|coeffs don't match", cat, ignore.case = TRUE) & failure_cat == ""] = "coefs"
  failure_cat[grepl("sb and so coefs", cat) & failure_cat == ""] = "so"

  failure_cat
}
```
!END_MODIFICATION


!MODIFICATION detail_table_rendering repboxReportSummary/R/sr_app.R
scope = "function"
file = "/home/rstudio/repbox/repboxReportSummary/R/sr_app.R"
function_name = "sr_app"
description = "Update the detail table rendering in `sr_app` to properly match failure categories when prefixed with TIMEOUT."
---
```r
#' Run the interactive Repbox Debugger Shiny App
#'
#' @param project_dirs A vector of full paths to repbox project directories.
#' @param parent_dir Optional parent directory containing project subdirectories.
#' @param output_dir Output directory for any temporary files (not strictly required for Shiny view).
#' @export
sr_app = function(
  parent_dir = NULL,
  project_dirs = NULL,
  output_dir = "/home/rstudio/repbox/reports",
  ...
) {
  library(repboxReportSummary)

  if (!is.null(parent_dir)) {
    project_dirs = union(project_dirs, list.dirs(parent_dir, full.names = TRUE, recursive = FALSE))
  }

  if (length(project_dirs) == 0) {
    stop("No project directories provided or found.")
  }

  message("Scanning projects for regcheck issues and repbox_problems... (This may take a moment)")

  df_issues = sr_aggregate_issues(project_dirs)
  df_probs = sr_aggregate_problems(project_dirs)

  # Safety fallback to ensure cmd is available for summarize logic
  if (NROW(df_issues) > 0 && !"cmd" %in% names(df_issues)) {
    df_issues$cmd = "unknown"
  }

  message("Data loaded. Launching Shiny app...")

  ui = shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        body { padding: 5px; }
        .nav-tabs > li > a { padding: 2px 4px; font-size: 12px; }
        .dataTables_wrapper { font-size: 12px; }
        .dataTables_filter { margin-bottom: 4px; float: none !important; text-align: left; }
        .dataTables_filter input { padding: 2px; height: auto; }
        table.dataTable thead th {
          position: sticky;
          top: 0;
          background-color: #f8f9fa !important;
          z-index: 10;
          box-shadow: inset 0 -1px 0 #ddd;
          padding: 4px 8px;
        }
        table.dataTable tbody td { padding: 4px 8px; }
        .btn-sm { padding: 2px 6px; font-size: 12px; }
        .container-fluid { padding-left: 5px; padding-right: 5px; }
        .col-sm-12 { padding-left: 0; padding-right: 0; }
        .table-wrapper { max-height: 60vh; overflow: auto; border-bottom: 1px solid #ddd; margin-bottom: 10px; padding-left: 4px }
      "))
    ),
    shiny::tabsetPanel(
      # TAB 1: Regcheck Issues
      shiny::tabPanel("Regcheck Issues",
        shiny::fluidRow(
          shiny::column(12,
            shiny::div(class = "table-wrapper", DT::DTOutput("summary_table")),

            shiny::uiOutput("project_actions"),
            shiny::div(class = "table-wrapper", DT::DTOutput("detail_table"))
          )
        )
      ),

      # TAB 2: Repbox Problems
      shiny::tabPanel("Repbox Problems",
        shiny::fluidRow(
          shiny::column(12,
            shiny::div(class = "table-wrapper", DT::DTOutput("prob_summary_table"))
          )
        ),
        shiny::fluidRow(
          shiny::column(12,
            shiny::uiOutput("prob_project_actions"),
            shiny::div(class = "table-wrapper", DT::DTOutput("prob_detail_table"))
          )
        )
      ),

      # TAB 3: XIssues
      sr_xissue_pane_ui()
    )
  )

  server = function(input, output, session) {

    # Load Initial XIssues to a ReactiveValue dataframe
    xissues_file = xissues_default_file()
    rxissues = shiny::reactiveVal(data.frame())

    load_xissues = function() {
      if (file.exists(xissues_file)) {
        df = try(readRDS(xissues_file), silent = TRUE)
        if (!inherits(df, "try-error") && is.data.frame(df)) {
          rxissues(df)
        }
      }
    }
    load_xissues()

    # Load ignored articles
    ign_rds = ignored_default_file(output_dir)
    ign_yaml = ignored_text_file(output_dir)
    ignored_df = shiny::reactiveVal(data.frame())

    load_ignored = function() {
      df = ignored_load(ign_rds)
      ignored_df(df)
    }
    load_ignored()

    # Match static raw df_issues to currently active xissues
    issues_with_xid = shiny::reactive({
      df = df_issues
      df$xid = sr_match_xissues(df, rxissues())
      df
    })

    sr_get_first_issue_pid = function() {
      sel = selected_issue_row()

      cur_issues = issues_with_xid() %>%
        dplyr::filter(
          project_dir == sel$project_dir,
          issue_category == sel$issue_category,
          xid == sel$xid
        )

      id_col = if ("pid" %in% names(cur_issues)) {
        "pid"
      } else if ("runid" %in% names(cur_issues)) {
        "runid"
      } else {
        return(NULL)
      }

      ids = cur_issues[[id_col]]
      ids = ids[!is.na(ids)]

      if (length(ids) == 0) {
        return(NULL)
      }

      ids[[1]]
    }

    # --- Regcheck Issues Logic ---
    summary_data = shiny::reactive({
      df = issues_with_xid()
      if (NROW(df) == 0) return(data.frame(Message = "No issues found"))

      ign_artids = ignored_df()$artid

      df %>%
        dplyr::group_by(issue_category, artid, project_dir, xid) %>%
        dplyr::summarize(
          n_issues = dplyr::n(),
          cmd_counts = {
            cmd_vals = as.character(cmd)
            cmd_vals[is.na(cmd_vals) | cmd_vals == ""] = "unknown"
            tab = sort(table(cmd_vals), decreasing = TRUE)
            paste0(names(tab), ": ", tab, collapse = ", ")
          },
          .groups = "drop"
        ) %>%
        dplyr::mutate(is_ignored = artid %in% ign_artids) %>%
        dplyr::arrange(
          is_ignored, # FALSE (not ignored) sorts before TRUE (ignored)
          nzchar(xid), # nzchar == FALSE (0) -> ordered first, matched ones later
          issue_category,
          dplyr::desc(n_issues)
        )
    })

    output$summary_table = DT::renderDT({
      df = summary_data()
      if ("Message" %in% names(df)) return(DT::datatable(df, class = "compact stripe hover", options = sr_dt_opts(dom="t")))

      DT::datatable(
        df %>% dplyr::select(Category = issue_category, ArtID = artid, Issues = n_issues, Commands = cmd_counts, XID = xid),
        selection = "single",
        class = "compact stripe hover",
        options = sr_dt_opts(),
        rownames = FALSE
      )
    })

    selected_issue_row = shiny::reactive({
      req(input$summary_table_rows_selected)
      summary_data()[input$summary_table_rows_selected, ]
    })

    output$project_actions = shiny::renderUI({
      req(selected_issue_row())
      sel = selected_issue_row()
      artid = sel$artid

      is_ign = artid %in% ignored_df()$artid
      btn_priority = if (is_ign) {
        shiny::actionButton("btn_unignore_article", "Un-ignore article", class = "btn-sm btn-info", style="margin-left:15px;")
      } else {
        shiny::actionButton("btn_ignore_article_modal", "Ignore article", class = "btn-sm btn-warning", style="margin-left:15px;")
      }

      shiny::tagList(
        shiny::div(style = "margin-bottom: 5px; padding-left: 4px;",
          shiny::strong(paste("Selected Article:", artid))
        ),
        shiny::div(style = "margin: 5px 0; padding-left: 4px;",
          shiny::actionButton("btn_study_and_close_issue", "Study and close", class = "btn-sm btn-default"),
          shiny::actionButton("btn_rstudio_issue", "Show in Files", icon = shiny::icon("folder-open"), class = "btn-sm btn-default"),
          shiny::actionButton("btn_report_issue", "do_report.html", icon = shiny::icon("file-code"), class = "btn-sm btn-default"),
          btn_priority
        )
      )
    })

    # Regcheck Ignore Modal logic
    shiny::observeEvent(input$btn_ignore_article_modal, {
      req(selected_issue_row())
      artid = selected_issue_row()$artid

      shiny::showModal(shiny::modalDialog(
        title = paste("Ignore Article:", artid),
        shiny::textInput("txt_ignore_descr", "Reason for ignoring (optional):", width = "100%"),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("btn_confirm_ignore", "Ignore", class = "btn-warning")
        )
      ))
    })

    shiny::observeEvent(input$btn_confirm_ignore, {
      req(selected_issue_row())
      artid = selected_issue_row()$artid
      descr = input$txt_ignore_descr
      ignored_add(artid, descr, file = ign_rds, text_file = ign_yaml)
      load_ignored()
      shiny::removeModal()
    })

    shiny::observeEvent(input$btn_unignore_article, {
      req(selected_issue_row())
      artid = selected_issue_row()$artid
      ignored_remove(artid, file = ign_rds, text_file = ign_yaml)
      load_ignored()
    })

    shiny::observeEvent(input$btn_study_and_close_issue, {
      project_dir = selected_issue_row()$project_dir
      pid = sr_get_first_issue_pid()
      sr_study_project(project_dir, pid)
      sr_close_app()
    })

    shiny::observeEvent(input$btn_rstudio_issue, {
      project_dir = selected_issue_row()$project_dir
      pid = sr_get_first_issue_pid()
      sr_study_project(project_dir, pid, open_report = FALSE)
    })

    shiny::observeEvent(input$btn_report_issue, {
      pdir = selected_issue_row()$project_dir
      rep_file = file.path(pdir, "reports", "do_report.html")
      if (file.exists(rep_file)) {
        try(utils::browseURL(rep_file), silent = TRUE)
      } else {
        shiny::showNotification("do_report.html not found in project.", type = "warning")
      }
    })

    output$detail_table = DT::renderDT({
      req(selected_issue_row())
      sel = selected_issue_row()

      cur_issues = issues_with_xid() %>%
        dplyr::filter(
          artid == sel$artid,
          issue_category == sel$issue_category,
          xid == sel$xid
        )

      # Attempt lazy loading of data prep failure info if it's a failure category
      fail_cats = c("1. sb_raw, sb and rb failed but so exists",
                    "2. sb and rb failed but sb_raw exists",
                    "3. rb failed")

      is_fail_cat = any(sapply(fail_cats, function(x) grepl(x, sel$issue_category, fixed=TRUE)))

      if (is_fail_cat || grepl("TIMEOUT", sel$issue_category, fixed=TRUE)) {
        pdir = sel$project_dir
        # Only load DRF safely
        drf = try(repboxDRF::drf_load(pdir, apply_caches = FALSE), silent = TRUE)
        if (!inherits(drf, "try-error") && !is.null(drf$r_err_runids) && length(drf$r_err_runids) > 0 && !is.null(drf$path_df)) {

          cur_issues$failed_prep_runid = NA_integer_
          cur_issues$failed_prep_cmd = NA_character_

          err_runids = drf$r_err_runids
          for (i in seq_len(nrow(cur_issues))) {
            pid = cur_issues$runid[i]
            if (is.na(pid)) next
            path = drf$path_df$runid[drf$path_df$pid == pid]
            bad_runids = intersect(path, err_runids)
            if (length(bad_runids) > 0) {
              first_bad = min(bad_runids)
              cur_issues$failed_prep_runid[i] = first_bad
              cmd_val = drf$run_df$cmd[drf$run_df$runid == first_bad]
              if (length(cmd_val) > 0) cur_issues$failed_prep_cmd[i] = cmd_val[1]
            }
          }
        }
      }

      show_cols = c("runid", "cmd")
      if ("problem" %in% names(cur_issues)) show_cols = c(show_cols, "problem")
      if ("cmdline" %in% names(cur_issues)) show_cols = c(show_cols, "cmdline")
      if ("failed_prep_runid" %in% names(cur_issues)) show_cols = c(show_cols, "failed_prep_runid", "failed_prep_cmd")

      out_df = cur_issues %>% dplyr::select(dplyr::any_of(show_cols))

      DT::datatable(
        out_df,
        class = "compact stripe hover",
        options = sr_dt_opts(),
        rownames = FALSE
      )
    })


    # --- Repbox Problems Logic ---
    prob_summary_data = shiny::reactive({
      if (NROW(df_probs) == 0) return(data.frame(Message = "No repbox_problems found"))

      ign_artids = ignored_df()$artid

      df_probs %>%
        dplyr::group_by(problem_type, artid, project_dir) %>%
        dplyr::summarize(
          n_problems = dplyr::n(),
          .groups = "drop"
        ) %>%
        dplyr::mutate(is_ignored = artid %in% ign_artids) %>%
        dplyr::arrange(is_ignored, dplyr::desc(n_problems))
    })

    output$prob_summary_table = DT::renderDT({
      df = prob_summary_data()
      if ("Message" %in% names(df)) return(DT::datatable(df, class = "compact stripe hover", options = sr_dt_opts(dom="t")))

      DT::datatable(
        df %>% dplyr::select(Type = problem_type, ArtID = artid, Count = n_problems),
        selection = "single",
        class = "compact stripe hover",
        options = sr_dt_opts(),
        rownames = FALSE
      )
    })

    selected_prob_row = shiny::reactive({
      req(input$prob_summary_table_rows_selected)
      prob_summary_data()[input$prob_summary_table_rows_selected, ]
    })

    output$prob_project_actions = shiny::renderUI({
      req(selected_prob_row())
      sel = selected_prob_row()
      artid = sel$artid

      is_ign = artid %in% ignored_df()$artid
      btn_priority = if (is_ign) {
        shiny::actionButton("btn_unignore_article_prob", "Un-ignore article", class = "btn-sm btn-info", style="margin-left:15px;")
      } else {
        shiny::actionButton("btn_ignore_article_modal_prob", "Ignore article", class = "btn-sm btn-warning", style="margin-left:15px;")
      }

      shiny::tagList(
        shiny::div(style = "margin-bottom: 5px;",
          shiny::strong(paste("Selected Article:", artid))
        ),
        shiny::div(style = "margin: 5px 0;",
          shiny::actionButton("btn_study_and_close", "Study and close", class = "btn-sm btn-default"),
          shiny::actionButton("btn_rstudio_prob", "Files", icon = shiny::icon("folder-open"), class = "btn-sm btn-default"),
          shiny::actionButton("btn_report_prob", "Report", icon = shiny::icon("file-code"), class = "btn-sm btn-default"),
          btn_priority
        )
      )
    })

    # Repbox Problems Ignore Modal logic
    shiny::observeEvent(input$btn_ignore_article_modal_prob, {
      req(selected_prob_row())
      artid = selected_prob_row()$artid

      shiny::showModal(shiny::modalDialog(
        title = paste("Ignore Article:", artid),
        shiny::textInput("txt_ignore_descr_prob", "Reason for ignoring (optional):", width = "100%"),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("btn_confirm_ignore_prob", "Ignore", class = "btn-warning")
        )
      ))
    })

    shiny::observeEvent(input$btn_confirm_ignore_prob, {
      req(selected_prob_row())
      artid = selected_prob_row()$artid
      descr = input$txt_ignore_descr_prob
      ignored_add(artid, descr, file = ign_rds, text_file = ign_yaml)
      load_ignored()
      shiny::removeModal()
    })

    shiny::observeEvent(input$btn_unignore_article_prob, {
      req(selected_prob_row())
      artid = selected_prob_row()$artid
      ignored_remove(artid, file = ign_rds, text_file = ign_yaml)
      load_ignored()
    })

    shiny::observeEvent(input$btn_study_and_close, {
      project_dir = selected_prob_row()$project_dir
      sr_study_project(project_dir)
      sr_close_app()
    })

    shiny::observeEvent(input$btn_rstudio_prob, {
      pdir = selected_prob_row()$project_dir
      try(rstudioapi::filesPaneNavigate(pdir), silent = TRUE)
    })

    shiny::observeEvent(input$btn_report_prob, {
      pdir = selected_prob_row()$project_dir
      rep_file = file.path(pdir, "reports", "do_report.html")
      if (file.exists(rep_file)) {
        try(utils::browseURL(rep_file), silent = TRUE)
      } else {
        shiny::showNotification("do_report.html not found in project.", type = "warning")
      }
    })

    output$prob_detail_table = DT::renderDT({
      req(selected_prob_row())
      sel = selected_prob_row()

      cur_probs = df_probs %>%
        dplyr::filter(artid == sel$artid, problem_type == sel$problem_type)

      show_cols = intersect(names(cur_probs), c("runid", "pid", "problem_type", "problem_descr", "file", "line"))
      out_df = cur_probs %>% dplyr::select(dplyr::any_of(show_cols))

      DT::datatable(
        out_df,
        class = "compact stripe hover",
        options = sr_dt_opts(),
        rownames = FALSE
      )
    })

    # --- XIssues Logic ---

    # Load initial yaml template if exists
    tpl_file = file.path(output_dir, "xissue_tpl_last.yaml")
    if (file.exists(tpl_file)) {
      init_yaml = try(paste0(readLines(tpl_file, warn=FALSE), collapse="\n"), silent=TRUE)
      if (!inherits(init_yaml, "try-error")) {
        shiny::updateTextAreaInput(session, "xissue_yaml_text", value = init_yaml)
      }
    }

    output$xissues_table = DT::renderDT({
      df = rxissues()
      if (NROW(df) == 0) return(DT::datatable(data.frame(Message = "No xissues"), options = sr_dt_opts(dom="t")))

      DT::datatable(
        df,
        selection = "single",
        class = "compact stripe hover",
        options = sr_dt_opts(dom="ft", paging=TRUE, pageLength=10),
        rownames = FALSE
      )
    })

    # Click on xissues table -> put into textarea
    shiny::observeEvent(input$xissues_table_rows_selected, {
      req(input$xissues_table_rows_selected)
      df = rxissues()
      sel = df[input$xissues_table_rows_selected, , drop=FALSE]
      if (NROW(sel) > 0) {
        sel$time = format(sel$time)
        yaml_txt = try(yaml::as.yaml(as.list(sel)), silent=TRUE)
        if (!inherits(yaml_txt, "try-error")) {
          shiny::updateTextAreaInput(session, "xissue_yaml_text", value = yaml_txt)
        }
      }
    })

    # Click add button -> add xissue
    shiny::observeEvent(input$btn_add_xissue, {
      yaml_txt = input$xissue_yaml_text
      if (!nzchar(trimws(yaml_txt))) return()

      res = try({
        xissue = xissue_from_yaml(yaml_txt)
        xissue_add(xissue)
      }, silent = TRUE)

      if (inherits(res, "try-error")) {
        shiny::showNotification(paste0("Error adding xissue: ", as.character(res)), type = "error")
      } else {
        shiny::showNotification("Xissue added/updated successfully.", type = "message")
        load_xissues()
      }
    })

    # When clicking an issue row, create or prefill template
    shiny::observeEvent(selected_issue_row(), {
      sel = selected_issue_row()
      req(sel)

      cur_issues = issues_with_xid() %>%
        dplyr::filter(
          project_dir == sel$project_dir,
          issue_category == sel$issue_category,
          xid == sel$xid
        )
      if (NROW(cur_issues) == 0) return()

      # Check current text in xissue_yaml_text and back it up
      cur_txt = input$xissue_yaml_text
      if (is.character(cur_txt) && nzchar(trimws(cur_txt))) {
        old_file = file.path(output_dir, "xissue_tpl_old.yaml")
        dir.create(dirname(old_file), showWarnings=FALSE, recursive=TRUE)
        writeLines(cur_txt, old_file)
      }

      if (!is.null(sel$xid) && nzchar(sel$xid)) {
        # Load the existing xissue directly into the editor
        xi_df = rxissues()
        xi = xi_df[xi_df$xid == sel$xid, , drop=FALSE]
        if (NROW(xi) > 0) {
          xi$time = format(xi$time)
          yaml_txt = try(yaml::as.yaml(as.list(xi[1, ])), silent=TRUE)
          if (inherits(yaml_txt, "try-error")) {
            yaml_txt = sr_make_xissue_template(cur_issues[1, , drop=FALSE], sel$artid)
          }
        } else {
          # Fallback
          yaml_txt = sr_make_xissue_template(cur_issues[1, , drop=FALSE], sel$artid)
        }
      } else {
        # Create a blank new template to fill out
        first_issue = cur_issues[1, , drop=FALSE]
        yaml_txt = sr_make_xissue_template(first_issue, sel$artid)
      }

      tpl_file = file.path(output_dir, "xissue_tpl_last.yaml")
      dir.create(dirname(tpl_file), showWarnings=FALSE, recursive=TRUE)
      writeLines(yaml_txt, tpl_file)

      shiny::updateTextAreaInput(session, "xissue_yaml_text", value = yaml_txt)
    })

  }

  shiny::shinyApp(ui, server)
}
```
!END_MODIFICATION
