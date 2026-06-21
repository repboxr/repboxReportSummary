example = function() {
  library(repboxReportSummary)
  parent_dir = "/home/rstudio/repbox/projects"
  #undebug(run_shiny_app_as_job)
  run_sr_app(parent_dir = parent_dir, in_viewer = TRUE,as_job = FALSE)
  report_file = "/home/rstudio/repbox/reports/projects_summary.html"
  browseURL(report_file)

}
run_sr_app = function(
    parent_dir = NULL,
    project_dirs = NULL,
    in_viewer = FALSE,
    as_job = FALSE,
    output_dir = "/home/rstudio/repbox/reports",
    job_name = "repbox report summary",
    ...) {

  library(repboxReportSummary)
  if (as_job) {
    return(invisible(run_sr_as_job(
      parent_dir = parent_dir,
      project_dirs = project_dirs,
      in_viewer = in_viewer,
      output_dir = output_dir,
      job_name = job_name,
      ...
    )))
  }

  app = sr_app(
    parent_dir = parent_dir,
    project_dirs = project_dirs,
    ...
  )

  if (in_viewer) {
    launch_browser = rstudioapi::viewer
  } else {
    launch_browser = TRUE
  }

  shiny::runApp(app,launch.browser = launch_browser)
}


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
        .table-wrapper { max-height: 40vh; overflow: auto; border-bottom: 1px solid #ddd; margin-bottom: 10px; padding-left: 4px }
      "))
    ),
    shiny::tabsetPanel(
      # TAB 1: Regcheck Issues
      shiny::tabPanel("Regcheck Issues",
        shiny::fluidRow(
          shiny::column(12,
            shiny::div(class = "table-wrapper", DT::DTOutput("summary_table"))
          )
        ),
        shiny::fluidRow(
          shiny::column(12,
            shiny::uiOutput("project_actions"),
            shiny::div(class = "table-wrapper", DT::DTOutput("detail_table"))
          )
        )
      ),

      # TAB 2: Repbox Problems
      shiny::tabPanel("Repbox Problems",
        shiny::fluidRow(
          shiny::column(12,
            shiny::p("Click a row to see detailed instances.", style = "font-size:12px; margin:2px 0; color:#666;"),
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
    sr_get_first_issue_pid = function() {
      sel = selected_issue_row()

      cur_issues = df_issues %>%
        dplyr::filter(
          project_dir == sel$project_dir,
          issue_category == sel$issue_category
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
      if (NROW(df_issues) == 0) return(data.frame(Message = "No issues found"))

      df_issues %>%
        dplyr::group_by(issue_category, project, project_dir) %>%
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
        dplyr::arrange(issue_category, dplyr::desc(n_issues))
    })

    output$summary_table = DT::renderDT({
      df = summary_data()
      if ("Message" %in% names(df)) return(DT::datatable(df, class = "compact stripe hover", options = sr_dt_opts(dom="t")))

      DT::datatable(
        df %>% dplyr::select(Category = issue_category, Project = project, Issues = n_issues, Commands = cmd_counts),
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
      shiny::tagList(
        shiny::div(style = "margin: 5px 0; padding-left: 4px; !important",
          shiny::actionButton("btn_study_and_close_issue", "Study and close", class = "btn-sm btn-default"),
          shiny::actionButton("btn_rstudio_issue", "Show in Files", icon = shiny::icon("folder-open"), class = "btn-sm btn-default"),
          shiny::actionButton("btn_report_issue", "do_report.html", icon = shiny::icon("file-code"), class = "btn-sm btn-default")
        )
      )
    })
    shiny::observeEvent(input$btn_study_and_close_issue, {
      project_dir = selected_issue_row()$project_dir
      pid = sr_get_first_issue_pid()
      sr_study_project(project_dir, pid)
      sr_close_app()
    })
    shiny::observeEvent(input$btn_rstudio_issue, {
      pdir = selected_issue_row()$project_dir
      try(rstudioapi::filesPaneNavigate(pdir), silent = TRUE)
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

      cur_issues = df_issues %>%
        dplyr::filter(project == sel$project, issue_category == sel$issue_category)

      # Attempt lazy loading of data prep failure info if it's a failure category
      fail_cats = c("1. sb_raw, sb and rb failed but so exists",
                    "2. sb and rb failed but sb_raw exists",
                    "3. rb failed")

      if (sel$issue_category %in% fail_cats) {
        pdir = sel$project_dir
        # Only load DRF safely
        drf = try(repboxDRF::drf_load(pdir, apply_caches = FALSE), silent = TRUE)
        if (!inherits(drf, "try-error") && !is.null(drf$r_err_runids) && length(drf$r_err_runids) > 0 && !is.null(drf$path_df)) {

          cur_issues$failed_prep_runid = NA_integer_
          cur_issues$failed_prep_cmd = NA_character_

          err_runids = drf$r_err_runids
          for (i in seq_len(nrow(cur_issues))) {
            pid = cur_issues$runid[i]
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

      df_probs %>%
        dplyr::group_by(type, project, project_dir) %>%
        dplyr::summarize(
          n_problems = dplyr::n(),
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(n_problems))
    })

    output$prob_summary_table = DT::renderDT({
      df = prob_summary_data()
      if ("Message" %in% names(df)) return(DT::datatable(df, class = "compact stripe hover", options = sr_dt_opts(dom="t")))

      DT::datatable(
        df %>% dplyr::select(Type = type, Project = project, Count = n_problems),
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
      shiny::tagList(
        shiny::div(style = "margin: 5px 0;",
          shiny::actionButton("btn_study_and_close", "Study and close", class = "btn-sm btn-default"),
          shiny::actionButton("btn_rstudio_prob", "Files", icon = shiny::icon("folder-open"), class = "btn-sm btn-default"),
          shiny::actionButton("btn_report_prob", "Report", icon = shiny::icon("file-code"), class = "btn-sm btn-default")
        )
      )
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
        dplyr::filter(project == sel$project, type == sel$type)

      show_cols = intersect(names(cur_probs), c("runid", "type", "msg", "fail_action", "file", "line"))
      out_df = cur_probs %>% dplyr::select(dplyr::any_of(show_cols))

      DT::datatable(
        out_df,
        class = "compact stripe hover",
        options = sr_dt_opts(),
        rownames = FALSE
      )
    })

    # --- XIssues Logic ---
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
        shiny::showNotification("Xissue added successfully.", type = "message")
        load_xissues()
      }
    })

    # When clicking an issue row, create template
    shiny::observeEvent(selected_issue_row(), {
      sel = selected_issue_row()
      req(sel)

      cur_issues = df_issues %>%
        dplyr::filter(
          project_dir == sel$project_dir,
          issue_category == sel$issue_category
        )
      if (NROW(cur_issues) == 0) return()

      first_issue = cur_issues[1, , drop=FALSE]
      yaml_txt = sr_make_xissue_template(first_issue, sel$project)

      # Check current text in xissue_yaml_text
      cur_txt = input$xissue_yaml_text
      if (is.character(cur_txt) && nzchar(trimws(cur_txt))) {
        old_file = file.path(output_dir, "xissue_tpl_old.yaml")
        dir.create(dirname(old_file), showWarnings=FALSE, recursive=TRUE)
        writeLines(cur_txt, old_file)
      }

      tpl_file = file.path(output_dir, "xissue_tpl_last.yaml")
      dir.create(dirname(tpl_file), showWarnings=FALSE, recursive=TRUE)
      writeLines(yaml_txt, tpl_file)

      shiny::updateTextAreaInput(session, "xissue_yaml_text", value = yaml_txt)
    })

  }

  shiny::shinyApp(ui, server)
}


sr_close_app = function() {
  shiny::stopApp()
}

sr_study_project = function(project_dir, pid=NULL) {
  restore.point("sr_study_project")
  .GlobalEnv$project_dir = project_dir
  try(rstudioapi::filesPaneNavigate(file.path(project_dir, "run")))
  try(rstudioapi::navigateToFile(file.path(project_dir, "run/run_test_data_path.R")))
  rep_file = file.path(project_dir, "reports", "do_report.html")
  try(utils::browseURL(rep_file), silent = TRUE)

  # Make the Files pane the active RStudio pane.
  try(
    rstudioapi::executeCommand("activateFiles", quiet = TRUE),
    silent = TRUE
  )
  if (!is.null(pid)) {
    cat("\npid = ", pid)
    .GlobalEnv$pid = pid
  }

}
