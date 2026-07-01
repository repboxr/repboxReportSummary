#' Aggregate issues from multiple project directories
#'
#' @param project_dirs Character vector of repbox project directories.
#' @return A data.frame of all issues combined and categorized.
#' @export
sr_aggregate_issues = function(project_dirs) {
  restore.point("sr_aggregrate_issues")
  dfs = lapply(project_dirs, sr_get_project_issues)
  dfs = dfs[!vapply(dfs, is.null, logical(1))]

  if (length(dfs) == 0) {
    return(data.frame())
  }

  res = dplyr::bind_rows(dfs)
  rownames(res) = NULL
  res
}

#' Aggregate repbox_problems from multiple project directories
#'
#' @param project_dirs Character vector of repbox project directories.
#' @return A data.frame of all problems combined.
#' @export
sr_aggregate_problems = function(project_dirs) {
  restore.point("sr_aggregate_problems")

  dfs = lapply(project_dirs, sr_get_project_problems)
  dfs = dfs[!vapply(dfs, is.null, logical(1))]

  if (length(dfs) == 0) {
    return(data.frame())
  }

  res = do.call(rbind, dfs)
  rownames(res) = NULL
  res
}

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

#' Get repbox_problems for a single project
sr_get_project_problems = function(project_dir) {
  restore.point("sr_get_project_problems")
  if (!repdb_has_parcel(project_dir, "problem")) {
    repboxRun::repbox_store_project_problems(project_dir = project_dir)
  }
  probs = repdb_load_parcels(project_dir, "problem")$problem
  if (is.null(probs) || NROW(probs) == 0) return(NULL)

  df_prob = as.data.frame(probs)
  if (!"artid" %in% names(df_prob)) {
    df_prob$artid = basename(project_dir)
  }
  df_prob$project_dir = project_dir

  # ignore some problems that are unimportant

  df_prob = dplyr::filter(df_prob, !stringi::stri_detect_fixed(df_prob$problem_type,"_journ_"))
  df_prob
}
