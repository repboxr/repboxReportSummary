#' Aggregate issues from multiple project directories
#'
#' @param project_dirs Character vector of repbox project directories.
#' @return A data.frame of all issues combined and categorized.
#' @export
sr_aggregate_issues = function(project_dirs) {
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
sr_get_project_issues = function(project_dir) {
  restore.point("sr_get_project_issues")
  regcheck = sr_read_parcel(project_dir, "regcheck")
  if (is.null(regcheck) || NROW(regcheck) == 0) return(NULL)

  df_rc = as.data.frame(regcheck)

  if (!"cmd" %in% names(df_rc)) {
    df_rc$cmd = NA_character_
  }

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
  df_rc$project = basename(project_dir)
  df_rc$project_dir = project_dir

  # Filter only actual issues
  is_issue = !reg_ok | (cat != "8. Other issues") | (!is.na(df_rc$problem) & nzchar(as.character(df_rc$problem)))
  df_rc = df_rc[is_issue, , drop = FALSE]

  if (NROW(df_rc) == 0) return(NULL)

  # Try to merge command line info from run_cmd if available for display
  run_cmd = sr_read_parcel(project_dir, "stata_run_cmd")
  if (!is.null(run_cmd) && "runid" %in% names(run_cmd)) {
    rcmd = as.data.frame(run_cmd)
    keep_rcmd = intersect(names(rcmd), c("runid", "cmdline", "file_path", "line"))
    rcmd = rcmd[, keep_rcmd, drop = FALSE]

    if (NROW(rcmd) > 0) {
      df_rc = merge(df_rc, rcmd, by = "runid", all.x = TRUE, suffixes=c("", ".y"))
      # Remove redundant merge columns
      for (col in names(df_rc)) {
        if (endsWith(col, ".y")) df_rc[[col]] = NULL
      }
    }
  }

  if (!"cmdline" %in% names(df_rc)) df_rc$cmdline = NA_character_

  df_rc
}

#' Get repbox_problems for a single project
sr_get_project_problems = function(project_dir) {
  probs = sr_read_parcel(project_dir, "repbox_problem")
  if (is.null(probs) || NROW(probs) == 0) return(NULL)

  df_prob = as.data.frame(probs)
  df_prob$project = basename(project_dir)
  df_prob$project_dir = project_dir

  df_prob
}
