# Helper functions to store info about expected issues
# Some issues will be expected and not solved by our repbox
# pipeline, but we want to know what those issues are
# and store them systematically

example = function() {
  xissue = xissue_make("ivregress_liml", "reg", "ivregress","ivregress liml ","coefs","LIML estimation not yet translated" )
  xissue_add(xissue)
  rstudioapi::filesPaneNavigate(dirname(xissues_text_file()))
}


xissue_failure_cats = function() {
  c("rb", "sb-rb", "sb_raw-sb-rb", "coefs","so", "")
}



# artids: example artids that have the problem
# pids: example pid that have the problem if there are multiple artid map one-to-one to artid with recycling
xissue_make = function(xid="", where = c("reg", "mod")[1], cmd="", fixed_pattern = "",  failure_cat = "" , descr="", artids="", pids="", time=Sys.time(), rx_pattern="") {
  if (length(cmd)>1) {
    cmd = paste0(cmd, collapse=",")
  }
  if (length(artids)>1) {
    artids = paste0(artids, collapse=",")
  }
  if (length(pids)>1) {
    pids = paste0(pids, collapse=",")
  }
  xissue = as.data.frame(as.list(environment()))
  if (!xissue$failure_cat %in% xissue_failure_cats()) {
    stop(paste0("\nNo allowable failure_cat=",xissue$failure_cat))
  }
  xissue
}

xissue_add = function(xissue, xissues_file = xissues_default_file(), backup_file = xissues_default_backupfile()) {

  if (is.null(xissue$xid) || !nzchar(trimws(as.character(xissue$xid)))) {
    stop("xid cannot be empty.")
  }

  if (file.exists(xissues_file)) {
    xi_df = readRDS(xissues_file)
  } else {
    xi_df = NULL
  }

  xi_df$time = as.POSIXct(xi_df$time)

  if (!is.null(xi_df) && nrow(xi_df) > 0 && xissue$xid %in% xi_df$xid) {
    # Replace existing xissue with same xid
    idx = which(xi_df$xid == xissue$xid)
    xi_df[idx[1], ] = xissue

    # Clean up any unexpected duplicates with the same xid
    if (length(idx) > 1) {
      xi_df = xi_df[-idx[-1], ]
    }
  } else {
    xi_df = dplyr::bind_rows(xi_df, xissue)
  }

  dupl = duplicated(xi_df %>% dplyr::select(-time))
  xi_df = xi_df[!dupl,]
  xissues_df_save(xi_df, xissues_file, backup_file)
}

xissues_df_save = function(xi_df,xissues_file = xissues_default_file(), backup_file = xissues_default_backupfile()) {
  try({
    saveRDS(xi_df, xissues_file)
    saveRDS(xi_df, backup_file)
    xissues_as_text(xi_df)
  })


}

xissues_as_text = function(xi_df, text_file = xissues_text_file()) {
  restore.point("xissues_as_text")
  xi_df = xi_df %>%
    arrange(desc(time))

  library(yaml)
  xi_df$time = format(xi_df$time)
  txt = yaml::as.yaml(xi_df, column.major = FALSE, indent=2)
  writeLines(txt, text_file)

}

xissues_text_file = function() {
  normalizePath("~/repbox/reports/xissues.yaml", mustWork = FALSE)
}


xissues_default_file = function() {
  normalizePath("~/repbox/reports/xissues.Rds", mustWork = FALSE)
}

xissues_default_backupfile = function() {
  normalizePath("~/repbox/reports/xissues_backup.Rds", mustWork = FALSE)
}

example = function() {
  xi_df = xissues_from_yaml_file()
  xissues_df_save(xi_df)
}

xissues_from_yaml_file = function(file = xissues_text_file()) {
  yaml = readLines(file)
  li = yaml.load(yaml)
  res_li = lapply(li, function(raw) do.call(xissue_make, raw))
  xi_df = bind_rows(res_li)
}

xissue_from_yaml = function(yaml) {
  li = yaml::yaml.load(yaml)
  li$time = Sys.time()

  # Ensure character lengths are 1 for df conversion
  for (nm in names(li)) {
    if (length(li[[nm]]) > 1) {
      li[[nm]] = paste0(li[[nm]], collapse = ",")
    } else if (length(li[[nm]]) == 0) {
      li[[nm]] = ""
    }
  }

  xissue = as.data.frame(li, stringsAsFactors = FALSE)
  if (!xissue$failure_cat %in% xissue_failure_cats()) {
    stop(paste0("\nNo allowable failure_cat=", xissue$failure_cat))
  }
  xissue
}
