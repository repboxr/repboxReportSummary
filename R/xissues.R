# Helper functions to store info about expected issues
# Some issues will be expected and not solved by our repbox
# pipeline, but we want to know what those issues are
# and store them systematically

example = function() {
  xissue = xissue_make("reg", "ivregress","ivregress liml ","coefs","LIML estimation not yet translated" )
  xissue_add(xissue)
  rstudioapi::filesPaneNavigate(dirname(xissues_text_file()))
}


xissue_failure_cats = function() {
  c("rb", "sb-rb", "sb_raw-sb-rb", "coefs","so", "")
}


# creates an xissue from yaml text
# needs to be implemented
xissue_from_yaml = function(yaml) {

}

# artids: example artids that have the problem
# pids: example pid that have the problem if there are multiple artid map one-to-one to artid with recycling
xissue_make = function(where = c("reg", "mod")[1], cmd="", fixed_pattern = "",  failure_cat = "" , descr="", artids="", pids="", time=Sys.time(), rx_pattern="") {
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

  if (file.exists(xissues_file)) {
    xi_df = readRDS(xissues_file)
  } else {
    xi_df = NULL
  }
  xi_df = bind_rows(xi_df, xissue)
  dupl = duplicated(xi_df %>% select(-time))
  xi_df = xi_df[!dupl,]

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
  normalizePath("~/repbox/reports/xissues.txt", mustWork = FALSE)
}


xissues_default_file = function() {
  normalizePath("~/repbox/reports/xissues.Rds", mustWork = FALSE)
}

xissues_default_backupfile = function() {
  normalizePath("~/repbox/reports/xissues_backup.Rds", mustWork = FALSE)
}
