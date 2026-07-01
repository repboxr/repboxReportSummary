ignored_default_file = function(output_dir = "~/repbox/reports") {
  normalizePath(file.path(output_dir, "ignored_articles.Rds"), mustWork = FALSE)
}

ignored_text_file = function(output_dir = "~/repbox/reports") {
  normalizePath(file.path(output_dir, "ignored_articles.yaml"), mustWork = FALSE)
}

ignored_load = function(file = ignored_default_file()) {
  if (file.exists(file)) {
    res = readRDS(file)
    if (!is.null(res) && is.data.frame(res)) return(res)
  }
  data.frame(artid = character(), descr = character(), stringsAsFactors = FALSE)
}

ignored_save = function(df, file = ignored_default_file(), text_file = ignored_text_file()) {
  saveRDS(df, file)
  if (nrow(df) > 0) {
    writeLines(yaml::as.yaml(df, column.major = FALSE), text_file)
  } else {
    writeLines("", text_file)
  }
}

ignored_add = function(artid, descr = "", file = ignored_default_file(), text_file = ignored_text_file()) {
  df = ignored_load(file)
  idx = which(df$artid == artid)
  if (length(idx) > 0) {
    df$descr[idx[1]] = descr
  } else {
    df = rbind(df, data.frame(artid = as.character(artid), descr = as.character(descr), stringsAsFactors = FALSE))
  }
  ignored_save(df, file, text_file)
  df
}

ignored_remove = function(artid, file = ignored_default_file(), text_file = ignored_text_file()) {
  df = ignored_load(file)
  df = df[df$artid != artid, , drop = FALSE]
  ignored_save(df, file, text_file)
  df
}
