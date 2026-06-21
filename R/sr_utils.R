# Utility functions for repboxReportSummary

sr_v_bool = function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0) return(logical(0))
  if (is.logical(x)) {
    x[is.na(x)] = default
    return(x)
  }
  if (is.numeric(x) || is.integer(x)) {
    x[is.na(x)] = if(default) 1 else 0
    return(x != 0)
  }
  x = tolower(trimws(as.character(x)))
  x[is.na(x)] = if (default) "true" else "false"
  x %in% c("true", "t", "yes", "y", "1")
}

sr_v_num = function(x, default = 0) {
  if (is.null(x) || length(x) == 0) return(numeric(0))
  x = suppressWarnings(as.numeric(x))
  x[is.na(x)] = default
  x
}

sr_read_parcel = function(project_dir, parcel_name) {
  file = file.path(project_dir, "repdb", paste0(parcel_name, ".Rds"))
  if (!file.exists(file)) return(NULL)

  obj = readRDS(file)

  if (is.list(obj) && !inherits(obj, "data.frame") && parcel_name %in% names(obj)) {
    obj = obj[[parcel_name]]
  } else if (is.list(obj) && !inherits(obj, "data.frame") && length(obj) == 1) {
    if (inherits(obj[[1]], "data.frame")) {
      obj = obj[[1]]
    }
  }

  obj
}

#' Default options for slim DataTables
#'
#' @param dom The dom layout string for DataTables.
#' @param ... Additional options passed to DataTables.
#' @export
sr_dt_opts = function(dom = "ft", ...) {
  list(
    dom = dom,
    paging = FALSE,
    info = FALSE,
    ordering = TRUE,
    autoWidth = FALSE,
    ...
  )
}
