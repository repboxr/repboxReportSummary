# General helpers for running one Shiny app instance as an RStudio job.
#
# Required packages:
#   rstudioapi, ps, shiny, later, stringi
#
# app_id must be a stable, filesystem-safe identifier. Starting the same
# app_id again stops the previous verified process and replaces its job.

.shiny_job_require_packages = function(packages) {
  missing = packages[
    !vapply(
      packages,
      requireNamespace,
      quietly = TRUE,
      FUN.VALUE = logical(1L)
    )
  ]

  if (length(missing) > 0L) {
    stop(
      "Missing required package(s): ",
      stringi::stri_join(missing, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


.shiny_job_validate_app_id = function(app_id) {
  if (
    !is.character(app_id) ||
    length(app_id) != 1L ||
    is.na(app_id) ||
    !nzchar(app_id) ||
    !stringi::stri_detect_regex(app_id, "^[A-Za-z0-9._-]+$")
  ) {
    stop(
      "app_id must contain only letters, digits, '.', '_' and '-'.",
      call. = FALSE
    )
  }

  app_id
}


.shiny_job_normalize_dir = function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  normalizePath(
    path,
    winslash = "/",
    mustWork = TRUE
  )
}


.shiny_job_paths = function(app_id, output_dir) {
  app_id = .shiny_job_validate_app_id(app_id)
  output_dir = .shiny_job_normalize_dir(output_dir)

  root_dir = file.path(output_dir, ".shiny-app-jobs")
  app_dir = file.path(root_dir, app_id)

  dir.create(app_dir, recursive = TRUE, showWarnings = FALSE)
  app_dir = normalizePath(app_dir, winslash = "/", mustWork = TRUE)

  list(
    output_dir = output_dir,
    root_dir = root_dir,
    app_dir = app_dir,
    state_path = file.path(app_dir, "state.rds"),
    control_path = file.path(app_dir, "control.rds"),
    registry_path = file.path(app_dir, "registry.rds")
  )
}


.shiny_job_save_rds_atomic = function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  tmp_path = tempfile(
    pattern = stringi::stri_c(".", basename(path), "-"),
    tmpdir = dirname(path)
  )
  on.exit(unlink(tmp_path), add = TRUE)

  saveRDS(object, tmp_path)

  ok = file.rename(tmp_path, path)
  if (!ok) {
    ok = file.copy(tmp_path, path, overwrite = TRUE)
  }
  if (!ok) {
    stop("Could not write file: ", path, call. = FALSE)
  }

  invisible(path)
}


.shiny_job_write_lines_atomic = function(text, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  tmp_path = tempfile(
    pattern = stringi::stri_c(".", basename(path), "-"),
    tmpdir = dirname(path)
  )
  on.exit(unlink(tmp_path), add = TRUE)

  writeLines(text, tmp_path, useBytes = TRUE)

  ok = file.rename(tmp_path, path)
  if (!ok) {
    ok = file.copy(tmp_path, path, overwrite = TRUE)
  }
  if (!ok) {
    stop("Could not write file: ", path, call. = FALSE)
  }

  invisible(path)
}


.shiny_job_read_rds = function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }

  tryCatch(
    readRDS(path),
    error = function(e) NULL
  )
}


.shiny_job_token = function() {
  stringi::stri_rand_strings(
    n = 1L,
    length = 32L,
    pattern = "[A-Za-z0-9]"
  )
}


.shiny_job_r_literal = function(x) {
  stringi::stri_join(
    deparse(x, width.cutoff = 500L),
    collapse = ""
  )
}


.shiny_job_file_md5 = function(path) {
  if (!file.exists(path)) {
    return(NA_character_)
  }

  unname(as.character(tools::md5sum(path)))
}


.shiny_job_same_state = function(x, y) {
  fields = c(
    "format_version",
    "app_id",
    "token",
    "pid",
    "create_time",
    "launcher_path",
    "launcher_md5"
  )

  if (is.null(x) || is.null(y)) {
    return(FALSE)
  }

  all(vapply(
    fields,
    function(field) identical(x[[field]], y[[field]]),
    FUN.VALUE = logical(1L)
  ))
}


.shiny_job_verify_process = function(state, app_id, paths) {
  fail = function(reason) {
    list(
      verified = FALSE,
      running = FALSE,
      reason = reason,
      handle = NULL
    )
  }

  if (!is.list(state)) {
    return(fail("No readable state file exists."))
  }

  required = c(
    "format_version",
    "app_id",
    "token",
    "pid",
    "create_time",
    "username",
    "exe",
    "cmdline",
    "launcher_path",
    "launcher_md5"
  )
  if (!all(required %in% names(state))) {
    return(fail("The state file is incomplete."))
  }

  if (!identical(state$format_version, 1L)) {
    return(fail("The state file has an unsupported format."))
  }

  if (!identical(state$app_id, app_id)) {
    return(fail("The state file belongs to another app_id."))
  }

  if (
    !is.character(state$token) ||
    length(state$token) != 1L ||
    !stringi::stri_detect_regex(state$token, "^[A-Za-z0-9]{32}$")
  ) {
    return(fail("The state token is invalid."))
  }

  scalar_character_fields = c(
    "username",
    "exe",
    "launcher_path",
    "launcher_md5"
  )
  valid_character_fields = all(vapply(
    scalar_character_fields,
    function(field) {
      value = state[[field]]
      is.character(value) &&
        length(value) == 1L &&
        !is.na(value) &&
        nzchar(value)
    },
    FUN.VALUE = logical(1L)
  ))
  if (!valid_character_fields || !is.character(state$cmdline)) {
    return(fail("The state file contains invalid process identity fields."))
  }

  pid = tryCatch(
    suppressWarnings(as.integer(state$pid)),
    error = function(e) NA_integer_
  )
  if (
    length(pid) != 1L ||
    is.na(pid) ||
    pid <= 0L ||
    identical(pid, Sys.getpid())
  ) {
    return(fail("The saved PID is invalid."))
  }

  expected_launcher = file.path(
    paths$app_dir,
    stringi::stri_c("launcher-", state$token, ".R")
  )
  expected_launcher = normalizePath(
    expected_launcher,
    winslash = "/",
    mustWork = FALSE
  )
  saved_launcher = normalizePath(
    state$launcher_path,
    winslash = "/",
    mustWork = FALSE
  )

  if (!identical(saved_launcher, expected_launcher)) {
    return(fail("The saved launcher path is not valid for this app_id."))
  }

  if (!file.exists(saved_launcher)) {
    return(fail("The saved launcher script no longer exists."))
  }

  expected_marker = stringi::stri_c(
    "# shiny-app-job-v1 token=",
    state$token
  )
  first_line = tryCatch(
    readLines(saved_launcher, n = 1L, warn = FALSE),
    error = function(e) character()
  )
  if (
    length(first_line) != 1L ||
    !identical(first_line, expected_marker)
  ) {
    return(fail("The launcher marker does not match the saved token."))
  }

  launcher_md5 = .shiny_job_file_md5(saved_launcher)
  if (
    is.na(launcher_md5) ||
    !identical(launcher_md5, state$launcher_md5)
  ) {
    return(fail("The launcher checksum does not match."))
  }

  handle = tryCatch(
    ps::ps_handle(pid),
    error = function(e) NULL
  )
  if (is.null(handle)) {
    return(fail("The saved PID does not exist."))
  }

  running = tryCatch(
    ps::ps_is_running(handle),
    error = function(e) FALSE
  )
  if (!isTRUE(running)) {
    return(fail("The saved process is no longer running."))
  }

  current_create_time = tryCatch(
    as.numeric(ps::ps_create_time(handle)),
    error = function(e) NA_real_
  )
  saved_create_time = tryCatch(
    suppressWarnings(as.numeric(state$create_time)),
    error = function(e) NA_real_
  )

  if (
    length(current_create_time) != 1L ||
    length(saved_create_time) != 1L ||
    is.na(current_create_time) ||
    is.na(saved_create_time) ||
    abs(current_create_time - saved_create_time) > 0.01
  ) {
    return(fail("The PID was reused by a different process."))
  }

  current_username = tryCatch(
    ps::ps_username(handle),
    error = function(e) NA_character_
  )
  caller_username = tryCatch(
    ps::ps_username(ps::ps_handle()),
    error = function(e) NA_character_
  )

  if (
    is.na(current_username) ||
    is.na(caller_username) ||
    !identical(current_username, state$username) ||
    !identical(current_username, caller_username)
  ) {
    return(fail("The process owner does not match."))
  }

  current_exe = tryCatch(
    ps::ps_exe(handle),
    error = function(e) NA_character_
  )
  if (
    is.na(current_exe) ||
    !identical(current_exe, state$exe)
  ) {
    return(fail("The process executable does not match."))
  }

  current_cmdline = tryCatch(
    ps::ps_cmdline(handle),
    error = function(e) character()
  )
  if (
    length(current_cmdline) == 0L ||
    !identical(as.character(current_cmdline), as.character(state$cmdline))
  ) {
    return(fail("The process command line does not match."))
  }

  has_launcher = any(
    stringi::stri_detect_fixed(current_cmdline, saved_launcher),
    na.rm = TRUE
  )
  if (!has_launcher) {
    return(fail("The process command line does not contain the launcher path."))
  }

  list(
    verified = TRUE,
    running = TRUE,
    reason = "The process identity was verified.",
    handle = handle
  )
}


.shiny_job_ids_by_name = function(jobs, job_name) {
  if (is.null(jobs) || length(jobs) == 0L) {
    return(character())
  }

  if (is.data.frame(jobs)) {
    name_col = intersect(c("name", "job_name", "jobName"), names(jobs))
    id_col = intersect(c("id", "job_id", "jobId"), names(jobs))

    if (length(name_col) == 0L || length(id_col) == 0L) {
      return(character())
    }

    keep = as.character(jobs[[name_col[[1L]]]]) == job_name
    return(unique(as.character(jobs[[id_col[[1L]]]][keep])))
  }

  if (
    is.list(jobs) &&
    all(c("id", "name") %in% names(jobs)) &&
    !is.list(jobs$name)
  ) {
    keep = as.character(jobs$name) == job_name
    return(unique(as.character(jobs$id[keep])))
  }

  if (!is.list(jobs)) {
    return(character())
  }

  ids = vapply(
    seq_along(jobs),
    function(i) {
      job = jobs[[i]]

      if (!is.list(job)) {
        return("")
      }

      name = job$name
      if (is.null(name)) {
        name = job$jobName
      }

      if (
        is.null(name) ||
        length(name) == 0L ||
        !identical(as.character(name[[1L]]), job_name)
      ) {
        return("")
      }

      id = job$id
      if (is.null(id)) {
        id = job$jobId
      }
      if (
        is.null(id) &&
        !is.null(names(jobs)) &&
        nzchar(names(jobs)[[i]])
      ) {
        id = names(jobs)[[i]]
      }

      if (is.null(id) || length(id) == 0L) {
        return("")
      }

      as.character(id[[1L]])
    },
    FUN.VALUE = character(1L)
  )

  unique(ids[nzchar(ids)])
}


.shiny_job_remove_pane_entries = function(job_name, registry_path) {
  ids = character()

  registry = .shiny_job_read_rds(registry_path)
  if (
    is.list(registry) &&
    !is.null(registry$job_id) &&
    length(registry$job_id) == 1L
  ) {
    ids = c(ids, as.character(registry$job_id))
  }

  jobs = tryCatch(
    rstudioapi::jobList(),
    error = function(e) NULL
  )
  ids = unique(c(ids, .shiny_job_ids_by_name(jobs, job_name)))
  ids = ids[nzchar(ids)]

  for (id in ids) {
    try(rstudioapi::jobRemove(id), silent = TRUE)
  }

  unlink(registry_path)
  invisible(ids)
}


shiny_app_job_status = function(
    app_id,
    output_dir
) {
  .shiny_job_require_packages(c("ps", "stringi"))

  paths = .shiny_job_paths(app_id, output_dir)
  state = .shiny_job_read_rds(paths$state_path)
  verification = .shiny_job_verify_process(state, app_id, paths)

  list(
    app_id = app_id,
    running = isTRUE(verification$running),
    verified = isTRUE(verification$verified),
    reason = verification$reason,
    pid = if (is.list(state)) state$pid else NULL,
    state = state
  )
}


stop_shiny_app_job = function(
    app_id,
    output_dir,
    job_name = app_id,
    grace = 1500L,
    remove_pane_entry = TRUE,
    quiet = FALSE
) {
  .shiny_job_require_packages(c("ps", "stringi"))

  paths = .shiny_job_paths(app_id, output_dir)

  # This safely asks a launcher started by these helpers to stop itself.
  .shiny_job_save_rds_atomic(
    list(
      format_version = 1L,
      app_id = app_id,
      active_token = NA_character_,
      action = "stop",
      updated_at = Sys.time()
    ),
    paths$control_path
  )

  state_before = .shiny_job_read_rds(paths$state_path)
  verification = .shiny_job_verify_process(
    state_before,
    app_id,
    paths
  )

  stopped = FALSE

  if (isTRUE(verification$verified)) {
    # Re-read the state immediately before sending a signal. If another launch
    # changed it during verification, do not signal anything.
    state_after = .shiny_job_read_rds(paths$state_path)

    if (.shiny_job_same_state(state_before, state_after)) {
      result = tryCatch(
        ps::ps_kill(
          verification$handle,
          grace = as.integer(grace)
        ),
        error = function(e) e
      )

      stopped = !inherits(result, "error")

      if (!stopped && !quiet) {
        warning(
          "The process was verified but could not be stopped: ",
          conditionMessage(result),
          call. = FALSE
        )
      }
    } else if (!quiet) {
      warning(
        "The job state changed during verification; no signal was sent.",
        call. = FALSE
      )
    }
  } else if (
    !is.null(state_before) &&
    !quiet
  ) {
    warning(
      "The saved PID was not signalled because process identity could not ",
      "be established. Reason: ",
      verification$reason,
      " The control file still asks a compatible running Shiny job to stop ",
      "itself.",
      call. = FALSE
    )
  }

  state_now = .shiny_job_read_rds(paths$state_path)
  if (
    stopped &&
    .shiny_job_same_state(state_before, state_now)
  ) {
    unlink(paths$state_path)
  }

  can_use_rstudioapi =
    requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()

  if (remove_pane_entry && can_use_rstudioapi) {
    .shiny_job_remove_pane_entries(
      job_name,
      paths$registry_path
    )
  }

  invisible(list(
    stopped = stopped,
    verified = isTRUE(verification$verified),
    reason = verification$reason,
    pid = if (is.list(state_before)) state_before$pid else NULL
  ))
}


.shiny_job_launcher = function(config_path, token) {
  marker = stringi::stri_c(
    "# shiny-app-job-v1 token=",
    token
  )

  c(
    marker,
    stringi::stri_c(
      "config_path = ",
      .shiny_job_r_literal(config_path)
    ),
    "",
    "save_rds_atomic = function(object, path) {",
    "  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)",
    "  tmp_path = tempfile(",
    "    pattern = paste0(\".\", basename(path), \"-\"),",
    "    tmpdir = dirname(path)",
    "  )",
    "  on.exit(unlink(tmp_path), add = TRUE)",
    "  saveRDS(object, tmp_path)",
    "  ok = file.rename(tmp_path, path)",
    "  if (!ok) {",
    "    ok = file.copy(tmp_path, path, overwrite = TRUE)",
    "  }",
    "  if (!ok) {",
    "    stop(\"Could not write file: \", path, call. = FALSE)",
    "  }",
    "  invisible(path)",
    "}",
    "",
    "read_rds = function(path) {",
    "  if (!file.exists(path)) {",
    "    return(NULL)",
    "  }",
    "  tryCatch(readRDS(path), error = function(e) NULL)",
    "}",
    "",
    "config = readRDS(config_path)",
    "control = read_rds(config$control_path)",
    "if (",
    "  is.null(control) ||",
    "  !identical(control$app_id, config$app_id) ||",
    "  !identical(control$active_token, config$token)",
    ") {",
    "  quit(save = \"no\", status = 0L)",
    "}",
    "",
    "handle = ps::ps_handle()",
    "state = list(",
    "  format_version = 1L,",
    "  app_id = config$app_id,",
    "  token = config$token,",
    "  pid = Sys.getpid(),",
    "  create_time = as.numeric(ps::ps_create_time(handle)),",
    "  username = ps::ps_username(handle),",
    "  exe = ps::ps_exe(handle),",
    "  cmdline = ps::ps_cmdline(handle),",
    "  launcher_path = config$launcher_path,",
    "  launcher_md5 = config$launcher_md5,",
    "  started_at = Sys.time()",
    ")",
    "save_rds_atomic(state, config$state_path)",
    "",
    "cleanup = function() {",
    "  current = read_rds(config$state_path)",
    "  if (",
    "    is.list(current) &&",
    "    identical(current$app_id, config$app_id) &&",
    "    identical(current$token, config$token)",
    "  ) {",
    "    unlink(config$state_path)",
    "  }",
    "  unlink(config_path)",
    "  unlink(config$launcher_path)",
    "}",
    "on.exit(cleanup(), add = TRUE)",
    "",
    "check_control = function() {",
    "  control = read_rds(config$control_path)",
    "  is_current =",
    "    is.list(control) &&",
    "    identical(control$app_id, config$app_id) &&",
    "    identical(control$active_token, config$token)",
    "",
    "  if (!is_current) {",
    "    try(shiny::stopApp(), silent = TRUE)",
    "    return(invisible(NULL))",
    "  }",
    "",
    "  later::later(",
    "    check_control,",
    "    delay = config$control_check_interval",
    "  )",
    "}",
    "later::later(",
    "  check_control,",
    "  delay = config$control_check_interval",
    ")",
    "",
    "if (!is.null(config$package)) {",
    "  target = get(",
    "    config$fun,",
    "    envir = asNamespace(config$package),",
    "    inherits = FALSE",
    "  )",
    "} else {",
    "  target_env = .GlobalEnv",
    "",
    "  if (length(config$source_files) > 0L) {",
    "    target_env = new.env(parent = .GlobalEnv)",
    "    for (source_file in config$source_files) {",
    "      sys.source(source_file, envir = target_env)",
    "    }",
    "  }",
    "",
    "  target = get(",
    "    config$fun,",
    "    envir = target_env,",
    "    inherits = TRUE",
    "  )",
    "}",
    "",
    "do.call(target, config$args)"
  )
}


run_shiny_app_as_job = function(
    app_id,
    fun,
    args = list(),
    package = NULL,
    source_files = character(),
    output_dir,
    job_name = app_id,
    working_dir = getwd(),
    import_env = FALSE,
    replace = TRUE,
    grace = 1500L,
    control_check_interval = 1,
    remove_old_pane_entry = TRUE
) {
  .shiny_job_require_packages(
    c("rstudioapi", "ps", "shiny", "later", "stringi")
  )

  if (!rstudioapi::isAvailable()) {
    stop(
      "RStudio is required to run a background job.",
      call. = FALSE
    )
  }

  app_id = .shiny_job_validate_app_id(app_id)

  if (
    !is.character(fun) ||
    length(fun) != 1L ||
    is.na(fun) ||
    !nzchar(fun)
  ) {
    stop("fun must be one function name.", call. = FALSE)
  }

  if (!is.list(args)) {
    stop("args must be a list.", call. = FALSE)
  }

  if (!is.null(package)) {
    if (
      !is.character(package) ||
      length(package) != 1L ||
      is.na(package) ||
      !nzchar(package)
    ) {
      stop("package must be NULL or one package name.", call. = FALSE)
    }

    if (length(source_files) > 0L) {
      stop(
        "Use either package or source_files, not both.",
        call. = FALSE
      )
    }

    if (!requireNamespace(package, quietly = TRUE)) {
      stop(
        "Package is not installed: ",
        package,
        call. = FALSE
      )
    }
  }

  if (
    is.null(package) &&
    length(source_files) == 0L &&
    !isTRUE(import_env)
  ) {
    stop(
      "Without package or source_files, import_env must be TRUE so that ",
      "the job can find fun.",
      call. = FALSE
    )
  }

  if (
    length(control_check_interval) != 1L ||
    !is.numeric(control_check_interval) ||
    is.na(control_check_interval) ||
    control_check_interval <= 0
  ) {
    stop(
      "control_check_interval must be one positive number.",
      call. = FALSE
    )
  }

  paths = .shiny_job_paths(app_id, output_dir)
  working_dir = .shiny_job_normalize_dir(working_dir)

  if (length(source_files) > 0L) {
    source_files = normalizePath(
      source_files,
      winslash = "/",
      mustWork = TRUE
    )
  }

  if (replace) {
    stop_shiny_app_job(
      app_id = app_id,
      output_dir = output_dir,
      job_name = job_name,
      grace = grace,
      remove_pane_entry = remove_old_pane_entry,
      quiet = TRUE
    )
  } else {
    status = shiny_app_job_status(app_id, output_dir)
    if (isTRUE(status$running)) {
      stop(
        "A verified job for this app_id is already running.",
        call. = FALSE
      )
    }
  }

  token = .shiny_job_token()
  launcher_path = file.path(
    paths$app_dir,
    stringi::stri_c("launcher-", token, ".R")
  )
  config_path = file.path(
    paths$app_dir,
    stringi::stri_c("config-", token, ".rds")
  )

  launcher = .shiny_job_launcher(config_path, token)
  .shiny_job_write_lines_atomic(launcher, launcher_path)

  launcher_path = normalizePath(
    launcher_path,
    winslash = "/",
    mustWork = TRUE
  )
  launcher_md5 = .shiny_job_file_md5(launcher_path)

  config = list(
    format_version = 1L,
    app_id = app_id,
    token = token,
    fun = fun,
    args = args,
    package = package,
    source_files = source_files,
    state_path = paths$state_path,
    control_path = paths$control_path,
    launcher_path = launcher_path,
    launcher_md5 = launcher_md5,
    control_check_interval = control_check_interval
  )
  .shiny_job_save_rds_atomic(config, config_path)

  .shiny_job_save_rds_atomic(
    list(
      format_version = 1L,
      app_id = app_id,
      active_token = token,
      action = "run",
      updated_at = Sys.time()
    ),
    paths$control_path
  )

  job_id = tryCatch(
    rstudioapi::jobRunScript(
      path = launcher_path,
      name = job_name,
      workingDir = working_dir,
      importEnv = import_env,
      exportEnv = ""
    ),
    error = function(e) {
      unlink(c(config_path, launcher_path))

      control = .shiny_job_read_rds(paths$control_path)
      if (
        is.list(control) &&
        identical(control$active_token, token)
      ) {
        unlink(paths$control_path)
      }

      stop(e)
    }
  )

  .shiny_job_save_rds_atomic(
    list(
      format_version = 1L,
      app_id = app_id,
      token = token,
      job_id = job_id,
      job_name = job_name,
      started_at = Sys.time()
    ),
    paths$registry_path
  )

  invisible(job_id)
}
