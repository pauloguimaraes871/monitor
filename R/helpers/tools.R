#Saving objects-----------------------------------------------------------------
init_pipeline_board <- function(){
  # Define local board path
  local_board_path <- file.path(here::here(), "data", "prod")
  # Create local board with versioning activated
  pins::board_folder(path = local_board_path, versioned = TRUE)
}

update_manifest <- function(manifest, manifest_row) {
  if (is.null(manifest)) {
    return(manifest_row)
  }

  dplyr::bind_rows(
    manifest,
    manifest_row
  )
}

read_pin_from_manifest <- function(board, manifest, object_name) {
  if (is.null(manifest)) {
    return(NULL)
  }

  required_cols <- c("object_name", "pin_hash")

  missing_cols <- setdiff(required_cols, names(manifest))

  if (length(missing_cols) > 0) {
    stop(
      paste0(
        "`manifest` is missing required columns: ",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  manifest_row <- manifest %>%
    dplyr::filter(object_name == !!object_name)

  if (nrow(manifest_row) == 0) {
    stop(
      paste0(
        "`manifest` does not contain object_name = '",
        object_name,
        "'."
      ),
      call. = FALSE
    )
  }

  if (nrow(manifest_row) > 1) {
    stop(
      paste0(
        "`manifest` has multiple rows for object_name = '",
        object_name,
        "'."
      ),
      call. = FALSE
    )
  }

  pins::pin_read(
    board = board,
    name = manifest_row$pin_hash[1]
  )
}

load_newest_manifest <- function(
    manifest_dir = here::here("data", "prod", "manifests")
) {
  if (!dir.exists(manifest_dir)) {
    message("Manifest directory does not exist: ", manifest_dir)
    return(NULL)
  }

  manifest_files <- list.files(
    path = manifest_dir,
    pattern = "\\.(rds|csv)$",
    full.names = TRUE
  )

  if (length(manifest_files) == 0) {
    message("No manifest files found in: ", manifest_dir)
    return(NULL)
  }

  read_manifest_file <- function(file_path) {
    ext <- tools::file_ext(file_path)

    manifest <- switch(
      ext,
      rds = readRDS(file_path),
      csv = readr::read_csv(file_path, show_col_types = FALSE),
      stop("Unsupported manifest file extension: ", ext, call. = FALSE)
    )

    if (!"deployed_at" %in% names(manifest)) {
      stop(
        "Manifest file is missing `deployed_at`: ",
        file_path,
        call. = FALSE
      )
    }

    manifest %>%
      dplyr::mutate(
        deployed_at = as.POSIXct(deployed_at),
        manifest_file = file_path
      )
  }

  manifests <- purrr::map_dfr(
    manifest_files,
    read_manifest_file
  )

  if (nrow(manifests) == 0) {
    message("Manifest files were found, but all were empty.")
    return(NULL)
  }

  newest_manifest_file <- manifests %>%
    dplyr::filter(deployed_at == max(deployed_at, na.rm = TRUE)) %>%
    dplyr::slice(1) %>%
    dplyr::pull(manifest_file)

  message("Newest manifest found: ", newest_manifest_file)

  read_manifest_file(newest_manifest_file) %>%
    dplyr::select(-manifest_file)
}

#Appending objects--------------------------------------------------------------
append_batch_table <- function(
    old_table,
    new_table,
    table_name,
    key_cols = NULL
) {
  ## Short circuit
  if (is.null(old_table)) {
    return(new_table)
  }

  ## Check for dates
  if (!"date" %in% names(old_table) || !"date" %in% names(new_table)) {
    stop(
      paste0(
        "Both old and new `",
        table_name,
        "` must have a `date` column."
      ),
      call. = FALSE
    )
  }

  old_table <- old_table %>%
    dplyr::mutate(date = as.Date(date))

  new_table <- new_table %>%
    dplyr::mutate(date = as.Date(date))

  old_dates <- unique(old_table$date)
  new_dates <- unique(new_table$date)

  overlap_dates <- intersect(old_dates, new_dates)

  ## Resolve overlaps
  if (length(overlap_dates) > 0) {
    warning(
      paste0(
        "`",
        table_name,
        "` has overlapping dates between old and new data. ",
        "New data will prevail for dates: ",
        paste(as.character(overlap_dates), collapse = ", ")
      ),
      call. = FALSE
    )

    old_overlap <- old_table %>%
      dplyr::filter(date %in% overlap_dates)

    new_overlap <- new_table %>%
      dplyr::filter(date %in% overlap_dates)

    common_cols <- intersect(names(old_overlap), names(new_overlap))

    old_overlap_cmp <- old_overlap %>%
      dplyr::select(dplyr::all_of(common_cols))

    new_overlap_cmp <- new_overlap %>%
      dplyr::select(dplyr::all_of(common_cols))

    old_not_in_new <- old_overlap_cmp %>%
      dplyr::anti_join(
        new_overlap_cmp,
        by = common_cols
      )

    new_not_in_old <- new_overlap_cmp %>%
      dplyr::anti_join(
        old_overlap_cmp,
        by = common_cols
      )

    if (nrow(old_not_in_new) > 0 || nrow(new_not_in_old) > 0) {
      mismatch_preview <- dplyr::bind_rows(
        old_not_in_new %>%
          dplyr::mutate(source_version = "old_not_in_new", .before = 1),
        new_not_in_old %>%
          dplyr::mutate(source_version = "new_not_in_old", .before = 1)
      ) %>%
        utils::head(20)

      warning(
        paste0(
          "`",
          table_name,
          "` differs across overlapping dates. ",
          "old_not_in_new = ",
          nrow(old_not_in_new),
          ", new_not_in_old = ",
          nrow(new_not_in_old),
          ". New data will prevail.\n",
          paste(utils::capture.output(print(mismatch_preview)), collapse = "\n")
        ),
        call. = FALSE
      )
    }
  }

  old_to_keep <- old_table %>%
    dplyr::filter(!date %in% new_dates)

  dplyr::bind_rows(
    old_to_keep,
    new_table
  ) %>%
    dplyr::arrange(date)
}

append_dados_batch <- function(
    old_dados,
    dados_batch
) {
  if (is.null(old_dados)) {
    return(dados_batch)
  }

  out <- list(
    rebalanceamento_tables = list(
      rebal_weights = append_batch_table(
        old_table = old_dados_batch$rebalanceamento_tables$rebal_weights,
        table = dados_batch$rebalanceamento_tables$rebal_weights,
        table_name = "rebalanceamento_tables$rebal_weights"
      ),
      sectors = append_batch_table(
        old_table = old_dados_batch$rebalanceamento_tables$sectors,
        table = dados_batch$rebalanceamento_tables$sectors,
        table_name = "rebalanceamento_tables$sectors"
      ),
      catalog = append_batch_table(
        old_table = old_dados_batch$rebalanceamento_tables$catalog,
        table = dados_batch$rebalanceamento_tables$catalog,
        table_name = "rebalanceamento_tables$catalog"
      )
    ),
    comdinheiro_data = append_batch_table(
      old_table = old_dados_batch$comdinheiro_data,
      table = dados_batch$comdinheiro_data,
      table_name = "comdinheiro_data"
    ),
    brokerage_data  = list(
      trade_data = append_batch_table(
        old_table = old_dados_batch$brokerage_data$trade_data,
        table = dados_batch$brokerage_data$trade_data,
        table_name = "brokerage_data$trade_data"
      ),
      brokerage_notes_log = append_batch_table(
        old_table = old_dados_batch$brokerage_data$brokerage_notes_log,
        table = dados_batch$brokerage_data$brokerage_notes_log,
        table_name = "brokerage_data$brokerage_notes_log"
      )
    )
  )

  out
}




save_local_pin <- function(
    board,
    data,
    object_name,
    stage,
    commit = NULL,
    source = "internal",
    table_type = "object"
) {
  if (!stage %in% c("bronze", "presilver", "silver", "pregold", "gold")) {
    stop("Invalid stage. Allowed values are: bronze, presilver, silver, pregold, gold.")
  }

  if (is.null(commit)) {
    commit <- readline(prompt = "Enter commit hash or description: ")

    if (commit == "") {
      stop("Commit is required. Aborting.", call. = FALSE)
    }
  }

  pin_name <- object_name

  pins::pin_write(
    board = board,
    x = data,
    name = pin_name,
    type = "rds",
    title = paste(
      "Object:", object_name,
      "| Stage:", stage,
      "| Source:", source,
      "| Type:", table_type
    ),
    description = paste0(
      "Pinned on ", Sys.Date(),
      ". Commit: ", commit,
      ". Source: ", source,
      ". Type: ", table_type
    ),
    metadata = list(
      object_name = object_name,
      stage = stage,
      commit_msg = commit,
      commit_hash = digest::digest(commit),
      created_at = as.character(Sys.time())
    ),
    versioned = TRUE,
    tags = c(stage, object_name)
  )

  manifest_row <- data.frame(
    object_name = object_name,
    pin_hash = pin_name,
    commit_hash = digest::digest(commit),
    deployed_at = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )

  manifest_row
}



#Logs-------------------------------------------------
run_logged <- function(fun, log_file, log_path) {
  # Ensure log path exists
  if (!dir.exists(log_path)) {
    dir.create(log_path, recursive = TRUE)
  }

  # Construct timestamped log filename
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  full_log_path <- file.path(log_path, paste0(log_file, "_", timestamp, ".log"))

  # Open file connection
  con <- file(full_log_path, open = "wt")

  # Ensure everything is cleaned up at the end
  on.exit({
    suppressWarnings(sink(NULL, type = "message"))
    suppressWarnings(sink(NULL, type = "output"))
    suppressWarnings(close(con))
  }, add = TRUE)

  # Redirect output and messages to file
  sink(con, type = "output")
  sink(con, type = "message")

  # Run function with error handling
  result <- tryCatch(
    fun(),
    error = function(e) {
      # Restore console output
      suppressWarnings(sink(NULL, type = "message"))
      suppressWarnings(sink(NULL, type = "output"))
      # Show error message to user
      message("❌ Error during execution: ", e$message)
      message("See log for details: ", full_log_path)
      stop(e)  # Propagate the error
    }
  )

  return(result)
}

run_logged_test <- function(fun, log_file, log_path) {
  # Ensure log path exists
  if (!dir.exists(log_path)) {
    dir.create(log_path, recursive = TRUE)
  }

  # Construct timestamped log filename
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  full_log_path <- file.path(log_path, paste0(log_file, "_", timestamp, ".log"))

  # Open file connection
  con <- file(full_log_path, open = "wt")

  # Ensure everything is cleaned up at the end
  on.exit({
    # These will silently fail if already removed
    suppressWarnings(sink(NULL, type = "message"))
    suppressWarnings(sink(NULL, type = "output"))
    suppressWarnings(close(con))
  }, add = TRUE)

  # Redirect both output and messages to file
  sink(con, type = "output")
  sink(con, type = "message")

  # Try running the function
  result <- tryCatch(
    {
      fun()
      return(TRUE)
    },
    error = function(e) {
      # Restore sink BEFORE printing error to console
      suppressWarnings(sink(NULL, type = "message"))
      suppressWarnings(sink(NULL, type = "output"))

      # Print to console to warn about error
      message("❌ Error during execution: ", e$message)
      message("See log for details: ", full_log_path)
      return(FALSE)
    }
  )

  return(invisible(result))
}

check_test_success <- function(success_flag) {
  if (!isTRUE(success_flag)) {
    stop(paste0(crayon::red("❌ Tests failed. Stopping deployment.")))
  } else {
    message(crayon::green("✅ All tests passed. Proceeding with deployment."))
  }
}

#Others-------------------------------------------------------------
create_current_dates <- function(date_begin, date_end, manifest = NULL,
                                 anbima_holidays) {

  ##Coerce
  date_begin <- as.Date(date_begin)
  date_end <- as.Date(date_end)

  ## Validations
  if (is.na(date_begin) || is.na(date_end)) {
    stop("`date_begin` and `date_end` must be coercible to Date.", call. = FALSE)
  }

  ## Check if there are rebalancing dates in the requested range
  if (date_begin > date_end) {
    stop("`date_begin` must be <= `date_end`.", call. = FALSE)
  }

  current_dates <- seq.Date(
    from = date_begin,
    to = date_end,
    by = "day"
  )

  ## Check if there are rebalancing dates in the requested range
  rebalancing_dates <- list.files(
    here::here("data", "dev", "rebalancing")
  ) %>%
    as.Date(format = "%Y%m%d")

  rebalancing_dates <- rebalancing_dates[!is.na(rebalancing_dates)]

  ## If there are no rebalancing_dates in current_dates, stop
  if (!any(rebalancing_dates %in% current_dates)) {
    stop(
      paste0(
        "No rebalance_date folder found inside the requested date range. ",
        "Date range: ",
        date_begin,
        " to ",
        date_end,
        "."
      ),
      call. = FALSE
    )
  }

  ## Exclude holidays
  current_dates <- setdiff(current_dates, anbima_holidays) %>%
    as.Date()

  weekdays_num <- as.POSIXlt(current_dates)$wday

  current_dates <- current_dates[!weekdays_num %in% c(0, 6)]

  if (length(current_dates) == 0) {
    stop(
      "No valid business dates left after removing weekends and ANBIMA holidays.",
      call. = FALSE
    )
  }

  ##If there is an available manifest, warn if max(current_dates in manifest) > min(current_dates)
  if (!is.null(manifest)) {
    manifest_dates <- manifest$current_dates
    manifest_dates <- as.Date(manifest_dates)

    if (length(manifest_dates) > 0 && max(manifest_dates) > min(current_dates)) {
      warning(
        paste0(
          "The `manifest` contains `current_dates` up to ",
          max(manifest_dates),
          ", which is greater than the minimum of the requested `current_dates` (",
          min(current_dates),
          "). This indicates that the requested date range overlaps with previously
          deployed data. Please review if necessary."
        ),
        call. = FALSE
      )
    }
  }

  sort(current_dates)
}
