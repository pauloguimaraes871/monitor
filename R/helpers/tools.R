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

  if (!is.list(manifest)) {
    stop(
      "`manifest` must be a list.",
      call. = FALSE
    )
  }

  if (is.null(manifest$versions_table)) {
    stop(
      "`manifest` is missing `versions_table`.",
      call. = FALSE
    )
  }

  versions_table <- manifest$versions_table

  required_cols <- c(
    "object_name",
    "pin_name",
    "latest_pin_version"
  )

  missing_cols <- setdiff(required_cols, names(versions_table))

  if (length(missing_cols) > 0L) {
    stop(
      paste0(
        "`manifest$versions_table` is missing required columns: ",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  manifest_row <- versions_table %>%
    dplyr::filter(object_name == !!object_name)

  if (nrow(manifest_row) == 0L) {
    stop(
      paste0(
        "`manifest$versions_table` does not contain object_name = '",
        object_name,
        "'."
      ),
      call. = FALSE
    )
  }

  if (nrow(manifest_row) > 1L) {
    stop(
      paste0(
        "`manifest$versions_table` has multiple rows for object_name = '",
        object_name,
        "'."
      ),
      call. = FALSE
    )
  }

  pins::pin_read(
    board = board,
    name = manifest_row$pin_name[[1]],
    version = manifest_row$latest_pin_version[[1]]
  )
}

get_latest_pin_version <- function(board, pin_name) {

  available_pins <- pins::pin_list(board)

  if (!pin_name %in% available_pins) {
    stop(
      "Pin not found in board: ", pin_name, ". ",
      "Available pins are: ", paste(available_pins, collapse = ", "),
      call. = FALSE
    )
  }

  pin_versions <- pins::pin_versions(
    board = board,
    name  = pin_name
  )

  if (nrow(pin_versions) == 0L) {
    stop("No versions found for pin: ", pin_name, call. = FALSE)
  }

  if ("created" %in% names(pin_versions)) {
    pin_versions <- pin_versions[order(pin_versions$created), , drop = FALSE]
  }

  latest_version <- pin_versions$version[[nrow(pin_versions)]]

  latest_version
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
    pattern = "\\.rds$",
    full.names = TRUE
  )

  if (length(manifest_files) == 0L) {
    message("No manifest files found in: ", manifest_dir)
    return(NULL)
  }

  infer_manifest_date_from_filename <- function(file_path) {
    date_string <- stringr::str_extract(
      string = basename(file_path),
      pattern = "\\d{4}_\\d{2}_\\d{2}"
    )

    if (is.na(date_string)) {
      return(as.Date(NA))
    }

    as.Date(date_string, format = "%Y_%m_%d")
  }

  read_manifest_index <- function(file_path) {
    manifest <- readRDS(file_path)

    if (!is.list(manifest)) {
      stop(
        "Manifest file must contain a list-like object: ",
        file_path,
        call. = FALSE
      )
    }

    current_date_max <- manifest$current_date_max

    if (is.null(current_date_max)) {
      current_date_max <- infer_manifest_date_from_filename(file_path)
    }

    created_at <- manifest$created_at

    if (is.null(created_at)) {
      created_at <- as.POSIXct(NA)
    }

    tibble::tibble(
      manifest_file    = file_path,
      current_date_max = as.Date(current_date_max),
      created_at       = as.POSIXct(created_at)
    )
  }

  manifest_index <- purrr::map_dfr(
    manifest_files,
    read_manifest_index
  )

  if (nrow(manifest_index) == 0L) {
    message("Manifest files were found, but all were empty.")
    return(NULL)
  }

  if (all(is.na(manifest_index$current_date_max))) {
    stop(
      "No valid `current_date_max` could be found or inferred from manifest files.",
      call. = FALSE
    )
  }

  newest_manifest_file <- manifest_index %>%
    dplyr::arrange(
      dplyr::desc(current_date_max),
      dplyr::desc(created_at)
    ) %>%
    dplyr::slice(1L) %>%
    dplyr::pull(manifest_file)

  message("Newest manifest found: ", newest_manifest_file)

  readRDS(newest_manifest_file)
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
        paste(as.Date(overlap_dates), collapse = ", ")
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
        old_table = old_dados$rebalanceamento_tables$rebal_weights,
        new_table = dados_batch$rebalanceamento_tables$rebal_weights,
        table_name = "rebalanceamento_tables$rebal_weights"
      ),
      sectors = append_batch_table(
        old_table = old_dados$rebalanceamento_tables$sectors,
        new_table = dados_batch$rebalanceamento_tables$sectors,
        table_name = "rebalanceamento_tables$sectors"
      ),
      catalog = append_batch_table(
        old_table = old_dados$rebalanceamento_tables$catalog,
        new_table = dados_batch$rebalanceamento_tables$catalog,
        table_name = "rebalanceamento_tables$catalog"
      )
    ),
    comdinheiro_data = append_batch_table(
      old_table = old_dados$comdinheiro_data,
      new_table = dados_batch$comdinheiro_data,
      table_name = "comdinheiro_data"
    ),
    brokerage_data  = list(
      trade_data = append_batch_table(
        old_table = old_dados$brokerage_data$trade_data,
        new_table = dados_batch$brokerage_data$trade_data,
        table_name = "brokerage_data$trade_data"
      ),
      brokerage_notes_log = append_batch_table(
        old_table = old_dados$brokerage_data$brokerage_notes_log,
        new_table = dados_batch$brokerage_data$brokerage_notes_log,
        table_name = "brokerage_data$brokerage_notes_log"
      )
    ),
    split_inplit_data = append_batch_table(
      old_table = old_dados$split_inplit_data,
      new_table = dados_batch$split_inplit_data,
      table_name = "split_inplit_data"
    ),
    port_iniciais     = if (!is.null(dados_batch$port_iniciais)){
      dados_batch$port_iniciais
    } else {
      NULL
    }
  )

  out
}

bind_old_dados_gold <- function(
    old_dados_gold,
    evolved_portfolios,
    current_dates,
    initial_rebalancing_date = NULL,
    verbose = TRUE
) {

  ## Init -----------------------------------------------------------------------

  current_dates <- as.Date(current_dates)

  if (length(current_dates) == 0L || any(is.na(current_dates))) {
    stop("`current_dates` must contain valid Date values.", call. = FALSE)
  }

  current_dates <- sort(unique(current_dates))
  min_current_date <- min(current_dates)

  if (is.null(initial_rebalancing_date)) {
    initial_rebalancing_date <- as.Date(NA)
  } else {
    initial_rebalancing_date <- as.Date(initial_rebalancing_date)
  }

  if (!is.list(old_dados_gold) || is.data.frame(old_dados_gold)) {
    stop("`old_dados_gold` must be a list.", call. = FALSE)
  }

  if (!is.list(evolved_portfolios) || is.data.frame(evolved_portfolios) || length(evolved_portfolios) == 0L) {
    stop("`evolved_portfolios` must be a non-empty list.", call. = FALSE)
  }

  ## Helpers --------------------------------------------------------------------

  path_label <- function(path) {
    paste(path, collapse = "$")
  }

  get_date_range <- function(df) {
    if (!is.data.frame(df) || !"date" %in% names(df) || nrow(df) == 0L) {
      return(as.Date(character()))
    }

    as.Date(df$date)
  }

  check_old_dados_gold_basic_schema <- function(x) {
    if (!"paper" %in% names(x)) {
      stop("`old_dados_gold` must contain element `paper`.", call. = FALSE)
    }

    if (!"real" %in% names(x)) {
      stop("`old_dados_gold` must contain element `real`.", call. = FALSE)
    }

    if (!is.list(x$paper) || is.data.frame(x$paper)) {
      stop("`old_dados_gold$paper` must be a list.", call. = FALSE)
    }

    if (!is.list(x$real) || is.data.frame(x$real)) {
      stop("`old_dados_gold$real` must be a list.", call. = FALSE)
    }

    if (!"portfolio" %in% names(x$paper)) {
      stop("`old_dados_gold$paper` must contain element `portfolio`.", call. = FALSE)
    }

    if (!"portfolio" %in% names(x$real)) {
      stop("`old_dados_gold$real` must contain element `portfolio`.", call. = FALSE)
    }

    if (!is.data.frame(x$paper$portfolio)) {
      stop("`old_dados_gold$paper$portfolio` must be a data.frame.", call. = FALSE)
    }

    if (!is.data.frame(x$real$portfolio)) {
      stop("`old_dados_gold$real$portfolio` must be a data.frame.", call. = FALSE)
    }

    invisible(TRUE)
  }

  check_evolved_portfolios_basic_schema <- function(x) {
    for (nm in names(x)) {
      out <- x[[nm]]

      portfolio_label <- if (nzchar(nm)) nm else "<unnamed>"

      if (!is.list(out) || is.data.frame(out)) {
        stop(
          "`evolved_portfolios[[", portfolio_label, "]]` must be a list.",
          call. = FALSE
        )
      }

      if (!"paper" %in% names(out)) {
        stop(
          "`evolved_portfolios[[", portfolio_label, "]]` must contain element `paper`.",
          call. = FALSE
        )
      }

      if (!"real" %in% names(out)) {
        stop(
          "`evolved_portfolios[[", portfolio_label, "]]` must contain element `real`.",
          call. = FALSE
        )
      }

      if (!is.list(out$paper) || is.data.frame(out$paper)) {
        stop(
          "`evolved_portfolios[[", portfolio_label, "]]$paper` must be a list.",
          call. = FALSE
        )
      }

      if (!is.list(out$real) || is.data.frame(out$real)) {
        stop(
          "`evolved_portfolios[[", portfolio_label, "]]$real` must be a list.",
          call. = FALSE
        )
      }
    }

    invisible(TRUE)
  }

  is_simplified_old_dados_gold <- function(x) {
    paper_names <- names(x$paper)
    real_names <- names(x$real)

    identical(sort(paper_names), "portfolio") &&
      identical(sort(real_names), "portfolio")
  }

  standardize_old_paper_portfolio_for_binding <- function(df) {
    if (!is.data.frame(df) || nrow(df) == 0L) {
      return(df)
    }

    if (!"eop_weights" %in% names(df) && "weights" %in% names(df)) {
      df <- dplyr::rename(df, eop_weights = weights)
    } else if ("eop_weights" %in% names(df) && "weights" %in% names(df)) {
      legacy_diff <- abs(as.numeric(df$weights) - as.numeric(df$eop_weights))

      if (any(is.finite(legacy_diff) & legacy_diff > 1e-12, na.rm = TRUE)) {
        stop(
          "`old_dados_gold$paper$portfolio` contains both `weights` and `eop_weights` with different values.",
          call. = FALSE
        )
      }

      df <- dplyr::select(df, -weights)
    }

    required_cols <- c("date", "id", "cvm_code_type", "eop_weights")
    missing_cols <- setdiff(required_cols, names(df))

    if (length(missing_cols) > 0L) {
      stop(
        "`old_dados_gold$paper$portfolio` is missing column(s): ",
        paste(missing_cols, collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    df <- dplyr::mutate(
      df,
      date = as.Date(date),
      id = as.character(id),
      cvm_code_type = as.character(cvm_code_type),
      eop_weights = as.numeric(eop_weights)
    )

    if (any(is.na(df$date))) {
      stop("`old_dados_gold$paper$portfolio$date` contains invalid dates.", call. = FALSE)
    }

    df
  }

  standardize_old_real_portfolio_for_binding <- function(df) {
    if (!is.data.frame(df) || nrow(df) == 0L) {
      return(df)
    }

    if (!"eop_positions" %in% names(df) && "positions" %in% names(df)) {
      df <- dplyr::rename(df, eop_positions = positions)
    } else if ("eop_positions" %in% names(df) && "positions" %in% names(df)) {
      legacy_diff <- abs(as.numeric(df$positions) - as.numeric(df$eop_positions))

      if (any(is.finite(legacy_diff) & legacy_diff > 1e-12, na.rm = TRUE)) {
        stop(
          "`old_dados_gold$real$portfolio` contains both `positions` and `eop_positions` with different values.",
          call. = FALSE
        )
      }

      df <- dplyr::select(df, -positions)
    }

    required_cols <- c(
      "date",
      "id",
      "fund_name",
      "cvm_code_type",
      "eop_positions",
      "price"
    )

    missing_cols <- setdiff(required_cols, names(df))

    if (length(missing_cols) > 0L) {
      stop(
        "`old_dados_gold$real$portfolio` is missing column(s): ",
        paste(missing_cols, collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    df <- dplyr::mutate(
      df,
      date = as.Date(date),
      id = as.character(id),
      fund_name = as.character(fund_name),
      cvm_code_type = as.character(cvm_code_type),
      eop_positions = as.numeric(eop_positions),
      price = as.numeric(price)
    )

    if (any(is.na(df$date))) {
      stop("`old_dados_gold$real$portfolio$date` contains invalid dates.", call. = FALSE)
    }

    df
  }

  collect_dated_table_dates <- function(x) {
    if (is.data.frame(x)) {
      if (!"date" %in% names(x) || nrow(x) == 0L) {
        return(as.Date(character()))
      }

      return(as.Date(x$date))
    }

    if (!is.list(x) || is.data.frame(x)) {
      return(as.Date(character()))
    }

    out <- as.Date(character())

    for (nm in names(x)) {
      out <- c(out, collect_dated_table_dates(x[[nm]]))
    }

    out
  }

  validate_dated_tables <- function(x, path) {
    if (is.data.frame(x)) {
      if ("date" %in% names(x) && nrow(x) > 0L) {
        dates <- as.Date(x$date)

        if (any(is.na(dates))) {
          stop(
            "Table `",
            path_label(path),
            "` contains invalid dates.",
            call. = FALSE
          )
        }
      }

      return(invisible(TRUE))
    }

    if (is.list(x) && !is.data.frame(x)) {
      for (nm in names(x)) {
        validate_dated_tables(x[[nm]], c(path, nm))
      }
    }

    invisible(TRUE)
  }

  bind_if_available <- function(old_df, new_df, table_name) {
    if (is.null(old_df) && is.null(new_df)) {
      return(NULL)
    }

    if (is.null(old_df)) {
      return(new_df)
    }

    if (!is.data.frame(old_df)) {
      stop("Old table `", table_name, "` must be a data.frame.", call. = FALSE)
    }

    if (!"date" %in% names(old_df)) {
      stop("Old table `", table_name, "` must contain column `date`.", call. = FALSE)
    }

    old_df <- dplyr::mutate(old_df, date = as.Date(date))

    if (any(is.na(old_df$date))) {
      stop("Old table `", table_name, "` contains invalid dates.", call. = FALSE)
    }

    old_rows_to_discard <- old_df$date %in% current_dates

    if (any(old_rows_to_discard) && isTRUE(verbose)) {
      message(
        "Discarding ",
        sum(old_rows_to_discard),
        " old row(s) from `",
        table_name,
        "` because their date overlaps with `current_dates`."
      )
    }

    old_df <- old_df[!old_rows_to_discard, , drop = FALSE]

    if (is.null(new_df)) {
      return(old_df)
    }

    if (!is.data.frame(new_df)) {
      stop("New table `", table_name, "` must be a data.frame.", call. = FALSE)
    }

    if (nrow(new_df) == 0L) {
      return(dplyr::bind_rows(old_df, new_df))
    }

    if (!"date" %in% names(new_df)) {
      stop("New table `", table_name, "` must contain column `date`.", call. = FALSE)
    }

    new_df <- dplyr::mutate(new_df, date = as.Date(date))

    if (any(is.na(new_df$date))) {
      stop("New table `", table_name, "` contains invalid dates.", call. = FALSE)
    }

    dplyr::bind_rows(old_df, new_df)
  }

  get_nested_element <- function(x, path) {
    out <- x

    for (nm in path) {
      if (
        is.null(out) ||
        !is.list(out) ||
        is.data.frame(out) ||
        !nm %in% names(out)
      ) {
        return(NULL)
      }

      out <- out[[nm]]
    }

    out
  }

  stack_evolved_node <- function(path) {
    nodes <- lapply(
      evolved_portfolios,
      function(out) {
        get_nested_element(out, path)
      }
    )

    nodes <- nodes[!vapply(nodes, is.null, logical(1))]

    if (length(nodes) == 0L) {
      return(NULL)
    }

    are_data_frames <- vapply(nodes, is.data.frame, logical(1))

    are_lists <- vapply(
      nodes,
      function(x) {
        is.list(x) && !is.data.frame(x)
      },
      logical(1)
    )

    if (all(are_data_frames)) {
      return(dplyr::bind_rows(nodes))
    }

    if (all(are_lists)) {
      child_names <- unique(unlist(lapply(nodes, names), use.names = FALSE))

      if (length(child_names) == 0L) {
        return(list())
      }

      out <- vector("list", length(child_names))
      names(out) <- child_names

      for (nm in child_names) {
        out[[nm]] <- stack_evolved_node(c(path, nm))
      }

      return(out)
    }

    stop(
      "Mixed table/list structure at evolved path `",
      path_label(path),
      "`.",
      call. = FALSE
    )
  }

  bind_dados_gold_node <- function(old_node, new_node, path) {
    table_name <- path_label(path)

    if (is.null(new_node)) {
      return(old_node)
    }

    if (is.null(old_node)) {
      return(new_node)
    }

    old_is_df <- is.data.frame(old_node)
    new_is_df <- is.data.frame(new_node)

    if (old_is_df || new_is_df) {
      if (!old_is_df || !new_is_df) {
        stop(
          "Mixed table/list structure at path `",
          table_name,
          "`.",
          call. = FALSE
        )
      }

      if (identical(path, c("paper", "portfolio"))) {
        old_node <- standardize_old_paper_portfolio_for_binding(old_node)
      }

      if (identical(path, c("real", "portfolio"))) {
        old_node <- standardize_old_real_portfolio_for_binding(old_node)
      }

      return(
        bind_if_available(
          old_df = old_node,
          new_df = new_node,
          table_name = table_name
        )
      )
    }

    if (!is.list(old_node) || is.data.frame(old_node)) {
      stop("Old node `", table_name, "` must be a list.", call. = FALSE)
    }

    if (!is.list(new_node) || is.data.frame(new_node)) {
      stop("New node `", table_name, "` must be a list.", call. = FALSE)
    }

    child_names <- unique(c(names(old_node), names(new_node)))

    if (length(child_names) == 0L) {
      return(list())
    }

    out <- vector("list", length(child_names))
    names(out) <- child_names

    for (nm in child_names) {
      out[[nm]] <- bind_dados_gold_node(
        old_node = old_node[[nm]],
        new_node = new_node[[nm]],
        path = c(path, nm)
      )
    }

    out
  }

  assert_no_duplicate_key <- function(df, table_name, key_cols) {
    if (!is.data.frame(df) || nrow(df) == 0L) {
      return(invisible(TRUE))
    }

    missing_cols <- setdiff(key_cols, names(df))

    if (length(missing_cols) > 0L) {
      return(invisible(TRUE))
    }

    duplicated_rows <- duplicated(df[, key_cols, drop = FALSE])

    if (any(duplicated_rows)) {
      stop(
        "Table `",
        table_name,
        "` contains duplicated rows by key: ",
        paste(key_cols, collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    invisible(TRUE)
  }

  bind_metadata_runs <- function(old_dados_gold, evolved_portfolios, node_name, current_dates) {
    if (!node_name %in% c("workflow", "diagnostics")) {
      stop("`node_name` must be 'workflow' or 'diagnostics'.", call. = FALSE)
    }

    old_node <- old_dados_gold[[node_name]]

    if (
      is.null(old_node) ||
      !is.list(old_node) ||
      is.data.frame(old_node)
    ) {
      old_runs <- list()
    } else if ("runs" %in% names(old_node) && is.list(old_node$runs)) {
      old_runs <- old_node$runs
    } else {
      old_runs <- list(
        legacy = old_node
      )
    }

    new_runs <- purrr::imap(
      evolved_portfolios,
      function(out, portfolio_id) {
        if (!node_name %in% names(out)) {
          return(NULL)
        }

        out[[node_name]]
      }
    )

    new_runs <- new_runs[!vapply(new_runs, is.null, logical(1))]

    if (length(new_runs) > 0L) {
      run_suffix <- paste(format(as.Date(current_dates), "%Y%m%d"), collapse = "_")

      names(new_runs) <- paste(
        names(new_runs),
        run_suffix,
        sep = "__"
      )

      old_runs <- old_runs[!names(old_runs) %in% names(new_runs)]
    }

    list(
      current_dates = as.Date(current_dates),
      runs = c(old_runs, new_runs),
      latest = new_runs
    )
  }

  ## Validate old dados_gold ----------------------------------------------------

  check_old_dados_gold_basic_schema(old_dados_gold)
  check_evolved_portfolios_basic_schema(evolved_portfolios)

  old_dados_gold$paper$portfolio <- standardize_old_paper_portfolio_for_binding(
    old_dados_gold$paper$portfolio
  )

  old_dados_gold$real$portfolio <- standardize_old_real_portfolio_for_binding(
    old_dados_gold$real$portfolio
  )

  old_paper_dates <- get_date_range(old_dados_gold$paper$portfolio)
  old_real_dates <- get_date_range(old_dados_gold$real$portfolio)

  if (length(old_paper_dates) == 0L) {
    stop("`old_dados_gold$paper$portfolio` cannot be empty.", call. = FALSE)
  }

  if (length(old_real_dates) == 0L) {
    stop("`old_dados_gold$real$portfolio` cannot be empty.", call. = FALSE)
  }

  validate_dated_tables(old_dados_gold$paper, c("old_dados_gold", "paper"))
  validate_dated_tables(old_dados_gold$real, c("old_dados_gold", "real"))

  old_dated_dates <- c(
    collect_dated_table_dates(old_dados_gold$paper),
    collect_dated_table_dates(old_dados_gold$real)
  )

  old_dated_dates <- old_dated_dates[!is.na(old_dated_dates)]

  if (length(old_dated_dates) == 0L) {
    stop("`old_dados_gold` must contain at least one dated table.", call. = FALSE)
  }

  old_dados_gold_last_date <- max(old_dated_dates, na.rm = TRUE)

  old_dates_overlapping_current_dates <- old_dated_dates[
    old_dated_dates %in% current_dates
  ]

  if (length(old_dates_overlapping_current_dates) > 0L && isTRUE(verbose)) {
    message(
      "`old_dados_gold` contains date(s) overlapping with `current_dates`: ",
      paste(sort(unique(old_dates_overlapping_current_dates)), collapse = ", "),
      ". Old rows for these dates will be discarded table-by-table and replaced by newly evolved rows."
    )
  }

  simplified_old <- is_simplified_old_dados_gold(old_dados_gold)

  if (isTRUE(simplified_old)) {
    if (
      length(initial_rebalancing_date) != 1L ||
      is.na(initial_rebalancing_date)
    ) {
      stop(
        "`initial_rebalancing_date` must be supplied when `old_dados_gold` has the simplified initial structure.",
        call. = FALSE
      )
    }

    if (old_dados_gold_last_date != initial_rebalancing_date) {
      stop(
        "Simplified `old_dados_gold` is only allowed when its last date equals ",
        "`initial_rebalancing_date`. old_dados_gold_last_date = ",
        old_dados_gold_last_date,
        ", initial_rebalancing_date = ",
        initial_rebalancing_date,
        ".",
        call. = FALSE
      )
    }
  }

  ## Stack evolved dados_gold ---------------------------------------------------

  new_dados_gold <- list(
    paper = stack_evolved_node(c("paper")),
    real = stack_evolved_node(c("real"))
  )

  if (is.null(new_dados_gold$paper)) {
    stop("Could not stack `paper` from `evolved_portfolios`.", call. = FALSE)
  }

  if (is.null(new_dados_gold$real)) {
    stop("Could not stack `real` from `evolved_portfolios`.", call. = FALSE)
  }

  ## Bind old + new -------------------------------------------------------------

  out <- list(
    paper = bind_dados_gold_node(
      old_node = old_dados_gold$paper,
      new_node = new_dados_gold$paper,
      path = c("paper")
    ),
    real = bind_dados_gold_node(
      old_node = old_dados_gold$real,
      new_node = new_dados_gold$real,
      path = c("real")
    ),
    workflow = bind_metadata_runs(
      old_dados_gold = old_dados_gold,
      evolved_portfolios = evolved_portfolios,
      node_name = "workflow",
      current_dates = current_dates
    ),
    diagnostics = bind_metadata_runs(
      old_dados_gold = old_dados_gold,
      evolved_portfolios = evolved_portfolios,
      node_name = "diagnostics",
      current_dates = current_dates
    )
  )

  ## Critical duplicate checks -------------------------------------------------

  assert_no_duplicate_key(
    df = out$paper$portfolio,
    table_name = "paper$portfolio",
    key_cols = c("date", "id", "cvm_code_type")
  )

  assert_no_duplicate_key(
    df = out$real$portfolio,
    table_name = "real$portfolio",
    key_cols = c("date", "id", "fund_name", "cvm_code_type")
  )

  out
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
