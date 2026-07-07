run_test_dados_bronze <- function(
    dados_bronze,
    broker_accounts,
    current_dates,
    initial_rebalancing_date,
    weight_tolerance = 1e-3,
    index_weight_tolerance = 0.5,
    min_proventos_zero_share = 0.50,
    min_population_share = 0.75,
    return_tolerance = 1e-4,
    capital_gain_tolerance = 1e-4,
    max_missing_share = 0.05,
    etfs = c("BOVA11", "BOVV11", "DIVO11", "SMAL11", "ISUS11", "LFTS11"),
    cash_tickers = c("BRL Curncy"),
    verbose = TRUE
) {
  if (!is.list(dados_bronze)) {
    stop("`dados_bronze` must be a list.", call. = FALSE)
  }

  if (!all(c("rebalanceamento_tables", "comdinheiro_data") %in% names(dados_bronze))) {
    stop(
      "`dados_bronze` must contain `rebalanceamento_tables` and `comdinheiro_data`.",
      call. = FALSE
    )
  }

  #Extract----------------------------------------------------------------------
  rebal_weights <- dados_bronze$rebalanceamento_tables$rebal_weights
  sectors <- dados_bronze$rebalanceamento_tables$sectors
  catalog <- dados_bronze$rebalanceamento_tables$catalog
  comdinheiro_data <- dados_bronze$comdinheiro_data
  brokerage_data <- dados_bronze$brokerage_data
  split_inplit_data <- dados_bronze$split_inplit_data
  other_events_data <- dados_bronze$other_events_data
  port_iniciais     <- dados_bronze$port_iniciais

  validate_bronze_table_types(
    rebal_weights = rebal_weights,
    sectors = sectors,
    catalog = catalog,
    comdinheiro_data = comdinheiro_data,
    split_inplit_data = split_inplit_data,
    other_events_data = other_events_data
  )

  validate_no_fully_na_columns(
    df = rebal_weights,
    object_name = "rebal_weights"
  )

  validate_no_fully_na_columns(
    df = sectors,
    object_name = "sectors"
  )

  validate_no_fully_na_columns(
    df = catalog,
    object_name = "catalog",
    excluded_cols = c("dates_cancel")
  )

  validate_no_fully_na_columns(
    df = comdinheiro_data,
    object_name = "comdinheiro_data",
    excluded_cols = c(
      "btc_estoque",
      "btc_novos_contratos",
      "btc_taxa_media",
      "w_ibov",
      "w_idiv",
      "w_smll",
      "w_ise"
    )
  )

  validate_no_fully_na_columns(
    df = split_inplit_data,
    object_name = "split_inplit_data"
  )

  validate_no_fully_na_columns(
    df = other_events_data,
    object_name = "other_events_data"
  )



  validate_rebal_weights_sum(
    rebal_weights = rebal_weights,
    tolerance = weight_tolerance
  )

  validate_rebal_tickers_in_sectors(
    rebal_weights = rebal_weights,
    sectors = sectors,
    etfs = etfs,
    cash_tickers = cash_tickers
  )

  validate_rebal_tickers_in_catalog(
    rebal_weights = rebal_weights,
    catalog = catalog,
    etfs = etfs,
    cash_tickers = cash_tickers
  )

  validate_rebal_cvm_code_types_in_comdinheiro_intervals(
    rebal_weights = rebal_weights,
    catalog = catalog,
    comdinheiro_data = comdinheiro_data,
    etfs = etfs,
    cash_tickers = cash_tickers,
    max_missing_share = max_missing_share
  )

  validate_proventos_are_mostly_zero(
    comdinheiro_data = comdinheiro_data,
    min_zero_share = min_proventos_zero_share
  )

  validate_comdinheiro_population(
    comdinheiro_data = comdinheiro_data,
    min_population_share = min_population_share
  )

  validate_index_weights_sum(
    comdinheiro_data = comdinheiro_data,
    tolerance = index_weight_tolerance
  )

  validate_ret_1d_matches_price_adj_variation(
    comdinheiro_data = comdinheiro_data,
    tolerance = return_tolerance
  )

  validate_ret_1d_matches_capital_gain_plus_proventos(
    comdinheiro_data = comdinheiro_data,
    tolerance = capital_gain_tolerance,
    max_unmatched_share = max_missing_share
  )

  validate_brokerage_data(
    brokerage_data = brokerage_data,
    broker_accounts = broker_accounts,
    current_dates = current_dates
  )

  validate_port_iniciais(
    port_iniciais = port_iniciais,
    broker_accounts = broker_accounts,
    current_dates = current_dates,
    initial_rebalancing_date = initial_rebalancing_date,
    etfs = etfs
  )

  if (isTRUE(verbose)) {
    message("All Bronze Data quality tests passed.")
  }

  invisible(TRUE)
}

#Helpers----------------------------------------------------------------------
validate_bronze_table_types <- function(
    rebal_weights,
    sectors,
    catalog,
    comdinheiro_data,
    split_inplit_data,
    other_events_data
) {
  required_rebal_cols <- c("date", "id", "legacy_ticker", "weights")
  required_sector_cols <- c("date", "legacy_ticker")
  required_catalog_cols <- c("date", "tickers", "cvm_code_type")
  required_comd_cols <- c(
    "date",
    "legacy_ticker",
    "ret_1d",
    "proventos",
    "price",
    "price_adj",
    "volume",
    "n_shares",
    "w_ibov",
    "w_idiv",
    "w_smll",
    "w_ise"
  )
  required_split_inplit_cols <- c("date", "legacy_ticker", "cvm_code_type", "split_factor")
  required_other_events_cols <- c("date", "old_legacy_ticker", "new_legacy_ticker",
                                  "old_cvm_code_type", "new_cvm_code_type", "adj_factor")

  check_required_cols(rebal_weights, required_rebal_cols, "rebal_weights")
  check_required_cols(sectors, required_sector_cols, "sectors")
  check_required_cols(catalog, required_catalog_cols, "catalog")
  check_required_cols(comdinheiro_data, required_comd_cols, "comdinheiro_data")
  check_required_cols(split_inplit_data, required_split_inplit_cols, "split_inplit_data")
  check_required_cols(other_events_data, required_other_events_cols, "other_events_data")


  if (!inherits(rebal_weights$date, "Date")) {
    stop("`rebal_weights$date` must be Date.", call. = FALSE)
  }

  if (!inherits(sectors$date, "Date")) {
    stop("`sectors$date` must be Date.", call. = FALSE)
  }

  if (!inherits(catalog$date, "Date")) {
    stop("`catalog$date` must be Date.", call. = FALSE)
  }

  if (!inherits(comdinheiro_data$date, "Date")) {
    stop("`comdinheiro_data$date` must be Date.", call. = FALSE)
  }

  if (!inherits(split_inplit_data$date, "Date")) {
    stop("`split_inplit_data$date` must be Date.", call. = FALSE)
  }

  if (!inherits(other_events_data$date, "Date")) {
    stop("`other_events_data$date` must be Date.", call. = FALSE)
  }

  numeric_cols <- c(
    "ret_1d",
    "proventos",
    "price",
    "price_adj",
    "volume",
    "n_shares",
    "w_ibov",
    "w_idiv",
    "w_smll",
    "w_ise"
  )

  non_numeric_cols <- numeric_cols[
    !vapply(comdinheiro_data[numeric_cols], is.numeric, logical(1))
  ]

  if (length(non_numeric_cols) > 0) {
    stop(
      paste0(
        "`comdinheiro_data` has non-numeric columns: ",
        paste(non_numeric_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (!is.numeric(rebal_weights$weights)) {
    stop("`rebal_weights$weights` must be numeric.", call. = FALSE)
  }

  if (!is.numeric(split_inplit_data$split_factor)) {
    stop("`split_inplit_data$split_factor` must be numeric.", call. = FALSE)
  }

  if (!is.numeric(other_events_data$adj_factor)) {
    stop("`other_events_data$adj_factor` must be numeric.", call. = FALSE)
  }


  invisible(TRUE)
}

check_required_cols <- function(df, required_cols, object_name) {
  missing_cols <- setdiff(required_cols, names(df))

  if (length(missing_cols) > 0) {
    stop(
      paste0(
        "`",
        object_name,
        "` is missing columns: ",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

validate_no_fully_na_columns <- function(
    df,
    object_name,
    excluded_cols = character()
) {
  cols_to_check <- setdiff(names(df), excluded_cols)

  fully_na_cols <- df %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(cols_to_check),
        ~ all(is.na(.x))
      )
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "column",
      values_to = "fully_na"
    ) %>%
    dplyr::filter(fully_na) %>%
    dplyr::pull(column)

  if (length(fully_na_cols) > 0) {
    stop(
      paste0(
        "`",
        object_name,
        "` has columns with only NAs: ",
        paste(fully_na_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

validate_rebal_weights_sum <- function(
    rebal_weights,
    tolerance = 1e-3
) {
  bad_weights <- rebal_weights %>%
    dplyr::group_by(date, id) %>%
    dplyr::summarise(
      weight_sum = sum(weights, na.rm = FALSE),
      has_na_weight = any(is.na(weights)),
      .groups = "drop"
    ) %>%
    dplyr::filter(
      has_na_weight | abs(weight_sum - 1) > tolerance
    )

  if (nrow(bad_weights) > 0) {
    stop(
      paste0(
        "Some rebalancing portfolios have invalid weights. ",
        "First issue: id = ",
        bad_weights$id[1],
        ", date = ",
        bad_weights$date[1],
        ", weight_sum = ",
        round(bad_weights$weight_sum[1], 8),
        "."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

validate_rebal_tickers_in_sectors <- function(
    rebal_weights,
    sectors,
    etfs,
    cash_tickers
) {

  excluded_tickers <- c(etfs, cash_tickers)

  sector_tickers <- sectors %>%
    dplyr::mutate(
      date = as.Date(date),
      legacy_ticker = as.character(legacy_ticker)
    ) %>%
    dplyr::distinct(date, legacy_ticker)

  missing_sector_tickers <- rebal_weights %>%
    dplyr::mutate(
      date = as.Date(date),
      legacy_ticker = as.character(legacy_ticker)
    ) %>%
    dplyr::filter(!legacy_ticker %in% excluded_tickers) %>%
    dplyr::distinct(date, legacy_ticker) %>%
    dplyr::anti_join(
      sector_tickers,
      by = c("date", "legacy_ticker")
    )

  if (nrow(missing_sector_tickers) > 0) {
    stop(
      paste0(
        "Some non-ETF/cash rebalancing tickers are missing from sectors. ",
        "First missing ticker: ",
        missing_sector_tickers$legacy_ticker[1],
        " at date ",
        missing_sector_tickers$date[1],
        "."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

validate_rebal_tickers_in_catalog <- function(
    rebal_weights,
    catalog,
    etfs,
    cash_tickers
) {
  excluded_tickers <- c(etfs, cash_tickers)

  catalog_lookup <- catalog %>%
    dplyr::mutate(
      newest_trading_date = as.Date(newest_trading_date),
      dates_cancel = as.Date(dates_cancel)
    ) %>%
    dplyr::arrange(
      date,
      tickers,
      dplyr::desc(is.na(dates_cancel)),
      dplyr::desc(newest_trading_date)
    ) %>%
    dplyr::group_by(date, tickers) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, tickers, cvm_code_type)

  duplicated_catalog_keys <- catalog %>%
    dplyr::count(date, tickers) %>%
    dplyr::filter(n > 1)

  if (nrow(duplicated_catalog_keys) > 0) {

    duplicated_examples <- catalog %>%
      dplyr::semi_join(
        duplicated_catalog_keys,
        by = c("date", "tickers")
      ) %>%
      dplyr::arrange(date, tickers) %>%
      dplyr::group_by(date, tickers) %>%
      dplyr::summarise(
        cvm_code_types = paste(unique(cvm_code_type), collapse = ", "),
        newest_trading_dates = paste(unique(newest_trading_date), collapse = ", "),
        dates_cancel_values = paste(unique(dates_cancel), collapse = ", "),
        .groups = "drop"
      )

    warning(
      paste0(
        "Catalog has duplicated date + ticker mappings. ",
        "Resolving using: dates_cancel == NA first, then largest newest_trading_date.\n\n",
        paste0(
          "- date = ",
          duplicated_examples$date,
          " | ticker = ",
          duplicated_examples$tickers,
          " | cvm_code_types = ",
          duplicated_examples$cvm_code_types,
          " | newest_trading_dates = ",
          duplicated_examples$newest_trading_dates,
          " | dates_cancel = ",
          duplicated_examples$dates_cancel_values,
          collapse = "\n"
        )
      ),
      call. = FALSE
    )
  }

  missing_catalog_tickers <- rebal_weights %>%
    dplyr::mutate(
      date = as.Date(date),
      legacy_ticker = as.character(legacy_ticker)
    ) %>%
    dplyr::filter(!legacy_ticker %in% excluded_tickers) %>%
    dplyr::distinct(date, legacy_ticker) %>%
    dplyr::anti_join(
      catalog_lookup,
      by = c("date" = "date", "legacy_ticker" = "tickers")
    )

  if (nrow(missing_catalog_tickers) > 0) {
    stop(
      paste0(
        "Some non-ETF/cash rebalancing tickers are missing from catalog. ",
        "First missing ticker: ",
        missing_catalog_tickers$legacy_ticker[1],
        " at date ",
        missing_catalog_tickers$date[1],
        "."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

validate_rebal_cvm_code_types_in_comdinheiro_intervals <- function(
    rebal_weights,
    catalog,
    comdinheiro_data,
    etfs,
    cash_tickers,
    max_missing_share = 0.01
) {

  excluded_tickers <- c(etfs, cash_tickers)

  rebalancing_dates <- sort(unique(as.Date(rebal_weights$date)))

  intervals <- data.frame(
    rebalance_date = rebalancing_dates,
    next_rebalance_date = dplyr::lead(rebalancing_dates),
    stringsAsFactors = FALSE
  )

  catalog_lookup <- catalog %>%
    dplyr::mutate(
      date = as.Date(date),
      tickers = as.character(tickers),
      newest_trading_date = as.Date(newest_trading_date),
      dates_cancel = as.Date(dates_cancel)
    ) %>%
    dplyr::arrange(
      date,
      tickers,
      dplyr::desc(is.na(dates_cancel)),
      dplyr::desc(newest_trading_date)
    ) %>%
    dplyr::group_by(date, tickers) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, tickers, cvm_code_type)

  missing_by_interval <- purrr::pmap_dfr(
    intervals,
    function(rebalance_date, next_rebalance_date) {
      interval_dates <- comdinheiro_data %>%
        dplyr::filter(
          date >= rebalance_date,
          if (is.na(next_rebalance_date)) {
            TRUE
          } else {
            date < next_rebalance_date
          }
        ) %>%
        dplyr::distinct(date) %>%
        dplyr::pull(date) %>%
        sort()

      if (length(interval_dates) == 0) {
        return(
          data.frame(
            rebalance_date = as.Date(rebalance_date),
            date = as.Date(NA),
            missing_cvm_code_type = NA_character_,
            n_expected = NA_integer_,
            n_missing = NA_integer_,
            missing_share = NA_real_,
            issue = "no_comdinheiro_dates_in_interval",
            stringsAsFactors = FALSE
          )
        )
      }

      expected_cvm_code_types <- rebal_weights %>%
        dplyr::filter(date == rebalance_date) %>%
        dplyr::filter(!legacy_ticker %in% excluded_tickers) %>%
        dplyr::distinct(legacy_ticker) %>%
        dplyr::left_join(
          catalog_lookup %>%
            dplyr::filter(date == rebalance_date) %>%
            dplyr::select(tickers, cvm_code_type),
          by = c("legacy_ticker" = "tickers")
        ) %>%
        dplyr::filter(!is.na(cvm_code_type)) %>%
        dplyr::distinct(cvm_code_type) %>%
        dplyr::pull(cvm_code_type)

      n_expected <- length(expected_cvm_code_types)

      if (n_expected == 0) {
        return(
          data.frame(
            rebalance_date = as.Date(rebalance_date),
            date = as.Date(NA),
            missing_cvm_code_type = NA_character_,
            n_expected = 0L,
            n_missing = NA_integer_,
            missing_share = NA_real_,
            issue = "no_expected_cvm_code_types",
            stringsAsFactors = FALSE
          )
        )
      }

      comdinheiro_with_cvm <- comdinheiro_data %>%
        dplyr::filter(date %in% interval_dates) %>%
        dplyr::left_join(
          catalog_lookup %>%
            dplyr::filter(date == rebalance_date) %>%
            dplyr::select(tickers, cvm_code_type),
          by = c("legacy_ticker" = "tickers")
        )

      purrr::map_dfr(
        interval_dates,
        function(date_i) {
          available_cvm_code_types <- comdinheiro_with_cvm %>%
            dplyr::filter(date == date_i) %>%
            dplyr::filter(!is.na(cvm_code_type)) %>%
            dplyr::distinct(cvm_code_type) %>%
            dplyr::pull(cvm_code_type)

          missing_cvm_code_types <- setdiff(
            expected_cvm_code_types,
            available_cvm_code_types
          )

          n_missing <- length(missing_cvm_code_types)

          if (n_missing == 0) {
            return(
              data.frame(
                rebalance_date = as.Date(character()),
                date = as.Date(character()),
                missing_cvm_code_type = character(),
                n_expected = integer(),
                n_missing = integer(),
                missing_share = numeric(),
                issue = character(),
                stringsAsFactors = FALSE
              )
            )
          }

          data.frame(
            rebalance_date = as.Date(rebalance_date),
            date = as.Date(date_i),
            missing_cvm_code_type = missing_cvm_code_types,
            n_expected = n_expected,
            n_missing = n_missing,
            missing_share = n_missing / n_expected,
            issue = "missing_cvm_code_type_in_comdinheiro",
            stringsAsFactors = FALSE
          )
        }
      )
    }
  )

  if (nrow(missing_by_interval) == 0 || !"issue" %in% names(missing_by_interval)) {
    invisible(TRUE)
  }

  no_interval_data <- missing_by_interval %>%
    dplyr::filter(issue == "no_comdinheiro_dates_in_interval")

  if (nrow(no_interval_data) > 0) {
    stop(
      paste0(
        "Some rebalance intervals have no Comdinheiro dates. ",
        "First issue: rebalance_date = ",
        no_interval_data$rebalance_date[1],
        "."
      ),
      call. = FALSE
    )
  }

  no_expected_data <- missing_by_interval %>%
    dplyr::filter(issue == "no_expected_cvm_code_types")

  if (nrow(no_expected_data) > 0) {
    stop(
      paste0(
        "Some rebalance intervals have no expected cvm_code_types after catalog translation. ",
        "First issue: rebalance_date = ",
        no_expected_data$rebalance_date[1],
        "."
      ),
      call. = FALSE
    )
  }

  missing_summary <- missing_by_interval %>%
    dplyr::filter(issue == "missing_cvm_code_type_in_comdinheiro") %>%
    dplyr::left_join(
      rebal_weights %>%
        dplyr::filter(!legacy_ticker %in% excluded_tickers) %>%
        dplyr::distinct(rebalance_date = date, legacy_ticker) %>%
        dplyr::left_join(
          catalog_lookup %>%
            dplyr::select(date, tickers, cvm_code_type),
          by = c("rebalance_date" = "date", "legacy_ticker" = "tickers")
        ) %>%
        dplyr::filter(!is.na(cvm_code_type)) %>%
        dplyr::distinct(rebalance_date, legacy_ticker, cvm_code_type),
      by = c(
        "rebalance_date",
        "missing_cvm_code_type" = "cvm_code_type"
      )
    ) %>%
    dplyr::group_by(rebalance_date, date) %>%
    dplyr::summarise(
      n_expected = dplyr::first(n_expected),
      n_missing = dplyr::n_distinct(missing_cvm_code_type),
      n_affected_tickers = dplyr::n_distinct(legacy_ticker, na.rm = TRUE),
      missing_share = n_missing / n_expected,
      missing_examples = paste(
        utils::head(
          unique(
            paste0(
              missing_cvm_code_type,
              " [",
              legacy_ticker,
              "]"
            )
          ),
          15
        ),
        collapse = ", "
      ),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(missing_share), rebalance_date, date)

  format_missing_summary <- function(x, max_rows = 20) {
    x_print <- x %>%
      dplyr::slice_head(n = max_rows) %>%
      dplyr::mutate(
        line = paste0(
          "- rebalance_date = ", rebalance_date,
          " | date = ", date,
          " | n_missing = ", n_missing,
          "/", n_expected,
          " | missing_share = ", round(missing_share, 4),
          " | n_affected_tickers = ", n_affected_tickers,
          " | examples = ", missing_examples
        )
      ) %>%
      dplyr::pull(line)

    extra_rows <- nrow(x) - length(x_print)

    if (extra_rows > 0) {
      x_print <- c(
        x_print,
        paste0("- ... plus ", extra_rows, " additional date-level issues.")
      )
    }

    paste(x_print, collapse = "\n")
  }

  excessive_missing <- missing_summary %>%
    dplyr::filter(missing_share > max_missing_share)

  if (nrow(excessive_missing) > 0) {
    stop(
      paste0(
        "Some portfolio cvm_code_types are missing from Comdinheiro data above tolerance.\n",
        "Max allowed missing_share = ",
        max_missing_share,
        ". Worst observed missing_share = ",
        round(max(excessive_missing$missing_share, na.rm = TRUE), 4),
        ".\n\n",
        "Problematic date-level cases:\n",
        format_missing_summary(excessive_missing, max_rows = 20)
      ),
      call. = FALSE
    )
  }

  if (nrow(missing_summary) > 0) {
    warning(
      paste0(
        "Some portfolio cvm_code_types are missing from Comdinheiro data, ",
        "but missingness is within tolerance.\n",
        "Max allowed missing_share = ",
        max_missing_share,
        ". Worst observed missing_share = ",
        round(max(missing_summary$missing_share, na.rm = TRUE), 4),
        ".\n\n",
        "Date-level cases:\n",
        format_missing_summary(missing_summary, max_rows = 20)
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

validate_proventos_are_mostly_zero <- function(
    comdinheiro_data,
    min_zero_share = 0.90
) {
  proventos_check <- comdinheiro_data %>%
    dplyr::filter(!is.na(proventos)) %>%
    dplyr::summarise(
      zero_share = mean(proventos == 0),
      n_obs = dplyr::n(),
      .groups = "drop"
    )

  if (nrow(proventos_check) == 0 || proventos_check$n_obs == 0) {
    stop("`proventos` has no valid observations.", call. = FALSE)
  }

  if (proventos_check$zero_share < min_zero_share) {
    stop(
      paste0(
        "`proventos` does not look sparse enough. Zero share = ",
        round(proventos_check$zero_share, 4),
        ", expected at least ",
        min_zero_share,
        "."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

validate_comdinheiro_population <- function(
    comdinheiro_data,
    min_population_share = 0.80
) {
  excluded_cols <- c(
    "date",
    "legacy_ticker",
    "cia_name",
    "proventos_date",
    "cvm_code",
    "cvm_code_full",
    "btc_estoque",
    "btc_novos_contratos",
    "btc_taxa_media",
    "w_ibov",
    "w_idiv",
    "w_smll",
    "w_ise"
  )

  cols_to_check <- setdiff(names(comdinheiro_data), excluded_cols)

  population_check <- comdinheiro_data %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(cols_to_check),
        ~ mean(!is.na(.x))
      )
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "column",
      values_to = "population_share"
    ) %>%
    dplyr::filter(population_share < min_population_share)

  if (nrow(population_check) > 0) {
    stop(
      paste0(
        "Some Comdinheiro columns are insufficiently populated. ",
        "First issue: column = ",
        population_check$column[1],
        ", population_share = ",
        round(population_check$population_share[1], 4),
        "."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

validate_index_weights_sum <- function(
    comdinheiro_data,
    tolerance = 0.5
) {
  weight_cols <- c("w_ibov", "w_idiv", "w_smll", "w_ise")
  weight_cols <- intersect(weight_cols, names(comdinheiro_data))

  bad_index_weights <- comdinheiro_data %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(weight_cols),
        ~ sum(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(weight_cols),
      names_to = "index_weight_col",
      values_to = "weight_sum"
    ) %>%
    dplyr::filter(
      !is.na(weight_sum),
      weight_sum > 0,
      abs(weight_sum - 100) > tolerance
    )

  if (nrow(bad_index_weights) > 0) {
    stop(
      paste0(
        "Some benchmark weights do not sum to 1. ",
        "First issue: date = ",
        bad_index_weights$date[1],
        ", column = ",
        bad_index_weights$index_weight_col[1],
        ", sum = ",
        round(bad_index_weights$weight_sum[1], 8),
        "."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

validate_ret_1d_matches_price_adj_variation <- function(
    comdinheiro_data,
    tolerance = 1e-4
) {
  return_check <- comdinheiro_data %>%
    dplyr::mutate(
      date = as.Date(date),
      legacy_ticker = as.character(legacy_ticker)
    ) %>%
    dplyr::arrange(legacy_ticker, date) %>%
    dplyr::group_by(legacy_ticker) %>%
    dplyr::mutate(
      price_adj_lag = dplyr::lag(price_adj),
      implied_ret_1d = price_adj / price_adj_lag - 1,
      ret_1d_decimal = ret_1d / 100,
      abs_error = abs(ret_1d_decimal - implied_ret_1d)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      !is.na(price_adj_lag),
      !is.na(implied_ret_1d),
      !is.na(ret_1d_decimal),
      abs_error > tolerance
    )

  if (nrow(return_check) > 0) {
    warning(
      paste0(
        "`ret_1d` does not match adjusted price variation. ",
        "First issue: ticker = ",
        return_check$legacy_ticker[1],
        ", date = ",
        return_check$date[1],
        ", ret_1d_decimal = ",
        round(return_check$ret_1d_decimal[1], 8),
        ", implied_ret_1d = ",
        round(return_check$implied_ret_1d[1], 8),
        "."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

validate_ret_1d_matches_capital_gain_plus_proventos <- function(
    comdinheiro_data,
    tolerance = 0.001,
    max_unmatched_share = 0.05
) {

  capital_gain_check <- comdinheiro_data %>%
    dplyr::mutate(
      date = as.Date(date),
      legacy_ticker = as.character(legacy_ticker),
      proventos = dplyr::coalesce(proventos, 0)
    ) %>%
    dplyr::arrange(legacy_ticker, date) %>%
    dplyr::group_by(legacy_ticker) %>%
    dplyr::mutate(
      price_lag = dplyr::lag(price),
      implied_total_ret = (price - price_lag + proventos) / price_lag,
      ret_1d_decimal = ret_1d / 100,
      abs_error = abs(ret_1d_decimal - implied_total_ret)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      !is.na(price_lag),
      price_lag > 0,
      !is.na(implied_total_ret),
      !is.na(ret_1d_decimal)
    )

  n_tested <- nrow(capital_gain_check)

  if (n_tested == 0) {
    stop(
      "No valid observations available to validate `ret_1d` against capital gain plus proventos.",
      call. = FALSE
    )
  }

  unmatched_obs <- capital_gain_check %>%
    dplyr::filter(abs_error > tolerance)

  n_unmatched <- nrow(unmatched_obs)
  unmatched_share <- n_unmatched / n_tested

  if (unmatched_share > max_unmatched_share) {
    problematic_tickers <- unmatched_obs %>%
      dplyr::arrange(dplyr::desc(abs_error)) %>%
      dplyr::group_by(legacy_ticker) %>%
      dplyr::summarise(
        n_issues = dplyr::n(),
        max_abs_error = max(abs_error, na.rm = TRUE),
        first_problem_date = min(date, na.rm = TRUE),
        examples = paste0(
          utils::head(
            paste0(
              date,
              " | ret_1d_decimal = ", round(ret_1d_decimal, 8),
              " | implied_total_ret = ", round(implied_total_ret, 8),
              " | abs_error = ", round(abs_error, 8),
              " | proventos = ", round(proventos, 8),
              " | price = ", round(price, 8),
              " | price_lag = ", round(price_lag, 8),
              " | price_adj = ", round(price_adj, 8)
            ),
            3
          ),
          collapse = " || "
        ),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(max_abs_error))

    stop(
      paste0(
        "`ret_1d` does not match capital gain plus proventos above tolerance. ",
        "Unmatched share = ",
        round(unmatched_share, 4),
        ", max_unmatched_share = ",
        max_unmatched_share,
        ". Problematic tickers:\n",
        paste0(
          problematic_tickers$legacy_ticker,
          " | n_issues = ",
          problematic_tickers$n_issues,
          " | max_abs_error = ",
          round(problematic_tickers$max_abs_error, 8),
          " | first_problem_date = ",
          problematic_tickers$first_problem_date,
          " | examples: ",
          problematic_tickers$examples,
          collapse = "\n"
        )
      ),
      call. = FALSE
    )
  }

  if (n_unmatched > 0) {
    problematic_tickers <- unmatched_obs %>%
      dplyr::arrange(dplyr::desc(abs_error)) %>%
      dplyr::group_by(legacy_ticker) %>%
      dplyr::summarise(
        n_issues = dplyr::n(),
        max_abs_error = max(abs_error, na.rm = TRUE),
        first_problem_date = min(date, na.rm = TRUE),
        examples = paste0(
          utils::head(
            paste0(
              date,
              " | ret_1d_decimal = ", round(ret_1d_decimal, 8),
              " | implied_total_ret = ", round(implied_total_ret, 8),
              " | abs_error = ", round(abs_error, 8),
              " | proventos = ", round(proventos, 8),
              " | price = ", round(price, 8),
              " | price_lag = ", round(price_lag, 8),
              " | price_adj = ", round(price_adj, 8)
            ),
            3
          ),
          collapse = " || "
        ),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(max_abs_error))

    warning(
      paste0(
        "`ret_1d` has some mismatches against capital gain plus proventos, ",
        "but they are within the allowed unmatched share. ",
        "Unmatched observations = ",
        n_unmatched,
        "/",
        n_tested,
        " (",
        round(unmatched_share, 4),
        "). Problematic tickers:\n",
        paste0(
          problematic_tickers$legacy_ticker,
          " | n_issues = ",
          problematic_tickers$n_issues,
          " | max_abs_error = ",
          round(problematic_tickers$max_abs_error, 8),
          " | first_problem_date = ",
          problematic_tickers$first_problem_date,
          " | examples: ",
          problematic_tickers$examples,
          collapse = "\n"
        )
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


validate_brokerage_data <- function(
    brokerage_data,
    current_dates,
    broker_accounts,
    brokerage_fee_bps = 5.5,
    max_no_trade_days_per_week = 2L,
    volume_tolerance = 1e-8,
    fee_tolerance = 1e-8
) {

  ## Initial setup--------------------------------------------------------------

  testthat::expect_true(
    is.list(brokerage_data),
    info = "`brokerage_data` must be a list."
  )

  testthat::expect_true(
    all(c("brokerage_notes_log", "trade_data") %in% names(brokerage_data)),
    info = "`brokerage_data` must contain `brokerage_notes_log` and `trade_data`."
  )

  brokerage_notes_log <- brokerage_data$brokerage_notes_log
  trade_data <- brokerage_data$trade_data

  current_dates <- as.Date(current_dates)

  ## Test brokerage_notes_log structure-----------------------------------------

  expected_log_cols <- c(
    "email_index",
    "received_datetime",
    "date",
    "sender",
    "subject",
    "attachment_index",
    "attachment_name",
    "saved_path"
  )

  testthat::expect_s3_class(
    brokerage_notes_log,
    "data.frame"
  )

  testthat::expect_true(
    all(expected_log_cols %in% names(brokerage_notes_log)),
    info = paste0(
      "`brokerage_notes_log` is missing columns: ",
      paste(setdiff(expected_log_cols, names(brokerage_notes_log)), collapse = ", ")
    )
  )

  brokerage_notes_log$date <- as.Date(brokerage_notes_log$date)

  match_pct <- mean(current_dates %in% brokerage_notes_log$date)

  testthat::expect_true(
    match_pct >= 0.9,
    info = sprintf(
      "Only %.1f%% of current_dates found in log (expected >= 90%%)",
      match_pct * 100
    )
  )

  if (nrow(brokerage_notes_log) > 0L) {

    testthat::expect_false(
      any(is.na(brokerage_notes_log$saved_path) | brokerage_notes_log$saved_path == ""),
      info = "`brokerage_notes_log$saved_path` cannot contain missing or empty paths."
    )

    testthat::expect_true(
      all(file.exists(brokerage_notes_log$saved_path)),
      info = "Some files listed in `brokerage_notes_log$saved_path` do not exist."
    )

    testthat::expect_false(
      anyDuplicated(brokerage_notes_log$saved_path) > 0,
      info = "`brokerage_notes_log$saved_path` contains duplicated file paths."
    )
  }

  trade_dates_from_log <- brokerage_notes_log %>%
    dplyr::filter(tolower(tools::file_ext(saved_path)) %in% c("xlsx", "xls")) %>%
    dplyr::pull(date) %>%
    as.Date() %>%
    unique()

  no_trade_dates <- current_dates[
    !current_dates %in% trade_dates_from_log
  ]

  no_trade_by_week <- data.frame(
    date = no_trade_dates,
    week = format(no_trade_dates, "%G-%V"),
    stringsAsFactors = FALSE
  ) %>%
    dplyr::count(week, name = "n_no_trade_days") %>%
    dplyr::filter(n_no_trade_days > max_no_trade_days_per_week)

  testthat::expect_equal(
    nrow(no_trade_by_week),
    0L,
    info = paste0(
      "Suspicious number of no-trade days in at least one week. ",
      "Maximum allowed: ", max_no_trade_days_per_week, "."
    )
  )

  ## Test trade_data structure--------------------------------------------------

  expected_trade_cols <- c(
    "date",
    "fund_account",
    "legacy_ticker",
    "side",
    "amount",
    "price",
    "traded_volume",
    "brokerage_fee_bps",
    "brokerage_fee_estimated",
    "source_file"
  )

  testthat::expect_s3_class(
    trade_data,
    "data.frame"
  )

  testthat::expect_true(
    all(expected_trade_cols %in% names(trade_data)),
    info = paste0(
      "`trade_data` is missing columns: ",
      paste(setdiff(expected_trade_cols, names(trade_data)), collapse = ", ")
    )
  )

  ## Test log versus trade_data consistency-------------------------------------

  downloaded_excel_files <- brokerage_notes_log %>%
    dplyr::filter(tolower(tools::file_ext(saved_path)) %in% c("xlsx", "xls")) %>%
    dplyr::pull(saved_path) %>%
    unique() %>%
    normalizePath(winslash = "/", mustWork = FALSE)

  trade_source_files <- trade_data$source_file %>%
    unique() %>%
    normalizePath(winslash = "/", mustWork = FALSE)

  testthat::expect_true(
    all(trade_source_files %in% downloaded_excel_files),
    info = "`trade_data$source_file` contains files that are not present in the downloaded Excel attachment log."
  )

  unused_excel_files <- setdiff(downloaded_excel_files, trade_source_files)

  testthat::expect_length(
    unused_excel_files,
    0
  )

  if (nrow(trade_data) == 0L) {
    return(invisible(TRUE))
  }

  trade_data$date <- as.Date(trade_data$date)

  ## Test valid fund accounts----------------------------------------------------

  valid_fund_accounts <- as.character(unname(broker_accounts))

  testthat::expect_true(
    all(trade_data$fund_account %in% valid_fund_accounts),
    info = paste0(
      "Unexpected fund accounts found: ",
      paste(
        sort(unique(trade_data$fund_account[
          !trade_data$fund_account %in% valid_fund_accounts
        ])),
        collapse = ", "
      )
    )
  )

  ## Test trade_data types------------------------------------------------------

  testthat::expect_s3_class(
    trade_data$date,
    "Date"
  )

  testthat::expect_type(
    trade_data$fund_account,
    "character"
  )

  testthat::expect_type(
    trade_data$legacy_ticker,
    "character"
  )

  testthat::expect_type(
    trade_data$side,
    "character"
  )

  numeric_cols <- c(
    "amount",
    "price",
    "traded_volume",
    "brokerage_fee_bps",
    "brokerage_fee_estimated"
  )

  purrr::walk(
    numeric_cols,
    function(col) {
      testthat::expect_true(
        is.numeric(trade_data[[col]]),
        info = paste0("`trade_data$", col, "` must be numeric.")
      )
    }
  )

  ## Test trade_data missingness and valid values----------------------------

  required_non_missing_cols <- c(
    "date",
    "fund_account",
    "legacy_ticker",
    "side",
    "amount",
    "price",
    "traded_volume",
    "brokerage_fee_bps",
    "brokerage_fee_estimated",
    "source_file"
  )

  purrr::walk(
    required_non_missing_cols,
    function(col) {
      testthat::expect_false(
        any(is.na(trade_data[[col]])),
        info = paste0("`trade_data$", col, "` cannot contain NAs.")
      )
    }
  )

  match_pct <- mean(current_dates %in% trade_data$date)

  testthat::expect_true(
    match_pct >= 0.9,
    info = sprintf(
      "Only %.1f%% of current_dates found in trade_data$date (expected >= 90%%)",
      match_pct * 100
    )
  )

  testthat::expect_true(
    all(trade_data$side %in% c("buy", "sell")),
    info = "`trade_data$side` must contain only 'buy' or 'sell'."
  )

  testthat::expect_true(
    all(trade_data$amount > 0),
    info = "`trade_data$amount` must be strictly positive."
  )

  testthat::expect_true(
    all(trade_data$price > 0),
    info = "`trade_data$price` must be strictly positive."
  )

  testthat::expect_true(
    all(trade_data$traded_volume > 0),
    info = "`trade_data$traded_volume` must be strictly positive."
  )

  testthat::expect_true(
    all(trade_data$brokerage_fee_estimated >= 0),
    info = "`trade_data$brokerage_fee_estimated` must be non-negative."
  )

  testthat::expect_true(
    all(file.exists(trade_data$source_file)),
    info = "Some files listed in `trade_data$source_file` do not exist."
  )

  ## Test formulas-------------------------------------------------------------

  expected_traded_volume <- abs(trade_data$amount * trade_data$price)

  testthat::expect_equal(
    trade_data$traded_volume,
    expected_traded_volume,
    tolerance = volume_tolerance,
    info = "`traded_volume` must equal abs(amount * price)."
  )

  expected_fee <- trade_data$traded_volume * trade_data$brokerage_fee_bps / 10000

  testthat::expect_equal(
    trade_data$brokerage_fee_estimated,
    expected_fee,
    tolerance = fee_tolerance,
    info = "`brokerage_fee_estimated` must equal traded_volume * brokerage_fee_bps / 10000."
  )

  testthat::expect_true(
    all(trade_data$brokerage_fee_bps == brokerage_fee_bps),
    info = "`trade_data$brokerage_fee_bps` is inconsistent with expected `brokerage_fee_bps`."
  )

  ## Test duplicate rows--------------------------------------------------------

  duplicated_trades <- trade_data %>%
    dplyr::count(
      date,
      fund_account,
      legacy_ticker,
      side,
      amount,
      price,
      source_file
    ) %>%
    dplyr::filter(n > 1L)

  testthat::expect_equal(
    nrow(duplicated_trades),
    0L,
    info = "`trade_data` contains duplicated trade rows."
  )

  invisible(TRUE)
}

validate_port_iniciais <- function(
    port_iniciais,
    broker_accounts,
    current_dates,
    etfs = c("BOVA11", "BOVV11", "DIVO11", "SMAL11", "ISUS11", "LFTS11"),
    position_tolerance = 1e-8,
    initial_rebalancing_date
) {

  if (is.null(port_iniciais)) {
    testthat::expect_false(initial_rebalancing_date %in% current_dates)

    return(invisible(TRUE))
  }

  expected_cols <- c(
    "date",
    "id",
    "fund_name",
    "legacy_ticker",
    "positions",
    "price"
  )

  testthat::expect_s3_class(port_iniciais, "data.frame")

  testthat::expect_true(all(expected_cols %in% names(port_iniciais)))

  testthat::expect_true(all(names(port_iniciais) == expected_cols))

  testthat::expect_s3_class(port_iniciais$date, "Date")

  testthat::expect_type(port_iniciais$id, "character")
  testthat::expect_type(port_iniciais$fund_name, "character")
  testthat::expect_type(port_iniciais$legacy_ticker, "character")

  testthat::expect_true(is.numeric(port_iniciais$positions))
  testthat::expect_true(is.numeric(port_iniciais$price))

  required_non_missing_cols <- c(
    "date",
    "id",
    "fund_name",
    "legacy_ticker",
    "positions",
    "price"
  )

  purrr::walk(
    required_non_missing_cols,
    function(col) {
      testthat::expect_false(any(is.na(port_iniciais[[col]])))
    }
  )

  testthat::expect_true(all(port_iniciais$positions >= 0))
  testthat::expect_true(all(port_iniciais$price > 0))

  testthat::expect_equal(
    min(current_dates),
    initial_rebalancing_date,
    info = paste0(
      "`port_iniciais` is not NULL, so the first date in `current_dates` must be ",
      "the first date from `data/dev/rebalancing`."
    )
  )

  testthat::expect_true(all(port_iniciais$date == initial_rebalancing_date))

  expected_ids <- c(
    "YMF_29",
    "YMF_33",
    "YMF_34",
    "YMF_35",
    "YMF_36",
    "YMF_37",
    "YMF_500",
    "YMF_501"
  )

  testthat::expect_setequal(
    unique(port_iniciais$id),
    expected_ids
  )

  expected_fund_names <- names(broker_accounts)

  testthat::expect_false(
    is.null(expected_fund_names) || any(expected_fund_names == "")
  )

  testthat::expect_setequal(
    unique(port_iniciais$fund_name),
    expected_fund_names
  )

  expected_id_fund_map <- data.frame(
    id = c(
      "YMF_29",
      "YMF_35",
      "YMF_36",
      "YMF_37",
      "YMF_33",
      "YMF_34",
      "YMF_500",
      "YMF_501"
    ),
    fund_name = c(
      "sicoob_acoes",
      "sicoob_small_caps",
      "sicoob_dividendos",
      "sicoob_asg_is",
      "vgbl_sicoob_seguradora_rv_30",
      "vgbl_sicoob_seguradora_rv_65",
      "previ_sicoob_500rv",
      "previ_sicoob_501rv"
    ),
    stringsAsFactors = FALSE
  )

  id_fund_check <- port_iniciais %>%
    dplyr::distinct(id, fund_name)

  unexpected_id_fund_pairs <- id_fund_check %>%
    dplyr::anti_join(
      expected_id_fund_map,
      by = c("id", "fund_name")
    )

  testthat::expect_equal(
    nrow(unexpected_id_fund_pairs),
    0L,
    info = paste0(
      "`port_iniciais` has unexpected id/fund_name pairs. First issue: ",
      if (nrow(unexpected_id_fund_pairs) > 0L) {
        paste0(
          unexpected_id_fund_pairs$id[1],
          " / ",
          unexpected_id_fund_pairs$fund_name[1]
        )
      } else {
        "none"
      }
    )
  )

  etf_coverage <- port_iniciais %>%
    dplyr::filter(legacy_ticker %in% etfs) %>%
    dplyr::group_by(fund_name) %>%
    dplyr::summarise(
      n_etfs = dplyr::n_distinct(legacy_ticker),
      etfs_found = paste(sort(unique(legacy_ticker)), collapse = ", "),
      .groups = "drop"
    )

  missing_etf_coverage <- data.frame(
    fund_name = expected_fund_names,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::left_join(
      etf_coverage,
      by = "fund_name"
    ) %>%
    dplyr::mutate(
      n_etfs = dplyr::coalesce(n_etfs, 0L),
      etfs_found = dplyr::coalesce(etfs_found, "")
    ) %>%
    dplyr::filter(n_etfs < 2L)

  testthat::expect_equal(
    nrow(missing_etf_coverage),
    0L,
    info = paste0(
      "Each `fund_name` must contain at least two ETFs from `etfs`. First issue: ",
      if (nrow(missing_etf_coverage) > 0L) {
        paste0(
          missing_etf_coverage$fund_name[1],
          " has ",
          missing_etf_coverage$n_etfs[1],
          " ETF(s): ",
          missing_etf_coverage$etfs_found[1]
        )
      } else {
        "none"
      }
    )
  )

  lfts11_coverage <- port_iniciais %>%
    dplyr::group_by(fund_name) %>%
    dplyr::summarise(
      has_lfts11 = any(legacy_ticker == "LFTS11"),
      .groups = "drop"
    )

  missing_lfts11 <- data.frame(
    fund_name = expected_fund_names,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::left_join(
      lfts11_coverage,
      by = "fund_name"
    ) %>%
    dplyr::mutate(
      has_lfts11 = dplyr::coalesce(has_lfts11, FALSE)
    ) %>%
    dplyr::filter(!has_lfts11)

  testthat::expect_equal(
    nrow(missing_lfts11),
    0L,
    info = paste0(
      "All fund_names must contain LFTS11. Missing fund_names: ",
      paste(missing_lfts11$fund_name, collapse = ", ")
    )
  )

  expected_position_sums <- data.frame(
    fund_name = c(
      "sicoob_acoes",
      "vgbl_sicoob_seguradora_rv_30",
      "vgbl_sicoob_seguradora_rv_65",
      "sicoob_small_caps",
      "sicoob_dividendos",
      "sicoob_asg_is",
      "previ_sicoob_500rv",
      "previ_sicoob_501rv"
    ),
    expected_positions = c(
      1721903,
      1242319,
      279673,
      802271,
      801165,
      307573,
      357349,
      2244123
    ),
    stringsAsFactors = FALSE
  )

  observed_position_sums <- port_iniciais %>%
    dplyr::filter(legacy_ticker != "LFTS11") %>%
    dplyr::group_by(fund_name) %>%
    dplyr::summarise(
      observed_positions = sum(positions, na.rm = FALSE),
      .groups = "drop"
    )

  position_sum_check <- expected_position_sums %>%
    dplyr::left_join(
      observed_position_sums,
      by = "fund_name"
    ) %>%
    dplyr::mutate(
      abs_error = abs(observed_positions - expected_positions)
    )

  bad_position_sums <- position_sum_check %>%
    dplyr::filter(
      is.na(observed_positions) |
        abs_error > position_tolerance
    )

  testthat::expect_equal(
    nrow(bad_position_sums),
    0L,
    info = paste0(
      "`port_iniciais` position sums do not match expected totals. First issue: ",
      if (nrow(bad_position_sums) > 0L) {
        paste0(
          bad_position_sums$fund_name[1],
          " expected = ",
          bad_position_sums$expected_positions[1],
          ", observed = ",
          bad_position_sums$observed_positions[1]
        )
      } else {
        "none"
      }
    )
  )

  duplicated_rows <- port_iniciais %>%
    dplyr::count(
      date,
      id,
      fund_name,
      legacy_ticker,
      positions,
      price
    ) %>%
    dplyr::filter(n > 1L)

  testthat::expect_equal(
    nrow(duplicated_rows),
    0L,
    info = "`port_iniciais` contains duplicated rows."
  )

  invisible(TRUE)
}
