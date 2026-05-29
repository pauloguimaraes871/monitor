evolve_portfolio  <- function(
    rebal_weights,
    catalog,
    comdinheiro_data,
    current_dates,
    old_weights = NULL,
    transaction_costs_bps = NULL,
    fund_fees_bps = 0,
    initial_aum = 1,
    weight_tolerance = 1e-2,
    allow_missing_returns = TRUE,
    verbose = TRUE
) {
  # Validate inputs -----------------------------------------------------------

  current_dates <- as.Date(current_dates)

  if (any(is.na(current_dates))) {
    stop("`current_dates` must be coercible to Date.", call. = FALSE)
  }

  current_dates <- sort(unique(current_dates))

  if (length(current_dates) == 0L) {
    stop("`current_dates` cannot be empty.", call. = FALSE)
  }

  required_rebal_cols <- c("date", "id", "legacy_ticker", "weights")
  missing_rebal_cols <- setdiff(required_rebal_cols, names(rebal_weights))

  if (length(missing_rebal_cols) > 0L) {
    stop(
      "`rebal_weights` is missing columns: ",
      paste(missing_rebal_cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  required_comd_cols <- c("date", "legacy_ticker", "ret_1d")
  missing_comd_cols <- setdiff(required_comd_cols, names(comdinheiro_data))

  if (length(missing_comd_cols) > 0L) {
    stop(
      "`comdinheiro_data` is missing columns: ",
      paste(missing_comd_cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  rebal_weights <- rebal_weights %>%
    dplyr::mutate(
      date = as.Date(date),
      id = as.character(id),
      legacy_ticker = as.character(legacy_ticker),
      weights = as.numeric(weights)
    )

  comdinheiro_data <- comdinheiro_data %>%
    dplyr::mutate(
      date = as.Date(date),
      legacy_ticker = as.character(legacy_ticker),
      ret_1d = as.numeric(ret_1d) / 100
    )

  if (any(is.na(rebal_weights$date))) {
    stop("`rebal_weights$date` contains NA values.", call. = FALSE)
  }

  if (any(is.na(comdinheiro_data$date))) {
    stop("`comdinheiro_data$date` contains NA values.", call. = FALSE)
  }

  if (any(is.na(rebal_weights$weights))) {
    stop("`rebal_weights$weights` contains NA values.", call. = FALSE)
  }

  if (any(rebal_weights$weights < -weight_tolerance)) {
    stop("`rebal_weights$weights` contains negative weights.", call. = FALSE)
  }

  rebal_weight_check <- rebal_weights %>%
    dplyr::group_by(date, id) %>%
    dplyr::summarise(
      weight_sum = sum(weights),
      .groups = "drop"
    ) %>%
    dplyr::filter(abs(weight_sum - 1) > weight_tolerance)

  if (nrow(rebal_weight_check) > 0L) {
    stop(
      paste0(
        "Some rebalance portfolios do not sum to 1. ",
        "First issue: date = ",
        rebal_weight_check$date[1],
        ", id = ",
        rebal_weight_check$id[1],
        ", weight_sum = ",
        round(rebal_weight_check$weight_sum[1], 8),
        "."
      ),
      call. = FALSE
    )
  }

  if (anyDuplicated(rebal_weights[c("date", "id", "legacy_ticker")]) > 0L) {
    stop(
      "`rebal_weights` has duplicated date + id + legacy_ticker rows.",
      call. = FALSE
    )
  }

  if (anyDuplicated(comdinheiro_data[c("date", "legacy_ticker")]) > 0L) {
    stop(
      "`comdinheiro_data` has duplicated date + legacy_ticker rows.",
      call. = FALSE
    )
  }

  # Validate old/new continuity ----------------------------------------------

  if (!is.null(old_weights)) {
    if (!all(c("bop_weights", "eop_weights") %in% names(old_weights))) {
      stop(
        "`old_weights` must be a list with `bop_weights` and `eop_weights`.",
        call. = FALSE
      )
    }

    old_max_date <- max(as.Date(old_weights$eop_weights$date), na.rm = TRUE)
    min_current_date <- min(current_dates)

    valid_start_dates <- sort(unique(comdinheiro_data$date))
    next_available_date <- valid_start_dates[valid_start_dates > old_max_date][1]

    if (is.na(next_available_date)) {
      stop(
        "Could not infer the next business date after old weights from `comdinheiro_data`.",
        call. = FALSE
      )
    }

    if (min_current_date > next_available_date) {
      stop(
        paste0(
          "`min(current_dates)` must be smaller than or equal to the first available ",
          "business date after old weights. old_max_date = ",
          old_max_date,
          ", next_available_date = ",
          next_available_date,
          ", min_current_date = ",
          min_current_date,
          "."
        ),
        call. = FALSE
      )
    }
  }

  # Prepare transaction costs and fees ----------------------------------------

  if (is.null(transaction_costs_bps)) {
    transaction_costs_bps <- tidyr::expand_grid(
      date = current_dates,
      id = unique(rebal_weights$id)
    ) %>%
      dplyr::mutate(
        transaction_cost_bps = 0
      )
  }

  transaction_costs_bps <- transaction_costs_bps %>%
    dplyr::mutate(
      date = as.Date(date),
      id = as.character(id),
      transaction_cost_bps = as.numeric(transaction_cost_bps)
    )

  if (!all(c("date", "id", "transaction_cost_bps") %in% names(transaction_costs_bps))) {
    stop(
      "`transaction_costs_bps` must contain date, id, and transaction_cost_bps.",
      call. = FALSE
    )
  }

  if (any(is.na(transaction_costs_bps$transaction_cost_bps))) {
    stop("`transaction_costs_bps` contains NA costs.", call. = FALSE)
  }

  if (any(transaction_costs_bps$transaction_cost_bps < -1e-12)) {
    stop("`transaction_costs_bps` cannot contain negative costs.", call. = FALSE)
  }

  if (!is.numeric(fund_fees_bps) || length(fund_fees_bps) != 1L || is.na(fund_fees_bps)) {
    stop("`fund_fees_bps` must be a single numeric value.", call. = FALSE)
  }

  if (fund_fees_bps < 0) {
    stop("`fund_fees_bps` cannot be negative.", call. = FALSE)
  }

  daily_fee_return <- fund_fees_bps / 10000

  # Helpers---------------------------------------------------------------------
  make_weight_vector <- function(df, assets) {

    if (anyDuplicated(df$cvm_code_type) > 0L) {
      stop("`df$cvm_code_type` cannot contain duplicates in `make_weight_vector()`.", call. = FALSE)
    }

    out <- stats::setNames(rep(0, length(assets)), assets)

    idx <- match(df$cvm_code_type, assets)

    if (any(is.na(idx))) {
      stop("Internal error: some weight tickers are not in `assets`.", call. = FALSE)
    }

    out[idx] <- df$weights
    out
  }

  make_return_vector <- function(df, assets, current_date, portfolio_id,
                                 allow_missing_returns = FALSE) {
    out <- stats::setNames(rep(NA_real_, length(assets)), assets)

    idx <- match(df$cvm_code_type, assets)

    if (any(is.na(idx))) {
      stop("Internal error: some return tickers are not in `assets`.", call. = FALSE)
    }

    out[idx] <- df$ret_1d

    missing_ret_assets <- names(out)[is.na(out)]

    if (length(missing_ret_assets) > 0L) {
      if (isTRUE(allow_missing_returns)) {
        warning(
          paste0(
            "Missing returns for portfolio `",
            portfolio_id,
            "` at date ",
            as.Date(current_date),
            ". Setting missing returns to zero. Missing assets: ",
            paste(missing_ret_assets, collapse = ", ")
          ),
          call. = FALSE
        )

        out[is.na(out)] <- 0
      } else {
        stop(
          paste0(
            "Missing returns for portfolio `",
            portfolio_id,
            "` at date ",
            current_date,
            ". Missing assets: ",
            paste(missing_ret_assets, collapse = ", ")
          ),
          call. = FALSE
        )
      }
    }

    out
  }

  # Prepare objects-------------------------------------------------------------
  browser()

  asset_ticker_lookup <- rebal_weights %>%
    dplyr::arrange(cvm_code_type, dplyr::desc(date)) %>%
    dplyr::distinct(cvm_code_type, legacy_ticker, .keep_all = TRUE) %>%
    dplyr::group_by(cvm_code_type) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(cvm_code_type, legacy_ticker)

  comdinheiro_data <- comdinheiro_data %>%
    dplyr::mutate(
      date = as.Date(date),
      legacy_ticker = as.character(legacy_ticker),
      ret_1d = as.numeric(ret_1d)
    ) %>%
    dplyr::left_join(
      catalog_lookup,
      by = "legacy_ticker"
    )

  missing_return_mapping <- comdinheiro_data %>%
    dplyr::filter(is.na(cvm_code_type)) %>%
    dplyr::distinct(date, legacy_ticker)

  if (nrow(missing_return_mapping) > 0) {
    warning(
      paste0(
        "Some Comdinheiro tickers could not be mapped to `cvm_code_type` using latest catalog. ",
        "They will not be used unless required by a portfolio. ",
        "First issue: date = ",
        missing_return_mapping$date[1],
        ", legacy_ticker = ",
        missing_return_mapping$legacy_ticker[1],
        "."
      ),
      call. = FALSE
    )
  }

  # Prepare return panel ------------------------------------------------------
  returns_long <- comdinheiro_data %>%
    dplyr::filter(date %in% current_dates) %>%
    dplyr::filter(!is.na(cvm_code_type)) %>%
    dplyr::select(date, legacy_ticker, cvm_code_type, ret_1d)

  if (anyDuplicated(returns_long[c("date", "cvm_code_type")]) > 0L) {
    stop(
      "`returns_long` has duplicated date + cvm_code_type rows after catalog mapping.",
      call. = FALSE
    )
  }

  missing_current_dates <- setdiff(current_dates, unique(returns_long$date))

  if (length(missing_current_dates) > 0L) {
    stop(
      "Missing Comdinheiro return dates: ",
      paste(as.character(missing_current_dates), collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  portfolio_ids <- sort(unique(rebal_weights$id))

  # Initialize outputs --------------------------------------------------------

  raw_ret_list <- list()
  net_ret_list <- list()
  aum_list <- list()
  turnover_list <- list()
  cost_list <- list()
  fee_list <- list()
  bop_weights_list <- list()
  eop_weights_list <- list()

  # Main loop by portfolio id -------------------------------------------------

  for (portfolio_id in portfolio_ids) {
    if (isTRUE(verbose)) {
      message("Evolving portfolio weights for id: ", portfolio_id)
    }

    target_weight_dates <- rebal_weights %>%
      dplyr::filter(id == portfolio_id) %>%
      dplyr::distinct(date) %>%
      dplyr::pull(date) %>%
      sort()

    portfolio_assets <- rebal_weights %>%
      dplyr::filter(id == portfolio_id) %>%
      dplyr::distinct(cvm_code_type) %>%
      dplyr::pull(cvm_code_type) %>%
      sort()

    if (length(portfolio_assets) == 0L) {
      stop("No assets found for portfolio id: ", portfolio_id, ".", call. = FALSE)
    }

    # Initial last weights ----------------------------------------------------

    old_eop_for_id <- NULL

    if (!is.null(old_weights)) {

      required_old_eop_cols <- c("date", "id", "cvm_code_type", "weights")
      missing_old_eop_cols <- setdiff(required_old_eop_cols, names(old_weights$eop_weights))

      if (length(missing_old_eop_cols) > 0L) {
        stop(
          "`old_weights$eop_weights` is missing columns: ",
          paste(missing_old_eop_cols, collapse = ", "),
          ".",
          call. = FALSE
        )
      }

      old_eop_for_id <- old_weights$eop_weights %>%
        dplyr::mutate(
          date = as.Date(date),
          id = as.character(id),
          cvm_code_type = as.character(cvm_code_type),
          weights = as.numeric(weights)
        ) %>%
        dplyr::filter(id == portfolio_id)

      if (nrow(old_eop_for_id) > 0L) {
        last_old_date <- max(old_eop_for_id$date)

        last_weights_df <- old_eop_for_id %>%
          dplyr::filter(date == last_old_date) %>%
          dplyr::select(cvm_code_type, weights)

        portfolio_assets <- sort(unique(c(
          portfolio_assets,
          last_weights_df$cvm_code_type
        )))

        last_weights <- make_weight_vector(
          df = last_weights_df,
          assets = portfolio_assets
        )
      } else {
        last_weights <- stats::setNames(rep(0, length(portfolio_assets)), portfolio_assets)
      }
    } else {
      last_weights <- stats::setNames(rep(0, length(portfolio_assets)), portfolio_assets)
    }

    current_aum <- initial_aum

    if (!is.null(old_weights) && "aum" %in% names(old_weights)) {
      old_aum_for_id <- old_weights$aum %>%
        dplyr::mutate(
          date = as.Date(date),
          id = as.character(id)
        ) %>%
        dplyr::filter(id == portfolio_id)

      if (nrow(old_aum_for_id) > 0L) {
        current_aum <- old_aum_for_id %>%
          dplyr::arrange(date) %>%
          dplyr::slice_tail(n = 1) %>%
          dplyr::pull(aum)
      }
    }

    # Daily loop --------------------------------------------------------------
    for (i in seq_along(current_dates)) {
      current_date <- current_dates[i]

      target_today <- rebal_weights %>%
        dplyr::filter(
          id == portfolio_id,
          date == current_date
        ) %>%
        dplyr::select(cvm_code_type, legacy_ticker, weights)

      assets_today <- sort(unique(c(
        names(last_weights),
        target_today$cvm_code_type
      )))

      if (!identical(names(last_weights), assets_today)) {
        expanded_last_weights <- stats::setNames(rep(0, length(assets_today)), assets_today)
        expanded_last_weights[names(last_weights)] <- last_weights
        last_weights <- expanded_last_weights
      }

      # BOP weights: weights valid at start of day before earning today's return
      bop_weights <- last_weights

      bop_weights_list[[length(bop_weights_list) + 1L]] <- data.frame(
        date = as.Date(current_date),
        id = portfolio_id,
        cvm_code_type = names(bop_weights),
        weights = as.numeric(bop_weights),
        stringsAsFactors = FALSE
      ) %>%
        dplyr::left_join(asset_ticker_lookup, by = "cvm_code_type") %>%
        dplyr::select(date, id, legacy_ticker, cvm_code_type, weights)

      ret_today_df <- returns_long %>%
        dplyr::filter(
          date == current_date,
          cvm_code_type %in% names(bop_weights)
        )

      ret_vec <- make_return_vector(
        df = ret_today_df,
        assets = names(bop_weights),
        current_date = current_date,
        portfolio_id = portfolio_id,
        allow_missing_returns = allow_missing_returns
      )

      raw_ret <- sum(bop_weights * ret_vec)

      drift_denominator <- sum(bop_weights * (1 + ret_vec))

      if (sum(bop_weights) > weight_tolerance) {
        if (!is.finite(drift_denominator) || drift_denominator <= 0) {
          stop(
            "Invalid drift denominator for portfolio `",
            portfolio_id,
            "` at date ",
            current_date,
            ".",
            call. = FALSE
          )
        }

        drifted_weights <- bop_weights * (1 + ret_vec) / drift_denominator
      } else {
        drifted_weights <- bop_weights
      }

      # EOP rebalance: target weights are valid only at the end of the date
      is_rebalance_date <- nrow(target_today) > 0L

      if (is_rebalance_date) {
        target_assets <- sort(unique(c(
          names(drifted_weights),
          target_today$cvm_code_type
        )))

        expanded_drifted_weights <- stats::setNames(rep(0, length(target_assets)), target_assets)
        expanded_drifted_weights[names(drifted_weights)] <- drifted_weights
        drifted_weights <- expanded_drifted_weights

        target_weights <- make_weight_vector(
          df = target_today,
          assets = target_assets
        )

        if (abs(sum(target_weights) - 1) > weight_tolerance) {
          stop(
            "Target weights do not sum to 1 for portfolio `",
            portfolio_id,
            "` at date ",
            current_date,
            ".",
            call. = FALSE
          )
        }

        turnover <- sum(abs(target_weights - drifted_weights))
        eop_weights <- target_weights
      } else {
        turnover <- 0
        eop_weights <- drifted_weights
      }

      cost_bps_today <- transaction_costs_bps %>%
        dplyr::filter(
          date == current_date,
          id == portfolio_id
        ) %>%
        dplyr::summarise(
          transaction_cost_bps = sum(transaction_cost_bps, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::pull(transaction_cost_bps)

      if (length(cost_bps_today) == 0L || is.na(cost_bps_today)) {
        cost_bps_today <- 0
      }

      transaction_cost_return <- cost_bps_today / 10000

      if (transaction_cost_return >= 1) {
        stop(
          "Transaction cost is greater than or equal to 100% for portfolio `",
          portfolio_id,
          "` at date ",
          current_date,
          ".",
          call. = FALSE
        )
      }

      net_ret <- (1 + raw_ret) * (1 - transaction_cost_return) * (1 - daily_fee_return) - 1

      current_aum <- current_aum * (1 + net_ret)

      raw_ret_list[[length(raw_ret_list) + 1L]] <- data.frame(
        date = as.Date(current_date),
        id = portfolio_id,
        raw_return = raw_ret,
        stringsAsFactors = FALSE
      )

      net_ret_list[[length(net_ret_list) + 1L]] <- data.frame(
        date = as.Date(current_date),
        id = portfolio_id,
        net_return = net_ret,
        stringsAsFactors = FALSE
      )

      aum_list[[length(aum_list) + 1L]] <- data.frame(
        date = as.Date(current_date),
        id = portfolio_id,
        aum = current_aum,
        stringsAsFactors = FALSE
      )

      turnover_list[[length(turnover_list) + 1L]] <- data.frame(
        date = as.Date(current_date),
        id = portfolio_id,
        turnover = turnover,
        stringsAsFactors = FALSE
      )

      cost_list[[length(cost_list) + 1L]] <- data.frame(
        date = as.Date(current_date),
        id = portfolio_id,
        transaction_cost_bps = cost_bps_today,
        transaction_cost_return = transaction_cost_return,
        stringsAsFactors = FALSE
      )

      fee_list[[length(fee_list) + 1L]] <- data.frame(
        date = as.Date(current_date),
        id = portfolio_id,
        fund_fees_bps = fund_fees_bps,
        fund_fee_return = daily_fee_return,
        stringsAsFactors = FALSE
      )

      eop_weights_list[[length(eop_weights_list) + 1L]] <- data.frame(
        date = as.Date(current_date),
        id = portfolio_id,
        cvm_code_type = names(eop_weights),
        weights = as.numeric(eop_weights),
        stringsAsFactors = FALSE
      ) %>%
        dplyr::left_join(asset_ticker_lookup, by = "cvm_code_type") %>%
        dplyr::select(date, id, legacy_ticker, cvm_code_type, weights)

      last_weights <- eop_weights
    }
  }

  # Bind outputs --------------------------------------------------------------

  out <- list(
    weights = list(
      bop_weights = dplyr::bind_rows(bop_weights_list),
      eop_weights = dplyr::bind_rows(eop_weights_list)
    ),
    returns = dplyr::bind_rows(raw_ret_list) %>%
      dplyr::left_join(
        dplyr::bind_rows(net_ret_list),
        by = c("date", "id")
      ),
    aum = dplyr::bind_rows(aum_list),
    turnover = dplyr::bind_rows(turnover_list),
    costs = dplyr::bind_rows(cost_list),
    fees = dplyr::bind_rows(fee_list),
    workflow = list(
      current_dates = current_dates,
      initial_aum = initial_aum,
      transaction_costs_bps = transaction_costs_bps,
      fund_fees_bps = fund_fees_bps,
      weight_tolerance = weight_tolerance,
      allow_missing_returns = allow_missing_returns
    )
  )

  out$weights$bop_weights$date <- as.Date(out$weights$bop_weights$date)
  out$weights$eop_weights$date <- as.Date(out$weights$eop_weights$date)

  # Output validation ---------------------------------------------------------

  validate_evolved_weight_table <- function(df, table_name) {
    bad_sums <- df %>%
      dplyr::group_by(date, id) %>%
      dplyr::summarise(
        weight_sum = sum(weights, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::filter(
        abs(weight_sum - 1) > weight_tolerance,
        abs(weight_sum) > weight_tolerance
      )

    if (nrow(bad_sums) > 0L) {
      stop(
        paste0(
          "`",
          table_name,
          "` has invalid weight sums. ",
          "First issue: date = ",
          bad_sums$date[1],
          ", id = ",
          bad_sums$id[1],
          ", weight_sum = ",
          round(bad_sums$weight_sum[1], 8),
          "."
        ),
        call. = FALSE
      )
    }

    invisible(TRUE)
  }

  validate_evolved_weight_table(out$weights$bop_weights, "bop_weights")
  validate_evolved_weight_table(out$weights$eop_weights, "eop_weights")

  if (any(!is.finite(out$returns$raw_return))) {
    stop("`raw_return` contains non-finite values.", call. = FALSE)
  }

  if (any(!is.finite(out$returns$net_return))) {
    stop("`net_return` contains non-finite values.", call. = FALSE)
  }

  if (any(!is.finite(out$aum$aum)) || any(out$aum$aum <= 0)) {
    stop("`aum` contains invalid values.", call. = FALSE)
  }

  out
}
