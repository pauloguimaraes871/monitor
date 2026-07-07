evolve_portfolio <- function(
    old_portfolio,
    rebal_weights,
    comdinheiro_data,
    current_dates,
    brokerage_data = NULL,
    id = NULL,
    fund_name = NULL,
    split_inplit_data = NULL,
    other_events_data = NULL,
    transaction_costs_bps = NULL,
    fund_fees_bps = 0,
    weight_tolerance = 1e-2,
    position_tolerance = 1e-8,
    split_rounding_tolerance = 0.08,
    split_warning_threshold = 0.25,
    allow_missing_returns = TRUE,
    verbose = TRUE
) {

  # Validate and prepare inputs ----------------------------------------------
  validated_inputs <- validate_evolve_portfolio_inputs(
    old_portfolio = old_portfolio,
    rebal_weights = rebal_weights,
    comdinheiro_data = comdinheiro_data,
    current_dates = current_dates,
    brokerage_data = brokerage_data,
    id = id,
    fund_name = fund_name,
    split_inplit_data = split_inplit_data,
    other_events_data = other_events_data,
    transaction_costs_bps = transaction_costs_bps,
    fund_fees_bps = fund_fees_bps,
    weight_tolerance = weight_tolerance,
    position_tolerance = position_tolerance
  )

  current_dates <- validated_inputs$current_dates
  old_portfolio <- validated_inputs$old_portfolio
  old_paper_last <- validated_inputs$old_paper_last
  old_real_last <- validated_inputs$old_real_last
  id <- validated_inputs$id
  fund_name <- validated_inputs$fund_name
  old_port_last_date <- validated_inputs$old_port_last_date
  old_port_last_eop_market_value <- validated_inputs$old_port_last_eop_market_value
  old_port_last_eop_weights <- validated_inputs$old_port_last_eop_weights
  old_port_last_eop_positions <- validated_inputs$old_port_last_eop_positions
  old_port_last_prices <- validated_inputs$old_port_last_prices
  rebal_weights <- validated_inputs$rebal_weights
  comdinheiro_data <- validated_inputs$comdinheiro_data
  trade_data <- validated_inputs$trade_data
  split_inplit_data <- validated_inputs$split_inplit_data
  other_events_data <- validated_inputs$other_events_data
  transaction_costs_bps <- validated_inputs$transaction_costs_bps
  daily_fee_return <- validated_inputs$daily_fee_return

  # Confirm splits/inplits-----------------------------------------------------
  split_candidates <- detect_candidate_splits(
    comdinheiro_data = comdinheiro_data,
    current_dates = current_dates,
    old_port_last_date = old_port_last_date,
    split_warning_threshold = split_warning_threshold,
    split_rounding_tolerance = split_rounding_tolerance
  )

  unconfirmed_split_candidates <- split_candidates %>%
    dplyr::anti_join(
      split_inplit_data %>%
        dplyr::select(date, cvm_code_type),
      by = c("date", "cvm_code_type")
    )

  if (nrow(unconfirmed_split_candidates) > 0L) {
    warning(
      paste0(
        "Potential split/inplit candidates were detected but not provided in `split_inplit_data`.\n",
        "No automatic positions adjustment will be made for unconfirmed events.\n\n",
        "Detected candidates:\n",
        format_split_candidate_warnings(unconfirmed_split_candidates)
      ),
      call. = FALSE
    )
  }

  confirmed_split_diagnostics <- split_inplit_data %>%
    dplyr::left_join(
      split_candidates %>%
        dplyr::select(date, cvm_code_type, inferred_split_factor),
      by = c("date", "cvm_code_type")
    ) %>%
    dplyr::mutate(
      factor_diff = abs(split_factor - inferred_split_factor),
      factor_diff_pct = factor_diff / abs(split_factor)
    )

  suspicious_confirmed_splits <- confirmed_split_diagnostics %>%
    dplyr::filter(
      is.finite(inferred_split_factor),
      factor_diff_pct > split_rounding_tolerance
    )

  if (nrow(suspicious_confirmed_splits) > 0L) {
    warning(
      paste0(
        "`split_inplit_data$split_factor` contains price factors that differ from the inferred price-implied split factor.\n\n",
        paste0(
          "- date = ", suspicious_confirmed_splits$date,
          " | cvm_code_type = ", suspicious_confirmed_splits$cvm_code_type,
          " | user split_factor = ", suspicious_confirmed_splits$split_factor,
          " | inferred_split_factor = ", suspicious_confirmed_splits$inferred_split_factor,
          collapse = "\n"
        )
      ),
      call. = FALSE
    )
  }

  # Prepare panels and state --------------------------------------------------

  returns_long <- comdinheiro_data %>%
    dplyr::filter(date %in% current_dates) %>%
    dplyr::select(
      date,
      legacy_ticker,
      cvm_code_type,
      ret_1d,
      price,
      proventos,
      proventos_date,
      event_factor,
      n_shares
    )

  portfolio_assets <- sort(unique(c(
    old_paper_last$cvm_code_type,
    old_real_last$cvm_code_type,
    rebal_weights$cvm_code_type,
    trade_data$cvm_code_type,
    split_inplit_data$cvm_code_type
  )))

  portfolio_assets <- portfolio_assets[!is.na(portfolio_assets)]

  paper_current_market_value   <- old_port_last_eop_market_value
  paper_last_eop_weights       <- old_port_last_eop_weights
  real_last_eop_positions      <- old_port_last_eop_positions

  if (isTRUE(verbose)) {
    message("Evolving portfolio id: ", id, " / fund_name: ", fund_name)
  }

  # Output containers ---------------------------------------------------------

  paper_raw_ret_list <- list()
  paper_net_ret_list <- list()
  paper_market_value_list <- list()
  paper_turnover_list <- list()
  paper_cost_list <- list()
  paper_fee_list <- list()
  paper_bop_weights_list <- list()
  paper_eop_weights_list <- list()
  paper_portfolio_list <- list()

  real_raw_ret_list <- list()
  real_net_ret_list <- list()
  real_market_value_list <- list()
  real_turnover_list <- list()
  real_cost_list <- list()
  real_fee_list <- list()
  real_bop_positions_list <- list()
  real_eop_positions_list <- list()
  real_trade_list <- list()
  real_split_list <- list()
  real_portfolio_list <- list()

  # Daily loop ----------------------------------------------------------------

  for (i in seq_along(current_dates)) {

    current_date <- current_dates[i]
    message("\nProcessing date ", current_date, " (", i, "/", length(current_dates), ")")

    # Gather daily inputs -----------------------------------------------------

    target_today <- rebal_weights %>%
      dplyr::filter(date == current_date) %>%
      dplyr::select(cvm_code_type, legacy_ticker, weights)

    trades_today <- trade_data %>%
      dplyr::filter(date == current_date) %>%
      dplyr::mutate(
        signed_position = dplyr::case_when(
          side == "buy" ~ amount,
          side == "sell" ~ -amount,
          TRUE ~ NA_real_
        ),
        signed_traded_volume = dplyr::case_when(
          side == "buy" ~ traded_volume,
          side == "sell" ~ -traded_volume,
          TRUE ~ NA_real_
        )
      )

    ## Build a robust lookup
    asset_ticker_lookup_today <- comdinheiro_data %>%
      dplyr::filter(date == current_date) %>%
      dplyr::select(cvm_code_type, legacy_ticker) %>%
      dplyr::distinct()


    splits_today <- split_inplit_data %>%
      dplyr::filter(date == current_date) %>%
      dplyr::select(date, legacy_ticker, cvm_code_type, split_factor, position_factor)

    other_events_today <- other_events_data %>%
      dplyr::filter(date == current_date) %>%
      dplyr::select(date, old_legacy_ticker, old_cvm_code_type,
                    new_legacy_ticker, new_cvm_code_type, adj_factor)

    assets_today <- sort(unique(c(
      paper_last_eop_weights$cvm_code_type,
      real_last_eop_positions$cvm_code_type,
      target_today$cvm_code_type,
      trades_today$cvm_code_type,
      splits_today$cvm_code_type,
      other_events_today$old_cvm_code_type,
      other_events_today$new_cvm_code_type
    )))

    assets_today <- assets_today[!is.na(assets_today)]

    ## Add assets_today to paper_last_eop_weights and real_last_positions if not there
    if (length(setdiff(assets_today, paper_last_eop_weights$cvm_code_type)) > 0L) {
      paper_last_eop_weights <- paper_last_eop_weights %>%
        dplyr::bind_rows(
          data.frame(
            cvm_code_type = setdiff(assets_today, paper_last_eop_weights$cvm_code_type),
            eop_weights = 0,
            stringsAsFactors = FALSE
          )
        )
    }

    if (length(setdiff(assets_today, real_last_eop_positions$cvm_code_type)) > 0L) {
      real_last_eop_positions <- real_last_eop_positions %>%
        dplyr::bind_rows(
          data.frame(
            cvm_code_type = setdiff(assets_today, real_last_eop_positions$cvm_code_type),
            eop_positions = 0,
            stringsAsFactors = FALSE
          )
        )
    }

    # Current prices and ret -----------------------------------------------------
    # One row is required for every asset active in the daily accounting state.
    # Missing or invalid current prices are filled with the last available positive
    # Comdinheiro price before the current date. Missing returns caused by stale
    # prices are set to zero to avoid double-counting prior returns.
    prices_today_raw <- returns_long %>%
      dplyr::filter(
        date == current_date,
        cvm_code_type %in% assets_today
      ) %>%
      dplyr::select(
        date,
        legacy_ticker,
        cvm_code_type,
        ret_1d,
        price
      )

    last_available_current_prices <- comdinheiro_data %>%
      dplyr::filter(
        date < current_date,
        cvm_code_type %in% assets_today,
        is.finite(price),
        price > 0
      ) %>%
      dplyr::arrange(cvm_code_type, dplyr::desc(date)) %>%
      dplyr::group_by(cvm_code_type) %>%
      dplyr::slice(1L) %>%
      dplyr::ungroup() %>%
      dplyr::select(
        cvm_code_type,
        legacy_ticker_fallback = legacy_ticker,
        price_fallback = price,
        price_fallback_date = date
      )

    prices_today <- data.frame(
      cvm_code_type = assets_today,
      stringsAsFactors = FALSE
    ) %>%
      dplyr::left_join(
        prices_today_raw,
        by = "cvm_code_type"
      ) %>%
      dplyr::left_join(
        last_available_current_prices,
        by = "cvm_code_type"
      ) %>%
      dplyr::mutate(
        date = dplyr::coalesce(date, as.Date(current_date)),

        current_price_is_invalid = (
          is.na(price) |
            !is.finite(price) |
            price <= 0
        ),

        used_current_price_fallback = (
          current_price_is_invalid &
            !is.na(price_fallback) &
            is.finite(price_fallback) &
            price_fallback > 0
        ),

        legacy_ticker = dplyr::coalesce(
          legacy_ticker,
          legacy_ticker_fallback
        ),

        price = dplyr::if_else(
          used_current_price_fallback,
          price_fallback,
          price
        ),

        ret_1d = dplyr::if_else(
          used_current_price_fallback & (is.na(ret_1d) | !is.finite(ret_1d)),
          0,
          ret_1d
        )
      )

    # Current-price fallback diagnostics -----------------------------------------
    # Fallback usage is reported because stale prices affect daily return and
    # portfolio valuation assumptions.

    current_price_fallback_used <- prices_today %>%
      dplyr::filter(used_current_price_fallback)

    if (nrow(current_price_fallback_used) > 0L) {
      warning(
        paste0(
          "Current price fallback was used at date ",
          current_date,
          " for asset(s):\n",
          paste0(
            "- cvm_code_type = ", current_price_fallback_used$cvm_code_type,
            " | legacy_ticker = ", current_price_fallback_used$legacy_ticker,
            " | fallback_price = ", current_price_fallback_used$price,
            " | fallback_date = ", current_price_fallback_used$price_fallback_date,
            collapse = "\n"
          ),
          "\nAssumption: stale current price was carried forward and missing return was set to zero."
        ),
        call. = FALSE
      )
    }

    # Proventos ------------------------------------------------------------------
    all_dates <- returns_long$date %>% unique() %>% sort()
    proventos_today <- returns_long %>%
      dplyr::filter(!is.na(proventos_date)) %>%
      dplyr::select(cvm_code_type, legacy_ticker, proventos_date, proventos) %>%
      dplyr::mutate(
        date = purrr::map(proventos_date, ~ all_dates[which(all_dates > .x)[1]]) %>%
          purrr::list_c() %>%
          as.Date(origin = "1970-01-01"),
        .before = dplyr::everything()
      ) %>%
      dplyr::rename(
        proventos_ex_t_1 = proventos_date
      ) %>%
      dplyr::filter(date == current_date,
                    cvm_code_type %in% assets_today) %>%
      dplyr::distinct()

    # Previous prices for real accounting ------------------------------------
    last_available_previous_prices <- comdinheiro_data %>%
      dplyr::filter(
        date < current_date,
        cvm_code_type %in% assets_today,
        is.finite(price),
        price > 0
      ) %>%
      dplyr::arrange(cvm_code_type, dplyr::desc(date)) %>%
      dplyr::group_by(cvm_code_type) %>%
      dplyr::slice(1L) %>%
      dplyr::ungroup() %>%
      dplyr::select(
        cvm_code_type,
        price_lag_fallback = price,
        price_lag_date = date
      )

    if (i == 1L) {
      prices_yesterday_raw <- old_port_last_prices %>%
        dplyr::select(
          cvm_code_type,
          price_lag
        )
    } else {
      previous_date <- current_dates[i - 1L]

      prices_yesterday_raw <- returns_long %>%
        dplyr::filter(
          date == previous_date,
          cvm_code_type %in% assets_today,
          is.finite(price),
          price > 0
        ) %>%
        dplyr::select(
          cvm_code_type,
          price_lag = price
        )
    }

    prices_yesterday <- data.frame(
      cvm_code_type = assets_today,
      stringsAsFactors = FALSE
    ) %>%
      dplyr::left_join(
        prices_yesterday_raw,
        by = "cvm_code_type"
      ) %>%
      dplyr::left_join(
        last_available_previous_prices,
        by = "cvm_code_type"
      ) %>%
      dplyr::mutate(
        used_previous_price_fallback = (
          is.na(price_lag) |
            !is.finite(price_lag) |
            price_lag <= 0
        ) &
          !is.na(price_lag_fallback) &
          is.finite(price_lag_fallback) &
          price_lag_fallback > 0,

        price_lag = dplyr::if_else(
          used_previous_price_fallback,
          price_lag_fallback,
          price_lag
        )
      )

    assets_with_real_position <- real_last_eop_positions$cvm_code_type[
      abs(real_last_eop_positions$eop_positions) > position_tolerance
    ]

    missing_lag_price_assets <- prices_yesterday %>%
      dplyr::filter(
        cvm_code_type %in% assets_with_real_position,
        (
          is.na(price_lag) |
            !is.finite(price_lag) |
            price_lag <= 0
        )
      ) %>%
      dplyr::pull(cvm_code_type)

    if (length(missing_lag_price_assets) > 0L) {
      stop(
        "Missing previous prices for real positions at date ",
        current_date,
        ". Assets: ",
        paste(missing_lag_price_assets, collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    ## Only the accounting columns expected by the real portfolio helper are retained.
    prices_yesterday <- prices_yesterday %>%
      dplyr::select(
        cvm_code_type,
        price_lag
      )


    # Paper portfolio-----------------------------------------------------------
    paper_step <- compute_paper_portfolio_step(
      current_date = current_date,
      id = id,
      fund_name = fund_name,
      paper_last_eop_weights = paper_last_eop_weights,
      paper_current_market_value = paper_current_market_value,
      asset_ticker_lookup_today = asset_ticker_lookup_today,
      prices_today = prices_today,
      target_today = target_today,
      other_events_today = other_events_today,
      transaction_costs_bps = transaction_costs_bps,
      daily_fee_return = daily_fee_return,
      allow_missing_returns = allow_missing_returns,
      weight_tolerance = weight_tolerance
    )

    paper_current_market_value <- paper_step$paper_current_market_value
    paper_last_eop_weights <- paper_step$paper_last_eop_weights

    paper_raw_ret_list[[length(paper_raw_ret_list) + 1L]] <- paper_step$tables$raw_return
    paper_net_ret_list[[length(paper_net_ret_list) + 1L]] <- paper_step$tables$net_return
    paper_market_value_list[[length(paper_market_value_list) + 1L]] <- paper_step$tables$market_value
    paper_turnover_list[[length(paper_turnover_list) + 1L]] <- paper_step$tables$turnover
    paper_cost_list[[length(paper_cost_list) + 1L]] <- paper_step$tables$costs
    paper_fee_list[[length(paper_fee_list) + 1L]] <- paper_step$tables$fees
    paper_bop_weights_list[[length(paper_bop_weights_list) + 1L]] <- paper_step$tables$bop_weights
    paper_eop_weights_list[[length(paper_eop_weights_list) + 1L]] <- paper_step$tables$eop_weights
    paper_portfolio_list[[length(paper_portfolio_list) + 1L]] <- paper_step$tables$portfolio

    # Real portfolio------------------------------------------------------------
    real_step <- compute_real_portfolio_step(
      current_date = current_date,
      id = id,
      fund_name = fund_name,
      real_last_eop_positions = real_last_eop_positions,
      asset_ticker_lookup_today = asset_ticker_lookup_today,
      prices_yesterday = prices_yesterday,
      prices_today = prices_today,
      proventos_today = proventos_today,
      splits_today = splits_today,
      other_events_today = other_events_today,
      trades_today = trades_today,
      target_today = target_today,
      fabricate_trades = !base::startsWith(id, "YMF_"),
      default_lot_size = 100,
      etf_lot_size = 1,
      etf_tickers = c("BOVA11", "BOVV11", "SMLL11", "DIVO11", "LFTS11", "ISUS11"),
      daily_fee_return = daily_fee_return,
      position_tolerance = position_tolerance,
      weight_tolerance = weight_tolerance
    )

    real_last_eop_positions <- real_step$real_last_eop_positions

    real_raw_ret_list[[length(real_raw_ret_list) + 1L]] <- real_step$tables$raw_return
    real_net_ret_list[[length(real_net_ret_list) + 1L]] <- real_step$tables$net_return
    real_market_value_list[[length(real_market_value_list) + 1L]] <- real_step$tables$market_value
    real_turnover_list[[length(real_turnover_list) + 1L]] <- real_step$tables$turnover
    real_cost_list[[length(real_cost_list) + 1L]] <- real_step$tables$costs
    real_fee_list[[length(real_fee_list) + 1L]] <- real_step$tables$fees
    real_bop_positions_list[[length(real_bop_positions_list) + 1L]] <- real_step$tables$bop_positions
    real_eop_positions_list[[length(real_eop_positions_list) + 1L]] <- real_step$tables$eop_positions
    real_trade_list[[length(real_trade_list) + 1L]] <- real_step$tables$trades
    real_split_list[[length(real_split_list) + 1L]] <- real_step$tables$splits
    real_portfolio_list[[length(real_portfolio_list) + 1L]] <- real_step$tables$portfolio

  }


  # Bind outputs --------------------------------------------------------------

  paper_returns <- safe_bind_rows(paper_raw_ret_list) %>%
    dplyr::left_join(
      safe_bind_rows(paper_net_ret_list),
      by = c("date", "id", "fund_name")
    )

  real_returns <- safe_bind_rows(real_raw_ret_list) %>%
    dplyr::left_join(
      safe_bind_rows(real_net_ret_list),
      by = c("date", "id", "fund_name")
    )

  paper_portfolio <- safe_bind_rows(paper_portfolio_list)
  real_portfolio <- safe_bind_rows(real_portfolio_list)

  ## Join everything into a single output object with a clear schema for downstream use and validation
  out <- list(
    paper = list(
      portfolio = paper_portfolio,
      weights = list(
        bop_weights = safe_bind_rows(paper_bop_weights_list),
        eop_weights = safe_bind_rows(paper_eop_weights_list)
      ),
      returns = paper_returns,
      market_value = safe_bind_rows(paper_market_value_list),
      turnover = safe_bind_rows(paper_turnover_list),
      costs = safe_bind_rows(paper_cost_list),
      fees = safe_bind_rows(paper_fee_list)
    ),
    real = list(
      portfolio = real_portfolio,
      positions = list(
        bop_positions = safe_bind_rows(real_bop_positions_list),
        eop_positions = safe_bind_rows(real_eop_positions_list)
      ),
      returns = real_returns,
      market_value = safe_bind_rows(real_market_value_list),
      trades = safe_bind_rows(real_trade_list),
      splits = safe_bind_rows(real_split_list),
      turnover = safe_bind_rows(real_turnover_list),
      costs = safe_bind_rows(real_cost_list),
      fees = safe_bind_rows(real_fee_list)
    ),
    workflow = list(
      current_dates = current_dates,
      id = id,
      fund_name = fund_name,
      old_port_last_date = old_port_last_date,
      old_port_last_eop_market_value = old_port_last_eop_market_value,
      fund_fees_bps = fund_fees_bps,
      weight_tolerance = weight_tolerance,
      position_tolerance = position_tolerance,
      allow_missing_returns = allow_missing_returns
    ),
    diagnostics = list(
      split_candidates = split_candidates,
      unconfirmed_split_candidates = unconfirmed_split_candidates,
      confirmed_split_diagnostics = confirmed_split_diagnostics,
      suspicious_confirmed_splits = suspicious_confirmed_splits
    )
  )

  # Output validation ---------------------------------------------------------

  validate_evolve_portfolio_output(
    out = out,
    current_dates = current_dates,
    id = id,
    fund_name = fund_name,
    weight_tolerance = weight_tolerance,
    position_tolerance = position_tolerance,
    require_bop_tables = TRUE
  )


  out
}




#Helpers------------------------------------------------------------------------
validate_evolve_portfolio_inputs <- function(
    old_portfolio,
    rebal_weights,
    comdinheiro_data,
    current_dates,
    brokerage_data = NULL,
    id = NULL,
    fund_name = NULL,
    split_inplit_data = NULL,
    other_events_data = NULL,
    transaction_costs_bps = NULL,
    fund_fees_bps = 0,
    weight_tolerance = 1e-2,
    position_tolerance = 1e-8
) {

  # Validate dates ------------------------------------------------------------

  current_dates <- as.Date(current_dates)

  if (length(current_dates) == 0L || any(is.na(current_dates))) {
    stop("`current_dates` must contain valid Date values.", call. = FALSE)
  }

  current_dates <- sort(unique(current_dates))

  # Validate id and fund_name --------------------------------------

  if (is.null(id)) {
    stop("`id` must be supplied.", call. = FALSE)
  }

  id <- as.character(id)

  if (length(id) != 1L || is.na(id)) {
    stop("`id` must be a single non-missing character value.", call. = FALSE)
  }

  if (!is.null(fund_name)) {
    fund_name <- as.character(fund_name)

    if (length(fund_name) != 1L || is.na(fund_name)) {
      stop("`fund_name` must be NULL or a single non-missing character value.", call. = FALSE)
    }
  }


  # Validate fund_name business rules ----------------------------------------
  if (!is.null(fund_name)) {
    if (grepl("_FIA$", id) && fund_name != "sicoob_acoes") {
      stop("For id ending with '_FIA', fund_name must be 'sicoob_acoes'.", call. = FALSE)
    }

    if (grepl("_IDIV$", id) && fund_name != "sicoob_dividendos") {
      stop("For id ending with '_IDIV', fund_name must be 'sicoob_dividendos'.", call. = FALSE)
    }

    if (grepl("_SMLL$", id) && fund_name != "sicoob_small_caps") {
      stop("For id ending with '_SMLL', fund_name must be 'sicoob_small_caps'.", call. = FALSE)
    }

    if (grepl("_ASG$", id) && fund_name != "sicoob_asg_is") {
      stop("For id ending with '_ASG', fund_name must be 'sicoob_asg_is'.", call. = FALSE)
    }

    if (
      grepl("_VGBLs$", id) &&
      !fund_name %in% c("vgbl_sicoob_seguradora_rv_65", "vgbl_sicoob_seguradora_rv_30")
    ) {
      stop(
        "For id ending with '_VGBLs', fund_name must be ",
        "'vgbl_sicoob_seguradora_rv_65' or 'vgbl_sicoob_seguradora_rv_30'.",
        call. = FALSE
      )
    }

    if (
      grepl("_PREVI$", id) &&
      !fund_name %in% c("previ_sicoob_500rv", "previ_sicoob_501rv")
    ) {
      stop(
        "For id ending with '_PREVI', fund_name must be ",
        "'previ_sicoob_500rv' or 'previ_sicoob_501rv'.",
        call. = FALSE
      )
    }
  }

  # Validate old portfolio ----------------------------------------------------
  # Contract:
  # old_portfolio <- list(
  #   paper = list(portfolio = old_paper_portfolio),
  #   real  = list(portfolio = old_real_portfolio)
  # )
  #
  # Required paper columns:
  # date, id, cvm_code_type, eop_weights
  #
  # Required real columns:
  # date, id, fund_name, cvm_code_type, eop_positions, price

  if (!is.list(old_portfolio) || is.data.frame(old_portfolio)) {
    stop("`old_portfolio` must be a list.", call. = FALSE)
  }

  if (!"paper" %in% names(old_portfolio)) {
    stop("`old_portfolio` must contain element `paper`.", call. = FALSE)
  }

  if (!"real" %in% names(old_portfolio)) {
    stop("`old_portfolio` must contain element `real`.", call. = FALSE)
  }

  if (!is.list(old_portfolio$paper) || !"portfolio" %in% names(old_portfolio$paper)) {
    stop("`old_portfolio$paper` must be a list containing `portfolio`.", call. = FALSE)
  }

  if (!is.list(old_portfolio$real) || !"portfolio" %in% names(old_portfolio$real)) {
    stop("`old_portfolio$real` must be a list containing `portfolio`.", call. = FALSE)
  }

  old_paper_portfolio <- old_portfolio$paper$portfolio
  old_real_portfolio <- old_portfolio$real$portfolio

  if (!is.data.frame(old_paper_portfolio)) {
    stop("`old_portfolio$paper$portfolio` must be a data.frame.", call. = FALSE)
  }

  if (!is.data.frame(old_real_portfolio)) {
    stop("`old_portfolio$real$portfolio` must be a data.frame.", call. = FALSE)
  }

  required_old_paper_cols <- c("date", "id", "cvm_code_type", "eop_weights")
  missing_old_paper_cols <- base::setdiff(required_old_paper_cols, names(old_paper_portfolio))

  if (length(missing_old_paper_cols) > 0L) {
    stop(
      "`old_portfolio$paper$portfolio` is missing column(s): ",
      paste(missing_old_paper_cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  required_old_real_cols <- c("date", "id", "fund_name", "cvm_code_type", "eop_positions", "price")
  missing_old_real_cols <- base::setdiff(required_old_real_cols, names(old_real_portfolio))

  if (length(missing_old_real_cols) > 0L) {
    stop(
      "`old_portfolio$real$portfolio` is missing column(s): ",
      paste(missing_old_real_cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  old_paper_portfolio <- old_paper_portfolio %>%
    dplyr::mutate(
      date = as.Date(date),
      id = as.character(id),
      cvm_code_type = as.character(cvm_code_type),
      eop_weights = as.numeric(eop_weights)
    )

  old_real_portfolio <- old_real_portfolio %>%
    dplyr::mutate(
      date = as.Date(date),
      id = as.character(id),
      fund_name = as.character(fund_name),
      cvm_code_type = as.character(cvm_code_type),
      eop_positions = as.numeric(eop_positions),
      price = as.numeric(price)
    )

  if (nrow(old_paper_portfolio) == 0L) {
    stop("`old_portfolio$paper$portfolio` cannot be empty.", call. = FALSE)
  }

  if (nrow(old_real_portfolio) == 0L) {
    stop("`old_portfolio$real$portfolio` cannot be empty.", call. = FALSE)
  }

  if (any(is.na(old_paper_portfolio$date))) {
    stop("`old_portfolio$paper$portfolio$date` contains invalid or missing Date values.", call. = FALSE)
  }

  if (any(is.na(old_real_portfolio$date))) {
    stop("`old_portfolio$real$portfolio$date` contains invalid or missing Date values.", call. = FALSE)
  }

  if (any(is.na(old_paper_portfolio$id))) {
    stop("`old_portfolio$paper$portfolio$id` contains NA values.", call. = FALSE)
  }

  if (any(is.na(old_real_portfolio$id))) {
    stop("`old_portfolio$real$portfolio$id` contains NA values.", call. = FALSE)
  }

  if (any(is.na(old_paper_portfolio$cvm_code_type))) {
    stop("`old_portfolio$paper$portfolio$cvm_code_type` contains NA values.", call. = FALSE)
  }

  if (any(is.na(old_real_portfolio$cvm_code_type))) {
    stop("`old_portfolio$real$portfolio$cvm_code_type` contains NA values.", call. = FALSE)
  }

  if (any(is.na(old_paper_portfolio$eop_weights))) {
    stop("`old_portfolio$paper$portfolio$eop_weights` contains NA values.", call. = FALSE)
  }

  if (any(is.na(old_real_portfolio$eop_positions))) {
    stop("`old_portfolio$real$portfolio$eop_positions` contains NA values.", call. = FALSE)
  }

  if (any(is.na(old_real_portfolio$price))) {
    stop("`old_portfolio$real$portfolio$price` contains NA values.", call. = FALSE)
  }

  bad_old_paper_portfolio_id <- old_paper_portfolio %>%
    dplyr::filter(id != !!id)

  if (nrow(bad_old_paper_portfolio_id) > 0L) {
    stop(
      "`old_portfolio$paper$portfolio$id` must match the supplied `id`. ",
      "Expected: ", id, ". ",
      "Found: ", paste(unique(bad_old_paper_portfolio_id$id), collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  bad_old_real_portfolio_id <- old_real_portfolio %>%
    dplyr::filter(id != !!id)

  if (nrow(bad_old_real_portfolio_id) > 0L) {
    stop(
      "`old_portfolio$real$portfolio$id` must match the supplied `id`. ",
      "Expected: ", id, ". ",
      "Found: ", paste(unique(bad_old_real_portfolio_id$id), collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (!is.null(fund_name)) {
    bad_old_real_fund_name <- old_real_portfolio %>%
      dplyr::filter(fund_name != !!fund_name)

    if (nrow(bad_old_real_fund_name) > 0L) {
      stop(
        "`old_portfolio$real$portfolio$fund_name` must match the supplied `fund_name`. ",
        "Expected: ", fund_name, ". ",
        "Found: ", paste(unique(bad_old_real_fund_name$fund_name), collapse = ", "),
        ".",
        call. = FALSE
      )
    }
  }

  old_paper_for_id <- old_paper_portfolio %>%
    dplyr::filter(id == !!id)

  old_real_for_id <- old_real_portfolio %>%
    dplyr::filter(id == !!id)

  if (!is.null(fund_name)) {
    old_real_for_id <- old_real_for_id %>%
      dplyr::filter(fund_name == !!fund_name)
  } else {
      n_funds <- dplyr::n_distinct(old_real_for_id$fund_name)

      if (n_funds > 1L) {
        stop(
          "`fund_name = NULL` is only allowed when `id` maps to a single fund_name in old real portfolio.",
          call. = FALSE
        )
      }
  }

  if (nrow(old_paper_for_id) == 0L) {
    stop(
      "No rows found in `old_portfolio$paper$portfolio` for the supplied `id`.",
      call. = FALSE
    )
  }

  if (nrow(old_real_for_id) == 0L) {
    stop(
      "No rows found in `old_portfolio$real$portfolio` for the supplied `id` and `fund_name`.",
      call. = FALSE
    )
  }

  old_paper_last_date <- max(old_paper_for_id$date, na.rm = TRUE)
  old_real_last_date <- max(old_real_for_id$date, na.rm = TRUE)

  if (!is.finite(as.numeric(old_paper_last_date)) || is.na(old_paper_last_date)) {
    stop("Could not compute the last paper portfolio date.", call. = FALSE)
  }

  if (!is.finite(as.numeric(old_real_last_date)) || is.na(old_real_last_date)) {
    stop("Could not compute the last real portfolio date.", call. = FALSE)
  }

  if (old_paper_last_date != old_real_last_date) {
    stop(
      "Old paper and real portfolios must have the same last date. ",
      "old_paper_last_date = ", old_paper_last_date,
      ", old_real_last_date = ", old_real_last_date,
      ".",
      call. = FALSE
    )
  }

  old_port_last_date <- old_paper_last_date

  if (old_port_last_date >= min(current_dates)) {
    stop(
      "`old_portfolio` last date must be strictly before `min(current_dates)`. ",
      "old_port_last_date = ", old_port_last_date,
      ", min(current_dates) = ", min(current_dates),
      ".",
      call. = FALSE
    )
  }

  old_paper_last <- old_paper_for_id %>%
    dplyr::filter(date == !!old_port_last_date)

  old_real_last <- old_real_for_id %>%
    dplyr::filter(date == !!old_port_last_date)

  if (nrow(old_paper_last) == 0L) {
    stop("Could not extract the old paper last state.", call. = FALSE)
  }

  if (nrow(old_real_last) == 0L) {
    stop("Could not extract the old real last state.", call. = FALSE)
  }

  if (anyDuplicated(old_paper_last[c("id", "cvm_code_type")]) > 0L) {
    stop(
      "`old_portfolio$paper$portfolio` has duplicated `id + cvm_code_type` rows on the last date.",
      call. = FALSE
    )
  }

  if (anyDuplicated(old_real_last[c("id", "fund_name", "cvm_code_type")]) > 0L) {
    stop(
      "`old_portfolio$real$portfolio` has duplicated `id + fund_name + cvm_code_type` rows on the last date.",
      call. = FALSE
    )
  }

  if (any(old_paper_last$eop_weights < -weight_tolerance, na.rm = TRUE)) {
    stop("`old_portfolio$paper$portfolio$eop_weights` contains negative weights on the last date.", call. = FALSE)
  }

  old_paper_eop_weight_sum <- sum(old_paper_last$eop_weights, na.rm = TRUE)

  if (abs(old_paper_eop_weight_sum - 1) > weight_tolerance) {
    stop(
      "`old_portfolio$paper$portfolio$eop_weights` must sum to 1 on the last old date. Current sum = ",
      round(old_paper_eop_weight_sum, 8),
      ".",
      call. = FALSE
    )
  }

  if (any(old_real_last$eop_positions < -position_tolerance, na.rm = TRUE)) {
    stop("`old_portfolio$real$portfolio$eop_positions` contains negative positions on the last date.", call. = FALSE)
  }

  if (any(!is.finite(old_real_last$price)) || any(old_real_last$price <= 0, na.rm = TRUE)) {
    stop("`old_portfolio$real$portfolio$price` must be positive and finite on the last date.", call. = FALSE)
  }

  old_real_last <- old_real_last %>%
    dplyr::mutate(
      eop_market_value = eop_positions * price
    )

  old_port_last_eop_market_value <- sum(old_real_last$eop_market_value, na.rm = TRUE)

  if (!is.finite(old_port_last_eop_market_value) || old_port_last_eop_market_value <= 0) {
    stop(
      "Could not compute a positive `old_port_last_eop_market_value` from the last old real portfolio state.",
      call. = FALSE
    )
  }

  old_real_last <- old_real_last %>%
    dplyr::mutate(
      eop_weights = eop_market_value / old_port_last_eop_market_value
    )

  old_real_eop_weight_sum <- sum(old_real_last$eop_weights, na.rm = TRUE)

  if (abs(old_real_eop_weight_sum - 1) > weight_tolerance) {
    stop(
      "Computed old real portfolio eop_weights do not sum to 1 on the last old date. Current sum = ",
      round(old_real_eop_weight_sum, 8),
      ".",
      call. = FALSE
    )
  }

  old_port_last_eop_weights <- old_paper_last %>%
    dplyr::select(cvm_code_type, eop_weights)

  old_port_last_eop_positions <- old_real_last %>%
    dplyr::select(cvm_code_type, eop_positions)

  old_port_last_prices <- old_real_last %>%
    dplyr::select(cvm_code_type, price_lag = price)

  # Normalize rebalancing weights --------------------------------------------

  required_rebal_cols <- c("date", "id", "legacy_ticker", "cvm_code_type", "weights")
  missing_rebal_cols <- base::setdiff(required_rebal_cols, names(rebal_weights))

  if (length(missing_rebal_cols) > 0L) {
    stop(
      "`rebal_weights` is missing columns: ",
      paste(missing_rebal_cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  rebal_weights <- rebal_weights %>%
    dplyr::mutate(
      date = as.Date(date),
      id = as.character(id),
      legacy_ticker = as.character(legacy_ticker),
      cvm_code_type = as.character(cvm_code_type),
      weights = as.numeric(weights)
    ) %>%
    dplyr::filter(.data$id == .env$id)

  if (nrow(rebal_weights) == 0L) {
    stop("No rows found in `rebal_weights` for `id = ", id, "`.", call. = FALSE)
  }

  if (any(is.na(rebal_weights$date))) {
    stop("`rebal_weights$date` contains NA values after coercion.", call. = FALSE)
  }

  if (any(is.na(rebal_weights$id))) {
    stop("`rebal_weights$id` contains NA values after coercion.", call. = FALSE)
  }

  if (any(is.na(rebal_weights$cvm_code_type))) {
    stop("`rebal_weights$cvm_code_type` contains NA values after coercion.", call. = FALSE)
  }

  if (any(is.na(rebal_weights$weights))) {
    stop("`rebal_weights$weights` contains NA values after numeric coercion.", call. = FALSE)
  }

  if (any(rebal_weights$weights < -weight_tolerance, na.rm = TRUE)) {
    stop("`rebal_weights$weights` contains negative weights.", call. = FALSE)
  }

  if (anyDuplicated(rebal_weights[c("date", "id", "cvm_code_type")]) > 0L) {
    stop(
      "`rebal_weights` has duplicated date + id + cvm_code_type rows.",
      call. = FALSE
    )
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
      "Some rebalance weights do not sum to 1. First issue: date = ",
      rebal_weight_check$date[1],
      ", id = ",
      rebal_weight_check$id[1],
      ", weight_sum = ",
      round(rebal_weight_check$weight_sum[1], 8),
      ".",
      call. = FALSE
    )
  }

  # Normalize Comdinheiro data ------------------------------------------------
  # Convention: `ret_1d` arrives in percent points.
  # Example: 0.281 means 0.281%, so it becomes 0.00281.

  required_comd_cols <- c(
    "date",
    "legacy_ticker",
    "cvm_code_type",
    "ret_1d",
    "price",
    "proventos",
    "event_factor",
    "n_shares"
  )

  missing_comd_cols <- base::setdiff(required_comd_cols, names(comdinheiro_data))

  if (length(missing_comd_cols) > 0L) {
    stop(
      "`comdinheiro_data` is missing columns: ",
      paste(missing_comd_cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (!"proventos_date" %in% names(comdinheiro_data)) {
    comdinheiro_data$proventos_date <- as.Date(NA)
  }

  comdinheiro_data <- comdinheiro_data %>%
    dplyr::mutate(
      date = as.Date(date),
      legacy_ticker = as.character(legacy_ticker),
      cvm_code_type = as.character(cvm_code_type),
      ret_1d = as.numeric(ret_1d) / 100,
      price = as.numeric(price),
      proventos = dplyr::coalesce(as.numeric(proventos), 0),
      proventos_date = as.Date(proventos_date),
      event_factor = as.numeric(event_factor),
      n_shares = as.numeric(n_shares)
    ) %>%
    dplyr::filter(!stringr::str_detect(legacy_ticker, "Q$"))

  if (anyDuplicated(comdinheiro_data[c("date", "cvm_code_type")]) > 0L) {
    stop("`comdinheiro_data` has duplicated date + cvm_code_type rows.", call. = FALSE)
  }

  if (any(is.na(comdinheiro_data$date))) {
    stop("`comdinheiro_data$date` contains NA values after coercion.", call. = FALSE)
  }

  if (any(is.na(comdinheiro_data$legacy_ticker))) {
    stop("`comdinheiro_data$legacy_ticker` contains NA values.", call. = FALSE)
  }

  if (any(is.na(comdinheiro_data$cvm_code_type))) {
    stop("`comdinheiro_data$cvm_code_type` contains NA values.", call. = FALSE)
  }

  if (any(comdinheiro_data$price <= 0, na.rm = TRUE)) {
    stop("`comdinheiro_data$price` cannot contain non-positive prices.", call. = FALSE)
  }

  # Validate continuity -------------------------------------------------------

  available_dates <- sort(unique(comdinheiro_data$date))

  missing_current_dates <- base::setdiff(
    as.character(current_dates),
    as.character(available_dates)
  )

  if (length(missing_current_dates) > 0L) {
    stop(
      "`comdinheiro_data` is missing required `current_dates`: ",
      paste(as.character(missing_current_dates), collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  next_available_date <- available_dates[available_dates > old_port_last_date][1]

  if (is.na(next_available_date)) {
    stop(
      "Could not infer the next available market date after `old_portfolio` last date. ",
      "old_port_last_date = ", old_port_last_date,
      ".",
      call. = FALSE
    )
  }

  if (min(current_dates) != next_available_date) {
    stop(
      "`current_dates` must start exactly on the first available market date after `old_portfolio` last date. ",
      "old_port_last_date = ", old_port_last_date,
      ", expected_start_date = ", next_available_date,
      ", actual_start_date = ", min(current_dates),
      ".",
      call. = FALSE
    )
  }

  # Prepare brokerage data ----------------------------------------------------

  if (is.null(fund_name) || is.null(brokerage_data)) {
    trade_data <- data.frame(
      date = as.Date(character()),
      fund_name = character(),
      legacy_ticker = character(),
      cvm_code_type = character(),
      side = character(),
      amount = numeric(),
      price = numeric(),
      traded_volume = numeric(),
      brokerage_fee_estimated = numeric(),
      stringsAsFactors = FALSE
    )
  } else {
    trade_data <- if (is.list(brokerage_data) && "trade_data" %in% names(brokerage_data)) {
      brokerage_data$trade_data
    } else {
      brokerage_data
    }

    required_trade_cols <- c(
      "date",
      "fund_name",
      "legacy_ticker",
      "cvm_code_type",
      "side",
      "amount",
      "price",
      "traded_volume",
      "brokerage_fee_estimated"
    )

    missing_trade_cols <- base::setdiff(required_trade_cols, names(trade_data))

    if (length(missing_trade_cols) > 0L) {
      stop(
        "`brokerage_data$trade_data` is missing columns: ",
        paste(missing_trade_cols, collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    trade_data <- trade_data %>%
      dplyr::mutate(
        date = as.Date(date),
        fund_name = as.character(fund_name),
        legacy_ticker = as.character(legacy_ticker),
        cvm_code_type = as.character(cvm_code_type),
        side = as.character(side),
        amount = as.numeric(amount),
        price = as.numeric(price),
        traded_volume = as.numeric(traded_volume),
        brokerage_fee_estimated = as.numeric(brokerage_fee_estimated)
      ) %>%
      dplyr::filter(
        fund_name == !!fund_name,
        date %in% current_dates
      )

    if (stringr::str_detect(id, "_ativa_")) {
      message("Active portfolio identified. Removing ETF holdings from trade data")
      trade_data <- trade_data %>%
        dplyr::filter(
          !legacy_ticker %in% c("BOVA11", "BOVV11", "SMAL11", "IVVB11", "DIVO11", "BOVB11", "ISUS11")
        )
    }
  }

  if (nrow(trade_data) > 0L) {
    invalid_side <- base::setdiff(unique(trade_data$side), c("buy", "sell"))

    if (length(invalid_side) > 0L) {
      stop("`trade_data$side` must contain only 'buy' or 'sell'.", call. = FALSE)
    }

    if (any(is.na(trade_data$date))) {
      stop("`trade_data$date` contains NA values after coercion.", call. = FALSE)
    }

    if (any(is.na(trade_data$cvm_code_type))) {
      stop("`trade_data$cvm_code_type` contains NA values.", call. = FALSE)
    }

    if (any(is.na(trade_data$amount)) || any(trade_data$amount < 0, na.rm = TRUE)) {
      stop("`trade_data$amount` must be non-negative and non-missing.", call. = FALSE)
    }

    if (any(is.na(trade_data$price)) || any(trade_data$price <= 0, na.rm = TRUE)) {
      stop("`trade_data$price` must be positive and non-missing.", call. = FALSE)
    }

    if (any(is.na(trade_data$traded_volume)) || any(trade_data$traded_volume < 0, na.rm = TRUE)) {
      stop("`trade_data$traded_volume` must be non-negative and non-missing.", call. = FALSE)
    }

    if (
      any(is.na(trade_data$brokerage_fee_estimated)) ||
      any(trade_data$brokerage_fee_estimated < 0, na.rm = TRUE)
    ) {
      stop("`trade_data$brokerage_fee_estimated` must be non-negative and non-missing.", call. = FALSE)
    }
  }

  # Prepare split/inplit data -------------------------------------------------

  if (is.null(split_inplit_data)) {
    split_inplit_data <- data.frame(
      date = as.Date(character()),
      legacy_ticker = character(),
      cvm_code_type = character(),
      split_factor = numeric(),
      position_factor = numeric(),
      stringsAsFactors = FALSE
    )
  } else {
    required_split_cols <- c("date", "cvm_code_type", "split_factor")
    missing_split_cols <- base::setdiff(required_split_cols, names(split_inplit_data))

    if (length(missing_split_cols) > 0L) {
      stop(
        "`split_inplit_data` is missing columns: ",
        paste(missing_split_cols, collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    if (!"legacy_ticker" %in% names(split_inplit_data)) {
      split_inplit_data$legacy_ticker <- NA_character_
    }

    split_inplit_data <- split_inplit_data %>%
      dplyr::mutate(
        date = as.Date(date),
        legacy_ticker = as.character(legacy_ticker),
        cvm_code_type = as.character(cvm_code_type),
        split_factor = as.numeric(split_factor),
        position_factor = 1 / split_factor
      ) %>%
      dplyr::filter(date %in% current_dates)

    if (anyDuplicated(split_inplit_data[c("date", "cvm_code_type")]) > 0L) {
      stop("`split_inplit_data` has duplicated date + cvm_code_type rows.", call. = FALSE)
    }

    if (any(is.na(split_inplit_data$date))) {
      stop("`split_inplit_data$date` contains NA values after coercion.", call. = FALSE)
    }

    if (any(is.na(split_inplit_data$cvm_code_type))) {
      stop("`split_inplit_data$cvm_code_type` contains NA values.", call. = FALSE)
    }

    if (
      any(is.na(split_inplit_data$split_factor)) ||
      any(!is.finite(split_inplit_data$split_factor)) ||
      any(split_inplit_data$split_factor <= 0, na.rm = TRUE)
    ) {
      stop(
        "`split_inplit_data$split_factor` must be positive, finite, and non-missing.",
        call. = FALSE
      )
    }

    if (
      any(is.na(split_inplit_data$position_factor)) ||
      any(!is.finite(split_inplit_data$position_factor)) ||
      any(split_inplit_data$position_factor <= 0, na.rm = TRUE)
    ) {
      stop(
        "`split_inplit_data$position_factor` must be positive, finite, and non-missing.",
        call. = FALSE
      )
    }
  }

  # Prepare other_events data --------------------------------------------------
  if (is.null(other_events_data)) {
    other_events_data <- data.frame(
      date = as.Date(character()),
      old_legacy_ticker = character(),
      old_cvm_code_type = character(),
      new_legacy_ticker = character(),
      new_cvm_code_type = character(),
      adj_factor = numeric(),
      stringsAsFactors = FALSE
    )
  } else {
    required_other_events_cols <- c("date", "old_cvm_code_type", "new_cvm_code_type", "adj_factor")
    missing_other_events_cols <- base::setdiff(required_other_events_cols, names(other_events_data))
    if (length(missing_other_events_cols) > 0L) {
      stop(
        "`other_events_data` is missing columns: ",
        paste(missing_other_events_cols, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
    if (!"old_legacy_ticker" %in% names(other_events_data)) {
      other_events_data$old_legacy_ticker <- NA_character_
    }
    if (!"new_legacy_ticker" %in% names(other_events_data)) {
      other_events_data$new_legacy_ticker <- NA_character_
    }
    other_events_data <- other_events_data %>%
      dplyr::mutate(
        date = as.Date(date),
        old_legacy_ticker = as.character(old_legacy_ticker),
        old_cvm_code_type = as.character(old_cvm_code_type),
        new_legacy_ticker = as.character(new_legacy_ticker),
        new_cvm_code_type = as.character(new_cvm_code_type),
        adj_factor = as.numeric(adj_factor)
      ) %>%
      dplyr::filter(date %in% current_dates)
    if (anyDuplicated(other_events_data[c("date", "old_cvm_code_type", "new_cvm_code_type")]) > 0L) {
      stop("`other_events_data` has duplicated date + old_cvm_code_type + new_cvm_code_type rows.", call. = FALSE)
    }
    if (any(is.na(other_events_data$date))) {
      stop("`other_events_data$date` contains NA values after coercion.", call. = FALSE)
    }
    if (any(is.na(other_events_data$old_cvm_code_type))) {
      stop("`other_events_data$old_cvm_code_type` contains NA values.", call. = FALSE)
    }
    if (any(is.na(other_events_data$new_cvm_code_type))) {
      stop("`other_events_data$new_cvm_code_type` contains NA values.", call. = FALSE)
    }
    if (
      any(is.na(other_events_data$adj_factor)) ||
      any(!is.finite(other_events_data$adj_factor)) ||
      any(other_events_data$adj_factor <= 0, na.rm = TRUE)
    ) {
      stop(
        "`other_events_data$adj_factor` must be positive, finite, and non-missing.",
        call. = FALSE
      )
    }
  }

  # Prepare paper transaction costs ------------------------------------------

  if (is.null(transaction_costs_bps)) {
    transaction_costs_bps <- data.frame(
      date = current_dates,
      id = id,
      transaction_cost_bps = 0,
      stringsAsFactors = FALSE
    )
  }

  required_cost_cols <- c("date", "id", "transaction_cost_bps")
  missing_cost_cols <- base::setdiff(required_cost_cols, names(transaction_costs_bps))

  if (length(missing_cost_cols) > 0L) {
    stop(
      "`transaction_costs_bps` is missing columns: ",
      paste(missing_cost_cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  transaction_costs_bps <- transaction_costs_bps %>%
    dplyr::mutate(
      date = as.Date(date),
      id = as.character(id),
      transaction_cost_bps = as.numeric(transaction_cost_bps)
    ) %>%
    dplyr::filter(.data$id == .env$id)

  if (any(is.na(transaction_costs_bps$date))) {
    stop("`transaction_costs_bps$date` contains NA values after coercion.", call. = FALSE)
  }

  if (any(is.na(transaction_costs_bps$id))) {
    stop("`transaction_costs_bps$id` contains NA values.", call. = FALSE)
  }

  if (any(is.na(transaction_costs_bps$transaction_cost_bps))) {
    stop("`transaction_costs_bps$transaction_cost_bps` contains NA values after coercion.", call. = FALSE)
  }

  if (any(transaction_costs_bps$transaction_cost_bps < -1e-12, na.rm = TRUE)) {
    stop("`transaction_costs_bps` cannot contain negative costs.", call. = FALSE)
  }

  if (anyDuplicated(transaction_costs_bps[c("date", "id")]) > 0L) {
    stop("`transaction_costs_bps` must contain at most one row per date + id.", call. = FALSE)
  }

  # Validate fees -------------------------------------------------------------

  if (!is.numeric(fund_fees_bps) || length(fund_fees_bps) != 1L || is.na(fund_fees_bps)) {
    stop("`fund_fees_bps` must be a single numeric value.", call. = FALSE)
  }

  if (fund_fees_bps < 0) {
    stop("`fund_fees_bps` cannot be negative.", call. = FALSE)
  }

  daily_fee_return <- fund_fees_bps / 10000

  # Return validated state ----------------------------------------------------

  list(
    current_dates = current_dates,
    old_portfolio = list(
      paper = list(
        portfolio = old_paper_portfolio
      ),
      real = list(
        portfolio = old_real_portfolio
      )
    ),
    old_paper_last = old_paper_last,
    old_real_last = old_real_last,
    id = id,
    fund_name = fund_name,
    old_port_last_date = old_port_last_date,
    old_port_last_eop_market_value = old_port_last_eop_market_value,
    old_port_last_eop_weights = old_port_last_eop_weights,
    old_port_last_eop_positions = old_port_last_eop_positions,
    old_port_last_prices = old_port_last_prices,
    rebal_weights = rebal_weights,
    comdinheiro_data = comdinheiro_data,
    trade_data = trade_data,
    split_inplit_data = split_inplit_data,
    other_events_data = other_events_data,
    transaction_costs_bps = transaction_costs_bps,
    daily_fee_return = daily_fee_return
  )
}

validate_evolve_portfolio_output <- function(
    out,
    current_dates = NULL,
    id = NULL,
    fund_name = NULL,
    weight_tolerance = 1e-2,
    position_tolerance = 1e-8,
    require_bop_tables = TRUE
) {

  check_named_list <- function(x, required_names, object_name) {
    if (!is.list(x)) {
      stop("`", object_name, "` must be a list.", call. = FALSE)
    }

    missing_names <- base::setdiff(required_names, names(x))

    if (length(missing_names) > 0L) {
      stop(
        "`", object_name, "` is missing required element(s): ",
        paste(missing_names, collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    invisible(TRUE)
  }

  check_data_frame <- function(df, object_name, allow_empty = FALSE) {
    if (!is.data.frame(df)) {
      stop("`", object_name, "` must be a data.frame.", call. = FALSE)
    }

    if (!isTRUE(allow_empty) && nrow(df) == 0L) {
      stop("`", object_name, "` is empty.", call. = FALSE)
    }

    invisible(TRUE)
  }

  check_required_cols <- function(df, required_cols, object_name) {
    missing_cols <- base::setdiff(required_cols, names(df))

    if (length(missing_cols) > 0L) {
      stop(
        "`", object_name, "` is missing required column(s): ",
        paste(missing_cols, collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    invisible(TRUE)
  }

  check_no_duplicate_keys <- function(df, key_cols, object_name) {
    duplicated_keys <- df %>%
      dplyr::count(dplyr::across(dplyr::all_of(key_cols)), name = "n") %>%
      dplyr::filter(n > 1L)

    if (nrow(duplicated_keys) > 0L) {
      first_issue <- duplicated_keys[1L, key_cols, drop = FALSE]

      stop(
        "`", object_name, "` contains duplicated key rows. First issue: ",
        paste(
          paste0(names(first_issue), " = ", as.character(first_issue[1L, ])),
          collapse = ", "
        ),
        ".",
        call. = FALSE
      )
    }

    invisible(TRUE)
  }

  check_finite_cols <- function(df, cols, object_name) {
    for (col in cols) {
      bad_rows <- which(!is.finite(df[[col]]))

      if (length(bad_rows) > 0L) {
        stop(
          "`", object_name, "$", col, "` contains non-finite values. ",
          "First bad row: ", bad_rows[1L], ".",
          call. = FALSE
        )
      }
    }

    invisible(TRUE)
  }

  check_non_negative_cols <- function(df, cols, object_name, tolerance = 0) {
    for (col in cols) {
      bad_rows <- which(df[[col]] < -tolerance)

      if (length(bad_rows) > 0L) {
        stop(
          "`", object_name, "$", col, "` contains negative values below tolerance. ",
          "First bad row: ", bad_rows[1L],
          ", value = ", df[[col]][bad_rows[1L]], ".",
          call. = FALSE
        )
      }
    }

    invisible(TRUE)
  }

  check_positive_cols <- function(df, cols, object_name) {
    for (col in cols) {
      bad_rows <- which(!is.finite(df[[col]]) | df[[col]] <= 0)

      if (length(bad_rows) > 0L) {
        stop(
          "`", object_name, "$", col, "` must contain strictly positive finite values. ",
          "First bad row: ", bad_rows[1L],
          ", value = ", df[[col]][bad_rows[1L]], ".",
          call. = FALSE
        )
      }
    }

    invisible(TRUE)
  }

  check_weight_sums <- function(
    df,
    weight_col,
    group_cols,
    object_name,
    tolerance
  ) {
    weight_sum_tbl <- df %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
      dplyr::summarise(
        weight_sum = sum(.data[[weight_col]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        weight_error = abs(weight_sum - 1)
      )

    bad_sums <- weight_sum_tbl %>%
      dplyr::filter(weight_error > tolerance)

    if (nrow(bad_sums) > 0L) {
      first_issue <- bad_sums[1L, group_cols, drop = FALSE]

      stop(
        "`", object_name, "` has invalid weight sums. First issue: ",
        paste(
          paste0(names(first_issue), " = ", as.character(first_issue[1L, ])),
          collapse = ", "
        ),
        ", weight_sum = ", round(bad_sums$weight_sum[1L], 8),
        ".",
        call. = FALSE
      )
    }

    invisible(TRUE)
  }

  check_expected_dates <- function(df, date_col, object_name, expected_dates) {
    if (is.null(expected_dates)) {
      return(invisible(TRUE))
    }

    df_dates <- sort(unique(as.Date(df[[date_col]])))
    expected_dates <- sort(unique(as.Date(expected_dates)))

    missing_dates <- base::setdiff(expected_dates, df_dates)
    extra_dates <- base::setdiff(df_dates, expected_dates)

    if (length(missing_dates) > 0L) {
      stop(
        "`", object_name, "` is missing expected date(s): ",
        paste(as.character(missing_dates), collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    if (length(extra_dates) > 0L) {
      stop(
        "`", object_name, "` contains unexpected date(s): ",
        paste(as.character(extra_dates), collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    invisible(TRUE)
  }

  check_expected_id <- function(df, id_col, object_name, expected_id) {
    if (is.null(expected_id)) {
      return(invisible(TRUE))
    }

    bad_rows <- which(df[[id_col]] != expected_id | is.na(df[[id_col]]))

    if (length(bad_rows) > 0L) {
      stop(
        "`", object_name, "$", id_col, "` contains values different from expected `id`. ",
        "First bad row: ", bad_rows[1L], ".",
        call. = FALSE
      )
    }

    invisible(TRUE)
  }

  check_expected_fund_name <- function(df, fund_col, object_name, expected_fund_name) {
    if (is.null(expected_fund_name)) {
      bad_rows <- which(!is.na(df[[fund_col]]))

      if (length(bad_rows) > 0L) {
        stop(
          "`", object_name, "$", fund_col, "` must be NA when `fund_name = NULL`. ",
          "First bad row: ", bad_rows[1L], ".",
          call. = FALSE
        )
      }

      return(invisible(TRUE))
    }

    bad_rows <- which(df[[fund_col]] != expected_fund_name | is.na(df[[fund_col]]))

    if (length(bad_rows) > 0L) {
      stop(
        "`", object_name, "$", fund_col, "` contains values different from expected `fund_name`. ",
        "First bad row: ", bad_rows[1L], ".",
        call. = FALSE
      )
    }

    invisible(TRUE)
  }

  check_return_identity <- function(df, object_name, cost_col, fee_col) {
    required_cols <- c("raw_return", "net_return", cost_col, fee_col)
    check_required_cols(df, required_cols, object_name)

    implied_net_return <- (1 + df$raw_return) *
      (1 - df[[cost_col]]) *
      (1 - df[[fee_col]]) - 1

    bad_rows <- which(abs(df$net_return - implied_net_return) > 1e-10)

    if (length(bad_rows) > 0L) {
      stop(
        "`", object_name, "` has inconsistent net return identity. ",
        "First bad row: ", bad_rows[1L],
        ", observed net_return = ", df$net_return[bad_rows[1L]],
        ", implied net_return = ", implied_net_return[bad_rows[1L]],
        ".",
        call. = FALSE
      )
    }

    invisible(TRUE)
  }

  check_numeric_allow_na_cols <- function(df, cols, object_name) {
    for (col in cols) {
      if (!is.numeric(df[[col]])) {
        stop(
          "`", object_name, "$", col, "` must be numeric.",
          call. = FALSE
        )
      }

      bad_rows <- which(!is.na(df[[col]]) & !is.finite(df[[col]]))

      if (length(bad_rows) > 0L) {
        stop(
          "`", object_name, "$", col, "` contains non-finite non-NA values. ",
          "First bad row: ", bad_rows[1L], ".",
          call. = FALSE
        )
      }
    }

    invisible(TRUE)
  }

  # Structure -----------------------------------------------------------------

  check_named_list(
    out,
    required_names = c("paper", "real", "workflow", "diagnostics"),
    object_name = "out"
  )

  check_named_list(
    out$paper,
    required_names = c(
      "portfolio",
      "weights",
      "returns",
      "market_value",
      "turnover",
      "costs",
      "fees"
    ),
    object_name = "out$paper"
  )

  check_named_list(
    out$paper$weights,
    required_names = c("bop_weights", "eop_weights"),
    object_name = "out$paper$weights"
  )

  check_named_list(
    out$real,
    required_names = c(
      "portfolio",
      "positions",
      "returns",
      "market_value",
      "trades",
      "splits",
      "turnover",
      "costs",
      "fees"
    ),
    object_name = "out$real"
  )

  check_named_list(
    out$real$positions,
    required_names = c("bop_positions", "eop_positions"),
    object_name = "out$real$positions"
  )

  # Paper portfolio -----------------------------------------------------------

  check_data_frame(out$paper$portfolio, "out$paper$portfolio")
  check_required_cols(
    out$paper$portfolio,
    required_cols = c(
      "date",
      "id",
      "fund_name",
      "legacy_ticker",
      "cvm_code_type",
      "bop_weights",
      "ret_1d",
      "drifted_weights",
      "eop_weights"
    ),
    object_name = "out$paper$portfolio"
  )

  check_expected_dates(out$paper$portfolio, "date", "out$paper$portfolio", current_dates)
  check_expected_id(out$paper$portfolio, "id", "out$paper$portfolio", id)
  check_expected_fund_name(out$paper$portfolio, "fund_name", "out$paper$portfolio", fund_name)

  check_finite_cols(
    out$paper$portfolio,
    cols = c("bop_weights", "drifted_weights", "eop_weights"),
    object_name = "out$paper$portfolio"
  )

  check_numeric_allow_na_cols(
    out$paper$portfolio,
    cols = c("ret_1d"),
    object_name = "out$paper$portfolio"
  )

  check_non_negative_cols(
    out$paper$portfolio,
    cols = c("bop_weights", "drifted_weights", "eop_weights"),
    object_name = "out$paper$portfolio",
    tolerance = weight_tolerance
  )

  check_weight_sums(
    out$paper$portfolio,
    weight_col = "bop_weights",
    group_cols = c("date", "id"),
    object_name = "out$paper$portfolio$bop_weights",
    tolerance = weight_tolerance
  )

  check_weight_sums(
    out$paper$portfolio,
    weight_col = "drifted_weights",
    group_cols = c("date", "id"),
    object_name = "out$paper$portfolio$drifted_weights",
    tolerance = weight_tolerance
  )

  check_weight_sums(
    out$paper$portfolio,
    weight_col = "eop_weights",
    group_cols = c("date", "id"),
    object_name = "out$paper$portfolio$eop_weights",
    tolerance = weight_tolerance
  )

  check_no_duplicate_keys(
    out$paper$portfolio,
    key_cols = c("date", "id", "cvm_code_type"),
    object_name = "out$paper$portfolio"
  )

  # Paper weight tables -------------------------------------------------------

  check_data_frame(out$paper$weights$eop_weights, "out$paper$weights$eop_weights")
  check_required_cols(
    out$paper$weights$eop_weights,
    required_cols = c("date", "id", "fund_name", "legacy_ticker", "cvm_code_type", "weights"),
    object_name = "out$paper$weights$eop_weights"
  )

  check_expected_dates(out$paper$weights$eop_weights, "date", "out$paper$weights$eop_weights", current_dates)
  check_expected_id(out$paper$weights$eop_weights, "id", "out$paper$weights$eop_weights", id)

  check_finite_cols(
    out$paper$weights$eop_weights,
    cols = c("weights"),
    object_name = "out$paper$weights$eop_weights"
  )

  check_non_negative_cols(
    out$paper$weights$eop_weights,
    cols = c("weights"),
    object_name = "out$paper$weights$eop_weights",
    tolerance = weight_tolerance
  )

  check_weight_sums(
    out$paper$weights$eop_weights,
    weight_col = "weights",
    group_cols = c("date", "id"),
    object_name = "out$paper$weights$eop_weights",
    tolerance = weight_tolerance
  )

  check_no_duplicate_keys(
    out$paper$weights$eop_weights,
    key_cols = c("date", "id", "cvm_code_type"),
    object_name = "out$paper$weights$eop_weights"
  )

  if (isTRUE(require_bop_tables)) {
    check_data_frame(out$paper$weights$bop_weights, "out$paper$weights$bop_weights")
    check_required_cols(
      out$paper$weights$bop_weights,
      required_cols = c("date", "id", "fund_name", "legacy_ticker", "cvm_code_type", "weights"),
      object_name = "out$paper$weights$bop_weights"
    )

    check_expected_dates(out$paper$weights$bop_weights, "date", "out$paper$weights$bop_weights", current_dates)
    check_expected_id(out$paper$weights$bop_weights, "id", "out$paper$weights$bop_weights", id)

    check_finite_cols(
      out$paper$weights$bop_weights,
      cols = c("weights"),
      object_name = "out$paper$weights$bop_weights"
    )

    check_weight_sums(
      out$paper$weights$bop_weights,
      weight_col = "weights",
      group_cols = c("date", "id"),
      object_name = "out$paper$weights$bop_weights",
      tolerance = weight_tolerance
    )

    check_no_duplicate_keys(
      out$paper$weights$bop_weights,
      key_cols = c("date", "id", "cvm_code_type"),
      object_name = "out$paper$weights$bop_weights"
    )
  }

  # Paper returns, market value, turnover, costs, fees -------------------------

  check_data_frame(out$paper$returns, "out$paper$returns")
  check_required_cols(
    out$paper$returns,
    required_cols = c("date", "id", "fund_name", "raw_return", "net_return"),
    object_name = "out$paper$returns"
  )

  check_expected_dates(out$paper$returns, "date", "out$paper$returns", current_dates)
  check_expected_id(out$paper$returns, "id", "out$paper$returns", id)
  check_finite_cols(out$paper$returns, c("raw_return", "net_return"), "out$paper$returns")
  check_no_duplicate_keys(out$paper$returns, c("date", "id"), "out$paper$returns")

  check_data_frame(out$paper$market_value, "out$paper$market_value")
  check_required_cols(
    out$paper$market_value,
    required_cols = c("date", "id", "fund_name", "eop_market_value"),
    object_name = "out$paper$market_value"
  )
  check_expected_dates(out$paper$market_value, "date", "out$paper$market_value", current_dates)
  check_expected_id(out$paper$market_value, "id", "out$paper$market_value", id)
  check_positive_cols(out$paper$market_value, c("eop_market_value"), "out$paper$market_value")
  check_no_duplicate_keys(out$paper$market_value, c("date", "id"), "out$paper$market_value")

  check_data_frame(out$paper$turnover, "out$paper$turnover")
  check_required_cols(
    out$paper$turnover,
    required_cols = c("date", "id", "fund_name", "turnover"),
    object_name = "out$paper$turnover"
  )
  check_expected_dates(out$paper$turnover, "date", "out$paper$turnover", current_dates)
  check_expected_id(out$paper$turnover, "id", "out$paper$turnover", id)
  check_finite_cols(out$paper$turnover, c("turnover"), "out$paper$turnover")
  check_non_negative_cols(out$paper$turnover, c("turnover"), "out$paper$turnover", tolerance = 0)

  check_data_frame(out$paper$costs, "out$paper$costs")
  check_required_cols(
    out$paper$costs,
    required_cols = c("date", "id", "fund_name", "transaction_cost_bps", "transaction_cost_return"),
    object_name = "out$paper$costs"
  )
  check_expected_dates(out$paper$costs, "date", "out$paper$costs", current_dates)
  check_expected_id(out$paper$costs, "id", "out$paper$costs", id)
  check_finite_cols(out$paper$costs, c("transaction_cost_bps", "transaction_cost_return"), "out$paper$costs")
  check_non_negative_cols(out$paper$costs, c("transaction_cost_bps", "transaction_cost_return"), "out$paper$costs", tolerance = 0)

  check_data_frame(out$paper$fees, "out$paper$fees")
  check_required_cols(
    out$paper$fees,
    required_cols = c("date", "id", "fund_name", "fund_fees_bps", "daily_fee_return"),
    object_name = "out$paper$fees"
  )
  check_expected_dates(out$paper$fees, "date", "out$paper$fees", current_dates)
  check_expected_id(out$paper$fees, "id", "out$paper$fees", id)
  check_finite_cols(out$paper$fees, c("fund_fees_bps", "daily_fee_return"), "out$paper$fees")

  paper_return_check <- out$paper$returns %>%
    dplyr::left_join(
      out$paper$costs %>%
        dplyr::select(date, id, transaction_cost_return),
      by = c("date", "id")
    ) %>%
    dplyr::left_join(
      out$paper$fees %>%
        dplyr::select(date, id, daily_fee_return),
      by = c("date", "id")
    )

  check_return_identity(
    paper_return_check,
    object_name = "paper return identity",
    cost_col = "transaction_cost_return",
    fee_col = "daily_fee_return"
  )

  # Real portfolio ------------------------------------------------------------
  check_data_frame(out$real$portfolio, "out$real$portfolio")
  check_required_cols(
    out$real$portfolio,
    required_cols = c(
      "date",
      "id",
      "fund_name",
      "legacy_ticker",
      "cvm_code_type",
      "bop_positions_before_split",
      "bop_positions_before_other_events",
      "position_factor",
      "bop_positions",
      "price_last_close",
      "market_value_last_close",
      "bop_weights",
      "eop_positions",
      "price",
      "ret_1d",
      "eop_market_value",
      "eop_weights",
      "dividends_per_share",
      "dividends_received",
      "net_trade",
      "net_traded_volume",
      "brokerage_fee_estimated",
      "avg_trade_price"
    ),
    object_name = "out$real$portfolio"
  )

  check_expected_dates(out$real$portfolio, "date", "out$real$portfolio", current_dates)
  check_expected_id(out$real$portfolio, "id", "out$real$portfolio", id)
  check_expected_fund_name(out$real$portfolio, "fund_name", "out$real$portfolio", fund_name)

  check_finite_cols(
    out$real$portfolio,
    cols = c(
      "bop_positions_before_split",
      "bop_positions_before_other_events",
      "position_factor",
      "bop_positions",
      "price_last_close",
      "market_value_last_close",
      "bop_weights",
      "eop_positions",
      "price",
      "eop_market_value",
      "eop_weights",
      "dividends_per_share",
      "dividends_received",
      "net_trade",
      "net_traded_volume",
      "brokerage_fee_estimated",
      "avg_trade_price"
    ),
    object_name = "out$real$portfolio"
  )

  check_numeric_allow_na_cols(
    out$real$portfolio,
    cols = c("ret_1d"),
    object_name = "out$real$portfolio"
  )

  check_positive_cols(out$real$portfolio, c("price_last_close", "price"), "out$real$portfolio")

  check_non_negative_cols(
    out$real$portfolio,
    cols = c(
      "bop_positions_before_split",
      "bop_positions_before_other_events",
      "position_factor",
      "bop_positions",
      "market_value_last_close",
      "bop_weights",
      "eop_positions",
      "eop_market_value",
      "eop_weights",
      "brokerage_fee_estimated"
    ),
    object_name = "out$real$portfolio",
    tolerance = position_tolerance
  )

  check_weight_sums(
    out$real$portfolio,
    weight_col = "bop_weights",
    group_cols = c("date", "id", "fund_name"),
    object_name = "out$real$portfolio$bop_weights",
    tolerance = weight_tolerance
  )

  check_weight_sums(
    out$real$portfolio,
    weight_col = "eop_weights",
    group_cols = c("date", "id", "fund_name"),
    object_name = "out$real$portfolio$eop_weights",
    tolerance = weight_tolerance
  )

  check_no_duplicate_keys(
    out$real$portfolio,
    key_cols = c("date", "id", "fund_name", "cvm_code_type"),
    object_name = "out$real$portfolio"
  )

  # Real position tables ------------------------------------------------------

  check_data_frame(out$real$positions$eop_positions, "out$real$positions$eop_positions")
  check_required_cols(
    out$real$positions$eop_positions,
    required_cols = c(
      "date",
      "id",
      "fund_name",
      "legacy_ticker",
      "cvm_code_type",
      "positions",
      "price",
      "market_value",
      "weights"
    ),
    object_name = "out$real$positions$eop_positions"
  )

  check_expected_dates(out$real$positions$eop_positions, "date", "out$real$positions$eop_positions", current_dates)
  check_expected_id(out$real$positions$eop_positions, "id", "out$real$positions$eop_positions", id)
  check_expected_fund_name(out$real$positions$eop_positions, "fund_name", "out$real$positions$eop_positions", fund_name)

  check_finite_cols(
    out$real$positions$eop_positions,
    cols = c("positions", "price", "market_value", "weights"),
    object_name = "out$real$positions$eop_positions"
  )

  check_non_negative_cols(
    out$real$positions$eop_positions,
    cols = c("positions", "market_value", "weights"),
    object_name = "out$real$positions$eop_positions",
    tolerance = position_tolerance
  )

  check_positive_cols(out$real$positions$eop_positions, c("price"), "out$real$positions$eop_positions")

  check_weight_sums(
    out$real$positions$eop_positions,
    weight_col = "weights",
    group_cols = c("date", "id", "fund_name"),
    object_name = "out$real$positions$eop_positions",
    tolerance = weight_tolerance
  )

  check_no_duplicate_keys(
    out$real$positions$eop_positions,
    key_cols = c("date", "id", "fund_name", "cvm_code_type"),
    object_name = "out$real$positions$eop_positions"
  )

  if (isTRUE(require_bop_tables)) {
    check_data_frame(out$real$positions$bop_positions, "out$real$positions$bop_positions")
    check_required_cols(
      out$real$positions$bop_positions,
      required_cols = c(
        "date",
        "id",
        "fund_name",
        "legacy_ticker",
        "cvm_code_type",
        "positions",
        "price_last_close",
        "market_value",
        "weights"
      ),
      object_name = "out$real$positions$bop_positions"
    )

    check_expected_dates(out$real$positions$bop_positions, "date", "out$real$positions$bop_positions", current_dates)
    check_expected_id(out$real$positions$bop_positions, "id", "out$real$positions$bop_positions", id)
    check_expected_fund_name(out$real$positions$bop_positions, "fund_name", "out$real$positions$bop_positions", fund_name)

    check_finite_cols(
      out$real$positions$bop_positions,
      cols = c("positions", "price", "market_value", "weights"),
      object_name = "out$real$positions$bop_positions"
    )

    check_non_negative_cols(
      out$real$positions$bop_positions,
      cols = c("positions", "market_value", "weights"),
      object_name = "out$real$positions$bop_positions",
      tolerance = position_tolerance
    )

    check_weight_sums(
      out$real$positions$bop_positions,
      weight_col = "weights",
      group_cols = c("date", "id", "fund_name"),
      object_name = "out$real$positions$bop_positions",
      tolerance = weight_tolerance
    )

    check_no_duplicate_keys(
      out$real$positions$bop_positions,
      key_cols = c("date", "id", "fund_name", "cvm_code_type"),
      object_name = "out$real$positions$bop_positions"
    )
  }

  # Real returns, market value, turnover, costs, fees --------------------------

  check_data_frame(out$real$returns, "out$real$returns")
  check_required_cols(
    out$real$returns,
    required_cols = c(
      "date",
      "id",
      "fund_name",
      "raw_return",
      "market_value_last_close",
      "eop_market_value",
      "dividends_received",
      "net_traded_volume",
      "net_return",
      "brokerage_cost_return",
      "daily_fee_return"
    ),
    object_name = "out$real$returns"
  )

  check_expected_dates(out$real$returns, "date", "out$real$returns", current_dates)
  check_expected_id(out$real$returns, "id", "out$real$returns", id)
  check_expected_fund_name(out$real$returns, "fund_name", "out$real$returns", fund_name)

  check_finite_cols(
    out$real$returns,
    cols = c(
      "raw_return",
      "market_value_last_close",
      "eop_market_value",
      "dividends_received",
      "net_traded_volume",
      "net_return",
      "brokerage_cost_return",
      "daily_fee_return"
    ),
    object_name = "out$real$returns"
  )

  check_positive_cols(out$real$returns, c("market_value_last_close", "eop_market_value"), "out$real$returns")
  check_non_negative_cols(out$real$returns, c("dividends_received", "brokerage_cost_return", "daily_fee_return"), "out$real$returns", tolerance = 0)
  check_no_duplicate_keys(out$real$returns, c("date", "id", "fund_name"), "out$real$returns")

  check_return_identity(
    out$real$returns,
    object_name = "real return identity",
    cost_col = "brokerage_cost_return",
    fee_col = "daily_fee_return"
  )

  check_data_frame(out$real$market_value, "out$real$market_value")
  check_required_cols(
    out$real$market_value,
    required_cols = c("date", "id", "fund_name", "market_value"),
    object_name = "out$real$market_value"
  )
  check_expected_dates(out$real$market_value, "date", "out$real$market_value", current_dates)
  check_expected_id(out$real$market_value, "id", "out$real$market_value", id)
  check_expected_fund_name(out$real$market_value, "fund_name", "out$real$market_value", fund_name)
  check_positive_cols(out$real$market_value, c("market_value"), "out$real$market_value")
  check_no_duplicate_keys(out$real$market_value, c("date", "id", "fund_name"), "out$real$market_value")

  check_data_frame(out$real$turnover, "out$real$turnover")
  check_required_cols(
    out$real$turnover,
    required_cols = c("date", "id", "fund_name", "turnover"),
    object_name = "out$real$turnover"
  )
  check_expected_dates(out$real$turnover, "date", "out$real$turnover", current_dates)
  check_expected_id(out$real$turnover, "id", "out$real$turnover", id)
  check_expected_fund_name(out$real$turnover, "fund_name", "out$real$turnover", fund_name)
  check_finite_cols(out$real$turnover, c("turnover"), "out$real$turnover")
  check_non_negative_cols(out$real$turnover, c("turnover"), "out$real$turnover", tolerance = 0)

  check_data_frame(out$real$costs, "out$real$costs")
  check_required_cols(
    out$real$costs,
    required_cols = c("date", "id", "fund_name", "brokerage_fee", "brokerage_cost_return"),
    object_name = "out$real$costs"
  )
  check_expected_dates(out$real$costs, "date", "out$real$costs", current_dates)
  check_expected_id(out$real$costs, "id", "out$real$costs", id)
  check_expected_fund_name(out$real$costs, "fund_name", "out$real$costs", fund_name)
  check_finite_cols(out$real$costs, c("brokerage_fee", "brokerage_cost_return"), "out$real$costs")
  check_non_negative_cols(out$real$costs, c("brokerage_fee", "brokerage_cost_return"), "out$real$costs", tolerance = 0)

  check_data_frame(out$real$fees, "out$real$fees")
  check_required_cols(
    out$real$fees,
    required_cols = c("date", "id", "fund_name", "fund_fees_bps", "daily_fee_return"),
    object_name = "out$real$fees"
  )
  check_expected_dates(out$real$fees, "date", "out$real$fees", current_dates)
  check_expected_id(out$real$fees, "id", "out$real$fees", id)
  check_expected_fund_name(out$real$fees, "fund_name", "out$real$fees", fund_name)
  check_finite_cols(out$real$fees, c("fund_fees_bps", "daily_fee_return"), "out$real$fees")

  real_raw_return_identity <- out$real$returns %>%
    dplyr::mutate(
      implied_raw_return = (
        (eop_market_value + dividends_received - net_traded_volume) /
          market_value_last_close
      ) - 1,
      raw_return_error = abs(raw_return - implied_raw_return)
    )

  bad_real_raw_return_identity <- real_raw_return_identity %>%
    dplyr::filter(raw_return_error > 1e-10)

  if (nrow(bad_real_raw_return_identity) > 0L) {
    stop(
      "`out$real$returns` has inconsistent raw return accounting identity. ",
      "First issue: date = ", bad_real_raw_return_identity$date[1L],
      ", id = ", bad_real_raw_return_identity$id[1L],
      ", fund_name = ", bad_real_raw_return_identity$fund_name[1L],
      ", raw_return = ", bad_real_raw_return_identity$raw_return[1L],
      ", implied_raw_return = ", bad_real_raw_return_identity$implied_raw_return[1L],
      ".",
      call. = FALSE
    )
  }

  # real trades and splits ------------------------------------------

  check_data_frame(out$real$trades, "out$real$trades", allow_empty = TRUE)

  if (nrow(out$real$trades) > 0L) {
    check_required_cols(
      out$real$trades,
      required_cols = c(
        "cvm_code_type",
        "net_trade",
        "net_traded_volume",
        "brokerage_fee_estimated",
        "avg_trade_price",
        "id"
      ),
      object_name = "out$real$trades"
    )

    check_expected_id(out$real$trades, "id", "out$real$trades", id)

    check_finite_cols(
      out$real$trades,
      cols = c("net_trade", "net_traded_volume", "brokerage_fee_estimated", "avg_trade_price"),
      object_name = "out$real$trades"
    )

    check_non_negative_cols(
      out$real$trades,
      cols = c("brokerage_fee_estimated"),
      object_name = "out$real$trades",
      tolerance = 0
    )
  }

  check_data_frame(out$real$splits, "out$real$splits", allow_empty = TRUE)

  if (nrow(out$real$splits) > 0L) {
    check_required_cols(
      out$real$splits,
      required_cols = c("date", "id", "fund_name", "legacy_ticker", "cvm_code_type",
                        "split_factor", "position_factor"),
      object_name = "out$real$splits"
    )

    check_expected_id(out$real$splits, "id", "out$real$splits", id)
    check_expected_fund_name(out$real$splits, "fund_name", "out$real$splits", fund_name)
    check_finite_cols(
      out$real$splits,
      cols = c("split_factor", "position_factor"),
      object_name = "out$real$splits"
    )
    check_positive_cols(
      out$real$splits,
      cols = c("split_factor", "position_factor"),
      object_name = "out$real$splits"
    )
    check_no_duplicate_keys(out$real$splits, c("date", "id", "fund_name", "cvm_code_type"), "out$real$splits")
  }

  # Cross-table consistency ---------------------------------------------------

  paper_mv_check <- out$paper$portfolio %>%
    dplyr::group_by(date, id) %>%
    dplyr::summarise(
      weight_sum = sum(eop_weights, na.rm = TRUE),
      .groups = "drop"
    )

  if (any(abs(paper_mv_check$weight_sum - 1) > weight_tolerance)) {
    stop("`out$paper$portfolio` failed final cross-table weight validation.", call. = FALSE)
  }

  real_mv_check <- out$real$positions$eop_positions %>%
    dplyr::group_by(date, id, fund_name) %>%
    dplyr::summarise(
      total_position_market_value = sum(market_value, na.rm = TRUE),
      weight_sum = sum(weights, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::left_join(
      out$real$market_value,
      by = c("date", "id", "fund_name")
    ) %>%
    dplyr::mutate(
      market_value_error = abs(total_position_market_value - market_value)
    )

  bad_real_mv <- real_mv_check %>%
    dplyr::filter(market_value_error > pmax(1e-8, abs(market_value) * 1e-8))

  if (nrow(bad_real_mv) > 0L) {
    stop(
      "`out$real$positions$eop_positions$market_value` does not reconcile with `out$real$market_value`. ",
      "First issue: date = ", bad_real_mv$date[1L],
      ", id = ", bad_real_mv$id[1L],
      ", fund_name = ", bad_real_mv$fund_name[1L],
      ", position market value = ", bad_real_mv$total_position_market_value[1L],
      ", reported market value = ", bad_real_mv$market_value[1L],
      ".",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

safe_bind_rows <- function(x) {
  if (length(x) == 0L) {
    return(data.frame())
  }

  dplyr::bind_rows(x)
}

detect_candidate_splits <- function(
    comdinheiro_data,
    current_dates,
    old_port_last_date,
    split_warning_threshold,
    split_rounding_tolerance
) {

  diagnostic_dates <- sort(unique(c(old_port_last_date, current_dates)))

  candidate_tbl <- comdinheiro_data %>%
    dplyr::filter(date %in% diagnostic_dates) %>%
    dplyr::arrange(cvm_code_type, date) %>%
    dplyr::group_by(cvm_code_type) %>%
    dplyr::mutate(
      price_lag = dplyr::lag(price),
      event_factor_lag = dplyr::lag(event_factor),
      n_shares_lag = dplyr::lag(n_shares),

      share_position_factor = n_shares / n_shares_lag,
      event_position_factor = event_factor_lag / event_factor,

      price_implied_position_factor = price_lag * (1 + ret_1d) / (price + proventos),
      price_implied_split_factor = 1 / price_implied_position_factor
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(date %in% current_dates) %>%
    dplyr::mutate(
      share_position_factor_round = round(share_position_factor),
      event_position_factor_round = round(event_position_factor),
      price_position_factor_round = round(price_implied_position_factor),

      share_factor_alert = is.finite(share_position_factor) &
        abs(share_position_factor - 1) > split_warning_threshold,

      event_factor_alert = is.finite(event_position_factor) &
        abs(event_position_factor - 1) > split_warning_threshold,

      price_factor_alert = is.finite(price_implied_position_factor) &
        abs(price_implied_position_factor - 1) > split_warning_threshold,

      price_position_factor_is_round = is.finite(price_implied_position_factor) &
        abs(price_implied_position_factor - price_position_factor_round) <= split_rounding_tolerance,

      supporting_flags = as.integer(share_factor_alert) +
        as.integer(event_factor_alert),

      candidate_confidence = dplyr::case_when(
        price_factor_alert & price_position_factor_is_round & supporting_flags == 2L ~ "high",
        price_factor_alert & price_position_factor_is_round & supporting_flags == 1L ~ "medium",
        price_factor_alert & price_position_factor_is_round & supporting_flags == 0L ~ "price_only",
        price_factor_alert & !price_position_factor_is_round ~ "price_break_non_round",
        !price_factor_alert & supporting_flags > 0L ~ "weak_non_price",
        TRUE ~ "none"
      ),

      warning_level = dplyr::case_when(
        candidate_confidence == "high" ~ "high",
        candidate_confidence %in% c("medium", "price_only") ~ "medium",
        candidate_confidence == "price_break_non_round" ~ "medium_data_quality",
        candidate_confidence == "weak_non_price" ~ "low",
        TRUE ~ "none"
      ),

      inferred_position_factor = dplyr::case_when(
        price_factor_alert & price_position_factor_is_round ~ as.numeric(price_position_factor_round),
        TRUE ~ NA_real_
      ),

      inferred_split_factor = dplyr::if_else(
        is.finite(inferred_position_factor),
        1 / inferred_position_factor,
        NA_real_
      )
    ) %>%
    dplyr::filter(warning_level != "none") %>%
    dplyr::select(
      date,
      legacy_ticker,
      cvm_code_type,
      warning_level,
      candidate_confidence,
      supporting_flags,
      share_factor_alert,
      event_factor_alert,
      price_factor_alert,
      price_lag,
      price,
      ret_1d,
      proventos,
      n_shares_lag,
      n_shares,
      event_factor_lag,
      event_factor,
      share_position_factor,
      event_position_factor,
      price_implied_position_factor,
      price_implied_split_factor,
      inferred_position_factor,
      inferred_split_factor
    )

  candidate_tbl
}

format_split_candidate_warnings <- function(unconfirmed_split_candidates) {

  if (nrow(unconfirmed_split_candidates) == 0L) {
    return(character(0))
  }

  warning_tbl <- unconfirmed_split_candidates %>%
    dplyr::mutate(
      warning_rank = dplyr::case_when(
        warning_level == "high" ~ 1L,
        warning_level == "medium" ~ 2L,
        warning_level == "medium_data_quality" ~ 3L,
        warning_level == "low" ~ 4L,
        TRUE ~ 5L
      ),
      warning_line = paste0(
        "- level = ", warning_level,
        " | confidence = ", candidate_confidence,
        " | date = ", as.character(date),
        " | legacy_ticker = ", legacy_ticker,
        " | cvm_code_type = ", cvm_code_type,
        " | inferred_split_factor = ", round(inferred_split_factor, 6),
        " | price_implied_split_factor = ", round(price_implied_split_factor, 6),
        " | inferred_position_factor = ", round(inferred_position_factor, 6),
        " | price_implied_position_factor = ", round(price_implied_position_factor, 6),
        " | event_position_factor = ", round(event_position_factor, 6),
        " | share_position_factor = ", round(share_position_factor, 6),
        " | supporting_flags = ", supporting_flags,
        " | flags = [price_return: ", price_factor_alert,
        ", event_factor: ", event_factor_alert,
        ", shares: ", share_factor_alert,
        "]"
      )
    ) %>%
    dplyr::arrange(
      warning_rank,
      date,
      legacy_ticker,
      cvm_code_type
    )

  paste(warning_tbl$warning_line, collapse = "\n")
}

compute_paper_portfolio_step <- function(
    current_date,
    id,
    fund_name = NULL,
    paper_last_eop_weights,
    paper_current_market_value,
    asset_ticker_lookup_today,
    prices_today,
    target_today,
    other_events_today,
    transaction_costs_bps,
    daily_fee_return,
    allow_missing_returns = TRUE,
    weight_tolerance = 1e-2
) {

  current_date <- as.Date(current_date)
  id <- as.character(id)
  fund_name_out <- if (is.null(fund_name)) NA_character_ else as.character(fund_name)

  # Validate required columns -------------------------------------------------

  required_last_weight_cols <- c("cvm_code_type", "eop_weights")
  missing_last_weight_cols <- base::setdiff(required_last_weight_cols, names(paper_last_eop_weights))

  if (length(missing_last_weight_cols) > 0L) {
    stop(
      "`paper_last_eop_weights` is missing column(s): ",
      paste(missing_last_weight_cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  required_lookup_cols <- c("cvm_code_type", "legacy_ticker")
  missing_lookup_cols <- base::setdiff(required_lookup_cols, names(asset_ticker_lookup_today))

  if (length(missing_lookup_cols) > 0L) {
    stop(
      "`asset_ticker_lookup_today` is missing column(s): ",
      paste(missing_lookup_cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  required_price_cols <- c("cvm_code_type", "ret_1d")
  missing_price_cols <- base::setdiff(required_price_cols, names(prices_today))

  if (length(missing_price_cols) > 0L) {
    stop(
      "`prices_today` is missing column(s): ",
      paste(missing_price_cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  required_target_cols <- c("cvm_code_type", "weights")
  if (nrow(target_today) > 0L) {
    missing_target_cols <- base::setdiff(required_target_cols, names(target_today))

    if (length(missing_target_cols) > 0L) {
      stop(
        "`target_today` is missing column(s): ",
        paste(missing_target_cols, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
  }

  required_other_events_cols <- c(
    "date", "old_legacy_ticker", "old_cvm_code_type",
    "new_legacy_ticker", "new_cvm_code_type", "adj_factor"
  )
  if (nrow(other_events_today) > 0L) {
    missing_other_events_cols <- base::setdiff(required_other_events_cols, names(other_events_today))

    if (length(missing_other_events_cols) > 0L) {
      stop(
        "`other_events_today` is missing column(s): ",
        paste(missing_other_events_cols, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
  }

  required_cost_cols <- c("date", "id", "transaction_cost_bps")
  missing_cost_cols <- base::setdiff(required_cost_cols, names(transaction_costs_bps))

  if (length(missing_cost_cols) > 0L) {
    stop(
      "`transaction_costs_bps` is missing column(s): ",
      paste(missing_cost_cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  transaction_costs_bps <- transaction_costs_bps %>%
    dplyr::mutate(
      date = as.Date(date),
      id = as.character(id),
      transaction_cost_bps = as.numeric(transaction_cost_bps)
    )

  if (any(is.na(transaction_costs_bps$date))) {
    stop("`transaction_costs_bps$date` contains invalid or missing Date values.", call. = FALSE)
  }

  if (any(is.na(transaction_costs_bps$id))) {
    stop("`transaction_costs_bps$id` contains NA values.", call. = FALSE)
  }

  if (any(is.na(transaction_costs_bps$transaction_cost_bps))) {
    stop("`transaction_costs_bps$transaction_cost_bps` contains NA values.", call. = FALSE)
  }

  if (any(!is.finite(transaction_costs_bps$transaction_cost_bps))) {
    stop("`transaction_costs_bps$transaction_cost_bps` must contain finite values.", call. = FALSE)
  }

  if (any(transaction_costs_bps$transaction_cost_bps < 0, na.rm = TRUE)) {
    stop("`transaction_costs_bps$transaction_cost_bps` cannot contain negative values.", call. = FALSE)
  }

  if (anyDuplicated(transaction_costs_bps[c("date", "id")]) > 0L) {
    stop("`transaction_costs_bps` must contain at most one row per date + id.", call. = FALSE)
  }

  # Normalize inputs ----------------------------------------------------------

  paper_last_eop_weights <- paper_last_eop_weights %>%
    dplyr::mutate(
      cvm_code_type = as.character(cvm_code_type),
      eop_weights = as.numeric(eop_weights)
    )

  asset_ticker_lookup_today <- asset_ticker_lookup_today %>%
    dplyr::mutate(
      cvm_code_type = as.character(cvm_code_type),
      legacy_ticker = as.character(legacy_ticker)
    )

  prices_today <- prices_today %>%
    dplyr::mutate(
      cvm_code_type = as.character(cvm_code_type),
      ret_1d = as.numeric(ret_1d)
    )

  target_today <- target_today %>%
    dplyr::mutate(
      cvm_code_type = as.character(cvm_code_type),
      weights = as.numeric(weights)
    )

  other_events_today <- other_events_today %>%
    dplyr::mutate(
      date = as.Date(date),
      old_legacy_ticker = as.character(old_legacy_ticker),
      old_cvm_code_type = as.character(old_cvm_code_type),
      new_legacy_ticker = as.character(new_legacy_ticker),
      new_cvm_code_type = as.character(new_cvm_code_type),
      adj_factor = as.numeric(adj_factor)
    )


  # Structural validations ----------------------------------------------------

  if (nrow(paper_last_eop_weights) == 0L) {
    stop("`paper_last_eop_weights` cannot be empty.", call. = FALSE)
  }

  if (any(is.na(paper_last_eop_weights$cvm_code_type))) {
    stop("`paper_last_eop_weights$cvm_code_type` contains NA values.", call. = FALSE)
  }

  if (any(is.na(paper_last_eop_weights$eop_weights))) {
    stop("`paper_last_eop_weights$eop_weights` contains NA values.", call. = FALSE)
  }

  if (anyDuplicated(paper_last_eop_weights$cvm_code_type) > 0L) {
    stop("`paper_last_eop_weights` has duplicated `cvm_code_type` rows.", call. = FALSE)
  }

  if (any(paper_last_eop_weights$eop_weights < -weight_tolerance, na.rm = TRUE)) {
    stop("`paper_last_eop_weights$eop_weights` contains negative eop_weights", call. = FALSE)
  }

  paper_last_weight_sum <- sum(paper_last_eop_weights$eop_weights, na.rm = TRUE)

  if (abs(paper_last_weight_sum - 1) > weight_tolerance) {
    stop(
      "`paper_last_eop_weights$eop_weights` must sum to 1. Current sum = ",
      round(paper_last_weight_sum, 8),
      ".",
      call. = FALSE
    )
  }

  if (anyDuplicated(asset_ticker_lookup_today$cvm_code_type) > 0L) {
    stop("`asset_ticker_lookup_today` has duplicated `cvm_code_type` rows.", call. = FALSE)
  }

  if (anyDuplicated(prices_today$cvm_code_type) > 0L) {
    stop("`prices_today` has duplicated `cvm_code_type` rows.", call. = FALSE)
  }

  if (nrow(target_today) > 0L) {
    if (anyDuplicated(target_today$cvm_code_type) > 0L) {
      stop("`target_today` has duplicated `cvm_code_type` rows.", call. = FALSE)
    }

    if (any(is.na(target_today$weights))) {
      stop("`target_today$weights` contains NA values.", call. = FALSE)
    }

    if (any(target_today$weights < -weight_tolerance, na.rm = TRUE)) {
      stop("`target_today$weights` contains negative weights.", call. = FALSE)
    }

    target_weight_sum <- sum(target_today$weights, na.rm = TRUE)

    if (abs(target_weight_sum - 1) > weight_tolerance) {
      stop(
        "Paper EOP weights do not sum to 1 at date ", current_date,
        ". Sum: ", target_weight_sum,
        ".",
        call. = FALSE
      )
    }
  }

  if (nrow(other_events_today) > 0L) {
    if (anyDuplicated(other_events_today$old_cvm_code_type) > 0L) {
      stop("`other_events_today` has duplicated `old_cvm_code_type` rows.", call. = FALSE)
    }
  }

  if (!is.numeric(paper_current_market_value) || length(paper_current_market_value) != 1L ||
      !is.finite(paper_current_market_value) || paper_current_market_value <= 0) {
    stop("`paper_current_market_value` must be a positive finite scalar.", call. = FALSE)
  }

  if (!is.numeric(daily_fee_return) || length(daily_fee_return) != 1L ||
      is.na(daily_fee_return) || !is.finite(daily_fee_return) || daily_fee_return < 0) {
    stop("`daily_fee_return` must be a single non-negative finite numeric value.", call. = FALSE)
  }

  # Build paper asset universe ------------------------------------------------

  paper_assets <- sort(unique(c(
    paper_last_eop_weights$cvm_code_type,
    target_today$cvm_code_type,
    other_events_today$old_cvm_code_type,
    other_events_today$new_cvm_code_type
  )))

  paper_assets <- paper_assets[!is.na(paper_assets)]

  missing_weight_assets <- base::setdiff(paper_assets, paper_last_eop_weights$cvm_code_type)

  if (length(missing_weight_assets) > 0L) {
    paper_last_eop_weights <- paper_last_eop_weights %>%
      dplyr::bind_rows(
        data.frame(
          cvm_code_type = missing_weight_assets,
          eop_weights = 0,
          stringsAsFactors = FALSE
        )
      )
  }

  missing_price_assets <- base::setdiff(paper_assets, prices_today$cvm_code_type)

  if (length(missing_price_assets) > 0L) {
    stop(
      "Missing paper return rows at date ", current_date,
      ". Assets: ",
      paste(missing_price_assets, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  missing_lookup_assets <- base::setdiff(paper_assets, asset_ticker_lookup_today$cvm_code_type)

  if (length(missing_lookup_assets) > 0L) {
    stop(
      "Missing ticker lookup rows at date ", current_date,
      ". Assets: ",
      paste(missing_lookup_assets, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  # Initialize paper portfolio ------------------------------------------------
  paper_portfolio <- data.frame(
    date = as.Date(current_date),
    id = id,
    fund_name = fund_name_out,
    cvm_code_type = paper_last_eop_weights$cvm_code_type,
    bop_weights = paper_last_eop_weights$eop_weights,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::left_join(asset_ticker_lookup_today, by = "cvm_code_type") %>%
    dplyr::select(date, id, fund_name, legacy_ticker, cvm_code_type, bop_weights)

  # Other Events Adjustments----------------------------------------------------
  if (nrow(other_events_today) > 0L) {

    ### Compute what flows INTO each new ticker, per fund
    transfers_w <- paper_portfolio %>%
      dplyr::inner_join(
        other_events_today,
        by = c("date",
               "legacy_ticker" = "old_legacy_ticker",
               "cvm_code_type" = "old_cvm_code_type")
      ) %>%
      dplyr::group_by(date, id, fund_name, new_legacy_ticker, new_cvm_code_type) %>%
      dplyr::summarise(incoming = sum(bop_weights), .groups = "drop")

    ### Flag old rows, join incoming amounts, and adjust
    paper_portfolio <- paper_portfolio %>%
      dplyr::left_join(
        other_events_today %>%
          dplyr::distinct(date, old_legacy_ticker, old_cvm_code_type) %>%
          dplyr::mutate(is_old = TRUE),
        by = c("date",
               "legacy_ticker" = "old_legacy_ticker",
               "cvm_code_type" = "old_cvm_code_type")
      ) %>%
      dplyr::left_join(
        transfers_w,
        by = c("date", "id", "fund_name",
               "legacy_ticker" = "new_legacy_ticker",
               "cvm_code_type" = "new_cvm_code_type")
      ) %>%
      dplyr::mutate(
        bop_weights_before_other_events = bop_weights,
        is_old                          = dplyr::coalesce(is_old, FALSE),
        incoming                        = dplyr::coalesce(incoming, 0),
        bop_weights                     = dplyr::if_else(
          is_old,
          0,
          bop_weights_before_other_events + incoming
        )
      ) %>%
      dplyr::select(-is_old, -incoming) %>%
      dplyr::relocate(bop_weights, .after = dplyr::last_col())

  } else {

    paper_portfolio <- paper_portfolio %>%
      dplyr::mutate(
        bop_weights_before_other_events = bop_weights
      ) %>%
      dplyr::relocate(bop_weights, .after = dplyr::last_col())

  }


  # Add return ----------------------------------------------------------------

  paper_portfolio <- paper_portfolio %>%
    dplyr::left_join(
      prices_today %>%
        dplyr::select(cvm_code_type, ret_1d),
      by = "cvm_code_type"
    )

  paper_bop_weights <- paper_portfolio$bop_weights
  paper_ret_1d <- paper_portfolio$ret_1d

  if (any(is.na(paper_ret_1d))) {
    if (isTRUE(allow_missing_returns)) {
      warning(
        "NA returns found for paper BOP weights at date ", current_date,
        ". These will be treated as zero return. Assets: ",
        paste(paper_portfolio$cvm_code_type[is.na(paper_ret_1d)], collapse = ", "),
        ".",
        call. = FALSE
      )

      paper_ret_1d <- ifelse(is.na(paper_ret_1d), 0, paper_ret_1d)
    } else {
      stop(
        "NA returns found for paper BOP weights at date ", current_date,
        ". Assets: ",
        paste(paper_portfolio$cvm_code_type[is.na(paper_ret_1d)], collapse = ", "),
        ".",
        call. = FALSE
      )
    }
  }

  # Raw return and drift ------------------------------------------------------

  paper_raw_ret <- sum(paper_bop_weights * paper_ret_1d)

  paper_drift_denominator <- sum(paper_bop_weights * (1 + paper_ret_1d))

  if (!is.finite(paper_drift_denominator) || paper_drift_denominator <= 0) {
    stop("Invalid paper drift denominator at date ", current_date, ".", call. = FALSE)
  }

  paper_portfolio <- paper_portfolio %>%
    dplyr::mutate(
      fixed_ret_1d = paper_ret_1d,
      drifted_weights = (bop_weights * (1 + fixed_ret_1d)) / paper_drift_denominator
    ) %>%
    dplyr::select(-fixed_ret_1d)

  if (any(paper_portfolio$drifted_weights < -weight_tolerance, na.rm = TRUE)) {
    stop(
      "Negative paper weights found after drift at date ", current_date,
      ". Assets: ",
      paste(
        paper_portfolio$cvm_code_type[
          paper_portfolio$drifted_weights < -weight_tolerance
        ],
        collapse = ", "
      ),
      ".",
      call. = FALSE
    )
  }

  drifted_weight_sum <- sum(paper_portfolio$drifted_weights, na.rm = TRUE)

  if (drifted_weight_sum > 1 + weight_tolerance || drifted_weight_sum < 1 - weight_tolerance) {
    stop(
      "Paper drifted weights do not sum to 1 at date ", current_date,
      ". Sum: ", drifted_weight_sum,
      ".",
      call. = FALSE
    )
  }

  # EOP rebalance -------------------------------------------------------------

  if (nrow(target_today) > 0L) {
    paper_portfolio <- paper_portfolio %>%
      dplyr::left_join(
        target_today %>%
          dplyr::select(cvm_code_type, target_weights = weights),
        by = "cvm_code_type"
      ) %>%
      dplyr::mutate(
        eop_weights = dplyr::coalesce(target_weights, 0)
      ) %>%
      dplyr::select(-target_weights)

    eop_weight_sum <- sum(paper_portfolio$eop_weights, na.rm = TRUE)

    if (eop_weight_sum > 1 + weight_tolerance || eop_weight_sum < 1 - weight_tolerance) {
      stop(
        "Paper EOP weights do not sum to 1 at date ", current_date,
        ". Sum: ", eop_weight_sum,
        ".",
        call. = FALSE
      )
    }

    paper_turnover <- sum(abs(paper_portfolio$eop_weights - paper_portfolio$drifted_weights), na.rm = TRUE)
  } else {
    paper_turnover <- 0

    paper_portfolio <- paper_portfolio %>%
      dplyr::mutate(
        eop_weights = drifted_weights
      )
  }

  # Costs ---------------------------------------------------------------------

  paper_cost_bps_today <- transaction_costs_bps %>%
    dplyr::filter(
      date == current_date,
      id == !!id
    ) %>%
    dplyr::pull(transaction_cost_bps)

  if (length(paper_cost_bps_today) > 1L) {
    stop(
      "`transaction_costs_bps` must contain at most one row for current date + id. ",
      "date = ", current_date,
      ", id = ", id,
      ".",
      call. = FALSE
    )
  }

  if (length(paper_cost_bps_today) == 0L || is.na(paper_cost_bps_today)) {
    paper_cost_bps_today <- 0
  }

  if (!is.finite(paper_cost_bps_today) || paper_cost_bps_today < 0) {
    stop("Invalid paper transaction cost at date ", current_date, ".", call. = FALSE)
  }

  paper_transaction_cost_return <- paper_cost_bps_today / 10000

  paper_net_ret <- (1 + paper_raw_ret) *
    (1 - paper_transaction_cost_return) *
    (1 - daily_fee_return) - 1

  paper_current_market_value <- paper_current_market_value * (1 + paper_net_ret)

  if (!is.finite(paper_current_market_value) || paper_current_market_value <= 0) {
    stop("Invalid paper current market value after update at date ", current_date, ".", call. = FALSE)
  }

  # Output tables -------------------------------------------------------------

  paper_raw_ret_tbl <- data.frame(
    date = as.Date(current_date),
    id = id,
    fund_name = fund_name_out,
    raw_return = paper_raw_ret,
    stringsAsFactors = FALSE
  )

  paper_net_ret_tbl <- data.frame(
    date = as.Date(current_date),
    id = id,
    fund_name = fund_name_out,
    net_return = paper_net_ret,
    stringsAsFactors = FALSE
  )

  paper_market_value_tbl <- data.frame(
    date = as.Date(current_date),
    id = id,
    fund_name = fund_name_out,
    eop_market_value = paper_current_market_value,
    stringsAsFactors = FALSE
  )

  paper_turnover_tbl <- data.frame(
    date = as.Date(current_date),
    id = id,
    fund_name = fund_name_out,
    turnover = paper_turnover,
    stringsAsFactors = FALSE
  )

  paper_cost_tbl <- data.frame(
    date = as.Date(current_date),
    id = id,
    fund_name = fund_name_out,
    transaction_cost_bps = paper_cost_bps_today,
    transaction_cost_return = paper_transaction_cost_return,
    stringsAsFactors = FALSE
  )

  paper_fee_tbl <- data.frame(
    date = as.Date(current_date),
    id = id,
    fund_name = fund_name_out,
    fund_fees_bps = daily_fee_return * 10000,
    daily_fee_return = daily_fee_return,
    stringsAsFactors = FALSE
  )

  paper_eop_weights_tbl <- data.frame(
    date = as.Date(current_date),
    id = id,
    fund_name = fund_name_out,
    legacy_ticker = as.character(paper_portfolio$legacy_ticker),
    cvm_code_type = as.character(paper_portfolio$cvm_code_type),
    weights = as.numeric(paper_portfolio$eop_weights),
    stringsAsFactors = FALSE
  )

  paper_bop_weights_tbl <- data.frame(
    date = as.Date(current_date),
    id = id,
    fund_name = fund_name_out,
    legacy_ticker = as.character(paper_portfolio$legacy_ticker),
    cvm_code_type = as.character(paper_portfolio$cvm_code_type),
    weights = as.numeric(paper_portfolio$bop_weights),
    stringsAsFactors = FALSE
  )

  paper_last_eop_weights <- paper_portfolio %>%
    dplyr::select(cvm_code_type, eop_weights)

  list(
    paper_portfolio = paper_portfolio,
    paper_raw_ret = paper_raw_ret,
    paper_net_ret = paper_net_ret,
    paper_turnover = paper_turnover,
    paper_cost_bps_today = paper_cost_bps_today,
    paper_transaction_cost_return = paper_transaction_cost_return,
    paper_current_market_value = paper_current_market_value,
    paper_last_eop_weights = paper_last_eop_weights,
    tables = list(
      raw_return = paper_raw_ret_tbl,
      net_return = paper_net_ret_tbl,
      market_value = paper_market_value_tbl,
      turnover = paper_turnover_tbl,
      costs = paper_cost_tbl,
      fees = paper_fee_tbl,
      bop_weights = paper_bop_weights_tbl,
      eop_weights = paper_eop_weights_tbl,
      portfolio = paper_portfolio
    )
  )
}

compute_real_portfolio_step <- function(
    current_date,
    id,
    fund_name,
    real_last_eop_positions,
    asset_ticker_lookup_today,
    prices_yesterday,
    prices_today,
    proventos_today,
    splits_today,
    other_events_today,
    trades_today,
    target_today = NULL,
    fabricate_trades = FALSE,
    default_lot_size = 100,
    etf_lot_size = 1,
    etf_tickers = c("BOVA11", "BOVV11", "SMLL11", "DIVO11", "LFTS11", "ISUS11"),
    daily_fee_return,
    position_tolerance = 1e-8,
    weight_tolerance = 1e-2
) {

  current_date <- as.Date(current_date)
  id <- as.character(id)
  fund_name_out <- if (is.null(fund_name)) NA_character_ else as.character(fund_name)

  # Required columns ----------------------------------------------------------

  required_last_position_cols <- c("cvm_code_type", "eop_positions")
  missing_last_position_cols <- base::setdiff(required_last_position_cols, names(real_last_eop_positions))

  if (length(missing_last_position_cols) > 0L) {
    stop(
      "`real_last_eop_positions` is missing column(s): ",
      paste(missing_last_position_cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  required_lookup_cols <- c("cvm_code_type", "legacy_ticker")
  missing_lookup_cols <- base::setdiff(required_lookup_cols, names(asset_ticker_lookup_today))

  if (length(missing_lookup_cols) > 0L) {
    stop(
      "`asset_ticker_lookup_today` is missing column(s): ",
      paste(missing_lookup_cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  required_yesterday_cols <- c("cvm_code_type", "price_lag")
  missing_yesterday_cols <- base::setdiff(required_yesterday_cols, names(prices_yesterday))

  if (length(missing_yesterday_cols) > 0L) {
    stop(
      "`prices_yesterday` is missing column(s): ",
      paste(missing_yesterday_cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  required_price_cols <- c("cvm_code_type", "ret_1d", "price")
  missing_price_cols <- base::setdiff(required_price_cols, names(prices_today))

  if (length(missing_price_cols) > 0L) {
    stop(
      "`prices_today` is missing column(s): ",
      paste(missing_price_cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  required_proventos_cols <- c("cvm_code_type", "proventos")
  if (nrow(proventos_today) > 0L) {
    missing_proventos_cols <- base::setdiff(required_proventos_cols, names(proventos_today))

    if (length(missing_proventos_cols) > 0L) {
      stop(
        "`proventos_today` is missing column(s): ",
        paste(missing_proventos_cols, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
  }

  required_split_cols <- c("date", "legacy_ticker", "cvm_code_type", "split_factor", "position_factor")
  if (nrow(splits_today) > 0L) {
    missing_split_cols <- base::setdiff(required_split_cols, names(splits_today))

    if (length(missing_split_cols) > 0L) {
      stop(
        "`splits_today` is missing column(s): ",
        paste(missing_split_cols, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
  }

  required_other_events_cols <- c(
    "date", "old_legacy_ticker", "old_cvm_code_type",
    "new_legacy_ticker", "new_cvm_code_type", "adj_factor"
  )
  if (nrow(other_events_today) > 0L) {
    missing_other_events_cols <- base::setdiff(required_other_events_cols, names(other_events_today))

    if (length(missing_other_events_cols) > 0L) {
      stop(
        "`other_events_today` is missing column(s): ",
        paste(missing_other_events_cols, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
  }

  required_trade_cols <- c(
    "cvm_code_type",
    "signed_position",
    "signed_traded_volume",
    "brokerage_fee_estimated",
    "price"
  )

  if (nrow(trades_today) > 0L) {
    missing_trade_cols <- base::setdiff(required_trade_cols, names(trades_today))

    if (length(missing_trade_cols) > 0L) {
      stop(
        "`trades_today` is missing column(s): ",
        paste(missing_trade_cols, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
  }

  required_target_cols <- c("cvm_code_type", "weights")

  if (isTRUE(fabricate_trades) && !is.null(target_today) && nrow(target_today) > 0L) {
    missing_target_cols <- base::setdiff(required_target_cols, names(target_today))

    if (length(missing_target_cols) > 0L) {
      stop(
        "`target_today` is missing column(s): ",
        paste(missing_target_cols, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
  }

  # Normalize inputs ----------------------------------------------------------

  real_last_eop_positions <- real_last_eop_positions %>%
    dplyr::mutate(
      cvm_code_type = as.character(cvm_code_type),
      eop_positions = as.numeric(eop_positions)
    )

  asset_ticker_lookup_today <- asset_ticker_lookup_today %>%
    dplyr::mutate(
      cvm_code_type = as.character(cvm_code_type),
      legacy_ticker = as.character(legacy_ticker)
    )

  prices_yesterday <- prices_yesterday %>%
    dplyr::mutate(
      cvm_code_type = as.character(cvm_code_type),
      price_lag = as.numeric(price_lag)
    )

  prices_today <- prices_today %>%
    dplyr::mutate(
      cvm_code_type = as.character(cvm_code_type),
      ret_1d = as.numeric(ret_1d),
      price = as.numeric(price)
    )

  proventos_today <- proventos_today %>%
    dplyr::mutate(
      cvm_code_type = as.character(cvm_code_type),
      proventos = as.numeric(proventos)
    )

  splits_today <- splits_today %>%
    dplyr::mutate(
      date = as.Date(date),
      legacy_ticker = as.character(legacy_ticker),
      cvm_code_type = as.character(cvm_code_type),
      split_factor = as.numeric(split_factor),
      position_factor = as.numeric(position_factor)
    )

  other_events_today <- other_events_today %>%
    dplyr::mutate(
      date = as.Date(date),
      old_legacy_ticker = as.character(old_legacy_ticker),
      old_cvm_code_type = as.character(old_cvm_code_type),
      new_legacy_ticker = as.character(new_legacy_ticker),
      new_cvm_code_type = as.character(new_cvm_code_type),
      adj_factor = as.numeric(adj_factor)
    )

  trades_today <- trades_today %>%
    dplyr::mutate(
      cvm_code_type = as.character(cvm_code_type),
      signed_position = as.numeric(signed_position),
      signed_traded_volume = as.numeric(signed_traded_volume),
      brokerage_fee_estimated = as.numeric(brokerage_fee_estimated),
      price = as.numeric(price)
    )

  if (is.null(target_today)) {
    target_today <- data.frame(
      cvm_code_type = character(),
      weights = numeric(),
      stringsAsFactors = FALSE
    )
  } else {
    target_today <- target_today %>%
      dplyr::mutate(
        cvm_code_type = as.character(cvm_code_type),
        weights = as.numeric(weights)
      )
  }

  # Structural validations ----------------------------------------------------

  if (nrow(real_last_eop_positions) == 0L) {
    stop("`real_last_eop_positions` cannot be empty.", call. = FALSE)
  }

  if (any(is.na(real_last_eop_positions$cvm_code_type))) {
    stop("`real_last_eop_positions$cvm_code_type` contains NA values.", call. = FALSE)
  }

  if (any(is.na(real_last_eop_positions$eop_positions))) {
    stop("`real_last_eop_positions$eop_positions` contains NA values.", call. = FALSE)
  }

  if (any(real_last_eop_positions$eop_positions < -position_tolerance, na.rm = TRUE)) {
    stop("`real_last_eop_positions$eop_positions` contains negative positions.", call. = FALSE)
  }

  if (anyDuplicated(real_last_eop_positions$cvm_code_type) > 0L) {
    stop("`real_last_eop_positions` has duplicated `cvm_code_type` rows.", call. = FALSE)
  }

  if (anyDuplicated(asset_ticker_lookup_today$cvm_code_type) > 0L) {
    stop("`asset_ticker_lookup_today` has duplicated `cvm_code_type` rows.", call. = FALSE)
  }

  if (anyDuplicated(prices_yesterday$cvm_code_type) > 0L) {
    stop("`prices_yesterday` has duplicated `cvm_code_type` rows.", call. = FALSE)
  }

  if (anyDuplicated(prices_today$cvm_code_type) > 0L) {
    stop("`prices_today` has duplicated `cvm_code_type` rows.", call. = FALSE)
  }

  if (any(is.na(prices_yesterday$price_lag)) || any(!is.finite(prices_yesterday$price_lag)) ||
      any(prices_yesterday$price_lag <= 0, na.rm = TRUE)) {
    stop("`prices_yesterday$price_lag` must be positive, finite, and non-missing.", call. = FALSE)
  }

  if (any(is.na(prices_today$price)) || any(!is.finite(prices_today$price)) ||
      any(prices_today$price <= 0, na.rm = TRUE)) {
    stop("`prices_today$price` must be positive, finite, and non-missing.", call. = FALSE)
  }

  if (nrow(proventos_today) > 0L) {
    if (anyDuplicated(proventos_today$cvm_code_type) > 0L) {
      stop("`proventos_today` has duplicated `cvm_code_type` rows.", call. = FALSE)
    }

    if (any(is.na(proventos_today$proventos)) || any(!is.finite(proventos_today$proventos))) {
      stop("`proventos_today$proventos` must be finite and non-missing.", call. = FALSE)
    }
  }

  if (nrow(splits_today) > 0L) {
    if (anyDuplicated(splits_today$cvm_code_type) > 0L) {
      stop("`splits_today` has duplicated `cvm_code_type` rows.", call. = FALSE)
    }

    if (any(is.na(splits_today$split_factor)) || any(!is.finite(splits_today$split_factor)) ||
        any(splits_today$split_factor <= 0, na.rm = TRUE)) {
      stop("`splits_today$split_factor` must be positive, finite, and non-missing.", call. = FALSE)
    }

    if (any(is.na(splits_today$position_factor)) || any(!is.finite(splits_today$position_factor)) ||
        any(splits_today$position_factor <= 0, na.rm = TRUE)) {
      stop("`splits_today$position_factor` must be positive, finite, and non-missing.", call. = FALSE)
    }

    split_consistency_error <- abs(splits_today$position_factor - 1 / splits_today$split_factor)

    if (any(split_consistency_error > weight_tolerance, na.rm = TRUE)) {
      stop(
        "`splits_today$position_factor` must equal `1 / splits_today$split_factor` within tolerance.",
        call. = FALSE
      )
    }
  }

  if (nrow(other_events_today) > 0L) {
    if (anyDuplicated(other_events_today$old_cvm_code_type) > 0L) {
      stop("`other_events_today` has duplicated `old_cvm_code_type` rows.", call. = FALSE)
    }

    if (any(is.na(other_events_today$adj_factor)) || any(!is.finite(other_events_today$adj_factor)) ||
        any(other_events_today$adj_factor <= 0, na.rm = TRUE)) {
      stop("`other_events_today$adj_factor` must be positive, finite, and non-missing.", call. = FALSE)
    }
  }

  if (nrow(trades_today) > 0L) {
    if (any(is.na(trades_today$cvm_code_type))) {
      stop("`trades_today$cvm_code_type` contains NA values.", call. = FALSE)
    }

    if (any(is.na(trades_today$signed_position)) || any(!is.finite(trades_today$signed_position))) {
      stop("`trades_today$signed_position` must be finite and non-missing.", call. = FALSE)
    }

    if (any(is.na(trades_today$signed_traded_volume)) || any(!is.finite(trades_today$signed_traded_volume))) {
      stop("`trades_today$signed_traded_volume` must be finite and non-missing.", call. = FALSE)
    }

    if (
      any(is.na(trades_today$brokerage_fee_estimated)) ||
      any(!is.finite(trades_today$brokerage_fee_estimated)) ||
      any(trades_today$brokerage_fee_estimated < 0, na.rm = TRUE)
    ) {
      stop("`trades_today$brokerage_fee_estimated` must be non-negative, finite, and non-missing.", call. = FALSE)
    }

    if (any(is.na(trades_today$price)) || any(!is.finite(trades_today$price)) ||
        any(trades_today$price <= 0, na.rm = TRUE)) {
      stop("`trades_today$price` must be positive, finite, and non-missing.", call. = FALSE)
    }
  }

  if (!is.numeric(daily_fee_return) || length(daily_fee_return) != 1L ||
      is.na(daily_fee_return) || !is.finite(daily_fee_return) || daily_fee_return < 0) {
    stop("`daily_fee_return` must be a single non-negative finite numeric value.", call. = FALSE)
  }

  if (isTRUE(fabricate_trades) && nrow(target_today) > 0L) {
    if (any(is.na(target_today$cvm_code_type))) {
      stop("`target_today$cvm_code_type` contains NA values.", call. = FALSE)
    }

    if (any(is.na(target_today$weights)) || any(!is.finite(target_today$weights))) {
      stop("`target_today$weights` must be finite and non-missing.", call. = FALSE)
    }

    if (any(target_today$weights < -weight_tolerance, na.rm = TRUE)) {
      stop("`target_today$weights` cannot contain negative weights.", call. = FALSE)
    }

    if (anyDuplicated(target_today$cvm_code_type) > 0L) {
      stop("`target_today` has duplicated `cvm_code_type` rows.", call. = FALSE)
    }

    target_weight_sum <- sum(target_today$weights, na.rm = TRUE)

    if (abs(target_weight_sum - 1) > weight_tolerance) {
      stop(
        "`target_today$weights` must sum to 1 when synthetic trades are requested. ",
        "Current sum = ", round(target_weight_sum, 8), ".",
        call. = FALSE
      )
    }
  }

  if (
    !is.numeric(default_lot_size) ||
    length(default_lot_size) != 1L ||
    is.na(default_lot_size) ||
    !is.finite(default_lot_size) ||
    default_lot_size <= 0
  ) {
    stop("`default_lot_size` must be a single positive finite numeric value.", call. = FALSE)
  }

  if (
    !is.numeric(etf_lot_size) ||
    length(etf_lot_size) != 1L ||
    is.na(etf_lot_size) ||
    !is.finite(etf_lot_size) ||
    etf_lot_size <= 0
  ) {
    stop("`etf_lot_size` must be a single positive finite numeric value.", call. = FALSE)
  }

  etf_tickers <- as.character(etf_tickers)

  # Build real asset universe -------------------------------------------------

  target_assets <- if (isTRUE(fabricate_trades) && nrow(target_today) > 0L) {
    target_today$cvm_code_type
  } else {
    character()
  }

  real_assets <- sort(unique(c(
    real_last_eop_positions$cvm_code_type,
    splits_today$cvm_code_type,
    trades_today$cvm_code_type,
    other_events_today$old_cvm_code_type,
    other_events_today$new_cvm_code_type,
    target_assets
  )))

  real_assets <- real_assets[!is.na(real_assets)]

  missing_position_assets <- base::setdiff(real_assets, real_last_eop_positions$cvm_code_type)

  if (length(missing_position_assets) > 0L) {
    real_last_eop_positions <- real_last_eop_positions %>%
      dplyr::bind_rows(
        data.frame(
          cvm_code_type = missing_position_assets,
          eop_positions = 0,
          stringsAsFactors = FALSE
        )
      )
  }

  missing_lookup_assets <- base::setdiff(real_assets, asset_ticker_lookup_today$cvm_code_type)

  if (length(missing_lookup_assets) > 0L) {
    stop(
      "Missing ticker lookup rows at date ", current_date,
      ". Assets: ",
      paste(missing_lookup_assets, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  missing_current_price_assets <- base::setdiff(real_assets, prices_today$cvm_code_type)

  if (length(missing_current_price_assets) > 0L) {
    stop(
      "Missing current price rows at date ", current_date,
      ". Assets: ",
      paste(missing_current_price_assets, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  assets_with_position <- real_last_eop_positions$cvm_code_type[
    abs(real_last_eop_positions$eop_positions) > position_tolerance
  ]

  missing_lag_price_assets <- base::setdiff(
    assets_with_position,
    prices_yesterday$cvm_code_type
  )

  if (length(missing_lag_price_assets) > 0L) {
    stop(
      "Missing previous prices for real positions at date ",
      current_date,
      ". Assets: ",
      paste(missing_lag_price_assets, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  # Initialize real portfolio -------------------------------------------------

  real_portfolio <- data.frame(
    date = as.Date(current_date),
    id = id,
    fund_name = fund_name_out,
    cvm_code_type = as.character(real_last_eop_positions$cvm_code_type),
    bop_positions_before_split = as.numeric(real_last_eop_positions$eop_positions),
    stringsAsFactors = FALSE
  ) %>%
    dplyr::left_join(asset_ticker_lookup_today, by = "cvm_code_type")

  # Splits --------------------------------------------------------------------

  if (nrow(splits_today) > 0L) {
    real_portfolio <- real_portfolio %>%
      dplyr::left_join(
        splits_today %>%
          dplyr::select(cvm_code_type, position_factor),
        by = "cvm_code_type"
      ) %>%
      dplyr::mutate(
        position_factor = dplyr::if_else(is.na(position_factor), 1, position_factor),
        bop_positions = bop_positions_before_split * position_factor
      )

    real_split_tbl <- splits_today %>%
      dplyr::mutate(
        id = id,
        fund_name = fund_name_out
      ) %>%
      dplyr::select(
        date,
        id,
        fund_name,
        legacy_ticker,
        cvm_code_type,
        split_factor,
        position_factor
      )
  } else {
    real_portfolio <- real_portfolio %>%
      dplyr::mutate(
        position_factor = 1,
        bop_positions = bop_positions_before_split
      )

    real_split_tbl <- data.frame(
      date = as.Date(character()),
      id = character(),
      fund_name = character(),
      legacy_ticker = character(),
      cvm_code_type = character(),
      split_factor = numeric(),
      position_factor = numeric(),
      stringsAsFactors = FALSE
    )
  }

  # Other Events Adjustments----------------------------------------------------

    ##Eg. 5/7: AXIA6/5 -> AXIA3
    if (nrow(other_events_today) > 0){

      ### Compute what flows INTO each new ticker, per fund
      transfers <- real_portfolio %>%
        dplyr::inner_join(
          other_events_today,
          by = c("date",
                 "legacy_ticker" = "old_legacy_ticker",
                 "cvm_code_type" = "old_cvm_code_type")
        ) %>%
        dplyr::group_by(date, id, fund_name, new_legacy_ticker, new_cvm_code_type) %>%
        dplyr::summarise(incoming = sum(bop_positions * adj_factor), .groups = "drop")

      ### Flag old rows, join incoming amounts, and adjust
      real_portfolio <- real_portfolio %>%
        dplyr::left_join(
          other_events_today %>%
            dplyr::distinct(date, old_legacy_ticker, old_cvm_code_type) %>%
            dplyr::mutate(is_old = TRUE),
          by = c("date",
                 "legacy_ticker" = "old_legacy_ticker",
                 "cvm_code_type" = "old_cvm_code_type")
        ) %>%
        dplyr::left_join(
          transfers,
          by = c("date", "id", "fund_name",
                 "legacy_ticker" = "new_legacy_ticker",
                 "cvm_code_type" = "new_cvm_code_type")
        ) %>%
        dplyr::mutate(
          bop_positions_before_other_events = bop_positions,
          is_old                            = dplyr::coalesce(is_old, FALSE),
          incoming                          = dplyr::coalesce(incoming, 0),
          bop_positions                     = dplyr::case_when(
            is_old ~ 0,
            TRUE   ~ bop_positions_before_other_events + incoming
          )
        ) %>%
        dplyr::select(-is_old, -incoming) %>%
        dplyr::relocate(bop_positions, .after = dplyr::last_col())

      real_other_events_tbl <- other_events_today %>%
        dplyr::mutate(
          id = id,
          fund_name = fund_name_out
        ) %>%
        dplyr::select(
          date,
          id,
          fund_name,
          old_legacy_ticker,
          old_cvm_code_type,
          new_legacy_ticker,
          new_cvm_code_type,
          adj_factor
        )


    } else {

      real_portfolio <- real_portfolio %>%
        dplyr::mutate(
          bop_positions_before_other_events = bop_positions
        ) %>%
        dplyr::relocate(bop_positions, .after = dplyr::last_col())

      real_other_events_tbl <- data.frame(
        date              = as.Date(character()),
        id                = character(),
        fund_name         = character(),
        old_legacy_ticker = character(),
        old_cvm_code_type = character(),
        new_legacy_ticker = character(),
        new_cvm_code_type = character(),
        adj_factor        = numeric(),
        stringsAsFactors  = FALSE
      )


    }



  # Price, dividends, and BOP values -----------------------------------------

  real_portfolio <- real_portfolio %>%
    dplyr::left_join(prices_yesterday, by = "cvm_code_type") %>%
    dplyr::mutate(
      market_value_last_close = price_lag * bop_positions_before_split
    ) %>%
    dplyr::rename(price_last_close = price_lag) %>%
    dplyr::left_join(
      prices_today %>%
        dplyr::select(cvm_code_type, ret_1d, price),
      by = "cvm_code_type"
    ) %>%
    dplyr::left_join(
      proventos_today %>%
        dplyr::select(cvm_code_type, proventos),
      by = "cvm_code_type"
    ) %>%
    dplyr::mutate(
      dividends_per_share = dplyr::coalesce(proventos, 0),
      dividends_received = dividends_per_share * bop_positions
    ) %>%
    dplyr::select(-proventos) %>%
    dplyr::relocate(legacy_ticker, .after = cvm_code_type)

  # Trades ----------------------------------------------------------------------
  # Real portfolios use broker-reported trades. Hypothetical portfolios fabricate
  # trades from target weights, current prices, and lot-size constraints.

  use_synthetic_rebalance <- isTRUE(fabricate_trades) && nrow(target_today) > 0L

  if (isTRUE(use_synthetic_rebalance) && nrow(trades_today) > 0L) {
    stop(
      "Synthetic trade mode received non-empty `trades_today` at date ",
      current_date,
      ". Use either broker trades or synthetic trades, not both.",
      call. = FALSE
    )
  }

  if (isTRUE(use_synthetic_rebalance)) {
    current_aum_before_trades <- sum(
      real_portfolio$bop_positions * real_portfolio$price,
      na.rm = TRUE
    )

    if (!is.finite(current_aum_before_trades) || current_aum_before_trades <= 0) {
      stop(
        "Invalid current AUM before synthetic trades at date ",
        current_date,
        ".",
        call. = FALSE
      )
    }

    net_trades_today <- real_portfolio %>%
      dplyr::select(
        cvm_code_type,
        legacy_ticker,
        bop_positions,
        price
      ) %>%
      dplyr::left_join(
        target_today %>%
          dplyr::select(
            cvm_code_type,
            target_weight = weights
          ),
        by = "cvm_code_type"
      ) %>%
      dplyr::mutate(
        target_weight = dplyr::coalesce(target_weight, 0),

        lot_size = dplyr::if_else(
          legacy_ticker %in% etf_tickers | cvm_code_type %in% etf_tickers,
          etf_lot_size,
          default_lot_size
        ),

        target_market_value = current_aum_before_trades * target_weight,
        target_position_raw = target_market_value / price,
        target_position = base::round(target_position_raw / lot_size) * lot_size,

        net_trade = target_position - bop_positions,
        net_traded_volume = net_trade * price,
        brokerage_fee_estimated = 0,

        avg_trade_price = dplyr::if_else(
          abs(net_trade) > position_tolerance,
          price,
          0
        )
      ) %>%
      dplyr::filter(abs(net_trade) > position_tolerance) %>%
      dplyr::select(
        cvm_code_type,
        net_trade,
        net_traded_volume,
        brokerage_fee_estimated,
        avg_trade_price
      )

    real_portfolio <- real_portfolio %>%
      dplyr::left_join(
        net_trades_today,
        by = "cvm_code_type"
      )
  } else if (nrow(trades_today) > 0L) {
    net_trades_today <- trades_today %>%
      dplyr::group_by(cvm_code_type) %>%
      dplyr::summarise(
        net_trade = sum(signed_position, na.rm = TRUE),
        net_traded_volume = sum(signed_traded_volume, na.rm = TRUE),
        brokerage_fee_estimated = sum(brokerage_fee_estimated, na.rm = TRUE),
        avg_trade_price = stats::weighted.mean(
          price,
          w = pmax(abs(signed_position), 1e-12),
          na.rm = TRUE
        ),
        .groups = "drop"
      )

    real_portfolio <- real_portfolio %>%
      dplyr::left_join(
        net_trades_today,
        by = "cvm_code_type"
      )
  } else {
    net_trades_today <- data.frame(
      cvm_code_type = character(),
      net_trade = numeric(),
      net_traded_volume = numeric(),
      brokerage_fee_estimated = numeric(),
      avg_trade_price = numeric(),
      stringsAsFactors = FALSE
    )

    real_portfolio$net_trade <- 0
    real_portfolio$net_traded_volume <- 0
    real_portfolio$brokerage_fee_estimated <- 0
    real_portfolio$avg_trade_price <- 0
  }

  real_trade_tbl <- net_trades_today %>%
    dplyr::mutate(
      date = as.Date(current_date),
      id = id,
      fund_name = fund_name_out,
      .before = dplyr::everything()
    )

  real_portfolio <- real_portfolio %>%
    dplyr::mutate(
      net_trade = dplyr::coalesce(net_trade, 0),
      net_traded_volume = dplyr::coalesce(net_traded_volume, 0),
      brokerage_fee_estimated = dplyr::coalesce(brokerage_fee_estimated, 0),
      avg_trade_price = dplyr::coalesce(avg_trade_price, 0),
      eop_positions = bop_positions + net_trade,
      eop_market_value = eop_positions * price
    )

  if (any(real_portfolio$eop_positions < -position_tolerance, na.rm = TRUE)) {
    stop(
      "Negative EOP positions found at date ", current_date,
      ". Assets: ",
      paste(
        real_portfolio$cvm_code_type[
          real_portfolio$eop_positions < -position_tolerance
        ],
        collapse = ", "
      ),
      ".",
      call. = FALSE
    )
  }

  # Returns -------------------------------------------------------------------

  real_eop_market_value <- sum(real_portfolio$eop_market_value, na.rm = TRUE)
  real_dividends_received <- sum(real_portfolio$dividends_received, na.rm = TRUE)
  real_net_traded_volume <- sum(real_portfolio$net_traded_volume, na.rm = TRUE)
  real_market_value_last_close <- sum(real_portfolio$market_value_last_close, na.rm = TRUE)

  if (!is.finite(real_market_value_last_close) || real_market_value_last_close <= 0) {
    stop("Invalid real BOP market value at date ", current_date, ".", call. = FALSE)
  }

  if (!is.finite(real_eop_market_value) || real_eop_market_value <= 0) {
    stop("Invalid real EOP market value at date ", current_date, ".", call. = FALSE)
  }

  real_raw_ret <- (
    (real_eop_market_value + real_dividends_received - real_net_traded_volume) /
      real_market_value_last_close
  ) - 1

  real_brokerage_fee_today <- sum(real_portfolio$brokerage_fee_estimated, na.rm = TRUE)

  if (!is.finite(real_brokerage_fee_today) || real_brokerage_fee_today < 0) {
    stop("Invalid real brokerage fee at date ", current_date, ".", call. = FALSE)
  }

  real_brokerage_cost_return <- real_brokerage_fee_today / real_market_value_last_close

  real_net_ret <- (1 + real_raw_ret) *
    (1 - real_brokerage_cost_return) *
    (1 - daily_fee_return) - 1

  # Weights -------------------------------------------------------------------

  real_portfolio <- real_portfolio %>%
    dplyr::mutate(
      bop_weights = dplyr::if_else(
        market_value_last_close > 0,
        market_value_last_close / real_market_value_last_close,
        0
      ),
      eop_weights = dplyr::if_else(
        eop_market_value > 0,
        eop_market_value / real_eop_market_value,
        0
      )
    ) %>%
    dplyr::select(
      date,
      id,
      fund_name,
      legacy_ticker,
      cvm_code_type,
      bop_positions_before_split,
      position_factor,
      bop_positions_before_other_events,
      bop_positions,
      price_last_close,
      market_value_last_close,
      bop_weights,
      eop_positions,
      price,
      ret_1d,
      eop_market_value,
      eop_weights,
      dividends_per_share,
      dividends_received,
      net_trade,
      net_traded_volume,
      brokerage_fee_estimated,
      avg_trade_price
    )

  bop_weight_sum <- sum(real_portfolio$bop_weights, na.rm = TRUE)
  eop_weight_sum <- sum(real_portfolio$eop_weights, na.rm = TRUE)

  if (abs(bop_weight_sum - 1) > weight_tolerance) {
    stop(
      "Real BOP weights do not sum to 1 at date ", current_date,
      ". Sum: ", bop_weight_sum,
      ".",
      call. = FALSE
    )
  }

  if (abs(eop_weight_sum - 1) > weight_tolerance) {
    stop(
      "Real EOP weights do not sum to 1 at date ", current_date,
      ". Sum: ", eop_weight_sum,
      ".",
      call. = FALSE
    )
  }

  # Turnover ------------------------------------------------------------------

  gross_traded_volume <- sum(abs(real_portfolio$net_traded_volume), na.rm = TRUE)
  real_turnover <- gross_traded_volume / real_market_value_last_close

  # Output tables -------------------------------------------------------------

  real_raw_ret_tbl <- data.frame(
    date = as.Date(current_date),
    id = id,
    fund_name = fund_name_out,
    raw_return = real_raw_ret,
    market_value_last_close = real_market_value_last_close,
    eop_market_value = real_eop_market_value,
    dividends_received = real_dividends_received,
    net_traded_volume = real_net_traded_volume,
    stringsAsFactors = FALSE
  )

  real_net_ret_tbl <- data.frame(
    date = as.Date(current_date),
    id = id,
    fund_name = fund_name_out,
    net_return = real_net_ret,
    brokerage_cost_return = real_brokerage_cost_return,
    daily_fee_return = daily_fee_return,
    stringsAsFactors = FALSE
  )

  real_market_value_tbl <- data.frame(
    date = as.Date(current_date),
    id = id,
    fund_name = fund_name_out,
    market_value = real_eop_market_value,
    stringsAsFactors = FALSE
  )

  real_turnover_tbl <- data.frame(
    date = as.Date(current_date),
    id = id,
    fund_name = fund_name_out,
    turnover = real_turnover,
    stringsAsFactors = FALSE
  )

  real_cost_tbl <- data.frame(
    date = as.Date(current_date),
    id = id,
    fund_name = fund_name_out,
    brokerage_fee = real_brokerage_fee_today,
    brokerage_cost_return = real_brokerage_cost_return,
    stringsAsFactors = FALSE
  )

  real_fee_tbl <- data.frame(
    date = as.Date(current_date),
    id = id,
    fund_name = fund_name_out,
    fund_fees_bps = daily_fee_return * 10000,
    daily_fee_return = daily_fee_return,
    stringsAsFactors = FALSE
  )

  real_bop_positions_tbl <- data.frame(
    date = as.Date(current_date),
    id = id,
    fund_name = fund_name_out,
    legacy_ticker = real_portfolio$legacy_ticker,
    cvm_code_type = real_portfolio$cvm_code_type,
    positions = as.numeric(real_portfolio$bop_positions),
    price_last_close = as.numeric(real_portfolio$price_last_close),
    market_value = as.numeric(real_portfolio$market_value_last_close),
    weights = as.numeric(real_portfolio$bop_weights),
    stringsAsFactors = FALSE
  )

  real_eop_positions_tbl <- data.frame(
    date = as.Date(current_date),
    id = id,
    fund_name = fund_name_out,
    legacy_ticker = real_portfolio$legacy_ticker,
    cvm_code_type = real_portfolio$cvm_code_type,
    positions = as.numeric(real_portfolio$eop_positions),
    price = as.numeric(real_portfolio$price),
    market_value = as.numeric(real_portfolio$eop_market_value),
    weights = as.numeric(real_portfolio$eop_weights),
    stringsAsFactors = FALSE
  )

  real_last_eop_positions <- real_portfolio %>%
    dplyr::select(cvm_code_type, eop_positions)

  list(
    real_portfolio = real_portfolio,
    real_raw_ret = real_raw_ret,
    real_net_ret = real_net_ret,
    real_turnover = real_turnover,
    real_brokerage_fee_today = real_brokerage_fee_today,
    real_brokerage_cost_return = real_brokerage_cost_return,
    real_market_value_last_close = real_market_value_last_close,
    real_eop_market_value = real_eop_market_value,
    real_dividends_received = real_dividends_received,
    real_net_traded_volume = real_net_traded_volume,
    real_last_eop_positions = real_last_eop_positions,
    tables = list(
      raw_return = real_raw_ret_tbl,
      net_return = real_net_ret_tbl,
      market_value = real_market_value_tbl,
      turnover = real_turnover_tbl,
      costs = real_cost_tbl,
      fees = real_fee_tbl,
      bop_positions = real_bop_positions_tbl,
      eop_positions = real_eop_positions_tbl,
      trades = real_trade_tbl,
      splits = real_split_tbl,
      other_events = real_other_events_tbl,
      portfolio = real_portfolio
    )
  )
}

