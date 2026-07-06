deploy_dados_gold <- function(dados_silver_refreshed, current_dates,
                              initial_rebalancing_date, manifest = NULL, commit = NULL, board) {

  ## Extract--------------------------------------------------------------------
  rebalanceamento_tables <- dados_silver_refreshed$rebalanceamento_tables
  comdinheiro_data       <- dados_silver_refreshed$comdinheiro_data
  brokerage_data         <- dados_silver_refreshed$brokerage_data
  split_inplit_data      <- dados_silver_refreshed$split_inplit_data
  port_iniciais          <- dados_silver_refreshed$port_iniciais

  rebal_weights <- rebalanceamento_tables$rebal_weights
  catalog       <- rebalanceamento_tables$catalog

  old_dados_gold <- read_pin_from_manifest(
    board       = board,
    manifest    = manifest,
    object_name = "dados_gold_refreshed"
  )

  ##Derive old port from gold
  old_portfolio <- derive_old_portfolio_and_validate_ids(
    old_dados_gold           = old_dados_gold,
    rebal_weights            = rebal_weights,
    comdinheiro_data         = comdinheiro_data,
    port_iniciais            = port_iniciais,
    current_dates            = current_dates,
    initial_rebalancing_date = initial_rebalancing_date,
    starting_aum             = 10000000
  )


  ##Ignore initial_rebalancing_date
  evolution_dates <- setdiff(
    as.Date(current_dates),
    as.Date(initial_rebalancing_date)
  ) %>% as.Date()

  if (length(evolution_dates) == 0L) {
    stop(
      "`current_dates` must contain at least one date after `initial_rebalancing_date`.",
      call. = FALSE
    )
  }

  evolution_dates <- sort(evolution_dates)

  ##Add rebal_weights for YMF ids, by using simulated quantitites instead of
  ##weights, using carteira_bbg_ as the reference
  rebal_weights <- create_ymf_rebal_weights(
    rebal_weights = rebal_weights,
    old_real_portfolio = old_portfolio$real$portfolio,
    comdinheiro_data = comdinheiro_data,
    default_lot_size = 100,
    etf_lot_size = 1,
    etf_tickers = c("BOVA11", "BOVV11", "SMLL11", "DIVO11", "LFTS11", "ISUS11"),
    id_map = NULL
  )

  newest_rebal_portfolio_ids <- rebal_weights %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::pull(id) %>%
    unique()

  ## Evolve weighs and returns--------------------------------------------------
  evolved_portfolios <- purrr::map(newest_rebal_portfolio_ids,
                                   function(portfolio_id){

    ### Get associated fund_name if portfolio id contains YMF_
    if (stringr::str_detect(portfolio_id, "YMF_")){
      fund_name <- old_portfolio$real$portfolio %>%
        dplyr::filter(id == portfolio_id) %>%
        dplyr::pull(fund_name) %>%
        unique()
    } else {
      fund_name <- NULL
    }

    ### Get fund_fees_bps for funds
    if (!is.null(fund_name)){
      fund_fees_bps <- dplyr::case_when(
        fund_name == "sicoob_acoes" ~ 0.004929693, #1.25 pa.
        fund_name == "sicoob_dividendos" ~ 0.003948622, #1.00 pa.
        fund_name == "sicoob_small_caps" ~  0.004929693, #1.25 pa.
        fund_name == "sicoob_asg" ~ 0.003162022, #0.80 pa.
        TRUE ~ 0
      )
    } else {
      fund_fees_bps <- 0
    }

    ### Current old portfolio
    current_old_portfolio <- list(
      paper = list(portfolio = old_portfolio$paper$portfolio %>% dplyr::filter(id == portfolio_id)),
      real  = list(portfolio = old_portfolio$real$portfolio %>% dplyr::filter(id == portfolio_id))
    )

    message("Processing portfolio id: ", portfolio_id, " with fund name: ", fund_name)
    evolve_portfolio(
      current_dates         = evolution_dates,
      old_portfolio         = current_old_portfolio,
      rebal_weights         = rebal_weights,
      comdinheiro_data      = comdinheiro_data,
      brokerage_data        = brokerage_data,
      id                    = portfolio_id,
      fund_name             = fund_name,
      fund_fees_bps         = fund_fees_bps,
      split_inplit_data     = split_inplit_data,
      allow_missing_returns = TRUE
    )

  })

  names(evolved_portfolios) <- newest_rebal_portfolio_ids

  ## Test-----------------------------------------------------------------------

  message("Testing Gold Data...")
  test_sucess <- run_logged_test(
    fun = function() {
      run_test_dados_gold(
        evolved_portfolios         = evolved_portfolios,
        newest_rebal_portfolio_ids = newest_rebal_portfolio_ids,
        rebal_weights              = rebal_weights,
        comdinheiro_data           = comdinheiro_data
      )
    },
    log_file = paste0("test_dados_gold_", min(current_dates), "_", max(current_dates)),
    log_path = file.path(here::here(), "logs", "tests")
  )

  check_test_success(test_sucess)

  ## Append---------------------------------------------------------------------
  message("Appending Gold Data...")
  dados_gold_refreshed <- bind_old_dados_gold(
    old_dados_gold           = old_dados_gold,
    evolved_portfolios       = evolved_portfolios,
    current_dates            = current_dates,
    initial_rebalancing_date = initial_rebalancing_date,
    verbose = TRUE
  )

  ## Save-----------------------------------------------------------------------
  message("Deploying Gold Data...")
  save_local_pin(
    board       = board,
    data        = dados_gold_refreshed,
    object_name = "dados_gold",
    stage       = "gold",
    commit      = commit,
    source      = "rebalanceamento_comdinheiro",
    table_type  = "dados_gold"
  )


  return(dados_gold_refreshed)



}

derive_old_portfolio_and_validate_ids <- function(
    old_dados_gold = NULL,
    rebal_weights,
    comdinheiro_data,
    port_iniciais = NULL,
    current_dates,
    initial_rebalancing_date,
    starting_aum = 10000000
) {

  ## Helper----------------------------------------------------------------------

  filter_old_portfolio_before_current_dates <- function(old_portfolio, current_dates) {

    old_portfolio$paper$portfolio <- filter_old_table_before_current_dates(
      df = old_portfolio$paper$portfolio,
      table_name = "old_portfolio$paper$portfolio",
      current_dates = current_dates
    )

    old_portfolio$real$portfolio <- filter_old_table_before_current_dates(
      df = old_portfolio$real$portfolio,
      table_name = "old_portfolio$real$portfolio",
      current_dates = current_dates
    )

    if (!is.null(old_portfolio$paper$weights$bop_weights)) {
      old_portfolio$paper$weights$bop_weights <- filter_old_table_before_current_dates(
        df = old_portfolio$paper$weights$bop_weights,
        table_name = "old_portfolio$paper$weights$bop_weights",
        current_dates = current_dates
      )
    }

    if (!is.null(old_portfolio$paper$weights$eop_weights)) {
      old_portfolio$paper$weights$eop_weights <- filter_old_table_before_current_dates(
        df = old_portfolio$paper$weights$eop_weights,
        table_name = "old_portfolio$paper$weights$eop_weights",
        current_dates = current_dates
      )
    }

    if (!is.null(old_portfolio$paper$returns)) {
      old_portfolio$paper$returns <- filter_old_table_before_current_dates(
        df = old_portfolio$paper$returns,
        table_name = "old_portfolio$paper$returns",
        current_dates = current_dates
      )
    }

    if (!is.null(old_portfolio$paper$market_value)) {
      old_portfolio$paper$market_value <- filter_old_table_before_current_dates(
        df = old_portfolio$paper$market_value,
        table_name = "old_portfolio$paper$market_value",
        current_dates = current_dates
      )
    }

    if (!is.null(old_portfolio$paper$turnover)) {
      old_portfolio$paper$turnover <- filter_old_table_before_current_dates(
        df = old_portfolio$paper$turnover,
        table_name = "old_portfolio$paper$turnover",
        current_dates = current_dates
      )
    }

    if (!is.null(old_portfolio$paper$costs)) {
      old_portfolio$paper$costs <- filter_old_table_before_current_dates(
        df = old_portfolio$paper$costs,
        table_name = "old_portfolio$paper$costs",
        current_dates = current_dates
      )
    }

    if (!is.null(old_portfolio$paper$fees)) {
      old_portfolio$paper$fees <- filter_old_table_before_current_dates(
        df = old_portfolio$paper$fees,
        table_name = "old_portfolio$paper$fees",
        current_dates = current_dates
      )
    }

    if (!is.null(old_portfolio$real$positions$bop_positions)) {
      old_portfolio$real$positions$bop_positions <- filter_old_table_before_current_dates(
        df = old_portfolio$real$positions$bop_positions,
        table_name = "old_portfolio$real$positions$bop_positions",
        current_dates = current_dates
      )
    }

    if (!is.null(old_portfolio$real$positions$eop_positions)) {
      old_portfolio$real$positions$eop_positions <- filter_old_table_before_current_dates(
        df = old_portfolio$real$positions$eop_positions,
        table_name = "old_portfolio$real$positions$eop_positions",
        current_dates = current_dates
      )
    }

    if (!is.null(old_portfolio$real$returns)) {
      old_portfolio$real$returns <- filter_old_table_before_current_dates(
        df = old_portfolio$real$returns,
        table_name = "old_portfolio$real$returns",
        current_dates = current_dates
      )
    }

    if (!is.null(old_portfolio$real$market_value)) {
      old_portfolio$real$market_value <- filter_old_table_before_current_dates(
        df = old_portfolio$real$market_value,
        table_name = "old_portfolio$real$market_value",
        current_dates = current_dates
      )
    }

    if (!is.null(old_portfolio$real$trades)) {
      old_portfolio$real$trades <- filter_old_table_before_current_dates(
        df = old_portfolio$real$trades,
        table_name = "old_portfolio$real$trades",
        current_dates = current_dates
      )
    }

    if (!is.null(old_portfolio$real$splits)) {
      old_portfolio$real$splits <- filter_old_table_before_current_dates(
        df = old_portfolio$real$splits,
        table_name = "old_portfolio$real$splits",
        current_dates = current_dates
      )
    }

    if (!is.null(old_portfolio$real$turnover)) {
      old_portfolio$real$turnover <- filter_old_table_before_current_dates(
        df = old_portfolio$real$turnover,
        table_name = "old_portfolio$real$turnover",
        current_dates = current_dates
      )
    }

    if (!is.null(old_portfolio$real$costs)) {
      old_portfolio$real$costs <- filter_old_table_before_current_dates(
        df = old_portfolio$real$costs,
        table_name = "old_portfolio$real$costs",
        current_dates = current_dates
      )
    }

    if (!is.null(old_portfolio$real$fees)) {
      old_portfolio$real$fees <- filter_old_table_before_current_dates(
        df = old_portfolio$real$fees,
        table_name = "old_portfolio$real$fees",
        current_dates = current_dates
      )
    }

    old_portfolio
  }

  filter_old_table_before_current_dates <- function(df, table_name, current_dates) {
    if (is.null(df)) {
      return(df)
    }

    if (!is.data.frame(df)) {
      stop("`", table_name, "` must be a data.frame.", call. = FALSE)
    }

    if (!"date" %in% names(df)) {
      stop("`", table_name, "` must contain column `date`.", call. = FALSE)
    }

    df <- df %>%
      dplyr::mutate(date = as.Date(date))

    min_current_date <- min(as.Date(current_dates))

    overlapping_rows <- df %>%
      dplyr::filter(date >= min_current_date)

    if (nrow(overlapping_rows) > 0L) {
      message(
        "Discarding ",
        nrow(overlapping_rows),
        " row(s) from `",
        table_name,
        "` because their date is >= first current date (",
        min_current_date,
        ")."
      )
    }

    df %>%
      dplyr::filter(date < min_current_date)
  }

  ## Derive old port from gold -------------------------------------------------
  if (!is.null(old_dados_gold)) {

    old_portfolio <- filter_old_portfolio_before_current_dates(
      old_portfolio = list(
        paper = list(portfolio = old_dados_gold$paper$portfolio),
        real  = list(portfolio = old_dados_gold$real$portfolio)
      ),
      current_dates = current_dates
    )

    ##Check for at least one row
    if (nrow(old_portfolio$paper$portfolio) == 0L) {
      stop(
        "`old_dados_gold$portfolio$paper$portfolio` has no rows before the first current date.",
        call. = FALSE
      )
    }
    if (nrow(old_portfolio$real$portfolio) == 0L) {
      stop(
        "`old_dados_gold$portfolio$real$portfolio` has no rows before the first current date.",
        call. = FALSE
      )
    }


  } else {

    if (current_dates[1] != initial_rebalancing_date) {
      stop(
        "When `old_dados_gold` is NULL, the first date in `current_dates` must be equal to `initial_rebalancing_date`.",
        call. = FALSE
      )
    }

    old_portfolio <- rebal_weights %>%
      dplyr::filter(date == min(date)) %>%
      dplyr::left_join(
        comdinheiro_data %>%
          dplyr::filter(date == min(date)) %>%
          dplyr::select(cvm_code_type, date, price),
        by = c("cvm_code_type", "date")
      )

    old_paper_portfolio <- old_portfolio %>%
      dplyr::select(date, id, legacy_ticker, cvm_code_type, eop_weights = weights)

    if (!is.null(port_iniciais)) {
      old_paper_portfolio <- old_paper_portfolio %>%
        dplyr::bind_rows(
          port_iniciais %>%
            dplyr::select(date, id, fund_name, legacy_ticker, cvm_code_type, positions, price) %>%
            dplyr::group_by(id) %>%
            dplyr::mutate(
              eop_weights  = (positions * price) / sum(positions * price)
            ) %>%
            dplyr::ungroup() %>%
            dplyr::select(date, id, fund_name, legacy_ticker, cvm_code_type, eop_weights)
        )
    }

    old_real_portfolio <- old_portfolio %>%
      dplyr::select(date, id, legacy_ticker, cvm_code_type, weights, price) %>%
      dplyr::mutate(
        fund_name = NA_character_
      ) %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(
        eop_positions = round(((starting_aum * weights) / price) / 100, 0) * 100,
        aum = eop_positions * price,
        weights = aum / sum(aum)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(date, id, fund_name, legacy_ticker, cvm_code_type, eop_positions, price)

    if (!is.null(port_iniciais)) {
      old_real_portfolio <- old_real_portfolio %>%
        dplyr::bind_rows(
          port_iniciais %>%
            dplyr::select(date, id, fund_name, legacy_ticker, cvm_code_type, eop_positions = positions, price)
        )
    }

    old_portfolio <- list(
      paper = list(portfolio = old_paper_portfolio),
      real = list(portfolio = old_real_portfolio)
    )
  }

  ## Define portfolio ids ------------------------------------------------------

  deployed_portfolio_ids <- c(
    old_portfolio$paper$portfolio$id,
    old_portfolio$real$portfolio$id
  ) %>%
    unique()

  newest_rebal_portfolio_ids <- rebal_weights %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::pull(id) %>%
    unique()

  if (!is.null(port_iniciais)) {
    newest_rebal_portfolio_ids <- c(
      newest_rebal_portfolio_ids,
      unique(port_iniciais$id)
    ) %>%
      unique()
  }

  if (!dplyr::setequal(deployed_portfolio_ids[!stringr::str_detect(deployed_portfolio_ids, "YMF_")],
                       newest_rebal_portfolio_ids)) {
    message(
      "New (hypothetical) portfolio ids differ from deployed portfolio ids. ",
      "Please check the changes in the portfolio ids and their impact on the deployed portfolio:"
    )

    message("Changes: ")

    new_ids <- dplyr::setdiff(newest_rebal_portfolio_ids, deployed_portfolio_ids)
    removed_ids <- dplyr::setdiff(deployed_portfolio_ids, newest_rebal_portfolio_ids)

    if (length(new_ids) > 0) {
      message("New ids: ", paste(new_ids, collapse = ", "))
    }

    if (length(removed_ids) > 0) {
      message("Removed ids: ", paste(removed_ids, collapse = ", "))
    }
  } else {
    message("No changes in portfolio ids. Proceeding with deployment.")
  }

  deployed_portfolio_ids_after_max_rebal_date <- c(
    old_portfolio$paper$portfolio %>%
      dplyr::filter(date >= max(rebal_weights$date)) %>%
      dplyr::pull(id) %>%
      unique(),
    old_portfolio$real$portfolio %>%
      dplyr::filter(date >= max(rebal_weights$date)) %>%
      dplyr::pull(id) %>%
      unique()
  ) %>%
    unique()

  if (length(deployed_portfolio_ids_after_max_rebal_date) > 0 &&
      !dplyr::setequal(
        deployed_portfolio_ids_after_max_rebal_date,
        newest_rebal_portfolio_ids)
      ){
    stop(
      "Deployed portfolio ids for dates >= the newest rebalancing date differ ",
      "from the newest rebalancing portfolio ids. ",
      call. = FALSE
    )
  }

  ##Return----------------------------------------------------------------------

  old_portfolio
}


create_ymf_rebal_weights <- function(
    rebal_weights,
    old_real_portfolio,
    default_lot_size = 100,
    comdinheiro_data,
    etf_lot_size = 1,
    etf_tickers = c("BOVA11", "BOVV11", "SMLL11", "DIVO11", "LFTS11", "ISUS11"),
    id_map = NULL
) {
  if (is.null(id_map)) {
    id_map <- data.frame(
      ymf_id = c(
        "YMF_29",
        "YMF_33",
        "YMF_34",
        "YMF_35",
        "YMF_36",
        "YMF_37",
        "YMF_500",
        "YMF_501"
      ),
      source_id = c(
        "carteira_bbg_FIA",
        "carteira_bbg_VGBLs",
        "carteira_bbg_VGBLs",
        "carteira_bbg_SMLL",
        "carteira_bbg_IDIV",
        "carteira_bbg_ASG",
        "carteira_bbg_PREVI",
        "carteira_bbg_PREVI"
      ),
      stringsAsFactors = FALSE
    )
  }

  ##Validate--------------------------------------------------------------------
  required_rebal_cols <- c(
    "date",
    "id",
    "legacy_ticker",
    "cvm_code_type",
    "weights"
  )

  missing_rebal_cols <- setdiff(required_rebal_cols, names(rebal_weights))

  if (length(missing_rebal_cols) > 0L) {
    stop(
      "`rebal_weights` is missing required columns: ",
      paste(missing_rebal_cols, collapse = ", "),
      call. = FALSE
    )
  }

  required_old_real_cols <- c(
    "date",
    "id",
    "cvm_code_type",
    "eop_positions",
    "price"
  )

  missing_old_real_cols <- setdiff(required_old_real_cols, names(old_real_portfolio))

  if (length(missing_old_real_cols) > 0L) {
    stop(
      "`old_real_portfolio` is missing required columns: ",
      paste(missing_old_real_cols, collapse = ", "),
      call. = FALSE
    )
  }

  required_map_cols <- c("ymf_id", "source_id")
  missing_map_cols <- setdiff(required_map_cols, names(id_map))

  if (length(missing_map_cols) > 0L) {
    stop(
      "`id_map` is missing required columns: ",
      paste(missing_map_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (anyDuplicated(id_map$ymf_id) > 0L) {
    stop("`id_map$ymf_id` must be unique.", call. = FALSE)
  }

  if (
    !is.numeric(default_lot_size) ||
    length(default_lot_size) != 1L ||
    is.na(default_lot_size) ||
    default_lot_size <= 0
  ) {
    stop("`default_lot_size` must be a single positive numeric value.", call. = FALSE)
  }

  if (
    !is.numeric(etf_lot_size) ||
    length(etf_lot_size) != 1L ||
    is.na(etf_lot_size) ||
    etf_lot_size <= 0
  ) {
    stop("`etf_lot_size` must be a single positive numeric value.", call. = FALSE)
  }

  if (!is.character(etf_tickers) || length(etf_tickers) == 0L || any(is.na(etf_tickers))) {
    stop("`etf_tickers` must be a non-empty character vector without NA values.", call. = FALSE)
  }

  missing_source_ids <- setdiff(id_map$source_id, unique(rebal_weights$id))

  if (length(missing_source_ids) > 0L) {
    stop(
      "Some `source_id` values are not present in `rebal_weights$id`: ",
      paste(missing_source_ids, collapse = ", "),
      call. = FALSE
    )
  }

  ##Compute--------------------------------------------------------------------
  ## Get AuM of old portfolio in order to simulate quantities for YMF portfolios
  ## based on their weights in the rebalancing and the AuM of the old portfolio
  ymf_aum <- old_real_portfolio %>%
    dplyr::filter(id %in% id_map$ymf_id,
                  date == max(date)) %>%
    dplyr::mutate(
      aum = eop_positions * price
    ) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      fund_aum = sum(aum, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::rename(
      ymf_id = id
    )

  missing_ymf_aum <- setdiff(id_map$ymf_id, ymf_aum$ymf_id)

  if (length(missing_ymf_aum) > 0L) {
    stop(
      "Some `ymf_id` values are not present in `old_real_portfolio$id`, so AUM cannot be computed: ",
      paste(missing_ymf_aum, collapse = ", "),
      call. = FALSE
    )
  }

  zero_or_invalid_aum <- ymf_aum %>%
    dplyr::filter(!is.finite(fund_aum) | fund_aum <= 0)

  if (nrow(zero_or_invalid_aum) > 0L) {
    stop(
      "Some YMF portfolios have invalid AUM: ",
      paste(zero_or_invalid_aum$ymf_id, collapse = ", "),
      call. = FALSE
    )
  }

  price_tbl <- comdinheiro_data %>%
    dplyr::select(date, cvm_code_type, price) %>%
    dplyr::distinct()

  duplicated_prices <- price_tbl %>%
    dplyr::count(date, cvm_code_type, name = "n") %>%
    dplyr::filter(n > 1L)

  if (nrow(duplicated_prices) > 0L) {
    stop(
      "`old_real_portfolio` has duplicated prices by `date` and `cvm_code_type`.",
      call. = FALSE
    )
  }

  new_ymf_rebal_weights <- id_map %>%
    dplyr::left_join(
      rebal_weights,
      by = c("source_id" = "id"),
      relationship = "many-to-many"
    ) %>%
    dplyr::mutate(
      id = ymf_id
    ) %>%
    dplyr::select(
      date,
      id,
      legacy_ticker,
      cvm_code_type,
      weights
    ) %>%
    dplyr::left_join(
      ymf_aum,
      by = c("id" = "ymf_id")
    ) %>%
    dplyr::left_join(
      price_tbl,
      by = c("date", "cvm_code_type")
    )

  missing_prices <- new_ymf_rebal_weights %>%
    dplyr::filter(is.na(price)) %>%
    dplyr::distinct(date, id, cvm_code_type)

  if (nrow(missing_prices) > 0L) {
    stop(
      "Some assets have missing prices after joining `old_real_portfolio` prices. ",
      "Inspect `date`, `id`, and `cvm_code_type` combinations.",
      call. = FALSE
    )
  }

  new_ymf_rebal_weights <- new_ymf_rebal_weights %>%
    dplyr::mutate(
      asset_lot_size = dplyr::if_else(
        legacy_ticker %in% etf_tickers,
        etf_lot_size,
        default_lot_size
      )
    ) %>%
    dplyr::group_by(date, id) %>%
    dplyr::mutate(
      positions = round(((fund_aum * weights) / price) / asset_lot_size, 0) * asset_lot_size,
      aum = positions * price,
      weights = aum / sum(aum, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      date,
      id,
      legacy_ticker,
      cvm_code_type,
      weights
    )

  rebal_weights_with_ymf <- rebal_weights %>%
    dplyr::filter(!id %in% id_map$ymf_id) %>%
    dplyr::bind_rows(new_ymf_rebal_weights) %>%
    dplyr::arrange(date, id, cvm_code_type)

  return(rebal_weights_with_ymf)
}




