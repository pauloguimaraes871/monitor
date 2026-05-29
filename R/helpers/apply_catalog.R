apply_catalog <- function(rebalanceamento_tables, comdinheiro_data, brokerage_data,
                          catalog_update, fund_tickers, broker_accounts,
                          verbose = TRUE){

  rebal_weights    <- rebalanceamento_tables$rebal_weights
  sectors          <- rebalanceamento_tables$sectors
  catalog          <- rebalanceamento_tables$catalog

  ## Update catalog-------------------------------------------------------------
  catalog <- catalog %>%
    dplyr::filter(date == max(date))

  ### Add new legacy_ticker to catalog
  if (!is.null(catalog_update)){

    #### Check structure: date, legacy_ticker, cvm_code_type columns
    if (!all(c("date", "legacy_ticker", "cvm_code_type") %in% colnames(catalog_update))) {
      stop("catalog_update must contain the following columns: date, legacy_ticker, cvm_code_type")
    }

    #### Check types: Date, character and character
    if (!inherits(catalog_update$date, "Date") ||
        !is.character(catalog_update$legacy_ticker) ||
        !is.character(catalog_update$cvm_code_type)) {
      stop("catalog_update columns must be of types: date (Date), legacy_ticker (character), cvm_code_type (character)")
    }

    #### CVM code type beginning with 'h'
    if (any(!grepl("^h", catalog_update$cvm_code_type))) {
      stop("cvm_code_type values must begin with 'h'")
    }

    #### catalog_update can't have duplicated legacy_ticker
    if (any(duplicated(catalog_update$legacy_ticker))) {
      stop("catalog_update cannot contain duplicated legacy_ticker")
    }

    #### Filter out dates different from date in catalog and tickers already in catalog
    existing_update_tickers <- catalog_update %>%
      dplyr::filter(legacy_ticker %in% catalog$tickers) %>%
      dplyr::pull(legacy_ticker)

    if (length(existing_update_tickers) > 0) {
      message(
        "The following tickers are already in the catalog and will be ignored: ",
        paste(existing_update_tickers, collapse = ", ")
      )
    }

    catalog_update <- catalog_update %>%
      dplyr::filter(date == max(catalog$date)) %>%
      dplyr::filter(!legacy_ticker %in% catalog$tickers)

    #### Add if nrow > 0 and message
    if (nrow(catalog_update) > 0){
      catalog <- dplyr::bind_rows(catalog, catalog_update %>%
                                    dplyr::select(date, tickers = legacy_ticker, cvm_code_type))
      message("The following tickers were added to the catalog: ",
              paste(catalog_update$legacy_ticker, collapse = ", "))
    } else {
      message("No new tickers to add to the catalog.")
    }

  }

  ### Add funds with cvm_code_type = etfs
  fund_catalog <- data.frame(
    date             = max(catalog$date),
    tickers          = unique(as.character(fund_tickers)),
    cvm_code_type    = unique(as.character(fund_tickers)),
    stringsAsFactors = FALSE
  )

  fund_catalog <- fund_catalog %>%
    dplyr::filter(!tickers %in% catalog$tickers)

  catalog <- dplyr::bind_rows(catalog, fund_catalog) %>%
    dplyr::arrange(date, tickers)

  ## Adjust catalog-------------------------------------------------------------
  catalog_lookup_obj <- build_catalog_lookup_for_silver(
    catalog = catalog
  )

  catalog_ref_date <- catalog_lookup_obj$catalog_ref_date
  catalog_lookup   <- catalog_lookup_obj$catalog_lookup

  if (isTRUE(verbose)) {
    message("Using catalog reference date for ticker mapping: ", catalog_ref_date)
  }

  ## Apply to rebal_weights-----------------------------------------------------
    ###Apply
    rebal_weights <- rebal_weights %>%
      dplyr::mutate(
        date = as.Date(date),
        id = as.character(id),
        legacy_ticker = as.character(legacy_ticker),
        weights = as.numeric(weights)
      ) %>%
      dplyr::left_join(
        catalog_lookup,
        by = "legacy_ticker"
      ) %>%
      ### Rename BRL Curncy with LFTS11
      dplyr::mutate(
        cvm_code_type = ifelse(
          legacy_ticker == "BRL Curncy",
          "LFTS11",
          cvm_code_type
        ),
        legacy_ticker = ifelse(
          legacy_ticker == "BRL Curncy",
          "LFTS11",
          legacy_ticker
        )
      )

    ### Check for duplicates in date + id + cvm_code_type
    if (anyDuplicated(rebal_weights[c("date", "id", "cvm_code_type")]) > 0L) {
      stop(
        "`rebal_weights` has duplicated date + id + cvm_code_type rows after catalog mapping.",
        call. = FALSE
      )
    }

    ### Check for missing mappings
    missing_rebal_mapping <- rebal_weights %>%
      dplyr::filter(is.na(cvm_code_type)) %>%
      dplyr::distinct(date, id, legacy_ticker)

    if (nrow(missing_rebal_mapping) > 0) {
      stop(
        paste0(
          "Some rebalancing tickers could not be mapped to `cvm_code_type` using latest catalog. ",
          "First issue: date = ",
          missing_rebal_mapping$date[1],
          ", id = ",
          missing_rebal_mapping$id[1],
          ", legacy_ticker = ",
          missing_rebal_mapping$legacy_ticker[1],
          "."
        ),
        call. = FALSE
      )
    }

  ## Apply to comdinheiro_data--------------------------------------------------
    ### Apply
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

    ### Check missingness
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

    ### Deduplicate
    comdinheiro_data <- deduplicate_comdinheiro_data(comdinheiro_data)

    ### Check duplicates only among mapped rows
    mapped_comdinheiro_data <- comdinheiro_data %>%
      dplyr::filter(!is.na(cvm_code_type))

    if (anyDuplicated(mapped_comdinheiro_data[c("date", "cvm_code_type")]) > 0L) {
      stop(
        "`mapped_comdinheiro_data` has duplicated date + cvm_code_type rows after catalog mapping.",
        call. = FALSE
      )
    }

  ## Relocate cvm_code_type to after legacy_ticker
  rebal_weights <- rebal_weights %>%
    dplyr::relocate(cvm_code_type, .after = legacy_ticker)

  comdinheiro_data <- comdinheiro_data %>%
    dplyr::relocate(cvm_code_type, .after = legacy_ticker)

  ## Apply to sectors-----------------------------------------------------------

    ### Deduplicate
    sectors <- sectors %>%
      dplyr::filter(date == max(date)) %>%
      dplyr::select(-sectors_dynamic) %>%
      dplyr::mutate(
        date = as.Date(date),
        legacy_ticker = as.character(legacy_ticker)
      ) %>%
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::contains("sector"),
          .fns = as.character
        )
      ) %>%
      dplyr::distinct()

      #### Check
      duplicated_sector_tickers <- sectors %>%
        dplyr::count(date, legacy_ticker) %>%
        dplyr::filter(n > 1)

      if (nrow(duplicated_sector_tickers) > 0) {
        stop(
          paste0(
            "`sectors` still has duplicated date + legacy_ticker rows after `dplyr::distinct()`. ",
            "First issue: date = ",
            duplicated_sector_tickers$date[1],
            ", legacy_ticker = ",
            duplicated_sector_tickers$legacy_ticker[1],
            ", n = ",
            duplicated_sector_tickers$n[1],
            "."
          ),
          call. = FALSE
        )
      }

    ### Join
    sectors <- sectors %>%
      dplyr::left_join(
        catalog_lookup,
        by = "legacy_ticker"
        ) %>%
      dplyr::relocate(cvm_code_type, .after = legacy_ticker)


    ### Check for missingness
    missing_sector_mapping <- sectors %>%
      dplyr::filter(is.na(cvm_code_type)) %>%
      dplyr::distinct(date, legacy_ticker)

    if (nrow(missing_sector_mapping) > 0) {
      stop(
        paste0(
          "Some sector tickers could not be mapped to `cvm_code_type` using latest catalog. ",
          "First issue: date = ",
          missing_sector_mapping$date[1],
          ", legacy_ticker = ",
          missing_sector_mapping$legacy_ticker[1],
          "."
        ),
        call. = FALSE
      )
    }

  ## Apply to brokerage_data----------------------------------------------------

    brokerage_notes_log <- brokerage_data$brokerage_notes_log
    trade_data          <- brokerage_data$trade_data

    broker_accounts_lookup <- data.frame(
      fund_name = names(broker_accounts),
      fund_account = as.character(unname(broker_accounts)),
      stringsAsFactors = FALSE
    )

    if (anyDuplicated(broker_accounts_lookup$fund_account) > 0L) {
      stop("`broker_accounts` has duplicated account values.", call. = FALSE)
    }

    trade_data <- trade_data %>%
      dplyr::mutate(
        date = as.Date(date),
        fund_account = as.character(fund_account),
        legacy_ticker = as.character(legacy_ticker),
        ## Remove fractional-market suffix (e.g. PETR4F -> PETR4)
        legacy_ticker = gsub(
          pattern = "F$",
          replacement = "",
          x = toupper(legacy_ticker)
        ),
        side = as.character(side),
        amount = as.numeric(amount),
        price = as.numeric(price)
      ) %>%
      dplyr::left_join(
        broker_accounts_lookup,
        by = "fund_account"
      ) %>%
      dplyr::left_join(
        catalog_lookup,
        by = "legacy_ticker"
      ) %>%
      dplyr::relocate(fund_name, .after = fund_account) %>%
      dplyr::relocate(cvm_code_type, .after = legacy_ticker)

    missing_broker_account_mapping <- trade_data %>%
      dplyr::filter(is.na(fund_name)) %>%
      dplyr::distinct(date, fund_account)

    if (nrow(missing_broker_account_mapping) > 0L) {
      stop(
        paste0(
          "Some brokerage fund accounts could not be mapped to fund names. ",
          "First issue: date = ",
          missing_broker_account_mapping$date[1],
          ", fund_account = ",
          missing_broker_account_mapping$fund_account[1],
          "."
        ),
        call. = FALSE
      )
    }

    missing_trade_mapping <- trade_data %>%
      dplyr::filter(is.na(cvm_code_type)) %>%
      dplyr::distinct(date, legacy_ticker)

    if (nrow(missing_trade_mapping) > 0L) {
      stop(
        paste0(
          "Some brokerage tickers could not be mapped to `cvm_code_type` using latest catalog. ",
          "First issue: date = ",
          missing_trade_mapping$date[1],
          ", legacy_ticker = ",
          missing_trade_mapping$legacy_ticker[1],
          "."
        ),
        call. = FALSE
      )
    }

    if (anyDuplicated(trade_data[c("date", "fund_account", "legacy_ticker", "side", "amount", "price", "source_file")]) > 0L) {
      stop(
        "`trade_data` has duplicated trade rows after catalog mapping.",
        call. = FALSE
      )
    }

  ## Return---------------------------------------------------------------------
  return(
    list(
      rebal_weights       = rebal_weights,
      comdinheiro_data    = comdinheiro_data,
      sectors             = sectors,
      catalog             = catalog,
      trade_data          = trade_data,
      brokerage_notes_log = brokerage_notes_log
    )
  )
}



#Helpers-----------------------------------------------------------------------
build_catalog_lookup_for_silver <- function(catalog) {

  # Standardize column types ---------------------------------------------------
  catalog <- catalog %>%
    dplyr::mutate(
      date                 = as.Date(date),
      tickers              = as.character(tickers),
      cvm_code_type        = as.character(cvm_code_type),
      first_trading_date   = as.Date(first_trading_date),
      oldest_trading_date  = as.Date(oldest_trading_date),
      newest_trading_date  = as.Date(newest_trading_date),
      dates_cancel         = as.Date(dates_cancel)
    )

  ## Define reference date -----------------------------------------------------
  catalog_ref_date <- max(catalog$date, na.rm = TRUE)

  ## Extract catalog snapshot for reference date -------------------------------
  catalog_ref <- catalog %>%
    dplyr::filter(date == catalog_ref_date)

  ## Check for tickers associated with multiple CVM code types -----------------
  duplicated_tickers <- catalog_ref %>%
    dplyr::filter(
      !is.na(tickers),
      !is.na(cvm_code_type)
    ) %>%
    dplyr::distinct(
      tickers,
      cvm_code_type
    ) %>%
    dplyr::count(
      tickers,
      name = "n_cvm_code_type"
    ) %>%
    dplyr::filter(
      n_cvm_code_type > 1L
    )

  ### Warn user when ambiguous mappings are found
  if (nrow(duplicated_tickers) > 0L) {

    message(
      paste0(
        "Found ",
        nrow(duplicated_tickers),
        " ticker(s) associated with multiple cvm_code_type values. ",
        "To ensure stable mappings across monthly updates, ",
        "the oldest available definition will be retained ",
        "(priority: oldest_trading_date, first_trading_date, ",
        "newest_trading_date, cvm_code_type). ",
        "Affected tickers: ",
        paste(duplicated_tickers$tickers, collapse = ", ")
      )
    )

  }

  ## Build lookup table --------------------------------------------------------
  catalog_lookup <- catalog_ref %>%
    dplyr::arrange(
      tickers,
      oldest_trading_date,
      first_trading_date,
      newest_trading_date,
      cvm_code_type
    ) %>%
    dplyr::group_by(tickers) %>%
    dplyr::slice(1L) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      legacy_ticker = tickers,
      cvm_code_type
    )

  ## Return outputs ------------------------------------------------------------
  list(
    catalog_ref_date = catalog_ref_date,
    catalog_lookup   = catalog_lookup
  )

}

deduplicate_comdinheiro_data <- function(comdinheiro_data) {

  ### Required cols
  required_cols <- c("date", "cvm_code_type", "ret_1d")

  missing_cols <- base::setdiff(required_cols, base::names(comdinheiro_data))

  if (base::length(missing_cols) > 0L) {
    base::stop(
      "Missing required columns: ",
      base::paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  ### Split mapped and unmapped data
  unmapped_data <- comdinheiro_data %>%
    dplyr::filter(is.na(cvm_code_type))

  mapped_data <- comdinheiro_data %>%
    dplyr::filter(!is.na(cvm_code_type))

  ### Check for inconsistent ret_1d values within date + cvm_code_type groups
  inconsistent_returns <- mapped_data %>%
    dplyr::group_by(date, cvm_code_type) %>%
    dplyr::summarise(
      n_distinct_non_na_ret_1d = dplyr::n_distinct(ret_1d[!base::is.na(ret_1d)]),
      non_na_ret_1d_values = base::paste(
        base::unique(ret_1d[!base::is.na(ret_1d)]),
        collapse = ", "
      ),
      legacy_tickers = base::paste(
        base::unique(legacy_ticker),
        collapse = ", "
      ),
      .groups = "drop"
    ) %>%
    dplyr::filter(n_distinct_non_na_ret_1d > 1L)

  if (base::nrow(inconsistent_returns) > 0L) {
    base::print(inconsistent_returns)

    base::stop(
      "Duplicated date + cvm_code_type groups have conflicting non-NA ret_1d values.",
      call. = FALSE
    )
  }

  ### Deduplicate
  mapped_data_deduped <- mapped_data %>%
    dplyr::mutate(
      .available_info = base::rowSums(!base::is.na(dplyr::pick(dplyr::everything()))),
      .has_ret_1d = !base::is.na(ret_1d)
    ) %>%
    dplyr::arrange(
      date,
      cvm_code_type,
      dplyr::desc(.has_ret_1d),
      dplyr::desc(.available_info)
    ) %>%
    dplyr::group_by(date, cvm_code_type) %>%
    dplyr::slice(1L) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.available_info, -.has_ret_1d)

  dplyr::bind_rows(
    mapped_data_deduped,
    unmapped_data
  ) %>%
    dplyr::arrange(date, cvm_code_type, legacy_ticker)
}
