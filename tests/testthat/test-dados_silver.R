run_test_dados_silver <- function(dados_silver) {

  # Unit tests: catalog lookup ------------------------------------------------

  testthat::test_that("build_catalog_lookup_for_silver keeps latest catalog date only", {
    fixture <- make_apply_catalog_fixture()

    lookup_obj <- build_catalog_lookup_for_silver(
      catalog = fixture$rebalanceamento_tables$catalog
    )

    testthat::expect_equal(
      lookup_obj$catalog_ref_date,
      as.Date("2026-02-28")
    )

    testthat::expect_true("OLD3" %in% lookup_obj$catalog_lookup$legacy_ticker)
    testthat::expect_false(any(lookup_obj$catalog_lookup$legacy_ticker == "OLD3" &
                                 lookup_obj$catalog_lookup$cvm_code_type != "h100_ON"))
  })

  testthat::test_that("build_catalog_lookup_for_silver resolves duplicated tickers by oldest definition", {
    fixture <- make_apply_catalog_fixture()

    testthat::expect_message(
      lookup_obj <- build_catalog_lookup_for_silver(
        catalog = fixture$rebalanceamento_tables$catalog
      ),
      "oldest available definition"
    )

    dup_mapping <- lookup_obj$catalog_lookup %>%
      dplyr::filter(legacy_ticker == "DUP3")

    testthat::expect_equal(nrow(dup_mapping), 1L)
    testthat::expect_equal(dup_mapping$cvm_code_type, "h300_OLD_ON")
  })

  testthat::test_that("build_catalog_lookup_for_silver messages when ticker has multiple cvm_code_types", {
    fixture <- make_apply_catalog_fixture()

    testthat::expect_message(
      build_catalog_lookup_for_silver(
        catalog = fixture$rebalanceamento_tables$catalog
      ),
      "DUP3"
    )
  })
  # Unit tests: deduplication -------------------------------------------------

  testthat::test_that("deduplicate_comdinheiro_data keeps non-NA ret_1d over NA ret_1d", {
    duplicated_comd <- data.frame(
      date = as.Date(c("2026-03-02", "2026-03-02")),
      legacy_ticker = c("OLD3", "NEW3"),
      cvm_code_type = c("h100_ON", "h100_ON"),
      ret_1d = c(NA_real_, 1.25),
      price = c(10, 10.5),
      volume = c(NA_real_, 1000),
      stringsAsFactors = FALSE
    )

    out <- deduplicate_comdinheiro_data(duplicated_comd)

    testthat::expect_equal(nrow(out), 1L)
    testthat::expect_equal(out$ret_1d, 1.25)
    testthat::expect_equal(out$legacy_ticker, "NEW3")
  })

  testthat::test_that("deduplicate_comdinheiro_data keeps row with more available information when ret_1d tie", {
    duplicated_comd <- data.frame(
      date = as.Date(c("2026-03-02", "2026-03-02")),
      legacy_ticker = c("OLD3", "NEW3"),
      cvm_code_type = c("h100_ON", "h100_ON"),
      ret_1d = c(1.25, 1.25),
      price = c(NA_real_, 10.5),
      volume = c(NA_real_, 1000),
      stringsAsFactors = FALSE
    )

    out <- deduplicate_comdinheiro_data(duplicated_comd)

    testthat::expect_equal(nrow(out), 1L)
    testthat::expect_equal(out$legacy_ticker, "NEW3")
    testthat::expect_equal(out$price, 10.5)
    testthat::expect_equal(out$volume, 1000)
  })

  testthat::test_that("deduplicate_comdinheiro_data rejects conflicting non-NA returns", {
    duplicated_comd <- data.frame(
      date = as.Date(c("2026-03-02", "2026-03-02")),
      legacy_ticker = c("OLD3", "NEW3"),
      cvm_code_type = c("h100_ON", "h100_ON"),
      ret_1d = c(1.25, 1.30),
      price = c(10, 10.5),
      stringsAsFactors = FALSE
    )

    testthat::expect_error(
      deduplicate_comdinheiro_data(duplicated_comd),
      "conflicting non-NA ret_1d"
    )
  })

  testthat::test_that("deduplicate_comdinheiro_data rejects missing required columns", {
    bad_comd <- data.frame(
      date = as.Date("2026-03-02"),
      cvm_code_type = "h100_ON"
    )

    testthat::expect_error(
      deduplicate_comdinheiro_data(bad_comd),
      "Missing required columns"
    )
  })

  # Unit tests: catalog_update validation -------------------------------------

  testthat::test_that("apply_catalog rejects catalog_update missing required columns", {
    fixture <- make_apply_catalog_fixture()

    bad_catalog_update <- data.frame(
      date = as.Date("2026-02-28"),
      legacy_ticker = "NEW3"
    )

    testthat::expect_error(
      apply_catalog(
        rebalanceamento_tables = fixture$rebalanceamento_tables,
        comdinheiro_data = fixture$dados_bronze$comdinheiro_data,
        catalog_update = bad_catalog_update,
        brokerage_data = fixture$dados_bronze$brokerage_data,
        broker_accounts = fixture$broker_accounts,
        fund_tickers = fixture$fund_tickers,
        verbose = FALSE
      ),
      "catalog_update must contain"
    )
  })

  testthat::test_that("apply_catalog rejects catalog_update wrong column types", {
    fixture <- make_apply_catalog_fixture()

    bad_catalog_update <- data.frame(
      date = "2026-02-28",
      legacy_ticker = "NEW3",
      cvm_code_type = "h100_ON",
      stringsAsFactors = FALSE
    )

    testthat::expect_error(
      apply_catalog(
        rebalanceamento_tables = fixture$rebalanceamento_tables,
        comdinheiro_data = fixture$dados_bronze$comdinheiro_data,
        catalog_update = bad_catalog_update,
        brokerage_data = fixture$dados_bronze$brokerage_data,
        broker_accounts = fixture$broker_accounts,
        fund_tickers = fixture$fund_tickers,
        verbose = FALSE
      ),
      "catalog_update columns must be"
    )
  })

  testthat::test_that("apply_catalog rejects catalog_update cvm_code_type not beginning with h", {
    fixture <- make_apply_catalog_fixture()

    bad_catalog_update <- data.frame(
      date = as.Date("2026-02-28"),
      legacy_ticker = "NEW3",
      cvm_code_type = "BAD100_ON",
      stringsAsFactors = FALSE
    )

    testthat::expect_error(
      apply_catalog(
        rebalanceamento_tables = fixture$rebalanceamento_tables,
        comdinheiro_data = fixture$dados_bronze$comdinheiro_data,
        brokerage_data = fixture$dados_bronze$brokerage_data,
        broker_accounts = fixture$broker_accounts,
        catalog_update = bad_catalog_update,
        fund_tickers = fixture$fund_tickers,
        verbose = FALSE
      ),
      "must begin with 'h'"
    )
  })

  testthat::test_that("apply_catalog rejects duplicated legacy_ticker in catalog_update", {
    fixture <- make_apply_catalog_fixture()

    bad_catalog_update <- data.frame(
      date = as.Date(c("2026-02-28", "2026-02-28")),
      legacy_ticker = c("NEW3", "NEW3"),
      cvm_code_type = c("h100_ON", "h100_ON"),
      stringsAsFactors = FALSE
    )

    testthat::expect_error(
      apply_catalog(
        rebalanceamento_tables = fixture$rebalanceamento_tables,
        comdinheiro_data = fixture$dados_bronze$comdinheiro_data,
        brokerage_data = fixture$dados_bronze$brokerage_data,
        broker_accounts = fixture$broker_accounts,
        catalog_update = bad_catalog_update,
        fund_tickers = fixture$fund_tickers,
        verbose = FALSE
      ),
      "cannot contain duplicated legacy_ticker"
    )
  })

  testthat::test_that("apply_catalog ignores catalog_update rows already in catalog", {
    fixture <- make_apply_catalog_fixture()

    catalog_update <- data.frame(
      date = as.Date("2026-02-28"),
      legacy_ticker = "OLD3",
      cvm_code_type = "h100_ON",
      stringsAsFactors = FALSE
    )

    testthat::expect_warning(
      testthat::expect_message(
        out <- apply_catalog(
          rebalanceamento_tables = fixture$rebalanceamento_tables,
          comdinheiro_data = fixture$dados_bronze$comdinheiro_data,
          brokerage_data = fixture$dados_bronze$brokerage_data,
          broker_accounts = fixture$broker_accounts,
          catalog_update = catalog_update,
          fund_tickers = fixture$fund_tickers,
          verbose = FALSE
        ),
        "already in the catalog"
      ),
      "could not be mapped"
    )

    old3_rows <- out$catalog %>%
      dplyr::filter(tickers == "OLD3")

    testthat::expect_equal(nrow(old3_rows), 1L)
  })

  # Integration tests: apply_catalog output -----------------------------------

  testthat::test_that("apply_catalog adds cvm_code_type to rebal_weights and comdinheiro_data", {
    fixture <- make_apply_catalog_fixture()

    testthat::expect_warning(
      out <- apply_catalog(
        rebalanceamento_tables = fixture$rebalanceamento_tables,
        comdinheiro_data = fixture$dados_bronze$comdinheiro_data,
        brokerage_data = fixture$dados_bronze$brokerage_data,
        broker_accounts = fixture$broker_accounts,
        catalog_update = fixture$catalog_update,
        fund_tickers = fixture$fund_tickers,
        verbose = FALSE
      )
    )

    testthat::expect_true("cvm_code_type" %in% names(out$rebal_weights))
    testthat::expect_true("cvm_code_type" %in% names(out$comdinheiro_data))

    testthat::expect_equal(
      names(out$rebal_weights)[
        match("legacy_ticker", names(out$rebal_weights)) + 1L
      ],
      "cvm_code_type"
    )

    testthat::expect_equal(
      names(out$comdinheiro_data)[
        match("legacy_ticker", names(out$comdinheiro_data)) + 1L
      ],
      "cvm_code_type"
    )
  })

  testthat::test_that("apply_catalog maps brokerage_data fund accounts and fractional tickers", {
    fixture <- make_apply_catalog_fixture()

    testthat::expect_warning(
      out <- apply_catalog(
        rebalanceamento_tables = fixture$rebalanceamento_tables,
        comdinheiro_data = fixture$dados_bronze$comdinheiro_data,
        brokerage_data = fixture$dados_bronze$brokerage_data,
        catalog_update = fixture$catalog_update,
        fund_tickers = fixture$fund_tickers,
        broker_accounts = fixture$broker_accounts,
        verbose = FALSE
      )
    )

    trade_data <- out$trade_data

    testthat::expect_true("fund_name" %in% names(trade_data))
    testthat::expect_true("cvm_code_type" %in% names(trade_data))

    testthat::expect_equal(trade_data$fund_name, "sicoob_acoes")
    testthat::expect_equal(trade_data$legacy_ticker, "OLD3")
    testthat::expect_equal(trade_data$cvm_code_type, "h100_ON")
  })

  testthat::test_that("apply_catalog maps BRL Curncy to LFTS11 in rebal_weights", {
    fixture <- make_apply_catalog_fixture()

    testthat::expect_warning(
      out <- apply_catalog(
        rebalanceamento_tables = fixture$rebalanceamento_tables,
        comdinheiro_data = fixture$dados_bronze$comdinheiro_data,
        brokerage_data = fixture$dados_bronze$brokerage_data,
        broker_accounts = fixture$broker_accounts,
        catalog_update = fixture$catalog_update,
        fund_tickers = fixture$fund_tickers,
        verbose = FALSE
      )
    )

    cash_row <- out$rebal_weights %>%
      dplyr::filter(id == "qa_port", weights == 0.20)

    testthat::expect_equal(cash_row$legacy_ticker, "LFTS11")
    testthat::expect_equal(cash_row$cvm_code_type, "LFTS11")
  })

  testthat::test_that("apply_catalog adds fund tickers with cvm_code_type equal to ticker", {
    fixture <- make_apply_catalog_fixture()

    testthat::expect_warning(
      out <- apply_catalog(
        rebalanceamento_tables = fixture$rebalanceamento_tables,
        comdinheiro_data = fixture$dados_bronze$comdinheiro_data,
        brokerage_data = fixture$dados_bronze$brokerage_data,
        broker_accounts = fixture$broker_accounts,
        catalog_update = fixture$catalog_update,
        fund_tickers = fixture$fund_tickers,
        verbose = FALSE
      )
    )

    fund_rows <- out$catalog %>%
      dplyr::filter(tickers %in% fixture$fund_tickers)

    testthat::expect_true(all(fixture$fund_tickers %in% out$catalog$tickers))

    testthat::expect_true(
      all(fund_rows$tickers == fund_rows$cvm_code_type)
    )
  })

  testthat::test_that("apply_catalog maps new comdinheiro ticker to old rebalancing cvm_code_type via catalog_update", {
    fixture <- make_apply_catalog_fixture()

    testthat::expect_warning(
      out <- apply_catalog(
        rebalanceamento_tables = fixture$rebalanceamento_tables,
        comdinheiro_data = fixture$dados_bronze$comdinheiro_data,
        brokerage_data = fixture$dados_bronze$brokerage_data,
        broker_accounts = fixture$broker_accounts,
        catalog_update = fixture$catalog_update,
        fund_tickers = fixture$fund_tickers,
        verbose = FALSE
      ),
      "UNKNOWN3"
    )

    old_rebal_mapping <- out$rebal_weights %>%
      dplyr::filter(legacy_ticker == "OLD3") %>%
      dplyr::distinct(cvm_code_type)

    new_return_mapping <- out$comdinheiro_data %>%
      dplyr::filter(legacy_ticker == "NEW3") %>%
      dplyr::distinct(cvm_code_type)

    testthat::expect_equal(old_rebal_mapping$cvm_code_type, "h100_ON")
    testthat::expect_equal(new_return_mapping$cvm_code_type, "h100_ON")
  })

  testthat::test_that("apply_catalog deduplicates comdinheiro_data by date and cvm_code_type using catalog_update", {
    fixture <- make_apply_catalog_fixture()

    duplicated_new_data <- dplyr::bind_rows(
      fixture$dados_bronze$comdinheiro_data,
      data.frame(
        date = as.Date("2026-03-02"),
        legacy_ticker = "OLD3",
        ret_1d = 1.50,
        price = NA_real_,
        price_adj = NA_real_,
        volume = NA_real_,
        stringsAsFactors = FALSE
      )
    )

    fixture$dados_bronze$comdinheiro_data <- duplicated_new_data

    testthat::expect_warning(
      out <- apply_catalog(
        rebalanceamento_tables = fixture$rebalanceamento_tables,
        comdinheiro_data = fixture$dados_bronze$comdinheiro_data,
        brokerage_data = fixture$dados_bronze$brokerage_data,
        broker_accounts = fixture$broker_accounts,
        catalog_update = fixture$catalog_update,
        fund_tickers = fixture$fund_tickers,
        verbose = FALSE
      ),
      "UNKNOWN"
    )

    duplicates_after <- out$comdinheiro_data %>%
      dplyr::count(date, cvm_code_type) %>%
      dplyr::filter(n > 1)

    testthat::expect_equal(nrow(duplicates_after), 0L)

    kept_row <- out$comdinheiro_data %>%
      dplyr::filter(
        date == as.Date("2026-03-02"),
        cvm_code_type == "h100_ON"
      )

    testthat::expect_equal(nrow(kept_row), 1L)
    testthat::expect_equal(kept_row$legacy_ticker, "NEW3")
  })

  testthat::test_that("apply_catalog rejects conflicting duplicated returns after cvm_code_type mapping", {
    fixture <- make_apply_catalog_fixture()

    conflicting_data <- dplyr::bind_rows(
      fixture$dados_bronze$comdinheiro_data,
      data.frame(
        date = as.Date("2026-03-02"),
        legacy_ticker = "OLD3",
        ret_1d = 1.55,
        price = 10,
        price_adj = 10,
        volume = 100,
        stringsAsFactors = FALSE
      )
    )

    fixture$dados_bronze$comdinheiro_data <- conflicting_data

    testthat::expect_warning(
      testthat::expect_error(
        apply_catalog(
          rebalanceamento_tables = fixture$rebalanceamento_tables,
          comdinheiro_data = fixture$dados_bronze$comdinheiro_data,
          brokerage_data = fixture$dados_bronze$brokerage_data,
          broker_accounts = fixture$broker_accounts,
          catalog_update = fixture$catalog_update,
          fund_tickers = fixture$fund_tickers,
          verbose = FALSE
        ),
        "conflicting non-NA ret_1d"
      ), "UNKNOWN3"
    )

  })

  testthat::test_that("apply_catalog warns but keeps unmapped comdinheiro tickers", {
    fixture <- make_apply_catalog_fixture()

    testthat::expect_warning(
      out <- apply_catalog(
        rebalanceamento_tables = fixture$rebalanceamento_tables,
        comdinheiro_data = fixture$dados_bronze$comdinheiro_data,
        brokerage_data = fixture$dados_bronze$brokerage_data,
        broker_accounts = fixture$broker_accounts,
        catalog_update = fixture$catalog_update,
        fund_tickers = fixture$fund_tickers,
        verbose = FALSE
      ),
      "could not be mapped"
    )

    unmapped <- out$comdinheiro_data %>%
      dplyr::filter(legacy_ticker == "UNKNOWN3")

    testthat::expect_equal(nrow(unmapped), 1L)
    testthat::expect_true(is.na(unmapped$cvm_code_type))
  })

  testthat::test_that("apply_catalog rejects unmapped rebalancing tickers", {
    fixture <- make_apply_catalog_fixture()

    fixture$rebalanceamento_tables$rebal_weights <- dplyr::bind_rows(
      fixture$rebalanceamento_tables$rebal_weights,
      data.frame(
        date = as.Date("2026-03-02"),
        id = "bad_port",
        legacy_ticker = "UNKNOWN_REBAL3",
        weights = 1,
        stringsAsFactors = FALSE
      )
    )

    testthat::expect_error(
      apply_catalog(
        rebalanceamento_tables = fixture$rebalanceamento_tables,
        comdinheiro_data = fixture$dados_bronze$comdinheiro_data,
        brokerage_data = fixture$dados_bronze$brokerage_data,
        broker_accounts = fixture$broker_accounts,
        catalog_update = fixture$catalog_update,
        fund_tickers = fixture$fund_tickers,
        verbose = FALSE
      ),
      "could not be mapped"
    )
  })

  testthat::test_that("apply_catalog rejects duplicated date id cvm_code_type in rebal_weights after mapping", {
    fixture <- make_apply_catalog_fixture()

    fixture$rebalanceamento_tables$rebal_weights <- dplyr::bind_rows(
      fixture$rebalanceamento_tables$rebal_weights,
      data.frame(
        date = as.Date("2026-03-02"),
        id = "qa_port",
        legacy_ticker = "NEW3",
        weights = 0,
        stringsAsFactors = FALSE
      )
    )

    testthat::expect_error(
      apply_catalog(
        rebalanceamento_tables = fixture$rebalanceamento_tables,
        comdinheiro_data = fixture$dados_bronze$comdinheiro_data,
        brokerage_data = fixture$dados_bronze$brokerage_data,
        broker_accounts = fixture$broker_accounts,
        catalog_update = fixture$catalog_update,
        fund_tickers = fixture$fund_tickers,
        verbose = FALSE
      ),
      "duplicated date \\+ id \\+ cvm_code_type"
    )
  })

  testthat::test_that("apply_catalog output preserves key row counts except comdinheiro deduplication", {
    fixture <- make_apply_catalog_fixture()

    testthat::expect_warning(
      out <- apply_catalog(
        rebalanceamento_tables = fixture$rebalanceamento_tables,
        comdinheiro_data = fixture$dados_bronze$comdinheiro_data,
        brokerage_data = fixture$dados_bronze$brokerage_data,
        broker_accounts = fixture$broker_accounts,
        catalog_update = fixture$catalog_update,
        fund_tickers = fixture$fund_tickers,
        verbose = FALSE
      )
    )

    testthat::expect_equal(
      nrow(out$rebal_weights),
      nrow(fixture$rebalanceamento_tables$rebal_weights)
    )

    testthat::expect_lte(
      nrow(out$comdinheiro_data),
      nrow(fixture$dados_bronze$comdinheiro_data)
    )
  })

  # Data quality on dados_silver------------------------------------------------
  rebal_weights <- dados_silver$rebalanceamento_tables$rebal_weights
  sectors <- dados_silver$rebalanceamento_tables$sectors
  catalog <- dados_silver$rebalanceamento_tables$catalog
  comdinheiro_data <- dados_silver$comdinheiro_data
  brokerage_data <- dados_silver$brokerage_data
  trade_data <- brokerage_data$trade_data
  split_inplit_data <- dados_silver$split_inplit_data
  port_iniciais <- dados_silver$port_iniciais

  testthat::test_that("dados_silver has expected top-level structure", {
    testthat::expect_true(is.list(dados_silver))
    testthat::expect_true("rebalanceamento_tables" %in% names(dados_silver))
    testthat::expect_true("comdinheiro_data" %in% names(dados_silver))
    testthat::expect_true("brokerage_data" %in% names(dados_silver))

    testthat::expect_true(is.list(dados_silver$rebalanceamento_tables))
    testthat::expect_true(all(c("rebal_weights", "sectors", "catalog") %in% names(dados_silver$rebalanceamento_tables)))
  })

  testthat::test_that("silver tables have required columns", {
    testthat::expect_true(all(c("date", "id", "legacy_ticker", "cvm_code_type", "weights") %in% names(rebal_weights)))
    testthat::expect_true(all(c("date", "legacy_ticker", "cvm_code_type") %in% names(sectors)))
    testthat::expect_true(all(c("date", "tickers", "cvm_code_type") %in% names(catalog)))
    testthat::expect_true(all(c("date", "legacy_ticker", "cvm_code_type", "ret_1d") %in% names(comdinheiro_data)))
    testthat::expect_true(
      all(c("date", "fund_account", "fund_name", "legacy_ticker", "cvm_code_type") %in% names(trade_data))
    )

  })

  testthat::test_that("silver date and identifier types are valid", {
    testthat::expect_true(inherits(rebal_weights$date, "Date"))
    testthat::expect_true(inherits(sectors$date, "Date"))
    testthat::expect_true(inherits(catalog$date, "Date"))
    testthat::expect_true(inherits(comdinheiro_data$date, "Date"))

    testthat::expect_true(is.character(rebal_weights$legacy_ticker))
    testthat::expect_true(is.character(rebal_weights$cvm_code_type))
    testthat::expect_true(is.character(sectors$legacy_ticker))
    testthat::expect_true(is.character(sectors$cvm_code_type))
    testthat::expect_true(is.character(catalog$tickers))
    testthat::expect_true(is.character(catalog$cvm_code_type))
    testthat::expect_true(is.character(comdinheiro_data$legacy_ticker))
    testthat::expect_true(is.character(comdinheiro_data$cvm_code_type) || all(is.na(comdinheiro_data$cvm_code_type)))
  })

  testthat::test_that("rebal_weights has complete mappings and valid portfolio weights", {
    testthat::expect_false(any(is.na(rebal_weights$cvm_code_type)))

    duplicated_keys <- rebal_weights %>%
      dplyr::count(date, id, cvm_code_type) %>%
      dplyr::filter(n > 1)

    testthat::expect_equal(nrow(duplicated_keys), 0L)

    weight_sums <- rebal_weights %>%
      dplyr::group_by(date, id) %>%
      dplyr::summarise(weight_sum = sum(weights, na.rm = TRUE), .groups = "drop")

    testthat::expect_true(all(abs(weight_sums$weight_sum - 1) < 1e-3))
    testthat::expect_false(any(is.na(rebal_weights$weights)))
    testthat::expect_false(any(rebal_weights$weights < -1e-12))
  })

  testthat::test_that("mapped comdinheiro_data has unique date + cvm_code_type keys", {
    mapped_comdinheiro <- comdinheiro_data %>%
      dplyr::filter(!is.na(cvm_code_type))

    duplicated_keys <- mapped_comdinheiro %>%
      dplyr::count(date, cvm_code_type) %>%
      dplyr::filter(n > 1)

    testthat::expect_equal(nrow(duplicated_keys), 0L)
  })

  testthat::test_that("brokerage trade_data is fully mapped", {

    testthat::expect_false(any(is.na(trade_data$fund_name)))

    testthat::expect_false(any(is.na(trade_data$cvm_code_type)))

    testthat::expect_true(
      all(!grepl("F$", trade_data$legacy_ticker))
    )

    duplicated_trades <- trade_data %>%
      dplyr::count(
        date,
        fund_account,
        legacy_ticker,
        side,
        amount,
        price
      ) %>%
      dplyr::filter(n > 1)

    testthat::expect_equal(
      nrow(duplicated_trades),
      0L
    )

  })

  testthat::test_that("all rebalancing cvm_code_types are available in comdinheiro_data on relevant dates", {
    rebal_dates <- sort(unique(rebal_weights$date))
    comd_dates <- sort(unique(comdinheiro_data$date))

    missing_by_rebal_date <- purrr::map_dfr(
      rebal_dates,
      function(rebal_date) {
        next_rebal_date <- rebal_dates[rebal_dates > rebal_date][1]

        interval_dates <- comd_dates[
          comd_dates >= rebal_date &
            if (is.na(next_rebal_date)) {
              TRUE
            } else {
              comd_dates < next_rebal_date
            }
        ]

        expected_assets <- rebal_weights %>%
          dplyr::filter(date == rebal_date) %>%
          dplyr::distinct(cvm_code_type) %>%
          dplyr::pull(cvm_code_type)

        purrr::map_dfr(
          interval_dates,
          function(date_i) {
            available_assets <- comdinheiro_data %>%
              dplyr::filter(date == date_i) %>%
              dplyr::filter(!is.na(cvm_code_type)) %>%
              dplyr::distinct(cvm_code_type) %>%
              dplyr::pull(cvm_code_type)

            missing_assets <- setdiff(expected_assets, available_assets)

            if (length(missing_assets) == 0L) {
              return(data.frame())
            }

            data.frame(
              rebal_date = rebal_date,
              date = date_i,
              missing_cvm_code_type = missing_assets,
              stringsAsFactors = FALSE
            )
          }
        )
      }
    )

    testthat::expect_equal(nrow(missing_by_rebal_date), 0L)
  })

  testthat::test_that("sectors are deduplicated and mapped", {
    testthat::expect_false(any(is.na(sectors$cvm_code_type)))

    duplicated_sector_keys <- sectors %>%
      dplyr::count(date, legacy_ticker) %>%
      dplyr::filter(n > 1)

    testthat::expect_equal(nrow(duplicated_sector_keys), 0L)

    duplicated_sector_cvm <- sectors %>%
      dplyr::count(date, legacy_ticker, cvm_code_type) %>%
      dplyr::filter(n > 1)

    testthat::expect_equal(nrow(duplicated_sector_cvm), 0L)
  })

  testthat::test_that("silver tables share coherent date coverage", {
    testthat::expect_true(all(unique(rebal_weights$date) %in% unique(catalog$date)))
    testthat::expect_true(all(unique(sectors$date) %in% unique(catalog$date)))

    testthat::expect_true(min(comdinheiro_data$date) <= min(rebal_weights$date))
    testthat::expect_true(max(comdinheiro_data$date) >= min(rebal_weights$date))
  })

  testthat::test_that("cvm_code_type of split_inplit data is present in catalog and comdinheiro_data", {
    split_cvm_types <- unique(split_inplit_data$cvm_code_type)
    catalog_cvm_types <- unique(catalog$cvm_code_type)
    comd_cvm_types <- unique(comdinheiro_data$cvm_code_type)

    missing_in_catalog <- setdiff(split_cvm_types, catalog_cvm_types)
    missing_in_comd <- setdiff(split_cvm_types, comd_cvm_types)

    testthat::expect_equal(length(missing_in_catalog), 0L, info = paste("Missing in catalog:", paste(missing_in_catalog, collapse = ", ")))
    testthat::expect_equal(length(missing_in_comd), 0L, info = paste("Missing in comdinheiro_data:", paste(missing_in_comd, collapse = ", ")))

    ## Also check that each cvm_code_type+legacy_ticker combination is present
    concatenated_ticker_cvm_code_type <- split_inplit_data %>%
      dplyr::mutate(concatenated_ticker_cvm_code_type =
                      paste0(cvm_code_type, "_", legacy_ticker)) %>%
      dplyr::pull(concatenated_ticker_cvm_code_type)

    catalog_combinations <- catalog %>%
      dplyr::mutate(concatenated_ticker_cvm_code_type =
                      paste0(cvm_code_type, "_", tickers)) %>%
      dplyr::pull(concatenated_ticker_cvm_code_type)

    comd_combinations <- comdinheiro_data %>%
      dplyr::mutate(concatenated_ticker_cvm_code_type =
                      paste0(cvm_code_type, "_", legacy_ticker)) %>%
      dplyr::pull(concatenated_ticker_cvm_code_type)

    missing_combinations <- setdiff(concatenated_ticker_cvm_code_type, catalog_combinations)
    testthat::expect_equal(length(missing_combinations), 0L)

    missing_combinations_comd <- setdiff(concatenated_ticker_cvm_code_type, comd_combinations)
    testthat::expect_equal(length(missing_combinations_comd), 0L)


  })

  testthat::test_that("cvm_code_type of port_iniciais is present in catalog and comdinheiro_data", {

    if (is.null(port_iniciais) || nrow(port_iniciais) == 0L) {
      testthat::skip("port_iniciais is not available or empty, skipping cvm_code_type consistency test")
    }

    port_iniciais_cvm_types <- unique(port_iniciais$cvm_code_type)
    catalog_cvm_types <- unique(catalog$cvm_code_type)
    comd_cvm_types <- unique(comdinheiro_data$cvm_code_type)

    missing_in_catalog <- setdiff(port_iniciais_cvm_types, catalog_cvm_types)
    missing_in_comd <- setdiff(port_iniciais_cvm_types, comd_cvm_types)

    testthat::expect_equal(length(missing_in_catalog), 0L, info = paste("Missing in catalog:", paste(missing_in_catalog, collapse = ", ")))
    testthat::expect_equal(length(missing_in_comd), 0L, info = paste("Missing in comdinheiro_data:", paste(missing_in_comd, collapse = ", ")))
  })

  testthat::test_that("port_iniciais prices match those in comdinheiro_data", {

    if (is.null(port_iniciais) || nrow(port_iniciais) == 0L) {
      testthat::skip("port_iniciais is not available or empty, skipping cvm_code_type consistency test")
    }

    testthat::expect_equal(
      port_iniciais %>% dplyr::left_join(
        comdinheiro_data %>% dplyr::select(cvm_code_type, date, price) %>%
          dplyr::distinct(),
        by = c("cvm_code_type", "date")
      ) %>%
        dplyr::mutate(diff = price.x - price.y) %>%
        dplyr::filter(abs(diff) > 1e-3) %>%
        nrow(),
      0
    )



  })



  invisible(TRUE)
}


##Helpers-----------------------------------------------------------------------
make_apply_catalog_fixture <- function() {
  catalog <- data.frame(
    date = as.Date(c(
      "2026-01-31", "2026-01-31", "2026-01-31",
      "2026-02-28", "2026-02-28", "2026-02-28",
      "2026-02-28", "2026-02-28"
    )),
    tickers = c(
      "OLD3", "KEEP3", "DUP3",
      "OLD3", "KEEP3", "DUP3",
      "DUP3", "BRST3"
    ),
    cvm_code_type = c(
      "h100_ON", "h200_ON", "h300_ON",
      "h100_ON", "h200_ON", "h300_OLD_ON",
      "h300_NEW_ON", "h400_ON"
    ),
    first_trading_date = as.Date(c(
      "2020-01-01", "2020-01-01", "2020-01-01",
      "2020-01-01", "2020-01-01", "2020-01-01",
      "2024-01-01", "2020-01-01"
    )),
    oldest_trading_date = as.Date(c(
      "2026-01-01", "2026-01-01", "2026-01-01",
      "2026-02-01", "2026-02-01", "2026-01-15",
      "2026-02-28", "2026-02-28"
    )),
    newest_trading_date = as.Date(c(
      "2026-01-30", "2026-01-30", "2026-01-30",
      "2026-02-20", "2026-02-25", "2026-01-15",
      "2026-02-28", "2026-02-28"
    )),
    dates_cancel = as.Date(c(
      NA, NA, NA,
      NA, NA, "2026-01-31",
      NA, NA
    )),
    stringsAsFactors = FALSE
  )


  rebal_weights <- data.frame(
    date = as.Date(c(
      "2026-03-02",
      "2026-03-02",
      "2026-03-02",
      "2026-03-02"
    )),
    id = c(
      "qa_port",
      "qa_port",
      "qa_port",
      "fund_port"
    ),
    legacy_ticker = c(
      "OLD3",
      "KEEP3",
      "BRL Curncy",
      "BOVA11"
    ),
    weights = c(
      0.40,
      0.40,
      0.20,
      1.00
    ),
    stringsAsFactors = FALSE
  )

  sectors <- data.frame(
    date = as.Date("2026-03-02"),
    legacy_ticker = c("OLD3", "KEEP3"),
    sectors_c1 = c("sector_a", "sector_b"),
    sectors_dynamic = c("dynamic_a", "dynamic_b"),
    stringsAsFactors = FALSE
  )

  comdinheiro_data <- data.frame(
    date = as.Date(c(
      "2026-03-02", "2026-03-02", "2026-03-02",
      "2026-03-02", "2026-03-02",
      "2026-03-03", "2026-03-03", "2026-03-03"
    )),
    legacy_ticker = c(
      "NEW3",      # added through catalog_update, same cvm as OLD3
      "KEEP3",
      "LFTS11",
      "BOVA11",
      "UNKNOWN3",
      "NEW3",
      "KEEP3",
      "LFTS11"
    ),
    ret_1d = c(
      1.50,
      -0.50,
      0.04,
      0.75,
      9.99,
      2.00,
      1.00,
      0.05
    ),
    price = c(
      10, 20, 100, 50, 999,
      10.2, 20.2, 100.1
    ),
    price_adj = c(
      10, 20, 100, 50, 999,
      10.2, 20.2, 100.1
    ),
    volume = c(
      1000, 2000, 3000, 4000, 9999,
      1100, 2100, 3100
    ),
    stringsAsFactors = FALSE
  )

  rebalanceamento_tables <- list(
    rebal_weights = rebal_weights,
    sectors = sectors,
    catalog = catalog
  )

  brokerage_data <- list(
    brokerage_notes_log = data.frame(
      received_date = as.Date("2026-03-02"),
      saved_path = "dummy.xlsx",
      stringsAsFactors = FALSE
    ),
    trade_data = data.frame(
      date = as.Date("2026-03-02"),
      fund_account = "40010",
      legacy_ticker = "OLD3F",
      side = "buy",
      amount = 50,
      price = 10,
      traded_volume = 500,
      brokerage_fee_bps = 5.5,
      brokerage_fee_estimated = 500 * 5.5 / 10000,
      source_file = "dummy.xlsx",
      stringsAsFactors = FALSE
    )
  )

  dados_bronze <- list(
      comdinheiro_data = comdinheiro_data,
      rebalanceamento_tables = rebalanceamento_tables,
      brokerage_data = brokerage_data
  )

  catalog_update <- data.frame(
    date = as.Date("2026-02-28"),
    legacy_ticker = "NEW3",
    cvm_code_type = "h100_ON",
    stringsAsFactors = FALSE
  )

  broker_accounts <- c(
    sicoob_acoes = "40010",
    sicoob_small_caps = "49887"
  )

  list(
    rebalanceamento_tables = rebalanceamento_tables,
    dados_bronze = dados_bronze,
    catalog_update = catalog_update,
    fund_tickers = c("BOVA11", "LFTS11"),
    broker_accounts = broker_accounts
  )
}
