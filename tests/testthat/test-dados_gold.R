run_test_dados_gold <- function(evolved_portfolios, newest_rebal_portfolio_ids,
                                rebal_weights, comdinheiro_data){

  run_validate_evolve_portfolio_inputs_test()

  run_split_candidate_helpers_test()

  run_compute_paper_portfolio_step_test()

  run_compute_real_portfolio_step_test()

  run_compute_real_portfolio_step_fabricated_trades_test()

  run_bind_old_dados_gold_test()

  run_derive_old_portfolio_and_validate_ids_test()

  run_evolve_portfolio_integration_test()

  run_test_evolved_portfolios_quality(
    evolved_portfolios = evolved_portfolios,
    newest_rebal_portfolio_ids = newest_rebal_portfolio_ids,
    rebal_weights = rebal_weights,
    comdinheiro_data = comdinheiro_data
  )

}
#Unit tests---------------------------------------------------------------------
run_validate_evolve_portfolio_inputs_test <- function() {

  build_valid_inputs <- function() {

    current_dates <- as.Date(c("2026-04-23", "2026-04-24"))

    old_portfolio <- list(
      paper = list(
        portfolio = data.frame(
          date = as.Date("2026-04-22"),
          id = "strategy_FIA",
          cvm_code_type = c("AAA3", "BBB4"),
          eop_weights = c(0.60, 0.40),
          stringsAsFactors = FALSE
        )
      ),
      real = list(
        portfolio = data.frame(
          date = as.Date("2026-04-22"),
          id = "strategy_FIA",
          fund_name = "sicoob_acoes",
          cvm_code_type = c("AAA3", "BBB4"),
          eop_positions = c(100, 200),
          price = c(10, 20),
          stringsAsFactors = FALSE
        )
      )
    )

    rebal_weights <- data.frame(
      date = as.Date("2026-04-23"),
      id = "strategy_FIA",
      legacy_ticker = c("AAA3", "BBB4"),
      cvm_code_type = c("AAA3", "BBB4"),
      weights = c(0.50, 0.50),
      stringsAsFactors = FALSE
    )

    comdinheiro_data <- data.frame(
      date = rep(as.Date(c("2026-04-22", "2026-04-23", "2026-04-24")), each = 2),
      legacy_ticker = rep(c("AAA3", "BBB4"), times = 3),
      cvm_code_type = rep(c("AAA3", "BBB4"), times = 3),
      ret_1d = c(0, 0, 1, -1, 0.5, 0.2),
      price = c(10, 20, 10.1, 19.8, 10.15, 19.84),
      proventos = 0,
      proventos_date = as.Date(NA),
      event_factor = 1,
      n_shares = c(1000000, 2000000, 1000000, 2000000, 1000000, 2000000),
      stringsAsFactors = FALSE
    )

    brokerage_data <- data.frame(
      date = as.Date("2026-04-23"),
      fund_name = "sicoob_acoes",
      legacy_ticker = "AAA3",
      cvm_code_type = "AAA3",
      side = "buy",
      amount = 10,
      price = 10.1,
      traded_volume = 101,
      brokerage_fee_estimated = 0.5,
      stringsAsFactors = FALSE
    )

    split_inplit_data <- data.frame(
      date = as.Date(character()),
      legacy_ticker = character(),
      cvm_code_type = character(),
      split_factor = numeric(),
      stringsAsFactors = FALSE
    )

    transaction_costs_bps <- data.frame(
      date = current_dates,
      id = "strategy_FIA",
      transaction_cost_bps = c(1, 0),
      stringsAsFactors = FALSE
    )

    list(
      old_portfolio = old_portfolio,
      rebal_weights = rebal_weights,
      comdinheiro_data = comdinheiro_data,
      current_dates = current_dates,
      brokerage_data = brokerage_data,
      id = "strategy_FIA",
      fund_name = "sicoob_acoes",
      split_inplit_data = split_inplit_data,
      transaction_costs_bps = transaction_costs_bps,
      fund_fees_bps = 2,
      weight_tolerance = 1e-2,
      position_tolerance = 1e-8
    )
  }

  call_validator <- function(inputs) {
    do.call(validate_evolve_portfolio_inputs, inputs)
  }

  testthat::test_that("valid inputs pass and return required validated objects", {
    inputs <- build_valid_inputs()

    out <- call_validator(inputs)

    testthat::expect_true(is.list(out))
    testthat::expect_equal(out$current_dates, inputs$current_dates)
    testthat::expect_equal(out$id, "strategy_FIA")
    testthat::expect_equal(out$fund_name, "sicoob_acoes")

    testthat::expect_true(is.data.frame(out$old_port_last_eop_weights))
    testthat::expect_true(is.data.frame(out$old_port_last_eop_positions))
    testthat::expect_true(is.data.frame(out$old_port_last_prices))

    testthat::expect_true(all(c("cvm_code_type", "eop_weights") %in% names(out$old_port_last_eop_weights)))
    testthat::expect_true(all(c("cvm_code_type", "eop_positions") %in% names(out$old_port_last_eop_positions)))
    testthat::expect_true(all(c("cvm_code_type", "price_lag") %in% names(out$old_port_last_prices)))

    testthat::expect_equal(sum(out$old_port_last_eop_weights$eop_weights), 1, tolerance = 1e-12)
    testthat::expect_true(is.finite(out$old_port_last_eop_market_value))
    testthat::expect_gt(out$old_port_last_eop_market_value, 0)

    testthat::expect_true(all(c("date", "id", "cvm_code_type", "eop_weights") %in% names(out$old_paper_last)))
    testthat::expect_true(all(c("date", "id", "fund_name", "cvm_code_type", "eop_positions", "price") %in% names(out$old_real_last)))
  })

  testthat::test_that("fund_name = NULL creates empty trade_data even when brokerage_data is supplied", {
    inputs <- build_valid_inputs()
    inputs$fund_name <- NULL

    out <- call_validator(inputs)

    testthat::expect_null(out$fund_name)
    testthat::expect_true(is.data.frame(out$trade_data))
    testthat::expect_equal(nrow(out$trade_data), 0L)
  })

  testthat::test_that("invalid current_dates are blocked", {
    inputs <- build_valid_inputs()
    inputs$current_dates <- as.Date(character())

    testthat::expect_error(
      call_validator(inputs),
      "`current_dates` must contain valid Date values.",
      fixed = TRUE
    )

    inputs <- build_valid_inputs()
    inputs$current_dates <- c(as.Date("2026-04-23"), NA)

    testthat::expect_error(
      call_validator(inputs),
      "`current_dates` must contain valid Date values.",
      fixed = TRUE
    )
  })

  testthat::test_that("missing id and fund_name are blocked", {
    inputs <- build_valid_inputs()
    inputs$id <- NULL

    testthat::expect_error(
      call_validator(inputs),
      "`id` must be supplied.",
      fixed = TRUE
    )

  })

  testthat::test_that("id and fund_name business rules are enforced", {
    inputs <- build_valid_inputs()
    inputs$id <- "strategy_FIA"
    inputs$fund_name <- "wrong_fund"

    testthat::expect_error(
      call_validator(inputs),
      "For id ending with '_FIA', fund_name must be 'sicoob_acoes'.",
      fixed = TRUE
    )

    inputs <- build_valid_inputs()
    inputs$id <- "strategy_IDIV"
    inputs$old_portfolio$paper$portfolio$id <- "strategy_IDIV"
    inputs$old_portfolio$real$portfolio$id <- "strategy_IDIV"
    inputs$rebal_weights$id <- "strategy_IDIV"
    inputs$transaction_costs_bps$id <- "strategy_IDIV"
    inputs$fund_name <- "sicoob_acoes"
    inputs$old_portfolio$real$portfolio$fund_name <- "sicoob_acoes"
    inputs$brokerage_data$fund_name <- "sicoob_acoes"

    testthat::expect_error(
      call_validator(inputs),
      "For id ending with '_IDIV', fund_name must be 'sicoob_dividendos'.",
      fixed = TRUE
    )
  })

  testthat::test_that("old_portfolio must follow the paper/real list schema", {
    inputs <- build_valid_inputs()
    inputs$old_portfolio <- data.frame()

    testthat::expect_error(
      call_validator(inputs),
      "`old_portfolio` must be a list.",
      fixed = TRUE
    )

    inputs <- build_valid_inputs()
    inputs$old_portfolio$paper <- NULL

    testthat::expect_error(
      call_validator(inputs),
      "`old_portfolio` must contain element `paper`.",
      fixed = TRUE
    )

    inputs <- build_valid_inputs()
    inputs$old_portfolio$real <- NULL

    testthat::expect_error(
      call_validator(inputs),
      "`old_portfolio` must contain element `real`.",
      fixed = TRUE
    )

    inputs <- build_valid_inputs()
    inputs$old_portfolio$paper$portfolio <- NULL

    testthat::expect_error(
      call_validator(inputs),
      "`old_portfolio$paper` must be a list containing `portfolio`.",
      fixed = TRUE
    )
  })

  testthat::test_that("old paper portfolio required columns are enforced", {
    inputs <- build_valid_inputs()
    inputs$old_portfolio$paper$portfolio$cvm_code_type <- NULL

    testthat::expect_error(
      call_validator(inputs),
      "`old_portfolio$paper$portfolio` is missing column(s): cvm_code_type.",
      fixed = TRUE
    )

    inputs <- build_valid_inputs()
    inputs$old_portfolio$paper$portfolio$eop_weights <- NULL

    testthat::expect_error(
      call_validator(inputs),
      "`old_portfolio$paper$portfolio` is missing column(s): eop_weights.",
      fixed = TRUE
    )

    inputs <- build_valid_inputs()
    inputs$old_portfolio$paper$portfolio$date <- NULL

    testthat::expect_error(
      call_validator(inputs),
      "`old_portfolio$paper$portfolio` is missing column(s): date.",
      fixed = TRUE
    )
  })

  testthat::test_that("old real portfolio required columns are enforced", {
    inputs <- build_valid_inputs()
    inputs$old_portfolio$real$portfolio$eop_positions <- NULL

    testthat::expect_error(
      call_validator(inputs),
      "`old_portfolio$real$portfolio` is missing column(s): eop_positions.",
      fixed = TRUE
    )

    inputs <- build_valid_inputs()
    inputs$old_portfolio$real$portfolio$price <- NULL

    testthat::expect_error(
      call_validator(inputs),
      "`old_portfolio$real$portfolio` is missing column(s): price.",
      fixed = TRUE
    )

    inputs <- build_valid_inputs()
    inputs$old_portfolio$real$portfolio$fund_name <- NULL

    testthat::expect_error(
      call_validator(inputs),
      "`old_portfolio$real$portfolio` is missing column(s): fund_name.",
      fixed = TRUE
    )
  })

  testthat::test_that("old portfolio ids and fund_name must match supplied arguments", {
    inputs <- build_valid_inputs()
    inputs$old_portfolio$paper$portfolio$id[1] <- "other_id"

    testthat::expect_error(
      call_validator(inputs),
      "`old_portfolio$paper$portfolio$id` must match the supplied `id`.",
      fixed = TRUE
    )

    inputs <- build_valid_inputs()
    inputs$old_portfolio$real$portfolio$id[1] <- "other_id"

    testthat::expect_error(
      call_validator(inputs),
      "`old_portfolio$real$portfolio$id` must match the supplied `id`.",
      fixed = TRUE
    )

    inputs <- build_valid_inputs()
    inputs$old_portfolio$real$portfolio$fund_name[1] <- "other_fund"

    testthat::expect_error(
      call_validator(inputs),
      "`old_portfolio$real$portfolio$fund_name` must match the supplied `fund_name`.",
      fixed = TRUE
    )
  })

  testthat::test_that("old paper last weights must be valid", {
    inputs <- build_valid_inputs()
    inputs$old_portfolio$paper$portfolio$eop_weights <- c(0.70, 0.40)

    testthat::expect_error(
      call_validator(inputs),
      "`old_portfolio$paper$portfolio$eop_weights` must sum to 1 on the last old date.",
      fixed = TRUE
    )

    inputs <- build_valid_inputs()
    inputs$old_portfolio$paper$portfolio$eop_weights <- c(1.10, -0.10)

    testthat::expect_error(
      call_validator(inputs),
      "`old_portfolio$paper$portfolio$eop_weights` contains negative weights on the last date.",
      fixed = TRUE
    )

    inputs <- build_valid_inputs()
    inputs$old_portfolio$paper$portfolio <- rbind(
      inputs$old_portfolio$paper$portfolio,
      inputs$old_portfolio$paper$portfolio[1, ]
    )

    testthat::expect_error(
      call_validator(inputs),
      "`old_portfolio$paper$portfolio` has duplicated `id + cvm_code_type` rows on the last date.",
      fixed = TRUE
    )
  })

  testthat::test_that("old real last positions and prices must be valid", {
    inputs <- build_valid_inputs()
    inputs$old_portfolio$real$portfolio$eop_positions[1] <- -1

    testthat::expect_error(
      call_validator(inputs),
      "`old_portfolio$real$portfolio$eop_positions` contains negative positions on the last date.",
      fixed = TRUE
    )

    inputs <- build_valid_inputs()
    inputs$old_portfolio$real$portfolio$price[1] <- 0

    testthat::expect_error(
      call_validator(inputs),
      "`old_portfolio$real$portfolio$price` must be positive and finite on the last date.",
      fixed = TRUE
    )

    inputs <- build_valid_inputs()
    inputs$old_portfolio$real$portfolio$eop_positions <- c(0, 0)

    testthat::expect_error(
      call_validator(inputs),
      "Could not compute a positive `old_port_last_eop_market_value` from the last old real portfolio state.",
      fixed = TRUE
    )

    inputs <- build_valid_inputs()
    inputs$old_portfolio$real$portfolio <- rbind(
      inputs$old_portfolio$real$portfolio,
      inputs$old_portfolio$real$portfolio[1, ]
    )

    testthat::expect_error(
      call_validator(inputs),
      "`old_portfolio$real$portfolio` has duplicated `id + fund_name + cvm_code_type` rows on the last date.",
      fixed = TRUE
    )
  })

  testthat::test_that("old paper and real last dates must be aligned and before current_dates", {
    inputs <- build_valid_inputs()
    inputs$old_portfolio$real$portfolio$date <- as.Date("2026-04-21")

    testthat::expect_error(
      call_validator(inputs),
      "Old paper and real portfolios must have the same last date.",
      fixed = TRUE
    )

    inputs <- build_valid_inputs()
    inputs$old_portfolio$paper$portfolio$date <- as.Date("2026-04-23")
    inputs$old_portfolio$real$portfolio$date <- as.Date("2026-04-23")

    testthat::expect_error(
      call_validator(inputs),
      "`old_portfolio` last date must be strictly before `min(current_dates)`.",
      fixed = TRUE
    )
  })

  testthat::test_that("rebal_weights required columns and values are enforced", {
    inputs <- build_valid_inputs()
    inputs$rebal_weights$weights <- NULL

    testthat::expect_error(
      call_validator(inputs),
      "`rebal_weights` is missing columns: weights.",
      fixed = TRUE
    )

    inputs <- build_valid_inputs()
    inputs$rebal_weights$weights <- c(0.60, 0.60)

    testthat::expect_error(
      call_validator(inputs),
      "Some rebalance weights do not sum to 1.",
      fixed = TRUE
    )

    inputs <- build_valid_inputs()
    inputs$rebal_weights$weights <- c(1.10, -0.10)

    testthat::expect_error(
      call_validator(inputs),
      "`rebal_weights$weights` contains negative weights.",
      fixed = TRUE
    )

    inputs <- build_valid_inputs()
    inputs$rebal_weights <- rbind(inputs$rebal_weights, inputs$rebal_weights[1, ])

    testthat::expect_error(
      call_validator(inputs),
      "`rebal_weights` has duplicated date + id + cvm_code_type rows.",
      fixed = TRUE
    )
  })

  testthat::test_that("comdinheiro_data required columns and values are enforced", {
    inputs <- build_valid_inputs()
    inputs$comdinheiro_data$price <- NULL

    testthat::expect_error(
      call_validator(inputs),
      "`comdinheiro_data` is missing columns: price.",
      fixed = TRUE
    )

    inputs <- build_valid_inputs()
    inputs$comdinheiro_data$price[1] <- 0

    testthat::expect_error(
      call_validator(inputs),
      "`comdinheiro_data$price` cannot contain non-positive prices.",
      fixed = TRUE
    )

    inputs <- build_valid_inputs()
    inputs$comdinheiro_data <- rbind(inputs$comdinheiro_data, inputs$comdinheiro_data[1, ])

    testthat::expect_error(
      call_validator(inputs),
      "`comdinheiro_data` has duplicated date + cvm_code_type rows.",
      fixed = TRUE
    )

    inputs <- build_valid_inputs()
    inputs$comdinheiro_data <- inputs$comdinheiro_data[
      inputs$comdinheiro_data$date != as.Date("2026-04-24"),
    ]

    testthat::expect_error(
      call_validator(inputs),
      "`comdinheiro_data` is missing required `current_dates`: 2026-04-24.",
      fixed = TRUE
    )
  })

  testthat::test_that("continuity against next available market date is enforced", {
    inputs <- build_valid_inputs()
    inputs$current_dates <- as.Date("2026-04-24")
    inputs$rebal_weights$date <- as.Date("2026-04-24")
    inputs$transaction_costs_bps$date <- as.Date("2026-04-24")

    testthat::expect_error(
      call_validator(inputs),
      "`current_dates` must start exactly on the first available market date after `old_portfolio` last date.",
      fixed = TRUE
    )
  })

  testthat::test_that("continuity uses next available market date when previous calendar day is a holiday", {
    inputs <- build_valid_inputs()

    # Old portfolio ends on 2026-04-20.
    inputs$old_portfolio$paper$portfolio$date <- as.Date("2026-04-20")
    inputs$old_portfolio$real$portfolio$date <- as.Date("2026-04-20")

    # 2026-04-21 is a holiday and is intentionally absent from comdinheiro_data.
    # The next available market date is 2026-04-22.
    inputs$current_dates <- as.Date(c("2026-04-22", "2026-04-23"))

    inputs$comdinheiro_data <- data.frame(
      date = rep(as.Date(c("2026-04-20", "2026-04-22", "2026-04-23")), each = 2),
      legacy_ticker = rep(c("AAA3", "BBB4"), times = 3),
      cvm_code_type = rep(c("AAA3", "BBB4"), times = 3),
      ret_1d = c(0, 0, 1, -1, 0.5, 0.2),
      price = c(10, 20, 10.1, 19.8, 10.15, 19.84),
      proventos = 0,
      proventos_date = as.Date(NA),
      event_factor = 1,
      n_shares = c(1000000, 2000000, 1000000, 2000000, 1000000, 2000000),
      stringsAsFactors = FALSE
    )

    inputs$rebal_weights <- data.frame(
      date = as.Date("2026-04-22"),
      id = "strategy_FIA",
      legacy_ticker = c("AAA3", "BBB4"),
      cvm_code_type = c("AAA3", "BBB4"),
      weights = c(0.50, 0.50),
      stringsAsFactors = FALSE
    )

    inputs$brokerage_data <- data.frame(
      date = as.Date("2026-04-22"),
      fund_name = "sicoob_acoes",
      legacy_ticker = "AAA3",
      cvm_code_type = "AAA3",
      side = "buy",
      amount = 10,
      price = 10.1,
      traded_volume = 101,
      brokerage_fee_estimated = 0.5,
      stringsAsFactors = FALSE
    )

    inputs$transaction_costs_bps <- data.frame(
      date = inputs$current_dates,
      id = "strategy_FIA",
      transaction_cost_bps = c(1, 0),
      stringsAsFactors = FALSE
    )

    out <- call_validator(inputs)

    testthat::expect_equal(out$old_port_last_date, as.Date("2026-04-20"))
    testthat::expect_equal(out$current_dates, as.Date(c("2026-04-22", "2026-04-23")))
  })

  testthat::test_that("brokerage_data validations are enforced", {
    inputs <- build_valid_inputs()
    inputs$brokerage_data$side <- "hold"

    testthat::expect_error(
      call_validator(inputs),
      "`trade_data$side` must contain only 'buy' or 'sell'.",
      fixed = TRUE
    )

    inputs <- build_valid_inputs()
    inputs$brokerage_data$amount <- -1

    testthat::expect_error(
      call_validator(inputs),
      "`trade_data$amount` must be non-negative and non-missing.",
      fixed = TRUE
    )

    inputs <- build_valid_inputs()
    inputs$brokerage_data$price <- 0

    testthat::expect_error(
      call_validator(inputs),
      "`trade_data$price` must be positive and non-missing.",
      fixed = TRUE
    )

    inputs <- build_valid_inputs()
    inputs$brokerage_data$brokerage_fee_estimated <- -0.01

    testthat::expect_error(
      call_validator(inputs),
      "`trade_data$brokerage_fee_estimated` must be non-negative and non-missing.",
      fixed = TRUE
    )
  })

  testthat::test_that("split_inplit_data validations are enforced", {
    inputs <- build_valid_inputs()
    inputs$split_inplit_data <- data.frame(
      date = as.Date("2026-04-23"),
      cvm_code_type = "AAA3",
      stringsAsFactors = FALSE
    )

    testthat::expect_error(
      call_validator(inputs),
      "`split_inplit_data` is missing columns: split_factor.",
      fixed = TRUE
    )

    inputs <- build_valid_inputs()
    inputs$split_inplit_data <- data.frame(
      date = as.Date("2026-04-23"),
      cvm_code_type = "AAA3",
      split_factor = 0,
      stringsAsFactors = FALSE
    )

    testthat::expect_error(
      call_validator(inputs),
      "`split_inplit_data$split_factor` must be positive, finite, and non-missing.",
      fixed = TRUE
    )

    inputs <- build_valid_inputs()
    inputs$split_inplit_data <- data.frame(
      date = as.Date(c("2026-04-23", "2026-04-23")),
      cvm_code_type = c("AAA3", "AAA3"),
      split_factor = c(0.5, 0.5),
      stringsAsFactors = FALSE
    )

    testthat::expect_error(
      call_validator(inputs),
      "`split_inplit_data` has duplicated date + cvm_code_type rows.",
      fixed = TRUE
    )
  })

  testthat::test_that("transaction_costs_bps validations are enforced", {
    inputs <- build_valid_inputs()
    inputs$transaction_costs_bps$transaction_cost_bps <- NULL

    testthat::expect_error(
      call_validator(inputs),
      "`transaction_costs_bps` is missing columns: transaction_cost_bps.",
      fixed = TRUE
    )

    inputs <- build_valid_inputs()
    inputs$transaction_costs_bps$transaction_cost_bps[1] <- -1

    testthat::expect_error(
      call_validator(inputs),
      "`transaction_costs_bps` cannot contain negative costs.",
      fixed = TRUE
    )

    inputs <- build_valid_inputs()
    inputs$transaction_costs_bps$transaction_cost_bps[1] <- NA_real_

    testthat::expect_error(
      call_validator(inputs),
      "`transaction_costs_bps$transaction_cost_bps` contains NA values after coercion.",
      fixed = TRUE
    )
  })

  testthat::test_that("fund fee validations are enforced", {
    inputs <- build_valid_inputs()
    inputs$fund_fees_bps <- NA_real_

    testthat::expect_error(
      call_validator(inputs),
      "`fund_fees_bps` must be a single numeric value.",
      fixed = TRUE
    )

    inputs <- build_valid_inputs()
    inputs$fund_fees_bps <- -1

    testthat::expect_error(
      call_validator(inputs),
      "`fund_fees_bps` cannot be negative.",
      fixed = TRUE
    )
  })

  invisible(TRUE)
}

run_split_candidate_helpers_test <- function() {

  build_split_test_data <- function() {
    data.frame(
      date = rep(as.Date(c("2026-04-22", "2026-04-23")), each = 6),
      legacy_ticker = rep(
        c(
          "NO_SPLIT3",
          "HIGH3",
          "MEDIUM3",
          "PRICEONLY3",
          "NONROUND3",
          "WEAK3"
        ),
        times = 2
      ),
      cvm_code_type = rep(
        c(
          "NO_SPLIT3",
          "HIGH3",
          "MEDIUM3",
          "PRICEONLY3",
          "NONROUND3",
          "WEAK3"
        ),
        times = 2
      ),
      price = c(
        # old date
        100, 100, 100, 100, 100, 100,
        # current date
        101, 50, 50, 50, 40, 100
      ),
      ret_1d = c(
        # old date
        0, 0, 0, 0, 0, 0,
        # current date
        0.01, 0, 0, 0, 0, 0
      ),
      proventos = 0,
      event_factor = c(
        # old date
        1, 1, 1, 1, 1, 1,
        # current date
        1, 0.5, 0.5, 1, 1, 0.5
      ),
      n_shares = c(
        # old date
        1000, 1000, 1000, 1000, 1000, 1000,
        # current date
        1000, 2000, 1000, 1000, 1000, 2000
      ),
      stringsAsFactors = FALSE
    )
  }

  call_detect <- function(
    data,
    current_dates = as.Date("2026-04-23"),
    old_port_last_date = as.Date("2026-04-22"),
    split_warning_threshold = 0.25,
    split_rounding_tolerance = 0.08
  ) {
    detect_candidate_splits(
      comdinheiro_data = data,
      current_dates = current_dates,
      old_port_last_date = old_port_last_date,
      split_warning_threshold = split_warning_threshold,
      split_rounding_tolerance = split_rounding_tolerance
    )
  }

  testthat::test_that("detect_candidate_splits returns the expected columns", {
    out <- call_detect(build_split_test_data())

    expected_cols <- c(
      "date",
      "legacy_ticker",
      "cvm_code_type",
      "warning_level",
      "candidate_confidence",
      "supporting_flags",
      "share_factor_alert",
      "event_factor_alert",
      "price_factor_alert",
      "price_lag",
      "price",
      "ret_1d",
      "proventos",
      "n_shares_lag",
      "n_shares",
      "event_factor_lag",
      "event_factor",
      "share_position_factor",
      "event_position_factor",
      "price_implied_position_factor",
      "price_implied_split_factor",
      "inferred_position_factor",
      "inferred_split_factor"
    )

    testthat::expect_true(all(expected_cols %in% names(out)))
  })

  testthat::test_that("detect_candidate_splits excludes assets with no warning", {
    out <- call_detect(build_split_test_data())

    testthat::expect_false("NO_SPLIT3" %in% out$cvm_code_type)
  })

  testthat::test_that("detect_candidate_splits identifies high confidence split candidates", {
    out <- call_detect(build_split_test_data())

    row <- out[out$cvm_code_type == "HIGH3", ]

    testthat::expect_equal(nrow(row), 1L)
    testthat::expect_equal(row$warning_level, "high")
    testthat::expect_equal(row$candidate_confidence, "high")
    testthat::expect_equal(row$supporting_flags, 2L)
    testthat::expect_true(row$share_factor_alert)
    testthat::expect_true(row$event_factor_alert)
    testthat::expect_true(row$price_factor_alert)
    testthat::expect_equal(row$inferred_position_factor, 2)
    testthat::expect_equal(row$inferred_split_factor, 0.5)
  })

  testthat::test_that("detect_candidate_splits identifies medium confidence split candidates", {
    out <- call_detect(build_split_test_data())

    row <- out[out$cvm_code_type == "MEDIUM3", ]

    testthat::expect_equal(nrow(row), 1L)
    testthat::expect_equal(row$warning_level, "medium")
    testthat::expect_equal(row$candidate_confidence, "medium")
    testthat::expect_equal(row$supporting_flags, 1L)
    testthat::expect_false(row$share_factor_alert)
    testthat::expect_true(row$event_factor_alert)
    testthat::expect_true(row$price_factor_alert)
    testthat::expect_equal(row$inferred_position_factor, 2)
    testthat::expect_equal(row$inferred_split_factor, 0.5)
  })

  testthat::test_that("detect_candidate_splits identifies price-only candidates", {
    out <- call_detect(build_split_test_data())

    row <- out[out$cvm_code_type == "PRICEONLY3", ]

    testthat::expect_equal(nrow(row), 1L)
    testthat::expect_equal(row$warning_level, "medium")
    testthat::expect_equal(row$candidate_confidence, "price_only")
    testthat::expect_equal(row$supporting_flags, 0L)
    testthat::expect_false(row$share_factor_alert)
    testthat::expect_false(row$event_factor_alert)
    testthat::expect_true(row$price_factor_alert)
    testthat::expect_equal(row$inferred_position_factor, 2)
    testthat::expect_equal(row$inferred_split_factor, 0.5)
  })

  testthat::test_that("detect_candidate_splits identifies non-round price breaks", {
    out <- call_detect(build_split_test_data())

    row <- out[out$cvm_code_type == "NONROUND3", ]

    testthat::expect_equal(nrow(row), 1L)
    testthat::expect_equal(row$warning_level, "medium_data_quality")
    testthat::expect_equal(row$candidate_confidence, "price_break_non_round")
    testthat::expect_true(row$price_factor_alert)
    testthat::expect_true(is.na(row$inferred_position_factor))
    testthat::expect_true(is.na(row$inferred_split_factor))

    # The non-round nature is indirectly confirmed by the classification
    # and by the fact that inferred factors remain NA.
    testthat::expect_equal(row$price_implied_position_factor, 2.5)
    testthat::expect_equal(row$price_implied_split_factor, 0.4)
  })

  testthat::test_that("detect_candidate_splits identifies weak non-price candidates", {
    out <- call_detect(build_split_test_data())

    row <- out[out$cvm_code_type == "WEAK3", ]

    testthat::expect_equal(nrow(row), 1L)
    testthat::expect_equal(row$warning_level, "low")
    testthat::expect_equal(row$candidate_confidence, "weak_non_price")
    testthat::expect_equal(row$supporting_flags, 2L)
    testthat::expect_true(row$share_factor_alert)
    testthat::expect_true(row$event_factor_alert)
    testthat::expect_false(row$price_factor_alert)
    testthat::expect_true(is.na(row$inferred_position_factor))
    testthat::expect_true(is.na(row$inferred_split_factor))
  })

  testthat::test_that("detect_candidate_splits returns only current_dates, not old diagnostic date", {
    out <- call_detect(build_split_test_data())

    testthat::expect_true(all(out$date == as.Date("2026-04-23")))
    testthat::expect_false(any(out$date == as.Date("2026-04-22")))
  })

  testthat::test_that("detect_candidate_splits returns empty data frame when no alerts are found", {
    data <- data.frame(
      date = rep(as.Date(c("2026-04-22", "2026-04-23")), each = 2),
      legacy_ticker = rep(c("AAA3", "BBB4"), times = 2),
      cvm_code_type = rep(c("AAA3", "BBB4"), times = 2),
      price = c(100, 200, 101, 202),
      ret_1d = c(0, 0, 0.01, 0.01),
      proventos = 0,
      event_factor = 1,
      n_shares = c(1000, 2000, 1000, 2000),
      stringsAsFactors = FALSE
    )

    out <- call_detect(data)

    testthat::expect_true(is.data.frame(out))
    testthat::expect_equal(nrow(out), 0L)
  })

  testthat::test_that("detect_candidate_splits respects split_warning_threshold", {
    data <- data.frame(
      date = rep(as.Date(c("2026-04-22", "2026-04-23")), each = 1),
      legacy_ticker = "AAA3",
      cvm_code_type = "AAA3",
      price = c(100, 80),
      ret_1d = c(0, 0),
      proventos = 0,
      event_factor = c(1, 1),
      n_shares = c(1000, 1000),
      stringsAsFactors = FALSE
    )

    out_high_threshold <- call_detect(
      data,
      split_warning_threshold = 0.30
    )

    out_low_threshold <- call_detect(
      data,
      split_warning_threshold = 0.20
    )

    testthat::expect_equal(nrow(out_high_threshold), 0L)
    testthat::expect_equal(nrow(out_low_threshold), 1L)
    testthat::expect_equal(out_low_threshold$candidate_confidence, "price_break_non_round")
  })

  testthat::test_that("detect_candidate_splits respects split_rounding_tolerance", {
    data <- data.frame(
      date = rep(as.Date(c("2026-04-22", "2026-04-23")), each = 1),
      legacy_ticker = "AAA3",
      cvm_code_type = "AAA3",
      price = c(100, 52),
      ret_1d = c(0, 0),
      proventos = 0,
      event_factor = c(1, 0.5),
      n_shares = c(1000, 2000),
      stringsAsFactors = FALSE
    )

    out_loose <- call_detect(
      data,
      split_rounding_tolerance = 0.08
    )

    out_strict <- call_detect(
      data,
      split_rounding_tolerance = 0.01
    )

    testthat::expect_equal(nrow(out_loose), 1L)
    testthat::expect_equal(out_loose$candidate_confidence, "high")
    testthat::expect_equal(out_loose$inferred_position_factor, 2)

    testthat::expect_equal(nrow(out_strict), 1L)
    testthat::expect_equal(out_strict$candidate_confidence, "price_break_non_round")
    testthat::expect_true(is.na(out_strict$inferred_position_factor))
  })

  testthat::test_that("detect_candidate_splits handles multiple current dates", {
    data <- data.frame(
      date = rep(as.Date(c("2026-04-22", "2026-04-23", "2026-04-24")), each = 1),
      legacy_ticker = "AAA3",
      cvm_code_type = "AAA3",
      price = c(100, 50, 25),
      ret_1d = c(0, 0, 0),
      proventos = 0,
      event_factor = c(1, 0.5, 0.25),
      n_shares = c(1000, 2000, 4000),
      stringsAsFactors = FALSE
    )

    out <- call_detect(
      data,
      current_dates = as.Date(c("2026-04-23", "2026-04-24")),
      old_port_last_date = as.Date("2026-04-22")
    )

    testthat::expect_equal(nrow(out), 2L)
    testthat::expect_equal(out$date, as.Date(c("2026-04-23", "2026-04-24")))
    testthat::expect_true(all(out$candidate_confidence == "high"))
    testthat::expect_true(all(out$inferred_split_factor == 0.5))
  })

  testthat::test_that("format_split_candidate_warnings returns character(0) for empty input", {
    out <- call_detect(build_split_test_data())
    empty_out <- out[0, ]

    formatted <- format_split_candidate_warnings(empty_out)

    testthat::expect_type(formatted, "character")
    testthat::expect_length(formatted, 0L)
  })

  testthat::test_that("format_split_candidate_warnings includes key diagnostic fields", {
    out <- call_detect(build_split_test_data())

    formatted <- format_split_candidate_warnings(out)

    testthat::expect_type(formatted, "character")
    testthat::expect_length(formatted, 1L)

    testthat::expect_true(grepl("level = high", formatted, fixed = TRUE))
    testthat::expect_true(grepl("confidence = high", formatted, fixed = TRUE))
    testthat::expect_true(grepl("date = 2026-04-23", formatted, fixed = TRUE))
    testthat::expect_true(grepl("legacy_ticker = HIGH3", formatted, fixed = TRUE))
    testthat::expect_true(grepl("cvm_code_type = HIGH3", formatted, fixed = TRUE))
    testthat::expect_true(grepl("inferred_split_factor = 0.5", formatted, fixed = TRUE))
    testthat::expect_true(grepl("price_implied_split_factor = 0.5", formatted, fixed = TRUE))
    testthat::expect_true(grepl("inferred_position_factor = 2", formatted, fixed = TRUE))
    testthat::expect_true(grepl("price_implied_position_factor = 2", formatted, fixed = TRUE))
    testthat::expect_true(grepl("event_position_factor = 2", formatted, fixed = TRUE))
    testthat::expect_true(grepl("share_position_factor = 2", formatted, fixed = TRUE))
    testthat::expect_true(grepl("supporting_flags = 2", formatted, fixed = TRUE))
    testthat::expect_true(grepl("flags = [price_return: TRUE", formatted, fixed = TRUE))
    testthat::expect_true(grepl("event_factor: TRUE", formatted, fixed = TRUE))
    testthat::expect_true(grepl("shares: TRUE", formatted, fixed = TRUE))
  })

  testthat::test_that("format_split_candidate_warnings orders warnings by rank, date, legacy_ticker, and cvm_code_type", {
    out <- call_detect(build_split_test_data())

    shuffled <- out[c(
      which(out$cvm_code_type == "WEAK3"),
      which(out$cvm_code_type == "PRICEONLY3"),
      which(out$cvm_code_type == "HIGH3"),
      which(out$cvm_code_type == "NONROUND3"),
      which(out$cvm_code_type == "MEDIUM3")
    ), ]

    formatted <- format_split_candidate_warnings(shuffled)

    lines <- strsplit(formatted, "\n", fixed = TRUE)[[1]]

    testthat::expect_equal(length(lines), nrow(shuffled))

    # Rank order should be:
    # high first, medium next, medium_data_quality next, low last.
    high_pos <- grep("cvm_code_type = HIGH3", lines, fixed = TRUE)
    medium_pos <- grep("cvm_code_type = MEDIUM3", lines, fixed = TRUE)
    price_only_pos <- grep("cvm_code_type = PRICEONLY3", lines, fixed = TRUE)
    nonround_pos <- grep("cvm_code_type = NONROUND3", lines, fixed = TRUE)
    weak_pos <- grep("cvm_code_type = WEAK3", lines, fixed = TRUE)

    testthat::expect_lt(high_pos, medium_pos)
    testthat::expect_lt(high_pos, price_only_pos)
    testthat::expect_lt(medium_pos, nonround_pos)
    testthat::expect_lt(price_only_pos, nonround_pos)
    testthat::expect_lt(nonround_pos, weak_pos)
  })

  testthat::test_that("format_split_candidate_warnings handles NA inferred factors for non-round and weak candidates", {
    out <- call_detect(build_split_test_data())

    subset_out <- out[out$cvm_code_type %in% c("NONROUND3", "WEAK3"), ]

    formatted <- format_split_candidate_warnings(subset_out)

    testthat::expect_true(grepl("cvm_code_type = NONROUND3", formatted, fixed = TRUE))
    testthat::expect_true(grepl("cvm_code_type = WEAK3", formatted, fixed = TRUE))
    testthat::expect_true(grepl("inferred_split_factor = NA", formatted, fixed = TRUE))
    testthat::expect_true(grepl("inferred_position_factor = NA", formatted, fixed = TRUE))
  })

  invisible(TRUE)
}

run_compute_paper_portfolio_step_test <- function() {

  build_valid_paper_step_inputs <- function() {
    list(
      current_date = as.Date("2026-04-23"),
      id = "strategy_FIA",
      paper_last_eop_weights = data.frame(
        cvm_code_type = c("AAA3", "BBB4"),
        eop_weights = c(0.60, 0.40),
        stringsAsFactors = FALSE
      ),
      paper_current_market_value = 1000,
      asset_ticker_lookup_today = data.frame(
        cvm_code_type = c("AAA3", "BBB4"),
        legacy_ticker = c("AAA3", "BBB4"),
        stringsAsFactors = FALSE
      ),
      prices_today = data.frame(
        cvm_code_type = c("AAA3", "BBB4"),
        ret_1d = c(0.01, -0.02),
        stringsAsFactors = FALSE
      ),
      target_today = data.frame(
        cvm_code_type = character(),
        weights = numeric(),
        stringsAsFactors = FALSE
      ),
      transaction_costs_bps = data.frame(
        date = as.Date("2026-04-23"),
        id = "strategy_FIA",
        transaction_cost_bps = 0,
        stringsAsFactors = FALSE
      ),
      daily_fee_return = 0,
      allow_missing_returns = TRUE,
      weight_tolerance = 1e-8
    )
  }

  call_step <- function(inputs) {
    do.call(compute_paper_portfolio_step, inputs)
  }

  testthat::test_that("valid no-rebalance paper step computes raw return, drift, and EOP weights", {
    inputs <- build_valid_paper_step_inputs()

    out <- call_step(inputs)

    expected_raw_return <- 0.60 * 0.01 + 0.40 * -0.02
    expected_denominator <- 0.60 * 1.01 + 0.40 * 0.98
    expected_drifted_weights <- c(
      0.60 * 1.01 / expected_denominator,
      0.40 * 0.98 / expected_denominator
    )

    testthat::expect_equal(out$paper_raw_ret, expected_raw_return, tolerance = 1e-12)
    testthat::expect_equal(out$paper_net_ret, expected_raw_return, tolerance = 1e-12)
    testthat::expect_equal(out$paper_turnover, 0, tolerance = 1e-12)
    testthat::expect_equal(out$paper_current_market_value, 1000 * (1 + expected_raw_return), tolerance = 1e-12)

    testthat::expect_equal(out$paper_portfolio$drifted_weights, expected_drifted_weights, tolerance = 1e-12)
    testthat::expect_equal(out$paper_portfolio$eop_weights, expected_drifted_weights, tolerance = 1e-12)
    testthat::expect_equal(sum(out$paper_portfolio$eop_weights), 1, tolerance = 1e-12)
  })

  testthat::test_that("valid rebalance computes EOP weights and turnover", {
    inputs <- build_valid_paper_step_inputs()
    inputs$target_today <- data.frame(
      cvm_code_type = c("AAA3", "BBB4"),
      weights = c(0.50, 0.50),
      stringsAsFactors = FALSE
    )

    out <- call_step(inputs)

    expected_denominator <- 0.60 * 1.01 + 0.40 * 0.98
    expected_drifted_weights <- c(
      0.60 * 1.01 / expected_denominator,
      0.40 * 0.98 / expected_denominator
    )
    expected_turnover <- sum(abs(c(0.50, 0.50) - expected_drifted_weights))

    testthat::expect_equal(out$paper_portfolio$eop_weights, c(0.50, 0.50), tolerance = 1e-12)
    testthat::expect_equal(out$paper_turnover, expected_turnover, tolerance = 1e-12)
    testthat::expect_equal(out$paper_last_eop_weights$eop_weights, c(0.50, 0.50), tolerance = 1e-12)
  })

  testthat::test_that("transaction costs and fees are applied to net return and market value", {
    inputs <- build_valid_paper_step_inputs()
    inputs$transaction_costs_bps$transaction_cost_bps <- 10
    inputs$daily_fee_return <- 5 / 10000

    out <- call_step(inputs)

    expected_raw_return <- 0.60 * 0.01 + 0.40 * -0.02
    expected_cost_return <- 10 / 10000
    expected_fee_return <- 5 / 10000

    expected_net_return <- (1 + expected_raw_return) *
      (1 - expected_cost_return) *
      (1 - expected_fee_return) - 1

    testthat::expect_equal(out$paper_cost_bps_today, 10)
    testthat::expect_equal(out$paper_transaction_cost_return, expected_cost_return)
    testthat::expect_equal(out$paper_net_ret, expected_net_return, tolerance = 1e-12)
    testthat::expect_equal(out$paper_current_market_value, 1000 * (1 + expected_net_return), tolerance = 1e-12)
  })

  testthat::test_that("transaction costs are correctly handled", {
    inputs <- build_valid_paper_step_inputs()
    inputs$transaction_costs_bps <- data.frame(
      id = c("strategy_FIA", "strategy_FIA"),
      date = as.Date(c("2026-04-22", "2026-04-23")),
      transaction_cost_bps = c(2, 5),
      stringsAsFactors = FALSE
    )

    out <- call_step(inputs)

    testthat::expect_equal(out$paper_cost_bps_today, 5)
    testthat::expect_equal(out$paper_transaction_cost_return, 5 / 10000)
  })

  testthat::test_that("duplicate transaction cost rows for a date are blocked", {
    inputs <- build_valid_paper_step_inputs()

    inputs$transaction_costs_bps <- data.frame(
      date = as.Date(c("2026-04-23", "2026-04-23")),
      id = c("strategy_FIA", "strategy_FIA"),
      transaction_cost_bps = c(2, 3),
      stringsAsFactors = FALSE
    )

    testthat::expect_error(
      call_step(inputs),
      "`transaction_costs_bps` must contain at most one row per date + id.",
      fixed = TRUE
    )
  })

  testthat::test_that("missing transaction cost for date defaults to zero", {
    inputs <- build_valid_paper_step_inputs()
    inputs$transaction_costs_bps <- data.frame(
      date = as.Date("2026-04-22"),
      id = "strategy_FIA",
      transaction_cost_bps = 10,
      stringsAsFactors = FALSE
    )

    out <- call_step(inputs)

    testthat::expect_equal(out$paper_cost_bps_today, 0)
    testthat::expect_equal(out$paper_transaction_cost_return, 0)
  })

  testthat::test_that("missing returns are preserved in output but treated as zero when allowed", {
    inputs <- build_valid_paper_step_inputs()
    inputs$prices_today$ret_1d[1] <- NA_real_

    testthat::expect_warning(
      out <- call_step(inputs),
      "NA returns found for paper BOP weights at date 2026-04-23.",
      fixed = TRUE
    )

    expected_raw_return <- 0.60 * 0 + 0.40 * -0.02
    expected_denominator <- 0.60 * 1 + 0.40 * 0.98

    testthat::expect_true(is.na(out$paper_portfolio$ret_1d[1]))
    testthat::expect_equal(out$paper_raw_ret, expected_raw_return, tolerance = 1e-12)
    testthat::expect_equal(sum(out$paper_portfolio$drifted_weights), 1, tolerance = 1e-12)
    testthat::expect_equal(
      out$paper_portfolio$drifted_weights,
      c(0.60 * 1 / expected_denominator, 0.40 * 0.98 / expected_denominator),
      tolerance = 1e-12
    )
  })

  testthat::test_that("missing returns are blocked when not allowed", {
    inputs <- build_valid_paper_step_inputs()
    inputs$prices_today$ret_1d[1] <- NA_real_
    inputs$allow_missing_returns <- FALSE

    testthat::expect_error(
      call_step(inputs),
      "NA returns found for paper BOP weights at date 2026-04-23.",
      fixed = TRUE
    )
  })

  testthat::test_that("invalid paper drift denominator is blocked", {
    inputs <- build_valid_paper_step_inputs()
    inputs$prices_today$ret_1d <- c(-1, -1)

    testthat::expect_error(
      call_step(inputs),
      "Invalid paper drift denominator at date 2026-04-23.",
      fixed = TRUE
    )
  })

  testthat::test_that("negative drifted weights are blocked", {
    inputs <- build_valid_paper_step_inputs()
    inputs$paper_last_eop_weights$eop_weights <- c(0.50, 0.50)
    inputs$prices_today$ret_1d <- c(-1.50, 1.00)

    testthat::expect_error(
      call_step(inputs),
      "Negative paper weights found after drift at date 2026-04-23.",
      fixed = TRUE
    )
  })

  testthat::test_that("target assets not in previous paper weights are added with zero BOP weight", {
    inputs <- build_valid_paper_step_inputs()

    inputs$target_today <- data.frame(
      cvm_code_type = c("AAA3", "BBB4", "CCC3"),
      weights = c(0.40, 0.40, 0.20),
      stringsAsFactors = FALSE
    )

    inputs$asset_ticker_lookup_today <- data.frame(
      cvm_code_type = c("AAA3", "BBB4", "CCC3"),
      legacy_ticker = c("AAA3", "BBB4", "CCC3"),
      stringsAsFactors = FALSE
    )

    inputs$prices_today <- data.frame(
      cvm_code_type = c("AAA3", "BBB4", "CCC3"),
      ret_1d = c(0.01, -0.02, 0.03),
      stringsAsFactors = FALSE
    )

    out <- call_step(inputs)

    ccc_row <- out$paper_portfolio[out$paper_portfolio$cvm_code_type == "CCC3", ]

    testthat::expect_equal(nrow(ccc_row), 1L)
    testthat::expect_equal(ccc_row$bop_weights, 0)
    testthat::expect_equal(ccc_row$eop_weights, 0.20)
  })

  testthat::test_that("missing price rows are blocked", {
    inputs <- build_valid_paper_step_inputs()
    inputs$prices_today <- inputs$prices_today[inputs$prices_today$cvm_code_type != "BBB4", ]

    testthat::expect_error(
      call_step(inputs),
      "Missing paper return rows at date 2026-04-23. Assets: BBB4.",
      fixed = TRUE
    )
  })

  testthat::test_that("missing ticker lookup rows are blocked", {
    inputs <- build_valid_paper_step_inputs()
    inputs$asset_ticker_lookup_today <- inputs$asset_ticker_lookup_today[
      inputs$asset_ticker_lookup_today$cvm_code_type != "BBB4",
    ]

    testthat::expect_error(
      call_step(inputs),
      "Missing ticker lookup rows at date 2026-04-23. Assets: BBB4.",
      fixed = TRUE
    )
  })

  testthat::test_that("duplicate input keys are blocked", {
    inputs <- build_valid_paper_step_inputs()
    inputs$paper_last_eop_weights <- rbind(inputs$paper_last_eop_weights, inputs$paper_last_eop_weights[1, ])

    testthat::expect_error(
      call_step(inputs),
      "`paper_last_eop_weights` has duplicated `cvm_code_type` rows.",
      fixed = TRUE
    )

    inputs <- build_valid_paper_step_inputs()
    inputs$prices_today <- rbind(inputs$prices_today, inputs$prices_today[1, ])

    testthat::expect_error(
      call_step(inputs),
      "`prices_today` has duplicated `cvm_code_type` rows.",
      fixed = TRUE
    )

    inputs <- build_valid_paper_step_inputs()
    inputs$asset_ticker_lookup_today <- rbind(
      inputs$asset_ticker_lookup_today,
      inputs$asset_ticker_lookup_today[1, ]
    )

    testthat::expect_error(
      call_step(inputs),
      "`asset_ticker_lookup_today` has duplicated `cvm_code_type` rows.",
      fixed = TRUE
    )

    inputs <- build_valid_paper_step_inputs()
    inputs$target_today <- data.frame(
      cvm_code_type = c("AAA3", "AAA3"),
      weights = c(0.50, 0.50),
      stringsAsFactors = FALSE
    )

    testthat::expect_error(
      call_step(inputs),
      "`target_today` has duplicated `cvm_code_type` rows.",
      fixed = TRUE
    )
  })

  testthat::test_that("invalid weight sums are blocked", {
    inputs <- build_valid_paper_step_inputs()
    inputs$paper_last_eop_weights$eop_weights <- c(0.70, 0.40)

    testthat::expect_error(
      call_step(inputs),
      "`paper_last_eop_weights$eop_weights` must sum to 1.",
      fixed = TRUE
    )

    inputs <- build_valid_paper_step_inputs()
    inputs$target_today <- data.frame(
      cvm_code_type = c("AAA3", "BBB4"),
      weights = c(0.70, 0.40),
      stringsAsFactors = FALSE
    )

    testthat::expect_error(
      call_step(inputs),
      "Paper EOP weights do not sum to 1 at date 2026-04-23.",
      fixed = TRUE
    )
  })

  testthat::test_that("negative weights and costs are blocked", {
    inputs <- build_valid_paper_step_inputs()
    inputs$paper_last_eop_weights$eop_weights <- c(1.10, -0.10)

    testthat::expect_error(
      call_step(inputs),
      "`paper_last_eop_weights$eop_weights` contains negative eop_weights",
      fixed = TRUE
    )

    inputs <- build_valid_paper_step_inputs()
    inputs$target_today <- data.frame(
      cvm_code_type = c("AAA3", "BBB4"),
      weights = c(1.10, -0.10),
      stringsAsFactors = FALSE
    )

    testthat::expect_error(
      call_step(inputs),
      "`target_today$weights` contains negative weights.",
      fixed = TRUE
    )

    inputs <- build_valid_paper_step_inputs()
    inputs$transaction_costs_bps$transaction_cost_bps <- -1

    testthat::expect_error(
      call_step(inputs),
      "`transaction_costs_bps$transaction_cost_bps` cannot contain negative values.",
      fixed = TRUE
    )
  })

  testthat::test_that("invalid scalar inputs are blocked", {
    inputs <- build_valid_paper_step_inputs()
    inputs$paper_current_market_value <- 0

    testthat::expect_error(
      call_step(inputs),
      "`paper_current_market_value` must be a positive finite scalar.",
      fixed = TRUE
    )

    inputs <- build_valid_paper_step_inputs()
    inputs$daily_fee_return <- -0.01

    testthat::expect_error(
      call_step(inputs),
      "`daily_fee_return` must be a single non-negative finite numeric value.",
      fixed = TRUE
    )
  })

  testthat::test_that("output table schemas are stable", {
    inputs <- build_valid_paper_step_inputs()
    out <- call_step(inputs)

    testthat::expect_true(all(c("date", "id", "raw_return") %in% names(out$tables$raw_return)))
    testthat::expect_true(all(c("date", "id", "net_return") %in% names(out$tables$net_return)))
    testthat::expect_true(all(c("date", "id", "eop_market_value") %in% names(out$tables$market_value)))
    testthat::expect_true(all(c("date", "id", "turnover") %in% names(out$tables$turnover)))
    testthat::expect_true(all(c("date", "id", "transaction_cost_bps", "transaction_cost_return") %in% names(out$tables$costs)))
    testthat::expect_true(all(c("date", "id", "fund_fees_bps", "daily_fee_return") %in% names(out$tables$fees)))
    testthat::expect_true(all(c("date", "id", "legacy_ticker", "cvm_code_type", "weights") %in% names(out$tables$bop_weights)))
    testthat::expect_true(all(c("date", "id", "legacy_ticker", "cvm_code_type", "weights") %in% names(out$tables$eop_weights)))

    testthat::expect_equal(sum(out$tables$bop_weights$weights), 1, tolerance = 1e-12)
    testthat::expect_equal(sum(out$tables$eop_weights$weights), 1, tolerance = 1e-12)
  })

  invisible(TRUE)
}

run_compute_real_portfolio_step_test <- function() {

  build_valid_real_step_inputs <- function() {
    list(
      current_date = as.Date("2026-04-23"),
      id = "strategy_FIA",
      fund_name = "sicoob_acoes",
      real_last_eop_positions = data.frame(
        cvm_code_type = c("AAA3", "BBB4"),
        eop_positions = c(100, 200),
        stringsAsFactors = FALSE
      ),
      asset_ticker_lookup_today = data.frame(
        cvm_code_type = c("AAA3", "BBB4"),
        legacy_ticker = c("AAA3", "BBB4"),
        stringsAsFactors = FALSE
      ),
      prices_yesterday = data.frame(
        cvm_code_type = c("AAA3", "BBB4"),
        price_lag = c(10, 20),
        stringsAsFactors = FALSE
      ),
      prices_today = data.frame(
        cvm_code_type = c("AAA3", "BBB4"),
        ret_1d = c(0.01, -0.01),
        price = c(10.1, 19.8),
        stringsAsFactors = FALSE
      ),
      proventos_today = data.frame(
        cvm_code_type = character(),
        proventos = numeric(),
        stringsAsFactors = FALSE
      ),
      splits_today = data.frame(
        date = as.Date(character()),
        legacy_ticker = character(),
        cvm_code_type = character(),
        split_factor = numeric(),
        position_factor = numeric(),
        stringsAsFactors = FALSE
      ),
      trades_today = data.frame(
        cvm_code_type = character(),
        signed_position = numeric(),
        signed_traded_volume = numeric(),
        brokerage_fee_estimated = numeric(),
        price = numeric(),
        stringsAsFactors = FALSE
      ),
      daily_fee_return = 0,
      position_tolerance = 1e-8,
      weight_tolerance = 1e-8
    )
  }

  call_step <- function(inputs) {
    do.call(compute_real_portfolio_step, inputs)
  }

  testthat::test_that("valid no-trade no-split step computes real accounting returns and weights", {
    inputs <- build_valid_real_step_inputs()
    out <- call_step(inputs)

    bop_mv <- 100 * 10 + 200 * 20
    eop_mv <- 100 * 10.1 + 200 * 19.8
    expected_raw_ret <- eop_mv / bop_mv - 1

    testthat::expect_equal(out$real_market_value_last_close, bop_mv, tolerance = 1e-12)
    testthat::expect_equal(out$real_eop_market_value, eop_mv, tolerance = 1e-12)
    testthat::expect_equal(out$real_raw_ret, expected_raw_ret, tolerance = 1e-12)
    testthat::expect_equal(out$real_net_ret, expected_raw_ret, tolerance = 1e-12)
    testthat::expect_equal(out$real_turnover, 0, tolerance = 1e-12)

    testthat::expect_equal(sum(out$real_portfolio$bop_weights), 1, tolerance = 1e-12)
    testthat::expect_equal(sum(out$real_portfolio$eop_weights), 1, tolerance = 1e-12)
    testthat::expect_equal(out$real_last_eop_positions$eop_positions, c(100, 200))
  })

  testthat::test_that("dividends enter raw return through dividends_received", {
    inputs <- build_valid_real_step_inputs()
    inputs$proventos_today <- data.frame(
      cvm_code_type = "AAA3",
      proventos = 1,
      stringsAsFactors = FALSE
    )

    out <- call_step(inputs)

    bop_mv <- 100 * 10 + 200 * 20
    eop_mv <- 100 * 10.1 + 200 * 19.8
    dividends <- 100 * 1
    expected_raw_ret <- (eop_mv + dividends) / bop_mv - 1

    testthat::expect_equal(out$real_dividends_received, dividends, tolerance = 1e-12)
    testthat::expect_equal(out$real_raw_ret, expected_raw_ret, tolerance = 1e-12)
  })

  testthat::test_that("splits adjust BOP positions by position_factor", {
    inputs <- build_valid_real_step_inputs()

    inputs$splits_today <- data.frame(
      date = as.Date("2026-04-23"),
      legacy_ticker = "AAA3",
      cvm_code_type = "AAA3",
      split_factor = 0.5,
      position_factor = 2,
      stringsAsFactors = FALSE
    )

    inputs$prices_today$price[inputs$prices_today$cvm_code_type == "AAA3"] <- 5.05

    out <- call_step(inputs)

    row <- out$real_portfolio[out$real_portfolio$cvm_code_type == "AAA3", ]

    testthat::expect_equal(row$bop_positions_before_split, 100)
    testthat::expect_equal(row$position_factor, 2)
    testthat::expect_equal(row$bop_positions, 200)
    testthat::expect_equal(row$eop_positions, 200)

    testthat::expect_equal(nrow(out$tables$splits), 1L)
    testthat::expect_equal(out$tables$splits$split_factor, 0.5)
    testthat::expect_equal(out$tables$splits$position_factor, 2)
  })

  testthat::test_that("inconsistent split_factor and position_factor are blocked", {
    inputs <- build_valid_real_step_inputs()

    inputs$splits_today <- data.frame(
      date = as.Date("2026-04-23"),
      legacy_ticker = "AAA3",
      cvm_code_type = "AAA3",
      split_factor = 0.5,
      position_factor = 3,
      stringsAsFactors = FALSE
    )

    testthat::expect_error(
      call_step(inputs),
      "`splits_today$position_factor` must equal `1 / splits_today$split_factor` within tolerance.",
      fixed = TRUE
    )
  })

  testthat::test_that("buy trades increase positions, cash-flow-adjusted return, costs, and turnover", {
    inputs <- build_valid_real_step_inputs()

    inputs$trades_today <- data.frame(
      cvm_code_type = "AAA3",
      signed_position = 10,
      signed_traded_volume = 101,
      brokerage_fee_estimated = 1,
      price = 10.1,
      stringsAsFactors = FALSE
    )

    out <- call_step(inputs)

    bop_mv <- 100 * 10 + 200 * 20
    eop_mv <- 110 * 10.1 + 200 * 19.8
    expected_raw_ret <- (eop_mv - 101) / bop_mv - 1
    expected_brokerage_cost_return <- 1 / bop_mv
    expected_net_ret <- (1 + expected_raw_ret) * (1 - expected_brokerage_cost_return) - 1

    testthat::expect_equal(out$real_last_eop_positions$eop_positions, c(110, 200))
    testthat::expect_equal(out$real_net_traded_volume, 101)
    testthat::expect_equal(out$real_brokerage_fee_today, 1)
    testthat::expect_equal(out$real_brokerage_cost_return, expected_brokerage_cost_return)
    testthat::expect_equal(out$real_raw_ret, expected_raw_ret, tolerance = 1e-12)
    testthat::expect_equal(out$real_net_ret, expected_net_ret, tolerance = 1e-12)
    testthat::expect_equal(out$real_turnover, 101 / bop_mv, tolerance = 1e-12)
  })

  testthat::test_that("sell trades decrease positions and use signed cash-flow adjustment", {
    inputs <- build_valid_real_step_inputs()

    inputs$trades_today <- data.frame(
      cvm_code_type = "BBB4",
      signed_position = -50,
      signed_traded_volume = -990,
      brokerage_fee_estimated = 2,
      price = 19.8,
      stringsAsFactors = FALSE
    )

    out <- call_step(inputs)

    bop_mv <- 100 * 10 + 200 * 20
    eop_mv <- 100 * 10.1 + 150 * 19.8
    expected_raw_ret <- (eop_mv - (-990)) / bop_mv - 1

    testthat::expect_equal(out$real_last_eop_positions$eop_positions, c(100, 150))
    testthat::expect_equal(out$real_net_traded_volume, -990)
    testthat::expect_equal(out$real_raw_ret, expected_raw_ret, tolerance = 1e-12)
    testthat::expect_equal(out$real_turnover, 990 / bop_mv, tolerance = 1e-12)
  })

  testthat::test_that("multiple trades in same asset are netted and weighted average price is computed", {
    inputs <- build_valid_real_step_inputs()

    inputs$trades_today <- data.frame(
      cvm_code_type = c("AAA3", "AAA3"),
      signed_position = c(10, 20),
      signed_traded_volume = c(101, 204),
      brokerage_fee_estimated = c(1, 2),
      price = c(10.1, 10.2),
      stringsAsFactors = FALSE
    )

    out <- call_step(inputs)

    trade_row <- out$tables$trades[out$tables$trades$cvm_code_type == "AAA3", ]

    testthat::expect_equal(trade_row$net_trade, 30)
    testthat::expect_equal(trade_row$net_traded_volume, 305)
    testthat::expect_equal(trade_row$brokerage_fee_estimated, 3)
    testthat::expect_equal(trade_row$avg_trade_price, stats::weighted.mean(c(10.1, 10.2), w = c(10, 20)))
  })

  testthat::test_that("daily fee is applied to net return", {
    inputs <- build_valid_real_step_inputs()
    inputs$daily_fee_return <- 0.001

    out <- call_step(inputs)

    bop_mv <- 100 * 10 + 200 * 20
    eop_mv <- 100 * 10.1 + 200 * 19.8
    expected_raw_ret <- eop_mv / bop_mv - 1
    expected_net_ret <- (1 + expected_raw_ret) * (1 - 0.001) - 1

    testthat::expect_equal(out$real_net_ret, expected_net_ret, tolerance = 1e-12)
    testthat::expect_equal(out$tables$fees$daily_fee_return, 0.001)
  })

  testthat::test_that("missing current prices are blocked", {
    inputs <- build_valid_real_step_inputs()
    inputs$prices_today <- inputs$prices_today[inputs$prices_today$cvm_code_type != "BBB4", ]

    testthat::expect_error(
      call_step(inputs),
      "Missing current price rows at date 2026-04-23. Assets: BBB4.",
      fixed = TRUE
    )
  })

  testthat::test_that("missing previous prices for nonzero positions are blocked", {
    inputs <- build_valid_real_step_inputs()
    inputs$prices_yesterday <- inputs$prices_yesterday[inputs$prices_yesterday$cvm_code_type != "BBB4", ]

    testthat::expect_error(
      call_step(inputs),
      "Missing previous prices for real positions at date 2026-04-23. Assets: BBB4.",
      fixed = TRUE
    )
  })

  testthat::test_that("missing previous prices for zero-position trade-only assets are allowed", {
    inputs <- build_valid_real_step_inputs()

    inputs$real_last_eop_positions <- data.frame(
      cvm_code_type = c("AAA3", "BBB4"),
      eop_positions = c(100, 0),
      stringsAsFactors = FALSE
    )

    inputs$prices_yesterday <- data.frame(
      cvm_code_type = "AAA3",
      price_lag = 10,
      stringsAsFactors = FALSE
    )

    inputs$trades_today <- data.frame(
      cvm_code_type = "BBB4",
      signed_position = 10,
      signed_traded_volume = 198,
      brokerage_fee_estimated = 1,
      price = 19.8,
      stringsAsFactors = FALSE
    )

    out <- call_step(inputs)

    bbb_row <- out$real_portfolio[out$real_portfolio$cvm_code_type == "BBB4", ]

    testthat::expect_equal(bbb_row$bop_positions_before_split, 0)
    testthat::expect_equal(bbb_row$price_last_close, NA_real_)
    testthat::expect_equal(bbb_row$market_value_last_close, NA_real_)
    testthat::expect_equal(bbb_row$eop_positions, 10)
  })

  testthat::test_that("missing ticker lookup rows are blocked", {
    inputs <- build_valid_real_step_inputs()
    inputs$asset_ticker_lookup_today <- inputs$asset_ticker_lookup_today[
      inputs$asset_ticker_lookup_today$cvm_code_type != "BBB4",
    ]

    testthat::expect_error(
      call_step(inputs),
      "Missing ticker lookup rows at date 2026-04-23. Assets: BBB4.",
      fixed = TRUE
    )
  })

  testthat::test_that("duplicate key inputs are blocked", {
    inputs <- build_valid_real_step_inputs()
    inputs$real_last_eop_positions <- rbind(inputs$real_last_eop_positions, inputs$real_last_eop_positions[1, ])

    testthat::expect_error(
      call_step(inputs),
      "`real_last_eop_positions` has duplicated `cvm_code_type` rows.",
      fixed = TRUE
    )

    inputs <- build_valid_real_step_inputs()
    inputs$prices_today <- rbind(inputs$prices_today, inputs$prices_today[1, ])

    testthat::expect_error(
      call_step(inputs),
      "`prices_today` has duplicated `cvm_code_type` rows.",
      fixed = TRUE
    )

    inputs <- build_valid_real_step_inputs()
    inputs$prices_yesterday <- rbind(inputs$prices_yesterday, inputs$prices_yesterday[1, ])

    testthat::expect_error(
      call_step(inputs),
      "`prices_yesterday` has duplicated `cvm_code_type` rows.",
      fixed = TRUE
    )

    inputs <- build_valid_real_step_inputs()
    inputs$splits_today <- data.frame(
      date = as.Date(c("2026-04-23", "2026-04-23")),
      legacy_ticker = c("AAA3", "AAA3"),
      cvm_code_type = c("AAA3", "AAA3"),
      split_factor = c(0.5, 0.5),
      position_factor = c(2, 2),
      stringsAsFactors = FALSE
    )

    testthat::expect_error(
      call_step(inputs),
      "`splits_today` has duplicated `cvm_code_type` rows.",
      fixed = TRUE
    )
  })

  testthat::test_that("invalid prices, positions, trades, and fees are blocked", {
    inputs <- build_valid_real_step_inputs()
    inputs$real_last_eop_positions$eop_positions[1] <- -1

    testthat::expect_error(
      call_step(inputs),
      "`real_last_eop_positions$eop_positions` contains negative positions.",
      fixed = TRUE
    )

    inputs <- build_valid_real_step_inputs()
    inputs$prices_today$price[1] <- 0

    testthat::expect_error(
      call_step(inputs),
      "`prices_today$price` must be positive, finite, and non-missing.",
      fixed = TRUE
    )

    inputs <- build_valid_real_step_inputs()
    inputs$trades_today <- data.frame(
      cvm_code_type = "AAA3",
      signed_position = 10,
      signed_traded_volume = 101,
      brokerage_fee_estimated = -1,
      price = 10.1,
      stringsAsFactors = FALSE
    )

    testthat::expect_error(
      call_step(inputs),
      "`trades_today$brokerage_fee_estimated` must be non-negative, finite, and non-missing.",
      fixed = TRUE
    )

    inputs <- build_valid_real_step_inputs()
    inputs$daily_fee_return <- -0.01

    testthat::expect_error(
      call_step(inputs),
      "`daily_fee_return` must be a single non-negative finite numeric value.",
      fixed = TRUE
    )
  })

  testthat::test_that("selling more than current position is blocked by negative EOP positions", {
    inputs <- build_valid_real_step_inputs()

    inputs$trades_today <- data.frame(
      cvm_code_type = "AAA3",
      signed_position = -101,
      signed_traded_volume = -1020.1,
      brokerage_fee_estimated = 1,
      price = 10.1,
      stringsAsFactors = FALSE
    )

    testthat::expect_error(
      call_step(inputs),
      "Negative EOP positions found at date 2026-04-23. Assets: AAA3.",
      fixed = TRUE
    )
  })

  testthat::test_that("invalid BOP and EOP market values are blocked", {
    inputs <- build_valid_real_step_inputs()
    inputs$real_last_eop_positions$eop_positions <- c(0, 0)

    testthat::expect_error(
      call_step(inputs),
      "Invalid real BOP market value at date 2026-04-23.",
      fixed = TRUE
    )

    inputs <- build_valid_real_step_inputs()
    inputs$trades_today <- data.frame(
      cvm_code_type = c("AAA3", "BBB4"),
      signed_position = c(-100, -200),
      signed_traded_volume = c(-1010, -3960),
      brokerage_fee_estimated = c(1, 1),
      price = c(10.1, 19.8),
      stringsAsFactors = FALSE
    )

    testthat::expect_error(
      call_step(inputs),
      "Invalid real EOP market value at date 2026-04-23.",
      fixed = TRUE
    )
  })

  testthat::test_that("output table schemas are stable", {
    inputs <- build_valid_real_step_inputs()
    out <- call_step(inputs)

    testthat::expect_true(all(c("date", "id", "fund_name", "raw_return") %in% names(out$tables$raw_return)))
    testthat::expect_true(all(c("date", "id", "fund_name", "net_return") %in% names(out$tables$net_return)))
    testthat::expect_true(all(c("date", "id", "fund_name", "market_value") %in% names(out$tables$market_value)))
    testthat::expect_true(all(c("date", "id", "fund_name", "turnover") %in% names(out$tables$turnover)))
    testthat::expect_true(all(c("date", "id", "fund_name", "brokerage_fee", "brokerage_cost_return") %in% names(out$tables$costs)))
    testthat::expect_true(all(c("date", "id", "fund_name", "daily_fee_return") %in% names(out$tables$fees)))
    testthat::expect_true(all(c("date", "id", "fund_name", "legacy_ticker", "cvm_code_type", "positions", "price_last_close", "market_value", "weights") %in% names(out$tables$bop_positions)))
    testthat::expect_true(all(c("date", "id", "fund_name", "legacy_ticker", "cvm_code_type", "positions", "price", "market_value", "weights") %in% names(out$tables$eop_positions)))

    testthat::expect_equal(sum(out$tables$bop_positions$weights), 1, tolerance = 1e-12)
    testthat::expect_equal(sum(out$tables$eop_positions$weights), 1, tolerance = 1e-12)
  })

  invisible(TRUE)
}

run_compute_real_portfolio_step_fabricated_trades_test <- function() {

  build_valid_synthetic_step_inputs <- function() {
    current_date <- as.Date("2026-04-23")

    real_last_eop_positions <- data.frame(
      cvm_code_type = c("AAA3", "BBB4"),
      eop_positions = c(100, 200),
      stringsAsFactors = FALSE
    )

    asset_ticker_lookup_today <- data.frame(
      cvm_code_type = c("AAA3", "BBB4", "CCC3", "BOVA11"),
      legacy_ticker = c("AAA3", "BBB4", "CCC3", "BOVA11"),
      stringsAsFactors = FALSE
    )

    prices_yesterday <- data.frame(
      cvm_code_type = c("AAA3", "BBB4", "CCC3", "BOVA11"),
      price_lag = c(10, 20, 5, 100),
      stringsAsFactors = FALSE
    )

    prices_today <- data.frame(
      date = current_date,
      legacy_ticker = c("AAA3", "BBB4", "CCC3", "BOVA11"),
      cvm_code_type = c("AAA3", "BBB4", "CCC3", "BOVA11"),
      ret_1d = c(0.01, -0.01, 0.02, 0),
      price = c(10, 20, 5, 100),
      stringsAsFactors = FALSE
    )

    proventos_today <- data.frame(
      cvm_code_type = character(),
      proventos = numeric(),
      stringsAsFactors = FALSE
    )

    splits_today <- data.frame(
      date = as.Date(character()),
      legacy_ticker = character(),
      cvm_code_type = character(),
      split_factor = numeric(),
      position_factor = numeric(),
      stringsAsFactors = FALSE
    )

    trades_today <- data.frame(
      cvm_code_type = character(),
      signed_position = numeric(),
      signed_traded_volume = numeric(),
      brokerage_fee_estimated = numeric(),
      price = numeric(),
      stringsAsFactors = FALSE
    )

    target_today <- data.frame(
      cvm_code_type = c("AAA3", "CCC3", "BOVA11"),
      legacy_ticker = c("AAA3", "CCC3", "BOVA11"),
      weights = c(0.40, 0.40, 0.20),
      stringsAsFactors = FALSE
    )

    list(
      current_date = current_date,
      id = "strategy_FIA",
      fund_name = "sicoob_acoes",
      real_last_eop_positions = real_last_eop_positions,
      asset_ticker_lookup_today = asset_ticker_lookup_today,
      prices_yesterday = prices_yesterday,
      prices_today = prices_today,
      proventos_today = proventos_today,
      splits_today = splits_today,
      trades_today = trades_today,
      target_today = target_today,
      fabricate_trades = TRUE,
      default_lot_size = 100,
      etf_lot_size = 1,
      etf_tickers = c("BOVA11", "BOVV11", "SMLL11", "DIVO11", "LFTS11", "ISUS11"),
      daily_fee_return = 0,
      position_tolerance = 1e-8,
      weight_tolerance = 1e-2
    )
  }

  call_step <- function(inputs) {
    do.call(compute_real_portfolio_step, inputs)
  }

  compute_expected_synthetic_trades <- function(
    real_last_eop_positions,
    prices_today,
    target_today,
    default_lot_size = 100,
    etf_lot_size = 1,
    etf_tickers = c("BOVA11", "BOVV11", "SMLL11", "DIVO11", "LFTS11", "ISUS11"),
    position_tolerance = 1e-8
  ) {
    real_portfolio_base <- dplyr::full_join(
      real_last_eop_positions,
      target_today %>%
        dplyr::select(cvm_code_type, legacy_ticker),
      by = "cvm_code_type"
    ) %>%
      dplyr::mutate(
        eop_positions = dplyr::coalesce(eop_positions, 0)
      ) %>%
      dplyr::left_join(
        prices_today %>%
          dplyr::select(cvm_code_type, price),
        by = "cvm_code_type"
      ) %>%
      dplyr::left_join(
        target_today %>%
          dplyr::select(cvm_code_type, target_weight = weights),
        by = "cvm_code_type"
      ) %>%
      dplyr::mutate(
        target_weight = dplyr::coalesce(target_weight, 0)
      )

    current_aum_before_trades <- sum(
      real_portfolio_base$eop_positions * real_portfolio_base$price,
      na.rm = TRUE
    )

    real_portfolio_base %>%
      dplyr::mutate(
        lot_size = dplyr::if_else(
          legacy_ticker %in% etf_tickers | cvm_code_type %in% etf_tickers,
          etf_lot_size,
          default_lot_size
        ),
        target_market_value = current_aum_before_trades * target_weight,
        target_position_raw = target_market_value / price,
        target_position = base::round(target_position_raw / lot_size) * lot_size,
        net_trade = target_position - eop_positions,
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
      ) %>%
      dplyr::arrange(cvm_code_type)
  }

  testthat::test_that("synthetic trade mode fabricates trades from target weights", {
    inputs <- build_valid_synthetic_step_inputs()

    out <- call_step(inputs)

    expected_trades <- compute_expected_synthetic_trades(
      real_last_eop_positions = inputs$real_last_eop_positions,
      prices_today = inputs$prices_today,
      target_today = inputs$target_today,
      default_lot_size = inputs$default_lot_size,
      etf_lot_size = inputs$etf_lot_size,
      etf_tickers = inputs$etf_tickers,
      position_tolerance = inputs$position_tolerance
    )

    observed_trades <- out$tables$trades %>%
      dplyr::select(
        cvm_code_type,
        net_trade,
        net_traded_volume,
        brokerage_fee_estimated,
        avg_trade_price
      ) %>%
      dplyr::arrange(cvm_code_type)

    testthat::expect_equal(observed_trades$cvm_code_type, expected_trades$cvm_code_type)
    testthat::expect_equal(observed_trades$net_trade, expected_trades$net_trade, tolerance = 1e-12)
    testthat::expect_equal(observed_trades$net_traded_volume, expected_trades$net_traded_volume, tolerance = 1e-12)
    testthat::expect_equal(observed_trades$brokerage_fee_estimated, expected_trades$brokerage_fee_estimated, tolerance = 1e-12)
    testthat::expect_equal(observed_trades$avg_trade_price, expected_trades$avg_trade_price, tolerance = 1e-12)
  })

  testthat::test_that("synthetic trade mode adds target assets not present in previous real portfolio", {
    inputs <- build_valid_synthetic_step_inputs()

    out <- call_step(inputs)

    ccc_trade <- out$tables$trades %>%
      dplyr::filter(cvm_code_type == "CCC3")

    ccc_eop <- out$tables$eop_positions %>%
      dplyr::filter(cvm_code_type == "CCC3")

    testthat::expect_equal(nrow(ccc_trade), 1L)
    testthat::expect_gt(ccc_trade$net_trade, 0)
    testthat::expect_equal(ccc_eop$positions, ccc_trade$net_trade, tolerance = 1e-12)
  })

  testthat::test_that("synthetic trade mode sells assets removed from target weights", {
    inputs <- build_valid_synthetic_step_inputs()

    out <- call_step(inputs)

    bbb_trade <- out$tables$trades %>%
      dplyr::filter(cvm_code_type == "BBB4")

    bbb_eop <- out$tables$eop_positions %>%
      dplyr::filter(cvm_code_type == "BBB4")

    testthat::expect_equal(nrow(bbb_trade), 1L)
    testthat::expect_lt(bbb_trade$net_trade, 0)
    testthat::expect_equal(bbb_eop$positions, 0, tolerance = 1e-12)
  })

  testthat::test_that("synthetic trade mode respects ETF lot size", {
    inputs <- build_valid_synthetic_step_inputs()

    out <- call_step(inputs)

    bova_eop <- out$tables$eop_positions %>%
      dplyr::filter(cvm_code_type == "BOVA11")

    testthat::expect_equal(bova_eop$positions %% inputs$etf_lot_size, 0)
    testthat::expect_gt(bova_eop$positions, 0)
  })

  testthat::test_that("synthetic trade mode blocks simultaneous broker trades", {
    inputs <- build_valid_synthetic_step_inputs()

    inputs$trades_today <- data.frame(
      cvm_code_type = "AAA3",
      signed_position = 100,
      signed_traded_volume = 1000,
      brokerage_fee_estimated = 1,
      price = 10,
      stringsAsFactors = FALSE
    )

    testthat::expect_error(
      call_step(inputs),
      "Use either broker trades or synthetic trades, not both.",
      fixed = TRUE
    )
  })

  testthat::test_that("synthetic trade mode blocks invalid target weights", {
    inputs <- build_valid_synthetic_step_inputs()
    inputs$target_today$weights <- c(0.40, 0.40, 0.40)

    testthat::expect_error(
      call_step(inputs),
      "`target_today$weights` must sum to 1 when synthetic trades are requested.",
      fixed = TRUE
    )

    inputs <- build_valid_synthetic_step_inputs()
    inputs$target_today <- dplyr::bind_rows(
      inputs$target_today,
      inputs$target_today[1, ]
    )

    testthat::expect_error(
      call_step(inputs),
      "`target_today` has duplicated `cvm_code_type` rows.",
      fixed = TRUE
    )
  })

  invisible(TRUE)
}

run_bind_old_dados_gold_test <- function() {

  build_simplified_old_dados_gold <- function() {
    list(
      paper = list(
        portfolio = data.frame(
          date = as.Date("2026-04-22"),
          id = "strategy_FIA",
          legacy_ticker = c("AAA3", "BBB4"),
          cvm_code_type = c("AAA3", "BBB4"),
          eop_weights = c(0.60, 0.40),
          stringsAsFactors = FALSE
        )
      ),
      real = list(
        portfolio = data.frame(
          date = as.Date("2026-04-22"),
          id = "strategy_FIA",
          fund_name = "sicoob_acoes",
          legacy_ticker = c("AAA3", "BBB4"),
          cvm_code_type = c("AAA3", "BBB4"),
          eop_positions = c(100, 200),
          price = c(10, 20),
          stringsAsFactors = FALSE
        )
      )
    )
  }

  build_evolved_portfolios <- function() {
    inputs <- build_valid_integration_inputs()

    inputs$current_dates <- as.Date(c("2026-04-23", "2026-04-24"))

    # Binder tests do not validate broker-trade ingestion. Broker trades are
    # removed so non-YMF portfolios can use synthetic trade fabrication.
    inputs$brokerage_data <- inputs$brokerage_data[0, ]

    list(
      strategy_FIA = do.call(evolve_portfolio, inputs)
    )
  }

  build_evolved_portfolios_for_dates <- function(current_dates, old_dados_gold) {
    inputs <- build_valid_integration_inputs()

    inputs$current_dates <- as.Date(current_dates)

    # evolve_portfolio() still receives this argument as `old_portfolio`.
    # The object passed here is now a dados_gold-compatible object.
    inputs$old_portfolio <- old_dados_gold

    # Binder tests validate accumulation of processed outputs, not broker-trade
    # ingestion. Non-YMF portfolios use synthetic trade fabrication.
    inputs$brokerage_data <- inputs$brokerage_data[0, ]

    inputs$transaction_costs_bps <- data.frame(
      date = as.Date(current_dates),
      id = inputs$id,
      transaction_cost_bps = 0,
      stringsAsFactors = FALSE
    )

    list(
      strategy_FIA = do.call(evolve_portfolio, inputs)
    )
  }

  get_nested_element_for_test <- function(x, path) {
    out <- x

    for (nm in path) {
      out <- out[[nm]]
    }

    out
  }

  expect_table_dates <- function(x, path, expected_dates) {
    tbl <- get_nested_element_for_test(x, path)

    testthat::expect_true(
      is.data.frame(tbl),
      info = paste(path, collapse = "$")
    )

    if (nrow(tbl) == 0L) {
      return(invisible(TRUE))
    }

    testthat::expect_true(
      "date" %in% names(tbl),
      info = paste(path, collapse = "$")
    )

    testthat::expect_equal(
      sort(unique(as.Date(tbl$date))),
      expected_dates,
      info = paste(path, collapse = "$")
    )

    invisible(TRUE)
  }

  add_stale_marker_to_dates <- function(x, stale_dates) {
    stale_dates <- as.Date(stale_dates)

    if (is.data.frame(x)) {
      if (!"date" %in% names(x) || nrow(x) == 0L) {
        return(x)
      }

      x$stale_marker <- dplyr::if_else(
        as.Date(x$date) %in% stale_dates,
        TRUE,
        NA
      )

      return(x)
    }

    if (is.list(x) && !is.data.frame(x)) {
      for (nm in names(x)) {
        x[[nm]] <- add_stale_marker_to_dates(
          x = x[[nm]],
          stale_dates = stale_dates
        )
      }
    }

    x
  }

  collect_stale_markers <- function(x) {
    if (is.data.frame(x)) {
      if (!"stale_marker" %in% names(x) || nrow(x) == 0L) {
        return(logical())
      }

      return(x$stale_marker)
    }

    if (!is.list(x) || is.data.frame(x)) {
      return(logical())
    }

    unlist(
      lapply(x, collect_stale_markers),
      use.names = FALSE
    )
  }

  testthat::test_that("simplified old dados_gold is allowed only at initial rebalancing date", {
    old_dados_gold <- build_simplified_old_dados_gold()
    evolved_portfolios <- build_evolved_portfolios()

    out <- bind_old_dados_gold(
      old_dados_gold = old_dados_gold,
      evolved_portfolios = evolved_portfolios,
      current_dates = as.Date(c("2026-04-23", "2026-04-24")),
      initial_rebalancing_date = as.Date("2026-04-22"),
      verbose = FALSE
    )

    testthat::expect_true(is.list(out))
    testthat::expect_true(all(c("paper", "real") %in% names(out)))

    testthat::expect_true(
      all(as.Date(old_dados_gold$paper$portfolio$date) %in% out$paper$portfolio$date)
    )

    testthat::expect_true(
      all(as.Date(evolved_portfolios$strategy_FIA$paper$portfolio$date) %in% out$paper$portfolio$date)
    )

    testthat::expect_true("weights" %in% names(out$paper))
    testthat::expect_true("returns" %in% names(out$paper))
    testthat::expect_true("positions" %in% names(out$real))
    testthat::expect_true("trades" %in% names(out$real))
  })

  testthat::test_that("simplified old dados_gold is blocked away from initial rebalancing date", {
    old_dados_gold <- build_simplified_old_dados_gold()
    evolved_portfolios <- build_evolved_portfolios()

    testthat::expect_error(
      bind_old_dados_gold(
        old_dados_gold = old_dados_gold,
        evolved_portfolios = evolved_portfolios,
        current_dates = as.Date(c("2026-04-23", "2026-04-24")),
        initial_rebalancing_date = as.Date("2026-04-20"),
        verbose = FALSE
      ),
      "Simplified `old_dados_gold` is only allowed when its last date equals",
      fixed = TRUE
    )
  })

  testthat::test_that("overlapping old dados_gold rows are discarded and new rows win", {
    old_dados_gold_initial <- build_simplified_old_dados_gold()

    pass_1_dates <- as.Date("2026-04-23")

    evolved_pass_1 <- build_evolved_portfolios_for_dates(
      current_dates = pass_1_dates,
      old_dados_gold = old_dados_gold_initial
    )

    dados_gold_after_pass_1 <- bind_old_dados_gold(
      old_dados_gold = old_dados_gold_initial,
      evolved_portfolios = evolved_pass_1,
      current_dates = pass_1_dates,
      initial_rebalancing_date = as.Date("2026-04-22"),
      verbose = FALSE
    )

    old_dados_gold_with_stale_overlap <- add_stale_marker_to_dates(
      x = dados_gold_after_pass_1,
      stale_dates = pass_1_dates
    )

    evolved_rerun_pass_1 <- build_evolved_portfolios_for_dates(
      current_dates = pass_1_dates,
      old_dados_gold = old_dados_gold_initial
    )

    dados_gold_after_rerun <- bind_old_dados_gold(
      old_dados_gold = old_dados_gold_with_stale_overlap,
      evolved_portfolios = evolved_rerun_pass_1,
      current_dates = pass_1_dates,
      initial_rebalancing_date = as.Date("2026-04-22"),
      verbose = FALSE
    )

    stale_markers <- collect_stale_markers(dados_gold_after_rerun)

    testthat::expect_false(
      any(isTRUE(stale_markers), na.rm = TRUE)
    )

    expected_portfolio_dates <- as.Date(c(
      "2026-04-22",
      "2026-04-23"
    ))

    expected_evolved_dates <- as.Date("2026-04-23")

    testthat::expect_equal(
      sort(unique(as.Date(dados_gold_after_rerun$paper$portfolio$date))),
      expected_portfolio_dates
    )

    testthat::expect_equal(
      sort(unique(as.Date(dados_gold_after_rerun$real$portfolio$date))),
      expected_portfolio_dates
    )

    testthat::expect_equal(
      sort(unique(as.Date(dados_gold_after_rerun$paper$returns$date))),
      expected_evolved_dates
    )

    testthat::expect_equal(
      sort(unique(as.Date(dados_gold_after_rerun$real$returns$date))),
      expected_evolved_dates
    )

    duplicated_paper_portfolio <- dados_gold_after_rerun$paper$portfolio |>
      dplyr::count(date, id, cvm_code_type, name = "n") |>
      dplyr::filter(n > 1L)

    duplicated_real_portfolio <- dados_gold_after_rerun$real$portfolio |>
      dplyr::count(date, id, fund_name, cvm_code_type, name = "n") |>
      dplyr::filter(n > 1L)

    testthat::expect_equal(nrow(duplicated_paper_portfolio), 0L)
    testthat::expect_equal(nrow(duplicated_real_portfolio), 0L)
  })

  testthat::test_that("binder keeps old dates and new evolved dates", {
    old_dados_gold <- build_simplified_old_dados_gold()
    evolved_portfolios <- build_evolved_portfolios()

    out <- bind_old_dados_gold(
      old_dados_gold = old_dados_gold,
      evolved_portfolios = evolved_portfolios,
      current_dates = as.Date(c("2026-04-23", "2026-04-24")),
      initial_rebalancing_date = as.Date("2026-04-22"),
      verbose = FALSE
    )

    expected_dates <- as.Date(c(
      "2026-04-22",
      "2026-04-23",
      "2026-04-24"
    ))

    testthat::expect_equal(
      sort(unique(as.Date(out$paper$portfolio$date))),
      expected_dates
    )

    testthat::expect_equal(
      sort(unique(as.Date(out$real$portfolio$date))),
      expected_dates
    )
  })

  testthat::test_that("non-simplified old dados_gold is accepted across repeated batch updates", {
    old_dados_gold_initial <- build_simplified_old_dados_gold()

    pass_1_dates <- as.Date("2026-04-23")
    pass_2_dates <- as.Date("2026-04-24")

    evolved_pass_1 <- build_evolved_portfolios_for_dates(
      current_dates = pass_1_dates,
      old_dados_gold = old_dados_gold_initial
    )

    dados_gold_after_pass_1 <- bind_old_dados_gold(
      old_dados_gold = old_dados_gold_initial,
      evolved_portfolios = evolved_pass_1,
      current_dates = pass_1_dates,
      initial_rebalancing_date = as.Date("2026-04-22"),
      verbose = FALSE
    )

    evolved_pass_2 <- build_evolved_portfolios_for_dates(
      current_dates = pass_2_dates,
      old_dados_gold = dados_gold_after_pass_1
    )

    dados_gold_after_pass_2 <- bind_old_dados_gold(
      old_dados_gold = dados_gold_after_pass_1,
      evolved_portfolios = evolved_pass_2,
      current_dates = pass_2_dates,
      initial_rebalancing_date = as.Date("2026-04-22"),
      verbose = FALSE
    )

    expected_portfolio_dates <- as.Date(c(
      "2026-04-22",
      "2026-04-23",
      "2026-04-24"
    ))

    expected_evolved_dates <- as.Date(c(
      "2026-04-23",
      "2026-04-24"
    ))

    testthat::expect_equal(
      sort(unique(as.Date(dados_gold_after_pass_2$paper$portfolio$date))),
      expected_portfolio_dates
    )

    testthat::expect_equal(
      sort(unique(as.Date(dados_gold_after_pass_2$real$portfolio$date))),
      expected_portfolio_dates
    )

    testthat::expect_equal(
      sort(unique(as.Date(dados_gold_after_pass_2$paper$returns$date))),
      expected_evolved_dates
    )

    testthat::expect_equal(
      sort(unique(as.Date(dados_gold_after_pass_2$real$returns$date))),
      expected_evolved_dates
    )

    testthat::expect_equal(
      sort(unique(as.Date(dados_gold_after_pass_2$paper$weights$eop_weights$date))),
      expected_evolved_dates
    )

    testthat::expect_equal(
      sort(unique(as.Date(dados_gold_after_pass_2$real$positions$eop_positions$date))),
      expected_evolved_dates
    )

    testthat::expect_true("portfolio" %in% names(dados_gold_after_pass_2$paper))
    testthat::expect_true("weights" %in% names(dados_gold_after_pass_2$paper))
    testthat::expect_true("returns" %in% names(dados_gold_after_pass_2$paper))
    testthat::expect_true("market_value" %in% names(dados_gold_after_pass_2$paper))
    testthat::expect_true("turnover" %in% names(dados_gold_after_pass_2$paper))
    testthat::expect_true("costs" %in% names(dados_gold_after_pass_2$paper))
    testthat::expect_true("fees" %in% names(dados_gold_after_pass_2$paper))

    testthat::expect_true("portfolio" %in% names(dados_gold_after_pass_2$real))
    testthat::expect_true("positions" %in% names(dados_gold_after_pass_2$real))
    testthat::expect_true("returns" %in% names(dados_gold_after_pass_2$real))
    testthat::expect_true("market_value" %in% names(dados_gold_after_pass_2$real))
    testthat::expect_true("trades" %in% names(dados_gold_after_pass_2$real))
    testthat::expect_true("splits" %in% names(dados_gold_after_pass_2$real))
    testthat::expect_true("turnover" %in% names(dados_gold_after_pass_2$real))
    testthat::expect_true("costs" %in% names(dados_gold_after_pass_2$real))
    testthat::expect_true("fees" %in% names(dados_gold_after_pass_2$real))

    duplicated_paper_portfolio <- dados_gold_after_pass_2$paper$portfolio |>
      dplyr::count(date, id, cvm_code_type, name = "n") |>
      dplyr::filter(n > 1L)

    duplicated_real_portfolio <- dados_gold_after_pass_2$real$portfolio |>
      dplyr::count(date, id, fund_name, cvm_code_type, name = "n") |>
      dplyr::filter(n > 1L)

    testthat::expect_equal(nrow(duplicated_paper_portfolio), 0L)
    testthat::expect_equal(nrow(duplicated_real_portfolio), 0L)
  })

  testthat::test_that("all expected nested dated tables are accumulated", {
    old_dados_gold_initial <- build_simplified_old_dados_gold()

    pass_1_dates <- as.Date("2026-04-23")
    pass_2_dates <- as.Date("2026-04-24")

    evolved_pass_1 <- build_evolved_portfolios_for_dates(
      current_dates = pass_1_dates,
      old_dados_gold = old_dados_gold_initial
    )

    dados_gold_after_pass_1 <- bind_old_dados_gold(
      old_dados_gold = old_dados_gold_initial,
      evolved_portfolios = evolved_pass_1,
      current_dates = pass_1_dates,
      initial_rebalancing_date = as.Date("2026-04-22"),
      verbose = FALSE
    )

    evolved_pass_2 <- build_evolved_portfolios_for_dates(
      current_dates = pass_2_dates,
      old_dados_gold = dados_gold_after_pass_1
    )

    dados_gold_after_pass_2 <- bind_old_dados_gold(
      old_dados_gold = dados_gold_after_pass_1,
      evolved_portfolios = evolved_pass_2,
      current_dates = pass_2_dates,
      initial_rebalancing_date = as.Date("2026-04-22"),
      verbose = FALSE
    )

    expected_portfolio_dates <- as.Date(c(
      "2026-04-22",
      "2026-04-23",
      "2026-04-24"
    ))

    expected_evolved_dates <- as.Date(c(
      "2026-04-23",
      "2026-04-24"
    ))

    expect_table_dates_subset <- function(x, path, allowed_dates) {
      tbl <- get_nested_element_for_test(x, path)

      testthat::expect_true(
        is.data.frame(tbl),
        info = paste(path, collapse = "$")
      )

      if (nrow(tbl) == 0L) {
        return(invisible(TRUE))
      }

      testthat::expect_true(
        "date" %in% names(tbl),
        info = paste(path, collapse = "$")
      )

      table_dates <- sort(unique(as.Date(tbl$date)))

      testthat::expect_true(
        all(table_dates %in% allowed_dates),
        info = paste(path, collapse = "$")
      )

      invisible(TRUE)
    }

    expect_table_dates(
      dados_gold_after_pass_2,
      c("paper", "portfolio"),
      expected_portfolio_dates
    )

    expect_table_dates(
      dados_gold_after_pass_2,
      c("real", "portfolio"),
      expected_portfolio_dates
    )

    dense_evolved_table_paths <- list(
      c("paper", "weights", "bop_weights"),
      c("paper", "weights", "eop_weights"),
      c("paper", "returns"),
      c("paper", "market_value"),
      c("paper", "turnover"),
      c("paper", "costs"),
      c("paper", "fees"),
      c("real", "positions", "bop_positions"),
      c("real", "positions", "eop_positions"),
      c("real", "returns"),
      c("real", "market_value"),
      c("real", "turnover"),
      c("real", "costs"),
      c("real", "fees")
    )

    purrr::walk(
      dense_evolved_table_paths,
      function(path) {
        expect_table_dates(
          dados_gold_after_pass_2,
          path,
          expected_evolved_dates
        )
      }
    )

    sparse_event_table_paths <- list(
      c("real", "trades"),
      c("real", "splits")
    )

    purrr::walk(
      sparse_event_table_paths,
      function(path) {
        expect_table_dates_subset(
          dados_gold_after_pass_2,
          path,
          expected_evolved_dates
        )
      }
    )

    duplicated_paper_portfolio <- dados_gold_after_pass_2$paper$portfolio |>
      dplyr::count(date, id, cvm_code_type, name = "n") |>
      dplyr::filter(n > 1L)

    duplicated_real_portfolio <- dados_gold_after_pass_2$real$portfolio |>
      dplyr::count(date, id, fund_name, cvm_code_type, name = "n") |>
      dplyr::filter(n > 1L)

    testthat::expect_equal(nrow(duplicated_paper_portfolio), 0L)
    testthat::expect_equal(nrow(duplicated_real_portfolio), 0L)
  })

  invisible(TRUE)
}

run_derive_old_portfolio_and_validate_ids_test <- function() {

  empty_date_df <- function() {
    data.frame(
      date = as.Date(character()),
      stringsAsFactors = FALSE
    )
  }

  build_deriver_test_inputs <- function() {
    initial_rebalancing_date <- as.Date("2026-04-22")
    current_dates <- as.Date(c("2026-04-22", "2026-04-23"))

    rebal_weights <- data.frame(
      date = rep(initial_rebalancing_date, 2),
      id = "carteira_bbg_FIA",
      legacy_ticker = c("AAA3", "BBB4"),
      cvm_code_type = c("AAA3", "BBB4"),
      weights = c(0.60, 0.40),
      stringsAsFactors = FALSE
    )

    comdinheiro_data <- data.frame(
      date = rep(as.Date(c("2026-04-22", "2026-04-23", "2026-04-24")), each = 2),
      legacy_ticker = rep(c("AAA3", "BBB4"), times = 3),
      cvm_code_type = rep(c("AAA3", "BBB4"), times = 3),
      ret_1d = c(0, 0, 1.00, -1.00, 0.50, 0.20),
      price = c(10, 20, 10.10, 19.80, 10.1505, 19.8396),
      proventos = 0,
      proventos_date = as.Date(NA),
      event_factor = 1,
      n_shares = c(1000000, 2000000, 1000000, 2000000, 1000000, 2000000),
      stringsAsFactors = FALSE
    )

    port_iniciais <- data.frame(
      date = rep(initial_rebalancing_date, 2),
      id = "YMF_29",
      fund_name = "sicoob_acoes",
      legacy_ticker = c("AAA3", "BBB4"),
      cvm_code_type = c("AAA3", "BBB4"),
      positions = c(100, 200),
      price = c(10, 20),
      stringsAsFactors = FALSE
    )

    list(
      initial_rebalancing_date = initial_rebalancing_date,
      current_dates = current_dates,
      rebal_weights = rebal_weights,
      comdinheiro_data = comdinheiro_data,
      port_iniciais = port_iniciais
    )
  }

  subset_old_portfolio_by_id <- function(old_portfolio, id, fund_name = NULL) {
    old_portfolio$paper$portfolio <- old_portfolio$paper$portfolio %>%
      dplyr::filter(.data$id == .env$id)

    old_portfolio$real$portfolio <- old_portfolio$real$portfolio %>%
      dplyr::filter(.data$id == .env$id)

    if (!is.null(fund_name)) {
      old_portfolio$real$portfolio <- old_portfolio$real$portfolio %>%
        dplyr::filter(.data$fund_name == .env$fund_name)
    }

    old_portfolio
  }

  empty_brokerage_data <- function() {
    data.frame(
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
  }

  testthat::test_that("initial derivation creates simplified old_portfolio with EOP state schema", {
    inputs <- build_deriver_test_inputs()

    old_portfolio <- derive_old_portfolio_and_validate_ids(
      old_dados_gold = NULL,
      rebal_weights = inputs$rebal_weights,
      comdinheiro_data = inputs$comdinheiro_data,
      port_iniciais = inputs$port_iniciais,
      current_dates = inputs$current_dates,
      initial_rebalancing_date = inputs$initial_rebalancing_date,
      starting_aum = 10000000
    )

    testthat::expect_true(is.list(old_portfolio))
    testthat::expect_true(all(c("paper", "real") %in% names(old_portfolio)))
    testthat::expect_true(is.data.frame(old_portfolio$paper$portfolio))
    testthat::expect_true(is.data.frame(old_portfolio$real$portfolio))

    testthat::expect_true("eop_weights" %in% names(old_portfolio$paper$portfolio))
    testthat::expect_true("eop_positions" %in% names(old_portfolio$real$portfolio))

    testthat::expect_false("weights" %in% names(old_portfolio$paper$portfolio))
    testthat::expect_false("positions" %in% names(old_portfolio$real$portfolio))

    testthat::expect_true(all(old_portfolio$paper$portfolio$date == inputs$initial_rebalancing_date))
    testthat::expect_true(all(old_portfolio$real$portfolio$date == inputs$initial_rebalancing_date))

    paper_weight_sums <- old_portfolio$paper$portfolio %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(weight_sum = sum(eop_weights), .groups = "drop")

    testthat::expect_true(all(abs(paper_weight_sums$weight_sum - 1) <= 1e-12))

    testthat::expect_true(all(old_portfolio$real$portfolio$eop_positions >= 0))
    testthat::expect_true(all(old_portfolio$real$portfolio$price > 0))
  })

  testthat::test_that("initial derivation requires current_dates to start at initial_rebalancing_date", {
    inputs <- build_deriver_test_inputs()
    inputs$current_dates <- as.Date(c("2026-04-23", "2026-04-24"))

    testthat::expect_error(
      derive_old_portfolio_and_validate_ids(
        old_dados_gold = NULL,
        rebal_weights = inputs$rebal_weights,
        comdinheiro_data = inputs$comdinheiro_data,
        port_iniciais = inputs$port_iniciais,
        current_dates = inputs$current_dates,
        initial_rebalancing_date = inputs$initial_rebalancing_date
      ),
      "When `old_dados_gold` is NULL, the first date in `current_dates` must be equal to `initial_rebalancing_date`.",
      fixed = TRUE
    )
  })

  testthat::test_that("old_dados_gold branch discards rows inside the new current_dates range", {
    inputs <- build_deriver_test_inputs()

    old_initial <- derive_old_portfolio_and_validate_ids(
      old_dados_gold = NULL,
      rebal_weights = inputs$rebal_weights,
      comdinheiro_data = inputs$comdinheiro_data,
      port_iniciais = inputs$port_iniciais,
      current_dates = inputs$current_dates,
      initial_rebalancing_date = inputs$initial_rebalancing_date
    )

    old_dados_gold <- list(
        paper = list(
          portfolio = dplyr::bind_rows(
            old_initial$paper$portfolio,
            old_initial$paper$portfolio %>%
              dplyr::mutate(date = as.Date("2026-04-23"))
          ),
          weights = list(
            bop_weights = data.frame(
              date = as.Date("2026-04-23"),
              id = "YMF_29",
              fund_name = "sicoob_acoes",
              legacy_ticker = c("AAA3", "BBB4"),
              cvm_code_type = c("AAA3", "BBB4"),
              weights = c(0.60, 0.40),
              stringsAsFactors = FALSE
            ),
            eop_weights = data.frame(
              date = as.Date("2026-04-23"),
              id = "YMF_29",
              fund_name = "sicoob_acoes",
              legacy_ticker = c("AAA3", "BBB4"),
              cvm_code_type = c("AAA3", "BBB4"),
              weights = c(0.55, 0.45),
              stringsAsFactors = FALSE
            )
          ),
          returns = data.frame(
            date = as.Date("2026-04-23"),
            id = "YMF_29",
            fund_name = "sicoob_acoes",
            raw_return = 0.01,
            net_return = 0.01,
            stringsAsFactors = FALSE
          ),
          market_value = data.frame(date = as.Date(character()), stringsAsFactors = FALSE),
          turnover = data.frame(date = as.Date(character()), stringsAsFactors = FALSE),
          costs = data.frame(date = as.Date(character()), stringsAsFactors = FALSE),
          fees = data.frame(date = as.Date(character()), stringsAsFactors = FALSE)
        ),
        real = list(
          portfolio = dplyr::bind_rows(
            old_initial$real$portfolio,
            old_initial$real$portfolio %>%
              dplyr::mutate(date = as.Date("2026-04-23"))
          ),
          positions = list(
            bop_positions = data.frame(
              date = as.Date("2026-04-23"),
              id = "YMF_29",
              fund_name = "sicoob_acoes",
              legacy_ticker = c("AAA3", "BBB4"),
              cvm_code_type = c("AAA3", "BBB4"),
              positions = c(100, 200),
              price_last_close = c(10, 20),
              market_value_last_close = c(1000, 4000),
              weights = c(0.20, 0.80),
              stringsAsFactors = FALSE
            ),
            eop_positions = data.frame(
              date = as.Date("2026-04-23"),
              id = "YMF_29",
              fund_name = "sicoob_acoes",
              legacy_ticker = c("AAA3", "BBB4"),
              cvm_code_type = c("AAA3", "BBB4"),
              positions = c(110, 190),
              price = c(10.10, 19.80),
              market_value = c(1111, 3762),
              weights = c(1111 / 4873, 3762 / 4873),
              stringsAsFactors = FALSE
            )
          ),
          returns = data.frame(
            date = as.Date("2026-04-23"),
            id = "YMF_29",
            fund_name = "sicoob_acoes",
            raw_return = 0.01,
            net_return = 0.01,
            stringsAsFactors = FALSE
          ),
          market_value = data.frame(date = as.Date(character()), stringsAsFactors = FALSE),
          trades = data.frame(date = as.Date(character()), stringsAsFactors = FALSE),
          splits = data.frame(date = as.Date(character()), stringsAsFactors = FALSE),
          turnover = data.frame(date = as.Date(character()), stringsAsFactors = FALSE),
          costs = data.frame(date = as.Date(character()), stringsAsFactors = FALSE),
          fees = data.frame(date = as.Date(character()), stringsAsFactors = FALSE)
        )
    )

    current_dates <- as.Date(c("2026-04-23", "2026-04-24"))

    filtered_old <- testthat::expect_message(
      derive_old_portfolio_and_validate_ids(
        old_dados_gold = old_dados_gold,
        rebal_weights = inputs$rebal_weights,
        comdinheiro_data = inputs$comdinheiro_data,
        port_iniciais = inputs$port_iniciais,
        current_dates = current_dates,
        initial_rebalancing_date = inputs$initial_rebalancing_date
      ),
      "Discarding"
    )

    testthat::expect_true(all(filtered_old$paper$portfolio$date < min(current_dates)))
    testthat::expect_true(all(filtered_old$real$portfolio$date < min(current_dates)))

    testthat::expect_false(any(filtered_old$paper$portfolio$date >= min(current_dates)))
    testthat::expect_false(any(filtered_old$real$portfolio$date >= min(current_dates)))
  })

  testthat::test_that("old_dados_gold branch errors when filtering removes all old rows", {
    inputs <- build_deriver_test_inputs()

    old_dados_gold <- list(
        paper = list(
          portfolio = data.frame(
            date = as.Date("2026-04-23"),
            id = "YMF_29",
            legacy_ticker = c("AAA3", "BBB4"),
            cvm_code_type = c("AAA3", "BBB4"),
            eop_weights = c(0.60, 0.40),
            stringsAsFactors = FALSE
          )
        ),
        real = list(
          portfolio = data.frame(
            date = as.Date("2026-04-23"),
            id = "YMF_29",
            fund_name = "sicoob_acoes",
            legacy_ticker = c("AAA3", "BBB4"),
            cvm_code_type = c("AAA3", "BBB4"),
            eop_positions = c(100, 200),
            price = c(10, 20),
            stringsAsFactors = FALSE
          )
        )
    )

    testthat::expect_error(
      derive_old_portfolio_and_validate_ids(
        old_dados_gold = old_dados_gold,
        rebal_weights = inputs$rebal_weights,
        comdinheiro_data = inputs$comdinheiro_data,
        port_iniciais = NULL,
        current_dates = as.Date(c("2026-04-23", "2026-04-24")),
        initial_rebalancing_date = inputs$initial_rebalancing_date
      ),
      "`old_dados_gold$portfolio$paper$portfolio` has no rows before the first current date.",
      fixed = TRUE
    )
  })

  testthat::test_that("initial derivation plus YMF rebal weights can evolve and create next continuation state", {
    inputs <- build_deriver_test_inputs()

    old_portfolio_all <- derive_old_portfolio_and_validate_ids(
      old_dados_gold = NULL,
      rebal_weights = inputs$rebal_weights,
      comdinheiro_data = inputs$comdinheiro_data,
      port_iniciais = inputs$port_iniciais,
      current_dates = inputs$current_dates,
      initial_rebalancing_date = inputs$initial_rebalancing_date
    )

    rebal_weights_with_ymf <- create_ymf_rebal_weights(
      rebal_weights = inputs$rebal_weights,
      comdinheiro_data = inputs$comdinheiro_data,
      old_real_portfolio = old_portfolio_all$real$portfolio,
      id_map = data.frame(
        ymf_id = "YMF_29",
        source_id = "carteira_bbg_FIA",
        stringsAsFactors = FALSE
      )
    )

    testthat::expect_true("YMF_29" %in% unique(rebal_weights_with_ymf$id))

    old_portfolio_ymf <- subset_old_portfolio_by_id(
      old_portfolio = old_portfolio_all,
      id = "YMF_29",
      fund_name = "sicoob_acoes"
    )

    first_inputs <- list(
      old_portfolio = old_portfolio_ymf,
      rebal_weights = rebal_weights_with_ymf,
      comdinheiro_data = inputs$comdinheiro_data,
      current_dates = as.Date("2026-04-23"),
      brokerage_data = empty_brokerage_data(),
      id = "YMF_29",
      fund_name = "sicoob_acoes",
      split_inplit_data = data.frame(
        date = as.Date(character()),
        legacy_ticker = character(),
        cvm_code_type = character(),
        split_factor = numeric(),
        stringsAsFactors = FALSE
      ),
      transaction_costs_bps = data.frame(
        date = as.Date("2026-04-23"),
        id = "YMF_29",
        transaction_cost_bps = 0,
        stringsAsFactors = FALSE
      ),
      fund_fees_bps = 0,
      allow_missing_returns = TRUE
    )

    first_out <- do.call(evolve_portfolio, first_inputs)

    testthat::expect_equal(first_out$workflow$id, "YMF_29")
    testthat::expect_equal(first_out$workflow$old_port_last_date, as.Date("2026-04-22"))
    testthat::expect_equal(first_out$workflow$current_dates, as.Date("2026-04-23"))

    testthat::expect_false("weights" %in% names(first_out$paper$portfolio))
    testthat::expect_false("positions" %in% names(first_out$real$portfolio))
    testthat::expect_true("eop_weights" %in% names(first_out$paper$portfolio))
    testthat::expect_true("eop_positions" %in% names(first_out$real$portfolio))

    next_old_portfolio <- list(
      paper = list(
        portfolio = first_out$paper$portfolio %>%
          dplyr::filter(date == as.Date("2026-04-23")) %>%
          dplyr::select(date, id, legacy_ticker, cvm_code_type, eop_weights)
      ),
      real = list(
        portfolio = first_out$real$portfolio %>%
          dplyr::filter(date == as.Date("2026-04-23")) %>%
          dplyr::select(date, id, fund_name, legacy_ticker, cvm_code_type, eop_positions, price)
      )
    )

    testthat::expect_true(all(c("date", "id", "cvm_code_type", "eop_weights") %in% names(next_old_portfolio$paper$portfolio)))
    testthat::expect_true(all(c("date", "id", "fund_name", "cvm_code_type", "eop_positions", "price") %in% names(next_old_portfolio$real$portfolio)))

    second_inputs <- first_inputs
    second_inputs$old_portfolio <- next_old_portfolio
    second_inputs$current_dates <- as.Date("2026-04-24")
    second_inputs$transaction_costs_bps <- data.frame(
      date = as.Date("2026-04-24"),
      id = "YMF_29",
      transaction_cost_bps = 0,
      stringsAsFactors = FALSE
    )

    second_out <- do.call(evolve_portfolio, second_inputs)

    testthat::expect_equal(second_out$workflow$old_port_last_date, as.Date("2026-04-23"))
    testthat::expect_equal(second_out$workflow$current_dates, as.Date("2026-04-24"))
    testthat::expect_equal(nrow(second_out$paper$returns), 1L)
    testthat::expect_equal(nrow(second_out$real$returns), 1L)
  })

  invisible(TRUE)
}

#Integration tests--------------------------------------------------------------
build_valid_integration_inputs <- function() {

  old_portfolio <- list(
    paper = list(
      portfolio = data.frame(
        date = as.Date("2026-04-22"),
        id = "strategy_FIA",
        cvm_code_type = c("AAA3", "BBB4"),
        eop_weights = c(0.60, 0.40),
        stringsAsFactors = FALSE
      )
    ),
    real = list(
      portfolio = data.frame(
        date = as.Date("2026-04-22"),
        id = "strategy_FIA",
        fund_name = "sicoob_acoes",
        cvm_code_type = c("AAA3", "BBB4"),
        eop_positions = c(100, 200),
        price = c(10, 20),
        stringsAsFactors = FALSE
      )
    )
  )

  rebal_weights <- data.frame(
    date = rep(as.Date(c("2026-04-23", "2026-04-24")), each = 2),
    id = rep("strategy_FIA", 4),
    legacy_ticker = rep(c("AAA3", "BBB4"), times = 2),
    cvm_code_type = rep(c("AAA3", "BBB4"), times = 2),
    weights = c(
      0.50, 0.50,
      0.55, 0.45
    ),
    stringsAsFactors = FALSE
  )

  comdinheiro_data <- data.frame(
    date = rep(as.Date(c("2026-04-22", "2026-04-23", "2026-04-24")), each = 2),
    legacy_ticker = rep(c("AAA3", "BBB4"), times = 3),
    cvm_code_type = rep(c("AAA3", "BBB4"), times = 3),
    ret_1d = c(
      0.00, 0.00,
      1.00, -1.00,
      0.50, 0.20
    ),
    price = c(
      10.00, 20.00,
      10.10, 19.80,
      10.1505, 19.8396
    ),
    proventos = 0,
    proventos_date = as.Date(NA),
    event_factor = 1,
    n_shares = c(
      1000000, 2000000,
      1000000, 2000000,
      1000000, 2000000
    ),
    stringsAsFactors = FALSE
  )

  brokerage_data <- data.frame(
    date = as.Date("2026-04-23"),
    fund_name = "sicoob_acoes",
    legacy_ticker = "AAA3",
    cvm_code_type = "AAA3",
    side = "buy",
    amount = 10,
    price = 10.10,
    traded_volume = 101,
    brokerage_fee_estimated = 1,
    stringsAsFactors = FALSE
  )

  split_inplit_data <- data.frame(
    date = as.Date(character()),
    legacy_ticker = character(),
    cvm_code_type = character(),
    split_factor = numeric(),
    stringsAsFactors = FALSE
  )

  transaction_costs_bps <- data.frame(
    date = as.Date(c("2026-04-23", "2026-04-24")),
    id = c("strategy_FIA", "strategy_FIA"),
    transaction_cost_bps = c(10, 0),
    stringsAsFactors = FALSE
  )

  list(
    old_portfolio = old_portfolio,
    rebal_weights = rebal_weights,
    comdinheiro_data = comdinheiro_data,
    current_dates = as.Date(c("2026-04-23", "2026-04-24")),
    brokerage_data = brokerage_data,
    id = "strategy_FIA",
    fund_name = "sicoob_acoes",
    split_inplit_data = split_inplit_data,
    transaction_costs_bps = transaction_costs_bps,
    fund_fees_bps = 5,
    weight_tolerance = 1e-8,
    position_tolerance = 1e-8,
    split_rounding_tolerance = 0.08,
    split_warning_threshold = 0.25,
    allow_missing_returns = TRUE,
    verbose = FALSE
  )
}

run_evolve_portfolio_integration_test <- function() {

  call_evolve <- function(inputs) {
    do.call(evolve_portfolio, inputs)
  }

  testthat::test_that("single-date integration succeeds and returns stable top-level schema", {
    inputs <- build_valid_integration_inputs()

    inputs$current_dates <- as.Date("2026-04-23")
    inputs$rebal_weights <- inputs$rebal_weights[
      inputs$rebal_weights$date == as.Date("2026-04-23"),
    ]
    inputs$transaction_costs_bps <- inputs$transaction_costs_bps[
      inputs$transaction_costs_bps$date == as.Date("2026-04-23"),
    ]

    inputs$brokerage_data <- inputs$brokerage_data[0, ]
    out <- call_evolve(inputs)

    testthat::expect_true(is.list(out))
    testthat::expect_true(all(c("paper", "real", "workflow", "diagnostics") %in% names(out)))

    testthat::expect_true(all(c(
      "portfolio",
      "weights",
      "returns",
      "market_value",
      "turnover",
      "costs",
      "fees"
    ) %in% names(out$paper)))

    testthat::expect_true(all(c(
      "portfolio",
      "positions",
      "returns",
      "market_value",
      "trades",
      "splits",
      "turnover",
      "costs",
      "fees"
    ) %in% names(out$real)))

    testthat::expect_equal(out$workflow$current_dates, as.Date("2026-04-23"))
    testthat::expect_equal(out$workflow$id, "strategy_FIA")
    testthat::expect_equal(out$workflow$fund_name, "sicoob_acoes")
    testthat::expect_equal(out$workflow$old_port_last_date, as.Date("2026-04-22"))
  })

  testthat::test_that("single-date integration computes expected paper return path", {
    inputs <- build_valid_integration_inputs()

    inputs$current_dates <- as.Date("2026-04-23")
    inputs$rebal_weights <- inputs$rebal_weights[
      inputs$rebal_weights$date == as.Date("2026-04-23"),
    ]
    inputs$transaction_costs_bps <- inputs$transaction_costs_bps[
      inputs$transaction_costs_bps$date == as.Date("2026-04-23"),
    ]

    inputs$brokerage_data <- inputs$brokerage_data[0, ]
    out <- call_evolve(inputs)

    expected_raw_return <- 0.60 * 0.01 + 0.40 * -0.01
    expected_cost_return <- 10 / 10000
    expected_fee_return <- 5 / 10000
    expected_net_return <- (1 + expected_raw_return) *
      (1 - expected_cost_return) *
      (1 - expected_fee_return) - 1

    old_market_value <- 100 * 10 + 200 * 20
    expected_eop_market_value <- old_market_value * (1 + expected_net_return)

    testthat::expect_equal(out$paper$returns$raw_return, expected_raw_return, tolerance = 1e-12)
    testthat::expect_equal(out$paper$returns$net_return, expected_net_return, tolerance = 1e-12)
    testthat::expect_equal(out$paper$market_value$eop_market_value, expected_eop_market_value, tolerance = 1e-8)

    testthat::expect_equal(sum(out$paper$weights$bop_weights$weights), 1, tolerance = 1e-12)
    testthat::expect_equal(sum(out$paper$weights$eop_weights$weights), 1, tolerance = 1e-12)
    testthat::expect_equal(out$paper$weights$eop_weights$weights, c(0.50, 0.50), tolerance = 1e-12)
    testthat::expect_false("weights" %in% names(out$paper$portfolio))
    testthat::expect_true("eop_weights" %in% names(out$paper$portfolio))
  })

  testthat::test_that("single-date integration computes expected real broker trade path", {
    inputs <- build_valid_integration_inputs()

    inputs$id <- "YMF_29"
    inputs$old_portfolio$paper$portfolio$id <- "YMF_29"
    inputs$old_portfolio$real$portfolio$id <- "YMF_29"
    inputs$rebal_weights$id <- "YMF_29"
    inputs$transaction_costs_bps$id <- "YMF_29"

    inputs$current_dates <- as.Date("2026-04-23")
    inputs$rebal_weights <- inputs$rebal_weights[
      inputs$rebal_weights$date == as.Date("2026-04-23"),
    ]
    inputs$transaction_costs_bps <- inputs$transaction_costs_bps[
      inputs$transaction_costs_bps$date == as.Date("2026-04-23"),
    ]

    out <- call_evolve(inputs)

    bop_mv <- 100 * 10 + 200 * 20
    eop_mv <- 110 * 10.10 + 200 * 19.80
    net_trade_cash_flow <- 101
    brokerage_fee <- 1
    daily_fee_return <- 5 / 10000

    expected_raw_return <- (eop_mv - net_trade_cash_flow) / bop_mv - 1
    expected_brokerage_cost_return <- brokerage_fee / bop_mv
    expected_net_return <- (1 + expected_raw_return) *
      (1 - expected_brokerage_cost_return) *
      (1 - daily_fee_return) - 1

    testthat::expect_equal(out$real$returns$raw_return, expected_raw_return, tolerance = 1e-12)
    testthat::expect_equal(out$real$returns$net_return, expected_net_return, tolerance = 1e-12)
    testthat::expect_equal(out$real$returns$market_value_last_close, bop_mv, tolerance = 1e-12)
    testthat::expect_equal(out$real$returns$eop_market_value, eop_mv, tolerance = 1e-12)
    testthat::expect_equal(out$real$returns$net_traded_volume, net_trade_cash_flow, tolerance = 1e-12)

    eop_positions <- out$real$positions$eop_positions
    aaa_eop <- eop_positions[eop_positions$cvm_code_type == "AAA3", ]
    bbb_eop <- eop_positions[eop_positions$cvm_code_type == "BBB4", ]

    testthat::expect_equal(aaa_eop$positions, 110)
    testthat::expect_equal(bbb_eop$positions, 200)
    testthat::expect_equal(sum(eop_positions$weights), 1, tolerance = 1e-12)

    testthat::expect_equal(out$real$turnover$turnover, 101 / bop_mv, tolerance = 1e-12)
    testthat::expect_equal(out$real$costs$brokerage_fee, 1, tolerance = 1e-12)

    testthat::expect_false("positions" %in% names(out$real$portfolio))
    testthat::expect_true("eop_positions" %in% names(out$real$portfolio))
  })

  testthat::test_that("evolve_portfolio works with fund_name = NULL and no real trades", {
    inputs <- build_valid_integration_inputs()

    inputs$id <- "YMF_29"
    inputs$fund_name <- NULL

    inputs$old_portfolio$paper$portfolio$id <- "YMF_29"
    inputs$old_portfolio$real$portfolio$id <- "YMF_29"
    inputs$rebal_weights$id <- "YMF_29"
    inputs$transaction_costs_bps$id <- "YMF_29"

    out <- call_evolve(inputs)

    testthat::expect_null(out$workflow$fund_name)
    testthat::expect_true(is.data.frame(out$real$trades))
    testthat::expect_equal(nrow(out$real$trades), 0L)

    testthat::expect_true(all(is.na(out$real$portfolio$fund_name)))
    testthat::expect_false("positions" %in% names(out$real$portfolio))
    testthat::expect_true("eop_positions" %in% names(out$real$portfolio))
  })

  testthat::test_that("fund_name = NULL still allows synthetic trades for non-YMF portfolios", {
    inputs <- build_valid_integration_inputs()
    inputs$fund_name <- NULL

    out <- call_evolve(inputs)

    testthat::expect_null(out$workflow$fund_name)
    testthat::expect_true(is.data.frame(out$real$trades))
    testthat::expect_gt(nrow(out$real$trades), 0L)

    testthat::expect_true(all(is.na(out$real$trades$fund_name)))
    testthat::expect_true(all(out$real$trades$id == "strategy_FIA"))
  })

  testthat::test_that("multi-date integration carries paper weights and real positions forward", {
    inputs <- build_valid_integration_inputs()
    inputs$brokerage_data <- inputs$brokerage_data[0, ]

    out <- call_evolve(inputs)

    testthat::expect_equal(sort(unique(out$paper$portfolio$date)), inputs$current_dates)
    testthat::expect_equal(sort(unique(out$real$portfolio$date)), inputs$current_dates)

    paper_eop_day_1 <- out$paper$weights$eop_weights[
      out$paper$weights$eop_weights$date == as.Date("2026-04-23"),
    ]

    paper_bop_day_2 <- out$paper$weights$bop_weights[
      out$paper$weights$bop_weights$date == as.Date("2026-04-24"),
    ]

    paper_eop_day_1 <- paper_eop_day_1[order(paper_eop_day_1$cvm_code_type), ]
    paper_bop_day_2 <- paper_bop_day_2[order(paper_bop_day_2$cvm_code_type), ]

    testthat::expect_equal(
      paper_bop_day_2$weights,
      paper_eop_day_1$weights,
      tolerance = 1e-12
    )

    real_eop_day_1 <- out$real$positions$eop_positions[
      out$real$positions$eop_positions$date == as.Date("2026-04-23"),
    ]

    real_bop_day_2 <- out$real$positions$bop_positions[
      out$real$positions$bop_positions$date == as.Date("2026-04-24"),
    ]

    real_eop_day_1 <- real_eop_day_1[order(real_eop_day_1$cvm_code_type), ]
    real_bop_day_2 <- real_bop_day_2[order(real_bop_day_2$cvm_code_type), ]

    testthat::expect_equal(
      real_bop_day_2$positions,
      real_eop_day_1$positions,
      tolerance = 1e-12
    )
  })

  testthat::test_that("multi-date integration computes previous prices from previous current date after day one", {
    inputs <- build_valid_integration_inputs()
    inputs$brokerage_data <- inputs$brokerage_data[0, ]

    out <- call_evolve(inputs)

    real_bop_day_2 <- out$real$positions$bop_positions[
      out$real$positions$bop_positions$date == as.Date("2026-04-24"),
    ]

    aaa_day_2 <- real_bop_day_2[real_bop_day_2$cvm_code_type == "AAA3", ]
    bbb_day_2 <- real_bop_day_2[real_bop_day_2$cvm_code_type == "BBB4", ]

    testthat::expect_equal(aaa_day_2$price_last_close, 10.10, tolerance = 1e-12)
    testthat::expect_equal(bbb_day_2$price_last_close, 19.80, tolerance = 1e-12)
  })

  testthat::test_that("wrong start date before next available market date is blocked", {
    inputs <- build_valid_integration_inputs()

    inputs$current_dates <- as.Date("2026-04-22")
    inputs$rebal_weights$date <- as.Date("2026-04-22")
    inputs$transaction_costs_bps$date <- as.Date("2026-04-22")

    testthat::expect_error(
      call_evolve(inputs),
      "`old_portfolio` last date must be strictly before `min(current_dates)`.",
      fixed = TRUE
    )
  })

  testthat::test_that("wrong start date after next available market date is blocked", {
    inputs <- build_valid_integration_inputs()

    inputs$current_dates <- as.Date("2026-04-24")
    inputs$rebal_weights <- inputs$rebal_weights[
      inputs$rebal_weights$date == as.Date("2026-04-24"),
    ]
    inputs$transaction_costs_bps <- inputs$transaction_costs_bps[
      inputs$transaction_costs_bps$date == as.Date("2026-04-24"),
    ]
    inputs$brokerage_data <- inputs$brokerage_data[0, ]

    testthat::expect_error(
      call_evolve(inputs),
      "`current_dates` must start exactly on the first available market date after `old_portfolio` last date.",
      fixed = TRUE
    )
  })

  testthat::test_that("missing current date in comdinheiro_data is blocked", {
    inputs <- build_valid_integration_inputs()

    inputs$comdinheiro_data <- inputs$comdinheiro_data[
      inputs$comdinheiro_data$date != as.Date("2026-04-24"),
    ]

    testthat::expect_error(
      call_evolve(inputs),
      "`comdinheiro_data` is missing required `current_dates`: 2026-04-24.",
      fixed = TRUE
    )
  })

  testthat::test_that("holiday continuity uses next available market date, not next calendar day", {
    inputs <- build_valid_integration_inputs()

    inputs$old_portfolio$paper$portfolio$date <- as.Date("2026-04-20")
    inputs$old_portfolio$real$portfolio$date <- as.Date("2026-04-20")

    inputs$current_dates <- as.Date(c("2026-04-22", "2026-04-23"))

    inputs$comdinheiro_data <- data.frame(
      date = rep(as.Date(c("2026-04-20", "2026-04-22", "2026-04-23")), each = 2),
      legacy_ticker = rep(c("AAA3", "BBB4"), times = 3),
      cvm_code_type = rep(c("AAA3", "BBB4"), times = 3),
      ret_1d = c(0, 0, 1.00, -1.00, 0.50, 0.20),
      price = c(10, 20, 10.10, 19.80, 10.1505, 19.8396),
      proventos = 0,
      proventos_date = as.Date(NA),
      event_factor = 1,
      n_shares = c(1000000, 2000000, 1000000, 2000000, 1000000, 2000000),
      stringsAsFactors = FALSE
    )

    inputs$rebal_weights <- data.frame(
      date = as.Date("2026-04-22"),
      id = "strategy_FIA",
      legacy_ticker = c("AAA3", "BBB4"),
      cvm_code_type = c("AAA3", "BBB4"),
      weights = c(0.50, 0.50),
      stringsAsFactors = FALSE
    )

    inputs$brokerage_data <- inputs$brokerage_data[0, ]

    inputs$transaction_costs_bps <- data.frame(
      date = as.Date(c("2026-04-22", "2026-04-23")),
      id = c("strategy_FIA", "strategy_FIA"),
      transaction_cost_bps = c(10, 0),
      stringsAsFactors = FALSE
    )

    out <- call_evolve(inputs)

    testthat::expect_equal(out$workflow$old_port_last_date, as.Date("2026-04-20"))
    testthat::expect_equal(out$workflow$current_dates, as.Date(c("2026-04-22", "2026-04-23")))
  })

  testthat::test_that("unconfirmed split candidate triggers warning and appears in diagnostics", {
    inputs <- build_valid_integration_inputs()

    inputs$current_dates <- as.Date("2026-04-23")
    inputs$rebal_weights <- inputs$rebal_weights[
      inputs$rebal_weights$date == as.Date("2026-04-23"),
    ]
    inputs$transaction_costs_bps <- inputs$transaction_costs_bps[
      inputs$transaction_costs_bps$date == as.Date("2026-04-23"),
    ]

    inputs$brokerage_data <- inputs$brokerage_data[0, ]

    inputs$comdinheiro_data$price[
      inputs$comdinheiro_data$date == as.Date("2026-04-23") &
        inputs$comdinheiro_data$cvm_code_type == "AAA3"
    ] <- 5.05

    inputs$comdinheiro_data$ret_1d[
      inputs$comdinheiro_data$date == as.Date("2026-04-23") &
        inputs$comdinheiro_data$cvm_code_type == "AAA3"
    ] <- 0

    inputs$comdinheiro_data$event_factor[
      inputs$comdinheiro_data$date == as.Date("2026-04-23") &
        inputs$comdinheiro_data$cvm_code_type == "AAA3"
    ] <- 0.5

    inputs$comdinheiro_data$n_shares[
      inputs$comdinheiro_data$date == as.Date("2026-04-23") &
        inputs$comdinheiro_data$cvm_code_type == "AAA3"
    ] <- 2000000

    testthat::expect_warning(
      out <- call_evolve(inputs),
      "Potential split/inplit candidates were detected but not provided in `split_inplit_data`.",
      fixed = TRUE
    )

    testthat::expect_true(nrow(out$diagnostics$split_candidates) >= 1L)
    testthat::expect_true("AAA3" %in% out$diagnostics$split_candidates$cvm_code_type)
    testthat::expect_true("AAA3" %in% out$diagnostics$unconfirmed_split_candidates$cvm_code_type)
  })

  testthat::test_that("confirmed split adjusts real positions and removes unconfirmed split warning", {
    inputs <- build_valid_integration_inputs()

    inputs$current_dates <- as.Date("2026-04-23")
    inputs$rebal_weights <- inputs$rebal_weights[
      inputs$rebal_weights$date == as.Date("2026-04-23"),
    ]
    inputs$transaction_costs_bps <- inputs$transaction_costs_bps[
      inputs$transaction_costs_bps$date == as.Date("2026-04-23"),
    ]
    inputs$brokerage_data <- inputs$brokerage_data[0, ]

    inputs$comdinheiro_data$price[
      inputs$comdinheiro_data$date == as.Date("2026-04-23") &
        inputs$comdinheiro_data$cvm_code_type == "AAA3"
    ] <- 5.05

    inputs$comdinheiro_data$ret_1d[
      inputs$comdinheiro_data$date == as.Date("2026-04-23") &
        inputs$comdinheiro_data$cvm_code_type == "AAA3"
    ] <- 0

    inputs$comdinheiro_data$event_factor[
      inputs$comdinheiro_data$date == as.Date("2026-04-23") &
        inputs$comdinheiro_data$cvm_code_type == "AAA3"
    ] <- 0.5

    inputs$comdinheiro_data$n_shares[
      inputs$comdinheiro_data$date == as.Date("2026-04-23") &
        inputs$comdinheiro_data$cvm_code_type == "AAA3"
    ] <- 2000000

    inputs$split_inplit_data <- data.frame(
      date = as.Date("2026-04-23"),
      legacy_ticker = "AAA3",
      cvm_code_type = "AAA3",
      split_factor = 0.5,
      stringsAsFactors = FALSE
    )

    out <- testthat::expect_warning(
      call_evolve(inputs),
      regexp = NA
    )

    real_aaa <- out$real$portfolio[out$real$portfolio$cvm_code_type == "AAA3", ]

    testthat::expect_equal(real_aaa$bop_positions_before_split, 100)
    testthat::expect_equal(real_aaa$position_factor, 2)
    testthat::expect_equal(real_aaa$bop_positions, 200)
    testthat::expect_equal(nrow(out$diagnostics$unconfirmed_split_candidates), 0L)
    testthat::expect_equal(out$diagnostics$split_candidates$warning_level, "high")
    testthat::expect_equal(nrow(out$real$splits), 1L)
  })

  testthat::test_that("paper and real outputs have aligned dates and ids", {
    inputs <- build_valid_integration_inputs()
    inputs$brokerage_data <- inputs$brokerage_data[0, ]

    out <- call_evolve(inputs)

    testthat::expect_equal(sort(unique(out$paper$returns$date)), inputs$current_dates)
    testthat::expect_equal(sort(unique(out$real$returns$date)), inputs$current_dates)

    testthat::expect_true(all(out$paper$returns$id == "strategy_FIA"))
    testthat::expect_true(all(out$real$returns$id == "strategy_FIA"))
    testthat::expect_true(all(out$real$returns$fund_name == "sicoob_acoes"))

    testthat::expect_equal(nrow(out$paper$returns), length(inputs$current_dates))
    testthat::expect_equal(nrow(out$real$returns), length(inputs$current_dates))
    testthat::expect_equal(nrow(out$paper$market_value), length(inputs$current_dates))
    testthat::expect_equal(nrow(out$real$market_value), length(inputs$current_dates))
  })

  testthat::test_that("final EOP portfolio tables can be used to build next old_portfolio continuation state", {
    inputs <- build_valid_integration_inputs()

    # This test validates continuation-state construction, not broker-trade ingestion.
    # Non-YMF portfolios fabricate trades from rebal_weights, so broker rows must be empty.
    inputs$brokerage_data <- inputs$brokerage_data[0, ]

    out <- call_evolve(inputs)

    last_date <- max(inputs$current_dates)

    next_old_portfolio <- list(
      paper = list(
        portfolio = out$paper$portfolio %>%
          dplyr::filter(date == !!last_date) %>%
          dplyr::select(
            date,
            id,
            legacy_ticker,
            cvm_code_type,
            eop_weights
          )
      ),
      real = list(
        portfolio = out$real$portfolio %>%
          dplyr::filter(date == !!last_date) %>%
          dplyr::select(
            date,
            id,
            fund_name,
            legacy_ticker,
            cvm_code_type,
            eop_positions,
            price
          )
      )
    )

    testthat::expect_true(
      all(c("date", "id", "cvm_code_type", "eop_weights") %in%
            names(next_old_portfolio$paper$portfolio))
    )

    testthat::expect_true(
      all(c("date", "id", "fund_name", "cvm_code_type", "eop_positions", "price") %in%
            names(next_old_portfolio$real$portfolio))
    )

    testthat::expect_false("weights" %in% names(next_old_portfolio$paper$portfolio))
    testthat::expect_false("positions" %in% names(next_old_portfolio$real$portfolio))

    testthat::expect_equal(
      sum(next_old_portfolio$paper$portfolio$eop_weights),
      1,
      tolerance = 1e-12
    )

    testthat::expect_true(
      all(next_old_portfolio$real$portfolio$eop_positions >= 0)
    )

    testthat::expect_true(
      all(next_old_portfolio$real$portfolio$price > 0)
    )
  })

  testthat::test_that("second chained call accepts continuation state from first call", {
    inputs <- build_valid_integration_inputs()

    # This test validates continuation-state reuse, not broker-trade ingestion.
    # Non-YMF portfolios fabricate trades from rebal_weights, so broker rows must be empty.
    inputs$brokerage_data <- inputs$brokerage_data[0, ]

    first_out <- call_evolve(inputs)

    last_date <- max(inputs$current_dates)

    next_old_portfolio <- list(
      paper = list(
        portfolio = first_out$paper$portfolio %>%
          dplyr::filter(date == !!last_date) %>%
          dplyr::select(
            date,
            id,
            legacy_ticker,
            cvm_code_type,
            eop_weights
          )
      ),
      real = list(
        portfolio = first_out$real$portfolio %>%
          dplyr::filter(date == !!last_date) %>%
          dplyr::select(
            date,
            id,
            fund_name,
            legacy_ticker,
            cvm_code_type,
            eop_positions,
            price
          )
      )
    )

    second_inputs <- build_valid_integration_inputs()

    second_inputs$old_portfolio <- next_old_portfolio
    second_inputs$current_dates <- as.Date("2026-04-25")

    second_inputs$comdinheiro_data <- dplyr::bind_rows(
      inputs$comdinheiro_data,
      data.frame(
        date = rep(as.Date("2026-04-25"), each = 2),
        legacy_ticker = c("AAA3", "BBB4"),
        cvm_code_type = c("AAA3", "BBB4"),
        ret_1d = c(0.10, -0.10),
        price = c(10.1606505, 19.8197604),
        proventos = 0,
        proventos_date = as.Date(NA),
        event_factor = 1,
        n_shares = c(1000000, 2000000),
        stringsAsFactors = FALSE
      )
    )

    second_inputs$rebal_weights <- data.frame(
      date = as.Date("2026-04-25"),
      id = "strategy_FIA",
      legacy_ticker = c("AAA3", "BBB4"),
      cvm_code_type = c("AAA3", "BBB4"),
      weights = c(0.55, 0.45),
      stringsAsFactors = FALSE
    )

    second_inputs$brokerage_data <- second_inputs$brokerage_data[0, ]

    second_inputs$transaction_costs_bps <- data.frame(
      date = as.Date("2026-04-25"),
      id = "strategy_FIA",
      transaction_cost_bps = 0,
      stringsAsFactors = FALSE
    )

    second_out <- call_evolve(second_inputs)

    testthat::expect_equal(second_out$workflow$old_port_last_date, as.Date("2026-04-24"))
    testthat::expect_equal(second_out$workflow$current_dates, as.Date("2026-04-25"))
    testthat::expect_equal(nrow(second_out$paper$returns), 1L)
    testthat::expect_equal(nrow(second_out$real$returns), 1L)

    testthat::expect_false("weights" %in% names(next_old_portfolio$paper$portfolio))
    testthat::expect_false("positions" %in% names(next_old_portfolio$real$portfolio))
    testthat::expect_true("eop_weights" %in% names(next_old_portfolio$paper$portfolio))
    testthat::expect_true("eop_positions" %in% names(next_old_portfolio$real$portfolio))
  })

  testthat::test_that("integration fabricates real trades from rebal_weights for non-YMF portfolio", {
    inputs <- build_valid_integration_inputs()

    inputs$id <- "strategy_FIA"
    inputs$old_portfolio$paper$portfolio$id <- "strategy_FIA"
    inputs$old_portfolio$real$portfolio$id <- "strategy_FIA"
    inputs$rebal_weights$id <- "strategy_FIA"
    inputs$transaction_costs_bps$id <- "strategy_FIA"

    inputs$brokerage_data <- inputs$brokerage_data[0, ]

    inputs$current_dates <- as.Date("2026-04-23")
    inputs$rebal_weights <- data.frame(
      date = as.Date("2026-04-23"),
      id = "strategy_FIA",
      legacy_ticker = c("AAA3", "BBB4"),
      cvm_code_type = c("AAA3", "BBB4"),
      weights = c(0.50, 0.50),
      stringsAsFactors = FALSE
    )
    inputs$transaction_costs_bps <- inputs$transaction_costs_bps[
      inputs$transaction_costs_bps$date == as.Date("2026-04-23"),
    ]

    out <- call_evolve(inputs)

    old_positions <- inputs$old_portfolio$real$portfolio %>%
      dplyr::select(cvm_code_type, eop_positions)

    price_tbl <- inputs$comdinheiro_data %>%
      dplyr::filter(date == as.Date("2026-04-23")) %>%
      dplyr::select(cvm_code_type, price)

    target_tbl <- inputs$rebal_weights %>%
      dplyr::select(cvm_code_type, legacy_ticker, target_weight = weights)

    current_aum <- old_positions %>%
      dplyr::left_join(price_tbl, by = "cvm_code_type") %>%
      dplyr::summarise(aum = sum(eop_positions * price), .groups = "drop") %>%
      dplyr::pull(aum)

    expected_trades <- target_tbl %>%
      dplyr::left_join(old_positions, by = "cvm_code_type") %>%
      dplyr::left_join(price_tbl, by = "cvm_code_type") %>%
      dplyr::mutate(
        eop_positions = dplyr::coalesce(eop_positions, 0),
        lot_size = 100,
        target_market_value = current_aum * target_weight,
        target_position_raw = target_market_value / price,
        target_position = base::round(target_position_raw / lot_size) * lot_size,
        net_trade = target_position - eop_positions,
        net_traded_volume = net_trade * price,
        brokerage_fee_estimated = 0,
        avg_trade_price = dplyr::if_else(abs(net_trade) > 1e-8, price, 0)
      ) %>%
      dplyr::filter(abs(net_trade) > 1e-8) %>%
      dplyr::select(
        cvm_code_type,
        net_trade,
        net_traded_volume,
        brokerage_fee_estimated,
        avg_trade_price
      ) %>%
      dplyr::arrange(cvm_code_type)

    observed_trades <- out$real$trades %>%
      dplyr::select(
        cvm_code_type,
        net_trade,
        net_traded_volume,
        brokerage_fee_estimated,
        avg_trade_price
      ) %>%
      dplyr::arrange(cvm_code_type)

    testthat::expect_equal(observed_trades$cvm_code_type, expected_trades$cvm_code_type)
    testthat::expect_equal(observed_trades$net_trade, expected_trades$net_trade, tolerance = 1e-12)
    testthat::expect_equal(observed_trades$net_traded_volume, expected_trades$net_traded_volume, tolerance = 1e-12)
    testthat::expect_equal(observed_trades$brokerage_fee_estimated, expected_trades$brokerage_fee_estimated, tolerance = 1e-12)
    testthat::expect_equal(observed_trades$avg_trade_price, expected_trades$avg_trade_price, tolerance = 1e-12)
  })

  testthat::test_that("integration fabricates buys and sells when target universe changes across rebalances", {
    inputs <- build_valid_integration_inputs()

    inputs$id <- "strategy_FIA"
    inputs$old_portfolio$paper$portfolio$id <- "strategy_FIA"
    inputs$old_portfolio$real$portfolio$id <- "strategy_FIA"
    inputs$transaction_costs_bps$id <- "strategy_FIA"

    inputs$brokerage_data <- inputs$brokerage_data[0, ]

    inputs$current_dates <- as.Date(c("2026-04-23", "2026-04-24"))

    inputs$rebal_weights <- data.frame(
      date = rep(as.Date(c("2026-04-23", "2026-04-24")), each = 2),
      id = rep("strategy_FIA", 4),
      legacy_ticker = c("AAA3", "BBB4", "AAA3", "CCC3"),
      cvm_code_type = c("AAA3", "BBB4", "AAA3", "CCC3"),
      weights = c(0.50, 0.50, 0.40, 0.60),
      stringsAsFactors = FALSE
    )

    extra_comdinheiro <- data.frame(
      date = as.Date(c("2026-04-22", "2026-04-23", "2026-04-24")),
      legacy_ticker = "CCC3",
      cvm_code_type = "CCC3",
      ret_1d = c(0, 2, 1),
      price = c(5.00, 5.10, 5.151),
      proventos = 0,
      proventos_date = as.Date(NA),
      event_factor = 1,
      n_shares = 1000000,
      stringsAsFactors = FALSE
    )

    inputs$comdinheiro_data <- dplyr::bind_rows(
      inputs$comdinheiro_data,
      extra_comdinheiro
    )

    out <- call_evolve(inputs)

    trades_day_2 <- out$real$trades %>%
      dplyr::filter(date == as.Date("2026-04-24"))

    bbb_trade <- trades_day_2 %>%
      dplyr::filter(cvm_code_type == "BBB4")

    ccc_trade <- trades_day_2 %>%
      dplyr::filter(cvm_code_type == "CCC3")

    ccc_eop <- out$real$positions$eop_positions %>%
      dplyr::filter(
        date == as.Date("2026-04-24"),
        cvm_code_type == "CCC3"
      )

    testthat::expect_equal(nrow(bbb_trade), 1L)
    testthat::expect_equal(nrow(ccc_trade), 1L)
    testthat::expect_lt(bbb_trade$net_trade, 0)
    testthat::expect_gt(ccc_trade$net_trade, 0)
    testthat::expect_gt(ccc_eop$positions, 0)
  })

  testthat::test_that("integration blocks new target asset without Comdinheiro price support", {
    inputs <- build_valid_integration_inputs()

    inputs$id <- "strategy_FIA"
    inputs$old_portfolio$paper$portfolio$id <- "strategy_FIA"
    inputs$old_portfolio$real$portfolio$id <- "strategy_FIA"
    inputs$transaction_costs_bps$id <- "strategy_FIA"

    inputs$brokerage_data <- inputs$brokerage_data[0, ]

    inputs$current_dates <- as.Date("2026-04-23")

    inputs$rebal_weights <- data.frame(
      date = as.Date("2026-04-23"),
      id = "strategy_FIA",
      legacy_ticker = c("AAA3", "MISSING3"),
      cvm_code_type = c("AAA3", "MISSING3"),
      weights = c(0.50, 0.50),
      stringsAsFactors = FALSE
    )

    inputs$transaction_costs_bps <- inputs$transaction_costs_bps[
      inputs$transaction_costs_bps$date == as.Date("2026-04-23"),
    ]

    testthat::expect_error(
      call_evolve(inputs),
      regexp = "Missing|price|lookup"
    )
  })

  testthat::test_that("YMF portfolios do not fabricate trades when brokerage_data is empty", {
    inputs <- build_valid_integration_inputs()

    inputs$id <- "YMF_29"
    inputs$old_portfolio$paper$portfolio$id <- "YMF_29"
    inputs$old_portfolio$real$portfolio$id <- "YMF_29"
    inputs$rebal_weights$id <- "YMF_29"
    inputs$transaction_costs_bps$id <- "YMF_29"

    inputs$brokerage_data <- inputs$brokerage_data[0, ]

    out <- call_evolve(inputs)

    testthat::expect_equal(nrow(out$real$trades), 0L)
  })

  testthat::test_that("old_portfolio must end on the latest available date before current_dates", {
    inputs <- build_valid_integration_inputs()

    # This test isolates old-portfolio date consistency. The synthetic-trade branch
    # is allowed to fabricate trades from rebal_weights, so broker trades are
    # removed to avoid testing two independent mechanisms at once.
    inputs$brokerage_data <- inputs$brokerage_data[0, ]

    out <- call_evolve(inputs)

    testthat::expect_equal(
      out$workflow$old_port_last_date,
      max(inputs$comdinheiro_data$date[inputs$comdinheiro_data$date < min(inputs$current_dates)])
    )

    inputs_stale <- build_valid_integration_inputs()
    inputs_stale$brokerage_data <- inputs_stale$brokerage_data[0, ]

    extra_market_date <- data.frame(
      date = rep(as.Date("2026-04-22"), 2),
      legacy_ticker = c("AAA3", "BBB4"),
      cvm_code_type = c("AAA3", "BBB4"),
      ret_1d = c(0.25, -0.25),
      price = c(10.025, 19.95),
      proventos = 0,
      proventos_date = as.Date(NA),
      event_factor = 1,
      n_shares = c(1000000, 2000000),
      stringsAsFactors = FALSE
    )

    inputs_stale$old_portfolio$paper$portfolio$date <- as.Date("2026-04-21")
    inputs_stale$old_portfolio$real$portfolio$date <- as.Date("2026-04-21")

    inputs_stale$comdinheiro_data <- dplyr::bind_rows(
      inputs_stale$comdinheiro_data[
        inputs_stale$comdinheiro_data$date != as.Date("2026-04-22"),
      ],
      extra_market_date
    ) %>%
      dplyr::arrange(date, cvm_code_type)

    testthat::expect_error(
      call_evolve(inputs_stale),
      "`current_dates` must start exactly on the first available market date after `old_portfolio` last date.",
      fixed = TRUE
    )

    inputs_overlapping <- build_valid_integration_inputs()
    inputs_overlapping$brokerage_data <- inputs_overlapping$brokerage_data[0, ]

    inputs_overlapping$old_portfolio$paper$portfolio$date <- as.Date("2026-04-23")
    inputs_overlapping$old_portfolio$real$portfolio$date <- as.Date("2026-04-23")

    testthat::expect_error(
      call_evolve(inputs_overlapping),
      "`old_portfolio` last date must be strictly before `min(current_dates)`.",
      fixed = TRUE
    )
  })

  invisible(TRUE)
}

#Data Quality tests-------------------------------------------------------------
run_test_evolved_portfolios_quality <- function(
    evolved_portfolios,
    newest_rebal_portfolio_ids,
    rebal_weights,
    comdinheiro_data,
    default_lot_size = 100,
    etf_lot_size = 1,
    etf_tickers = c("BOVA11", "BOVV11", "SMLL11", "DIVO11", "LFTS11", "ISUS11"),
    position_tolerance = 1e-8,
    weight_tolerance = 1e-2,
    synthetic_trade_tolerance = 1e-8,
    max_abs_return_gap = 0.05
) {

  check_required_columns <- function(df, required_cols, object_name) {
    missing_cols <- base::setdiff(required_cols, names(df))

    if (length(missing_cols) > 0L) {
      stop(
        object_name,
        " is missing required column(s): ",
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
      stop(
        object_name,
        " has duplicated key rows. First duplicated key: ",
        paste(
          paste0(
            names(duplicated_keys[1L, key_cols, drop = FALSE]),
            " = ",
            as.character(duplicated_keys[1L, key_cols, drop = FALSE])
          ),
          collapse = ", "
        ),
        ".",
        call. = FALSE
      )
    }

    invisible(TRUE)
  }

  check_finite_no_na <- function(df, cols, object_name) {
    for (col in cols) {
      bad_rows <- which(is.na(df[[col]]) | !is.finite(df[[col]]))

      if (length(bad_rows) > 0L) {
        stop(
          object_name,
          "$",
          col,
          " contains NA or non-finite values. First bad row: ",
          bad_rows[1L],
          ".",
          call. = FALSE
        )
      }
    }

    invisible(TRUE)
  }

  compute_expected_synthetic_trades <- function(
    bop_positions,
    target_weights,
    price_tbl,
    default_lot_size,
    etf_lot_size,
    etf_tickers,
    position_tolerance
  ) {
    trade_base <- dplyr::full_join(
      bop_positions %>%
        dplyr::select(cvm_code_type, bop_positions = positions),
      target_weights %>%
        dplyr::select(cvm_code_type, legacy_ticker, target_weight = weights),
      by = "cvm_code_type"
    ) %>%
      dplyr::mutate(
        bop_positions = dplyr::coalesce(bop_positions, 0),
        target_weight = dplyr::coalesce(target_weight, 0)
      ) %>%
      dplyr::left_join(
        price_tbl %>%
          dplyr::select(cvm_code_type, price),
        by = "cvm_code_type"
      )

    current_aum <- sum(trade_base$bop_positions * trade_base$price, na.rm = TRUE)

    trade_base %>%
      dplyr::mutate(
        lot_size = dplyr::if_else(
          legacy_ticker %in% etf_tickers | cvm_code_type %in% etf_tickers,
          etf_lot_size,
          default_lot_size
        ),
        target_market_value = current_aum * target_weight,
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
      ) %>%
      dplyr::arrange(cvm_code_type)
  }

  testthat::test_that("all newest rebalance portfolio ids are present in evolved_portfolios", {
    missing_portfolio_ids <- base::setdiff(
      newest_rebal_portfolio_ids,
      names(evolved_portfolios)
    )

    extra_portfolio_ids <- base::setdiff(
      names(evolved_portfolios),
      newest_rebal_portfolio_ids
    )

    testthat::expect_equal(missing_portfolio_ids, character())
    testthat::expect_equal(extra_portfolio_ids, character())
  })

  testthat::test_that("all evolved portfolios have the expected top-level schema", {
    for (portfolio_id in names(evolved_portfolios)) {
      out <- evolved_portfolios[[portfolio_id]]

      testthat::expect_true(
        all(c("paper", "real", "workflow", "diagnostics") %in% names(out)),
        info = portfolio_id
      )

      testthat::expect_true(
        all(c("portfolio", "weights", "returns", "market_value", "turnover", "costs", "fees") %in% names(out$paper)),
        info = portfolio_id
      )

      testthat::expect_true(
        all(c("portfolio", "positions", "returns", "market_value", "trades", "splits", "turnover", "costs", "fees") %in% names(out$real)),
        info = portfolio_id
      )
    }
  })

  testthat::test_that("all evolved portfolios have required output columns and no duplicated accounting keys", {
    for (portfolio_id in names(evolved_portfolios)) {
      out <- evolved_portfolios[[portfolio_id]]

      check_required_columns(
        out$paper$portfolio,
        c("date", "id", "fund_name", "legacy_ticker", "cvm_code_type", "bop_weights", "ret_1d", "drifted_weights", "eop_weights"),
        paste0(portfolio_id, "$paper$portfolio")
      )

      check_required_columns(
        out$real$portfolio,
        c(
          "date",
          "id",
          "fund_name",
          "legacy_ticker",
          "cvm_code_type",
          "bop_positions_before_split",
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
        paste0(portfolio_id, "$real$portfolio")
      )

      testthat::expect_false(
        "weights" %in% names(out$paper$portfolio),
        info = paste0(portfolio_id, "$paper$portfolio must not contain legacy `weights`.")
      )

      testthat::expect_false(
        "positions" %in% names(out$real$portfolio),
        info = paste0(portfolio_id, "$real$portfolio must not contain legacy `positions`.")
      )

      check_no_duplicate_keys(
        out$paper$portfolio,
        c("date", "id", "cvm_code_type"),
        paste0(portfolio_id, "$paper$portfolio")
      )

      check_no_duplicate_keys(
        out$real$portfolio,
        c("date", "id", "fund_name", "cvm_code_type"),
        paste0(portfolio_id, "$real$portfolio")
      )
    }
  })

  testthat::test_that("all evolved portfolios have finite numeric accounting values", {
    for (portfolio_id in names(evolved_portfolios)) {
      out <- evolved_portfolios[[portfolio_id]]

      check_finite_no_na(
        out$paper$portfolio,
        c("bop_weights", "drifted_weights", "eop_weights"),
        paste0(portfolio_id, "$paper$portfolio")
      )

      check_finite_no_na(
        out$paper$returns,
        c("raw_return", "net_return"),
        paste0(portfolio_id, "$paper$returns")
      )

      check_finite_no_na(
        out$real$portfolio,
        c(
          "bop_positions_before_split",
          "position_factor",
          "bop_positions",
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
        paste0(portfolio_id, "$real$portfolio")
      )

      check_finite_no_na(
        out$real$returns,
        c("raw_return", "net_return"),
        paste0(portfolio_id, "$real$returns")
      )
    }
  })

  testthat::test_that("paper and real weights sum to one by portfolio date", {
    for (portfolio_id in names(evolved_portfolios)) {
      out <- evolved_portfolios[[portfolio_id]]

      paper_eop_sums <- out$paper$weights$eop_weights %>%
        dplyr::group_by(date, id) %>%
        dplyr::summarise(weight_sum = sum(weights, na.rm = TRUE), .groups = "drop")

      real_eop_sums <- out$real$positions$eop_positions %>%
        dplyr::group_by(date, id, fund_name) %>%
        dplyr::summarise(weight_sum = sum(weights, na.rm = TRUE), .groups = "drop")

      testthat::expect_true(
        all(abs(paper_eop_sums$weight_sum - 1) <= weight_tolerance),
        info = portfolio_id
      )

      testthat::expect_true(
        all(abs(real_eop_sums$weight_sum - 1) <= weight_tolerance),
        info = portfolio_id
      )
    }
  })

  testthat::test_that("current prices in real portfolios are consistent with Comdinheiro current or fallback prices", {
    valid_comdinheiro_prices <- comdinheiro_data %>%
      dplyr::filter(
        is.finite(price),
        price > 0
      ) %>%
      dplyr::select(date, cvm_code_type, price)

    for (portfolio_id in names(evolved_portfolios)) {
      out <- evolved_portfolios[[portfolio_id]]

      real_prices <- out$real$portfolio %>%
        dplyr::select(date, cvm_code_type, observed_price = price)

      fallback_prices <- real_prices %>%
        dplyr::select(date, cvm_code_type) %>%
        dplyr::left_join(
          valid_comdinheiro_prices,
          by = "cvm_code_type",
          relationship = "many-to-many"
        ) %>%
        dplyr::filter(date.y < date.x) %>%
        dplyr::arrange(cvm_code_type, date.x, dplyr::desc(date.y)) %>%
        dplyr::group_by(date.x, cvm_code_type) %>%
        dplyr::slice(1L) %>%
        dplyr::ungroup() %>%
        dplyr::transmute(
          date = date.x,
          cvm_code_type,
          fallback_price = price,
          fallback_price_date = date.y
        )

      expected_prices <- real_prices %>%
        dplyr::left_join(
          valid_comdinheiro_prices %>%
            dplyr::rename(current_price = price),
          by = c("date", "cvm_code_type")
        ) %>%
        dplyr::left_join(
          fallback_prices,
          by = c("date", "cvm_code_type")
        ) %>%
        dplyr::mutate(
          expected_price = dplyr::coalesce(current_price, fallback_price)
        )

      bad_prices <- expected_prices %>%
        dplyr::filter(
          is.na(expected_price) |
            abs(observed_price - expected_price) > 1e-8
        )

      testthat::expect_equal(
        nrow(bad_prices),
        0L,
        info = paste0(portfolio_id, " has prices inconsistent with Comdinheiro current/fallback prices.")
      )
    }
  })

  testthat::test_that("synthetic real trades match rebal_weights on rebalance dates for non-YMF portfolios", {
    synthetic_portfolio_ids <- names(evolved_portfolios)[
      !base::startsWith(names(evolved_portfolios), "YMF_")
    ]

    for (portfolio_id in synthetic_portfolio_ids) {
      out <- evolved_portfolios[[portfolio_id]]

      portfolio_rebal_weights <- rebal_weights %>%
        dplyr::filter(id == portfolio_id)

      if (nrow(portfolio_rebal_weights) == 0L) {
        next
      }

      rebalance_dates <- sort(unique(portfolio_rebal_weights$date))

      for (rebalance_date in rebalance_dates) {
        target_today <- portfolio_rebal_weights %>%
          dplyr::filter(date == rebalance_date)

        bop_today <- out$real$positions$bop_positions %>%
          dplyr::filter(date == rebalance_date)

        price_today <- out$real$portfolio %>%
          dplyr::filter(date == rebalance_date) %>%
          dplyr::select(cvm_code_type, price)

        expected_trades <- compute_expected_synthetic_trades(
          bop_positions = bop_today,
          target_weights = target_today,
          price_tbl = price_today,
          default_lot_size = default_lot_size,
          etf_lot_size = etf_lot_size,
          etf_tickers = etf_tickers,
          position_tolerance = position_tolerance
        )

        observed_trades <- out$real$trades %>%
          dplyr::filter(date == rebalance_date) %>%
          dplyr::select(
            cvm_code_type,
            net_trade,
            net_traded_volume,
            brokerage_fee_estimated,
            avg_trade_price
          ) %>%
          dplyr::arrange(cvm_code_type)

        testthat::expect_equal(
          observed_trades$cvm_code_type,
          expected_trades$cvm_code_type,
          info = paste0(portfolio_id, " / ", rebalance_date)
        )

        testthat::expect_equal(
          observed_trades$net_trade,
          expected_trades$net_trade,
          tolerance = synthetic_trade_tolerance,
          info = paste0(portfolio_id, " / ", rebalance_date)
        )

        testthat::expect_equal(
          observed_trades$net_traded_volume,
          expected_trades$net_traded_volume,
          tolerance = synthetic_trade_tolerance,
          info = paste0(portfolio_id, " / ", rebalance_date)
        )
      }
    }
  })

  testthat::test_that("paper and real returns are reasonably close", {
    for (portfolio_id in names(evolved_portfolios)) {
      out <- evolved_portfolios[[portfolio_id]]

      return_check <- out$paper$returns %>%
        dplyr::select(date, id, paper_raw_return = raw_return, paper_net_return = net_return) %>%
        dplyr::left_join(
          out$real$returns %>%
            dplyr::select(date, id, real_raw_return = raw_return, real_net_return = net_return),
          by = c("date", "id")
        ) %>%
        dplyr::mutate(
          raw_return_gap = abs(paper_raw_return - real_raw_return),
          net_return_gap = abs(paper_net_return - real_net_return)
        )

      testthat::expect_true(
        all(return_check$raw_return_gap <= max_abs_return_gap, na.rm = TRUE),
        info = paste0(portfolio_id, " has large raw-return gap between paper and real portfolios.")
      )

      testthat::expect_true(
        all(return_check$net_return_gap <= max_abs_return_gap, na.rm = TRUE),
        info = paste0(portfolio_id, " has large net-return gap between paper and real portfolios.")
      )
    }
  })

  testthat::test_that("main portfolio tables use EOP state schema without legacy state columns", {
    for (portfolio_id in names(evolved_portfolios)) {
      out <- evolved_portfolios[[portfolio_id]]

      testthat::expect_true(
        "eop_weights" %in% names(out$paper$portfolio),
        info = portfolio_id
      )

      testthat::expect_true(
        "eop_positions" %in% names(out$real$portfolio),
        info = portfolio_id
      )

      testthat::expect_false(
        "weights" %in% names(out$paper$portfolio),
        info = portfolio_id
      )

      testthat::expect_false(
        "positions" %in% names(out$real$portfolio),
        info = portfolio_id
      )

      check_finite_no_na(
        out$paper$portfolio,
        cols = c("eop_weights"),
        object_name = paste0(portfolio_id, "$paper$portfolio")
      )

      check_finite_no_na(
        out$real$portfolio,
        cols = c("eop_positions"),
        object_name = paste0(portfolio_id, "$real$portfolio")
      )
    }
  })

  invisible(TRUE)
}
