load_dados_bronze <- function(current_dates, anbima_holidays, overwrite = FALSE){

  #Check------------------------------------------------------------------------
    ##Check for holidays and exclude them from current_dates
    current_dates <- setdiff(current_dates, anbima_holidays) %>% as.Date()

    ##Exclude weekends (Saturday = 6, Sunday = 0)
    weekdays_num <- as.POSIXlt(current_dates)$wday
    current_dates <- current_dates[!weekdays_num %in% c(0, 6)]

    ##Error if current_dates is empty after removing holidays/weekends
    if (length(current_dates) == 0) {
      stop(
        "All current_dates are holidays or weekends. ",
        "Please provide at least one business day."
      )
    }

  #Load-------------------------------------------------------------------------
  message("Loading data for dates: ", paste(as.character(current_dates), collapse = ", "))
  rebalanceamento_tables <- load_rebalanceamento(max(current_dates))
  comdinheiro_data       <- download_comdinheiro(
    current_dates = current_dates,
    fund_tickers = c("bova11", "bovv11", "smal11", "divo11", "isus11", "lfts11",
                     "31339342000164", "55225565000169", "55225719000112",
                     "55597712000121", "35603568000181", "35603488000126",
                     "11182064000177", "32065814000109", "37569846000157",
                     "12845796000162","35948858000167", "35110510000104",
                     "07882792000114"),
    overwrite = overwrite,
    verbose = TRUE
  )

  brokerage_notes_log   <- download_brokerage_notes_from_outlook(current_dates = current_dates)
  trade_data            <- load_brokerage_notes(current_dates)

  #Return-----------------------------------------------------------------------
  list(
    rebalanceamento_tables = rebalanceamento_tables,
    comdinheiro_data       = comdinheiro_data,
    brokerage_data         = list(
      brokerage_notes_log = brokerage_notes_log,
      trade_data          = trade_data
    )
  )

}


load_rebalanceamento <- function(current_date, etfs = c("BOVA11", "BOVV11", "DIVO11", "SMAL11", "ISUS11")){

  #Helpers----------------------------------------------------------------------
  ##Helpers
  parse_weight <- function(x) {
    if (is.numeric(x)) {
      return(as.numeric(x))
    }

    readr::parse_number(
      as.character(x),
      locale = readr::locale(decimal_mark = ".", grouping_mark = ",")
    )
  }
  validate_weights <- function(df, id, date, tolerance = 1e-6) {
    if (!all(c("legacy_ticker", "weights") %in% names(df))) {
      stop(
        paste0(
          "Portfolio ", id, " at ", date,
          " must have columns legacy_ticker and weights."
        ),
        call. = FALSE
      )
    }

    if (any(is.na(df$legacy_ticker)) || any(df$legacy_ticker == "")) {
      stop(
        paste0("Portfolio ", id, " at ", date, " has missing legacy_ticker values."),
        call. = FALSE
      )
    }

    if (any(is.na(df$weights))) {
      stop(
        paste0("Portfolio ", id, " at ", date, " has missing weights."),
        call. = FALSE
      )
    }

    weight_sum <- sum(df$weights)

    if (!isTRUE(abs(weight_sum - 1) <= tolerance)) {
      stop(
        paste0(
          "Portfolio ", id, " at ", date,
          " has weights summing to ", round(weight_sum, 8),
          ", not 1."
        ),
        call. = FALSE
      )
    }

    df
  }
  read_bbg_block <- function(file_path, sheet, range, id, date, tolerance = 1e-6) {
    raw_df <- readxl::read_excel(
      path = file_path,
      sheet = sheet,
      range = range
    )

    if (ncol(raw_df) < 2) {
      stop(
        paste0("Sheet ", sheet, " / ", id, " does not have at least two columns."),
        call. = FALSE
      )
    }

    df <- raw_df %>%
      dplyr::select(1, 2) %>%
      stats::setNames(c("legacy_ticker", "weights")) %>%
      dplyr::filter(!is.na(legacy_ticker)) %>%
      dplyr::transmute(
        date = as.Date(date),
        id = as.character(id),
        legacy_ticker = as.character(legacy_ticker),
        weights = parse_weight(weights)
      )

    validate_weights(
      df = df,
      id = id,
      date = date,
      tolerance = tolerance
    )
  }
  validate_weights_change_across_dates <- function(rebal_weights, tolerance = 1e-10) {
    portfolio_signatures <- rebal_weights %>%
      dplyr::mutate(
        date = as.Date(date),
        id = as.character(id),
        legacy_ticker = as.character(legacy_ticker),
        weights = as.numeric(weights)
      ) %>%
      dplyr::arrange(id, date, legacy_ticker) %>%
      dplyr::group_by(id, date) %>%
      dplyr::summarise(
        portfolio_signature = paste0(
          legacy_ticker,
          ":",
          round(weights / tolerance) * tolerance,
          collapse = "|"
        ),
        .groups = "drop"
      )

    unchanged_portfolios <- portfolio_signatures %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(
        n_dates = dplyr::n_distinct(date),
        n_unique_portfolios = dplyr::n_distinct(portfolio_signature),
        dates = paste(as.character(date), collapse = ", "),
        .groups = "drop"
      ) %>%
      dplyr::filter(
        n_dates > 1,
        n_unique_portfolios == 1
      )

    if (nrow(unchanged_portfolios) > 0) {
      stop(
        paste0(
          "Some portfolios do not change across rebalancing dates. ",
          "First unchanged id: ",
          unchanged_portfolios$id[1],
          ". Dates checked: ",
          unchanged_portfolios$dates[1],
          "."
        ),
        call. = FALSE
      )
    }

    invisible(TRUE)
  }
  validate_all_rebalancing_dates_present <- function(
    df,
    date_col,
    rebalancing_dates,
    object_name
  ) {
    available_dates <- df %>%
      dplyr::pull({{ date_col }}) %>%
      as.Date() %>%
      unique()

    missing_dates <- as.Date(rebalancing_dates) %>%
      setdiff(available_dates)

    if (length(missing_dates) > 0) {
      stop(
        paste0(
          object_name,
          " is missing the following rebalancing_dates: ",
          paste(as.character(missing_dates), collapse = ", ")
        ),
        call. = FALSE
      )
    }

    invisible(TRUE)
  }
  #Get all rebalancing dates and fles-------------------------------------------
    ##Get all rebalancing dates
    rebalancing_dates <- list.files(here::here("data", "dev", "rebalancing")) %>%
      as.Date(format = "%Y%m%d")

    ##Consider only those < current_date
    rebalancing_dates <- rebalancing_dates[rebalancing_dates < current_date]

    ##Stop if no available date
    if (length(rebalancing_dates) == 0) {
      stop(
        "No rebalancing dates available before current_date. ",
        "Please check the data/rebalancing folder."
      )
    }

    ##Get all the files in rebalancing_dates from the folders
    rebalancing_files <- unlist(lapply(rebalancing_dates, function(date) {
      list.files(here::here("data", "dev", "rebalancing", stringr::str_remove_all(date, "-")),
                 full.names = TRUE)
    }))

  #Read all rebal weight files and combine into a single data frame-------------
    ##Setup
    tolerance <- 1e-3
    weights_csv <- c(
      "all_universe", "alpha_model", "cluster", "mandato", "modelo_final",
      "Pesos Fundos BBG"
    )

    ## Create a data frame with file paths and metadata
    rebalancing_file_index <- purrr::map_dfr(
      rebalancing_dates,
      function(date) {
        folder <- here::here(
          "data",
          "dev",
          "rebalancing",
          stringr::str_remove_all(as.character(date), "-")
        )

        data.frame(
          date = date,
          file_path = list.files(folder, full.names = TRUE),
          stringsAsFactors = FALSE
        )
      }
    ) %>%
      dplyr::mutate(
        file_name = basename(file_path),
        file_ext = tools::file_ext(file_name),
        file_id = tools::file_path_sans_ext(file_name)
      )

    ## Filter for CSV files that match the specified weight file identifiers
    csv_weight_files <- rebalancing_file_index %>%
      dplyr::filter(file_ext == "csv") %>%
      dplyr::filter(stringr::str_detect(file_id, paste0(weights_csv, collapse = "|")))

    ## Read and validate each CSV file, combining results into a single data frame

    csv_weights <- purrr::pmap_dfr(
      list(
        file_path = csv_weight_files$file_path,
        date = csv_weight_files$date,
        id = csv_weight_files$file_id
      ),
      function(file_path, date, id) {
        df <- readr::read_csv(
          file_path,
          show_col_types = FALSE,
          name_repair = "unique_quiet"
        ) %>%
          dplyr::transmute(
            date = as.Date(date),
            id = as.character(id),
            legacy_ticker = as.character(legacy_ticker),
            weights = parse_weight(weights)
          )

        validate_weights(
          df = df,
          id = id,
          date = date,
          tolerance = tolerance
        )
      }
    )
  #Pesos BBG (Implemented)------------------------------------------------------
    ##Define the mapping of sheet suffixes to portfolio identifiers
    bbg_sheet_suffix <- c(
      "pesos VGBLs" = "VGBLs",
      "pesos FIA" = "FIA",
      "pesos smal" = "SMLL",
      "pesos dividendos" = "IDIV",
      "pesos ASG" = "ASG",
      "pesos PREVI" = "PREVI"
    )

    bbg_files <- rebalancing_file_index %>%
      dplyr::filter(file_name == "Pesos Fundos BBG.xlsx")

    ## Read and validate each BBG file, combining results into a single data frame
    bbg_weights <- purrr::pmap_dfr(
      list(
        file_path = bbg_files$file_path,
        date = bbg_files$date
      ),
      function(file_path, date) {
        active_weights <- purrr::imap_dfr(
          bbg_sheet_suffix,
          function(suffix, sheet) {
            read_bbg_block(
              file_path = file_path,
              sheet = sheet,
              range = readxl::cell_cols("A:B"),
              id = paste0("carteira_ativa_", suffix),
              date = date,
              tolerance = tolerance
            )
          }
        )

        bbg_reference_weights <- purrr::imap_dfr(
          bbg_sheet_suffix,
          function(suffix, sheet) {
            range <- if (sheet == "pesos PREVI") {
              readxl::cell_cols("I:J")
            } else {
              readxl::cell_cols("E:F")
            }

            read_bbg_block(
              file_path = file_path,
              sheet = sheet,
              range = range,
              id = paste0("carteira_bbg_", suffix),
              date = date,
              tolerance = tolerance
            )
          }
        )

        dplyr::bind_rows(
          active_weights,
          bbg_reference_weights
        )
      }
    )

  #Combine all------------------------------------------------------------------
    ## Combine CSV and BBG weights, ensuring consistent ordering
    rebal_weights <- dplyr::bind_rows(
      csv_weights,
      bbg_weights
    ) %>%
      dplyr::arrange(date, id, dplyr::desc(weights))

    ##Validate
    validate_weights_change_across_dates(
      rebal_weights = rebal_weights,
      tolerance = 1e-10
    )

  #Store sectors and catalog separately-----------------------------------------
    ##Sectors
    sectors_df <- rebalancing_file_index %>%
      dplyr::filter(file_ext == "csv", file_id == "sectors") %>%
      purrr::pmap_dfr(
        function(date, file_path, file_name, file_ext, file_id) {
          readr::read_csv(
            file_path,
            show_col_types = FALSE
          ) %>%
            dplyr::mutate(
              date = as.Date(date),
              .before = 1
            )
        }
      )

    ##Catalog
    catalog_df <- rebalancing_file_index %>%
      dplyr::filter(file_ext == "csv", file_id == "catalog") %>%
      purrr::pmap_dfr(
        function(date, file_path, file_name, file_ext, file_id) {
          readr::read_csv(
            file_path,
            show_col_types = FALSE
          ) %>%
            dplyr::mutate(
              date = as.Date(date),
              .before = 1
            )
        }
      )

    ##Validate
    sector_tickers <- sectors_df %>%
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
      dplyr::filter(!legacy_ticker %in% c(etfs, "BRL Curncy")) %>%
      dplyr::distinct(date, legacy_ticker) %>%
      dplyr::anti_join(
        sector_tickers,
        by = c("date", "legacy_ticker")
      )

    if (nrow(missing_sector_tickers) > 0) {
      stop(
        paste0(
          "Some non-ETF portfolio legacy_tickers are missing from sectors. ",
          "First missing ticker: ",
          missing_sector_tickers$legacy_ticker[1],
          " at date ",
          missing_sector_tickers$date[1],
          "."
        ),
        call. = FALSE
      )
    }

  #Return all----------------------------------------------------------------------

      ##Check if all rebalancing_dates are contemplated
      purrr::walk(
        list(
          rebal_weights = rebal_weights,
          sectors_df = sectors_df,
          catalog_df = catalog_df
        ),
        ~ validate_all_rebalancing_dates_present(
          df = .x,
          date_col = date,
          rebalancing_dates = rebalancing_dates,
          object_name = deparse(substitute(.x))
        )
      )

      return(list(
        rebal_weights = rebal_weights,
        sectors = sectors_df,
        catalog = catalog_df
      ))




}

download_comdinheiro <- function(
    current_dates,
    output_dir = here::here("data", "dev", "comdinheiro"),
    fund_tickers = c("bova11", "bovv11", "smal11", "divo11", "isus11", "lfts11",
                     "31339342000164", "55225565000169", "55225719000112",
                     "55597712000121", "35603568000181", "35603488000126",
                     "11182064000177", "32065814000109", "37569846000157",
                     "12845796000162","35948858000167", "35110510000104",
                     "07882792000114"),
    tolerance = 1e-8,
    overwrite = FALSE,
    verbose = TRUE
) {
  # Initial setup -------------------------------------------------------------

  current_dates <- as.Date(current_dates)

  if (any(is.na(current_dates))) {
    stop("`current_dates` must be coercible to Date.", call. = FALSE)
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  COMD_USER <- Sys.getenv("COMD_USER")
  COMD_PASS <- Sys.getenv("COMD_PASS")

  if (nchar(COMD_USER) == 0 || nchar(COMD_PASS) == 0) {
    stop(
      "Please set COMD_USER and COMD_PASS environment variables with your ",
      "comdinheiro.com.br credentials.",
      call. = FALSE
    )
  }

  endpoint <- "https://api.comdinheiro.com.br/v1/ep1/import-data"

  # Helpers ------------------------------------------------------------------

  clean_hdr <- function(nm) {
    nm <- gsub("[\r\n]+", " ", nm)
    nm <- gsub("\\s+", " ", nm)
    trimws(nm)
  }

  br_num <- function(x) {
    if (!is.character(x)) {
      return(as.numeric(x))
    }

    v <- trimws(x)
    v[v %in% c("", "nd", "n/d", "NA", "NaN", "-")] <- NA_character_

    is_date <- grepl("^\\d{2}/\\d{2}/\\d{4}$", v)
    cand <- !is_date & grepl("[0-9]", v)

    out <- rep(NA_real_, length(v))

    if (any(cand, na.rm = TRUE)) {
      w <- v[cand]
      w <- gsub("\\s+", "", w)
      w <- gsub("\u2212|\u2013|\u2014", "-", w, useBytes = TRUE)
      w <- sub("^\\((.*)\\)$", "-\\1", w)
      w <- gsub("\\.(?=\\d{3}(\\D|$))", "", w, perl = TRUE)
      w <- gsub(",", ".", w, fixed = TRUE)

      out[cand] <- suppressWarnings(as.numeric(w))
    }

    out
  }

  parse_json3_table <- function(tab) {
    if (!is.list(tab) || is.null(tab$lin0)) {
      stop("Invalid json3 table structure.", call. = FALSE)
    }

    header <- unlist(tab$lin0, use.names = FALSE)
    header <- make.unique(header, sep = "_dup")

    lin_names <- names(tab)
    lin_names <- lin_names[grepl("^lin\\d+$", lin_names)]
    lin_ids <- as.integer(sub("^lin", "", lin_names))
    lin_names <- lin_names[order(lin_ids)]

    row_names <- setdiff(lin_names, "lin0")

    if (length(row_names) == 0) {
      return(data.frame())
    }

    rows_list <- lapply(
      row_names,
      function(nm) {
        unlist(tab[[nm]], use.names = FALSE)
      }
    )

    max_len <- length(header)

    rows_list <- lapply(
      rows_list,
      function(v) {
        if (length(v) < max_len) {
          v <- c(v, rep(NA_character_, max_len - length(v)))
        }

        if (length(v) > max_len) {
          v <- v[seq_len(max_len)]
        }

        v
      }
    )

    mat <- do.call(rbind, rows_list)
    colnames(mat) <- header

    df <- as.data.frame(
      mat,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    df[df == ""] <- NA_character_

    df
  }

  pick_table <- function(x_tables) {
    ids <- names(x_tables)

    for (id in ids) {
      tab <- x_tables[[id]]

      if (!is.null(tab$lin0)) {
        hdr <- unlist(tab$lin0, use.names = FALSE)

        if (any(tolower(hdr) %in% c("data", "date", "ticker"))) {
          return(id)
        }

        if (!is.null(tab$lin1$col0)) {
          first_value <- tab$lin1$col0

          if (grepl("^\\d{2}/\\d{2}/\\d{4}$", first_value)) {
            return(id)
          }
        }
      }
    }

    best_id <- NULL
    best_rows <- -Inf

    for (id in ids) {
      n_rows <- sum(grepl("^lin\\d+$", names(x_tables[[id]]))) - 1L

      if (n_rows > best_rows) {
        best_rows <- n_rows
        best_id <- id
      }
    }

    best_id
  }

  parse_response_table <- function(res) {
    if (is.null(res)) {
      stop("Failed to download after repeated network errors.", call. = FALSE)
    }

    if (!inherits(res, "httr2_response")) {
      stop("Invalid response object.", call. = FALSE)
    }

    txt <- httr2::resp_body_string(res, encoding = "UTF-8")
    x <- try(jsonlite::fromJSON(txt, simplifyVector = FALSE), silent = TRUE)

    if (inherits(x, "try-error")) {
      txt <- httr2::resp_body_string(res, encoding = "ISO-8859-1")
      x <- try(jsonlite::fromJSON(txt, simplifyVector = FALSE), silent = TRUE)
    }

    if (inherits(x, "try-error") || is.null(x$tables) || !length(x$tables)) {
      stop(
        paste0(
          "Could not parse JSON. First 200 chars: ",
          substr(gsub("\\s+", " ", txt), 1, 200)
        ),
        call. = FALSE
      )
    }

    tab_id <- pick_table(x$tables)

    if (is.null(tab_id)) {
      stop("No suitable table found in Comdinheiro response.", call. = FALSE)
    }

    df <- parse_json3_table(x$tables[[tab_id]])

    if (nrow(df) == 0) {
      stop("Parsed table is empty.", call. = FALSE)
    }

    names(df) <- make.unique(clean_hdr(names(df)), sep = "_dup")

    df
  }

  parse_fund_indicadores_wide <- function(
    fund_wide,
    fund_ids,
    fund_aliases = NULL
  ) {
    if (!"Data" %in% names(fund_wide)) {
      names(fund_wide)[1] <- "Data"
    }

    fund_ids_clean <- fund_ids %>%
      as.character() %>%
      stringr::str_to_lower()

    fund_ids_digits <- gsub("[^0-9]", "", fund_ids_clean)

    clean_fund_alias <- function(x) {
      x <- as.character(x)

      x <- iconv(
        x,
        from = "",
        to = "ASCII//TRANSLIT"
      )

      x <- stringr::str_to_lower(x)

      x <- stringr::str_replace_all(
        x,
        "\\b(fundo de investimento|fundo|classe de investimento|em\\s+cotas\\s+de|fi|ficfi|fic|fif)\\b",
        " "
      )

      x <- stringr::str_replace_all(
        x,
        "\\b(finiro|financeiro|financeira|em\\s+acoes|de\\s+acoes|da\\s+cic)\\b",
        " "
      )

      x <- stringr::str_replace_all(
        x,
        "\\b(responsabilidade limitada|responsab|resp limitada|rl)\\b",
        " "
      )

      x <- stringr::str_replace_all(
        x,
        "\\b(mm)\\b",
        " "
      )

      x <- stringr::str_replace_all(
        x,
        "[^a-z0-9]+",
        "_"
      )

      x <- stringr::str_replace_all(
        x,
        "_+",
        "_"
      )

      x <- stringr::str_replace_all(
        x,
        "^_|_$",
        ""
      )

      x
    }

    extract_instrument <- function(col_name) {
      col_name_clean <- clean_hdr(col_name)

      variable <- dplyr::case_when(
        stringr::str_detect(
          stringr::str_to_lower(col_name_clean),
          "retorno"
        ) ~ "ret_1d",
        stringr::str_detect(
          stringr::str_to_lower(col_name_clean),
          "preco_aj|preço_aj|preco aj|preço aj"
        ) ~ "price_adj",
        TRUE ~ NA_character_
      )

      if (is.na(variable)) {
        stop(
          paste0("Could not identify variable from column name: ", col_name),
          call. = FALSE
        )
      }

      cnpj_match <- stringr::str_extract(
        col_name_clean,
        "\\d{2}\\.\\d{3}\\.\\d{3}/\\d{4}-\\d{2}"
      )

      if (!is.na(cnpj_match)) {
        cnpj_digits <- gsub("[^0-9]", "", cnpj_match)

        fund_name <- col_name_clean %>%
          stringr::str_replace(
            paste0("^.*", stringr::fixed(cnpj_match), "\\s*"),
            ""
          )

        if (!is.null(fund_aliases) && cnpj_digits %in% names(fund_aliases)) {
          legacy_ticker <- fund_aliases[[cnpj_digits]]
        } else {
          legacy_ticker <- clean_fund_alias(fund_name)
        }

        return(
          data.frame(
            raw_column = col_name,
            variable = variable,
            legacy_ticker = legacy_ticker,
            instrument_id = cnpj_digits,
            instrument_name = fund_name,
            stringsAsFactors = FALSE
          )
        )
      }

      col_name_lower <- stringr::str_to_lower(col_name_clean)

      is_cnpj_id <- nchar(fund_ids_digits) == 14L

      matched_ticker <- fund_ids_clean[
        fund_ids_clean != "" &
          !is_cnpj_id &
          purrr::map_lgl(
            fund_ids_clean,
            ~ stringr::str_detect(col_name_lower, stringr::fixed(.x))
          )
      ]

      if (length(matched_ticker) != 1L) {
        stop(
          paste0("Could not identify fund/ETF from column name: ", col_name),
          call. = FALSE
        )
      }

      data.frame(
        raw_column = col_name,
        variable = variable,
        legacy_ticker = stringr::str_to_upper(matched_ticker),
        instrument_id = stringr::str_to_upper(matched_ticker),
        instrument_name = stringr::str_to_upper(matched_ticker),
        stringsAsFactors = FALSE
      )
    }

    value_cols <- setdiff(names(fund_wide), "Data")

    col_map <- purrr::map_dfr(
      value_cols,
      extract_instrument
    )

    duplicated_pairs <- col_map %>%
      dplyr::count(legacy_ticker, variable) %>%
      dplyr::filter(n > 1L)

    if (nrow(duplicated_pairs) > 0) {
      stop(
        paste0(
          "Duplicated fund variable columns after parsing: ",
          paste(
            paste0(duplicated_pairs$legacy_ticker, "/", duplicated_pairs$variable),
            collapse = ", "
          )
        ),
        call. = FALSE
      )
    }

    clean_col_names <- paste0(
      col_map$legacy_ticker,
      "__",
      col_map$variable
    )

    names(fund_wide) <- c("Data", clean_col_names)

    fund_wide %>%
      dplyr::mutate(date = as.Date(Data, format = "%d/%m/%Y")) %>%
      dplyr::select(-Data) %>%
      tidyr::pivot_longer(
        cols = -date,
        names_to = c("legacy_ticker", ".value"),
        names_sep = "__"
      ) %>%
      dplyr::mutate(
        legacy_ticker = as.character(legacy_ticker),
        ret_1d = br_num(ret_1d),
        price_adj = br_num(price_adj),
        price = price_adj,
        cia_name = legacy_ticker,
        cvm_code = NA_character_,
        cvm_code_full = NA_character_,
        proventos = NA_real_,
        volume = NA_real_,
        n_shares = NA_real_,
        w_ibov = NA_real_,
        w_idiv = NA_real_,
        w_smll = NA_real_,
        w_ise = NA_real_,
        btc_estoque = NA_real_,
        btc_novos_contratos = NA_real_,
        btc_taxa_media = NA_real_
      ) %>%
      dplyr::select(
        date,
        legacy_ticker,
        cia_name,
        cvm_code,
        cvm_code_full,
        ret_1d,
        proventos,
        price,
        price_adj,
        volume,
        n_shares,
        w_ibov,
        w_idiv,
        w_smll,
        w_ise,
        btc_estoque,
        btc_novos_contratos,
        btc_taxa_media
      )
  }

  backoff_sleep <- function(res, attempt) {
    base_wait <- 10

    wait <- NA_real_

    if (!is.null(res)) {
      retry_after <- httr2::resp_header(res, "retry-after")

      if (
        !is.null(retry_after) &&
        nzchar(retry_after) &&
        !is.na(suppressWarnings(as.numeric(retry_after)))
      ) {
        wait <- as.numeric(retry_after)
      }
    }

    if (is.na(wait)) {
      wait <- min(120, base_wait * 2^(attempt - 1))
    }

    wait <- wait + stats::runif(1, 0, 5)

    if (isTRUE(verbose)) {
      status <- if (!is.null(res)) httr2::resp_status(res) else "network-error"
      message("HTTP ", status, "; backing off ", round(wait, 1), "s.")
    }

    Sys.sleep(wait)
  }

  make_request <- function(rel_url) {
    req <- httr2::request(endpoint) |>
      httr2::req_headers(
        "Content-Type" = "application/x-www-form-urlencoded",
        "User-Agent" = "comdinheiro-r/0.1"
      ) |>
      httr2::req_retry(max_tries = 1) |>
      httr2::req_timeout(3600) |>
      httr2::req_body_form(
        username = COMD_USER,
        password = COMD_PASS,
        URL = rel_url,
        format = "json3"
      )

    ok <- FALSE
    res <- NULL

    for (attempt in seq_len(8L)) {
      res <- try(httr2::req_perform(req), silent = TRUE)

      if (inherits(res, "try-error") || is.null(res)) {
        if (isTRUE(verbose)) {
          message("Network error on attempt ", attempt, ".")
        }

        backoff_sleep(NULL, attempt)
        next
      }

      if (!inherits(res, "httr2_response")) {
        if (isTRUE(verbose)) {
          message("Invalid response object on attempt ", attempt, ".")
        }

        backoff_sleep(NULL, attempt)
        next
      }

      code <- httr2::resp_status(res)

      if (code %in% c(429L, 500L, 502L, 503L, 504L)) {
        backoff_sleep(res, attempt)
        next
      }

      if (code == 200L && !httr2::resp_has_body(res)) {
        if (isTRUE(verbose)) {
          message("HTTP 200 but empty body on attempt ", attempt, ".")
        }

        backoff_sleep(res, attempt)
        next
      }

      if (code == 200L) {
        txt_try <- try(
          httr2::resp_body_string(res, encoding = "UTF-8"),
          silent = TRUE
        )

        json_try <- try(
          jsonlite::fromJSON(txt_try),
          silent = TRUE
        )

        if (inherits(txt_try, "try-error") || inherits(json_try, "try-error")) {
          if (isTRUE(verbose)) {
            message("HTTP 200 but invalid JSON on attempt ", attempt, ".")
          }

          backoff_sleep(res, attempt)
          next
        }

        ok <- TRUE
        break
      }

      stop(
        paste0("Non-retriable HTTP status from Comdinheiro: ", code),
        call. = FALSE
      )
    }

    if (!ok) {
      stop("Giving up after repeated failed Comdinheiro requests.", call. = FALSE)
    }

    res
  }

  set_qp <- function(rel_url, name, value, base = "https://www.comdinheiro.com.br/") {
    rel_url <- sub("\\?&", "?", rel_url, perl = TRUE)
    rel_url <- gsub("&&+", "&", rel_url, perl = TRUE)

    full_url <- paste0(base, rel_url)
    parsed_url <- httr2::url_parse(full_url)

    query <- parsed_url$query

    if (is.null(query)) {
      query <- list()
    }

    bad <- which(is.na(names(query)) | names(query) == "")

    if (length(bad) > 0) {
      query <- query[-bad]
    }

    if (is.null(value)) {
      query[[name]] <- NULL
    } else {
      query[[name]] <- as.character(value)
    }

    parsed_url$query <- query

    out <- httr2::url_build(parsed_url)

    if (startsWith(out, base)) {
      out <- substring(out, nchar(base) + 1L)
    }

    out
  }

  deduplicate_stock_df <- function(stock_df, current_date) {
    duplicated_tickers <- stock_df %>%
      dplyr::count(legacy_ticker) %>%
      dplyr::filter(n > 1)

    if (nrow(duplicated_tickers) == 0) {
      return(stock_df)
    }

    warning(
      paste0(
        "Duplicated stock tickers found in Comdinheiro stock data for ",
        current_date,
        ". Resolving by highest non-NA information count; ",
        "ties prefer rows whose `cvm_code` does not contain 'US:'. ",
        "Duplicated tickers: ",
        paste(duplicated_tickers$legacy_ticker, collapse = ", ")
      ),
      call. = FALSE
    )

    stock_df %>%
      dplyr::mutate(
        non_na_info_count = rowSums(!is.na(dplyr::pick(dplyr::everything()))),
        cvm_code_has_us = stringr::str_detect(
          string = dplyr::coalesce(as.character(cvm_code), ""),
          pattern = "US:"
        )
      ) %>%
      dplyr::arrange(
        legacy_ticker,
        dplyr::desc(non_na_info_count),
        cvm_code_has_us
      ) %>%
      dplyr::group_by(legacy_ticker) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::select(-non_na_info_count, -cvm_code_has_us)
  }

  normalize_stock_columns <- function(df) {
    expected_names <- c(
      "legacy_ticker",
      "cia_name",
      "cvm_code",
      "cvm_code_full",
      "ret_1d",
      "proventos",
      "price",
      "price_adj",
      "volume",
      "n_shares",
      "w_ibov",
      "w_idiv",
      "w_smll",
      "w_ise",
      "btc_estoque",
      "btc_novos_contratos",
      "btc_taxa_media"
    )

    if (ncol(df) < length(expected_names)) {
      stop(
        paste0(
          "Stock screener response has fewer columns than expected. ",
          "Expected at least ", length(expected_names), ", got ", ncol(df), "."
        ),
        call. = FALSE
      )
    }

    df <- df[, seq_along(expected_names), drop = FALSE]
    names(df) <- expected_names

    df %>%
      dplyr::mutate(
        legacy_ticker = as.character(legacy_ticker),
        cia_name = as.character(cia_name),
        cvm_code = as.character(cvm_code),
        cvm_code_full = as.character(cvm_code_full),
        dplyr::across(
          .cols = c(
            ret_1d,
            proventos,
            price,
            price_adj,
            volume,
            n_shares,
            w_ibov,
            w_idiv,
            w_smll,
            w_ise,
            btc_estoque,
            btc_novos_contratos,
            btc_taxa_media
          ),
          .fns = br_num
        )
      )
  }

  validate_stock_df <- function(df, date) {
    required_cols <- c(
      "legacy_ticker",
      "cia_name",
      "cvm_code",
      "cvm_code_full",
      "ret_1d",
      "proventos",
      "price",
      "price_adj",
      "volume",
      "n_shares",
      "w_ibov",
      "w_idiv",
      "w_smll",
      "w_ise",
      "btc_estoque",
      "btc_novos_contratos",
      "btc_taxa_media"
    )

    missing_cols <- setdiff(required_cols, names(df))

    if (length(missing_cols) > 0) {
      stop(
        paste0(
          "Stock data for ",
          date,
          " is missing columns: ",
          paste(missing_cols, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    if (any(is.na(df$legacy_ticker)) || any(df$legacy_ticker == "")) {
      stop("Stock data has missing `legacy_ticker` values.", call. = FALSE)
    }

    if (anyDuplicated(df$legacy_ticker) > 0) {
      stop("Stock data has duplicated `legacy_ticker` values.", call. = FALSE)
    }

    invisible(TRUE)
  }

  validate_final_date_df <- function(df, current_date) {
    required_cols <- c(
      "date",
      "legacy_ticker",
      "cia_name",
      "cvm_code",
      "cvm_code_full",
      "ret_1d",
      "proventos",
      "price",
      "price_adj",
      "volume",
      "n_shares",
      "w_ibov",
      "w_idiv",
      "w_smll",
      "w_ise",
      "btc_estoque",
      "btc_novos_contratos",
      "btc_taxa_media"
    )

    missing_cols <- setdiff(required_cols, names(df))

    if (length(missing_cols) > 0) {
      stop(
        paste0(
          "Final data for ",
          current_date,
          " is missing columns: ",
          paste(missing_cols, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    df <- df %>%
      dplyr::mutate(date = as.Date(date))

    if (!identical(unique(df$date), as.Date(current_date))) {
      stop("Final data has inconsistent `date` values.", call. = FALSE)
    }

    fully_na_cols <- df %>%
      dplyr::summarise(
        dplyr::across(
          dplyr::everything(),
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
          "Final data for ",
          current_date,
          " has columns with only NAs: ",
          paste(fully_na_cols, collapse = ", "),
          ". This suggests full data unavailability for these fields."
        ),
        call. = FALSE
      )
    }

    if (anyDuplicated(df$legacy_ticker) > 0) {
      stop(
        paste0(
          "Final data for ",
          current_date,
          " has duplicated `legacy_ticker` values."
        ),
        call. = FALSE
      )
    }

    invisible(TRUE)
  }

  parse_stock_return_fallback_wide <- function(return_wide, fallback_tickers) {
    if (!"Data" %in% names(return_wide)) {
      names(return_wide)[1] <- "Data"
    }

    fallback_tickers <- stringr::str_to_upper(fallback_tickers)

    value_cols <- setdiff(names(return_wide), "Data")

    clean_col_names <- purrr::map_chr(
      value_cols,
      function(col_name) {
        col_name_upper <- stringr::str_to_upper(col_name)

        ticker <- fallback_tickers[
          purrr::map_lgl(
            fallback_tickers,
            ~ stringr::str_detect(col_name_upper, stringr::fixed(.x))
          )
        ]

        if (length(ticker) != 1L) {
          stop(
            paste0("Could not identify fallback ticker from column name: ", col_name),
            call. = FALSE
          )
        }

        paste0(ticker, "__ret_1d_fallback")
      }
    )

    names(return_wide) <- c("Data", clean_col_names)

    return_wide %>%
      dplyr::mutate(date = as.Date(Data, format = "%d/%m/%Y")) %>%
      dplyr::select(-Data) %>%
      tidyr::pivot_longer(
        cols = -date,
        names_to = c("legacy_ticker", ".value"),
        names_sep = "__"
      ) %>%
      dplyr::mutate(
        legacy_ticker = as.character(legacy_ticker),
        ret_1d_fallback = 100 * br_num(ret_1d_fallback)
      )
  }

  # URLs ---------------------------------------------------------------------

  stock_url <- paste0(
    "StockScreenerFull.php?",
    "&relat=",
    "&data_analise=25/05/2026",
    "&data_dem=31/12/9999",
    "&variaveis=TICKER+NOME_EMPRESA+CODIGO_CVM+codigo_cvm_full+ret_01d+",
    "PROVENTOS(01d,,,A1,C,todos)+PRECO+PRECO_AJ(,,,A,C)+VOLUME_DIA+",
    "QUANT_ACAO(,TOTAL_OUT,)+",
    "PESO_INDICE(participacao,IBOVESPA,,,)+",
    "PESO_INDICE(participacao,IDIV,,,)+",
    "PESO_INDICE(participacao,SMLL,,,)+",
    "PESO_INDICE(participacao,ISE,,,)+",
    "BTC_ALUGUEL_ACOES(TA,01d,,91)+",
    "BTC_ALUGUEL_ACOES(NAA,01d,,91)+",
    "BTC_ALUGUEL_ACOES(TMEDD,01d,,91)",
    "&segmento=todos",
    "&setor=todos",
    "&filtro=VOLUME_MEDIO(03m,,)+%3E+1",
    "&demonstracao=consolidado%20preferencialmente",
    "&convencao=MIXED",
    "&acumular=12",
    "&valores_em=1",
    "&num_casas=2",
    "&salve=",
    "&salve_obs=",
    "&opcao_salvar=nenhum",
    "&opcao_portfolio=quantidade",
    "&opcao_serie=cash",
    "&indicador_pesos_portfolio=",
    "&data_analise_portfolio=25/05/2026",
    "&var_control=0",
    "&overwrite=0",
    "&setor_bov=todos",
    "&subsetor_bov=todos",
    "&subsubsetor_bov=todos",
    "&group_by=",
    "&relat_alias_automatico=cmd_alias_01",
    "&primeira_coluna_ticker=0",
    "&periodos=0",
    "&periodicidade=anual",
    "&formato_data=1",
    "&tipo_on_pn=todos",
    "&tipo_listagem=listada_em_bolsa",
    "&casos_especiais_01=nenhum",
    "&casos_especiais_02=",
    "&pais_origem=BR",
    "&exchange=B3",
    "&limit_01=",
    "&order_by=",
    "&moeda=MOEDA_ORIGINAL",
    "&nome_serie=",
    "&republicacoes=0",
    "&linppag=50"
  )

  fund_base_url <- paste0(
    "HistoricoIndicadoresFundos001.php?",
    "&cnpjs=bova11+bovv11+smal11+divo11+isus11+lfts11+",
    "31339342000164+55225565000169+55225719000112+55597712000121+35603568000181+",
    "35603488000126+11182064000177+32065814000109+37569846000157+12845796000162+",
    "35948858000167+35110510000104+07882792000114",
    "&data_ini=27012026",
    "&data_fim=19052026",
    "&indicadores=ret_01d+PRECO_AJ(,,,A,C)",
    "&op01=tabela_h",
    "&num_casas=2",
    "&enviar_email=0",
    "&periodicidade=diaria",
    "&cabecalho_excel=modo2",
    "&transpor=0",
    "&asc_desc=desc",
    "&tipo_grafico=linha",
    "&relat_alias_automatico=cmd_alias_01"
  )

  return_base_url <- paste0(
    "HistoricoCotacao002.php?",
    "&x=PETR4+LEVE3+MYPK3",
    "&data_ini=27012026",
    "&data_fim=19052026",
    "&pagina=1",
    "&d=MOEDA_ORIGINAL",
    "&g=0",
    "&m=0",
    "&info_desejada=retorno",
    "&retorno=discreto",
    "&tipo_data=du_br",
    "&tipo_ajuste=todosajustes",
    "&num_casas=2",
    "&enviar_email=0",
    "&ordem_legenda=1",
    "&cabecalho_excel=modo1",
    "&classes_ativos=fklk448oj5v5r",
    "&ordem_data=0",
    "&rent_acum=rent_acum",
    "&minY=",
    "&maxY=",
    "&deltaY=",
    "&preco_nd_ant=0",
    "&base_num_indice=100",
    "&flag_num_indice=0",
    "&eixo_x=Data",
    "&startX=0",
    "&max_list_size=20",
    "&line_width=2",
    "&titulo_grafico=",
    "&legenda_eixoy=",
    "&tipo_grafico=line",
    "&script=",
    "&tooltip=unica"
  )

  # Check cache
  cached_paths <- file.path(
    output_dir,
    paste0("comdinheiro_market_", format(current_dates, "%Y%m%d"), ".csv")
  )

  all_cached <- all(file.exists(cached_paths)) && !isTRUE(overwrite)

  if (isTRUE(all_cached)) {

    cached_out <- purrr::map_dfr(
      cached_paths,
      function(file_path) {
        existing_df <- readr::read_csv(
          file_path,
          show_col_types = FALSE
        ) %>%
          dplyr::mutate(date = as.Date(date))

        expected_date <- as.Date(
          sub(
            "^comdinheiro_market_(\\d{8})\\.csv$",
            "\\1",
            basename(file_path)
          ),
          format = "%Y%m%d"
        )

        validate_final_date_df(existing_df, expected_date)

        existing_df
      }
    )

    if (isTRUE(verbose)) {
      message("Valid cached Comdinheiro files found for all current_dates. Skipping all downloads.")
    }

    return(cached_out)
  }

  # fund download --------------------------------------------------------------
  date_begin <- min(current_dates)
  date_end <- max(current_dates)

  fund_cnpjs <- paste(stringr::str_to_lower(fund_tickers), collapse = "+")

  fund_url <- fund_base_url %>%
    set_qp("cnpjs", fund_cnpjs) %>%
    set_qp("data_ini", format(date_begin, "%d%m%Y")) %>%
    set_qp("data_fim", format(date_end, "%d%m%Y")) %>%
    set_qp("indicadores", "ret_01d+PRECO_AJ(,,,A,C)")

  if (isTRUE(verbose)) {
    message("Downloading fund returns and adjusted prices from ", date_begin, " to ", date_end, ".")
  }

  fund_wide <- make_request(fund_url) %>%
    parse_response_table()

  fund_aliases <- c(
    "31339342000164" = "sicoob_acoes",
    "55225565000169" = "sicoob_small_caps",
    "55225719000112" = "sicoob_dividendos",
    "55597712000121" = "sicoob_asg_is",
    "35603568000181" = "vgbl_sicoob_seguradora_rv_30",
    "35603488000126" = "vgbl_sicoob_seguradora_rv_65",
    "11182064000177" = "constancia_fundamento",
    "32065814000109" = "avantgarde_multifatores",
    "37569846000157" = "az_quest_bayes_sistematico",
    "12845796000162" = "kadima_equities",
    "35948858000167" = "v8_veyron",
    "35110510000104" = "itau_smart_acoes_brasil_50",
    "07882792000114" = "bb_acoes_selecao_fatorial"
  )


  fund_df <- parse_fund_indicadores_wide(
    fund_wide = fund_wide,
    fund_ids = fund_tickers
  )

  missing_fund_dates <- setdiff(current_dates, unique(fund_df$date))

  if (length(missing_fund_dates) > 0) {
    stop(
      paste0(
        "fund data is missing dates: ",
        paste(as.character(missing_fund_dates), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Stock download + per-date consolidation ----------------------------------

  out <- purrr::map_dfr(
    current_dates,
    function(current_date) {
      file_path <- file.path(
        output_dir,
        paste0("comdinheiro_market_", format(current_date, "%Y%m%d"), ".csv")
      )

      if (file.exists(file_path) && !isTRUE(overwrite)) {
        existing_df <- readr::read_csv(
          file_path,
          show_col_types = FALSE
        ) %>%
          dplyr::mutate(date = as.Date(date))

        validate_final_date_df(existing_df, current_date)

        if (isTRUE(verbose)) {
          message("Valid cached file found for ", current_date, ". Skipping download.")
        }

        return(existing_df)
      }

      if (isTRUE(verbose)) {
        message("Downloading stock data for ", current_date, ".")
      }

      current_date_cmd <- format(current_date, "%d/%m/%Y")

      stock_url_i <- stock_url %>%
        set_qp("data_analise", current_date_cmd) %>%
        set_qp("data_analise_portfolio", current_date_cmd)

      stock_df <- make_request(stock_url_i) %>%
        parse_response_table() %>%
        normalize_stock_columns() %>%
        deduplicate_stock_df(current_date = current_date)

      validate_stock_df(stock_df, current_date)

      stock_df <- stock_df %>%
        dplyr::mutate(
          date = as.Date(current_date),
          .before = 1
        )

      ##Fallback for missing tickers
      missing_return_tickers <- stock_df %>%
        dplyr::filter(is.na(ret_1d)) %>%
        dplyr::pull(legacy_ticker) %>%
        unique() %>%
        stats::na.omit() %>%
        as.character()

      if (length(missing_return_tickers) > 0) {
        if (isTRUE(verbose)) {
          message(
            "Trying fallback return download for ",
            length(missing_return_tickers),
            " tickers on ",
            current_date,
            ": ",
            paste(missing_return_tickers, collapse = ", "),
            "."
          )
        }

        fallback_tickers_query <- paste(missing_return_tickers, collapse = "+")

        return_url_i <- return_base_url %>%
          set_qp("x", fallback_tickers_query) %>%
          set_qp("data_ini", format(date_begin, "%d%m%Y")) %>%
          set_qp("data_fim", format(date_end, "%d%m%Y"))

        fallback_return_df <- make_request(return_url_i) %>%
          parse_response_table() %>%
          parse_stock_return_fallback_wide(
            fallback_tickers = missing_return_tickers
          ) %>%
          dplyr::filter(date == as.Date(current_date)) %>%
          dplyr::select(
            date,
            legacy_ticker,
            ret_1d_fallback
          )

        stock_df <- stock_df %>%
          dplyr::left_join(
            fallback_return_df,
            by = c("date", "legacy_ticker")
          ) %>%
          dplyr::mutate(
            ret_1d = dplyr::coalesce(ret_1d, ret_1d_fallback)
          ) %>%
          dplyr::select(-ret_1d_fallback)
      }


      fund_date_df <- fund_df %>%
        dplyr::filter(date == current_date)

      final_df <- dplyr::bind_rows(
        stock_df,
        fund_date_df
      ) %>%
        dplyr::arrange(legacy_ticker)

      validate_final_date_df(final_df, current_date)

      readr::write_csv(
        final_df,
        file_path
      )

      if (isTRUE(verbose)) {
        message("Saved file: ", file_path)
      }

      final_df
    }
  )

  out
}

download_brokerage_notes_from_outlook <- function(
    current_dates,
    subject_pattern = "Nota de Corretagem Fundos",
    sender_pattern = "mirae",
    target_dir = file.path(here::here("data", "dev", "notas_corretagem")),
    outlook_folder = NULL,
    max_emails = Inf,
    overwrite = FALSE,
    require_excel_cache = TRUE,
    verbose = TRUE
) {

  ## ---------------------------------------------------------------------------
  ## Validate inputs
  ## ---------------------------------------------------------------------------

  if (missing(current_dates) || length(current_dates) == 0L) {
    stop("`current_dates` must be a non-empty Date vector.")
  }

  current_dates <- as.Date(current_dates)

  if (any(is.na(current_dates))) {
    stop("`current_dates` contains values that cannot be converted to Date.")
  }

  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
  }

  ## ---------------------------------------------------------------------------
  ## Helper: build cache log from existing files
  ## ---------------------------------------------------------------------------

  build_cache_log <- function(dates) {

    dates <- as.Date(dates)

    cache_log_list <- lapply(
      dates,
      function(current_date) {

        date_dir <- file.path(target_dir, as.character(current_date))

        if (!dir.exists(date_dir)) {
          return(NULL)
        }

        cached_files <- list.files(
          path = date_dir,
          full.names = TRUE,
          recursive = FALSE
        )

        cached_files <- cached_files[
          tolower(tools::file_ext(cached_files)) %in% c("xlsx", "xls")
        ]

        if (length(cached_files) == 0L) {
          return(NULL)
        }

        data.frame(
          email_index = NA_integer_,
          received_datetime = as.POSIXct(NA),
          date = as.Date(current_date),
          sender = NA_character_,
          subject = NA_character_,
          attachment_index = seq_along(cached_files),
          attachment_name = basename(cached_files),
          saved_path = cached_files,
          log_source = "cache",
          stringsAsFactors = FALSE
        )
      }
    )

    cache_log_list <- cache_log_list[!vapply(cache_log_list, is.null, logical(1))]

    if (length(cache_log_list) == 0L) {
      return(
        data.frame(
          email_index = integer(),
          received_datetime = as.POSIXct(character()),
          date = as.Date(character()),
          sender = character(),
          subject = character(),
          attachment_index = integer(),
          attachment_name = character(),
          saved_path = character(),
          log_source = character(),
          stringsAsFactors = FALSE
        )
      )
    }

    do.call(rbind, cache_log_list)
  }


  date_setdiff <- function(x, y) {
    x <- as.Date(x)
    y <- as.Date(y)

    x[!x %in% y]
  }

  ## ---------------------------------------------------------------------------
  ## Check cache before touching Outlook
  ## ---------------------------------------------------------------------------

  cache_log <- build_cache_log(current_dates)

  if (isTRUE(require_excel_cache)) {

    dates_with_cache <- cache_log %>%
      dplyr::filter(tolower(tools::file_ext(saved_path)) %in% c("xlsx", "xls")) %>%
      dplyr::pull(date) %>%
      as.Date() %>%
      unique()

  } else {

    dates_with_cache <- cache_log %>%
      dplyr::pull(date) %>%
      as.Date() %>%
      unique()
  }

  missing_dates <- date_setdiff(current_dates, dates_with_cache)

  if (length(missing_dates) == 0L && !isTRUE(overwrite)) {

    if (isTRUE(verbose)) {
      message(
        "Valid cached brokerage notes found for all current_dates. ",
        "Skipping Outlook download."
      )
    }

    return(cache_log)
  }

  if (isTRUE(verbose)) {
    message(
      "Brokerage notes missing from cache for dates: ",
      paste(as.Date(as.character(missing_dates)), collapse = ", "),
      ". Searching Outlook."
    )
  }

  ## Connect to Outlook only when needed----------------------------------------

  outlook_app <- RDCOMClient::COMCreate("Outlook.Application")
  namespace <- outlook_app$GetNamespace("MAPI")

  if (is.null(outlook_folder)) {
    inbox <- namespace$GetDefaultFolder(6)
  } else {
    inbox <- outlook_folder
  }

  items <- inbox$Items()
  items$Sort("[ReceivedTime]", TRUE)

  n_items <- items$Count()
  n_scan <- min(n_items, max_emails)

  ## Initialize Outlook download log--------------------------------------------

  outlook_log <- data.frame(
    email_index = integer(),
    received_datetime = as.POSIXct(character()),
    date = as.Date(character()),
    sender = character(),
    subject = character(),
    attachment_index = integer(),
    attachment_name = character(),
    saved_path = character(),
    log_source = character(),
    stringsAsFactors = FALSE
  )
  ## Scan Outlook only for missing dates----------------------------------------

  for (i in seq_len(n_scan)) {

    email <- tryCatch(
      items$Item(i),
      error = function(e) NULL
    )

    if (is.null(email)) {
      next
    }

    subject <- tryCatch(email$Subject(), error = function(e) NA_character_)
    sender <- tryCatch(email$SenderEmailAddress(), error = function(e) NA_character_)
    received_raw <- tryCatch(email$ReceivedTime(), error = function(e) NA_real_)

    if (is.na(subject) || is.na(received_raw)) {
      next
    }

    received_datetime <- as.POSIXct(
      as.numeric(received_raw) * 86400,
      origin = "1899-12-30",
      tz = Sys.timezone()
    )

    date <- as.Date(received_datetime)

    ## Only download missing dates
    if (!date %in% missing_dates) {
      next
    }

    subject_match <- grepl(
      pattern = subject_pattern,
      x = subject,
      ignore.case = TRUE
    )

    if (!subject_match) {
      next
    }

    sender_match <- TRUE

    if (!is.null(sender_pattern)) {
      sender_match <- !is.na(sender) &&
        grepl(
          pattern = sender_pattern,
          x = sender,
          ignore.case = TRUE
        )
    }

    if (!sender_match) {
      next
    }

    attachments <- tryCatch(
      email$Attachments(),
      error = function(e) NULL
    )

    if (is.null(attachments)) {
      next
    }

    n_attachments <- attachments$Count()

    if (n_attachments == 0L) {
      next
    }

    date_dir <- file.path(target_dir, as.character(date))

    if (!dir.exists(date_dir)) {
      dir.create(date_dir, recursive = TRUE)
    }

    for (j in seq_len(n_attachments)) {

      attachment <- attachments$Item(j)

      attachment_name <- attachment$FileName()

      attachment_ext <- tolower(tools::file_ext(attachment_name))

      ## Skip PDFs, PNGs, JPGs, etc.
      if (!attachment_ext %in% c("xlsx", "xls")) {
        next
      }

      safe_attachment_name <- gsub(
        pattern = "[/:*?\"<>|]",
        replacement = "_",
        x = attachment_name
      )

      saved_path <- file.path(date_dir, safe_attachment_name)

      ## Important change:
      ## If file exists and overwrite = FALSE, do not create a duplicated file.
      ## Just log the existing cached file.
      if (file.exists(saved_path) && !isTRUE(overwrite)) {

        if (isTRUE(verbose)) {
          message("Cached attachment already exists. Skipping save: ", saved_path)
        }

        log_source <- "cache_existing"

      } else {

        attachment$SaveAsFile(
          normalizePath(saved_path, winslash = "\\", mustWork = FALSE)
        )

        log_source <- "outlook_download"
      }

      outlook_log <- rbind(
        outlook_log,
        data.frame(
          email_index = i,
          received_datetime = received_datetime,
          date = date,
          sender = sender,
          subject = subject,
          attachment_index = j,
          attachment_name = attachment_name,
          saved_path = saved_path,
          log_source = log_source,
          stringsAsFactors = FALSE
        )
      )
    }
  }

  ## Rebuild final log from cache plus newly downloaded files-------------------
  final_cache_log <- build_cache_log(current_dates)

  ## Keep Outlook metadata when available, but guarantee all cached files appear
  final_log <- dplyr::bind_rows(
    outlook_log,
    final_cache_log
  ) %>%
    dplyr::mutate(
      saved_path_norm = normalizePath(saved_path, winslash = "/", mustWork = FALSE),
      date = as.Date(date)
    ) %>%
    dplyr::arrange(
      saved_path_norm,
      dplyr::desc(log_source != "cache")
    ) %>%
    dplyr::distinct(
      saved_path_norm,
      .keep_all = TRUE
    ) %>%
    dplyr::select(-saved_path_norm) %>%
    dplyr::arrange(date, attachment_name)

  ## Check dates and return-----------------------------------------------------
  found_dates <- final_log %>%
    dplyr::pull(date) %>%
    as.Date() %>%
    unique()

  dates_without_attachment <- current_dates[
    !current_dates %in% found_dates
  ]

  if (length(dates_without_attachment) > 0L && isTRUE(verbose)) {
    message(
      "No trade information found for dates: ",
      paste(as.character(dates_without_attachment), collapse = ", ")
    )
  }

  return(final_log)
}


load_brokerage_notes <- function(
    current_dates,
    notes_dir = here::here("data", "dev", "notas_corretagem"),
    brokerage_fee_bps = 5.5,
    allow_empty = TRUE,
    recursive = TRUE
) {

  ## Validate inputs -----------------------------------------------------------

  if (missing(current_dates) || length(current_dates) == 0L) {
    stop("`current_dates` must be a non-empty vector of dates.")
  }

  current_dates <- as.Date(current_dates)

  if (any(is.na(current_dates))) {
    stop("`current_dates` contains values that could not be converted to Date.")
  }

  if (!dir.exists(notes_dir)) {
    stop(
      paste0(
        "`notes_dir` does not exist: ",
        notes_dir
      )
    )
  }

  if (!is.numeric(brokerage_fee_bps) || length(brokerage_fee_bps) != 1L || is.na(brokerage_fee_bps)) {
    stop("`brokerage_fee_bps` must be a single numeric value.")
  }

  ## Internal Helpers---------------------------------------------------------

  parse_numeric_safe <- function(x) {

    ## Convert input to character
    x <- as.character(x)

    ## Remove leading/trailing spaces
    x <- trimws(x)

    ## Convert empty strings to NA
    x[x == ""] <- NA_character_

    ## Detect values with both Brazilian thousand separator and decimal comma
    ## Example: "1.577,25"
    has_dot_and_comma <- grepl("\\.", x) & grepl(",", x)

    ## Detect values with decimal comma only
    ## Example: "15,77"
    has_comma_only <- !grepl("\\.", x) & grepl(",", x)

    ## Case 1: Brazilian format with thousand dot and decimal comma
    x[has_dot_and_comma] <- gsub(
      pattern = "\\.",
      replacement = "",
      x = x[has_dot_and_comma]
    )

    x[has_dot_and_comma] <- gsub(
      pattern = ",",
      replacement = ".",
      x = x[has_dot_and_comma]
    )

    ## Case 2: Decimal comma only
    x[has_comma_only] <- gsub(
      pattern = ",",
      replacement = ".",
      x = x[has_comma_only]
    )

    ## Case 3: Decimal dot only
    ## Example: "15.77"
    ## No transformation needed.

    as.numeric(x)
  }

  parse_mirae_date <- function(x) {

    x_chr <- as.character(x)

    numeric_x <- suppressWarnings(as.numeric(x_chr))

    parsed_date <- rep(as.Date(NA), length(x_chr))

    ## Excel serial date case, e.g. 46132
    is_excel_serial <- !is.na(numeric_x)

    parsed_date[is_excel_serial] <- as.Date(
      numeric_x[is_excel_serial],
      origin = "1899-12-30"
    )

    ## Character date cases
    is_not_serial <- !is_excel_serial & !is.na(x_chr)

    parsed_date[is_not_serial] <- suppressWarnings(
      as.Date(x_chr[is_not_serial], format = "%d-%m-%Y")
    )

    still_na <- is.na(parsed_date) & !is.na(x_chr)

    parsed_date[still_na] <- suppressWarnings(
      as.Date(x_chr[still_na], format = "%Y-%m-%d")
    )

    parsed_date
  }

  empty_trade_data <- function() {
    data.frame(
      date = as.Date(character()),
      fund_account = character(),
      legacy_ticker = character(),
      side = character(),
      amount = numeric(),
      price = numeric(),
      traded_volume = numeric(),
      brokerage_fee_bps = numeric(),
      brokerage_fee_estimated = numeric(),
      source_file = character(),
      stringsAsFactors = FALSE
    )
  }

  ## Find folders matching current_dates----------------------------------------

  date_folders <- list.dirs(
    path = notes_dir,
    full.names = TRUE,
    recursive = FALSE
  )

  if (length(date_folders) == 0L) {
    stop(
      paste0(
        "No date folders found inside: ",
        notes_dir
      )
    )
  }

  folder_names <- basename(date_folders)
  folder_dates <- suppressWarnings(as.Date(folder_names))

  valid_date_folder <- !is.na(folder_dates)
  date_folders <- date_folders[valid_date_folder]
  folder_dates <- folder_dates[valid_date_folder]

  selected_folders <- date_folders[folder_dates %in% current_dates]

  if (length(selected_folders) == 0L) {
    if (isTRUE(allow_empty)) {
      return(empty_trade_data())
    }
    stop(
      paste0(
        "No folders matching `current_dates` were found inside: ",
        notes_dir,
        ". Expected folders named as dates, for example: 2026-04-30."
      )
    )
  }

  ## Find Excel files inside selected folders-----------------------------------

  excel_files <- unlist(
    lapply(
      selected_folders,
      function(folder) {
        list.files(
          path = folder,
          pattern = "\\.(xlsx|xls)$",
          full.names = TRUE,
          recursive = recursive,
          ignore.case = TRUE
        )
      }
    ),
    use.names = FALSE
  )

  excel_files <- unique(excel_files)

  if (length(excel_files) == 0L) {
    if (isTRUE(allow_empty)) {
      return(empty_trade_data())
    }
    stop("No .xlsx or .xls files were found inside the selected date folders.")
  }

  ## Internal helper to read one Mirae brokerage XLSX file ---------------------
  read_one_brokerage_file <- function(file_path) {

    ### Read raw file only once
    raw_df <- readxl::read_excel(
      path = file_path,
      col_names = FALSE,
      .name_repair = "minimal"
    )

    raw_df <- as.data.frame(
      raw_df,
      stringsAsFactors = FALSE
    )

    ### Find header row by locating Pregão in any column
    header_position <- which(raw_df == "Pregão", arr.ind = TRUE)

    if (nrow(header_position) == 0L) {
      stop(
        paste0(
          "Could not find header cell `Pregão` in file: ",
          file_path
        )
      )
    }

    header_row <- header_position[1L, "row"]

    ### Extract header names from the detected row
    header_names <- as.character(unlist(raw_df[header_row, ], use.names = FALSE))

    header_names[is.na(header_names) | header_names == ""] <- paste0(
      "unnamed_",
      which(is.na(header_names) | header_names == "")
    )

    header_names <- make.unique(header_names)

    ### Keep only rows after the header row
    trades_raw <- raw_df[
      seq.int(header_row + 1L, nrow(raw_df)),
      ,
      drop = FALSE
    ]

    names(trades_raw) <- header_names

    ### Check required columns
    required_cols <- c(
      "Pregão",
      "Cliente",
      "Cod. Negocio",
      "Natureza",
      "Qtd. Qtdesp",
      "Preço"
    )

    missing_cols <- setdiff(required_cols, names(trades_raw))

    if (length(missing_cols) > 0L) {
      stop(
        paste0(
          "Missing required columns in file: ",
          file_path,
          ". Missing columns: ",
          paste(missing_cols, collapse = ", ")
        )
      )
    }

    ### Keep only actual trade rows
    trades_raw <- trades_raw[
      !is.na(trades_raw[["Pregão"]]) &
        trades_raw[["Pregão"]] != "" &
        !is.na(trades_raw[["Cliente"]]) &
        trades_raw[["Cliente"]] != "" &
        !is.na(trades_raw[["Cod. Negocio"]]) &
        trades_raw[["Cod. Negocio"]] != "",
      ,
      drop = FALSE
    ]

    if (nrow(trades_raw) == 0L) {
      return(
        data.frame(
          date = as.Date(character()),
          fund_account = character(),
          legacy_ticker = character(),
          side = character(),
          amount = numeric(),
          price = numeric(),
          traded_volume = numeric(),
          brokerage_fee_bps = numeric(),
          brokerage_fee_estimated = numeric(),
          source_file = character(),
          stringsAsFactors = FALSE
        )
      )
    }

    ### Standardize columns
    trades_df <- data.frame(
      date = parse_mirae_date(trades_raw[["Pregão"]]),
      fund_account = as.character(trades_raw[["Cliente"]]),
      legacy_ticker = as.character(trades_raw[["Cod. Negocio"]]),
      side = as.character(trades_raw[["Natureza"]]),
      amount = parse_numeric_safe(trades_raw[["Qtd. Qtdesp"]]),
      price = parse_numeric_safe(trades_raw[["Preço"]]),
      source_file = file_path,
      stringsAsFactors = FALSE
    )

    ### Remove rows with invalid essential fields
    trades_df <- trades_df[
      !is.na(trades_df$date) &
        !is.na(trades_df$fund_account) &
        trades_df$fund_account != "" &
        !is.na(trades_df$legacy_ticker) &
        trades_df$legacy_ticker != "" &
        !is.na(trades_df$amount) &
        !is.na(trades_df$price),
      ,
      drop = FALSE
    ]

    ### Translate side
    trades_df$side <- dplyr::case_when(
      trades_df$side == "C" ~ "buy",
      trades_df$side == "V" ~ "sell",
      TRUE ~ trades_df$side
    )

    ### Compute traded volume and estimated brokerage fee
    trades_df$traded_volume <- abs(trades_df$amount * trades_df$price)
    trades_df$brokerage_fee_bps <- brokerage_fee_bps
    trades_df$brokerage_fee_estimated <- trades_df$traded_volume * brokerage_fee_bps / 10000

    ### Reorder columns
    trades_df <- trades_df[
      ,
      c(
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
      ),
      drop = FALSE
    ]

    trades_df
  }


  ## Read and consolidate all Excel files--------------------------------------

  trades_list <- lapply(
    excel_files,
    read_one_brokerage_file
  )

  trades_df <- do.call(rbind, trades_list)

  ## Filter by actual Pregão date, not only folder date-----------------------

  trades_df <- trades_df[
    trades_df$date %in% current_dates,
    ,
    drop = FALSE
  ]

  if (nrow(trades_df) == 0L) {
    if (isTRUE(allow_empty)) {
      return(empty_trade_data())
    }
    stop("No brokerage trades were found for `current_dates` after reading the Excel files.")
  }

  trades_df
}









