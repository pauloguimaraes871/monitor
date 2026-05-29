deploy_dados_silver <- function(dados_bronze_refreshed, catalog_update, current_dates, manifest = NULL,
                                broker_accounts, fund_tickers, commit = NULL, board){

  message("Deploying Silver Data...")
  ##Adjust objects and apply catalog--------------------------------------------

    ### Extract batch for rebalancing dates
    dados_bronze_batch <- dados_bronze_refreshed
      #### comdinheiro
      dados_bronze_batch$comdinheiro_data <-
        dados_bronze_batch$comdinheiro_data %>%
        dplyr::filter(date %in% current_dates)
      #### rebalanceamento
      dados_bronze_batch$rebalanceamento_tables$rebal_weights <-
        dados_bronze_batch$rebalanceamento_tables$rebal_weights %>%
        dplyr::filter(date %in% current_dates)
      dados_bronze_batch$rebalanceamento_tables$sectors <-
        dados_bronze_batch$rebalanceamento_tables$sectors %>%
        dplyr::filter(date %in% current_dates)
      dados_bronze_batch$rebalanceamento_tables$catalog <-
        dados_bronze_batch$rebalanceamento_tables$catalog %>%
        dplyr::filter(date %in% current_dates)
      #### brokerage
      dados_bronze_batch$brokerage_data$trade_data <-
        dados_bronze_batch$brokerage_data$trade_data %>%
        dplyr::filter(date %in% current_dates)
      dados_bronze_batch$brokerage_data$brokerage_notes_log <-
        dados_bronze_batch$brokerage_data$brokerage_notes_log %>%
        dplyr::filter(date %in% current_dates)

    ### Apply catalog
    translated_tables <- run_logged(
      fun = function(){
        apply_catalog(
          ###Note those bronze are full datasets
          rebalanceamento_tables = dados_bronze_batch$rebalanceamento_tables,
          comdinheiro_data       = dados_bronze_batch$comdinheiro_data,
          brokerage_data         = dados_bronze_batch$brokerage_data,
          broker_accounts        = broker_accounts,
          catalog_update         = catalog_update,
          fund_tickers           = fund_tickers,
          verbose                = TRUE
        )
      },
      log_file = paste0("apply_catalog", min(current_dates), "_", max(current_dates)),
      log_path = file.path(here::here(), "logs", "preprocessing")
    )

    ### Rearrange structure
    dados_silver_batch <- list(
      rebalanceamento_tables = list(
        rebal_weights  = translated_tables$rebal_weights,
        sectors        = translated_tables$sectors,
        catalog        = translated_tables$catalog
      ),
      comdinheiro_data = translated_tables$comdinheiro_data,
      brokerage_data         = list(
        trade_data          = translated_tables$trade_data,
        brokerage_notes_log = translated_tables$brokerage_notes_log
      )
    )

  ##Test------------------------------------------------------------------------
  message("Testing Silver Data...")
    test_sucess <- run_logged_test(
      fun = function() {
        run_test_dados_silver(
          dados_silver = dados_silver_batch
        )
      },
      log_file = paste0("test_dados_silver_", min(current_dates), "_", max(current_dates)),
      log_path = file.path(here::here(), "logs", "tests")
    )

    check_test_success(test_sucess)

  ##Append----------------------------------------------------------------------
  message("Appending Silver Data...")
  old_dados_silver <- read_pin_from_manifest(
    board       = board,
    manifest    = manifest,
    object_name = "dados_silver"
  )

  ##Erase old catalog
  if (!is.null(old_dados_silver)){
    old_dados_silver$rebalanceamento_tables$catalog <- NULL
  }

  ##Add batch for silver
  dados_silver_refreshed <- append_dados_batch(
    old_dados       = old_dados_silver,
    dados_batch     = dados_silver_batch
  )


  ##Deploy----------------------------------------------------------------------
  message("Deploying Silver Data...")
  save_local_pin(
    board       = board,
    data        = dados_silver_refreshed,
    object_name = "dados_silver",
    stage       = "silver",
    commit      = commit,
    source      = "rebalanceamento_comdinheiro",
    table_type  = "dados_silver"
  )


  return(dados_silver_refreshed)

}
