deploy_dados_bronze <- function(current_dates, anbima_holidays, broker_accounts,
                                overwrite = FALSE, manifest = NULL, commit = NULL, board){

  message("Deploying Bronze Data...")
  #Load-------------------------------------------------------------------------
  message("Loading Bronze Data...")
  dados_bronze_batch <- run_logged(
    fun = function(){
      load_dados_bronze(
        current_dates   = current_dates,
        anbima_holidays = anbima_holidays,
        overwrite       = overwrite
      )
    },
    log_file = paste0("load_dados_bronze_", min(current_dates), "_", max(current_dates)),
    log_path = file.path(here::here(), "logs", "load")
  )


  #Append-----------------------------------------------------------------------
  message("Appending Bronze Data...")
  old_dados_bronze <- read_pin_from_manifest(
    board = board,
    manifest = manifest,
    object_name = "dados_bronze"
  )

  dados_bronze_refreshed <- append_dados_batch(
    old_dados   = old_dados_bronze,
    dados_batch = dados_bronze_batch
  )

  #Test-------------------------------------------------------------------------
  message("Testing Bronze Data...")

  test_sucess <- run_logged_test(
    fun = function() {
      run_test_dados_bronze(
        dados_bronze             = dados_bronze_refreshed,
        broker_accounts          = broker_accounts,
        current_dates            = current_dates,
        weight_tolerance         = 1e-3,
        index_weight_tolerance   = 0.5,
        min_proventos_zero_share = 0.50,
        min_population_share     = 0.75,
        return_tolerance         = 1e-4,
        capital_gain_tolerance   = 1e-4,
        max_missing_share        = 0.05,
        etfs                     = c("BOVA11", "BOVV11", "DIVO11",
                                     "SMAL11", "ISUS11", "LFTS11"),
        cash_tickers             = c("BRL Curncy"),
        verbose                  = TRUE
      )
    },
    log_file = paste0("test_dados_bronze_", min(current_dates), "_", max(current_dates)),
    log_path = file.path(here::here(), "logs", "tests")
  )

  check_test_success(test_sucess)

  #Deploy-----------------------------------------------------------------------
  message("Deploying Bronze Data...")
  save_local_pin(
    board       = board,
    data        = dados_bronze_refreshed,
    object_name = "dados_bronze",
    stage       = "bronze",
    commit      = commit,
    source      = "rebalanceamento_comdinheiro",
    table_type  = "dados_bronze"
  )


  return(dados_bronze_refreshed)


}
