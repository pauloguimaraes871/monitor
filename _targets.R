# Setup-------------------------------------------------------------------------
library(targets)
library(tarchetypes)
library(dplyr)

# Requirements------------------------------------------------------------------
## External Packages + Seed
tar_option_set(
  packages = c(
    "dplyr",
    "tidyr",
    "lubridate",
    "readxl",
    "here",
    "PerformanceAnalytics",
    "purrr",
    "magrittr",
    "testthat",
    "readr",
    "pins",
    "ggplot2",
    "RDCOMClient"
  ),
  seed = 123
)

## Custom Functions
tar_source(c(
  list.files(here::here("R", "load"),          pattern = "\\.R$", full.names = TRUE),
  list.files(here::here("R", "core"),          pattern = "\\.R$", full.names = TRUE),
  list.files(here::here("R", "deploy"),        pattern = "\\.R$", full.names = TRUE),
  list.files(here::here("R", "metrics"),       pattern = "\\.R$", full.names = TRUE),
  list.files(here::here("R", "helpers"),       pattern = "\\.R$", full.names = TRUE),
  list.files(here::here("R", "infra"),         pattern = "\\.R$", full.names = TRUE),
  list.files(here::here("reports"),            pattern = "\\.R$", full.names = TRUE),
  list.files(here::here("tests", "testthat"),  pattern = "\\.R$", full.names = TRUE)
))


# Workflow----------------------------------------------------------------------
list(
  #############################
  #           Setup
  #############################
  # Creates a centralized storage system for sharing data across pipeline steps
  tar_target(
    board,
    init_pipeline_board()  # tools.R
  ),

  tar_target(
    manifest,
    load_newest_manifest()
  ),

  tar_target(
    anbima_holidays,
    {
      anbima_holidays <- readxl::read_excel(
        file.path(here::here("data", "dev", "feriados_nacionais.xls")),
        sheet = "Feriados",
        range = "A1:A1265"
      )$Data %>%
        as.Date()
    }
  ),

  tar_target(
    broker_accounts,
    {
      c(
        "sicoob_acoes" = "40010",
        "sicoob_small_caps" = "49887",
        "sicoob_dividendos" = "49888",
        "sicoob_asg_is" = "49930",
        "vgbl_sicoob_seguradora_rv_30" = "44356",
        "vgbl_sicoob_seguradora_rv_65" = "44357",
        "previ_sicoob_500rv" = "48694",
        "previ_sicoob_501rv" = "48695"
      )
    }
  ),

  tar_target(
    initial_rebalancing_date,
    {
      list.files(here::here("data", "dev", "rebalancing")) %>%
        as.Date(format = "%Y%m%d") %>%
        min()
    }
  ),

  tar_target(
    current_dates,
    create_current_dates(
      date_begin      = "2026-05-15",
      date_end        = "2026-06-30",
      manifest        = manifest,
      anbima_holidays = anbima_holidays
    )
  ),

  tar_target(
    dados_bronze_refreshed,
    deploy_dados_bronze(
      current_dates            = current_dates,
      initial_rebalancing_date = initial_rebalancing_date,
      manifest                 = manifest,
      anbima_holidays          = anbima_holidays,
      broker_accounts          = broker_accounts,
      overwrite                = FALSE,
      commit                   = "Deploying Bronze Data",
      board                    = board
    )
  ),

  tar_target(
    catalog_update,
    ## The purpose of this is to adjust a ticker that has changed after
    ## the last catalog update. This will be used in the apply_catalog step
    ## In subsequent months, the change will already be in catalog from pipeline
    ## Keeping a change already impacted causes no harm
    data.frame(
      date          = as.Date(rep("2026-04-20", 2)),
      legacy_ticker = c("SAUD3", "AZUL3"),
      cvm_code_type = c("h2012_ON","h2411_ON")
    )
  ),

  tar_target(
    dados_silver_refreshed,
    deploy_dados_silver(
      dados_bronze_refreshed   = dados_bronze_refreshed,
      catalog_update           = catalog_update,
      current_dates            = current_dates,
      initial_rebalancing_date = initial_rebalancing_date,
      manifest                 = manifest,
      broker_accounts          = broker_accounts,
      fund_tickers             =
        c("BOVA11", "BOVV11", "SMAL11", "DIVO11", "ISUS11", "LFTS11",
          "avantgarde_multifatores", "az_quest_bayes_sistematico",
          "bb_acoes_selecao_fatorial", "constancia_fundamento",
          "itau_smart_acoes_brasil_50", "kadima_equities", "sicoob_acoes",
          "sicoob_asg_is", "sicoob_dividendos", "sicoob_small_caps",
          "v8_veyron", "vgbl_sicoob_seguradora_rv_30", "vgbl_sicoob_seguradora_rv_65"),
      commit = "Deploying silver batch",
      board  = board
    )
  ),

  tar_target(
    dados_gold_refreshed,
    deploy_dados_gold(
      dados_silver_refreshed   = dados_silver_refreshed,
      current_dates            = current_dates,
      initial_rebalancing_date = initial_rebalancing_date,
      manifest                 = manifest,
      commit                   = "Deploying gold batch",
      board                    = board
    )
  ),

  tar_target(
    manifest_refreshed,
    save_manifest(
      board                  = board,
      current_dates          = current_dates,
      dados_bronze_refreshed = dados_bronze_refreshed,
      dados_silver_refreshed = dados_silver_refreshed,
      dados_gold_refreshed   = dados_gold_refreshed,
      previous_manifest      = manifest,
      commit                 = "Deploying full bronze/silver/gold refresh",
      source                 = "internal",
      manifest_pin_name      = "manifest",
      comparison             = "identical"
    )
  )
)


