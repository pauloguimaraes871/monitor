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
    current_dates,
    create_current_dates(
      date_begin      = "2026-04-20",
      date_end        = "2026-05-02",
      manifest        = manifest,
      anbima_holidays = anbima_holidays
    )
  ),

  tar_target(
    dados_bronze_refreshed,
    deploy_dados_bronze(
      current_dates  = current_dates,
      manifest       = manifest,
      anbima_holidays= anbima_holidays,
      broker_accounts= broker_accounts,
      overwrite      = FALSE,
      commit         = "Deploying Bronze Data",
      board          = board
    )
  ),

  tar_target(
    catalog_update,
    data.frame(
      date          = as.Date(rep("2026-04-20", 2)),
      legacy_ticker = c("SAUD3", "AZUL3"),
      cvm_code_type = c("h2012_ON","h2411_ON")
    )
  ),

  tar_target(
    dados_silver_refreshed,
    deploy_dados_silver(
      dados_bronze_refreshed = dados_bronze_refreshed,
      catalog_update         = catalog_update,
      current_dates          = current_dates,
      manifest               = manifest,
      broker_accounts        = broker_accounts,
      fund_tickers           =
        c("BOVA11", "BOVV11", "SMAL11", "DIVO11", "ISUS11", "LFTS11",
          "avantgarde_multifatores", "az_quest_bayes_sistematico",
          "bb_acoes_selecao_fatorial", "constancia_fundamento",
          "itau_smart_acoes_brasil_50", "kadima_equities", "sicoob_acoes",
          "sicoob_asg_is", "sicoob_dividendos", "sicoob_small_caps",
          "v8_veyron", "vgbl_sicoob_seguradora_rv_30", "vgbl_sicoob_seguradora_rv_65"),
      commit = "Deploying silver batch",
      board  = board
    )
  )
)


