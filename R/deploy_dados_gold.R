## Evolve weighs and returns--------------------------------------------------
evolved_paper_portfolios <- evolve_portfolio(
  rebal_weights    = rebalanceamento_tables$rebal_weights,
  catalog          = catalog,
  comdinheiro_data = comdinheiro_data,
  current_dates    = current_dates,
  old_weights      = old_weights
)


## Get old weights------------------------------------------------------------
old_dados_silver <- read_pin_from_manifest(
  board = board,
  manifest = manifest,
  object_name = "dados_bronze"
)

if (!is.null(old_dados_silver)){

} else {
  old_weights <- NULL
}
