##Atribuição do Modelo----------------------------------------------------------
#perf_atb_port_backtest_results_bundles <- targets::tar_read(perf_atb_port_backtest_results_bundles, store = "_targets_update")
#pregold_m_df                           <- targets::tar_read(pregold_m_df_refreshed, store = "_targets_update")

perf_atb_port_backtest_results_bundles$perf_atb_ports_backtest_cohort@port_backtest_results_list$chosen_mb_cs@final_stock_universe_m_d_ref@data %>%
  dplyr::left_join(
    pregold_m_df@data %>% dplyr::filter(dates == max(dates)) %>% dplyr::select(tickers, legacy_ticker),
    by = "tickers"
  ) %>%
  dplyr::select(legacy_ticker, weights) %>%
  dplyr::arrange(dplyr::desc(weights)) %>%
  write.csv("alpha_model_allocation.csv")


perf_atb_port_backtest_results_bundles$perf_atb_ports_backtest_cohort@port_backtest_results_list$ew_selected_pb@final_stock_universe_m_d_ref@data %>%
  dplyr::left_join(
    pregold_m_df@data %>% dplyr::filter(dates == max(dates)) %>% dplyr::select(tickers, legacy_ticker),
    by = "tickers"
  ) %>%
  dplyr::select(legacy_ticker, weights) %>%
  dplyr::arrange(dplyr::desc(weights)) %>%
  write.csv("alpha_model_selection.csv")

perf_atb_port_backtest_results_bundles$perf_atb_ports_backtest_cohort@port_backtest_results_list$ew_selected_sb_cs@final_stock_universe_m_d_ref@data %>%
  dplyr::left_join(
    pregold_m_df@data %>% dplyr::filter(dates == max(dates)) %>% dplyr::select(tickers, legacy_ticker),
    by = "tickers"
  ) %>%
  dplyr::select(legacy_ticker, weights) %>%
  dplyr::arrange(dplyr::desc(weights)) %>%
  write.csv("alpha_model_integration.csv")

perf_atb_port_backtest_results_bundles$perf_atb_ports_backtest_cohort@port_backtest_results_list$ew_all_pb@final_stock_universe_m_d_ref@data %>%
  dplyr::left_join(
    pregold_m_df@data %>% dplyr::filter(dates == max(dates)) %>% dplyr::select(tickers, legacy_ticker),
    by = "tickers"
  ) %>%
  dplyr::select(legacy_ticker, weights) %>%
  dplyr::arrange(dplyr::desc(weights)) %>%
  write.csv("all_universe.csv")

##Mandato-----------------------------------------------------------------------
perf_atb_port_backtest_results_bundles$mandates_port_backtest_results_list$chosen_mb_sw_ibov@final_stock_universe_m_d_ref@data %>%
  dplyr::left_join(
    pregold_m_df@data %>% dplyr::filter(dates == max(dates)) %>% dplyr::select(tickers, legacy_ticker),
    by = "tickers"
  ) %>%
  dplyr::select(legacy_ticker, weights) %>%
  dplyr::arrange(dplyr::desc(weights)) %>%
  write.csv("mandato_ibov.csv")

perf_atb_port_backtest_results_bundles$mandates_port_backtest_results_list$chosen_mb_sw_smll@final_stock_universe_m_d_ref@data %>%
  dplyr::left_join(
    pregold_m_df@data %>% dplyr::filter(dates == max(dates)) %>% dplyr::select(tickers, legacy_ticker),
    by = "tickers"
  ) %>%
  dplyr::select(legacy_ticker, weights) %>%
  dplyr::arrange(dplyr::desc(weights)) %>%
  write.csv("mandato_smll.csv")

perf_atb_port_backtest_results_bundles$mandates_port_backtest_results_list$chosen_mb_sw_large_payers@final_stock_universe_m_d_ref@data %>%
  dplyr::left_join(
    pregold_m_df@data %>% dplyr::filter(dates == max(dates)) %>% dplyr::select(tickers, legacy_ticker),
    by = "tickers"
  ) %>%
  dplyr::select(legacy_ticker, weights) %>%
  dplyr::arrange(dplyr::desc(weights)) %>%
  write.csv("mandato_large_payers.csv")

perf_atb_port_backtest_results_bundles$mandates_port_backtest_results_list$chosen_mb_sw_isee@final_stock_universe_m_d_ref@data %>%
  dplyr::left_join(
    pregold_m_df@data %>% dplyr::filter(dates == max(dates)) %>% dplyr::select(tickers, legacy_ticker),
    by = "tickers"
  ) %>%
  dplyr::select(legacy_ticker, weights) %>%
  dplyr::arrange(dplyr::desc(weights)) %>%
  write.csv("mandato_isee.csv")

##Final Portfolios---------------------------------------------------------------
#meta_port_backtest_results_bundles_refreshed <- targets::tar_read(meta_port_backtest_results_bundles_refreshed, store = "_targets_update")

meta_port_backtest_results_bundles_refreshed$backtest_meta_portfolios_results$ibov_port_backtest_cohort@port_backtest_results_list$meta_rp_box_ibov@final_stock_universe_m_d_ref@data %>%
  dplyr::left_join(
    pregold_m_df@data %>% dplyr::filter(dates == max(dates)) %>% dplyr::select(tickers, legacy_ticker),
    by = "tickers"
  ) %>%
  dplyr::select(legacy_ticker, weights) %>%
  dplyr::arrange(dplyr::desc(weights)) %>%
  write.csv("modelo_final_ibov.csv")

meta_port_backtest_results_bundles_refreshed$backtest_meta_portfolios_results$smll_port_backtest_cohort@port_backtest_results_list$meta_rp_box_smll@final_stock_universe_m_d_ref@data %>%
  dplyr::left_join(
    pregold_m_df@data %>% dplyr::filter(dates == max(dates)) %>% dplyr::select(tickers, legacy_ticker),
    by = "tickers"
  ) %>%
  dplyr::select(legacy_ticker, weights) %>%
  dplyr::arrange(dplyr::desc(weights)) %>%
  write.csv("modelo_final_smll.csv")

meta_port_backtest_results_bundles_refreshed$backtest_meta_portfolios_results$large_payers_port_backtest_cohort@port_backtest_results_list$meta_rp_box_large_payers@final_stock_universe_m_d_ref@data %>%
  dplyr::left_join(
    pregold_m_df@data %>% dplyr::filter(dates == max(dates)) %>% dplyr::select(tickers, legacy_ticker),
    by = "tickers"
  ) %>%
  dplyr::select(legacy_ticker, weights) %>%
  dplyr::arrange(dplyr::desc(weights)) %>%
  write.csv("modelo_final_large_payers.csv")

meta_port_backtest_results_bundles_refreshed$backtest_meta_portfolios_results$isee_port_backtest_cohort@port_backtest_results_list$meta_rp_box_isee@final_stock_universe_m_d_ref@data %>%
  dplyr::left_join(
    pregold_m_df@data %>% dplyr::filter(dates == max(dates)) %>% dplyr::select(tickers, legacy_ticker),
    by = "tickers"
  ) %>%
  dplyr::select(legacy_ticker, weights) %>%
  dplyr::arrange(dplyr::desc(weights)) %>%
  write.csv("modelo_final_isee.csv")

##Clusters
#signal_port_backtest_cohorts_refreshed <- targets::tar_read(signal_port_backtest_cohorts_refreshed, store = "_targets_update")

cluster_list <- signal_port_backtest_cohorts_refreshed$
  signal_port_backtest_cohort_clusters@
  port_backtest_results_list %>%
  base::names()

latest_ticker_map <- pregold_m_df@data %>%
  dplyr::filter(dates == base::max(dates, na.rm = TRUE)) %>%
  dplyr::select(tickers, legacy_ticker)

for (cluster_name in cluster_list) {

  signal_port_backtest_cohorts_refreshed$
    signal_port_backtest_cohort_clusters@
    port_backtest_results_list[[cluster_name]]@
    final_stock_universe_m_d_ref@data %>%
    dplyr::left_join(
      latest_ticker_map,
      by = "tickers"
    ) %>%
    dplyr::select(legacy_ticker, weights) %>%
    dplyr::arrange(dplyr::desc(weights)) %>%
    utils::write.csv(
      file = base::paste0("cluster_", cluster_name, ".csv"),
      row.names = FALSE
    )
}


#Setores-------------------------------------------------------------------------
pregold_m_df <- load_old_object(manifest, object_name = "pregold")
sectors <- pregold_m_df@data %>%
  dplyr::select(legacy_ticker, sectors_comdinheiro, sectors_ind,
                subsectors, subsubsectors, sectors_c1, sectors_c2,
                sectors_dynamic, sectors_to_treat_as_banks, sectors_to_treat_as_insurers)

write.csv(sectors, "sectors.csv", row.names = FALSE)


#Catalog------------------------------------------------------------------------
catalog <- load_old_object(manifest, object_name = "gold_tickers_catalog")
write.csv(catalog, "catalog.csv", row.names = FALSE)



