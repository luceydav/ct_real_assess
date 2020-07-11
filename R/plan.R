
the_plan <- 
  drake_plan(
    raw_99_11 = try(load_raw_data()),
    cleaned_01_recent  = load_socrata(),
    new = merge_and_filter_sources(
      raw_99_11, 
      cleaned_01_recent) %>%
      final_cleanup() %>%
      add_reval(),
    saveRDS(new, "ct_sales_99_2018.RDS"),
    shiny = run_shiny(new)
)