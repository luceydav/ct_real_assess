library(drake)

the_plan <-
  drake_plan(
    raw_99_11 = try(load_raw_data()),
    cleaned_01_recent = load_socrata(),
    new = merge_and_clean_sources(
      raw_99_11,
      cleaned_01_recent
    ), 
    save_file = saveRDS(new, file_out("ct_sales_99_2018.RDS")),
    deployment = rsconnect::deployApp(
      appFiles = file_in(
        "ct_sales_99_2018.RDS",
        "app.R",
        "R/plot_dotplot.R",
        "R/plot_spaghetti.R",
        "R/plot_timeplot.R"
      ),
      appName = "ct_real_assess",
      forceUpdate = TRUE
    )
  )