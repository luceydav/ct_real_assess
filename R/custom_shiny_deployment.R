# If Shiny deployment from drake is needed
custom_shiny_deployment <- function(file, ...) {
  rsconnect::deployApp(
    appFiles = file,
    appName = "your_name",
    forceUpdate = TRUE
  )
}