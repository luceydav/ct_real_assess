# If to deploy Shiny from drake
# Won't work with data.table 1.9 dev version
custom_deployment_function <- function(file, ...) {
  rsconnect::deployApp(
    appFiles = file,
    appName = "your_name",
    forceUpdate = TRUE
  )
}