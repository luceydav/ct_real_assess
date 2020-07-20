
## Load your packages, e.g. library(drake).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

rsconnect::setAccountInfo(
  name = keyring::key_get("RSCONNECT_NAME"),
  token = keyring::key_get("RSCONNECT_TOKEN"),
  secret = keyring::key_get("RSCONNECT_SECRET")
)

## _drake.R must end with a call to drake_config().
## The arguments to drake_config() are basically the same as those to make().
## lock_envir allows functions that alter the random seed to be used. The biggest
## culprits of this seem to be interactive graphics e.g. plotly and mapdeck.
drake_config(the_plan,
             lock_envir = FALSE)