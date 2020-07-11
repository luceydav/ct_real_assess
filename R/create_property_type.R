
#Function to equalize ResidentialType in early years with other datasets
create_property_type <- function(a, b, c, d, e, f, g) {
  new <- fcase(
    !is.na(a), "Residential",
    !is.na(b), "Residential",
    !is.na(c), "Residential",
    !is.na(d), "Commercial",
    !is.na(e), "Industrial",
    !is.na(f), "Vacant",
    !is.na(g), "Utility",
    TRUE ~ "Unknown"
  )
  return(new)
}