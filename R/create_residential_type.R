
#Function to equalize ResidentialType in early years with other datasets
create_residential_type <- function(a, b, c) {
  new <- fcase(
    a >= 1, "Single Family",
    b >= 1, "Two Family",
    c >= 1, "Three Family",
    TRUE ~ "Unknown"
  )
  return(new)
}