# Function to check and install required packages
check_and_install_packages <- function(packages) {
  missing_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if (length(missing_packages) > 0) {
    message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
    install.packages(missing_packages, dependencies = TRUE)
  } else {
    message("All packages are installed")
  }
  invisible(lapply(packages, require, character.only = TRUE))
}

# List of required packages for the app
required_packages <- c(
  "shiny",
  "stringr",
  "tidyverse",
  "shinyWidgets",
  "leaflet",
  "RColorBrewer",
  "scales",
  "countrycode",
  "shinybrowser",
  "sf"
)

# Check and install missing packages
check_and_install_packages(required_packages)