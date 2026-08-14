#### ----------------------
#### 00. packages & library
#### ----------------------

required_packages <- c("shiny", "dplyr", "stringr", "janitor", "tidyr", "lubridate", "ggplot2")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

