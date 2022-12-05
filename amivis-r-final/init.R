# init.R
#
# Example R code to install packages if not already installed
#

my_packages = c("shinyWidgets", "shinythemes", "shiny", "DT", "dplyr", "tools",
                "leaflet", "sf", "ggplot2", "tidyr", "RColorBrewer", "maps", "mapdata",
                "sp", "tiff", "GADMTools")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))
