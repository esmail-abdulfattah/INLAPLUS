
setwd("~/Documents/INLAPLUS")

library(devtools)
library(usethis)
library(roxygen2)

#To create a package:
usethis::create_package("~/Documents/INLAPLUS")

#We can use the options from Rstudio to connect devtools to R-Projects in Rstudio:
# by the following:
#1. File -> New Project -> existing directory
#2.
usethis::create_package("~/Documents/INLAPLUS")
#3
usethis::use_rstudio()


#Tools --> Project Tools

#Need to add the tag: (#' @export) for every function you want to export!

#Once you add the function, document them, add description what each one does: then run this
devtools::document()

#3 main commands:
document()
check()
install()

#To use ggplot: as optional way to plot:

# the suggested package is required
my_fun <- function(a, b) {
  if (!requireNamespace("pkg", quietly = TRUE)) {
    stop(
      "Package \"pkg\" must be installed to use this function.",
      call. = FALSE
    )
  }
  # code that includes calls such as pkg::f()
}

# the suggested package is optional; a fallback method is available
my_fun <- function(a, b) {
  if (requireNamespace("pkg", quietly = TRUE)) {
    pkg::f()
  } else {
    g()
  }
}
