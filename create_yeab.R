library(usethis)
library(devtools)
setwd("../") # fijar wd un directorio antes
use_description(
  fields = list(
    Title = "Package To Analyze Data From Analysis Of Behavior Experiments",
    Description = "This is a colletion of functions aimed to analyze data from behavioral
                   experiments from MED output and others. It also have functions to fit exponential or
                   hyperbolic models from delay discounting tasks, exponential mixtures to IRTs, Gaussian
                  plus ramp model for peak procedures data, etc.",
    `Authors@R` = c(
      person(
        "Emmanuel", "Alcala",
        email = "jealcala@gmail.com",
        role = c("aut", "cre")
      ),
      person(
        "Rodrigo", "Sosa",
        email = "rsosas@up.edu.mx",
        role = "aut"
      )
    ),
    License = "CC0"
  )
)


pck_import <- c("dplyr", "minpack.lm", "scales", "ggplot2", "VGAM", "grid", "gridExtra")

sapply(pck_import, use_package)
