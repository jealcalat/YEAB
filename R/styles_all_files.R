sapply( getOption("defaultPackages"), require, character.only = TRUE)
pkg <- c("tidyverse", "default", "grid", "gridExtra")

new.packages <- pkg[!(pkg %in% installed.packages()[,"Package"])]

# Install packages. The condition inside if() here is an integer,
# but R evaluates it as logical: if length(new.packages) is not 0,
# then is TRUE and procedes with the statement 'install.packages()'

if(length(new.packages)){
  install.packages(new.packages)}

sapply(pkg, require, character.only = TRUE)


#### Current file: add_shims.R 

#' @title Add Shims
#'
#' @description Add new shims.
#'
add_shims <- function () {
  attach (.shims,
          name = "shims",
          warn.conflicts = FALSE)
}


#### Current file: apply_defaults.R 

#' @title Apply Defaults
#'
#' @description Reapplys the defaults.
#'
apply_defaults <- function (style) {

  package_styles <- style[names(style) != "par"]
  package_names <- names(package_styles)

  for (i in seq_along(package_styles)) {

    package <- package_styles[[i]]
    fun_names <- names(package)

    for (j in seq_along(package))
      set_default(fun_names[j], package[[j]], package = package_names)

  }

}

#' 
#' 
#' #### Current file: check_style.R 

#' @title Check Style
#'
#' @description Checks the current style.
#'
#' @export
#'
#'
check_style <- function (list) {}


#### Current file: example_plots.R 

#' @title Example Plots
#'
#' @description Plots four plots (hist, bar, dot, and box) to demonstrate style.
#'
#' @export
#'
example_plots <- function () {

  with(mtcars, hist(qsec))

  plot(mpg ~ wt, data = mtcars)

  tab <- tapply(mtcars$qsec, paste(mtcars$cyl, "cyl"), mean)
  barplot(tab, ylab = "qsec")

  boxplot(qsec ~ paste(mtcars$gear, "gear"),
          data = mtcars,
          ylab = "qsec")

}


#### Current file: get_old_par.R 

#' @title Get Old Par
#'
#' @description Gets the old par.
#'
get_old_par <- function (style) {
  op <- par(no.readonly = TRUE)
  op <- op[names(style$par)]
  .cache$old_par <- op
}


#### Current file: new_style.R 

#' @title New Style.
#'
#' @description Exports the new style
#'
#' @include check_style.R
#'
#' @export
#'
#'
new_style <- function (..., inherit = list()) {
  list <- list(...)
  check_style(list)
  inherit[names(list)] <- list
  class(inherit) <- c("style", class(inherit))
  inherit
}


#### Current file: plot_style.R 

#' @title Plot Style
#'
#' @description Plots internal styles to show user what they look like
#'
#' @include example_plots.R
#'
#' @export
#'
plot_style <- function(x, y, ...){

  on.exit( {remove_style()} )
  plot_mat <- matrix(1:4, nrow = 2, byrow = FALSE)
  layout(plot_mat)
  style(x)
  example_plots()

}


#### Current file: prep.R 

.cache <- new.env()
.cache$old_par <- list()
.cache$current_style <- list()

.shims <- new.env()


#### Current file: remove_default.R 

#' @title Remove Default
#'
#' @description Remove the defaults.
#'
#' @export
#'
remove_default <- function (fun_name, package = "graphics") {

  ns <- asNamespace(package)
  fun <- get(fun_name, ns)
  fun <- reset_default(fun)

  unlockBinding(fun_name, ns)
  ns[[fun_name]] <- fun
  lockBinding(fun_name, ns)

  rm(list = fun_name, envir = .shims)

}


#### Current file: remove_defaults.R 

#' @title Remove Defaults
#'
#' @description Removes the defaults.
#'
#'@include remove_default.R
#'
remove_defaults <- function () {

  package_styles <- .cache$current_style[names(.cache$current_style) != "par"]
  package_names <- names(package_styles)

  for (i in seq_along(package_styles)) {

    lapply(names(package_styles[[i]]),
           remove_default,
           package = package_names[i])

  }

}


#### Current file: remove_shims.R 

#' @title Remove Shims
#'
#' @description Removes current shims.
#'
remove_shims <- function () {
  if ("shims" %in% search())
    detach ("shims")
}


#### Current file: remove_style.R 

#' @title Remove Style.
#'
#' @description Removes the current style.
#'
#' @include remove_defaults.R remove_shims.R
#'
#' @export
#'
#'
remove_style <- function() {
  remove_shims()
  par(.cache$old_par)
  remove_defaults()
  .cache$current_style <- list()
}


#### Current file: set_default.R 

#' @title Set Default
#'
#' @description Set the defaults.
#'
set_default <- function (fun_name, value, package = "graphics") {

  ns <- asNamespace(package)
  fun <- get(fun_name, ns)
  default(fun) <- value

  unlockBinding(fun_name, ns)
  ns[[fun_name]] <- fun
  lockBinding(fun_name, ns)

  .shims[[fun_name]] <- fun

}


#### Current file: style.R 

#' @title Style.
#'
#' @description Create new style.
#'
#' @export
#'
#'
style <- function (style) {
  check_style()
  remove_style()
  .cache$current_style <- style
  get_old_par(style)
  par(style$par)
  apply_defaults(style)
  add_shims()
  invisible(NULL)
}

#' #### Current file: blue_dot_grey 
#' 
#' #' @title blue_dot_grey
#' #'
#' #' @description Internal style
#' #'
#' #' @include new_style.R
#' #'
#' #' @export
#' #'
blue_dot_grey <- new_style(par = list(pch = 21,
                                      cex = 1.2,
                                      bty = "o",
                                      las = 1,
                                      lwd = 2,
                                      lend = "butt",
                                      family = "sans-serif",
                                      tck = -0.015,
                                      xaxs = 'r',
                                      mgp = c(1.8, 0.3, 0),
                                      col.axis = grey(0.4),
                                      cex.axis = 1.2,
                                      cex.lab = 1.2,
                                      col.lab = grey(0.2),
                                      fg = grey(0.4),
                                      mar = c(3, 3.5, 1.5, 0.5)),
                           graphics = list(plot.xy = list(col = 1,
                                                          pch = 21,
                                                          bg = 8),
                                           hist.default = list(col = "navyblue",
                                                               border = "white",
                                                               main = "",
                                                               ylab = "count"),
                                           boxplot.default = list(col = scales::alpha("navyblue", 0.5),
                                                                  border = "navyblue",
                                                                  pars = list(boxwex = 0.8,
                                                                              staplewex = 0.5,
                                                                              outwex = 0.5,
                                                                              cex = 1,
                                                                              whisklty = "solid")),
                                           axis = list(lwd = 1.5,
                                                       col.ticks = grey(0.2))))

#' #### Current file: better.R 
#' 
#' #' @title Better
#' #'
#' #' @description Internal style
#' #'
#' #' @include new_style.R
#' #'
#' #' @export
#' #'
better <- new_style(par = list(pch = 16,
                             fg = grey(0.6),
                             col.axis = grey(0.4),
                             col.lab = grey(0.2),
                             las = 1,
                             col.main = grey(0.4),
                             col.sub = grey(0.4),
                             tcl = -0.25,
                             bty="l"),
                  graphics = list(axis = list(hadj = 0.5),
                                  hist.default = list(col = "navyblue",
                                                      border = "white",
                                                      main = "",
                                                      ylab = "count"),
                                  plot.xy = list(col = "navyblue",
                                                 lwd = 2.5,
                                                 cex = 1.1),
                                  barplot.default = list(col = "navyblue",
                                                         border = NA,
                                                         space = 0.3),
                                  boxplot.default = list(col = scales::alpha("navyblue", 0.5),
                                                         border = "navyblue",
                                                         pars = list(boxwex = 0.8,
                                                                     staplewex = 0.5,
                                                                     outwex = 0.5,
                                                                     cex = 1,
                                                                     whisklty = "solid")),
                                  title = list(line = 2)))