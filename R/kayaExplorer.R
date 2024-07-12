# kayaExplorer is open-source software; you can redistribute it and/or
# modify it under the terms of the MIT License as published by the Open Source
# Initiative.
#
# kayaExplorer is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the MIT License for more details.
#
# You should have received a copy of the MIT License along with this program; if
# not, see <https://opensource.org/licenses/MIT>.

#' @title kayaExplorer package
#'
#' @description
#'
#' A Shiny-based explorer for Kaya identity. Loads data from the kayadata package and
#' provides an interactive shiny app for exploring the data: choose a country or region and
#' examine trends in the Kaya variables (population, gross domestic product, primary energy
#' consumption, and carbon dioxide emissions), compare historical trends to the implied trends
#' necessary to hit decarbonization policy targets (e.g., 70% reduction of emissions by 2050).
#' Also allows the user to explore the fuel mix that different countries and regions use to
#' supply their energy needs.
#'
#' @section License:
#'
#'  The \pkg{kayaExplorer} package is open source licensed under the
#'  MIT License.
#'
#' @section Bug reports:
#' \itemize{
#'  \item kayaExplorer issue tracker (\url{https://github.com/jonathan-g/kaya-explorer/issues})
#' }
#'
#' @name kayaExplorer-package
#'
#' @import ggplot2
#' @import rlang
#' @importFrom magrittr "%>%" "%$%"
#'
'_PACKAGE'
