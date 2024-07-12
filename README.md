# kayaExplorer

<!-- badges: start -->

[![CRAN Status
Badge](https://www.r-pkg.org/badges/version-last-release/kayaExplorer)](https://cran.r-project.org/package=kayaExplorer)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://github.com/jonathan-g/kayaExplorer/workflows/R-CMD-check/badge.svg)](https://github.com/jonathan-g/kayaExplorer/actions)
**GitLab:** [![Build
Status](https://gitlab.jgilligan.org/gilligan_teaching/ees_3310/ees_3310_software/kaya-explorer/badges/master/build.svg)](https://gitlab.jgilligan.org/gilligan_teaching/ees_3310/ees_3310_software/kaya-explorer/commits/master)
<!-- badges: end -->

The `kayaExplorer` package is an R Shiny package to interactively
explore the Kaya identity for a number of countries and regions of the
world.

## Installation

To install this package, you can install the latest release from CRAN:

    install.packages('kayaExplorer')

To install this package you can use the `devtools` package in RStudio to
run

    library(devtools)
    install_github("jonathan-g/kayaExplorer")

and then load the package with

    library(kayaExplorer)

or use the `pacman` package to load `kayaExplorer`, installing it first
if necessary:

    library(pacman)
    p_load_current_gh("jonathan-g/kayaExplorer")

Once you have loaded the package, you can launch it from RStudio with

    launch_kaya_explorer()

## Credits

This interactive application was inspired by Roger Pielke’s book, [*The
Climate
Fix*](https://books.google.com/books/about/The_Climate_Fix.html?id=WgcCoYsR41IC)
and the app closely follows the analysis presented in chapters 3–4 of
that book.

See also,

-   R.A. Pielke, Jr., “[The British Climate Change Act: A Critical
    Evaluation and Proposed Alternative
    Approach](https://doi.org/10.1088/1748-9326/4/2/024010),” *Environ.
    Res. Lett.* **4**, 024010 (2009). doi
    [10.1088/1748-9326/4/2/024010](https://doi.org/10.1088/1748-9326/4/2/024010)
-   R.A. Pielke, Jr., “[Mamizu Climate Policy: An Evaluation of Japanese
    Carbon Emissions Reduction
    Targets](https://doi.org/10.1088/1748-9326/4/4/044001),” *Environ.
    Res. Lett.* **4**, 044001 (2009). doi
    [10.1088/1748-9326/4/4/044001](https://doi.org/10.1088/1748-9326/4/4/044001)
-   R.A. Pielke, Jr., “[An Evaluation of the Targets and Timetables of
    Proposed Australian Emissions Reduction
    policies](https://doi.org/10.1016/j.envsci.2010.10.008),” *Environ.
    Sci. & Pol.* **14**, 20–27 (2011). doi
    [10.1016/j.envsci.2010.10.008](https://doi.org/10.1016/j.envsci.2010.10.008)
