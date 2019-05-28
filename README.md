kayatool
========

**GitHub:** [![Build
Status](https://travis-ci.org/jonathan-g/kayatool.svg?branch=master)](https://github.com/jonathan-g/kayatool/commits/master)

**GitLab:** [![Build
Status](https://gitlab.jgilligan.org/gilligan_teaching/ees_3310/ees_3310_software/kayatool/badges/master/build.svg)](https://gitlab.jgilligan.org/gilligan_teaching/ees_3310/ees_3310_software/kayatool/commits/master)

The `kayatool` package is an R Shiny package to interactively explore
the Kaya identity for a number of countries and regions of the world.

Installation
------------

To install this package you can use the `devtools` package in RStudio to
run

    library(devtools)
    install_github("jonathan-g/kayatool")

and then load the package with

    library(kayatool)

or use the `pacman` package to load `kayatool`, installing it first if
necessary:

    library(pacman)
    p_load_current_gh("jonathan-g/kayatool")

Once you have loaded the package, you can launch it from RStudio with

    launch_kaya_tool()

Credits
-------

This tool was inspired by Roger Pielke’s book, [*The Climate
Fix*](https://books.google.com/books/about/The_Climate_Fix.html?id=WgcCoYsR41IC)
and the tool closely follows the analysis presented in chapters 3–4 of
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
