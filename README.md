kayatool
========

The `kayatool` package is an R Shiny package to interactively explore the Kaya identity for a number of countries and regions of the world.

Installation
------------

To install this package you can use the `devtools` package in RStudio to run

    library(devtools)
    install_github("gilligan-ees-3310/kayatool")

and then load the package with

    library(kayatool)

or use the `pacman` package to load `kayatool`, installing it first if necessary:

    library(pacman)
    p_load_gh("gilligan-ees-3310/kayatool")

Once you have loaded the package, you can launch it from RStudio with

    launch_kaya_tool()
