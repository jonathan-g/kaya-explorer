kayatool
========

**GitHub:** [![Build Status](https://travis-ci.org/gilligan-ees-3310/kayatool.svg?branch=master)](https://github.com/gilligan-ees-3310/kayatool/commits/master)

**GitLab:** [![Build Status](https://gitlab.jgilligan.org/gilligan_teaching/ees_3310/ees_3310_software/kayatool/badges/master/build.svg)](https://gitlab.jgilligan.org/gilligan_teaching/ees_3310/ees_3310_software/kayatool/commits/master)

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
