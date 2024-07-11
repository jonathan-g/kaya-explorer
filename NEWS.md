# kayatool 0.5.0

* Refactored into a structure that's better suited for a CRAN package.
* Fix error handling in data to deal with NAs and infinities more robustly.

# kayatool 0.4.2

* Fixed bug where an NA in the last year of the `kaya_data` set for a given 
  nation would cause NA's in the projected future values of the Kaya variables.

# kayatool 0.4.1

* Add LICENCE file (different to LICENSE) to kludge licensee detecction of MIT
  license on GitLab.
* Update DESCRIPTION with new version number and date, and update package
  dependencies.  Remove REMOTES dependency on GitHub version of kayadata.

# kayatool 0.4.0

Updated to work with kayadata library version >= 0.4.0

# kayatool 0.3.3

Updated to use kayadata library.

# kayatool 0.2.1

Moved github repository to https://github.com/gilligan-ees-3310/kayatool

# kayatool 0.2.0

* Rewrite data loading to work with BP Statistical Review of World Energy
  and World Bank population and economic data because that is more reliably
  up to date than the other sources I had used, and they cover more
  countries.
  
    The down side is that there aren't top-down predictions for most of the
    countries.

* Remove top-down tab.

#  kayatool 0.1.0

* Initial release
