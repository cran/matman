# matman 1.1.3 (13/12/2021)
* increase performance of functions by waiveing package ISOweek from the code
* new needed packages: lubridate, parsedate
* function aggregateData is now consistent for multiple value columns
* function expandData is now consistent for multiple boolean latest_values and multiple expand variables
* ABC-Analysis just uses positive sums.

# matman 1.1.2 (27/11/2020)

## Bug Fixes
* matmanDemo() works now if the package was installed from binary

# matman 1.1.1 (23/11/2020)

## Bug Fixes
* Warning of error at start of matmanDemo() because of installation from binary

# matman 1.1.0 (05/13/2020)

## New Features
* shiny app for demonstrating how to use the functions provided by package matman

## Bug Fixes
* fixed problems with identical column- and variable-names over all functions
* in function expandData(): ValueLevels can have one or multiple values for multiple valueColumns

## Changes
* method summary for ABCXYZComparison objects now can show missing categories
* functions overPerformer() and underPerformer() have opportunity to expand data in a first step

# matman 1.0.0 (04/28/2020)

## Initial Version
