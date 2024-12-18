rcttext
================

An R package for impact analysis in randomized trials with text outcomes

## Installation
You can install the development version of **rcttext** from GitHub with:

``` r
# the devtools package needs to be installed for this to work
devtools::install_github("reaganmozer/rcttext") 
```

The package is not yet on CRAN.  It relies on some other packages that may need instillation:
``` r
devtools::install_github( "https://github.com/quanteda/quanteda.sentiment" )
```


**rcttext** provides a flexible and user-friendly toolkit for performing impact analysis in randomized trials with outcomes generated through human, machine, and/or hybrid scoring of text data. Provides functionality for feature extraction and aggregaion, applying supervised and unsupervised machine learning models for semi- automated text scoring, estimating model-assisted treatment impacts with respect to text outcomes under various randomized designs, visually representing found impacts on text outcomes, and additional functionality for performing text analysis using existing frameworks, especially quanteda.



**rcttext** provides a flexible and user-friendly toolkit for performing impact analysis in randomized trials with outcomes generated through human, machine, and/or hybrid scoring of text data. Provides functionality for feature extraction and aggregation, applying supervised and unsupervised machine learning models for semi- automated text scoring, estimating model-assisted treatment impacts with respect to text outcomes under various randomized designs, visually representing found impacts on text outcomes, and additional functionality for performing text analysis using existing frameworks, especially quanteda.
