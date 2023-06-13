
# Header library load script for all the replication files

options(stringsAsFactors = FALSE)

library(tm)
library(quanteda)
library(textmatch)
library(data.table)
library(tidyverse)
library(quanteda.sentiment)
library(quanteda.dictionaries)
library(quanteda.textstats)

#devtools::install_github( "https://github.com/cran/softmaxreg" )
library( softmaxreg )

# Our library
library( tada )


reads_file_path <- function( file ) {
  here::here( "../../", file )
}
