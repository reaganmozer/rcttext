## code to prepare `data/glove.50d.RData` dataset goes here

load( "data-raw/glove.50d.RData" )
usethis::use_data( glove.50d, overwrite = TRUE)
