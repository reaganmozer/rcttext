## code to prepare `mini_glove` dataset


source( here::here( "reads-replication/setup.R" ) )

freq = read_csv( "data-raw/unigram_freq.csv" )
head( freq )

pilot = read.csv( reads_file_path("Data/G1sci_pilot_dat.csv") )

stops = stopwords::stopwords()
length( stops )
head( stops )

sources = stopwords::stopwords_getsources()
library( tidyverse )
library( purrr )
safe_stop = safely( stopwords::stopwords )
mp = map( sources, function( s ) {
  print( s )
  safe_stop( source = s )
}  )
mp = transpose(mp)
mp = as_tibble(mp)
mp$keep = map_lgl(mp$error, is.null )
mp = filter( mp, keep )
mp
mp = unique( sort( do.call( c, mp$result ) ) )
length( mp )
mp = tibble( word = mp )
nrow(mp)
sample_n( mp, 20 )
head( mp)
head(freq)
nr <- nrow(freq)
freq_small = anti_join(freq, mp)
nr - nrow(freq_small)

head( freq )
freq_small = mutate( freq_small,
                     fr = count / 9081174698 )
head( freq_small )
freq_small = freq_small[1:1000,]
sample( freq_small$word, 20 )

load( "data-raw/glove.50d.RData")
gd <- rownames_to_column( glove.50d, var="word" )
gd <- dplyr::semi_join(gd, freq_small)
nrow(gd)

mini_glove = column_to_rownames(gd,var = "word") %>%
  as.matrix()
head(mini_glove)

dim(mini_glove)
usethis::use_data(mini_glove, overwrite = TRUE)
