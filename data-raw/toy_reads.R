## code to prepare `toy_reads` dataset goes here

source( here::here( "reads-replication/setup.R" ) )

pilot = read.csv( reads_file_path("Data/G1sci_pilot_dat.csv") )

  # For testing
  # Cut down for illustration purposes (make faster)
set.seed(103403)
toy_reads = slice_sample( pilot, n=20 )
names(toy_reads)

usethis::use_data(toy_reads, overwrite = TRUE)


liwc = read.csv(  reads_file_path("Generated Data/pilot/pilot_LIWC.csv" ) )
head( liwc )
liwc = dplyr::semi_join( liwc, toy_reads, by = c( "Source..A." = "ID" ) )
nrow( liwc )
write_csv( liwc, here::here( "data/reads_liwc.csv" ) )


taaco = read_csv( reads_file_path("Generated Data/pilot/pilot_taaco_results.csv") )
tr = toy_reads %>%
  mutate( Filename = paste0( ID, ".txt" ) )
taaco = dplyr::semi_join(taaco, tr, by="Filename" )
nrow(taaco)
write_csv( taaco, here::here( "data/reads_taaco.csv" ) )
