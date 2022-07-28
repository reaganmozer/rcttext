## code to prepare `toy_reads` dataset goes here

source( here::here( "reads-replication/setup.R" ) )

pilot = read.csv( reads_file_path("Data/G1sci_pilot_dat.csv") )

  # For testing
  # Cut down for illustration purposes (make faster)
set.seed(103403)
toy_reads = slice_sample( pilot, n=20 )
names(toy_reads)

usethis::use_data(toy_reads, overwrite = TRUE)
