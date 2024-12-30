## code to prepare `toy_reads` dataset goes here

# THIS NEEDS TO BE FIXED --- THE Replication file has changed and I
# think I am loading the wrong dataset and the IDs don't match and we
# do not have the human coded outcomes merged in???

set.seed( 40404 )

source( here::here( "../reads-replication/scripts/00_setup.R" ) )


load( here::here( "../reads-replication/data-generated/all.pilot.RData" ) )

names( all.pilot )[1:30]

pilot = read.csv( here::here( "../reads-replication/data-raw/write_machinelearning_pilot_replication.csv" ),
                  encoding='WINDOWS-1252')
names( pilot )

pilot$response <- iconv(pilot$response, from='WINDOWS-1252', to='ASCII', sub=" ")

pilot <- pilot %>%
  mutate( ID = row_number(),
          Q1 = writing_quality_score_2 ) %>%
  rename( text = response,
          Q2 = writing_quality_score_2 ) %>%
  relocate( ID ) %>%
  filter( text != "" )

#pilot = read.csv( here::here( "../reads-replication/data-generated/text_pilot.csv" ) )
names(pilot)
table(table(pilot$ID) )

# For testing
# Cut down for illustration purposes (make faster)
set.seed(103403)
toy_reads = pilot %>%
  group_by( more ) %>%
  slice_sample( n=20 ) %>%
  ungroup()
names(toy_reads)
table( toy_reads$more )
mean( pilot$more )


# Put thumb on the scale to make tx-co differences in text ----
nmore = sum( toy_reads$more==1 )
nmore
tags = sample( c( " I love monkeys!", " apes are awesome!",
                  " monkeys rule!" ), nmore, replace=TRUE )
toy_reads$text[toy_reads$more==1] = paste0(
  toy_reads$text[toy_reads$more==1], tags )

usethis::use_data(toy_reads, overwrite = TRUE)






# Add in LIWC and TAACO scores ----

warning( "These LIWC and TAACO reads do not align anymore by ID---need to fix" )

liwc = read.csv(  here::here( "../reads-replication/data-generated/LIWC_pilot.csv" ) )
names( liwc )
names( toy_reads )
liwc = dplyr::semi_join( liwc, toy_reads, by = "ID" )
nrow( liwc ) # This should be 40????
write_csv( liwc, here::here( "data/reads_liwc.csv" ) )


taaco = read_csv( here::here( "../reads-replication/data-generated/taaco_pilot.csv" ) )
tr = toy_reads %>%
  mutate( Filename = paste0( ID, ".txt" ) )
taaco = dplyr::semi_join(taaco, tr, by="Filename" )
nrow(taaco)
write_csv( taaco, here::here( "data/reads_taaco.csv" ) )



