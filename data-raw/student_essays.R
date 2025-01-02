## code to prepare `student_essays` dataset goes here

set.seed( 40404 )



pilot = read.csv( here::here( "../reads-replication/data-raw/write_machinelearning_pilot_replication.csv" ),
                  encoding='WINDOWS-1252')


pilot$response <- iconv(pilot$response, from='WINDOWS-1252', to='ASCII', sub=" ")

pilot <- pilot %>%
  mutate( studentid = row_number() ) %>%
  rename( text = response,
          treatment=more,
          Q1 = writing_quality_score_1,
          Q2 = writing_quality_score_2 ) %>%
  relocate( studentid ) %>%
  filter( text != "" )


names(pilot)
table(table(pilot$ID) )



# For testing
# Cut down for illustration purposes (make faster)
set.seed(103403)

student_essays = pilot %>%
  group_by( treatment ) %>%
  slice_sample( n=20) %>%
  ungroup() %>%
  select(-Q1) %>%
  rename(score=Q2)
names(student_essays)
table( student_essays$treatment)
mean( student_essays$treatment )

diff(tapply(student_essays$score, student_essays$treatment,mean )) # treatment effect in this sample


# Put thumb on the scale to make tx-co differences in text ----
nmore = sum( student_essays$treatment==1 )
nmore
tags = sample( c( " I love monkeys!", " apes are awesome!",
                  " monkeys rule!" ), nmore, replace=TRUE )
student_essays$text[student_essays$treatment==1] = paste0(
  student_essays$text[student_essays$treatment==1], tags )

usethis::use_data(student_essays, overwrite = T)

