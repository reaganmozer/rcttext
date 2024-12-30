test_that("pairwise distance calculations work", {

  data( "toy_reads" )
  rr = toy_reads$text[c( 1,3,10 )]

  toy_reads$text[1] = ""
  toy_reads$text[3] = "dfjkdfskjgsdfarf"
  expect_warning( d <- generate_distance_features( toy_reads$text,
                                                   meta=toy_reads[ , c("ID","Q1","Q2","more") ],
                                                   rr ) )
  toy_reads$text[1] = "This is a test"
  d <- generate_distance_features( toy_reads$text,
                                   meta=NULL,
                                   rr )

  d
  expect_equal( ncol( d ), 3 )
  head( d )
  expect_true( is.data.frame( d ) )
  expect_equal( d$doc_3[10], 0 )

  names( rr ) = c( "A", "Bob", "Cat" )
  d = generate_distance_features( toy_reads$text,
                                  meta=toy_reads[ , c("ID","Q1","Q2","more") ],
                                  rr, method = "w2v" )
  d
  head( d )
  expect_true( all( c( "doc_A", "doc_Bob", "doc_Cat" ) %in% colnames( d ) ) )
})
