test_that("pairwise distance calculations work", {

  # sanity check on underlying method
  docs <- c( "dog", "cat", "pig", "dog", "cat cat cat", "pig cat dog", "tiger lion leapord jaguar")
  Z = c(1,0,0,0,1,1,0)
  docs <- quanteda::dfm(quanteda::tokens(docs))
  docs
  #debug( textmatch::pair_distances )
  tmp <- textmatch::pair_distances( docs, Z, include="cosine", form="data.frame" )
  expect_equal( nrow(tmp), 4*3 )


  corp1 = c( "harry harry dog", "dog cat pig", "dog cat", "hamster bunny", "house car moose", "zebra giraffe", "a", "b", "c", "d", "e", "f", "g" )
  corp0 = c( "dog cat pig", "zebra big cat fierce cat stripy cat elephant", "tiger lion leapord jaguar" )
  #debug( pairwise_distances )
  pd <- pairwise_distances( corp1, corp0, wide=TRUE )
  pd
  expect_true( pd$doc_1[2] == 0 )
  expect_true( pd$doc_2[6] < 1 )
  expect_true( all( pd$doc_3 == 1 ) )

  glove.50d = textdata::embedding_glove6b(dimensions = 50)

  pd_w2v = pairwise_distances( corp1, corp0, wide=TRUE, method="w2v", model=glove.50d )
  pd_w2v
  expect_true( pd$doc_1[2] == 0 )

  expect_equal( dim( pd ), dim( pd_w2v ) )


  names( corp0 ) = c( "domestic", "semiwild", "wild" )
  pdn = pairwise_distances( corp1, corp0, wide=TRUE )
  pdn
  expect_equal( pdn$doc_domestic, pd$doc_1 )

  pdf = pairwise_distances( corp0, corp1 )
  pdf
  expect_true( "doc_09" %in% colnames(pdf) )

  pdl = pairwise_distances( corp1, corp0, wide=FALSE )
  pdl
  expect_equal( nrow( pdl ), length(corp1) * length(corp0) )

})




library( testthat )

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
  d = generate_distance_features( clean_text( toy_reads$text ),
                                  meta=toy_reads[ , c("ID","Q1","Q2","more") ],
                                  rr, method = "w2v" )
  d
  head( d )
  expect_true( all( c( "doc_A", "doc_Bob", "doc_Cat" ) %in% colnames( d ) ) )

})


