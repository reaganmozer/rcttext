
test_that("machine learning works", {

  data( toy_reads )

  expect_warning( features <- generate_features( toy_reads$text ) )

  bml <- best_ML( features, toy_reads$Q2, method = "cv" )
  bml$mean_performance
  expect_true( is.data.frame( bml$mean_performance ) )


  rs <- ML_iteration( features, toy_reads$Q2 )
  length( rs )

  expect_true( !is.null( rs ) )
})
