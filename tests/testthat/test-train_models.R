
test_that("training machine learners works", {

  data( toy_reads )

  tt = dplyr::bind_rows(toy_reads, toy_reads, toy_reads )
  tt$text = paste( tt$text,
                   sample( c("dog", "cat", "pig", "cow", tt$text), nrow(tt), replace = TRUE ),
                   sample( c( "cow", "pig", "cat", "goat"), nrow(tt), replace = TRUE ), sep = " " )
  tt$Q2 = tt$Q2 + rnorm( nrow(tt) )
  expect_warning( feat <- generate_features( tt$text, lex = TRUE, sent = FALSE ) )
  colnames(feat)
  expect_true( is.matrix( feat ) )

  mods <- train_models( feat, tt$Q2, methods = c("glmnet", "knn" ),
                        include_BART = FALSE, verbose = FALSE )
  names(mods)

  expect_true( is.list( mods ) )
  # This is actually 4 due to gbm?
  # expect_equal( length(mods), 3 )

  pd = predict( mods$glmnet, feat )
  pds = predict( mods, feat )
  expect_equal( length( pds ), length(mods) )
  expect_equal( length( pds[[1]]$pred ), nrow(feat) )

  class( dplyr::bind_cols( pds ) )

  tt$text = NULL
  pds = add_predictions( mods, feat, tt )
  head( pds )
  expect_true( !is.null( pds$mod_glmnet ) )
})





