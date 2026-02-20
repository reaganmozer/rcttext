

test_that("clean_features works", {

  data( "toy_reads" )

  # Generate set of general features
  all.feats = generate_features( toy_reads$text,
                                meta = NULL,
                                sent = TRUE,
                                clean_features = FALSE,
                                terms = "you",
                                read = c("ELF", "meanWordSyllables"),
                                verbose = FALSE )

  expect_warning( afc <- clean_features( all.feats ) )
  ncol( all.feats )
  ncol( afc )
  expect_true( ncol( afc ) < ncol( all.feats ) )
})
