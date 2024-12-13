
if ( FALSE ) {
  library( testthat )
}

test_that("basic feature generation works", {
  data( "toy_reads" )


  # Grab key columns of metadata
  all.feats = dplyr::select(toy_reads, ID, Q1, Q2, more)

  # Get text (and repair one piece of spelling)
  essay.text = toy_reads$text %>%
    repair_spelling( "shoud", "should" )
  expect_true( length(essay.text) == nrow(all.feats) )

  # Generate set of general features
  all.feats = generate_features(essay.text,
                                meta = all.feats,
                                sent = TRUE,
                                clean_features = FALSE,
                                terms = "xxx",
                                read = c("Flesch","Flesch.Kincaid", "ARI", "ELF",
                                         "meanWordSyllables"),
                                verbose = FALSE )
  names(all.feats)
  expect_true( is.data.frame(all.feats) )
  expect_equal( nrow( all.feats ), nrow( toy_reads ) )

  # Note: term of 'xxx' are illegible words/phrases
  tt <- table( all.feats$xxx )
  expect_true( tt[2] == 3 )

  # Add Word2Vec projections for each essay on 50 dimensions
  all.feats2 = extract_w2v( clean_text(essay.text),
                           meta = all.feats,
                           model = NULL )

  all.feats2
  expect_true( all( paste0( "X", 1:50 ) %in% colnames(all.feats2) ) )

  expect_true( all( colnames(all.feats) %in% colnames(all.feats2) ) )

  # Add externally computed LIWC-generated features
  all.feats3 <- extract_liwc( here::here( "data/reads_liwc.csv" ),
                             meta = all.feats2, ID.liwc = 1, ID.meta = "ID",
                             clean = FALSE )

  expect_true( !is.null( all.feats3$liwc_Quote ) )

  # And externally computed TAACO features
  all.feats4 <- extract_taaco( here::here( "data/reads_taaco.csv" ),
                              meta = all.feats3,
                              ID.meta = "ID" )

  names(all.feats)
  names(all.feats4)
  # Drop features we don't need/that are redundant
  dd <- dim( all.feats4 )
  ignames = c( "ID", "Q1", "Q2", "more" )

  names(all.feats4)
  expect_warning( all.feats4 <- clean_features( all.feats4,
                              ignore = ignames ) )

  dd2 <- dim( all.feats4 )
  expect_true( dd[1] == dd2[1] )
  expect_true( dd2[2] <= dd[2] )
  expect_true( all( colnames(all.feats)[1:4] == ignames ) )

})
