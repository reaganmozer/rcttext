
test_that("multiplication works", {
  data( "toy_reads" )

  txt = toy_reads$text
  txt = txt[ !is.na(txt) & txt != "" ]
  txt[20]
  length(txt)
  trA = apply_hunspell( txt, verbose=TRUE, threshold = 0)
  expect_true( !all( trA == txt ) )

  txt[20]
  trA[20]

  tr = apply_hunspell( txt,
                       verbose=TRUE,
                       additional_words = c( "dont", "rainforest" ),
                       threshold = 0 )

  expect_true( all( tr == txt ) )
})
