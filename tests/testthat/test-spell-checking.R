

test_that("spell checking works", {
  data( "toy_reads" )

  txt = clean_text( toy_reads$text )
  txt = txt[ !is.na(txt) & txt != "" ]
  txt[25]
  length(txt)
  trA = apply_hunspell( txt, verbose=TRUE, threshold = 0)
  expect_true( !all( trA == txt ) )

  which( trA != txt )
  txt[23]
  trA[23]

  tr = apply_hunspell( txt,
                       verbose=TRUE,
                       additional_words = c( "dont", "rainforest" ,
                                             "shouldn", "thats",
                                             "theyre", "youre" ),
                       threshold = 0 )

  expect_true( all( tr == txt ) )
})
