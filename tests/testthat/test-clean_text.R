
test_that("string cleaning works", {

  txt = clean_text( c( "  . ", "", "dog.      cat") )

  expect_equal( txt, c( "","", "dog cat" ) )

})
