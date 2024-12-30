
library( tidyverse )

test_that("CCS functions work", {

  data( toy_reads )
  toy_reads$clean_text = clean_text( toy_reads$text )
  m1 = ccs_tuned_textreg( corpus = toy_reads$clean_text,
                          Z = toy_reads$more,
                          R = 10 )
  m2 = ccs_tuned_textreg( corpus = toy_reads$clean_text,
                          Z = toy_reads$more,
                          R = 10,
                          Lq = 4 )

  m3 = textreg::textreg( corpus = toy_reads$clean_text,
                         labeling = 2*toy_reads$more - 1,
                         verbosity = 0,
                         banned = c( "monkeys", "apes", "love"),
                         C = 4 )

  mods = list( m1 = m1, m2 = m2, m3 = m3 )

  ctbl <- ccs_list_table( mods )
  ctbl

  rtbl <- ccs_result_table( mods, toy_reads$clean_text,
                            toy_reads$more )
  rtbl
  expect_true( is.data.frame( rtbl ) )

  rtbl2 <- ccs_result_table( ctbl, toy_reads$clean_text,
                            toy_reads$more )
  expect_equal( rtbl, rtbl2 )

})
