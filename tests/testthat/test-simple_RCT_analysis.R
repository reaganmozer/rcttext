

test_that("simple_RCT_analysis works", {

  fake_data <- readRDS(here::here("data/fake_data.rds") )

  test_fake <- simple_RCT_analysis( fake_data$reason,
                                    formula = ~ el_ml, data=fake_data, cluster = fake_data$s_id )

  test_fake

  fake_data$el_ml = as.character(fake_data$el_ml)
  test_fake2 <- simple_RCT_analysis( fake_data$reason,
                                    formula = ~ el_ml, data=fake_data, cluster = fake_data$s_id )


  fake_data$el_ml = as.numeric( fake_data$el_ml )
  test_fake3 <- simple_RCT_analysis( fake_data$reason,
                                     formula = ~ el_ml, data=fake_data, cluster = fake_data$s_id )

  expect_equal( test_fake, test_fake2 )
  expect_equal( test_fake, test_fake3 )

})
