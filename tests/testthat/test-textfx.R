
library( tidyverse )

test_that("estimating impacts on text features works", {
  set.seed(44404)
  dta = tibble( grp = rep( 1:10, each = 10 ),
                MORE = sample( c(0,1), 100, replace=TRUE),
                baseline = sample( c("A","B","C"), 100, replace=TRUE),
                X1 = rnorm(100) + 5 * (baseline=="A") + 10 * (baseline=="B") + 15 * (baseline=="C"),
                X2 = rnorm(100) + 0.2*MORE + grp,
                X3 = rnorm(100, mean = 500, sd=1000) + 1500*MORE,
                X4 = rnorm(100, sd=0.15) + 0.1*MORE + grp,
                X5 = round( rnorm(100) ) )

  rs <- impacts_on_features( dta, ignore = c( "grp", "MORE","baseline" ),
                       standardize = FALSE,
                       formula = ~ MORE )

  expect_true( rs$p.value[3] < 0.05 )

  rs3 <- impacts_on_features( dta, ignore = c("grp", "MORE","baseline" ),
                       standardize = FALSE,
                       formula = ~ MORE,
                       cluster = dta$grp )
  rs$std.error / rs3$std.error

  rs <- impacts_on_features( dta, ignore = c("MORE", "grp", "baseline" ),
                       standardize = FALSE,
                       formula = ~ MORE )
  expect_true( all( rs$p.adj >= rs$p.value ) )

  rs <- impacts_on_features( dta, ignore = c("grp", "MORE","baseline" ),
                       planned_features = c( "X1", "X3" ),
                       formula = ~ MORE )
  rs
  expect_true( all( rs$p.adj >= rs$p.value ) )
  expect_equal( rs$planned, c( 1,0,1,0,0) )
  expect_true( all( rs$estimate_std <= 2 ) )

  rs2 <- impacts_on_features( dta, ignore = c("MORE","baseline" ,"grp"),
                             planned_features = c( "X1", "X3" ),
                             formula = ~ MORE + baseline )
  cbind( rs$std.error_std, rs2$std.error_std)

  # We got massive precision gains in X1 due to controlling for baseline
  expect_true(rs2$std.error[[1]] < 0.5 * rs$std.error[[1]] )


  # Checking plotting function
  plt <- plot_textfx( rs2, standardize = FALSE )
  expect_equal( plt, 0 )

  plt2 <- plot_textfx( rs, standardize = FALSE )
  expect_equal( plt2, 0 )


})
