context("get_ana_rate")

test_that("use area constant diversity-independent", {
  ps_ana_rate <- 1
  n_immigrants <- 5
  expect_silent(ana_rate <- DAISIE:::get_ana_rate(
    laa = 1,
    num_immigrants = 5)
  )
  expect_true(is.numeric(ana_rate))
  expected <- DAISIE_calc_clade_ana_rate(
    ps_ana_rate = ps_ana_rate,
    n_immigrants = n_immigrants
  )
  created <- DAISIE:::get_ana_rate(
    laa = 1,
    num_immigrants = 5
  )
  expect_equal(expected, created)
})

test_that("use area constant diversity-independent", {
  ps_ana_rate <- 1
  n_immigrants <- 5
  expect_silent(ana_rate <- DAISIE:::get_ana_rate(
    laa = ps_ana_rate,
    num_immigrants = 5
  )
  )
  expect_true(is.numeric(ana_rate))
  expected <- DAISIE_calc_clade_ana_rate(
    ps_ana_rate = ps_ana_rate,
    n_immigrants = n_immigrants
  )
  created <- DAISIE:::get_ana_rate(
    laa = 1,
    num_immigrants = 5
  )
  expect_equal(expected, created)
})
