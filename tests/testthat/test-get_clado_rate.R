context("get_clado_rate")

test_that("use area constant diversity-independent without hyper_pars", {
  default_pars <- DAISIE:::create_default_pars(
    island_ontogeny = 0,
    sea_level = 0,
    totaltime = 5,
    area_pars = NULL,
    hyper_pars = NULL
  )
  expect_silent(
    DAISIE:::get_clado_rate(lac = 2,
                            hyper_pars = default_pars$hyper_pars,
                            num_spec = 0,
                            K = 10,
                            A = 1
    )
  )
})

test_that("use area constant diversity-independent with hyper_pars", {
  ps_clado_rate <- 0.2
  carr_cap <- 9
  n_species <- 4
  default_pars <- DAISIE:::create_default_pars(
    island_ontogeny = 0,
    sea_level = 0,
    totaltime = 5,
    area_pars = NULL,
    hyper_pars = DAISIE:::create_hyper_pars(d = 4, x = 3)
  )
  created <- DAISIE:::get_clado_rate(
    lac = ps_clado_rate,
    hyper_pars = default_pars$hyper_pars,
    num_spec = n_species,
    K = carr_cap,
    A = 1
  )
  expected <- DAISIE_calc_clade_clado_rate(
    ps_clado_rate = ps_clado_rate,
    n_species = n_species,
    carr_cap = carr_cap
  )
  expect_equal(created, expected)
})

test_that("use area constant diversity-dependent without hyper_pars", {
  ps_clado_rate <- 0.2
  carr_cap <- 9
  n_species <- 4
  default_pars <- DAISIE:::create_default_pars(
    island_ontogeny = 0,
    sea_level = 0,
    totaltime = 5,
    area_pars = NULL,
    hyper_pars = NULL
  )

  created <- DAISIE:::get_clado_rate(
    lac = ps_clado_rate,
    hyper_pars = default_pars$hyper_pars,
    num_spec = n_species,
    K = carr_cap,
    A = 1
  )
  expected <- DAISIE_calc_clade_clado_rate(
    ps_clado_rate = ps_clado_rate,
    n_species = n_species,
    carr_cap = carr_cap
  )
  expect_equal(created, expected)
})

test_that("use area constant diversity-dependent with hyper_pars", {
  ps_clado_rate <- 0.2
  carr_cap <- 9
  n_species <- 4
  ps_clado_rate <- 0.2
  carr_cap <- 9
  n_species <- 4
  default_pars <- DAISIE:::create_default_pars(
    island_ontogeny = 0,
    sea_level = 0,
    totaltime = 5,
    area_pars = NULL,
    hyper_pars = NULL
  )

  created <- DAISIE:::get_clado_rate(
    lac = ps_clado_rate,
    hyper_pars = default_pars$hyper_pars,
    num_spec = n_species,
    K = carr_cap,
    A = 1
  )
  expected <- DAISIE_calc_clade_clado_rate(
    ps_clado_rate = ps_clado_rate,
    n_species = n_species,
    carr_cap = carr_cap
  )
  expect_equal(created, expected)
})

test_that("use area constant diversity-independent with hyper_pars", {
  ps_clado_rate <- 0.2
  carr_cap <- Inf
  n_species <- 4
  default_pars <- DAISIE:::create_default_pars(
    island_ontogeny = 0,
    sea_level = 0,
    totaltime = 5,
    area_pars = NULL,
    hyper_pars = NULL
  )
  created <- DAISIE:::get_clado_rate(
    lac = ps_clado_rate,
    hyper_pars = default_pars$hyper_pars,
    num_spec = n_species,
    K = carr_cap,
    A = 1
  )
  expected <- DAISIE_calc_clade_clado_rate(
    ps_clado_rate = ps_clado_rate,
    n_species = n_species,
    carr_cap = Inf
  )
  expect_equal(created, expected)
})

test_that("use area variable (ontogeny) diversity-dependent without
          hyper_pars", {
  ps_clado_rate <- 0.2
  carr_cap <- 9
  n_species <- 4
  default_pars <- DAISIE:::create_default_pars(
    island_ontogeny = 1,
    sea_level = 0,
    totaltime = 5,
    area_pars = DAISIE:::create_area_pars(
      max_area = 1,
      proportional_peak_t = 0.5,
      peak_sharpness = 1,
      total_island_age = 10,
      sea_level_amplitude = 0,
      sea_level_frequency = 0,
      island_gradient_angle = 0
    ),
    hyper_pars = NULL
  )
  created <- DAISIE:::get_clado_rate(
    lac = ps_clado_rate,
    hyper_pars = default_pars$hyper_pars,
    num_spec = n_species,
    K = carr_cap,
    A = DAISIE::island_area(timeval = 5,
                    area_pars =
                      default_pars$area_pars,
                    island_ontogeny = 1,
                    sea_level = 0)
  )
  expected <- DAISIE_calc_clade_clado_rate(
    ps_clado_rate = ps_clado_rate,
    n_species = n_species,
    carr_cap = carr_cap
  )
  expect_equal(created, expected)
})

test_that("use area variable (ontogeny) diversity-dependent with hyper_pars", {
  ps_clado_rate <- 0.2
  carr_cap <- 9
  n_species <- 4
  default_pars <- DAISIE:::create_default_pars(
    island_ontogeny = 0,
    sea_level = 0,
    totaltime = 5,
    area_pars = DAISIE:::create_area_pars(1, 0.5, 1, 10, 0, 0, 0),
    hyper_pars = DAISIE:::create_hyper_pars(
      d = 4,
      x = 3
    )
  )
  created <- DAISIE:::get_clado_rate(
    lac = ps_clado_rate,
    hyper_pars = default_pars$hyper_pars,
    num_spec = n_species,
    K = carr_cap,
    A = DAISIE::island_area(timeval = 5,
                    area_pars = default_pars$area_pars,
                    island_ontogeny = 1,
                    sea_level = 0)
  )
  expected <- DAISIE_calc_clade_clado_rate(
    ps_clado_rate = ps_clado_rate,
    n_species = n_species,
    carr_cap = carr_cap
  )
  expect_equal(created, expected)
})

test_that("use area variable (sea-level) diversity-dependent without
          hyper_pars", {
  ps_clado_rate <- 0.2
  carr_cap <- 9
  n_species <- 4
  default_pars <- DAISIE:::create_default_pars(
    island_ontogeny = 0,
    sea_level = 1,
    totaltime = 5,
    area_pars = DAISIE:::create_area_pars(1, 0, 0, 10, 1, 1, 45),
    hyper_pars = DAISIE:::create_hyper_pars(
      d = 0,
      x = 0
    )
  )
  created <- DAISIE:::get_clado_rate(
    lac = ps_clado_rate,
    hyper_pars = default_pars$hyper_pars,
    num_spec = n_species,
    K = carr_cap,
    A = DAISIE::island_area(timeval = 5,
                    area_pars = default_pars$area_pars,
                    island_ontogeny = 0,
                    sea_level = 1)
  )
  expected <- DAISIE_calc_clade_clado_rate(
    ps_clado_rate = ps_clado_rate,
    n_species = n_species,
    carr_cap = carr_cap
  )
  expect_equal(created, expected)
})

test_that("use area variable (sea-level) diversity-dependent with hyper_pars", {
  ps_clado_rate <- 0.2
  default_pars <- DAISIE:::create_default_pars(
    island_ontogeny = 0,
    sea_level = 1,
    totaltime = 5,
    area_pars = DAISIE:::create_area_pars(1, 0, 0, 10, 1, 1, 45),
    hyper_pars = DAISIE:::create_hyper_pars(
      d = 1,
      x = 3
    )
  )
  ps_clado_rate <- 0.2
  carr_cap <- 9
  n_species <- 4
  created <- DAISIE:::get_clado_rate(
    lac = ps_clado_rate,
    hyper_pars = default_pars$hyper_pars,
    num_spec = n_species,
    K = carr_cap,
    A = DAISIE::island_area(timeval = 5,
                    area_pars = default_pars$area_pars,
                    island_ontogeny = 0,
                    sea_level = 1)
  )
  expected <- DAISIE_calc_clade_clado_rate(
    ps_clado_rate = ps_clado_rate,
    n_species = n_species,
    carr_cap = carr_cap
  )
  expect_equal(created, expected)
})

test_that("use area variable (ontogeny and sea-level) diversity-dependent
          without hyper_pars", {
  ps_clado_rate <- 0.2
  default_pars <- DAISIE:::create_default_pars(
    island_ontogeny = 0,
    sea_level = 1,
    totaltime = 5,
    area_pars = DAISIE:::create_area_pars(1, 0.5, 1, 10, 1, 1, 45),
    hyper_pars = DAISIE:::create_hyper_pars(
      d = 0,
      x = 0
    )
  )
  ps_clado_rate <- 0.2
  carr_cap <- 9
  n_species <- 4
  created <- DAISIE:::get_clado_rate(
    lac = ps_clado_rate,
    hyper_pars = default_pars$hyper_pars,
    num_spec = n_species,
    K = carr_cap,
    A = DAISIE::island_area(timeval = 5,
                    area_pars = default_pars$area_pars,
                    island_ontogeny = 1,
                    sea_level = 1)
  )
  expected <- DAISIE_calc_clade_clado_rate(
    ps_clado_rate = ps_clado_rate,
    n_species = n_species,
    carr_cap = carr_cap
  )
  expect_equal(created, expected)
})

test_that("use area variable (ontogeny and sea-level) diversity-dependent with
          hyper_pars", {
  ps_clado_rate <- 0.2
  default_pars <- DAISIE:::create_default_pars(
    island_ontogeny = 0,
    sea_level = 1,
    totaltime = 5,
    area_pars = DAISIE:::create_area_pars(1, 0.5, 1, 10, 1, 1, 45),
    hyper_pars = DAISIE:::create_hyper_pars(
      d = 1,
      x = 3
    )
  )
  ps_clado_rate <- 0.2
  carr_cap <- 9
  n_species <- 4
  created <- DAISIE:::get_clado_rate(
    lac = ps_clado_rate,
    hyper_pars = default_pars$hyper_pars,
    num_spec = n_species,
    K = carr_cap,
    A = DAISIE::island_area(timeval = 5,
                    area_pars = default_pars$area_pars,
                    island_ontogeny = 1,
                    sea_level = 1)
  )
  expected <- DAISIE_calc_clade_clado_rate(
    ps_clado_rate = ps_clado_rate,
    n_species = n_species,
    carr_cap = carr_cap
  )
  expect_equal(created, expected)
})
