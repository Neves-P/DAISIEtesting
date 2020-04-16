context("get_ext_rate")

test_that("use area constant diversity-independent without hyper_pars", {
  carr_cap <- 10
  ps_ext_rate <- 2
  n_species <- 5
  n_mainland_species <- 2
  default_pars <- DAISIE:::create_default_pars(
    island_ontogeny = 1,
    sea_level = 0,
    totaltime = 5,
    area_pars = DAISIE:::create_area_pars(
      max_area = 1,
      proportional_peak_t = 0,
      peak_sharpness = 0,
      total_island_age = 0,
      sea_level_amplitude = 0,
      sea_level_frequency = 0,
      island_gradient_angle = 0
    ),
    hyper_pars = NULL
  )

  expect_silent(ext_rate <- DAISIE:::get_ext_rate(
    mu = ps_ext_rate,
    hyper_pars = default_pars$hyper_pars,
    extcutoff = 1000,
    num_spec = 0,
    K = 10,
    A = DAISIE::island_area(timeval = 0,
                    area_pars = default_pars$area_pars,
                    island_ontogeny = 0,
                    sea_level = 0)
  ))
  expect_true(is.numeric(ext_rate))
  expected <- DAISIE_calc_clade_ext_rate(
    ps_ext_rate = ps_ext_rate,
    n_species = n_species
  )
  created <- DAISIE:::get_ext_rate(
    mu = ps_ext_rate,
    hyper_pars = default_pars$hyper_pars,
    extcutoff = 1000,
    num_spec = n_species,
    K = carr_cap,
    A = DAISIE::island_area(timeval = 10,
                    area_pars = default_pars$area_pars,
                    island_ontogeny = 0,
                    sea_level = 0)

  )
  expect_equal(expected, created)
})

test_that("use area constant diversity-independent with hyper_pars", {
  ps_ext_rate <- 2
  default_pars <- DAISIE:::create_default_pars(
    island_ontogeny = 1,
    sea_level = 0,
    totaltime = 5,
    area_pars = DAISIE:::create_area_pars(
      max_area = 1,
      proportional_peak_t = 0,
      peak_sharpness = 0,
      total_island_age = 0,
      sea_level_amplitude = 0,
      sea_level_frequency = 0,
      island_gradient_angle = 0
    ),
    hyper_pars = DAISIE:::create_hyper_pars(
      d = 4,
      x = 3
    )
  )
  expect_silent(ext_rate <- DAISIE:::get_ext_rate(
    mu = ps_ext_rate,
    hyper_pars = default_pars$hyper_pars,
    extcutoff = 1000,
    num_spec = 0,
    K = 10,
    A = DAISIE::island_area(timeval = 0,
                    area_pars = default_pars$area_pars,
                    island_ontogeny = 0,
                    sea_level = 0)
  ))
  expect_true(is.numeric(ext_rate))
})

test_that("use area variable (ontogeny) diversity-independent without
          hyper_pars", {
  ps_ext_rate <- 2
  default_pars <- DAISIE:::create_default_pars(
    island_ontogeny = 1,
    sea_level = 0,
    totaltime = 5,
    area_pars = DAISIE:::create_area_pars(
      max_area = 1000,
      proportional_peak_t = 0.5,
      peak_sharpness = 1,
      total_island_age = 15,
      sea_level_amplitude = 0,
      sea_level_frequency = 0,
      island_gradient_angle = 0
    ),
    hyper_pars = NULL
  )

  expect_silent(ext_rate <- DAISIE:::get_ext_rate(
    mu = 1,
    hyper_pars = default_pars$hyper_pars,
    extcutoff = 1000,
    num_spec = 10,
    K = 20,
    A = DAISIE::island_area(timeval = 5,
                    area_pars = default_pars$area_pars,
                    island_ontogeny = 1,
                    sea_level = 0)
  )
  )
  expect_true(is.numeric(ext_rate))
})

test_that("use area variable (sea-level) diversity-independent without
          hyper_pars", {
  ps_ext_rate <- 2
  default_pars <- DAISIE:::create_default_pars(
    island_ontogeny = 0,
    sea_level = 1,
    totaltime = 5,
    area_pars = DAISIE:::create_area_pars(
      max_area = 10,
      proportional_peak_t = 0,
      peak_sharpness = 0,
      total_island_age = 11,
      sea_level_amplitude = 1,
      sea_level_frequency = 1,
      island_gradient_angle = 45
    ),
    hyper_pars = NULL
  )

  expect_silent(ext_rate <- DAISIE:::get_ext_rate(
    mu = 2,
    hyper_pars = default_pars$hyper_pars,
    extcutoff = 1000,
    num_spec = 10,
    K = 20,
    A = DAISIE::island_area(timeval = 5,
                    area_pars = default_pars$area_pars,
                    island_ontogeny = 0,
                    sea_level = 1)
    )
  )
})


test_that("use area variable (ontogeny and sea-level) diversity-independent
          without hyper_pars", {
  ps_ext_rate <- 2
  default_pars <- DAISIE:::create_default_pars(
    island_ontogeny = 0,
    sea_level = 1,
    totaltime = 5,
    area_pars = DAISIE:::create_area_pars(
      max_area = 1000,
      proportional_peak_t = 0.3,
      peak_sharpness = 1,
      total_island_age = 11,
      sea_level_amplitude = 1,
      sea_level_frequency = 1,
      island_gradient_angle = 45
    ),
    hyper_pars = NULL
  )

  expect_silent(ext_rate <- DAISIE:::get_ext_rate(
    mu = 2,
    hyper_pars = default_pars$hyper_pars,
    extcutoff = 1000,
    num_spec = 10,
    K = 20,
    A = DAISIE::island_area(timeval = 5,
                    area_pars = default_pars$area_pars,
                    island_ontogeny = 1,
                    sea_level = 1)
  )
  )
})

test_that("use area variable (ontogeny and sea-level) diversity-independent
          with hyper_pars", {
  ps_ext_rate <- 2
  default_pars <- DAISIE:::create_default_pars(
    island_ontogeny = 0,
    sea_level = 1,
    totaltime = 5,
    area_pars = DAISIE:::create_area_pars(
      max_area = 1000,
      proportional_peak_t = 0.3,
      peak_sharpness = 1,
      total_island_age = 11,
      sea_level_amplitude = 1,
      sea_level_frequency = 1,
      island_gradient_angle = 45
    ),
    hyper_pars = DAISIE:::create_hyper_pars(
      d = 4,
      x = 3
    )
  )

  expect_silent(ext_rate <- DAISIE:::get_ext_rate(
    mu = 2,
    hyper_pars = default_pars$hyper_pars,
    extcutoff = 1000,
    num_spec = 10,
    K = Inf,
    A = DAISIE::island_area(timeval = 5,
                    area_pars = default_pars$area_pars,
                    island_ontogeny = 1,
                    sea_level = 1)
  )
  )
})
