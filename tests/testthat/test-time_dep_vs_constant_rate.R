context("DAISIE_sim_time_dependent first line")

test_that("constant rate output matches time dependent code", {

  # Note: Since both algorithms do not call the RNG an equal number of times,
  # the output of Gillespie runs must necessarily be different. To at least
  # test some of the output, we verify if the first events match, as the first
  # event of each algorithm is sampled from the same number of RNG calls.



# Constant rate code ------------------------------------------------------
  sim_time <- 10
  n_mainland_species <- 1
  clado_rate <- 1.0
  ext_rate <- 0.5
  carr_cap <- 10
  imm_rate <- 1.0
  ana_rate <- 1.0
  pars <- c(clado_rate, ext_rate, carr_cap, imm_rate, ana_rate)
  rng_seed <- 42
  set.seed(rng_seed)
  constant_rate_out <- DAISIE:::DAISIE_sim_constant_rate(
    time = sim_time,
    M = n_mainland_species,
    pars = pars,
    replicates = 1,
    plot_sims = FALSE,
    verbose = FALSE,
    sample_freq = Inf
  )


  #   Ontogeny code running constant case -----------------------------------
  area_pars <- DAISIE::create_area_pars(
    max_area = 1,
    proportional_peak_t = 0,
    peak_sharpness = 0,
    total_island_age = sim_time,
    sea_level_amplitude = 0,
    sea_level_frequency = 0,
    island_gradient_angle = 0
  )
  set.seed(rng_seed)
  time_dependent_out <- DAISIE::DAISIE_sim_time_dependent(
    time = sim_time,
    M = n_mainland_species,
    pars = c(clado_rate, ext_rate, carr_cap, imm_rate, ana_rate),
    replicates = 1,
    island_ontogeny = "beta",
    sea_level = "const",
    area_pars = area_pars,
    ext_pars = c(ext_rate, 0),
    plot_sims = FALSE,
    verbose = FALSE,
    sample_freq = Inf
  )

  expect_equal(
    time_dependent_out[[1]][[1]]$stt_all[2, ],
    constant_rate_out[[1]][[1]]$stt_all[2, ]
  )

  # Following lines will necessarily be different, see note.
  expect_true(
    !all(time_dependent_out[[1]][[1]]$stt_all[3, ] !=
      constant_rate_out[[1]][[1]]$stt_all[3, ])
    )
})
