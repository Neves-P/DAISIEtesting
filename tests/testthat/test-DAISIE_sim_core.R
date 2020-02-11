context("DAISIE_sim_core_constant_rate")

test_that("new and v1.4 should give same results", {
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
  new <- DAISIE:::DAISIE_sim_core_constant_rate(
    time = sim_time,
    mainland_n = n_mainland_species,
    pars = pars
  )
  set.seed(rng_seed)
  old <- DAISIE_sim_core_1_4(
    time = sim_time,
    mainland_n = n_mainland_species,
    pars = pars
  )
  #new has init_nonend_spec and init_end_spec in names(new[6:7])
  expect_true(all(names(new[1:5]) == names(old)))
  # stt_table has different content
  expect_true(nrow(new$stt_table) == nrow(old$stt_table))
  # different branching times
  expect_equal(length(new$branching_times), length(old$branching_times))
  expect_true(new$stac == old$stac)
  expect_true(new$missing_species == old$missing_species)
  expect_true(length(new$other_clades_same_ancestor) ==
                length(old$other_clades_same_ancestor))
  expect_true(new$other_clades_same_ancestor[[1]]$species_type ==
                old$other_clades_same_ancestor[[1]]$species_type)

  expect_true(all(new$stt_table == old$stt_table))
  expect_true(all(new$branching_times == old$branching_times))
  expect_true(new$other_clades_same_ancestor[[1]]$brts_miss ==
                old$other_clades_same_ancestor[[1]]$brts_miss)
})

