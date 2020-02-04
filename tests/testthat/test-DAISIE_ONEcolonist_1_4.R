context("DAISIE_ONEcolonist_1_4")

test_that("One immigrant works", {
  sim_time <- 10
  island_spec <- matrix(nrow = 1, ncol = 7, data = "x")
  island_spec[, 1] <- c("1")
  island_spec[, 2] <- c("1")
  island_spec[, 3] <- c("6.92580955162582")
  island_spec[, 4] <- c("I")
  island_spec[, 5] <- NA
  island_spec[, 6] <- NA
  island_spec[, 7] <- ""
  colnames(island_spec) <- c(
    "Species",
    "Mainland Ancestor",
    "Colonisation time (BP)",
    "Species type",
    "branch_code",
    "branching time (BP)",
    "Anagenetic_origin"
  )

  stt_table <- data.frame(
    Time = c(
      10,
      9.8016632,
      9.786974,
      9.6022772,
      9.4117448,
      8.8270858,
      7.9951318,
      7.8522784,
      6.9258096,
      6.6612814,
      4.996701,
      4.7943237,
      4.0461558,
      3.8633278,
      0.6456057,
      0.2628436,
      0
    ),
    nI = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1),
    nA = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    nC = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  )


  expect_silent(result <- DAISIE_ONEcolonist_1_4(
    time = sim_time,
    island_spec = island_spec,
    stt_table = stt_table
  )
  )
})
test_that("One anagenetic works", {
  sim_time <- 10
  island_spec <- matrix(nrow = 1, ncol = 7, data = "x")
  island_spec[, 1] <- c("1")
  island_spec[, 2] <- c("1")
  island_spec[, 3] <- c("6.92580955162582")
  island_spec[, 4] <- c("A")
  island_spec[, 5] <- NA
  island_spec[, 6] <- NA
  island_spec[, 7] <- "Immig_parent"
  colnames(island_spec) <- c(
    "Species",
    "Mainland Ancestor",
    "Colonisation time (BP)",
    "Species type",
    "branch_code",
    "branching time (BP)",
    "Anagenetic_origin"
  )

  stt_table <- data.frame(
    Time = c(
      10,
      9.8016632,
      9.786974,
      9.6022772,
      9.4117448,
      8.8270858,
      7.9951318,
      7.8522784,
      6.9258096,
      6.6612814,
      4.996701,
      4.7943237,
      4.0461558,
      3.8633278,
      0.6456057,
      0.2628436,
      0
    ),
    nI = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
    nA = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1),
    nC = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  )


  expect_silent(result <- DAISIE_ONEcolonist_1_4(
    time = sim_time,
    island_spec = island_spec,
    stt_table = stt_table
  )
  )
})
test_that("One cladogenetic works", {
  sim_time <- 10
  island_spec <- matrix(nrow = 1, ncol = 7, data = "x")
  island_spec[, 1] <- c("1")
  island_spec[, 2] <- c("1")
  island_spec[, 3] <- c("6.92580955162582")
  island_spec[, 4] <- c("C")
  island_spec[, 5] <- NA
  island_spec[, 6] <- NA
  island_spec[, 7] <- ""
  colnames(island_spec) <- c(
    "Species",
    "Mainland Ancestor",
    "Colonisation time (BP)",
    "Species type",
    "branch_code",
    "branching time (BP)",
    "Anagenetic_origin"
  )

  stt_table <- data.frame(
    Time = c(
      10,
      9.8016632,
      9.786974,
      9.6022772,
      9.4117448,
      8.8270858,
      7.9951318,
      7.8522784,
      6.9258096,
      6.6612814,
      4.996701,
      4.7943237,
      4.0461558,
      3.8633278,
      0.6456057,
      0.2628436,
      0
    ),
    nI = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
    nA = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    nC = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 1, 1, 1, 1, 1, 1)
  )


  expect_silent(result <- DAISIE_ONEcolonist_1_4(
    time = sim_time,
    island_spec = island_spec,
    stt_table = stt_table
  )
  )
})


test_that("DAISIE_ONEcolonist_1_4 works with >=2 cladogenetic with same ancestor", {
  set.seed(42)
  sim_time <- 10
  n_mainland_species <- 1
  clado_rate <- 1
  ext_rate <- 0.00001
  carr_cap <- 4
  imm_rate <- 1
  ana_rate <- 0.000001
  expect_silent(out <- DAISIE_sim_core_1_4(
    time = sim_time,
    mainland_n = n_mainland_species,
    pars = c(clado_rate, ext_rate, carr_cap, imm_rate, ana_rate)
  )
  )
})


test_that("DAISIE_ONEcolonist_1_4 works with >=2 anagenetic with same ancestor", {
  set.seed(42)
  sim_time <- 10
  n_mainland_species <- 1
  clado_rate <- 0.0000001
  ext_rate <- 0.00001
  carr_cap <- 4
  imm_rate <- 1
  ana_rate <- 2
  expect_silent(out <- DAISIE_sim_core_1_4(
    time = sim_time,
    mainland_n = n_mainland_species,
    pars = c(clado_rate, ext_rate, carr_cap, imm_rate, ana_rate)
  )
  )
})
test_that("DAISIE_ONEcolonist_1_4 works with >=2 nonendemic with same ancestor", {
  set.seed(44)
  sim_time <- 10
  n_mainland_species <- 1
  clado_rate <- 0.0000001
  ext_rate <- 0.00001
  carr_cap <- 4
  imm_rate <- 3
  ana_rate <- 1
  expect_silent(out <- DAISIE_sim_core_1_4(
    time = sim_time,
    mainland_n = n_mainland_species,
    pars = c(clado_rate, ext_rate, carr_cap, imm_rate, ana_rate)
  )
  )
})


