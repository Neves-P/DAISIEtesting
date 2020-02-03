#' Calculate the clade-wide extinction rate
#'
#' @param ps_ext_rate per species extinction rate
#' @param n_species number of species in that clade
#'
#' @return the clade's extinction rate
#' @author Richel J.C. Bilderbeek
#' @examples
#'   testit::assert(
#'     DAISIE_calc_clade_ext_rate(
#'       ps_ext_rate = 0.2,
#'       n_species = 4
#'     ) == 0.8
#'   )
#' @export
DAISIE_calc_clade_ext_rate <- function(ps_ext_rate, n_species) {
  testit::assert(ps_ext_rate >= 0.0)
  testit::assert(n_species >= 0)
  ps_ext_rate * n_species
}

#' Calculate the clade-wide effective anagenesis rate.
#' With 'effective', this means that if an immigrant
#' undergoes anagenesis, it will become a new species.
#' Would such a species undergo anagenesis again, no net new
#' species is created; the species only gets renamed
#'
#' @param ps_ana_rate per species anagensis rate
#' @param n_immigrants number of immigrants in that clade
#'
#' @return the clade's effective anagenesis rate
#' @author Richel J.C. Bilderbeek
#' @examples
#'   testit::assert(
#'     DAISIE_calc_clade_ana_rate(
#'       ps_ana_rate = 0.3,
#'       n_immigrants = 5
#'     ) == 1.5
#'   )
#' @export
DAISIE_calc_clade_ana_rate <- function(ps_ana_rate, n_immigrants) {
  testit::assert(ps_ana_rate >= 0.0)
  testit::assert(n_immigrants >= 0)
  ps_ana_rate * n_immigrants
}

#' Calculate the clade-wide cladogenesis rate.
#'
#' @param ps_clado_rate per species cladogenesis rate
#' @param n_species number of species in that clade
#' @param carr_cap carrying capacity, number of species this clade will
#'   grow to
#' @return the clade's cladogenesis rate, which is at least zero. This
#'   rate will be zero if there are more species than the carrying capacity
#'   allows for
#' @note For clade-specific carrying capacity,
#'   each clade is simulated seperately in \code{\link{DAISIE_sim}}
#' @author Richel J.C. Bilderbeek
#' @examples
#'   testit::assert(
#'     DAISIE_calc_clade_clado_rate(
#'       ps_clado_rate = 0.2,
#'       n_species = 5,
#'       carr_cap = 10
#'     ) == 0.5
#'   )
#'   testit::assert(
#'     DAISIE_calc_clade_clado_rate(
#'       ps_clado_rate = 0.2,
#'       n_species = 2,
#'       carr_cap = 1
#'     ) == 0.0
#'   )
#' @export
DAISIE_calc_clade_clado_rate <- function(ps_clado_rate, n_species, carr_cap) {
  testit::assert(ps_clado_rate >= 0.0)
  testit::assert(n_species >= 0)
  testit::assert(carr_cap >= 0)
  return(max(
    0.0,
    n_species * ps_clado_rate * (1.0 - (n_species / carr_cap))
  ))
}

#' Calculate the clade-wide immigration rate.
#'
#' @param ps_imm_rate per species immigration rate
#' @param n_island_species number of species in that clade on the island
#' @param n_mainland_species number of species in that clade on the mainland
#' @param carr_cap carrying capacity, number of species this clade will
#'   grow to
#' @return the clade's immigration rate, which is at least zero. This
#'   rate will be zero if there are more species than the carrying capacity
#'   allows for
#' @author Richel J.C. Bilderbeek
#' @examples
#'   testit::assert(
#'     DAISIE_calc_clade_imm_rate(
#'       ps_imm_rate = 0.1,
#'       n_island_species = 5,
#'       n_mainland_species = 2,
#'       carr_cap = 10
#'     ) == 0.1
#'   )
#'   testit::assert(
#'     DAISIE_calc_clade_imm_rate(
#'       ps_imm_rate = 0.1,
#'       n_island_species = 5,
#'       n_mainland_species = 2,
#'       carr_cap = 1
#'     ) == 0.0
#'   )
#' @export
DAISIE_calc_clade_imm_rate <- function(
  ps_imm_rate,
  n_island_species,
  n_mainland_species,
  carr_cap
) {
  testit::assert(ps_imm_rate >= 0.0)
  testit::assert(n_island_species >= 0)
  testit::assert(n_mainland_species >= 0)
  testit::assert(carr_cap >= 0)
  return(max(
    0.0,
    n_mainland_species * ps_imm_rate * (1.0 - (n_island_species / carr_cap))
  ))
}
