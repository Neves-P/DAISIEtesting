context("DAISIE_ML2")
test_that("use", {
  if (Sys.getenv("TRAVIS") != "") {

    # This is a rough MLE test, built for fast execution. A more thorough test
    # can be found in the GitHub repository Neves-P/DAISIEtesting

    utils::data(Macaronesia_datalist, package = "DAISIE")
    tested_MLE <- DAISIE:::DAISIE_ML2(
      datalist = Macaronesia_datalist,
      initparsopt = c(
        1.053151832,
        0.052148979,
        0.512939011,
        0.133766934,
        0.152763179
      ),
      idparsmat = rbind(
        1:5,
        c(6, 2, 3, 7, 5),
        1:5,1:5
      ),
      idparsopt = c(2, 4, 5, 6, 7),
      parsfix = c(0, Inf),
      idparsfix = c(1, 3),
    )

    expected_MLE <- data.frame(
      lambda_c = c(0.0,
                   0.133766934,
                   0.0,
                   0.0),
      mu = c(1.0531518319999997,
             1.0531518319999997,
             1.0531518319999997,
             1.0531518319999997),
      K = c(Inf,
            Inf,
            Inf,
            Inf),
      gamma = c(0.052148978999999998,
                0.152763178999999999,
                0.052148978999999998,
                0.052148978999999998),
      lambda_a = c(0.51293901099999994,
                   0.51293901099999994,
                   0.51293901099999994,
                   0.51293901099999994),
      loglik = c(-454.93478332906614,
                 -454.93478332906614,
                 -454.93478332906614,
                 -454.93478332906614),
      df = c(5L, 5L, 5L, 5L),
      conv = c(0L, 0L, 0L, 0L)
    )
    expect_equal(tested_MLE, expected_MLE)
  } else {
    testthat::skip("Run only on Travis or AppVeyor")
  }
})

test_that("abuse", {
  if (Sys.getenv("TRAVIS") != "") {
    expect_error(tested_MLE <- DAISIE:::DAISIE_ML2(
      datalist = "nonsense",
      initparsopt = c(
        1.053151832,
        0.052148979,
        0.512939011,
        0.133766934,
        0.152763179
      ),
      idparsmat = rbind(
        1:5,
        c(6, 2, 3, 7, 5),
        1:5,1:5
      ),
      idparsopt = c(2, 4, 5, 6, 7),
      parsfix = c(0, Inf),
      idparsfix = c(1, 3),
      tol = c(0.01, 0.1, 0.001),
      res = 15,
      tolint = c(0.1, 0.01)
    ))
  } else {
    testthat::skip("Run only on Travis or AppVeyor")
  }
})
