context("wider()")

test_that("Produces the correct output", {

  qnat <- import_qnat(wise_select(), TRUE, TRUE)
  qnat_w <- wider(qnat)

  qnat <- import_qnat(wise_select(), TRUE, FALSE)
  qnat_w <- wider(qnat)

  })
