
context("import_qnat()")

#library(data.table)
library(lubridate)

test_that("Produces the correct output.", {
  #qnat <- data.table(import_qnat(NA_character_))
  #2796267
  qnat <- data.table(
    import_qnat(
    file = ifelse(.check_user(),
                  find_data(TRUE),
                  find_data(FALSE)
    ),
    complete = TRUE,
    add_stn_info = TRUE
    )
  )

  # expected dimension
  exp_dim <- c(2796267, 5)
  # expected start and end dates
  exp_se_dts <- c("1931-01-02", "2018-12-31")

  expect_equal(dim(qnat), exp_dim)
  expect_equal(as.character(c(qnat[["date"]][1], qnat[["date"]][nrow(qnat)])),
               exp_se_dts)


  # check if num. of rows matches num. of expected dates
  n_days_actual <- qnat[, .N, by = .(date)][, sum(N)]

  int <- interval(ymd(exp_se_dts[1]), ymd(exp_se_dts[2]))
  n_sites <- length(qnat[, unique(code_stn)])
  n_days_expected <- (time_length(int, "day") + 1) * n_sites
  expect_equal(n_days_actual, n_days_expected)


  # check time step
  calc_time_step <- function(x) {
    time_step <- unique(c(diff(x)))
    time_step
  }
  daily_step <- qnat[,
                     .(time_step = calc_time_step(date)), by = .(id)
                     ][, unique(time_step)]
  expect_equal(daily_step, 1)
})

test_that("Produces the correct errors.", {
  expect_error(import_qnat(file = ""))
})
