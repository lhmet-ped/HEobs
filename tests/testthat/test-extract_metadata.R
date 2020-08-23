context("extract_data()")

# library(data.table)
library(lubridate)
library(checkmate)


test_that("Produces the correct dimensions of output.", {
  # qnat <- data.table(import_qnat(NA_character_))
  # 2796267
  qnat_meta <- extract_metadata(
    file = wise_select(),
    # file = find_data(FALSE),
    informative = TRUE
  )

  # expected dimension
  exp_dim <- c(87, 5)
  expect_equal(dim(qnat_meta), exp_dim)


  qnat_meta <- extract_metadata(
    file = wise_select(),
    # file = find_data(FALSE),
    informative = FALSE
  )
  exp_dim <- c(87, 15)
  expect_equal(dim(qnat_meta), exp_dim)


  # expected variables
  exp_nms <- c(
    "estacao_codigo", "latitude", "longitude",
    "nome_estacao", "municipio"
  )

  checkmate::expect_names(
    x = names(qnat_meta),
    must.include = exp_nms
  )
})

test_that("Produces the correct output type.", {
  qnat_meta <- extract_metadata(
    file = wise_select(),
    # file = find_data(FALSE),
    informative = TRUE
  )
  expect_is(qnat_meta, "tbl_df")
})

test_that("Produces the correct errors.", {
  expect_error(extract_metadata(file = ""))

  expect_error(
    extract_metadata(
      file = wise_select(),
      informative = NULL
    )
  )

  expect_error(
    extract_metadata(file = "https://ndownloader.figshare.com/files/13366451")
  )
})

test_that("Produces the correct output type.", {
  expect_data_frame(extract_metadata(file = NA))
})

test_that("Produces the correct output type with no local data.", {
  qnat_meta <- extract_metadata(
    file = find_data(local = FALSE),
    # file = find_data(FALSE),
    informative = TRUE
  )
  expect_is(qnat_meta, "tbl_df")
})
