#' Import data file of daily naturalized streamflow
#'
#' @inheritParams extract_metadata
#' @param complete logical, Default: TRUE. Make missing dates explicit using
#'  `complete_dates()`.
#' @param add_stn_info logical, Default: TRUE. Get code and station name
#' from stations metadata.
#' @details The source ascii file contains data and metadata from ONS Hydroelectric
#' Plants. Metadata is extracted with [extract_metadata] while the data is
#' extract here.
#' @return a [tibble][tibble::tibble-package] with tidy data.
#' @export
#'
#' @examples
#' if (FALSE) {
#'   qnat <- import_qnat(NA_character_, complete = TRUE, add_stn = TRUE)
#'   str(qnat)
#' }
import_qnat <- function(
                        file,
                        complete = TRUE,
                        add_stn_info = TRUE) {
  checkmate::assert(
    checkmate::assert_character(file),
    checkmate::assert_logical(complete),
    checkmate::assert_logical(add_stn_info)
  )

  if (is.na(file)) file <- find_data()
  # find row were data start
  srow <- readr::read_lines(file, n_max = 30) %>%
    grep("^Data;Valor", .)

  # read
  qnat_raw <- rio::import(
    file = as.character(file),
    format = "csv",
    fread = TRUE,
    sep = ";",
    skip = srow + 1,
    head = FALSE,
    # fill = TRUE,
    na.strings = "null"
    # check.names = TRUE
  ) %>%
    # start cleanup
    tibble::as_tibble() %>%
    # remove last column due to a extra sep
    dplyr::select(-ncol(.)) %>%
    stats::setNames(
      paste0(
        c("data", "valor"),
        "_",
        # as data are paired (date, value)
        rep(1:(ncol(.) %/% 2), each = 2)
      )
    )

  # tidy data
  qnat_tidy <- qnat_raw %>%
    tidyr::pivot_longer(
      tidyselect::everything(),
      names_to = c(".value", "id"),
      names_sep = "_",
      values_drop_na = TRUE
    ) %>%
    dplyr::transmute(
      id = as.integer(id),
      date = lubridate::dmy(data),
      qnat = as.numeric(valor),
      qnat = data.table::fifelse(qnat < 0, NA_real_, qnat)
    ) %>%
    dplyr::arrange(id)


  if (!complete & !add_stn_info) {
    return(qnat_tidy)
  }
  # data with complete dates and constant time step
  if (complete) {
    qnat_tidy <- lhmetools::complete_dates(
      x = qnat_tidy,
      group = "id",
      time_step = "days"
    )
    if (!add_stn_info) {
      return(qnat_tidy)
    }
  }
  # want station's codes and names
  if (add_stn_info) {
    qnat_tidy <- .add_cod_name(qnat_tidy, file)
    return(qnat_tidy)
  }

  qnat
}
