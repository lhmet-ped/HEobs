import_qnat <- function(
  txt_file = "../inst/extdata/VazoesNaturaisONS_D_87UHEsDirceuAssis_2018.dat",
  complete = TRUE,
  add_stn_info = TRUE
) {
  # find row were data start
  srow <- readr::read_lines(txt_file, n_max = 30) %>%
    grep("^Data;Valor", .)

  qnat_raw <- rio::import(
    file = as.character(txt_file),
    format = "csv",
    fread = TRUE,
    sep = ";",
    skip = srow + 1,
    head = FALSE,
    # fill = TRUE,
    na.strings = "null"
    # check.names = TRUE
  ) %>%
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
  # data with complete dates and constant tome step
  if (complete) {
    qnat_tidy <- complete_dates(
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
    qnat_tidy <- .add_cod_name(x = qnat_tidy, txt_file)
    return(qnat_tidy)
  }

  qnat
}
