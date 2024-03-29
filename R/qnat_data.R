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
#'   data_link <- paste0('https://www.dropbox.com/s/d40adhw66uwueet/',
#'                       'VazoesNaturaisONS_D_87UHEsDirceuAssis_2018.dat?dl=1'
#'   )
#'   qnat <- import_qnat(data_link, complete = TRUE, add_stn_info = TRUE)
#'   str(qnat)
#'   # saveRDS(qnat, file = "qnat.RDS")
#' }
import_qnat <- function(
                        file,
                        complete = TRUE,
                        add_stn_info = TRUE) {

    checkmate::assert_character(file)
    checkmate::assert_logical(complete)
    checkmate::assert_logical(add_stn_info)


  if (is.na(file)) {
    file <- wise_select()
  }
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


  # if (!complete & !add_stn_info) {
  #   return(qnat_tidy)
  # }
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

  qnat_tidy
}


#' Pivot data from long to wide format
#'
#' @param qnat a [tibble][tibble::tibble-package] with tidy data. Usually the
#'  output from [import_qnat].
#' @inheritParams tidyr::pivot_wider
#' @return a widen [tibble][tibble::tibble-package] version from
#' input data (`qnat`). Each column will correspond to a time series of a
#' station. Default for variable names will be a string like `qnat_{names_from}`
#' . Most of the time the value of names_from is `code_stn` or `id`.
#'
#' @details This almost a wrapper function of `tidyr::pivot_wider()`.
#' @export
#'
#' @examples
#' if(FALSE){
#'  qnat <- import_qnat(NA_character_, complete = TRUE, add_stn_info = TRUE)
#'  str(qnat)
#'  qnat_wide <- wider(qnat)
#'  str(qnat_wide)
#' }
wider <- function(qnat,
                  names_from = "code_stn",
                  names_prefix = "stn_",
                  values_from = "qnat"
) {

  checkmate::assert(
    checkmate::assert_data_frame(qnat),
    checkmate::assert_character(names_from),
    checkmate::assert_character(names_prefix),
    checkmate::assert_character(values_from)
  )

  checkmate::assert_names(
    names(qnat),
    must.include = c("date", "id", "qnat")
  )
  checkmate::assert_choice(names_from, c("code_stn", "id"))
  checkmate::assert_set_equal(values_from, "qnat")
  if(names_from == "code_stn"){
    checkmate::assert_names(
      names(qnat),
      must.include = c("date", "qnat", "code_stn")
    )
  }


  qnat %>%
    dplyr::select(tidyselect::all_of(c("date", names_from, values_from))) %>%
    tidyr::pivot_wider(.,
                names_from = tidyselect::all_of(names_from),
                names_prefix,
                values_from
    )
}

#' Extract natural stramflow data from a ONS station and save to RDS file
#'
#' @param qnat_file Character. Path to ascii data file (or a URL).
#' @param stn_id integer, station code from ONS station (output from
#' `info_station()[["posto"]]`).
#' @param save logical, TRUE to export data to RDS file.
#' @param prefix prefix to RDS file
#' @param dest_dir a character with the name of where the RDS file is
#' saved. Default: `fusepoc-prep/output`.
#' @return a tibble with columns `date`, `posto`, `qnat` (cmecs).
#' @export
#'
#' @examples
#' if(FALSE){
#'  qnat_posto <- extract_qnat(
#'                  qnat_file = NA,
#'                  stn_id = 74,
#'                  save = FALSE
#'                  )
#'  str(qnat_posto)
#' }
#' @seealso import_qnat
extract_qnat <- function(qnat_file = NA,
                         stn_id = 74,
                         save = TRUE,
                         prefix = "qnat-obs-posto-",
                         dest_dir = "output") {
  checkmate::assert_true(requireNamespace("HEobs", quietly = TRUE))

  # quando file NA usa arquivo local
  qnat <- import_qnat(
    file = qnat_file,
    complete = TRUE,
    add_stn_info = TRUE
  )

  qnat_posto <- qnat %>%
    dplyr::filter(code_stn == stn_id) %>%
    dplyr::select(date, posto = code_stn, qnat)

  # período de dados válidos
  se_date <- qnat_posto %>%
    dplyr::filter(!is.na(qnat)) %>%
    dplyr::summarise(
      start = min(date),
      end = max(date)
    )

  # filtra para período de dados válidos
  qnat_posto <- qnat_posto %>%
    dplyr::filter(date >= se_date[[1]]
                  &
                    date <= se_date[[2]])

  if(save){
    save_data(
      data_posto = qnat_posto,
      .prefix = prefix,
      .posto_id = stn_id,
      .dest_dir = dest_dir
    )
  }
  qnat_posto
}

