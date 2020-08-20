#' Import metadata from streamflow stations of ONS
#' @param file Character. Path to ascii data file (or a URL).
#' @param informative Logical. Drop non-informative variables. Default: FALSE
#' @return a [tibble][tibble::tibble-package] with tidy data
#' @details The text file contains data and metadata from ONS Hydroelectric
#' Plants. Metadata is extracted from the first 15 lines of the ascii file.
#' The information for each HPP is stored in pairs of columns. The first column
#'  is the name of the variables and the second the values of the variables.
#'  The data is processed in tidy format.
#' @examples
#' if(TRUE){
#'   qnat_meta <- extract_metadata(find_data(), informative = TRUE)
#'   str(qnat_meta)
#' }
#' @source The metadata 87 hydroelectric power plants operated by ONS were
#' supplied by Saul Aires (ANA skilled in Water Resources) by Prof. Carlos
#' Lima (UnB), by email on 2020-03-17.
#' @rdname extract_metadata
#' @export
#' @importFrom rio import
#' @importFrom dplyr select num_range slice mutate across everything
#' @importFrom tibble as_tibble
#' @importFrom janitor clean_names
#' @importFrom readr parse_guess
#' @importFrom tidyselect vars_select_helpers
extract_metadata <- function(file, informative = FALSE) {
  meta_data <- rio::import(
    file = as.character(file),
    format = "csv",
    fread = TRUE,
    sep = ";",
    nrows = 15,
    fill = TRUE,
    na.strings = c("null", "-99999.0"),
    encoding = "Latin-1"
  )
  # remove repeated columns
  meta_data <- meta_data %>%
    dplyr::select(
      .,
      V1:V2,
      dplyr::num_range(
        prefix = "V",
        range = seq(4, ncol(.), by = 2)
      )
    ) %>%
    # transpose data and fix names
    t() %>%
    tibble::as_tibble()

  # janitor::make_clean_names(nms)
  nms <- unlist(dplyr::slice(meta_data, 1), use.names = FALSE)
  meta_data <- stats::setNames(meta_data, nms) %>%
    # stats::setNames(., unlist(dplyr::slice(., 1), use.names = FALSE)) %>%
    dplyr::slice(., -1)

  # fix variable types and replace numeric vars < -999 by NA
  meta_data <- meta_data %>%
    dplyr::mutate(
      .,
      dplyr::across(dplyr::everything(), readr::parse_guess),
      dplyr::across(
        tidyselect::vars_select_helpers$where(is.numeric),
        # where(is.numeric),
        .replace_bigneg
      )
    ) %>%
    janitor::clean_names(.) # %T>% glimpse()

  # keep informative columns
  if (informative) {
    meta_data <- .informative(meta_data)
  }
  meta_data
}


