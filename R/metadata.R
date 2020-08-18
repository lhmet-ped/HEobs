#' Import metadata from streamflow stations of ONS
#' @param txt_file Character. File path (or link to data file).
#' @param informative Logical. Drop non-informative variables. Default: FALSE
#' @return a [tibble][tibble::tibble-package]
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'   file <- "https://www.dropbox.com/s/d40adhw66uwueet/VazoesNaturaisONS_D_87UHEsDirceuAssis_2018.dat?dl=1"
#'   qnat_meta <- import_metadata(file, informative = TRUE)
#'   str(qnat_meta)
#'  }
#' }
#' @seealso
#'  \code{\link[rio]{import}}
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{reexports}},\code{\link[dplyr]{slice}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{across}}
#'  \code{\link[tibble]{as_tibble}}
#'  \code{\link[tidyselect]{vars_select_helpers}}
#'  \code{\link[janitor]{clean_names}}
#' @rdname extract_metadata
#' @export
#' @importFrom rio import
#' @importFrom dplyr select num_range slice mutate across everything
#' @importFrom tibble as_tibble
#' @importFrom tidyselect vars_select_helpers
#' @importFrom janitor clean_names
extract_metadata <- function(txt_file = "", informative = FALSE) {
  # txt_file = "../inst/extdata/VazoesNaturaisONS_D_87UHEsDirceuAssis_2018.dat"
  # txt_file =  "https://www.dropbox.com/s/d40adhw66uwueet/VazoesNaturaisONS_D_87UHEsDirceuAssis_2018.dat?dl=1"

  # meta_data <- data.table::fread(
  #   input = as.character(txt_file),
  #   sep = ";",
  #   nrows = 15,
  #   fill = TRUE,
  #   na.strings = c("null", "-99999.0")
  # )

  meta_data <- rio::import(
    file = as.character(txt_file),
    format = 'csv',
    fread = TRUE,
    sep = ';',
    nrows = 15,
    fill = TRUE,
    na.strings = c('null', '-99999.0')
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
    tibble::as_tibble() %>%
    setNames(., slice(., 1)) %>%
    dplyr::slice(., -1)

  # fix variable types and replace numeric vars < -999 by NA
  meta_data <- meta_data %>%
    dplyr::mutate(
      .,
      dplyr::across(dplyr::everything(), parse_guess),
      dplyr::across(
        tidyselect::vars_select_helpers$where(is.numeric),
        .replace_bigneg
        )
    ) %>%
    janitor::clean_names(.) # %T>% glimpse()

  # keep informative columns
  if(informative){
    meta_data <- .informative(meta_data)
  }
  meta_data
}
