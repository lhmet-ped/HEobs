
import_metadata <- function(txt_file, informative = FALSE) {
  # txt_file = "../inst/extdata/VazoesNaturaisONS_D_87UHEsDirceuAssis_2018.dat"
  meta_data <- rio::import(
    file = as.character(txt_file),
    format = "csv",
    fread = TRUE,
    sep = ";",
    nrows = 15,
    fill = TRUE,
    na.strings = c("null", "-99999.0")
  )

  cols <- paste0("V", c(1:2, seq(4, ncol(meta_data), by = 2)))
  meta_data[, V1:V2]

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

  # fix variable types and replace -99999 by NA
  meta_data <- meta_data %>%
    dplyr::mutate(
      .,
      dplyr::across(dplyr::everything(), parse_guess),
      dplyr::across(tidyselect:::where(is.numeric), .replace_bigneg)
    ) %>%
    janitor::clean_names(.) # %T>% glimpse()

  # keep informative columns
  if(informative){
    meta_data <- .informative(meta_data)
  }
  meta_data
}
