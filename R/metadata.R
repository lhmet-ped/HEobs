
extract_metadata <- function(txt_file, informative = FALSE) {
  # txt_file = "../inst/extdata/VazoesNaturaisONS_D_87UHEsDirceuAssis_2018.dat"
  # txt_file =  "https://www.dropbox.com/s/d40adhw66uwueet/VazoesNaturaisONS_D_87UHEsDirceuAssis_2018.dat?dl=1"
  meta_data <- data.table::fread(
    input = as.character(txt_file),
    sep = ";",
    nrows = 15,
    fill = TRUE,
    na.strings = c("null", "-99999.0")
  )

  cols <- paste0("V", c(1:2, seq(4, ncol(meta_data), by = 2)))
  cols_remove <- names(meta_data)[!names(meta_data) %in% cols]
  #meta_data <- meta_data[, cols, with = FALSE]
  meta_data[, (cols_remove) := NULL]

  DT <- as.data.table(t(meta_data))

  # address(DT)
  setnames(DT,
           old = names(DT),
           new = unlist(DT[1], use.names = FALSE)
           )
  # using function delete while delete rows by reference in data.table
  # is not available
  DT <- delete(DT, del.idxs = 1)

  #------------------------------------------
  #! PAREI AQUI NA CONVERSAO PARA data.table
  #------------------------------------------

  #DT[, lapply(.SD, readr::parse_guess)]

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
