#---------------------------------------------------------------
.replace_bigneg <- function(x) replace(x, which(x < -9999), NA)

#---------------------------------------------------------------
#' Drops variables in DT which have the same values for all observations.
#' @param DT `data.frame`, `tibble` or `data.table`.
#' @return \code{\link[tibble]{as_tibble}} of original data excluding
#' non-informative variables.
#' @details It is used in `import_metadata`.
#' @seealso
#'  \code{\link[data.table]{as.data.table}}, \code{\link[tibble]{as_tibble}},
#' "\code{\link[import_metadata]{HEobs}}"
#' @importFrom checkmate assert_data_frame
#' @importFrom data.table as.data.table uniqueN
#' @importFrom tibble as_tibble
.informative <- function(DT) {
  #DT = m
  checkmate::assert_data_frame(DT)
  #assertive::is_data.frame(DT)
  DT <- data.table::as.data.table(DT)
  sel <- as.vector(
    (unlist(DT[, lapply(data.table::.SD, data.table::uniqueN)]))
    ) > 1
  cols <- names(DT)[sel]
  tibble::as_tibble(
    #DT[, ..cols]
    DT[, cols, with = FALSE]
  )
}

#---------------------------------------------------------------
#' Add code and station name from stations metadata

#' Add code and station name from stations metadata
#' @param x tibble
#' @param txt_file path to ASCII file
#' @return original data with extra columns `code_stn` and `name_stn`.
#' @details It is used in `import_qnat`
#' @seealso
#'  "\code{\link[import_metadata]{HEobs}}"
#' @importFrom dplyr mutate
.add_cod_name <- function(x, txt_file) {
  md <- import_metadata(txt_file)
  x <- x %>%
    dplyr::mutate(.,
           code_stn = md$estacao_codigo[x$id],
           name_stn = md$nome_estacao[x$id]
    )
  x
}


