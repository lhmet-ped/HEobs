utils::globalVariables(c(
  ".", "V1", "V2", "id", "where"
))


#---------------------------------------------------------------
.replace_bigneg <- function(x) replace(x, which(x < -999), NA)

#---------------------------------------------------------------
#' Drops variables in DT which have the same values for all observations.
#' @param DT `data.frame`, `tibble` or `data.table`.
#' @return \code{\link[tibble]{as_tibble}} of original data excluding
#' non-informative variables.
#' @details It is used in `extract_metadata`.
#' @seealso
#'  \code{\link[data.table]{as.data.table}}, \code{\link[tibble]{as_tibble}}
#' @importFrom checkmate assert_data_frame
#' @importFrom data.table as.data.table uniqueN .SD
#' @importFrom tibble as_tibble
.informative <- function(DT) {
  #DT = meta_data
  checkmate::assert_data_frame(DT)
  #assertive::is_data.frame(DT)
  DT <- data.table::as.data.table(DT)
  sel <- as.vector(
    (unlist(DT[, lapply(.SD, data.table::uniqueN)]))
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
#' @param DT tibble
#' @param txt_file path to ASCII file
#' @return original data with extra columns `code_stn` and `name_stn`.
#' @details It is used in `import_qnat`
#' @importFrom data.table as.data.table
#' @importFrom tibble as_tibble
#'
.add_cod_name <- function(DT, txt_file) {
  DT <- data.table::as.data.table(DT)
  md <- extract_metadata(txt_file)
  DT[, c("code_stn", "name_stn") :=
       .(md$estacao_codigo[ DT[[id]] ],  md$nome_estacao[ DT[[id]] ])
     ]
  tibble::as_tibble(DT)
}
