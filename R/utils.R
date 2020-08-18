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
.add_cod_name <- function(DT, txt_file) {
  md <- import_metadata(txt_file)
  DT[, c("code_stn", "name_stn") :=
       .(md$estacao_codigo[ DT[[id]] ],  md$nome_estacao[ DT[[id]] ])
     ]
  DT
}

# while Delete rows by reference in data.table is not possible
# issue https://github.com/Rdatatable/data.table/issues/635
# work around ...
# https://stackoverflow.com/questions/10790204/how-to-delete-a-row-by-reference-in-data-table/10791729#10791729
.delete <- function(DT, del.idxs) {           # pls note 'del.idxs' vs. 'keep.idxs'
  keep.idxs <- setdiff(DT[, .I], del.idxs);  # select row indexes to keep
  cols = names(DT);
  DT.subset <- data.table(DT[[1]][keep.idxs]); # this is the subsetted table
  setnames(DT.subset, cols[1]);
  for (col in cols[2:length(cols)]) {
    DT.subset[, (col) := DT[[col]][keep.idxs]];
    DT[, (col) := NULL];  # delete
  }
  return(DT.subset);
}
