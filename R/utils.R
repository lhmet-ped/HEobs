utils::globalVariables(c(
  ".", "V1", "V2", "id", "data", "valor", "qnat", "code_stn"
))


#-----------------------------------------------------------------------------
#' Save data from a ONS station in a RDS file
#' @noRd
save_data <- function(data_posto,
                      .prefix = "qnat-obs-posto-",
                      .posto_id,
                      .dest_dir = "output"){

  data_posto_file <- paste0(.prefix, .posto_id, ".RDS")
  data_posto_file <- file.path(.dest_dir, data_posto_file)

  saveRDS(data_posto, file = data_posto_file)
  checkmate::assert_file_exists(data_posto_file)
  data_posto_file
}


#---------------------------------------------------------------
.replace_bigneg <- function(x) replace(x, which(x < -999), NA)

#---------------------------------------------------------------
# Drops variables in DT which have the same values for all observations.
# @param DT `data.frame`, `tibble` or `data.table`.
# @return \code{\link[tibble]{as_tibble}} of original data excluding
# non-informative variables.
# @details It is used in `extract_metadata`.
# @seealso
#  \code{\link[data.table]{as.data.table}}, \code{\link[tibble]{as_tibble}}
# @importFrom checkmate assert_data_frame
# @importFrom data.table as.data.table uniqueN .SD
# @importFrom tibble as_tibble
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
# Add code and station name from stations metadata
# @param DT tibble
# @param txt_file path to ASCII file
# @return original data with extra columns `code_stn` and `name_stn`.
# @details It is used in `import_qnat`
# @importFrom data.table as.data.table
# @importFrom tibble as_tibble
.add_cod_name <- function(DT, file) {
  # DT = qnat; file =
  DT <- data.table::as.data.table(DT)
  md <- extract_metadata(file, informative = TRUE)
  DT[, c("code_stn", "name_stn") :=
       .(md$estacao_codigo[ DT[["id"]] ],  md$nome_estacao[ DT[["id"]] ])
     ]
  tibble::as_tibble(DT)
}


.check_user <- function(user = "hidrometeorologista"){
  Sys.info()[["login"]] == user
}


wise_select <- function(user = "hidrometeorologista"){
  dplyr::if_else(.check_user(user),
         find_data(TRUE),
         find_data(FALSE)
  )
}


# ------------------------------------------------------------------------------
# Print file
#
# Provides access to the internal data used in example of
# \code{\link[HEobs]{extract_metadata}}
#
# @return character
find_data <- function(local = TRUE){

  if(!local){
    return(data_link)
  }

  # use local file for tests
  if(.check_user("hidrometeorologista")){
    ds_dir <- "~/Dropbox/datasets/GIS/BaciaHidrograficaONS-enviadoProfAssis"
    checkmate::assert_directory_exists(ds_dir)
    #if(dir.exists(ds_dir)){
      ds_file <- list.files(
        ds_dir,
        pattern = "^V.*_2018\\.dat$",
        full.names = TRUE
      )
      return(ds_file)
    }

 return(NULL)
}
