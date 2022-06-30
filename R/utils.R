utils::globalVariables(c(
  ".", "V1", "V2", "id", "data", "valor", "qnat", "code_stn"
))

## last update 30/06/2022 ------------------------------------------------------
# info_ons <- ons_metadata()
# dput(unique(info_ons$res_nomecurto))
ons_reservoir_names <- c("14 DE JULHO", "A. VERMELHA", "AIMORES", "ANTA", "ANTA ART",
  "B. BONITA", "B. BONITA ART", "B.COQUEIROS", "BAGUARI", "BAIXO IGUACU",
  "BALBINA", "BARIRI", "BARIRI ART", "BARRA BRAUNA", "BARRA GRANDE",
  "BATALHA", "BELO MONTE", "BILL E PEDRAS", "BILLINGS", "BILLINGS ART",
  "BOA ESPERANÇA", "C. DOURADA", "C.BRANCO-1", "C.BRANCO-2", "CACHOEIRA CALDEIRAO",
  "CACONDE", "CACU", "CAMARGOS", "CAMPOS NOVOS", "CANA BRAVA",
  "CANAL P. BARRETO", "CANDONGA", "CANOAS I", "CANOAS II", "CAPANEMA",
  "CAPIVARA", "CASTRO ALVES", "CHAVANTES", "COARACY NUNES", "COLIDER",
  "CORUMBA", "CORUMBA-3", "CORUMBA-4", "CURUA-UNA", "D. FRANCISCA",
  "DARDANELOS", "E. DA CUNHA", "EDGARD SOUZA", "EMBORCAÇÃO",
  "ERNESTINA", "ESPORA", "ESTREITO", "FERREIRA GOMES", "FONTES",
  "FOZ CHAPECO", "FOZ DO RIO CLARO", "FUNDÃO", "FUNIL", "FUNIL-MG",
  "FURNAS", "G. B. MUNHOZ", "G. P. SOUZA", "GARIBALDI", "GUAPORE",
  "GUARAPIRANGA", "GUILM. AMORIM", "HENRY BORDEN", "I. SOLTEIRA",
  "IBITINGA", "IBITINGA ART", "IGARAPAVA", "ILHA + T. IRMÃOS",
  "ILHA EQUIVALENTE ART", "ILHA POMBOS", "ILHA POMBOS ART", "IRAPE",
  "ITAIPU", "ITAIPU ART", "ITAPARICA", "ITAPEBI", "ITAUBA", "ITIQUIRA I",
  "ITIQUIRA II", "ITUMBIARA", "ITUTINGA", "ITÁ", "JACUI", "JAGUARA",
  "JAGUARI", "JAURU", "JIRAU", "JORDAO ART", "JORDÃO", "JUPIA",
  "JUPIA ART", "JURUMIRIM", "L. C. BARRETO", "LAJEADO", "LAJES",
  "LAJES ART", "LIMOEIRO", "M. MORAES", "MACHADINHO", "MANSO",
  "MARIMBONDO", "MASCARENHAS", "MAUA", "MIRANDA", "MONJOLINHO",
  "MONTE CLARO", "MOXOTO", "N. AVANHANDAVA", "N. AVANHANDAVA ART",
  "NILO PEÇANHA", "NOVA PONTE", "OURINHOS", "P. AFONSO 1,2,3",
  "P. AFONSO 4", "P. COLOMBIA", "PARAIBUNA", "PASSO FUNDO", "PASSO REAL",
  "PASSO SAO JOAO", "PEDRA DO CAVALO", "PEDRAS", "PEIXE ANGICAL",
  "PEREIRA PASSOS", "PEREIRA PASSOS ART", "PICADA", "PIMENTAL",
  "PIMENTAL ART", "PIRAJU", "PONTE DE PEDRA", "PONTE NOVA", "PORTO ESTRELA",
  "PORTO PRIMAVERA", "PORTO PRIMAVERA ART", "PROMISSAO ART", "PROMISSÃO",
  "QUEBRA QUEIXO", "QUEIMADO", "R-11", "RETIRO BAIXO", "RONDON II",
  "ROSAL", "ROSANA", "S.DO FACÃO", "S.R.VERDINHO", "SA CARVALHO",
  "SALTO", "SALTO CAXIAS", "SALTO GRANDE CM", "SALTO GRANDE CS",
  "SALTO OSORIO", "SALTO PILAO", "SALTO SANTIAGO", "SAMUEL", "SANTA BRANCA",
  "SANTA CECILIA", "SANTA CLARA-PR", "SANTANA", "SANTANA ART",
  "SANTO ANTONIO", "SANTONIO CM", "SAO DOMINGOS", "SAO JOSE", "SAO MANOEL",
  "SAO ROQUE", "SAO SALVADOR", "SEGREDO", "SEGREDO ART", "SERRA DA MESA",
  "SIMPLICIO", "SINOP", "SOBRADINHO", "SOBRADINHO INCR", "SOBRAGI",
  "STA.CLARA-MG", "STO ANTONIO DO JARI", "SÃO SIMÃO", "TAQUARUÇU",
  "TELES PIRES", "TIBAGI MONTANTE", "TOCOS", "TRES IRMAOS ART",
  "TRÊS IRMÃOS", "TRÊS MARIAS", "TUCURUI", "VIGARIO", "VOLTA GRANDE",
  "XINGO")

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
