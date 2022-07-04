
# Atualização dos dados de vazão usando API da ONS ----------------------------

#' Download Historical data observed in the reservoirs dispatched by the ONS.
#'
#' @param res_name
#' @param start_date
#' @param end_date
#'
#' @return
#' @export
#'
#' @examples
ons_hist_data <- function(res_name = "FURNAS",
                          start_date = "01/05/2021",
                          end_date = format(lubridate::dmy(start_date) + 365, "%d/%m/%Y")
                          ){

  hist_url <- .ons_url("historico")
  hist_url <- glue::glue(hist_url)

  # to avoid getting flagged as a spammer
  Sys.sleep(1)

  r <- httr::GET(hist_url,httr::timeout(30))

  #checkmate::assert_set_equal(httr::status_code(r), 200)
  if (((httr::status_code(r) %/% 200) != 1)) {
    warning(
      sprintf("Requests for [%s] responded but without an HTTP status code in the 200-299 range",
              hist_url
      )
    )
    return(NA_character_)
  }

  rc <- httr::content(r, "parsed")

  data_tbl <- XML::xmlParse(rc) %>%
    XML::getNodeSet(path = "//tb_historico") %>%
    XML::xmlToDataFrame(stringsAsFactors = FALSE) %>%
    readr::type_convert(
      col_types = readr::cols(
        res_id = readr::col_character(),
        Reservatorio = readr::col_character(),
        Data = readr::col_datetime(format = "%d/%m/%Y %H:%M:%S"),
        Grandeza = readr::col_character(),
        Valor = readr::col_double()
      )
    ) %>%
    tibble::as_tibble() %>%
    tidyr::pivot_wider(names_from = "Grandeza", values_from = "Valor") %>%
    janitor::clean_names() %>%
    dplyr::rename_with(~ .x %>%
                         stringr::str_replace(
                           pattern = "m3_s",
                           replacement = "cumecs"
                         )) %>%
    dplyr::rename("date" = "data") %>%
    dplyr::relocate("vazao_natural_cumecs", .after = "date") %>%
    dplyr::relocate("vazao_incremental_mm_cumecs", .after = "vazao_natural_cumecs")

}


# to download data by year
# if(!is.null(year)){
#   # year = 2021
#   start_date <- paste0("01/01/", min(year))
#   end_date <- paste0("31/12/", year)
# }

# Visualização dos dados baixados
.plot_ons_hist_data <- function(flow_data){

  checkmate::assert_names(names(flow_data),
                          must.include = c("date",
                                           "vazao_natural_cumecs",
                                           "vazao_incremental_mm_cumecs"
                                           )
                          )
  ## check
  flow_data %>%
    openair::timePlot(.,
                      names(select(., contains("cumecs"))),
                      group = TRUE,
                      key.columns = 2,
                      date.format = "%b\n%Y",
                      ylab = "cumecs",
                      lwd = 1.5
    )

}

# Para verificar a correlação entre a vazao_natural e a vazao_incremental_mm ---
# Aparentemente nao precisamos calcular a vazão incremental.
.check_corr <- function(flow_data){
  ## incrementais mais correlacionadas com qnat
  flow_data %>%
    janitor::remove_constant() %>%
    dplyr::select(contains("cumecs")) %>%
    cor(use = "pair") %>%
    as.data.frame() %>%
    tibble::ownames_to_column() %>%
    tibble::s_tibble() %>%
    dplyr::select(1:2)

  # 1 vazao_natural_cumecs                          1     *
  # 2 afluencia_cumecs                              0.985
  # 3 defluencia_cumecs                            -0.377
  # 4 vazao_turbinada_cumecs                       -0.374
  # 5 vazao_de_evaporacao_cumecs                   -0.284
  # 6 vazao_incremental_cumecs                      0.981
  # 7 vazao_incremental_mm_cumecs                   0.990 *
  # 8 vazao_de_uso_consuntivo_cumecs               -0.308

}



# tbl <- ons_hist_data(res_name = "FURNAS", start_date = "01/05/2021")
# .plot_ons_hist_data(tbl)
# .check_corr(tbl)






#  -----------------------------------------------
#' Download metetadata from ONS reservoirs
#'
#' @param res_name short name of reservoir, default is '*' to get metadata
#' from all reservoirs.
#'
#' @return tibble with 46 variables.
#' @export
#'
#' @examples
#' if(FALSE){
#'  library(dplyr)
#'  info_ons <- ons_metadata()
#'  head(info_ons)
#'  # no artificial reservoirs
#'  filter(info_ons, tpres_id != "ART")
#' }
#'
ons_metadata <- function(res_name = "*"){


  cad_url <- .ons_url("cadastro")
  cad_url <- glue::glue(cad_url)

  # to avoid getting flagged as a spammer
  Sys.sleep(1)

  r <- httr::GET(cad_url, httr::timeout(20))

  #checkmate::assert_set_equal(httr::status_code(r), 200)
  if (((httr::status_code(r) %/% 200) != 1)) {
    warning(
      sprintf("Requests for [%s] responded but without an HTTP status code in the 200-299 range",
              cad_url
      )
    )
    return(NULL)
  }

  rc <- httr::content(r, "parsed")

  info_df <- XML::xmlParse(rc) %>%
    XML::getNodeSet(path = "//tb_CadastroReservatorio") %>%
    XML::xmlToDataFrame(stringsAsFactors = FALSE)

  info_tbl <- type.convert(info_df, as.is = TRUE) %>%
     tibble::as_tibble() %>%
    dplyr::mutate(dplyr::across(where(is.character),
                                ~stringr::str_trim(.x, side = "both")
                                )
                  ) %>%
    dplyr::relocate(res_nomecurto, usi_id, tpusina_id, id_resjusante, .before = "bacia_id")

  info_tbl
}

# filter(info_ons, tpres_id != "ART")
# 172 reservatorios
# 19 reservatorios artificiais


#' Sets the proper url according to the requested operation
#'
#' @param type one of the options: "cadastro", "historico",
#'  "media_de_longo_tempo" or "tabela_cota_volume".
#'
#' @return URL for the operation request.
#' @export
#' @keywords internal
#' @examples
.ons_url <- function(
  type = c("cadastro", "historico", "media_de_longo_tempo", "tabela_cota_volume")
) {
  base_url <- "http://aplicam.ons.org.br/hidrologia/reservatorio.asmx"

  ons_url <- switch(type,
    cadastro = paste0(
      base_url,
      "/Cadastro?",
      "Nome={res_name}"
    ),
    historico = paste0(
      base_url, "/Historico?",
      "Reservatorio={res_name}&Inicio={start_date}&Fim={end_date}"
    ),
    media_de_longo_tempo = paste0(
      base_url,
      "/Media_de_longo_tempo?",
      "Reservatorio={res_name}"
    ),
    tabela_cota_volume = paste0(
      base_url,
      "/Tabela_cota_volume?",
      "Reservatorio={res_name}"
    )
  )

  #if(HEgis:::url_exists(ons_url)) return(ons_url)

  #invisible(ons_url)
}


#  -----------------------------------------------
.fix_res_name <- function(x) {
  # x = data_mlt
  if ("ARTIFICIAL" %in% unique(x$tipo_vazao)) {
    x <- x %>%
      dplyr::mutate(res_nomecurto = ifelse(tipo_vazao == "ARTIFICIAL",
        paste0(res_nomecurto, " ART"),
        res_nomecurto
      ))
  }
  x
}



#' Dowload the long-term averages of the flow series of the ONS reservoirs
#'
#' @param res_name short name of reservoir.
#'
#' @return tibble with X columns and Y rows..
#' @export
#'
#' @examples
#' if(FALSE){
#'  mlt <- .ons_mlt(res_name = "ANTA")
#'  mlt
#' }

.ons_mlt <- function(res_name = "FURNAS"){

  message("Downloading MLT para: ", res_name, "\n")

  # res_name = 'ANTA'
  # res_name = 'TIBAGI MONTANTE'
  res_name_orig <- res_name
  res_name <- gsub(" ", "%20", res_name)
  mlt_url <- .ons_url("media_de_longo_tempo") %>%
    glue::glue()

  # to avoid getting flagged as a spammer
  Sys.sleep(1)

  r <- httr::GET(mlt_url, httr::timeout(20))

  #checkmate::assert_set_equal(httr::status_code(r), 200)
  if (((httr::status_code(r) %/% 200) != 1)) {
    warning(
      sprintf("Requests for [%s] responded but without an HTTP status code in the 200-299 range",
              mlt_url
             )
    )
    return(NULL)
  }


  rc <- httr::content(r, "parsed")
  mlt_list <- xml2::as_list(rc)
  mlt_list <- purrr::flatten_df(mlt_list)[["NewDataSet"]] %>%
    purrr::compact()

  if(length(names(mlt_list)) == 1) {
    warning("Não foram encontrados dados para o resrvatório: ",
            res_name_orig,
            "\n",
            mlt_url
    )
    return(
      tibble::tribble(
        ~res_nomecurto, ~res_id,       ~mes,          ~tipo_vazao,   ~valor,
        res_name_orig,  NA_character_, NA_character_, NA_character_, NA_real_
      )
    )
  }

  #names(nested_df) <- paste0(names(nested_df), 1:length(nested_df))
  data_mlt <- purrr::map_df(mlt_list, ~.x %>% purrr::flatten_df())


  data_mlt <- type.convert(data_mlt, as.is = TRUE) %>%
    dplyr::mutate(dplyr::across(where(is.character),
                                ~stringr::str_trim(.x, side = "both")
    )
    ) %>%
    dplyr::rename_with(.fn = ~.x %>% stringr::str_replace_all("x0020_", "")) %>%
    janitor::clean_names() %>%
    dplyr::mutate(res_nomecurto = stringr::str_replace_all(res_name, "%20", " ")) %>%
    dplyr::relocate(res_nomecurto)

  .fix_res_name(data_mlt)


}


ons_longterm_all <- function(){
  md <- ons_metadata()
  # dplyr::filter(md, is.na(tpusina_id))
  # table(md$tpres_id)
  # ART CED FIC FIO INC RBB RCU RES
  # 19   1   2  92   1   3  62  11
  #res_names <-  dplyr::filter(md, !is.na(tpusina_id)) %>%
  #  dplyr::pull("res_nomecurto")
  purrr::map_df(res_names, ~.ons_mlt(.x))
}

# ons_mlt_data <- ons_longterm_all()
#
# by_res <- ons_mlt_data %>% dplyr::group_by(res_nomecurto) %>% dplyr::tally()
# #24 linhas?
# by_res
#
# nao_baixados <- res_names[!res_names %in% by_res$res_nomecurto]
# View(dplyr::filter(md, !res_nomecurto %in% by_res$res_nomecurto) %>%
#        dplyr::relocate("tpres_id", .after = "res_nomecurto")
# )
#
#
#
# # tabela cota x volume
# # http://aplicam.ons.org.br/hidrologia/Reservatorio.asmx?op=Tabela_cota_volume
#
# ons_mlt_data %>%
# dplyr::filter(tipo_vazao == "ARTIFICIAL") %>%
#   dplyr::distinct(res_nomecurto, tipo_vazao)
#
#
# by_res %>%
#   dplyr::filter(n == 24)
#
