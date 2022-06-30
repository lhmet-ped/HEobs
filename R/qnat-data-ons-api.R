
# Atualização dos dados de vazão usando API da ONS ----------------------------

ons_hist_data <- function(res_name = "FURNAS",
                          start_date = "01/05/2021",
                          end_date = format(lubridate::dmy(start_date) + 365, "%d/%m/%Y"),
                          ){

  base_url <- "http://aplicam.ons.org.br/hidrologia/Reservatorio.asmx"
  hist_url <- paste0(
    base_url, "/Historico?",
    "Reservatorio={res_name}&Inicio={start_date}&Fim={end_date}"
  )
  hist_url <- glue::glue(hist_url)

  # to avoid getting flagged as a spammer
  Sys.sleep(1)

  r <- httr::GET(hist_url)

  checkmate::assert_set_equal(httr::status_code(r), 200)

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



tbl <- ons_hist_data(res_name = "FURNAS", start_date = "01/05/2021")
.plot_ons_hist_data(tbl)
.check_corr(tbl)






# metadata dos reservatorios ONS -----------------------------------------------
# r <- httr::GET("http://aplicam.ons.org.br/hidrologia/Reservatorio.asmx/Cadastro?Nome=*")
# rc <- httr::content(r, "parsed")
#
# info_df <- XML::xmlParse(rc) %>%
#   XML::getNodeSet(path = "//tb_CadastroReservatorio") %>%
#   XML::xmlToDataFrame(stringsAsFactors = FALSE)
#
# info_tbl <- type.convert(info_df, as.is = TRUE) %>%
#    tibble::as_tibble() %>%
#   dplyr::mutate(across(where(is.character), ~str_trim(.x, side = "both")))
#
# info_tbl$res_nomecurto


# Media de longo termo --------------------------------------------------------
# TO DO
# http://aplicam.ons.org.br/hidrologia/Reservatorio.asmx?op=Media_de_longo_tempo


# tabela cota x volume
# http://aplicam.ons.org.br/hidrologia/Reservatorio.asmx?op=Tabela_cota_volume


