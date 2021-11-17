montar_DRE_por_empresa <- function(tbl_dre = dfs$tbl_dre,
                                   tbl_ds_conta = dfs$tbl_ds_conta,
                                   anual_trimestral = "trimestral",
                                   contas_selec = c("Resultado Bruto"),
                                   empresa_selec = "MGLU",
                                   linha_coluna = "linha") {
  contas_selec = (tbl_ds_conta %>% filter(ds_conta %in% contas_selec))$cd_conta
  tbl_dre_empresa = tbl_dre %>% filter(regex == empresa_selec) %>% filter(cd_conta %in% contas_selec)
  if (anual_trimestral == "anual") {
    tbl_dre_empresa = tbl_dre_empresa %>%
      group_by(paste(cd_conta, lubridate::year(dte_data))) %>%
      summarise(
        dte_data = last(lubridate::year(dte_data)),
        ds_conta = last(ds_conta),
        vl_conta = sum(vl_conta, na.rm = TRUE)
      ) %>%
      ungroup()
  }
  if (anual_trimestral == "trimestral") {
    tbl_dre_empresa = tbl_dre_empresa %>%
      ungroup() %>%
      arrange(dte_data) %>%
      mutate(dte_data = as.Date(dte_data))
  }
  if (linha_coluna == "linha") {
    type = "scatter"
  }
  if (linha_coluna == "coluna") {
    type = "bar"
  }
  #  print(min(tbl_dre_empresa$dte_data))
  return(
    plot_ly(
      tbl_dre_empresa,
      x =  ~ dte_data,
      y =  ~ vl_conta,
      color =  ~ ds_conta,
      type = type,
      mode = 'lines'
    ) %>%
      layout(legend = list(x = 0,
                           y = 1))
  )
}

montar_BP_por_empresa <-
  function(tbl_bp = rbind(dfs$tbl_bp_ativo, dfs$tbl_bp_passivo),
           tbl_ds_conta,
           contas_selec = c("Ativo Total", "Passivo Total"),
           empresa_selec = "MGLU",
           linha_coluna = "linha") {
    contas_selec = (tbl_ds_conta %>% filter(ds_conta %in% contas_selec))$cd_conta
    tbl_bp_empresa = tbl_bp %>%
      filter(regex == empresa_selec) %>%
      filter(cd_conta %in% contas_selec)
    if (linha_coluna == "linha") {
      type = "scatter"
    }
    if (linha_coluna == "coluna") {
      type = "bar"
    }
    return(
      plot_ly(
        tbl_bp_empresa %>% arrange(dte_data),
        x =  ~ dte_data,
        y =  ~ vl_conta,
        color =  ~ ds_conta,
        type = type,
        mode = 'lines'
      ) %>%
        layout(legend = list(x = 0,
                             y = 1))
    )
  }
montar_DRE_entre_empresa <- function(tbl_dre = dfs$tbl_dre,
                                     tbl_ds_conta = tbl_ds_conta,
                                     anual_trimestral = "trimestral",
                                     conta_selec = "Resultado Bruto",
                                     empresas_selec = c("MGLU", "RADL"),
                                     linha_coluna = "linha") {
  conta_selec = (tbl_ds_conta %>% filter(ds_conta == conta_selec))$cd_conta
  tbl_dre_empresa = tbl_dre %>% filter(regex %in% empresas_selec) %>% filter(cd_conta ==
                                                                               conta_selec)
  print(anual_trimestral)
  if (anual_trimestral == "anual") {
    tbl_dre_empresa = tbl_dre_empresa %>%
      group_by(paste(regex, lubridate::year(dte_data))) %>%
      summarise(
        dte_data = last(lubridate::year(dte_data)),
        regex = last(regex),
        vl_conta = sum(vl_conta, na.rm = TRUE)
      ) %>%
      ungroup()
  }
  if (anual_trimestral == "trimestral") {
    tbl_dre_empresa = tbl_dre_empresa %>% ungroup() %>% arrange(dte_data) %>%
      mutate(dte_data = as.Date(dte_data))
  }
  if (linha_coluna == "linha") {
    type = "scatter"
  }
  if (linha_coluna == "coluna") {
    type = "bar"
  }
  return(
    plot_ly(
      tbl_dre_empresa,
      x =  ~ dte_data,
      y =  ~ vl_conta,
      color =  ~ regex,
      type = type,
      mode = 'lines'
    ) %>%
      layout(legend = list(x = 0,
                           y = 1))
  )
}
montar_BP_entre_empresa <- function(tbl_bp,
                                    tbl_ds_conta,
                                    conta_selec = "Ativo Total",
                                    empresas_selec = c("MGLU", "RADL"),
                                    linha_coluna = "linha") {
  conta_selec = (tbl_ds_conta %>% filter(ds_conta == conta_selec))$cd_conta
  tbl_bp_empresa = tbl_bp %>% filter(regex %in% empresas_selec)
  tbl_bp_empresa = tbl_bp_empresa %>% filter(cd_conta == conta_selec)
  if (linha_coluna == "linha") {
    type = "scatter"
  }
  if (linha_coluna == "coluna") {
    type = "bar"
  }
  return(
    plot_ly(
      tbl_bp_empresa %>% arrange(dte_data),
      x =  ~ dte_data,
      y =  ~ vl_conta,
      color =  ~ regex,
      type = type,
      mode = 'lines'
    ) %>%
      layout(legend = list(x = 0,
                           y = 1))
  )
}


montar_cotacao_entre_empresa <- function(tbl_cotacao,
                                         tickers_selec,
                                         qual_variavel) {
  if (!is.null(tickers_selec)) {
    tbl_cotacao_empresa = tbl_cotacao %>% filter(str_ticker %in% tickers_selec)
    tbl_cotacao_empresa = merge(tbl_cotacao_empresa, buscar_num_acoes(tickers_selec))
    tbl_cotacao_empresa = tbl_cotacao_empresa %>%
      ungroup() %>% mutate(valor_empresa = vl_acao * vl_close)
    tbl_cotacao_empresa$qual_variavel = tbl_cotacao_empresa[, qual_variavel]
    return(
      plot_ly(
        tbl_cotacao_empresa %>% arrange(dte_data),
        x =  ~ dte_data,
        y =  ~ qual_variavel,
        color =  ~ str_ticker,
        type = "scatter",
        mode = 'lines'
      ) %>%
        layout(legend = list(x = 0,
                             y = 1))
    )
  }
  return()
}

montar_multiplo_entre_empresas <- function(tbl_dre = dfs$tbl_dre,
                                           tbl_bp = rbind(dfs$tbl_bp_ativo, dfs$tbl_bp_passivo),
                                           #tbl_empresas=dfs$tbl_empresas,
                                           #tbl_cotacoes=dfs$tbl_cotacoes,
                                           empresas_selec,
                                           qual_variavel = "ROE",
                                           linha_coluna = "linha") {
  if (!is.null(empresas_selec)) {
    tbl_dre = tbl_dre %>% filter(regex %in% empresas_selec) %>% ungroup()
    tbl_bp = tbl_bp %>% filter(regex %in% empresas_selec) %>% ungroup()
    
    print(empresas_selec)
    
    
    tbl_painel_metricas = construir_metricas(tbl_dre, tbl_bp, empresas_selec)
    
    tbl_painel_metricas$qual_variavel = tbl_painel_metricas[, qual_variavel]
    if (linha_coluna == "linha") {
      type = "scatter"
    }
    if (linha_coluna == "coluna") {
      type = "bar"
    }
    return(
      plot_ly(
        tbl_painel_metricas %>% arrange(dte_data),
        x =  ~ dte_data,
        y =  ~ qual_variavel,
        color =  ~ str_empresa,
        type = type,
        mode = 'lines'
      ) %>%
        layout(legend = list(x = 0,
                             y = 1))
    )
  }
  return()
}
montar_variavel_macro <- function(variavel_macro) {
  if (variavel_macro != "") {
    if (variavel_macro %in% c("dolar", "juros")) {
      df = importar_sgs(variavel_macro)
      colnames(df) = c("dte_data", "vl_valor")
    } else{
      df = importar_sidra(variavel_macro)
      colnames(df) = c("dte_data", "vl_valor")
    }
    return(plot_ly(
      df,
      x =  ~ dte_data,
      y =  ~ vl_valor,
      type = 'scatter',
      mode = 'lines'
    ))
  }
  return()
}


divulgacao_entre_empresa <-
  function(tbl_divulgacoes = dfs$tbl_divulgacoes,
           tbl_empresas = dfs$tbl_empresas,
           empresas_selec = c("MGLU", "PETR")) {
    empresas = (tbl_empresas %>% filter(regex %in% empresas_selec))
    
    tbl_divulgacoes = merge(tbl_divulgacoes, empresas %>% summarise(regex, str_cia_CVM))
    
    return(
      plot_ly(
        tbl_divulgacoes %>% arrange(dte_refer),
        x =  ~ dt_publi,
        y =  ~ dte_refer,
        type = 'scatter',
        color =  ~ regex,
        mode = 'lines+markers'
      ) %>% layout(legend = list(x = 0, y = 1))
    )
  }
