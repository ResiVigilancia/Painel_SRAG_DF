# # Baixar pacotes --------------------------------------------------------

# 1) Lista dos pacotes CRAN necessários
cran_pkgs <- c(
  "shiny", "shinydashboard", "tidyverse", "lubridate", "plotly", "forecast",
  "EpiEstim", "DT", "officer", "rvg", "scales", "readxl",
  "purrr", "mem", "zoo", "fmesher"
)

# 2) Instala apenas os que faltam
to_install <- cran_pkgs[! cran_pkgs %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) {
  install.packages(to_install)
} else {
  message("Todos os pacotes CRAN já estão instalados.")
}

# 3) Opcional: remotes para eventuais pacotes GitHub
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# 4) INLA (repositório oficial)
if (!requireNamespace("INLA", quietly = TRUE)) {
  install.packages(
    "INLA",
    repos = c(INLA = "https://inla.r-inla-download.org/R/stable"),
    dependencies = TRUE
  )
}

# 5) Verificação do Rtools (Windows)
if (tolower(.Platform$OS.type) == "windows") {
  if (!pkgbuild::has_build_tools(debug = FALSE)) {
    warning(
      "Rtools não encontrado: instale o Rtools 4.4 em ",
      "https://cran.r-project.org/bin/windows/Rtools/ e reinicie o R."
    )
  }
}

# 6) Carregamento das bibliotecas
lapply(
  c(cran_pkgs, "remotes", "INLA"),
  function(pkg) suppressPackageStartupMessages(library(pkg, character.only = TRUE))
)

if (!require("shinyWidgets")) install.packages("shinyWidgets")
library(shinyWidgets)  # Para o pickerInput melhorado


library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(plotly)
library(forecast)
library(EpiEstim)
library(DT)
library(officer)
library(rvg)
library(scales)
library(readxl)
library(purrr)
library(mem)
library(zoo)
library(devtools)
library(INLA)
library(fmesher)


# Baixar banco de dados ---------------------------------------------------


# Definir caminho da pasta de dados
pasta_dados <- "CAMINHO DA PASTA PARA O BANCO DE DADOS DE SRAG"

# Função para carregar o arquivo SRAG mais recente
carregar_srag <- function() {
  arquivos <- list.files(path = pasta_dados,
                         pattern = "^Casos_SRAG_.*\\.xlsx$",  #Procura uma planilha em xlsx que contenha o nome Casos_SRAG
                         full.names = TRUE)
  mais_recente <- arquivos[which.max(file.mtime(arquivos))]
  readxl::read_excel(mais_recente)
}

# Carregar dados brutos e padronizar
srag <- carregar_srag() %>%
  mutate(
    ID_MN_RESI_pad = ID_MN_RESI %>% tolower() %>% str_trim(),
    ano            = year(DT_SIN_PRI),
    SE             = as.integer(SE)
  ) %>%
  # filtrar apenas Leste e remover "brasilia" se existir
  filter(res_RS == "Leste", ID_MN_RESI_pad != "brasilia")

pop <- readxl::read_excel(file.path(pasta_dados, "PLANILHA DA POPULAÇÃO")) %>%
  mutate(
    REG_ADM_pad = REG_ADM %>%
      stringi::stri_trans_general("Latin-ASCII") %>% tolower() %>% str_trim()
  )
regioes_adm <- sort(unique(srag$ID_MN_RESI_pad))
regioes_adm <- c("Todas", regioes_adm)

# 1. No início do script (após carregar os dados), adicione:

# Identifica quais vírus estão presentes nos dados (com pelo menos um registro = 1)
virus_presentes <- srag %>%
  select(starts_with("PCR_")) %>%
  summarise(across(everything(), ~any(. == 1, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "codigo", values_to = "presente") %>%
  filter(presente) %>%
  pull(codigo)

# Mapeamento dos nomes dos vírus
virus_map <- c(
  PCR_FLUASU = "Influenza A",
  PCR_FLUBLI = "Influenza B",
  PCR_SARS2  = "SARS-CoV-2",
  PCR_VSR    = "Vírus Sinsicial Respiratório",
  PCR_PARA1  = "Parainfluenza 1",
  PCR_PARA2  = "Parainfluenza 2",
  PCR_PARA3  = "Parainfluenza 3",
  PCR_PARA4  = "Parainfluenza 4",
  PCR_ADENO  = "Adenovírus",
  PCR_METAP  = "Metapneumovírus",
  PCR_BOCA   = "Bocavírus",
  PCR_RINO   = "Rinovírus",
  PCR_OUTRO  = "Outro vírus respiratório"
) %>% 
  keep(names(.) %in% c("Todos", virus_presentes))

# Opções de filtro
anos_disponiveis <- sort(unique(srag$ano))
niveis_idade    <- c("Menor de 2","2 a 10","11 a 19","20 a 29","30 a 39",
                     "40 a 49","50 a 59","60 a 69","70 a 79","80 e mais")


# # Definir interface (UI) ------------------------------------------------


ui <- dashboardPage(
  dashboardHeader(title = "Painel SRAG – Análise Epidemiológica"),
  dashboardSidebar(
    sidebarMenu(
      sliderInput("filtro_ano", "Ano",
                  min   = min(anos_disponiveis),
                  max   = max(anos_disponiveis),
                  value = range(anos_disponiveis),
                  step  = 1, sep = ""),
      # Novo filtro de vírus (substitua o existente)
      pickerInput(
        inputId = "filtro_virus_monitor",
        label = "Tipo de Vírus", 
        choices = virus_map,
        selected = "Todos",  # Seleciona "Todos" por padrão
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `selected-text-format` = "count > 2",
          `count-selected-text` = "{0} tipos selecionados",
          `none-selected-text` = "Nenhum vírus selecionado"
        )
      ),
      selectInput(
        "filtro_regiao",
        "Região Administrativa",
        choices = regioes_adm,
        selected = "Todas"),
      menuItem("Visão Geral",       tabName = "visao_geral",    icon = icon("chart-bar")),
      menuItem("Monitoramento",     tabName = "monitoramento",  icon = icon("exclamation-triangle")),
      menuItem("Previsão",          tabName = "previsao",       icon = icon("calendar")),
      menuItem("Resultados PCR",    tabName = "resultados_pcr", icon = icon("vial"))
    )
  ),
  dashboardBody(
    tabItems(
      # Aba Visão Geral (sem alterações)…
      tabItem(tabName = "visao_geral",
              fluidRow(
                valueBoxOutput("box_total_casos",  width = 4),
                valueBoxOutput("box_media_casos",  width = 4),
                valueBoxOutput("box_total_obitos", width = 4)
              ),
              fluidRow(box(width=12, plotlyOutput("graf_casos_ano"))),
              fluidRow(
                box(width=6, plotlyOutput("graf_faixa_idade")),
                box(width=6, plotlyOutput("graf_ventilacao"))
              ),
              fluidRow(box(width=12, plotlyOutput("graf_obitos"))),
              fluidRow(
                box(width=6, plotlyOutput("graf_tempo_internacao")),
                box(width=6, plotlyOutput("graf_evolucao"))
              ),
              fluidRow(box(width=12, plotlyOutput("graf_hospitalizacao")))
      ),
      
      # Aba Monitoramento (com o novo filtro interno)
      tabItem(tabName = "monitoramento",
              # filtro de ano
              fluidRow(
                box(
                  width = 4,
                  selectInput(
                    "ano_thresh",
                    "Ano p/ Limiar Epidêmico",
                    choices  = sort(unique(srag$ano)),
                    selected = max(srag$ano)
                  )
                )
              ),
              
              # Diagrama de controle
              box(
                title = "Diagrama de Controle de SRAG",
                status = "primary", solidHeader = TRUE, width = 12,
                plotlyOutput("diagrama_controle", height = "400px")
              ),
              
              fluidRow(box(width = 12, plotlyOutput("graf_nowcasting"))),
              fluidRow(box(width = 12, plotlyOutput("graf_rt")))
      ),
      
      
      # Aba Previsão (sem alterações)…
      tabItem(tabName = "previsao",
              fluidRow(box(width=12, plotlyOutput("graf_previsao")))
      ),
      
      # Aba Resultados PCR (sem alterações)…
      tabItem(tabName = "resultados_pcr",
              fluidRow(box(width=12, plotlyOutput("graf_pcr_totais"))),
              fluidRow(box(width=12, plotlyOutput("graf_pcr_semana")))
      )
    )
  )
)


# # Servidor --------------------------------------------------------------


server <- function(input, output, session) {
  
  # Dados filtrados apenas por ano
  dados_filtrados <- reactive({
    srag %>%
      filter(
        ano >= input$filtro_ano[1],
        ano <= input$filtro_ano[2]
      )
  })
  
  # 2) define quais colunas PCR foram selecionadas
  virus_sel_monitor <- reactive({
    names(virus_map)[virus_map %in% input$filtro_virus_monitor]
  })
  
  # Filtra os dados por vírus selecionados
  # Define quais colunas PCR foram selecionadas
  virus_sel_monitor <- reactive({
    if ("Todos" %in% input$filtro_virus_monitor) {
      names(virus_map)[-1]  # Todos exceto a opção "Todos"
    } else {
      names(virus_map)[virus_map %in% input$filtro_virus_monitor]
    }
  })
  
  # Filtra os dados por vírus selecionados
  dados_monitorados <- reactive({
    req(input$filtro_virus_monitor)
    
    sel <- virus_sel_monitor()
    if (length(sel) == 0) return(dados_filtrados()[FALSE, ])  # Retorna dataframe vazio se nenhum selecionado
    
    dados_filtrados() %>%
      filter(if_any(all_of(sel), ~ . == 1))
  })
    
  # Infoboxes
  output$box_total_casos <- renderValueBox({
    tot <- nrow(dados_filtrados())
    valueBox(
      formatC(tot, format="d", big.mark=".", decimal.mark=",") ,
      "Total de Casos",
      icon = icon("virus"),
      color = "blue"
    )
  })
  
  output$box_media_casos <- renderValueBox({
    stats <- dados_filtrados() %>%
      group_by(ano, SE) %>% summarise(casos = n(), .groups = "drop")
    m  <- round(mean(stats$casos, na.rm = TRUE), 1)
    dp <- round(sd( stats$casos, na.rm = TRUE), 1)
    valueBox(
      paste0(m, " ± ", dp),
      "Média de Casos (±DP)",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  output$box_total_obitos <- renderValueBox({
    ob <- dados_filtrados() %>% filter(OBITOSRAG == "Sim") %>% nrow()
    valueBox(
      formatC(ob, format="d", big.mark=".", decimal.mark=",") ,
      "Total de Óbitos",
      icon = icon("heartbeat"),
      color = "red"
    )
  })
  
  # Gráfico de Casos por Ano
  output$graf_casos_ano <- renderPlotly({
    df <- dados_filtrados() %>%
      group_by(ano, SE) %>% summarise(casos = n(), .groups = "drop") %>%
      mutate(SE = as.integer(SE)) %>%
      complete(ano, SE = 1:53, fill = list(casos = 0)) %>%
      arrange(ano, SE) %>%
      mutate(data_inicio = make_date(ano) + weeks(SE - 1))
    
    p <- ggplot(df, aes(x = data_inicio, y = casos, fill = factor(ano))) +
      geom_col() +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      labs(title = "Casos por Semana Epidemiológica", x = "Ano", y = "Casos", fill = "Ano") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Faixa Etária
  output$graf_faixa_idade <- renderPlotly({
    df <- dados_filtrados() %>%
      filter(!is.na(FE1)) %>%
      mutate(FE1 = factor(FE1, levels = niveis_idade)) %>%
      group_by(FE1) %>%
      summarise(casos = n(), .groups = "drop") %>%
      mutate(percentual = round(casos / sum(casos) * 100, 1))
    
    p <- ggplot(df, aes(x = FE1, y = casos)) +
      geom_col(fill = "#4E79A7") +
      geom_text(aes(label = paste0(percentual, "%")),
                position = position_stack(vjust = 0.5), size = 3) +
      labs(title = "Distribuição por Faixa Etária", x = "Faixa Etária", y = "Casos") +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  # Gráfico de Suporte Ventilatório com percentual
  output$graf_ventilacao <- renderPlotly({
    df <- dados_filtrados() %>%
      count(SUPORT_VEN2) %>%
      rename(casos = n) %>%
      mutate(percentual = round(casos / sum(casos) * 100, 1))
    
    p <- ggplot(df, aes(x = SUPORT_VEN2, y = casos)) +
      geom_col(fill = "#E15759") +
      geom_text(aes(label = paste0(percentual, "%")),
                position = position_stack(vjust = 0.5), size = 3) +
      labs(title = "Casos por Suporte Ventilatório", x = "Tipo de Suporte", y = "Casos") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")
    ggplotly(p)
  })
  
  # Óbitos
  output$graf_obitos <- renderPlotly({
    df <- dados_filtrados() %>%
      group_by(ano, SE) %>%
      summarise(obitos = sum(OBITOSRAG == "Sim", na.rm = TRUE), .groups = "drop") %>%
      mutate(SE = as.integer(SE)) %>%
      complete(ano, SE = 1:53, fill = list(obitos = 0)) %>%
      arrange(ano, SE) %>%
      mutate(data_inicio = make_date(ano) + weeks(SE - 1))
    
    p <- ggplot(df, aes(x = data_inicio, y = obitos)) +
      geom_col(fill = "red") +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      labs(title = "Óbitos por Semana Epidemiológica", x = "Ano", y = "Óbitos") +
      theme_minimal()
    ggplotly(p)
  })
  
  # ——— Boxplot de Tempo de Internação POR Faixa Etária ———
  output$graf_tempo_internacao <- renderPlotly({
    df <- dados_filtrados() %>%
      filter(!is.na(Tempo_Evolucao), !is.na(FE1)) %>%
      # garante a ordem correta das faixas
      mutate(FE1 = factor(FE1, levels = niveis_idade))
    
    p <- ggplot(df, aes(x = FE1, y = Tempo_Evolucao)) +
      geom_boxplot(fill = "#59A14F") +
      labs(
        title = "Tempo de Internação (dias) por Faixa Etária",
        x     = "Faixa Etária",
        y     = "Dias"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Evolução dos Casos
  output$graf_evolucao <- renderPlotly({
    df <- dados_filtrados() %>%
      filter(!is.na(EVOL1)) %>%
      group_by(EVOL1) %>% summarise(casos=n(), .groups="drop")
    p <- ggplot(df, aes(x=reorder(EVOL1, casos), y=casos)) +
      geom_col(fill="#F28E2B") +
      labs(title="Evolução dos Casos", x="Desfecho", y="Casos") +
      theme_minimal() + theme(legend.position="none")
    ggplotly(p)
  })
  
  # Hospitalização por Unidade
  output$graf_hospitalizacao <- renderPlotly({
    df <- dados_filtrados() %>%
      filter(!is.na(NM_UN_INTE)) %>%
      group_by(NM_UN_INTE) %>% summarise(casos=n(), .groups="drop")
    plot_ly(df, x=~casos, y=~reorder(NM_UN_INTE, casos),
            type='bar', orientation='h',
            marker=list(color='rgba(50,171,96,0.7)')) %>%
      layout(title="Hospitalizações por Unidade",
             xaxis=list(title="Casos"), yaxis=list(title="Unidade"))
  })
  
  ## ─────────────────────────────────────────────────────────────────────────────
  ## Diagrama de controle
  ## ─────────────────────────────────────────────────────────────────────────────
  output$diagrama_controle <- renderPlotly({
    req(input$regiao_admin, input$ano_thresh)
    
    # 1) filtra SRAG pela região selecionada na sidebar
    # use exatamente o valor que vem do filtro da sidebar
    srag_sel <- srag %>%
      filter(ID_MN_RESI == input$regiao_admin)
    
    
    # 2) junta população e calcula incidência
    dados_casos <- srag_sel %>%
      left_join(pop, by = c("ano", "ID_MN_RESI_pad")) %>%
      filter(!is.na(populacao)) %>%
      mutate(inc = 100000 * 1 / populacao)
    
    # 3) monta histórico e limiares com input$ano_thresh…
    ano_ref   <- as.integer(input$ano_thresh)
    anos_hist <- (ano_ref - 10):(ano_ref - 1)
    
    historico <- dados_casos %>%
      filter(ano %in% anos_hist) %>%
      group_by(SE) %>%
      summarise(
        media  = mean(100000 * n() / mean(populacao)),
        sd     = sd(   100000 * n() / mean(populacao)),
        .groups = "drop"
      ) %>%
      mutate(
        lim_1 = media + sd,
        lim_2 = media + 2 * sd,
        lim_3 = media + 3 * sd
      )
    
    atual <- dados_casos %>%
      filter(ano == ano_ref) %>%
      group_by(SE) %>%
      summarise(
        casos = n(),
        pop   = mean(populacao),
        inc   = 100000 * casos / pop,
        .groups = "drop"
      )
    
    p <- ggplot() +
      geom_ribbon(data = historico, aes(x = SE, ymin = 0,   ymax = media, fill = "Zona de êxito"),    alpha = 0.3) +
      geom_ribbon(data = historico, aes(x = SE, ymin = media, ymax = lim_1,  fill = "Zona de segurança"), alpha = 0.3) +
      geom_ribbon(data = historico, aes(x = SE, ymin = lim_1, ymax = lim_2,  fill = "Zona de alerta"),    alpha = 0.3) +
      geom_ribbon(data = historico, aes(x = SE, ymin = lim_2, ymax = lim_3,  fill = "Zona de risco"),     alpha = 0.3) +
      geom_line(  data = atual,     aes(x = SE, y = inc, color = "Incidência atual"), size = 1.2) +
      labs(
        x    = "Semana Epidemiológica",
        y    = "Incidência por 100 mil hab.",
        fill = "Faixas de risco",
        color = NULL,
        title = paste("Diagrama de Controle de SRAG —", ano_ref, "| Região:", input$regiao_admin)
      ) +
      scale_fill_manual(values = c(
        "Zona de êxito"     = "green",
        "Zona de segurança" = "yellow",
        "Zona de alerta"    = "orange",
        "Zona de risco"     = "red"
      )) +
      scale_color_manual(values = c("Incidência atual" = "blue")) +
      theme_minimal() +
      theme(legend.position = "right")
    
    ggplotly(p)
  })
  
  

  
  # Monitoramento: Nowcasting
  output$graf_nowcasting <- renderPlotly({
    # Filtra por região selecionada
    dados_filtrados_regiao <- if (input$filtro_regiao == "Todas") {
      dados_filtrados()
    } else {
      dados_filtrados() %>% filter(ID_MN_RESI_pad == input$filtro_regiao)
    }
    
    # Filtra para o ano mais recente
    dados_now <- dados_filtrados_regiao %>%
      filter(ano == input$filtro_ano[2]) %>%
      filter(
        !is.na(DT_SIN_PRI), 
        !is.na(DT_NOTIFIC),
        !is.na(SE),
        as.numeric(DT_NOTIFIC - DT_SIN_PRI) >= 0
      ) %>%
      mutate(
        atraso_dias = as.numeric(DT_NOTIFIC - DT_SIN_PRI),
        SE = as.integer(SE)
      ) %>%
      count(SE, atraso_dias, name = "n")
    
    # Verificação robusta de dados
    if (nrow(dados_now) < 30 || length(unique(dados_now$SE)) < 3) {
      return(
        plotly_empty() %>% 
          layout(title = paste("Dados insuficientes para modelagem (n =", nrow(dados_now), "| Mínimo: 30 observações e 3 semanas)"))
      )
    }
    
    # Modelo simplificado e estável
    tryCatch({
      dados_modelo <- dados_now %>%
        mutate(
          SE_f = factor(SE),  # Usa factor() em vez de as.numeric()
          atraso_f = factor(atraso_dias)
        )
      
      # Fórmula mais simples
      formula <- n ~ SE_f + f(atraso_f, model = "iid")  # Removido efeito aleatório de SE
      
      modelo <- inla(
        formula,
        data = dados_modelo,
        family = "poisson",
        control.predictor = list(compute = TRUE),
        control.compute = list(config = TRUE),
        control.fixed = list(
          prec.intercept = 0.001,  # Aumenta estabilidade
          prec = 1
        ),
        verbose = FALSE
      )
      
      # Prepara resultados
      nowcast_df <- dados_modelo %>%
        mutate(
          estimado = modelo$summary.fitted.values$mean,
          inf = modelo$summary.fitted.values$`0.025quant`,
          sup = modelo$summary.fitted.values$`0.975quant`
        ) %>%
        group_by(SE) %>%
        summarise(
          estimado = sum(estimado),
          inf = sum(inf),
          sup = sum(sup),
          observado = sum(n),
          .groups = "drop"
        ) %>%
        mutate(
          is_nowcast = SE %in% tail(sort(unique(SE)), 4)
        )
      
      # Gráfico
      p <- ggplot(nowcast_df, aes(x = SE)) +
        geom_line(aes(y = observado, color = "Notificados"), linewidth = 1) +
        geom_ribbon(
          data = filter(nowcast_df, is_nowcast),
          aes(ymin = inf, ymax = sup, fill = "IC 95%"),
          alpha = 0.3
        ) +
        geom_line(
          data = filter(nowcast_df, is_nowcast),
          aes(y = estimado, color = "Nowcasting"),
          linetype = "dashed", linewidth = 1
        ) +
        scale_color_manual(
          name = "",
          values = c("Notificados" = "#1f77b4", "Nowcasting" = "#ff7f0e")
        ) +
        scale_fill_manual(name = "", values = c("IC 95%" = "grey70")) +
        labs(
          title = paste("Nowcasting -", input$filtro_regiao),
          x = "Semana Epidemiológica",
          y = "Número de Casos"
        ) +
        theme_minimal()
      
      ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.1))
      
    }, error = function(e) {
      message("Erro detalhado: ", e$message)
      plotly_empty() %>% 
        layout(title = paste("Erro no modelo:", strwrap(e$message, width = 50) %>% paste(collapse = "<br>")))
    })
  })
  # Monitoramento: Rt
  output$graf_rt <- renderPlotly({
    sel_virus <- virus_sel_monitor()
    df_rt <- dados_monitorados() %>%
      select(ID_MN_RESI_pad, SE, all_of(sel_virus)) %>%
      filter(if_any(all_of(sel_virus), ~ . == 1)) %>%
      group_by(ID_MN_RESI_pad, SE) %>%
      summarise(casos = n(), .groups = "drop") %>%
      arrange(ID_MN_RESI_pad, SE)
    
    if (nrow(df_rt) < 5) {
      plotly_empty() %>% layout(title="Rt insuficiente")
    } else {
      rt_df <- purrr::map_dfr(unique(df_rt$ID_MN_RESI_pad), function(reg) {
        sub <- df_rt %>% filter(ID_MN_RESI_pad == reg)
        conf <- make_config(list(mean_si = 3, std_si = 1.5))
        est  <- estimate_R(incid = sub$casos, method = "parametric_si", config = conf)
        tibble(
          ID_MN_RESI_pad = reg,
          SE    = sub$SE[est$R$t_end],
          Rt    = est$R$`Mean(R)`,
          lower = est$R$`Quantile.0.025(R)`,
          upper = est$R$`Quantile.0.975(R)`
        )
      })
      
      # último Rt de cada região, para o rótulo
      labels <- rt_df %>%
        group_by(ID_MN_RESI_pad) %>%
        slice_tail(n = 1) %>%
        ungroup()
      
      p <- ggplot(rt_df, aes(x = SE, y = Rt)) +
        geom_line(color = "red") +
        geom_ribbon(aes(ymin = lower, ymax = upper), fill = "red", alpha = 0.2) +
        geom_hline(yintercept = 1, linetype = "dashed") +
        geom_text(
          data = labels,
          aes(label = sprintf("%.2f", Rt)),
          vjust = -0.5, size = 3
        ) +
        facet_wrap(~ ID_MN_RESI_pad) +
        labs(
          title = "Rt – Semana Epidemiológica",
          x     = "Semana",
          y     = "Rt"
        ) +
        theme_minimal()
      
      ggplotly(p)
    }
  })
  
  # Previsão
  output$graf_previsao <- renderPlotly({
    df <- dados_filtrados() %>% group_by(SE) %>% summarise(casos=n(), .groups="drop")
    df <- df %>% mutate(SE=as.integer(SE))
    if(nrow(df)>=10){
      ts_casos <- ts(df$casos, frequency=52)
      mod <- auto.arima(ts_casos)
      prev <- forecast(mod,h=4)
      pred_df <- data.frame(
        SE    =(max(df$SE)+1):(max(df$SE)+4),
        fit   = prev$mean,
        lower = prev$lower[,2],
        upper = prev$upper[,2]
      )
      p<-ggplot()+
        geom_line(data=df, aes(x=SE,y=casos), color="black")+
        geom_line(data=pred_df, aes(x=SE,y=fit), color="blue")+
        geom_ribbon(data=pred_df, aes(x=SE,ymin=lower,ymax=upper), fill="blue",alpha=0.2)+
        labs(title="Previsão – Próximas 4 Semanas", x="Semana", y="Casos")+theme_minimal()
      ggplotly(p)
    } else { plotly_empty() %>% layout(title="Previsão insuficiente") }
  })
  
  # Gráfico agregando todos os registros de PCR por tipo de vírus
  output$graf_pcr_totais <- renderPlotly({
    cols_pcr <- names(virus_map)
    df_tot <- dados_filtrados() %>%
      select(all_of(cols_pcr)) %>%
      pivot_longer(cols = everything(), names_to = "virus", values_to = "valor") %>%
      filter(valor == 1) %>%
      mutate(virus = virus_map[virus]) %>%
      count(virus)
    
    p <- ggplot(df_tot, aes(x = virus, y = n)) +
      geom_col() +
      labs(
        title = "Registros PCR por Tipo de Vírus",
        x = "Tipo de Vírus",
        y = "Número de Registros"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    ggplotly(p)
  })
  
  # Gráfico de registros PCR empilhados por Semana Epidemiológica
  output$graf_pcr_semana <- renderPlotly({
    cols_pcr <- names(virus_map)
    df_week <- dados_filtrados() %>%
      select(SE, all_of(cols_pcr)) %>%
      mutate(SE = as.integer(SE)) %>%
      pivot_longer(cols = -SE, names_to = "virus", values_to = "valor") %>%
      filter(valor == 1) %>%
      mutate(virus = virus_map[virus]) %>%
      group_by(SE, virus) %>%
      summarise(casos = n(), .groups = "drop")
    
    p <- ggplot(df_week, aes(x = SE, y = casos, fill = virus)) +
      geom_col() +
      labs(
        title = "Registros PCR por Semana Epidemiológica",
        x = "Semana Epidemiológica",
        y = "Número de Registros",
        fill = "Tipo de Vírus"
      ) +
      theme_minimal()
    ggplotly(p)
  })
  
}

# Executar o app
shinyApp(ui = ui, server = server)
