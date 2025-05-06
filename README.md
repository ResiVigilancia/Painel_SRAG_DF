# Painel SRAG – Análise Epidemiológica (Shiny)

Este repositório contém um aplicativo **Shiny** para visualização interativa e análise epidemiológica de Síndromes Respiratórias Agudas Graves (SRAG) na Região Leste.

---

## Conteúdo

* `app.R`       – Código fonte do Shiny (UI + server).
* `global.R`    – (Opcional) arquivo para objetos globais, se separado.
* `data/`       – Pasta com:

  * `Casos_SRAG_*.xlsx` – arquivos de dados de casos de SRAG por semana.
  * `POP.xlsx`          – dados populacionais por Região Administrativa (RA).

---

## Requisitos

* **R** (>= 4.0)
* **Pacotes CRAN**:

  * shiny, shinydashboard, shinyWidgets
  * tidyverse, lubridate, plotly, forecast, zoo
  * EpiEstim, DT, officer, rvg, scales, readxl
  * purrr, mem, devtools
* **Pacotes especiais**:

  * INLA (via `repos = "https://inla.r-inla-download.org/R/stable"`)
  * fmesher
* **Ferramentas**:

  * Rtools (Windows) para compilar pacotes de fonte

---

## Instalação de Dependências

No topo de `app.R` há um chunk que verifica e instala automaticamente todos os pacotes necessários:

```r
# Lista de pacotes CRAN
cran_pkgs <- c(
  "shiny", "shinydashboard", "tidyverse", "lubridate", "plotly", "forecast",
  "EpiEstim", "DT", "officer", "rvg", "scales", "readxl",
  "purrr", "mem", "zoo", "fmesher"
)

# Instala pacotes faltantes
install.packages(setdiff(cran_pkgs, installed.packages()[, "Package"]))

# Instala remotes (para GitHub) e INLA
if (!requireNamespace("remotes", quietly=TRUE)) install.packages("remotes")
install.packages("INLA", repos = c(INLA = "https://inla.r-inla-download.org/R/stable"), dependencies = TRUE)

# Verifica Rtools no Windows
if (tolower(.Platform$OS.type) == "windows" && !pkgbuild::has_build_tools()) {
  warning("Instale Rtools 4.4: https://cran.r-project.org/bin/windows/Rtools/")
}

# Carrega todas as bibliotecas em sequência
i <- c(cran_pkgs, "remotes", "INLA")
lapply(i, function(p) suppressPackageStartupMessages(library(p, character.only=TRUE)))
```

Execute essa parte apenas uma vez para garantir que todas as dependências estejam instaladas.

---

## Organização do Código

1. **Carregamento de Dados**

   * `carregar_srag()` encontra e lê o arquivo SRAG mais recente na pasta de dados.
   * Planilha de população (`POP.xlsx`) é lida e padronizada.

2. **Mapeamento de Vírus**

   * Detecta quais vírus estão presentes (colunas `PCR_*`).
   * Cria `virus_map` apenas com esses códigos e um rótulo legível.

3. **Interface do Usuário (UI)**

   * `dashboardHeader`, `dashboardSidebar` e `dashboardBody`.
   * Filtros: intervalo de anos, tipos de vírus (com `pickerInput`), Regiões Administrativas.
   * Quatro abas principais: Visão Geral, Monitoramento, Previsão e Resultados PCR.

4. **Lógica do Servidor**

   * **Reativos**:

     * `dados_filtrados()` para anos.
     * `virus_sel_monitor()` para vírus selecionados.
     * `dados_monitorados()` aplica filtro de vírus.
   * **ValueBoxes** na aba Visão Geral (total de casos, média ± DP, total de óbitos).
   * **Gráficos** com `renderPlotly` e `plotly_empty` em caso de dados insuficientes:

     * Casos e óbitos por semana, faixas etárias, ventilação, internação, evolução e hospitalizações.
   * **Diagrama de Controle** na aba Monitoramento (limiares históricos + incidência atual).
   * **Nowcasting** (correção de atrasos) e **Rt** (taxa de transmissibilidade) com `INLA` e `EpiEstim`.
   * **Previsão** (4 semanas) via `forecast::auto.arima`.
   * **Resultados PCR**: totais e empilhados por semana.

---

## Configuração

* Edite o caminho da pasta de dados em:

  ```r
  pasta_dados <- "CAMINHO DA PASTA PARA O BANCO DE DADOS DO SIVEP-GRIPE"
  ```

* Ajuste níveis de idade (`niveis_idade`) ou opções de filtro conforme necessidade.

---

## Executando o App

No console do R, a partir da pasta raiz do projeto, execute:

```r
library(shiny)
runApp("app.R")
```

Ou simplesmente abra `app.R` no **RStudio** e clique em **Run App**.

---

## Extensões e Personalizações

* **UI**: adicione novos filtros ou altere layouts (`fluidRow`, `box`).
* **Server**: incorpore novas análises (por exemplo, modelagem adicional ou relatórios).
* **Temas**: customize cores e estilos no `theme_minimal()` ou use `bslib` para temas Bootstrap.

---

## Contato

Para dúvidas ou contribuições, abra uma *issue* ou contate:

Residentes de vigilância em saúde da Unb — [rvs.unb@gmail.com](mailto:rvs.unb@gmail.com)
