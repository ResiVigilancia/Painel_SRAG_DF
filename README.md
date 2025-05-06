# Análise Epidemiológica SRAG

Este repositório contém um relatório em R Markdown para análise epidemiológica de Síndromes Respiratórias Agudas Graves (SRAG) na Região Leste. O relatório gera tanto um documento Word quanto um HTML interativo com tabelas paginadas e gráficos.

## Autor

Residentes de vigilância em saúde da Unb

## Data

02/05/2025

## Descrição

O arquivo principal é um R Markdown que:

* Instala e carrega pacotes necessários de CRAN e repositórios específicos.
* Lê dados de casos de SRAG e população de planilhas Excel.
* Padroniza nomes de variáveis e regiões.
* Gera resumos estatísticos (notificações, óbitos, letalidade).
* Cria gráficos de séries temporais, proporção de vírus detectados, hospitalizações, faixas etárias e uso de ventilação.
* Aplica métodos de nowcasting para correção de atrasos de notificação e modelagem por Região Administrativa (RA).
* Constrói diagramas de controle de incidência por faixa de risco.
* Calcula a taxa de transmissibilidade viral (Rt) para diferentes vírus e regiões.

O relatório é parametrizado para facilitar a escolha de anos de análise e referência epidemiológica.

## Estrutura de Arquivos

* `análise_epidemiológica_srag.Rmd`: R Markdown contendo todo o código e narrativa.
* Pastas de dados:

  * `Casos_SRAG_*.xlsx`: planilhas de casos por semana.
  * `POP.xlsx`: planilha com dados populacionais por RA.

## Requisitos

* **R** (versão >= 4.0)
* **Pacotes CRAN**:

  * tidyverse, lubridate, plotly, forecast, EpiEstim, DT, officer, rvg, scales, readxl, purrr, mem, zoo, stringi, stringr
* **Pacotes de repositório**:

  * fmesher (via INLA)
* **Ferramentas**:

  * Rtools (Windows)

## Instalação de Pacotes

No início do R Markdown há um chunk para instalar pacotes:

```r
source("análise_epidemiológica_srag.Rmd", echo = FALSE)
```

Ou manualmente:

```r
# Instalar pacotes CRAN
cran_packages <- c(
  "tidyverse", "lubridate", "plotly", "forecast", "EpiEstim", "DT",
  "officer", "rvg", "scales", "readxl", "purrr", "mem", "zoo", "stringi",
  "stringr"
)
install.packages(setdiff(cran_packages, installed.packages()[,"Package"]))

# Instalar fmesher
install.packages("fmesher", repos = "https://inla.r-inla-download.org/R/stable")
```

## Uso

Renderize o relatório para gerar saída Word e HTML:

```r
library(rmarkdown)
rmarkdown::render(
  "análise_epidemiológica_srag.Rmd",
  output_format = "all"
)
```

O HTML gerado possui menus de navegação com índice de conteúdo e tabelas paginadas.

## Parâmetros

* `filtro_ano`: vetor de anos para os gráficos de séries temporais (ex: `c(2024, 2025)`).
* `ano_epidemico`: vetor de anos a excluir do histórico de controle (ex: `c(2021)`).
* `ano_ref`: ano de referência para resumo e gráficos detalhados (ex: `2025`).

Modifique esses valores diretamente na seção `params:` no início do Rmd.

## Seções Principais

1. **Instalação e Carregamento de Pacotes**
2. **Leitura e Processamento de Dados**
3. **Visão Geral da Situação Epidemiológica**
4. **Casos por Semana e por Ano**
5. **Detecção de Vírus (PCR)**
6. **Proporção e Distribuição Semanal de Vírus**
7. **Nowcasting e Correção de Atrasos**
8. **Hospitalizações e Ventilação**
9. **Análise por Faixa Etária e Tipo de Vírus**
10. **Diagrama de Controle de Incidência**
11. **Cálculo de Rt (Taxa de Transmissibilidade)**

## Contato

Para dúvidas ou sugestões, entre em contato: [rvs.unb@gmail.com](mailto:rvs.unb@gmail.com)
