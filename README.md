# MSNA 2026 Moçambique — Limpeza de Dados e Relatórios de Plausibilidade

Este repositório foi adaptado para a avaliação **MSNA 2026 de Moçambique (REACH)**. Todas as referências, dados e arquivos legados da missão de Uganda foram removidos, estabelecendo um pipeline independente de limpeza de dados e geração de relatórios de plausibilidade em saúde pública e segurança alimentar (FSL).

---

## 📌 Sumário das Alterações e Mudanças Realizadas

### 1. Limpeza e Adaptação para Moçambique
- **Remoção de Legados de Uganda**: Exclusão de arquivos de dados brutos (`REACH_MSNA2026_uganda_*.xlsx`), pastas de saída de Uganda e limpeza de referências nos scripts.
- **Estrutura de Entrada de Moçambique**:
  - Dados brutos: `inputs/REACH_MOZ_Data_MSNA_01102026.xlsx`
  - Formulário Kobo: `inputs/kobo-used.xlsx`
- **Pipeline Principal de Limpeza (`R-scripts/moz/run.R`)**:
  - Importação da folha principal (`Submissions`) e das folhas repetidas configuradas (`roster`, etc.).
  - Mapeamento robusto de cabeçalhos e rótulos de perguntas (Português/Inglês).
  - Normalização de respostas de escolha múltipla para códigos XLSForm.
  - Verificação de duplicados, vínculos de agregados (UUIDs), tipos numéricos e sentinelas.
  - Geração de saídas versionadas por data/hora em `outputs/moz/<timestamp>/`.

---

### 2. Adaptação do Módulo de Plausibilidade FSL (`PH_plausibility_reports-develop`)

O subprojeto `PH_plausibility_reports-develop` (baseado no pacote `impactR4PHU`) foi configurado e customizado para operar com os dados do MSNA Moçambique através do script:
`PH_plausibility_reports-develop/fsl_quality_report/standalone_fsl_quality_html_report.R`

#### Principais Mudanças Técnicas:
1. **Consumo Direto dos Dados Processados**:
   - Em vez de reprocessar o Excel bruto de >120MB, o script agora lê automaticamente a execução mais recente gerada pelo pipeline de Moçambique (`outputs/moz/<latest_run>/Submissions.csv` e `raw_snapshot.rds`).
2. **Resgate das Frequências do HHS**:
   - As perguntas de frequência do indicador de fome (*Household Hunger Scale* — HHS: `m2755`, `m2757`, `m2759`) são extraídas diretamente do snapshot bruto para permitir o cálculo correto pelo `impactR4PHU::add_hhs()`.
3. **Mapeamento e Padronização de Colunas FSL**:
   - Variáveis específicas de Moçambique foram mapeadas para os nomes canônicos esperados internamente pelo modelo RMarkdown e pela função `create_fsl_plaus()`:
     - **FCS (Food Consumption Score)**: `m3269_hh_fcs_staples` $\to$ `fsl_fcs_cereal`, `m3270_hh_fcs_pulses` $\to$ `fsl_fcs_legumes`, `m3273_hh_fcs_veg` $\to$ `fsl_fcs_veg`, `m3271_hh_fcs_dairy` $\to$ `fsl_fcs_dairy`, `m3274_hh_fcs_fruits` $\to$ `fsl_fcs_fruit`, `m3272_hh_fcs_protein` $\to$ `fsl_fcs_meat`, `m3276_hh_fcs_sugar` $\to$ `fsl_fcs_sugar`, `m3275_hh_fcs_fat` $\to$ `fsl_fcs_oil`.
     - **rCSI (reduced Coping Strategies)**: `fsl_rcsi_rely_on_less_expensive_food` $\to$ `fsl_rcsi_lessquality`.
     - **HHS**: `m2754_hh_hhs_no_food` $\to$ `fsl_hhs_nofoodhh`, etc.
4. **Recodificação do LCSI (Livelihood Coping Strategies)**:
   - Os códigos numéricos do questionário de Moçambique (`l_fsl_lcsi`) foram convertidos para as categorias textuais exigidas pelo `impactR4PHU::add_lcsi()`:
     - `30` $\to$ `"yes"`
     - `10` $\to$ `"no_had_no_need"`
     - `20` $\to$ `"no_exhausted"`
     - `9999` $\to$ `"not_applicable"`
   - As estratégias avaliadas em Moçambique foram mapeadas nos slots correspondentes (`stress1`, `stress2`, `crisis1`) e as demais estratégias não coletadas foram marcadas como `"not_applicable"`.
5. **Iteração por Província (P-Codes)**:
   - O loop de relatórios foi ajustado para iterar sobre os P-codes de província de Moçambique (`MZ01` a `MZ11`) com agregação por distrito (`admin2`), gerando um relatório HTML individual para cada província.
6. **Suporte de Renderização Pandoc**:
   - Adicionada detecção automática do Pandoc embutido no RStudio/Quarto, permitindo rodar o script tanto via RStudio quanto via terminal/linha de comando (Rscript).

---

## 🚀 Como Executar

### Passo 1: Executar o Pipeline de Limpeza de Moçambique

1. Abra o projeto no RStudio ou no terminal na pasta raiz.
2. Certifique-se de que os pacotes básicos estão instalados:
   ```r
   install.packages(c("readxl", "openxlsx", "dplyr", "fs"))
   ```
3. Execute o script de limpeza:
   ```r
   source("R-scripts/moz/run.R")
   ```
   *Alternativa via PowerShell / Terminal:*
   ```powershell
   Rscript R-scripts/moz/run.R
   ```
4. As saídas serão salvas em `outputs/moz/<YYYY-MM-DD_HHMMSS>/`:
   - `Submissions.csv`: base principal limpa/mapeada.
   - `review_log.xlsx`: log com anomalias e registros sinalizados.
   - `raw_snapshot.rds`: snapshot dos dados brutos.
   - `corrections_template.csv`: modelo para aplicação de correções aprovadas.

---

### Passo 2: Gerar os Relatórios de Plausibilidade FSL

1. Abra o arquivo do relatório:
   `PH_plausibility_reports-develop/fsl_quality_report/standalone_fsl_quality_html_report.R`
2. No RStudio, clique no botão **Source** (ou pressione `Ctrl + Shift + Enter`).
   
   *Alternativa via PowerShell / Terminal:*
   ```powershell
   cd PH_plausibility_reports-develop
   & "C:\Program Files\R\R-4.6.0\bin\Rscript.exe" "fsl_quality_report/standalone_fsl_quality_html_report.R"
   ```

3. Os relatórios HTML interativos serão gerados automaticamente na pasta:
   `PH_plausibility_reports-develop/fsl_quality_report/reports/`

   - `fsl_plaus_report_MZ01_<data>.html` (Cabo Delgado)
   - `fsl_plaus_report_MZ02_<data>.html` (Gaza)
   - `fsl_plaus_report_MZ03_<data>.html` (Inhambane)
   - `fsl_plaus_report_MZ04_<data>.html` (Manica)
   - `fsl_plaus_report_MZ05_<data>.html` (Maputo)
   - `fsl_plaus_report_MZ06_<data>.html` (Cidade de Maputo)
   - `fsl_plaus_report_MZ07_<data>.html` (Nampula)
   - `fsl_plaus_report_MZ08_<data>.html` (Niassa)
   - `fsl_plaus_report_MZ09_<data>.html` (Sofala)
   - `fsl_plaus_report_MZ10_<data>.html` (Tete)
   - `fsl_plaus_report_MZ11_<data>.html` (Zambézia)

---

## 🔒 Boas Práticas e Segurança dos Dados

- Os arquivos de dados brutos (`inputs/*.xlsx`), os snapshots (`outputs/moz/`), logs detalhados e relatórios HTML gerados (`reports/`) contêm dados de pesquisa e estão configurados no `.gitignore`. **Nunca envie dados brutos ou identificáveis (PII) para o repositório público.**
- Caso seja necessário aplicar correções aprovadas pelo time de campo, preencha o arquivo `inputs/feedback/moz_corrections.csv` com base no `corrections_template.csv` e execute novamente `R-scripts/moz/run.R`.
