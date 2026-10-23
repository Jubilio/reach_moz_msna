# MSNA 2026 Moçambique — Limpeza de Dados, Plausibilidade e Análise Integrada

Este repositório foi adaptado para a avaliação **MSNA 2026 de Moçambique (REACH)**. Todas as referências, dados e arquivos legados da missão de Uganda foram removidos, estabelecendo um pipeline independente de limpeza de dados, geração de relatórios de plausibilidade e análise estatística multissetorial.

---

## 📌 Sumário dos Módulos do Repositório

### 1. Limpeza de Dados (`R-scripts/moz/run.R`)
- **Entradas**:
  - Dados brutos: `inputs/REACH_MOZ_Data_MSNA_01102026.xlsx`
  - Formulário Kobo: `inputs/kobo-used.xlsx`
- **Funcionalidades**:
  - Importação da folha principal (`Submissions`) e folhas repetidas (`roster`, `edu_ind`, `health_ind`, `health_wra`, etc.).
  - Mapeamento multilíngue (Português/Inglês) e normalização de escolhas múltiplas para códigos XLSForm.
  - Verificação de duplicados, vínculos de agregados (UUIDs), tipos numéricos e sentinelas.
  - Geração de saídas versionadas em `outputs/moz/<timestamp>/` (`Submissions.csv`, `raw_snapshot.rds`, `candidate_data.rds`, `review_log.xlsx`).

---

### 2. Módulo de Plausibilidade FSL (`PH_plausibility_reports-develop`)
- **Script**: `PH_plausibility_reports-develop/fsl_quality_report/standalone_fsl_quality_html_report.R`
- **Funcionalidades**:
  - Consome diretamente a saída limpa mais recente de Moçambique (`outputs/moz/<latest_run>/Submissions.csv`).
  - Mapeamento padronizado para cálculo de indicadores de segurança alimentar (FCS, rCSI, HHS, LCSI).
  - Gera relatórios HTML de plausibilidade e qualidade para todas as **11 províncias de Moçambique** (`MZ01` a `MZ11`) em `PH_plausibility_reports-develop/fsl_quality_report/reports/`.

---

### 3. Módulo de Análise Integrada: Educação & MSNI (`analysis/`)
Construído com base nas ferramentas e orientações padronizadas da **IMPACT Initiatives / HPPU**:
- **[EduAnalysisTool (martivit/IMPACT)](https://github.com/martivit/EduAnalysisTool)**:
  - Categorização etária oficial do Sistema Nacional de Educação (SNE) de Moçambique (Pré-Escolar 5 anos, Primário 6-11 anos, Secundário 1º Ciclo 12-15 anos, Secundário 2º Ciclo 16-17 anos).
  - Taxa de crianças fora da escola (*Out-of-School Children* - OOS/OSC).
  - Taxa de sobre-idade escolar ($\ge 2$ anos acima da idade prevista para a classe).
  - Interrupções escolares (desastres naturais, ausência de professores, abrigos para deslocados, ataques).
  - Barreiras ao acesso escolar (financeiras, distância, proteção/segurança no trajeto, etc.).
- **[Humind MSNI Workflow](https://impact-initiatives-hppu.github.io/humind/articles/msni-workflow.html)**:
  - Compósitos setoriais (*Living Standards Gaps* - LSG, severidade 1 a 4+):
    - Segurança Alimentar (`comp_foodsec_score`)
    - Água, Saneamento e Higiene (`comp_wash_score`)
    - Educação (`comp_edu_score`)
    - Saúde (`comp_health_score`)
    - Abrigo e Artigos Não-Alimentares (`comp_snfi_score`)
    - Proteção (`comp_prot_score`)
  - Índice Multissetorial de Necessidades ($MSNI = \max(LSG)$).
  - Agregados familiares em Necessidade ($MSNI \ge 3$) e Necessidade Severa ($MSNI \ge 4$).
  - Co-ocorrência multidimensional de necessidades setoriais.
- **Planos de Desagregação Suportados**:
  - Total Nacional (`National`)
  - Província (`admin1`)
  - Distrito (`admin2`)
  - Estatuto de População/Deslocamento (`population_group`)
  - Género do Chefe do Agregado (`hoh_gender`)
  - Cruzamentos específicos de Educação por Género da Criança e Categoria de Idade Escolar.

---

## 🚀 Como Executar

### Passo 1: Executar a Limpeza de Dados de Moçambique
```r
source("R-scripts/moz/run.R")
```

### Passo 2: Gerar os Relatórios de Plausibilidade FSL
Abra `PH_plausibility_reports-develop/fsl_quality_report/standalone_fsl_quality_html_report.R` no RStudio e clique em **Source**, ou via terminal:
```powershell
cd PH_plausibility_reports-develop
& "C:\Program Files\R\R-4.6.0\bin\Rscript.exe" "fsl_quality_report\standalone_fsl_quality_html_report.R"
cd ..
```

### Passo 3: Executar a Análise Integrada (Educação & MSNI)
Abra `analysis/run_analysis.R` no RStudio e clique em **Source**, ou via terminal:
```powershell
& "C:\Program Files\R\R-4.6.0\bin\Rscript.exe" "analysis\run_analysis.R"
```
Os resultados serão exportados em `analysis/output/`:
- **Livro Excel (.xlsx)**: `MSNA_2026_MOZ_Analysis_Results_<timestamp>.xlsx` com abas para cada plano de desagregação.
- **Dataset RDS (.rds)**: `MSNA_2026_MOZ_Analyzed_Data_<timestamp>.rds` com dados enriquecidos com todos os indicadores calculados.

---

## 🔒 Boas Práticas e Segurança dos Dados

- Os arquivos de dados brutos (`inputs/*.xlsx`), os snapshots (`outputs/moz/`), logs detalhados, relatórios HTML (`reports/`) e tabelas exportadas de análise (`analysis/output/`) contêm dados de pesquisa e estão estritamente configurados no `.gitignore`.
- **Nunca envie dados brutos ou identificáveis (PII) para o repositório público.**
