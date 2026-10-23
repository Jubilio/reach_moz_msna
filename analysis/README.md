# Módulo de Análise Integrada MSNA 2026 Moçambique
### Educação (EduAnalysisTool) & Índice Multissetorial de Necessidades (Humind MSNI)

Este módulo implementa a análise integrada da avaliação **MSNA 2026 de Moçambique**, combinando duas metodologias padronizadas da **IMPACT Initiatives / HPPU**:
1. **[EduAnalysisTool (martivit/IMPACT)](https://github.com/martivit/EduAnalysisTool)**: Framework para análise setorial de educação no nível individual e familiar.
2. **[Humind MSNI Workflow](https://impact-initiatives-hppu.github.io/humind/articles/msni-workflow.html)**: Framework padronizado para cálculo dos compósitos setoriais (*Living Standards Gaps* - LSG) e do Índice Multissetorial de Necessidades (*Multi-Sector Needs Index* - MSNI).

---

## 📁 Estrutura do Módulo

```
analysis/
├── config/
│   ├── config.R                  # Configurações do projeto, limiares e desagregações
│   ├── moz_isced_mapping.csv     # Mapeamento do Sistema Nacional de Educação (SNE) de Moçambique
│   └── lcsi_strategies.csv       # Mapeamento das 11 estratégias de subsistência (LCSI)
├── R/
│   ├── 00_utils.R                # Utilitários de carregamento automático e tratamento de dados
│   ├── 01_edu_indicators.R       # Motor de cálculo de Educação (EduAnalysisTool & Humind)
│   ├── 02_msni_indicators.R      # Motor de cálculo dos Compósitos Setoriais e MSNI (Humind)
│   ├── 03_analysis_engine.R      # Motor de agregação estatística com desagregações
│   └── 04_export_outputs.R       # Exportador para livro Excel (.xlsx) formatado e RDS
├── run_analysis.R                # Script principal de execução (Entry point)
├── output/                       # Diretório onde os resultados são salvos (ignorado no Git)
└── README.md                     # Documentação técnica do módulo
```

---

## 🧠 Indicadores e Metodologias Implementadas

### 1. Educação (EduAnalysisTool)
A análise é realizada no nível individual (crianças de 5 a 17 anos da folha `edu_ind`) e agregada para o domicílio:
- **Categorias de Idade Escolar**:
  - `5 anos`: Pré-Escolar (ECE) / Idade de entrada na 1ª classe
  - `6 a 11 anos`: Ensino Primário (1ª a 6ª classe)
  - `12 a 15 anos`: Ensino Secundário 1º Ciclo (7ª a 10ª classe)
  - `16 a 17 anos`: Ensino Secundário 2º Ciclo (11ª e 12ª classe)
- **Acesso e Crianças Fora da Escola (OOS)**:
  - Taxa de frequência escolar no ano letivo corrente.
  - Taxa de crianças fora da escola (*Out-of-School Children* - OOS/OSC).
- **Adequação Idade-Série (Sobre-Idade / Over-age)**:
  - Crianças matriculadas que têm 2 ou mais anos acima da idade prevista para a classe em que estão matriculadas, de acordo com o mapeamento curricular oficial de Moçambique (`moz_isced_mapping.csv`).
- **Interrupção Escolar (Disruption Events)**:
  - Desastres naturais (ciclones, cheias, secas).
  - Ausência de professores.
  - Escolas ocupadas como abrigo temporário para deslocados.
  - Conflito ou ataques diretos à educação.
- **Barreiras ao Acesso Escolar**:
  - Custos financeiros (propinas, uniformes, transporte, material escolar).
  - Distância excessiva até ao estabelecimento de ensino.
  - Insegurança / proteção no trajeto.
  - Casamento precoce e gravidez na adolescência.
  - Falta de certidão de nascimento / documentação civil.
  - Deficiência ou condição de saúde.
  - Encerramento da escola.

### 2. Compósitos Setoriais (Living Standards Gaps - LSG) & MSNI (Humind)
Cada setor recebe uma pontuação de severidade de 1 a 4+ (5):
- **Segurança Alimentar (`comp_foodsec_score`)**:
  - Combinação do *Food Consumption Score* (FCS), *reduced Coping Strategies Index* (rCSI), *Household Hunger Scale* (HHS) e *Livelihood Coping Strategies Index* (LCSI) via matriz FCM/FCLCM.
- **WASH (`comp_wash_score`)**:
  - Classificação pela escada JMP da OMS/UNICEF para fonte de água potável (Básica, Limitada, Não Melhorada, Água de Superfície) combinada com tempo de recolha, quantidade de água (<15 L/pessoa/dia) e instalações de saneamento.
- **Educação (`comp_edu_score`)**:
  - Presença de crianças fora da escola, interrupções graves e barreiras de proteção.
- **Saúde (`comp_health_score`)**:
  - Necessidade de cuidados de saúde não satisfeita (*unmet healthcare needs*) no agregado.
- **Abrigo e Artigos Não-Alimentares (`comp_snfi_score`)**:
  - Danos severos ou destruição do abrigo, abrigos improvisados (*makeshift*), tendas e centros coletivos.
- **Proteção (`comp_prot_score`)**:
  - Crianças separadas acolhidas ou ausentes, trabalho infantil e graves preocupações de violência/discriminação.
- **Índice Multissetorial de Necessidades (`msni_score`)**:
  - $$MSNI = \max(LSG_{\text{foodsec}}, LSG_{\text{wash}}, LSG_{\text{edu}}, LSG_{\text{health}}, LSG_{\text{snfi}}, LSG_{\text{prot}})$$
  - **Em Necessidade (*In Need*)**: $MSNI \ge 3$
  - **Em Necessidade Severa (*Severe Need*)**: $MSNI \ge 4$
  - **Co-ocorrência de Necessidades**: Número de setores em que o domicílio atinge severidade $\ge 3$.

---

## 📊 Planos de Desagregação Suportados

Todas as tabelas de resumo são geradas automaticamente para:
1. **Total Nacional** (`National`)
2. **Nível Provincial** (`admin1` — 11 províncias de Moçambique: MZ01 a MZ11)
3. **Nível Distrital** (`admin2`)
4. **Estatuto de População / Deslocamento** (`population_group` — Deslocados internos / População acolhedora)
5. **Género do Chefe do Domicílio** (`hoh_gender`)
6. **Desagregações de Educação**: Cruzamento adicional por **Género da Criança** e **Categoria de Idade Escolar**.

---

## 🚀 Como Executar

### Pré-requisito
Certifique-se de que os dados limpos foram gerados:
```r
source("R-scripts/moz/run.R")
```

### Execução via RStudio
1. Abra o arquivo [analysis/run_analysis.R](file:///c:/Users/IMPACT%20-%20DBO/ACTED(1)/IMPACT%20MOZ%20-%20Documentos/General/2_Research/7_MSNA_2026/2_Data/2_Data_cleaning/MSNA_cleaning/analysis/run_analysis.R).
2. Clique no botão **Source** (ou pressione `Ctrl + Shift + Enter`).

### Execução via Terminal / PowerShell
```powershell
& "C:\Program Files\R\R-4.6.0\bin\Rscript.exe" "analysis\run_analysis.R"
```

---

## 📈 Saídas Geradas

Os arquivos são gerados na pasta `analysis/output/`:
- **Livro Excel (.xlsx)**: `MSNA_2026_MOZ_Analysis_Results_<timestamp>.xlsx`
  - Contém abas formatadas para cada plano de desagregação (Nacional, Província, Distrito, Deslocamento, Género, Fases do MSNI, e tabelas de Educação).
- **Pacote RDS (.rds)**: `MSNA_2026_MOZ_Analyzed_Data_<timestamp>.rds`
  - Contém as bases completas enriquecidas com todos os novos indicadores calculados e tabelas prontas para relatórios ou visualização GIS.
