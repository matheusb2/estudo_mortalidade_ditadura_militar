#==============================================================================
# PACOTES
#==============================================================================

library(readr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(ipeaplot)
library(readxl)
library(strucchange)
library(broom)
library(patchwork)
library(broom)
library(dplyr)


dados_1960_2022 <- read_csv("~/Documentos/IPEA/modelos/reconstrução homicídios/reconstrucao_homicidios_desde_1960/serie_historica_1960_em_diante.csv")
View(serie_historica_1960_em_diante)

salvar_reconstrucao <- "/home/matheus/Documentos/IPEA/modelos/reconstrução homicídios/reconstrucao_homicidios_desde_1960"


#================================================================
#Integrar com demais dados
#================================================================
dados_para_join <- dados_1960_2022 |>
  dplyr::select(
    ano,
    excesso_homicidios_cf1,
    excesso_homicidios_cf2,
    excesso_homicidios_cf3,
    excesso_homicidios_cfex,
    excesso_mce_cf1,
    excesso_mce_cf2,
    excesso_mce_cf3,
    excesso_mce_cfex,
    tx_homicidios,
    tx_mce,
    tx_mortes_violentas
  )

saveRDS(
  dados_para_join,
  file = file.path(
    salvar_reconstrucao,
    "serie_historica_homicidios_para_join.rds"
  )
)

# ==============================================================================
# 1. DADOS DA VALIDAÇÃO (CENSOS 1950-2022 E ASSASSINADOS POLÍTICOS DOCUMENTADOS DE 1964-1985)
# ==============================================================================

# Dados do Censo, mortalidade (obs 1979-2022, imputado 1960-1978), acidentes de trabalho)
#altere o endereço para onde o arquivo estiver
#pacotes necessários

library(dplyr)
library(tidyverse)
library(ggplot2)
library(ipeaplot)
library(readxl)
library(strucchange)
library(broom)
library(patchwork)


#arquivos

#ATENÇÃO >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#RENOMEIE O ENDEREÇO ABAIXO COM A LOCALIZAÇÃO DO ARQUIVO
arquivo_base_homicidios_ocultos <- "/home/matheus/Documentos/IPEA/modelos/reconstrução homicídios/dados_mortes_1979_2022.xlsx"

library(readxl)
dados_mortes <- read_excel("~/Documentos/IPEA/modelos/reconstrução homicídios/dados_mortes_1979_2022.xlsx", 
                           sheet = "Planilha1", col_types = c("numeric", "text", "text", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric"))

#ATENÇÃO >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#RENOMEIE O ENDEREÇO ABAIXO COM A PASTA ONDE QUER SALVAR OS RESULTADOS
salvar_reconstrucao <- "/home/matheus/Documentos/IPEA/modelos/reconstrução homicídios/reconstrucao_homicidios_desde_1960"

View(dados_mortes)

dados_mortes <- subset(dados_mortes, !is.na(dados_mortes$agressoes_e_confrontos))

dados_mortes$prop_hopmmd <- dados_mortes$agressoes_e_confrontos /(dados_mortes$MG - dados_mortes$MCMD)

dados_mortes$prop_hopmvii <- dados_mortes$agressoes_e_confrontos / (dados_mortes$MCE_total - dados_mortes$MVII)

dados_mortes$prop_MVII_ocultos_CMD <- dados_mortes$MVII/ ((dados_mortes$MG - dados_mortes$MCMD))


# ==============================================================================
# 1. DADOS
# ==============================================================================

# Dados do Censo, mortalidade (obs 1979-2022, imputado 1960-1978), acidentes de trabalho)
#altere o endereço para onde o arquivo estiver
arquivo <- "~/Documentos/IPEA/modelos/demografia social da ditadura militar/censos_mortes_bruto.xlsx"

df_raw <- read_excel(
  path = arquivo,
  sheet = "BR_1950_2022"
)

names(df_raw)

arquivo_join <- "/home/matheus/Documentos/IPEA/modelos/reconstrução homicídios/reconstrucao_homicidios_desde_1960/serie_historica_homicidios_para_join.rds"

dados_join <- readRDS(arquivo_join)

# Checar chave
stopifnot(
  !anyDuplicated(dados_join$ano),
  "ano" %in% names(df_raw)
)

# Aviso se houver anos fora do domínio
anos_fora <- setdiff(dados_join$ano, df_raw$ano)
if (length(anos_fora) > 0) {
  warning("Anos no join que não existem em df_raw: ", paste(anos_fora, collapse = ", "))
}

df_raw <- df_raw |>
  left_join(
    dados_join,
    by = "ano"
  )

# Verificar se as colunas críticas existem
vars_criticas <- c(
  "excesso_homicidios_cf1",
  "excesso_mce_cf1",
  "população"
)

stopifnot(all(vars_criticas %in% names(df_raw)))


#==================
#PASTAS
#==================
pasta_saida <- file.path("validação de homicídios")

dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# INTERPOLAÇÕES POR MÉTODO EXPONENCIAL MANTENDO O FORMATO LARGO (WIDE)
# ==============================================================================

# Identificar os anos censitários
anos_censo <- c(1950, 1960, 1970, 1980, 1991, 2000, 2010, 2022)

# Função para interpolação exponencial
interp_exponencial <- function(ano, pop, anos_censo) {
  stopifnot(length(ano) == length(pop))
  
  resultado <- pop
  
  for (i in seq_len(length(anos_censo) - 1)) {
    a0 <- anos_censo[i]
    a1 <- anos_censo[i + 1]
    
    idx0 <- match(a0, ano)
    idx1 <- match(a1, ano)
    
    if (is.na(idx0) || is.na(idx1)) {
      next
    }
    
    p0 <- pop[idx0]
    p1 <- pop[idx1]
    
    if (anyNA(c(p0, p1)) || p0 <= 0 || p1 <= 0) {
      next
    }
    
    r <- (log(p1) - log(p0)) / (a1 - a0)
    
    anos_inter <- ano > a0 & ano < a1
    resultado[anos_inter] <- p0 * exp(r * (ano[anos_inter] - a0))
  }
  
  resultado
}

# Selecionar colunas a serem interpoladas
cols_pop <- names(df_raw) |>
  str_subset("^(homens|mulheres)\\s") |>
  str_subset("contrafactual|déficit", negate = TRUE)

# Aplicar interpolação exponencial para cada grupo populacional
df_interp <- df_raw |>
  mutate(
    across(
      all_of(cols_pop),
      ~ interp_exponencial(
        ano = ano,
        pop = .x,
        anos_censo = anos_censo
      )
    )
  )

df_raw <- df_interp

# ==============================================================================
# IMPUTAÇÃO DA POPULAÇÃO TOTAL (SOMA DOS GRUPOS)
# ==============================================================================

# Identificar as colunas de grupos populacionais (homens e mulheres por faixa etária)
# Excluir colunas que não são grupos populacionais básicos
grupos_pop <- names(df_raw) |>
  str_subset("^(homens|mulheres)\\s\\d+ a \\d+ anos$")


# Verificar se encontramos as colunas
if (length(grupos_pop) == 0) {
  # Tenta um padrão alternativo mais simples para debug
  grupos_teste <- names(df_raw) |>
    str_subset("^(homens|mulheres)\\s")
  cat("Colunas encontradas começando com 'homens' ou 'mulheres':\n")
  print(grupos_teste)
  stop("O padrão regex não encontrou colunas. Verifique os nomes exatos acima.")
}

cat("Número de colunas de grupos populacionais encontradas:", length(grupos_pop), "\n")

# Imputar população total como soma dos grupos populacionais para anos com NA
df_raw <- df_raw %>%
  mutate(
    # Calcular a soma dos grupos para todas as linhas (para verificação)
    soma_grupos_calculada = rowSums(across(all_of(grupos_pop)), na.rm = TRUE),
    # Substituir NA na coluna 'população' pela soma calculada
    população = if_else(is.na(população), soma_grupos_calculada, população)
  ) %>%
  # Remover a coluna auxiliar
  select(-soma_grupos_calculada)

# Verificar resultado
cat("\nVerificação da imputação:\n")
cat("Total de NAs na coluna 'população' antes da imputação:", 
    sum(is.na(df_raw$população)), "\n")

# Mostrar anos onde população foi imputada
anos_imputados <- df_raw |>
  select(ano, população) |>
  filter(!is.na(população))

cat("\nPopulação imputada para os anos 1960-1978:\n")
print(anos_imputados)

# Verificar consistência (opcional: comparar com anos censitários)
cat("\nComparação com anos censitários (1950-2022):\n")
df_raw |>
  filter(ano %in% anos_censo) |>
  select(ano, população) |>
  print()

# ==============================================================================
# VERIFICAÇÃO DE CONSISTÊNCIA
# ==============================================================================

# Verificar se a população imputada é consistente com a soma dos grupos
df_raw <- df_raw |>
  mutate(
    # Calcular soma dos grupos novamente para verificação
    soma_grupos_verif = rowSums(across(all_of(grupos_pop)), na.rm = TRUE),
    
    # Calcular diferença percentual entre população e soma dos grupos
    # (apenas para anos não-censitários onde temos ambos os valores)
    diff_percent = ifelse(
      !ano %in% anos_censo & !is.na(população) & soma_grupos_verif > 0,
      (população - soma_grupos_verif) / soma_grupos_verif * 100,
      NA
    )
  )

# Resumo das diferenças
cat("\nResumo das diferenças entre população total e soma dos grupos (1960-1978):\n")
df_raw |>
  summarise(
    n_anos = n(),
    media_diff = mean(diff_percent, na.rm = TRUE),
    min_diff = min(diff_percent, na.rm = TRUE),
    max_diff = max(diff_percent, na.rm = TRUE)
  ) |>
  print()


# ==============================================================================
# ANÁLISE DE COORTES COM CONTRAFACTUAL BASEADO NA SOBREVIVÊNCIA 1950-1960
# ==============================================================================

# 1. ESTRUTURAÇÃO DOS DADOS EM FORMATO TIDY/LONGO
# ------------------------------------------------------------------------------
# Função para processar dados de um sexo específico
processar_dados_sexo <- function(df, prefixo, nome_sexo) {
  padrao_col <- paste0("^", prefixo, "\\s")
  
  df |>
    select(ano, matches(padrao_col)) |>
    select(ano, !matches("contrafactual|déficit")) |>
    pivot_longer(
      cols = -ano,
      names_to = "faixa_original",
      values_to = "populacao"
    ) |>
    mutate(
      sexo = nome_sexo,
      faixa_etaria = str_remove(faixa_original, paste0("^", prefixo, "\\s")),
      idade_central = case_when(
        faixa_etaria == "0 a 4 anos" ~ 2.5,
        faixa_etaria == "5 a 9 anos" ~ 7.5,
        faixa_etaria == "10 a 14 anos" ~ 12.5,
        faixa_etaria == "15 a 19 anos" ~ 17.5,
        faixa_etaria == "20 a 24 anos" ~ 22.5,
        faixa_etaria == "25 a 29 anos" ~ 27.5,
        faixa_etaria == "30 a 39 anos" ~ 34.5,
        faixa_etaria == "40 a 49 anos" ~ 44.5,
        faixa_etaria == "50 a 59 anos" ~ 54.5,
        faixa_etaria == "60 a 69 anos" ~ 64.5,
        faixa_etaria == "70 ou mais" ~ 75,
        TRUE ~ NA_real_
      ),
      coorte = ano - idade_central
    ) |>
    dplyr::select(ano, sexo, faixa_etaria, idade_central, coorte, populacao)
}

# Processar dados para homens e mulheres
dados_homens <- processar_dados_sexo(df_raw, "homens", "homens")
dados_mulheres <- processar_dados_sexo(df_raw, "mulheres", "mulheres")

# Combinar todos os dados
dados_long <- bind_rows(dados_homens, dados_mulheres)


#===========================================================================
# Análise da conexão déficit jovem específico masculino vs. mortes violentas
#===========================================================================

# Function to calculate survival probabilities for both sexes
calc_surv_prob_both_sexes <- function(df_long, year_start, year_end, age_start, age_end) {
  
  # Initialize list to store results
  surv_list <- list()
  
  for (sex in c("homens", "mulheres")) {
    
    # Get population at start and end for the cohort
    pop_start <- df_long %>%
      filter(sexo == sex,
             ano == year_start,
             idade_central >= age_start & idade_central <= age_end) %>%
      group_by(coorte) %>%
      summarise(pop_start = sum(populacao, na.rm = TRUE), .groups = 'drop')
    
    pop_end <- df_long %>%
      filter(sexo == sex,
             ano == year_end,
             idade_central >= (age_start + (year_end - year_start)) & 
               idade_central <= (age_end + (year_end - year_start))) %>%
      group_by(coorte) %>%
      summarise(pop_end = sum(populacao, na.rm = TRUE), .groups = 'drop')
    
    # Calculate survival rate (decennial)
    surv_df <- pop_start %>%
      inner_join(pop_end, by = "coorte") %>%
      mutate(
        surv_prob_decennial = pop_end / pop_start,
        # Annual survival rate (assuming constant)
        surv_prob_annual = surv_prob_decennial^(1/(year_end - year_start)),
        ref_period = paste(year_start, year_end, sep = "-"),
        sexo = sex
      ) %>%
      rename_with(~paste0(., "_", sex), c(pop_start, pop_end, surv_prob_decennial, surv_prob_annual))
    
    surv_list[[sex]] <- surv_df
  }
  
  # Combine both sexes
  surv_df_combined <- surv_list[["homens"]] %>%
    full_join(surv_list[["mulheres"]], by = c("coorte", "ref_period"))
  
  return(surv_df_combined)
}

# Apply to all census intervals from 1960 to 2022
census_years <- c(1960, 1970, 1980, 1991, 2000, 2010, 2022)
survival_list <- list()

for(i in 1:(length(census_years)-1)) {
  surv <- calc_surv_prob_both_sexes(dados_long, 
                                    census_years[i], 
                                    census_years[i+1], 
                                    age_start = 15, 
                                    age_end = 29)
  survival_list[[i]] <- surv
}

survival_ref <- bind_rows(survival_list)

# Create grids for both sexes
all_cohorts <- unique(dados_long$coorte)
all_years <- min(dados_long$ano):max(dados_long$ano)

# Function to create deficit grid for a specific sex
create_deficit_grid <- function(sex) {
  grid <- expand_grid(coorte = all_cohorts, ano = all_years) %>%
    mutate(idade_central = ano - coorte) %>%
    filter(idade_central >= 10, idade_central <= 29) %>%
    left_join(dados_long %>% 
                filter(sexo == sex) %>% 
                select(coorte, ano, pop_obs = populacao),
              by = c("coorte", "ano"))
  
  return(grid)
}

# Create grids for both sexes
deficit_grid_homens <- create_deficit_grid("homens")
deficit_grid_mulheres <- create_deficit_grid("mulheres")

# Project expected population using sex-specific survival probabilities
project_deficit <- function(deficit_grid, sex) {
  sex_col <- ifelse(sex == "homens", "homens", "mulheres")
  
  deficit_grid %>%
    arrange(coorte, ano) %>%
    group_by(coorte) %>%
    mutate(
      # Find the starting population for this cohort at age 15
      pop_start = first(na.omit(pop_obs)),
      # Determine the appropriate reference period based on year
      ref_period = case_when(
        ano >= 1960 & ano < 1970 ~ "1960-1970",
        ano >= 1970 & ano < 1980 ~ "1970-1980",
        ano >= 1980 & ano < 1991 ~ "1980-1991",
        ano >= 1991 & ano < 2000 ~ "1991-2000",
        ano >= 2000 & ano < 2010 ~ "2000-2010",
        ano >= 2010 ~ "2010-2022"
      )
    ) %>%
    left_join(survival_ref %>% 
                select(coorte, ref_period, 
                       surv_prob_annual = paste0("surv_prob_annual_", sex_col)),
              by = c("coorte", "ref_period")) %>%
    # Project expected population forward year by year
    mutate(
      surv_prob_annual = replace_na(surv_prob_annual, 1), # Default if missing
      pop_exp = cumprod(surv_prob_annual) * pop_start
    ) %>%
    # Calculate deficit
    mutate(
      deficit = pop_exp - pop_obs,
      deficit = if_else(is.na(deficit) | deficit < 0, 0, deficit) # Non-negative
    ) %>%
    ungroup() %>%
    mutate(sexo = sex)
}

# Calculate deficits for both sexes
deficit_homens <- project_deficit(deficit_grid_homens, "homens")
deficit_mulheres <- project_deficit(deficit_grid_mulheres, "mulheres")

# Combine and calculate specific male deficit
deficit_combined <- deficit_homens %>%
  rename(deficit_homens = deficit, pop_obs_homens = pop_obs, pop_exp_homens = pop_exp) %>%
  select(coorte, ano, idade_central, deficit_homens, pop_obs_homens, pop_exp_homens) %>%
  left_join(
    deficit_mulheres %>%
      rename(deficit_mulheres = deficit, pop_obs_mulheres = pop_obs, pop_exp_mulheres = pop_exp) %>%
      select(coorte, ano, idade_central, deficit_mulheres, pop_obs_mulheres, pop_exp_mulheres),
    by = c("coorte", "ano", "idade_central")
  ) %>%
  mutate(
    # Déficit específico masculino: déficit masculino ajustado pelo déficit feminino
    deficit_especifico_masculino = deficit_homens - deficit_mulheres,
    deficit_especifico_masculino = if_else(is.na(deficit_especifico_masculino) | 
                                             deficit_especifico_masculino < 0, 
                                           0, deficit_especifico_masculino)
  )

# Aggregate total annual specific male deficit across all cohorts
annual_deficit_total <- deficit_combined %>%
  group_by(ano) %>%
  summarise(
    total_deficit = sum(deficit_especifico_masculino, na.rm = TRUE),
    total_deficit_homens = sum(deficit_homens, na.rm = TRUE),
    total_deficit_mulheres = sum(deficit_mulheres, na.rm = TRUE),
    .groups = 'drop'
  )

# Merge with your homicide/violent death estimates
analysis_data <- annual_deficit_total %>%
  left_join(dados_1960_2022 %>% 
              select(ano, 
                     homicidios = homicidios_intencionais_rand_media, # Using your preferred estimate
                     mortes_violentas), # Your MCE + homicides series
            by = "ano") %>%
  mutate(across(c(homicidios, mortes_violentas), ~replace_na(., 0)))

# Calculate moving average for visualization
analysis_data$mmdeficit <- zoo::rollmean(analysis_data$total_deficit, k = 10, 
                                         fill = "extend", align = "center")

# Calculate moving averages for both sexes deficits as well (for comparison)
analysis_data$mmdeficit_homens <- zoo::rollmean(analysis_data$total_deficit_homens, k = 10, 
                                                fill = "extend", align = "center")
analysis_data$mmdeficit_mulheres <- zoo::rollmean(analysis_data$total_deficit_mulheres, k = 10, 
                                                  fill = "extend", align = "center")

#===========================================================================
# VISUALIZAÇÕES (MANTIDAS IGUAIS PARA CONSISTÊNCIA)
#===========================================================================

linha_tempo_deficit_masc <- ggplot(analysis_data, aes(x = ano))+
  geom_line(aes(y = mmdeficit))+
  labs(y = "deficit específico masculino jovem (pessoas-ano)")+
  theme_ipea() +
  scale_fill_ipea(palette = "Red-Blue-White") +
  scale_color_ipea(palette = "Red-Blue-White")

linha_tempo_deficit_masc

save_ipeaplot(linha_tempo_deficit_masc,
              "deficit linha do tempo",
              path = pasta_saida,
              width = 10,
              height = 7,
              format = c("eps", "png")
)

# Gráfico adicional para comparar déficits masculino e feminino (opcional)
linha_tempo_comparacao <- ggplot(analysis_data, aes(x = ano)) +
  geom_line(aes(y = mmdeficit_homens, colour = "Déficit masculino bruto")) +
  geom_line(aes(y = mmdeficit_mulheres, colour = "Déficit feminino")) +
  geom_line(aes(y = mmdeficit, colour = "Déficit específico masculino"), size = 1.2) +
  labs(y = "Déficit (pessoas-ano)", title = "Comparação de déficits por sexo",
       subtitle = "Déficit específico masculino = Déficit masculino - Déficit feminino") +
  theme_ipea(legend.position = "bottom") +
  scale_color_manual(values = c("Déficit masculino bruto" = "#1f77b4",
                                "Déficit feminino" = "#ff7f0e",
                                "Déficit específico masculino" = "#d62728"))

linha_tempo_comparacao

save_ipeaplot(linha_tempo_comparacao,
              "comparacao_deficits_sexo",
              path = pasta_saida,
              width = 10,
              height = 7,
              format = c("eps", "png")
)

linha_tempo_padronizada <- ggplot(analysis_data, aes(x = ano)) +
  geom_line(aes(y = scale(mmdeficit), colour = "deficit demográfico (pessas-ano)"))+
  geom_line(aes(y = scale(mortes_violentas), colour = "mortes violentas"))+
  labs(y = "z-score")+
  theme_ipea(legend.position = "bottom")+
  scale_fill_ipea(palette = "Red-Blue-White") +
  scale_color_ipea(palette = "Red-Blue-White")

linha_tempo_padronizada

save_ipeaplot(linha_tempo_padronizada,
              "deficit  e mortes linha do tempo padronizado",
              path = pasta_saida,
              width = 10,
              height = 7,
              format = c("eps", "png")
)

#===========================================================================
# REGRESSÕES (MANTIDAS IGUAIS)
#===========================================================================

# Define periods
analysis_data <- analysis_data %>%
  mutate(period = case_when(
    ano >= 1960 & ano <= 1980 ~ "1960_1980",
    ano > 1980 & ano <= 2022 ~ "1980_2022",
    TRUE ~ "full"
  ))

# Regression 1: 1960-1980
lm_1960_1980 <- lm(total_deficit ~ mortes_violentas, 
                   data = analysis_data %>% filter(period == "1960_1980"))
summary(lm_1960_1980)

# Regression 2: 1980-2022
lm_1980_2022 <- lm(total_deficit ~ mortes_violentas, 
                   data = analysis_data %>% filter(period == "1980_2022"))
summary(lm_1980_2022)

# Regression 3: 1960-2022 (Full period)
lm_full <- lm(total_deficit ~ mortes_violentas, 
              data = analysis_data)

summary(lm_full)

# Optional: Compare model fits
model_comparison <- tibble(
  Period = c("1960-1980", "1980-2022", "1960-2022"),
  R_squared = c(summary(lm_1960_1980)$r.squared,
                summary(lm_1980_2022)$r.squared,
                summary(lm_full)$r.squared),
  Observations = c(nobs(lm_1960_1980),
                   nobs(lm_1980_2022),
                   nobs(lm_full))
)

print(model_comparison)


# ==============================================================================
# TABELA DE COMPARAÇÃO DE MODELOS (MELHORADA)
# ==============================================================================

# Criar uma tabela de comparação mais detalhada
comparacao_melhorada <- data.frame(
  Periodo = c("1960-1980", "1980-2022", "1960-2022"),
  N = c(nobs(lm_1960_1980), nobs(lm_1980_2022), nobs(lm_full)),
  Intercepto = c(
    sprintf("%.0f (%.0f)", coef(lm_1960_1980)[1], confint(lm_1960_1980)[1, 1]),
    sprintf("%.0f (%.0f)", coef(lm_1980_2022)[1], confint(lm_1980_2022)[1, 1]),
    sprintf("%.0f (%.0f)", coef(lm_full)[1], confint(lm_full)[1, 1])
  ),
  Coeficiente = c(
    sprintf("%.4f (%.4f)", coef(lm_1960_1980)[2], confint(lm_1960_1980)[2, 1]),
    sprintf("%.4f (%.4f)", coef(lm_1980_2022)[2], confint(lm_1980_2022)[2, 1]),
    sprintf("%.4f (%.4f)", coef(lm_full)[2], confint(lm_full)[2, 1])
  ),
  IC_95 = c(
    sprintf("[%.4f, %.4f]", confint(lm_1960_1980)[2, 1], confint(lm_1960_1980)[2, 2]),
    sprintf("[%.4f, %.4f]", confint(lm_1980_2022)[2, 1], confint(lm_1980_2022)[2, 2]),
    sprintf("[%.4f, %.4f]", confint(lm_full)[2, 1], confint(lm_full)[2, 2])
  ),
  Valor_p = c(
    ifelse(summary(lm_1960_1980)$coefficients[2, 4] < 0.001, "< 0.001",
           sprintf("%.3f", summary(lm_1960_1980)$coefficients[2, 4])),
    ifelse(summary(lm_1980_2022)$coefficients[2, 4] < 0.001, "< 0.001",
           sprintf("%.3f", summary(lm_1980_2022)$coefficients[2, 4])),
    ifelse(summary(lm_full)$coefficients[2, 4] < 0.001, "< 0.001",
           sprintf("%.3f", summary(lm_full)$coefficients[2, 4]))
  ),
  R2 = sprintf("%.3f", c(
    summary(lm_1960_1980)$r.squared,
    summary(lm_1980_2022)$r.squared,
    summary(lm_full)$r.squared
  )),
  R2_Ajustado = sprintf("%.3f", c(
    summary(lm_1960_1980)$adj.r.squared,
    summary(lm_1980_2022)$adj.r.squared,
    summary(lm_full)$adj.r.squared
  )),
  AIC = sprintf("%.1f", c(AIC(lm_1960_1980), AIC(lm_1980_2022), AIC(lm_full))),
  BIC = sprintf("%.1f", c(BIC(lm_1960_1980), BIC(lm_1980_2022), BIC(lm_full)))
)

# Exibir tabela de comparação
cat("\n" , strrep("=", 80), "\n", sep = "")
cat("COMPARAÇÃO DETALHADA DOS MODELOS DE REGRESSÃO\n")
cat(strrep("=", 80), "\n\n")

print(knitr::kable(comparacao_melhorada, format = "simple"))


# Salvar tabela de comparação
write.csv2(comparacao_melhorada, 
           file.path(pasta_saida, "comparacao_modelos.csv"), 
           row.names = FALSE)


# ==============================================================================
# PREPARAR DADOS CONSISTENTES COM OS MODELOS LM
# ==============================================================================

# Criar variável period com os mesmos valores usados nos modelos LM
analysis_data <- analysis_data %>%
  mutate(
    period = case_when(
      ano >= 1960 & ano <= 1980 ~ "1960_1980",
      ano > 1980 & ano <= 2022 ~ "1980_2022",
      TRUE ~ "full"
    ),
    period_label = factor(period,
                          levels = c("1960_1980", "1980_2022", "full"),
                          labels = c("1960-1980", "1981-2022", "1960-2022"))
  )

# Agregar por período para gráficos de barra
period_summary <- analysis_data %>%
  group_by(period_label) %>%
  summarise(
    total_deficit = sum(total_deficit, na.rm = TRUE),
    total_homicidios = sum(homicidios, na.rm = TRUE),
    total_mortes_violentas = sum(mortes_violentas, na.rm = TRUE),
    media_deficit_anual = mean(total_deficit, na.rm = TRUE),
    media_homicidios_anual = mean(homicidios, na.rm = TRUE),
    .groups = 'drop'
  )

# ==============================================================================
# GRÁFICOS DE REGRESSÃO - USANDO OS MODELOS LM EXISTENTES
# ==============================================================================

# Função para criar gráfico de regressão a partir de um modelo LM já ajustado
create_regression_plot_from_model <- function(lm_model, data, period_name) {
  
  # Extrair informações do modelo
  intercept <- coef(lm_model)[1]
  slope <- coef(lm_model)[2]
  r2 <- summary(lm_model)$r.squared
  p_val <- summary(lm_model)$coefficients[2, 4]
  
  # Criar texto da equação
  equation_text <- paste0(
    "y = ", format(round(intercept, 0), big.mark = ".", decimal.mark = ","),
    " + ", sprintf("%.4f", slope), "x\n",
    "R² = ", sprintf("%.3f", r2), "\n",
    "p = ", ifelse(p_val < 0.001, "< 0.001", sprintf("%.3f", p_val))
  )
  
  # Determinar posição para a equação
  x_range <- range(data$mortes_violentas, na.rm = TRUE)
  y_range <- range(data$total_deficit, na.rm = TRUE)
  
  # Criar gráfico
  p <- ggplot(data, aes(x = mortes_violentas, y = total_deficit)) +
    geom_point(
      size = 3,
      alpha = 0.6
    ) +
    geom_smooth(
      method = "lm",
      se = TRUE,
      alpha = 0.2
    ) +
    geom_text(
      aes(x = x_range[1] + 0.7 * diff(x_range),
          y = y_range[1] + 0.9 * diff(y_range),
          label = equation_text),
      hjust = 0,
      vjust = 0,
      size = 4,
      color = "black",
      fontface = "bold"
    ) +
    labs(
      title = paste("déficit demográfico vs mortes violentas"),
      subtitle = paste("Período:", period_name),
      x = "mortes violentas anuais (pessoas)",
      y = "déficit demográfico anual (pessoas-ano)"
    ) +
    theme_ipea() +
    scale_color_ipea(palette = "Red-Blue-White") +
    scale_fill_ipea(palette = "Red-Blue-White") +
    theme(
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 13, hjust = 0.5, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    ) +
    scale_x_continuous(labels = scales::comma) +
    scale_y_continuous(labels = scales::comma)
  
  return(p)
}

# 3.1 Gráfico de regressão: 1960-1980
regression_1960_1980_plot <- create_regression_plot_from_model(
  lm_1960_1980,
  data = analysis_data %>% filter(period == "1960_1980"),
  period_name = "1960-1980"
)

regression_1960_1980_plot

# 3.2 Gráfico de regressão: 1980-2022
regression_1980_2022_plot <- create_regression_plot_from_model(
  lm_1980_2022,
  data = analysis_data %>% filter(period == "1980_2022"),
  period_name = "1980-2022"
)

regression_1980_2022_plot

# 3.3 Gráfico de regressão: Período completo
regression_full_plot <- create_regression_plot_from_model(
  lm_full,
  data = analysis_data,
  period_name = "1960-2022"
)

regression_full_plot

# ==============================================================================
# SALVAR VISUALIZAÇÕES
# ==============================================================================

# Salvar gráficos de regressão
save_ipeaplot(regression_1960_1980_plot,
              "regressao_1960_1980",
              path = pasta_saida,
              width = 10,
              height = 7,
              format = c("eps", "png")
)

save_ipeaplot(regression_1980_2022_plot,
              "regressao_1980_2022",
              path = pasta_saida,
              width = 10,
              height = 7,
              format = c("eps", "png")
)

save_ipeaplot(regression_full_plot,
              "regressao_1960_2022",
              path = pasta_saida,
              width = 10,
              height = 7,
              format = c("eps", "png")
)


# ==============================================================================
# MORTES DOCUMENTADAS × EXCESSO DE MORTES DURANTE A DITADURA MILITAR (1964-1985)
# ==============================================================================

dados_disp <- df_raw |>
  filter(ano >= 1964, ano <= 1985) |>
  transmute(
    documentadas = `mortos e desaparecidos documentados`,
    hom_t1 = excesso_homicidios_cf1,
    hom_t2 = excesso_homicidios_cf2,
    hom_t3 = excesso_homicidios_cf3,
    viol_t1 = excesso_mce_cf1,
    viol_t2 = excesso_mce_cf2,
    viol_t3 = excesso_mce_cf3
  )

dados_long_disp <- dados_disp |>
  pivot_longer(-documentadas, names_to = "cenario", values_to = "excesso")

# Criação de modelos de regressão
summary(lm(hom_t1 ~ documentadas, data = dados_disp))
summary(lm(hom_t2 ~ documentadas, data = dados_disp))
summary(lm(hom_t3 ~ documentadas, data = dados_disp))
summary(lm(viol_t1 ~ documentadas, data = dados_disp))
summary(lm(viol_t2 ~ documentadas, data = dados_disp))
summary(lm(viol_t3 ~ documentadas, data = dados_disp))

# Preparar labels para as equações
labels_eq <- dados_long_disp |>
  group_by(cenario) |>
  do({
    modelo <- lm(excesso ~ documentadas, data = .)
    s <- summary(modelo)
    
    tibble(
      intercepto = coef(modelo)[1],
      incl = coef(modelo)[2],
      r2 = s$r.squared,
      p = coef(s)[2, 4]
    )
  }) |>
  ungroup() |>
  mutate(
    label = paste0(
      "y = ",
      round(intercepto, 1),
      " + ",
      round(incl, 2),
      "x\n",
      "R² = ",
      round(r2, 3),
      "\n",
      "p = ",
      format.pval(p, digits = 2)
    )
  )

posicoes <- dados_long_disp |>
  group_by(cenario) |>
  summarise(
    x = min(documentadas, na.rm = TRUE),
    y = max(excesso, na.rm = TRUE)
  )

labels_eq <- labels_eq |>
  left_join(posicoes, by = "cenario")

# Gráfico de dispersão
vitimas_oficiais_excesso_mortes <- ggplot(dados_long_disp, aes(x = documentadas, y = excesso)) +
  geom_point(
    alpha = 0.6,
    size = 2,
    shape = 4
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 1,
    color = "steelblue"
  ) +
  geom_text(
    data = labels_eq,
    aes(
      x = Inf,
      y = -Inf,
      label = label
    ),
    hjust = 1.1,
    vjust = -0.5,
    size = 3,
    color = "red"
  ) +
  geom_label(
    data = labels_eq,
    aes(
      x = Inf,
      y = -Inf,
      label = label
    ),
    hjust = 1.05,
    vjust = -0.4,
    size = 3,
    color = "red",
    fill = "white",
    label.size = 0,
    alpha = 0.85
  ) +
  facet_wrap(~ cenario, scales = "free_y") +
  labs(
    title = "Mortes documentadas vs. estimativas de excesso de violência",
    subtitle = "Equações de regressão por cenário (1964–1985)",
    x = "Mortes documentadas (por ano)",
    y = "Excesso estimado de mortes violentas"
  ) +
  theme_ipea() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)

vitimas_oficiais_excesso_mortes

save_ipeaplot(vitimas_oficiais_excesso_mortes, "vítimas oficiais e excesso de mortes violentas",
              path = pasta_saida, width = 8, height = 5,
              format = c("eps", "png"))

#=============================================================================
# Acidentes de trabalho, repressão política e mortalidade por acidentes
#=============================================================================

#taxas por 100 mil habitantes

dados_doc_trab <- df_raw |>
  filter(ano >= 1968, ano <= 1985) |>
  transmute(
    documentadas = `taxa mortos e desaparecidos`,
    obitos_trabalho = `óbitos por 100 mil trabalhadores`,
    acidentes_trabalho = `taxa de acidentes por 100 mil trabalhadores`
  )

summary(dados_doc_trab$acidentes_trabalho)

modelo_doc_trab <- lm(obitos_trabalho ~ documentadas, data = dados_doc_trab)
s_doc_trab <- summary(modelo_doc_trab)

label_doc_trab <- paste0(
  "y = ", round(coef(modelo_doc_trab)[1], 1),
  " + ", round(coef(modelo_doc_trab)[2], 2), "x\n",
  "R² = ", round(s_doc_trab$r.squared, 3), "\n",
  "p = ", format.pval(s_doc_trab$coefficients[2, 4], digits = 2)
)


grafico_doc_trab <- ggplot(
  dados_doc_trab,
  aes(x = documentadas, y = obitos_trabalho)
) +
  geom_point(alpha = 0.6, size = 2, shape = 4) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2, color = "steelblue") +
  annotate(
    "label",
    x = Inf, y = -Inf,
    label = label_doc_trab,
    hjust = 1.05, vjust = -0.4,
    size = 3,
    color = "red",
    fill = "white",
    label.size = 0
  ) +
  labs(
    title = "Mortes documentadas vs. óbitos em acidentes de trabalho",
    subtitle = "período 1970–1985",
    x = "mortes pela repressão política militar por 10⁵ habitantes",
    y = "mortes em acidentes de trabalho por 10⁵ trabalhadores"
  ) +
  theme_ipea() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)

grafico_doc_trab

#com acidentes (fatais ou não)


modelo_doc_trab_ac <- lm(acidentes_trabalho ~ documentadas, data = dados_doc_trab)
s_doc_trab_ac <- summary(modelo_doc_trab_ac)

label_doc_trab_ac <- paste0(
  "y = ", round(coef(modelo_doc_trab_ac)[1], 1),
  " + ", round(coef(modelo_doc_trab_ac)[2], 2), "x\n",
  "R² = ", round(s_doc_trab_ac$r.squared, 3), "\n",
  "p = ", format.pval(s_doc_trab_ac$coefficients[2, 4], digits = 2)
)


grafico_doc_trab_ac <- ggplot(
  dados_doc_trab,
  aes(x = documentadas, y = acidentes_trabalho)
) +
  geom_point(alpha = 0.6, size = 2, shape = 4) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2, color = "steelblue") +
  annotate(
    "label",
    x = Inf, y = -Inf,
    label = label_doc_trab_ac,
    hjust = 1.05, vjust = -0.4,
    size = 3,
    color = "red",
    fill = "white",
    label.size = 0
  ) +
  labs(
    title = "assassinatos políticos vs. acidentes de trabalho",
    subtitle = "período 1968–1985",
    x = "mortes pela repressão política militar por 10⁵ habitantes",
    y = "acidentes de trabalho por 10⁵ trabalhadores"
  ) +
  theme_ipea() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)

grafico_doc_trab_ac


#=============================================================================
# Variáveis instrumentais: estimador exógeno de mortalidade por acidentes
#=============================================================================

#vamos usar assassinatos políticos documentados como variável instrumental
#acidentes por 10⁵ trabalhadores como instrumentada
#mortes por causas externas como resultado
#assim, teremos uma medida de violência estrutural-indireta do regime militar
#ou seja, a repressão política foi instrumental para a super-exploração
#cada assassinato político ajudava a impor piores condições de trabalho:
#insalubridade, insegurança, jornadas exaustivas, gerência opressiva
#repressão eliminou lideranças, organizadores e intelectuais
#mas cada um deles permitiu ampliar a exploração

library(AER)      # ivreg
library(dplyr)
library(broom)

dados_iv <- df_raw |>
  filter(ano >= 1968, ano <= 1985) |>
  select(
    ano,
    tx_mce = tx_mce,
   tx_acidentes = `taxa de acidentes por 100 mil trabalhadores`,
    tx_repressao = `taxa mortos e desaparecidos`
  ) |>
  drop_na()


summary(dados_iv)

modelo_iv <- ivreg(
  tx_mce ~tx_acidentes | tx_repressao,
  data = dados_iv
)

summary(modelo_iv, diagnostics = TRUE)

modelo_ols <- lm(tx_mce ~tx_acidentes, data = dados_iv)

summary(modelo_ols)

library(modelsummary)

modelsummary(
  list(
    "OLS" = modelo_ols,
    "IV (2SLS)" = modelo_iv
  ),
  statistic = "({std.error})",
  stars = TRUE,
  gof_map = c("nobs", "r.squared")
)


summary(modelo_iv, diagnostics = TRUE)$diagnostics


#padronizar coeficientes

sd_tx_acidentes <- sd(dados_iv$tx_acidentes)
sd_tx_mce <- sd(dados_iv$tx_mce)

beta_iv_padronizado <- coef(modelo_iv)["tx_acidentes"] *
  sd_tx_acidentes / sd_tx_mce

beta_ols_padronizado <- coef(modelo_ols)["tx_acidentes"] *
  sd_tx_acidentes / sd_tx_mce


beta_iv_padronizado
beta_ols_padronizado

#efeito: beta é desvio-padrão de tx_mce em resposta a 1 desvio-padrão detx_acidentes
#gráfico da primeira etapa:

eq_label_lm <- function(modelo, dig = 2) {
  s <- summary(modelo)
  paste0(
    "y = ",
    round(coef(modelo)[1], dig),
    " + ",
    round(coef(modelo)[2], 4), "x\n",
    "R² = ",
    round(s$r.squared, 3), "\n",
    "p = ",
    format.pval(s$coefficients[2, 4], digits = 2)
  )
}

modelo_first <- lm(
  tx_acidentes ~ tx_repressao,
  data = dados_iv
)

label_first <- eq_label_lm(modelo_first)




label_first <- eq_label_lm(modelo_first)


grafico_first_stage <- ggplot(
  dados_iv,
  aes(x = tx_repressao, y =tx_acidentes)
) +
  geom_point(size = 2, alpha = 0.6, shape = 4) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "steelblue",
    alpha = 0.2
  ) +
  annotate(
    "label",
    x = Inf,
    y = -Inf,
    label = label_first,
    hjust = 1.05,
    vjust = -0.4,
    size = 3,
    fill = "white",
    color = "red",
    label.size = 0,
    alpha = 0.85
  ) +
  labs(
    title = "primeira etapa (first stage)",
    subtitle = "mortes políticas documentadas → acidentes de trabalho",
    x = "taxa de mortos e desaparecidos políticos (por 100 mil hab.)",
    y = "taxa de acidentes de trabalho (por 100 mil trabalhadores)"
  ) +
  theme_ipea() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)

grafico_first_stage

#gráfico da segunda etapa

dados_iv$tx_acidentes_hat <- fitted(modelo_first)

modelo_second <- lm(tx_mce ~ tx_acidentes_hat, data = dados_iv)
label_second <- eq_label_lm(modelo_second)


grafico_second_stage <- ggplot(
  dados_iv,
  aes(x = tx_acidentes_hat, y = tx_mce)
) +
  geom_point(size = 2, alpha = 0.6, shape = 4) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "darkred",
    alpha = 0.2
  ) +
  annotate(
    "label",
    x = Inf,
    y = -Inf,
    label = label_second,
    hjust = 1.05,
    vjust = -0.4,
    size = 3,
    fill = "white",
    color = "red",
    linewidth = 0,
    alpha = 0.85
  ) +
  labs(
    title = "segunda etapa (second stage)",
    subtitle = "acidentes de trabalho (componente exógena) → mortes por causas externas",
    x = "taxa de acidentes de trabalho (predita)",
    y = "taxa de mortes por causas externas"
  ) +
  theme_ipea() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)

grafico_second_stage


#3feito acumulado
#predição contrafactual ano a ano
dados_iv <- dados_iv |>
  mutate(
    efeito_iv_anual = coef(modelo_iv)["tx_acidentes"] * tx_acidentes
  )

#acúmulo no período
efeito_acumulado <- sum(dados_iv$efeito_iv_anual, na.rm = TRUE)

#efeito_acumulado
dados_iv <- dados_iv |>
  left_join(
    df_raw |> select(ano, populacao_total = população),
    by = "ano"
  )

dados_iv <- dados_iv |>
  mutate(
    mortes_atribuidas = efeito_iv_anual * populacao_total / 100000
  )

mortes_atribuidas_total <- sum(dados_iv$mortes_atribuidas, na.rm = TRUE)

mortes_atribuidas_total

#gráfico patchwork

library(patchwork)

painel_iv <- grafico_first_stage /
  grafico_second_stage +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(
    title = "modelo de variáveis instrumentais (2SLS)",
    subtitle = "primeira e segunda etapas: 1968–1985",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 13, hjust = 0.5)
    )
  )

painel_iv


#==============================================================
# Função bootstrap
#==============================================================

library(AER)
library(dplyr)

boot_iv_mortes <- function(data, indices) {
  
  # Reamostragem
  d <- data[indices, ]
  
  # Reestimar IV corretamente
  mod_iv <- try(
    ivreg(
      tx_mce ~ tx_acidentes | tx_repressao,
      data = d
    ),
    silent = TRUE
  )
  
  # Se falhar, retorna 0 (não NA!)
  if (inherits(mod_iv, "try-error")) {
    return(0)
  }
  
  # Coeficiente IV correto
  beta_iv <- coef(mod_iv)["tx_acidentes"]
  
  if (is.na(beta_iv) || !is.finite(beta_iv)) {
    return(0)
  }
  
  # Mortes atribuídas ano a ano
  mortes_ano <- beta_iv *
    d$tx_acidentes *
    d$populacao_total / 100000
  
  # Efeito acumulado no período
  sum(mortes_ano, na.rm = TRUE)
}

#executar bootstrap

library(boot)

set.seed(12345)

boot_res <- boot(
  data = dados_iv,
  statistic = boot_iv_mortes,
  R = 5000
)

# Intervalo de confiança percentil
boot_ci <- boot.ci(
  boot_res,
  type = "perc"
)

boot_ci

#ou 

boot_ci_bca <- boot.ci(
  boot_res,
  type = "bca"
)

boot_ci_bca

#Estimativa pontual e intervalo de confiança

estimativa <- boot_res$t0

ic <- tibble(
  estimativa = estimativa,
  ic_inf = boot_ci$percent[4],
  ic_sup = boot_ci$percent[5]
)

ic

#Distribuição acumulada

library(ggplot2)

boot_df <- tibble(efeito = boot_res$t)

ggplot(boot_df, aes(x = efeito)) +
  geom_histogram(bins = 40, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = estimativa, linetype = "dashed", linewidth = 1) +
  labs(
    title = "Distribuição bootstrap das mortes atribuídas (IV)",
    x = "Mortes atribuídas no período",
    y = "Frequência"
  ) +
  theme_ipea()


#===============================================================
# Análise exploratória de todas as coortes e períodos
#===============================================================
#preparação dos dados
faixas_idade <- tribble(
  ~faixa_idade, ~idade_min, ~idade_max,
  "0-14",        0,          14,
  "15-29",       15,         29,
  "30-49",       30,         49,
  "50-69",       50,         69
)

periodos_intercensais <- tribble(
  ~periodo,      ~ano_ini, ~ano_fim,
  "1960-1970",   1960,     1970,
  "1970-1980",   1970,     1980,
  "1980-1991",   1980,     1991,
  "1991-2000",   1991,     2000,
  "2000-2010",   2000,     2010,
  "2010-2022",   2010,     2022
)

#função para calcular a sobrevivência por idade
calc_surv_ref <- function(df, idade_min, idade_max, sexo) {
  
  pop_1950 <- df |>
    filter(
      ano == 1950,
      sexo == sexo,
      idade_central >= idade_min,
      idade_central <= idade_max
    ) |>
    group_by(coorte) |>
    summarise(pop_1950 = sum(populacao, na.rm = TRUE), .groups = "drop")
  
  pop_1960 <- df |>
    filter(
      ano == 1960,
      sexo == sexo,
      idade_central >= idade_min + 10,
      idade_central <= idade_max + 10
    ) |>
    group_by(coorte) |>
    summarise(pop_1960 = sum(populacao, na.rm = TRUE), .groups = "drop")
  
  pop_1950 |>
    inner_join(pop_1960, by = "coorte") |>
    mutate(
      surv_dec = pop_1960 / pop_1950,
      surv_annual = surv_dec^(1 / 10)
    )
}

surv_ref_all <- function(df, i_min, i_max, s_alvo) {
  # População total da faixa no início e fim do período de referência
  pop_ini <- df |>
    filter(ano == 1950, sexo == s_alvo, idade_central >= i_min, idade_central <= i_max) |>
    summarise(total = sum(populacao, na.rm = TRUE)) |> pull(total)
  
  pop_fim <- df |>
    filter(ano == 1960, sexo == s_alvo, idade_central >= i_min + 10, idade_central <= i_max + 10) |>
    summarise(total = sum(populacao, na.rm = TRUE)) |> pull(total)
  
  # Retorna apenas a taxa anual média para essa faixa
  return(tibble(surv_annual = (pop_fim / pop_ini)^(1/10)))
}

surv_lookup <- expand_grid(faixas_idade, sexo = c("homens", "mulheres")) |>
  group_by(faixa_idade, sexo) |>
  group_modify(~ surv_ref_all(dados_long, .x$idade_min, .x$idade_max, .y$sexo)) |>
  ungroup()

# Função para calcular o déficit em função da faixa etária

calc_deficit_faixa <- function(df, faixa_info, lookup) {
  
  df_base <- df |>
    filter(
      sexo == faixa_info$sexo,
      idade_central >= faixa_info$idade_min,
      idade_central <= faixa_info$idade_max
    ) |>
    arrange(coorte, ano)
  
  if(nrow(df_base) == 0) return(tibble())
  
  df_base |>
    mutate(
      faixa_etaria_momento = cut(idade_central, 
                                 breaks = c(faixas_idade$idade_min, Inf), 
                                 labels = faixas_idade$faixa_idade, 
                                 right = FALSE)
    ) |>
    left_join(lookup, by = c("faixa_etaria_momento" = "faixa_idade", "sexo" = "sexo")) |>
    group_by(coorte) |>
    mutate(
      pop_start = first(na.omit(populacao)),
      surv_annual = replace_na(surv_annual, 1),
      pop_exp = pop_start * cumprod(surv_annual),
      deficit = pmax(pop_exp - populacao, 0)
    ) |>
    ungroup() |>
    # O SEGREDO: Removemos 'sexo' porque o group_modify vai adicioná-lo de volta
    select(-faixa_etaria_momento, -surv_annual, -sexo) 
}

#Aplicar
deficits_all <- expand_grid(faixas_idade, sexo = c("homens", "mulheres")) |>
  group_by(faixa_idade, sexo) |>
  group_modify(~ calc_deficit_faixa(
    df = dados_long, 
    faixa_info = bind_cols(.x, .y), # <--- Aqui está o segredo
    lookup = surv_lookup
  )) |>
  ungroup()

#Fazer uma tabela
tabela_deficits <- deficits_all |>
  # Substituímos o between por desigualdades diretas
  left_join(
    periodos_intercensais, 
    join_by(ano >= ano_ini, ano < ano_fim)
  ) |>
  group_by(faixa_idade, sexo, periodo) |>
  summarise(
    deficit_total = sum(deficit, na.rm = TRUE),
    .groups = "drop"
  ) |>
  # Removemos os NAs de anos que não caíram em nenhum período (ex: antes de 1960)
  filter(!is.na(periodo)) |> 
  unite("faixa_idade_sexo", faixa_idade, sexo, sep = " - ") |>
  pivot_wider(
    names_from = periodo,
    values_from = deficit_total
  ) |>
  arrange(faixa_idade_sexo)

print(tabela_deficits)

#gráfico de linhas

grafico_deficits <- deficits_all |>
  mutate(
    grupo = paste(faixa_idade, sexo, sep = " - ")
  ) |>
  group_by(ano, grupo) |>
  summarise(
    deficit_total = sum(deficit, na.rm = TRUE),
    .groups = "drop"
  ) |>
  ggplot(aes(x = ano, y = deficit_total, colour = grupo)) +
  geom_line(size = 1) +
  labs(
    x = "Ano",
    y = "Déficit demográfico (pessoas-ano)",
    colour = "Faixa idade-sexo"
  ) +
  theme_ipea() +
  scale_color_ipea(palette = "Red-Blue-White")

print(grafico_deficits)


#déficits relativos
deficits_relativos <- deficits_all |>
  group_by(coorte, sexo) |>
  arrange(ano) |>
  mutate(
    # Pega a população do registro anterior daquela coorte específica
    pop_anterior = lag(populacao),
    # Calcula a taxa (por 100 pessoas para facilitar a leitura em %)
    taxa_deficit = (deficit / pop_anterior) * 100
  ) |>
  ungroup() |>
  filter(!is.na(taxa_deficit)) # Remove o primeiro ano de cada coorte (onde não há 'anterior')

# Resumo por faixa etária para a tabela
tabela_padronizada <- deficits_relativos |>
  left_join(periodos_intercensais, join_by(ano >= ano_ini, ano < ano_fim)) |>
  group_by(faixa_idade, sexo, periodo) |>
  summarise(
    taxa_media_periodo = mean(taxa_deficit, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(!is.na(periodo)) |>
  unite("grupo", faixa_idade, sexo, sep = " - ") |>
  pivot_wider(names_from = periodo, values_from = taxa_media_periodo)

print(tabela_padronizada)


#Gráfico das taxas
grafico_taxas <- deficits_relativos |>
  mutate(
    grupo = paste(faixa_idade, sexo, sep = " - ")
  ) |>
  group_by(ano, grupo) |>
  summarise(
    taxa_media = mean(taxa_deficit, na.rm = TRUE),
    .groups = "drop"
  ) |>
  ggplot(aes(x = ano, y = taxa_media, colour = grupo)) +
  geom_line(linewidth = 1) + 
  scale_x_continuous(breaks = c(1950, 1960, 1970, 1980, 1991, 2000, 2010, 2022)) +
  labs(
    title = "Intensidade do déficit demográfico relativo",
    subtitle = "padronizado pela população da coorte no período anterior",
    x = "ano",
    y = "taxa de déficit relativo (%)",
    colour = "Faixa idade-sexo"
  ) +
  theme_ipea() +
  scale_color_ipea(palette = "Red-Blue-White")

print(grafico_taxas)

#Gráfico de área

library(dplyr)
library(ggplot2)
library(forcats) # Para ordenar os fatores do gráfico

# 1. Preparação dos dados: Ordenação e Agregação
dados_grafico_area <- deficits_all |>
  # Garante a ordem correta das faixas (da base para o topo)
  mutate(faixa_idade = factor(faixa_idade, levels = c("0-14", "15-29", "30-49", "50-69"))) |>
  # Cria o grupo combinado
  mutate(grupo_label = paste(faixa_idade, sexo, sep = " - ")) |>
  # Transforma o label em fator ordenado pela faixa etária para o empilhamento ficar correto
  mutate(grupo = fct_reorder(grupo_label, as.numeric(faixa_idade))) |>
  # --- O PASSO CRUCIAL: AGREGAR POR ANO ---
  # Somamos os déficits de todas as coortes que caem no mesmo grupo e ano
  group_by(ano, grupo) |>
  summarise(deficit_total_ano = sum(deficit, na.rm = TRUE), .groups = "drop") |>
  # Removemos anos onde o déficit total é zero para evitar linhas na base
  filter(deficit_total_ano > 0)

# 2. Gerando o Gráfico
grafico_area_empilhado_liso <- dados_grafico_area |>
  ggplot(aes(x = ano, y = deficit_total_ano, fill = grupo)) +
  # geom_area empilha automaticamente por padrão
  geom_area(alpha = 0.85, colour = "white", linewidth = 0.05) +
  scale_x_continuous(breaks = c(1950, 1960, 1970, 1980, 1991, 2000, 2010, 2022)) +
  # Formata o eixo Y para números legíveis (milhares/milhões)
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  labs(
    title = "Composição acumulada do déficit demográfico",
    subtitle = "Volume total de pessoas-ano 'ausentes', somando todas as coortes",
    x = "ano",
    y = "déficit total acumulado (pessoas-ano)",
    fill = "Faixa Etária e Sexo"
  ) +
  geom_vline(xintercept = 1964, linetype="dotted")+
  geom_vline(xintercept = 1985, linetype="dotted")+
  # theme_ipea() + # (Reative se estiver usando o pacote do IPEA)
  # scale_fill_ipea(palette = "Red-Blue-White") + # (Reative se estiver usando o pacote do IPEA)
  theme_minimal() + # Tema genérico para exemplo
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(grafico_area_empilhado_liso)


# Agrupamento final para o gráfico de área das taxas
dados_taxas_area <- deficits_relativos |>
  mutate(faixa_idade = factor(faixa_idade, levels = c("0-14", "15-29", "30-49", "50-69"))) |>
  mutate(grupo_label = paste(faixa_idade, sexo, sep = " - ")) |>
  mutate(grupo = fct_reorder(grupo_label, as.numeric(faixa_idade))) |>
  group_by(ano, grupo) |>
  summarise(taxa_total_ano = mean(taxa_deficit, na.rm = TRUE), .groups = "drop")

# Gráfico de Intensidade Acumulada
grafico_final_taxas <- dados_taxas_area |>
  ggplot(aes(x = ano, y = taxa_total_ano, fill = grupo)) +
  geom_area(alpha = 0.85, colour = "white", linewidth = 0.1) +
  scale_x_continuous(breaks = c(1950, 1960, 1970, 1980, 1991, 2000, 2010, 2022)) +
  labs(
    title = "Intensidade acumulada do déficit demográfico",
    subtitle = "Soma das taxas médias de déficit relativo por faixa-sexo",
    x = "ano",
    y = "soma das taxas (%)",
    fill = "Faixa Etária e Sexo"
  ) +
  theme_ipea() +
  scale_fill_ipea(palette = "Red-Blue-White") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_vline(xintercept = 1964, linetype="dotted") +
  geom_vline(xintercept = 1985, linetype="dotted")

print(grafico_final_taxas)

#análise de regressão: assassinatos políticos vs déficit demográfico

# 1. Preparação dos Dados para Regressão
# Agregamos a taxa média de déficit (todas as faixas) por ano para cruzar com dados oficiais
analise_repressao_deficit <- deficits_relativos |>
  group_by(ano) |>
  summarise(taxa_deficit_media = mean(taxa_deficit, na.rm = TRUE), .groups = "drop") |>
  # Join com os dados brutos que contêm a taxa de mortos e desaparecidos
  left_join(
    df_raw |> select(ano, taxa_repressao = `taxa mortos e desaparecidos`), 
    by = "ano"
  ) |>
  # Filtramos o período da Ditadura (ou conforme disponibilidade dos dados documentados)
  filter(ano >= 1960 & ano <= 1991) |>
  drop_na(taxa_repressao, taxa_deficit_media)

# 2. Cálculo do Modelo de Regressão
modelo_deficit_repressao <- lm(taxa_deficit_media ~ taxa_repressao, data = analise_repressao_deficit)
resumo_lm <- summary(modelo_deficit_repressao)

# 3. Preparação da Equação para o Gráfico
label_reg_deficit <- paste0(
  "y = ", round(coef(modelo_deficit_repressao)[1], 2), 
  " + ", round(coef(modelo_deficit_repressao)[2], 2), "x\n",
  "R² = ", round(resumo_lm$r.squared, 3), "\n",
  "p = ", format.pval(resumo_lm$coefficients[2, 4], digits = 2)
)

# 4. Geração do Gráfico de Regressão
grafico_regressao_repressao <- ggplot(analise_repressao_deficit, aes(x = taxa_repressao, y = taxa_deficit_media)) +
  geom_point(alpha = 0.7, size = 3, color = "#d62728", shape = 16) +
  # Adiciona a linha de tendência com intervalo de confiança (se = TRUE)
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2, color = "steelblue", fill = "steelblue") +
  annotate(
    "label",
    x = Inf, y = -Inf,
    label = label_reg_deficit,
    hjust = 1.05, vjust = -0.4,
    size = 4,
    color = "black",
    fill = "white",
    label.size = 0.5
  ) +
  labs(
    title = "Intensidade do déficit demográfico vs. repressão política letal",
    subtitle = "Correlação entre déficit relativo médio e taxa de mortes/desaparecidos (1960–1991)",
    x = "taxa de mortos e desaparecidos documentados (por 10⁵ hab.)",
    y = "taxa média de déficit demográfico (%)"
  ) +
  theme_ipea() +
  scale_x_continuous(labels = scales::label_number(decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ","))

# Exibir e Salvar
print(grafico_regressao_repressao)

save_ipeaplot(
  grafico_regressao_repressao, 
  "regressao_deficit_vs_repressao_oficial",
  path = pasta_saida, 
  width = 9, 
  height = 6,
  format = c("png", "eps")
)

# =============================================================================
# Migração como explicação alternativa dos déficits populacionais
# =============================================================================

#=======================================================================================
#dados migração
#=======================================================================================

#erro censitário:
omissao_censitaria <- data.frame(
  ano = c(1950, 1960, 1970, 1980, 1991, 2000, 2010, 2022),
  omissao_percentual = c(NA, NA, 0.073, 0.043, 0.047, 0.058, 0.022, 0.0833)
)
#fonte: IBGE. Pesquisa de Avaliação da Cobertura do Censo (1970, 1980, 1991, 2000, 2010, 2022)

summary(omissao_censitaria)



#Fonte: Adas, Melhem.Panorama geográfico brasileiro. São Paulo: Moderna, 2004, p. 286.
migracao_participacao <- tibble::tribble(
  ~periodo,        ~percentual,
  "1808–1890",      8.0,
  "1891–1900",     29.0,
  "1901–1920",      7.1,
  "1921–1940",      8.1,
  "1941–1950",      1.0,
  "1951–1960",      3.4,
  "1961–1970",      0.9
)

#LEVY, M. S. F.. O papel da migração internacional na evolução da população brasileira (1872 a 1972). Revista de Saúde Pública, v. 8, p. 49–90, jun. 1974. https://doi.org/10.1590/S0034-89101974000500003 
entradas_imigrantes_decada <- tibble::tribble(
  ~periodo,        ~imigrantes, ~percentual_total,
  "1872–1879",      176337,       3.10,
  "1880–1889",      448622,       7.66,
  "1890–1899",     1198327,      20.47,
  "1900–1909",      622407,      10.63,
  "1910–1919",      815453,      13.93,
  "1920–1929",      846647,      14.46,
  "1930–1939",      332768,       5.68,
  "1940–1949",      114085,       1.94,
  "1950–1959",      583068,       9.95,
  "1960–1969",      197587,       3.37,
  "1970–2004",      520877,       8.80
)

#IBGE. Estatísticas históricas do Brasil. Séries econômicas, Demográficas e Sociais de 1550 a 1985. V. 3, p. 28-33, 1986.
pop_imigrante_censos <- tibble::tribble(
  ~ano, ~pop_total,   ~pop_imigrante, ~percentual,
  1872, 10112061,       389459,         3.85,
  1890, 14333915,       351618,         2.45,
  1900, 17318565,      1074311,         6.20,
  1920, 30635605,      1565961,         5.11,
  1940, 41236315,      1406342,         3.41,
  1950, 51944397,      1213974,         2.33,
  1960, 70992343,      1400245,         1.97,
  1970, 93134846,      1229128,         1.32,
  1980,119011052,      1110910,         0.93,
  1991,146825475,       767773,         0.52,
  2000,169590693,       683982,         0.40
)

migracao_censo <- tibble::tribble(
  ~ano, ~perc_imigrantes,
  1950, 2.33,
  1960, 1.97,
  1970, 1.32,
  1980, 0.93,
  1991, 0.52,
  2000, 0.4
)

# ============================================================
# DADOS DE MIGRAÇÃO (CENSOS – % da população)
# ============================================================



# interpolar para anos intercensitários
migracao_ano <- tibble(ano = 1950:1991) |>
  left_join(migracao_censo, by = "ano") |>
  mutate(
    perc_imigrantes = approx(
      x = migracao_censo$ano,
      y = migracao_censo$perc_imigrantes,
      xout = ano,
      rule = 2
    )$y
  )



#========================================================================================
#SALDO MIGRATÓRIO ANUAL
#========================================================================================

# População total por ano
pop_ano <- df_raw |>
  select(ano, populacao = população)

# Participação da migração no crescimento (%)
migracao_part <- tibble::tribble(
  ~decada, ~participacao_migracao,
  1950, 0.0233,
  1960, 0.0197,
  1970, 0.0132,
  1980, 0.009,
  1990, 0.0052,
  2000, 0.004
)

#Crescimento populacional por década
crescimento_decada <- pop_ano |>
  filter(ano %% 10 == 0) |>
  arrange(ano) |>
  mutate(
    decada = ano - 10,
    pop_inicial = lag(populacao),
    pop_final = populacao,
    crescimento = pop_final - pop_inicial
  ) |>
  filter(!is.na(crescimento))

#Saldo migratório da década
saldo_migratorio_decada <- crescimento_decada |>
  left_join(migracao_part, by = "decada") |>
  mutate(
    saldo_migratorio = crescimento * participacao_migracao
  )


#saldo migratório anual
saldo_migratorio_anual <- pop_ano |>
  mutate(
    decada = floor(ano / 10) * 10
  ) |>
  left_join(
    saldo_migratorio_decada |> 
      select(decada, saldo_migratorio),
    by = "decada"
  ) |>
  group_by(decada) |>
  mutate(
    crescimento_anual = populacao - lag(populacao),
    peso = crescimento_anual / sum(crescimento_anual, na.rm = TRUE),
    saldo_migratorio_ano = saldo_migratorio * peso
  ) |>
  ungroup()

#distribuir linearmente o saldo migratório anual da década por ano

saldo_migratorio_anual_exponencial <- pop_ano |>
  mutate(decada = floor(ano / 10) * 10) |>
  left_join(
    saldo_migratorio_decada |>
      select(decada, saldo_migratorio, pop_inicial, pop_final),
    by = "decada"
  ) |>
  group_by(decada) |>
  mutate(
    # tempo dentro da década
    t = ano - decada,
    
    # taxa exponencial implícita da década
    r = ifelse(
      pop_inicial > 0 & pop_final > 0,
      log(pop_final / pop_inicial) / 10,
      NA_real_
    ),
    
    # pesos exponenciais
    peso_exp = exp(r * t),
    peso_exp = peso_exp / sum(peso_exp, na.rm = TRUE),
    
    # saldo migratório anual
    saldo_migratorio_ano = saldo_migratorio * peso_exp
  ) |>
  ungroup()


# ==============================================================================
# CÁLCULO DA DIFERENÇA DO SALDO MIGRATÓRIO EM RELAÇÃO A 1960
# ==============================================================================

# 1. Encontrar o saldo migratório da década de 1960
saldo_1960 <- saldo_migratorio_decada |>
  filter(decada == 1960) |>
  pull(saldo_migratorio)

cat("Saldo migratório da década de 1960:", format(saldo_1960, big.mark = ".", decimal.mark = ","), "\n\n")

# 2. Calcular a diferença em relação a 1960 para cada década
diferenca_saldo <- saldo_migratorio_decada |>
  mutate(
    diferenca_1960 = saldo_migratorio - saldo_1960,
    diferenca_percentual = (saldo_migratorio - saldo_1960) / saldo_1960 * 100,
    # Classificar se é maior, menor ou igual ao saldo de 1960
    classificacao = case_when(
      saldo_migratorio > saldo_1960 ~ "Maior que 1960",
      saldo_migratorio < saldo_1960 ~ "Menor que 1960",
      TRUE ~ "Igual a 1960"
    )
  ) |>
  arrange(decada)

# 3. Exibir os resultados
print("DIFERENÇA DO SALDO MIGRATÓRIO EM RELAÇÃO À DÉCADA DE 1960")
print("===========================================================")

diferenca_saldo |>
  select(decada, populacao, crescimento, participacao_migracao, 
         saldo_migratorio, diferenca_1960, diferenca_percentual, classificacao) |>
  print(n = Inf)

# 4. Visualização gráfica
grafico_diferenca_saldo <- ggplot(diferenca_saldo, aes(x = as.factor(decada))) +
  geom_col(aes(y = saldo_migratorio, fill = classificacao), alpha = 0.8) +
  geom_hline(yintercept = saldo_1960, linetype = "dashed", color = "red", linewidth = 1) +
  geom_text(aes(y = saldo_migratorio, 
                label = paste0(format(round(saldo_migratorio/1000, 1), big.mark = ".", decimal.mark = ","), 
                               " mil\n",
                               "Δ: ", format(round(diferenca_1960/1000, 1), big.mark = ".", decimal.mark = ","), 
                               " mil")),
            vjust = -0.3, size = 3) +
  geom_text(aes(x = 2, y = saldo_1960, 
                label = paste0("1960: ", format(round(saldo_1960/1000, 1), big.mark = ".", decimal.mark = ","), " mil")),
            vjust = -1, hjust = 0.5, color = "red") +
  scale_fill_manual(values = c("Maior que 1960" = "green", 
                               "Menor que 1960" = "orange",
                               "Igual a 1960" = "gray")) +
  labs(
    title = "Diferença do Saldo Migratório por Década em Relação a 1960",
    subtitle = paste0("Linha vermelha: saldo de 1960 (", 
                      format(round(saldo_1960/1000, 1), big.mark = ".", decimal.mark = ","), 
                      " mil pessoas)"),
    x = "Década (ano inicial)",
    y = "Saldo Migratório (pessoas)",
    fill = "Classificação"
  ) +
  theme_ipea(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

grafico_diferenca_saldo

# 5. Gráfico da diferença percentual
grafico_diferenca_percentual <- ggplot(diferenca_saldo, aes(x = as.factor(decada))) +
  geom_col(aes(y = diferenca_percentual, 
               fill = ifelse(diferenca_percentual >= 0, "Positiva", "Negativa")),
           alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_text(aes(y = diferenca_percentual, 
                label = paste0(round(diferenca_percentual, 1), "%")),
            vjust = ifelse(diferenca_saldo$diferenca_percentual >= 0, -0.3, 1.3), 
            size = 3.5) +
  scale_fill_manual(values = c("Positiva" = "green", "Negativa" = "red")) +
  labs(
    title = "Variação Percentual do Saldo Migratório em Relação a 1960",
    subtitle = "Percentual de diferença em relação ao saldo migratório de 1960",
    x = "Década (ano inicial)",
    y = "Diferença Percentual (%)",
    fill = "Variação"
  ) +
  theme_ipea(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

grafico_diferenca_percentual

# 6. Combinar os gráficos
grafico_combinado <- grafico_diferenca_saldo / grafico_diferenca_percentual +
  plot_annotation(
    title = "Análise Comparativa do Saldo Migratório por Década",
    subtitle = "Comparação com a década de 1960 (referência)"
  )

grafico_combinado

# 7. Salvar os gráficos
pasta_saida_migracao <- file.path("analise_migracao")
dir.create(pasta_saida_migracao, recursive = TRUE, showWarnings = FALSE)

save_ipeaplot(grafico_diferenca_saldo, 
              "diferenca_saldo_migratorio_absoluta",
              path = pasta_saida_migracao,
              format = c("eps", "png"))

save_ipeaplot(grafico_diferenca_percentual, 
              "diferenca_saldo_migratorio_percentual",
              path = pasta_saida_migracao,
              format = c("eps", "png"))

# 8. Análise por período (antes/depois de 1960)
analise_periodo <- diferenca_saldo |>
  mutate(
    periodo = ifelse(decada < 1960, "Antes de 1960", 
                     ifelse(decada == 1960, "1960", "Depois de 1960")),
    periodo = factor(periodo, levels = c("Antes de 1960", "1960", "Depois de 1960"))
  ) |>
  group_by(periodo) |>
  summarise(
    n_decadas = n(),
    saldo_medio = mean(saldo_migratorio),
    diferenca_media = mean(diferenca_1960),
    diferenca_percentual_media = mean(diferenca_percentual)
  )

print("ANÁLISE POR PERÍODO (ANTES/DEPOIS DE 1960)")
print("==========================================")
print(analise_periodo)

# 9. Cálculo do impacto acumulado
impacto_acumulado <- diferenca_saldo |>
  mutate(
    impacto_acumulado = cumsum(diferenca_1960),
    impacto_percentual_acumulado = cumsum(diferenca_percentual)
  )

grafico_impacto_acumulado <- ggplot(impacto_acumulado, aes(x = decada)) +
  geom_line(aes(y = impacto_acumulado), color = "blue", linewidth = 1.5) +
  geom_point(aes(y = impacto_acumulado), color = "blue", size = 3) +
  geom_col(aes(y = diferenca_1960), alpha = 0.3, fill = "gray") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_text(aes(y = impacto_acumulado, 
                label = format(round(impacto_acumulado/1000, 1), 
                               big.mark = ".", decimal.mark = ",")),
            vjust = -0.5, size = 3) +
  labs(
    title = "Impacto Acumulado das Diferenças em Relação a 1960",
    subtitle = "Linha: impacto acumulado | Barras: diferença por década",
    x = "Década (ano inicial)",
    y = "Impacto Acumulado (pessoas)"
  ) +
  theme_ipea()

grafico_impacto_acumulado

# 10. Salvar resultados em CSV
write.csv(diferenca_saldo,
          file.path(pasta_saida_migracao, "diferenca_saldo_migratorio.csv"),
          row.names = FALSE, na = "")

write.csv(analise_periodo,
          file.path(pasta_saida_migracao, "analise_periodo_migracao.csv"),
          row.names = FALSE, na = "")

write.csv(impacto_acumulado,
          file.path(pasta_saida_migracao, "impacto_acumulado_migracao.csv"),
          row.names = FALSE, na = "")

# 11. Resumo estatístico
cat("\n\nRESUMO ESTATÍSTICO DAS DIFERENÇAS:\n")
cat("====================================\n")
cat("Média das diferenças:", 
    format(round(mean(diferenca_saldo$diferenca_1960, na.rm = TRUE)), 
           big.mark = ".", decimal.mark = ","), "pessoas\n")
cat("Mediana das diferenças:", 
    format(round(median(diferenca_saldo$diferenca_1960, na.rm = TRUE)), 
           big.mark = ".", decimal.mark = ","), "pessoas\n")
cat("Desvio padrão das diferenças:", 
    format(round(sd(diferenca_saldo$diferenca_1960, na.rm = TRUE)), 
           big.mark = ".", decimal.mark = ","), "pessoas\n")
cat("Máxima diferença positiva:", 
    format(round(max(diferenca_saldo$diferenca_1960, na.rm = TRUE)), 
           big.mark = ".", decimal.mark = ","), "pessoas (",
    diferenca_saldo$decada[which.max(diferenca_saldo$diferenca_1960)], ")\n")
cat("Máxima diferença negativa:", 
    format(round(min(diferenca_saldo$diferenca_1960, na.rm = TRUE)), 
           big.mark = ".", decimal.mark = ","), "pessoas (",
    diferenca_saldo$decada[which.min(diferenca_saldo$diferenca_1960)], ")\n")

cat("\n✅ Análise concluída. Resultados salvos em:", pasta_saida_migracao, "\n")

# ==============================================================================
# CÁLCULO DA EMIGRAÇÃO E DIFERENÇA EM RELAÇÃO A 1960
# ==============================================================================

# 1. Preparar dados de imigração por década
# Transformar os dados de entrada de imigrantes por década
imigracao_decada <- entradas_imigrantes_decada |>
  mutate(
    decada = as.numeric(str_extract(periodo, "^[0-9]{4}")),
    imigrantes_decada = imigrantes
  ) |>
  select(decada, imigrantes_decada, percentual_total)

# 2. Juntar com dados de saldo migratório e calcular emigração
emigracao_decada <- saldo_migratorio_decada |>
  left_join(imigracao_decada, by = "decada") |>
  mutate(
    # Calcular emigração: imigração - saldo migratório = emigração
    emigracao = imigrantes_decada - saldo_migratorio,
    # Taxa de emigração (em relação à população)
    taxa_emigracao = emigracao / pop_inicial * 100,
    # Taxa de imigração (em relação à população)
    taxa_imigracao = imigrantes_decada / pop_inicial * 100,
    # Saldo migratório como % da população
    taxa_saldo_migratorio = saldo_migratorio / pop_inicial * 100
  )

# 3. Encontrar valores da década de 1960 para referência
valores_1960 <- emigracao_decada |>
  filter(decada == 1960) |>
  summarise(
    saldo_1960 = first(saldo_migratorio),
    imigracao_1960 = first(imigrantes_decada),
    emigracao_1960 = first(emigracao),
    taxa_emigracao_1960 = first(taxa_emigracao),
    taxa_imigracao_1960 = first(taxa_imigracao)
  )

cat("VALORES DE REFERÊNCIA - DÉCADA DE 1960:\n")
cat("=======================================\n")
cat("Saldo migratório:", format(valores_1960$saldo_1960, big.mark = ".", decimal.mark = ","), "\n")
cat("Imigração:", format(valores_1960$imigracao_1960, big.mark = ".", decimal.mark = ","), "\n")
cat("Emigração:", format(valores_1960$emigracao_1960, big.mark = ".", decimal.mark = ","), "\n")
cat("Taxa de emigração:", round(valores_1960$taxa_emigracao_1960, 3), "%\n")
cat("Taxa de imigração:", round(valores_1960$taxa_imigracao_1960, 3), "%\n\n")

# 4. Calcular diferenças em relação a 1960
diferencas_emigracao <- emigracao_decada |>
  mutate(
    # Diferenças absolutas
    diferenca_saldo = saldo_migratorio - valores_1960$saldo_1960,
    diferenca_imigracao = imigrantes_decada - valores_1960$imigracao_1960,
    diferenca_emigracao = emigracao - valores_1960$emigracao_1960,
    
    # Diferenças percentuais
    diferenca_saldo_percentual = (saldo_migratorio - valores_1960$saldo_1960) / abs(valores_1960$saldo_1960) * 100,
    diferenca_imigracao_percentual = (imigrantes_decada - valores_1960$imigracao_1960) / valores_1960$imigracao_1960 * 100,
    diferenca_emigracao_percentual = (emigracao - valores_1960$emigracao_1960) / valores_1960$emigracao_1960 * 100,
    
    # Relações entre imigração e emigração
    razao_imigracao_emigracao = imigrantes_decada / emigracao,
    emigracao_por_imigrante = emigracao / imigrantes_decada * 100,
    
    # Classificação
    classificacao_emigracao = case_when(
      emigracao > valores_1960$emigracao_1960 ~ "Maior que 1960",
      emigracao < valores_1960$emigracao_1960 ~ "Menor que 1960",
      TRUE ~ "Igual a 1960"
    )
  ) |>
  arrange(decada)

# 5. Exibir resultados
cat("ANÁLISE COMPARATIVA DA EMIGRAÇÃO EM RELAÇÃO A 1960:\n")
cat("===================================================\n")

diferencas_emigracao |>
  select(
    decada, 
    imigrantes_decada, 
    saldo_migratorio, 
    emigracao,
    diferenca_emigracao,
    diferenca_emigracao_percentual,
    classificacao_emigracao
  ) |>
  print(n = Inf)

# 6. Visualização da emigração e diferenças
library(scales)

# Gráfico 1: Evolução da emigração
grafico_emigracao <- ggplot(diferencas_emigracao, aes(x = as.factor(decada))) +
  geom_col(aes(y = emigracao, fill = classificacao_emigracao), alpha = 0.8) +
  geom_hline(yintercept = valores_1960$emigracao_1960, 
             linetype = "dashed", color = "red", linewidth = 1) +
  geom_text(aes(y = emigracao, 
                label = paste0(format(round(emigracao/1000, 1), big.mark = ".", decimal.mark = ","), 
                               " mil\n",
                               "Δ: ", format(round(diferenca_emigracao/1000, 1), big.mark = ".", decimal.mark = ","), 
                               " mil")),
            vjust = -0.3, size = 3) +
  scale_fill_manual(values = c("Maior que 1960" = "orange", 
                               "Menor que 1960" = "green",
                               "Igual a 1960" = "gray")) +
  labs(
    title = "Emigração por Década e Diferença em Relação a 1960",
    subtitle = paste0("Linha vermelha: emigração de 1960 (", 
                      format(round(valores_1960$emigracao_1960/1000, 1), 
                             big.mark = ".", decimal.mark = ","), " mil pessoas)"),
    x = "Década (ano inicial)",
    y = "Emigração (pessoas)",
    fill = "Emigração vs 1960"
  ) +
  theme_ipea(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# Gráfico 2: Comparação entre imigração e emigração
grafico_comparacao <- diferencas_emigracao |>
  select(decada, imigrantes_decada, emigracao) |>
  pivot_longer(cols = c(imigrantes_decada, emigracao), 
               names_to = "tipo", values_to = "valor") |>
  mutate(
    tipo = factor(tipo, 
                  levels = c("imigrantes_decada", "emigracao"),
                  labels = c("Imigração", "Emigração"))
  ) |>
  ggplot(aes(x = as.factor(decada), y = valor, fill = tipo)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_text(aes(label = format(round(valor/1000, 1), 
                               big.mark = ".", decimal.mark = ",")),
            position = position_dodge(width = 0.9),
            vjust = -0.3, size = 3) +
  scale_fill_manual(values = c("Imigração" = "blue", "Emigração" = "red")) +
  labs(
    title = "Imigração vs Emigração por Década",
    subtitle = "Comparação entre entrada e saída de migrantes",
    x = "Década (ano inicial)",
    y = "Número de Pessoas",
    fill = "Tipo"
  ) +
  theme_ipea(legend.position = "bottom")

# Gráfico 3: Diferença percentual da emigração
grafico_diferenca_percentual <- ggplot(diferencas_emigracao, 
                                       aes(x = as.factor(decada))) +
  geom_col(aes(y = diferenca_emigracao_percentual, 
               fill = ifelse(diferenca_emigracao_percentual >= 0, 
                             "Positiva", "Negativa")),
           alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", 
             color = "black", linewidth = 0.5) +
  geom_text(aes(y = diferenca_emigracao_percentual, 
                label = paste0(round(diferenca_emigracao_percentual, 1), "%")),
            vjust = ifelse(diferencas_emigracao$diferenca_emigracao_percentual >= 0, 
                           -0.3, 1.3), 
            size = 3.5) +
  scale_fill_manual(values = c("Positiva" = "red", "Negativa" = "green")) +
  labs(
    title = "Variação Percentual da Emigração em Relação a 1960",
    subtitle = "Percentual de diferença em relação à emigração de 1960",
    x = "Década (ano inicial)",
    y = "Diferença Percentual (%)",
    fill = "Variação"
  ) +
  theme_ipea(legend.position = "bottom")

# Gráfico 4: Razão entre imigração e emigração
grafico_razao <- ggplot(diferencas_emigracao, aes(x = as.factor(decada))) +
  geom_col(aes(y = razao_imigracao_emigracao), 
           fill = "steelblue", alpha = 0.8) +
  geom_hline(yintercept = 1, linetype = "dashed", 
             color = "red", linewidth = 1) +
  geom_text(aes(y = razao_imigracao_emigracao, 
                label = paste0(round(razao_imigracao_emigracao, 2), ":1")),
            vjust = -0.3, size = 3.5) +
  labs(
    title = "Razão entre Imigração e Emigração",
    subtitle = "Valores > 1: mais imigração que emigração | Valores < 1: mais emigração que imigração",
    x = "Década (ano inicial)",
    y = "Razão (Imigração / Emigração)"
  ) +
  theme_ipea()

# 7. Combinar gráficos
grafico_combinado_emigracao <- (grafico_emigracao + grafico_comparacao) / 
  (grafico_diferenca_percentual + grafico_razao) +
  plot_annotation(
    title = "Análise Completa da Emigração Brasileira por Década",
    subtitle = "Comparação com a década de 1960 como referência"
  )

grafico_combinado_emigracao

# 8. Análise detalhada por período
analise_detalhada <- diferencas_emigracao |>
  mutate(
    periodo = case_when(
      decada < 1960 ~ "Antes de 1960",
      decada == 1960 ~ "1960 (Referência)",
      decada > 1960 ~ "Depois de 1960"
    ),
    periodo = factor(periodo, 
                     levels = c("Antes de 1960", "1960 (Referência)", "Depois de 1960"))
  ) |>
  group_by(periodo) |>
  summarise(
    n_decadas = n(),
    emigracao_media = mean(emigracao, na.rm = TRUE),
    imigracao_media = mean(imigrantes_decada, na.rm = TRUE),
    saldo_medio = mean(saldo_migratorio, na.rm = TRUE),
    diferenca_emigracao_media = mean(diferenca_emigracao, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nANÁLISE POR PERÍODO (ANTES/DEPOIS DE 1960):\n")
cat("===========================================\n")
print(analise_detalhada)

# 9. Calcular impacto acumulado
impacto_acumulado_emigracao <- diferencas_emigracao |>
  mutate(
    impacto_acumulado = cumsum(diferenca_emigracao),
    impacto_percentual_acumulado = cumsum(diferenca_emigracao_percentual)
  )

grafico_impacto_acumulado_emigracao <- ggplot(impacto_acumulado_emigracao, 
                                              aes(x = decada)) +
  geom_line(aes(y = impacto_acumulado), color = "purple", linewidth = 1.5) +
  geom_point(aes(y = impacto_acumulado), color = "purple", size = 3) +
  geom_col(aes(y = diferenca_emigracao), alpha = 0.3, fill = "gray") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_text(aes(y = impacto_acumulado, 
                label = format(round(impacto_acumulado/1000, 1), 
                               big.mark = ".", decimal.mark = ",")),
            vjust = -0.5, size = 3) +
  labs(
    title = "Impacto Acumulado das Diferenças de Emigração em Relação a 1960",
    subtitle = "Linha roxa: impacto acumulado | Barras cinzas: diferença por década",
    x = "Década (ano inicial)",
    y = "Impacto Acumulado (pessoas)"
  ) +
  theme_ipea()

grafico_impacto_acumulado_emigracao

# 10. Salvar resultados
pasta_saida_emigracao <- file.path("analise_emigracao")
dir.create(pasta_saida_emigracao, recursive = TRUE, showWarnings = FALSE)

write.csv(diferencas_emigracao,
          file.path(pasta_saida_emigracao, "dados_emigracao_completo.csv"),
          row.names = FALSE, na = "")

write.csv(analise_detalhada,
          file.path(pasta_saida_emigracao, "analise_emigracao_por_periodo.csv"),
          row.names = FALSE, na = "")

write.csv(impacto_acumulado_emigracao,
          file.path(pasta_saida_emigracao, "impacto_acumulado_emigracao.csv"),
          row.names = FALSE, na = "")

# 11. Salvar gráficos
save_ipeaplot(grafico_emigracao,
              "emigracao_diferenca_1960",
              path = pasta_saida_emigracao,
              format = c("eps", "png"))

save_ipeaplot(grafico_comparacao,
              "imigracao_vs_emigracao",
              path = pasta_saida_emigracao,
              format = c("eps", "png"))

save_ipeaplot(grafico_combinado_emigracao,
              "analise_completa_emigracao",
              path = pasta_saida_emigracao,
              width = 12, height = 10,
              format = c("eps", "png"))

#=============================================================================
#interação, mediação e variáveis instrumentais
#=============================================================================

analise_repressao_deficit <- deficits_relativos |>
  group_by(ano) |>
  summarise(
    taxa_deficit_media = mean(taxa_deficit, na.rm = TRUE),
    .groups = "drop"
  )

pop_ano <- df_raw |>
  select(ano, populacao = população)

taxa_migracao <- saldo_migratorio_anual_exponencial |>
  mutate(
    taxa_migracao = (.data$saldo_migratorio_ano / .data$populacao) * 100
  ) |>
  select(ano, taxa_migracao)



head(taxa_migracao)
names(taxa_migracao)
dados_taxas_final <- analise_repressao_deficit |>
  left_join(
    taxa_migracao,
    by = "ano"
  ) |>
  left_join(
    df_raw |> select(ano, tx_mortes_violentas),
    by = "ano"
  ) |>
  drop_na(taxa_deficit_media, taxa_migracao, tx_mortes_violentas)
m1_t <- lm(taxa_deficit_media ~ tx_mortes_violentas, data = dados_taxas_final)

m2_t <- lm(taxa_deficit_media ~ taxa_migracao, data = dados_taxas_final)

m3_t <- lm(taxa_deficit_media ~ tx_mortes_violentas + taxa_migracao,
           data = dados_taxas_final)

m4_t <- lm(taxa_migracao ~ tx_mortes_violentas, data = dados_taxas_final)

dados_iv_taxas <- dados_taxas_final |>
  left_join(
    df_raw |> select(ano, taxa_repressao = `taxa mortos e desaparecidos`),
    by = "ano"
  ) |>
  filter(ano >= 1964 & ano <= 1985) |>
  drop_na(taxa_repressao)
modelo_iv_taxas <- ivreg(
  taxa_deficit_media ~ tx_mortes_violentas + taxa_migracao |
    tx_mortes_violentas + taxa_repressao,
  data = dados_iv_taxas
)

summary(modelo_iv_taxas, diagnostics = TRUE)



# hipótese: taxa de repressão como proxy da intensidade da autocracia militar
# violência política de Estado se propagou, gerando violência, reduzindo saldo migratório
# modelo de mediação poderia capturar este efeito
# efeito direto da repressão política: proxy do impacto social (mortes indiretas)
# efeito indireto via mortes violentas: violência policial e privada permitida pelo regime
# efeito indireto via migração: exílio político, emigração econômica, dissuasão da imigração


#preparação dos dados
dados_mediacao <- dados_taxas_final |>
  left_join(
    df_raw |> select(
      ano,
      taxa_repressao = `taxa mortos e desaparecidos`
    ),
    by = "ano"
  ) |>
  filter(ano >= 1964 & ano <= 1985) |>
  drop_na(
    taxa_deficit_media,
    taxa_repressao,
    tx_mortes_violentas,
    taxa_migracao
  )

#modelos
m_mortes_violentas <- lm(
  tx_mortes_violentas ~ taxa_repressao,
  data = dados_mediacao
)
summary(m_mortes_violentas)

m_mig <- lm(
  taxa_migracao ~ taxa_repressao,
  data = dados_mediacao
)
summary(m_mig)

m_y <- lm(
  taxa_deficit_media ~
    taxa_repressao +
    tx_mortes_violentas +
    taxa_migracao,
  data = dados_mediacao
)

summary(m_y)

#coeficientes

a1 <- coef(m_mortes_violentas)["taxa_repressao"]
a2 <- coef(m_mig)["taxa_repressao"]

b1 <- coef(m_y)["tx_mortes_violentas"]
b2 <- coef(m_y)["taxa_migracao"]

c_prime <- coef(m_y)["taxa_repressao"]

ind_mortes_violentas <- a1 * b1
ind_mig <- a2 * b2

ind_total <- ind_mortes_violentas + ind_mig

m_total <- lm(
  taxa_deficit_media ~ taxa_repressao,
  data = dados_mediacao
)

summary(m_total)

c_total <- coef(m_total)["taxa_repressao"]

c(
  efeito_total = c_total,
  efeito_direto = c_prime,
  efeito_indireto_violencia = ind_mortes_violentas,
  efeito_indireto_migracao = ind_mig,
  proporcao_mediada_total = ind_total / c_total
)

#bootstrap
library(boot)

boot_mediation <- function(data, indices) {
  d <- data[indices, ]
  
  m1 <- lm(tx_mortes_violentas ~ taxa_repressao, data = d)
  m2 <- lm(taxa_migracao ~ taxa_repressao, data = d)
  m3 <- lm(taxa_deficit_media ~ taxa_repressao + tx_mortes_violentas + taxa_migracao, data = d)
  
  a1 <- coef(m1)["taxa_repressao"]
  a2 <- coef(m2)["taxa_repressao"]
  
  b1 <- coef(m3)["tx_mortes_violentas"]
  b2 <- coef(m3)["taxa_migracao"]
  
  c(a1*b1, a2*b2)
}

set.seed(123)

boot_res <- boot(
  data = dados_mediacao,
  statistic = boot_mediation,
  R = 2000
)

boot.ci(boot_res, index = 1, type = "perc") # via violência
boot.ci(boot_res, index = 2, type = "perc") # via migração

#==============================================================================
# Número de vítimas
#==============================================================================

ano_inicial <- 1961
ano_final <- 1990

deficit_total <- deficits_all |>
  filter(
    ano >= ano_inicial,
    ano <= ano_final,
    idade_central <= 64.5
  )

coortes_info <- deficit_total |>
  group_by(coorte) |>
  summarise(
    ano_inicio = min(ano),
    idade_inicio = min(idade_central),
    .groups = "drop"
  )

coortes_info <- deficit_total |>
  group_by(coorte) |>
  summarise(
    ano_inicio = min(ano),
    idade_inicio = min(idade_central),
    .groups = "drop"
  )

coortes_info <- coortes_info |>
  mutate(
    anos_restantes_idade = pmax(69 - idade_inicio, 0),
    anos_restantes_tempo = ano_final - ano_inicio,
    anos_exposicao = pmin(anos_restantes_idade, anos_restantes_tempo)
  )

deficit_por_coorte <- deficit_total |>
  group_by(coorte) |>
  summarise(
    deficit_pessoas_ano = sum(deficit, na.rm = TRUE),
    .groups = "drop"
  )

pessoas_unicas_coorte <- deficit_por_coorte |>
  left_join(coortes_info, by = "coorte") |>
  mutate(
    pessoas_unicas = if_else(
      anos_exposicao > 0,
      deficit_pessoas_ano / anos_exposicao,
      0
    )
  )

pessoas_unicas_coorte <- pessoas_unicas_coorte |>
  mutate(
    peso = pmin(anos_exposicao / 10, 1),
    pessoas_unicas = pessoas_unicas * peso
  )

total_pessoas_unicas <- pessoas_unicas_coorte |>
  summarise(
    pessoas_unicas_total = sum(pessoas_unicas, na.rm = TRUE)
  )

total_pessoas_unicas

migracao_reduzida_total <- impacto_acumulado |>
  filter(decada <= 1990) |>
  summarise(
    reducao = -sum(pmin(diferenca_1960, 0), na.rm = TRUE)
  ) |>
  pull(reducao)


mortes_mais_erro <- total_pessoas_unicas$pessoas_unicas_total -
  migracao_reduzida_total

mortes_mais_erro


excesso_violencia_acumulado <- df_raw |>
  filter(ano >= ano_inicial, ano <= ano_final) |>
  summarise(
    homicidios_cf1 = sum(excesso_homicidios_cf1, na.rm = TRUE),
    homicidios_cf2 = sum(excesso_homicidios_cf2, na.rm = TRUE),
    homicidios_cf3 = sum(excesso_homicidios_cf3, na.rm = TRUE),
    homicidios_cf4 = sum(excesso_homicidios_cfex, na.rm = TRUE),
    
    outras_violencias_cf1 = sum(excesso_mce_cf1, na.rm = TRUE),
    outras_violencias_cf2 = sum(excesso_mce_cf2, na.rm = TRUE),
    outras_violencias_cf3 = sum(excesso_mce_cf3, na.rm = TRUE),
    outras_violencias_cf4 = sum(excesso_mce_cfex, na.rm = TRUE)
    
    
    
  )


tabela_comparativa_final <- tibble::tibble(
  Medida = c(
    "Déficit demográfico líquido (pessoas únicas)",
    "Excesso de homicídios – cenário alto (cf1)",
    "Excesso de homicídios – cenário conservador (cf2)",
    "Excesso de homicídios – cenário intermediário (cf3)",
    "Excesso de homicídios – cenário extremo (cf4)",
    
    "Excesso de demais violencias – alto (cf1)",
    "Excesso de demais violencias – conservador (cf2)",
    "Excesso de demais violencias – intermediário (cf3)",
    "Excesso de demais violencias – extremo (cf4)"
    
  ),
  Estimativa = c(
    mortes_mais_erro,
    excesso_violencia_acumulado$homicidios_cf1,
    excesso_violencia_acumulado$homicidios_cf2,
    excesso_violencia_acumulado$homicidios_cf3,
    excesso_violencia_acumulado$homicidios_cf4,
    
    excesso_violencia_acumulado$outras_violencias_cf1,
    excesso_violencia_acumulado$outras_violencias_cf2,
    excesso_violencia_acumulado$outras_violencias_cf3,
    excesso_violencia_acumulado$outras_violencias_cf4
  )
) |>
  mutate(
    Proporcao_do_deficit = Estimativa / mortes_mais_erro
  )

knitr::kable(
  tabela_comparativa_final,
  digits = 2,
  caption = "Comparação entre déficit demográfico líquido e estimativas de excesso de mortes violentas (1964–1985)"
)

write.csv(
  tabela_comparativa_final,
  file = file.path(pasta_saida, "tabela_comparativa_deficit_vs_violencia.csv"),
  row.names = FALSE
)


#========================================
#Separar por idade e sexo
#========================================

deficit_base <- deficits_all |>
  filter(
    ano >= ano_inicial,
    ano <= ano_final,
    idade_central <= 64.5   # coerente com saída aos 70
  )

deficit_base <- deficit_base |>
  mutate(
    faixa_macro = case_when(
      idade_central < 15 ~ "0–14",
      idade_central < 30 & idade_central > 15 ~ "15–29",
      idade_central < 50 & idade_central > 30 ~ "30–49",
      TRUE               ~ "50–69"
    )
  )


coortes_info <- deficit_base |>
  group_by(coorte) |>
  summarise(
    ano_inicio   = min(ano),
    idade_inicio = min(idade_central),
    .groups = "drop"
  ) |>
  mutate(
    anos_restantes_idade = pmax(69 - idade_inicio, 0),
    anos_restantes_tempo = ano_final - ano_inicio,
    anos_exposicao       = pmin(anos_restantes_idade, anos_restantes_tempo),
    peso_exposicao       = pmin(anos_exposicao / 10, 1)
  )


pessoas_unicas_idade_sexo <- deficit_base |>
  group_by(coorte, sexo, faixa_macro) |>
  summarise(
    deficit_pessoas_ano = sum(deficit, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(coortes_info, by = "coorte") |>
  mutate(
    pessoas_unicas = if_else(
      anos_exposicao > 0,
      deficit_pessoas_ano / anos_exposicao,
      0
    ),
    pessoas_unicas = pessoas_unicas * peso_exposicao
  )

tabela_final <- pessoas_unicas_idade_sexo |>
  group_by(faixa_macro, sexo) |>
  summarise(
    pessoas_unicas = sum(pessoas_unicas, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    percentual = pessoas_unicas / sum(pessoas_unicas) * 100
  ) |>
  arrange(desc(pessoas_unicas))

print(tabela_final)

#Sensibilidade

anos_exposicao_media <- 20.256149924
#obtido pela multiplicação dos tamanhos das coortes pelo tempo de exposição
#cenário mais conservador: 31

pessoas_unicas_media <- deficit_total |>
  summarise(
    deficit_pessoas_ano_total = sum(deficit, na.rm = TRUE)
  ) |>
  mutate(
    pessoas_unicas = deficit_pessoas_ano_total / anos_exposicao_media
  )


pessoas_unicas_media

mortes_mais_erro_media <- pessoas_unicas_media$pessoas_unicas -
  migracao_reduzida_total

mortes_mais_erro_media

#modelo de mediação revertido em mortes individuais estimadas

c_total  <- c_total
c_direct <- c_prime
c_ind_v  <- ind_mortes_violentas
c_ind_m  <- ind_mig

impactos_ano <- dados_mediacao |>
  left_join(pop_ano, by = "ano") |>
  mutate(
    impacto_total = (c_total  * taxa_repressao / 100) * populacao,
    impacto_direto = (c_direct * taxa_repressao / 100) * populacao,
    impacto_ind_violencia = (c_ind_v * taxa_repressao / 100) * populacao,
    impacto_ind_migracao  = (c_ind_m * taxa_repressao / 100) * populacao
  )

impactos_acumulados <- impactos_ano |>
  summarise(
    total = sum(impacto_total, na.rm = TRUE),
    direto = sum(impacto_direto, na.rm = TRUE),
    via_violencia = sum(impacto_ind_violencia, na.rm = TRUE),
    via_migracao = sum(impacto_ind_migracao, na.rm = TRUE)
  )

impactos_acumulados$total
mortes_mais_erro

#validação com soma feita diretamente na planilha para os anos médios
anos_exposicao_media <- 21.92971776

#cenário mais conservador: 31

pessoas_unicas_media <- deficit_total |>
  summarise(
    deficit_pessoas_ano_total = sum(deficit, na.rm = TRUE)
  ) |>
  mutate(
    pessoas_unicas = deficit_pessoas_ano_total / anos_exposicao_media
  )


pessoas_unicas_media

mortes_mais_erro_media <- pessoas_unicas_media$pessoas_unicas -
  migracao_reduzida_total

mortes_mais_erro_media

#lower, upper and central bound

violencia_central <- 
  excesso_violencia_acumulado$homicidios_cf4 +
  excesso_violencia_acumulado$outras_violencias_cf4

migracao_central <- migracao_reduzida_total

residual_central <- total_media -
  (violencia_central + migracao_central)



R2_modelo <- summary(m_y)$r.squared
prop_direto <- c_prime / c_total
prop_ind_violencia <- ind_mortes_violentas / c_total
prop_ind_migracao  <- ind_mig / c_total

prop_mediado <- prop_ind_violencia + prop_ind_migracao

prop_direto + prop_mediado
# deve ser ≈ 1

total_media <- pessoas_unicas_media$pessoas_unicas
total_demografico <- total_media
total_explicado_min <- total_media * R2_modelo
total_alto <- total_media

tabela_cenarios <- tibble::tibble(
  Cenario = c(
    "Conservador (mínimo explicado pelo modelo)",
    "Central (estimativas empíricas independentes)",
    "Alto (teto estrutural pelo modelo)"
  ),
  
  Total_pessoas = c(
    total_media * R2_modelo,
    total_media,
    total_media
  ),
  
  Socioeconomico = c(
    (total_media * R2_modelo) * prop_direto,
    NA_real_,
    total_media * prop_direto
  ),
  
  Violencia = c(
    (total_media * R2_modelo) * prop_ind_violencia,
    violencia_central,
    total_media * prop_ind_violencia
  ),
  
  Migracao = c(
    (total_media * R2_modelo) * prop_ind_migracao,
    migracao_central,
    total_media * prop_ind_migracao
  ),
  
  Nao_explicadas = c(
    total_media * (1 - R2_modelo),
    residual_central,
    0
  )
)


tabela_cenarios_formatada <- tabela_cenarios |>
  mutate(
    across(where(is.numeric), ~ round(.x, 0))
  )

knitr::kable(
  tabela_cenarios_formatada,
  caption = "Cenários de impacto populacional do regime militar (pessoas únicas)",
  align = "lrrrrr"
)

write.csv(
  tabela_cenarios_formatada,
  file = file.path(pasta_saida, "tabela_cenarios_integrada.csv"),
  row.names = FALSE
)
