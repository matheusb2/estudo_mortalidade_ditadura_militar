#==============================================================================
# PACOTES
#==============================================================================


pacotes <- c("tidyverse", "ggplot2", "ipeaplot", "strucchange", "broom", "patchwork", "tseries", "aTSA", 
             "forcats", "knitr", "kableExtra", "forecast")
# Verifica quais pacotes da lista não estão instalados
pacotes_instalados <- pacotes %in% rownames(installed.packages())

# Instala apenas os que faltam
if (any(!pacotes_instalados)) {
  install.packages(pacotes[!pacotes_instalados])
}

# Carrega todos os pacotes
invisible(lapply(pacotes, library, character.only = TRUE))


salvar_reconstrucao <- "/home/matheus/Documentos/IPEA/modelos/reconstrução homicídios/reconstrucao_homicidios_desde_1960"


#===============================================================
# Carregar dados
#===============================================================

str(dados_1960_2022)

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
    tx_mce
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


#arquivos


#ATENÇÃO >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#RENOMEIE O ENDEREÇO ABAIXO COM A PASTA ONDE QUER SALVAR OS RESULTADOS
salvar_reconstrucao <- "/home/matheus/Documentos/IPEA/modelos/reconstrução homicídios/reconstrucao_homicidios_desde_1960"

str(dados_mortes)



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
# CORREÇÃO DE SUBENUMERAÇÃO CENSITÁRIA USANDO TÁBUAS DE VIDA (1950‑1990)
# ==============================================================================
# Esta correção atua sobre o data frame df_raw (formato largo) antes de qualquer
# interpolação. Para cada par de censos consecutivos, ajusta a população do censo
# anterior sempre que a sobrevivência decenal observada de uma coorte ultrapassar
# o limite máximo plausível (obtido das tábuas de vida).
# 
# Pressuposto: as tábuas de vida fornecem a sobrevivência decenal esperada para
# cada sexo, faixa etária e ano do primeiro censo. Usamos essas sobrevivências
# como limite superior (qualquer valor acima é considerado erro de cobertura).
# ==============================================================================

# ----------------------------------------------------------------------
# 1. DEFINIR SOBREVIVÊNCIA DECENAL MÁXIMA POR SEXO, FAIXA ETÁRIA E ANO
# ----------------------------------------------------------------------
# Valores extraídos manualmente das tábuas da CELADE/CEPAL (arquivos PDF).
# Para cada ano e sexo, temos a sobrevivência decenal para as faixas:
#   0-14, 15-29, 30-49, 50-69.
# A sobrevivência decenal é a probabilidade de uma coorte sobreviver 10 anos,
# calculada a partir das funções l(x) da tábua (ex: l(10)/l(0) para faixa 0-14).

limites_sobrevivencia <- tribble(
  ~ano, ~sexo, ~faixa_idade, ~surv_max_dec,
  # 1950
  1950, "homens",   "0-14",   0.8478,
  1950, "homens",   "15-29",  0.876,   # estimado a partir de l(20)/l(10) e l(30)/l(20)
  1950, "homens",   "30-49",  0.737,
  1950, "homens",   "50-69",  0.594,
  1950, "mulheres", "0-14",   0.9087,
  1950, "mulheres", "15-29",  0.919,
  1950, "mulheres", "30-49",  0.809,
  1950, "mulheres", "50-69",  0.691,
  # 1960
  1960, "homens",   "0-14",   0.8724,
  1960, "homens",   "15-29",  0.916,
  1960, "homens",   "30-49",  0.849,
  1960, "homens",   "50-69",  0.748,
  1960, "mulheres", "0-14",   0.8947,
  1960, "mulheres", "15-29",  0.939,
  1960, "mulheres", "30-49",  0.881,
  1960, "mulheres", "50-69",  0.782,
  # 1970 (valores aproximados – você pode refiná‑los com sua leitura das tábuas)
  1970, "homens",   "0-14",   0.876,
  1970, "homens",   "15-29",  0.930,
  1970, "homens",   "30-49",  0.880,
  1970, "homens",   "50-69",  0.790,
  1970, "mulheres", "0-14",   0.907,
  1970, "mulheres", "15-29",  0.950,
  1970, "mulheres", "30-49",  0.900,
  1970, "mulheres", "50-69",  0.820,
  # 1980
  1980, "homens",   "0-14",   0.908,
  1980, "homens",   "15-29",  0.940,
  1980, "homens",   "30-49",  0.895,
  1980, "homens",   "50-69",  0.824,
  1980, "mulheres", "0-14",   0.926,
  1980, "mulheres", "15-29",  0.958,
  1980, "mulheres", "30-49",  0.920,
  1980, "mulheres", "50-69",  0.855,
  # 1990
  1990, "homens",   "0-14",   0.945,
  1990, "homens",   "15-29",  0.965,
  1990, "homens",   "30-49",  0.938,
  1990, "homens",   "50-69",  0.887,
  1990, "mulheres", "0-14",   0.954,
  1990, "mulheres", "15-29",  0.974,
  1990, "mulheres", "30-49",  0.951,
  1990, "mulheres", "50-69",  0.907
)

# ----------------------------------------------------------------------
# 2. FUNÇÃO PARA CORRIGIR UM PAR DE CENSOS (FORMATO LARGO)
# ----------------------------------------------------------------------
# Dado df_raw (já com anos de censo e colunas de população), percorre todas as
# combinações de sexo, faixa etária no censo t0, encontra a faixa correspondente
# no censo t1 (idade +10 anos) e aplica a correção se a sobrevivência observada
# ultrapassar o limite da tábua de vida.

corrigir_par_censo <- function(df, ano0, ano1, limites) {
  
  # Copia para não alterar o original
  df_corr <- df
  
  # Lista de sexos
  sexos <- c("homens", "mulheres")
  
  for (sex in sexos) {
    # Colunas do censo t0 para este sexo (ex: "homens 0 a 4 anos", "homens 5 a 9 anos", ...)
    cols_t0 <- names(df_corr) %>%
      str_subset(paste0("^", sex, "\\s\\d+ a \\d+ anos$"))
    
    for (col in cols_t0) {
      # Extrair limites inferior e superior da faixa etária em t0
      faixa_str <- str_extract(col, "\\d+ a \\d+")
      idade_min_t0 <- as.numeric(str_extract(faixa_str, "^\\d+"))
      idade_max_t0 <- as.numeric(str_extract(faixa_str, "\\d+$"))
      
      # Idades correspondentes em t1 (10 anos depois)
      idade_min_t1 <- idade_min_t0 + 10
      idade_max_t1 <- idade_max_t0 + 10
      
      # Procurar a coluna em t1 que contém exatamente essa faixa etária
      col_t1 <- names(df_corr) %>%
        str_subset(paste0("^", sex, "\\s\\d+ a \\d+ anos$")) %>%
        keep(~ {
          faixa_t1 <- str_extract(.x, "\\d+ a \\d+")
          min_t1 <- as.numeric(str_extract(faixa_t1, "^\\d+"))
          max_t1 <- as.numeric(str_extract(faixa_t1, "\\d+$"))
          min_t1 == idade_min_t1 && max_t1 == idade_max_t1
        })
      
      if (length(col_t1) == 0) next
      col_t1 <- col_t1[1]
      
      # Obter os valores populacionais nos dois anos
      pop0 <- df_corr %>% filter(ano == ano0) %>% pull(col) %>% sum()
      pop1 <- df_corr %>% filter(ano == ano1) %>% pull(col_t1) %>% sum()
      
      if (pop0 <= 0 || pop1 <= 0) next
      
      surv_obs <- pop1 / pop0
      
      # Determinar a faixa etária para consulta ao limite (usar idade mínima em t0)
      faixa_consulta <- case_when(
        idade_min_t0 < 15 ~ "0-14",
        idade_min_t0 < 30 ~ "15-29",
        idade_min_t0 < 50 ~ "30-49",
        TRUE ~ "50-69"
      )
      
      limite <- limites %>%
        filter(ano == ano0, sexo == sex, faixa_idade == faixa_consulta) %>%
        pull(surv_max_dec)
      
      if (length(limite) == 0) {
        # Se não houver limite definido, usa um valor conservador (0.99)
        limite <- 0.99
      }
      
      if (surv_obs > limite) {
        fator <- limite / surv_obs
        # Corrige a população em t0 (multiplica pelo fator)
        df_corr <- df_corr %>%
          mutate(!!col := if_else(ano == ano0, get(col) * fator, get(col)))
      }
    }
  }
  
  return(df_corr)
}

# ----------------------------------------------------------------------
# 3. APLICAR A CORREÇÃO PARA TODOS OS PARES DE CENSOS CONSECUTIVOS
# ----------------------------------------------------------------------
# Defina os anos censitários disponíveis (devem estar todos presentes em df_raw)
anos_censo <- c(1950, 1960, 1970, 1980, 1991)

# Cria uma cópia de df_raw para receber as correções sequenciais
df_raw_corrigido <- df_raw

# Para cada par consecutivo (1950‑1960, 1960‑1970, 1970‑1980, 1980‑1991)
for (i in seq_len(length(anos_censo) - 1)) {
  ano0 <- anos_censo[i]
  ano1 <- anos_censo[i + 1]
  
  cat("Corrigindo período:", ano0, "-", ano1, "\n")
  df_raw_corrigido <- corrigir_par_censo(df_raw_corrigido, ano0, ano1, limites_sobrevivencia)
}

# ----------------------------------------------------------------------
# 4. SUBSTITUIR df_raw PELA VERSÃO CORRIGIDA E SEGUIR COM O SCRIPT
# ----------------------------------------------------------------------
# Agora você pode usar df_raw_corrigido no lugar de df_raw para todas as etapas
# seguintes: interpolação exponencial, criação de dados_long, etc.

# Opcional: salvar a base corrigida para inspeção
# write.csv2(df_raw_corrigido, "df_raw_corrigido.csv", row.names = FALSE)

# Se quiser manter o nome original para não alterar o restante do script:
df_raw <- df_raw_corrigido


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
  dplyr::select(-soma_grupos_calculada)

# Verificar resultado
cat("\nVerificação da imputação:\n")
cat("Total de NAs na coluna 'população' antes da imputação:", 
    sum(is.na(df_raw$população)), "\n")

# Mostrar anos onde população foi imputada
anos_imputados <- df_raw |>
  dplyr::select(ano, população) |>
  filter(!is.na(população))

cat("\nPopulação imputada para os anos 1960-1978:\n")
print(anos_imputados)

# Verificar consistência (opcional: comparar com anos censitários)
cat("\nComparação com anos censitários (1950-2022):\n")
df_raw |>
  filter(ano %in% anos_censo) |>
  dplyr::select(ano, população) |>
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
    dplyr::select(ano, matches(padrao_col)) |>
    dplyr::select(ano, !matches("contrafactual|déficit")) |>
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
# ----------------------------------------------------------------------
# GRÁFICO DE CORRELAÇÃO TEMPORAL: DÉFICIT DEMOGRÁFICO VS REPRESSÃO LETAL
# ----------------------------------------------------------------------

# 1. Preparar dados
dados_cor <- analise_repressao_deficit |>
  arrange(ano) |>
  filter(!is.na(taxa_deficit_media), !is.na(taxa_repressao))

# 2. Calcular estatísticas
cor_pearson <- cor.test(dados_cor$taxa_deficit_media, dados_cor$taxa_repressao, method = "pearson")
r_value <- cor_pearson$estimate
p_value <- cor_pearson$p.value
modelo <- lm(taxa_deficit_media ~ taxa_repressao, data = dados_cor)
r2 <- summary(modelo)$r.squared
coef_int <- 2.986
coef_ang <- 6.995


# 3. Fator de escala
# Referência: Déficit (Eixo Y principal), Repressão (Eixo Y secundário)
max_def <- max(dados_cor$taxa_deficit_media, na.rm = TRUE)
max_rep <- max(dados_cor$taxa_repressao, na.rm = TRUE)
escala <- max_def / max_rep

dados_cor <- dados_cor |>
  mutate(taxa_repressao_escalada = taxa_repressao * escala)

# 4. Construir o gráfico
grafico_correlacao_temporal <- ggplot(dados_cor, aes(x = ano)) +
  # Linhas
  geom_line(aes(y = taxa_deficit_media, colour = "Déficit Demográfico"), linewidth = 1) +
  geom_line(aes(y = taxa_repressao_escalada, colour = "Repressão Letal"), 
            linewidth = 1, linetype = "dashed") +
  
  # Marcadores de período (1964-1985)
  geom_vline(xintercept = c(1964, 1985), linetype = "dotted", color = "gray50") +
  
  # Caixa de texto (Ajustada para o canto inferior direito)
  annotate("label", x = 1991, y = 0.1,  # Posição no canto inferior
           label = paste0("Pearson r: ", round(cor_pearson$estimate, 3),
                          "\np-valor: ", format.pval(cor_pearson$p.value, digits = 2),
                          "\nR²: ", round(r2, 3)),
           hjust = 1, vjust = 0, size = 3.2, 
           fill = "white", alpha = 0.9, label.size = 0.2, family = "sans") +
  
  # Configuração de Eixos
  scale_x_continuous(breaks = seq(1960, 1990, by = 5)) +
  scale_y_continuous(
    name = "Déficit demográfico médio (%)",
    limits = c(0, max_def * 1.1),
    sec.axis = sec_axis(~ . / escala, name = "Mortes/Desaparecidos (por 10⁵ hab.)")
  ) +
  
  # Cores e Legenda
  scale_colour_manual(
    values = c("Déficit Demográfico" = "#d62728", "Repressão Letal" = "#1f77b4")
  ) +
  labs(
    title = "Correlação: Déficit Demográfico e Repressão Política",
    subtitle = "Séries temporais sobrepostas (1960–1991)",
    x = "Ano",
    colour = NULL
  ) +
  
  # Estética do Tema
  theme_minimal() + # Use theme_ipea() aqui se tiver o pacote carregado
  theme(
    legend.position = "bottom",
    # Colorindo os títulos dos eixos para facilitar a leitura do eixo duplo
    axis.title.y.left = element_text(color = "#d62728", face = "bold"),
    axis.text.y.left = element_text(color = "#d62728"),
    axis.title.y.right = element_text(color = "#1f77b4", face = "bold"),
    axis.text.y.right = element_text(color = "#1f77b4"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  )

print(grafico_correlacao_temporal)

# 6. Salvar (ajuste o caminho 'pasta_saida' conforme seu ambiente)
save_ipeaplot(
  grafico_correlacao_temporal,
  "correlacao_temporal_deficit_repressao",
  path = pasta_saida,
  width = 11,
  height = 7,
  format = c("png", "eps")
)

# ----------------------------------------------------------------------
# (Opcional) Tabela separada com estatísticas completas
# ----------------------------------------------------------------------


tabela_cor <- data.frame(
  Estatística = c("Correlação de Pearson (r)", "p-valor", "R²", 
                  "Equação da reta", "N (anos)"),
  Valor = c(
    sprintf("%.4f", r_value),
    format.pval(p_value, digits = 4),
    sprintf("%.4f", r2),
    sprintf("y = %.2f + %.4f x", coef_int, coef_ang),
    as.character(nrow(dados_cor))
  )
)

kable(tabela_cor, format = "simple", caption = "Resultados da associação entre déficit demográfico e repressão letal")

# Para salvar a tabela em CSV
write.csv2(tabela_cor, file.path(pasta_saida, "tabela_correlacao_deficit_repressao.csv"),
           row.names = FALSE)



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
                dplyr::select(coorte, ano, pop_obs = populacao),
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
                dplyr::select(coorte, ref_period, 
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
  dplyr::select(coorte, ano, idade_central, deficit_homens, pop_obs_homens, pop_exp_homens) %>%
  left_join(
    deficit_mulheres %>%
      rename(deficit_mulheres = deficit, pop_obs_mulheres = pop_obs, pop_exp_mulheres = pop_exp) %>%
      dplyr::select(coorte, ano, idade_central, deficit_mulheres, pop_obs_mulheres, pop_exp_mulheres),
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
    espec_deficit = sum(deficit_especifico_masculino, na.rm = TRUE),
    total_deficit_homens = sum(deficit_homens, na.rm = TRUE),
    total_deficit_mulheres = sum(deficit_mulheres, na.rm = TRUE),
    .groups = 'drop'
  )

# Merge with your homicide/violent death estimates
analysis_data <- annual_deficit_total %>%
  left_join(dados_1960_2022 %>% 
              dplyr::select(ano, 
                            homicidios = homicidios_intencionais_rand_media, # Using your preferred estimate
                            mortes_violentas), # Your MCE + homicides series
            by = "ano") %>%
  mutate(across(c(homicidios, mortes_violentas), ~replace_na(., 0)))

# Calculate moving average for visualization
analysis_data$mmdeficit <- zoo::rollmean(analysis_data$espec_deficit, k = 10, 
                                         fill = "extend", align = "center")

# Calculate moving averages for both sexes deficits as well (for comparison)
analysis_data$mmdeficit_homens <- zoo::rollmean(analysis_data$total_deficit_homens, k = 10, 
                                                fill = "extend", align = "center")
analysis_data$mmdeficit_mulheres <- zoo::rollmean(analysis_data$total_deficit_mulheres, k = 10, 
                                                  fill = "extend", align = "center")

#===========================================================================
# VISUALIZAÇÕES
#===========================================================================

# Gráfico adicional para comparar déficits masculino e feminino (opcional)
linha_tempo_comparacao <- ggplot(analysis_data, aes(x = ano)) +
  geom_line(aes(y = mmdeficit_homens, colour = "Déficit masculino bruto")) +
  geom_line(aes(y = mmdeficit_mulheres, colour = "Déficit feminino")) +
  geom_line(aes(y = mmdeficit, colour = "Déficit específico masculino"), linewidth = 1.2) +
  labs(y = "Déficit (pessoas-ano)", title = "Comparação de déficits por sexo",
       subtitle = "Déficit específico masculino = Déficit masculino - Déficit feminino",
       colour = "") +
  theme_ipea(legend.position = "bottom") +
  scale_color_manual(values = c("Déficit masculino bruto" = "#1f77b4",
                                "Déficit feminino" = "#ff7f0e",
                                "Déficit específico masculino" = "#d62728"))

print(linha_tempo_comparacao)

save_ipeaplot(linha_tempo_comparacao,
              "comparacao_deficits_sexo",
              path = pasta_saida,
              width = 10,
              height = 7,
              format = c("eps", "png")
)


#===========================================================================
# REGRESSÕES
#===========================================================================

# Define periods
analysis_data <- analysis_data %>%
  mutate(period = case_when(
    ano >= 1960 & ano <= 1980 ~ "1960_1978",
    ano > 1979 & ano <= 2022 ~ "1979_2022",
    TRUE ~ "full"
  ))

# Regression 1: 1960-1978
lm_1960_1978 <- lm(espec_deficit ~ mortes_violentas, 
                   data = analysis_data %>% filter(period == "1960_1978"))
summary(lm_1960_1978)

# Regression 2: 1979-2022
lm_1979_2022 <- lm(espec_deficit ~ mortes_violentas, 
                   data = analysis_data %>% filter(period == "1979_2022"))
summary(lm_1979_2022)

# Regression 3: 1960-2022 (Full period)
lm_full <- lm(espec_deficit ~ mortes_violentas, 
              data = analysis_data)

summary(lm_full)

analysis_data_79_22 <- analysis_data %>% filter(period == "1979_2022")

coint.test(analysis_data_79_22$espec_deficit, analysis_data_79_22$mortes_violentas)

# Optional: Compare model fits
model_comparison <- tibble(
  Period = c("1960-1978", "1979-2022", "1960-2022"),
  R_squared = c(summary(lm_1960_1978)$r.squared,
                summary(lm_1979_2022)$r.squared,
                summary(lm_full)$r.squared),
  Observations = c(nobs(lm_1960_1978),
                   nobs(lm_1979_2022),
                   nobs(lm_full))
)

print(model_comparison)

#==============================================================================
#Déficit padronizado e taxas de mortes violentas
#==============================================================================

# ==============================================================================
# TABELA DE COMPARAÇÃO DE MODELOS (MELHORADA)
# ==============================================================================

# Criar uma tabela de comparação mais detalhada
comparacao_melhorada <- data.frame(
  Periodo = c("1960-1978", "1979-2022", "1960-2022"),
  N = c(nobs(lm_1960_1978), nobs(lm_1979_2022), nobs(lm_full)),
  Intercepto = c(
    sprintf("%.0f (%.0f)", coef(lm_1960_1978)[1], confint(lm_1960_1978)[1, 1]),
    sprintf("%.0f (%.0f)", coef(lm_1979_2022)[1], confint(lm_1979_2022)[1, 1]),
    sprintf("%.0f (%.0f)", coef(lm_full)[1], confint(lm_full)[1, 1])
  ),
  Coeficiente = c(
    sprintf("%.4f (%.4f)", coef(lm_1960_1978)[2], confint(lm_1960_1978)[2, 1]),
    sprintf("%.4f (%.4f)", coef(lm_1979_2022)[2], confint(lm_1979_2022)[2, 1]),
    sprintf("%.4f (%.4f)", coef(lm_full)[2], confint(lm_full)[2, 1])
  ),
  IC_95 = c(
    sprintf("[%.4f, %.4f]", confint(lm_1960_1978)[2, 1], confint(lm_1960_1978)[2, 2]),
    sprintf("[%.4f, %.4f]", confint(lm_1979_2022)[2, 1], confint(lm_1979_2022)[2, 2]),
    sprintf("[%.4f, %.4f]", confint(lm_full)[2, 1], confint(lm_full)[2, 2])
  ),
  Valor_p = c(
    ifelse(summary(lm_1960_1978)$coefficients[2, 4] < 0.001, "< 0.001",
           sprintf("%.3f", summary(lm_1960_1978)$coefficients[2, 4])),
    ifelse(summary(lm_1979_2022)$coefficients[2, 4] < 0.001, "< 0.001",
           sprintf("%.3f", summary(lm_1979_2022)$coefficients[2, 4])),
    ifelse(summary(lm_full)$coefficients[2, 4] < 0.001, "< 0.001",
           sprintf("%.3f", summary(lm_full)$coefficients[2, 4]))
  ),
  R2 = sprintf("%.3f", c(
    summary(lm_1960_1978)$r.squared,
    summary(lm_1979_2022)$r.squared,
    summary(lm_full)$r.squared
  )),
  R2_Ajustado = sprintf("%.3f", c(
    summary(lm_1960_1978)$adj.r.squared,
    summary(lm_1979_2022)$adj.r.squared,
    summary(lm_full)$adj.r.squared
  )),
  AIC = sprintf("%.1f", c(AIC(lm_1960_1978), AIC(lm_1979_2022), AIC(lm_full))),
  BIC = sprintf("%.1f", c(BIC(lm_1960_1978), BIC(lm_1979_2022), BIC(lm_full)))
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
      ano >= 1960 & ano <= 1978 ~ "1960_1978",
      ano > 1980 & ano <= 2022 ~ "1979_2022",
      TRUE ~ "full"
    ),
    period_label = factor(period,
                          levels = c("1960_1978", "1979_2022", "full"),
                          labels = c("1960-1978", "1981-2022", "1960-2022"))
  )

# Agregar por período para gráficos de barra
period_summary <- analysis_data %>%
  group_by(period_label) %>%
  summarise(
    espec_deficit = sum(espec_deficit, na.rm = TRUE),
    total_homicidios = sum(homicidios, na.rm = TRUE),
    total_mortes_violentas = sum(mortes_violentas, na.rm = TRUE),
    media_deficit_anual = mean(espec_deficit, na.rm = TRUE),
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
  y_range <- range(data$espec_deficit, na.rm = TRUE)
  
  # Criar gráfico
  p <- ggplot(data, aes(x = mortes_violentas, y = espec_deficit)) +
    geom_point(
      linewidth = 3,
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
      linewidth = 4,
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
      plot.title = element_text(linewidth = 15, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(linewidth = 13, hjust = 0.5, face = "bold"),
      axis.title = element_text(linewidth = 12),
      axis.text = element_text(linewidth = 10)
    ) +
    scale_x_continuous(labels = scales::comma) +
    scale_y_continuous(labels = scales::comma)
  
  return(p)
}

# 3.1 Gráfico de regressão: 1960-1978
regression_1960_1978_plot <- create_regression_plot_from_model(
  lm_1960_1978,
  data = analysis_data %>% filter(period == "1960_1978"),
  period_name = "1960-1978"
)

regression_1960_1978_plot

# 3.2 Gráfico de regressão: 1979-2022
regression_1979_2022_plot <- create_regression_plot_from_model(
  lm_1979_2022,
  data = analysis_data %>% filter(period == "1979_2022"),
  period_name = "1979-2022"
)

regression_1979_2022_plot

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
save_ipeaplot(regression_1960_1978_plot,
              "regressao_1960_1978",
              path = pasta_saida,
              width = 10,
              height = 7,
              format = c("eps", "png")
)

save_ipeaplot(regression_1979_2022_plot,
              "regressao_1979_2022",
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

#=============================================================================
# Comparação entre taxas
#=============================================================================

pop_jovem_masculina <- dados_long |>
  filter(
    sexo == "homens",
    idade_central >= 15,
    idade_central <= 29
  ) |>
  group_by(ano) |>
  summarise(
    pop_jovem_masculina = sum(populacao, na.rm = TRUE),
    .groups = "drop"
  )

series_taxas <- annual_deficit_total |>
  left_join(pop_jovem_masculina, by = "ano") |>
  left_join(
    dados_1960_2022 |>
      dplyr::select(ano, mortes_violentas, população),
    by = "ano"
  ) |>
  mutate(
    taxa_deficit_jovem_masc =
      (espec_deficit / pop_jovem_masculina) * 1e5,
    
    taxa_mortes_violentas =
      (mortes_violentas / população) * 1e5
  ) |>
  filter(!is.na(taxa_deficit_jovem_masc),
         !is.na(taxa_mortes_violentas))


summary(series_taxas[, c("taxa_deficit_jovem_masc", "taxa_mortes_violentas")])


# fator de escala entre as duas taxas
fator_escala <- max(log(series_taxas$taxa_deficit_jovem_masc), na.rm = TRUE) /
  max(log(series_taxas$taxa_mortes_violentas), na.rm = TRUE)


summary(lm(log(taxa_deficit_jovem_masc) ~ log(taxa_mortes_violentas), data = series_taxas))


# Correlação de Pearson
cor_test <- cor.test(
  log(series_taxas$taxa_deficit_jovem_masc),
  log(series_taxas$taxa_mortes_violentas),
  method = "pearson"
)

cor_test

label_cor <- paste0(
  "r = ", round(cor_test$estimate, 2), "\n",
  "p = ", format.pval(cor_test$p.value, digits = 2), "\n",
  "N = ", cor_test$parameter + 2
)

linha_tempo_taxas_duplo_eixo_cor <- ggplot(series_taxas, aes(x = ano)) +
  
  # Déficit jovem masculino (eixo primário)
  geom_line(
    aes(
      y = log(taxa_deficit_jovem_masc),
      colour = "Déficit jovem masculino"
    ),
    linewidth = 1.2
  ) +
  # Mortes violentas (re-escaladas)
  geom_line(
    aes(
      y = log(taxa_mortes_violentas) * fator_escala,
      colour = "Mortes violentas"
    ),
    linewidth = 1,
    linetype = "dashed"
  ) +
  
  # Linhas do regime
  geom_vline(xintercept = 1964, linetype = "dotted") +
  geom_vline(xintercept = 1985, linetype = "dotted") +
  
  # Anotação da correlação
  annotate(
    "label",
    x = Inf, y = Inf,
    label = label_cor,
    hjust = 1.05, vjust = 6,
    linewidth = 0.5,
    fill = "white",
    label.linewidth = 0.4
  ) +
  
  # Eixos
  scale_y_continuous(
    name = "Déficit jovem masculino (por 100 mil)",
    sec.axis = sec_axis(
      ~ . / fator_escala,
      name = "Mortes violentas (por 100 mil)"
    )
  ) +
  
  labs(
    x = "Ano",
    title = "Déficit jovem masculino e mortalidade violenta",
    subtitle = "Taxas por 100 mil jovens (15–29 anos) e correlação produto-momento",
    colour = ""
  ) +  
  theme_ipea(legend.position = "bottom") +
  scale_color_ipea(palette = "Red-Blue-White")

linha_tempo_taxas_duplo_eixo_cor

save_ipeaplot(
  linha_tempo_taxas_duplo_eixo_cor,
  "linha_tempo_deficit_jovem_masculino_correlacao",
  path = pasta_saida,
  width = 10,
  height = 7,
  format = c("eps", "png")
)

#linha do tempo número

fator_escala_abs <- max(analysis_data$mmdeficit, na.rm = TRUE) /
  max(analysis_data$mortes_violentas, na.rm = TRUE)

# Correlação de Pearson
cor_test_abs <- cor.test(
  analysis_data$mmdeficit,
  analysis_data$mortes_violentas,
  method = "pearson"
)

cor_test_abs

label_cor_abs <- paste0(
  "r = ", round(cor_test_abs$estimate, 2), "\n",
  "p = ", format.pval(cor_test_abs$p.value, digits = 2), "\n",
  "N = ", cor_test_abs$parameter + 2
)

linha_tempo_numero_cor <- ggplot(analysis_data, aes(x = ano)) +
  
  # Déficit jovem masculino (eixo primário)
  geom_line(
    aes(
      y = espec_deficit,
      colour = "Déficit masculino"
    ),
    linewidth = 1.2
  ) +
  
  # Mortes violentas (re-escaladas)
  geom_line(
    aes(
      y = mortes_violentas * fator_escala_abs,
      colour = "Mortes violentas"
    ),
    linewidth = 1,
    linetype = "dashed"
  ) +
  
  # Linhas do regime
  geom_vline(xintercept = 1964, linetype = "dotted") +
  geom_vline(xintercept = 1985, linetype = "dotted") +
  
  # Anotação da correlação
  annotate(
    "label",
    x = Inf, y = Inf,
    label = label_cor_abs,
    hjust = 1.05, vjust = 6,
    linewidth = 0.5,
    fill = "white",
    label.linewidth = 0.4
  ) +
  
  # Eixos
  scale_y_continuous(
    name = "Déficit jovem masculino (por 100 mil)",
    sec.axis = sec_axis(
      ~ . / fator_escala_abs,
      name = "Mortes violentas (vítimas)"
    )
  ) +
  
  labs(
    x = "ano",
    y = "déficit jovem específico masculino em pessoas-ano",
    title = "Déficit jovem masculino e mortalidade violenta",
    subtitle = "correlação produto-momento entre pessoas-ano e vítimas fatais",
    colour = ""
  ) +  
  theme_ipea(legend.position = "bottom") +
  scale_color_ipea(palette = "Red-Blue-White")

linha_tempo_numero_cor

save_ipeaplot(
  linha_tempo_numero_cor,
  "linha_abs_correlacao",
  path = pasta_saida,
  width = 10,
  height = 7,
  format = c("eps", "png")
)


# ==============================================================================
# IMPUTAÇÃO DE MORTES VIOLENTAS (1960–1978) VIA DÉFICIT ESPECÍFICO MASCULINO
# ==============================================================================

# 1. PREPARAR DADOS OBSERVADOS (1979–2022) E DÉFICIT
# ------------------------------------------------------------------------------

# Juntar déficit com os dados observados (usar MCE em vez de mce_total)
obs_data <- analysis_data |>
  filter(ano >= 1979, ano <= 2022) |>
  left_join(
    dados_1960_2022 |> dplyr::select(ano, 
                                     homicidios = homicidios_intencionais_rand_media,
                                     mortes_violentas, 
                                     MCE),   # <-- coluna correta
    by = "ano"
  ) |>
  mutate(
    outras_mce = MCE   # MCE já são as demais causas externas
  )

obs_data$mortes_violentas <- obs_data$mortes_violentas.x


# 1.1. TESTES DE RAIZ UNITÁRIA E COINTEGRAÇÃO


# Teste em nível (H0: Possui raiz unitária / Não é estacionária)
adf.test(obs_data$mortes_violentas)
adf.test(obs_data$espec_deficit)

# Teste na primeira diferença (H0: Possui raiz unitária)
adf.test(diff(obs_data$mortes_violentas))
adf.test(diff(obs_data$espec_deficit))



coint.test(obs_data$mortes_violentas, obs_data$espec_deficit)

coint.test(log(obs_data$mortes_violentas), obs_data$espec_deficit)

coint.test(obs_data$mortes_violentas, poly(obs_data$espec_deficit, 2))

coint.test(log(obs_data$mortes_violentas), log(obs_data$espec_deficit))


coint.test(log(obs_data$mortes_violentas), poly(obs_data$espec_deficit, 2))


#================================================
#Retropolação ARIMAX das mortes violentas
#================================================


# Inverter a ordem dos dados observados (1979 -> 2023 vira 2023 -> 1979)
obs_rev <- obs_data %>% arrange(desc(ano))

# Criar as séries temporais invertidas (frequência 1, início arbitrário 1)
y_rev <- ts(obs_rev$mortes_violentas, frequency = 1)
x_rev <- ts(obs_rev$espec_deficit, frequency = 1)

# Ajustar o modelo ARIMAX na série invertida
# O modelo aprende como os dados se comportam "voltando no tempo"
fit_rev <- auto.arima(y_rev, xreg = x_rev, stepwise = FALSE, approximation = FALSE)



# Realizar o diagnóstico do modelo invertido
checkresiduals(fit_rev)

# Se você quiser extrair o p-valor do teste de Ljung-Box manualmente:
test_lb <- Box.test(residuals(fit_rev), type = "Ljung-Box")
print(test_lb)

# Preparar o regressor para o passado (1978 até 1960)
# IMPORTANTE: Deve estar em ordem decrescente para o forecast
pred_years <- 1960:1978
pred_data <- analysis_data |>
  filter(ano %in% pred_years) |>
  dplyr::select(ano, espec_deficit)

pred_rev <- pred_data %>% arrange(desc(ano))
x_pred_rev <- ts(pred_rev$espec_deficit, frequency = 1)

# Gerar a previsão para o passado
h_periodos <- nrow(pred_rev)
if (requireNamespace("forecast", quietly = TRUE)) {
  prev_rev <- forecast::forecast(fit_rev, xreg = x_pred_rev, h = h_periodos)
} else {
  # Alternativa com predict (retorna matriz com pred, se, etc.)
  prev_rev <- predict(fit_rev, newxreg = x_pred_rev, n.ahead = h_periodos)
}

# Organizar os resultados e DESINVERTER (voltar para ordem cronológica)
backcast_mv <- data.frame(
  ano = pred_rev$ano,
  mortes_violentas_imputado = as.numeric(prev_rev$mean),
  mortes_violentas_lwr_95 = as.numeric(prev_rev$lower[,2]),
  mortes_violentas_upr_95 = as.numeric(prev_rev$upper[,2])
) %>% arrange(ano) # Volta para 1960 -> 1978

# Unificar para visualização
serie_completa <- bind_rows(
  obs_data %>% dplyr::select(ano, valor = mortes_violentas) %>% mutate(tipo = "observado"),
  backcast_mv %>% dplyr::select(ano, valor = mortes_violentas_imputado) %>% mutate(tipo = "imputado")
)


# Gráfico Final
rp_ARIMAX <- ggplot() +
  geom_ribbon(data = backcast_mv, aes(x = ano, 
                                      ymin = mortes_violentas_lwr_95, ymax = mortes_violentas_upr_95), fill = "red", alpha = 0.1) +
  geom_line(data = serie_completa, aes(x = ano, y = valor, color = tipo), linewidth = 1) +
  geom_vline(xintercept = 1978.5, linetype = "dashed") +
  labs(title = "Retropolação de Mortes Violentas (1960-1978)",
       subtitle = "Modelo ARIMAX com série invertida e regressor externo",
       x = "ano", y = "nº de mortes") +
  theme_ipea() +
  scale_color_ipea(palette = "Red-Blue-White") +
  scale_fill_ipea(palette = "Red-Blue-White")

print(rp_ARIMAX)

# Cálculo das Taxas
# 1. Unificar população (verifique se pop_imputacao existe no seu ambiente)

pop_total <- df_raw %>%
  dplyr::select(ano, população)


# 2. Join e cálculo para a série completa (Corrigido: nome do objeto e da coluna)
serie_completa_tx <- serie_completa %>%
  left_join(pop_total, by = "ano") %>%
  mutate(tx_mortes_violentas = valor * 1e5 / população) # 'valor' é o nome que você deu no bind_rows

# 3. Join e cálculo para o intervalo de confiança
backcast_tx <- backcast_mv %>%
  left_join(pop_total, by = "ano") %>%
  mutate(
    # Faltava calcular a taxa imputada pontual:
    tx_mortes_violentas_imputado = mortes_violentas_imputado * 1e5 / população,
    tx_lwr = mortes_violentas_lwr_95 * 1e5 / população,
    tx_upr = mortes_violentas_upr_95 * 1e5 / população
  )

# 4. Gráfico de Taxas
tx_ARIMAX <- ggplot() + 
  geom_ribbon(data = backcast_tx,
              aes(x = ano, ymin = tx_lwr, ymax = tx_upr),
              fill = "gray80", alpha = 0.6) +
  geom_line(data = serie_completa_tx,
            aes(x = ano, y = tx_mortes_violentas, color = tipo), linewidth = 1) +
  geom_vline(xintercept = 1978.5, linetype = "dashed") +
  labs(title = "Taxa de mortes violentas: retropolação ARIMAX (1960–1978)",
       subtitle = "Valores imputados com base no déficit educacional",
       x = "ano", y = "taxa por 100 mil habitantes", color = "") +
  theme_ipea(legend.position = "bottom") +
  scale_color_ipea(palette = "Red-Blue-White") +
  scale_fill_ipea(palette = "Red-Blue-White")


print(tx_ARIMAX)


# 1. Preparar dados do método SP (pontuais) para 1960–1978 e 1979–2022
# ------------------------------------------------------------------------------



# Criar série completa do método déficit ARIMAX
deficit_mortes_violentas <- bind_rows(
  # Período observado (1979–2022) – use os valores observados
  obs_data %>% 
    dplyr::select(ano, mortes_violentas_deficit = mortes_violentas) %>% 
    mutate(tipo = "observado"),
  # Período imputado (1960–1978) – use as imputações ARIMAX
  backcast_mv %>% 
    dplyr::select(ano, mortes_violentas_deficit = mortes_violentas_imputado) %>% 
    mutate(tipo = "imputado")
) %>%
  left_join(
    backcast_mv %>% 
      dplyr::select(ano, lwr = mortes_violentas_lwr_95, upr = mortes_violentas_upr_95),
    by = "ano"
  )

deficit_tx_mortes_violentas <- bind_rows(
  
  # Observado (correto)
  serie_completa_tx %>% 
    dplyr::filter(tipo == "observado") %>%
    dplyr::select(ano, tx_deficit = tx_mortes_violentas) %>% 
    mutate(tipo = "observado"),
  
  # Imputado
  backcast_tx %>% 
    dplyr::select(ano, tx_deficit = tx_mortes_violentas_imputado) %>% 
    mutate(tipo = "imputado")
  
) %>%
  left_join(
    backcast_tx %>% 
      dplyr::select(ano, lwr = tx_lwr, upr = tx_upr),
    by = "ano"
  )


# População completa de df_raw (já imputada para anos sem censo)
pop_completa <- df_raw %>% 
  dplyr::select(ano, populacao_df = população)

sp_mortes_violentas <- dados_1960_2022 %>%
  dplyr::select(ano, mortes_violentas_sp = mortes_violentas) %>%
  left_join(pop_completa, by = "ano") %>%
  mutate(
    tx_mortes_violentas_sp = (mortes_violentas_sp / populacao_df) * 1e5
  ) %>%
  dplyr::select(-populacao_df)

# Em vez de sp_homicidios, use sp_mortes_violentas
comparacao_numeros <- sp_mortes_violentas %>%
  left_join(
    deficit_mortes_violentas %>% 
      dplyr::select(ano, mortes_violentas_deficit, lwr_mv = lwr, upr_mv = upr),
    by = "ano"
  )

comparacao_taxas <- sp_mortes_violentas %>%
  left_join(
    deficit_tx_mortes_violentas %>% 
      dplyr::select(ano, tx_mortes_violentas_deficit = tx_deficit, 
                    lwr_tx = lwr, upr_tx = upr),
    by = "ano"
  )

# 4. Gráficos comparativos
# ------------------------------------------------------------------------------

# 4.1 mortes violentas - número
p_comp_mv <- ggplot() +
  # Faixa de incerteza do método déficit (apenas para anos imputados)
  geom_ribbon(
    data = deficit_mortes_violentas %>% filter(tipo == "imputado"),
    aes(x = ano, ymin = lwr, ymax = upr),
    fill = "gray80", alpha = 0.5
  ) +
  # Linha do método SP
  geom_line(
    data = comparacao_numeros,
    aes(x = ano, y = mortes_violentas_sp, color = "SP"),
    linewidth = 1
  ) +
  # Linha do método déficit (mediana)
  geom_line(
    data = comparacao_numeros,
    aes(x = ano, y = mortes_violentas_deficit, color = "Déficit"),
    linewidth = 1
  ) +
  # Pontos observados (são os mesmos para ambos)
  geom_point(
    data = obs_data,
    aes(x = ano, y = mortes_violentas),
    size = 2, color = "black"
  ) +
  geom_vline(xintercept = 1978.5, linetype = "dashed", color = "gray50") +
  annotate("text", x = 1978.5, y = Inf, label = "início SIM", 
           hjust = 1.1, vjust = 2, linewidth = 3, angle = 90, color = "gray50") +
  labs(
    title = "Mortes violentas: comparação SP vs Déficit",
    x = "Ano", y = "Número de mortes violentas", color = "Método"
  ) +
  theme_ipea(legend.position = "bottom") +
  scale_color_manual(values = c("SP" = "black", "Déficit" = "red")) +
  scale_x_continuous(breaks = seq(1960, 2020, by = 10))

print(p_comp_mv)


# 4.4 mortes violentas - taxa
p_comp_tx_mv <- ggplot() +
  geom_ribbon(data = deficit_tx_mortes_violentas %>% filter(tipo == "imputado"),
              aes(x = ano, ymin = lwr, ymax = upr),
              fill = "gray80", alpha = 0.5) +
  geom_line(data = comparacao_taxas,
            aes(x = ano, y = tx_mortes_violentas_sp, color = "SP"), linewidth = 1) +
  geom_line(data = comparacao_taxas,
            aes(x = ano, y = tx_mortes_violentas_deficit, color = "Déficit"), linewidth = 1) +
  geom_point(data = deficit_tx_mortes_violentas %>% filter(tipo == "observado"),
             aes(x = ano, y = tx_deficit), linewidth = 1.5, color = "black") +
  geom_vline(xintercept = 1978.5, linetype = "dashed", color = "gray50") +
  annotate("text", x = 1978.5, y = Inf, label = "início SIM", 
           hjust = 1.1, vjust = 2, linewidth = 3, angle = 90, color = "gray50") +
  labs(title = "Taxa de mortes violentas: imputação por SP vs por déficit específico",
       x = "Ano", y = "Taxa por 100 mil hab.", color = "Método") +
  theme_ipea(legend.position = "bottom") +
  scale_color_manual(values = c("SP" = "black", "Déficit" = "red")) +
  scale_x_continuous(breaks = seq(1960, 2020, by = 10))

print(p_comp_tx_mv)


# 5. Salvar os gráficos (opcional)
# ------------------------------------------------------------------------------
save_ipeaplot(p_comp_mv, "comparacao_mortes_violentas_numero",
              path = pasta_saida, width = 10, height = 6, format = c("eps", "png"))
save_ipeaplot(p_comp_tx_mv, "comparacao_mortes_violentas_taxa",
              path = pasta_saida, width = 10, height = 6, format = c("eps", "png"))

#Comparação quantitativa

# ============================================
# COMPARAÇÃO NUMÉRICA ENTRE MÉTODOS (1960–1978)
# ============================================



comparacao_1960_1978 <- comparacao_numeros %>%
  dplyr::filter(ano >= 1960, ano <= 1978) %>%
  mutate(
    diff_abs = mortes_violentas_deficit - mortes_violentas_sp,
    diff_pct = 100 * diff_abs / mortes_violentas_sp
  )

# 1. Soma total no período
resumo_total <- comparacao_1960_1978 %>%
  summarise(
    total_sp = sum(mortes_violentas_sp, na.rm = TRUE),
    total_deficit = sum(mortes_violentas_deficit, na.rm = TRUE),
    diff_total = total_deficit - total_sp,
    diff_pct_total = 100 * diff_total / total_sp
  )

print(resumo_total)


#criar variável de período intercensal

comparacao_periodos <- comparacao_numeros %>%
  mutate(
    periodo = case_when(
      ano >= 1960 & ano < 1970 ~ "1960-1970",
      ano >= 1970 & ano < 1980 ~ "1970-1980"
    )
  ) %>%
  filter(!is.na(periodo))

#Agregar por período
# Totais
resumo_total_periodos <- comparacao_periodos %>%
  group_by(periodo) %>%
  summarise(
    sp_total = sum(mortes_violentas_sp, na.rm = TRUE),
    deficit_total = sum(mortes_violentas_deficit, na.rm = TRUE),
    diff_abs = deficit_total - sp_total,
    diff_pct = 100 * diff_abs / sp_total,
    .groups = "drop"
  )

print(resumo_total_periodos)

# Médias
resumo_media_periodos <- comparacao_periodos %>%
  group_by(periodo) %>%
  summarise(
    sp_media = mean(mortes_violentas_sp, na.rm = TRUE),
    deficit_media = mean(mortes_violentas_deficit, na.rm = TRUE),
    diff_media = deficit_media - sp_media,
    diff_pct = 100 * diff_media / sp_media,
    .groups = "drop"
  )  
print(resumo_media_periodos)

#Visualizar

ggplot(resumo_total_periodos, aes(x = periodo)) +
  geom_col(aes(y = sp_total, fill = "imput. por SP"), position = "dodge") +
  geom_col(aes(y = deficit_total, fill = "imput. por déficit"), position = "dodge") +
  labs(
    title = "Mortes violentas por período intercensal",
    x = "Período",
    y = "Total de mortes",
    fill = "Método"
  ) +
  theme_ipea() +
  scale_color_ipea(palette = "Red-Blue-White") +
  scale_fill_ipea(palette = "Red-Blue-White")

ggplot(resumo_total_periodos, aes(x = periodo, y = diff_pct)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Diferença percentual entre métodos por período",
    y = "% (imput. déficit vs SP)",
    x = ""
  ) +
  theme_ipea() +
  scale_color_ipea(palette = "Red-Blue-White") +
  scale_fill_ipea(palette = "Red-Blue-White")


sum(backcast_mv$mortes_violentas_imputado)

#==================
#PASTAS
#==================
pasta_saida <- file.path("aprofundamento")

dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)

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
    x = "mortes pela repressão política militar por 100k hab.",
    y = "mortes por 100k trabalhadores"
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
    x = "mortes pela repressão política militar por 100k hab.",
    y = "sinistros por 100k trabalhadores"
  ) +
  theme_ipea() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)

print(grafico_doc_trab_ac)

grafico_doc_trab/grafico_doc_trab_ac

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
  dplyr::select(ano, populacao = população)

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
      dplyr::select(decada, saldo_migratorio),
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
      dplyr::select(decada, saldo_migratorio, pop_inicial, pop_final),
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
  filter(decada == 1950) |>
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

diferenca_saldo |>
  dplyr::select(decada, populacao, crescimento, participacao_migracao, 
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
  dplyr::select(decada, imigrantes_decada, percentual_total)

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
  dplyr::select(
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
  dplyr::select(decada, imigrantes_decada, emigracao) |>
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
  dplyr::select(ano, populacao = população)

taxa_migracao <- saldo_migratorio_anual_exponencial |>
  mutate(
    taxa_migracao = (.data$saldo_migratorio_ano / .data$populacao) * 100
  ) |>
  dplyr::select(ano, taxa_migracao)



head(taxa_migracao)
names(taxa_migracao)


dados_taxas_final <- analise_repressao_deficit |>
  left_join(
    taxa_migracao,
    by = "ano"
  ) |>
  left_join(
    df_raw |> dplyr::select(ano, tx_homicidios, tx_mce),
    by = "ano"
  ) |>
  drop_na(taxa_deficit_media, taxa_migracao, , tx_homicidios, tx_mce)

dados_taxas_final$tx_mortes_violentas <- dados_taxas_final$tx_homicidios +dados_taxas_final$tx_mce


# hipótese: taxa de repressão como proxy da intensidade da autocracia militar
# violência política de Estado se propagou, gerando violência, reduzindo saldo migratório
# modelo de mediação poderia capturar este efeito
# efeito direto da repressão política: proxy do impacto social (mortes indiretas)
# efeito indireto via mortes violentas: violência policial e privada permitida pelo regime
# efeito indireto via migração: exílio político, emigração econômica, dissuasão da imigração


#preparação dos dados
dados_mediacao <- dados_taxas_final |>
  left_join(
    df_raw |> dplyr::select(
      ano,
      taxa_repressao = `taxa mortos e desaparecidos`,
      ac_trabalho = `taxa de acidentes por 100 mil trabalhadores`,
    ),
    by = "ano"
  ) |>
  filter(ano >= 1961 & ano <= 1990) |>
  drop_na(
    taxa_deficit_media,
    taxa_repressao,
    tx_mortes_violentas,
    taxa_migracao,
    ac_trabalho
  )

# ----------------------------------------------------------------------
# MODELO A: mediadores = acidentes_trabalho + migracao
# ----------------------------------------------------------------------

# Modelos das variáveis mediadoras
m_acidente <- lm(ac_trabalho ~ taxa_repressao, data = dados_mediacao)

summary(m_acidente)

m_migracao <- lm(taxa_migracao ~ taxa_repressao, data = dados_mediacao)

summary(m_migracao)

# Modelo final (Y)
m_y_A <- lm(taxa_deficit_media ~ taxa_repressao + ac_trabalho + taxa_migracao,
            data = dados_mediacao)

summary(m_y_A)

summary(lm(taxa_deficit_media ~ ac_trabalho,
           data = dados_mediacao))

summary(lm(taxa_deficit_media ~ taxa_migracao,
           data = dados_mediacao))

# Coeficientes
a1 <- coef(m_acidente)["taxa_repressao"]
a2 <- coef(m_migracao)["taxa_repressao"]
b1 <- coef(m_y_A)["ac_trabalho"]
b2 <- coef(m_y_A)["taxa_migracao"]
c_prime <- coef(m_y_A)["taxa_repressao"]

# Efeitos indiretos
ind_acidente <- a1 * b1
ind_migracao <- a2 * b2
ind_total_A <- ind_acidente + ind_migracao

# Efeito total (regressão simples)
m_total_A <- lm(taxa_deficit_media ~ taxa_repressao, data = dados_mediacao)

summary(m_total_A)

c_total <- coef(m_total_A)["taxa_repressao"]

# Tabela de decomposição (Modelo A)
tabela_A <- tibble::tibble(
  Componente = c("Efeito total", "Efeito direto (não mediado)",
                 "Indireto via acidentes trabalho", "Indireto via migração"),
  Coeficiente = c(c_total, c_prime, ind_acidente, ind_migracao),
  Proporcao = c(1, c_prime / c_total, ind_acidente / c_total, ind_migracao / c_total)
)

print("=== MODELO A (acidentes + migração) ===")
print(tabela_A)

# Bootstrap para inferência (modelo A)
boot_mediation_A <- function(data, indices) {
  d <- data[indices, ]
  m1 <- lm(ac_trabalho ~ taxa_repressao, data = d)
  m2 <- lm(taxa_migracao ~ taxa_repressao, data = d)
  m3 <- lm(taxa_deficit_media ~ taxa_repressao + ac_trabalho + taxa_migracao, data = d)
  
  a1 <- coef(m1)["taxa_repressao"]
  a2 <- coef(m2)["taxa_repressao"]
  b1 <- coef(m3)["ac_trabalho"]
  b2 <- coef(m3)["taxa_migracao"]
  
  c(a1*b1, a2*b2, a1*b1 + a2*b2)  # ind_acidente, ind_migracao, total_indireto
}

set.seed(123)
boot_A <- boot(dados_mediacao, boot_mediation_A, R = 5000)

# Intervalos de confiança
ic_acidente <- boot.ci(boot_A, index = 1, type = "perc")
ic_migracao  <- boot.ci(boot_A, index = 2, type = "perc")
ic_total_ind <- boot.ci(boot_A, index = 3, type = "perc")

cat("\n=== IC 95% (Modelo A) ===\n")
cat("Efeito indireto via acidentes:", round(ic_acidente$percent[4:5], 3), "\n")
cat("Efeito indireto via migração: ", round(ic_migracao$percent[4:5], 3), "\n")
cat("Efeito indireto total:        ", round(ic_total_ind$percent[4:5], 3), "\n")


# ----------------------------------------------------------------------
# MODELO B: três mediadores (violência + acidentes + migração)
# ----------------------------------------------------------------------

# Modelos das mediadoras
m_violencia <- lm(tx_mortes_violentas ~ taxa_repressao, data = dados_mediacao)

summary(m_violencia)

summary(lm(taxa_deficit_media ~ taxa_migracao,
           data = dados_mediacao))

m_acidente  <- lm(ac_trabalho ~ taxa_repressao, data = dados_mediacao)

m_migracao  <- lm(taxa_migracao ~ taxa_repressao, data = dados_mediacao)

# Modelo final
m_y_B <- lm(taxa_deficit_media ~ taxa_repressao + tx_mortes_violentas + ac_trabalho + taxa_migracao,
            data = dados_mediacao)

summary(m_y_B)

# Coeficientes
a1 <- coef(m_violencia)["taxa_repressao"]
a2 <- coef(m_acidente)["taxa_repressao"]
a3 <- coef(m_migracao)["taxa_repressao"]
b1 <- coef(m_y_B)["tx_mortes_violentas"]
b2 <- coef(m_y_B)["ac_trabalho"]
b3 <- coef(m_y_B)["taxa_migracao"]
c_prime <- coef(m_y_B)["taxa_repressao"]

# Efeitos indiretos individuais
ind_violencia <- a1 * b1
ind_acidente  <- a2 * b2
ind_migracao  <- a3 * b3
ind_total_B <- ind_violencia + ind_acidente + ind_migracao

# Efeito total (mesmo do modelo A, pois Y e X são os mesmos)
c_total <- coef(m_total_A)["taxa_repressao"]  # já calculado

# Tabela
tabela_B <- tibble::tibble(
  Componente = c("Efeito total", "Efeito direto",
                 "Indireto via violência", "Indireto via acidentes", "Indireto via migração"),
  Coeficiente = c(c_total, c_prime, ind_violencia, ind_acidente, ind_migracao),
  Proporcao = c(1, c_prime / c_total,
                ind_violencia / c_total, ind_acidente / c_total, ind_migracao / c_total)
)

print("=== MODELO B (violência + acidentes + migração) ===")
print(tabela_B)

# Bootstrap (três mediadores)
boot_mediation_B <- function(data, indices) {
  d <- data[indices, ]
  m1 <- lm(tx_mortes_violentas ~ taxa_repressao, data = d)
  m2 <- lm(ac_trabalho ~ taxa_repressao, data = d)
  m3 <- lm(taxa_migracao ~ taxa_repressao, data = d)
  m4 <- lm(taxa_deficit_media ~ taxa_repressao + tx_mortes_violentas + ac_trabalho + taxa_migracao,
           data = d)
  
  a1 <- coef(m1)["taxa_repressao"]; b1 <- coef(m4)["tx_mortes_violentas"]
  a2 <- coef(m2)["taxa_repressao"]; b2 <- coef(m4)["ac_trabalho"]
  a3 <- coef(m3)["taxa_repressao"]; b3 <- coef(m4)["taxa_migracao"]
  
  c(a1*b1, a2*b2, a3*b3, a1*b1 + a2*b2 + a3*b3)
}

set.seed(123)
boot_B <- boot(dados_mediacao, boot_mediation_B, R = 5000)

# ICs
cat("\n=== IC 95% (Modelo B) ===\n")
cat("Via violência :", round(boot.ci(boot_B, index=1, type="perc")$percent[4:5], 3), "\n")
cat("Via acidentes :", round(boot.ci(boot_B, index=2, type="perc")$percent[4:5], 3), "\n")
cat("Via migração  :", round(boot.ci(boot_B, index=3, type="perc")$percent[4:5], 3), "\n")
cat("Total indireto:", round(boot.ci(boot_B, index=4, type="perc")$percent[4:5], 3), "\n")

write.csv2(tabela_A, file.path(pasta_saida, "mediacao_acidentes_migracao.csv"), row.names = FALSE)
write.csv2(tabela_B, file.path(pasta_saida, "mediacao_violencia_acidentes_migracao.csv"), row.names = FALSE)

saveRDS(boot_A, file.path(pasta_saida, "boot_mediacao_A.rds"))
saveRDS(boot_B, file.path(pasta_saida, "boot_mediacao_B.rds"))

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


coortes_info <- coortes_info |>
  mutate(
    anos_restantes_idade = pmax(69 - idade_inicio, 0),
    anos_restantes_tempo = ano_final - ano_inicial,
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

print(total_pessoas_unicas)

migracao_reduzida_total <- round(impacto_acumulado |>
                                   filter(decada <= 1990) |>
                                   summarise(
                                     reducao = -sum(pmin(diferenca_1960, 0), na.rm = TRUE)
                                   ) |>
                                   pull(reducao), 0)

round(migracao_reduzida_total,0)
round(migracao_reduzida_total*100/total_pessoas_unicas,2)

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
  caption = "Comparação entre déficit demográfico líquido e estimativas de excesso de mortes violentas (1961–1990)"
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

sum(tabela_final$pessoas_unicas)



#lower, upper and central bound

violencia_central <- 
  excesso_violencia_acumulado$homicidios_cf4 +
  excesso_violencia_acumulado$outras_violencias_cf4

migracao_central <- migracao_reduzida_total
total_media <- pessoas_unicas_media$pessoas_unicas
total_demografico <- total_media
R2_modelo <- summary(m_y)$r.squared
total_explicado_min <- total_media * R2_modelo
total_alto <- total_media
residual_central <- total_media - (violencia_central + migracao_central)




prop_direto <- c_prime / c_total
prop_ind_violencia <- ind_mortes_violentas / c_total
prop_ind_migracao  <- ind_mig / c_total

prop_mediado <- prop_ind_violencia + prop_ind_migracao

prop_direto + prop_mediado
# deve ser ≈ 1



tabela_cenarios <- tibble::tibble(
  Cenario = c(
    "Estimativas empíricas independentes",
    "Mínimo explicado pelo modelo",
    
    "Teto estrutural pelo modelo"
  ),
  
  Total_pessoas = c(
    violencia_central + migracao_central,
    sum(tabela_final$pessoas_unicas) * R2_modelo,
    sum(tabela_final$pessoas_unicas)
  ),
  
  choque_regime = c(
    NA_real_,
    (sum(tabela_final$pessoas_unicas) * R2_modelo) * prop_direto,
    sum(tabela_final$pessoas_unicas) * prop_direto
  ),
  
  Regime_via_Violencia = c(
    violencia_central,
    (sum(tabela_final$pessoas_unicas) * R2_modelo) * prop_ind_violencia,
    sum(tabela_final$pessoas_unicas) * prop_ind_violencia
  ),
  
  Regime_via_Migracao = c(
    migracao_central,
    (sum(tabela_final$pessoas_unicas) * R2_modelo) * prop_ind_migracao,
    sum(tabela_final$pessoas_unicas) * prop_ind_migracao
  ),
  
  Nao_explicadas = c(
    residual_central,
    sum(tabela_final$pessoas_unicas) * (1 - R2_modelo),
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

# ==============================================================================
# PREPARAÇÃO DOS DADOS PARA ANÁLISE DE IMPACTO DO REGIME (0-14 e 50-69)
# ==============================================================================

# 1. Isolar as taxas médias de déficit por ano e por faixa etária de interesse
dados_impacto_regime <- deficits_relativos |>
  filter(faixa_idade %in% c("0-14", "15-29", "30-49", "50-69")) |>
  group_by(ano, faixa_idade) |>
  summarise(
    taxa_deficit_media = mean(taxa_deficit, na.rm = TRUE),
    .groups = "drop"
  ) |>
  # Fazer o join com as variáveis de regime e tempo do df_raw
  left_join(
    df_raw |> dplyr::select(ano, regime_militar, tempo),
    by = "ano"
  ) |>
  # Remover NAs caso haja anos sem dados na base crua
  drop_na(regime_militar, tempo, taxa_deficit_media)



# ==============================================================================
# ABORDAGEM 1: MODELO COM DUMMY (EFEITO MÉDIO) via regressão linear segmentada
# ==============================================================================

# Adultos mais velhos (50-69 anos)
modelo_dummy_idosos <- lm(
  taxa_deficit_media ~ regime_militar*tempo, 
  data = dados_impacto_regime |> filter(faixa_idade == "50-69")
)

summary(modelo_dummy_idosos)


# jovens
modelo_dummy_jovens <- lm(
  taxa_deficit_media ~ regime_militar*tempo, 
  data = dados_impacto_regime |> filter(faixa_idade == "15-29")
)

summary(modelo_dummy_jovens)


# adultos
modelo_dummy_adultos <- lm(
  taxa_deficit_media ~ regime_militar*tempo, 
  data = dados_impacto_regime |> filter(faixa_idade == "30-49")
)

summary(modelo_dummy_adultos)

# criancas
modelo_dummy_criancas <- lm(
  taxa_deficit_media ~ regime_militar*tempo, 
  data = dados_impacto_regime |> filter(faixa_idade == "0-14")
)

summary(modelo_dummy_criancas)


cat("\n--- Efeito do Regime Militar nos Adultos (50-69) ---\n")
summary(modelo_dummy_idosos)

# ==============================================================================
# 
# ==============================================================================
# ----------------------------------------------------------------------
# Preparar dados comuns (fator regime)
# ----------------------------------------------------------------------

# Criar uma variável categórica que indica se o ano está dentro do regime militar
dados_plot <- dados_impacto_regime %>%
  mutate(
    regime_f = factor(regime_militar, 
                      levels = c(0, 1),
                      labels = c("Fora do regime", "Durante o regime"))
  )

# ----------------------------------------------------------------------
# Gráfico 1: Faixa etária 50-69 (Adultos mais velhos)
# ----------------------------------------------------------------------

dados_idosos <- dados_plot %>% filter(faixa_idade == "50-69")

grafico_idosos <- ggplot(dados_idosos, aes(x = ano, y = taxa_deficit_media)) +
  geom_point(alpha = 0.5, size = 2.5) +
  geom_smooth(aes(color = regime_f, group = regime_f),
              method = "lm", se = TRUE, linewidth = 1.2) +
  geom_vline(xintercept = c(1964, 1985), linetype = "dashed", color = "gray40") +
  scale_color_manual(values = c("grey50", "#D62728")) +
  labs(
    title = "50-69 anos",
    subtitle = "",
    x = "ano", y = "",
    color = "Período"
  ) +
  theme_ipea(legend.position = "none")

print(grafico_idosos)

# ----------------------------------------------------------------------
# Gráfico 2: Faixa etária 15-29 (Jovens)
# ----------------------------------------------------------------------

dados_jovens <- dados_plot %>% filter(faixa_idade == "15-29")

grafico_jovens <- ggplot(dados_jovens, aes(x = ano, y = taxa_deficit_media)) +
  geom_point(alpha = 0.5, size = 2.5) +
  geom_smooth(aes(color = regime_f, group = regime_f),
              method = "lm", se = TRUE, linewidth = 1.2) +
  geom_vline(xintercept = c(1964, 1985), linetype = "dashed", color = "gray40") +
  scale_color_manual(values = c("grey50", "#D62728")) +
  labs(
    title = "15-29 anos (jovens)",
    subtitle = "Regressão linear separada antes e durante o regime militar",
    x = "ano", y = "taxa de déficit demográfico (%)",
    color = "Período"
  ) +
  theme_ipea(legend.position = "bottom")

print(grafico_jovens)

# ----------------------------------------------------------------------
# Gráfico 3: Faixa etária 30-49 (Adultos)
# ----------------------------------------------------------------------

dados_adultos <- dados_plot %>% filter(faixa_idade == "30-49")

grafico_adultos <- ggplot(dados_adultos, aes(x = ano, y = taxa_deficit_media)) +
  geom_point(alpha = 0.5, size = 2.5) +
  geom_smooth(aes(color = regime_f, group = regime_f),
              method = "lm", se = TRUE, linewidth = 1.2) +
  geom_vline(xintercept = c(1964, 1985), linetype = "dashed", color = "gray40") +
  scale_color_manual(values = c("grey50", "#D62728")) +
  labs(
    title = "30-49 anos",
    subtitle = "",
    x = "ano", y = "",
    color = "Período"
  ) +
  theme_ipea(legend.position = "none")

print(grafico_adultos)


#---------------------------------------------
# Gráfico 4: crianças de 10 a 14 anos
#---------------------------------------------
dados_criancas <- dados_plot %>% filter(faixa_idade == "10-14")

grafico_criancas <- ggplot(dados_criancas, aes(x = ano, y = taxa_deficit_media)) +
  geom_point(alpha = 0.5, size = 2.5) +
  geom_smooth(aes(color = regime_f, group = regime_f),
              method = "lm", se = TRUE, linewidth = 1.2) +
  geom_vline(xintercept = c(1964, 1985), linetype = "dashed", color = "gray40") +
  scale_color_manual(values = c("grey50", "#D62728")) +
  labs(
    title = "10-14 anos",
    subtitle = "",
    x = "ano", y = "",
    color = "Período"
  ) +
  theme_ipea(legend.position = "none")

print(grafico_criancas)

#Gráfico síntese

# Filtrar apenas as idades desejadas
dados_filtrados <- dados_plot %>% 
  filter(faixa_idade %in% c("15-29", "30-49", "50-69"))

# Criar o gráfico com facet_wrap
ggplot(dados_filtrados, aes(x = ano, y = taxa_deficit_media)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(aes(color = regime_f, group = regime_f), 
              method = "lm", se = TRUE, linewidth = 1) +
  geom_vline(xintercept = c(1964, 1985), linetype = "dashed", color = "gray40") +
  
  # Cria os painéis para as 3 categorias
  facet_wrap(~faixa_idade, scales = "free_y") + 
  
  scale_color_manual(values = c("grey50", "#D62728")) +
  
  labs(
    title = "Evolução do déficit demográfico",
    subtitle = "Regressão linear separada antes e durante o regime militar",
    x = "Ano", 
    y = "Taxa de déficit demográfico (%)",
    color = "Período"
  ) +
  theme_ipea(legend.position = "bottom")

#=====================================================================================
# Migração
#=====================================================================================

# ----------------------------------------------------------------------
# Preparar dados (certifique-se de que taxa_migracao está presente)
# ----------------------------------------------------------------------

# Exemplo: se taxa_migracao estiver em dados_mediacao, faça o join
dados_migracao_plot <- dados_impacto_regime %>%
  left_join(
    taxa_migracao %>% select(ano, taxa_migracao),
    by = "ano"
  ) %>%
  filter(!is.na(taxa_migracao)) %>%
  mutate(
    regime_f = factor(regime_militar,
                      levels = c(0, 1),
                      labels = c("Fora do regime", "Durante o regime"))
  )

# ----------------------------------------------------------------------
# Gráfico: Taxa de migração ao longo do tempo, com retas separadas
# ----------------------------------------------------------------------

grafico_migracao <- ggplot(dados_migracao_plot, aes(x = ano, y = taxa_migracao)) +
  geom_point(alpha = 0.5, size = 2.5, color = "black") +
  geom_smooth(aes(color = regime_f, group = regime_f),
              method = "lm", se = TRUE, linewidth = 1.2) +
  geom_vline(xintercept = c(1964, 1985), linetype = "dashed", color = "gray40") +
  scale_color_manual(values = c("grey50", "#D62728")) +
  labs(
    title = "Taxa de migração líquida e regime militar",
    subtitle = "Regressão linear separada antes/durante o regime",
    x = "Ano",
    y = "Taxa de migração (saldo líquido por 100 habitantes)",
    color = "Período"
  ) +
  theme_ipea(legend.position = "bottom")

print(grafico_migracao)

#=======================================================================================
# Fecundidade
#=======================================================================================
fecundidade_decada <- tibble::tribble( ~ano_decada, ~tft, 1940, 6.16, 1950, 6.21,
                                       1960, 6.28, 1970, 5.76, 1980, 4.35, 1991, 
                                       2.89, 2000, 2.38, 2010, 1.90, 2022, 1.55)
#Fonte: IBGE, Censo Demográfico 1940/2022. 
#Link: https://educa.ibge.gov.br/professores/educa-atividades/17658-fecundidade-no-brasil-1940-a-2010.html 
#https://agenciadenoticias.ibge.gov.br/agencia-noticias/2012-agencia-de-noticias/noticias/43837-censo-2022-mostra-um-pais-com-menos-filhos-e-menos-maes
#Acesso 26/12/2025.

# 1. Preparar e Interpolar a TFT
# 1. Nova Interpolação (Exponencial/Log-Linear)
fecundidade_anual <- fecundidade_decada |>
  rename(ano = ano_decada) |>
  full_join(tibble(ano = 1940:2022), by = "ano") |>
  arrange(ano) |>
  mutate(
    # Interpolação Log-Linear: aproximamos o log e depois desfazemos com exp
    tft_interp = exp(approx(ano, log(tft), xout = ano)$y)
  )

plot(x = fecundidade_anual$ano, y = fecundidade_anual$tft_interp, type = "line", xlab = "ano", ylab = "taxa de fecundidade")

# 2. Ajuste na Função de Janela Fixa (10 anos)
# Agora com o divisor calibrado e janela de 10 anos rigorosa
calcular_mortalidade_janela_robusta <- function(ano_censo, df, divisor = 31) {
  
  # Pegamos exatamente os 10 anos anteriores ao censo (ex: 1971 a 1980)
  janela <- df |> 
    filter(ano <= ano_censo & ano > (ano_censo - 10))
  
  # Estimativa de nascimentos com a nova TFT exponencial
  nascimentos_estimados <- sum((janela$tft_interp / divisor) * janela$mulheres_15_49, na.rm = TRUE)
  
  # População real 0-9 observada no censo
  pop_real <- df |> 
    filter(ano == ano_censo) |> 
    mutate(pop_0_9 = `homens 0 a 4 anos` + `homens 5 a 9 anos` + 
             `mulheres 0 a 4 anos` + `mulheres 5 a 9 anos`) |>
    pull(pop_0_9)
  
  return(1 - (pop_real / nascimentos_estimados))
}

# 2. Integrar com sua base de impacto
dados_completos <- dados_impacto_regime |>
  left_join(fecundidade_anual, by = "ano")

# 3. Rodar o modelo controlado pela Fecundidade
# Isso permite dizer: "Controlando pela queda natural de nascimentos, 
# o regime ainda teve um efeito X no déficit de crianças"
modelo_controlado <- lm(
  taxa_deficit_media ~ regime_militar + tft_interp + tempo, 
  data = dados_completos |> filter(faixa_idade == "0-14")
)

summary(modelo_controlado)

# 1. Criar a série anual de TFT por interpolação
fecundidade_anual <- tibble::tribble(
  ~ano, ~tft, 
  1940, 6.16, 1950, 6.21, 1960, 6.28, 1970, 5.76, 
  1980, 4.35, 1991, 2.89, 2000, 2.38, 2010, 1.86, 2022, 1.55
) |>
  full_join(tibble(ano = 1940:2022), by = "ano") |>
  arrange(ano) |>
  mutate(tft_interp = approx(ano, tft, xout = ano)$y)

# 2. Calcular a Mulheres em Idade Reprodutiva (15-49)
df_nascimentos <- df_raw |>
  mutate(
    mulheres_15_49 = `mulheres 15 a 19 anos` + `mulheres 20 a 24 anos` + 
      `mulheres 25 a 29 anos` + `mulheres 30 a 39 anos` + 
      `mulheres 40 a 49 anos`
  ) |>
  left_join(fecundidade_anual, by = "ano")


df_nascimentos <- df_nascimentos |>
  mutate(nascidos_estimados = (tft_interp / 34) * mulheres_15_49)


# ==============================================================================
# TAXA DE MORTALIDADE IMPLÍCITA POR DÉCADA (1950-2022)
# ==============================================================================

# 1. Agrupar os nascimentos estimados por década
nascimentos_por_periodo <- df_nascimentos |>
  arrange(ano) |>
  mutate(
    periodo_censo = case_when(
      ano > 1950 & ano <= 1960 ~ 1960,
      ano > 1960 & ano <= 1970 ~ 1970,
      ano > 1970 & ano <= 1980 ~ 1980,
      ano > 1980 & ano <= 1991 ~ 1991,
      ano > 1991 & ano <= 2000 ~ 2000,
      ano > 2000 & ano <= 2010 ~ 2010,
      ano > 2010 & ano <= 2022 ~ 2022,
      TRUE ~ NA_real_
    )
  ) |>
  group_by(periodo_censo) |>
  summarise(total_nascidos_periodo = sum(nascidos_estimados, na.rm = TRUE)) |>
  drop_na()

# 2. Unir com a população observada (0-9 anos) no ano do censo
# 1. Garantimos que a população 0-9 está calculada na base principal
df_nascimentos <- df_nascimentos |>
  mutate(
    pop_obs_0_9 = `homens 0 a 4 anos` + `homens 5 a 9 anos` + 
      `mulheres 0 a 4 anos` + `mulheres 5 a 9 anos`
  )

# 2. Unir com a população observada (0-9 anos) no ano do censo
analise_final_infantil <- nascimentos_por_periodo |>
  left_join(
    df_nascimentos |> 
      dplyr::select(ano, pop_obs_0_9, regime_militar) |> 
      rename(periodo_censo = ano, pop_real_0_9 = pop_obs_0_9), # <--- CORREÇÃO AQUI
    by = "periodo_censo"
  ) |>
  mutate(
    # Taxa de Sobrevivência: Quantos dos que nasceram chegaram ao censo
    taxa_sobrevivencia = pop_real_0_9 / total_nascidos_periodo,
    
    # Taxa de Mortalidade Implícita (0-9 anos)
    mortalidade_implicita_percent = (1 - taxa_sobrevivencia) * 100
  )

# Visualizar o resultado
print(analise_final_infantil)

# ==============================================================================
# 1. PREPARAÇÃO DOS NASCIMENTOS (JANELA FIXA DE 10 ANOS)
# ==============================================================================

# Função para calcular a mortalidade de uma janela de 10 anos retroativos
calcular_mortalidade_janela <- function(ano_censo, df, divisor = 31) {
  # Seleciona apenas os 10 anos que compõem a coorte 0-9 do censo
  janela_nascimentos <- df |> 
    filter(ano <= ano_censo & ano > (ano_censo - 10)) |>
    summarise(total_nascidos = sum((tft_interp / divisor) * mulheres_15_49, na.rm = TRUE)) |>
    pull(total_nascidos)
  
  pop_real <- df |> 
    filter(ano == ano_censo) |> 
    mutate(pop_0_9 = `homens 0 a 4 anos` + `homens 5 a 9 anos` + 
             `mulheres 0 a 4 anos` + `mulheres 5 a 9 anos`) |>
    pull(pop_0_9)
  
  return(1 - (pop_real / janela_nascimentos))
}


# ==============================================================================
# 1. INTERPOLAÇÃO VIA SPLINE CÚBICO NATURAL
# ==============================================================================

# Criar a série anual usando Spline Natural
fecundidade_anual_spline <- fecundidade_decada |>
  rename(ano = ano_decada) |>
  # Criamos o objeto de spline
  (\(d) {
    s <- spline(d$ano, d$tft, n = (max(d$ano) - min(d$ano) + 1), method = "natural")
    tibble(ano = s$x, tft_spline = s$y)
  })()

# Unir com a base principal
df_nascimentos <- df_nascimentos |>
  left_join(fecundidade_anual_spline, by = "ano")

plot(x = fecundidade_anual_spline$ano, y = fecundidade_anual_spline$tft_spline)


# ==============================================================================
# 1. ATUALIZAÇÃO DA FUNÇÃO: PRIORIDADE PARA DADOS REAIS (SINASC)
# ==============================================================================

calcular_mortalidade_hibrida_spline <- function(ano_censo, df, divisor_media = 31) {
  
  # Filtramos a janela de 10 anos daquela coorte
  janela <- df |> 
    filter(ano <= ano_censo & ano > (ano_censo - 10))
  
  # Cálculo dos nascimentos na janela com Lógica Híbrida:
  # Se 'nascidos_vivos' existe (SINASC), usamos ele. 
  # Se é NA (pré-1994), estimamos via Spline de TFT.
  janela_nascimentos <- janela |>
    mutate(
      nasc_ano = ifelse(!is.na(nascidos_vivos), 
                        nascidos_vivos, 
                        (tft_spline / divisor_media) * mulheres_15_49)
    ) |>
    summarise(total = sum(nasc_ano, na.rm = TRUE)) |>
    pull(total)
  
  # População observada 0-9 no censo
  pop_real <- df |> 
    filter(ano == ano_censo) |> 
    summarise(pop_0_9 = `homens 0 a 4 anos` + `homens 5 a 9 anos` + 
                `mulheres 0 a 4 anos` + `mulheres 5 a 9 anos`) |>
    pull(pop_0_9)
  
  return(1 - (pop_real / janela_nascimentos))
}

# ==============================================================================
# 2. BOOTSTRAP HÍBRIDO (CALIBRADO PELO SINASC)
# ==============================================================================

set.seed(42)
anos_censo <- c(1960, 1970, 1980, 1991, 2000, 2010, 2022)

resultados_bootstrap_hibrido <- map_df(anos_censo, function(a) {
  reps <- replicate(1000, {
    # Incerteza do divisor (afeta apenas o período pré-SINASC)
    div_sim <- rnorm(1, 31, 0.4)
    calcular_mortalidade_hibrida_spline(a, df_nascimentos, divisor_media = div_sim)
  })
  
  tibble(
    ano = a,
    mortalidade_ponto = mean(reps) * 100,
    ic_inf = quantile(reps, 0.025) * 100,
    ic_sup = quantile(reps, 0.975) * 100,
    regime = ifelse(a %in% c(1970, 1980), "Regime Militar", "Democracia")
  )
})

# ==============================================================================
# CONTRAFACTUAL PARA MORTALIDADE INFANTIL (0-9 ANOS) – INTERPOLAÇÃO EXPONENCIAL
# ==============================================================================

library(dplyr)
library(ggplot2)
library(tidyr)

# 1. Preparar dados observados (já deve existir no seu ambiente)
# resultados_bootstrap_hibrido tem colunas: ano, mortalidade_ponto, ic_inf, ic_sup, regime

# 2. Calcular contrafactual exponencial com intervalo de confiança via Monte Carlo
# (use o mesmo código que você já escreveu e funcionou para gerar contra_df)
# Vou replicar aqui apenas para garantir:

set.seed(123)
n_sim <- 10000

ano_zero <- 1960
ano_final <- 2000

# Extrair valores e erros de 1960 e 1991
obs_ano_zero <- resultados_bootstrap_hibrido %>% filter(ano == ano_zero)
obs_ano_final <- resultados_bootstrap_hibrido %>% filter(ano == ano_final)

mort_zero <- obs_ano_zero$mortalidade_ponto
mort_final <- obs_ano_final$mortalidade_ponto
se_ano_zero <- (obs_ano_zero$ic_sup - obs_ano_zero$ic_inf) / (2 * 1.96)
se_ano_final <- (obs_ano_final$ic_sup - obs_ano_final$ic_inf) / (2 * 1.96)

delta_ano <- ano_final - ano_zero
r_obs <- log(mort_final / mort_zero) / delta_ano

# Simular
sim_ano_zero <- rnorm(n_sim, mean = mort_zero, sd = se_ano_zero) %>% pmax(0.01)
sim_ano_final <- rnorm(n_sim, mean = mort_final, sd = se_ano_final) %>% pmax(0.01)
r_sim <- log(sim_ano_final / sim_ano_zero) / delta_ano

anos_contra <- seq(ano_zero, ano_final, length.out = 100)
mat_sim <- sapply(1:n_sim, function(i) sim_ano_zero[i] * exp(r_sim[i] * (anos_contra - ano_zero)))

contra_df <- tibble(
  ano = anos_contra,
  mortalidade_contra = apply(mat_sim, 1, median),
  lwr = apply(mat_sim, 1, quantile, probs = 0.025),
  upr = apply(mat_sim, 1, quantile, probs = 0.975)
)

# 3. Preparar dados para o gráfico (ano numérico, manter regime)
resultados_num <- resultados_bootstrap_hibrido %>%
  mutate(ano_num = as.numeric(as.character(ano)))  # se 'ano' for fator, converta

# 4. Gráfico com visual idêntico ao original + contrafactual
# Vamos usar theme_ipea() se disponível; senão, tema manual consistente

# Verificar se ipeaplot está carregado
if (require(ipeaplot)) {
  tema_final <- theme_ipea(legend.position = "bottom")
} else {
  tema_final <- theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
      axis.line = element_line(color = "black", linewidth = 0.3),
      axis.ticks = element_line(color = "black"),
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
}

# Construir o gráfico
grafico_final_hibrido_com_contra <- ggplot() +
  # Barras originais (geom_col com x numérico)
  geom_col(data = resultados_num,
           aes(x = ano_num, y = mortalidade_ponto, fill = regime),
           width = 5, alpha = 0.85, color = "white") +
  # Barras de erro originais
  geom_errorbar(data = resultados_num,
                aes(x = ano_num, ymin = ic_inf, ymax = ic_sup),
                width = 1.5, color = "gray20", linewidth = 0.5) +
  # Rótulos de texto
  geom_text(data = resultados_num,
            aes(x = ano_num, y = mortalidade_ponto,
                label = paste0(round(mortalidade_ponto, 1), "%")),
            vjust = -1.2, fontface = "bold", size = 3.8) +
  # Contrafactual: faixa de incerteza (IC 95%)
  geom_ribbon(data = contra_df,
              aes(x = ano, ymin = lwr, ymax = upr),
              fill = "steelblue", alpha = 0.25) +
  # Linha contrafactual (mediana)
  geom_line(data = contra_df,
            aes(x = ano, y = mortalidade_contra),
            color = "blue", linetype = "dashed", linewidth = 1.2) +
  # Cores das barras (mesmas do original)
  scale_fill_manual(values = c("Democracia" = "#1f77b4", "Regime Militar" = "#d62728")) +
  # Ajuste dos eixos (sem duplicar escalas)
  scale_x_continuous(breaks = unique(resultados_num$ano_num),
                     labels = unique(resultados_num$ano_num)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  # Rótulos e título
  labs(
    title = "Mortalidade implícita na infância (0-9 anos) e contrafactual",
    subtitle = "Linha tracejada: interpolação exponencial 1960–2000 (cenário sem regime)",
    x = "Ano do censo",
    y = "Mortalidade acumulada na década (%)",
    fill = "Período",
    caption = "Faixa azul: IC 95% da linha contrafactual (bootstrap)."
  ) +
  tema_final

# Exibir
print(grafico_final_hibrido_com_contra)

save_ipeaplot(grafico_final_hibrido, "mortalidade implícita intercensitária 0-9 anos (spline híbrido)",
              path = pasta_saida, 
              width = 10, 
              height = 6,
              format = c("png", "eps"))


#=========================================================================
# A taxa de fecundidade foi influenciada pelo regime militar?
#========================================================================

# ----------------------------------------------------------------------
# PREPARAR DADOS DE FECUNDIDADE PARA MODELO COM INTERAÇÃO regime * tempo
# ----------------------------------------------------------------------

# Se você ainda não tem fecundidade_anual, recrie a partir dos censos (exemplo)
# fecundidade_decada <- tibble(ano = c(1940,1950,1960,1970,1980,1991,2000,2010,2022),
#                              tft = c(6.16,6.21,6.28,5.76,4.35,2.89,2.38,1.90,1.55))
# fecundidade_anual <- fecundidade_decada %>%
#   complete(ano = 1940:2022) %>%
#   mutate(tft_interp = approx(ano, tft, xout = ano, rule = 2)$y)

# Criar data frame com variáveis regime e tempo
dados_fec <- fecundidade_anual %>%
  filter(ano >= 1950, ano <= 1991) %>%  # mesmo período da análise principal
  mutate(
    regime_militar = ifelse(ano >= 1964 & ano <= 1985, 1, 0),
    tempo = ano - 1964,  # centralizado no golpe
    regime_f = factor(regime_militar, labels = c("Fora do regime", "Durante o regime"))
  )

# Modelo de regressão segmentada (interação)
modelo_fec <- lm(tft_interp ~ regime_militar * tempo, data = dados_fec)
summary(modelo_fec)

# ----------------------------------------------------------------------
# GRÁFICO COM PONTOS E RETAS SEPARADAS (MESMO ESTILO DO DÉFICIT)
# ----------------------------------------------------------------------

grafico_fecundidade <- ggplot(dados_fec, aes(x = ano, y = tft_interp)) +
  geom_point(alpha = 0.5, size = 2.5) +
  geom_smooth(aes(color = regime_f, group = regime_f),
              method = "lm", se = FALSE, linewidth = 1.2) +
  geom_vline(xintercept = c(1964, 1985), linetype = "dashed", color = "gray40") +
  scale_color_manual(values = c("grey50", "#D62728")) +
  labs(
    title = "Taxa de Fecundidade Total (TFT) e regime militar",
    subtitle = "Regressão linear separada antes/durante o regime",
    x = "Ano", y = "Número médio de filhos por mulher",
    color = "Período"
  ) +
  theme_ipea(legend.position = "bottom")

print(grafico_fecundidade)

# Salvar (opcional)
save_ipeaplot(grafico_fecundidade, "fecundidade_regime",
              path = pasta_saida, format = c("eps", "png"))

#=======================================================================
# Os excessos estimados de homicídios e outras mortes violentas explicam o déficit?
# Déficit de sobrevivência específico jovem-masculino
#=======================================================================

# ----------------------------------------------------------------------
# REGRESSÕES: DÉFICIT JOVEM (15‑29) vs. EXCESSOS DE MORTES VIOLENTAS
# ----------------------------------------------------------------------

# 1. Preparar o data frame com as variáveis de interesse
#    Assumindo que `dados_1960_2022` contém as colunas:
#    - ano
#    - excesso_homicidios_cf1, cf2, cf3, cfex (ou cf4)
#    - excesso_mce_cf1, cf2, cf3, cfex
#    - população (para taxa, se quiser, mas usaremos números brutos)
#    E que `analysis_data` (do script 3) contém `espec_deficit` (déficit específico masculino 15-29)
#    Vamos combinar por ano.

# Ajuste os nomes conforme seu ambiente
deficit_jovem <- analysis_data %>% select(ano, espec_deficit)

# Juntar com os excessos (usando dados_1960_2022 ou df_raw)
# Se os excessos estiverem em dados_1960_2022, use:
dados_reg <- dados_1960_2022 %>%
  select(ano, starts_with("excesso_homicidios_cf"), starts_with("excesso_mce_cf")) %>%
  left_join(deficit_jovem, by = "ano") %>%
  filter(ano >= 1964 & ano <= 1985)  # período do regime (opcional, mas recomendado)

# Verificar nomes
names(dados_reg)

# 2. Definir lista de variáveis independentes (excessos)
vars_excesso <- c(
  "excesso_homicidios_cf1", "excesso_homicidios_cf2", "excesso_homicidios_cf3", "excesso_homicidios_cfex",
  "excesso_mce_cf1", "excesso_mce_cf2", "excesso_mce_cf3", "excesso_mce_cfex"
)

# Rótulos para os gráficos
rotulos <- c(
  "Excesso homicídios – CF1 (efeito amplo)",
  "Excesso homicídios – CF2 (duração regime)",
  "Excesso homicídios – CF3 (decaimento pós-78)",
  "Excesso homicídios – CF4 (extremo)",
  "Excesso outras causas externas – CF1",
  "Excesso outras causas externas – CF2",
  "Excesso outras causas externas – CF3",
  "Excesso outras causas externas – CF4"
)

# 3. Função para criar gráfico de regressão
plot_regressao <- function(dados, x_var, y_var = "espec_deficit", titulo) {
  # Modelo linear
  modelo <- lm(as.formula(paste(y_var, "~", x_var)), data = dados)
  s <- summary(modelo)
  
  # Extrair coeficientes e estatísticas
  intercepto <- coef(modelo)[1]
  inclinacao <- coef(modelo)[2]
  r2 <- s$r.squared
  p_val <- s$coefficients[2, 4]
  
  # Formatar equação (tratando valores grandes)
  eq_label <- paste0(
    "y = ", round(intercepto, 0), " + ", formatC(inclinacao, format = "e", digits = 2), "x\n",
    "R² = ", round(r2, 3), "\n",
    "p = ", ifelse(p_val < 0.001, "< 0.001", round(p_val, 4))
  )
  
  # Gráfico
  ggplot(dados, aes_string(x = x_var, y = y_var)) +
    geom_point(size = 2.5, alpha = 0.7, color = "#1f77b4", shape = 16) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.2, color = "steelblue", fill = "steelblue") +
    annotate("label", x = Inf, y = -Inf, hjust = 1.05, vjust = -0.4,
             label = eq_label, size = 3.5, color = "black", fill = "white", label.size = 0.3) +
    labs(
      title = titulo,
      x = "Excesso de mortes (nº de vítimas)",
      y = "Déficit específico masculino (15-29 anos) – pessoas-ano"
    ) +
    theme_ipea() +
    scale_x_continuous(labels = scales::comma) +
    scale_y_continuous(labels = scales::comma)
}

# 4. Gerar e exibir cada gráfico
# (você pode salvá‑los em uma lista ou imprimir um por um)
for (i in seq_along(vars_excesso)) {
  p <- plot_regressao(dados_reg, vars_excesso[i], "espec_deficit", rotulos[i])
  print(p)
  # Opcional: salvar
  save_ipeaplot(p, paste0("regressao_", vars_excesso[i]), path = pasta_saida, format = c("eps", "png"))
}

#######################################################################
#Se for figura única:
# ----------------------------------------------------------------------
# REGRESSÕES: DÉFICIT JOVEM (15‑29) vs. EXCESSOS DE MORTES VIOLENTAS
# FIGURA ÚNICA COM FACETAS
# ----------------------------------------------------------------------

library(tidyverse)
library(patchwork) # opcional para organizar

# 1. Preparar dados no formato longo (uma linha por ano x tipo de excesso)
dados_long_reg <- dados_reg %>%
  pivot_longer(
    cols = starts_with("excesso_homicidios_cf") | starts_with("excesso_mce_cf"),
    names_to = "variavel",
    values_to = "excesso"
  ) %>%
  filter(!is.na(excesso), !is.na(espec_deficit))

# 2. Criar rótulos descritivos para cada variável
rotulos <- c(
  "excesso_homicidios_cf1" = "Homicídios (CF1 – efeito amplo)",
  "excesso_homicidios_cf2" = "Homicídios (CF2 – duração regime)",
  "excesso_homicidios_cf3" = "Homicídios (CF3 – decaimento pós-78)",
  "excesso_homicidios_cfex" = "Homicídios (CF4 – extremo)",
  "excesso_mce_cf1" = "Outras causas externas (CF1)",
  "excesso_mce_cf2" = "Outras causas externas (CF2)",
  "excesso_mce_cf3" = "Outras causas externas (CF3)",
  "excesso_mce_cfex" = "Outras causas externas (CF4)"
)

dados_long_reg <- dados_long_reg %>%
  mutate(variavel_label = recode(variavel, !!!rotulos))

# 3. Calcular os modelos e extrair estatísticas para cada variável
estatisticas <- dados_long_reg %>%
  group_by(variavel, variavel_label) %>%
  summarise(
    intercepto = coef(lm(espec_deficit ~ excesso))[1],
    inclinacao = coef(lm(espec_deficit ~ excesso))[2],
    r2 = summary(lm(espec_deficit ~ excesso))$r.squared,
    p_val = summary(lm(espec_deficit ~ excesso))$coefficients[2, 4],
    .groups = "drop"
  ) %>%
  mutate(
    eq_label = paste0(
      "y = ", round(intercepto, 0), " + ", formatC(inclinacao, format = "e", digits = 2), "x\n",
      "R² = ", round(r2, 3), "\n",
      "p = ", ifelse(p_val < 0.001, "< 0.001", round(p_val, 4))
    )
  )

# 4. Juntar os rótulos aos dados originais
dados_plot <- dados_long_reg %>%
  left_join(estatisticas %>% select(variavel, eq_label), by = "variavel")

# 5. Criar o gráfico com facetas
grafico_reg_facet <- ggplot(dados_plot, aes(x = excesso, y = espec_deficit)) +
  geom_point(size = 1.5, alpha = 0.5, color = "#1f77b4") +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2, color = "steelblue", fill = "steelblue") +
  geom_text(data = estatisticas, 
            aes(x = -Inf, y = -Inf, label = eq_label),
            hjust = -0.1, vjust = -0.5, size = 3, inherit.aes = FALSE) +
  facet_wrap(~ variavel_label, scales = "free", ncol = 2) +
  labs(
    title = "Relação entre excesso de mortes violentas e déficit de jovens (15-29 anos)",
    subtitle = "Regressões lineares para cada cenário contrafactual – Brasil, 1964-1985",
    x = "Excesso de mortes (nº de vítimas)",
    y = "Déficit específico masculino (15-29 anos) – pessoas-ano"
  ) +
  theme_ipea() +
  theme(
    strip.background = element_rect(fill = "#f0f0f0", color = NA),
    strip.text = element_text(face = "bold", size = 9)
  ) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)

print(grafico_reg_facet)

# Salvar (opcional)
# ggsave("regressoes_facet_excessos.png", grafico_reg_facet, width = 10, height = 12, dpi = 300)
# save_ipeaplot(grafico_reg_facet, "regressoes_excessos_deficit", format = c("eps", "png"))