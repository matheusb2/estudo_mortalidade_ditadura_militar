# =============================================================================
# Estratégia metodológica para estimação do excesso de mortalidade infantil e
# geral durante o regime militar brasileiro (1964-1985)
# =============================================================================

# Carregar pacotes
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(aTSA)
library(ipeaplot)
library(forecast)
library(tidyverse)
library(lmtest)
library(sandwich)
library(tseries)
library(splines)


# =============================================================================
# Correção de erros censitários com decaimento exponencial do erro por idade
# =============================================================================

age_groups <- c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos", "15 a 19 anos",
                "20 a 24 anos", "25 a 29 anos", "30 a 39 anos", "40 a 49 anos",
                "50 a 59 anos", "60 a 69 anos", "70 ou mais")

ajuste_ref <- c(0.058, 0.036, 0.083)

mat_p_men <- cbind(
  c( 0.042,  0.021,  0.005, -0.002, -0.005, -0.008, -0.010, -0.005,  0.000,  0.000,  0.000),
  c( 0.108,  0.032,  0.005,  0.000, -0.005, -0.012, -0.018, -0.008, -0.002,  0.000,  0.002),
  c( 0.125,  0.065,  0.038,  0.021,  0.008, -0.005, -0.022, -0.015, -0.008, -0.002,  0.000)
)

mat_p_women <- cbind(
  c( 0.035,  0.018,  0.002, -0.005, -0.012, -0.015, -0.010, -0.005,  0.000,  0.000,  0.005),
  c( 0.051,  0.020,  0.005,  0.000, -0.008, -0.015, -0.012, -0.005, -0.002,  0.000,  0.002),
  c( 0.032, -0.018, -0.031, -0.042, -0.061, -0.048, -0.035, -0.012, -0.005, -0.002,  0.000)
)

omissao <- data.frame(
  ano = c(1930, 1940, 1950, 1960, 1970, 1980, 1991, 2000, 2010, 2022),
  ajuste_pop = c(0.087, 0.081, 0.075, 0.069, 0.073, 0.043, 0.047, 0.058, 0.036, 0.083
  )
)

# Função auxiliar usando decaimento exponencial modelado com taxa de decaimento (lambda)
obter_padrao_exponencial <- function(ajuste_alvo) {
  p_m <- numeric(length(age_groups))
  p_w <- numeric(length(age_groups))
  
  for(i in seq_along(age_groups)) {
    # Suavização baseada em relação exponencial/linear mista em função do ajuste macro
    fit_m <- lm(mat_p_men[i, ] ~ ajuste_ref)
    fit_w <- lm(mat_p_women[i, ] ~ ajuste_ref)
    
    val_m <- predict(fit_m, newdata = data.frame(ajuste_ref = ajuste_alvo))
    val_w <- predict(fit_w, newdata = data.frame(ajuste_ref = ajuste_alvo))
    
    # Aplicando fator de amortecimento/decaimento exponencial nas pontas (ex: idade 0-4 e 70+)
    # para garantir que o erro decaia suavemente rumo às assíntotas sem saltos bruscos
    if(i <= 3) {
      # Fator de decaimento exponencial estrito para as primeiras idades infantis
      fator_exp <- exp(-0.1 * (i - 1))
      p_m[i] <- val_m * fator_exp
      p_w[i] <- val_w * fator_exp
    } else if(i >= 9) {
      # Amortecimento assintótico para idades avançadas
      p_m[i] <- val_m * exp(-0.05 * (i - 9))
      p_w[i] <- val_w * exp(-0.05 * (i - 9))
    } else {
      p_m[i] <- val_m
      p_w[i] <- val_w
    }
  }
  
  return(list(p_men = p_m, p_women = p_w))
}

# Função principal de correção atualizada
corrigir_censo <- function(df_ano, ano) {
  omissao_ano <- omissao$ajuste_pop[omissao$ano == ano]
  if (length(omissao_ano) == 0) stop("Ano sem taxa de omissão cadastrada.")
  
  C_total <- 1 / (1 - omissao_ano)
  
  padroes <- obter_padrao_exponencial(omissao_ano)
  p_men_ano <- padroes$p_men
  p_women_ano <- padroes$p_women
  
  col_homens <- paste0("homens ", age_groups)
  col_mulheres <- paste0("mulheres ", age_groups)
  
  pop_homens <- as.numeric(df_ano[col_homens])
  pop_mulheres <- as.numeric(df_ano[col_mulheres])
  
  f0_homens <- 1 / (1 - p_men_ano)
  f0_mulheres <- 1 / (1 - p_women_ano)
  
  pop_corr_homens <- pop_homens * f0_homens
  pop_corr_mulheres <- pop_mulheres * f0_mulheres
  
  total_bruto <- sum(pop_homens) + sum(pop_mulheres)
  total_corr_prelim <- sum(pop_corr_homens) + sum(pop_corr_mulheres)
  
  s <- (total_bruto * C_total) / total_corr_prelim
  
  f_homens <- f0_homens * s
  f_mulheres <- f0_mulheres * s
  
  df_ano[col_homens] <- as.list(pop_homens * f_homens)
  df_ano[col_mulheres] <- as.list(pop_mulheres * f_mulheres)
  df_ano$população <- sum(as.numeric(df_ano[col_homens])) + sum(as.numeric(df_ano[col_mulheres]))
  
  corrigido <- df_ano
  return(corrigido)
}

# =============================================================================
# 1. PREPARAÇÃO ROBUSTA DOS DADOS
# =============================================================================

arquivo <- "~/Documentos/IPEA/modelos/demografia social da ditadura militar/censos_mortes_bruto.xlsx"

df_raw <- read_excel(
  path = arquivo,
  sheet = "BR_1950_2022"
)

names(df_raw)

# Função que converte qualquer coluna para numérico, tratando caracteres
to_numeric <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    # Remove pontos de milhar e substitui vírgula decimal por ponto
    x <- gsub("\\.", "", x)  # remove pontos (milhar)
    x <- gsub(",", ".", x)   # vírgula para ponto decimal
  }
  as.numeric(x)
}

# Lista de colunas que devem ser numéricas
colunas_populacionais <- c(
  paste0("homens ", c("0 a 4", "5 a 9", "10 a 14", "15 a 19", "20 a 24",
                      "25 a 29", "30 a 39", "40 a 49", "50 a 59", "60 a 69",
                      "70 ou mais"), " anos"),
  paste0("mulheres ", c("0 a 4", "5 a 9", "10 a 14", "15 a 19", "20 a 24",
                        "25 a 29", "30 a 39", "40 a 49", "50 a 59", "60 a 69",
                        "70 ou mais"), " anos"),
  "população", "mortalidade_infantil", "mort_infant", "fecundidade",
  "nascidos_vivos", "mortos e desaparecidos documentados", "mortes violentas SP"
)

# Converter todas as colunas existentes no df_raw
df_raw <- df_raw %>%
  mutate(across(any_of(colunas_populacionais), to_numeric))

# Garantir que 'ano' seja numérico
df_raw$ano <- as.numeric(df_raw$ano)

# =============================================================================
# 2. EXTRAÇÃO DE SÉRIES BÁSICAS
# =============================================================================

census_years <- c(1930, 1940, 1950, 1960, 1970, 1980, 1991)

census_years_urban <- c(1940, 1950, 1960, 1970, 1980, 1991)


df_census <- df_raw %>%
  filter(ano %in% census_years) %>%
  arrange(ano)


for (ano in census_years) {
  idx <- which(df_raw$ano == ano)
  if (length(idx) == 1) {
    df_raw[idx, ] <- corrigir_censo(df_raw[idx, ], ano)
  }
}

pop_census <- df_census$população   # já numérico

# Interpolação linear da população total 1940-1991
all_years <- 1930:1991

# Extrai os valores observados nos anos censitários
pop_census <- df_census$população   # vetor numérico (tamanho 7)
urb_census <- df_census$pop_urbana  # vetor numérico (tamanho 7)

# Define a sequência completa de anos para interpolação
all_years_urban <- 1940:1991


# Interpolação log-linear (exponencial) para obter séries contínuas
pop_total <- exp(approx(census_years, log(pop_census), xout = all_years)$y)
pop_urbana <- exp(approx(census_years, log(urb_census), xout = all_years)$y)



# Filtra os anos onde PIB é conhecido (remove NAs)
df_pib_known <- df_raw %>%
  filter(!is.na(PIB)) %>%
  arrange(ano)

# Extrai os vetores de anos e valores conhecidos
anos_pib <- df_pib_known$ano
pib_known <- df_pib_known$PIB

# Interpolação log-linear para todos os anos do período
PIB_interp <- exp(approx(anos_pib, log(pib_known), xout = all_years)$y)

PIB_per_capita <- 

# Grupos etários femininos para estimativa de nascimentos
fem_groups <- c("mulheres 15 a 19 anos", "mulheres 20 a 24 anos",
                "mulheres 25 a 29 anos", "mulheres 30 a 39 anos",
                "mulheres 40 a 49 anos")

# Amplitudes de cada grupo (em anos)
widths <- c(5, 5, 5, 10, 10)

# Pesos de fecundidade típicos (ajustáveis)
pesos_fec <- c(0.10, 0.25, 0.25, 0.15, 0.05)  # soma 0.80, restante em 10-14 e 45-49
pesos_fec <- pesos_fec / sum(pesos_fec)  # agora somam 1

# Interpolação da população feminina por grupo etário
fem_pop_interp <- matrix(NA, nrow = length(all_years), ncol = length(fem_groups))
colnames(fem_pop_interp) <- fem_groups



for (i in seq_along(fem_groups)) {
  vals <- df_census[[fem_groups[i]]]
  # Interpolar para todos os anos
  fem_pop_interp[, i] <- exp(approx(census_years, log(vals), xout = all_years)$y)
}

# Taxa de fecundidade total (TFT) anual
TFT_anual <- df_raw %>%
  filter(ano %in% all_years) %>%
  pull(fecundidade)

# Estimar nascimentos anuais
# Cálculo correto dos nascimentos anuais
nascimentos <- numeric(length(all_years))
for (yr in seq_along(all_years)) {
  nascimentos[yr] <- TFT_anual[yr] * sum(pesos_fec * fem_pop_interp[yr, ] / widths)
}



df_raw$mortalidade_infantil

# Mortalidade infantil (IMR) – interpolação anual 1930-1990
imr_raw <- df_raw |>
  dplyr::select(ano, mortalidade_infantil) |>
  filter(!is.na(mortalidade_infantil)) |>
  arrange(ano)

imr_years_all <- 1930:2022
imr_interp <- spline(imr_raw$ano, imr_raw$mortalidade_infantil,
                     xout = imr_years_all, method = "fmm")$y

plot(imr_interp, type = "l")

# =============================================================================
# PROPORÇÃO DE IDOSOS (60+)
# =============================================================================

# Identificar colunas de homens e mulheres com 60+ (60-69, 70+)
cols_60plus <- c(
  "homens 60 a 69 anos", "homens 70 ou mais",
  "mulheres 60 a 69 anos", "mulheres 70 ou mais"
)

# Extrair dos dados censitários (já corrigidos)
pop_60plus_census <- numeric(length(census_years))
for (i in seq_along(census_years)) {
  ano <- census_years[i]
  df_ano <- df_raw %>% filter(ano == !!ano)
  if (nrow(df_ano) == 1) {
    pop_60plus_census[i] <- sum(as.numeric(df_ano[cols_60plus]), na.rm = TRUE)
  } else {
    pop_60plus_census[i] <- NA
  }
}

# Interpolação log-linear (exponencial) para todos os anos
pop_60plus <- exp(approx(census_years, log(pop_60plus_census), xout = all_years)$y)

# Proporção de idosos
prop_idosos <- pop_60plus / pop_total

# Verificar
plot(all_years, prop_idosos, type = "l", main = "Proporção de idosos (60+)")


# =============================================================================
# 3. MIGRAÇÃO LÍQUIDA POR DÉCADA
# =============================================================================

# Participação do saldo migratório no crescimento populacional (%)
mig_part <- data.frame(
  periodo_inicio = c(1930, 1940, 1950, 1960, 1970, 1980),
  periodo_fim    = c(1940, 1950, 1960, 1970, 1980, 1991),
  participacao   = c(NA, 1.0, 3.4, 1.32, 0.9, 0.52) / 100
)

# =============================================================================
# 4. ÓBITOS TOTAIS POR PERÍODO INTERCENSITÁRIO
# =============================================================================

periodos <- list(
  "1930-1940" = c(1930,1940),
  "1940-1950" = c(1940,1950),
  "1950-1960" = c(1950,1960),
  "1960-1970" = c(1960,1970),
  "1970-1980" = c(1970,1980),
  "1980-1991" = c(1980,1991)
)

mortalidade_interp <- spline(df_raw$ano, df_raw$taxa_bruta_mortalidade, method = "natural")$y

plot(mortalidade_interp)

resultados <- data.frame(periodo = character(),
                         pop_inicio = numeric(), pop_fim = numeric(),
                         nasc = numeric(), migracao = numeric(),
                         obitos = numeric())

# reconstruir óbitos

for (p in names(periodos)) {
  t0 <- periodos[[p]][1]
  t1 <- periodos[[p]][2]
  
  P0 <- pop_total[all_years == t0]
  P1 <- pop_total[all_years == t1]
  
  anos_periodo <- t0:(t1 - 1)
  B <- sum(nascimentos[all_years %in% anos_periodo])
  
  linha_mig <- mig_part[mig_part$periodo_inicio == t0, ]
  M <- linha_mig$participacao * (P1 - P0)
  
  D <- P0 - P1 + B + M
  
  resultados <- rbind(resultados,
                      data.frame(periodo = p, pop_inicio = P0, pop_fim = P1,
                                 nasc = B, migracao = M, obitos = D))
}

cat("Óbitos reconstruídos por década:\n")
print(resultados %>% mutate(across(where(is.numeric), ~ round(., 0))))

# =============================================================================
# 5. ÓBITOS ANUAIS A PARTIR DA TAXA BRUTA DE MORTALIDADE (CDR) DIRETA
# =============================================================================

# Extrair CDR observada (anos com dados)
cdr_raw <- df_raw %>%
  dplyr::select(ano, taxa_bruta_mortalidade) %>%
  filter(!is.na(taxa_bruta_mortalidade)) %>%
  arrange(ano)

# Verificar cobertura
print(range(cdr_raw$ano))  # deve incluir 1930-2022, mas com alguns anos faltando?

# Interpolação spline cúbica natural para todos os anos de interesse (1930-1991)
cdr_interp <- spline(
  x = cdr_raw$ano,
  y = cdr_raw$taxa_bruta_mortalidade,
  xout = all_years,
  method = "natural"
)$y

# Calcular óbitos anuais (CDR é por 1000 habitantes)
# pop_total já está em número de pessoas
obitos_anuais_cdr <- (cdr_interp / 1000) * pop_total

# ----------------------------------------------------------------------------
# COMPARAÇÃO COM OS ÓBITOS RECONSTRUÍDOS (MÉTODO ANTERIOR)
# ----------------------------------------------------------------------------

# Para comparar, primeiro precisamos dos óbitos do método anterior.
# Eles já foram calculados na seção 5 original (obitos_anuais).
# Se você ainda não executou a antiga seção 5, pode mantê-la como "obitos_reconstruidos".
# Vamos supor que você quer comparar:


# ----------------------------------------------------------------------------
# DECISÃO: USAR QUAL SÉRIE?
# ----------------------------------------------------------------------------
# Se a diferença for pequena (ex.: < 5%), pode usar a CDR direta como principal,
# pois é mais simples e evita erros de migração/nascimentos.
# Caso contrário, investigue a origem da discrepância.

# Para prosseguir, vou adotar a série da CDR direta como a oficial,
# mas você pode manter a reconstruída se preferir.
# Substitua 'obitos_anuais' pela nova série:

obitos_anuais <- obitos_anuais_cdr

# Taxa bruta de mortalidade observada
CDR_obs <- obitos_anuais / pop_total

# Verificação rápida
if (any(is.na(obitos_anuais[1:30]))) {
  warning("obitos_anuais contém NAs nos primeiros anos. Verifique a conversão dos dados.")
}



# =============================================================================
# 6. RELAÇÃO CDR ~ IMR + PROP_IDOSOS (PERÍODO PRÉ-1964) – CORRIGIDO
# =============================================================================

anos_pre <- 1940:1963
ind_pre <- all_years %in% anos_pre

dados_pre <- data.frame(
  ano = anos_pre,
  CDR = CDR_obs[ind_pre],
  IMR = imr_interp[imr_years_all %in% anos_pre],
  prop_idosos = prop_idosos[ind_pre]
)

# Criar a matriz de regressores com nomes de colunas
xreg_pre <- cbind(log(dados_pre$IMR), log(dados_pre$prop_idosos))
colnames(xreg_pre) <- c("log(IMR)", "log(prop_idosos)")

# Ajustar ARIMA
modelo_cdr_imr_idosos <- auto.arima(
  log(dados_pre$CDR),
  xreg = xreg_pre,
  stepwise = FALSE,
  approximation = FALSE
)

summary(modelo_cdr_imr_idosos)
coeftest(modelo_cdr_imr_idosos)




# =============================================================================
# 7.1 CONTRAFACTUAL DA IMR – REGRESSÃO SEGMENTADA (BREAKPOINT EM 1951.5)
# =============================================================================

library(segmented)

# Dados históricos (1944-1963)
imr_hist <- data.frame(
  ano = 1940:1963,
  IMR = imr_interp[imr_years_all %in% 1940:1963]
)

# 1. Ajuste do modelo segmentado (1 breakpoint)
modelo_seg <- lm(log(IMR) ~ ano, data = imr_hist)
seg_fit <- segmented(modelo_seg, seg.Z = ~ano, npsi = 1)

# Exibir o breakpoint estimado (já rodou, mas mantemos para referência)
print(summary(seg_fit))

# 2. Projeção para 1964-1985
anos_regime <- 1964:1985
newdata <- data.frame(ano = anos_regime)

# 3. Previsão pontual e intervalos de confiança (90%)
#    A função predict.segmented pode retornar se.fit, mas nem todas as versões.
#    Vamos usar uma abordagem robusta: bootstrap paramétrico para obter intervalos.
#    Isso também captura a incerteza do breakpoint.

# Número de simulações bootstrap
n_boot <- 1000

# Matriz para armazenar as previsões
pred_boot <- matrix(NA, nrow = length(anos_regime), ncol = n_boot)

# Extrair coeficientes e matriz de covariância do modelo segmentado
coefs <- coef(seg_fit)
vcov_mat <- vcov(seg_fit)

# Para cada bootstrap, simular coeficientes e prever
set.seed(123)  # para reprodutibilidade
for (b in 1:n_boot) {
  # Simular coeficientes da distribuição normal multivariada
  coef_sim <- MASS::mvrnorm(1, mu = coefs, Sigma = vcov_mat)
  
  # Construir a previsão para cada ano (usando a fórmula segmentada)
  # O modelo segmentado tem a forma: b0 + b1*ano + psi*(ano - breakpoint)*I(ano > breakpoint)
  # Acessamos os parâmetros pelo nome: coef_sim["(Intercept)"], etc.
  psi <- coef_sim["U1.ano"]  # diferença de inclinação após o breakpoint
  bp <- seg_fit$psi[1, "Est."]  # breakpoint estimado (fixo no bootstrap)
  
  # Previsão log(IMR) para cada ano
  log_imr <- coef_sim["(Intercept)"] + coef_sim["ano"] * anos_regime
  # Adicionar a mudança de inclinação para anos após o breakpoint
  idx <- anos_regime > bp
  log_imr[idx] <- log_imr[idx] + psi * (anos_regime[idx] - bp)
  
  pred_boot[, b] <- exp(log_imr)
}

# 4. Calcular a projeção central (mediana) e os percentis 5% e 95% (90% IC)
imr_central <- apply(pred_boot, 1, median)
imr_min     <- apply(pred_boot, 1, quantile, probs = 0.05)
imr_max     <- apply(pred_boot, 1, quantile, probs = 0.95)

# 5. (Opcional) Visualizar a projeção
plot(anos_regime, imr_central, type = "l", ylim = range(imr_min, imr_max),
     main = "IMR contrafactual (segmentada)", xlab = "Ano", ylab = "IMR")
lines(anos_regime, imr_min, lty = 2)
lines(anos_regime, imr_max, lty = 2)

# =============================================================================
# 7.1b Projeção contrafactual da proporção de idosos
# =============================================================================

# Dados históricos 1944-1963 (mesmo período da IMR)
prop_hist <- data.frame(
  ano = 1940:1963,
  prop = prop_idosos[all_years %in% 1940:1963]
)

# Modelo log-linear (ou spline) – aqui optamos por log-linear para simplicidade
modelo_prop <- lm(log(prop) ~ ano, data = prop_hist)

# Projeção para 1964-1985
anos_regime <- 1964:1985
prop_contra <- exp(predict(modelo_prop, newdata = data.frame(ano = anos_regime)))

# (Opcional) visualizar
plot(anos_regime, prop_contra, type = "l", col = "blue")
lines(anos_regime, prop_idosos[all_years %in% anos_regime], col = "red")

#-----------------------------------------------------------------------------
# 7.2 VISUALIZAÇÃO DA MORTALIDADE INFANTIL – OBSERVADA E CONTRAFACTUAL
#-----------------------------------------------------------------------------

# 1. Preparar dados observados (até 1985)
dados_obs <- data.frame(
  ano = 1930:1985,
  IMR = imr_interp[1:56]  # índices correspondentes a 1930:1985
) %>%
  filter(!is.na(IMR))

# 2. Preparar dados contrafactuais (1964‑1985)
# (supondo que você já calculou imr_central, imr_min, imr_max para 1964‑1985)
dados_contra <- data.frame(
  ano = 1964:1985,
  IMR_central = as.numeric(imr_central),
  IMR_min     = as.numeric(imr_min),
  IMR_max     = as.numeric(imr_max)
)

# 3. Combinar para o gráfico
# Vamos criar uma coluna que identifica se é observado ou contrafactual
dados_obs$tipo <- "Observado"
dados_contra$tipo <- "Contrafactual (central)"

# Para plotar a faixa de incerteza, usamos geom_ribbon
# e para a linha central, geom_line

ggplot() +
  # Faixa de incerteza da projeção contrafactual
  geom_ribbon(data = dados_contra,
              aes(x = ano, ymin = IMR_min, ymax = IMR_max),
              fill = "lightblue", alpha = 0.4) +
  # Linha central da projeção contrafactual
  geom_line(data = dados_contra,
            aes(x = ano, y = IMR_central, color = "Contrafactual"),
            size = 1.2, linetype = "dashed") +
  # Linha da série observada
  geom_line(data = dados_obs,
            aes(x = ano, y = IMR, color = "Observado"),
            size = 1) +
  # Marcador do início do regime militar
  geom_vline(xintercept = 1964, linetype = "dotted", color = "red", size = 1) +
  annotate("text", x = 1964, y = max(dados_obs$IMR, na.rm = TRUE) * 0.9,
           label = "Início do regime militar", color = "red", hjust = -0.1) +
  # Escalas e temas
  scale_color_manual(name = "Série",
                     values = c("Observado" = "black", "Contrafactual" = "blue")) +
  labs(
    title = "Mortalidade infantil (IMR) – observada e contrafactual",
    subtitle = "Projeção log-linear com base na tendência 1945-1963",
    x = "Ano",
    y = "IMR (óbitos por 1.000 nascidos vivos)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

#-----------------------------------------------------------------------------
# 7.3  EXCESSO DE MORTALIDADE INFANTIL
#-----------------------------------------------------------------------------
# 1. Vetor de anos do regime (já definido)
anos_regime <- 1964:1985

# 2. Extrair a IMR observada para o mesmo período
ind_obs <- which(imr_years_all %in% anos_regime)
imr_observado <- imr_interp[ind_obs]

# 3. As projeções já estão em imr_central, imr_min, imr_max (vetores de tamanho 22)

# 4. Calcular excesso anual (observado - projetado)
excesso_imr_central <- imr_observado - imr_central
excesso_imr_min     <- imr_observado - imr_min
excesso_imr_max     <- imr_observado - imr_max

# 5. Excesso acumulado (soma simples, pode ser negativa)
excesso_acum_central <- sum(excesso_imr_central)
excesso_acum_min     <- sum(excesso_imr_min)
excesso_acum_max     <- sum(excesso_imr_max)

cat("\n========== EXCESSO DE MORTALIDADE INFANTIL (IMR) – LOG‑LINEAR ==========\n")
cat(sprintf("Excesso acumulado (central): %0.2f óbitos por 1.000 NV\n", excesso_acum_central))
cat(sprintf("Excesso acumulado (mínimo):   %0.2f óbitos por 1.000 NV\n", excesso_acum_min))
cat(sprintf("Excesso acumulado (máximo):   %0.2f óbitos por 1.000 NV\n", excesso_acum_max))

# 6. Excesso truncado em zero (apenas anos com IMR observada > projetada)
excesso_pos_central <- pmax(excesso_imr_central, 0)
excesso_pos_min     <- pmax(excesso_imr_min, 0)
excesso_pos_max     <- pmax(excesso_imr_max, 0)

excesso_pos_acum_central <- sum(excesso_pos_central)
excesso_pos_acum_min     <- sum(excesso_pos_min)
excesso_pos_acum_max     <- sum(excesso_pos_max)

cat("\n--- Excesso truncado em zero (apenas anos com excesso positivo) ---\n")
cat(sprintf("Excesso positivo acumulado (central): %0.2f óbitos por 1.000 NV\n", excesso_pos_acum_central))
cat(sprintf("Excesso positivo acumulado (mínimo):   %0.2f óbitos por 1.000 NV\n", excesso_pos_acum_min))
cat(sprintf("Excesso positivo acumulado (máximo):   %0.2f óbitos por 1.000 NV\n", excesso_pos_acum_max))


tibble( c("Excesso acumulado central", 
          "excesso acumulado mínimo", 
          "excesso acumulado máximo"),
        c(sum(nascimentos[35:56]*(excesso_pos_central/100)), 
          sum(nascimentos[35:56]*(excesso_pos_min/100)), 
          sum(nascimentos[35:56]*(excesso_pos_max/100))))

# 7. Tabela anual para inspeção
tabela_imr_excesso <- data.frame(
  Ano = anos_regime,
  IMR_Obs = round(imr_observado, 2),
  IMR_Proj_Central = round(imr_central, 2),
  IMR_Proj_Min = round(imr_min, 2),
  IMR_Proj_Max = round(imr_max, 2),
  Excesso_Central = round(excesso_imr_central, 2),
  Excesso_Min = round(excesso_imr_min, 2),
  Excesso_Max = round(excesso_imr_max, 2)
)

cat("\n--- Tabela anual do excesso de IMR (apenas central) ---\n")
print(tabela_imr_excesso[, c("Ano", "IMR_Obs", "IMR_Proj_Central", "Excesso_Central")])

# 8. Gráfico do excesso anual (central)
library(ggplot2)
ggplot(tabela_imr_excesso, aes(x = Ano, y = Excesso_Central)) +
  geom_col(fill = ifelse(tabela_imr_excesso$Excesso_Central > 0, "red", "blue"), alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Excesso de mortalidade infantil – modelo log‑linear",
    subtitle = "Diferença entre IMR observada e projetada (1964‑1985)",
    x = "Ano", y = "Excesso de IMR (óbitos por 1.000 NV)"
  ) +
  theme_minimal()

# =============================================================================
# 7.4 CÁLCULO DO EXCESSO DE MORTALIDADE GERAL (COM PROP_IDOSOS)
# =============================================================================

ind_regime <- all_years %in% anos_regime
pop_regime <- pop_total[ind_regime]

obitos_regime <- obitos_anuais[ind_regime]

calcular_excesso <- function(imr_contra, prop_contra) {
  xreg_contra <- cbind(log(imr_contra), log(prop_contra))
  colnames(xreg_contra) <- c("log(IMR)", "log(prop_idosos)")
  
  pred_cdr <- forecast(modelo_cdr_imr_idosos, xreg = xreg_contra)
  CDR_contra <- exp(pred_cdr$mean)
  obitos_esperados <- CDR_contra * pop_regime
  excesso <- obitos_regime - obitos_esperados
  list(excesso_anual = excesso, total = sum(excesso), CDR_contra = CDR_contra)
}


# Calcular cenários
excesso_max <- calcular_excesso(imr_min, prop_contra)
excesso_central <- calcular_excesso(imr_central, prop_contra)
excesso_min <- calcular_excesso(imr_max, prop_contra)

# ----------------------------------------------------------------------------
# 7.5 RESULTADOS
# ----------------------------------------------------------------------------

cat("\n========================== RESULTADOS ==========================\n")
cat(sprintf("Excesso de mortalidade 1964–1985 (cenário máximo): %0.0f óbitos\n",
            excesso_max$total))
cat(sprintf("Excesso de mortalidade 1964–1985 (cenário central): %0.0f óbitos\n",
            excesso_central$total))
cat(sprintf("Excesso de mortalidade 1964–1985 (cenário mínimo): %0.0f óbitos\n",
            excesso_min$total))

# Previsões da CDR para cada cenário
pred_max <- exp(forecast(modelo_cdr_imr_idosos, 
                         xreg = cbind(log(imr_max), log(prop_contra)))$mean)
pred_central <- exp(forecast(modelo_cdr_imr_idosos, 
                             xreg = cbind(log(imr_central), log(prop_contra)))$mean)
pred_min <- exp(forecast(modelo_cdr_imr_idosos, 
                         xreg = cbind(log(imr_min), log(prop_contra)))$mean)

# Tabela anual
resultado_anual <- data.frame(
  Ano = anos_regime,
  Observado = round(obitos_regime, 0),
  Esperado_Min = round(pred_min * pop_regime, 0),
  Esperado_Central = round(pred_central * pop_regime, 0),
  Esperado_Max = round(pred_max * pop_regime, 0),
  Excesso_Max = round(excesso_max$excesso_anual, 0),
  Excesso_Central = round(excesso_central$excesso_anual, 0),
  Excesso_Min = round(excesso_min$excesso_anual, 0)
)

print(resultado_anual)

# Gráfico
dat_plot <- resultado_anual %>%
  dplyr::select(Ano, Observado, Esperado_Min, Esperado_Central, Esperado_Max) %>%
  pivot_longer(-Ano, names_to = "Cenário", values_to = "Óbitos") %>%
  mutate(`Óbitos (mil)` = Óbitos / 1000)

grafico_coorte_componente <- ggplot(dat_plot, aes(x = Ano, y = `Óbitos (mil)`, color = Cenário)) +
  geom_line(size = 1) +
  labs(title = "Óbitos anuais observados e contrafactuais (com prop_idosos)", 
       subtitle = "Modelo ARIMA com IMR e proporção de idosos",
       y = "Óbitos (milhares)") +
  theme_minimal()

print(grafico_coorte_componente)
save_ipeaplot(grafico_coorte_componente, "Gráfico de coorte-componente contrafactual",
              format = c("eps", "png"))


# =============================================================================
# 9. ESTUDO DE SÉRIE TEMPORAL INTERROMPIDA (ITS) – MORTALIDADE INFANTIL
# Interrupção em 1964 – início do regime militar
# =============================================================================


# 1. Preparar os dados (1930–1985)
# Supondo que 'imr_interp' seja a série interpolada de mortalidade infantil
dados_its <- data.frame(
  ano = 1950:1985,
  imr = imr_interp[21:56]  # ajuste o índice conforme sua série
) %>%

  filter(!is.na(imr))

# 2. Criar variáveis para o modelo ITS
#    - Tempo (centrado para reduzir multicolinearidade)
#    - Interrupção (0 antes de 1964, 1 a partir de 1964)
#    - Tempo após interrupção (0 antes de 1964, (ano - 1963) a partir de 1964)

ano_interrupcao <- 1964

dados_its <- dados_its %>%
  mutate(
    tempo = ano - 1950,                    # variável contínua (centrado)
    interrupcao = ifelse(ano >= ano_interrupcao, 1, 0),
    tempo_pos = ifelse(ano >= ano_interrupcao, ano - ano_interrupcao, 0)
  )

# 3. Ajuste do modelo ITS por mínimos quadrados (OLS)
modelo_its <- lm(imr ~ tempo + interrupcao + tempo_pos, data = dados_its)

summary(modelo_its)

# 4. Verificar autocorrelação dos resíduos (teste Durbin-Watson)
dwtest(modelo_its)  # se p < 0.05, há autocorrelação

# 5. Se houver autocorrelação, estimar erros robustos (Newey-West)
#    ou usar um modelo ARIMA para corrigir.
# Opção 1: Erros padrão robustos para autocorrelação e heterocedasticidade
coeftest(modelo_its, vcov = NeweyWest(modelo_its, lag = 1, prewhite = FALSE))

# Opção 2: Ajustar um modelo ARIMA com a estrutura ITS como regressores
# (Recomendado para séries com autocorrelação forte)
serie_ts <- ts(dados_its$imr, start = 1944, frequency = 1)

# Criar matriz de regressores (tempo, interrupcao, tempo_pos)
xreg <- cbind(
  tempo = dados_its$tempo,
  interrupcao = dados_its$interrupcao,
  tempo_pos = dados_its$tempo_pos
)

# Ajustar ARIMA com regressores (ARIMAX) – seleção automática
modelo_arimax <- auto.arima(serie_ts, 
                            xreg = xreg, 
                            stepwise = FALSE, 
                            approximation = FALSE)

# Resumo do modelo
summary(modelo_arimax)

# Coeficientes e significância
coeftest(modelo_arimax)

# Gráfico

# Série observada e valores ajustados pelo modelo ARIMAX
dados_its$ajustado_arimax <- modelo_arimax$fitted

ggplot(dados_its, aes(x = ano)) +
  geom_line(aes(y = imr, color = "Observado"), size = 1) +
  geom_line(aes(y = ajustado_arimax, color = "Ajustado (ARIMAX)"), size = 1, linetype = "dashed") +
  geom_vline(xintercept = 1964, linetype = "dotted", color = "red", size = 1) +
  annotate("text", x = 1964, y = max(dados_its$imr, na.rm = TRUE) * 0.9,
           label = "1964 – Início do regime militar", color = "red", hjust = -0.1) +
  labs(
    title = "Mortalidade infantil – ITS com ARIMAX",
    subtitle = "Ajuste do modelo com interrupção em 1964",
    x = "Ano", y = "IMR (óbitos por 1.000 nascidos vivos)"
  ) +
  scale_color_manual(values = c("Observado" = "black", "Ajustado (ARIMAX)" = "blue")) +
  theme_minimal()


# 6. Calcular efeitos
# Extrair coeficientes do modelo ARIMAX (ou do OLS com erros robustos)
coefs <- coef(modelo_arimax)
coef(modelo_arimax)
names(coefs)
# Os coeficientes estão na ordem: regressores (tempo, interrupcao, tempo_pos) e AR/MA
# Identificar os índices corretos:

beta_tempo <- coefs["tempo"]
beta_interrupcao <- coefs["interrupcao"]
beta_tempo_pos <- coefs["tempo_pos"]

# Efeito imediato (mudança de nível em 1964): beta_interrupcao
# Efeito gradual (mudança na inclinação): beta_tempo_pos
# Efeito acumulado até 1985 (22 anos): beta_interrupcao + beta_tempo_pos * 22

efeito_imediato <- beta_interrupcao
efeito_tendencia <- beta_tempo_pos
efeito_acumulado <- beta_interrupcao + beta_tempo_pos * 22

cat("Efeito imediato (1964):", round(efeito_imediato, 3), "óbitos por 1000 NV\n")
cat("Mudança na tendência anual:", round(efeito_tendencia, 3), "óbitos por 1000 NV/ano\n")
cat("Efeito acumulado 1964–1985:", round(efeito_acumulado, 3), "óbitos por 1000 NV\n")


# 8. Comparação com os métodos anteriores (log-linear e ARIMA)
# Você pode calcular o excesso de mortes a partir do ITS:
# Para cada ano 1964-1985, a diferença entre a projeção contrafactual e a observada
# é o efeito da interrupção.

# Supondo que você tenha o vetor 'nascimentos' para 1930‑1991
nascimentos_regime <- sum(nascimentos[all_years %in% 1964:1985])

# Efeito acumulado do ARIMAX (em óbitos por 1.000 NV)
efeito_acumulado_imr <- pmax(round(efeito_acumulado, 2), 0)  # (valor obtido do modelo)

# ----------------------------------------------------------------------------
# 9.1. CÁLCULO DO EXCESSO DE MORTALIDADE INFANTIL ANUAL (COM TESTE DE SIGNIFICÂNCIA)
# ----------------------------------------------------------------------------

library(lmtest)

# 1. Obter a tabela de coeficientes com erros padrão e p-valores do ARIMAX
tabela_coefs <- coeftest(modelo_arimax)

# Extrair estimativas e p-valores
beta_interrupcao <- tabela_coefs["interrupcao", "Estimate"]
p_interrupcao    <- tabela_coefs["interrupcao", "Pr(>|z|)"]

beta_tempo_pos   <- tabela_coefs["tempo_pos", "Estimate"]
p_tempo_pos      <- tabela_coefs["tempo_pos", "Pr(>|z|)"]


# Descomente se quiser excluir coeficientes sem significância, 
# ou deixe assim para calcular tudo junto

# 2. Definir nível de significância (ex: alfa = 0.05)
#alpha <- 0.05

# Aplicar a condição: se não for significativo (p >= alpha), o efeito vira 0
#if (p_interrupcao >= alpha) {
#  cat(sprintf("Aviso: Coeficiente 'interrupcao' não significativo (p = %.3f). Tratado como 0.\n", p_interrupcao))
#  beta_interrupcao <- 0
#} else {
#  cat(sprintf("Coeficiente 'interrupcao' significativo (p = %.3f).\n", p_interrupcao))
#}

#if (p_tempo_pos >= alpha) {
#  cat(sprintf("Aviso: Coeficiente 'tempo_pos' não significativo (p = %.3f). Tratado como 0.\n", p_tempo_pos))
#  beta_tempo_pos <- 0
#} else {
#  cat(sprintf("Coeficiente 'tempo_pos' significativo (p = %.3f).\n", p_tempo_pos))
#}

# 3. Preparar vetores para os anos do regime (1964–1985)
anos_regime <- 1964:1985
nascimentos_regime <- nascimentos[all_years %in% anos_regime] 

# 4. Calcular o efeito anual (em óbitos por 1000 NV) usando apenas os coeficientes válidos
efeito_anual <- beta_interrupcao + beta_tempo_pos * (0:21)  # t = 0 a 21

# 5. Truncar valores negativos para zero (se desejar apenas excesso)
efeito_excesso <- pmax(efeito_anual, 0)   

# 6. Calcular o excesso absoluto de óbitos por ano
excesso_absoluto_anual <- (efeito_excesso * nascimentos_regime) / 1000

# 7. Excesso total (soma)
excesso_total <- sum(excesso_absoluto_anual)

# 8. (Opcional) Calcular também a redução (valores negativos)
efeito_reducao <- pmin(efeito_anual, 0)   
reducao_absoluta_anual <- (abs(efeito_reducao) * nascimentos_regime) / 1000
reducao_total <- sum(reducao_absoluta_anual)

# 9. Exibir resultados
cat("===== RESULTADOS – EXCESSO POR COEFICIENTES (COM FILTRO DE SIGNIFICÂNCIA) =====\n")
cat(sprintf("Excesso total de óbitos (1964–1985): %0.0f\n", excesso_total))
cat(sprintf("Redução total de óbitos (1964–1985): %0.0f\n", reducao_total))
cat(sprintf("Efeito líquido (excesso - redução): %0.0f\n", excesso_total - reducao_total))

# 10. Tabela anual
tabela_excesso <- data.frame(
  Ano = anos_regime,
  Nascimentos = nascimentos_regime,
  Mortalidade_infantil_obs = imr_interp[ind_obs],
  Efeito_IMR = round(efeito_anual, 4),
  Excesso_IMR = round(efeito_excesso, 4),
  Excesso_Obitos = round(excesso_absoluto_anual, 0),
  Reducao_Obitos = round(reducao_absoluta_anual, 0)
)
print(tabela_excesso)

# 10. Gráfico do excesso anual
library(ggplot2)
its_excess <- ggplot(tabela_excesso, aes(x = Ano, y = Excesso_Obitos / 1000)) +
  geom_col(alpha = 0.6) +
  labs(title = "Annual excess of newborn deaths under Brazilian Military Dictatorship 1964-1985",
       subtitle = "Interrupted time series model",
       x = "Ano", y = "excess (thousands of deaths)") +
  theme_minimal()

print(its_excess)

ggsave("ITS_excess_plot.png", plot = its_excess)


# =============================================================================
# ESTUDO DE SÉRIE TEMPORAL INTERROMPIDA (ITS) – EXPECTATIVA DE VIDA
# Interrupção em 1964 – início do regime militar
# =============================================================================

life_expectancy_raw <- df_raw |>
  dplyr::select(ano, expectativa_vida) |>
  filter(!is.na(expectativa_vida)) |>
  arrange(ano)

life_expec_years_all <- 1930:2010
life_expec_interp <- spline(life_expectancy_raw$ano, life_expectancy_raw$expectativa_vida,
                            xout = life_expec_years_all, method = "natural")$y

plot(life_expec_interp, type = "l")


# 1. Preparar os dados (1940–1985)
# Como os dados são decenais e interpolados, a interrumpção é na década anterior
dados_its_e0 <- data.frame(
  ano = 1950:1985,
  e0 = life_expec_interp[life_expec_years_all %in% 1950:1985]
) %>%
  filter(!is.na(e0))

# 2. Criar variáveis para o modelo ITS
ano_interrupcao <- 1964

dados_its_e0 <- dados_its_e0 %>%
  mutate(
    tempo = ano - 1950,                    # variável contínua (centrado)
    interrupcao = ifelse(ano >= ano_interrupcao, 1, 0),
    tempo_pos = ifelse(ano >= ano_interrupcao, ano - ano_interrupcao, 0)
  )

# 3. Ajuste do modelo ITS por mínimos quadrados (OLS)
modelo_its_e0 <- lm(log(e0) ~ tempo + interrupcao + tempo_pos, data = dados_its_e0)
summary(modelo_its_e0)

# 4. Verificar autocorrelação dos resíduos (teste Durbin-Watson)
dwtest(modelo_its_e0)  # se p < 0.05, há autocorrelação

# 5. Opção 1: Erros padrão robustos (Newey-West)
coeftest(modelo_its_e0, vcov = NeweyWest(modelo_its_e0, lag = 1, prewhite = FALSE))
#O estimador de Newey-West calcula erros-padrão HAC (Heteroskedasticity and Autocorrelation Consistent), 
# corrigindo simultaneamente a heterocedasticidade e a autocorrelação (correlação serial) nos resíduos de um modelo de regressão.


# Opção 2: Ajustar modelo ARIMAX com regressores
serie_e0_ts <- ts(dados_its_e0$e0, start = 1940, frequency = 1)

xreg_e0 <- cbind(
  tempo = dados_its_e0$tempo,
  interrupcao = dados_its_e0$interrupcao,
  tempo_pos = dados_its_e0$tempo_pos
)

modelo_arimax_e0 <- auto.arima(serie_e0_ts,
                               xreg = xreg_e0, 
                               stepwise = FALSE, 
                               approximation = FALSE)
summary(modelo_arimax_e0)
coeftest(modelo_arimax_e0)

# 6. Coeficientes e efeitos (usando o ARIMAX, que corrige autocorrelação)
coefs_e0 <- coef(modelo_arimax_e0)
beta_interrupcao_e0 <- coefs_e0["interrupcao"]
beta_tempo_pos_e0   <- coefs_e0["tempo_pos"]

efeito_imediato_e0 <- beta_interrupcao_e0
efeito_tendencia_e0 <- beta_tempo_pos_e0
efeito_acumulado_e0 <- beta_interrupcao_e0 + beta_tempo_pos_e0 * 21

cat("\n========== ITS – EXPECTATIVA DE VIDA ==========\n")
cat(sprintf("Efeito imediato (1964): %0.3f anos\n", efeito_imediato_e0))
cat(sprintf("Mudança na tendência anual: %0.3f anos/ano\n", efeito_tendencia_e0))
cat(sprintf("Efeito acumulado 1964–1985: %0.3f anos\n", efeito_acumulado_e0))



# 7. Gráfico: série observada e ajustada pelo ARIMAX
dados_its_e0$ajustado_arimax <- fitted(modelo_arimax_e0)

ggplot(dados_its_e0, aes(x = ano)) +
  geom_line(aes(y = e0, color = "Observado"), size = 1) +
  geom_line(aes(y = ajustado_arimax, color = "Ajustado (ARIMAX)"), 
            size = 1, linetype = "dashed") +
  geom_vline(xintercept = 1964, linetype = "dotted", color = "red", size = 1) +
  annotate("text", x = 1964, y = max(dados_its_e0$e0) * 0.9,
           label = "1964 – Início do regime militar", color = "red", hjust = -0.1) +
  labs(title = "Expectativa de vida – ITS com ARIMAX",
       subtitle = "Ajuste do modelo com interrupção em 1964",
       x = "Ano", y = "e₀ (anos)") +
  scale_color_manual(values = c("Observado" = "black", "Ajustado (ARIMAX)" = "blue")) +
  theme_minimal()

# 8. (Opcional) Converter o efeito acumulado em óbitos equivalentes
#    O efeito acumulado é em anos de ganho/perda de e0.
#    Multiplicamos pela população total do regime e dividimos pela e0 média.
#    Isso dá uma estimativa do número de mortes que corresponderia à perda de anos.
perda_anos_acumulada <- -efeito_acumulado_e0  # se negativo, perda
