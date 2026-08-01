# =============================================================================
# Estratégia metodológica para estimação do excesso de mortalidade
# durante o regime militar brasileiro (1964-1985)
# Versão corrigida - 2026-07-05
# =============================================================================

# Carregar pacotes
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# =============================================================================
# 1. PREPARAÇÃO ROBUSTA DOS DADOS
# =============================================================================

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

census_years <- c(1940, 1950, 1960, 1970, 1980, 1991)

df_census <- df_raw %>%
  filter(ano %in% census_years) %>%
  arrange(ano)

pop_census <- df_census$população   # já numérico

# Interpolação linear da população total 1940-1991
all_years <- 1940:1991
pop_total <- exp(approx(census_years, log(pop_census), xout = all_years)$y)



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

# Mortalidade infantil (IMR) – interpolação anual 1930-1990
imr_raw <- df_raw %>%
  select(ano, mortalidade_infantil) %>%
  filter(!is.na(mortalidade_infantil)) %>%
  arrange(ano)

imr_years_all <- 1930:1990
imr_interp <- exp(approx(imr_raw$ano, log(imr_raw$mortalidade_infantil),
                     xout = imr_years_all, rule = 2)$y)

# =============================================================================
# 3. MIGRAÇÃO LÍQUIDA POR DÉCADA
# =============================================================================

# Participação do saldo migratório no crescimento populacional (%)
mig_part <- data.frame(
  periodo_inicio = c(1940, 1950, 1960, 1970, 1980),
  periodo_fim    = c(1950, 1960, 1970, 1980, 1991),
  participacao   = c(1.0, 3.4, 1.32, 0.9, 0.52) / 100
)

# =============================================================================
# 4. ÓBITOS TOTAIS POR PERÍODO INTERCENSITÁRIO
# =============================================================================

periodos <- list(
  "1940-1950" = c(1940,1950),
  "1950-1960" = c(1950,1960),
  "1960-1970" = c(1960,1970),
  "1970-1980" = c(1970,1980),
  "1980-1991" = c(1980,1991)
)

resultados <- data.frame(periodo = character(),
                         pop_inicio = numeric(), pop_fim = numeric(),
                         nasc = numeric(), migracao = numeric(),
                         obitos = numeric())

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
# 5. ANUALIZAÇÃO DOS ÓBITOS (USANDO IMR)
# =============================================================================

obitos_anuais <- numeric(length(all_years))

for (i in 1:nrow(resultados)) {
  t0 <- periodos[[resultados$periodo[i]]][1]
  t1 <- periodos[[resultados$periodo[i]]][2]
  anos <- t0:(t1 - 1)
  
  # IMR para cada ano do período
  imr_periodo <- imr_interp[imr_years_all %in% anos]
  
  # Se a soma das IMR for zero (improvável), distribuir uniformemente
  if (sum(imr_periodo) == 0) {
    pesos <- rep(1/length(anos), length(anos))
  } else {
    pesos <- imr_periodo / sum(imr_periodo)
  }
  
  obitos_anuais[all_years %in% anos] <- resultados$obitos[i] * pesos
}

# Taxa bruta de mortalidade observada
CDR_obs <- obitos_anuais / pop_total

# Verificação rápida
if (any(is.na(obitos_anuais[1:30]))) {
  warning("obitos_anuais contém NAs nos primeiros anos. Verifique a conversão dos dados.")
}

# =============================================================================
# 6. RELAÇÃO CDR ~ IMR NO PERÍODO PRÉ-1964
# =============================================================================

anos_pre <- 1940:1963
ind_pre <- all_years %in% anos_pre

dados_pre <- data.frame(
  ano = anos_pre,
  CDR = CDR_obs[ind_pre],
  IMR = imr_interp[imr_years_all %in% anos_pre]
)

modelo_cdr_imr <- lm(CDR ~ IMR, data = dados_pre)

# =============================================================================
# 7. CENÁRIOS CONTRAFACTUAIS DE IMR
# =============================================================================

# IMR histórica 1930-1960 para modelo log-linear
imr_hist <- data.frame(
  ano = seq(1930, 1960, 5),
  IMR = imr_interp[imr_years_all %in% seq(1930, 1960, 5)]
)

modelo_log <- lm(log(IMR) ~ ano, data = imr_hist)

summary(modelo_log)

anos_regime <- 1964:1985

# modelo preditivo de tendência de mortalidade

imr_model <- exp(predict(modelo_log, interval = "prediction", level = 0.95, 
                           newdata = data.frame(ano = anos_regime)))

summary(imr_model)

imr_central <- imr_model[, "fit"]
imr_max     <- imr_model[, "lwr"]
imr_min     <- imr_model[, "upr"]

# Verificar
plot(anos_regime, imr_central, type = "l", ylim = range(imr_lwr, imr_upr),
     main = "IMR contrafactual (1964-1985)", xlab = "Ano", ylab = "IMR")
lines(anos_regime, imr_lwr, lty = 2)
lines(anos_regime, imr_upr, lty = 2)

# =============================================================================
# 8. CÁLCULO DO EXCESSO DE MORTALIDADE
# =============================================================================

ind_regime <- all_years %in% anos_regime
pop_regime <- pop_total[ind_regime]
obitos_regime <- obitos_anuais[ind_regime]

calcular_excesso <- function(imr_contra) {
  CDR_contra <- predict(modelo_cdr_imr, newdata = data.frame(IMR = imr_contra))
  obitos_esperados <- CDR_contra * pop_regime
  excesso <- obitos_regime - obitos_esperados
  total <- sum(excesso)
  list(excesso_anual = excesso, total = total)
}

excesso_max <- calcular_excesso(imr_max)   # queda máxima de mortalidade 
excesso_central <- calcular_excesso((imr_central)) #tendência central
excesso_min <- calcular_excesso(imr_min)   # queda mínima de mortalidade

# =============================================================================
# 9. RESULTADOS
# =============================================================================

cat("\n========================== RESULTADOS ==========================\n")
cat(sprintf("Excesso de mortalidade 1964–1985 (cenário máximo): %0.0f óbitos\n",
            excesso_max$total))
cat(sprintf("Excesso de mortalidade 1964–1985 (cenário central): %0.0f óbitos\n",
            excesso_central$total))
cat(sprintf("Excesso de mortalidade 1964–1985 (cenário mínimo): %0.0f óbitos\n",
            excesso_min$total))

# Tabela anual do excesso
resultado_anual <- data.frame(
  Ano = anos_regime,
  Observado = round(obitos_regime, 0),
  Esperado_Min = round(predict(modelo_cdr_imr, newdata = data.frame(IMR = imr_max)) * pop_regime, 0),
  Esperado_Max = round(predict(modelo_cdr_imr, newdata = data.frame(IMR = imr_min)) * pop_regime, 0),
  Esperado_Central = round(predict(modelo_cdr_imr, newdata = data.frame(IMR = imr_central))*pop_regime, 0),
  Excesso_Max = round(excesso_max$excesso_anual, 0),
  Excesso_Central = round(excesso_central$excesso_anual, 0),
  Excesso_Min = round(excesso_min$excesso_anual, 0)
)

print(resultado_anual)

# Visualização
dat_plot <- resultado_anual %>%
  select(Ano, Observado, Esperado_Min, Esperado_Central, Esperado_Max) %>%
  pivot_longer(-Ano, names_to = "Cenário", values_to = "Óbitos") %>%
  mutate(`Óbitos (mil)` = `Óbitos` / 1000)

ggplot(dat_plot, aes(x = Ano, y = `Óbitos (mil)`, color = Cenário)) +
  geom_line(size = 1) +
  labs(title = "Óbitos anuais observados e contrafactuais (1964–1985)",
       y = "Óbitos (milhares)") +
  theme_minimal()