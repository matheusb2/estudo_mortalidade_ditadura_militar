
#Déficit populacional e mortalidade associada ao regime militar brasileiro (1964–1985)
# ==============================================================================
# 0. PACOTES
# ==============================================================================

library(readxl)
library(tidyverse)
library(readxl)
library(ggplot2)
library(ipeaplot)
library(strucchange)
library(broom)
library(scales)
library(patchwork)

#===============================================================================
#REFERÊNCIAS
#===============================================================================

#GIVISIEZ, Gustavo Henrique Naves. Introdução a métodos de estimativas e interpolação populacionais. In: RIANI, J.L.R.; RIOS-NETO, E.L.G. (Org.). Introdução à Demografia da Educação. Campinas: ABEP; 2004. p.45-70.

#GADDY, Hampton; GARGIULO, Maria. Can we estimate crisis death tolls by subtracting total population estimates? A critical review and appraisal. Demographic Research, v. 52, p. 741-796, 2025.

# ==============================================================================
# 1. DADOS
# ==============================================================================



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


arquivo <- "~/Documentos/IPEA/modelos/demografia social da ditadura militar/censos interpolados.xlsx" #Exemplo

df_raw <- read_excel(
  path = arquivo,
  sheet = "BR_1950_2022"
)

names(df_raw)
#nome das colunas
#[1] "ano"                                                           
#[2] "tempo"                                                         
# [3] "crianças 0 a 4 anos"                                           
# [4] "contrafactual crianças 0 a 4 anos"                             
# [5] "déficit populacional crianças 0 a 4 anos"                      
# [6] "homens 0 a 4 anos"                                             
# [7] "contrafactual homens 0 a 4 anos"                               
# [8] "déficit populacional homens 0 a 4 anos"                        
# [9] "mulheres 0 a 4 anos"                                           
#[10] "contrafactual mulheres 0 a 4 anos"                             
#[11] "déficit populacional mulheres 0 a 4 anos"                      
#[12] "homens 5 a 9 anos"                                             
#[13] "contrafactual homens 5 a 9 anos"                               
#[14] "déficit populacional homens 5 a 9 anos"                        
#[15] "mulheres 5 a 9 anos"                                           
#[16] "contrafactual mulheres 5 a 9 anos"                             
#[17] "déficit populacional mulheres 5 a 9 anos"                      
#[18] "homens 10 a 14 anos"                                           
#[19] "contrafactual homens 10 a 14 anos"                             
#[20] "déficit populacional homens 10 a 14 anos"                      
#[21] "mulheres 10 a 14 anos"                                         
#[22] "contrafactual mulheres 10 a 14 anos"                           
#[23] "déficit populacional mulheres 10 a 14 anos"                    
#[24] "homens 15 a 19 anos"                                           
#[25] "contrafactual homens 15 a 19 anos"                             
#[26] "déficit populacional homens 15 a 19 anos"                      
#[27] "mulheres 15 a 19 anos"                                         
#[28] "contrafactual mulheres 15 a 19 anos"                           
#[29] "déficit populacional mulheres 15 a 19 anos"                    
#[30] "homens 20 a 24 anos"                                           
#[31] "contrafactual homens 20 a 24 anos"                             
#[32] "déficit populacional homens 20 a 24 anos"                      
#[33] "mulheres 20 a 24 anos"                                         
#[34] "contrafactual mulheres 20 a 24 anos"                           
#[35] "déficit populacional mulheres 20 a 24 anos"                    
#[36] "homens 25 a 29 anos"                                           
#[37] "contrafactual homens 25 a 29 anos"                             
#[38] "déficit populacional homens 25 a 29 anos"                      
#[39] "mulheres 25 a 29 anos"                                         
#[40] "contrafactual mulheres 25 a 29 anos"                           
#[41] "déficit populacional mulheres 25 a 29 anos"                    
#[42] "homens 30 a 39 anos"                                           
#[43] "contrafactual homens 30 a 39 anos"                             
#[44] "déficit populacional homens 30 a 39 anos"                      
#[45] "mulheres 30 a 39 anos"                                         
#[46] "contrafactual mulheres 30 a 39 anos"                           
#[47] "déficit populacional mulheres 30 a 39 anos"                    
#[48] "homens 40 a 49 anos"                                           
#[49] "contrafactual homens 40 a 49 anos"                             
#[50] "déficit populacional homens 40 a 49 anos"                      
#[51] "mulheres 40 a 49 anos"                                         
#[52] "contrafactual mulheres 40 a 49 anos"                           
#[53] "déficit populacional mulheres 40 a 49 anos"                    
#[54] "homens 50 a 59 anos"                                           
#[55] "contrafactual homens 50 a 59 anos"                             
#[56] "déficit populacional homens 50 a 59 anos"                      
#[57] "mulheres 50 a 59 anos"                                         
#[58] "contrafactual mulheres 50 a 59 anos"                           
#[59] "déficit populacional mulheres 50 a 59 anos"                    
#[60] "homens 60 a 69 anos"                                           
#[61] "contrafactual homens 60 a 69 anos"                             
#[62] "déficit populacional homens 60 a 69 anos"                      
#[63] "mulheres 60 a 69 anos"                                         
#[64] "contrafactual mulheres 60 a 69 anos"                           
#[65] "déficit populacional mulheres 60 a 69 anos"                    
#[66] "homens 70 ou mais"                                             
#[67] "contrafactual homens 70 ou mais"                               
#[68] "déficit populacional homens 70 ou mais"                        
#[69] "mulheres 70 ou mais"                                           
#[70] "contrafactual mulheres 70 ou mais"                             
#[71] "déficit populacional mulheres 70 ou mais"                      
#[72] "razão biológica menos razão observada"                         
#[73] "população"                                                     
#[74] "mortos e desaparecidos documentados"                           
#[75] "mortes violentas SP"                                           
#[76] "pop_urbana"                                                    
#[77] "taxa mortos e desaparecidos"                                   
#[78] "homicídios projetados"                                         
#[79] "taxa de homicídios tendência 1"                                
#[80] "taxa de homicídios tendência 2"                                
#[81] "taxa de homicídios tendência 3"                                
#[82] "taxa de homicídios projetados"                                 
#[83] "homicídios excesso (projetado - tendência1)"                   
#[84] "homicídios excesso (projetado - tendência2)"                   
#[85] "homicídios excesso (projetado - tendência3)"                   
#[86] "mortes por causas externas projetadas"                         
#[87] "taxa de mortes violentas"                                      
#[88] "tendência mortes violentas 1"                                  
#[89] "tendência mortes violentas 2"                                  
#[90] "tendência mortes violentas 3"                                  
#[91] "excesso de mortes violentas (projetado - tendência 1)"         
#[92] "excesso de mortes violentas (projetado - tendência 2)"         
#[93] "excesso de mortes violentas (projetado - tendência 3)"         
#[94] "óbitos em acidentes de trabalho"                               
#[95] "taxa de mortes por acidente de trabalho por 100 mil habitantes"
#[96] "óbitos por 100 mil trabalhadores"    

#Os dados dos Censos por sexo e faixa etária foram interpolados ano a ano
#Censos de 1950, 1960, 1970, 1980, 1991, 2000, 2010 e 2022
#formato wide
#dados da população por sexo e faixa etária precisam ser interpolados
#colunas de contrafactual precisam ser projetadas
#dados de mortalidade calculados em outro script

#==============================================================================
#INTERPOLAÇÕES POR MÉTODO EXPONENCIAL MANTENDO O FORMATO LARGO (WIDE)
#==============================================================================


#identificar os anos censitários
anos_censo <- c(1950, 1960, 1970, 1980, 1991, 2000, 2010, 2022)

#função para interpolação exponencial

interp_exponencial <- function(ano, pop, anos_censo) {
  
  resultado <- pop
  
  for (i in seq_len(length(anos_censo) - 1)) {
    
    a0 <- anos_censo[i]
    a1 <- anos_censo[i + 1]
    
    idx0 <- which(ano == a0)
    idx1 <- which(ano == a1)
    
    # Se faltar dado em algum censo, pula
    if (length(idx0) == 0 || length(idx1) == 0) {
      next
    }
    
    p0 <- pop[idx0]
    p1 <- pop[idx1]
    
    # Evita log de zero ou valores negativos
    if (is.na(p0) || is.na(p1) || p0 <= 0 || p1 <= 0) {
      next
    }
    
    delta_t <- a1 - a0
    r <- (log(p1) - log(p0)) / delta_t
    
    anos_inter <- which(ano > a0 & ano < a1)
    t <- ano[anos_inter] - a0
    
    resultado[anos_inter] <- p0 * exp(r * t)
  }
  
  resultado
}

#selecionar colunas a serem interpoladas

cols_pop <- names(df_raw) |>
  str_subset("^(homens|mulheres|crianças)")


#aplicar interpolação exponencial para cada grupo populacional

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

write_csv2(df_interp, "censos_interp_exp.csv")

df_raw <- df_interp

#==============================================================================
#CONTRAFACTUAIS
#===============================================================================

#Função genérica de contrafactual exponencial
# usa 1960 como âncora fixa
# ano final parametrizável
# Escreve apenas no intervalo desejado
# Preserva valores fora do intervalo

contrafactual_exponencial <- function(
    ano,
    pop,
    ano_base = 1960,
    ano_final = 1991,
    extrapolar = FALSE
) {
  
  resultado <- rep(NA_real_, length(pop))
  
  idx_base <- which(ano == ano_base)
  idx_final <- which(ano == ano_final)
  
  if (length(idx_base) == 0 || length(idx_final) == 0) {
    return(resultado)
  }
  
  p0 <- pop[idx_base]
  p1 <- pop[idx_final]
  
  if (is.na(p0) || is.na(p1) || p0 <= 0 || p1 <= 0) {
    return(resultado)
  }
  
  delta_t <- ano_final - ano_base
  r <- (log(p1) - log(p0)) / delta_t
  
  idx_intervalo <- which(ano >= ano_base & ano <= ano_final)
  t <- ano[idx_intervalo] - ano_base
  
  resultado[idx_intervalo] <- p0 * exp(r * t)
  
  if (extrapolar) {
    idx_pos <- which(ano > ano_final)
    t_pos <- ano[idx_pos] - ano_base
    resultado[idx_pos] <- p0 * exp(r * t_pos)
  }
  
  resultado
}

#sensibilidade
anos_finais_cf <- c(1980, 1991, 2000, 2010)

lista_contrafactuais <- vector("list", length(anos_finais_cf))

names(lista_contrafactuais) <- paste0("cf_", anos_finais_cf)

for (j in seq_along(anos_finais_cf)) {
  
  ano_final_cf <- anos_finais_cf[j]
  
  df_tmp <- df_raw
  
  for (i in seq_along(cols_contrafactual)) {
    
    col_cf <- cols_contrafactual[i]
    
    col_obs <- col_cf |>
      str_remove("^contrafactual ")
    
    df_tmp[[col_cf]] <-
      contrafactual_exponencial(
        ano = df_tmp$ano,
        pop = df_tmp[[col_obs]],
        ano_base = 1960,
        ano_final = ano_final_cf,
        extrapolar = FALSE
      )
  }
  
  df_tmp$cenario_contrafactual <- ano_final_cf
  
  lista_contrafactuais[[j]] <- df_tmp
}

df_sensibilidade_cf <- bind_rows(lista_contrafactuais)

df_sensibilidade_cf |>
  filter(
    ano %in% c(1960, 1970, 1980, 1991),
    cenario_contrafactual %in% c(1980, 1991)
  ) |>
  select(
    ano,
    cenario_contrafactual,
    `homens 0 a 4 anos`,
    `contrafactual homens 0 a 4 anos`
  ) |>
  arrange(cenario_contrafactual, ano)


# ==========================================================
# CÁLCULO AUTOMÁTICO DOS DÉFICITS POPULACIONAIS
# ==========================================================

# identificar colunas contrafactuais
cols_cf <- names(df_sensibilidade_cf) |>
  str_subset("^contrafactual ")

# derivar nomes das colunas observadas correspondentes
cols_obs <- cols_cf |>
  str_remove("^contrafactual ")

# nomes das colunas de déficit
cols_def <- paste0("déficit populacional ", cols_obs)

# calcular déficit = observado - contrafactual
for (i in seq_along(cols_cf)) {
  
  col_cf  <- cols_cf[i]
  col_obs <- cols_obs[i]
  col_def <- cols_def[i]
  
  df_sensibilidade_cf[[col_def]] <-
    df_sensibilidade_cf[[col_cf]]-  df_sensibilidade_cf[[col_obs]]
  
}

df_sensibilidade_cf |>
  filter(
    ano %in% c(1970, 1980, 1991),
    cenario_contrafactual == 1991
  ) |>
  select(
    ano,
    cenario_contrafactual,
    `homens 20 a 24 anos`,
    `contrafactual homens 20 a 24 anos`,
    `déficit populacional homens 20 a 24 anos`
  )

#gráfico de sensibilidade

library(ggplot2)

df_sensibilidade_cf |>
  filter(ano >= 1960) |>
  ggplot(
    aes(
      x = ano,
      y = `déficit populacional homens 20 a 24 anos`,
      color = factor(cenario_contrafactual)
    )
  ) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_brewer(
    palette = "Dark2",
    name = "Ano final do contrafactual"
  ) +
  labs(
    title = "Sensibilidade do déficit populacional",
    subtitle = "Homens 20 a 24 anos",
    x = "Ano",
    y = "Déficit populacional"
  ) +
  theme_minimal()

#==============================================================================
#INTEGRAR AO BANCO DE DADOS AS INTERPOLAÇÕES COM O CONTRAFACTUAL ESCOLHIDO
#==============================================================================

#ANO ESCOLHIDO
cenario_escolhido <- 1991 #pode ser 1980, 1991, 2000, 2010

#EXTRAÇÃO DE CENÁRIOS

df_cenario <- df_sensibilidade_cf |>
  filter(cenario_contrafactual == cenario_escolhido) |>
  select(
    ano,
    starts_with("contrafactual "),
    starts_with("déficit populacional ")
  )

#INTEGRAR AOS DADOS

df_raw <- df_raw |>
  select(-starts_with("contrafactual "),
         -starts_with("déficit populacional ")) |>
  left_join(df_cenario, by = "ano")

#VERIFICAR

summary(df_raw |> select(starts_with("contrafactual homens")))
summary(df_raw |> select(starts_with("déficit populacional homens")))


# ==============================================================================
# 2. CONVERTER OS DÉFICITS POPULACIONAIS EM FORMATO LONGO
# ==============================================================================

dados_long <- df_raw |>
  select(ano, matches("^déficit populacional")) |>
  pivot_longer(-ano, names_to = "variavel", values_to = "deficit") |>
  mutate(
    sexo = if_else(str_detect(variavel, "homens"), "homens", "mulheres"),
    faixa = str_extract(variavel, "\\d+ a \\d+|\\d+ ou mais")
  )


# ==============================================================================
# 3. MORTALIDADE ADULTA (25–59)
# ==============================================================================

mortes_long <- dados_long |>
  arrange(sexo, faixa, ano) |>
  group_by(sexo, faixa) |>
  mutate(mortes_incrementais = pmax(deficit - lag(deficit, default = first(deficit)), 0)) |>
  ungroup()

summary(mortes_long)

mortes_adultos <- mortes_long |>
  filter(
    ano >= 1964, ano <= 1985,
    faixa %in% c("25 a 29", "30 a 39", "40 a 49", "50 a 59")
  )

summary(mortes_adultos)

mortes_adultos_total_valor <- mortes_adultos |>
  summarise(excesso_adultos = sum(mortes_incrementais, na.rm = TRUE)) |>
  pull(excesso_adultos)

print(mortes_adultos_total_valor)

mortes_adultos_homens <- mortes_adultos |>
  filter(sexo == "homens") |>
  summarise(total = sum(mortes_incrementais, na.rm = TRUE)) |>
  pull(total)

print(mortes_adultos_homens)

desaparecimento_mulheres_adultas <- mortes_adultos_total_valor - mortes_adultos_homens

razao_homem_mulher_adultos <- mortes_adultos_homens/desaparecimento_mulheres_adultas
print(razao_homem_mulher_adultos)

print(desaparecimento_mulheres_adultas)

mortes_documentadas <- df_raw |>
  filter(ano >= 1964, ano <= 1985) |>
  summarise(total = sum(`mortos e desaparecidos documentados`, na.rm = TRUE)) |>
  pull(total)

print(mortes_documentadas)

#===============================================================================
#3.1 TESTES FORMAIS DA TRAJETÓRIA DA MORTALIDADE ADULTA
#==============================================================================

#arrumar os dados

adultos_25_59 <- df_raw |>
  transmute(
    ano,
    deficit =
      `déficit populacional homens 25 a 29 anos` +
      `déficit populacional mulheres 25 a 29 anos` +
      `déficit populacional homens 30 a 39 anos` +
      `déficit populacional mulheres 30 a 39 anos` +
      `déficit populacional homens 40 a 49 anos` +
      `déficit populacional mulheres 40 a 49 anos` +
      `déficit populacional homens 50 a 59 anos` +
      `déficit populacional mulheres 50 a 59 anos`
  )


#variável da mudança de regime de 1964 a 1985 e tempo no regime
adultos_25_59 <- adultos_25_59 |>
  mutate(
    regime = as.integer(ano >= 1964 & ano <= 1985),
    t_regime = if_else(regime == 1, ano - 1963, 0)
  )


#Quebras estruturais

modelo_adultos <- lm(deficit ~ ano, data = adultos_25_59)

summary(modelo_adultos)

quebras_adultos <- breakpoints(deficit ~ ano, data = adultos_25_59)

summary(quebras_adultos)

#As quebras estruturais serão relevantes!

#modelo de intervenção temporária
modelo_interv_adultos <- lm(
  deficit ~ ano + regime + t_regime,
  data = adultos_25_59
)

summary(modelo_interv_adultos)

#modelo de quebra dupla

adultos_25_59 <- adultos_25_59 |>
  mutate(
    t_pos_1964 = pmax(ano - 1963, 0),
    t_pos_1985 = pmax(ano - 1985, 0)
  )

modelo_dupla_quebra_adultos <- lm(
  deficit ~ ano + t_pos_1964 - t_pos_1985,
  data = adultos_25_59
)

summary(modelo_dupla_quebra_adultos)


#regressão segmentada

#Regressão segmentada pé e pós 1964


modelo_segmentado_adultos <- lm(
  deficit ~ ano * regime,
  data = adultos_25_59
)

summary(modelo_segmentado_adultos)

#teste de Bai-Perron
bp_adultos <- breakpoints(deficit ~ ano, data = adultos_25_59)

summary(bp_adultos)
plot(bp_adultos)

#teste placebo (1950-1963)
summary(
  lm(
    deficit ~ ano,
    data = subset(adultos_25_59, ano < 1964)
  )
)

summary(
  lm(
    deficit ~ ano,
    data = subset(adultos_25_59, ano > 1985)
  )
)

#Visualização

adultos_25_59 <- adultos_25_59 |>
  mutate(regime_f = factor(regime, labels = c("Fora do regime", "Durante o regime")))


adultos_graf <- ggplot(adultos_25_59, aes(x = ano, y = deficit)) +
  geom_point(alpha = 0.5) +
  geom_smooth(
    aes(color = regime_f, group = regime_f),
    method = "lm",
    se = FALSE,
    linewidth = 1.2
  ) +
  geom_vline(xintercept = c(1964, 1985), linetype = "dashed") +
  scale_color_manual(values = c("grey50", "#D62728")) +
  labs(
    title = "Déficit populacional adulto (25–59) e regime autoritário",
    x = "Ano",
    y = "Déficit populacional",
    color = "Período"
  ) +
  theme_ipea()

adultos_graf

save_ipeaplot(
  adultos_graf,
  "deficit_adultos_25_59_regime",format = c("png", "eps")
)


# ==============================================================================
# 4. MORTALIDADE INFANTIL (0–4) — DIAGNÓSTICO SEM CONTABILIZAR
# ==============================================================================
#os dados do Censo não são adequados pra mensurar a mortalidade infantil
#para isso existem outros estudos da época, como:
#PAIM, Jairnison Silva; COSTA, Maria da Conceição Nascimento. Variação da mortalidade infantil em diferentes capitais brasileiras (1960-1979). Revista Baiana de Saúde Pública, v. 9, n. 3/4, p. 125-125, 1982.
#YUNES, João; RONCHEZEL, Vera Shirley Carvalho. Evolução da mortalidade geral, infantil e proporcional no Brasil. Revista de Saúde Pública, v. 8, p. 3-48, 1974.
#só vamos verificar se o choque causado pela mudança de políticas públicas devido ao golpe se confirma

infantil <- df_raw |>
  transmute(ano, deficit = `déficit populacional crianças 0 a 4 anos`)

infantil <- infantil |> mutate(
  regime = as.integer(ano >= 1964 & ano <= 1985),
  t_regime = if_else(regime == 1, ano - 1963, 0)
)

modelo_linear <- lm(deficit ~ ano, data = infantil)
quebras <- breakpoints(deficit ~ ano, data = infantil)

summary(modelo_linear)
summary(quebras)


# ==============================================================================
# 4.1 DIFERENÇAS-EM-DIFERENÇAS (0–4) DA MORTALIDADE INFANTIL
# ==============================================================================

infantil_sexo <- df_raw |>
  transmute(
    ano,
    homens = `déficit populacional homens 0 a 4 anos`,
    mulheres = `déficit populacional mulheres 0 a 4 anos`
  ) |>
  pivot_longer(-ano, names_to = "sexo", values_to = "deficit") |>
  mutate(pos_1964 = as.integer(ano >= 1964))

infantil_sexo <- infantil_sexo |>
  mutate(
    regime = as.integer(ano >= 1964 & ano <= 1985),
    t_regime = if_else(regime == 1, ano - 1963, 0)
  )

modelo_did <- lm(deficit ~ sexo * regime * ano, data = infantil_sexo)

summary(modelo_did)


# ==============================================================================
# 4.2 PLACEBO (20–24) JOVENS
# ==============================================================================

adultos_placebo <- df_raw |>
  transmute(
    ano,
    homens = `déficit populacional homens 20 a 24 anos`,
    mulheres = `déficit populacional mulheres 20 a 24 anos`
  ) |>
  pivot_longer(-ano, names_to = "sexo", values_to = "deficit") |>
  mutate(pos_1964 = as.integer(ano >= 1964))

adultos_placebo <- adultos_placebo |>
  mutate(
    regime = as.integer(ano >= 1964 & ano <= 1985),
    t_regime = if_else(regime == 1, ano - 1963, 0)
  )

modelo_placebo <- lm(deficit ~ sexo * regime + ano, data = adultos_placebo)

summary(modelo_placebo)

# ==============================================================================
# 5. MORTALIDADE NA INFÂNCIA (5–9) — ESTIMÁVEL (MÉTODO CORRIGIDO)
# ==============================================================================

# Dados de fecundidade (mantido igual)
fecundidade_decada <- tibble::tribble(
  ~ano_decada, ~tft,
  1940, 6.16, 1950, 6.21,
  1960, 6.28, 1970, 5.76,
  1980, 4.35, 1991, 2.89,
  2000, 2.38, 2010, 1.90,
  2022, 1.55
)

# Interpolação anual da TFT (mantido igual)
fecundidade_ano <- tibble(
  ano = seq(
    min(fecundidade_decada$ano_decada),
    max(fecundidade_decada$ano_decada),
    by = 1
  )
) |> 
  mutate(
    tft = approx(
      x = fecundidade_decada$ano_decada,
      y = fecundidade_decada$tft,
      xout = ano,
      rule = 2
    )$y
  )

# ==============================================================================
# MÉTODO 2 SIMPLIFICADO: AJUSTE PELA DIFERENÇA ENTRE TFT REAL E TFT IMPLÍCITA
# ==============================================================================

# 1. Criar TFT linear implícita na interpolação 1960-1991
tft_1960 <- fecundidade_ano |> filter(ano == 1960) |> pull(tft)
tft_1991 <- fecundidade_ano |> filter(ano == 1991) |> pull(tft)

# TFT linear esperada para cada ano entre 1960 e 1991
anos_interp <- 1960:1991
tft_linear <- tft_1960 + (anos_interp - 1960) * (tft_1991 - tft_1960) / (1991 - 1960)

# Criar dataframe com TFT linear implícita
tft_linear_df <- tibble(
  ano = anos_interp,
  tft_linear = tft_linear
)

# 2. Calcular fator de ajuste para cada coorte 5-9 anos
calcular_fator_coorte_5_9 <- function(ano_obs) {
  # Coorte 5-9 anos no ano de observação tem idade central ~7.5
  # Anos de nascimento aproximados: ano_obs - 9 até ano_obs - 5
  anos_nascimento <- (ano_obs - 9):(ano_obs - 5)
  
  # Filtrar anos dentro do período com dados (1940-2022)
  anos_nascimento <- anos_nascimento[anos_nascimento >= 1940 & anos_nascimento <= 2022]
  
  if (length(anos_nascimento) == 0) {
    return(1.0)  # Sem ajuste se fora do período
  }
  
  # Calcular média da TFT real no período de nascimento
  tft_real_media <- fecundidade_ano |>
    filter(ano %in% anos_nascimento) |>
    summarise(media = mean(tft, na.rm = TRUE)) |>
    pull(media)
  
  # Calcular média da TFT linear implícita (apenas para anos entre 1960-1991)
  anos_nasc_interp <- anos_nascimento[anos_nascimento >= 1960 & anos_nascimento <= 1991]
  
  if (length(anos_nasc_interp) > 0) {
    # Para anos entre 1960-1991: usar TFT linear
    tft_linear_media <- tft_linear_df |>
      filter(ano %in% anos_nasc_interp) |>
      summarise(media = mean(tft_linear, na.rm = TRUE)) |>
      pull(media)
    
    # Para anos fora de 1960-1991: usar TFT real (não há TFT linear definida)
    anos_fora <- anos_nascimento[!(anos_nascimento %in% anos_nasc_interp)]
    
    if (length(anos_fora) > 0) {
      tft_fora_media <- fecundidade_ano |>
        filter(ano %in% anos_fora) |>
        summarise(media = mean(tft, na.rm = TRUE)) |>
        pull(media)
      
      # Média ponderada
      n_interp <- length(anos_nasc_interp)
      n_fora <- length(anos_fora)
      tft_impl_media <- (tft_linear_media * n_interp + tft_fora_media * n_fora) / (n_interp + n_fora)
    } else {
      tft_impl_media <- tft_linear_media
    }
  } else {
    # Se nenhum ano de nascimento está entre 1960-1991, usar TFT real como referência
    tft_impl_media <- tft_real_media
  }
  
  # Fator de ajuste: TFT real / TFT implícita
  # Se > 1: fecundidade real foi maior que a implícita → contrafactual subestimado
  # Se < 1: fecundidade real foi menor que a implícita → contrafactual superestimado
  return(tft_real_media / tft_impl_media)
}

# 3. Aplicar o fator aos anos relevantes (1960-1991)
anos_analise <- 1960:1991
fatores_ajuste <- sapply(anos_analise, calcular_fator_coorte_5_9)

fatores_df <- tibble(
  ano = anos_analise,
  fator_fecundidade_5_9 = fatores_ajuste
)

# Verificar os fatores
summary(fatores_df)
cat("\nFatores de ajuste para coorte 5-9 anos:\n")
print(fatores_df |> arrange(ano))

# Visualizar a trajetória dos fatores
ggplot(fatores_df, aes(x = ano, y = fator_fecundidade_5_9)) +
  geom_line(linewidth = 1.2, color = "#2E86AB") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  geom_vline(xintercept = c(1964, 1985), linetype = "dashed", color = "gray50") +
  labs(
    title = "Fator de ajuste de fecundidade para coorte 5-9 anos",
    subtitle = "Razão entre TFT real e TFT implícita na interpolação 1960-1991",
    x = "Ano de observação da coorte (5-9 anos)",
    y = "Fator de ajuste (TFT_real / TFT_implícita)",
    caption = "Fator > 1: fecundidade real > implícita → contrafactual estava subestimado\nFator < 1: fecundidade real < implícita → contrafactual estava superestimado"
  ) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0.8, 1.2, by = 0.05)) +
  theme(plot.caption = element_text(hjust = 0, size = 9))

# 4. Aplicar ajuste aos contrafactuais
dados_5_9_aj <- df_raw |>
  left_join(fatores_df, by = "ano") |>
  # Se ano fora do intervalo 1960-1991, usar fator = 1 (sem ajuste)
  mutate(
    fator_fecundidade_5_9 = ifelse(is.na(fator_fecundidade_5_9), 1, fator_fecundidade_5_9)
  ) |>
  mutate(
    # Ajustar contrafactuais
    `contrafactual homens 5 a 9 anos` = 
      `contrafactual homens 5 a 9 anos` * fator_fecundidade_5_9,
    
    `contrafactual mulheres 5 a 9 anos` = 
      `contrafactual mulheres 5 a 9 anos` * fator_fecundidade_5_9
  ) |>
  # Recalcular déficits
  mutate(
    `déficit populacional homens 5 a 9 anos` =
      `contrafactual homens 5 a 9 anos` - `homens 5 a 9 anos`,
    
    `déficit populacional mulheres 5 a 9 anos` =
      `contrafactual mulheres 5 a 9 anos` - `mulheres 5 a 9 anos`
  )

# 5. Análise de sensibilidade: comparar com método antigo
sensibilidade_fecundidade <- dados_5_9_aj |>
  select(ano, fator_fecundidade_5_9) |>
  # Comparar com método antigo se disponível
  left_join(
    fecundidade_coorte_5_9 |> select(ano_obs, fator_antigo = fator_fecundidade_5_9),
    by = c("ano" = "ano_obs")
  ) |>
  filter(!is.na(fator_antigo)) |>
  mutate(diferenca = fator_fecundidade_5_9 - fator_antigo)

cat("\nComparação com método anterior:\n")
print(sensibilidade_fecundidade)

# 6. Continuar com análise de mortalidade (igual ao seu código original)
infancia_5_9 <- dados_5_9_aj |>
  select(
    ano,
    `déficit populacional homens 5 a 9 anos`,
    `déficit populacional mulheres 5 a 9 anos`
  ) |>
  pivot_longer(-ano, names_to = "sexo", values_to = "deficit")

mortes_infancia <- infancia_5_9 |>
  arrange(sexo, ano) |>
  group_by(sexo) |>
  mutate(
    mortes_incrementais = pmax(deficit - lag(deficit, default = first(deficit)), 0)
  ) |>
  ungroup()

summary(mortes_infancia)

excesso_mortalidade_infancia <- mortes_infancia |>
  filter(ano >= 1964, ano <= 1985) |>
  summarise(total = sum(mortes_incrementais, na.rm = TRUE)) |>
  pull(total)

cat("\nExcesso de mortalidade na infância (5-9 anos):", 
    format(round(excesso_mortalidade_infancia), big.mark = "."), "\n")

# 7. Análise de regime (mantido igual)
mortes_infancia <- mortes_infancia |>
  mutate(
    regime = as.integer(ano >= 1964 & ano <= 1985),
    t_regime = if_else(regime == 1, ano - 1963, 0),
    regime_f = factor(regime, labels = c("Fora do regime", "Durante o regime"))
  )

# 8. Visualização (mantido igual com adição do fator)
defict_infant <- ggplot(mortes_infancia, aes(x = ano, y = deficit)) +
  geom_point(alpha = 0.5) +
  geom_smooth(
    aes(color = regime_f, group = regime_f),
    method = "lm",
    se = FALSE,
    linewidth = 1.2
  ) +
  geom_vline(xintercept = c(1964, 1985), linetype = "dashed") +
  scale_color_manual(values = c("grey50", "#D62728")) +
  labs(
    title = "Déficit populacional na infância (5-9 anos) e regime autoritário",
    subtitle = paste("Com ajuste de fecundidade pelo método simplificado",
                     "\nExcesso estimado 1964-1985:", 
                     format(round(excesso_mortalidade_infancia), big.mark = ".")),
    x = "Ano",
    y = "Déficit populacional",
    color = "Período"
  ) +
  theme_ipea()

defict_infant
save_ipeaplot(
  defict_infant,
  "deficit_infantil_5_9_regime_ajustado",
  format = c("png", "eps")
)

# 9. Salvar dados ajustados
write_csv2(dados_5_9_aj, "dados_5_9_ajustados_fecundidade.csv")

# ==============================================================================
# EXPLICAÇÃO DO MÉTODO
# ==============================================================================
cat("\n" , strrep("=", 80), "\n")
cat("EXPLICAÇÃO DO MÉTODO DE AJUSTE DE FECUNDIDADE\n")
cat(strrep("=", 80), "\n\n")

cat("PROBLEMA: A interpolação exponencial entre censos (1960-1991) para a população\n")
cat("5-9 anos assume uma trajetória implícita de fecundidade.\n\n")

cat("SOLUÇÃO: Comparar a TFT real no período de nascimento da coorte com a TFT\n")
cat("que estaria implícita na interpolação linear entre 1960 e 1991.\n\n")

cat("CÁLCULO PARA CADA ANO t (observação da coorte 5-9 anos):\n")
cat("1. Anos de nascimento: t-9 até t-5\n")
cat("2. TFT_real = média da TFT real nesses anos\n")
cat("3. TFT_implícita = média da TFT linear entre 1960-1991 para os mesmos anos\n")
cat("4. Fator = TFT_real / TFT_implícita\n\n")

cat("INTERPRETAÇÃO DOS FATORES:\n")
cat("• Fator > 1: Fecundidade real foi MAIOR que a implícita\n")
cat("  → Contrafactual estava SUBESTIMADO\n")
cat("  → Ajuste para CIMA\n\n")

cat("• Fator < 1: Fecundidade real foi MENOR que a implícita\n")
cat("  → Contrafactual estava SUPERESTIMADO\n")
cat("  → Ajuste para BAIXO\n\n")

cat("• Fator ≈ 1: Fecundidade real próxima da implícita\n")
cat("  → Contrafactual adequado\n")
cat("  → Pouco ou nenhum ajuste necessário\n")


#==============================================================================
# 6. MORTALIDADE DE JOVENS (15-24)
#==============================================================================

# Preparar dados (15–19 e 20–24)

excesso_15_24_long <- df_raw |>
  select(
    ano,
    `déficit populacional homens 15 a 19 anos`,
    `déficit populacional mulheres 15 a 19 anos`,
    `déficit populacional homens 20 a 24 anos`,
    `déficit populacional mulheres 20 a 24 anos`
  ) |>
  pivot_longer(
    -ano,
    names_to = "variavel",
    values_to = "deficit"
  ) |>
  mutate(
    sexo = if_else(str_detect(variavel, "homens"), "homens", "mulheres"),
    faixa = case_when(
      str_detect(variavel, "15 a 19") ~ "15–19",
      str_detect(variavel, "20 a 24") ~ "20–24"
    )
  )

#Mortes incrementais (estoque → fluxo)
mortes_15_24 <- excesso_15_24_long |>
  arrange(sexo, faixa, ano) |>
  group_by(sexo, faixa) |>
  mutate(
    mortes_incrementais = pmax(deficit - lag(deficit, default = first(deficit)), 0)
  ) |>
  ungroup()

summary(mortes_15_24)

#Restringir ao período do regime

mortes_15_24_evento <- mortes_15_24 |>
  filter(ano >= 1964, ano <= 1985)

#Agregar resultados
excesso_15_24_resumo <- mortes_15_24_evento |>
  group_by(sexo, faixa) |>
  summarise(
    excesso_mortalidade = sum(mortes_incrementais, na.rm = TRUE),
    .groups = "drop"
  )

print(excesso_15_24_resumo)

excesso_15_24_masculino <- excesso_15_24_resumo |> filter(sexo == "homens")

print(excesso_15_24_masculino)

excesso_15_24_masc_total <-  sum(excesso_15_24_masculino$excesso_mortalidade)

print(excesso_15_24_masc_total)

excesso_15_24_total <- excesso_15_24_resumo |>
  summarise(
    excesso_15_24 = sum(excesso_mortalidade)
  ) |>
  pull(excesso_15_24)

print(excesso_15_24_total)

# Testes formais (fortemente recomendados)
#Quebra estrutural (Bai–Perron)

teste_quebra_15_24 <- lm(
  deficit ~ ano,
  data = excesso_15_24_long
)
summary(teste_quebra_15_24)


#Você deve observar quebras nos anos 1960/70, não antes.

#Placebo (10–14 anos)
placebo_10_14 <- df_raw |>
  select(
    ano,
    `déficit populacional homens 10 a 14 anos`,
    `déficit populacional mulheres 10 a 14 anos`
  ) |>
  pivot_longer(-ano, values_to = "deficit")

summary(lm(deficit ~ ano, data = placebo_10_14))


#para visualização
#construir estoque agregado:

jovens_15_24 <- df_raw |>
  transmute(
    ano,
    deficit =
      `déficit populacional homens 15 a 19 anos` +
      `déficit populacional mulheres 15 a 19 anos` +
      `déficit populacional homens 20 a 24 anos` +
      `déficit populacional mulheres 20 a 24 anos`
  )

#variáveis de regime: 
jovens_15_24 <- jovens_15_24 |>
  mutate(
    regime = as.integer(ano >= 1964 & ano <= 1985),
    t_regime = if_else(regime == 1, ano - 1963, 0)
  )

#gráfico

jovens_15_24 <- jovens_15_24 |>
  mutate(regime_f = factor(regime, labels = c("Fora do regime", "Durante o regime")))

ggplot(jovens_15_24, aes(x = ano, y = deficit)) +
  geom_point(alpha = 0.5) +
  geom_smooth(
    aes(color = regime_f, group = regime_f),
    method = "lm",
    se = FALSE,
    linewidth = 1.2
  ) +
  geom_vline(xintercept = c(1964, 1985), linetype = "dashed") +
  scale_color_manual(values = c("grey50", "#D62728")) +
  labs(
    title = "Déficit populacional 15–24 anos e regime autoritário",
    x = "Ano",
    y = "Déficit populacional",
    color = "Período"
  ) +
  theme_ipea()


# ==============================================================================
# 7. MORTALIDADE EM IDOSOS (60+)
# ==============================================================================

deficit_idosos <- df_raw |>
  select(ano, matches("déficit populacional (homens|mulheres) (60|70)")) |>
  pivot_longer(-ano, names_to = "variavel", values_to = "deficit")

mortes_idosos <- deficit_idosos |>
  arrange(variavel, ano) |>
  group_by(variavel) |>
  mutate(mortes_incrementais = pmax(deficit - lag(deficit, default = first(deficit)), 0)) |>
  ungroup()

summary(mortes_idosos)

excesso_idosos_total <- mortes_idosos |>
  filter(ano >= 1964, ano <= 1985) |>
  summarise(total = sum(mortes_incrementais, na.rm = TRUE)) |>
  pull(total)

print(excesso_idosos_total)

#===============================================================================
#7.1 Testes formais da mortalidade de idosos
#===============================================================================

#arrumar os dados para os testes

idosos_60mais <- df_raw |>
  transmute(
    ano,
    deficit =
      `déficit populacional homens 60 a 69 anos` +
      `déficit populacional mulheres 60 a 69 anos` +
      `déficit populacional homens 70 ou mais` +
      `déficit populacional mulheres 70 ou mais`
  )

idosos_60mais <- idosos_60mais |>
  mutate(
    regime = as.integer(ano >= 1964 & ano <= 1985),
    t_regime = if_else(regime == 1, ano - 1963, 0)
  )

#Teste de quebra estrutural endógena de Bai-Perron

modelo_idosos <- lm(deficit ~ ano, data = idosos_60mais)

quebras_idosos <- breakpoints(deficit ~ ano, data = idosos_60mais)

summary(quebras_idosos)
plot(quebras_idosos)

#Regressão segmentada pé e pós 1964

idosos_60mais <- idosos_60mais |>
  mutate(pos_1964 = as.integer(ano >= 1964))

modelo_segmentado_idosos <- lm(
  deficit ~ ano * regime,
  data = idosos_60mais
)

summary(modelo_segmentado_idosos)


#intervenção temporária

modelo_interv_idosos <- lm(
  deficit ~ ano + regime + t_regime,
  data = idosos_60mais
)

summary(modelo_interv_idosos)


#modelo de dupla quebra

idosos_60mais <- idosos_60mais |>
  mutate(
    t_pos_1964 = pmax(ano - 1963, 0),
    t_pos_1985 = pmax(ano - 1985, 0)
  )

modelo_dupla_quebra_idosos <- lm(
  deficit ~ ano + t_pos_1964 - t_pos_1985,
  data = idosos_60mais
)

summary(modelo_dupla_quebra_idosos)


#Teste placeboo
placebo_idosos <- idosos_60mais |>
  filter(ano < 1964)

summary(lm(deficit ~ ano, data = placebo_idosos))

#Visualização


idosos_60mais <- idosos_60mais |>
  mutate(regime_f = factor(regime, labels = c("Fora do regime", "Durante o regime")))

ggplot(idosos_60mais, aes(x = ano, y = deficit)) +
  geom_point(alpha = 0.5) +
  geom_smooth(
    aes(color = regime_f, group = regime_f),
    method = "lm",
    se = FALSE,
    linewidth = 1.2
  ) +
  geom_vline(xintercept = c(1964, 1985), linetype = "dashed") +
  scale_color_manual(values = c("grey50", "#D62728")) +
  labs(
    title = "Déficit populacional 60 ou mais anos  e regime autoritário",
    x = "Ano",
    y = "Déficit populacional",
    color = "Período"
  ) +
  theme_ipea()

# ==============================================================================
# 8. INTERVALOS FINAIS
# ==============================================================================

limite_inferior <- excesso_15_24_masc_total + mortes_adultos_homens 
estimativa_central <- excesso_15_24_total + mortes_adultos_total_valor
limite_superior <- excesso_15_24_total +
  mortes_adultos_total_valor +
  excesso_mortalidade_infancia +
  excesso_idosos_total

# ==============================================================================
# 9. TABELA FINAL
# ==============================================================================

tabela_componentes <- tibble(
  Componente = c(
    "Mortes documentadas",
    'Excesso jovens (15-24)',
    "Excesso adultos (25–59)",
    "Excesso infância (5–9)",
    "Excesso idosos (60+)",
    "TOTAL"
  ),
  Inferior = c(mortes_documentadas, excesso_15_24_masc_total, mortes_adultos_homens, 0, 0, limite_inferior),
  Central  = c(mortes_documentadas, excesso_15_24_total, mortes_adultos_total_valor, 0, 0, estimativa_central),
  Superior = c(mortes_documentadas, excesso_15_24_total, mortes_adultos_total_valor,
               excesso_mortalidade_infancia, excesso_idosos_total, limite_superior)
) |>
  mutate(across(-Componente, ~ comma(round(.x))))

print(tabela_componentes)


write_csv2(tabela_componentes, 
           file = "tabela de componentes.csv")

# ==============================================================================
# 10. DISPERSÃO: DOCUMENTADAS × ESTIMATIVAS
# ==============================================================================

dados_disp <- df_raw |>
  filter(ano >= 1964, ano <= 1985) |>
  transmute(
    documentadas = `mortos e desaparecidos documentados`,
    hom_t1 = `homicídios excesso (projetado - tendência1)`,
    hom_t2 = `homicídios excesso (projetado - tendência2)`,
    hom_t3 = `homicídios excesso (projetado - tendência3)`,
    viol_t1 = `excesso de mortes violentas (projetado - tendência 1)`,
    viol_t2 = `excesso de mortes violentas (projetado - tendência 2)`,
    viol_t3 = `excesso de mortes violentas (projetado - tendência 3)`
  )

dados_long_disp <- dados_disp |>
  pivot_longer(-documentadas, names_to = "cenario", values_to = "excesso")

#criação de alguns modelos
summary(lm(hom_t1 ~ documentadas, data = dados_disp))
summary(lm(hom_t2 ~ documentadas, data = dados_disp))
summary(lm(hom_t3 ~ documentadas, data = dados_disp))
summary(lm(viol_t1 ~ documentadas, data = dados_disp))
summary(lm(viol_t2 ~ documentadas, data = dados_disp))
summary(lm(viol_t3 ~ documentadas, data = dados_disp))

#Enfim, o gráfico
ggplot(dados_long_disp, aes(x = documentadas, y = excesso)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  facet_wrap(~ cenario, scales = "free_y") +
  labs(
    title = "Associação entre mortes documentadas e estimativas de excesso",
    subtitle = "Homicídios e mortes violentas – diferentes tendências (1964–1985)",
    x = "Mortes documentadas (por ano)",
    y = "Excesso estimado de mortes"
  ) +
  theme_ipea() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)

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

ggplot(dados_long_disp, aes(x = documentadas, y = excesso)) +
  geom_point(alpha = 0.7, size = 2, shape = 4) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  geom_text(
    data = labels_eq,
    aes(x = x, y = y, label = label),
    hjust = -0.9,
    vjust = 2.55,
    size = 3,
    color = "red"
  ) +
  facet_wrap(~ cenario, scales = "free_y") +
  labs(
    title = "Associação entre mortes documentadas e estimativas de excesso",
    subtitle = "Equações de regressão estimadas separadamente por cenário (1964–1985)",
    x = "Mortes documentadas (por ano)",
    y = "Excesso estimado de mortes"
  ) +
  theme_ipea() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)

#===================================================================
#11. TRIANGULAÇÃO DE DÉFICIT DEMOGRÁFICO E MORTES VIOLENTAS
#===================================================================

#ORGANIZAR OS DADOS
triang <- df_raw |>
  filter(ano >= 1964, ano <= 1985) |>
  transmute(
    ano,
    deficit_demografico =
      `déficit populacional homens 15 a 19 anos` +
      `déficit populacional mulheres 15 a 19 anos` +
      `déficit populacional homens 25 a 29 anos` +
      `déficit populacional homens 30 a 39 anos` +
      `déficit populacional homens 40 a 49 anos` +
      `déficit populacional homens 50 a 59 anos`,
    
    documentadas = `mortos e desaparecidos documentados`,
    
    homic_excesso_t1 = `homicídios excesso (projetado - tendência1)`,
    homic_excesso_t2 = `homicídios excesso (projetado - tendência2)`,
    homic_excesso_t3 = `homicídios excesso (projetado - tendência3)`,
    
    viol_excesso_t1 = `excesso de mortes violentas (projetado - tendência 1)`,
    viol_excesso_t2 = `excesso de mortes violentas (projetado - tendência 2)`,
    viol_excesso_t3 = `excesso de mortes violentas (projetado - tendência 3)`
  )

#regressões trianguladas

summary(lm(deficit_demografico ~ homic_excesso_t1 + ano, data = triang))

summary(lm(deficit_demografico ~ homic_excesso_t2 + ano, data = triang))

summary(lm(deficit_demografico ~ homic_excesso_t3 + ano, data = triang))

summary(lm(deficit_demografico ~ viol_excesso_t1 + ano, data = triang))

summary(lm(deficit_demografico ~ viol_excesso_t2 + ano, data = triang))

summary(lm(deficit_demografico ~ viol_excesso_t3 + ano, data = triang))

summary(lm(deficit_demografico ~ documentadas +  ano, data = triang))

summary(lm(deficit_demografico ~ documentadas + homic_excesso_t1 + ano, data = triang))

summary(lm(deficit_demografico ~ documentadas + homic_excesso_t2 + ano, data = triang))

summary(lm(deficit_demografico ~ documentadas + homic_excesso_t3 + ano, data = triang))

summary(lm(deficit_demografico ~ documentadas + viol_excesso_t1 + ano, data = triang))

summary(lm(deficit_demografico ~ documentadas + viol_excesso_t2 + ano, data = triang))

summary(lm(deficit_demografico ~ documentadas + viol_excesso_t3 + ano, data = triang))



#visualização: excesso de homicídios trend 1 => déficit populacional

ggplot(triang, aes(x = homic_excesso_t1, y = deficit_demografico)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(
    title = "Triangulação: homicídios e déficit demográfico",
    subtitle = "Brasil, 1964–1985",
    x = "Excesso de homicídios (tendência 1)",
    y = "Déficit populacional implícito"
  ) +
  theme_ipea()

#visualização: excesso de homicídios trend1 => déficit populacional

ggplot(triang, aes(x = homic_excesso_t2, y = deficit_demografico)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(
    title = "Triangulação: homicídios e déficit demográfico",
    subtitle = "Brasil, 1964–1985",
    x = "Excesso de homicídios (tendência 2)",
    y = "Déficit populacional implícito"
  ) +
  theme_ipea()

#visualização: excesso de homicídios trend 3 => déficit populacional

ggplot(triang, aes(x = homic_excesso_t3, y = deficit_demografico)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(
    title = "Triangulação: homicídios e déficit demográfico",
    subtitle = "Brasil, 1964–1985",
    x = "Excesso de homicídios (tendência 3)",
    y = "Déficit populacional implícito"
  ) +
  theme_ipea()

#visualização: mortes documentadas => déficit populacional; controle pelo ano

tempdef <- lm(deficit_demografico ~ ano, data = triang)
triang$residuos_ano <- tempdef$residuals
docdef <- lm(residuos_ano ~ documentadas, data = triang)
intercep_docdef <- coef(docdef)[["(Intercept)"]]
slope_docdef <- coef(docdef)[[2]]
txtdocdef <- sprintf('y = %.2f + %.2fx, r² = %.2f', intercep_docdef, slope_docdef, 
                     summary(docdef)$r.squared)

ggplot(triang, aes(x = documentadas, y = residuos_ano)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = T, linewidth = 1) +
  labs(
    title = "mortos e desaparecidos e déficit demográfico, controlado por trajetória temporal",
    subtitle = "Brasil, 1964–1985",
    x = "mortos e desaparecidos políticos documentados",
    y = "Déficit populacional implícito controlado por ano"
  ) +
  theme_ipea() + scale_color_ipea() +
  geom_text(x = Inf, y = -Inf, label = txtdocdef, color="black", size=5, hjust=1.1, vjust=-1.1)


#visualização: mortes violentas 1=> déficit populacional

ggplot(triang, aes(x = viol_excesso_t1, y = deficit_demografico)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(
    title = "Triangulação: excesso de mortes violentas e déficit demográfico",
    subtitle = "Brasil, 1964–1985",
    x = "excesso de mortes violentas (tendência 1)",
    y = "Déficit populacional implícito"
  ) +
  theme_ipea()

#visualização: mortes violentas 2=> déficit populacional

ggplot(triang, aes(x = viol_excesso_t2, y = deficit_demografico)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(
    title = "Triangulação: excesso de mortes violentas e déficit demográfico",
    subtitle = "Brasil, 1964–1985",
    x = "excesso de mortes violentas (tendência 2)",
    y = "Déficit populacional implícito"
  ) +
  theme_ipea()

#visualização: mortes violentas 3=> déficit populacional

ggplot(triang, aes(x = viol_excesso_t3, y = deficit_demografico)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(
    title = "Triangulação: excesso de mortes violentas e déficit demográfico",
    subtitle = "Brasil, 1964–1985",
    x = "excesso de mortes violentas (tendência 3)",
    y = "Déficit populacional implícito"
  ) +
  theme_ipea()


#Sincronia: magnitudes diferentes, tendências convergentes?

triang <- df_raw |>
  filter(ano >= 1964, ano <= 1985) |>
  transmute(
    ano,
    
    # Déficit demográfico implícito (jovens + adultos)
    deficit_demografico =
      `déficit populacional homens 15 a 19 anos` +
      `déficit populacional mulheres 15 a 19 anos` +
      `déficit populacional homens 25 a 29 anos` +
      `déficit populacional mulheres 25 a 29 anos` +
      `déficit populacional homens 30 a 39 anos` +
      `déficit populacional mulheres 30 a 39 anos` +
      `déficit populacional homens 40 a 49 anos` +
      `déficit populacional mulheres 40 a 49 anos` +
      `déficit populacional homens 50 a 59 anos` +
      `déficit populacional mulheres 50 a 59 anos`,
    
    documentadas = `mortos e desaparecidos documentados`,
    homic_excesso1 = `homicídios excesso (projetado - tendência1)`,
    homic_excesso2 = `homicídios excesso (projetado - tendência2)`,
    homic_excesso3 = `homicídios excesso (projetado - tendência3)`,
    viol_excesso1  = `excesso de mortes violentas (projetado - tendência 1)`,
    viol_excesso2  = `excesso de mortes violentas (projetado - tendência 2)`,
    viol_excesso3  = `excesso de mortes violentas (projetado - tendência 3)`
    
    
  )

triang_long <- triang |>
  pivot_longer(
    -ano,
    names_to = "serie",
    values_to = "valor"
  ) |>
  group_by(serie) |>
  mutate(valor_z = as.numeric(scale(valor))) |>
  ungroup()

ggplot(triang_long, aes(x = ano, y = valor_z, color = serie)) +
  geom_line(linewidth = 1.1) +
  geom_vline(xintercept = c(1964, 1985), linetype = "dashed") +
  labs(
    title = "Tendências temporais de diversas estimativas",
    subtitle = "Séries padronizadas (z-score)",
    x = NULL,
    y = "Desvio-padrão",
    color = NULL
  ) +
  scale_color_ipea(palette = "Orange-Blue") +
  theme_ipea()

#===============================================================================
#dados defasados
#===============================================================================

#lags

names(triang)

triang_lag <- triang |>
  arrange(ano) |>
  mutate(
    homic_excesso1_lag1 = lag(homic_excesso1, 1),
    homic_excesso1_lag2 = lag(homic_excesso1, 2)
  )

summary(lm(deficit_demografico ~ homic_excesso1 + homic_excesso1_lag1 + homic_excesso1_lag2, data = triang_lag))


#lags

triang_lag <- triang |>
  arrange(ano) |>
  mutate(
    homic_excesso2_lag1 = lag(homic_excesso2, 1),
    homic_excesso2_lag2 = lag(homic_excesso2, 2)
  )

summary(lm(deficit_demografico ~ homic_excesso2 + homic_excesso2_lag1 + homic_excesso2_lag2, data = triang_lag))


#lags

triang_lag <- triang |>
  arrange(ano) |>
  mutate(
    homic_excesso3_lag1 = lag(homic_excesso3, 1),
    homic_excesso3_lag2 = lag(homic_excesso3, 2)
  )

summary(lm(deficit_demografico ~ homic_excesso3 + homic_excesso3_lag1 + homic_excesso3_lag2, data = triang_lag))
#==============================================================================
#Gráfico de triangulação entre défict demográfico e excessos de mortes violentas
#==============================================================================

#Preparar os dados em formato long (passo-chave)
triang_long_disp <- triang |>
  select(
    deficit_demografico,
    homic_excesso1,
    homic_excesso2,
    homic_excesso3,
    viol_excesso1,
    viol_excesso2,
    viol_excesso3
  ) |>
  pivot_longer(
    -deficit_demografico,
    names_to = "cenario",
    values_to = "excesso"
  )

#Estimar regressões separadas por cenário (para as equações)
labels_eq_triang <- triang_long_disp |>
  group_by(cenario) |>
  do({
    m <- lm(deficit_demografico ~ excesso, data = .)
    s <- summary(m)
    
    tibble(
      intercepto = coef(m)[1],
      incl = coef(m)[2],
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

#Definir posições dos rótulos em cada facet
posicoes_triang <- triang_long_disp |>
  group_by(cenario) |>
  summarise(
    x = min(excesso, na.rm = TRUE),
    y = max(deficit_demografico, na.rm = TRUE)
  )

labels_eq_triang <- labels_eq_triang |>
  left_join(posicoes_triang, by = "cenario")

#GRÁFICO FINAL (síntese, 6 em 1)
ggplot(triang_long_disp, aes(x = excesso, y = deficit_demografico)) +
  geom_point(alpha = 0.7, size = 2, shape = 3) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  geom_text(
    data = labels_eq_triang,
    aes(x = x, y = y, label = label),
    hjust = -0.1,
    vjust = 1.1,
    size = 3,
    color = "red"
  ) +
  facet_wrap(~ cenario, scales = "free_x") +
  labs(
    title = "Triangulação entre déficit demográfico e violência letal",
    subtitle = "Excesso de homicídios e mortes violentas segundo diferentes tendências (1964–1985)",
    x = "Excesso estimado de mortes",
    y = "Déficit demográfico implícito"
  ) +
  theme_ipea() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)

#==============================================================================
#EXPLICAÇÃO ALTERNATIVA: A MIGRAÇÃO
#==============================================================================


# ============================================================
# DADOS DE MIGRAÇÃO (CENSOS – % da população)
# ============================================================

migracao_censo <- tibble::tribble(
  ~ano, ~perc_imigrantes,
  1950, 2.33,
  1960, 1.97,
  1970, 1.32,
  1980, 0.93
)

# interpolar para anos intercensitários
migracao_ano <- tibble(ano = 1950:1985) |>
  left_join(migracao_censo, by = "ano") |>
  mutate(
    perc_imigrantes = approx(
      x = migracao_censo$ano,
      y = migracao_censo$perc_imigrantes,
      xout = ano,
      rule = 2
    )$y
  )

# ============================================================
# DÉFICIT DEMOGRÁFICO (jovens + adultos, como no seu modelo)
# ============================================================

deficit_demo <- df_raw |>
  filter(ano >= 1950, ano <= 1985) |>
  transmute(
    ano,
    deficit =
      `déficit populacional homens 15 a 19 anos` +
      `déficit populacional mulheres 15 a 19 anos` +
      `déficit populacional homens 25 a 29 anos` +
      `déficit populacional mulheres 25 a 29 anos` +
      `déficit populacional homens 30 a 39 anos` +
      `déficit populacional mulheres 30 a 39 anos` +
      `déficit populacional homens 40 a 49 anos` +
      `déficit populacional mulheres 40 a 49 anos` +
      `déficit populacional homens 50 a 59 anos` +
      `déficit populacional mulheres 50 a 59 anos`
  )

#========================================================================================
#TESTE FORMAL
#========================================================================================
sintese <- deficit_demo |>
  left_join(migracao_ano, by = "ano") |>
  mutate(
    deficit_z   = as.numeric(scale(deficit)),
    migracao_z  = as.numeric(scale(perc_imigrantes)),
    regime = ano >= 1964 & ano <= 1985
  )


summary(lm(deficit ~ perc_imigrantes + ano, data = sintese))


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
  1980, 0.0093
)

#Crescimento populacional por década
crescimento_decada <- pop_ano |>
  filter(ano %% 10 == 0) |>
  arrange(ano) |>
  mutate(
    decada = ano - 10,
    pop_inicial = lag(populacao),
    crescimento = populacao - pop_inicial
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

saldo_migratorio_anual_linear <- df_raw |>
  select(ano) |>
  mutate(
    decada = floor(ano / 10) * 10
  ) |>
  left_join(
    saldo_migratorio_decada,
    by = "decada"
  )



saldo_migratorio_anual_linear <- saldo_migratorio_anual_linear |>
  group_by(decada) |>
  mutate(
    n_anos = n(),
    saldo_migratorio_ano = saldo_migratorio / n_anos
  ) |>
  ungroup()


saldo_migratorio_anual_linear$ano <- saldo_migratorio_anual_linear$ano.x

triang_migracao <- triang |>
  left_join(
    saldo_migratorio_anual_linear |> select(ano, saldo_migratorio_ano),
    by = "ano"
  )

summary(
  lm(
    deficit_demografico ~ saldo_migratorio_ano + homic_excesso1 + ano,
    data = triang_migracao
  )
)

summary(
  lm(
    deficit_demografico ~ saldo_migratorio_ano + homic_excesso2 + ano,
    data = triang_migracao
  )
)

summary(
  lm(
    deficit_demografico ~ saldo_migratorio_ano + homic_excesso3 + ano,
    data = triang_migracao
  )
)

#visualização

anodef <- lm(deficit_demografico ~ ano, data = triang_migracao)
summary(anodef)
triang_migracao$residuos <- anodef$residuals
defic_migra <- lm(residuos ~ saldo_migratorio_ano, data = triang_migracao)
summary(defic_migra)
intercept_defmig <- coef(defic_migra)[["(Intercept)"]]
slope_defmig <- coef(defic_migra)[[2]]
txt_lm_dm <- sprintf('y = %.2f + %.2fx, r² = %.2f', intercept_defmig, slope_defmig, 
                     summary(defic_migra)$r.squared)
ggplot(triang_migracao, aes(x = saldo_migratorio_ano, y = residuos)) +
  geom_point() +
  geom_smooth(method = "lm", se = T, formula = y ~x, show.legend = T) +
  labs(title = "Déficit populacional controlado por ano e saldo migratório",
       x = "ano", y = "saldo migratório controlado por ano") +
  theme_ipea() + scale_color_ipea() +
  geom_text(x = Inf, y = -Inf, label = txt_lm_dm, color="black", size=5, hjust=1.1, vjust=-1.1)


#==============================================================
#ESTUDO DE COORTE
#==============================================================

#Coortes = pseudo-coortes (idade central)
#Unidade analítica = (sexo × coorte)
#Variável principal = gap coorte-ano
#70+ → idade central = 75 (assunção explícita)

library(dplyr)
library(tidyr)
library(stringr)

#Ir para formato long (apenas população observada e contrafactual)
df_long <- df_raw |>
  select(
    ano,
    matches("^(homens|mulheres|crianças) .* (anos|ou mais)$"),
    matches("^contrafactual (homens|mulheres|crianças) .* (anos|ou mais)$")
  ) |>
  pivot_longer(
    cols = -ano,
    names_to = "variavel",
    values_to = "pop"
  ) |>
  mutate(
    tipo = if_else(str_detect(variavel, "^contrafactual"), "contrafactual", "observado"),
    variavel = str_remove(variavel, "^contrafactual "),
    sexo = case_when(
      str_detect(variavel, "^homens") ~ "homens",
      str_detect(variavel, "^mulheres") ~ "mulheres",
      TRUE ~ "total"
    ),
    faixa_etaria = str_remove(variavel, "^(homens|mulheres|crianças) ")
  )

#Definir idade central e coorte
df_long <- df_long |>
  mutate(
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
      faixa_etaria == "70 ou mais" ~ 75
    ),
    coorte = ano - idade_central
  )


# Seguir as coortes ao longo do tempo.

#Abrir observado vs contrafactual lado a lado
df_coorte <- df_long |>
  select(ano, sexo, faixa_etaria, idade_central, coorte, tipo, pop) |>
  pivot_wider(
    names_from = tipo,
    values_from = pop
  ) |>
  mutate(
    gap = contrafactual - observado
  )


#gap = déficit coorte-ano, sem nenhuma suposição dinâmica.

#Trajetória da coorte (núcleo da análise)
df_coorte_traj <- df_coorte |>
  group_by(sexo, coorte) |>
  arrange(ano) |>
  mutate(
    delta_obs = observado - lag(observado),
    delta_cf  = contrafactual - lag(contrafactual),
    gap_fluxo = gap - lag(gap)
  ) |>
  ungroup()




#gap:	déficit instantâneo
#gap_fluxo:	variação líquida do déficit
#observado vs contrafactual:	sobrevivência implícita da coorte

#Foco nas coortes de identificação (robustez máxima)

#Exemplo: homens, 20–49 durante o choque.

df_id <- df_coorte_traj |>
  filter(
    sexo == "homens",
    idade_central >= 22.5,
    idade_central <= 44.5,
    ano >= 1964,
    ano <= 1985
  )




#tudo é mortalidade + migração

#Agregar por coorte (resultado “forte”)
df_resumo_coorte <- df_id |>
  group_by(coorte) |>
  summarise(
    deficit_medio = mean(gap, na.rm = TRUE),
    deficit_max = max(gap, na.rm = TRUE),
    perda_acumulada = sum(pmax(gap_fluxo, 0), na.rm = TRUE),
    .groups = "drop"
  )

summary(df_resumo_coorte)

#Teste de sanidade essencial (placebo)

#Coortes não expostas (ex.: homens 40–49 antes de 1964):

df_placebo <- df_coorte_traj |>
  filter(
    sexo == "homens",
    idade_central >= 44.5,
    idade_central <= 54.5,
    ano < 1964
  )

summary(df_placebo$gap)


#Deve ser ≈ 0.


