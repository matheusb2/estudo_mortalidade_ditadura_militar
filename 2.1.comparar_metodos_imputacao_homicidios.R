#==================================================
#imputação usando GAM (Generalized Additive Model)
#==================================================

library(mgcv)

gam_agressoes <- gam(
  agressoes_e_confrontos ~ s(MCE_SP, k = 5),
  data = dados_79_99,
  method = "REML"
)

gam_prop_media <- gam(
  homicidios_intencionais_prop_media ~ s(MCE_SP, k = 5),
  data = dados_79_99,
  method = "REML"
)

gam_rand_media <- gam(
  homicidios_intencionais_rand_media ~ s(MCE_SP, k = 5),
  data = dados_79_99,
  method = "REML"
)

#diagnóstico
gam.check(gam_agressoes)

gam.check(gam_prop_media)

gam.check(gam_rand_media)

#interpretação:
#k-index próximo de 1 → base suficiente
#EDF ≈ 1 → relação essencialmente linear
#EDF > 1 → não-linearidade relevante
# Se EDF ≈ 1, o GAM colapsa para um modelo linear, reforçando sua escolha original.

#====================================================
#Modelo Bayesiano (brms)
#====================================================

library(brms)

dados_79_99_bayes <- dados_79_99 |>
  mutate(
    MCE_SP_z = as.numeric(scale(MCE_SP)),
    agressoes_z = as.numeric(scale(agressoes_e_confrontos)),
    homicid_prp_z = as.numeric(scale(homicidios_intencionais_prop_media)),
    homicid_rnd_z = as.numeric(scale(homicidios_intencionais_rand_media))
  )

library(brms)

prior_corrigido <- c(
  prior(normal(0, 1), class = "b"),
  prior(normal(0, 1), class = "Intercept"),
  prior(exponential(1), class = "sigma")
)

bayes_agressoes_fix <- brm(
  agressoes_z ~ MCE_SP_z,
  data = dados_79_99_bayes,
  prior = prior_corrigido,
  chains = 4,
  iter = 4000,
  cores = 4,
  seed = 123,
  control = list(adapt_delta = 0.99)
)

bayes_hmcd_prp_fix <- brm(
  homicid_prp_z ~ MCE_SP_z,
  data = dados_79_99_bayes,
  prior = prior_corrigido,
  chains = 4,
  iter = 4000,
  cores = 4,
  seed = 123,
  control = list(adapt_delta = 0.99)
)

bayes_hmcd_rnd_fix <- brm(
  homicid_rnd_z ~ MCE_SP_z,
  data = dados_79_99_bayes,
  prior = prior_corrigido,
  chains = 4,
  iter = 4000,
  cores = 4,
  seed = 123,
  control = list(adapt_delta = 0.99)
)

#diagnóstico

bayes_R2(bayes_agressoes_fix)
pp_check(bayes_agressoes_fix)

bayes_R2(bayes_hmcd_prp_fix)
pp_check(bayes_hmcd_prp_fix)

bayes_R2(bayes_hmcd_rnd_fix)
pp_check(bayes_hmcd_rnd_fix)

#avaliação:
#Bayes R² >0.9
#relativo alinhamento entre as linhas dos gráficos

#=================================================
#Comparação entre métodos
#=================================================

tibble(
  modelo = c("LM", "GAM", "Bayes"),
  metrica = c("R² clássico", "R² clássico", "Bayes R²"),
  r2 = c(
    summary(mod_agressoes)$r.squared,
    summary(gam_agressoes)$r.sq,
    bayes_R2(bayes_agressoes_fix)[1]
  )
)

bayes_R2(bayes_agressoes_fix)[1]

tibble(
  modelo = c("LM", "GAM", "Bayes"),
  r2 = c(
    summary(mod_prop_media)$r.squared,
    summary(gam_prop_media)$r.sq,
    bayes_R2(bayes_hmcd_prp_fix)[1]
  )
)

tibble(
  modelo = c("LM", "GAM", "Bayes"),
  r2 = c(
    summary(mod_rand_media)$r.squared,
    summary(gam_rand_media)$r.sq,
    bayes_R2(bayes_hmcd_rnd_fix)[1]
  )
)


anos_para_imputar <- dados_1960_2022 |>
  filter(
    ano %in% c(1960, 1965, 1970, 1975),
    !is.na(MCE_SP)
  )


#especificações do modelo bayesiano

mce_media <- mean(dados_79_99$MCE_SP, na.rm = TRUE)
mce_sd <- sd(dados_79_99$MCE_SP, na.rm = TRUE)
anos_para_imputar_bayes <- anos_para_imputar |>
  mutate(
    MCE_SP_z = (MCE_SP - mce_media) / mce_sd
  )

pred_comp <- anos_para_imputar |>
  transmute(
    ano,
    lm = predict(mod_rand_media, newdata = anos_para_imputar),
    gam = predict(gam_rand_media, newdata = anos_para_imputar),
    bayes = fitted(
      bayes_hmcd_rnd_fix,
      newdata = anos_para_imputar_bayes,
      summary = TRUE
    )[, "Estimate"]
  )

pred_comp

plot(pred_comp)
summary(pred_comp)


# ==============================================================================
# CONFIGURAÇÃO DO BOOTSTRAP
# ==============================================================================
set.seed(123) # Para garantir que os resultados sejam replicáveis
n_bootstrap <- 2000
anos_imputacao <- c(1960, 1965, 1970, 1975)
dados_alvo <- pred_data %>% filter(ano %in% anos_imputacao)

# Matriz para guardar os resultados das simulações
# Linhas = Anos alvo, Colunas = Cada simulação
simulacoes <- matrix(NA, nrow = length(anos_imputacao), ncol = n_bootstrap)

# ==============================================================================
# EXECUÇÃO DO BOOTSTRAP (Resampling Residuals)
# ==============================================================================
# Pegamos os resíduos do seu modelo original (mod_rand_media)
residuos_hom <- residuals(mod_rand_media)
predicoes_base_hom <- predict(mod_rand_media, newdata = dados_alvo)

for(i in 1:n_bootstrap) {
  # Sorteamos resíduos com reposição
  residuos_sorteados <- sample(residuos, size = length(predicoes_base), replace = TRUE)
  
  # A nova simulação é a predição base + o erro sorteado
  simulacoes[, i] <- predicoes_base + residuos_sorteados
}

# ==============================================================================
# CÁLCULO DOS PERCENTIS E TRATAMENTO DE NEGATIVOS
# ==============================================================================
intervalos_bootstrap <- data.frame(
  ano = anos_imputacao,
  valor_imputado = predicoes_base,
  # Calculamos os percentis 2.5% e 97.5% das simulações
  lwr_90 = apply(simulacoes, 1, function(x) quantile(x, 0.05)),
  upr_90 = apply(simulacoes, 1, function(x) quantile(x, 0.95))
) %>%
  # REGRA DE SEGURANÇA: Se o limite inferior for negativo, vira zero
  mutate(across(c(lwr_90, upr_90), ~pmax(0, .x)))

# Visualizar resultados
print(intervalos_bootstrap)