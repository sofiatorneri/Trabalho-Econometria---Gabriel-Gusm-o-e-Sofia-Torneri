# =========================================================
# 1. Pacotes
# =========================================================
library(readxl)
library(readr)
library(dplyr)
library(ggplot2)
library(lmtest)
library(sandwich)
library(modelsummary)
library(margins)
library(car)
library(tidyr)
library(moments)
library(stringr)
library(scales)

# =========================================================
# 2. Caminhos dos arquivos
# =========================================================
layout_path  <- "C:/Users/sofia/Downloads/microdados_ 14_municipios_com_areas_redefinidas_20160331/microdados_ 14_municipios_com_areas_redefinidas/Documentação/Layout/Layout_microdados_Amostra_14_munic_20160301.xls"

pessoas_path <- "C:/Users/sofia/Downloads/microdados_ 14_municipios_com_areas_redefinidas_20160331/microdados_ 14_municipios_com_areas_redefinidas/Dados/Amostra_Pessoas_14munic.txt"

# =========================================================
# 3. Leitura do layout e dos microdados de pessoas
# =========================================================

## 3.1 Layout
layout_raw <- read_excel(layout_path, sheet = "PESS")

layout_pessoa <- layout_raw %>%
  slice(-1) %>%
  transmute(
    var_name  = `Variáveis do Registro de Pessoas`,
    descricao = ...2,
    pos_ini   = as.integer(...8),
    pos_fim   = as.integer(...9),
    largura   = pos_fim - pos_ini + 1
  )

## 3.2 Microdados
pessoas <- read_fwf(
  file = pessoas_path,
  col_positions = fwf_widths(
    widths    = layout_pessoa$largura,
    col_names = layout_pessoa$var_name
  ),
  locale = locale(encoding = "latin1"),
  show_col_types = FALSE
)

# =========================================================
# 4. Construção da base de interesse
# =========================================================
base_final <- pessoas %>%
  mutate(
    idade_num        = as.numeric(V6036),
    ocupado_num      = as.numeric(V6910),
    renda_num        = as.numeric(V6511),
    peso_num         = as.numeric(V0014),
    tempo_desloc_raw = as.numeric(V0662)
  ) %>%
  filter(
    ocupado_num == 1,
    !is.na(idade_num),
    idade_num >= 15,
    idade_num <= 65,
    !is.na(renda_num),
    renda_num > 0,
    !is.na(tempo_desloc_raw),
    tempo_desloc_raw %in% 1:5
  ) %>%
  transmute(
    formal = case_when(
      V0648 %in% c(1, 3) ~ 1,
      TRUE               ~ 0
    ),
    tempo_desloc   = tempo_desloc_raw,
    idade          = idade_num,
    sexo = factor(
      V0601,
      levels = c(1, 2),
      labels = c("homem", "mulher")
    ),
    raca           = factor(V0606),
    escolaridade   = factor(V0633),
    renda_trabalho = renda_num,
    renda_log      = log(renda_trabalho + 1),
    peso           = peso_num
  )

# =========================================================
# 5. Base para regressão
# =========================================================
base_reg <- base_final %>%
  filter(
    !is.na(renda_log),
    !is.na(escolaridade)
  ) %>%
  mutate(
    tempo_cat = factor(
      tempo_desloc,
      levels = 1:5,
      labels = c(
        "até 5 min",
        "6–30 min",
        "31–60 min",
        "61–120 min",
        "mais de 120 min"
      )
    ),
    peso_norm   = peso / 1e14,
    idade2      = idade^2,
    desloc_curto = if_else(tempo_desloc <= 2, 1, 0)  # até 30 min
  )

# =========================================================
# 6. Modelos econométricos principais
# =========================================================

## 6.1 LPM
m1 <- lm(formal ~ tempo_cat,
         data = base_reg)

m2 <- lm(formal ~ tempo_cat + idade + idade2 + sexo + raca + escolaridade,
         data = base_reg)

m3 <- lm(formal ~ tempo_cat + idade + idade2 + sexo + raca + escolaridade + renda_log,
         data = base_reg)

## 6.2 Logit e Probit
logit <- glm(
  formal ~ tempo_cat + idade + idade2 + sexo + raca + escolaridade + renda_log,
  data   = base_reg,
  family = binomial(link = "logit")
)

probit <- glm(
  formal ~ tempo_cat + idade + idade2 + sexo + raca + escolaridade + renda_log,
  data   = base_reg,
  family = binomial(link = "probit")
)

# =========================================================
# 7. Probabilidades previstas por categoria de tempo
# =========================================================
calc_prob_logit <- function(cat, model, data) {
  nd <- data
  nd$tempo_cat <- factor(cat, levels = levels(data$tempo_cat))
  mean(predict(model, newdata = nd, type = "response"))
}

calc_prob_probit <- function(cat, model, data) {
  nd <- data
  nd$tempo_cat <- factor(cat, levels = levels(data$tempo_cat))
  mean(predict(model, newdata = nd, type = "response"))
}

prob_logit <- sapply(
  levels(base_reg$tempo_cat),
  calc_prob_logit,
  model = logit, data = base_reg
)

prob_probit <- sapply(
  levels(base_reg$tempo_cat),
  calc_prob_probit,
  model = probit, data = base_reg
)

# tabela de probabilidades prevista (para o artigo)
prob_df <- tibble(
  `Categoria de tempo` = factor(levels(base_reg$tempo_cat),
                                levels = levels(base_reg$tempo_cat)),
  Logit  = as.numeric(prob_logit),
  Probit = as.numeric(prob_probit)
)

# base longa para o gráfico das probabilidades
prob_plot_df <- tibble(
  tempo_cat   = factor(levels(base_reg$tempo_cat),
                       levels = levels(base_reg$tempo_cat)),
  prob_logit  = as.numeric(prob_logit),
  prob_probit = as.numeric(prob_probit)
) %>%
  pivot_longer(
    cols      = c(prob_logit, prob_probit),
    names_to  = "modelo",
    values_to = "prob"
  )

# =========================================================
# 8. Tabelas descritivas
# =========================================================

## 8.1 Numéricas
vars_num <- c(
  "idade",
  "renda_trabalho",
  "tempo_desloc",
  "formal"
)

labels_num <- c(
  idade          = "Idade (anos)",
  renda_trabalho = "Renda do trabalho principal (R$)",
  tempo_desloc   = "Tempo habitual de deslocamento (1–5)",
  formal         = "Emprego formal (1 = sim)"
)

calc_stats <- function(x) {
  c(
    "Média"         = mean(x, na.rm = TRUE),
    "Desvio-padrão" = sd(x,   na.rm = TRUE),
    "Mínimo"        = min(x,  na.rm = TRUE),
    "Máximo"        = max(x,  na.rm = TRUE)
  )
}

tab1_num <- lapply(vars_num, function(v) {
  x     <- base_reg[[v]]
  stats <- calc_stats(x)
  
  data.frame(
    Variável = labels_num[[v]],
    t(stats),
    check.names = FALSE,
    row.names   = NULL
  )
}) %>%
  bind_rows()

## 8.2 Categóricas
vars_cat <- c("sexo", "raca", "escolaridade", "tempo_cat")

labels_cat <- c(
  sexo         = "Sexo",
  raca         = "Raça/cor",
  escolaridade = "Escolaridade",
  tempo_cat    = "Tempo habitual de deslocamento (categorias)"
)

tab2_cat <- lapply(vars_cat, function(v) {
  x <- base_reg[[v]]
  
  df <- as.data.frame(table(x), stringsAsFactors = FALSE)
  names(df) <- c("Categoria", "n")
  
  df$`Proporção (%)` <- round(df$n / sum(df$n) * 100, 1)
  df$Variável        <- labels_cat[[v]]
  
  df[, c("Variável", "Categoria", "n", "Proporção (%)")]
}) %>%
  bind_rows()

# =========================================================
# 9. Testes adicionais e diagnósticos
# =========================================================

## 9.1 Heterocedasticidade (Breusch–Pagan) nos LPM
bp_m1 <- bptest(m1)
bp_m2 <- bptest(m2)
bp_m3 <- bptest(m3)

## 9.2 Resíduos do modelo principal
res_m3 <- residuals(m3)

## 9.3 Teste t de diferença de proporções (formalidade)
t_teste_formal <- t.test(formal ~ desloc_curto, data = base_reg)

## 9.4 Composição sexo x tempo de deslocamento
tab_comp_tempo_sexo <- base_reg %>%
  group_by(tempo_cat, sexo) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(tempo_cat) %>%
  mutate(prop = n / sum(n))

## 9.5 Multicolinearidade (VIF – modelo m3)
vif_m3 <- vif(m3)

# =========================================================
# 10. Análises de robustez
# =========================================================

## 10.1 Tempo de deslocamento como variável ordinal contínua
m_ord <- lm(formal ~ tempo_desloc + idade + idade2 + sexo + raca + escolaridade + renda_log,
            data = base_reg)

## 10.2 Exclusão de extremos (> 120 min)
base_noext <- base_reg %>%
  filter(tempo_cat != "mais de 120 min")

m_noext <- lm(formal ~ tempo_cat + idade + idade2 + sexo + raca + escolaridade + renda_log,
              data = base_noext)

# =========================================================
# 11. Momentos finitos e caudas
# =========================================================

## 11.1 Renda do trabalho
sum_renda_nivel <- summary(base_reg$renda_trabalho)
qt_renda_nivel  <- quantile(
  base_reg$renda_trabalho,
  probs = c(0, 0.01, 0.05, 0.5, 0.95, 0.99, 1),
  na.rm = TRUE
)

sum_renda_log <- summary(base_reg$renda_log)
qt_renda_log  <- quantile(
  base_reg$renda_log,
  probs = c(0, 0.01, 0.05, 0.5, 0.95, 0.99, 1),
  na.rm = TRUE
)

## 11.2 Resíduos do modelo m3
sum_res_m3 <- summary(res_m3)
qt_res_m3  <- quantile(
  res_m3,
  probs = c(0, 0.01, 0.05, 0.5, 0.95, 0.99, 1),
  na.rm = TRUE
)

skewness_res <- skewness(res_m3, na.rm = TRUE)
kurtosis_res <- kurtosis(res_m3, na.rm = TRUE)

# =========================================================
# 12. Paleta e tema para as figuras principais
# =========================================================
paleta <- list(
  primario    = "#1F4E79",
  secundario  = "#2E75B6",
  claro       = "#A6A6A6",
  escuro      = "#595959",
  fundo_claro = "#F2F2F2"
)

theme_artigo <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title         = element_blank(),
      plot.subtitle      = element_blank(),
      axis.title.x       = element_text(face = "bold"),
      axis.title.y       = element_text(face = "bold"),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position    = "bottom",
      legend.title       = element_blank()
    )
}

# =========================================================
# 13. Figuras principais
# =========================================================

## Figura 1 – distribuição de trabalhadores por tempo de deslocamento
fig1_tempo_dist <- base_reg %>%
  count(tempo_cat) %>%
  ggplot(aes(x = tempo_cat, y = n)) +
  geom_col(fill = paleta$primario) +
  labs(
    x = "Tempo habitual de deslocamento até o trabalho",
    y = "Número de trabalhadores"
  ) +
  theme_artigo()

## Figura 2 – proporção de emprego formal por tempo de deslocamento
tab_tempo <- base_reg %>%
  group_by(tempo_cat) %>%
  summarise(
    prop_formal = mean(formal),
    .groups     = "drop"
  )

fig2_formal_tempo <- ggplot(tab_tempo,
                            aes(x = tempo_cat, y = prop_formal)) +
  geom_col(fill = paleta$secundario) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = "Tempo habitual de deslocamento até o trabalho",
    y = "Proporção de trabalhadores em emprego formal"
  ) +
  theme_artigo()

## Figura 3 – matriz de correlação
vars_corr <- c("formal", "tempo_desloc", "idade", "renda_trabalho", "renda_log")

corr_mat <- base_reg %>%
  select(all_of(vars_corr)) %>%
  cor(use = "pairwise.complete.obs")

corr_long <- as.data.frame(as.table(corr_mat))
names(corr_long) <- c("Var1", "Var2", "correlacao")

corr_long$Var1 <- factor(corr_long$Var1, levels = vars_corr)
corr_long$Var2 <- factor(corr_long$Var2, levels = vars_corr)

fig3_corr <- ggplot(corr_long,
                    aes(x = Var1, y = Var2, fill = correlacao)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(correlacao, 2)),
            color = "black", size = 3) +
  scale_fill_gradient2(
    low    = paleta$secundario,
    mid    = "white",
    high   = "#0B5345",
    limits = c(-1, 1)
  ) +
  labs(
    x    = "",
    y    = "",
    fill = "Correlação"
  ) +
  theme_artigo() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

## Figura 4 – boxplot da renda do trabalho por tempo de deslocamento
fig4_box_renda_tempo <- ggplot(base_reg,
                               aes(x = tempo_cat, y = renda_trabalho)) +
  geom_boxplot(fill = paleta$primario, alpha = 0.8, outlier.alpha = 0.4) +
  scale_y_continuous(trans = "log1p") +
  labs(
    x = "Tempo habitual de deslocamento até o trabalho",
    y = "Renda do trabalho (R$)"
  ) +
  theme_artigo()

## Figura extra – probabilidades previstas (logit e probit)
fig_prob_modelos <- ggplot(prob_plot_df,
                           aes(x = tempo_cat, y = prob,
                               group = modelo, color = modelo)) +
  geom_point(size = 2) +
  geom_line() +
  labs(
    x     = "Tempo de deslocamento",
    y     = "Probabilidade prevista de emprego formal",
    color = "Modelo"
  ) +
  theme_minimal()

# =========================================================
# 14. Saídas (tabelas, testes e gráficos)
# =========================================================

## 14.1 Tabela – LPM
modelsummary(
  list(
    "LPM 1" = m1,
    "LPM 2" = m2,
    "LPM 3" = m3
  ),
  vcov  = "HC1",
  stars = TRUE
)

## 14.2 Tabela – probabilidades previstas (Logit e Probit)
datasummary_df(
  prob_df,
  title  = "Probabilidades previstas de emprego formal por tempo de deslocamento",
  notes  = "Probabilidades médias previstas a partir dos modelos Logit e Probit com o mesmo conjunto de controles.",
  output = "markdown"
)

## 14.3 Tabela – estatísticas descritivas numéricas
datasummary_df(
  tab1_num,
  title  = "Estatísticas descritivas das variáveis numéricas",
  notes  = "Fonte: elaboração própria a partir dos microdados da Amostra de Pessoas do Censo Demográfico de 2010 (IBGE).",
  output = "markdown"
)

## 14.4 Tabela – distribuição das variáveis categóricas
datasummary_df(
  tab2_cat,
  title  = "Distribuição das variáveis categóricas",
  notes  = "Fonte: elaboração própria a partir dos microdados da Amostra de Pessoas do Censo Demográfico de 2010 (IBGE).",
  output = "markdown"
)

## 14.5 Resultados dos testes
bp_m1; bp_m2; bp_m3
t_teste_formal
tab_comp_tempo_sexo
vif_m3
sum_renda_nivel; qt_renda_nivel
sum_renda_log;   qt_renda_log
sum_res_m3;      qt_res_m3
skewness_res;    kurtosis_res

## 14.6 Gráficos
print(fig1_tempo_dist)
print(fig2_formal_tempo)
print(fig3_corr)
print(fig4_box_renda_tempo)
print(fig_prob_modelos)

## 14.7 Gráficos de diagnóstico (base R)
qqnorm(res_m3,
       main = "QQ-plot dos resíduos – modelo m3")
qqline(res_m3, col = "red", lwd = 2)

hist(res_m3,
     breaks = 50,
     main   = "Histograma dos resíduos – m3",
     xlab   = "Resíduo")
