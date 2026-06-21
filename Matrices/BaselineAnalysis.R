library(tidyverse)

# --- ARGENTINA ---
load("Matrices/MatrizARG.RData")
ARG_df <- as.data.frame(Matriz)
ARG_texts <- ARGTexts

# --- BRAZIL ---
load("Matrices/MatrizBRA.RData")
BRA_df <- as.data.frame(MATRIZ_BR)
BRA_texts <- BRATexts

# --- CHILE ---
load("Matrices/MatrizCHL.RData")
CHL_df <- as.data.frame(MATRIX_CL)
CHL_texts <- CHLTexts

# --- COLOMBIA ---
load("Matrices/MatrizCOL.RData")
COL_df <- as.data.frame(MATRIZ_CO)
COL_texts <- COLTexts

# --- COSTA RICA ---
load("Matrices/MatrizCORI.RData")
CRI_df <- as.data.frame(MATRIZ_CR)
CRI_texts <- CORITexts

# --- ECUADOR ---
load("Matrices/MatrizECU.RData")
ECU_df <- as.data.frame(MATRIZ_EC)
ECU_texts <- ECUTexts

# --- MEXICO ---
load("Matrices/MatrizMEX.RData")
MEX_df <- as.data.frame(MATRIX_MX)
MEX_texts <- MEXTexts

# --- URUGUAY ---
load("Matrices/MatrizURU.RData")
URY_df <- as.data.frame(MATRIZ_UY)
URY_texts <- URUTexts

# --- VENEZUELA ---
load("Matrices/MatrizVEN.RData")
VEN_df <- as.data.frame(MATRIZ_VE)
VEN_texts <- VENTexts



process_country <- function(df, texts, country_name) {
  
  df$program_id <- rownames(df)
  
  lengths <- texts %>%
    rename(program_id = Text,
           tokens = Tokens)
  
  data <- df %>%
    left_join(lengths, by = "program_id")
  
  stopifnot(sum(is.na(data$tokens)) == 0)
  
  skill_cols <- colnames(df)[1:10]
  
  data_norm <- data %>%
    mutate(across(all_of(skill_cols),
                  ~ ifelse(tokens > 0, .x / tokens, 0)))
  
  summary <- data_norm %>%
    summarise(across(all_of(skill_cols), mean))
  
  summary$country <- country_name
  
  return(summary)
}

country_summary <- bind_rows(
  process_country(ARG_df, ARG_texts, "Argentina"),
  process_country(BRA_df, BRA_texts, "Brazil"),
  process_country(CHL_df, CHL_texts, "Chile"),
  process_country(COL_df, COL_texts, "Colombia"),
  process_country(CRI_df, CRI_texts, "Costa Rica"),
  process_country(ECU_df, ECU_texts, "Ecuador"),
  process_country(MEX_df, MEX_texts, "Mexico"),
  process_country(URY_df, URY_texts, "Uruguay"),
  process_country(VEN_df, VEN_texts, "Venezuela")
)

library(tidyverse)

country_ranks <- country_summary %>%
  pivot_longer(-country, names_to = "skill", values_to = "value") %>%
  group_by(country) %>%
  mutate(rank = rank(-value, ties.method = "average")) %>%
  ungroup() %>%                      # ✅ important
  select(country, skill, rank) %>%   # ✅ THIS is the key fix
  pivot_wider(names_from = skill, values_from = rank)

country_ranks

library(Hmisc)
rank_matrix <- country_ranks %>%
  column_to_rownames("country") %>%
  as.matrix()

cor_matrix <- cor(t(rank_matrix), method = "spearman")

cor_matrix
range(cor_matrix[lower.tri(cor_matrix)])
