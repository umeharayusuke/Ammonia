library(tidyverse)
library(dplyr)
library(ggplot2)
library(gdxrrw)
library(stringr)
library(gridExtra)
library(patchwork)


setwd("C:/temp")

# theme -------------------------------------------------------------------

theme_1 <- theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 18),
        axis.text.x = element_text(angle = 45, size = 18, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        legend.position = "bottom", 
        legend.title = element_blank(),
        strip.background = element_blank())



# file check ------------------------------------------------------------

files <- list(
#  list(file = "SSP2_BaU_NoCC_No.gdx", scenario = "排出制約なし"),
  list(file = "SSP2_700C_2030CP_AMN_NoCC_No.gdx", scenario = "400C")
)
region <- "World"


df_Hyd <- data.frame()
for (file_info in files) {
  gdx_data <- rgdx.param(file_info$file, "IAMC_template")
  df_temp <- gdx_data %>%
    rename(Year = "Y")
  df_Hyd <- rbind(df_Hyd, df_temp) %>% 
    filter(REMF == region) %>%
    filter(str_detect(VEMF, "Hyd")) %>% 
    filter(Year == "2050")
}

df_Amm <- data.frame()
for (file_info in files) {
  gdx_data <- rgdx.param(file_info$file, "IAMC_template")
  df_temp <- gdx_data %>%
    rename(Year = "Y")
  df_Amm <- rbind(df_Amm, df_temp) %>% 
    filter(REMF == region) %>%
    filter(str_detect(VEMF, "Amm")) %>% 
    filter(Year == "2100")
}



folder_path <- "../../Routput"
name <- "GHG.png"
full_path <- file.path(folder_path, name)
ggsave(full_path, plot = g, width = 8, height = 5, dpi = 300)


# TRS_ENE_FL_load ---------------------------------------------------------


files <- list(
    #list(file = "global_17_SSP2_700C_2030CP_AMN_new_NoCC_No.gdx", scenario = "700C_new"),
  list(file = "global_17_SSP2_700C_2030CP_AMN_new_NoCC_No.gdx", scenario = "700C")
)


df_FL <- data.frame()
for (file_info in files) {
  gdx_data <- rgdx.param(file_info$file, "TRS_ENE_FL_load")
  df_temp <- gdx_data 
  df_FL <- rbind(df_FL, df_temp) %>% 
    filter(i3 == "NVFRIT") %>% 
    filter(i2=="JPN") %>% 
    rename(commodity = "i4")
}

#df_FL$i1 <- as.numeric(as.character(df_FL$i1))

g<-ggplot(df_FL, aes(x = i1, y = value, group = commodity, fill = commodity, color = commodity)) +
  geom_area(alpha = 1, position = "stack") +
  labs(
    title = "cbnal0/TRS_ENE_FL_load/NVFRIT/JPN",
    x = "Year",
    y = "energy souce type"
  ) +
  theme_minimal() +
   scale_x_discrete(
    breaks = c("2010", "2020", "2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
  ) 

plot(g)

folder_path <- "C:/temp/R/Ammonia"
name <- "TRS_ENE_FL_load_NVFRIT_JPN.png"
full_path <- file.path(folder_path, name)
ggsave(full_path, plot = g)


# Ammonia_IAMC ------------------------------------------------------------


files <- list(
  #  list(file = "SSP2_BaU_NoCC_No.gdx", scenario = "排出制約なし"),
  list(file = "SSP2_700C_2030CP_AMN_NoCC_No.gdx", scenario = "700C")
)
region <- "World"


df_Hyd <- data.frame()
for (file_info in files) {
  gdx_data <- rgdx.param(file_info$file, "IAMC_template")
  df_temp <- gdx_data %>%
    rename(Year = "Y")
  df_Hyd <- rbind(df_Hyd, df_temp) %>% 
    filter(REMF == region) %>%
    filter(str_detect(VEMF, "Fin_Ene")) %>% 
    filter(Year == "2050")
}

df_Amm <- data.frame()
for (file_info in files) {
  gdx_data <- rgdx.param(file_info$file, "IAMC_template")
  df_temp <- gdx_data %>%
    rename(Year = "Y")
  df_Amm <- rbind(df_Amm, df_temp) %>% 
    filter(REMF == region) %>%
    filter(str_detect(VEMF, "Amm")) %>% 
    filter(Year == "2100")
}



# PSAM_value---------------------------------------------------------


files <- list(
  #  list(file = "SSP2_BaU_NoCC_No.gdx", scenario = "排出制約なし"),
  list(file = "global_17_SSP2_700C_2030CP_AMN_new_NoCC_No.gdx", scenario = "700C")
)


df_FL <- data.frame()
for (file_info in files) {
  gdx_data <- rgdx.param(file_info$file, "PSAM_value")
  df_temp <- gdx_data 
  df_FL <- rbind(df_FL, df_temp) %>% 
    filter(i3 %in% c("HGB","HGG", "HGE","COM_HYG") )%>% 
    filter(i2=="JPN") %>% 
    filter(i4 %in% c("HGB","HGG", "HGE","COM_HYG") ) %>% 
    rename(materials = "i3")
}


g<-ggplot(df_FL, aes(x = i1, y = value, group = materials, fill = materials, color = materials)) +
  geom_area(alpha = 1, position = "stack") +
  labs(
    title = "cbnal0/PSAM_value/COM_AMN/JPN",
    x = "Year",
    y = "Energy materials"
  ) +
  theme_minimal() +
  scale_x_discrete(
    breaks = c("2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
  ) +
  # 手動で色を設定
  scale_fill_manual(
    values = c(
      "HGB" = "#1b9e77",
      "HGG" = "#d95f02",
      "HGE" = "#7570b3"
    )
  ) +
  scale_color_manual(
    values = c(
      "HGB" = "#1b9e77",
      "HGG" = "#d95f02",
      "HGE" = "#7570b3"
    )
  )

plot(g)

folder_path <- "C:/temp/R/Ammonia"
name <- "PSAM_value_JPN.png"
full_path <- file.path(folder_path, name)
ggsave(full_path, plot = g)


# Sec_Ene -----------------------------------------------------------------

df_FL <- data.frame()

gdx_data <- rgdx.param(file_info$file, "IAMC_template")
df_temp <- gdx_data %>%
  filter(REMF == "World") %>%
  filter(VEMF %in% c(
    "Sec_Ene_Hyd_Bio_w_CCS",
    "Sec_Ene_Hyd_Bio_wo_CCS",
    "Sec_Ene_Hyd_Ele",
    "Sec_Ene_Hyd_Gas_wo_CCS"
  ))

# 積み上げ順を指定（Gas → Bio → Ele）
df_temp$VEMF <- factor(
  df_temp$VEMF,
  levels = c(
    "Sec_Ene_Hyd_Gas_wo_CCS",
    "Sec_Ene_Hyd_Bio_w_CCS",
    "Sec_Ene_Hyd_Bio_wo_CCS",
    "Sec_Ene_Hyd_Ele"
  )
)

df_FL <- rbind(df_FL, df_temp)

g <- ggplot(df_FL, aes(x = Y, y = IAMC_template, group = VEMF, fill = VEMF, color = VEMF)) +
  geom_area(alpha = 1, position = "stack") +
  labs(
    title = "Secondary Energy",
    x = "Year",
    y = "Secondary Energy (EJ)"
  ) +
  theme_minimal() +
  scale_x_discrete(
    breaks = c("2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
  )+
  scale_fill_manual(
    values = c(
      "Sec_Ene_Hyd_Bio_w_CCS" = "green",
      "Sec_Ene_Hyd_Bio_wo_CCS" = "#1b9e77",
      "Sec_Ene_Hyd_Gas_wo_CCS" = "#d95f02",
      "Sec_Ene_Hyd_Ele" = "#7570b3"
    )
  ) +
  scale_color_manual(
    values = c(
      "Sec_Ene_Hyd_Bio_w_CCS" = "green",
      "Sec_Ene_Hyd_Bio_wo_CCS" = "#1b9e77",
      "Sec_Ene_Hyd_Gas_wo_CCS" = "#d95f02",
      "Sec_Ene_Hyd_Ele" = "#7570b3"
    )
  )

plot(g)



folder_path <- "C:/temp/R/Ammonia"
name <- "Sec_Ene_World.png"
full_path <- file.path(folder_path, name)
ggsave(full_path, plot = g)


# Fin_Ene -----------------------------------------------------------------

df_FL <- data.frame()

gdx_data <- rgdx.param(file_info$file, "IAMC_template")
df_temp <- gdx_data %>%
  filter(REMF == "World") %>%
  filter(VEMF %in% c(
    "Fin_Ene_Tra_Dom_Shi_Amm",
    "Fin_Ene_Tra_Int_Shi_Amm"
  ))


df_FL <- rbind(df_FL, df_temp)

g <- ggplot(df_FL, aes(x = Y, y = IAMC_template, group = VEMF, fill = VEMF, color = VEMF)) +
  geom_area(alpha = 1, position = "stack") +
  labs(
    title = "Final Energy",
    x = "Year",
    y = "Final Energy (EJ)"
  ) +
  theme_minimal() +
  scale_x_discrete(
    breaks = c("2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
  )

plot(g)



folder_path <- "C:/temp/R/Ammonia"
name <- "Fin_Ene_World.png"
full_path <- file.path(folder_path, name)
ggsave(full_path, plot = g)


# Inv_Ene_Sup_Amm -----------------------------------------------------------------

df_FL <- data.frame()

gdx_data <- rgdx.param(file_info$file, "IAMC_template")
df_temp <- gdx_data %>%
  filter(REMF == "World") %>%
  filter(VEMF %in% c(
    "Inv_Ene_Sup_Amm_Gas",
    "Inv_Ene_Sup_Amm_Ele",
    "Inv_Ene_Sup_Amm_Bio"
  ))


df_FL <- rbind(df_FL, df_temp)

g <- ggplot(df_FL, aes(x = Y, y = IAMC_template, group = VEMF, color = VEMF)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Investment",
    x = "Year",
    y = "Investments (billion US$2010)"
  ) +
  theme_minimal() +
  scale_x_discrete(
    breaks = c("2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
  )

plot(g)



folder_path <- "C:/temp/R/Ammonia"
name <- "Inv_Ene_World.png"
full_path <- file.path(folder_path, name)
ggsave(full_path, plot = g)





# Prc -----------------------------------------------------------------

df_FL <- data.frame()

gdx_data <- rgdx.param(file_info$file, "IAMC_template")
df_temp <- gdx_data %>%
  filter(REMF == "World") %>%
  filter(VEMF %in% c(
    "Prc_Sec_Ene_Amm",
    "Prc_Fin_Ene_Tra_Amm",
    "Prc_Fin_Ene_Amm"
  ))


df_FL <- rbind(df_FL, df_temp)

g <- ggplot(df_FL, aes(x = Y, y = IAMC_template, group = VEMF, color = VEMF)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Price",
    x = "Year",
    y = "Price (US$2010/GJ)"
  ) +
  theme_minimal() +
  scale_x_discrete(
    breaks = c("2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
  )

plot(g)



folder_path <- "C:/temp/R/Ammonia"
name <- "Prc_World.png"
full_path <- file.path(folder_path, name)
ggsave(full_path, plot = g)



# Capacity -----------------------------------------------------------------

df_FL <- data.frame()

gdx_data <- rgdx.param(file_info$file, "IAMC_template")
df_temp <- gdx_data %>%
  filter(REMF == "World") %>%
  filter(VEMF %in% c(
    "Cap_Amm_Bio_w_CCS",
    "Cap_Amm_Bio_wo_CCS",
    "Cap_Amm_Ele",
    "Cap_Amm_Gas_w_CCS",
    "Cap_Amm_Gas_wo_CCS"
  ))


df_FL <- rbind(df_FL, df_temp)

g <- ggplot(df_FL, aes(x = Y, y = IAMC_template, group = VEMF, color = VEMF)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Total available capacity of Ammonia plants",
    x = "Year",
    y = "Capacity(GW) "
  ) +
  theme_minimal() +
  scale_x_discrete(
    breaks = c("2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
  )

plot(g)



folder_path <- "C:/temp/R/Ammonia"
name <- "Cap_World.png"
full_path <- file.path(folder_path, name)
ggsave(full_path, plot = g)


# Trade -----------------------------------------------------------------

df_FL <- data.frame()

gdx_data <- rgdx.param(file_info$file, "IAMC_template")
df_temp <- gdx_data %>%
  filter(REMF == "World") %>%
  filter(VEMF %in% c(
    "Trd_Sec_Ene_Imp_Amm_Vol",
    "Trd_Sec_Ene_Exp_Amm_Vol"
  ))


df_FL <- rbind(df_FL, df_temp)

g <- ggplot(df_FL, aes(x = Y, y = IAMC_template, group = VEMF, color = VEMF)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Trade",
    x = "Year",
    y = "Import and Export volume (EJ)"
  ) +
  theme_minimal() +
  scale_x_discrete(
    breaks = c("2030", "2040", "2050", "2060", "2070", "2080", "2090", "2100")
  )

plot(g)



folder_path <- "C:/temp/R/Ammonia"
name <- "Trd_World.png"
full_path <- file.path(folder_path, name)
ggsave(full_path, plot = g)

