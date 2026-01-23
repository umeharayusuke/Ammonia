library(tidyverse)
library(dplyr)
library(ggplot2)
library(gdxrrw)
library(stringr)
library(gridExtra)
library(patchwork)
library(cowplot)
library(lemon)
library(purrr)
library(rnaturalearthdata)
library(rnaturalearth)


theme_1 <- theme_bw() +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, size = 16, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        legend.position = "right", 
        #legend.title = element_blank(),
        strip.background = element_blank())

setwd("data")


output_dir <- file.path("..", "output")
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# TRS_ENE_FL_load ---------------------------------------------------------


files <- list(
    #list(file = "global_17_SSP2_700C_2030CP_AMN_new_NoCC_No.gdx", scenario = "700C_new"),
  list(file = "global_17_SSP2_400C_2030CP_NoCC_No.gdx", scenario = "400C")
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

name <- "TRS_ENE_FL_load_NVFRIT_JPN.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)


# PSAM_value---------------------------------------------------------


files <- list(
  #  list(file = "SSP2_BaU_NoCC_No.gdx", scenario = "排出制約なし"),
  list(file = "global_17_SSP2_400C_2030CP_NoCC_No.gdx", scenario = "400C")
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

name <- "PSAM_value_JPN.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)
# Variable check ------------------------------------------------------------



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


# file load ---------------------------------------------------------------


files <- list(
  list(file = "SSP2_BaU_NoCC_No.gdx", scenario = "BaU"),
  list(file = "SSP2_400C_2030CP_NoCC_No.gdx", scenario = "400C")
)
region <- "World"


# Sec_Ene -----------------------------------------------------------------


fuel <- "Amm"   # "Hyd" or "Amm"


if (fuel == "Hyd") {
  VEMF_list <- c(
    "Sec_Ene_Hyd_Bio_w_CCS",
    "Sec_Ene_Hyd_Bio_wo_CCS",
    "Sec_Ene_Hyd_Ele",
    "Sec_Ene_Hyd_Gas_wo_CCS"
  )
  
} else if (fuel == "Amm") {
  VEMF_list <- c(
    "Sec_Ene_Amm_Bio_w_CCS",
    "Sec_Ene_Amm_Bio_wo_CCS",
    "Sec_Ene_Amm_Ele",
    "Sec_Ene_Amm_Gas_wo_CCS"
  )
  
}

df_FL <- data.frame()

for (file_info in files) {
  
  gdx_data <- rgdx.param(file_info$file, "IAMC_template")
  
  df_temp <- gdx_data %>%
    filter(REMF == region) %>%
    filter(VEMF %in% VEMF_list) %>%
    mutate(Scenario = file_info$scenario) 
  
  df_FL <- rbind(df_FL, df_temp)
}

df_FL$Y <- as.numeric(as.character(df_FL$Y))

g <- ggplot(
  df_FL,
  aes(x = Y, y = IAMC_template, fill = VEMF, color = VEMF)
) +
  geom_area(position = "stack") +
  labs(
    title = "Secondary Energy",
    x = "Year",
    y = "Secondary Energy (EJ)"
  ) +
  facet_wrap(~ Scenario, ncol = 1) +
  scale_x_continuous(
    breaks = c(2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)
  )

plot(g)

name <- "Sec_Ene_Amm.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)

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



name <- "Fin_Ene_World.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)



# Inv_Ene_Sup_Amm -----------------------------------------------------------------


fuel <- "Hyd"   # "Hyd" or "Amm"

if (fuel == "Hyd") {
  VEMF_list <- c(
    "Inv_Ene_Sup_Hyd_Gas",
    "Inv_Ene_Sup_Hyd_Ele",
    "Inv_Ene_Sup_Hyd_Bio"
  )
} else if (fuel == "Amm") {
  VEMF_list <- c(
    "Inv_Ene_Sup_Amm_Gas",
    "Inv_Ene_Sup_Amm_Ele",
    "Inv_Ene_Sup_Amm_Bio"
  )
}

df_FL <- data.frame()

gdx_data <- rgdx.param(file_info$file, "IAMC_template")

df_temp <- gdx_data %>%
  filter(REMF == "World") %>%
  filter(VEMF %in% VEMF_list)

df_FL <- rbind(df_FL, df_temp)


df_FL$Y <- as.numeric(as.character(df_FL$Y))

g <- ggplot(
  df_FL,
  aes(x = Y, y = IAMC_template, color = VEMF)
) +
  geom_line(linewidth = 1) +
  labs(
    title = paste("Investment –", fuel),
    x = "Year",
    y = "Investments (billion US$2010)"
  ) +
  theme_minimal() +
  scale_x_continuous(
    breaks = c(2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)
  )

plot(g)


name <- "Inv_Ene_World.png"


ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)


# Prc -----------------------------------------------------------------
fuel <- "Amm"

if (fuel == "Hyd") {
  VEMF_list <- c(
    "Prc_Sec_Ene_Hyd",
    "Prc_Fin_Ene_Tra_Hyd",
    "Prc_Fin_Ene_Hyd"
  )
} else if (fuel == "Amm") {
  VEMF_list <- c(
    "Prc_Sec_Ene_Amm",
    "Prc_Fin_Ene_Tra_Amm",
    "Prc_Fin_Ene_Amm"
  )
}

df_FL <- data.frame()

gdx_data <- rgdx.param(file_info$file, "IAMC_template")

df_temp <- gdx_data %>%
  filter(REMF == "World") %>%
  filter(VEMF %in% VEMF_list)

df_FL <- rbind(df_FL, df_temp)

df_FL$Y <- as.numeric(as.character(df_FL$Y))

g <- ggplot(
  df_FL,
  aes(x = Y, y = IAMC_template, color = VEMF)
) +
  geom_line(linewidth = 1) +
  labs(
    title = paste("Price –", fuel),
    x = "Year",
    y = "Price (US$2010/GJ)"
  ) +
  theme_minimal() +
  scale_x_continuous(
    breaks = c(2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)
  )

plot(g)


name <- "Prc_World.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)

# Capacity -----------------------------------------------------------------

fuel <- "Hyd"

if (fuel == "Hyd") {
  VEMF_list <- c(
    "Cap_Hyd_Bio_w_CCS",
    "Cap_Hyd_Bio_wo_CCS",
    "Cap_Hyd_Ele",
    "Cap_Hyd_Gas_w_CCS",
    "Cap_Hyd_Gas_wo_CCS"
  )
} else if (fuel == "Amm") {
  VEMF_list <- c(
    "Cap_Amm_Bio_w_CCS",
    "Cap_Amm_Bio_wo_CCS",
    "Cap_Amm_Ele",
    "Cap_Amm_Gas_w_CCS",
    "Cap_Amm_Gas_wo_CCS"
  )
}

df_FL <- data.frame()

gdx_data <- rgdx.param(file_info$file, "IAMC_template")

df_temp <- gdx_data %>%
  filter(REMF == "World") %>%
  filter(VEMF %in% VEMF_list)

df_FL <- rbind(df_FL, df_temp)

df_FL$Y <- as.numeric(as.character(df_FL$Y))

g <- ggplot(
  df_FL,
  aes(x = Y, y = IAMC_template, color = VEMF)
) +
  geom_line(linewidth = 1) +
  labs(
    title = paste("Total available capacity of", fuel, "plants"),
    x = "Year",
    y = "Capacity (GW)"
  ) +
  theme_minimal() +
  scale_x_continuous(
    breaks = c(2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)
  )

plot(g)


name <- "Cap_World.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)
# Trade -----------------------------------------------------------------
fuel <- "Amm"

if (fuel == "Hyd") {
  VEMF_list <- c(
    "Trd_Sec_Ene_Imp_Hyd_Vol",
    "Trd_Sec_Ene_Exp_Hyd_Vol"
  )
} else if (fuel == "Amm") {
  VEMF_list <- c(
    "Trd_Sec_Ene_Imp_Amm_Vol",
    "Trd_Sec_Ene_Exp_Amm_Vol"
  )
}

df_FL <- data.frame()

gdx_data <- rgdx.param(file_info$file, "IAMC_template")

df_temp <- gdx_data %>%
  filter(REMF == "World") %>%
  filter(VEMF %in% VEMF_list)

df_FL <- rbind(df_FL, df_temp)

df_FL$Y <- as.numeric(as.character(df_FL$Y))

g <- ggplot(
  df_FL,
  aes(x = Y, y = IAMC_template, color = VEMF)
) +
  geom_line(linewidth = 1) +
  labs(
    title = paste("Trade –", fuel),
    x = "Year",
    y = "Import and Export volume (EJ)"
  ) +
  theme_minimal() +
  scale_x_continuous(
    breaks = c(2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)
  )

plot(g)



name <- "Trd_World.png"

ggsave(
  filename = file.path(output_dir, name),
  plot = g,
  width = 12,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)