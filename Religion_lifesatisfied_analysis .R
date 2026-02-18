library(ggplot2)
library(dplyr)
library(haven)
library(sf)        
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis) 
# 1. データ読み込み 
df0 <- readRDS("data/WVS_Cross-National_Wave_7_rds_v6_0.rds")

df1 <- df0 %>% 
  select(A_YEAR:B_COUNTRY, Q164:Q175, Q46:Q49, Q106) %>% 
  zap_labels()

      
# 2. 国コード対応表 
df_country_code <- tribble(
  ~code, ~iso3,
  8, "ALB",
  226, "GNQ",
  466, "MLI",
  705, "SVN",
  12, "DZA",
  231, "ETH",
  470, "MLT",
  706, "SOM",
  16, "ASM",
  232, "ERI",
  474, "MTQ",
  710, "ZAF",
  20, "AND",
  233, "EST",
  478, "MRT",
  724, "ESP",
  24, "AGO",
  246, "FIN",
  480, "MUS",
  736, "SDN",
  28, "ATG",
  250, "FRA",
  484, "MEX",
  740, "SUR",
  32, "ARG",
  268, "GEO",
  492, "MCO",
  752, "SWE",
  51, "ARM",
  270, "GMB",
  496, "MNG",
  756, "CHE",
  36, "AUS",
  624, "GNB",
  498, "MDA",
  760, "SYR",
  40, "AUT",
  276, "DEU",
  504, "MAR",
  410, "KOR",
  31, "AZE",
  288, "GHA",
  508, "MOZ",
  891, "SCG",
  50, "BGD",
  292, "GIB",
  104, "MMR",
  911, "SRB",
  52, "BRB",
  300, "GRC",
  912, "MNE",
  626, "TLS",
  56, "BEL",
  320, "GTM",
  807, "MKD",
  762, "TJK",
  60, "BMU",
  324, "GIN",
  516, "NAM",
  764, "THA",
  64, "BTN",
  328, "GUY",
  524, "NPL",
  768, "TGO",
  68, "BOL",
  826, "GBR",
  528, "NLD",
  780, "TTO",
  70, "BIH",
  332, "HTI",
  554, "NZL",
  788, "TUN",
  72, "BWA",
  340, "HND",
  558, "NIC",
  158, "TWN",
  76, "BRA",
  344, "HKG",
  562, "NER",
  792, "TUR",
  84, "BLZ",
  348, "HUN",
  566, "NGA",
  795, "TKM",
  100, "BGR",
  352, "ISL",
  578, "NOR",
  834, "TZA",
  854, "BFA",
  356, "IND",
  408, "PRK",
  784, "ARE",
  108, "BDI",
  360, "IDN",
  512, "OMN",
  800, "UGA",
  112, "BLR",
  364, "IRN",
  586, "PAK",
  804, "UKR",
  116, "KHM",
  368, "IRQ",
  591, "PAN",
  840, "USA",
  120, "CMR",
  372, "IRL",
  598, "PNG",
  850, "VIR",
  124, "CAN",
  376, "ISR",
  600, "PRY",
  858, "URY",
  148, "TCD",
  380, "ITA",
  604, "PER",
  860, "UZB",
  152, "CHL",
  400, "JOR",
  608, "PHL",
  704, "VNM",
  156, "CHN",
  388, "JAM",
  616, "POL",
  862, "VEN",
  170, "COL",
  392, "JPN",
  620, "PRT",
  887, "YEM",
  384, "CIV",
  398, "KAZ",
  275, "PSE",
  894, "ZMB",
  184, "COK",
  404, "KEN",
  630, "PRI",
  716, "ZWE",
  188, "CRI",
  414, "KWT",
  634, "QAT",
  191, "HRV",
  417, "KGZ",
  642, "ROU",
  192, "CUB",
  418, "LAO",
  643, "RUS",
  196, "CYP",
  422, "LBN",
  646, "RWA",
  203, "CZE",
  426, "LSO",
  682, "SAU",
  180, "COD",
  428, "LVA",
  144, "LKA",
  208, "DNK",
  430, "LBR",
  686, "SEN",
  214, "DOM",
  434, "LBY",
  690, "SYC",
  818, "EGY",
  450, "MDG",
  694, "SLE",
  218, "ECU",
  454, "MWI",
  702, "SGP",
  222, "SLV",
  458, "MYS",
  703, "SVK"
)

#  3. ISO3 を付与 
df2 <- df1 %>% 
  left_join(df_country_code, by = c("B_COUNTRY" = "code"))

# 確認
df2 %>% count(iso3)

df1 %>% count(B_COUNTRY)

# 4. 変数名を整理 
df3 <- df2 %>% 
  rename(
    important_god = Q164,
    believe_god = Q165,
    believe_life_after_death = Q166,
    believe_hell = Q167,
    believe_heaven = Q168, 
    happy = Q46, 
    healthy = Q47, 
    freedom_of_choice = Q48,
    life_satisfied = Q49,
    income_equality = Q106
  )


# ヒストグラムの作成(図2)
df4 <-df3 |> filter(life_satisfied>0) 
df4 %>%
  ggplot(aes(x = life_satisfied)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "white") +
  theme_minimal() +
  labs(title = "Distribution of Life Satisfaction", x = "Life Satisfaction (1-10)")


#欠損値を削除
df_country <- df3 %>% 
  filter(life_satisfied >= 0,
         important_god >= 0) %>%
  group_by(iso3) %>% 
  summarise(
    avg_god = mean(important_god, na.rm = TRUE),
    avg_sat = mean(life_satisfied, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )
#主要統計値の確認(分析対象国の確認)
summary(df_country)

#単回帰分析(表1)
model_country <- lm(avg_sat ~ avg_god, data = df_country)
summary(model_country)


#国に主要宗教の割り当て
df_religion <- tribble(
  ~iso3, ~major_religion,
  
  # --- Christianity (Catholic / Protestant / Orthodox) ---
  "AND","Christianity",
  "ARG","Christianity",
  "AUS","Christianity",
  "BOL","Christianity",
  "BRA","Christianity",
  "CAN","Christianity",
  "CHL","Christianity",
  "COL","Christianity",
  "CYP","Christianity",
  "CZE","Christianity",
  "DEU","Christianity",
  "ECU","Christianity",
  "GBR","Christianity",
  "GRC","Christianity",
  "GTM","Christianity",
  "MEX","Christianity",
  "NIC","Christianity",
  "NLD","Christianity",
  "NZL","Christianity",
  "PER","Christianity",
  "PHL","Christianity",
  "ROU","Christianity",
  "RUS","Christianity",
  "SVK","Christianity",
  "UKR","Christianity",
  "URY","Christianity",
  "USA","Christianity",
  "VEN","Christianity",
  "PRI","Christianity",
  
  # --- Islam ---
  "BGD","Islam",
  "EGY","Islam",
  "ETH","Christianity",   # キリスト教が僅差で多数
  "IDN","Islam",
  "IRN","Islam",
  "JOR","Islam",
  "KAZ","Islam",
  "KEN","Christianity",   # キリスト教多数
  "KGZ","Islam",
  "LBN","Islam",
  "LBY","Islam",
  "MAR","Islam",
  "MMR","Buddhism",
  "MYS","Islam",
  "NGA","Christianity",   # キリスト教多数
  "PAK","Islam",
  "SGP","Buddhism",
  "TJK","Islam",
  "TUN","Islam",
  "TUR","Islam",
  "UZB","Islam",
  
  # --- Buddhism / East Asian religions ---
  "CHN","None",
  "HKG","None",
  "JPN","None",
  "KOR","Christianity",
  "MNG","Buddhism",
  "THA","Buddhism",
  "TWN","None",
  "VNM","Buddhism",
  
  # --- Hinduism ---
  "IND","Hinduism",
  
  # --- Africa (Christian majority) ---
  "ZWE","Christianity",
  
  # --- Armenia ---
  "ARM","Christianity"
)
df_religion %>% count(major_religion)
df_country_religion <- df_country %>% 
  left_join(df_religion, by = "iso3")

#散布図の描画(図3)
df_country_religion %>% 
  ggplot(aes(x = avg_god, y = avg_sat, colour = major_religion)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", linetype = "dashed") +
  theme_minimal() +
  labs(
    x = "Average importance of God",
    y = "Average life satisfaction",
    colour = "Major religion"
  )



#重回帰分析(表1b)
re<-lm(avg_sat ~ avg_god + major_religion, data = df_country_religion)
summary(re)

# イスラム教の国だけを抜き出す
df_islam <- df_country_religion %>% 
  filter(major_religion == "Islam")

# イスラム教だけの回帰分析(表2)
model_islam <- lm(avg_sat ~ avg_god, data = df_islam)
summary(model_islam)

# サンプルサイズ（国数）の確認
nrow(df_islam)
#残差プロット(図5)
plot(model_country, which = 1)


#データの整形
df5<-df0 %>% 
  select(A_YEAR:B_COUNTRY, Q164:Q175, Q46:Q49, Q106) %>% 
  zap_labels() %>% 
  left_join(df_country_code, by = c("B_COUNTRY" = "code")) %>% 
  left_join(df_religion, by = "iso3") 
df5 <-df5 %>% 
  rename(
    important_god = Q164,
    believe_god = Q165,
    believe_life_after_death = Q166,
    believe_hell = Q167,
    believe_heaven = Q168, 
    happy = Q46, 
    healthy = Q47, 
    freedom_of_choice = Q48,
    life_satisfied = Q49, 
    income_equality = Q106
  )
#箱ひげ図(図4)
df5%>% 
  filter(!is.na(major_religion),
         !is.na(life_satisfied), 
         life_satisfied >= 1) %>% 
  ggplot(aes(x = major_religion, y = life_satisfied, fill = major_religion)) +
  geom_boxplot(outlier.alpha = 0.3) +
  labs(
    x = "Major religion (country level)",
    y = "Life satisfaction",
    title = "Life satisfaction by major religion"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#地図を描くための下準備
df_country_religion <- df_country %>% 
  left_join(df_religion, by = "iso3")

world <- ne_countries(scale = "medium", returnclass = "sf")

df_map <- world %>%
  left_join(df_country_religion, by = c("iso_a3" = "iso3"))

#地図(図1のa)
ggplot(df_map) +
  geom_sf(aes(fill = avg_god)) +
  scale_fill_viridis(name = "Importance of God", option = "magma", na.value = "grey90") +
  theme_minimal() +
  labs(title = "Average Importance of God by Country (WVS Wave 7)") +
  theme(legend.position = "bottom")
#地図(図1のb)
ggplot(df_map) +
  geom_sf(aes(fill = major_religion)) +
  scale_fill_brewer(palette = "Set2", na.value = "grey90") +
  theme_minimal() +
  labs(title = "Major Religion by Country") +
  theme(legend.position = "bottom")


