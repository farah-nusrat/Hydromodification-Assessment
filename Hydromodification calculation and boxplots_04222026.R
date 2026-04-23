
################Step 1: Hydromodification Calculation###################
library(ffcAPIClient)
library(writexl)
percentiles <- read.csv("~/Input Files/ffc_percentiles/ffc_percentiles_LR_Mendon_AA.csv")      # reference percentiles
ffc_values   <- read.csv("~/Input Files/ffc_output/ffc_output_LR_Mendon_AA.csv")     # same input reused
predictions  <- read.csv("~/Input Files/predictions/predictions_LR_Mendon_AA.csv")    # modeled values

result <- assess_alteration(
  percentiles = percentiles,
  predictions = predictions,
  ffc_values = ffc_values,
  comid = 664488, ##need to change the comid to match input gage
  annual = FALSE
)
##save result in excel file
write_xlsx(result, path = "~/Boxplot Input Files/LR_Mendon_AA.xlsx")
########This step will give us a table about the high/low/early/late/ altered/not altered for COMID


############################Step 2: Extract Predicted Values to create input Files#################

library(dplyr)
library(stringr)
library(purrr)
#COMIDS we are interested in
ids_keep <- c("4605050", "666170", "666156", "664424", "664488", "664510",
              "10274468", "10274270", "10093066", "10276836", "10276856",
              "10276878", "10093052", "10093110", "10274616", "10093214",
              "10390290", "10389562", "10348934", "10376596", "10373794", 
              "10349360", "10349220")

file_path <- "C:/Update RF scripts & files/RF_Prediction_08072025/UT_NHD_FFMs/Model Prediction_04212026/HF_Dur_Top_variables_nhd_avg_New.csv"
df <- read.csv("C:/Update RF scripts & files/RF_Prediction_08072025/UT_NHD_FFMs/Model Prediction_04212026/HF_Dur_Top_variables_nhd_avg_New.csv")
#Take file prefix
prefix <- basename(file_path) %>%
  str_remove("_Top_variables_nhd_avg_New\\.csv$")

####Each predicted metric data with COMID will be saved as a dataframe and later they will be merged based on COMID and Water Year
####Peak_2 does not have Water Year column, so they will be joined separately
####Same rule needs to follow for observed/current metric and then both observed and predicted files need to be merged. 
####Here, only Prediction file part is shown. 

HF_Dur <- df %>%
  select(-AREA) %>%        # drop unwanted columns
  filter(COMID %in% ids_keep) %>%
  rename(!!prefix := Avg)

#################
file_path <- "C:/Update RF scripts & files/RF_Prediction_08072025/UT_NHD_FFMs/Model Prediction_04212026/HFR_Dur_Top_variables_nhd_avg_New.csv"
df <- read.csv("C:/Update RF scripts & files/RF_Prediction_08072025/UT_NHD_FFMs/Model Prediction_04212026/HFR_Dur_Top_variables_nhd_avg_New.csv")
prefix <- basename(file_path) %>%
  str_remove("_Top_variables_nhd_avg_New\\.csv$")

HFR_Dur <- df %>%
  select(-AREA) %>%        # drop unwanted columns
  filter(COMID %in% ids_keep) %>%
  rename(!!prefix := Avg)

#################
file_path <- "C:/Update RF scripts & files/RF_Prediction_08072025/UT_NHD_FFMs/Model Prediction_04212026/HF_Tim_Top_variables_nhd_avg_New.csv"
df <- read.csv("C:/Update RF scripts & files/RF_Prediction_08072025/UT_NHD_FFMs/Model Prediction_04212026/HF_Tim_Top_variables_nhd_avg_New.csv")
prefix <- basename(file_path) %>%
  str_remove("_Top_variables_nhd_avg_New\\.csv$")

HF_Tim <- df %>%
  select(-AREA) %>%        # drop unwanted columns
  filter(COMID %in% ids_keep) %>%
  rename(!!prefix := Avg)

#################
file_path <- "C:/Update RF scripts & files/RF_Prediction_08072025/UT_NHD_FFMs/Model Prediction_04212026/HFR_Tim_Top_variables_nhd_avg_New.csv"
df <- read.csv("C:/Update RF scripts & files/RF_Prediction_08072025/UT_NHD_FFMs/Model Prediction_04212026/HFR_Tim_Top_variables_nhd_avg_New.csv")
prefix <- basename(file_path) %>%
  str_remove("_Top_variables_nhd_avg_New\\.csv$")

HFR_Tim <- df %>%
  select(-AREA) %>%        # drop unwanted columns
  filter(COMID %in% ids_keep) %>%
  rename(!!prefix := Avg)

#################
file_path <- "C:/Update RF scripts & files/RF_Prediction_08072025/UT_NHD_FFMs/Model Prediction_04212026/WLF_Dur_Top_variables_nhd_avg_New.csv"
df <- read.csv("C:/Update RF scripts & files/RF_Prediction_08072025/UT_NHD_FFMs/Model Prediction_04212026/WLF_Dur_Top_variables_nhd_avg_New.csv")
prefix <- basename(file_path) %>%
  str_remove("_Top_variables_nhd_avg_New\\.csv$")

WLF_Dur <- df %>%
  select(-AREA) %>%        # drop unwanted columns
  filter(COMID %in% ids_keep) %>%
  rename(!!prefix := Avg)
#################
file_path <- "C:/Update RF scripts & files/RF_Prediction_08072025/UT_NHD_FFMs/Model Prediction_04212026/SLF_Dur_Top_variables_nhd_avg_New.csv"
df <- read.csv("C:/Update RF scripts & files/RF_Prediction_08072025/UT_NHD_FFMs/Model Prediction_04212026/SLF_Dur_Top_variables_nhd_avg_New.csv")
prefix <- basename(file_path) %>%
  str_remove("_Top_variables_nhd_avg_New\\.csv$")

SLF_Dur <- df %>%
  select(-AREA) %>%        # drop unwanted columns
  filter(COMID %in% ids_keep) %>%
  rename(!!prefix := Avg)

#################
file_path <- "C:/Update RF scripts & files/RF_Prediction_08072025/UT_NHD_FFMs/Model Prediction_04212026/SLF_Tim_Top_variables_nhd_avg_New.csv"
df <- read.csv("C:/Update RF scripts & files/RF_Prediction_08072025/UT_NHD_FFMs/Model Prediction_04212026/SLF_Tim_Top_variables_nhd_avg_New.csv")
prefix <- basename(file_path) %>%
  str_remove("_Top_variables_nhd_avg_New\\.csv$")

SLF_Tim <- df %>%
  select(-AREA) %>%        # drop unwanted columns
  filter(COMID %in% ids_keep) %>%
  rename(!!prefix := Avg)

#################
file_path <- "C:/Update RF scripts & files/RF_Prediction_08072025/UT_NHD_FFMs/Model Prediction_04212026/Peak_Fre_2_Top_variables_nhd_avg_New.csv"
df <- read.csv("C:/Update RF scripts & files/RF_Prediction_08072025/UT_NHD_FFMs/Model Prediction_04212026/Peak_Fre_2_Top_variables_nhd_avg_New.csv")
prefix <- basename(file_path) %>%
  str_remove("_Top_variables_nhd_avg_New\\.csv$")

Peak_Fre_2 <- df %>%
  select(-AREA) %>%        # drop unwanted columns
  filter(COMID %in% ids_keep) %>%
  rename(!!prefix := Avg)

#################
file_path <- "C:/Update RF scripts & files/RF_Prediction_08072025/UT_NHD_FFMs/Model Prediction_04212026/HF_Mag_10_Top_variables_nhd_avg_New.csv"
df <- read.csv("C:/Update RF scripts & files/RF_Prediction_08072025/UT_NHD_FFMs/Model Prediction_04212026/HF_Mag_10_Top_variables_nhd_avg_New.csv")
prefix <- basename(file_path) %>%
  str_remove("_Top_variables_nhd_avg_New\\.csv$")

HF_Mag_10 <- df %>%
  select(-AREA) %>%        # drop unwanted columns
  filter(COMID %in% ids_keep) %>%
  rename(!!prefix := Avg)
#################
file_path <- "C:/Update RF scripts & files/RF_Prediction_08072025/UT_NHD_FFMs/Model Prediction_04212026/HF_Mag_50_Top_variables_nhd_avg_New.csv"
df <- read.csv("C:/Update RF scripts & files/RF_Prediction_08072025/UT_NHD_FFMs/Model Prediction_04212026/HF_Mag_50_Top_variables_nhd_avg_New.csv")
prefix <- basename(file_path) %>%
  str_remove("_Top_variables_nhd_avg_New\\.csv$")

HF_Mag_50 <- df %>%
  select(-AREA) %>%        # drop unwanted columns
  filter(COMID %in% ids_keep) %>%
  rename(!!prefix := Avg)

#################
file_path <- "C:/Update RF scripts & files/RF_Prediction_08072025/UT_NHD_FFMs/Model Prediction_04212026/SLF_Mag_50_Top_variables_nhd_avg_New.csv"
df <- read.csv("C:/Update RF scripts & files/RF_Prediction_08072025/UT_NHD_FFMs/Model Prediction_04212026/SLF_Mag_50_Top_variables_nhd_avg_New.csv")
prefix <- basename(file_path) %>%
  str_remove("_Top_variables_nhd_avg_New\\.csv$")

SLF_Mag_50 <- df %>%
  select(-AREA) %>%        # drop unwanted columns
  filter(COMID %in% ids_keep) %>%
  rename(!!prefix := Avg)
#################
file_path <- "C:/Update RF scripts & files/RF_Prediction_08072025/UT_NHD_FFMs/Model Prediction_04212026/SLF_Mag_90_Top_variables_nhd_avg_New.csv"
df <- read.csv("C:/Update RF scripts & files/RF_Prediction_08072025/UT_NHD_FFMs/Model Prediction_04212026/SLF_Mag_90_Top_variables_nhd_avg_New.csv")
prefix <- basename(file_path) %>%
  str_remove("_Top_variables_nhd_avg_New\\.csv$")

SLF_Mag_90 <- df %>%
  select(-AREA) %>%        # drop unwanted columns
  filter(COMID %in% ids_keep) %>%
  rename(!!prefix := Avg)

#################
file_path <- "C:/Update RF scripts & files/RF_Prediction_08072025/UT_NHD_FFMs/Model Prediction_04212026/WLF_Mag_90_Top_variables_nhd_avg_New.csv"
df <- read.csv("C:/Update RF scripts & files/RF_Prediction_08072025/UT_NHD_FFMs/Model Prediction_04212026/WLF_Mag_90_Top_variables_nhd_avg_New.csv")
prefix <- basename(file_path) %>%
  str_remove("_Top_variables_nhd_avg_New\\.csv$")

WLF_Mag_90 <- df %>%
  select(-AREA) %>%        # drop unwanted columns
  filter(COMID %in% ids_keep) %>%
  rename(!!prefix := Avg)

#################
file_path <- "C:/Update RF scripts & files/RF_Prediction_08072025/UT_NHD_FFMs/Model Prediction_04212026/WLF_Mag_50_Top_variables_nhd_avg_New.csv"
df <- read.csv("C:/Update RF scripts & files/RF_Prediction_08072025/UT_NHD_FFMs/Model Prediction_04212026/WLF_Mag_50_Top_variables_nhd_avg_New.csv")
prefix <- basename(file_path) %>%
  str_remove("_Top_variables_nhd_avg_New\\.csv$")

WLF_Mag_50 <- df %>%
  select(-AREA) %>%        # drop unwanted columns
  filter(COMID %in% ids_keep) %>%
  rename(!!prefix := Avg)

#################
file_path <- "C:/Update RF scripts & files/RF_Prediction_08072025/UT_NHD_FFMs/Model Prediction_04212026/HFA_ROC_Top_variables_nhd_avg_New.csv"
df <- read.csv("C:/Update RF scripts & files/RF_Prediction_08072025/UT_NHD_FFMs/Model Prediction_04212026/HFA_ROC_Top_variables_nhd_avg_New.csv")
prefix <- basename(file_path) %>%
  str_remove("_Top_variables_nhd_avg_New\\.csv$")

HFA_ROC <- df %>%
  select(-AREA) %>%        # drop unwanted columns
  filter(COMID %in% ids_keep) %>%
  rename(!!prefix := Avg)

#################
file_path <- "C:/Update RF scripts & files/RF_Prediction_08072025/UT_NHD_FFMs/Model Prediction_04212026/HFR_ROC_Top_variables_nhd_avg_New.csv"
df <- read.csv("C:/Update RF scripts & files/RF_Prediction_08072025/UT_NHD_FFMs/Model Prediction_04212026/HFR_ROC_Top_variables_nhd_avg_New.csv")
prefix <- basename(file_path) %>%
  str_remove("_Top_variables_nhd_avg_New\\.csv$")

HFR_ROC <- df %>%
  select(-AREA) %>%        # drop unwanted columns
  filter(COMID %in% ids_keep) %>%
  rename(!!prefix := Avg)
#################
file_path <- "C:/Update RF scripts & files/RF_Prediction_08072025/UT_NHD_FFMs/Model Prediction_04212026/Peak_Dur_2_Top_variables_nhd_avg_New.csv"
df <- read.csv("C:/Update RF scripts & files/RF_Prediction_08072025/UT_NHD_FFMs/Model Prediction_04212026/Peak_Dur_2_Top_variables_nhd_avg_New.csv")
prefix <- basename(file_path) %>%
  str_remove("_Top_variables_nhd_avg_New\\.csv$")

Peak_Dur_2 <- df %>%
  select(-AREA) %>%        # drop unwanted columns
  filter(COMID %in% ids_keep) %>%
  rename(!!prefix := Avg)

#################
file_path <- "C:/Update RF scripts & files/RF_Prediction_08072025/UT_NHD_FFMs/Model Prediction_04212026/Peak_2_Top_variables_nhd_avg_New.csv"
df <- read.csv("C:/Update RF scripts & files/RF_Prediction_08072025/UT_NHD_FFMs/Model Prediction_04212026/Peak_2_Top_variables_nhd_avg_New.csv")
prefix <- basename(file_path) %>%
  str_remove("_Top_variables_nhd_avg_New\\.csv$")


Peak_2 <- df %>%
  select(-AREA) %>%        # drop unwanted columns
  filter(COMID %in% ids_keep) %>%
  group_by(COMID) %>%
  summarise(Avg = mean(Avg, na.rm =TRUE), .groups = "drop")%>%
  rename(!!prefix := Avg)

##combine all 17 metrics except Peak_2 as it does not have WY column
df_list <- list(HF_Mag_10, HF_Mag_50, HF_Tim, HF_Dur, Peak_Dur_2, Peak_Fre_2, HFA_ROC, HFR_Tim, HFR_Dur, HFR_ROC, SLF_Tim, WLF_Mag_50, WLF_Mag_90, WLF_Dur, SLF_Mag_50, SLF_Mag_90, SLF_Dur)
df_combined <- reduce(df_list, full_join, by = c("COMID","WY"))

#join peak_2
df_combined <- df_combined %>%
  left_join(Peak_2, by = "COMID")
#relocat peak_2 column to matc with observed/current metrics
df_combined_Final <- df_combined %>%
  relocate(Peak_2, .after = HF_Dur)
#Add a Group column with Prediction in text
df_combined_Final <- df_combined_Final %>%
  mutate(Group = "Prediction") %>%
  select(Group, everything())

#Now split the dataframe to separate COMID for input files, which later need to be merged with current/observed metric value
df_combined_Final %>%
  group_split(COMID) %>%
  walk(~write.csv(.x, paste0("C:/Users/A02353401/Desktop/Output Prediction/COMID_", unique(.x$COMID), ".csv")))



###################################Step 3: Create Hydromodification Boxplots##################################
library(ggplot2)
library(tidyr)
library(scales)
library(dplyr)

df <- read.csv("~/Boxplot Input Files/LR_Mendon_AA_Input.csv")
#######1-HF_Mag_10#########

df$HF_Mag_10 = as.numeric(df$HF_Mag_10)

# Create custom summary function
percentile_box <- function(x) {
  data.frame(
    ymin  = quantile(x, 0.10),  # 10th percentile
    lower = quantile(x, 0.25),  # 25th percentile
    middle= quantile(x, 0.50),  # median
    upper = quantile(x, 0.75),  # 75th percentile
    ymax  = quantile(x, 0.90)   # 90th percentile
  )
}

p <- ggplot(df, aes(x = Group, y = HF_Mag_10, fill = Group)) +
  stat_summary(
    fun.data = percentile_box,
    geom = "boxplot",
    width = 0.4,
    color = "black"
  ) +
  labs(y = "HF_Mag_10 (cfs)", x = NULL) +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_manual(values = c("Current" = "gray80", "Predicted" = "#1f77b4")) +
  theme_classic(base_size = 16) +  
  theme(legend.position = "none")
ggsave("HF_Mag_10.png", plot = p, width = 8, height = 6, dpi = 300)

#######2-HF_Mag_50#########

df$HF_Mag_50 = as.numeric(df$HF_Mag_50)

# Create custom summary function
percentile_box <- function(x) {
  data.frame(
    ymin  = quantile(x, 0.10),  # 10th percentile
    lower = quantile(x, 0.25),  # 25th percentile
    middle= quantile(x, 0.50),  # median
    upper = quantile(x, 0.75),  # 75th percentile
    ymax  = quantile(x, 0.90)   # 90th percentile
  )
}

p <- ggplot(df, aes(x = Group, y = HF_Mag_50, fill = Group)) +
  stat_summary(
    fun.data = percentile_box,
    geom = "boxplot",
    width = 0.4,
    color = "black"
  ) +
  labs(y = "HF_Mag_50 (cfs)", x = NULL) +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_manual(values = c("Current" = "gray80", "Predicted" = "#1f77b4")) + 
  theme_classic(base_size = 16) +  
  theme(legend.position = "none")
ggsave("HF_Mag_50.png", plot = p, width = 8, height = 6, dpi = 300)


#######3-HF_Dur#########

df$HF_Dur = as.numeric(df$HF_Dur)

# Create custom summary function
percentile_box <- function(x) {
  data.frame(
    ymin  = quantile(x, 0.10),  # 10th percentile
    lower = quantile(x, 0.25),  # 25th percentile
    middle= quantile(x, 0.50),  # median
    upper = quantile(x, 0.75),  # 75th percentile
    ymax  = quantile(x, 0.90)   # 90th percentile
  )
}

p <- ggplot(df, aes(x = Group, y = HF_Dur, fill = Group)) +
  stat_summary(
    fun.data = percentile_box,
    geom = "boxplot",
    width = 0.4,
    color = "black"
  ) +
  labs(y = "HF_Dur (days)", x = NULL) +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_manual(values = c("Current" = "gray80", "Predicted" = "#1f77b4")) +
  theme_classic(base_size = 16) +  
  theme(legend.position = "none")
ggsave("HF_Dur.png", plot = p, width = 8, height = 6, dpi = 300)


#######4-HF_Tim#########

df$HF_Tim = as.numeric(df$HF_Tim)

# Create custom summary function
percentile_box <- function(x) {
  data.frame(
    ymin  = quantile(x, 0.10),  # 10th percentile
    lower = quantile(x, 0.25),  # 25th percentile
    middle= quantile(x, 0.50),  # median
    upper = quantile(x, 0.75),  # 75th percentile
    ymax  = quantile(x, 0.90)   # 90th percentile
  )
}

p <- ggplot(df, aes(x = Group, y = HF_Tim, fill = Group)) +
  stat_summary(
    fun.data = percentile_box,
    geom = "boxplot",
    width = 0.4,
    color = "black"
  ) +
  labs(y = "HF_Tim (water year days)", x = NULL) +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_manual(values = c("Current" = "gray80", "Predicted" = "#1f77b4")) +  
  theme_classic(base_size = 16) +  
  theme(legend.position = "none")
ggsave("HF_Tim.png", plot = p, width = 8, height = 6, dpi = 300)

#######5-HFA_ROC#########

df$HFA_ROC = as.numeric(df$HFA_ROC)

# Create custom summary function
percentile_box <- function(x) {
  data.frame(
    ymin  = quantile(x, 0.10),  # 10th percentile
    lower = quantile(x, 0.25),  # 25th percentile
    middle= quantile(x, 0.50),  # median
    upper = quantile(x, 0.75),  # 75th percentile
    ymax  = quantile(x, 0.90)   # 90th percentile
  )
}

p <- ggplot(df, aes(x = Group, y = HFA_ROC, fill = Group)) +
  stat_summary(
    fun.data = percentile_box,
    geom = "boxplot",
    width = 0.4,
    color = "black"
  ) +
  labs(y = "HFA_ROC (fraction)", x = NULL) +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_manual(values = c("Current" = "gray80", "Predicted" = "#1f77b4")) +
  theme_classic(base_size = 16) +  
  theme(legend.position = "none")
ggsave("HFA_ROC.png", plot = p, width = 8, height = 6, dpi = 300)

#######6-HFR_Dur#########

df$HFR_Dur = as.numeric(df$HFR_Dur)

# Create custom summary function
percentile_box <- function(x) {
  data.frame(
    ymin  = quantile(x, 0.10),  # 10th percentile
    lower = quantile(x, 0.25),  # 25th percentile
    middle= quantile(x, 0.50),  # median
    upper = quantile(x, 0.75),  # 75th percentile
    ymax  = quantile(x, 0.90)   # 90th percentile
  )
}

p <- ggplot(df, aes(x = Group, y = HFR_Dur, fill = Group)) +
  stat_summary(
    fun.data = percentile_box,
    geom = "boxplot",
    width = 0.4,
    color = "black"
  ) +
  labs(y = "HFR_Dur (days)", x = NULL) +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_manual(values = c("Current" = "gray80", "Predicted" = "#1f77b4")) +
  theme_classic(base_size = 16) +  
  theme(legend.position = "none")
ggsave("HFR_Dur.png", plot = p, width = 8, height = 6, dpi = 300)


#######7-HFR_ROC#########

df$HFR_ROC = as.numeric(df$HFR_ROC)

# Create custom summary function
percentile_box <- function(x) {
  data.frame(
    ymin  = quantile(x, 0.10),  # 10th percentile
    lower = quantile(x, 0.25),  # 25th percentile
    middle= quantile(x, 0.50),  # median
    upper = quantile(x, 0.75),  # 75th percentile
    ymax  = quantile(x, 0.90)   # 90th percentile
  )
}

p <- ggplot(df, aes(x = Group, y = HFR_ROC, fill = Group)) +
  stat_summary(
    fun.data = percentile_box,
    geom = "boxplot",
    width = 0.4,
    color = "black"
  ) +
  labs(y = "HFR_ROC (fraction)", x = NULL) +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_manual(values = c("Current" = "gray80", "Predicted" = "#1f77b4")) +  
  theme_classic(base_size = 16) +  
  theme(legend.position = "none")
ggsave("HFR_ROC.png", plot = p, width = 8, height = 6, dpi = 300)


#######8-HFR_Tim#########

df$HFR_Tim = as.numeric(df$HFR_Tim)

# Create custom summary function
percentile_box <- function(x) {
  data.frame(
    ymin  = quantile(x, 0.10),  # 10th percentile
    lower = quantile(x, 0.25),  # 25th percentile
    middle= quantile(x, 0.50),  # median
    upper = quantile(x, 0.75),  # 75th percentile
    ymax  = quantile(x, 0.90)   # 90th percentile
  )
}

p <- ggplot(df, aes(x = Group, y = HFR_Tim, fill = Group)) +
  stat_summary(
    fun.data = percentile_box,
    geom = "boxplot",
    width = 0.4,
    color = "black"
  ) +
  labs(y = "HFR_Tim (water year days)", x = NULL) +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_manual(values = c("Current" = "gray80", "Predicted" = "#1f77b4")) +
  theme_classic(base_size = 16) +  
  theme(legend.position = "none")
ggsave("HFR_Tim.png", plot = p, width = 8, height = 6, dpi = 300)


#######9-Peak_Dur_2#########

df$Peak_Dur_2 = as.numeric(df$Peak_Dur_2)

# Create custom summary function
percentile_box <- function(x) {
  data.frame(
    ymin  = quantile(x, 0.10),  # 10th percentile
    lower = quantile(x, 0.25),  # 25th percentile
    middle= quantile(x, 0.50),  # median
    upper = quantile(x, 0.75),  # 75th percentile
    ymax  = quantile(x, 0.90)   # 90th percentile
  )
}

p <- ggplot(df, aes(x = Group, y = Peak_Dur_2, fill = Group)) +
  stat_summary(
    fun.data = percentile_box,
    geom = "boxplot",
    width = 0.4,
    color = "black"
  ) +
  labs(y = "Peak_Dur_2 (days)", x = NULL) +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_manual(values = c("Current" = "gray80", "Predicted" = "#1f77b4")) + 
  theme_classic(base_size = 16) +  
  theme(legend.position = "none")
ggsave("Peak_Dur_2.png", plot = p, width = 8, height = 6, dpi = 300)


#######10-Peak_Fre_2#########

df$Peak_Fre_2 = as.numeric(df$Peak_Fre_2)

# Create custom summary function
percentile_box <- function(x) {
  data.frame(
    ymin  = quantile(x, 0.10),  # 10th percentile
    lower = quantile(x, 0.25),  # 25th percentile
    middle= quantile(x, 0.50),  # median
    upper = quantile(x, 0.75),  # 75th percentile
    ymax  = quantile(x, 0.90)   # 90th percentile
  )
}

p <- ggplot(df, aes(x = Group, y = Peak_Fre_2, fill = Group)) +
  stat_summary(
    fun.data = percentile_box,
    geom = "boxplot",
    width = 0.4,
    color = "black"
  ) +
  labs(y = "Peak_Fre_2 (occurences)", x = NULL) +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_manual(values = c("Current" = "gray80", "Predicted" = "#1f77b4")) + 
  theme_classic(base_size = 16) +  
  theme(legend.position = "none")
ggsave("Peak_Fre_2.png", plot = p, width = 8, height = 6, dpi = 300)

#######11-SLF_Dur#########

df$SLF_Dur = as.numeric(df$SLF_Dur)

# Create custom summary function
percentile_box <- function(x) {
  data.frame(
    ymin  = quantile(x, 0.10),  # 10th percentile
    lower = quantile(x, 0.25),  # 25th percentile
    middle= quantile(x, 0.50),  # median
    upper = quantile(x, 0.75),  # 75th percentile
    ymax  = quantile(x, 0.90)   # 90th percentile
  )
}

p <- ggplot(df, aes(x = Group, y = SLF_Dur, fill = Group)) +
  stat_summary(
    fun.data = percentile_box,
    geom = "boxplot",
    width = 0.4,
    color = "black"
  ) +
  labs(y = "SLF_Dur (days)", x = NULL) +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_manual(values = c("Current" = "gray80", "Predicted" = "#1f77b4")) +
  theme_classic(base_size = 16) +  
  theme(legend.position = "none")
ggsave("SLF_Dur.png", plot = p, width = 8, height = 6, dpi = 300)


#######12-SLF_Mag_50#########

df$SLF_Mag_50 = as.numeric(df$SLF_Mag_50)
percentile_box <- function(x) {
  data.frame(
    ymin  = quantile(x, 0.10),  # 10th percentile
    lower = quantile(x, 0.25),  # 25th percentile
    middle= quantile(x, 0.50),  # median
    upper = quantile(x, 0.75),  # 75th percentile
    ymax  = quantile(x, 0.90)   # 90th percentile
  )
}

p <- ggplot(df, aes(x = Group, y = SLF_Mag_50, fill = Group)) +
  stat_summary(
    fun.data = percentile_box,
    geom = "boxplot",
    width = 0.4,
    color = "black"
  ) +
  labs(y = "SLF_Mag_50 (cfs)", x = NULL) +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_manual(values = c("Current" = "gray80", "Predicted" = "#1f77b4")) +
  theme_classic(base_size = 16) +  
  theme(legend.position = "none")
ggsave("SLF_Mag_50.png", plot = p, width = 8, height = 6, dpi = 300)

#######13-SLF_Mag_90#########

df$SLF_Mag_90 = as.numeric(df$SLF_Mag_90)

# Create custom summary function
percentile_box <- function(x) {
  data.frame(
    ymin  = quantile(x, 0.10),  # 10th percentile
    lower = quantile(x, 0.25),  # 25th percentile
    middle= quantile(x, 0.50),  # median
    upper = quantile(x, 0.75),  # 75th percentile
    ymax  = quantile(x, 0.90)   # 90th percentile
  )
}

p <- ggplot(df, aes(x = Group, y = SLF_Mag_90, fill = Group)) +
  stat_summary(
    fun.data = percentile_box,
    geom = "boxplot",
    width = 0.4,
    color = "black"
  ) +
  labs(y = "SLF_Mag_90 (cfs)", x = NULL) +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_manual(values = c("Current" = "gray80", "Predicted" = "#1f77b4")) + 
  theme_classic(base_size = 16) +  
  theme(legend.position = "none")
ggsave("SLF_Mag_90.png", plot = p, width = 8, height = 6, dpi = 300)


#######14-SLF_Tim#########

df$SLF_Tim = as.numeric(df$SLF_Tim)

# Create custom summary function
percentile_box <- function(x) {
  data.frame(
    ymin  = quantile(x, 0.10),  # 10th percentile
    lower = quantile(x, 0.25),  # 25th percentile
    middle= quantile(x, 0.50),  # median
    upper = quantile(x, 0.75),  # 75th percentile
    ymax  = quantile(x, 0.90)   # 90th percentile
  )
}

p <- ggplot(df, aes(x = Group, y = SLF_Tim, fill = Group)) +
  stat_summary(
    fun.data = percentile_box,
    geom = "boxplot",
    width = 0.4,
    color = "black"
  ) +
  labs(y = "SLF_Tim (water year days)", x = NULL) +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_manual(values = c("Current" = "gray80", "Predicted" = "#1f77b4")) +
  theme_classic(base_size = 16) +  
  theme(legend.position = "none")
ggsave("SLF_Tim.png", plot = p, width = 8, height = 6, dpi = 300)


#######15-WLF_Dur#########

df$WLF_Dur = as.numeric(df$WLF_Dur)

# Create custom summary function
percentile_box <- function(x) {
  data.frame(
    ymin  = quantile(x, 0.10),  # 10th percentile
    lower = quantile(x, 0.25),  # 25th percentile
    middle= quantile(x, 0.50),  # median
    upper = quantile(x, 0.75),  # 75th percentile
    ymax  = quantile(x, 0.90)   # 90th percentile
  )
}

p <- ggplot(df, aes(x = Group, y = WLF_Dur, fill = Group)) +
  stat_summary(
    fun.data = percentile_box,
    geom = "boxplot",
    width = 0.4,
    color = "black"
  ) +
  labs(y = "WLF_Dur (days)", x = NULL) +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_manual(values = c("Current" = "gray80", "Predicted" = "#1f77b4")) +
  theme_classic(base_size = 16) +  
  theme(legend.position = "none")
ggsave("WLF_Dur.png", plot = p, width = 8, height = 6, dpi = 300)


#######16-WLF_Mag_50#########

df$WLF_Mag_50 = as.numeric(df$WLF_Mag_50)

# Create custom summary function
percentile_box <- function(x) {
  data.frame(
    ymin  = quantile(x, 0.10),  # 10th percentile
    lower = quantile(x, 0.25),  # 25th percentile
    middle= quantile(x, 0.50),  # median
    upper = quantile(x, 0.75),  # 75th percentile
    ymax  = quantile(x, 0.90)   # 90th percentile
  )
}

p <- ggplot(df, aes(x = Group, y = WLF_Mag_50, fill = Group)) +
  stat_summary(
    fun.data = percentile_box,
    geom = "boxplot",
    width = 0.4,
    color = "black"
  ) +
  labs(y = "WLF_Mag_50 (cfs)", x = NULL) +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_manual(values = c("Current" = "gray80", "Predicted" = "#1f77b4")) +
  theme_classic(base_size = 16) +  
  theme(legend.position = "none")
ggsave("WLF_Mag_50.png", plot = p, width = 8, height = 6, dpi = 300)


#######17-WLF_Mag_90#########

df$WLF_Mag_90 = as.numeric(df$WLF_Mag_90)

# Create custom summary function
percentile_box <- function(x) {
  data.frame(
    ymin  = quantile(x, 0.10),  # 10th percentile
    lower = quantile(x, 0.25),  # 25th percentile
    middle= quantile(x, 0.50),  # median
    upper = quantile(x, 0.75),  # 75th percentile
    ymax  = quantile(x, 0.90)   # 90th percentile
  )
}

p <- ggplot(df, aes(x = Group, y = WLF_Mag_90, fill = Group)) +
  stat_summary(
    fun.data = percentile_box,
    geom = "boxplot",
    width = 0.4,
    color = "black"
  ) +
  labs(y = "WLF_Mag_90 (cfs)", x = NULL) +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_manual(values = c("Current" = "gray80", "Predicted" = "#1f77b4")) +
  theme_classic(base_size = 16) +  
  theme(legend.position = "none")
ggsave("WLF_Mag_90.png", plot = p, width = 8, height = 6, dpi = 300)

#######18-Peak_2#########

df$Peak_2 = as.numeric(df$Peak_2)

# Create custom summary function
percentile_box <- function(x) {
  data.frame(
    ymin  = quantile(x, 0.10),  # 10th percentile
    lower = quantile(x, 0.25),  # 25th percentile
    middle= quantile(x, 0.50),  # median
    upper = quantile(x, 0.75),  # 75th percentile
    ymax  = quantile(x, 0.90)   # 90th percentile
  )
}

p <- ggplot(df, aes(x = Group, y = Peak_2, fill = Group)) +
  stat_summary(
    fun.data = percentile_box,
    geom = "boxplot",
    width = 0.4,
    color = "black"
  ) +
  labs(y = "Peak_2 (cfs)", x = NULL) +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_manual(values = c("Current" = "gray80", "Predicted" = "#1f77b4")) +
  theme_classic(base_size = 16) +  
  theme(legend.position = "none")
ggsave("Peak_2.png", plot = p, width = 8, height = 6, dpi = 300)
