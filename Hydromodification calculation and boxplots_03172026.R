################Hydromodification Calculation###################
library(ffcAPIClient)
library(writexl)
percentiles <- read.csv("~/Input Files/ffc_percentiles/ffc_percentiles_LR_Mendon_AA.csv")      # reference percentiles
ffc_values   <- read.csv("~/Input Files/ffc_output/ffc_output_LR_Mendon_AA.csv")     # same input reused
predictions  <- read.csv("~/Input Files/predictions/predictions_LR_Mendon_AA.csv")    # modeled values

result <- assess_alteration(
  percentiles = percentiles,
  predictions = predictions,
  ffc_values = ffc_values,
  comid = 664488,
  annual = FALSE
)

write_xlsx(result, path = "~/Boxplot Input Files/LR_Mendon_AA.xlsx")






###################################Boxplots##################################
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
