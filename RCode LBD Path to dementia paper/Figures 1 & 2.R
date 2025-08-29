
# Quant vs Semiquant panels ( with stats export)##################


# -------- Packages
req <- c("readxl","dplyr","ggplot2","scales","writexl")
to_install <- setdiff(req, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
invisible(lapply(req, library, character.only = TRUE))

# LOAD DIRECTORIES
excel_path <- #"/LOAD.xlsx"
setwd(#/FILE LOCATION")

base_family <- "Helvetica"   
base_size   <- 12

# Background fills 
bg_lb   <- NA
bg_ab   <- NA
bg_ptau <- NA

# -------- Load data
df <- readxl::read_excel(excel_path, sheet = "QUANTVSEMIQUANT")

# Column names (exact in Excel)
x_asyn <- "ASYN SEMIQUANT"; y_asyn <- "ASYN QUANT"
x_ab   <- "ABETA SEMIQUANT"; y_ab   <- "ABETA QUANT"
x_ptau <- "AT8 SEMIQUANT";   y_ptau <- "AT8 QUANT"

# -------- Helper: make a panel (no annotation, black line) 
make_panel <- function(df, x_name, y_name, y_lab, bg_fill) {
  d <- df %>%
    dplyr::transmute(
      x = as.numeric(.data[[x_name]]),
      y = as.numeric(.data[[y_name]])
    ) %>%
    dplyr::filter(is.finite(x), is.finite(y))
  
  if (!all(d$x %% 1 == 0, na.rm = TRUE)) d$x <- round(d$x)
  
  d$xf <- factor(d$x, levels = sort(unique(d$x)))
  d$x_num <- as.numeric(d$xf)
  
  ggplot(d, aes(x = xf, y = y)) +
    geom_point(size = 0.8, alpha = 1, color = "black") +  # <- no jitter, straight vertical strip
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") +  # horizontal dotted line at 0
    stat_smooth(aes(x = x_num, y = y), method = "lm", se = FALSE,
                color = "black", linewidth = 0.6) +
    scale_y_continuous(labels = scales::label_number(accuracy = 1)) +
    labs(x = "Semiquantitative score", y = y_lab) +
    theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      panel.background = element_rect(fill = NA, color = NA),
      plot.background  = element_rect(fill = NA, color = NA),
      axis.line        = element_line(color = "black", linewidth = 0.4),
      axis.ticks       = element_line(color = "black", linewidth = 0.4),
      axis.text        = element_text(color = "black"),
      axis.title       = element_text(color = "black", margin = margin(t = 6)),
      plot.margin      = margin(5.5, 5.5, 5.5, 5.5)
    )
}
# -------- Compute Spearman stats for all 3 
get_spearman <- function(df, x_name, y_name, label) {
  d <- df %>%
    dplyr::transmute(
      x = as.numeric(.data[[x_name]]),
      y = as.numeric(.data[[y_name]])
    ) %>%
    dplyr::filter(is.finite(x), is.finite(y))
  test <- tryCatch({
    suppressWarnings(cor.test(d$x, d$y, method = "spearman", exact = FALSE))
  }, error = function(e) NULL)
  if (is.null(test)) return(data.frame(Measure = label, rho = NA, p.value = NA, n = nrow(d)))
  data.frame(Measure = label, rho = unname(test$estimate),
             p.value = test$p.value, n = nrow(d))
}

stats_list <- list(
  get_spearman(df, x_asyn, y_asyn, "ASYN (LB/mm2)"),
  get_spearman(df, x_ab,   y_ab,   "ABETA (%Aβ)"),
  get_spearman(df, x_ptau, y_ptau, "AT8 (%pTau)")
)
stats_df <- dplyr::bind_rows(stats_list)

# Save stats to Excel
writexl::write_xlsx(stats_df, "QuantvSemiquant_SpearmanStats.xlsx")

# -------- Build panels 
p1 <- make_panel(df, x_asyn, y_asyn, expression(LB/mm^2), bg_lb)
p2 <- make_panel(df, x_ab,   y_ab,   "% A\u03B2",               bg_ab)
p3 <- make_panel(df, x_ptau, y_ptau, "% pTau",                  bg_ptau)
p1
p2
p3
# -------- Save each panel
ggsave("Panel_LB_vs_Semiquant.png",    p1, width = 4.6, height = 3.5, dpi = 600, bg = "transparent")
ggsave("Panel_Abeta_vs_Semiquant.png", p2, width = 4.6, height = 3.5, dpi = 600, bg = "transparent")
ggsave("Panel_pTau_vs_Semiquant.png",  p3, width = 4.6, height = 3.5, dpi = 600, bg = "transparent")

# Optional PDFs
ggsave("Panel_LB_vs_Semiquant.pdf",    p1, width = 4.6, height = 3.5, useDingbats = FALSE)
ggsave("Panel_Abeta_vs_Semiquant.pdf", p2, width = 4.6, height = 3.5, useDingbats = FALSE)
ggsave("Panel_pTau_vs_Semiquant.pdf",  p3, width = 4.6, height = 3.5, useDingbats = FALSE)


#Fig2 regional path plots######
library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rstatix)

setwd(#load data)
datadir <- #Location
dir.create(datadir, showWarnings = FALSE)
setwd(datadir)

process_and_plot <- function(data, prefix, y_label, plot_title, output_filename, ylim1, ylim2) {
  
  # Ensure AD_LEVEL_MASTER is an ordered factor for legend order
  data <- data %>%
    filter(!is.na(AD_LEVEL_MASTER)) %>%
    mutate(AD_LEVEL_MASTER = factor(AD_LEVEL_MASTER,
                                    levels = c("None", "Low", "Intermediate", "High")))
  
  # Identify columns that start with prefix
  cols_to_use <- grep(paste0("^", prefix), colnames(data), value = TRUE)
  if (length(cols_to_use) == 0) stop("No columns match the prefix: ", prefix)
  
  # Summarize mean and sample size per marker (no SD now)
  summary <- data %>%
    select(all_of(cols_to_use), AD_LEVEL_MASTER) %>%
    pivot_longer(
      cols = all_of(cols_to_use),
      names_to = "Type",
      values_to = "Value"
    ) %>%
    group_by(Type, AD_LEVEL_MASTER) %>%
    summarize(
      n = sum(!is.na(Value)), 
      Mean_Value = mean(Value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Short_Type = str_remove(Type, paste0("^", prefix)),
      Short_Type = gsub(" ", "\n", Short_Type)  # Line break for x-axis labels
    )
  
  # Maintain original x-axis order
  column_order <- gsub(paste0("^", prefix), "", cols_to_use)
  column_order <- gsub(" ", "\n", column_order)
  
  # Custom color palette
  custom_colors <- c(
    "None" = "#CC66FF",
    "Low" = "#33CCCC",
    "Intermediate" = "#669900",
    "High" = "#FF6666"
  )
  
  # Plot without error bars
  plot <- ggplot(summary, aes(x = Short_Type, y = Mean_Value, 
                              color = AD_LEVEL_MASTER, group = AD_LEVEL_MASTER)) +
    geom_line(size = 0.8) +
    geom_point(size = 2) +
    scale_x_discrete(limits = column_order) +
    scale_y_continuous(limits = c(ylim1, ylim2), breaks = seq(ylim1, ylim2, length.out = 6)) +
    scale_color_manual(values = custom_colors) +
    labs(
      title = plot_title,
      x = NULL,
      y = y_label,
      color = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 23),
      axis.text.y = element_text(size = 30),
      axis.title.y = element_text(size = 30),
      plot.title = element_text(size = 30, hjust = 0.5),
      legend.position = "bottom",
      legend.text = element_text(size = 30),
      panel.grid.major = element_line(color = "gray80"),
      panel.grid.minor = element_blank()
    ) +
    guides(color = guide_legend(nrow = 1))
  
  print(plot)
  
  # Save plot
  ggsave(filename = paste0(output_filename, ".png"), plot = plot, width = 10, height = 6, dpi = 300)
  
  # Export summary table
  table <- summary %>%
    arrange(factor(Short_Type, levels = column_order), AD_LEVEL_MASTER) %>%
    rename(
      Marker = Short_Type,
      Group = AD_LEVEL_MASTER,
      `Sample Size (n)` = n,
      Mean = Mean_Value
    )
  write.csv(table, file = paste0(output_filename, "_table.csv"), row.names = FALSE)
  
  # Prepare for KW and Dunn tests
  long_data <- data %>%
    select(all_of(cols_to_use), AD_LEVEL_MASTER) %>%
    pivot_longer(cols = all_of(cols_to_use), names_to = "Type", values_to = "Value") %>%
    group_by(Type)
  
  # Kruskal-Wallis test
  kw_results <- long_data %>%
    kruskal_test(Value ~ AD_LEVEL_MASTER)
  write.csv(kw_results, file = paste0(output_filename, "_KWtest.csv"), row.names = FALSE)
  
  # Dunn pairwise test with BH adjustment
  pairwise_results <- long_data %>%
    dunn_test(Value ~ AD_LEVEL_MASTER, p.adjust.method = "BH")
  write.csv(pairwise_results, file = paste0(output_filename, "_DunnTest.csv"), row.names = FALSE)
}

# Load BLOCK_REGION data and process
block_data <- read_excel (Load data, 
                         sheet = #Sheet)
block_df <- as.data.frame(block_data)
colnames(block_df)
process_and_plot(block_df, "ABETA_", "% Aβ", "", "ABETA_BLOCK_PLOT", 0,10)



setwd(#Add excel)
datadir <- #add
dir.create(datadir, showWarnings = FALSE)
setwd(datadir)

process_and_plot <- function(data, prefix, y_label, plot_title, output_filename, ylim1, ylim2) {
  
  # Summarize the data to calculate the mean, SD, and sample size for each column by AD_LEVEL_MASTER
  summary <- data %>%
    filter(!is.na(AD_LEVEL_MASTER)) %>%  # Exclude NA
    select(starts_with(prefix), AD_LEVEL_MASTER) %>%
    pivot_longer(
      cols = starts_with(prefix),
      names_to = "Type",
      values_to = "Value"
    ) %>%
    group_by(Type, AD_LEVEL_MASTER) %>%
    summarize(
      n = sum(!is.na(Value)),             # Count non-NA values
      Mean_Value = mean(Value, na.rm = TRUE),
      SD_Value = sd(Value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Short_Type = str_remove(Type, paste0("^", prefix)))  # Clean x-axis labels
  
  # Retain the original order of columns (without prefix)
  column_order <- data %>%
    select(starts_with(prefix)) %>%
    colnames() %>%
    str_remove(paste0("^", prefix))
  
  # Ensure AD_LEVEL_MASTER has a fixed order
  data <- data %>%
    filter(!is.na(AD_LEVEL_MASTER)) %>%
    mutate(AD_LEVEL_MASTER = factor(AD_LEVEL_MASTER,
                                    levels = c("None", "Low", "Intermediate", "High")))
  
  summary <- summary %>%
    mutate(AD_LEVEL_MASTER = factor(AD_LEVEL_MASTER,
                                    levels = c("None", "Low", "Intermediate", "High")))
  
  # Custom color palette
  custom_colors <- c(
    "None" = "#CC66FF",
    "Low" = "#33CCCC",
    "Intermediate" = "#669900",
    "High" = "#FF6666"
  )
  
  
  # Plot with your original colors
  plot <- ggplot(summary, aes(x = Short_Type, y = Mean_Value,
                              color = AD_LEVEL_MASTER, group = AD_LEVEL_MASTER)) +
    geom_line(size = 0.8) +
    geom_point(size = 2) +
    scale_x_discrete(limits = column_order) +
    scale_y_continuous(limits = c(ylim1, ylim2)) +
    scale_color_manual(values = custom_colors) +
    labs(
      title = plot_title,
      x = NULL,
      y = y_label,
      color = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 23),
      axis.text.y = element_text(size = 30),
      axis.title.y = element_text(size = 30),
      plot.title = element_text(size = 30, hjust = 0.5),
      legend.position = "bottom",
      legend.text = element_text(size = 30),
      panel.grid.major = element_line(color = "gray80"),
      panel.grid.minor = element_blank()
    ) +
    guides(color = guide_legend(nrow = 1))  # Single-row legend
  
  print(plot)
  
  # Save the plot
  ggsave(filename = paste0(output_filename, ".png"), plot = plot, width = 10, height = 6, dpi = 300)
  
  # Create and save summary table
  table <- summary %>%
    arrange(factor(Short_Type, levels = column_order), AD_LEVEL_MASTER) %>%
    rename(
      "Marker" = Short_Type,
      "Group" = AD_LEVEL_MASTER,
      "Sample Size (n)" = n,
      "Mean" = Mean_Value,
      "Standard Deviation" = SD_Value
    )
  write.csv(table, file = paste0(output_filename, "_table.csv"), row.names = FALSE)
  
  # Prepare long data for stats
  long_data <- data %>%
    select(starts_with(prefix), AD_LEVEL_MASTER) %>%
    pivot_longer(cols = starts_with(prefix), names_to = "Type", values_to = "Value") %>%
    group_by(Type)
  
  # Kruskal-Wallis test
  kw_results <- long_data %>%
    kruskal_test(Value ~ AD_LEVEL_MASTER)
  write.csv(kw_results, file = paste0(output_filename, "_KWtest.csv"), row.names = FALSE)
  
  # Dunn pairwise comparisons with BH adjustment
  pairwise_results <- long_data %>%
    dunn_test(Value ~ AD_LEVEL_MASTER, p.adjust.method = "BH")
  write.csv(pairwise_results, file = paste0(output_filename, "_DunnTest.csv"), row.names = FALSE)
}





process_and_plot(block_df, "ASYN_", "LB/mm²", "", "ASYN_BLOCK_PLOT", 0,110)
process_and_plot(block_df, "AT8_", "% pTau", "", "AT8_BLOCK_PLOT", 0,25)

# Compare Quant vs. SemiQuant#############
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(pROC)
library(ggtext) 
library(fmsb)
library(stringr)

# LOAD DIRECTORIES ASYN##############
excel_path <- #"/LOAD.xlsx"
  setwd(#/FILE LOCATION")
################Braak_STAGING##################################
# Define LB category
excel_Braak <- excel %>%
  mutate(LB_Category = case_when(
    `Braak ASyn` %in% c(0, 2, 3, 4) ~ "LB_LOW",
    `Braak ASyn` %in% c(5, 6) ~ "LB_HIGH",
    TRUE ~ NA_character_
  ))


excel_Braak_filter <- excel_Braak %>% filter(!is.na(LB_Category))  # Remove NA values

# Summarize data for `excel_filtered_UPR50`
summary_data_Braak <- excel_Braak_filter %>%
  group_by(LB_Category, ApoE, DEMENTIA) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Group = paste(ApoE, DEMENTIA, sep = "_"),
         ApoE_Group = paste(ApoE, LB_Category, sep = "_"))

# Summarize data for dementia proportions
summary_prop_data_Braak <- excel_Braak_filter %>%
  group_by(ApoE, LB_Category, DEMENTIA) %>%
  summarise(Count = n(), .groups = "drop") %>%
  spread(key = DEMENTIA, value = Count, fill = 0) %>%
  mutate(Proportion_Dementia_1 = DEM_1 / (DEM_0 + DEM_1),
         ApoE_Group = paste(ApoE, LB_Category, sep = "_"))

summary_prop_data_Braak <- summary_prop_data_Braak %>%
  filter(!grepl("ApoE3_NA|ApoE4_NA", ApoE_Group))  # Exclude specific NA groups

# Create proportion table
proportion_table_Braak <- summary_prop_data_Braak %>%
  select(ApoE, LB_Category, Proportion_Dementia_1) %>%
  spread(key = LB_Category, value = Proportion_Dementia_1)

# Remove NA groups for ApoE
summary_prop_data_Braak <- summary_prop_data_Braak %>%
  filter(!grepl("^NA_", ApoE_Group))

#SUMMARY Proportions ###################
write.csv(summary_prop_data_Braak, "summary_prop_data_Braak.csv", row.names = FALSE)

# Add Proportion of No Dementia Cases
summary_prop_data_Braak <- summary_prop_data_Braak %>%
  mutate(Proportion_No_Dementia = 1 - Proportion_Dementia_1)

# Convert data to long format for ggplot
summary_prop_data_Braak_long <- summary_prop_data_Braak %>%
  tidyr::pivot_longer(cols = c(Proportion_Dementia_1, Proportion_No_Dementia),
                      names_to = "Dementia_Status",
                      values_to = "Proportion")

# Rename labels
summary_prop_data_Braak_long$Dementia_Status <- factor(summary_prop_data_Braak_long$Dementia_Status,
                                                       levels = c("Proportion_No_Dementia", "Proportion_Dementia_1"),
                                                       labels = c("No Dementia", "Dementia"))


summary_prop_data_Braak_long <- summary_prop_data_Braak_long %>%
  mutate(
    ApoE_Group = case_when(
      ApoE_Group == "ApoE3_LB_HIGH" ~ "ApoE3: H",
      ApoE_Group == "ApoE3_LB_LOW"  ~ "ApoE3: N/L",
      ApoE_Group == "ApoE4_LB_HIGH" ~ "ApoE4: H",
      ApoE_Group == "ApoE4_LB_LOW"  ~ "ApoE4: N/L",
      TRUE ~ ApoE_Group  # Keep any other values unchanged
    )
  )
summary_prop_data_Braak_long$Dementia_Status <- factor(summary_prop_data_Braak_long$Dementia_Status, levels = c("Dementia", "No Dementia"))
# Create stacked bar plot
summary_prop_data_Braak_plot <- ggplot(summary_prop_data_Braak_long, aes(x = ApoE_Group, y = Proportion, fill = Dementia_Status)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = ifelse(Proportion == 0, "", paste0(round(Proportion * 100, 0)))),
            position = position_stack(vjust = 0.5), 
            size = 9, color =ifelse(summary_prop_data_Braak_long$Dementia_Status== "Dementia", "black", "black"),fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "Proportion (%)", fill = "Dementia Status") +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  ) +
  scale_fill_manual(values = c("Dementia" = "#FF9966", "No Dementia" = "#6699CC")) +  # Custom bar colors
  scale_color_manual(values = c("Dementia" = "black", "No Dementia" = "black"), guide = "none") +  # Fix text colors & remove legend
  coord_flip()  # Flip the axes
summary_prop_data_Braak_plot
# Save the plot
ggsave("summary_prop_data_Braak.png", summary_prop_data_Braak_plot, width = 10, height = 8, dpi = 300)
# Display the plot
print(summary_prop_data_Braak_plot)


# Get unique ApoE Groups (after NA removal)
apoE_groups_Braak <- unique(summary_prop_data_Braak$ApoE_Group)

# Contingency table for ApoE_Group and Proportion_Dementia_1
contingency_data_Braak <- summary_prop_data_Braak %>%
  group_by(ApoE_Group) %>%
  summarise(Proportion_Dementia_1 = mean(Proportion_Dementia_1))

# Print updated contingency table
print(contingency_data_Braak)

# Construct global contingency table
global_contingency_Braak <- table(contingency_data_Braak$ApoE_Group, contingency_data_Braak$Proportion_Dementia_1)

# Perform global test (Fisher's Exact or Chi-Square)
global_test_Braak <- if (any(global_contingency_Braak < 5)) {
  fisher.test(global_contingency_Braak)
} else {
  chisq.test(global_contingency_Braak)
}

# Store global test result
all_results_Braak <- data.frame(
  Group1 = "Global",
  Group2 = "All Groups",
  Group1_N = sum(summary_prop_data_Braak$DEM_0) + sum(summary_prop_data_Braak$DEM_1),
  Group2_N = "N/A",
  Method = ifelse(any(global_contingency_Braak < 5), "Fisher's Exact Test", "Chi-Square Test"),
  P.Value = global_test_Braak$p.value
)

# Pairwise comparisons for ApoE Groups
for (i in seq_along(apoE_groups_Braak)) {
  for (j in seq_along(apoE_groups_Braak)) {
    if (i < j) {  # Avoid duplicate comparisons
      group1_data <- summary_prop_data_Braak %>% filter(ApoE_Group == apoE_groups_Braak[i])
      group2_data <- summary_prop_data_Braak %>% filter(ApoE_Group == apoE_groups_Braak[j])
      
      # Construct contingency table
      contingency_table <- matrix(
        c(sum(group1_data$DEM_1), sum(group1_data$DEM_0),
          sum(group2_data$DEM_1), sum(group2_data$DEM_0)),
        nrow = 2
      )
      
      # Perform appropriate test
      test <- if (any(contingency_table < 5)) {
        fisher.test(contingency_table)
      } else {
        chisq.test(contingency_table)
      }
      
      # Append results
      all_results_Braak <- rbind(
        all_results_Braak,
        data.frame(
          Group1 = apoE_groups_Braak[i],
          Group2 = apoE_groups_Braak[j],
          Group1_N = sum(group1_data$DEM_1 + group1_data$DEM_0),
          Group2_N = sum(group2_data$DEM_1 + group2_data$DEM_0),
          P.Value = test$p.value,
          Method = ifelse(any(contingency_table < 5), "Fisher's Exact Test", "Chi-Square Test")
        )
      )
    }
  }
}

# Apply multiple testing corrections
all_results_Braak <- all_results_Braak %>%
  mutate(
    Bonferroni = p.adjust(P.Value, method = "bonferroni"),
    Holm = p.adjust(P.Value, method = "holm"),
    FDR = p.adjust(P.Value, method = "fdr")
  )

# Print final results
print(all_results_Braak)

# Save results
write.csv(all_results_Braak, "DEMENTIA_Braak_Test_Results.csv", row.names = FALSE)
write.table(all_results_Braak, "DEMENTIA_Braak_ASYN_Test_Results.txt", row.names = FALSE, sep = "\t", quote = FALSE)

# Print final cleaned results
final_results_Braak <- all_results_Braak %>%
  select(Group1, Group2, Group1_N, Group2_N, P.Value, Bonferroni, Holm, FDR)

print(final_results_Braak)
################UPR50_STAGING##################################
# Define LB category
excel_UPR50 <- excel %>%
  mutate(LB_Category = case_when(
    `LB_LEVEL_QUART` %in% c("Very Low", "Low") ~ "LB_LOW",
    `LB_LEVEL_QUART` %in% c("Moderate","Severe") ~ "LB_HIGH",
    TRUE ~ NA_character_
  ))

excel_UPR50_filter <- excel_UPR50 %>% filter(!is.na(LB_Category))  # Remove NA values

# Summarize data for `excel_filtered_UPR50`
summary_data_UPR50 <- excel_UPR50_filter %>%
  group_by(LB_Category, ApoE, DEMENTIA) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Group = paste(ApoE, DEMENTIA, sep = "_"),
         ApoE_Group = paste(ApoE, LB_Category, sep = "_"))

# Summarize data for dementia proportions
summary_prop_data_UPR50 <- excel_UPR50_filter %>%
  group_by(ApoE, LB_Category, DEMENTIA) %>%
  summarise(Count = n(), .groups = "drop") %>%
  spread(key = DEMENTIA, value = Count, fill = 0) %>%
  mutate(Proportion_Dementia_1 = DEM_1 / (DEM_0 + DEM_1),
         ApoE_Group = paste(ApoE, LB_Category, sep = "_"))

summary_prop_data_UPR50 <- summary_prop_data_UPR50 %>%
  filter(!grepl("ApoE3_NA|ApoE4_NA", ApoE_Group))  # Exclude specific NA groups

# Create proportion table
proportion_table_UPR50 <- summary_prop_data_UPR50 %>%
  select(ApoE, LB_Category, Proportion_Dementia_1) %>%
  spread(key = LB_Category, value = Proportion_Dementia_1)

# Remove NA groups for ApoE
summary_prop_data_UPR50 <- summary_prop_data_UPR50 %>%
  filter(!grepl("^NA_", ApoE_Group))


#SUMMARY Proportions ###################
write.csv(summary_prop_data_UPR50, "summary_prop_data_ASYN_UPR50.csv", row.names = FALSE)

# Add Proportion of No Dementia Cases
summary_prop_data_UPR50 <- summary_prop_data_UPR50 %>%
  mutate(Proportion_No_Dementia = 1 - Proportion_Dementia_1)

# Convert data to long format for ggplot
summary_prop_data_UPR50_long <- summary_prop_data_UPR50 %>%
  tidyr::pivot_longer(cols = c(Proportion_Dementia_1, Proportion_No_Dementia),
                      names_to = "Dementia_Status",
                      values_to = "Proportion")

# Rename labels
summary_prop_data_UPR50_long$Dementia_Status <- factor(summary_prop_data_UPR50_long$Dementia_Status,
                                                       levels = c("Proportion_No_Dementia", "Proportion_Dementia_1"),
                                                       labels = c("No Dementia", "Dementia"))
summary_prop_data_UPR50_long <- summary_prop_data_UPR50_long %>%
  mutate(
    ApoE_Group = case_when(
      ApoE_Group == "ApoE3_LB_HIGH" ~ "ApoE3: H",
      ApoE_Group == "ApoE3_LB_LOW"  ~ "ApoE3: N/L",
      ApoE_Group == "ApoE4_LB_HIGH" ~ "ApoE4: H",
      ApoE_Group == "ApoE4_LB_LOW"  ~ "ApoE4: N/L",
      TRUE ~ ApoE_Group  # Keep any other values unchanged
    )
  )
summary_prop_data_UPR50_long$Dementia_Status <- factor(summary_prop_data_UPR50_long$Dementia_Status, levels = c("Dementia", "No Dementia"))
# Create stacked bar plot
summary_prop_data_UPR50_plot <- ggplot(summary_prop_data_UPR50_long, aes(x = ApoE_Group, y = Proportion, fill = Dementia_Status)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = ifelse(Proportion == 0, "", paste0(round(Proportion * 100, 0)))),
            position = position_stack(vjust = 0.5), 
            size = 9, color =ifelse(summary_prop_data_Braak_long$Dementia_Status== "Dementia", "black", "black"),fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "Proportion (%)", fill = "Dementia Status") +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  ) +
  scale_fill_manual(values = c("Dementia" = "#FF9966", "No Dementia" = "#6699CC")) +  # Custom bar colors
  scale_color_manual(values = c("Dementia" = "black", "No Dementia" = "black"), guide = "none") +  # Fix text colors & remove legend
  coord_flip()  # Flip the axes


# Save the plot
ggsave("summary_prop_data_ASYN_UPR50.png", summary_prop_data_UPR50_plot, width = 8, height = 6, dpi = 300)
# Display the plot
print(summary_prop_data_UPR50_plot)











# Get unique ApoE Groups (after NA removal)
apoE_groups_UPR50 <- unique(summary_prop_data_UPR50$ApoE_Group)

# Contingency table for ApoE_Group and Proportion_Dementia_1
contingency_data_UPR50 <- summary_prop_data_UPR50 %>%
  group_by(ApoE_Group) %>%
  summarise(Proportion_Dementia_1 = mean(Proportion_Dementia_1))

# Print updated contingency table
print(contingency_data_UPR50)
# Get unique ApoE Groups (after NA removal)
apoE_groups_UPR50 <- unique(summary_prop_data_UPR50$ApoE_Group)

# Contingency table for ApoE_Group and Proportion_Dementia_1
contingency_data_UPR50 <- summary_prop_data_UPR50 %>%
  group_by(ApoE_Group) %>%
  summarise(Proportion_Dementia_1 = mean(Proportion_Dementia_1))

# Print updated contingency table
print(contingency_data_UPR50)

# Construct global contingency table
global_contingency_UPR50 <- table(contingency_data_UPR50$ApoE_Group, contingency_data_UPR50$Proportion_Dementia_1)

# Perform global test (Fisher's Exact or Chi-Square)
global_test_UPR50 <- if (any(global_contingency_UPR50 < 5)) {
  fisher.test(global_contingency_UPR50)
} else {
  chisq.test(global_contingency_UPR50)
}

# Store global test result
all_results_UPR50 <- data.frame(
  Group1 = "Global",
  Group2 = "All Groups",
  Group1_N = sum(summary_prop_data_UPR50$DEM_0) + sum(summary_prop_data_UPR50$DEM_1),
  Group2_N = "N/A",
  Method = ifelse(any(global_contingency_UPR50 < 5), "Fisher's Exact Test", "Chi-Square Test"),
  P.Value = global_test_UPR50$p.value
)

# Pairwise comparisons for ApoE Groups
for (i in seq_along(apoE_groups_UPR50)) {
  for (j in seq_along(apoE_groups_UPR50)) {
    if (i < j) {  # Avoid duplicate comparisons
      group1_data <- summary_prop_data_UPR50 %>% filter(ApoE_Group == apoE_groups_UPR50[i])
      group2_data <- summary_prop_data_UPR50 %>% filter(ApoE_Group == apoE_groups_UPR50[j])
      
      # Construct contingency table
      contingency_table <- matrix(
        c(sum(group1_data$DEM_1), sum(group1_data$DEM_0),
          sum(group2_data$DEM_1), sum(group2_data$DEM_0)),
        nrow = 2
      )
      
      # Perform appropriate test
      test <- if (any(contingency_table < 5)) {
        fisher.test(contingency_table)
      } else {
        chisq.test(contingency_table)
      }
      
      # Append results
      all_results_UPR50 <- rbind(
        all_results_UPR50,
        data.frame(
          Group1 = apoE_groups_UPR50[i],
          Group2 = apoE_groups_UPR50[j],
          Group1_N = sum(group1_data$DEM_1 + group1_data$DEM_0),
          Group2_N = sum(group2_data$DEM_1 + group2_data$DEM_0),
          P.Value = test$p.value,
          Method = ifelse(any(contingency_table < 5), "Fisher's Exact Test", "Chi-Square Test")
        )
      )
    }
  }
}

# Apply multiple testing corrections
all_results_UPR50 <- all_results_UPR50 %>%
  mutate(
    Bonferroni = p.adjust(P.Value, method = "bonferroni"),
    Holm = p.adjust(P.Value, method = "holm"),
    FDR = p.adjust(P.Value, method = "fdr")
  )

# Print final results
print(all_results_UPR50)

# Save results
write.csv(all_results_UPR50, "DEMENTIA_UPR50_ASYN_Test_Results.csv", row.names = FALSE)
write.table(all_results_UPR50, "DEMENTIA_UPR50_ASYN_Test_Results.txt", row.names = FALSE, sep = "\t", quote = FALSE)

# Print final cleaned results
final_results_ASYN_UPR50 <- all_results_UPR50 %>%
  select(Group1, Group2, Group1_N, Group2_N, P.Value, Bonferroni, Holm, FDR)

print(final_results_ASYN_UPR50)


################UPR75_STAGING##################################
# Ensure ApoE is a factor and remove NA levels from facets
excel_graph <- excel %>%
  filter(!is.na(ApoE))
# Convert DEMENTIA to a factor for better visualization
excel_graph$DEMENTIA <- as.factor(excel_graph$DEMENTIA)

# Define custom colors for ApoE3 and ApoE4 groups
custom_colors <- c("ApoE3" = "#339999", "ApoE4" = "#FFDD57")  # Light/Dark blue for ApoE3

# Create the plot
ggplot(excel_graph, aes(x = ASYN_FRNT_HIPP, fill = ApoE, color = ApoE)) +
  geom_density(aes(y = ..density.. * 100), alpha = 0.4, size = 1) +  # Convert density to percentage & adjust transparency
  scale_fill_manual(
    values = c("ApoE3" = "#339999", "ApoE4" = "#FFDD57"),
    labels = c(
      bquote(italic("APOE")~ε3),  # Bold "APOE", normal ε3
      bquote(italic("APOE")~ε4)   # Bold "APOE", normal ε4
    )
  ) + scale_color_manual(
    values = c("ApoE3" = "black", "ApoE4" = "black"),
    labels = c(
      bquote(italic("APOE")~ε3),  # Bold "APOE", normal ε3
      bquote(italic("APOE")~ε4)   # Bold "APOE", normal ε4
    )
  ) +
  labs(
    title = "",
    x = "LB/mm²",
    y = "% cases",  # Updated y-axis label
    fill = "ApoE Group",
    color = "ApoE Group"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.85, 0.85),  # Move legend inside top-right corner
    legend.key = element_rect(fill = NA),  # Keep legend keys clean
    legend.title = element_blank(),
    legend.text = element_text(size = 30),
    axis.text.y = element_text(angle = 0, hjust = 0.5, size = 30), # Rotate and adjust size
    axis.text.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.title.x = element_text(size = 30),
    plot.title = element_blank()
  )

ggsave("ASYN_FRNT_HIPP_Distribution_ApoE.png", width = 8, height = 6, dpi = 300)

# KS test comparing ASYN_FRNT_HIPP between ApoE3 and ApoE4
library(writexl)

data_apoE3 <- excel_graph %>% filter(ApoE == "ApoE3") %>% pull(ASYN_FRNT_HIPP)
data_apoE4 <- excel_graph %>% filter(ApoE == "ApoE4") %>% pull(ASYN_FRNT_HIPP)

ks_result <- ks.test(data_apoE3, data_apoE4)

# Convert to data frame for export
ks_df <- data.frame(
  statistic = ks_result$statistic,
  p_value = ks_result$p.value,
  alternative = ks_result$alternative,
  method = ks_result$method
)

# Write to Excel
write_xlsx(ks_df, "KS_Test_ASYN_FRNT_HIPP_ApoE.xlsx")



# Calculate the 75th percentile
# Calculate the 75th percentile cutoff
upper_quartile <- quantile(excel$ASYN_FRNT_HIPP, probs = 0.75, na.rm = TRUE)

# Define LB category based on 75th percentile
excel_UPR75 <- excel %>%
  mutate(LB_Category = case_when(
    `LB_LEVEL_QUART` %in% c("Very Low", "Low","Moderate") ~ "LB_LOW",
    `LB_LEVEL_QUART` %in% c("Severe") ~ "LB_HIGH",
    TRUE ~ NA_character_
  ))

# Remove NA values
excel_UPR75_filter <- excel_UPR75 %>%
  filter(!is.na(LB_Category))

# Summarize data for `excel_filtered_UPR75`
summary_data_UPR75 <- excel_UPR75_filter %>%
  group_by(LB_Category, ApoE, DEMENTIA) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(
    Group = paste(ApoE, DEMENTIA, sep = "_"),
    ApoE_Group = paste(ApoE, LB_Category, sep = "_")
  ) %>%
  distinct()  # Ensure unique groups

# Summarize data for dementia proportions
summary_prop_data_UPR75_ASYN <- excel_UPR75_filter %>%
  group_by(ApoE, LB_Category, DEMENTIA) %>%
  summarise(Count = n(), .groups = "drop") %>%
  spread(key = DEMENTIA, value = Count, fill = 0) %>%
  mutate(
    Proportion_Dementia_1 = DEM_1 / (DEM_0 + DEM_1),
    ApoE_Group = paste(ApoE, LB_Category, sep = "_")
  )

# Remove NA groups for ApoE
summary_prop_data_UPR75_ASYN <- summary_prop_data_UPR75_ASYN %>%
  filter(!grepl("^NA_|ApoE3_NA|ApoE4_NA", ApoE_Group))  # Remove all NA groups

# Create proportion table
proportion_table_UPR75 <- summary_prop_data_UPR75_ASYN %>%
  select(ApoE, LB_Category, Proportion_Dementia_1) %>%
  spread(key = LB_Category, value = Proportion_Dementia_1)


#SUMMARY Proportions ###################
write.csv(summary_prop_data_UPR75_ASYN, "summary_prop_data_ASYN_UPR75.csv", row.names = FALSE)

# Add Proportion of No Dementia Cases
summary_prop_data_UPR75_ASYN <- summary_prop_data_UPR75_ASYN %>%
  mutate(Proportion_No_Dementia = 1 - Proportion_Dementia_1)

# Convert data to long format for ggplot
summary_prop_data_UPR75_ASYN_long <- summary_prop_data_UPR75_ASYN %>%
  tidyr::pivot_longer(cols = c(Proportion_Dementia_1, Proportion_No_Dementia),
                      names_to = "Dementia_Status",
                      values_to = "Proportion")

# Rename labels
summary_prop_data_UPR75_ASYN_long$Dementia_Status <- factor(summary_prop_data_UPR75_ASYN_long$Dementia_Status,
                                                            levels = c("Proportion_No_Dementia", "Proportion_Dementia_1"),
                                                            labels = c("No Dementia", "Dementia"))

summary_prop_data_UPR75_ASYN_long <- summary_prop_data_UPR75_ASYN_long %>%
  mutate(
    ApoE_Group = case_when(
      ApoE_Group == "ApoE3_LB_HIGH" ~ "ApoE3: H",
      ApoE_Group == "ApoE3_LB_LOW"  ~ "ApoE3: N/L",
      ApoE_Group == "ApoE4_LB_HIGH" ~ "ApoE4: H",
      ApoE_Group == "ApoE4_LB_LOW"  ~ "ApoE4: N/L",
      TRUE ~ ApoE_Group  # Keep any other values unchanged
    )
  )
summary_prop_data_UPR75_ASYN_long$Dementia_Status <- factor(summary_prop_data_UPR75_ASYN_long$Dementia_Status, levels = c("Dementia", "No Dementia"))
# Create stacked bar plot
summary_prop_data_UPR75_ASYN_plot <- ggplot(summary_prop_data_UPR75_ASYN_long, aes(x = ApoE_Group, y = Proportion, fill = Dementia_Status)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(Proportion * 100, 0))), 
            position = position_stack(vjust = 0.5), 
            size = 9, color =ifelse(summary_prop_data_Braak_long$Dementia_Status== "Dementia", "black", "black"),fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "Proportion (%)", fill = "Dementia Status") +
  theme_minimal() +
  theme(
    text = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  )  +
  scale_fill_manual(values = c("Dementia" = "#FF9966", "No Dementia" = "#6699CC")) +  # Custom bar colors
  scale_color_manual(values = c("Dementia" = "black", "No Dementia" = "black"), guide = "none") +  # Fix text colors & remove legend
  coord_flip()  # Flip the axes

# Save the plot
ggsave("summary_prop_data_ASYN_UPR75.png", summary_prop_data_UPR75_ASYN_plot, width = 10, height = 8, dpi = 300)
# Display the plot
print(summary_prop_data_UPR75_ASYN_plot)













# Get unique ApoE Groups (after NA removal)
apoE_groups_UPR75 <- unique(summary_prop_data_UPR75_ASYN$ApoE_Group)

# Contingency table for ApoE_Group and Proportion_Dementia_1
contingency_data_UPR75 <- summary_prop_data_UPR75_ASYN %>%
  group_by(ApoE_Group) %>%
  summarise(Proportion_Dementia_1 = mean(Proportion_Dementia_1))

# Print updated contingency table
print(contingency_data_UPR75)

# Construct global contingency table
global_contingency_UPR75 <- table(contingency_data_UPR75$ApoE_Group, contingency_data_UPR75$Proportion_Dementia_1)

# Perform global test (Fisher's Exact or Chi-Square)
global_test_UPR75 <- if (any(global_contingency_UPR75 < 5)) {
  fisher.test(global_contingency_UPR75)
} else {
  chisq.test(global_contingency_UPR75)
}

# Store global test result
all_results_UPR75 <- data.frame(
  Group1 = "Global",
  Group2 = "All Groups",
  Group1_N = sum(summary_prop_data_UPR75_ASYN$DEM_0) + sum(summary_prop_data_UPR75_ASYN$DEM_1),
  Group2_N = "N/A",
  Method = ifelse(any(global_contingency_UPR75 < 5), "Fisher's Exact Test", "Chi-Square Test"),
  P.Value = global_test_UPR75$p.value
)

# Pairwise comparisons for ApoE Groups
for (i in seq_along(apoE_groups_UPR75)) {
  for (j in seq_along(apoE_groups_UPR75)) {
    if (i < j) {  # Avoid duplicate comparisons
      group1_data <- summary_prop_data_UPR75_ASYN %>% filter(ApoE_Group == apoE_groups_UPR75[i])
      group2_data <- summary_prop_data_UPR75_ASYN %>% filter(ApoE_Group == apoE_groups_UPR75[j])
      
      # Construct contingency table
      contingency_table <- matrix(
        c(sum(group1_data$DEM_1), sum(group1_data$DEM_0),
          sum(group2_data$DEM_1), sum(group2_data$DEM_0)),
        nrow = 2
      )
      
      # Perform appropriate test
      test <- if (any(contingency_table < 5)) {
        fisher.test(contingency_table)
      } else {
        chisq.test(contingency_table)
      }
      
      # Append results
      all_results_UPR75 <- rbind(
        all_results_UPR75,
        data.frame(
          Group1 = apoE_groups_UPR75[i],
          Group2 = apoE_groups_UPR75[j],
          Group1_N = sum(group1_data$DEM_1 + group1_data$DEM_0),
          Group2_N = sum(group2_data$DEM_1 + group2_data$DEM_0),
          P.Value = test$p.value,
          Method = ifelse(any(contingency_table < 5), "Fisher's Exact Test", "Chi-Square Test")
        )
      )
    }
  }
}

# Apply multiple testing corrections
all_results_UPR75 <- all_results_UPR75 %>%
  mutate(
    Bonferroni = p.adjust(P.Value, method = "bonferroni"),
    Holm = p.adjust(P.Value, method = "holm"),
    FDR = p.adjust(P.Value, method = "fdr")
  )

# Print final results
print(all_results_UPR75)

# Save results
write.csv(all_results_UPR75, "DEMENTIA_UPR75_ASYN_Test_Results.csv", row.names = FALSE)
write.table(all_results_UPR75, "DEMENTIA_UPR75_ASYN_Test_Results.txt", row.names = FALSE, sep = "\t", quote = FALSE)

# Print final cleaned results
final_results_ASYN_UPR75 <- all_results_UPR75 %>%
  select(Group1, Group2, Group1_N, Group2_N, P.Value, Bonferroni, Holm, FDR)

print(final_results_ASYN_UPR75)
######################## Combine_Cutoff ####################################

excel$Braak_Level <- excel_Braak$LB_Category
excel$ASYN_UPR50 <- excel_UPR50$LB_Category
excel$ASYN_UPR75<- excel_UPR75$LB_Category

excel_plot <- excel %>%
  filter(!is.na(Braak_Level))
excel_plot <- excel_plot %>%
  filter(!is.na(ASYN_UPR50))
excel_plot <- excel_plot %>%
  filter(!is.na(ASYN_UPR75))


######################## LINE PLOT ####################################
# LoLB necessary libraries
library(ggplot2)
library(dplyr)

# Convert grouping columns to factors
excel <- excel %>%
  mutate(
    Braak_Level = as.factor(Braak_Level),
    ASYN_UPR50 = as.factor(ASYN_UPR50),
    ASYN_UPR75 = as.factor(ASYN_UPR75)
  )

# Reshape data for long format (to plot multiple groups) and remove NA values
excel_long <- excel %>%
  pivot_longer(cols = c("Braak_Level", "ASYN_UPR50", "ASYN_UPR75"), 
               names_to = "LB_Group", values_to = "LB_Value") %>%
  drop_na(LB_Value)  # Remove rows where LB_Value is NA

# Compute mean and standard error for ASYN_FRNT_HIPP within each group
summary_data <- excel_long %>%
  group_by(LB_Group, LB_Value) %>%
  summarise(
    Mean_ASYN = mean(ASYN_FRNT_HIPP, na.rm = TRUE),
    SE_ASYN = sd(ASYN_FRNT_HIPP, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Ensure LB_Value is a factor with correct ordering
summary_data$LB_Value <- factor(summary_data$LB_Value, levels = c("LB_LOW", "LB_HIGH"))
# Ensure correct legend order by setting factor levels
summary_data <- summary_data %>%
  mutate(LB_Group = factor(LB_Group, 
                           levels = c("Braak_Level", "ASYN_UPR50", "ASYN_UPR75")))
# Plot the updated data
plot <- ggplot(summary_data, aes(x = LB_Value, y = Mean_ASYN, group = LB_Group, color = LB_Group)) +
  geom_line(size = 1) +  # Line for mean ASYN_FRNT_HIPP
  geom_point(size = 3) +  # Points for mean values
  geom_errorbar(aes(ymin = Mean_ASYN - SE_ASYN, ymax = Mean_ASYN + SE_ASYN), width = 0.05) +  # Error bars
  scale_color_manual(values = c("Braak_Level" = "red",
                                "ASYN_UPR50" = "blue",
                                "ASYN_UPR75" = "green3"),
                     labels = c("Braak_Level" = "Cortical LB present",
                                "ASYN_UPR50" = "LB in 50% quartile",
                                "ASYN_UPR75" = "LB in 75% quartile")) +
  scale_x_discrete(labels = c("LB_LOW" = "Absent\nor low",
                              "LB_HIGH" = "Present\nor high")) +
  labs(
    x = "",
    y = expression("LB/mm"^2),
    color = "LB Group") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 30, color = "black", hjust = 0.5, vjust = 0.5, lineheight = 1),  # Larger and bolder x-axis text
    axis.text.y = element_text(size = 30, color = "black"),  # Larger and bolder y-axis text
    axis.title.x = element_text(size = 30, color = "black"),  # Larger x-axis title
    axis.title.y = element_text(size = 30, color = "black"),  # Larger y-axis title
    axis.line = element_line(size = 1.5),  # Make axes bolder
    axis.ticks = element_line(size = 1),  # Make axis ticks bolder
    legend.text = element_text(size = 16),  # Larger and bolder legend text
    legend.title = element_blank(),  # Larger and bolder legend title
    plot.title = element_blank(),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_blank()  # Remove border around the plot
  )
# Display the plot
print(plot)

# Save the plot as a PNG
ggsave("ASYN_FRNT_HIPP_Line_Graph_UPR50_75.png", plot, width = 7, height = 6)


library(dunn.test)

# Kruskal-Wallis test across LB_Group + LB_Value
kw <- kruskal.test(ASYN_FRNT_HIPP ~ interaction(LB_Group, LB_Value), data = excel_long)

# Pairwise Dunn's test
excel_long$Group_Label <- paste(excel_long$LB_Group, excel_long$LB_Value)
dunn_res <- dunn.test(excel_long$ASYN_FRNT_HIPP, excel_long$Group_Label, method = "bonferroni")

# View results
print(kw)
print(dunn_res)

# Save results to CSV
dunn_table <- data.frame(
  Comparison = dunn_res$comparisons,
  Z = dunn_res$Z,
  P_Adjusted = dunn_res$P.adjusted
)

# Rename group and value labels in comparison strings
dunn_table$Comparison <- dunn_table$Comparison %>%
  str_replace_all("Braak_Level", "Cortical LB present") %>%
  str_replace_all("ASYN_UPR50", "LB in 50% quartile") %>%
  str_replace_all("ASYN_UPR75", "LB in 75% quartile") %>%
  str_replace_all("LB_LOW", "Absent/low") %>%
  str_replace_all("LB_HIGH", "Present/high")

write.csv(dunn_table, "ASYN_FRNT_HIPP_Line_Graph_DunnTest_Results.csv", row.names = FALSE)



######################## RATIO SUMMARY TABLE ####################################

# Load necessary libraries
library(dplyr)

# Summarize data for each combination of DEMENTIA and LB_Category
summary_table <- excel %>%
  pivot_longer(cols = all_of(c("Braak_Level", "ASYN_UPR50", "ASYN_UPR75")), names_to = "LB_Group", values_to = "LB_Category") %>%
  filter(!is.na(LB_Category)) %>%  # Remove NA values in LB_Category
  group_by(LB_Group, DEMENTIA, LB_Category) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = c(DEMENTIA, LB_Category),
    values_from = Count,  # Use the "Count" column for values
    names_glue = "{LB_Category}_{DEMENTIA}"
  ) %>%
  mutate(
    LB_LOW_RATIO_DEM1_DEM0 = LB_LOW_DEM_1 / LB_LOW_DEM_0,
    LB_HIGH_RATIO_DEM1_DEM0 = LB_HIGH_DEM_1 / LB_HIGH_DEM_0
  )

# Print the updated summary table
print(summary_table)
summary_table <- summary_table %>%
  mutate(LB_Group = factor(LB_Group, levels = c("Braak_Level", "ASYN_UPR50", "ASYN_UPR75"))) %>%
  arrange(LB_Group)

print(summary_table)
# Save as CSV
write.csv(summary_table, "DEMENTIA_ASYN_Ratio_Table.csv", row.names = FALSE)
write.table(summary_table, "DEMENTIA_ASYN_Ratio_Table.txt", row.names = FALSE, sep = "\t", quote = FALSE)


############################PATHOLOGY_TABLE##########################


# Use the existing dataset
summary_table <- excel %>%
  pivot_longer(cols = all_of(c("Braak_Level", "ASYN_UPR50", "ASYN_UPR75")), names_to = "LB_Group", values_to = "LB_Category") %>%
  filter(!is.na(LB_Category)) %>%  # Remove NA values in LB_Category
  group_by(LB_Group, DEMENTIA, LB_Category) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = c(DEMENTIA, LB_Category),
    values_from = Count,  # Use the "Count" column for values
    names_glue = "{LB_Category}_{DEMENTIA}"
  ) %>%
  mutate(
    Sensitivity = LB_HIGH_DEM_1 / (LB_HIGH_DEM_1 + LB_LOW_DEM_1),
    Specificity = LB_LOW_DEM_0 / (LB_LOW_DEM_0 + LB_HIGH_DEM_0),
    Odds_Ratio = (LB_HIGH_DEM_1 / LB_HIGH_DEM_0) / (LB_LOW_DEM_1 / LB_LOW_DEM_0),
    PPV = LB_HIGH_DEM_1 / (LB_HIGH_DEM_1 + LB_HIGH_DEM_0),
    Pathology_Absence_Ratio = (LB_LOW_DEM_1 / LB_LOW_DEM_0),
    Pathology_Presence_Ratio = (LB_HIGH_DEM_1 / LB_HIGH_DEM_0)
  )
summary_table <- summary_table %>%
  mutate(LB_Group = factor(LB_Group, levels = c("Braak_Level", "ASYN_UPR50", "ASYN_UPR75"))) %>%
  arrange(LB_Group)

print(summary_table)
# Save to CSV
write.csv(summary_table, "Pathology_Statistics_ASYN.csv", row.names = FALSE)
write.table(summary_table, "Pathology_Statistics_ASYN.txt", row.names = FALSE, sep = "\t", quote = FALSE)
# Print the final table
print(summary_table)

#################RADAR_PLOT#######################

library(fmsb)

# Normalize values
summary_table <- summary_table %>% mutate(
  Odds_Ratio = Odds_Ratio / max(Odds_Ratio, na.rm = TRUE),
  Pathology_Absence_Ratio = Pathology_Absence_Ratio / max(Pathology_Absence_Ratio, na.rm = TRUE),
  Pathology_Presence_Ratio = Pathology_Presence_Ratio / max(Pathology_Presence_Ratio, na.rm = TRUE)
)


# Save to CSV
write.csv(summary_table, "Pathology_Statistics_Norm.csv", row.names = FALSE)
write.table(summary_table, "Pathology_Statistics_Norm.txt", row.names = FALSE, sep = "\t", quote = FALSE)
# Print the final table
print(summary_table)
# Define max and min values
max_vals <- rep(1, ncol(summary_table) - 1)
min_vals <- rep(0, ncol(summary_table) - 1)

# Combine min/max rows with the actual data
data <- rbind(max_vals, min_vals, summary_table[,-1])

data <- as.data.frame(data)

# Reorder columns for proper radar chart visualization
data_rotated <- data[, c("Specificity", "Pathology_Absence_Ratio", "Pathology_Presence_Ratio",
                         "Odds_Ratio", "PPV", "Sensitivity")]

# Plot radar chart
# adjust margins to allow space for the legend
# Increase right margin to make space for legend
colnames(data_rotated) <- c("Specificity", 
                            "PAR",
                            "PPR",
                            "OR", 
                            "PPV", 
                            "Sensitivity")
par(mar = c(1, 1, 1, 10) + 0.1)  
# Set font to bold before plotting
par(font = 1)  # 2 = Bold
# Plot radar chart
radarchart(data_rotated,
           seg = 5,                     
           axistype = 2,                
           pcol = c("red", "blue", "green"),  # Border colors
           pfcol = c(rgb(1, 0, 0, 0.5),  # Ensure fill matches border
                     rgb(0, 0, 1, 0.5),  # Blue (not green!)
                     rgb(0, 1, 0, 0.5)), # Green (not blue!)
           plwd = 4,                    
           cglcol = "grey",             
           cglty = 1,                   
           axislabcol = "grey",         
           caxislabels = seq(0, 1, 0.2), 
           vlcex = 3,  # Increase text size of variable labels
           cex.axis = 2,  # Increase size of axis labels
           cex.lab = 2  # (If applicable) Increase size of axis labels
)

# LBding a properly formatted legend
legend("right",
       inset = c(-0.2, 0),    # Moves legend further right
       legend = c("Cortical LB pathology present",
                  "LB pathology in 50% quartile",
                  "LB pathology in 75% quartile"),
       bty = "n", pch = 20,
       col = c("red", "blue", "green"),
       text.col = "black",    cex = 1.2,   xpd = TRUE)  # Ensure text is visible outside

# Save radar chart as PNG
png("radar_Chart_ASYN_UPR75.png", width = 3700, height = 2000, res=300,  bg = "transparent")

# Increase right margin to make space for legend
par(mar = c(1, 1, 1, 10) + 0.1)  
# Set font to bold before plotting
par(font = 1)  # 2 = Bold
# Plot radar chart
radarchart(data_rotated,
           seg = 5,                     
           axistype = 2,                
           pcol = c("red", "blue", "green"),  # Border colors
           pfcol = c(rgb(1, 0, 0, 0.5),  # Ensure fill matches border
                     rgb(0, 0, 1, 0.5),  # Blue 
                     rgb(0, 1, 0, 0.5)), # Green 
           plwd = 4,                    
           cglcol = "grey",             
           cglty = 1,                   
           axislabcol = "grey",         
           caxislabels = seq(0, 1, 0.2), 
           vlcex = 3,  # Increase text size of variable labels
           cex.axis = 2,  # Increase size of axis labels
           cex.lab = 2  # (If applicable) Increase size of axis labels
)

# LBding a properly formatted legend
legend("right",
       inset = c(-0.2, 0),    # Moves legend further right
       legend = c("Cortical LB pathology present",
                  "LB pathology in 50% quartile",
                  "LB pathology in 75% quartile"),
       bty = "n", pch = 20,
       col = c("red", "blue", "green"),
       text.col = "black",    cex = 1.2,   xpd = TRUE)  # Ensure text is visible outside

dev.off()
# Set font to bold before plotting
par(font = 1) 
##########################CHECKERBOARD_PLOT_Braak###################################
setwd(datadir)

final_results_Braak <- final_results_Braak %>%
  filter(Group1 != "Global" & Group2 != "All Groups") %>%
  mutate(
    Group = "GROUP",
    Group1 = case_when(
      Group1 == "ApoE3_LB_HIGH" ~ "ApoE3: H",
      Group1 == "ApoE3_LB_LOW"  ~ "ApoE3: N/L",
      Group1 == "ApoE4_LB_HIGH" ~ "ApoE4: H",
      Group1 == "ApoE4_LB_LOW"  ~ "ApoE4: N/L",
      TRUE ~ Group1  # Keep any other values unchanged
    ),
    Group2 = case_when(
      Group2 == "ApoE3_LB_HIGH" ~ "ApoE3: H",
      Group2 == "ApoE3_LB_LOW"  ~ "ApoE3: N/L",
      Group2 == "ApoE4_LB_HIGH" ~ "ApoE4: H",
      Group2 == "ApoE4_LB_LOW"  ~ "ApoE4: N/L",
      TRUE ~ Group2  # Keep any other values unchanged
    )
  )


combined_all_results <- bind_rows(final_results_Braak,)


# Step 1: LoLB the dataset
df <- combined_all_results %>%
  as.data.frame()
# Step 2: Duplicate all rows and swap `Group1` and `Group2` in the duplicated rows
df_reciprocal <- df %>%
  rename(Group1 = Group2, Group2 = Group1)  # Swap the values

# Step 3: Combine the original and reciprocal data
df_full <- bind_rows(df, df_reciprocal) %>%
  distinct(Group1, Group2, .keep_all = TRUE)  # Ensure no duplicates are removed

# Step 4: Display the final dataset
print(df_full)

df<- df_full

# Order variables within each "Group"
df <- df %>%
  group_by(Group) %>%
  arrange(Group, Group1, Group2, .by_group = TRUE)

# Get unique labels maintaining order within each "Group"
all_labels <- df %>%
  select(Group, Group1, Group2) %>%
  pivot_longer(cols = c(Group1, Group2), values_to = "Label") %>%
  distinct(Group, Label) %>%
  arrange(Group) %>%
  pull(Label)

# Create full grid while keeping group-wise structure
complete_grid <- expand.grid(Group1 = all_labels, Group2 = all_labels, stringsAsFactors = FALSE)

# Merge with actual data while preserving "Group" order
df_full <- complete_grid %>%
  left_join(df, by = c("Group1", "Group2"))

# **Step 2: LBd Reciprocal Values Properly**
df_reciprocal <- df_full %>%
  rename(Group1 = Group2, Group2 = Group1)  # Swap Group1 and Group2

# Ensure reciprocal pairs have the same Bonferroni values
df_reciprocal <- df_reciprocal %>%
  left_join(df_full %>% select(Group1, Group2, Bonferroni), by = c("Group1", "Group2")) %>%
  mutate(Bonferroni = coalesce(Bonferroni.x, Bonferroni.y)) %>%
  select(-Bonferroni.x, -Bonferroni.y)  # Keep only the updated Bonferroni values

df_full <- bind_rows(df_full, df_reciprocal) %>%
  distinct(Group1, Group2, .keep_all = TRUE)  # Ensure duplicates are kept

# Categorize Bonferroni values for coloring
df_full <- df_full %>%
  mutate(
    color = case_when(
      Bonferroni < 0.05 ~ "grey40",  # Use "grey40" explicitly
      Bonferroni >= 0.05 ~ "grey95",
      is.na(Bonferroni) ~ NA_character_
    )
  )

# Ensure levels preserve the correct group-wise order
df_full$Group1 <- factor(df_full$Group1, levels = all_labels)
df_full$Group2 <- factor(df_full$Group2, levels = rev(all_labels))  # Ensure full mirroring

# Extract Group1 and Group2 levels for correct coloring of axis labels
group1_levels <- levels(df_full$Group1)
group2_levels <- levels(df_full$Group2)

# Step 3: Plot checkerboard heatmap with FULL Reciprocal Values
library(ggplot2)
library(ggtext)
library(ggnewscale)

# Define color labels
color_labels <- c("p <0.05" = "grey40",
                  "p 0.05-0.1" = "grey70",
                  "p <0.1" = "grey95")

# Ensure missing values (NA) are properly handled
df_full$Bonferroni_cat <- cut(df_full$Bonferroni, 
                              breaks = c(-Inf, 0.05, 0.1, Inf), 
                              labels = names(color_labels))

# Convert NA values to explicit category for better visualization
df_full$Bonferroni_cat <- as.character(df_full$Bonferroni_cat)
df_full$Bonferroni_cat[is.na(df_full$Bonferroni)] <- "NA"

# LBd "NA" to the color scale so blank cells appear white
color_labels["NA"] <- "white"

# Create color mappings for text labels
group2_colors <- ifelse(grepl("ApoE3", df_full$Group2), "grey40", "black")
group1_colors <- ifelse(grepl("ApoE3", df_full$Group1), "grey40", "black")

# Create the heatmap plot
ggplot(df_full, aes(x = Group2, y = Group1, fill = Bonferroni_cat)) +
  geom_tile(color = "black") +
  geom_richtext(
    aes(label = ifelse(
      !is.na(Bonferroni),
      ifelse(Bonferroni < 0.01, "**<0.01**",  # Replace very low p-values with "<0.001"
             ifelse(Bonferroni < 0.05, paste0("**", sprintf("%.2f", Bonferroni), "**"), 
                    sprintf("%.2f", Bonferroni))),
      "")),  
    size = 8, fill = NA, label.color = NA,  
    color = ifelse(!is.na(df_full$Bonferroni) & df_full$Bonferroni < 0.05, "white", "black")  
  ) +
  scale_fill_manual(
    name = "Bonferroni P-Value",
    values = c(
      "p <0.05" = "grey40",
      "p 0.05-0.1" = "grey70",
      "p <0.1" = "grey95"
    ),
    breaks = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    labels = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    na.value = "white"  # Keeps blank cells white
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_markdown(angle = 90, hjust = 1, size = 14),  
    axis.text.y = element_markdown(angle = 0, hjust = 1, size = 14),
    panel.grid = element_blank()
  ) +
  labs(x = "", y = "", fill = "Bonferroni P-Value") +
  ggtitle("") +  
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +  
  coord_fixed()


Bonferroni_Heatmap_plot <-ggplot(df_full, aes(x = Group2, y = Group1, fill = Bonferroni_cat)) +
  geom_tile(color = "black") +
  geom_richtext(
    aes(label = ifelse(
      !is.na(Bonferroni),
      ifelse(Bonferroni < 0.01, "**<0.01**",  # Replace very low p-values with "<0.001"
             ifelse(Bonferroni < 0.05, paste0("**", sprintf("%.2f", Bonferroni), "**"), 
                    sprintf("%.2f", Bonferroni))),
      "")),  
    size = 8, fill = NA, label.color = NA,  
    color = ifelse(!is.na(df_full$Bonferroni) & df_full$Bonferroni < 0.05, "white", "black")  
  ) +
  scale_fill_manual(
    name = "Bonferroni P-Value",
    values = c(
      "p <0.05" = "grey40",
      "p 0.05-0.1" = "grey70",
      "p <0.1" = "grey95"
    ),
    breaks = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    labels = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    na.value = "white"  # Keeps blank cells white
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_markdown(angle = 90, hjust = 1, size = 14),  
    axis.text.y = element_markdown(angle = 0, hjust = 1, size = 14),
    panel.grid = element_blank()
  ) +
  labs(x = "", y = "", fill = "Bonferroni P-Value") +
  ggtitle("") +  
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +  
  coord_fixed()

# Step 5: Save the plot as a PNG
ggsave("Bonferroni_Heatmap_plot_ASYN_Braak.png", 
       plot = Bonferroni_Heatmap_plot, width = 10, height = 8, dpi = 300)
Bonferroni_Heatmap_plot_ASYN_Braak <- Bonferroni_Heatmap_plot

##########################CHECKERBOARD_PLOT_UPR75###################################
setwd(datadir)

final_results_ASYN_UPR75 <- final_results_ASYN_UPR75 %>%
  filter(Group1 != "Global" & Group2 != "All Groups") %>%
  mutate(
    Group = "GROUP",
    Group1 = case_when(
      Group1 == "ApoE3_LB_HIGH" ~ "ApoE3: H",
      Group1 == "ApoE3_LB_LOW"  ~ "ApoE3: N/L",
      Group1 == "ApoE4_LB_HIGH" ~ "ApoE4: H",
      Group1 == "ApoE4_LB_LOW"  ~ "ApoE4: N/L",
      TRUE ~ Group1  # Keep any other values unchanged
    ),
    Group2 = case_when(
      Group2 == "ApoE3_LB_HIGH" ~ "ApoE3: H",
      Group2 == "ApoE3_LB_LOW"  ~ "ApoE3: N/L",
      Group2 == "ApoE4_LB_HIGH" ~ "ApoE4: H",
      Group2 == "ApoE4_LB_LOW"  ~ "ApoE4: N/L",
      TRUE ~ Group2  # Keep any other values unchanged
    )
  )


combined_all_results <- bind_rows(final_results_ASYN_UPR75,)


# Step 1: LoLB the dataset
df <- combined_all_results %>%
  as.data.frame()
# Step 2: Duplicate all rows and swap `Group1` and `Group2` in the duplicated rows
df_reciprocal <- df %>%
  rename(Group1 = Group2, Group2 = Group1)  # Swap the values

# Step 3: Combine the original and reciprocal data
df_full <- bind_rows(df, df_reciprocal) %>%
  distinct(Group1, Group2, .keep_all = TRUE)  # Ensure no duplicates are removed

# Step 4: Display the final dataset
print(df_full)

df<- df_full

# Order variables within each "Group"
df <- df %>%
  group_by(Group) %>%
  arrange(Group, Group1, Group2, .by_group = TRUE)

# Get unique labels maintaining order within each "Group"
all_labels <- df %>%
  select(Group, Group1, Group2) %>%
  pivot_longer(cols = c(Group1, Group2), values_to = "Label") %>%
  distinct(Group, Label) %>%
  arrange(Group) %>%
  pull(Label)

# Create full grid while keeping group-wise structure
complete_grid <- expand.grid(Group1 = all_labels, Group2 = all_labels, stringsAsFactors = FALSE)

# Merge with actual data while preserving "Group" order
df_full <- complete_grid %>%
  left_join(df, by = c("Group1", "Group2"))

# **Step 2: LBd Reciprocal Values Properly**
df_reciprocal <- df_full %>%
  rename(Group1 = Group2, Group2 = Group1)  # Swap Group1 and Group2

# Ensure reciprocal pairs have the same Bonferroni values
df_reciprocal <- df_reciprocal %>%
  left_join(df_full %>% select(Group1, Group2, Bonferroni), by = c("Group1", "Group2")) %>%
  mutate(Bonferroni = coalesce(Bonferroni.x, Bonferroni.y)) %>%
  select(-Bonferroni.x, -Bonferroni.y)  # Keep only the updated Bonferroni values

df_full <- bind_rows(df_full, df_reciprocal) %>%
  distinct(Group1, Group2, .keep_all = TRUE)  # Ensure duplicates are kept

# Categorize Bonferroni values for coloring
df_full <- df_full %>%
  mutate(
    color = case_when(
      Bonferroni < 0.05 ~ "grey40",  # Use "grey40" explicitly
      Bonferroni >= 0.05 ~ "grey95",
      is.na(Bonferroni) ~ NA_character_
    )
  )

# Ensure levels preserve the correct group-wise order
df_full$Group1 <- factor(df_full$Group1, levels = all_labels)
df_full$Group2 <- factor(df_full$Group2, levels = rev(all_labels))  # Ensure full mirroring

# Extract Group1 and Group2 levels for correct coloring of axis labels
group1_levels <- levels(df_full$Group1)
group2_levels <- levels(df_full$Group2)

# Step 3: Plot checkerboard heatmap with FULL Reciprocal Values
library(ggplot2)
library(ggtext)
library(ggnewscale)

# Define color labels
color_labels <- c("p <0.05" = "grey40",
                  "p 0.05-0.1" = "grey70",
                  "p <0.1" = "grey95")

# Ensure missing values (NA) are properly handled
df_full$Bonferroni_cat <- cut(df_full$Bonferroni, 
                              breaks = c(-Inf, 0.05, 0.1, Inf), 
                              labels = names(color_labels))

# Convert NA values to explicit category for better visualization
df_full$Bonferroni_cat <- as.character(df_full$Bonferroni_cat)
df_full$Bonferroni_cat[is.na(df_full$Bonferroni)] <- "NA"

# LBd "NA" to the color scale so blank cells appear white
color_labels["NA"] <- "white"

# Create color mappings for text labels
group2_colors <- ifelse(grepl("ApoE3", df_full$Group2), "grey40", "black")
group1_colors <- ifelse(grepl("ApoE3", df_full$Group1), "grey40", "black")

# Create the heatmap plot
ggplot(df_full, aes(x = Group2, y = Group1, fill = Bonferroni_cat)) +
  geom_tile(color = "black") +
  geom_richtext(
    aes(label = ifelse(
      !is.na(Bonferroni),
      ifelse(Bonferroni < 0.01, "**<0.01**",  # Replace very low p-values with "<0.001"
             ifelse(Bonferroni < 0.05, paste0("**", sprintf("%.2f", Bonferroni), "**"), 
                    sprintf("%.2f", Bonferroni))),
      "")),  
    size = 8, fill = NA, label.color = NA,  
    color = ifelse(!is.na(df_full$Bonferroni) & df_full$Bonferroni < 0.05, "white", "black")  
  ) +
  scale_fill_manual(
    name = "Bonferroni P-Value",
    values = c(
      "p <0.05" = "grey40",
      "p 0.05-0.1" = "grey70",
      "p <0.1" = "grey95"
    ),
    breaks = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    labels = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    na.value = "white"  # Keeps blank cells white
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_markdown(angle = 90, hjust = 1, size = 14),  
    axis.text.y = element_markdown(angle = 0, hjust = 1, size = 14),
    panel.grid = element_blank()
  ) +
  labs(x = "", y = "", fill = "Bonferroni P-Value") +
  ggtitle("") +  
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +  
  coord_fixed()


Bonferroni_Heatmap_plot <-ggplot(df_full, aes(x = Group2, y = Group1, fill = Bonferroni_cat)) +
  geom_tile(color = "black") +
  geom_richtext(
    aes(label = ifelse(
      !is.na(Bonferroni),
      ifelse(Bonferroni < 0.01, "**<0.01**",  # Replace very low p-values with "<0.001"
             ifelse(Bonferroni < 0.05, paste0("**", sprintf("%.2f", Bonferroni), "**"), 
                    sprintf("%.2f", Bonferroni))),
      "")),  
    size = 8, fill = NA, label.color = NA,  
    color = ifelse(!is.na(df_full$Bonferroni) & df_full$Bonferroni < 0.05, "white", "black")  
  ) +
  scale_fill_manual(
    name = "Bonferroni P-Value",
    values = c(
      "p <0.05" = "grey40",
      "p 0.05-0.1" = "grey70",
      "p <0.1" = "grey95"
    ),
    breaks = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    labels = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    na.value = "white"  # Keeps blank cells white
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_markdown(angle = 90, hjust = 1, size = 14),  
    axis.text.y = element_markdown(angle = 0, hjust = 1, size = 14),
    panel.grid = element_blank()
  ) +
  labs(x = "", y = "", fill = "Bonferroni P-Value") +
  ggtitle("") +  
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +  
  coord_fixed()

# Step 5: Save the plot as a PNG
ggsave("Bonferroni_Heatmap_plot_ASYN_UPR75.png", 
       plot = Bonferroni_Heatmap_plot, width = 10, height = 8, dpi = 300)

Bonferroni_Heatmap_plot_ASYN_UPR75 <- Bonferroni_Heatmap_plot

##########################CHECKERBOARD_PLOT###################################
setwd(datadir)

final_results_Braak <- final_results_Braak %>%
  filter(Group1 != "Global" & Group2 != "All Groups") %>%
  mutate(
    Group = "3_Braak",
    Group1 = paste0(Group1, "_Braak"),
    Group2 = paste0(Group2, "_Braak")
  )
final_results_ASYN_UPR50 <- final_results_ASYN_UPR50 %>%
  filter(Group1 != "Global" & Group2 != "All Groups") %>%
  mutate(
    Group = "2_UPR50",
    Group1 = paste0(Group1, "_ASYN_50thPercentile"),
    Group2 = paste0(Group2, "_ASYN_50thPercentile")
  )

final_results_ASYN_UPR75 <- final_results_ASYN_UPR75 %>%
  filter(Group1 != "Global" & Group2 != "All Groups") %>%
  mutate(
    Group = "1_UPR75",
    Group1 = paste0(Group1, "_ASYN_75%Quartile"),
    Group2 = paste0(Group2, "_ASYN_75%Quartile")
  )

combined_all_results <- bind_rows(final_results_Braak, final_results_ASYN_UPR50, final_results_ASYN_UPR75)

# Print the updated combined dataframe
print(combined_all_results)

# Save the final dataframe as a CSV file
write.csv(combined_all_results, "combined_all_results.csv", row.names = FALSE)
# Step 1: LoLB the dataset
df <- combined_all_results %>%
  as.data.frame()
# Step 2: Duplicate all rows and swap `Group1` and `Group2` in the duplicated rows
df_reciprocal <- df %>%
  rename(Group1 = Group2, Group2 = Group1)  # Swap the values

# Step 3: Combine the original and reciprocal data
df_full <- bind_rows(df, df_reciprocal) %>%
  distinct(Group1, Group2, .keep_all = TRUE)  # Ensure no duplicates are removed

# Step 4: Display the final dataset
print(df_full)

df<- df_full

# Order variables within each "Group"
df <- df %>%
  group_by(Group) %>%
  arrange(Group, Group1, Group2, .by_group = TRUE)

# Get unique labels maintaining order within each "Group"
all_labels <- df %>%
  select(Group, Group1, Group2) %>%
  pivot_longer(cols = c(Group1, Group2), values_to = "Label") %>%
  distinct(Group, Label) %>%
  arrange(Group) %>%
  pull(Label)

# Create full grid while keeping group-wise structure
complete_grid <- expand.grid(Group1 = all_labels, Group2 = all_labels, stringsAsFactors = FALSE)

# Merge with actual data while preserving "Group" order
df_full <- complete_grid %>%
  left_join(df, by = c("Group1", "Group2"))

# **Step 2: LBd Reciprocal Values Properly**
df_reciprocal <- df_full %>%
  rename(Group1 = Group2, Group2 = Group1)  # Swap Group1 and Group2

# Ensure reciprocal pairs have the same Bonferroni values
df_reciprocal <- df_reciprocal %>%
  left_join(df_full %>% select(Group1, Group2, Bonferroni), by = c("Group1", "Group2")) %>%
  mutate(Bonferroni = coalesce(Bonferroni.x, Bonferroni.y)) %>%
  select(-Bonferroni.x, -Bonferroni.y)  # Keep only the updated Bonferroni values

df_full <- bind_rows(df_full, df_reciprocal) %>%
  distinct(Group1, Group2, .keep_all = TRUE)  # Ensure duplicates are kept

# Categorize Bonferroni values for coloring
df_full <- df_full %>%
  mutate(
    color = case_when(
      Bonferroni < 0.05 ~ "dodgerblue",  # Use "dodgerblue" explicitly
      Bonferroni >= 0.05 ~ "grey",
      is.na(Bonferroni) ~ NA_character_
    )
  )

# Ensure levels preserve the correct group-wise order
df_full$Group1 <- factor(df_full$Group1, levels = all_labels)
df_full$Group2 <- factor(df_full$Group2, levels = rev(all_labels))  # Ensure full mirroring

# Extract Group1 and Group2 levels for correct coloring of axis labels
group1_levels <- levels(df_full$Group1)
group2_levels <- levels(df_full$Group2)

# Step 3: Plot checkerboard heatmap with FULL Reciprocal Values
library(ggplot2)
library(ggtext)
library(ggnewscale)

# Define color labels
color_labels <- c("p <0.05" = "dodgerblue",
                  "p 0.05-0.1" = "lightskyblue3",
                  "p <0.1" = "grey")

# Ensure missing values (NA) are properly handled
df_full$Bonferroni_cat <- cut(df_full$Bonferroni, 
                              breaks = c(-Inf, 0.05, 0.1, Inf), 
                              labels = names(color_labels))

# Convert NA values to explicit category for better visualization
df_full$Bonferroni_cat <- as.character(df_full$Bonferroni_cat)
df_full$Bonferroni_cat[is.na(df_full$Bonferroni)] <- "NA"

# LBd "NA" to the color scale so blank cells appear white
color_labels["NA"] <- "white"

# Create color mappings for text labels
group2_colors <- ifelse(grepl("ApoE3", df_full$Group2), "dodgerblue", "firebrick")
group1_colors <- ifelse(grepl("ApoE3", df_full$Group1), "dodgerblue", "firebrick")

# Create the heatmap plot
ggplot(df_full, aes(x = Group2, y = Group1, fill = Bonferroni_cat)) +
  geom_tile(color = "black") +
  geom_richtext(
    aes(label = ifelse(
      !is.na(Bonferroni),
      ifelse(Bonferroni < 0.01, "**<0.01**",  # Replace very low p-values with "<0.001"
             ifelse(Bonferroni < 0.05, paste0("**", sprintf("%.2f", Bonferroni), "**"), 
                    sprintf("%.2f", Bonferroni))),
      "")),  
    size = 8, fill = NA, label.color = NA,  
    color = ifelse(!is.na(df_full$Bonferroni) & df_full$Bonferroni < 0.05, "white", "black")  
  ) +
  scale_fill_manual(
    name = "Bonferroni P-Value",
    values = c(
      "p <0.05" = "dodgerblue",
      "p 0.05-0.1" = "lightskyblue3",
      "p <0.1" = "grey"
    ),
    breaks = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    labels = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    na.value = "white"  # Keeps blank cells white
  ) +
  scale_x_discrete(labels = function(x) { 
    ifelse(grepl("ApoE3", x), paste0("<span style='color:dodgerblue;'>", x, "</span>"),
           paste0("<span style='color:firebrick;'>", x, "</span>")) 
  }) +
  scale_y_discrete(labels = function(x) { 
    ifelse(grepl("ApoE3", x), paste0("<span style='color:dodgerblue;'>", x, "</span>"),
           paste0("<span style='color:firebrick;'>", x, "</span>")) 
  }) +
  theme_minimal() +
  theme(
    axis.text.x = element_markdown(angle = 45, hjust = 1, size = 14),  
    axis.text.y = element_markdown(angle = 0, hjust = 1, size = 14),
    panel.grid = element_blank()
  ) +
  labs(x = "Group2", y = "Group1", fill = "Bonferroni P-Value") +
  ggtitle("") +  
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +  
  coord_fixed()


Bonferroni_Heatmap_plot <-ggplot(df_full, aes(x = Group2, y = Group1, fill = Bonferroni_cat)) +
  geom_tile(color = "black") +
  geom_richtext(
    aes(label = ifelse(
      !is.na(Bonferroni),
      ifelse(Bonferroni < 0.01, "**<0.01**",  # Replace very low p-values with "<0.001"
             ifelse(Bonferroni < 0.05, paste0("**", sprintf("%.2f", Bonferroni), "**"), 
                    sprintf("%.2f", Bonferroni))),
      "")),  
    size = 8, fill = NA, label.color = NA,  
    color = ifelse(!is.na(df_full$Bonferroni) & df_full$Bonferroni < 0.05, "white", "black")  
  ) +
  scale_fill_manual(
    name = "Bonferroni P-Value",
    values = c(
      "p <0.05" = "dodgerblue",
      "p 0.05-0.1" = "lightskyblue3",
      "p <0.1" = "grey"
    ),
    breaks = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    labels = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    na.value = "white"  # Keeps blank cells white
  ) +
  scale_x_discrete(labels = function(x) { 
    ifelse(grepl("ApoE3", x), paste0("<span style='color:dodgerblue;'>", x, "</span>"),
           paste0("<span style='color:firebrick;'>", x, "</span>")) 
  }) +
  scale_y_discrete(labels = function(x) { 
    ifelse(grepl("ApoE3", x), paste0("<span style='color:dodgerblue;'>", x, "</span>"),
           paste0("<span style='color:firebrick;'>", x, "</span>")) 
  }) +
  theme_minimal() +
  theme(
    axis.text.x = element_markdown(angle = 90, hjust = 1, size = 14),  
    axis.text.y = element_markdown(angle = 0, hjust = 1, size = 14),
    panel.grid = element_blank()
  ) +
  labs(x = "Group2", y = "Group1", fill = "Bonferroni P-Value") +
  ggtitle("") +  
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +  
  coord_fixed()

# Step 5: Save the plot as a PNG
ggsave("Bonferroni_Heatmap_plot_ASYN_UPR50_75.png", 
       plot = Bonferroni_Heatmap_plot, width = 16, height = 14, dpi = 300)



# LOAD DIRECTORIES ABETA##############
excel_path <- #"/LOAD.xlsx"
  setwd(#/FILE LOCATION")
#UPR75####################################################
#############IMPUTATION_ABETA################
# Load dataset
excel <- read_excel("/Users/WORK_2023/Jaunmuktane_Lab/DIGIPATH/ASAP DATA/PATHWAYS_TO_DEMENTIA_PAPER/PATHWAYS_TO_DEMENTIA_SUMMARY.xlsx", 
                    sheet = "300_R_BLOCK") %>%
  as.data.frame()

excel <- excel %>%
  mutate(
    # Impute ABETA_FRNT_CTX based on available columns
    ABETA_FRNT_CTX = case_when(
      !is.na(ABETA_FRNT_CTX) ~ ABETA_FRNT_CTX,
      !is.na(ABETA_PRT_CTX)  ~ ABETA_PRT_CTX,
      !is.na(ABETA_TEMP_CTX) ~ ABETA_TEMP_CTX,
      !is.na(ABETA_OCCI_CTX) ~ ABETA_OCCI_CTX,
      TRUE ~ NA_real_  # Keep NA if all are missing
    )
  ) %>%
  
  # Impute ABETA_HIPPO based on Thal criteria
  mutate(
    ABETA_HIPPO = case_when(
      is.na(ABETA_HIPPO) & !is.na(Thal) & Thal <= 2 ~ 0.00001, # Impute if Thal is ≤ 2
      TRUE ~ ABETA_HIPPO  # Keep existing values
    )
  ) %>%
  
  # Compute ABETA_COMB_FH as before, but set to NA if AD_LEVEL is missing
  mutate(
    ABETA_COMB_FH = ifelse(is.na(AD_LEVEL), NA_real_, (ABETA_FRNT_CTX + ABETA_HIPPO) / 2)
  ) %>%
  
  # If ABETA_COMB_FH is NA, make AD_LEVEL and Thal_BIN also NA
  mutate(
    AD_LEVEL = ifelse(is.na(ABETA_COMB_FH), NA, AD_LEVEL),
    Thal_BIN = ifelse(is.na(ABETA_COMB_FH), NA, Thal_BIN)
  )

#############UPR75_STAGING##################################
# Ensure ApoE is a factor and remove NA levels from facets
excel_graph <- excel %>%
  filter(!is.na(ApoE))
# Convert DEMENTIA to a factor for better visualization
excel_graph$DEMENTIA <- as.factor(excel_graph$DEMENTIA)

# Define custom colors for ApoE3 and ApoE4 groups
custom_colors <- c("ApoE3" = "#339999", "ApoE4" = "#FFDD57")  # Light/Dark blue for ApoE3
# Create the plot
ggplot(excel_graph, aes(x = ABETA_FRNT_HIPP, fill = ApoE, color = ApoE)) +
  geom_density(aes(y = ..density.. * 100), alpha = 0.4, size = 1) +  # Convert density to percentage & adjust transparency
  scale_fill_manual(values = c("ApoE3" = "#339999", "ApoE4" = "#FFDD57")) +  # Set fill colors
  scale_color_manual(values = c("ApoE3" = "black", "ApoE4" = "black")) +  # Set outline colors
  labs(
    title = "",
    x = "% Aβ",
    y = "% cases",  # Updated y-axis label
    fill = "ApoE Group",
    color = "ApoE Group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    legend.key = element_rect(fill = NA),  # Keep legend keys clean
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30),
    axis.text.y = element_text(angle = 0, hjust = 0.5, size = 30), # Rotate and adjust size
    axis.text.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.title.x = element_text(size = 30),
    plot.title = element_blank()
  )



ggsave("ABETA_FRNT_HIPP_Distribution_ApoE.png", width = 8, height = 6, dpi = 300)

# KS test comparing ASYN_FRNT_HIPP between ApoE3 and ApoE4
library(writexl)

data_apoE3 <- excel_graph %>% filter(ApoE == "ApoE3") %>% pull(ABETA_FRNT_HIPP)
data_apoE4 <- excel_graph %>% filter(ApoE == "ApoE4") %>% pull(ABETA_FRNT_HIPP)

ks_result <- ks.test(data_apoE3, data_apoE4)

# Convert to data frame for export
ks_df <- data.frame(
  statistic = ks_result$statistic,
  p_value = ks_result$p.value,
  alternative = ks_result$alternative,
  method = ks_result$method
)

# Write to Excel
write_xlsx(ks_df, "KS_Test_ABETA_FRNT_HIPP_ApoE.xlsx")

# Calculate the percentile cutoff (median of ABETA_COMB_FH)
upper_quartile <- quantile(excel$ABETA_COMB_FH, probs = 0.75, na.rm = TRUE)

# Define AD category based on 75th percentile
excel_UPR75 <- excel %>%
  mutate(AD_Category = case_when(
    ABETA_COMB_FH >= upper_quartile ~ "ABETA_HIGH",  # Strictly greater than
    ABETA_COMB_FH < upper_quartile ~ "ABETA_LOW"    # Less than or equal
  )) 

# Remove NA values
excel_UPR75_filter <- excel_UPR75 %>%
  filter(!is.na(AD_Category))

# Summarize data for `excel_filtered_UPR75`
summary_data_UPR75 <- excel_UPR75_filter %>%
  group_by(AD_Category, ApoE, DEMENTIA) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(
    Group = paste(ApoE, DEMENTIA, sep = "_"),
    ApoE_Group = paste(ApoE, AD_Category, sep = "_")
  ) %>%
  distinct()  # Ensure unique groups

# Summarize data for dementia proportions
summary_prop_data_UPR75 <- excel_UPR75_filter %>%
  group_by(ApoE, AD_Category, DEMENTIA) %>%
  summarise(Count = n(), .groups = "drop") %>%
  spread(key = DEMENTIA, value = Count, fill = 0) %>%
  mutate(
    Proportion_Dementia_1 = DEM_1 / (DEM_0 + DEM_1),
    ApoE_Group = paste(ApoE, AD_Category, sep = "_")
  )

# Remove NA groups for ApoE
summary_prop_data_UPR75 <- summary_prop_data_UPR75 %>%
  filter(!grepl("^NA_|ApoE3_NA|ApoE4_NA", ApoE_Group))  # Remove all NA groups



#SUMMARY Proportions ###################
write.csv(summary_prop_data_UPR75, "summary_prop_data_ABETA_UPR75.csv", row.names = FALSE)

# Add Proportion of No Dementia Cases
summary_prop_data_UPR75 <- summary_prop_data_UPR75 %>%
  mutate(Proportion_No_Dementia = 1 - Proportion_Dementia_1)

# Convert data to long format for ggplot
summary_prop_data_UPR75_long <- summary_prop_data_UPR75 %>%
  tidyr::pivot_longer(cols = c(Proportion_Dementia_1, Proportion_No_Dementia),
                      names_to = "Dementia_Status",
                      values_to = "Proportion")

# Rename labels
summary_prop_data_UPR75_long$Dementia_Status <- factor(summary_prop_data_UPR75_long$Dementia_Status,
                                                       levels = c("Proportion_No_Dementia", "Proportion_Dementia_1"),
                                                       labels = c("No Dementia", "Dementia"))
summary_prop_data_UPR75_long <- summary_prop_data_UPR75_long %>%
  mutate(
    ApoE_Group = case_when(
      ApoE_Group == "ApoE3_ABETA_HIGH" ~ "ApoE3: H",
      ApoE_Group == "ApoE3_ABETA_LOW"  ~ "ApoE3: N/L",
      ApoE_Group == "ApoE4_ABETA_HIGH" ~ "ApoE4: H",
      ApoE_Group == "ApoE4_ABETA_LOW"  ~ "ApoE4: N/L",
      TRUE ~ ApoE_Group  # Keep any other values unchanged
    )
  )
summary_prop_data_UPR75_long$Dementia_Status <- factor(summary_prop_data_UPR75_long$Dementia_Status, levels = c("Dementia", "No Dementia"))
# Create stacked bar plot
summary_prop_data_UPR75_plot <- ggplot(summary_prop_data_UPR75_long, aes(x = ApoE_Group, y = Proportion, fill = Dementia_Status)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = ifelse(Proportion == 0, "", paste0(round(Proportion * 100, 0)))),
            position = position_stack(vjust = 0.5), 
            size = 9, color =ifelse(summary_prop_data_Braak_long$Dementia_Status== "Dementia", "black", "black"),fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "Proportion (%)", fill = "Dementia Status") +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  ) +
  scale_fill_manual(values = c("Dementia" = "#FF9966", "No Dementia" = "#6699CC")) +  # Custom bar colors
  scale_color_manual(values = c("Dementia" = "black", "No Dementia" = "black"), guide = "none") +  # Fix text colors & remove legend
  coord_flip()  # Flip the axes

# Save the plot
ggsave("summary_prop_data_ABETA_UPR75.png", summary_prop_data_UPR75_plot, width = 8, height = 6, dpi = 300)
# Display the plot
print(summary_prop_data_UPR75_plot)


















# Create proportion table
proportion_table_UPR75 <- summary_prop_data_UPR75 %>%
  select(ApoE, AD_Category, Proportion_Dementia_1) %>%
  spread(key = AD_Category, value = Proportion_Dementia_1)

# Get unique ApoE Groups (after NA removal)
apoE_groups_UPR75 <- unique(summary_prop_data_UPR75$ApoE_Group)

# Contingency table for ApoE_Group and Proportion_Dementia_1
contingency_data_UPR75 <- summary_prop_data_UPR75 %>%
  group_by(ApoE_Group) %>%
  summarise(Proportion_Dementia_1 = mean(Proportion_Dementia_1))

# Print updated contingency table
print(contingency_data_UPR75)

# Construct global contingency table
global_contingency_UPR75 <- table(contingency_data_UPR75$ApoE_Group, contingency_data_UPR75$Proportion_Dementia_1)

# Perform global test (Fisher's Exact or Chi-Square)
global_test_UPR75 <- if (any(global_contingency_UPR75 < 5)) {
  fisher.test(global_contingency_UPR75)
} else {
  chisq.test(global_contingency_UPR75)
}

# Store global test result
all_results_UPR75 <- data.frame(
  Group1 = "Global",
  Group2 = "All Groups",
  Group1_N = sum(summary_prop_data_UPR75$DEM_0) + sum(summary_prop_data_UPR75$DEM_1),
  Group2_N = "N/A",
  Method = ifelse(any(global_contingency_UPR75 < 5), "Fisher's Exact Test", "Chi-Square Test"),
  P.Value = global_test_UPR75$p.value
)

# Pairwise comparisons for ApoE Groups
for (i in seq_along(apoE_groups_UPR75)) {
  for (j in seq_along(apoE_groups_UPR75)) {
    if (i < j) {  # Avoid duplicate comparisons
      group1_data <- summary_prop_data_UPR75 %>% filter(ApoE_Group == apoE_groups_UPR75[i])
      group2_data <- summary_prop_data_UPR75 %>% filter(ApoE_Group == apoE_groups_UPR75[j])
      
      # Construct contingency table
      contingency_table <- matrix(
        c(sum(group1_data$DEM_1), sum(group1_data$DEM_0),
          sum(group2_data$DEM_1), sum(group2_data$DEM_0)),
        nrow = 2
      )
      
      # Perform appropriate test
      test <- if (any(contingency_table < 5)) {
        fisher.test(contingency_table)
      } else {
        chisq.test(contingency_table)
      }
      
      # Append results
      all_results_UPR75 <- rbind(
        all_results_UPR75,
        data.frame(
          Group1 = apoE_groups_UPR75[i],
          Group2 = apoE_groups_UPR75[j],
          Group1_N = sum(group1_data$DEM_1 + group1_data$DEM_0),
          Group2_N = sum(group2_data$DEM_1 + group2_data$DEM_0),
          P.Value = test$p.value,
          Method = ifelse(any(contingency_table < 5), "Fisher's Exact Test", "Chi-Square Test")
        )
      )
    }
  }
}

# Separate global test from pairwise comparisons
global_result_UPR75 <- all_results_UPR75 %>% filter(Group1 == "Global")
pairwise_results_UPR75 <- all_results_UPR75 %>% filter(Group1 != "Global")

# Apply multiple testing corrections **only to pairwise comparisons**
pairwise_results_UPR75 <- pairwise_results_UPR75 %>%
  mutate(
    Bonferroni = p.adjust(P.Value, method = "bonferroni"),
    Holm = p.adjust(P.Value, method = "holm"),
    FDR = p.adjust(P.Value, method = "fdr")
  )

# Combine back with the global test result (without corrections)
all_results_UPR75 <- bind_rows(global_result_UPR75, pairwise_results_UPR75)

# Print final results
print(all_results_UPR75)

# Save results
write.csv(all_results_UPR75, "DEMENTIA_UPR75_ABETA_Test_Results.csv", row.names = FALSE)
write.table(all_results_UPR75, "DEMENTIA_UPR75_ABETA_Test_Results.txt", row.names = FALSE, sep = "\t", quote = FALSE)

# Print final cleaned results
final_results_ABETA_UPR75 <- all_results_UPR75 %>%
  select(Group1, Group2, Group1_N, Group2_N, P.Value, Bonferroni, Holm, FDR)

print(final_results_ABETA_UPR75)









#############ABC_STAGING##################################
# Set AD_LEVEL to NA if ABETA_COMB_FH is NA
excel <- excel %>%
  mutate(
    AD_LEVEL = ifelse(is.na(ABETA_COMB_FH), NA, AD_LEVEL)
  )
# Define AD category
excel_ABC <- excel %>%
  mutate(AD_Category = case_when(
    AD_LEVEL == "AD_LOW" ~ "ABETA_LOW",
    AD_LEVEL == "AD_HIGH" ~ "ABETA_HIGH",
    TRUE ~ NA_character_
  )) 


excel_ABC_filter <- excel_ABC %>% filter(!is.na(AD_Category))  # Remove NA values

# Summarize data for `excel_filtered_Thal`
summary_data_ABC <- excel_ABC_filter %>%
  group_by(AD_Category, ApoE, DEMENTIA) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Group = paste(ApoE, DEMENTIA, sep = "_"),
         ApoE_Group = paste(ApoE, AD_Category, sep = "_"))

# Summarize data for dementia proportions
summary_prop_data_ABC <- excel_ABC_filter %>%
  group_by(ApoE, AD_Category, DEMENTIA) %>%
  summarise(Count = n(), .groups = "drop") %>%
  spread(key = DEMENTIA, value = Count, fill = 0) %>%
  mutate(Proportion_Dementia_1 = DEM_1 / (DEM_0 + DEM_1),
         ApoE_Group = paste(ApoE, AD_Category, sep = "_"))

summary_prop_data_ABC <- summary_prop_data_ABC %>%
  filter(!grepl("ApoE3_NA|ApoE4_NA", ApoE_Group))  # Exclude specific NA groups

# Create proportion table
proportion_table_ABC <- summary_prop_data_ABC %>%
  select(ApoE, AD_Category, Proportion_Dementia_1) %>%
  spread(key = AD_Category, value = Proportion_Dementia_1)

# Remove NA groups for ApoE
summary_prop_data_ABC <- summary_prop_data_ABC %>%
  filter(!grepl("^NA_", ApoE_Group))




#SUMMARY Proportions ###################
write.csv(summary_prop_data_ABC, "summary_prop_data_ABETA_ABC.csv", row.names = FALSE)

# Add Proportion of No Dementia Cases
summary_prop_data_ABC <- summary_prop_data_ABC %>%
  mutate(Proportion_No_Dementia = 1 - Proportion_Dementia_1)

# Convert data to long format for ggplot
summary_prop_data_ABC_long <- summary_prop_data_ABC %>%
  tidyr::pivot_longer(cols = c(Proportion_Dementia_1, Proportion_No_Dementia),
                      names_to = "Dementia_Status",
                      values_to = "Proportion")

# Rename labels
summary_prop_data_ABC_long$Dementia_Status <- factor(summary_prop_data_ABC_long$Dementia_Status,
                                                     levels = c("Proportion_No_Dementia", "Proportion_Dementia_1"),
                                                     labels = c("No Dementia", "Dementia"))

summary_prop_data_ABC_long <- summary_prop_data_ABC_long %>%
  mutate(
    ApoE_Group = case_when(
      ApoE_Group == "ApoE3_ABETA_HIGH" ~ "ApoE3: H",
      ApoE_Group == "ApoE3_ABETA_LOW"  ~ "ApoE3: N/L",
      ApoE_Group == "ApoE4_ABETA_HIGH" ~ "ApoE4: H",
      ApoE_Group == "ApoE4_ABETA_LOW"  ~ "ApoE4: N/L",
      TRUE ~ ApoE_Group  # Keep any other values unchanged
    )
  )
summary_prop_data_ABC_long$Dementia_Status <- factor(summary_prop_data_ABC_long$Dementia_Status, levels = c("Dementia", "No Dementia"))

# Create stacked bar plot
summary_prop_data_ABC_plot <- ggplot(summary_prop_data_ABC_long, aes(x = ApoE_Group, y = Proportion, fill = Dementia_Status)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = ifelse(Proportion == 0, "", paste0(round(Proportion * 100, 0)))),
            position = position_stack(vjust = 0.5), 
            size = 9, color =ifelse(summary_prop_data_Braak_long$Dementia_Status== "Dementia", "black", "black"),fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "Proportion (%)", fill = "Dementia Status") +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  ) +
  scale_fill_manual(values = c("Dementia" = "#FF9966", "No Dementia" = "#6699CC")) +  # Custom bar colors
  scale_color_manual(values = c("Dementia" = "black", "No Dementia" = "black"), guide = "none") +  # Fix text colors & remove legend
  coord_flip()  # Flip the axes

# Save the plot
ggsave("summary_prop_data_ABETA_ABC.png", summary_prop_data_ABC_plot, width = 8, height = 6, dpi = 300)
# Display the plot
print(summary_prop_data_ABC_plot)





# Get unique ApoE Groups (after NA removal)
apoE_groups_ABC <- unique(summary_prop_data_ABC$ApoE_Group)

# Contingency table for ApoE_Group and Proportion_Dementia_1
contingency_data_ABC <- summary_prop_data_ABC %>%
  group_by(ApoE_Group) %>%
  summarise(Proportion_Dementia_1 = mean(Proportion_Dementia_1))

# Print updated contingency table
print(contingency_data_ABC)

# Construct global contingency table
global_contingency_ABC <- table(contingency_data_ABC$ApoE_Group, contingency_data_ABC$Proportion_Dementia_1)

# Perform global test (Fisher's Exact or Chi-Square)
global_test_ABC <- if (any(global_contingency_ABC < 5)) {
  fisher.test(global_contingency_ABC)
} else {
  chisq.test(global_contingency_ABC)
}

# Store global test result
all_results_ABC <- data.frame(
  Group1 = "Global",
  Group2 = "All Groups",
  Group1_N = sum(summary_prop_data_ABC$DEM_0) + sum(summary_prop_data_ABC$DEM_1),
  Group2_N = "N/A",
  Method = ifelse(any(global_contingency_ABC < 5), "Fisher's Exact Test", "Chi-Square Test"),
  P.Value = global_test_ABC$p.value
)

# Pairwise comparisons for ApoE Groups
for (i in seq_along(apoE_groups_ABC)) {
  for (j in seq_along(apoE_groups_ABC)) {
    if (i < j) {  # Avoid duplicate comparisons
      group1_data <- summary_prop_data_ABC %>% filter(ApoE_Group == apoE_groups_ABC[i])
      group2_data <- summary_prop_data_ABC %>% filter(ApoE_Group == apoE_groups_ABC[j])
      
      # Construct contingency table
      contingency_table <- matrix(
        c(sum(group1_data$DEM_1), sum(group1_data$DEM_0),
          sum(group2_data$DEM_1), sum(group2_data$DEM_0)),
        nrow = 2
      )
      
      # Perform appropriate test
      test <- if (any(contingency_table < 5)) {
        fisher.test(contingency_table)
      } else {
        chisq.test(contingency_table)
      }
      
      # Append results
      all_results_ABC <- rbind(
        all_results_ABC,
        data.frame(
          Group1 = apoE_groups_ABC[i],
          Group2 = apoE_groups_ABC[j],
          Group1_N = sum(group1_data$DEM_1 + group1_data$DEM_0),
          Group2_N = sum(group2_data$DEM_1 + group2_data$DEM_0),
          P.Value = test$p.value,
          Method = ifelse(any(contingency_table < 5), "Fisher's Exact Test", "Chi-Square Test")
        )
      )
    }
  }
}

# Separate global test from pairwise comparisons
global_result_ABC <- all_results_ABC %>% filter(Group1 == "Global")
pairwise_results_ABC <- all_results_ABC %>% filter(Group1 != "Global")

# Apply multiple testing corrections **only to pairwise comparisons**
pairwise_results_ABC <- pairwise_results_ABC %>%
  mutate(
    Bonferroni = p.adjust(P.Value, method = "bonferroni"),
    Holm = p.adjust(P.Value, method = "holm"),
    FDR = p.adjust(P.Value, method = "fdr")
  )

# Combine back with the global test result (without corrections)
all_results_ABC <- bind_rows(global_result_ABC, pairwise_results_ABC)

# Print final results
print(all_results_ABC)

# Save results
write.csv(all_results_ABC, "DEMENTIA_ABC_ABETA_Test_Results.csv", row.names = FALSE)
write.table(all_results_ABC, "DEMENTIA_ABC_ABETA_Test_Results.txt", row.names = FALSE, sep = "\t", quote = FALSE)

# Print final cleaned results
final_results_ABETA_ABC <- all_results_ABC %>%
  select(Group1, Group2, Group1_N, Group2_N, P.Value, Bonferroni, Holm, FDR)

print(final_results_ABETA_ABC)
#############Thal_STAGING##################################
# Define AD category
excel_Thal <- excel %>%
  mutate(AD_Category = case_when(
    Thal_BIN == "ABETA_LOW" ~ "ABETA_LOW",
    Thal_BIN == "ABETA_HIGH" ~ "ABETA_HIGH",
    TRUE ~ NA_character_
  )) 

excel_Thal_filter <- excel_Thal %>% filter(!is.na(AD_Category))  # Remove NA values

# Summarize data for `excel_filtered_Thal`
summary_data_Thal <- excel_Thal_filter %>%
  group_by(AD_Category, ApoE, DEMENTIA) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Group = paste(ApoE, DEMENTIA, sep = "_"),
         ApoE_Group = paste(ApoE, AD_Category, sep = "_"))

# Summarize data for dementia proportions
summary_prop_data_Thal <- excel_Thal_filter %>%
  group_by(ApoE, AD_Category, DEMENTIA) %>%
  summarise(Count = n(), .groups = "drop") %>%
  spread(key = DEMENTIA, value = Count, fill = 0) %>%
  mutate(Proportion_Dementia_1 = DEM_1 / (DEM_0 + DEM_1),
         ApoE_Group = paste(ApoE, AD_Category, sep = "_"))

summary_prop_data_Thal <- summary_prop_data_Thal %>%
  filter(!grepl("ApoE3_NA|ApoE4_NA", ApoE_Group))  # Exclude specific NA groups

# Create proportion table
proportion_table_Thal <- summary_prop_data_Thal %>%
  select(ApoE, AD_Category, Proportion_Dementia_1) %>%
  spread(key = AD_Category, value = Proportion_Dementia_1)

# Remove NA groups for ApoE
summary_prop_data_Thal <- summary_prop_data_Thal %>%
  filter(!grepl("^NA_", ApoE_Group))




#SUMMARY Proportions ###################
write.csv(summary_prop_data_Thal, "summary_prop_data_Thal.csv", row.names = FALSE)

# Add Proportion of No Dementia Cases
summary_prop_data_Thal <- summary_prop_data_Thal %>%
  mutate(Proportion_No_Dementia = 1 - Proportion_Dementia_1)

# Convert data to long format for ggplot
summary_prop_data_Thal_long <- summary_prop_data_Thal %>%
  tidyr::pivot_longer(cols = c(Proportion_Dementia_1, Proportion_No_Dementia),
                      names_to = "Dementia_Status",
                      values_to = "Proportion")

# Rename labels
summary_prop_data_Thal_long$Dementia_Status <- factor(summary_prop_data_Thal_long$Dementia_Status,
                                                      levels = c("Proportion_No_Dementia", "Proportion_Dementia_1"),
                                                      labels = c("No Dementia", "Dementia"))
summary_prop_data_Thal_long <- summary_prop_data_Thal_long %>%
  mutate(
    ApoE_Group = case_when(
      ApoE_Group == "ApoE3_ABETA_HIGH" ~ "ApoE3: H",
      ApoE_Group == "ApoE3_ABETA_LOW"  ~ "ApoE3: N/L",
      ApoE_Group == "ApoE4_ABETA_HIGH" ~ "ApoE4: H",
      ApoE_Group == "ApoE4_ABETA_LOW"  ~ "ApoE4: N/L",
      TRUE ~ ApoE_Group  # Keep any other values unchanged
    )
  )

summary_prop_data_Thal_long$Dementia_Status <- factor(summary_prop_data_Thal_long$Dementia_Status, levels = c("Dementia", "No Dementia"))
# Create stacked bar plot
summary_prop_data_Thal_plot <- ggplot(summary_prop_data_Thal_long, aes(x = ApoE_Group, y = Proportion, fill = Dementia_Status)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = ifelse(Proportion == 0, "", paste0(round(Proportion * 100, 0)))),
            position = position_stack(vjust = 0.5), 
            size = 9, color =ifelse(summary_prop_data_Braak_long$Dementia_Status== "Dementia", "black", "black"),fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "Proportion (%)", fill = "Dementia Status") +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  ) +
  scale_fill_manual(values = c("Dementia" = "#FF9966", "No Dementia" = "#6699CC")) +  # Custom bar colors
  scale_color_manual(values = c("Dementia" = "black", "No Dementia" = "black"), guide = "none") +  # Fix text colors & remove legend
  coord_flip()  # Flip the axes

# Save the plot
ggsave("summary_prop_data_Thal.png", summary_prop_data_Thal_plot, width = 8, height = 6, dpi = 300)
# Display the plot
print(summary_prop_data_Thal_plot)










# Get unique ApoE Groups (after NA removal)
apoE_groups_Thal <- unique(summary_prop_data_Thal$ApoE_Group)

# Contingency table for ApoE_Group and Proportion_Dementia_1
contingency_data_Thal <- summary_prop_data_Thal %>%
  group_by(ApoE_Group) %>%
  summarise(Proportion_Dementia_1 = mean(Proportion_Dementia_1))

# Print updated contingency table
print(contingency_data_Thal)

# Construct global contingency table
global_contingency_Thal <- table(contingency_data_Thal$ApoE_Group, contingency_data_Thal$Proportion_Dementia_1)

# Perform global test (Fisher's Exact or Chi-Square)
global_test_Thal <- if (any(global_contingency_Thal < 5)) {
  fisher.test(global_contingency_Thal)
} else {
  chisq.test(global_contingency_Thal)
}

# Store global test result
all_results_Thal <- data.frame(
  Group1 = "Global",
  Group2 = "All Groups",
  Group1_N = sum(summary_prop_data_Thal$DEM_0) + sum(summary_prop_data_Thal$DEM_1),
  Group2_N = "N/A",
  Method = ifelse(any(global_contingency_Thal < 5), "Fisher's Exact Test", "Chi-Square Test"),
  P.Value = global_test_Thal$p.value
)

# Pairwise comparisons for ApoE Groups
for (i in seq_along(apoE_groups_Thal)) {
  for (j in seq_along(apoE_groups_Thal)) {
    if (i < j) {  # Avoid duplicate comparisons
      group1_data <- summary_prop_data_Thal %>% filter(ApoE_Group == apoE_groups_Thal[i])
      group2_data <- summary_prop_data_Thal %>% filter(ApoE_Group == apoE_groups_Thal[j])
      
      # Construct contingency table
      contingency_table <- matrix(
        c(sum(group1_data$DEM_1), sum(group1_data$DEM_0),
          sum(group2_data$DEM_1), sum(group2_data$DEM_0)),
        nrow = 2
      )
      
      # Perform appropriate test
      test <- if (any(contingency_table < 5)) {
        fisher.test(contingency_table)
      } else {
        chisq.test(contingency_table)
      }
      
      # Append results
      all_results_Thal <- rbind(
        all_results_Thal,
        data.frame(
          Group1 = apoE_groups_Thal[i],
          Group2 = apoE_groups_Thal[j],
          Group1_N = sum(group1_data$DEM_1 + group1_data$DEM_0),
          Group2_N = sum(group2_data$DEM_1 + group2_data$DEM_0),
          P.Value = test$p.value,
          Method = ifelse(any(contingency_table < 5), "Fisher's Exact Test", "Chi-Square Test")
        )
      )
    }
  }
}

# Separate global test from pairwise comparisons
global_result_Thal <- all_results_Thal %>% filter(Group1 == "Global")
pairwise_results_Thal <- all_results_Thal %>% filter(Group1 != "Global")

# Apply multiple testing corrections **only to pairwise comparisons**
pairwise_results_Thal <- pairwise_results_Thal %>%
  mutate(
    Bonferroni = p.adjust(P.Value, method = "bonferroni"),
    Holm = p.adjust(P.Value, method = "holm"),
    FDR = p.adjust(P.Value, method = "fdr")
  )

# Combine back with the global test result (without corrections)
all_results_Thal <- bind_rows(global_result_Thal, pairwise_results_Thal)
# Print final results
print(all_results_Thal)

# Save results
write.csv(all_results_Thal, "DEMENTIA_Thal_ABETA_Test_Results.csv", row.names = FALSE)
write.table(all_results_Thal, "DEMENTIA_Thal_ABETA_Test_Results.txt", row.names = FALSE, sep = "\t", quote = FALSE)

# Print final cleaned results
final_results_Thal <- all_results_Thal %>%
  select(Group1, Group2, Group1_N, Group2_N, P.Value, Bonferroni, Holm, FDR)

print(final_results_Thal)


###########################BREAK##############################








#############Combine_Cutoff ####################################

excel$ABC_Level <- excel_ABC$AD_Category
excel$Thal_Level <- excel_Thal$AD_Category
excel$ABETA_Upperquartile<- excel_UPR75$AD_Category

excel_plot <- excel %>%
  filter(!is.na(ABC_Level))
excel_plot <- excel_plot %>%
  filter(!is.na(Thal_Level))
excel_plot <- excel_plot %>%
  filter(!is.na(ABETA_Upperquartile))


#############LINE PLOT ####################################
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Convert grouping columns to factors
excel <- excel %>%
  mutate(
    ABC_Level = as.factor(ABC_Level),
    Thal_Level = as.factor(Thal_Level),
    ABETA_Upperquartile = as.factor(ABETA_Upperquartile)
  )

# Reshape data for long format (to plot multiple groups) and remove NA values
excel_long <- excel %>%
  pivot_longer(cols = c("ABC_Level", "Thal_Level", "ABETA_Upperquartile"), 
               names_to = "AD_Group", values_to = "AD_Value") %>%
  drop_na(AD_Value)  # Remove rows where LB_Value is NA

# Compute mean and standard error for ASYN_FRNT_HIPP within each group
summary_data <- excel_long %>%
  group_by(AD_Group, AD_Value) %>%
  summarise(
    Mean_ABETA = mean(ABETA_COMB_FH, na.rm = TRUE),
    SE_ABETA = sd(ABETA_COMB_FH, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Ensure AD_Value is a factor with correct ordering
summary_data$AD_Value <- factor(summary_data$AD_Value, levels = c("ABETA_LOW", "ABETA_HIGH"))
# Ensure correct legend order by setting factor levels
summary_data <- summary_data %>%
  mutate(AD_Group = factor(AD_Group, 
                           levels = c("ABC_Level", "Thal_Level", "ABETA_Upperquartile")))
# Plot the updated data
plot <- ggplot(summary_data, aes(x = AD_Value, y = Mean_ABETA, group = AD_Group, color = AD_Group)) +
  geom_line(size = 1) +  # Line for mean ASYN_FRNT_HIPP
  geom_point(size = 3) +  # Points for mean values
  geom_errorbar(aes(ymin = Mean_ABETA - SE_ABETA, ymax = Mean_ABETA + SE_ABETA), width = 0.05) +  # Error bars
  scale_color_manual(values = c("ABC_Level" = "red",
                                "Thal_Level" = "blue",
                                "ABETA_Upperquartile" = "green3"),
                     labels = c("ABC_Level" = "ABC moderate or high",
                                "Thal_Level" = "Thal 4 & 5",
                                "ABETA_Upperquartile" = "Aβ in 75% quartile")) +
  scale_x_discrete(labels = c("Absent\nor low",
                              "Present\nor high")) +
  labs(
    x = "",
    y = "% Aβ",
    color = "AD Group") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 30, color = "black", hjust = 0.5, vjust = 0.5, lineheight = 1),  # Larger and bolder x-axis text
    axis.text.y = element_text(size = 30, color = "black"),  # Larger and bolder y-axis text
    axis.title.x = element_text(size = 30, color = "black"),  # Larger x-axis title
    axis.title.y = element_text(size = 30, color = "black"),  # Larger y-axis title
    axis.line = element_line(size = 1.5),  # Make axes bolder
    axis.ticks = element_line(size = 1),  # Make axis ticks bolder
    legend.text = element_text(size = 16),  # Larger and bolder legend text
    legend.title = element_blank(),  # Larger and bolder legend title
    plot.title = element_blank(),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_blank()  # Remove border around the plot
  )
# Display the plot
print(plot)

# Save the plot as a PNG
ggsave("ABETA_COMB_FH_Line_Graph_UPR75.png", plot, width = 7, height = 6)

library(dunn.test)

# Kruskal-Wallis test across AD_Group + AD_Value
kw <- kruskal.test(ABETA_COMB_FH ~ interaction(AD_Group, AD_Value), data = excel_long)

# Pairwise Dunn's test
excel_long$Group_Label <- paste(excel_long$AD_Group, excel_long$AD_Value)
dunn_res <- dunn.test(excel_long$ABETA_COMB_FH, excel_long$Group_Label, method = "bonferroni")

# View results
print(kw)
print(dunn_res)

# Save results to CSV
dunn_table <- data.frame(
  Comparison = dunn_res$comparisons,
  Z = dunn_res$Z,
  P_Adjusted = dunn_res$P.adjusted
)

# Rename group and value labels in comparison strings
dunn_table$Comparison <- dunn_table$Comparison %>%
  str_replace_all("ABC_Level", "ABC moderate or high") %>%
  str_replace_all("Thal_Level", "Thal 4 & 5") %>%
  str_replace_all("ABETA_Upperquartile", "Aβ in 75% quartile") %>%
  str_replace_all("ABETA_LOW", "Absent/low") %>%
  str_replace_all("ABETA_HIGH", "Present/high")

write.csv(dunn_table, "ABETA_COMB_FH_Line_Graph_DunnTest_Results.csv", row.names = FALSE)

#############RATIO SUMMARY TABLE ####################################

# Load necessary libraries
library(dplyr)

# Summarize data for each combination of DEMENTIA and LB_Category
summary_table <- excel %>%
  pivot_longer(cols = all_of(c("ABC_Level", "Thal_Level", "ABETA_Upperquartile")), names_to = "AD_Group", values_to = "AD_Category") %>%
  filter(!is.na(AD_Category)) %>%  # Remove NA values in LB_Category
  group_by(AD_Group, DEMENTIA, AD_Category) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = c(DEMENTIA, AD_Category),
    values_from = Count,  # Use the "Count" column for values
    names_glue = "{AD_Category}_{DEMENTIA}"
  ) %>%
  mutate(
    ABETA_LOW_RATIO_DEM1_DEM0 = ABETA_LOW_DEM_1 / ABETA_LOW_DEM_0,
    ABETA_HIGH_RATIO_DEM1_DEM0 = ABETA_HIGH_DEM_1 / ABETA_HIGH_DEM_0,
    Total_Count = rowSums(select(., starts_with("ABETA_")), na.rm = TRUE)  # Sum of all count columns per row
  )

# Print the updated summary table
print(summary_table)
summary_table <- summary_table %>%
  mutate(AD_Group = factor(AD_Group, levels = c("ABC_Level", "Thal_Level", "ABETA_Upperquartile"))) %>%
  arrange(AD_Group)

print(summary_table)
# Save as CSV
write.csv(summary_table, "DEMENTIA_ABETA_Ratio_Table_UPR75.csv", row.names = FALSE)
write.table(summary_table, "DEMENTIA_ABETA_Ratio_Table_UPR75.txt", row.names = FALSE, sep = "\t", quote = FALSE)


#############PATHOLOGY_TABLE##########################


# Use the existing dataset
summary_table <- excel %>%
  pivot_longer(cols = all_of(c("ABC_Level", "Thal_Level", "ABETA_Upperquartile")), names_to = "AD_Group", values_to = "AD_Category") %>%
  filter(!is.na(AD_Category)) %>%  # Remove NA values in LB_Category
  group_by(AD_Group, DEMENTIA, AD_Category) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = c(DEMENTIA, AD_Category),
    values_from = Count,  # Use the "Count" column for values
    names_glue = "{AD_Category}_{DEMENTIA}"
  ) %>%
  mutate(
    Sensitivity = ABETA_HIGH_DEM_1 / (ABETA_HIGH_DEM_1 + ABETA_LOW_DEM_1),
    Specificity = ABETA_LOW_DEM_0 / (ABETA_LOW_DEM_0 + ABETA_HIGH_DEM_0),
    Odds_Ratio = (ABETA_HIGH_DEM_1 / ABETA_HIGH_DEM_0) / (ABETA_LOW_DEM_1 / ABETA_LOW_DEM_0),
    PPV = ABETA_HIGH_DEM_1 / (ABETA_HIGH_DEM_1 + ABETA_HIGH_DEM_0),
    Pathology_Absence_Ratio = (ABETA_LOW_DEM_1 / ABETA_LOW_DEM_0),
    Pathology_Presence_Ratio = (ABETA_HIGH_DEM_1 / ABETA_HIGH_DEM_0),
    Total_Count = rowSums(select(., starts_with("ABETA_")), na.rm = TRUE)  # Sum of all count columns per row
  )
summary_table <- summary_table %>%
  mutate(AD_Group = factor(AD_Group, levels = c("ABC_Level", "Thal_Level", "ABETA_Upperquartile"))) %>%
  arrange(AD_Group)

print(summary_table)
# Save to CSV
write.csv(summary_table, "Pathology_Statistics_ABETA_UPR75.csv", row.names = FALSE)
write.table(summary_table, "Pathology_Statistics_ABETA_UPR75.txt", row.names = FALSE, sep = "\t", quote = FALSE)
# Print the final table
print(summary_table)

library(fmsb)

# Normalize values
summary_table <- summary_table %>% mutate(
  Odds_Ratio = Odds_Ratio / max(Odds_Ratio, na.rm = TRUE),
  Pathology_Absence_Ratio = Pathology_Absence_Ratio / max(Pathology_Absence_Ratio, na.rm = TRUE),
  Pathology_Presence_Ratio = Pathology_Presence_Ratio / max(Pathology_Presence_Ratio, na.rm = TRUE)
)
# Save to CSV
write.csv(summary_table, "Pathology_Statistics_Norm_UPR75.csv", row.names = FALSE)
write.table(summary_table, "Pathology_Statistics_Norm_UPR75.txt", row.names = FALSE, sep = "\t", quote = FALSE)
# Print the final table
print(summary_table)
#############RADAR_PLOT#######################
# Define max and min values
max_vals <- rep(1, ncol(summary_table) - 1)
min_vals <- rep(0, ncol(summary_table) - 1)

# Combine min/max rows with the actual data
data <- rbind(max_vals, min_vals, summary_table[,-1])

data <- as.data.frame(data)

# Reorder columns for proper radar chart visualization
data_rotated <- data[, c("Specificity", "Pathology_Absence_Ratio", "Pathology_Presence_Ratio",
                         "Odds_Ratio", "PPV", "Sensitivity")]

colnames(data_rotated) <- c("Specificity", 
                            "PAR",
                            "PPR",
                            "OR", 
                            "PPV", 
                            "Sensitivity")
# Plot radar chart
# Adjust margins for a better layout
par(mar = c(1, 1, 1, 10) + 0.1)   
# Set font to bold before plotting
par(font = 1)  # 2 = Bold
# Plot radar chart
radarchart(data_rotated,
           seg = 5,                     
           axistype = 1,                
           pcol = c("red",  "blue", "green"),  
           pfcol = c(rgb(1, 0, 0, 0.3),  
                     rgb(0, 0, 1, 0.3),  
                     rgb(0, 1, 0, 0.3)),  
           cglcol = "grey",             
           cglty = 1,                   
           axislabcol = "grey",         
           caxislabels = seq(0, 1, 0.2), 
           vlcex = 3,  # Increase text size of variable labels
           cex.axis = 2,  # Increase size of axis labels
           cex.lab = 2  # (If applicable) Increase size of axis labels
)

# Adding a properly formatted legend
legend("right",
       inset = c(-0.25, 0),    # Moves legend further right
       legend = c("ABC moderate or high",
                  "Thal 4 & 5",
                  expression(italic(A) * beta ~ " pathology in 75% quartile")),
       bty = "n", pch = 20,
       col = c("red", "blue", "green"),
       text.col = "black",    cex = 1.2,   xpd = TRUE)  # Ensure text is visible outside

# Save radar chart as PNG
png("Radar_Chart_ABETA_UPR75_2.png", width = 3500, height = 2000, res=300,  bg = "transparent")

# Increase right margin to make space for legend
par(mar = c(1, 1, 1, 10) + 0.1) 
# Set font to bold before plotting
par(font = 1)  # 2 = Bold
# Plot radar chart
radarchart(data_rotated,
           seg = 5,                     
           axistype = 1,                
           pcol = c("red", "blue", "green"),  
           pfcol = c(rgb(1, 0, 0, 0.3),  
                     rgb(0, 0, 1, 0.3),  
                     rgb(0, 1, 0, 0.3)),  
           plwd = 4,                    
           cglcol = "grey",             
           cglty = 1,                   
           axislabcol = "grey",         
           caxislabels = seq(0, 1, 0.2), 
           vlcex = 3,  # Increase text size of variable labels
           cex.axis = 2,  # Increase size of axis labels
           cex.lab = 2  # (If applicable) Increase size of axis labels
)

# Adding a properly formatted legend
legend("right",
       inset = c(-0.2, 0),    # Moves legend further right
       legend = c("ABC moderate or high",
                  "Thal 4 & 5",
                  expression(italic(A) * beta ~ " pathology in 75% quartile")),
       bty = "n", pch = 20,
       col = c("red", "blue", "green"),
       text.col = "black",    cex = 1.2,   xpd = TRUE)  # Ensure text is visible outside

dev.off()
# Set font to bold before plotting
par(font = 1)  

##########################CHECKERBOARD_PLOT_UPR75###################################
setwd(datadir)

final_results_ABETA_UPR75 <- final_results_ABETA_UPR75 %>%
  filter(Group1 != "Global" & Group2 != "All Groups") %>%
  mutate(
    Group = "GROUP",
    Group1 = case_when(
      Group1 == "ApoE3_ABETA_HIGH" ~ "ApoE3: H",
      Group1 == "ApoE3_ABETA_LOW"  ~ "ApoE3: N/L",
      Group1 == "ApoE4_ABETA_HIGH" ~ "ApoE4: H",
      Group1 == "ApoE4_ABETA_LOW"  ~ "ApoE4: N/L",
      TRUE ~ Group1  # Keep any other values unchanged
    ),
    Group2 = case_when(
      Group2 == "ApoE3_ABETA_HIGH" ~ "ApoE3: H",
      Group2 == "ApoE3_ABETA_LOW"  ~ "ApoE3: N/L",
      Group2 == "ApoE4_ABETA_HIGH" ~ "ApoE4: H",
      Group2 == "ApoE4_ABETA_LOW"  ~ "ApoE4: N/L",
      TRUE ~ Group2  # Keep any other values unchanged
    )
  )


combined_all_results <- bind_rows(final_results_ABETA_UPR75,)


# Step 1: LoLB the dataset
df <- combined_all_results %>%
  as.data.frame()
# Step 2: Duplicate all rows and swap `Group1` and `Group2` in the duplicated rows
df_reciprocal <- df %>%
  rename(Group1 = Group2, Group2 = Group1)  # Swap the values

# Step 3: Combine the original and reciprocal data
df_full <- bind_rows(df, df_reciprocal) %>%
  distinct(Group1, Group2, .keep_all = TRUE)  # Ensure no duplicates are removed

# Step 4: Display the final dataset
print(df_full)

df<- df_full

# Order variables within each "Group"
df <- df %>%
  group_by(Group) %>%
  arrange(Group, Group1, Group2, .by_group = TRUE)

# Get unique labels maintaining order within each "Group"
all_labels <- df %>%
  select(Group, Group1, Group2) %>%
  pivot_longer(cols = c(Group1, Group2), values_to = "Label") %>%
  distinct(Group, Label) %>%
  arrange(Group) %>%
  pull(Label)

# Create full grid while keeping group-wise structure
complete_grid <- expand.grid(Group1 = all_labels, Group2 = all_labels, stringsAsFactors = FALSE)

# Merge with actual data while preserving "Group" order
df_full <- complete_grid %>%
  left_join(df, by = c("Group1", "Group2"))

# **Step 2: LBd Reciprocal Values Properly**
df_reciprocal <- df_full %>%
  rename(Group1 = Group2, Group2 = Group1)  # Swap Group1 and Group2

# Ensure reciprocal pairs have the same Bonferroni values
df_reciprocal <- df_reciprocal %>%
  left_join(df_full %>% select(Group1, Group2, Bonferroni), by = c("Group1", "Group2")) %>%
  mutate(Bonferroni = coalesce(Bonferroni.x, Bonferroni.y)) %>%
  select(-Bonferroni.x, -Bonferroni.y)  # Keep only the updated Bonferroni values

df_full <- bind_rows(df_full, df_reciprocal) %>%
  distinct(Group1, Group2, .keep_all = TRUE)  # Ensure duplicates are kept

# Categorize Bonferroni values for coloring
df_full <- df_full %>%
  mutate(
    color = case_when(
      Bonferroni < 0.05 ~ "grey40",  # Use "grey40" explicitly
      Bonferroni >= 0.05 ~ "grey95",
      is.na(Bonferroni) ~ NA_character_
    )
  )

# Ensure levels preserve the correct group-wise order
df_full$Group1 <- factor(df_full$Group1, levels = all_labels)
df_full$Group2 <- factor(df_full$Group2, levels = rev(all_labels))  # Ensure full mirroring

# Extract Group1 and Group2 levels for correct coloring of axis labels
group1_levels <- levels(df_full$Group1)
group2_levels <- levels(df_full$Group2)

# Step 3: Plot checkerboard heatmap with FULL Reciprocal Values
library(ggplot2)
library(ggtext)
library(ggnewscale)

# Define color labels
color_labels <- c("p <0.05" = "grey40",
                  "p 0.05-0.1" = "grey70",
                  "p <0.1" = "grey95")

# Ensure missing values (NA) are properly handled
df_full$Bonferroni_cat <- cut(df_full$Bonferroni, 
                              breaks = c(-Inf, 0.05, 0.1, Inf), 
                              labels = names(color_labels))

# Convert NA values to explicit category for better visualization
df_full$Bonferroni_cat <- as.character(df_full$Bonferroni_cat)
df_full$Bonferroni_cat[is.na(df_full$Bonferroni)] <- "NA"

# LBd "NA" to the color scale so blank cells appear white
color_labels["NA"] <- "white"

# Create color mappings for text labels
group2_colors <- ifelse(grepl("ApoE3", df_full$Group2), "grey40", "black")
group1_colors <- ifelse(grepl("ApoE3", df_full$Group1), "grey40", "black")

# Create the heatmap plot
ggplot(df_full, aes(x = Group2, y = Group1, fill = Bonferroni_cat)) +
  geom_tile(color = "black") +
  geom_richtext(
    aes(label = ifelse(
      !is.na(Bonferroni),
      ifelse(Bonferroni < 0.01, "**<0.01**",  # Replace very low p-values with "<0.001"
             ifelse(Bonferroni < 0.05, paste0("**", sprintf("%.2f", Bonferroni), "**"), 
                    sprintf("%.2f", Bonferroni))),
      "")),  
    size = 8, fill = NA, label.color = NA,  
    color = ifelse(!is.na(df_full$Bonferroni) & df_full$Bonferroni < 0.05, "white", "black")  
  ) +
  scale_fill_manual(
    name = "Bonferroni P-Value",
    values = c(
      "p <0.05" = "grey40",
      "p 0.05-0.1" = "grey70",
      "p <0.1" = "grey95"
    ),
    breaks = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    labels = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    na.value = "white"  # Keeps blank cells white
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_markdown(angle = 90, hjust = 1, size = 14),  
    axis.text.y = element_markdown(angle = 0, hjust = 1, size = 14),
    panel.grid = element_blank()
  ) +
  labs(x = "", y = "", fill = "Bonferroni P-Value") +
  ggtitle("") +  
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +  
  coord_fixed()


Bonferroni_Heatmap_plot <-ggplot(df_full, aes(x = Group2, y = Group1, fill = Bonferroni_cat)) +
  geom_tile(color = "black") +
  geom_richtext(
    aes(label = ifelse(
      !is.na(Bonferroni),
      ifelse(Bonferroni < 0.01, "**<0.01**",  # Replace very low p-values with "<0.001"
             ifelse(Bonferroni < 0.05, paste0("**", sprintf("%.2f", Bonferroni), "**"), 
                    sprintf("%.2f", Bonferroni))),
      "")),  
    size = 8, fill = NA, label.color = NA,  
    color = ifelse(!is.na(df_full$Bonferroni) & df_full$Bonferroni < 0.05, "white", "black")  
  ) +
  scale_fill_manual(
    name = "Bonferroni P-Value",
    values = c(
      "p <0.05" = "grey40",
      "p 0.05-0.1" = "grey70",
      "p <0.1" = "grey95"
    ),
    breaks = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    labels = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    na.value = "white"  # Keeps blank cells white
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_markdown(angle = 90, hjust = 1, size = 14),  
    axis.text.y = element_markdown(angle = 0, hjust = 1, size = 14),
    panel.grid = element_blank()
  ) +
  labs(x = "", y = "", fill = "Bonferroni P-Value") +
  ggtitle("") +  
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +  
  coord_fixed()

# Step 5: Save the plot as a PNG
ggsave("Bonferroni_Heatmap_plot_ABETA_UPR75.png", 
       plot = Bonferroni_Heatmap_plot, width = 10, height = 8, dpi = 300)
Bonferroni_Heatmap_plot_ABETA_UPR75 <- Bonferroni_Heatmap_plot

##########################CHECKERBOARD_PLOT_Thal###################################
setwd(datadir)

final_results_Thal <- final_results_Thal %>%
  filter(Group1 != "Global" & Group2 != "All Groups") %>%
  mutate(
    Group = "GROUP",
    Group1 = case_when(
      Group1 == "ApoE3_ABETA_HIGH" ~ "ApoE3: H",
      Group1 == "ApoE3_ABETA_LOW"  ~ "ApoE3: N/L",
      Group1 == "ApoE4_ABETA_HIGH" ~ "ApoE4: H",
      Group1 == "ApoE4_ABETA_LOW"  ~ "ApoE4: N/L",
      TRUE ~ Group1  # Keep any other values unchanged
    ),
    Group2 = case_when(
      Group2 == "ApoE3_ABETA_HIGH" ~ "ApoE3: H",
      Group2 == "ApoE3_ABETA_LOW"  ~ "ApoE3: N/L",
      Group2 == "ApoE4_ABETA_HIGH" ~ "ApoE4: H",
      Group2 == "ApoE4_ABETA_LOW"  ~ "ApoE4: N/L",
      TRUE ~ Group2  # Keep any other values unchanged
    )
  )


combined_all_results <- bind_rows(final_results_Thal,)


# Step 1: LoLB the dataset
df <- combined_all_results %>%
  as.data.frame()
# Step 2: Duplicate all rows and swap `Group1` and `Group2` in the duplicated rows
df_reciprocal <- df %>%
  rename(Group1 = Group2, Group2 = Group1)  # Swap the values

# Step 3: Combine the original and reciprocal data
df_full <- bind_rows(df, df_reciprocal) %>%
  distinct(Group1, Group2, .keep_all = TRUE)  # Ensure no duplicates are removed

# Step 4: Display the final dataset
print(df_full)

df<- df_full

# Order variables within each "Group"
df <- df %>%
  group_by(Group) %>%
  arrange(Group, Group1, Group2, .by_group = TRUE)

# Get unique labels maintaining order within each "Group"
all_labels <- df %>%
  select(Group, Group1, Group2) %>%
  pivot_longer(cols = c(Group1, Group2), values_to = "Label") %>%
  distinct(Group, Label) %>%
  arrange(Group) %>%
  pull(Label)

# Create full grid while keeping group-wise structure
complete_grid <- expand.grid(Group1 = all_labels, Group2 = all_labels, stringsAsFactors = FALSE)

# Merge with actual data while preserving "Group" order
df_full <- complete_grid %>%
  left_join(df, by = c("Group1", "Group2"))

# **Step 2: LBd Reciprocal Values Properly**
df_reciprocal <- df_full %>%
  rename(Group1 = Group2, Group2 = Group1)  # Swap Group1 and Group2

# Ensure reciprocal pairs have the same Bonferroni values
df_reciprocal <- df_reciprocal %>%
  left_join(df_full %>% select(Group1, Group2, Bonferroni), by = c("Group1", "Group2")) %>%
  mutate(Bonferroni = coalesce(Bonferroni.x, Bonferroni.y)) %>%
  select(-Bonferroni.x, -Bonferroni.y)  # Keep only the updated Bonferroni values

df_full <- bind_rows(df_full, df_reciprocal) %>%
  distinct(Group1, Group2, .keep_all = TRUE)  # Ensure duplicates are kept

# Categorize Bonferroni values for coloring
df_full <- df_full %>%
  mutate(
    color = case_when(
      Bonferroni < 0.05 ~ "grey40",  # Use "grey40" explicitly
      Bonferroni >= 0.05 ~ "grey95",
      is.na(Bonferroni) ~ NA_character_
    )
  )

# Ensure levels preserve the correct group-wise order
df_full$Group1 <- factor(df_full$Group1, levels = all_labels)
df_full$Group2 <- factor(df_full$Group2, levels = rev(all_labels))  # Ensure full mirroring

# Extract Group1 and Group2 levels for correct coloring of axis labels
group1_levels <- levels(df_full$Group1)
group2_levels <- levels(df_full$Group2)

# Step 3: Plot checkerboard heatmap with FULL Reciprocal Values
library(ggplot2)
library(ggtext)
library(ggnewscale)

# Define color labels
color_labels <- c("p <0.05" = "grey40",
                  "p 0.05-0.1" = "grey70",
                  "p <0.1" = "grey95")

# Ensure missing values (NA) are properly handled
df_full$Bonferroni_cat <- cut(df_full$Bonferroni, 
                              breaks = c(-Inf, 0.05, 0.1, Inf), 
                              labels = names(color_labels))

# Convert NA values to explicit category for better visualization
df_full$Bonferroni_cat <- as.character(df_full$Bonferroni_cat)
df_full$Bonferroni_cat[is.na(df_full$Bonferroni)] <- "NA"

# LBd "NA" to the color scale so blank cells appear white
color_labels["NA"] <- "white"

# Create color mappings for text labels
group2_colors <- ifelse(grepl("ApoE3", df_full$Group2), "grey40", "black")
group1_colors <- ifelse(grepl("ApoE3", df_full$Group1), "grey40", "black")

# Create the heatmap plot
ggplot(df_full, aes(x = Group2, y = Group1, fill = Bonferroni_cat)) +
  geom_tile(color = "black") +
  geom_richtext(
    aes(label = ifelse(
      !is.na(Bonferroni),
      ifelse(Bonferroni < 0.01, "**<0.01**",  # Replace very low p-values with "<0.001"
             ifelse(Bonferroni < 0.05, paste0("**", sprintf("%.2f", Bonferroni), "**"), 
                    sprintf("%.2f", Bonferroni))),
      "")),  
    size = 8, fill = NA, label.color = NA,  
    color = ifelse(!is.na(df_full$Bonferroni) & df_full$Bonferroni < 0.05, "white", "black")  
  ) +
  scale_fill_manual(
    name = "Bonferroni P-Value",
    values = c(
      "p <0.05" = "grey40",
      "p 0.05-0.1" = "grey70",
      "p <0.1" = "grey95"
    ),
    breaks = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    labels = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    na.value = "white"  # Keeps blank cells white
  ) +
  theme(
    axis.text.x = element_markdown(angle = 90, hjust = 1, size = 14),  
    axis.text.y = element_markdown(angle = 0, hjust = 1, size = 14),
    panel.grid = element_blank()
  ) +
  labs(x = "", y = "", fill = "Bonferroni P-Value") +
  ggtitle("") +  
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +  
  coord_fixed()


Bonferroni_Heatmap_plot <-ggplot(df_full, aes(x = Group2, y = Group1, fill = Bonferroni_cat)) +
  geom_tile(color = "black") +
  geom_richtext(
    aes(label = ifelse(
      !is.na(Bonferroni),
      ifelse(Bonferroni < 0.01, "**<0.01**",  # Replace very low p-values with "<0.001"
             ifelse(Bonferroni < 0.05, paste0("**", sprintf("%.2f", Bonferroni), "**"), 
                    sprintf("%.2f", Bonferroni))),
      "")),  
    size = 8, fill = NA, label.color = NA,  
    color = ifelse(!is.na(df_full$Bonferroni) & df_full$Bonferroni < 0.05, "white", "black")  
  ) +
  scale_fill_manual(
    name = "Bonferroni P-Value",
    values = c(
      "p <0.05" = "grey40",
      "p 0.05-0.1" = "grey70",
      "p <0.1" = "grey95"
    ),
    breaks = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    labels = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    na.value = "white"  # Keeps blank cells white
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_markdown(angle = 90, hjust = 1, size = 14),  
    axis.text.y = element_markdown(angle = 0, hjust = 1, size = 14),
    panel.grid = element_blank()
  ) +
  labs(x = "", y = "", fill = "Bonferroni P-Value") +
  ggtitle("") +  
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +  
  coord_fixed()

# Step 5: Save the plot as a PNG
ggsave("Bonferroni_Heatmap_plot_ABETA_Thal.png", 
       plot = Bonferroni_Heatmap_plot, width = 10, height = 8, dpi = 300)

Bonferroni_Heatmap_plot_ABETA_Thal <- Bonferroni_Heatmap_plot

#############CHECKERBOARD_PLOT###################################
setwd(datadir)

final_results_ABETA_ABC <- final_results_ABETA_ABC %>%
  filter(Group1 != "Global" & Group2 != "All Groups") %>%
  mutate(
    Group = "3_ABC",
    Group1 = paste0(Group1, "_ABC"),
    Group2 = paste0(Group2, "_ABC")
  )
final_results_Thal <- final_results_Thal %>%
  filter(Group1 != "Global" & Group2 != "All Groups") %>%
  mutate(
    Group = "2_Thal",
    Group1 = paste0(Group1, "_Thal"),
    Group2 = paste0(Group2, "_Thal")
  )

final_results_ABETA_UPR75 <- final_results_ABETA_UPR75 %>%
  filter(Group1 != "Global" & Group2 != "All Groups") %>%
  mutate(
    Group = "1_UPR75",
    Group1 = paste0(Group1, "_ABETA_75%Quartile"),
    Group2 = paste0(Group2, "_ABETA_75%Quartile")
  )

combined_all_results <- bind_rows(final_results_ABETA_ABC, final_results_Thal, final_results_ABETA_UPR75)

# Print the updated combined dataframe
print(combined_all_results)

# Save the final dataframe as a CSV file
write.csv(combined_all_results, "combined_all_results_UPR75.csv", row.names = FALSE)
# Step 1: Load the dataset
df <- combined_all_results %>%
  as.data.frame()
# Step 2: Duplicate all rows and swap `Group1` and `Group2` in the duplicated rows
df_reciprocal <- df %>%
  rename(Group1 = Group2, Group2 = Group1)  # Swap the values

# Step 3: Combine the original and reciprocal data
df_full <- bind_rows(df, df_reciprocal) %>%
  distinct(Group1, Group2, .keep_all = TRUE)  # Ensure no duplicates are removed

# Step 4: Display the final dataset
print(df_full)

df<- df_full

# Order variables within each "Group"
df <- df %>%
  group_by(Group) %>%
  arrange(Group, Group1, Group2, .by_group = TRUE)

# Get unique labels maintaining order within each "Group"
all_labels <- df %>%
  select(Group, Group1, Group2) %>%
  pivot_longer(cols = c(Group1, Group2), values_to = "Label") %>%
  distinct(Group, Label) %>%
  arrange(Group) %>%
  pull(Label)

# Create full grid while keeping group-wise structure
complete_grid <- expand.grid(Group1 = all_labels, Group2 = all_labels, stringsAsFactors = FALSE)

# Merge with actual data while preserving "Group" order
df_full <- complete_grid %>%
  left_join(df, by = c("Group1", "Group2"))

# **Step 2: Add Reciprocal Values Properly**
df_reciprocal <- df_full %>%
  rename(Group1 = Group2, Group2 = Group1)  # Swap Group1 and Group2

# Ensure reciprocal pairs have the same Bonferroni values
df_reciprocal <- df_reciprocal %>%
  left_join(df_full %>% select(Group1, Group2, Bonferroni), by = c("Group1", "Group2")) %>%
  mutate(Bonferroni = coalesce(Bonferroni.x, Bonferroni.y)) %>%
  select(-Bonferroni.x, -Bonferroni.y)  # Keep only the updated Bonferroni values

df_full <- bind_rows(df_full, df_reciprocal) %>%
  distinct(Group1, Group2, .keep_all = TRUE)  # Ensure duplicates are kept

# Categorize Bonferroni values for coloring
df_full <- df_full %>%
  mutate(
    color = case_when(
      Bonferroni < 0.05 ~ "dodgerblue",  # Use "dodgerblue" explicitly
      Bonferroni >= 0.05 ~ "grey",
      is.na(Bonferroni) ~ NA_character_
    )
  )

# Ensure levels preserve the correct group-wise order
df_full$Group1 <- factor(df_full$Group1, levels = all_labels)
df_full$Group2 <- factor(df_full$Group2, levels = rev(all_labels))  # Ensure full mirroring

# Extract Group1 and Group2 levels for correct coloring of axis labels
group1_levels <- levels(df_full$Group1)
group2_levels <- levels(df_full$Group2)

# Step 3: Plot checkerboard heatmap with FULL Reciprocal Values
library(ggplot2)
library(ggtext)
library(ggnewscale)

# Define color labels
color_labels <- c("p <0.05" = "dodgerblue",
                  "p 0.05-0.1" = "lightskyblue3",
                  "p <0.1" = "grey")

# Ensure missing values (NA) are properly handled
df_full$Bonferroni_cat <- cut(df_full$Bonferroni, 
                              breaks = c(-Inf, 0.05, 0.1, Inf), 
                              labels = names(color_labels))

# Convert NA values to explicit category for better visualization
df_full$Bonferroni_cat <- as.character(df_full$Bonferroni_cat)
df_full$Bonferroni_cat[is.na(df_full$Bonferroni)] <- "NA"

# Add "NA" to the color scale so blank cells appear white
color_labels["NA"] <- "white"

# Create color mappings for text labels
group2_colors <- ifelse(grepl("ApoE3", df_full$Group2), "dodgerblue", "firebrick")
group1_colors <- ifelse(grepl("ApoE3", df_full$Group1), "dodgerblue", "firebrick")

# Create the heatmap plot
ggplot(df_full, aes(x = Group2, y = Group1, fill = Bonferroni_cat)) +
  geom_tile(color = "black") +
  geom_richtext(
    aes(label = ifelse(
      !is.na(Bonferroni),
      ifelse(Bonferroni < 0.01, "**<0.01**",  # Replace very low p-values with "<0.001"
             ifelse(Bonferroni < 0.05, paste0("**", sprintf("%.2f", Bonferroni), "**"), 
                    sprintf("%.2f", Bonferroni))),
      "")),  
    size = 8, fill = NA, label.color = NA,  
    color = ifelse(!is.na(df_full$Bonferroni) & df_full$Bonferroni < 0.05, "white", "black")  
  ) +
  scale_fill_manual(
    name = "Bonferroni P-Value",
    values = c(
      "p <0.05" = "dodgerblue",
      "p 0.05-0.1" = "lightskyblue3",
      "p <0.1" = "grey"
    ),
    breaks = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    labels = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    na.value = "white"  # Keeps blank cells white
  ) +
  scale_x_discrete(labels = function(x) { 
    ifelse(grepl("ApoE3", x), paste0("<span style='color:dodgerblue;'>", x, "</span>"),
           paste0("<span style='color:firebrick;'>", x, "</span>")) 
  }) +
  scale_y_discrete(labels = function(x) { 
    ifelse(grepl("ApoE3", x), paste0("<span style='color:dodgerblue;'>", x, "</span>"),
           paste0("<span style='color:firebrick;'>", x, "</span>")) 
  }) +
  theme_minimal() +
  theme(
    axis.text.x = element_markdown(angle = 45, hjust = 1, size = 14),  
    axis.text.y = element_markdown(angle = 0, hjust = 1, size = 14),
    panel.grid = element_blank()
  ) +
  labs(x = "Group2", y = "Group1", fill = "Bonferroni P-Value") +
  ggtitle("") +  
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +  
  coord_fixed()


Bonferroni_Heatmap_plot <-ggplot(df_full, aes(x = Group2, y = Group1, fill = Bonferroni_cat)) +
  geom_tile(color = "black") +
  geom_richtext(
    aes(label = ifelse(
      !is.na(Bonferroni),
      ifelse(Bonferroni < 0.01, "**<0.01**",  # Replace very low p-values with "<0.001"
             ifelse(Bonferroni < 0.05, paste0("**", sprintf("%.2f", Bonferroni), "**"), 
                    sprintf("%.2f", Bonferroni))),
      "")),  
    size = 8, fill = NA, label.color = NA,  
    color = ifelse(!is.na(df_full$Bonferroni) & df_full$Bonferroni < 0.05, "white", "black")  
  ) +
  scale_fill_manual(
    name = "Bonferroni P-Value",
    values = c(
      "p <0.05" = "dodgerblue",
      "p 0.05-0.1" = "lightskyblue3",
      "p <0.1" = "grey"
    ),
    breaks = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    labels = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    na.value = "white"  # Keeps blank cells white
  ) +
  scale_x_discrete(labels = function(x) { 
    ifelse(grepl("ApoE3", x), paste0("<span style='color:dodgerblue;'>", x, "</span>"),
           paste0("<span style='color:firebrick;'>", x, "</span>")) 
  }) +
  scale_y_discrete(labels = function(x) { 
    ifelse(grepl("ApoE3", x), paste0("<span style='color:dodgerblue;'>", x, "</span>"),
           paste0("<span style='color:firebrick;'>", x, "</span>")) 
  }) +
  theme_minimal() +
  theme(
    axis.text.x = element_markdown(angle = 90, hjust = 1, size = 14),  
    axis.text.y = element_markdown(angle = 0, hjust = 1, size = 14),
    panel.grid = element_blank()
  ) +
  labs(x = "Group2", y = "Group1", fill = "Bonferroni P-Value") +
  ggtitle("") +  
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +  
  coord_fixed()

# Step 5: Save the plot as a PNG
ggsave("Bonferroni_Heatmap_plot_ABETA_UPR75_UPR75.png", 
       plot = Bonferroni_Heatmap_plot, width = 16, height = 14, dpi = 300)


# LOAD DIRECTORIES AT8##############
excel_path <- #"/LOAD.xlsx"
  setwd(#/FILE LOCATION")
#UPR85####################################################
#############IMPUTATION_AT8_NEW################
# Load dataset
excel <- read_excel("/Users/WORK_2023/Jaunmuktane_Lab/DIGIPATH/ASAP DATA/PATHWAYS_TO_DEMENTIA_PAPER/PATHWAYS_TO_DEMENTIA_SUMMARY.xlsx", 
                    sheet = "300_R_BLOCK") %>%
  as.data.frame()

excel <- excel %>%
  # Set AT8_HIPPO to NA for specific cases before further modifications
  mutate(
    AT8_HIPPO = ifelse(`CASE. NO.` %in% c("P7/02", "P47/00"), NA_real_, AT8_HIPPO)
  ) %>%
  # Impute AT8_HIPPO based on Braak_Braak criteria
  mutate(
    AT8_HIPPO = case_when(
      is.na(AT8_HIPPO) & !is.na(Braak_Braak) & Braak_Braak < 2 ~ 0.00001, # Impute if Braak_Braak < 2
      TRUE ~ AT8_HIPPO  # Keep existing values
    )
  ) %>%
  # Impute AT8_FRNT_CTX if it is NA and AT8_HIPPO has a value
  mutate(
    AT8_FRNT_CTX = case_when(
      is.na(AT8_FRNT_CTX) & !is.na(AT8_HIPPO) ~ 0.00001,  # Impute if missing and AT8_HIPPO exists
      TRUE ~ AT8_FRNT_CTX  # Keep existing values
    )
  ) %>%
  
  # Compute AT8_COMB_FH, but set to NA if AD_LEVEL is missing
  mutate(
    AT8_COMB_FH = ifelse(is.na(AD_LEVEL), NA_real_, (AT8_FRNT_CTX + AT8_HIPPO) / 2)
  ) %>%
  
  # If AT8_COMB_FH is NA, make AD_LEVEL and Braak_Braak_BIN also NA
  mutate(
    AD_LEVEL = ifelse(is.na(AT8_COMB_FH), NA, AD_LEVEL),
    Braak_Braak__NEWBIN = ifelse(is.na(AT8_COMB_FH), NA, Braak_Braak_BIN)
  )
#############UPR85_STAGING##################################
# Ensure ApoE is a factor and remove NA levels from facets
excel_graph <- excel %>%
  filter(!is.na(ApoE))
# Convert DEMENTIA to a factor for better visualization
excel_graph$DEMENTIA <- as.factor(excel_graph$DEMENTIA)

# Define custom colors for ApoE3 and ApoE4 groups
custom_colors <- c("ApoE3" = "#339999", "ApoE4" = "#FFDD57")  # Light/Dark blue for ApoE3

# Create the plot
ggplot(excel_graph, aes(x = AT8_FRNT_HIPP, fill = ApoE, color = ApoE)) +
  geom_density(aes(y = ..density.. * 100), alpha = 0.4, size = 1) +  # Convert density to percentage & adjust transparency
  scale_fill_manual(values = c("ApoE3" = "#339999", "ApoE4" = "#FFDD57")) +  # Set fill colors
  scale_color_manual(values = c("ApoE3" = "black", "ApoE4" = "black")) +  # Set outline colors
  labs(
    title = "",
    x = "% pTau",
    y = "% cases",  # Updated y-axis label
    fill = "ApoE Group",
    color = "ApoE Group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    legend.key = element_rect(fill = NA),  # Keep legend keys clean
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30),
    axis.text.y = element_text(angle = 0, hjust = 0.5, size = 30), # Rotate and adjust size
    axis.text.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.title.x = element_text(size = 30),
    plot.title = element_blank()
  )


ggsave("AT8_FRNT_HIPP_Distribution_ApoE.png", width = 8, height = 6, dpi = 300)


# KS test comparing ASYN_FRNT_HIPP between ApoE3 and ApoE4
library(writexl)

data_apoE3 <- excel_graph %>% filter(ApoE == "ApoE3") %>% pull(AT8_FRNT_HIPP)
data_apoE4 <- excel_graph %>% filter(ApoE == "ApoE4") %>% pull(AT8_FRNT_HIPP)

ks_result <- ks.test(data_apoE3, data_apoE4)

# Convert to data frame for export
ks_df <- data.frame(
  statistic = ks_result$statistic,
  p_value = ks_result$p.value,
  alternative = ks_result$alternative,
  method = ks_result$method
)

# Write to Excel
write_xlsx(ks_df, "KS_Test_AT8_FRNT_HIPP_ApoE.xlsx")


# Calculate the percentile cutoff (median of AT8_COMB_FH)
upper_quartile <- quantile(excel$AT8_COMB_FH, probs = 0.85, na.rm = TRUE)

# Define AD category based on 75th percentile
excel_UPR85 <- excel %>%
  mutate(AD_Category = case_when(
    AT8_COMB_FH >= upper_quartile ~ "AT8_HIGH",  # Strictly greater than
    AT8_COMB_FH < upper_quartile ~ "AT8_LOW"    # Less than or equal
  )) 

# Remove NA values
excel_UPR85_filter <- excel_UPR85 %>%
  filter(!is.na(AD_Category))

# Summarize data for `excel_filtered_UPR85`
summary_data_UPR85 <- excel_UPR85_filter %>%
  group_by(AD_Category, ApoE, DEMENTIA) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(
    Group = paste(ApoE, DEMENTIA, sep = "_"),
    ApoE_Group = paste(ApoE, AD_Category, sep = "_")
  ) %>%
  distinct()  # Ensure unique groups

# Summarize data for dementia proportions
summary_prop_data_UPR85 <- excel_UPR85_filter %>%
  group_by(ApoE, AD_Category, DEMENTIA) %>%
  summarise(Count = n(), .groups = "drop") %>%
  spread(key = DEMENTIA, value = Count, fill = 0) %>%
  mutate(
    Proportion_Dementia_1 = DEM_1 / (DEM_0 + DEM_1),
    ApoE_Group = paste(ApoE, AD_Category, sep = "_")
  )

# Remove NA groups for ApoE
summary_prop_data_UPR85 <- summary_prop_data_UPR85 %>%
  filter(!grepl("^NA_|ApoE3_NA|ApoE4_NA", ApoE_Group))  # Remove all NA groups
#SUMMARY Proportions ###################

write.csv(summary_prop_data_UPR85, "summary_prop_data_AT8_UPR85.csv", row.names = FALSE)

# Add Proportion of No Dementia Cases
summary_prop_data_UPR85 <- summary_prop_data_UPR85 %>%
  mutate(Proportion_No_Dementia = 1 - Proportion_Dementia_1)

# Convert data to long format for ggplot
summary_prop_data_UPR85_long <- summary_prop_data_UPR85 %>%
  tidyr::pivot_longer(cols = c(Proportion_Dementia_1, Proportion_No_Dementia),
                      names_to = "Dementia_Status",
                      values_to = "Proportion")

# Rename labels
summary_prop_data_UPR85_long$Dementia_Status <- factor(summary_prop_data_UPR85_long$Dementia_Status,
                                                       levels = c("Proportion_No_Dementia", "Proportion_Dementia_1"),
                                                       labels = c("No Dementia", "Dementia"))

summary_prop_data_UPR85_long <- summary_prop_data_UPR85_long %>%
  mutate(
    ApoE_Group = case_when(
      ApoE_Group == "ApoE3_AT8_HIGH" ~ "ApoE3: H",
      ApoE_Group == "ApoE3_AT8_LOW"  ~ "ApoE3: N/L",
      ApoE_Group == "ApoE4_AT8_HIGH" ~ "ApoE4: H",
      ApoE_Group == "ApoE4_AT8_LOW"  ~ "ApoE4: N/L",
      TRUE ~ ApoE_Group  # Keep any other values unchanged
    )
  )

summary_prop_data_UPR85_long$Dementia_Status <- factor(summary_prop_data_UPR85_long$Dementia_Status, levels = c("Dementia", "No Dementia"))
# Create stacked bar plot
summary_prop_data_UPR85_plot <- ggplot(summary_prop_data_UPR85_long, aes(x = ApoE_Group, y = Proportion, fill = Dementia_Status)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = ifelse(Proportion == 0, "", paste0(round(Proportion * 100, 0)))),
            position = position_stack(vjust = 0.5), 
            size = 9, color =ifelse(summary_prop_data_Braak_long$Dementia_Status== "Dementia", "black", "black"),fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "Proportion (%)", fill = "Dementia Status") +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  ) +
  scale_fill_manual(values = c("Dementia" = "#FF9966", "No Dementia" = "#6699CC")) +  # Custom bar colors
  scale_color_manual(values = c("Dementia" = "black", "No Dementia" = "black"), guide = "none") +  # Fix text colors & remove legend
  coord_flip()  # Flip the axes


# Save the plot
ggsave("summary_prop_data_AT8_UPR85.png", summary_prop_data_UPR85_plot, width = 8, height = 6, dpi = 300)

# Display the plot
print(summary_prop_data_UPR85_plot)



# Create proportion table
proportion_table_UPR85 <- summary_prop_data_UPR85 %>%
  select(ApoE, AD_Category, Proportion_Dementia_1) %>%
  spread(key = AD_Category, value = Proportion_Dementia_1)

# Get unique ApoE Groups (after NA removal)
apoE_groups_UPR85 <- unique(summary_prop_data_UPR85$ApoE_Group)

# Contingency table for ApoE_Group and Proportion_Dementia_1
contingency_data_UPR85 <- summary_prop_data_UPR85 %>%
  group_by(ApoE_Group) %>%
  summarise(Proportion_Dementia_1 = mean(Proportion_Dementia_1))

# Print updated contingency table
print(contingency_data_UPR85)

# Construct global contingency table
global_contingency_UPR85 <- table(contingency_data_UPR85$ApoE_Group, contingency_data_UPR85$Proportion_Dementia_1)

# Perform global test (Fisher's Exact or Chi-Square)
global_test_UPR85 <- if (any(global_contingency_UPR85 < 5)) {
  fisher.test(global_contingency_UPR85)
} else {
  chisq.test(global_contingency_UPR85)
}

# Store global test result
all_results_UPR85 <- data.frame(
  Group1 = "Global",
  Group2 = "All Groups",
  Group1_N = sum(summary_prop_data_UPR85$DEM_0) + sum(summary_prop_data_UPR85$DEM_1),
  Group2_N = "N/A",
  Method = ifelse(any(global_contingency_UPR85 < 5), "Fisher's Exact Test", "Chi-Square Test"),
  P.Value = global_test_UPR85$p.value
)

# Pairwise comparisons for ApoE Groups
for (i in seq_along(apoE_groups_UPR85)) {
  for (j in seq_along(apoE_groups_UPR85)) {
    if (i < j) {  # Avoid duplicate comparisons
      group1_data <- summary_prop_data_UPR85 %>% filter(ApoE_Group == apoE_groups_UPR85[i])
      group2_data <- summary_prop_data_UPR85 %>% filter(ApoE_Group == apoE_groups_UPR85[j])
      
      # Construct contingency table
      contingency_table <- matrix(
        c(sum(group1_data$DEM_1), sum(group1_data$DEM_0),
          sum(group2_data$DEM_1), sum(group2_data$DEM_0)),
        nrow = 2
      )
      
      # Perform appropriate test
      test <- if (any(contingency_table < 5)) {
        fisher.test(contingency_table)
      } else {
        chisq.test(contingency_table)
      }
      
      # Append results
      all_results_UPR85 <- rbind(
        all_results_UPR85,
        data.frame(
          Group1 = apoE_groups_UPR85[i],
          Group2 = apoE_groups_UPR85[j],
          Group1_N = sum(group1_data$DEM_1 + group1_data$DEM_0),
          Group2_N = sum(group2_data$DEM_1 + group2_data$DEM_0),
          P.Value = test$p.value,
          Method = ifelse(any(contingency_table < 5), "Fisher's Exact Test", "Chi-Square Test")
        )
      )
    }
  }
}

# Separate global test from pairwise comparisons
global_result_UPR85 <- all_results_UPR85 %>% filter(Group1 == "Global")
pairwise_results_UPR85 <- all_results_UPR85 %>% filter(Group1 != "Global")

# Apply multiple testing corrections **only to pairwise comparisons**
pairwise_results_UPR85 <- pairwise_results_UPR85 %>%
  mutate(
    Bonferroni = p.adjust(P.Value, method = "bonferroni"),
    Holm = p.adjust(P.Value, method = "holm"),
    FDR = p.adjust(P.Value, method = "fdr")
  )

# Combine back with the global test result (without corrections)
all_results_UPR85 <- bind_rows(global_result_UPR85, pairwise_results_UPR85)

# Print final results
print(all_results_UPR85)

# Save results
write.csv(all_results_UPR85, "DEMENTIA_UPR85_AT8_Test_Results.csv", row.names = FALSE)
write.table(all_results_UPR85, "DEMENTIA_UPR85_AT8_Test_Results.txt", row.names = FALSE, sep = "\t", quote = FALSE)

# Print final cleaned results
final_results_AT8_UPR85 <- all_results_UPR85 %>%
  select(Group1, Group2, Group1_N, Group2_N, P.Value, Bonferroni, Holm, FDR)

print(final_results_AT8_UPR85)









#############ABC_STAGING##################################
# Set AD_LEVEL to NA if AT8_COMB_FH is NA
excel <- excel %>%
  mutate(
    AD_LEVEL = ifelse(is.na(AT8_COMB_FH), NA, AD_LEVEL)
  )
# Define AD category
excel_ABC <- excel %>%
  mutate(AD_Category = case_when(
    AD_LEVEL == "AD_LOW" ~ "AT8_LOW",
    AD_LEVEL == "AD_HIGH" ~ "AT8_HIGH",
    TRUE ~ NA_character_
  )) 


excel_ABC_filter <- excel_ABC %>% filter(!is.na(AD_Category))  # Remove NA values

# Summarize data for `excel_filtered_Braak_Braak`
summary_data_ABC <- excel_ABC_filter %>%
  group_by(AD_Category, ApoE, DEMENTIA) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Group = paste(ApoE, DEMENTIA, sep = "_"),
         ApoE_Group = paste(ApoE, AD_Category, sep = "_"))

# Summarize data for dementia proportions
summary_prop_data_ABC <- excel_ABC_filter %>%
  group_by(ApoE, AD_Category, DEMENTIA) %>%
  summarise(Count = n(), .groups = "drop") %>%
  spread(key = DEMENTIA, value = Count, fill = 0) %>%
  mutate(Proportion_Dementia_1 = DEM_1 / (DEM_0 + DEM_1),
         ApoE_Group = paste(ApoE, AD_Category, sep = "_"))

summary_prop_data_ABC <- summary_prop_data_ABC %>%
  filter(!grepl("ApoE3_NA|ApoE4_NA", ApoE_Group))  # Exclude specific NA groups

# Create proportion table
proportion_table_ABC <- summary_prop_data_ABC %>%
  select(ApoE, AD_Category, Proportion_Dementia_1) %>%
  spread(key = AD_Category, value = Proportion_Dementia_1)

# Remove NA groups for ApoE
summary_prop_data_ABC <- summary_prop_data_ABC %>%
  filter(!grepl("^NA_", ApoE_Group))



#SUMMARY Proportions ###################
write.csv(summary_prop_data_ABC, "summary_prop_data_AT8_ABC.csv", row.names = FALSE)

# Add Proportion of No Dementia Cases
summary_prop_data_ABC <- summary_prop_data_ABC %>%
  mutate(Proportion_No_Dementia = 1 - Proportion_Dementia_1)

# Convert data to long format for ggplot
summary_prop_data_ABC_long <- summary_prop_data_ABC %>%
  tidyr::pivot_longer(cols = c(Proportion_Dementia_1, Proportion_No_Dementia),
                      names_to = "Dementia_Status",
                      values_to = "Proportion")

# Rename labels
summary_prop_data_ABC_long$Dementia_Status <- factor(summary_prop_data_ABC_long$Dementia_Status,
                                                     levels = c("Proportion_No_Dementia", "Proportion_Dementia_1"),
                                                     labels = c("No Dementia", "Dementia"))


summary_prop_data_ABC_long <- summary_prop_data_ABC_long %>%
  mutate(
    ApoE_Group = case_when(
      ApoE_Group == "ApoE3_AT8_HIGH" ~ "ApoE3: H",
      ApoE_Group == "ApoE3_AT8_LOW"  ~ "ApoE3: N/L",
      ApoE_Group == "ApoE4_AT8_HIGH" ~ "ApoE4: H",
      ApoE_Group == "ApoE4_AT8_LOW"  ~ "ApoE4: N/L",
      TRUE ~ ApoE_Group  # Keep any other values unchanged
    )
  )
summary_prop_data_ABC_long$Dementia_Status <- factor(summary_prop_data_ABC_long$Dementia_Status, levels = c("Dementia", "No Dementia"))
# Create stacked bar plot
summary_prop_data_ABC_plot <- ggplot(summary_prop_data_ABC_long, aes(x = ApoE_Group, y = Proportion, fill = Dementia_Status)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = ifelse(Proportion == 0, "", paste0(round(Proportion * 100, 0)))),
            position = position_stack(vjust = 0.5), 
            size = 9, color =ifelse(summary_prop_data_Braak_long$Dementia_Status== "Dementia", "black", "black"),fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "Proportion (%)", fill = "Dementia Status") +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  ) +
  scale_fill_manual(values = c("Dementia" = "#FF9966", "No Dementia" = "#6699CC")) +  # Custom bar colors
  scale_color_manual(values = c("Dementia" = "black", "No Dementia" = "black"), guide = "none") +  # Fix text colors & remove legend
  coord_flip()  # Flip the axes


# Save the plot
ggsave("summary_prop_data_AT8_ABC.png", summary_prop_data_ABC_plot, width = 8, height = 6, dpi = 300)
# Display the plot
print(summary_prop_data_ABC_plot)













# Get unique ApoE Groups (after NA removal)
apoE_groups_ABC <- unique(summary_prop_data_ABC$ApoE_Group)

# Contingency table for ApoE_Group and Proportion_Dementia_1
contingency_data_ABC <- summary_prop_data_ABC %>%
  group_by(ApoE_Group) %>%
  summarise(Proportion_Dementia_1 = mean(Proportion_Dementia_1))

# Print updated contingency table
print(contingency_data_ABC)

# Construct global contingency table
global_contingency_ABC <- table(contingency_data_ABC$ApoE_Group, contingency_data_ABC$Proportion_Dementia_1)

# Perform global test (Fisher's Exact or Chi-Square)
global_test_ABC <- if (any(global_contingency_ABC < 5)) {
  fisher.test(global_contingency_ABC)
} else {
  chisq.test(global_contingency_ABC)
}

# Store global test result
all_results_ABC <- data.frame(
  Group1 = "Global",
  Group2 = "All Groups",
  Group1_N = sum(summary_prop_data_ABC$DEM_0) + sum(summary_prop_data_ABC$DEM_1),
  Group2_N = "N/A",
  Method = ifelse(any(global_contingency_ABC < 5), "Fisher's Exact Test", "Chi-Square Test"),
  P.Value = global_test_ABC$p.value
)

# Pairwise comparisons for ApoE Groups
for (i in seq_along(apoE_groups_ABC)) {
  for (j in seq_along(apoE_groups_ABC)) {
    if (i < j) {  # Avoid duplicate comparisons
      group1_data <- summary_prop_data_ABC %>% filter(ApoE_Group == apoE_groups_ABC[i])
      group2_data <- summary_prop_data_ABC %>% filter(ApoE_Group == apoE_groups_ABC[j])
      
      # Construct contingency table
      contingency_table <- matrix(
        c(sum(group1_data$DEM_1), sum(group1_data$DEM_0),
          sum(group2_data$DEM_1), sum(group2_data$DEM_0)),
        nrow = 2
      )
      
      # Perform appropriate test
      test <- if (any(contingency_table < 5)) {
        fisher.test(contingency_table)
      } else {
        chisq.test(contingency_table)
      }
      
      # Append results
      all_results_ABC <- rbind(
        all_results_ABC,
        data.frame(
          Group1 = apoE_groups_ABC[i],
          Group2 = apoE_groups_ABC[j],
          Group1_N = sum(group1_data$DEM_1 + group1_data$DEM_0),
          Group2_N = sum(group2_data$DEM_1 + group2_data$DEM_0),
          P.Value = test$p.value,
          Method = ifelse(any(contingency_table < 5), "Fisher's Exact Test", "Chi-Square Test")
        )
      )
    }
  }
}

# Separate global test from pairwise comparisons
global_result_ABC <- all_results_ABC %>% filter(Group1 == "Global")
pairwise_results_ABC <- all_results_ABC %>% filter(Group1 != "Global")

# Apply multiple testing corrections **only to pairwise comparisons**
pairwise_results_ABC <- pairwise_results_ABC %>%
  mutate(
    Bonferroni = p.adjust(P.Value, method = "bonferroni"),
    Holm = p.adjust(P.Value, method = "holm"),
    FDR = p.adjust(P.Value, method = "fdr")
  )

# Combine back with the global test result (without corrections)
all_results_ABC <- bind_rows(global_result_ABC, pairwise_results_ABC)

# Print final results
print(all_results_ABC)

# Save results
write.csv(all_results_ABC, "DEMENTIA_ABC_AT8_Test_Results.csv", row.names = FALSE)
write.table(all_results_ABC, "DEMENTIA_ABC_AT8_Test_Results.txt", row.names = FALSE, sep = "\t", quote = FALSE)

# Print final cleaned results
final_results_AT8_ABC <- all_results_ABC %>%
  select(Group1, Group2, Group1_N, Group2_N, P.Value, Bonferroni, Holm, FDR)

print(final_results_AT8_ABC)
#############Braak_Braak_STAGING##################################
# Define AD category
excel_Braak_Braak <- excel %>%
  mutate(AD_Category = case_when(
    Braak_Braak_BIN == "AT8_LOW" ~ "AT8_LOW",
    Braak_Braak_BIN == "AT8_HIGH" ~ "AT8_HIGH",
    TRUE ~ NA_character_
  )) 

excel_Braak_Braak_filter <- excel_Braak_Braak %>% filter(!is.na(AD_Category))  # Remove NA values

# Summarize data for `excel_filtered_Braak_Braak`
summary_data_Braak_Braak <- excel_Braak_Braak_filter %>%
  group_by(AD_Category, ApoE, DEMENTIA) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Group = paste(ApoE, DEMENTIA, sep = "_"),
         ApoE_Group = paste(ApoE, AD_Category, sep = "_"))

# Summarize data for dementia proportions
summary_prop_data_Braak_Braak <- excel_Braak_Braak_filter %>%
  group_by(ApoE, AD_Category, DEMENTIA) %>%
  summarise(Count = n(), .groups = "drop") %>%
  spread(key = DEMENTIA, value = Count, fill = 0) %>%
  mutate(Proportion_Dementia_1 = DEM_1 / (DEM_0 + DEM_1),
         ApoE_Group = paste(ApoE, AD_Category, sep = "_"))

summary_prop_data_Braak_Braak <- summary_prop_data_Braak_Braak %>%
  filter(!grepl("ApoE3_NA|ApoE4_NA", ApoE_Group))  # Exclude specific NA groups

# Create proportion table
proportion_table_Braak_Braak <- summary_prop_data_Braak_Braak %>%
  select(ApoE, AD_Category, Proportion_Dementia_1) %>%
  spread(key = AD_Category, value = Proportion_Dementia_1)

# Remove NA groups for ApoE
summary_prop_data_Braak_Braak <- summary_prop_data_Braak_Braak %>%
  filter(!grepl("^NA_", ApoE_Group))

#SUMMARY Proportions ###################
write.csv(summary_prop_data_Braak_Braak, "summary_prop_data_Braak_Braak.csv", row.names = FALSE)

# Add Proportion of No Dementia Cases
summary_prop_data_Braak_Braak <- summary_prop_data_Braak_Braak %>%
  mutate(Proportion_No_Dementia = 1 - Proportion_Dementia_1)

# Convert data to long format for ggplot
summary_prop_data_Braak_Braak_long <- summary_prop_data_Braak_Braak %>%
  tidyr::pivot_longer(cols = c(Proportion_Dementia_1, Proportion_No_Dementia),
                      names_to = "Dementia_Status",
                      values_to = "Proportion")

# Rename labels
summary_prop_data_Braak_Braak_long$Dementia_Status <- factor(summary_prop_data_Braak_Braak_long$Dementia_Status,
                                                             levels = c("Proportion_No_Dementia", "Proportion_Dementia_1"),
                                                             labels = c("No Dementia", "Dementia"))

summary_prop_data_Braak_Braak_long <- summary_prop_data_Braak_Braak_long %>%
  mutate(
    ApoE_Group = case_when(
      ApoE_Group == "ApoE3_AT8_HIGH" ~ "ApoE3: H",
      ApoE_Group == "ApoE3_AT8_LOW"  ~ "ApoE3: N/L",
      ApoE_Group == "ApoE4_AT8_HIGH" ~ "ApoE4: H",
      ApoE_Group == "ApoE4_AT8_LOW"  ~ "ApoE4: N/L",
      TRUE ~ ApoE_Group  # Keep any other values unchanged
    )
  )
summary_prop_data_Braak_Braak_long$Dementia_Status <- factor(summary_prop_data_Braak_Braak_long$Dementia_Status, levels = c("Dementia", "No Dementia"))

# Create stacked bar plot
summary_prop_data_Braak_Braak_plot <- ggplot(summary_prop_data_Braak_Braak_long, aes(x = ApoE_Group, y = Proportion, fill = Dementia_Status)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = ifelse(Proportion == 0, "", paste0(round(Proportion * 100, 0)))),
            position = position_stack(vjust = 0.5), 
            size = 9, color =ifelse(summary_prop_data_Braak_long$Dementia_Status== "Dementia", "black", "black"),fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "Proportion (%)", fill = "Dementia Status") +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  ) +
  scale_fill_manual(values = c("Dementia" = "#FF9966", "No Dementia" = "#6699CC")) +  # Custom bar colors
  scale_color_manual(values = c("Dementia" = "black", "No Dementia" = "black"), guide = "none") +  # Fix text colors & remove legend
  coord_flip()  # Flip the axes


# Save the plot
ggsave("summary_prop_data_Braak_Braak.png", summary_prop_data_Braak_Braak_plot, width = 8, height = 6, dpi = 300)
# Display the plot
print(summary_prop_data_Braak_Braak_plot)

# Get unique ApoE Groups (after NA removal)
apoE_groups_Braak_Braak <- unique(summary_prop_data_Braak_Braak$ApoE_Group)

# Contingency table for ApoE_Group and Proportion_Dementia_1
contingency_data_Braak_Braak <- summary_prop_data_Braak_Braak %>%
  group_by(ApoE_Group) %>%
  summarise(Proportion_Dementia_1 = mean(Proportion_Dementia_1))

# Print updated contingency table
print(contingency_data_Braak_Braak)

# Construct global contingency table
global_contingency_Braak_Braak <- table(contingency_data_Braak_Braak$ApoE_Group, contingency_data_Braak_Braak$Proportion_Dementia_1)

# Perform global test (Fisher's Exact or Chi-Square)
global_test_Braak_Braak <- if (any(global_contingency_Braak_Braak < 5)) {
  fisher.test(global_contingency_Braak_Braak)
} else {
  chisq.test(global_contingency_Braak_Braak)
}

# Store global test result
all_results_Braak_Braak <- data.frame(
  Group1 = "Global",
  Group2 = "All Groups",
  Group1_N = sum(summary_prop_data_Braak_Braak$DEM_0) + sum(summary_prop_data_Braak_Braak$DEM_1),
  Group2_N = "N/A",
  Method = ifelse(any(global_contingency_Braak_Braak < 5), "Fisher's Exact Test", "Chi-Square Test"),
  P.Value = global_test_Braak_Braak$p.value
)

# Pairwise comparisons for ApoE Groups
for (i in seq_along(apoE_groups_Braak_Braak)) {
  for (j in seq_along(apoE_groups_Braak_Braak)) {
    if (i < j) {  # Avoid duplicate comparisons
      group1_data <- summary_prop_data_Braak_Braak %>% filter(ApoE_Group == apoE_groups_Braak_Braak[i])
      group2_data <- summary_prop_data_Braak_Braak %>% filter(ApoE_Group == apoE_groups_Braak_Braak[j])
      
      # Construct contingency table
      contingency_table <- matrix(
        c(sum(group1_data$DEM_1), sum(group1_data$DEM_0),
          sum(group2_data$DEM_1), sum(group2_data$DEM_0)),
        nrow = 2
      )
      
      # Perform appropriate test
      test <- if (any(contingency_table < 5)) {
        fisher.test(contingency_table)
      } else {
        chisq.test(contingency_table)
      }
      
      # Append results
      all_results_Braak_Braak <- rbind(
        all_results_Braak_Braak,
        data.frame(
          Group1 = apoE_groups_Braak_Braak[i],
          Group2 = apoE_groups_Braak_Braak[j],
          Group1_N = sum(group1_data$DEM_1 + group1_data$DEM_0),
          Group2_N = sum(group2_data$DEM_1 + group2_data$DEM_0),
          P.Value = test$p.value,
          Method = ifelse(any(contingency_table < 5), "Fisher's Exact Test", "Chi-Square Test")
        )
      )
    }
  }
}

# Separate global test from pairwise comparisons
global_result_Braak_Braak <- all_results_Braak_Braak %>% filter(Group1 == "Global")
pairwise_results_Braak_Braak <- all_results_Braak_Braak %>% filter(Group1 != "Global")

# Apply multiple testing corrections **only to pairwise comparisons**
pairwise_results_Braak_Braak <- pairwise_results_Braak_Braak %>%
  mutate(
    Bonferroni = p.adjust(P.Value, method = "bonferroni"),
    Holm = p.adjust(P.Value, method = "holm"),
    FDR = p.adjust(P.Value, method = "fdr")
  )

# Combine back with the global test result (without corrections)
all_results_Braak_Braak <- bind_rows(global_result_Braak_Braak, pairwise_results_Braak_Braak)
# Print final results
print(all_results_Braak_Braak)

# Save results
write.csv(all_results_Braak_Braak, "DEMENTIA_Braak_Braak_AT8_Test_Results.csv", row.names = FALSE)
write.table(all_results_Braak_Braak, "DEMENTIA_Braak_Braak_AT8_Test_Results.txt", row.names = FALSE, sep = "\t", quote = FALSE)

# Print final cleaned results
final_results_Braak_Braak <- all_results_Braak_Braak %>%
  select(Group1, Group2, Group1_N, Group2_N, P.Value, Bonferroni, Holm, FDR)

print(final_results_Braak_Braak)


#############Combine_Cutoff ####################################

excel$ABC_Level <- excel_ABC$AD_Category
excel$Braak_Braak_Level <- excel_Braak_Braak$AD_Category
excel$AT8_Upperquartile<- excel_UPR85$AD_Category

excel_plot <- excel %>%
  filter(!is.na(ABC_Level))
excel_plot <- excel_plot %>%
  filter(!is.na(Braak_Braak_Level))
excel_plot <- excel_plot %>%
  filter(!is.na(AT8_Upperquartile))


#############LINE PLOT ####################################
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Convert grouping columns to factors
excel <- excel %>%
  mutate(
    ABC_Level = as.factor(ABC_Level),
    Braak_Braak_Level = as.factor(Braak_Braak_Level),
    AT8_Upperquartile = as.factor(AT8_Upperquartile)
  )

# Reshape data for long format (to plot multiple groups) and remove NA values
excel_long <- excel %>%
  pivot_longer(cols = c("ABC_Level", "Braak_Braak_Level", "AT8_Upperquartile"), 
               names_to = "AD_Group", values_to = "AD_Value") %>%
  drop_na(AD_Value)  # Remove rows where LB_Value is NA

# Compute mean and standard error for ASYN_FRNT_HIPP within each group
summary_data <- excel_long %>%
  group_by(AD_Group, AD_Value) %>%
  summarise(
    Mean_AT8 = mean(AT8_COMB_FH, na.rm = TRUE),
    SE_AT8 = sd(AT8_COMB_FH, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Ensure AD_Value is a factor with correct ordering
summary_data$AD_Value <- factor(summary_data$AD_Value, levels = c("AT8_LOW", "AT8_HIGH"))
# Ensure correct legend order by setting factor levels
summary_data <- summary_data %>%
  mutate(AD_Group = factor(AD_Group, 
                           levels = c("ABC_Level", "Braak_Braak_Level", "AT8_Upperquartile")))
# Plot the updated data
plot <- ggplot(summary_data, aes(x = AD_Value, y = Mean_AT8, group = AD_Group, color = AD_Group)) +
  geom_line(size = 1) +  # Line for mean AT8
  geom_point(size = 3) +  # Points for mean values
  geom_errorbar(aes(ymin = Mean_AT8 - SE_AT8, ymax = Mean_AT8 + SE_AT8), width = 0.05) +  # Error bars
  
  # Custom colors for AD_Group
  scale_color_manual(
    values = c("ABC_Level" = "red",
               "Braak_Braak_Level" = "blue",
               "AT8_Upperquartile" = "green3"),
    labels = c("ABC_Level" = "ABC moderate or high",
               "Braak_Braak_Level" = "Braak & Braak 4, 5 & 6",
               "AT8_Upperquartile" = "pTau in 85th percentile")
  ) +
  
  # Custom x-axis labels
  scale_x_discrete(
    labels = c("AT8_LOW" = "Absent\nor low",
               "AT8_HIGH" = "Present\nor high")
  ) +
  
  # Labels and title
  labs(
    x = "", 
    y = "% pTau",
    color = "AD Group"
  ) +
  
  # Minimal theme and text formatting
  theme_minimal() +
  theme(           
    axis.text.x = element_text(size = 30, color = "black", hjust = 0.5, vjust = 0.5, lineheight = 1),  # Larger and bolder x-axis text
    axis.text.y = element_text(size = 30, color = "black"),  # Larger and bolder y-axis text
    axis.title.x = element_text(size = 30, color = "black"),  # Larger x-axis title
    axis.title.y = element_text(size = 30, color = "black"),  # Larger y-axis title
    axis.line = element_line(size = 1.5),  # Make axes bolder
    axis.ticks = element_line(size = 1),  # Make axis ticks bolder
    legend.text = element_text(size = 16),  # Larger and bolder legend text
    legend.title = element_blank(),  # Larger and bolder legend title
    plot.title = element_blank(),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_blank()  # Remove border around the plot
  )

# Display plot
print(plot)
# Save the plot as a PNG
ggsave("AT8_COMB_FH_Line_Graph_UPR85.png", plot, width = 7, height = 6)
library(dunn.test)

# Kruskal-Wallis test across AD_Group + AD_Value
kw <- kruskal.test(AT8_COMB_FH ~ interaction(AD_Group, AD_Value), data = excel_long)

# Pairwise Dunn's test
excel_long$Group_Label <- paste(excel_long$AD_Group, excel_long$AD_Value)
dunn_res <- dunn.test(excel_long$AT8_COMB_FH, excel_long$Group_Label, method = "bonferroni")

# View results
print(kw)
print(dunn_res)

# Save results to CSV
dunn_table <- data.frame(
  Comparison = dunn_res$comparisons,
  Z = dunn_res$Z,
  P_Adjusted = dunn_res$P.adjusted
)

# Rename group and value labels in comparison strings
dunn_table$Comparison <- dunn_table$Comparison %>%
  str_replace_all("ABC_Level", "ABC moderate or high") %>%
  str_replace_all("Braak_Braak_Level", "Braak & Braak 4, 5 & 6") %>%
  str_replace_all("AT8_Upperquartile", "pTau in 85th percentile") %>%
  str_replace_all("AT8_LOW", "Absent/low") %>%
  str_replace_all("AT8_HIGH", "Present/high")

write.csv(dunn_table, "AT8_COMB_FH_Line_Graph_DunnTest_Results.csv", row.names = FALSE)



#############RATIO SUMMARY TABLE ####################################

# Load necessary libraries
library(dplyr)

# Summarize data for each combination of DEMENTIA and LB_Category
summary_table <- excel %>%
  pivot_longer(cols = all_of(c("ABC_Level", "Braak_Braak_Level", "AT8_Upperquartile")), names_to = "AD_Group", values_to = "AD_Category") %>%
  filter(!is.na(AD_Category)) %>%  # Remove NA values in LB_Category
  group_by(AD_Group, DEMENTIA, AD_Category) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = c(DEMENTIA, AD_Category),
    values_from = Count,  # Use the "Count" column for values
    names_glue = "{AD_Category}_{DEMENTIA}"
  ) %>%
  mutate(
    AT8_LOW_RATIO_DEM1_DEM0 = AT8_LOW_DEM_1 / AT8_LOW_DEM_0,
    AT8_HIGH_RATIO_DEM1_DEM0 = AT8_HIGH_DEM_1 / AT8_HIGH_DEM_0,
    Total_Count = rowSums(select(., starts_with("AT8_")), na.rm = TRUE)  # Sum of all count columns per row
  )

# Print the updated summary table
print(summary_table)
summary_table <- summary_table %>%
  mutate(AD_Group = factor(AD_Group, levels = c("ABC_Level", "Braak_Braak_Level", "AT8_Upperquartile"))) %>%
  arrange(AD_Group)

print(summary_table)
# Save as CSV
write.csv(summary_table, "DEMENTIA_AT8_Ratio_Table_UPR85.csv", row.names = FALSE)
write.table(summary_table, "DEMENTIA_AT8_Ratio_Table_UPR85.txt", row.names = FALSE, sep = "\t", quote = FALSE)


#############PATHOLOGY_TABLE##########################


# Use the existing dataset
summary_table <- excel %>%
  pivot_longer(cols = all_of(c("ABC_Level", "Braak_Braak_Level", "AT8_Upperquartile")), names_to = "AD_Group", values_to = "AD_Category") %>%
  filter(!is.na(AD_Category)) %>%  # Remove NA values in LB_Category
  group_by(AD_Group, DEMENTIA, AD_Category) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = c(DEMENTIA, AD_Category),
    values_from = Count,  # Use the "Count" column for values
    names_glue = "{AD_Category}_{DEMENTIA}"
  ) %>%
  mutate(
    Sensitivity = AT8_HIGH_DEM_1 / (AT8_HIGH_DEM_1 + AT8_LOW_DEM_1),
    Specificity = AT8_LOW_DEM_0 / (AT8_LOW_DEM_0 + AT8_HIGH_DEM_0),
    Odds_Ratio = (AT8_HIGH_DEM_1 / AT8_HIGH_DEM_0) / (AT8_LOW_DEM_1 / AT8_LOW_DEM_0),
    PPV = AT8_HIGH_DEM_1 / (AT8_HIGH_DEM_1 + AT8_HIGH_DEM_0),
    Pathology_Absence_Ratio = (AT8_LOW_DEM_1 / AT8_LOW_DEM_0),
    Pathology_Presence_Ratio = (AT8_HIGH_DEM_1 / AT8_HIGH_DEM_0),
    Total_Count = rowSums(select(., starts_with("AT8_")), na.rm = TRUE)  # Sum of all count columns per row
  )
summary_table <- summary_table %>%
  mutate(AD_Group = factor(AD_Group, levels = c("ABC_Level", "Braak_Braak_Level", "AT8_Upperquartile"))) %>%
  arrange(AD_Group)

print(summary_table)
# Save to CSV
write.csv(summary_table, "Pathology_Statistics_AT8_UPR85.csv", row.names = FALSE)
write.table(summary_table, "Pathology_Statistics_AT8_UPR85.txt", row.names = FALSE, sep = "\t", quote = FALSE)
# Print the final table
print(summary_table)

library(fmsb)

# Normalize values
summary_table <- summary_table %>% mutate(
  Odds_Ratio = Odds_Ratio / max(Odds_Ratio, na.rm = TRUE),
  Pathology_Absence_Ratio = Pathology_Absence_Ratio / max(Pathology_Absence_Ratio, na.rm = TRUE),
  Pathology_Presence_Ratio = Pathology_Presence_Ratio / max(Pathology_Presence_Ratio, na.rm = TRUE)
)
# Save to CSV
write.csv(summary_table, "Pathology_Statistics_Norm_UPR85.csv", row.names = FALSE)
write.table(summary_table, "Pathology_Statistics_Norm_UPR85.txt", row.names = FALSE, sep = "\t", quote = FALSE)
# Print the final table
print(summary_table)
#############RADAR_PLOT#######################
# Define max and min values
max_vals <- rep(1, ncol(summary_table) - 1)
min_vals <- rep(0, ncol(summary_table) - 1)

# Combine min/max rows with the actual data
data <- rbind(max_vals, min_vals, summary_table[,-1])

data <- as.data.frame(data)

# Reorder columns for proper radar chart visualization
data_rotated <- data[, c("Specificity", "Pathology_Absence_Ratio", "Pathology_Presence_Ratio",
                         "Odds_Ratio", "PPV", "Sensitivity")]

colnames(data_rotated) <- c("Specificity", 
                            "PAR",
                            "PPR",
                            "OR", 
                            "PPV", 
                            "Sensitivity")

# Plot radar chart
# Adjust margins for a better layout
par(mar = c(1, 1, 1, 10) + 0.1)  
# Set font to bold before plotting
par(font = 1)  # 2 = Bold
# Plot radar chart
radarchart(data_rotated,
           seg = 5,                     
           axistype = 1,                
           pcol = c("red",  "blue", "green"),  
           pfcol = c(rgb(1, 0, 0, 0.3),  
                     rgb(0, 0, 1, 0.3),  
                     rgb(0, 1, 0, 0.3)),  
           cglcol = "grey",             
           cglty = 1,                   
           axislabcol = "grey",         
           caxislabels = seq(0, 1, 0.2), 
           vlcex = 3,  # Increase text size of variable labels
           cex.axis = 2,  # Increase size of axis labels
           cex.lab = 2  # (If applicable) Increase size of axis labels
)

# Adding a properly formatted legend
legend("right",
       inset = c(-0.25, 0),    # Moves legend further right
       legend = c("ABC moderate or high",
                  "Braak_Braak 4, 5 & 6",
                  "pTau pathology in 85th percentile"),
       bty = "n", pch = 20,
       col = c("red", "blue", "green"),
       text.col = "black",    cex = 1.2,   xpd = TRUE)  # Ensure text is visible outside

# Save radar chart as PNG
png("Radar_Chart_AT8_UPR85_2.png", width = 3700, height = 2000, res=300,  bg = "transparent")

# Increase right margin to make space for legend
par(mar = c(1, 1, 1, 10) + 0.1) 
# Set font to bold before plotting
par(font = 1)  # 2 = Bold
# Plot radar chart
radarchart(data_rotated,
           seg = 5,                     
           axistype = 1,                
           pcol = c("red", "blue", "green"),  
           pfcol = c(rgb(1, 0, 0, 0.3),  
                     rgb(0, 0, 1, 0.3),  
                     rgb(0, 1, 0, 0.3)),  
           plwd = 4,                    
           cglcol = "grey",             
           cglty = 1,                   
           axislabcol = "grey",         
           caxislabels = seq(0, 1, 0.2), 
           vlcex = 3,  # Increase text size of variable labels
           cex.axis = 2,  # Increase size of axis labels
           cex.lab = 2  # (If applicable) Increase size of axis labels
)

# Adding a properly formatted legend
legend("right",
       inset = c(-0.2, 0),    # Moves legend further right
       legend = c("ABC moderate or high",
                  "Braak_Braak 4, 5 & 6",
                  "pTau pathology in 85th percentile"),
       bty = "n", pch = 20,
       col = c("red", "blue", "green"),
       text.col = "black",    cex = 1.2,   xpd = TRUE)  # Ensure text is visible outside

dev.off()
# Set font to bold before plotting
par(font = 1) 

##########################CHECKERBOARD_PLOT_AT8_UPR85###################################
setwd(datadir)

final_results_AT8_UPR85 <- final_results_AT8_UPR85 %>%
  filter(Group1 != "Global" & Group2 != "All Groups") %>%
  mutate(
    Group = "GROUP",
    Group1 = case_when(
      Group1 == "ApoE3_AT8_HIGH" ~ "ApoE3: H",
      Group1 == "ApoE3_AT8_LOW"  ~ "ApoE3: N/L",
      Group1 == "ApoE4_AT8_HIGH" ~ "ApoE4: H",
      Group1 == "ApoE4_AT8_LOW"  ~ "ApoE4: N/L",
      TRUE ~ Group1  # Keep any other values unchanged
    ),
    Group2 = case_when(
      Group2 == "ApoE3_AT8_HIGH" ~ "ApoE3: H",
      Group2 == "ApoE3_AT8_LOW"  ~ "ApoE3: N/L",
      Group2 == "ApoE4_AT8_HIGH" ~ "ApoE4: H",
      Group2 == "ApoE4_AT8_LOW"  ~ "ApoE4: N/L",
      TRUE ~ Group2  # Keep any other values unchanged
    )
  )


combined_all_results <- bind_rows(final_results_AT8_UPR85,)


# Step 1: LoLB the dataset
df <- combined_all_results %>%
  as.data.frame()
# Step 2: Duplicate all rows and swap `Group1` and `Group2` in the duplicated rows
df_reciprocal <- df %>%
  rename(Group1 = Group2, Group2 = Group1)  # Swap the values

# Step 3: Combine the original and reciprocal data
df_full <- bind_rows(df, df_reciprocal) %>%
  distinct(Group1, Group2, .keep_all = TRUE)  # Ensure no duplicates are removed

# Step 4: Display the final dataset
print(df_full)

df<- df_full

# Order variables within each "Group"
df <- df %>%
  group_by(Group) %>%
  arrange(Group, Group1, Group2, .by_group = TRUE)

# Get unique labels maintaining order within each "Group"
all_labels <- df %>%
  select(Group, Group1, Group2) %>%
  pivot_longer(cols = c(Group1, Group2), values_to = "Label") %>%
  distinct(Group, Label) %>%
  arrange(Group) %>%
  pull(Label)

# Create full grid while keeping group-wise structure
complete_grid <- expand.grid(Group1 = all_labels, Group2 = all_labels, stringsAsFactors = FALSE)

# Merge with actual data while preserving "Group" order
df_full <- complete_grid %>%
  left_join(df, by = c("Group1", "Group2"))

# **Step 2: LBd Reciprocal Values Properly**
df_reciprocal <- df_full %>%
  rename(Group1 = Group2, Group2 = Group1)  # Swap Group1 and Group2

# Ensure reciprocal pairs have the same Bonferroni values
df_reciprocal <- df_reciprocal %>%
  left_join(df_full %>% select(Group1, Group2, Bonferroni), by = c("Group1", "Group2")) %>%
  mutate(Bonferroni = coalesce(Bonferroni.x, Bonferroni.y)) %>%
  select(-Bonferroni.x, -Bonferroni.y)  # Keep only the updated Bonferroni values

df_full <- bind_rows(df_full, df_reciprocal) %>%
  distinct(Group1, Group2, .keep_all = TRUE)  # Ensure duplicates are kept

# Categorize Bonferroni values for coloring
df_full <- df_full %>%
  mutate(
    color = case_when(
      Bonferroni < 0.05 ~ "grey40",  # Use "grey40" explicitly
      Bonferroni >= 0.05 ~ "grey95",
      is.na(Bonferroni) ~ NA_character_
    )
  )

# Ensure levels preserve the correct group-wise order
df_full$Group1 <- factor(df_full$Group1, levels = all_labels)
df_full$Group2 <- factor(df_full$Group2, levels = rev(all_labels))  # Ensure full mirroring

# Extract Group1 and Group2 levels for correct coloring of axis labels
group1_levels <- levels(df_full$Group1)
group2_levels <- levels(df_full$Group2)

# Step 3: Plot checkerboard heatmap with FULL Reciprocal Values
library(ggplot2)
library(ggtext)
library(ggnewscale)

# Define color labels
color_labels <- c("p <0.05" = "grey40",
                  "p 0.05-0.1" = "grey70",
                  "p <0.1" = "grey95")

# Ensure missing values (NA) are properly handled
df_full$Bonferroni_cat <- cut(df_full$Bonferroni, 
                              breaks = c(-Inf, 0.05, 0.1, Inf), 
                              labels = names(color_labels))

# Convert NA values to explicit category for better visualization
df_full$Bonferroni_cat <- as.character(df_full$Bonferroni_cat)
df_full$Bonferroni_cat[is.na(df_full$Bonferroni)] <- "NA"

# LBd "NA" to the color scale so blank cells appear white
color_labels["NA"] <- "white"

# Create color mappings for text labels
group2_colors <- ifelse(grepl("ApoE3", df_full$Group2), "grey40", "black")
group1_colors <- ifelse(grepl("ApoE3", df_full$Group1), "grey40", "black")

# Create the heatmap plot
ggplot(df_full, aes(x = Group2, y = Group1, fill = Bonferroni_cat)) +
  geom_tile(color = "black") +
  geom_richtext(
    aes(label = ifelse(
      !is.na(Bonferroni),
      ifelse(Bonferroni < 0.01, "**<0.01**",  # Replace very low p-values with "<0.001"
             ifelse(Bonferroni < 0.05, paste0("**", sprintf("%.2f", Bonferroni), "**"), 
                    sprintf("%.2f", Bonferroni))),
      "")),  
    size = 8, fill = NA, label.color = NA,  
    color = ifelse(!is.na(df_full$Bonferroni) & df_full$Bonferroni < 0.05, "white", "black")  
  ) +
  scale_fill_manual(
    name = "Bonferroni P-Value",
    values = c(
      "p <0.05" = "grey40",
      "p 0.05-0.1" = "grey70",
      "p <0.1" = "grey95"
    ),
    breaks = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    labels = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    na.value = "white"  # Keeps blank cells white
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_markdown(angle = 90, hjust = 1, size = 14),  
    axis.text.y = element_markdown(angle = 0, hjust = 1, size = 14),
    panel.grid = element_blank()
  ) +
  labs(x = "", y = "", fill = "Bonferroni P-Value") +
  ggtitle("") +  
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +  
  coord_fixed()


Bonferroni_Heatmap_plot <-ggplot(df_full, aes(x = Group2, y = Group1, fill = Bonferroni_cat)) +
  geom_tile(color = "black") +
  geom_richtext(
    aes(label = ifelse(
      !is.na(Bonferroni),
      ifelse(Bonferroni < 0.01, "**<0.01**",  # Replace very low p-values with "<0.001"
             ifelse(Bonferroni < 0.05, paste0("**", sprintf("%.2f", Bonferroni), "**"), 
                    sprintf("%.2f", Bonferroni))),
      "")),  
    size = 8, fill = NA, label.color = NA,  
    color = ifelse(!is.na(df_full$Bonferroni) & df_full$Bonferroni < 0.05, "white", "black")  
  ) +
  scale_fill_manual(
    name = "Bonferroni P-Value",
    values = c(
      "p <0.05" = "grey40",
      "p 0.05-0.1" = "grey70",
      "p <0.1" = "grey95"
    ),
    breaks = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    labels = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    na.value = "white"  # Keeps blank cells white
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_markdown(angle = 90, hjust = 1, size = 14),  
    axis.text.y = element_markdown(angle = 0, hjust = 1, size = 14),
    panel.grid = element_blank()
  ) +
  labs(x = "", y = "", fill = "Bonferroni P-Value") +
  ggtitle("") +  
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +  
  coord_fixed()

# Step 5: Save the plot as a PNG
ggsave("Bonferroni_Heatmap_plot_AT8_UPR85.png", 
       plot = Bonferroni_Heatmap_plot, width = 10, height = 8, dpi = 300)
Bonferroni_Heatmap_plot_AT8_UPR85 <- Bonferroni_Heatmap_plot

##########################CHECKERBOARD_PLOT_Braak_Braak###################################
setwd(datadir)

final_results_Braak_Braak <- final_results_Braak_Braak %>%
  filter(Group1 != "Global" & Group2 != "All Groups") %>%
  mutate(
    Group = "GROUP",
    Group1 = case_when(
      Group1 == "ApoE3_AT8_HIGH" ~ "ApoE3: H",
      Group1 == "ApoE3_AT8_LOW"  ~ "ApoE3: N/L",
      Group1 == "ApoE4_AT8_HIGH" ~ "ApoE4: H",
      Group1 == "ApoE4_AT8_LOW"  ~ "ApoE4: N/L",
      TRUE ~ Group1  # Keep any other values unchanged
    ),
    Group2 = case_when(
      Group2 == "ApoE3_AT8_HIGH" ~ "ApoE3: H",
      Group2 == "ApoE3_AT8_LOW"  ~ "ApoE3: N/L",
      Group2 == "ApoE4_AT8_HIGH" ~ "ApoE4: H",
      Group2 == "ApoE4_AT8_LOW"  ~ "ApoE4: N/L",
      TRUE ~ Group2  # Keep any other values unchanged
    )
  )


combined_all_results <- bind_rows(final_results_Braak_Braak,)


# Step 1: LoLB the dataset
df <- combined_all_results %>%
  as.data.frame()
# Step 2: Duplicate all rows and swap `Group1` and `Group2` in the duplicated rows
df_reciprocal <- df %>%
  rename(Group1 = Group2, Group2 = Group1)  # Swap the values

# Step 3: Combine the original and reciprocal data
df_full <- bind_rows(df, df_reciprocal) %>%
  distinct(Group1, Group2, .keep_all = TRUE)  # Ensure no duplicates are removed

# Step 4: Display the final dataset
print(df_full)

df<- df_full

# Order variables within each "Group"
df <- df %>%
  group_by(Group) %>%
  arrange(Group, Group1, Group2, .by_group = TRUE)

# Get unique labels maintaining order within each "Group"
all_labels <- df %>%
  select(Group, Group1, Group2) %>%
  pivot_longer(cols = c(Group1, Group2), values_to = "Label") %>%
  distinct(Group, Label) %>%
  arrange(Group) %>%
  pull(Label)

# Create full grid while keeping group-wise structure
complete_grid <- expand.grid(Group1 = all_labels, Group2 = all_labels, stringsAsFactors = FALSE)

# Merge with actual data while preserving "Group" order
df_full <- complete_grid %>%
  left_join(df, by = c("Group1", "Group2"))

# **Step 2: LBd Reciprocal Values Properly**
df_reciprocal <- df_full %>%
  rename(Group1 = Group2, Group2 = Group1)  # Swap Group1 and Group2

# Ensure reciprocal pairs have the same Bonferroni values
df_reciprocal <- df_reciprocal %>%
  left_join(df_full %>% select(Group1, Group2, Bonferroni), by = c("Group1", "Group2")) %>%
  mutate(Bonferroni = coalesce(Bonferroni.x, Bonferroni.y)) %>%
  select(-Bonferroni.x, -Bonferroni.y)  # Keep only the updated Bonferroni values

df_full <- bind_rows(df_full, df_reciprocal) %>%
  distinct(Group1, Group2, .keep_all = TRUE)  # Ensure duplicates are kept

# Categorize Bonferroni values for coloring
df_full <- df_full %>%
  mutate(
    color = case_when(
      Bonferroni < 0.05 ~ "grey40",  # Use "grey40" explicitly
      Bonferroni >= 0.05 ~ "grey95",
      is.na(Bonferroni) ~ NA_character_
    )
  )

# Ensure levels preserve the correct group-wise order
df_full$Group1 <- factor(df_full$Group1, levels = all_labels)
df_full$Group2 <- factor(df_full$Group2, levels = rev(all_labels))  # Ensure full mirroring

# Extract Group1 and Group2 levels for correct coloring of axis labels
group1_levels <- levels(df_full$Group1)
group2_levels <- levels(df_full$Group2)

# Step 3: Plot checkerboard heatmap with FULL Reciprocal Values
library(ggplot2)
library(ggtext)
library(ggnewscale)

# Define color labels
color_labels <- c("p <0.05" = "grey40",
                  "p 0.05-0.1" = "grey70",
                  "p <0.1" = "grey95")

# Ensure missing values (NA) are properly handled
df_full$Bonferroni_cat <- cut(df_full$Bonferroni, 
                              breaks = c(-Inf, 0.05, 0.1, Inf), 
                              labels = names(color_labels))

# Convert NA values to explicit category for better visualization
df_full$Bonferroni_cat <- as.character(df_full$Bonferroni_cat)
df_full$Bonferroni_cat[is.na(df_full$Bonferroni)] <- "NA"

# LBd "NA" to the color scale so blank cells appear white
color_labels["NA"] <- "white"

# Create color mappings for text labels
group2_colors <- ifelse(grepl("ApoE3", df_full$Group2), "grey40", "black")
group1_colors <- ifelse(grepl("ApoE3", df_full$Group1), "grey40", "black")

# Create the heatmap plot
ggplot(df_full, aes(x = Group2, y = Group1, fill = Bonferroni_cat)) +
  geom_tile(color = "black") +
  geom_richtext(
    aes(label = ifelse(
      !is.na(Bonferroni),
      ifelse(Bonferroni < 0.01, "**<0.01**",  # Replace very low p-values with "<0.001"
             ifelse(Bonferroni < 0.05, paste0("**", sprintf("%.2f", Bonferroni), "**"), 
                    sprintf("%.2f", Bonferroni))),
      "")),  
    size = 8, fill = NA, label.color = NA,  
    color = ifelse(!is.na(df_full$Bonferroni) & df_full$Bonferroni < 0.05, "white", "black")  
  ) +
  scale_fill_manual(
    name = "Bonferroni P-Value",
    values = c(
      "p <0.05" = "grey40",
      "p 0.05-0.1" = "grey70",
      "p <0.1" = "grey95"
    ),
    breaks = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    labels = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    na.value = "white"  # Keeps blank cells white
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_markdown(angle = 90, hjust = 1, size = 14),  
    axis.text.y = element_markdown(angle = 0, hjust = 1, size = 14),
    panel.grid = element_blank()
  ) +
  labs(x = "", y = "", fill = "Bonferroni P-Value") +
  ggtitle("") +  
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +  
  coord_fixed()


Bonferroni_Heatmap_plot <-ggplot(df_full, aes(x = Group2, y = Group1, fill = Bonferroni_cat)) +
  geom_tile(color = "black") +
  geom_richtext(
    aes(label = ifelse(
      !is.na(Bonferroni),
      ifelse(Bonferroni < 0.01, "**<0.01**",  # Replace very low p-values with "<0.001"
             ifelse(Bonferroni < 0.05, paste0("**", sprintf("%.2f", Bonferroni), "**"), 
                    sprintf("%.2f", Bonferroni))),
      "")),  
    size = 8, fill = NA, label.color = NA,  
    color = ifelse(!is.na(df_full$Bonferroni) & df_full$Bonferroni < 0.05, "white", "black")  
  ) +
  scale_fill_manual(
    name = "Bonferroni P-Value",
    values = c(
      "p <0.05" = "grey40",
      "p 0.05-0.1" = "grey70",
      "p <0.1" = "grey95"
    ),
    breaks = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    labels = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    na.value = "white"  # Keeps blank cells white
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_markdown(angle = 90, hjust = 1, size = 14),  
    axis.text.y = element_markdown(angle = 0, hjust = 1, size = 14),
    panel.grid = element_blank()
  ) +
  labs(x = "", y = "", fill = "Bonferroni P-Value") +
  ggtitle("") +  
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +  
  coord_fixed()

# Step 5: Save the plot as a PNG
ggsave("Bonferroni_Heatmap_plot_AT8_Braak_Braak.png", 
       plot = Bonferroni_Heatmap_plot, width = 10, height = 8, dpi = 300)

Bonferroni_Heatmap_plot_AT8_Braak_Braak <- Bonferroni_Heatmap_plot


#############CHECKERBOARD_PLOT###################################
setwd(datadir)

final_results_AT8_ABC <- final_results_AT8_ABC %>%
  filter(Group1 != "Global" & Group2 != "All Groups") %>%
  mutate(
    Group = "3_ABC",
    Group1 = paste0(Group1, "_ABC"),
    Group2 = paste0(Group2, "_ABC")
  )
final_results_Braak_Braak <- final_results_Braak_Braak %>%
  filter(Group1 != "Global" & Group2 != "All Groups") %>%
  mutate(
    Group = "2_Braak_Braak",
    Group1 = paste0(Group1, "Braak_Braak"),
    Group2 = paste0(Group2, "Braak_Braak")
  )

final_results_AT8_UPR85 <- final_results_AT8_UPR85 %>%
  filter(Group1 != "Global" & Group2 != "All Groups") %>%
  mutate(
    Group = "1_UPR85",
    Group1 = paste0(Group1, "_AT8_85th Percentile"),
    Group2 = paste0(Group2, "_AT8_85th Percentile")
  )

combined_all_results <- bind_rows(final_results_AT8_ABC, final_results_Braak_Braak, final_results_AT8_UPR85)

# Print the updated combined dataframe
print(combined_all_results)

# Save the final dataframe as a CSV file
write.csv(combined_all_results, "combined_all_results_UPR85.csv", row.names = FALSE)
# Step 1: Load the dataset
df <- combined_all_results %>%
  as.data.frame()
# Step 2: Duplicate all rows and swap `Group1` and `Group2` in the duplicated rows
df_reciprocal <- df %>%
  rename(Group1 = Group2, Group2 = Group1)  # Swap the values

# Step 3: Combine the original and reciprocal data
df_full <- bind_rows(df, df_reciprocal) %>%
  distinct(Group1, Group2, .keep_all = TRUE)  # Ensure no duplicates are removed

# Step 4: Display the final dataset
print(df_full)

df<- df_full

# Order variables within each "Group"
df <- df %>%
  group_by(Group) %>%
  arrange(Group, Group1, Group2, .by_group = TRUE)

# Get unique labels maintaining order within each "Group"
all_labels <- df %>%
  select(Group, Group1, Group2) %>%
  pivot_longer(cols = c(Group1, Group2), values_to = "Label") %>%
  distinct(Group, Label) %>%
  arrange(Group) %>%
  pull(Label)

# Create full grid while keeping group-wise structure
complete_grid <- expand.grid(Group1 = all_labels, Group2 = all_labels, stringsAsFactors = FALSE)

# Merge with actual data while preserving "Group" order
df_full <- complete_grid %>%
  left_join(df, by = c("Group1", "Group2"))

# **Step 2: Add Reciprocal Values Properly**
df_reciprocal <- df_full %>%
  rename(Group1 = Group2, Group2 = Group1)  # Swap Group1 and Group2

# Ensure reciprocal pairs have the same Bonferroni values
df_reciprocal <- df_reciprocal %>%
  left_join(df_full %>% select(Group1, Group2, Bonferroni), by = c("Group1", "Group2")) %>%
  mutate(Bonferroni = coalesce(Bonferroni.x, Bonferroni.y)) %>%
  select(-Bonferroni.x, -Bonferroni.y)  # Keep only the updated Bonferroni values

df_full <- bind_rows(df_full, df_reciprocal) %>%
  distinct(Group1, Group2, .keep_all = TRUE)  # Ensure duplicates are kept

# Categorize Bonferroni values for coloring
df_full <- df_full %>%
  mutate(
    color = case_when(
      Bonferroni <= 0.05 ~ "dodgerblue",  # Use "dodgerblue" explicitly
      Bonferroni > 0.05 ~ "grey",
      is.na(Bonferroni) ~ NA_character_
    )
  )

# Ensure levels preserve the correct group-wise order
df_full$Group1 <- factor(df_full$Group1, levels = all_labels)
df_full$Group2 <- factor(df_full$Group2, levels = rev(all_labels))  # Ensure full mirroring

# Extract Group1 and Group2 levels for correct coloring of axis labels
group1_levels <- levels(df_full$Group1)
group2_levels <- levels(df_full$Group2)

# Step 3: Plot checkerboard heatmap with FULL Reciprocal Values
library(ggplot2)
library(ggtext)
library(ggnewscale)

# Define color labels
color_labels <- c("p <0.05" = "dodgerblue",
                  "p 0.05-0.1" = "lightskyblue3",
                  "p <0.1" = "grey")

# Ensure missing values (NA) are properly handled
df_full$Bonferroni_cat <- cut(df_full$Bonferroni, 
                              breaks = c(-Inf, 0.05, 0.1, Inf), 
                              labels = names(color_labels))

# Convert NA values to explicit category for better visualization
df_full$Bonferroni_cat <- as.character(df_full$Bonferroni_cat)
df_full$Bonferroni_cat[is.na(df_full$Bonferroni)] <- "NA"

# Add "NA" to the color scale so blank cells appear white
color_labels["NA"] <- "white"

# Create color mappings for text labels
group2_colors <- ifelse(grepl("ApoE3", df_full$Group2), "dodgerblue", "firebrick")
group1_colors <- ifelse(grepl("ApoE3", df_full$Group1), "dodgerblue", "firebrick")

# Create the heatmap plot
ggplot(df_full, aes(x = Group2, y = Group1, fill = Bonferroni_cat)) +
  geom_tile(color = "black") +
  geom_richtext(
    aes(label = ifelse(
      !is.na(Bonferroni),
      ifelse(Bonferroni < 0.01, "**<0.01**",  # Replace very low p-values with "<0.001"
             ifelse(Bonferroni < 0.05, paste0("**", sprintf("%.2f", Bonferroni), "**"), 
                    sprintf("%.2f", Bonferroni))),
      "")),   
    size = 8, fill = NA, label.color = NA,  
    color = ifelse(!is.na(df_full$Bonferroni) & df_full$Bonferroni <= 0.05, "white", "black")  
  ) +
  scale_fill_manual(
    name = "Bonferroni P-Value",
    values = c(
      "p <0.05" = "dodgerblue",
      "p 0.05-0.1" = "lightskyblue3",
      "p <0.1" = "grey"
    ),
    breaks = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    labels = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    na.value = "white"  # Keeps blank cells white
  ) +
  scale_x_discrete(labels = function(x) { 
    ifelse(grepl("ApoE3", x), paste0("<span style='color:dodgerblue;'>", x, "</span>"),
           paste0("<span style='color:firebrick;'>", x, "</span>")) 
  }) +
  scale_y_discrete(labels = function(x) { 
    ifelse(grepl("ApoE3", x), paste0("<span style='color:dodgerblue;'>", x, "</span>"),
           paste0("<span style='color:firebrick;'>", x, "</span>")) 
  }) +
  theme_minimal() +
  theme(
    axis.text.x = element_markdown(angle = 45, hjust = 1, size = 14),  
    axis.text.y = element_markdown(angle = 0, hjust = 1, size = 14),
    panel.grid = element_blank()
  ) +
  labs(x = "Group2", y = "Group1", fill = "Bonferroni P-Value") +
  ggtitle("") +  
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +  
  coord_fixed()


Bonferroni_Heatmap_plot <-ggplot(df_full, aes(x = Group2, y = Group1, fill = Bonferroni_cat)) +
  geom_tile(color = "black") +
  geom_richtext(
    aes(label = ifelse(
      !is.na(Bonferroni),
      ifelse(Bonferroni < 0.01, "**<0.01**",  # Replace very low p-values with "<0.001"
             ifelse(Bonferroni < 0.05, paste0("**", sprintf("%.2f", Bonferroni), "**"), 
                    sprintf("%.2f", Bonferroni))),
      "")),  
    size = 8, fill = NA, label.color = NA,  
    color = ifelse(!is.na(df_full$Bonferroni) & df_full$Bonferroni <= 0.05, "white", "black")  
  ) +
  scale_fill_manual(
    name = "Bonferroni P-Value",
    values = c(
      "p <0.05" = "dodgerblue",
      "p 0.05-0.1" = "lightskyblue3",
      "p <0.1" = "grey"
    ),
    breaks = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    labels = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    na.value = "white"  # Keeps blank cells white
  ) +
  scale_x_discrete(labels = function(x) { 
    ifelse(grepl("ApoE3", x), paste0("<span style='color:dodgerblue;'>", x, "</span>"),
           paste0("<span style='color:firebrick;'>", x, "</span>")) 
  }) +
  scale_y_discrete(labels = function(x) { 
    ifelse(grepl("ApoE3", x), paste0("<span style='color:dodgerblue;'>", x, "</span>"),
           paste0("<span style='color:firebrick;'>", x, "</span>")) 
  }) +
  theme_minimal() +
  theme(
    axis.text.x = element_markdown(angle = 90, hjust = 1, size = 14),  
    axis.text.y = element_markdown(angle = 0, hjust = 1, size = 14),
    panel.grid = element_blank()
  ) +
  labs(x = "Group2", y = "Group1", fill = "Bonferroni P-Value") +
  ggtitle("") +  
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +  
  coord_fixed()

# Step 5: Save the plot as a PNG
ggsave("Bonferroni_Heatmap_plot_AT8_UPR85.png", 
       plot = Bonferroni_Heatmap_plot, width = 16, height = 14, dpi = 300)


# LOAD DIRECTORIES ABETA+AT8##############
excel_path <- #"/LOAD.xlsx"
  setwd(#/FILE LOCATION")
#############IMPUTATION_ABETA################
# Load dataset
excel <- read_excel("/Users/WORK_2023/Jaunmuktane_Lab/DIGIPATH/ASAP DATA/PATHWAYS_TO_DEMENTIA_PAPER/PATHWAYS_TO_DEMENTIA_SUMMARY.xlsx", 
                    sheet = "300_R_BLOCK") %>%
  as.data.frame()

excel <- excel %>%
  mutate(
    # Impute ABETA_FRNT_CTX based on available columns
    ABETA_FRNT_CTX = case_when(
      !is.na(ABETA_FRNT_CTX) ~ ABETA_FRNT_CTX,
      !is.na(ABETA_PRT_CTX)  ~ ABETA_PRT_CTX,
      !is.na(ABETA_TEMP_CTX) ~ ABETA_TEMP_CTX,
      !is.na(ABETA_OCCI_CTX) ~ ABETA_OCCI_CTX,
      TRUE ~ NA_real_  # Keep NA if all are missing
    )
  ) %>%
  
  # Impute ABETA_HIPPO based on Thal criteria
  mutate(
    ABETA_HIPPO = case_when(
      is.na(ABETA_HIPPO) & !is.na(Thal) & Thal <= 2 ~ 0.00001, # Impute if Thal is ≤ 2
      TRUE ~ ABETA_HIPPO  # Keep existing values
    )
  ) %>%
  
  # Compute ABETA_COMB_FH as before, but set to NA if AD_LEVEL is missing
  mutate(
    ABETA_COMB_FH = ifelse(is.na(AD_LEVEL), NA_real_, (ABETA_FRNT_CTX + ABETA_HIPPO) / 2)
  ) %>%
  
  # If ABETA_COMB_FH is NA, make AD_LEVEL and Thal_BIN also NA
  mutate(
    AD_LEVEL = ifelse(is.na(ABETA_COMB_FH), NA, AD_LEVEL),
    Thal_BIN = ifelse(is.na(ABETA_COMB_FH), NA, Thal_BIN)
  )







#############IMPUTATION_AT8_NEW################

excel <- excel %>%
  # Set AT8_HIPPO to NA for specific cases before further modifications
  mutate(
    AT8_HIPPO = ifelse(`CASE. NO.` %in% c("P7/02", "P47/00"), NA_real_, AT8_HIPPO)
  ) %>%
  # Impute AT8_HIPPO based on Braak_Braak criteria
  mutate(
    AT8_HIPPO = case_when(
      is.na(AT8_HIPPO) & !is.na(Braak_Braak) & Braak_Braak < 2 ~ 0.00001, # Impute if Braak_Braak < 2
      TRUE ~ AT8_HIPPO  # Keep existing values
    )
  ) %>%
  # Impute AT8_FRNT_CTX if it is NA and AT8_HIPPO has a value
  mutate(
    AT8_FRNT_CTX = case_when(
      is.na(AT8_FRNT_CTX) & !is.na(AT8_HIPPO) ~ 0.00001,  # Impute if missing and AT8_HIPPO exists
      TRUE ~ AT8_FRNT_CTX  # Keep existing values
    )
  ) %>%
  
  # Compute AT8_COMB_FH, but set to NA if AD_LEVEL is missing
  mutate(
    AT8_COMB_FH = ifelse(is.na(AD_LEVEL), NA_real_, (AT8_FRNT_CTX + AT8_HIPPO) / 2)
  ) %>%
  
  # If AT8_COMB_FH is NA, make AD_LEVEL and Braak_Braak_BIN also NA
  mutate(
    AD_LEVEL = ifelse(is.na(AT8_COMB_FH), NA, AD_LEVEL),
    Braak_Braak__NEWBIN = ifelse(is.na(AT8_COMB_FH), NA, Braak_Braak_BIN)
  )
#############COMBINED_ABETA_AT8_PERCENTILE##############
excel <- excel %>%
  mutate(
    ABETA_PERCENTILE = percent_rank(ABETA_COMB_FH),  # Compute percentile for ABETA_COMB_FH
    AT8_PERCENTILE = percent_rank(AT8_COMB_FH),      # Compute percentile for AT8_COMB_FH
    ABETA_AT8_COMB_FH = (ABETA_PERCENTILE + AT8_PERCENTILE) / 2  # Take the mean
  )
####################################################UPR75####################################################
#############UPR75_STAGING##################################
# Ensure ApoE is a factor and remove NA levels from facets
excel_graph <- excel %>%
  filter(!is.na(ApoE))
# Convert DEMENTIA to a factor for better visualization
excel_graph$DEMENTIA <- as.factor(excel_graph$DEMENTIA)

# Define custom colors for ApoE3 and ApoE4 groups
custom_colors <- c("ApoE3" = "#88CCEE", "ApoE4" = "#CC6677")  # Light/Dark blue for ApoE3

# Create the plot
ggplot(excel_graph, aes(x = ABETA_AT8_COMB_FH, fill = ApoE, color = ApoE)) +
  geom_density(aes(y = ..density.. * 100), alpha = 0.4, size = 1) +  # Convert density to percentage & adjust transparency
  scale_fill_manual(values = c("ApoE3" = "dodgerblue", "ApoE4" = "firebrick")) +  # Set fill colors
  scale_color_manual(values = c("ApoE3" = "dodgerblue", "ApoE4" = "firebrick")) +  # Set outline colors
  labs(
    title = "",
    x = expression(italic(A) * beta ~ " & pTau pathology percentile"),
    y = "Density (%) of Cases",  # Updated y-axis label
    fill = "ApoE Group",
    color = "ApoE Group"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.85, 0.85),  # Move legend inside top-right corner
    legend.key = element_rect(fill = NA),  # Keep legend keys clean
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )


ggsave("ABETA_AT8_FRNT_HIPP_Distribution_ApoE.png", width = 8, height = 6, dpi = 300)


# Calculate the percentile cutoff (median of ABETA_AT8_COMB_FH)
upper_quartile <- quantile(excel$ABETA_AT8_COMB_FH, probs = 0.75, na.rm = TRUE)

# Define AD category based on 75th percentile
excel_UPR75 <- excel %>%
  mutate(AD_Category = case_when(
    ABETA_AT8_COMB_FH >= upper_quartile ~ "AD_HIGH",  # Strictly greater than
    ABETA_AT8_COMB_FH < upper_quartile ~ "AD_LOW"    # Less than or equal
  )) 

# Remove NA values
excel_UPR75_filter <- excel_UPR75 %>%
  filter(!is.na(AD_Category))

# Summarize data for `excel_filtered_UPR75`
summary_data_UPR75 <- excel_UPR75_filter %>%
  group_by(AD_Category, ApoE, DEMENTIA) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(
    Group = paste(ApoE, DEMENTIA, sep = "_"),
    ApoE_Group = paste(ApoE, AD_Category, sep = "_")
  ) %>%
  distinct()  # Ensure unique groups

# Summarize data for dementia proportions
summary_prop_data_UPR75_comb <- excel_UPR75_filter %>%
  group_by(ApoE, AD_Category, DEMENTIA) %>%
  summarise(Count = n(), .groups = "drop") %>%
  spread(key = DEMENTIA, value = Count, fill = 0) %>%
  mutate(
    Proportion_Dementia_1 = DEM_1 / (DEM_0 + DEM_1),
    ApoE_Group = paste(ApoE, AD_Category, sep = "_")
  )

# Remove NA groups for ApoE
summary_prop_data_UPR75_comb <- summary_prop_data_UPR75_comb %>%
  filter(!grepl("^NA_|ApoE3_NA|ApoE4_NA", ApoE_Group))  # Remove all NA groups





#SUMMARY Proportions ###################
write.csv(summary_prop_data_UPR75_comb, "summary_prop_data_ABETA_AT8_UPR75.csv", row.names = FALSE)

# Add Proportion of No Dementia Cases
summary_prop_data_UPR75_comb <- summary_prop_data_UPR75_comb %>%
  mutate(Proportion_No_Dementia = 1 - Proportion_Dementia_1)

# Convert data to long format for ggplot
summary_prop_data_UPR75_comb_long <- summary_prop_data_UPR75_comb %>%
  tidyr::pivot_longer(cols = c(Proportion_Dementia_1, Proportion_No_Dementia),
                      names_to = "Dementia_Status",
                      values_to = "Proportion")

# Rename labels
summary_prop_data_UPR75_comb_long$Dementia_Status <- factor(summary_prop_data_UPR75_comb_long$Dementia_Status,
                                                            levels = c("Proportion_No_Dementia", "Proportion_Dementia_1"),
                                                            labels = c("No Dementia", "Dementia"))

library(stringr)

# Remove "ABETA_AT8_" from ApoE_Group names
#summary_prop_data_UPR75_comb_long <- summary_prop_data_UPR75_comb_long %>%
#mutate(ApoE_Group = str_replace(ApoE_Group, "AD_", "MPP_"))

summary_prop_data_UPR75_comb_long <- summary_prop_data_UPR75_comb_long %>%
  mutate(
    ApoE_Group = case_when(
      ApoE_Group == "ApoE3_AD_HIGH" ~ "ApoE3: H",
      ApoE_Group == "ApoE3_AD_LOW"  ~ "ApoE3: N/L",
      ApoE_Group == "ApoE4_AD_HIGH" ~ "ApoE4: H",
      ApoE_Group == "ApoE4_AD_LOW"  ~ "ApoE4: N/L",
      TRUE ~ ApoE_Group  # Keep any other values unchanged
    )
  )

summary_prop_data_UPR75_comb_long$Dementia_Status <- factor(summary_prop_data_UPR75_comb_long$Dementia_Status, levels = c("Dementia", "No Dementia"))
# Create stacked bar plot
summary_prop_data_UPR75_comb_plot <- ggplot(summary_prop_data_UPR75_comb_long, aes(x = ApoE_Group, y = Proportion, fill = Dementia_Status)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = ifelse(Proportion == 0, "", paste0(round(Proportion * 100, 0)))),
            position = position_stack(vjust = 0.5), 
            size = 9, color =ifelse(summary_prop_data_Braak_long$Dementia_Status== "Dementia", "black", "black"),fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "Proportion (%)", fill = "Dementia Status") +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  ) +
  scale_fill_manual(values = c("Dementia" = "#FF9966", "No Dementia" = "#6699CC")) +  # Custom bar colors
  scale_color_manual(values = c("Dementia" = "black", "No Dementia" = "black"), guide = "none") +  # Fix text colors & remove legend
  coord_flip()  # Flip the axes


# Save the plot
ggsave("summary_prop_data_ABETA_AT8_UPR75.png", summary_prop_data_UPR75_comb_plot, width = 8, height = 6, dpi = 300)
# Display the plot
print(summary_prop_data_UPR75_comb_plot)

# Create proportion table
proportion_table_UPR75 <- summary_prop_data_UPR75_comb %>%
  select(ApoE, AD_Category, Proportion_Dementia_1) %>%
  spread(key = AD_Category, value = Proportion_Dementia_1)

# Get unique ApoE Groups (after NA removal)
apoE_groups_UPR75 <- unique(summary_prop_data_UPR75_comb$ApoE_Group)

# Contingency table for ApoE_Group and Proportion_Dementia_1
contingency_data_UPR75 <- summary_prop_data_UPR75_comb %>%
  group_by(ApoE_Group) %>%
  summarise(Proportion_Dementia_1 = mean(Proportion_Dementia_1))

# Print updated contingency table
print(contingency_data_UPR75)

# Construct global contingency table
global_contingency_UPR75 <- table(contingency_data_UPR75$ApoE_Group, contingency_data_UPR75$Proportion_Dementia_1)

# Perform global test (Fisher's Exact or Chi-Square)
global_test_UPR75 <- if (any(global_contingency_UPR75 < 5)) {
  fisher.test(global_contingency_UPR75)
} else {
  chisq.test(global_contingency_UPR75)
}

# Store global test result
all_results_UPR75 <- data.frame(
  Group1 = "Global",
  Group2 = "All Groups",
  Group1_N = sum(summary_prop_data_UPR75_comb$DEM_0) + sum(summary_prop_data_UPR75_comb$DEM_1),
  Group2_N = "N/A",
  Method = ifelse(any(global_contingency_UPR75 < 5), "Fisher's Exact Test", "Chi-Square Test"),
  P.Value = global_test_UPR75$p.value
)

# Pairwise comparisons for ApoE Groups
for (i in seq_along(apoE_groups_UPR75)) {
  for (j in seq_along(apoE_groups_UPR75)) {
    if (i < j) {  # Avoid duplicate comparisons
      group1_data <- summary_prop_data_UPR75_comb %>% filter(ApoE_Group == apoE_groups_UPR75[i])
      group2_data <- summary_prop_data_UPR75_comb %>% filter(ApoE_Group == apoE_groups_UPR75[j])
      
      # Construct contingency table
      contingency_table <- matrix(
        c(sum(group1_data$DEM_1), sum(group1_data$DEM_0),
          sum(group2_data$DEM_1), sum(group2_data$DEM_0)),
        nrow = 2
      )
      
      # Perform appropriate test
      test <- if (any(contingency_table < 5)) {
        fisher.test(contingency_table)
      } else {
        chisq.test(contingency_table)
      }
      
      # Append results
      all_results_UPR75 <- rbind(
        all_results_UPR75,
        data.frame(
          Group1 = apoE_groups_UPR75[i],
          Group2 = apoE_groups_UPR75[j],
          Group1_N = sum(group1_data$DEM_1 + group1_data$DEM_0),
          Group2_N = sum(group2_data$DEM_1 + group2_data$DEM_0),
          P.Value = test$p.value,
          Method = ifelse(any(contingency_table < 5), "Fisher's Exact Test", "Chi-Square Test")
        )
      )
    }
  }
}

# Separate global test from pairwise comparisons
global_result_UPR75 <- all_results_UPR75 %>% filter(Group1 == "Global")
pairwise_results_UPR75 <- all_results_UPR75 %>% filter(Group1 != "Global")

# Apply multiple testing corrections **only to pairwise comparisons**
pairwise_results_UPR75 <- pairwise_results_UPR75 %>%
  mutate(
    Bonferroni = p.adjust(P.Value, method = "bonferroni"),
    Holm = p.adjust(P.Value, method = "holm"),
    FDR = p.adjust(P.Value, method = "fdr")
  )

# Combine back with the global test result (without corrections)
all_results_UPR75 <- bind_rows(global_result_UPR75, pairwise_results_UPR75)

# Print final results
print(all_results_UPR75)

# Save results
write.csv(all_results_UPR75, "DEMENTIA_UPR75_ABETA_AT8_Test_Results.csv", row.names = FALSE)
write.table(all_results_UPR75, "DEMENTIA_UPR75_ABETA_AT8_Test_Results.txt", row.names = FALSE, sep = "\t", quote = FALSE)

# Print final cleaned results
final_results_ABETA_AT8_UPR75 <- all_results_UPR75 %>%
  select(Group1, Group2, Group1_N, Group2_N, P.Value, Bonferroni, Holm, FDR)

print(final_results_ABETA_AT8_UPR75)









#############ABC_STAGING##################################
# Set AD_LEVEL to NA if ABETA_COMB_FH is NA
excel <- excel %>%
  mutate(
    AD_LEVEL = ifelse(is.na(ABETA_COMB_FH), NA, AD_LEVEL)
  )
# Define AD category
excel_ABC <- excel %>%
  mutate(AD_Category = case_when(
    AD_LEVEL == "AD_LOW" ~ "AD_LOW",
    AD_LEVEL == "AD_HIGH" ~ "AD_HIGH",
    TRUE ~ NA_character_
  )) 


excel_ABC_filter <- excel_ABC %>% filter(!is.na(AD_Category))  # Remove NA values

# Summarize data for `excel_filtered_Thal`
summary_data_ABC <- excel_ABC_filter %>%
  group_by(AD_Category, ApoE, DEMENTIA) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Group = paste(ApoE, DEMENTIA, sep = "_"),
         ApoE_Group = paste(ApoE, AD_Category, sep = "_"))

# Summarize data for dementia proportions
summary_prop_data_ABC <- excel_ABC_filter %>%
  group_by(ApoE, AD_Category, DEMENTIA) %>%
  summarise(Count = n(), .groups = "drop") %>%
  spread(key = DEMENTIA, value = Count, fill = 0) %>%
  mutate(Proportion_Dementia_1 = DEM_1 / (DEM_0 + DEM_1),
         ApoE_Group = paste(ApoE, AD_Category, sep = "_"))

summary_prop_data_ABC <- summary_prop_data_ABC %>%
  filter(!grepl("ApoE3_NA|ApoE4_NA", ApoE_Group))  # Exclude specific NA groups

# Create proportion table
proportion_table_ABC <- summary_prop_data_ABC %>%
  select(ApoE, AD_Category, Proportion_Dementia_1) %>%
  spread(key = AD_Category, value = Proportion_Dementia_1)

# Remove NA groups for ApoE
summary_prop_data_ABC <- summary_prop_data_ABC %>%
  filter(!grepl("^NA_", ApoE_Group))






#SUMMARY Proportions ###################
write.csv(summary_prop_data_ABC, "summary_prop_data_ABETA_AT8_ABC.csv", row.names = FALSE)

# Add Proportion of No Dementia Cases
summary_prop_data_ABC <- summary_prop_data_ABC %>%
  mutate(Proportion_No_Dementia = 1 - Proportion_Dementia_1)

# Convert data to long format for ggplot
summary_prop_data_ABC_long <- summary_prop_data_ABC %>%
  tidyr::pivot_longer(cols = c(Proportion_Dementia_1, Proportion_No_Dementia),
                      names_to = "Dementia_Status",
                      values_to = "Proportion")

# Rename labels
summary_prop_data_ABC_long$Dementia_Status <- factor(summary_prop_data_ABC_long$Dementia_Status,
                                                     levels = c("Proportion_No_Dementia", "Proportion_Dementia_1"),
                                                     labels = c("No Dementia", "Dementia"))


library(stringr)

# Remove "ABETA_AT8_" from ApoE_Group names
#summary_prop_data_ABC_long <- summary_prop_data_ABC_long %>%
# mutate(ApoE_Group = str_replace(ApoE_Group, "AD_", "MPP_"))

summary_prop_data_ABC_long <- summary_prop_data_ABC_long %>%
  mutate(
    ApoE_Group = case_when(
      ApoE_Group == "ApoE3_AD_HIGH" ~ "ApoE3: H",
      ApoE_Group == "ApoE3_AD_LOW"  ~ "ApoE3: N/L",
      ApoE_Group == "ApoE4_AD_HIGH" ~ "ApoE4: H",
      ApoE_Group == "ApoE4_AD_LOW"  ~ "ApoE4: N/L",
      TRUE ~ ApoE_Group  # Keep any other values unchanged
    )
  )

summary_prop_data_ABC_long$Dementia_Status <- factor(summary_prop_data_ABC_long$Dementia_Status, levels = c("Dementia", "No Dementia"))
# Create stacked bar plot
summary_prop_data_ABC_plot <- ggplot(summary_prop_data_ABC_long, aes(x = ApoE_Group, y = Proportion, fill = Dementia_Status)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = ifelse(Proportion == 0, "", paste0(round(Proportion * 100, 0)))),
            position = position_stack(vjust = 0.5), 
            size = 9, color =ifelse(summary_prop_data_Braak_long$Dementia_Status== "Dementia", "black", "black"),fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "Proportion (%)", fill = "Dementia Status") +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  ) +
  scale_fill_manual(values = c("Dementia" = "#FF9966", "No Dementia" = "#6699CC")) +  # Custom bar colors
  scale_color_manual(values = c("Dementia" = "black", "No Dementia" = "black"), guide = "none") +  # Fix text colors & remove legend
  coord_flip()  # Flip the axes


# Save the plot
ggsave("summary_prop_data_ABETA_AT8_ABC.png", summary_prop_data_ABC_plot, width = 8, height = 6, dpi = 300)
# Display the plot
print(summary_prop_data_ABC_plot)

# Get unique ApoE Groups (after NA removal)
apoE_groups_ABC <- unique(summary_prop_data_ABC$ApoE_Group)

# Contingency table for ApoE_Group and Proportion_Dementia_1
contingency_data_ABC <- summary_prop_data_ABC %>%
  group_by(ApoE_Group) %>%
  summarise(Proportion_Dementia_1 = mean(Proportion_Dementia_1))

# Print updated contingency table
print(contingency_data_ABC)

# Construct global contingency table
global_contingency_ABC <- table(contingency_data_ABC$ApoE_Group, contingency_data_ABC$Proportion_Dementia_1)

# Perform global test (Fisher's Exact or Chi-Square)
global_test_ABC <- if (any(global_contingency_ABC < 5)) {
  fisher.test(global_contingency_ABC)
} else {
  chisq.test(global_contingency_ABC)
}

# Store global test result
all_results_ABC <- data.frame(
  Group1 = "Global",
  Group2 = "All Groups",
  Group1_N = sum(summary_prop_data_ABC$DEM_0) + sum(summary_prop_data_ABC$DEM_1),
  Group2_N = "N/A",
  Method = ifelse(any(global_contingency_ABC < 5), "Fisher's Exact Test", "Chi-Square Test"),
  P.Value = global_test_ABC$p.value
)

# Pairwise comparisons for ApoE Groups
for (i in seq_along(apoE_groups_ABC)) {
  for (j in seq_along(apoE_groups_ABC)) {
    if (i < j) {  # Avoid duplicate comparisons
      group1_data <- summary_prop_data_ABC %>% filter(ApoE_Group == apoE_groups_ABC[i])
      group2_data <- summary_prop_data_ABC %>% filter(ApoE_Group == apoE_groups_ABC[j])
      
      # Construct contingency table
      contingency_table <- matrix(
        c(sum(group1_data$DEM_1), sum(group1_data$DEM_0),
          sum(group2_data$DEM_1), sum(group2_data$DEM_0)),
        nrow = 2
      )
      
      # Perform appropriate test
      test <- if (any(contingency_table < 5)) {
        fisher.test(contingency_table)
      } else {
        chisq.test(contingency_table)
      }
      
      # Append results
      all_results_ABC <- rbind(
        all_results_ABC,
        data.frame(
          Group1 = apoE_groups_ABC[i],
          Group2 = apoE_groups_ABC[j],
          Group1_N = sum(group1_data$DEM_1 + group1_data$DEM_0),
          Group2_N = sum(group2_data$DEM_1 + group2_data$DEM_0),
          P.Value = test$p.value,
          Method = ifelse(any(contingency_table < 5), "Fisher's Exact Test", "Chi-Square Test")
        )
      )
    }
  }
}

# Separate global test from pairwise comparisons
global_result_ABC <- all_results_ABC %>% filter(Group1 == "Global")
pairwise_results_ABC <- all_results_ABC %>% filter(Group1 != "Global")

# Apply multiple testing corrections **only to pairwise comparisons**
pairwise_results_ABC <- pairwise_results_ABC %>%
  mutate(
    Bonferroni = p.adjust(P.Value, method = "bonferroni"),
    Holm = p.adjust(P.Value, method = "holm"),
    FDR = p.adjust(P.Value, method = "fdr")
  )

# Combine back with the global test result (without corrections)
all_results_ABC <- bind_rows(global_result_ABC, pairwise_results_ABC)

# Print final results
print(all_results_ABC)
# Print final cleaned results
final_results_ABC <- all_results_ABC %>%
  select(Group1, Group2, Group1_N, Group2_N, P.Value, Bonferroni, Holm, FDR)

print(final_results_ABC)
###########################BREAK##############################








#############Combine_Cutoff ####################################

excel$ABC_Level <- excel_ABC$AD_Category
excel$ABETA_AT8_Upperquartile<- excel_UPR75$AD_Category

excel_plot <- excel %>%
  filter(!is.na(ABC_Level))
excel_plot <- excel_plot %>%
  filter(!is.na(ABETA_AT8_Upperquartile))


#############LINE PLOT ####################################
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Convert grouping columns to factors
excel <- excel %>%
  mutate(
    ABC_Level = as.factor(ABC_Level),
    ABETA_AT8_Upperquartile = as.factor(ABETA_AT8_Upperquartile)
  )

# Reshape data for long format (to plot multiple groups) and remove NA values
excel_long <- excel %>%
  pivot_longer(cols = c("ABC_Level", "ABETA_AT8_Upperquartile"), 
               names_to = "AD_Group", values_to = "AD_Value") %>%
  drop_na(AD_Value)  # Remove rows where LB_Value is NA

# Compute mean and standard error for ASYN_FRNT_HIPP within each group
summary_data <- excel_long %>%
  group_by(AD_Group, AD_Value) %>%
  summarise(
    Mean_ABETA_AT8 = mean(ABETA_AT8_COMB_FH, na.rm = TRUE),
    SE_ABETA_AT8 = sd(ABETA_AT8_COMB_FH, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Ensure AD_Value is a factor with correct ordering
summary_data$AD_Value <- factor(summary_data$AD_Value, levels = c("AD_LOW", "AD_HIGH"))
# Ensure correct legend order by setting factor levels
summary_data <- summary_data %>%
  mutate(AD_Group = factor(AD_Group, 
                           levels = c("ABC_Level", "ABETA_AT8_Upperquartile")))
# Plot the updated data
plot <- ggplot(summary_data, aes(x = AD_Value, y = Mean_ABETA_AT8, group = AD_Group, color = AD_Group)) +
  geom_line(size = 1) +  # Line for mean ASYN_FRNT_HIPP
  geom_point(size = 3) +  # Points for mean values
  geom_errorbar(aes(ymin = Mean_ABETA_AT8 - SE_ABETA_AT8, ymax = Mean_ABETA_AT8 + SE_ABETA_AT8), width = 0.05) +  # Error bars
  scale_color_manual(values = c("ABC_Level" = "red",
                                "ABETA_AT8_Upperquartile" = "green3"),
                     labels = c("ABC_Level" = "ABC moderate or high",
                                "ABETA_AT8_Upperquartile" = expression(italic(A) * beta ~ " & pTau pathology in 75% quartile"))) +
  scale_x_discrete(labels = c("AD_LOW" = expression(bold(italic(A) * beta ~ " & pTau Absent \nor low")),
                              "AD_HIGH" = expression(bold(italic(A) * beta ~ " & pTau Present \nor high")))) +
  labs(
    x = "",
    y = expression(bold("Perecntile" ~ italic(A) * beta ~ " & pTau immunoreactivity")),
    color = "AD Group") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 18, face = "bold"),  # Larger and bolder x-axis text
    axis.text.y = element_text(size = 18, face = "bold"),  # Larger and bolder y-axis text
    axis.title.x = element_text(size = 18, face = "bold"),  # Larger x-axis title
    axis.title.y = element_text(size = 18, face = "bold"),  # Larger y-axis title
    axis.line = element_line(size = 2),  # Make axes bolder
    axis.ticks = element_line(size = 2),  # Make axis ticks bolder
    legend.text = element_text(size = 16, face = "bold"),  # Larger and bolder legend text
    legend.title = element_text(size = 18, face = "bold"),  # Larger and bolder legend title
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),  # Larger and centered title
  )
# Display the plot
print(plot)

# Save the plot as a PNG
ggsave("ABETA_AT8_COMB_FH_Line_Graph_UPR75.png", plot, width = 10, height = 6)


#############RATIO SUMMARY TABLE ####################################

# Load necessary libraries
library(dplyr)

# Summarize data for each combination of DEMENTIA and LB_Category
summary_table <- excel %>%
  pivot_longer(cols = all_of(c("ABC_Level", "ABETA_AT8_Upperquartile")), names_to = "AD_Group", values_to = "AD_Category") %>%
  filter(!is.na(AD_Category)) %>%  # Remove NA values in LB_Category
  group_by(AD_Group, DEMENTIA, AD_Category) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = c(DEMENTIA, AD_Category),
    values_from = Count,  # Use the "Count" column for values
    names_glue = "{AD_Category}_{DEMENTIA}"
  ) %>%
  mutate(
    AD_LOW_RATIO_DEM1_DEM0 = AD_LOW_DEM_1 / AD_LOW_DEM_0,
    AD_HIGH_RATIO_DEM1_DEM0 = AD_HIGH_DEM_1 / AD_HIGH_DEM_0,
    Total_Count = rowSums(select(., AD_HIGH_DEM_0, AD_LOW_DEM_0, AD_HIGH_DEM_1, AD_LOW_DEM_1), na.rm = TRUE)  # Sum of all count columns per row
  )

# Print the updated summary table
print(summary_table)
summary_table <- summary_table %>%
  mutate(AD_Group = factor(AD_Group, levels = c("ABC_Level", "ABETA_AT8_Upperquartile"))) %>%
  arrange(AD_Group)

print(summary_table)
# Save as CSV
write.csv(summary_table, "DEMENTIA_ABETA_AT8_Ratio_Table_UPR75.csv", row.names = FALSE)
write.table(summary_table, "DEMENTIA_ABETA_AT8_Ratio_Table_UPR75.txt", row.names = FALSE, sep = "\t", quote = FALSE)


#############PATHOLOGY_TABLE##########################


# Use the existing dataset
summary_table <- excel %>%
  pivot_longer(cols = all_of(c("ABC_Level", "ABETA_AT8_Upperquartile")), names_to = "AD_Group", values_to = "AD_Category") %>%
  filter(!is.na(AD_Category)) %>%  # Remove NA values in LB_Category
  group_by(AD_Group, DEMENTIA, AD_Category) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = c(DEMENTIA, AD_Category),
    values_from = Count,  # Use the "Count" column for values
    names_glue = "{AD_Category}_{DEMENTIA}"
  ) %>%
  mutate(
    Sensitivity = AD_HIGH_DEM_1 / (AD_HIGH_DEM_1 + AD_LOW_DEM_1),
    Specificity = AD_LOW_DEM_0 / (AD_LOW_DEM_0 + AD_HIGH_DEM_0),
    Odds_Ratio = (AD_HIGH_DEM_1 / AD_HIGH_DEM_0) / (AD_LOW_DEM_1 / AD_LOW_DEM_0),
    PPV = AD_HIGH_DEM_1 / (AD_HIGH_DEM_1 + AD_HIGH_DEM_0),
    Pathology_Absence_Ratio = (AD_LOW_DEM_1 / AD_LOW_DEM_0),
    Pathology_Presence_Ratio = (AD_HIGH_DEM_1 / AD_HIGH_DEM_0),
    Total_Count = rowSums(select(., AD_HIGH_DEM_0, AD_LOW_DEM_0, AD_HIGH_DEM_1, AD_LOW_DEM_1), na.rm = TRUE)  # Sum of all count columns per row
  )
summary_table <- summary_table %>%
  mutate(AD_Group = factor(AD_Group, levels = c("ABC_Level", "ABETA_AT8_Upperquartile"))) %>%
  arrange(AD_Group)

print(summary_table)
# Save to CSV
write.csv(summary_table, "Pathology_Statistics_ABETA_AT8_UPR75.csv", row.names = FALSE)
write.table(summary_table, "Pathology_Statistics_ABETA_AT8_UPR75.txt", row.names = FALSE, sep = "\t", quote = FALSE)
# Print the final table
print(summary_table)

library(fmsb)

# Normalize values
summary_table <- summary_table %>% mutate(
  Odds_Ratio = Odds_Ratio / max(Odds_Ratio, na.rm = TRUE),
  Pathology_Absence_Ratio = Pathology_Absence_Ratio / max(Pathology_Absence_Ratio, na.rm = TRUE),
  Pathology_Presence_Ratio = Pathology_Presence_Ratio / max(Pathology_Presence_Ratio, na.rm = TRUE)
)
# Save to CSV
write.csv(summary_table, "Pathology_Statistics_Norm_UPR75.csv", row.names = FALSE)
write.table(summary_table, "Pathology_Statistics_Norm_UPR75.txt", row.names = FALSE, sep = "\t", quote = FALSE)
# Print the final table
print(summary_table)
#############RADAR_PLOT#######################
# Define max and min values
max_vals <- rep(1, ncol(summary_table) - 1)
min_vals <- rep(0, ncol(summary_table) - 1)

# Combine min/max rows with the actual data
data <- rbind(max_vals, min_vals, summary_table[,-1])

data <- as.data.frame(data)

# Reorder columns for proper radar chart visualization
data_rotated <- data[, c("Specificity", "Pathology_Absence_Ratio", "Pathology_Presence_Ratio",
                         "Odds_Ratio", "PPV", "Sensitivity")]

colnames(data_rotated) <- c("Specificity", 
                            "PAR",
                            "PPR",
                            "OR", 
                            "PPV", 
                            "Sensitivity")
# Plot radar chart
# Adjust margins for a better layout
par(mar = c(1, 1, 1, 10) + 0.1)  
# Plot radar chart
radarchart(data_rotated,
           seg = 5,                     
           axistype = 1,                
           pcol = c("red", "green"),  
           pfcol = c(rgb(1, 0, 0, 0.3),  
                     rgb(0, 1, 0, 0.3),  
                     rgb(0, 0, 1, 0.3)),  
           cglcol = "grey",             
           cglty = 1,                   
           axislabcol = "grey",         
           caxislabels = seq(0, 1, 0.2), 
           vlcex = 3,  # Increase text size of variable labels
           cex.axis = 2,  # Increase size of axis labels
           cex.lab = 2  # (If applicable) Increase size of axis labels
)

# Adding a properly formatted legend
legend("right",
       inset = c(-0.2, 0),    # Moves legend further right
       legend = c("ABC moderate or high",
                  expression(italic(A) * beta ~ " & pTau pathology in 75% quartile")),
       bty = "n", pch = 20,
       col = c("red", "green"),
       text.col = "black",    cex = 1.2,   xpd = TRUE)  # Ensure text is visible outside

# Save radar chart as PNG
png("Radar_Chart_ABETA_AT8_UPR75_2.png", width = 3500, height = 2000, res=300,  bg = "transparent")
# Adjust margins for a better layout
par(mar = c(1, 1, 1, 10) + 0.1)  
# Plot radar chart
radarchart(data_rotated,
           seg = 5,                     
           axistype = 1,                
           pcol = c("red", "green"),  
           pfcol = c(rgb(1, 0, 0, 0.3),  
                     rgb(0, 1, 0, 0.3),  
                     rgb(0, 0, 1, 0.3)),  
           cglcol = "grey",             
           cglty = 1,                   
           axislabcol = "grey",         
           caxislabels = seq(0, 1, 0.2), 
           vlcex = 3,  # Increase text size of variable labels
           cex.axis = 2,  # Increase size of axis labels
           cex.lab = 2  # (If applicable) Increase size of axis labels
)

# Adding a properly formatted legend
legend("right",
       inset = c(-0.25, 0),    # Moves legend further right
       legend = c("ABC moderate or high",
                  expression(italic(A) * beta ~ " & pTau pathology in 75% quartile")),
       bty = "n", pch = 20,
       col = c("red", "green"),
       text.col = "black",    cex = 1.2,   xpd = TRUE)  # Ensure text is visible outside

dev.off()

##########################CHECKERBOARD_PLOT_ABETA_AT8_UPR75###################################
setwd(datadir)

final_results_ABETA_AT8_UPR75 <- final_results_ABETA_AT8_UPR75 %>%
  filter(Group1 != "Global" & Group2 != "All Groups") %>%
  mutate(
    Group = "GROUP",
    Group1 = case_when(
      Group1 == "ApoE3_AD_HIGH" ~ "ApoE3: H",
      Group1 == "ApoE3_AD_LOW"  ~ "ApoE3: N/L",
      Group1 == "ApoE4_AD_HIGH" ~ "ApoE4: H",
      Group1 == "ApoE4_AD_LOW"  ~ "ApoE4: N/L",
      TRUE ~ Group1  # Keep any other values unchanged
    ),
    Group2 = case_when(
      Group2 == "ApoE3_AD_HIGH" ~ "ApoE3: H",
      Group2 == "ApoE3_AD_LOW"  ~ "ApoE3: N/L",
      Group2 == "ApoE4_AD_HIGH" ~ "ApoE4: H",
      Group2 == "ApoE4_AD_LOW"  ~ "ApoE4: N/L",
      TRUE ~ Group2  # Keep any other values unchanged
    )
  )


combined_all_results <- bind_rows(final_results_ABETA_AT8_UPR75,)


# Step 1: LoLB the dataset
df <- combined_all_results %>%
  as.data.frame()
# Step 2: Duplicate all rows and swap `Group1` and `Group2` in the duplicated rows
df_reciprocal <- df %>%
  rename(Group1 = Group2, Group2 = Group1)  # Swap the values

# Step 3: Combine the original and reciprocal data
df_full <- bind_rows(df, df_reciprocal) %>%
  distinct(Group1, Group2, .keep_all = TRUE)  # Ensure no duplicates are removed

# Step 4: Display the final dataset
print(df_full)

df<- df_full

# Order variables within each "Group"
df <- df %>%
  group_by(Group) %>%
  arrange(Group, Group1, Group2, .by_group = TRUE)

# Get unique labels maintaining order within each "Group"
all_labels <- df %>%
  select(Group, Group1, Group2) %>%
  pivot_longer(cols = c(Group1, Group2), values_to = "Label") %>%
  distinct(Group, Label) %>%
  arrange(Group) %>%
  pull(Label)

# Create full grid while keeping group-wise structure
complete_grid <- expand.grid(Group1 = all_labels, Group2 = all_labels, stringsAsFactors = FALSE)

# Merge with actual data while preserving "Group" order
df_full <- complete_grid %>%
  left_join(df, by = c("Group1", "Group2"))

# **Step 2: LBd Reciprocal Values Properly**
df_reciprocal <- df_full %>%
  rename(Group1 = Group2, Group2 = Group1)  # Swap Group1 and Group2

# Ensure reciprocal pairs have the same Bonferroni values
df_reciprocal <- df_reciprocal %>%
  left_join(df_full %>% select(Group1, Group2, Bonferroni), by = c("Group1", "Group2")) %>%
  mutate(Bonferroni = coalesce(Bonferroni.x, Bonferroni.y)) %>%
  select(-Bonferroni.x, -Bonferroni.y)  # Keep only the updated Bonferroni values

df_full <- bind_rows(df_full, df_reciprocal) %>%
  distinct(Group1, Group2, .keep_all = TRUE)  # Ensure duplicates are kept

# Categorize Bonferroni values for coloring
df_full <- df_full %>%
  mutate(
    color = case_when(
      Bonferroni < 0.05 ~ "grey40",  # Use "grey40" explicitly
      Bonferroni >= 0.05 ~ "grey95",
      is.na(Bonferroni) ~ NA_character_
    )
  )

# Ensure levels preserve the correct group-wise order
df_full$Group1 <- factor(df_full$Group1, levels = all_labels)
df_full$Group2 <- factor(df_full$Group2, levels = rev(all_labels))  # Ensure full mirroring

# Extract Group1 and Group2 levels for correct coloring of axis labels
group1_levels <- levels(df_full$Group1)
group2_levels <- levels(df_full$Group2)

# Step 3: Plot checkerboard heatmap with FULL Reciprocal Values
library(ggplot2)
library(ggtext)
library(ggnewscale)

# Define color labels
color_labels <- c("p <0.05" = "grey40",
                  "p 0.05-0.1" = "grey70",
                  "p <0.1" = "grey95")

# Ensure missing values (NA) are properly handled
df_full$Bonferroni_cat <- cut(df_full$Bonferroni, 
                              breaks = c(-Inf, 0.05, 0.1, Inf), 
                              labels = names(color_labels))

# Convert NA values to explicit category for better visualization
df_full$Bonferroni_cat <- as.character(df_full$Bonferroni_cat)
df_full$Bonferroni_cat[is.na(df_full$Bonferroni)] <- "NA"

# LBd "NA" to the color scale so blank cells appear white
color_labels["NA"] <- "white"

# Create color mappings for text labels
group2_colors <- ifelse(grepl("ApoE3", df_full$Group2), "grey40", "black")
group1_colors <- ifelse(grepl("ApoE3", df_full$Group1), "grey40", "black")

# Create the heatmap plot
ggplot(df_full, aes(x = Group2, y = Group1, fill = Bonferroni_cat)) +
  geom_tile(color = "black") +
  geom_richtext(
    aes(label = ifelse(
      !is.na(Bonferroni),
      ifelse(Bonferroni < 0.01, "**<0.01**",  # Replace very low p-values with "<0.001"
             ifelse(Bonferroni < 0.05, paste0("**", sprintf("%.2f", Bonferroni), "**"), 
                    sprintf("%.2f", Bonferroni))),
      "")),  
    size = 8, fill = NA, label.color = NA,  
    color = ifelse(!is.na(df_full$Bonferroni) & df_full$Bonferroni < 0.05, "white", "black")  
  ) +
  scale_fill_manual(
    name = "Bonferroni P-Value",
    values = c(
      "p <0.05" = "grey40",
      "p 0.05-0.1" = "grey70",
      "p <0.1" = "grey95"
    ),
    breaks = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    labels = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    na.value = "white"  # Keeps blank cells white
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_markdown(angle = 90, hjust = 1, size = 14),  
    axis.text.y = element_markdown(angle = 0, hjust = 1, size = 14),
    panel.grid = element_blank()
  ) +
  labs(x = "", y = "", fill = "Bonferroni P-Value") +
  ggtitle("") +  
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +  
  coord_fixed()


Bonferroni_Heatmap_plot <-ggplot(df_full, aes(x = Group2, y = Group1, fill = Bonferroni_cat)) +
  geom_tile(color = "black") +
  geom_richtext(
    aes(label = ifelse(
      !is.na(Bonferroni),
      ifelse(Bonferroni < 0.01, "**<0.01**",  # Replace very low p-values with "<0.001"
             ifelse(Bonferroni < 0.05, paste0("**", sprintf("%.2f", Bonferroni), "**"), 
                    sprintf("%.2f", Bonferroni))),
      "")),  
    size = 8, fill = NA, label.color = NA,  
    color = ifelse(!is.na(df_full$Bonferroni) & df_full$Bonferroni < 0.05, "white", "black")  
  ) +
  scale_fill_manual(
    name = "Bonferroni P-Value",
    values = c(
      "p <0.05" = "grey40",
      "p 0.05-0.1" = "grey70",
      "p <0.1" = "grey95"
    ),
    breaks = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    labels = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    na.value = "white"  # Keeps blank cells white
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_markdown(angle = 90, hjust = 1, size = 14),  
    axis.text.y = element_markdown(angle = 0, hjust = 1, size = 14),
    panel.grid = element_blank()
  ) +
  labs(x = "", y = "", fill = "Bonferroni P-Value") +
  ggtitle("") +  
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +  
  coord_fixed()

# Step 5: Save the plot as a PNG
ggsave("Bonferroni_Heatmap_plot_ABETA_AT8_UPR75.png", 
       plot = Bonferroni_Heatmap_plot, width = 10, height = 8, dpi = 300)
Bonferroni_Heatmap_plot_ABETA_AT8_UPR75 <- Bonferroni_Heatmap_plot

##########################CHECKERBOARD_PLOT_ABC###################################
setwd(datadir)

final_results_ABC <- final_results_ABC %>%
  filter(Group1 != "Global" & Group2 != "All Groups") %>%
  mutate(
    Group = "GROUP",
    Group1 = case_when(
      Group1 == "ApoE3_AD_HIGH" ~ "ApoE3: H",
      Group1 == "ApoE3_AD_LOW"  ~ "ApoE3: N/L",
      Group1 == "ApoE4_AD_HIGH" ~ "ApoE4: H",
      Group1 == "ApoE4_AD_LOW"  ~ "ApoE4: N/L",
      TRUE ~ Group1  # Keep any other values unchanged
    ),
    Group2 = case_when(
      Group2 == "ApoE3_AD_HIGH" ~ "ApoE3: H",
      Group2 == "ApoE3_AD_LOW"  ~ "ApoE3: N/L",
      Group2 == "ApoE4_AD_HIGH" ~ "ApoE4: H",
      Group2 == "ApoE4_AD_LOW"  ~ "ApoE4: N/L",
      TRUE ~ Group2  # Keep any other values unchanged
    )
  )


combined_all_results <- bind_rows(final_results_ABC,)


# Step 1: LoLB the dataset
df <- combined_all_results %>%
  as.data.frame()
# Step 2: Duplicate all rows and swap `Group1` and `Group2` in the duplicated rows
df_reciprocal <- df %>%
  rename(Group1 = Group2, Group2 = Group1)  # Swap the values

# Step 3: Combine the original and reciprocal data
df_full <- bind_rows(df, df_reciprocal) %>%
  distinct(Group1, Group2, .keep_all = TRUE)  # Ensure no duplicates are removed

# Step 4: Display the final dataset
print(df_full)

df<- df_full

# Order variables within each "Group"
df <- df %>%
  group_by(Group) %>%
  arrange(Group, Group1, Group2, .by_group = TRUE)

# Get unique labels maintaining order within each "Group"
all_labels <- df %>%
  select(Group, Group1, Group2) %>%
  pivot_longer(cols = c(Group1, Group2), values_to = "Label") %>%
  distinct(Group, Label) %>%
  arrange(Group) %>%
  pull(Label)

# Create full grid while keeping group-wise structure
complete_grid <- expand.grid(Group1 = all_labels, Group2 = all_labels, stringsAsFactors = FALSE)

# Merge with actual data while preserving "Group" order
df_full <- complete_grid %>%
  left_join(df, by = c("Group1", "Group2"))

# **Step 2: LBd Reciprocal Values Properly**
df_reciprocal <- df_full %>%
  rename(Group1 = Group2, Group2 = Group1)  # Swap Group1 and Group2

# Ensure reciprocal pairs have the same Bonferroni values
df_reciprocal <- df_reciprocal %>%
  left_join(df_full %>% select(Group1, Group2, Bonferroni), by = c("Group1", "Group2")) %>%
  mutate(Bonferroni = coalesce(Bonferroni.x, Bonferroni.y)) %>%
  select(-Bonferroni.x, -Bonferroni.y)  # Keep only the updated Bonferroni values

df_full <- bind_rows(df_full, df_reciprocal) %>%
  distinct(Group1, Group2, .keep_all = TRUE)  # Ensure duplicates are kept

# Categorize Bonferroni values for coloring
df_full <- df_full %>%
  mutate(
    color = case_when(
      Bonferroni < 0.05 ~ "grey40",  # Use "grey40" explicitly
      Bonferroni >= 0.05 ~ "grey95",
      is.na(Bonferroni) ~ NA_character_
    )
  )

# Ensure levels preserve the correct group-wise order
df_full$Group1 <- factor(df_full$Group1, levels = all_labels)
df_full$Group2 <- factor(df_full$Group2, levels = rev(all_labels))  # Ensure full mirroring

# Extract Group1 and Group2 levels for correct coloring of axis labels
group1_levels <- levels(df_full$Group1)
group2_levels <- levels(df_full$Group2)

# Step 3: Plot checkerboard heatmap with FULL Reciprocal Values
library(ggplot2)
library(ggtext)
library(ggnewscale)

# Define color labels
color_labels <- c("p <0.05" = "grey40",
                  "p 0.05-0.1" = "grey70",
                  "p <0.1" = "grey95")

# Ensure missing values (NA) are properly handled
df_full$Bonferroni_cat <- cut(df_full$Bonferroni, 
                              breaks = c(-Inf, 0.05, 0.1, Inf), 
                              labels = names(color_labels))

# Convert NA values to explicit category for better visualization
df_full$Bonferroni_cat <- as.character(df_full$Bonferroni_cat)
df_full$Bonferroni_cat[is.na(df_full$Bonferroni)] <- "NA"

# LBd "NA" to the color scale so blank cells appear white
color_labels["NA"] <- "white"

# Create color mappings for text labels
group2_colors <- ifelse(grepl("ApoE3", df_full$Group2), "grey40", "black")
group1_colors <- ifelse(grepl("ApoE3", df_full$Group1), "grey40", "black")

# Create the heatmap plot
ggplot(df_full, aes(x = Group2, y = Group1, fill = Bonferroni_cat)) +
  geom_tile(color = "black") +
  geom_richtext(
    aes(label = ifelse(
      !is.na(Bonferroni),
      ifelse(Bonferroni < 0.01, "**<0.01**",  # Replace very low p-values with "<0.001"
             ifelse(Bonferroni < 0.05, paste0("**", sprintf("%.2f", Bonferroni), "**"), 
                    sprintf("%.2f", Bonferroni))),
      "")),  
    size = 8, fill = NA, label.color = NA,  
    color = ifelse(!is.na(df_full$Bonferroni) & df_full$Bonferroni < 0.05, "white", "black")  
  ) +
  scale_fill_manual(
    name = "Bonferroni P-Value",
    values = c(
      "p <0.05" = "grey40",
      "p 0.05-0.1" = "grey70",
      "p <0.1" = "grey95"
    ),
    breaks = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    labels = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    na.value = "white"  # Keeps blank cells white
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_markdown(angle = 90, hjust = 1, size = 14),  
    axis.text.y = element_markdown(angle = 0, hjust = 1, size = 14),
    panel.grid = element_blank()
  ) +
  labs(x = "", y = "", fill = "Bonferroni P-Value") +
  ggtitle("") +  
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +  
  coord_fixed()


Bonferroni_Heatmap_plot <-ggplot(df_full, aes(x = Group2, y = Group1, fill = Bonferroni_cat)) +
  geom_tile(color = "black") +
  geom_richtext(
    aes(label = ifelse(
      !is.na(Bonferroni),
      ifelse(Bonferroni < 0.01, "**<0.01**",  # Replace very low p-values with "<0.001"
             ifelse(Bonferroni < 0.05, paste0("**", sprintf("%.2f", Bonferroni), "**"), 
                    sprintf("%.2f", Bonferroni))),
      "")),  
    size = 8, fill = NA, label.color = NA,  
    color = ifelse(!is.na(df_full$Bonferroni) & df_full$Bonferroni < 0.05, "white", "black")  
  ) +
  scale_fill_manual(
    name = "Bonferroni P-Value",
    values = c(
      "p <0.05" = "grey40",
      "p 0.05-0.1" = "grey70",
      "p <0.1" = "grey95"
    ),
    breaks = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    labels = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    na.value = "white"  # Keeps blank cells white
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_markdown(angle = 90, hjust = 1, size = 14),  
    axis.text.y = element_markdown(angle = 0, hjust = 1, size = 14),
    panel.grid = element_blank()
  ) +
  labs(x = "", y = "", fill = "Bonferroni P-Value") +
  ggtitle("") +  
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +  
  coord_fixed()

# Step 5: Save the plot as a PNG
ggsave("Bonferroni_Heatmap_plot_ABC.png", 
       plot = Bonferroni_Heatmap_plot, width = 10, height = 8, dpi = 300)

Bonferroni_Heatmap_plot_ABC <- Bonferroni_Heatmap_plot


#############CHECKERBOARD_PLOT###################################
setwd(datadir)

final_results_ABC <- final_results_ABC %>%
  filter(Group1 != "Global" & Group2 != "All Groups") %>%
  mutate(
    Group = "2_ABC",
    Group1 = paste0(Group1, "_ABC"),
    Group2 = paste0(Group2, "_ABC")
  )


final_results_ABETA_AT8_UPR75 <- final_results_ABETA_AT8_UPR75 %>%
  filter(Group1 != "Global" & Group2 != "All Groups") %>%
  mutate(
    Group = "1_UPR75",
    Group1 = paste0(Group1, "_AD_75%Quartile"),
    Group2 = paste0(Group2, "_AD_75%Quartile")
  )

combined_all_results <- bind_rows(final_results_ABC, final_results_ABETA_AT8_UPR75)

# Print the updated combined dataframe
print(combined_all_results)

# Save the final dataframe as a CSV file
write.csv(combined_all_results, "combined_all_results_UPR75.csv", row.names = FALSE)
# Step 1: Load the dataset
df <- combined_all_results %>%
  as.data.frame()
# Step 2: Duplicate all rows and swap `Group1` and `Group2` in the duplicated rows
df_reciprocal <- df %>%
  rename(Group1 = Group2, Group2 = Group1)  # Swap the values

# Step 3: Combine the original and reciprocal data
df_full <- bind_rows(df, df_reciprocal) %>%
  distinct(Group1, Group2, .keep_all = TRUE)  # Ensure no duplicates are removed

# Step 4: Display the final dataset
print(df_full)

df<- df_full

# Order variables within each "Group"
df <- df %>%
  group_by(Group) %>%
  arrange(Group, Group1, Group2, .by_group = TRUE)

# Get unique labels maintaining order within each "Group"
all_labels <- df %>%
  select(Group, Group1, Group2) %>%
  pivot_longer(cols = c(Group1, Group2), values_to = "Label") %>%
  distinct(Group, Label) %>%
  arrange(Group) %>%
  pull(Label)

# Create full grid while keeping group-wise structure
complete_grid <- expand.grid(Group1 = all_labels, Group2 = all_labels, stringsAsFactors = FALSE)

# Merge with actual data while preserving "Group" order
df_full <- complete_grid %>%
  left_join(df, by = c("Group1", "Group2"))

# **Step 2: Add Reciprocal Values Properly**
df_reciprocal <- df_full %>%
  rename(Group1 = Group2, Group2 = Group1)  # Swap Group1 and Group2

# Ensure reciprocal pairs have the same Bonferroni values
df_reciprocal <- df_reciprocal %>%
  left_join(df_full %>% select(Group1, Group2, Bonferroni), by = c("Group1", "Group2")) %>%
  mutate(Bonferroni = coalesce(Bonferroni.x, Bonferroni.y)) %>%
  select(-Bonferroni.x, -Bonferroni.y)  # Keep only the updated Bonferroni values

df_full <- bind_rows(df_full, df_reciprocal) %>%
  distinct(Group1, Group2, .keep_all = TRUE)  # Ensure duplicates are kept

# Categorize Bonferroni values for coloring
df_full <- df_full %>%
  mutate(
    color = case_when(
      Bonferroni < 0.05 ~ "dodgerblue",  # Use "dodgerblue" explicitly
      Bonferroni >= 0.05 ~ "grey",
      is.na(Bonferroni) ~ NA_character_
    )
  )

# Ensure levels preserve the correct group-wise order
df_full$Group1 <- factor(df_full$Group1, levels = all_labels)
df_full$Group2 <- factor(df_full$Group2, levels = rev(all_labels))  # Ensure full mirroring

# Extract Group1 and Group2 levels for correct coloring of axis labels
group1_levels <- levels(df_full$Group1)
group2_levels <- levels(df_full$Group2)

# Step 3: Plot checkerboard heatmap with FULL Reciprocal Values
library(ggplot2)
library(ggtext)
library(ggnewscale)

# Define color labels
color_labels <- c("p <0.05" = "dodgerblue",
                  "p 0.05-0.1" = "lightskyblue3",
                  "p <0.1" = "grey")

# Ensure missing values (NA) are properly handled
df_full$Bonferroni_cat <- cut(df_full$Bonferroni, 
                              breaks = c(-Inf, 0.05, 0.1, Inf), 
                              labels = names(color_labels))

# Convert NA values to explicit category for better visualization
df_full$Bonferroni_cat <- as.character(df_full$Bonferroni_cat)
df_full$Bonferroni_cat[is.na(df_full$Bonferroni)] <- "NA"

# Add "NA" to the color scale so blank cells appear white
color_labels["NA"] <- "white"

# Create color mappings for text labels
group2_colors <- ifelse(grepl("ApoE3", df_full$Group2), "dodgerblue", "firebrick")
group1_colors <- ifelse(grepl("ApoE3", df_full$Group1), "dodgerblue", "firebrick")

# Create the heatmap plot
ggplot(df_full, aes(x = Group2, y = Group1, fill = Bonferroni_cat)) +
  geom_tile(color = "black") +
  geom_richtext(
    aes(label = ifelse(
      !is.na(Bonferroni),
      ifelse(Bonferroni < 0.01, "**<0.01**",  # Replace very low p-values with "<0.001"
             ifelse(Bonferroni < 0.05, paste0("**", sprintf("%.2f", Bonferroni), "**"), 
                    sprintf("%.2f", Bonferroni))),
      "")),  
    size = 8, fill = NA, label.color = NA,  
    color = ifelse(!is.na(df_full$Bonferroni) & df_full$Bonferroni < 0.05, "white", "black")  
  ) +
  scale_fill_manual(
    name = "Bonferroni P-Value",
    values = c(
      "p <0.05" = "dodgerblue",
      "p 0.05-0.1" = "lightskyblue3",
      "p <0.1" = "grey"
    ),
    breaks = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    labels = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    na.value = "white"  # Keeps blank cells white
  ) +
  scale_x_discrete(labels = function(x) { 
    ifelse(grepl("ApoE3", x), paste0("<span style='color:dodgerblue;'>", x, "</span>"),
           paste0("<span style='color:firebrick;'>", x, "</span>")) 
  }) +
  scale_y_discrete(labels = function(x) { 
    ifelse(grepl("ApoE3", x), paste0("<span style='color:dodgerblue;'>", x, "</span>"),
           paste0("<span style='color:firebrick;'>", x, "</span>")) 
  }) +
  theme_minimal() +
  theme(
    axis.text.x = element_markdown(angle = 45, hjust = 1, size = 14),  
    axis.text.y = element_markdown(angle = 0, hjust = 1, size = 14),
    panel.grid = element_blank()
  ) +
  labs(x = "Group2", y = "Group1", fill = "Bonferroni P-Value") +
  ggtitle("") +  
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +  
  coord_fixed()


Bonferroni_Heatmap_plot <-ggplot(df_full, aes(x = Group2, y = Group1, fill = Bonferroni_cat)) +
  geom_tile(color = "black") +
  geom_richtext(
    aes(label = ifelse(
      !is.na(Bonferroni),
      ifelse(Bonferroni < 0.01, "**<0.01**",  # Replace very low p-values with "<0.001"
             ifelse(Bonferroni < 0.05, paste0("**", sprintf("%.2f", Bonferroni), "**"), 
                    sprintf("%.2f", Bonferroni))),
      "")),   
    size = 8, fill = NA, label.color = NA,  
    color = ifelse(!is.na(df_full$Bonferroni) & df_full$Bonferroni < 0.05, "white", "black")  
  ) +
  scale_fill_manual(
    name = "Bonferroni P-Value",
    values = c(
      "p <0.05" = "dodgerblue",
      "p 0.05-0.1" = "lightskyblue3",
      "p <0.1" = "grey"
    ),
    breaks = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    labels = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    na.value = "white"  # Keeps blank cells white
  ) +
  scale_x_discrete(labels = function(x) { 
    ifelse(grepl("ApoE3", x), paste0("<span style='color:dodgerblue;'>", x, "</span>"),
           paste0("<span style='color:firebrick;'>", x, "</span>")) 
  }) +
  scale_y_discrete(labels = function(x) { 
    ifelse(grepl("ApoE3", x), paste0("<span style='color:dodgerblue;'>", x, "</span>"),
           paste0("<span style='color:firebrick;'>", x, "</span>")) 
  }) +
  theme_minimal() +
  theme(
    axis.text.x = element_markdown(angle = 90, hjust = 1, size = 14),  
    axis.text.y = element_markdown(angle = 0, hjust = 1, size = 14),
    panel.grid = element_blank()
  ) +
  labs(x = "Group2", y = "Group1", fill = "Bonferroni P-Value") +
  ggtitle("") +  
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +  
  coord_fixed()

# Step 5: Save the plot as a PNG
ggsave("Bonferroni_Heatmap_plot_ABETA_AT8_UPR75.png", 
       plot = Bonferroni_Heatmap_plot, width = 16, height = 14, dpi = 300)


#############################################################DIRECTORY##############################
setwd(dir2)
##########################COMBINED_PLOT_SEMIQUANT###################################


final_results_ABC <- final_results_ABC %>%
  filter(Group1 != "Global" & Group2 != "All Groups") %>%
  mutate(
    Group = "1_ABC",
    Group1 = paste0(Group1, ""),
    Group2 = paste0(Group2, "")
  )
final_results_Thal <- final_results_Thal %>%
  filter(Group1 != "Global" & Group2 != "All Groups") %>%
  mutate(
    Group = "2_Thal",
    Group1 = paste0(Group1, ""),
    Group2 = paste0(Group2, "")
  )
final_results_Braak_Braak<- final_results_Braak_Braak %>%
  filter(Group1 != "Global" & Group2 != "All Groups") %>%
  mutate(
    Group = "3_Braak_Braak",
    Group1 = paste0(Group1, ""),
    Group2 = paste0(Group2, "")
  )
final_results_Braak<- final_results_Braak %>%
  filter(Group1 != "Global" & Group2 != "All Groups") %>%
  mutate(
    Group = "4_Braak",
    Group1 = paste0(Group1, ""),
    Group2 = paste0(Group2, "")
  )
combined_all_results <- bind_rows(final_results_Braak, final_results_Braak_Braak, final_results_Thal, final_results_ABC)

# Print the updated combined dataframe
print(combined_all_results)

# Save the final dataframe as a CSV file
write.csv(combined_all_results, "combined_all_results_SemiQuant.csv", row.names = FALSE)
# Step 1: Load the dataset
df <- combined_all_results %>%
  as.data.frame()
# Step 2: Duplicate all rows and swap `Group1` and `Group2` in the duplicated rows
df_reciprocal <- df %>%
  rename(Group1 = Group2, Group2 = Group1)  # Swap the values

# Step 3: Combine the original and reciprocal data
df_full <- bind_rows(df, df_reciprocal) %>%
  distinct(Group1, Group2, .keep_all = TRUE)  # Ensure no duplicates are removed

# Step 4: Display the final dataset
print(df_full)

df<- df_full

# Order variables within each "Group"
df <- df %>%
  group_by(Group) %>%
  arrange(Group, Group1, Group2, .by_group = TRUE)

# Get unique labels maintaining order within each "Group"
all_labels <- df %>%
  select(Group, Group1, Group2) %>%
  pivot_longer(cols = c(Group1, Group2), values_to = "Label") %>%
  distinct(Group, Label) %>%
  arrange(Group) %>%
  pull(Label)

# Create full grid while keeping group-wise structure
complete_grid <- expand.grid(Group1 = all_labels, Group2 = all_labels, stringsAsFactors = FALSE)

# Merge with actual data while preserving "Group" order
df_full <- complete_grid %>%
  left_join(df, by = c("Group1", "Group2"))

# **Step 2: Add Reciprocal Values Properly**
df_reciprocal <- df_full %>%
  rename(Group1 = Group2, Group2 = Group1)  # Swap Group1 and Group2

# Ensure reciprocal pairs have the same Bonferroni values
df_reciprocal <- df_reciprocal %>%
  left_join(df_full %>% select(Group1, Group2, Bonferroni), by = c("Group1", "Group2")) %>%
  mutate(Bonferroni = coalesce(Bonferroni.x, Bonferroni.y)) %>%
  select(-Bonferroni.x, -Bonferroni.y)  # Keep only the updated Bonferroni values

df_full <- bind_rows(df_full, df_reciprocal) %>%
  distinct(Group1, Group2, .keep_all = TRUE)  # Ensure duplicates are kept

# Categorize Bonferroni values for coloring
df_full <- df_full %>%
  mutate(
    color = case_when(
      Bonferroni < 0.05 ~ "gray20",  # Use "dodgerblue" explicitly
      Bonferroni >= 0.05 ~ "gray95",
      is.na(Bonferroni) ~ NA_character_
    )
  )

# Ensure levels preserve the correct group-wise order
df_full$Group1 <- factor(df_full$Group1, levels = all_labels)
df_full$Group2 <- factor(df_full$Group2, levels = rev(all_labels))  # Ensure full mirroring

# Extract Group1 and Group2 levels for correct coloring of axis labels
group1_levels <- levels(df_full$Group1)
group2_levels <- levels(df_full$Group2)

# Step 3: Plot checkerboard heatmap with FULL Reciprocal Values
library(ggplot2)
library(ggtext)
library(ggnewscale)

# Define color labels
color_labels <- c("p <0.05" = "gray20",
                  "p 0.05-0.1" = "grey70",
                  "p <0.1" = "gray95")

# Ensure missing values (NA) are properly handled
df_full$Bonferroni_cat <- cut(df_full$Bonferroni, 
                              breaks = c(-Inf, 0.05, 0.1, Inf), 
                              labels = names(color_labels))

# Convert NA values to explicit category for better visualization
df_full$Bonferroni_cat <- as.character(df_full$Bonferroni_cat)
df_full$Bonferroni_cat[is.na(df_full$Bonferroni)] <- "NA"

# Add "NA" to the color scale so blank cells appear white
color_labels["NA"] <- "white"

# Create color mappings for text labels
group2_colors <- ifelse(grepl("ApoE3", df_full$Group2), "black", "black")
group1_colors <- ifelse(grepl("ApoE3", df_full$Group1), "black", "black")

# Create the heatmap plot
ggplot(df_full, aes(x = Group2, y = Group1, fill = Bonferroni_cat)) +
  geom_tile(color = "black") +
  geom_richtext(
    aes(label = ifelse(
      !is.na(Bonferroni),
      ifelse(Bonferroni < 0.01, "**<0.01**",  # Replace very low p-values with "<0.001"
             ifelse(Bonferroni < 0.05, paste0("**", sprintf("%.2f", Bonferroni), "**"), 
                    sprintf("%.2f", Bonferroni))),
      "")),    
    size = 8, fill = NA, label.color = NA,  
    color = ifelse(!is.na(df_full$Bonferroni) & df_full$Bonferroni < 0.05, "white", "black")  
  ) +
  scale_fill_manual(
    name = "Bonferroni P-Value",
    values = c(
      "p <0.05" = "gray20",
      "p 0.05-0.1" = "grey70",
      "p <0.1" = "gray95"
    ),
    breaks = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    labels = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    na.value = "white"  # Keeps blank cells white
  ) +
  scale_x_discrete(labels = function(x) { 
    ifelse(grepl("ApoE3", x), paste0("<span style='color:black;'>", x, "</span>"),
           paste0("<span style='color:black;'>", x, "</span>")) 
  }) +
  scale_y_discrete(labels = function(x) { 
    ifelse(grepl("ApoE3", x), paste0("<span style='color:black;'>", x, "</span>"),
           paste0("<span style='color:black;'>", x, "</span>")) 
  }) +
  theme_minimal() +
  theme(
    axis.text.x = element_markdown(angle = 45, hjust = 1, size = 14),  
    axis.text.y = element_markdown(angle = 0, hjust = 1, size = 14),
    panel.grid = element_blank()
  ) +
  labs(x = "Group2", y = "Group1", fill = "Bonferroni P-Value") +
  ggtitle("") +  
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +  
  coord_fixed()


Bonferroni_Heatmap_plot_semiquant <-ggplot(df_full, aes(x = Group2, y = Group1, fill = Bonferroni_cat)) +
  geom_tile(color = "black") +
  geom_richtext(
    aes(label = ifelse(
      !is.na(Bonferroni),
      ifelse(Bonferroni < 0.01, "**<0.01**",  # Replace very low p-values with "<0.001"
             ifelse(Bonferroni < 0.05, paste0("**", sprintf("%.2f", Bonferroni), "**"), 
                    sprintf("%.2f", Bonferroni))),
      "")),    
    size = 8, fill = NA, label.color = NA,  
    color = ifelse(!is.na(df_full$Bonferroni) & df_full$Bonferroni < 0.05, "white", "black")  
  ) +
  scale_fill_manual(
    name = "Bonferroni P-Value",
    values = c(
      "p <0.05" = "grey20",
      "p 0.05-0.1" = "grey70",
      "p <0.1" = "gray95"
    ),
    breaks = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    labels = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    na.value = "white"  # Keeps blank cells white
  ) +
  scale_x_discrete(labels = function(x) { 
    ifelse(grepl("ApoE3", x), paste0("<span style='color:black;'>", x, "</span>"),
           paste0("<span style='color:black;'>", x, "</span>")) 
  }) +
  scale_y_discrete(labels = function(x) { 
    ifelse(grepl("ApoE3", x), paste0("<span style='color:black;'>", x, "</span>"),
           paste0("<span style='color:black;'>", x, "</span>")) 
  }) +
  theme_minimal() +
  theme(
    axis.text.x = element_markdown(angle = 90, hjust = 1, size = 14),  
    axis.text.y = element_markdown(angle = 0, hjust = 1, size = 14),
    panel.grid = element_blank()
  ) +
  labs(x = "Group2", y = "Group1", fill = "Bonferroni P-Value") +
  ggtitle("Bonferroni-corrected p-value Heatmap: Pathological Staging") +  
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +  
  coord_fixed()

# Step 5: Save the plot as a PNG
ggsave("Combined_Bonferroni_Heatmap_plot_SemiQuant.png", 
       plot = Bonferroni_Heatmap_plot_semiquant, width = 16, height = 14, dpi = 300)


##########################COMBINED_PLOT_QUANT###################################
setwd(dir2)
final_results_ABETA_AT8_UPR75 <- final_results_ABETA_AT8_UPR75 %>%
  filter(Group1 != "Global" & Group2 != "All Groups") %>%
  mutate(
    Group = "1_ABC",
    Group1 = paste0(Group1, ""),
    Group2 = paste0(Group2, "")
  )
final_results_ABETA_UPR75 <- final_results_ABETA_UPR75 %>%
  filter(Group1 != "Global" & Group2 != "All Groups") %>%
  mutate(
    Group = "2_Thal",
    Group1 = paste0(Group1, ""),
    Group2 = paste0(Group2, "")
  )
final_results_AT8_UPR85<- final_results_AT8_UPR85 %>%
  filter(Group1 != "Global" & Group2 != "All Groups") %>%
  mutate(
    Group = "3_Braak_Braak",
    Group1 = paste0(Group1, ""),
    Group2 = paste0(Group2, "")
  )
final_results_ASYN_UPR75<- final_results_ASYN_UPR75 %>%
  filter(Group1 != "Global" & Group2 != "All Groups") %>%
  mutate(
    Group = "4_Braak",
    Group1 = paste0(Group1, ""),
    Group2 = paste0(Group2, "")
  )
combined_all_results <- bind_rows(final_results_ASYN_UPR75, final_results_AT8_UPR85, final_results_ABETA_UPR75, final_results_ABETA_AT8_UPR75)

# Print the updated combined dataframe
print(combined_all_results)

# Save the final dataframe as a CSV file
write.csv(combined_all_results, "combined_all_results_Quant.csv", row.names = FALSE)
# Step 1: Load the dataset
df <- combined_all_results %>%
  as.data.frame()
# Step 2: Duplicate all rows and swap `Group1` and `Group2` in the duplicated rows
df_reciprocal <- df %>%
  rename(Group1 = Group2, Group2 = Group1)  # Swap the values

# Step 3: Combine the original and reciprocal data
df_full <- bind_rows(df, df_reciprocal) %>%
  distinct(Group1, Group2, .keep_all = TRUE)  # Ensure no duplicates are removed

# Step 4: Display the final dataset
print(df_full)

df<- df_full

# Order variables within each "Group"
df <- df %>%
  group_by(Group) %>%
  arrange(Group, Group1, Group2, .by_group = TRUE)

# Get unique labels maintaining order within each "Group"
all_labels <- df %>%
  select(Group, Group1, Group2) %>%
  pivot_longer(cols = c(Group1, Group2), values_to = "Label") %>%
  distinct(Group, Label) %>%
  arrange(Group) %>%
  pull(Label)

# Create full grid while keeping group-wise structure
complete_grid <- expand.grid(Group1 = all_labels, Group2 = all_labels, stringsAsFactors = FALSE)

# Merge with actual data while preserving "Group" order
df_full <- complete_grid %>%
  left_join(df, by = c("Group1", "Group2"))

# **Step 2: Add Reciprocal Values Properly**
df_reciprocal <- df_full %>%
  rename(Group1 = Group2, Group2 = Group1)  # Swap Group1 and Group2

# Ensure reciprocal pairs have the same Bonferroni values
df_reciprocal <- df_reciprocal %>%
  left_join(df_full %>% select(Group1, Group2, Bonferroni), by = c("Group1", "Group2")) %>%
  mutate(Bonferroni = coalesce(Bonferroni.x, Bonferroni.y)) %>%
  select(-Bonferroni.x, -Bonferroni.y)  # Keep only the updated Bonferroni values

df_full <- bind_rows(df_full, df_reciprocal) %>%
  distinct(Group1, Group2, .keep_all = TRUE)  # Ensure duplicates are kept

# Categorize Bonferroni values for coloring
df_full <- df_full %>%
  mutate(
    color = case_when(
      Bonferroni < 0.05 ~ "grey20",  # Use "dodgerblue" explicitly
      Bonferroni >= 0.05 ~ "grey95",
      is.na(Bonferroni) ~ NA_character_
    )
  )

# Ensure levels preserve the correct group-wise order
df_full$Group1 <- factor(df_full$Group1, levels = all_labels)
df_full$Group2 <- factor(df_full$Group2, levels = rev(all_labels))  # Ensure full mirroring

# Extract Group1 and Group2 levels for correct coloring of axis labels
group1_levels <- levels(df_full$Group1)
group2_levels <- levels(df_full$Group2)

# Step 3: Plot checkerboard heatmap with FULL Reciprocal Values
library(ggplot2)
library(ggtext)
library(ggnewscale)

# Define color labels
color_labels <- c("p <0.05" = "grey20",
                  "p 0.05-0.1" = "grey70",
                  "p <0.1" = "grey95")

# Ensure missing values (NA) are properly handled
df_full$Bonferroni_cat <- cut(df_full$Bonferroni, 
                              breaks = c(-Inf, 0.05, 0.1, Inf), 
                              labels = names(color_labels))

# Convert NA values to explicit category for better visualization
df_full$Bonferroni_cat <- as.character(df_full$Bonferroni_cat)
df_full$Bonferroni_cat[is.na(df_full$Bonferroni)] <- "NA"

# Add "NA" to the color scale so blank cells appear white
color_labels["NA"] <- "white"

# Create color mappings for text labels
group2_colors <- ifelse(grepl("ApoE3", df_full$Group2), "black", "black")
group1_colors <- ifelse(grepl("ApoE3", df_full$Group1), "black", "black")

# Create the heatmap plot
ggplot(df_full, aes(x = Group2, y = Group1, fill = Bonferroni_cat)) +
  geom_tile(color = "black") +
  geom_richtext(
    aes(label = ifelse(
      !is.na(Bonferroni),
      ifelse(Bonferroni < 0.01, "**<0.01**",  # Replace very low p-values with "<0.001"
             ifelse(Bonferroni < 0.05, paste0("**", sprintf("%.2f", Bonferroni), "**"), 
                    sprintf("%.2f", Bonferroni))),
      "")),   
    size = 8, fill = NA, label.color = NA,  
    color = ifelse(!is.na(df_full$Bonferroni) & df_full$Bonferroni < 0.05, "white", "black")  
  ) +
  scale_fill_manual(
    name = "Bonferroni P-Value",
    values = c(
      "p <0.05" = "grey20",
      "p 0.05-0.1" = "grey70",
      "p <0.1" = "grey95"
    ),
    breaks = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    labels = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    na.value = "white"  # Keeps blank cells white
  ) +
  scale_x_discrete(labels = function(x) { 
    ifelse(grepl("ApoE3", x), paste0("<span style='color:black;'>", x, "</span>"),
           paste0("<span style='color:black;'>", x, "</span>")) 
  }) +
  scale_y_discrete(labels = function(x) { 
    ifelse(grepl("ApoE3", x), paste0("<span style='color:black;'>", x, "</span>"),
           paste0("<span style='color:black;'>", x, "</span>")) 
  }) +
  theme_minimal() +
  theme(
    axis.text.x = element_markdown(angle = 45, hjust = 1, size = 14),  
    axis.text.y = element_markdown(angle = 0, hjust = 1, size = 14),
    panel.grid = element_blank()
  ) +
  labs(x = "Group2", y = "Group1", fill = "Bonferroni P-Value") +
  ggtitle("") +  
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +  
  coord_fixed()


Bonferroni_Heatmap_plot_quant <-ggplot(df_full, aes(x = Group2, y = Group1, fill = Bonferroni_cat)) +
  geom_tile(color = "black") +
  geom_richtext(
    aes(label = ifelse(
      !is.na(Bonferroni),
      ifelse(Bonferroni < 0.01, "**<0.01**",  # Replace very low p-values with "<0.001"
             ifelse(Bonferroni < 0.05, paste0("**", sprintf("%.2f", Bonferroni), "**"), 
                    sprintf("%.2f", Bonferroni))),
      "")),   
    size = 8, fill = NA, label.color = NA,  
    color = ifelse(!is.na(df_full$Bonferroni) & df_full$Bonferroni < 0.05, "white", "black")  
  ) +
  scale_fill_manual(
    name = "Bonferroni P-Value",
    values = c(
      "p <0.05" = "grey20",
      "p 0.05-0.1" = "grey70",
      "p <0.1" = "grey95"
    ),
    breaks = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    labels = c("p <0.05", "p 0.05-0.1", "p <0.1"),
    na.value = "white"  # Keeps blank cells white
  ) +
  scale_x_discrete(labels = function(x) { 
    ifelse(grepl("ApoE3", x), paste0("<span style='color:black;'>", x, "</span>"),
           paste0("<span style='color:black;'>", x, "</span>")) 
  }) +
  scale_y_discrete(labels = function(x) { 
    ifelse(grepl("ApoE3", x), paste0("<span style='color:black;'>", x, "</span>"),
           paste0("<span style='color:black;'>", x, "</span>")) 
  }) +
  theme_minimal() +
  theme(
    axis.text.x = element_markdown(angle = 90, hjust = 1, size = 14),  
    axis.text.y = element_markdown(angle = 0, hjust = 1, size = 14),
    panel.grid = element_blank()
  ) +
  labs(x = "Group2", y = "Group1", fill = "Bonferroni P-Value") +
  ggtitle("Bonferroni-corrected p-value Heatmap: Quantitative pathology") +  
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +  
  coord_fixed()

# Step 5: Save the plot as a PNG
ggsave("Combined_Bonferroni_Heatmap_plot_Quant.png", 
       plot = Bonferroni_Heatmap_plot_quant, width = 16, height = 14, dpi = 300)



######################COMBINED SUMMARY PROP BARPLOT SEMIQUANT########
# Define custom order for ApoE_Group
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

# Function to prepare data for plotting
prepare_data <- function(data) {
  data <- data %>%
    mutate(Proportion_No_Dementia = 1 - Proportion_Dementia_1) %>%
    pivot_longer(cols = c(Proportion_Dementia_1, Proportion_No_Dementia),
                 names_to = "Dementia_Status",
                 values_to = "Proportion") %>%
    mutate(Dementia_Status = factor(Dementia_Status,
                                    levels = c("Proportion_No_Dementia", "Proportion_Dementia_1"),
                                    labels = c("No Dementia", "Dementia")))
  
  return(data)
}
custom_order <- rev(c("ApoE4_LB_LOW", "ApoE4_LB_HIGH", "ApoE3_LB_LOW", "ApoE3_LB_HIGH", 
                      "ApoE4_AT8_LOW", "ApoE4_AT8_HIGH", "ApoE3_AT8_LOW", "ApoE3_AT8_HIGH",
                      "ApoE4_ABETA_LOW", "ApoE4_ABETA_HIGH", "ApoE3_ABETA_LOW", "ApoE3_ABETA_HIGH", 
                      "ApoE4_AD_LOW", "ApoE4_AD_HIGH", "ApoE3_AD_LOW", "ApoE3_AD_HIGH"))


# Process all datasets
summary_prop_data_ABC_long <- prepare_data(summary_prop_data_ABC)
summary_prop_data_Thal_long <- prepare_data(summary_prop_data_Thal)
summary_prop_data_Braak_Braak_long <- prepare_data(summary_prop_data_Braak_Braak)
summary_prop_data_Braak_long <- prepare_data(summary_prop_data_Braak)

# Combine all data
combined_data_semiquant <- bind_rows(summary_prop_data_ABC_long, 
                                     summary_prop_data_Thal_long, 
                                     summary_prop_data_Braak_Braak_long, 
                                     summary_prop_data_Braak_long)

# Create the combined stacked bar plot
# Reorder the fill variable
combined_data_semiquant$Dementia_Status <- factor(combined_data_semiquant$Dementia_Status, levels = c("Dementia", "No Dementia"))

combined_barplot_semiquant <- ggplot(combined_data_semiquant, aes(x = ApoE_Group, y = Proportion, fill = Dementia_Status)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(
    label = ifelse(Proportion == 0, "", paste0(round(Proportion * 100, 1), "%")),
    color = Dementia_Status  # Set text color based on Dementia_Status
  ), 
  position = position_stack(vjust = 0.5), 
  size = 2.5)+  # Adjust size if needed
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "", fill = "Dementia Status") +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  ) +
  scale_fill_manual(values = c("Dementia" = "#7E3E94", "No Dementia" = "#DBBDE3")) +  # Custom bar colors
  scale_color_manual(values = c("Dementia" = "black", "No Dementia" = "black"), guide = "none") +  # Fix text colors & remove legend
  coord_flip()  # Flip the axes

# Save and display the plot
ggsave("combined_summary_plot_semiquant.png", combined_barplot_semiquant, width = 6, height = 8, dpi = 300)
print(combined_barplot_semiquant)

######################COMBINED SUMMARY PROP BARPLOT QUANT########
# Define custom order for ApoE_Group
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

# Function to prepare data for plotting
prepare_data <- function(data) {
  data <- data %>%
    mutate(Proportion_No_Dementia = 1 - Proportion_Dementia_1) %>%
    pivot_longer(cols = c(Proportion_Dementia_1, Proportion_No_Dementia),
                 names_to = "Dementia_Status",
                 values_to = "Proportion") %>%
    mutate(Dementia_Status = factor(Dementia_Status,
                                    levels = c("Proportion_No_Dementia", "Proportion_Dementia_1"),
                                    labels = c("No Dementia", "Dementia")))
  
  return(data)
}
custom_order <- rev(c("ApoE4_LB_LOW", "ApoE4_LB_HIGH", "ApoE3_LB_LOW", "ApoE3_LB_HIGH", 
                      "ApoE4_AT8_LOW", "ApoE4_AT8_HIGH", "ApoE3_AT8_LOW", "ApoE3_AT8_HIGH",
                      "ApoE4_ABETA_LOW", "ApoE4_ABETA_HIGH", "ApoE3_ABETA_LOW", "ApoE3_ABETA_HIGH", 
                      "ApoE4_AD_LOW", "ApoE4_AD_HIGH", "ApoE3_AD_LOW", "ApoE3_AD_HIGH"))


# Process all datasets
summary_prop_data_UPR75_comb_long <- prepare_data(summary_prop_data_UPR75_comb)
summary_prop_data_UPR75_long <- prepare_data(summary_prop_data_UPR75)
summary_prop_data_UPR85_long <- prepare_data(summary_prop_data_UPR85)
summary_prop_data_UPR75_ASYN <- prepare_data(summary_prop_data_UPR75_ASYN)

# Combine all data
combined_data_quant <- bind_rows(summary_prop_data_UPR75_comb_long, 
                                 summary_prop_data_UPR75_long, 
                                 summary_prop_data_UPR85_long, 
                                 summary_prop_data_UPR75_ASYN)
# Reorder the fill variable
combined_data_quant$Dementia_Status <- factor(combined_data_quant$Dementia_Status, levels = c("Dementia", "No Dementia"))
# Create the combined stacked bar plot
combined_barplot_quant <- ggplot(combined_data_quant, aes(x = ApoE_Group, y = Proportion, fill = Dementia_Status)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(
    label = ifelse(Proportion == 0, "", paste0(round(Proportion * 100, 1), "%")),
    color = Dementia_Status  # Set text color based on Dementia_Status
  ), 
  position = position_stack(vjust = 0.5), 
  size = 2.5) +  # Adjust size if needed
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "Proportion (%)", fill = "Dementia Status") +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  ) +
  scale_fill_manual(values = c("Dementia" = "#7E3E94", "No Dementia" = "#DBBDE3")) +  # Custom bar colors
  scale_color_manual(values = c("Dementia" = "black", "No Dementia" = "black"), guide = "none") +  # Fix text colors & remove legend
  coord_flip()  # Flip the axes

# Save and display the plot
ggsave("combined_summary_plot_quant.png", combined_barplot_quant, width = 6, height = 8, dpi = 300)
print(combined_barplot_quant)



######################COMBINED SUMMARY PROP BARPLOT & HEATMAP#################

library(ggplot2)
library(patchwork)

# Arrange plots into a properly aligned 4x4 grid
grid_plot <- wrap_plots(
  Bonferroni_Heatmap_plot_ASYN_Braak  , Bonferroni_Heatmap_plot_ASYN_UPR75  , summary_prop_data_Braak_plot  , summary_prop_data_UPR75_ASYN_plot,
  Bonferroni_Heatmap_plot_ABETA_Thal, Bonferroni_Heatmap_plot_ABETA_UPR75  , summary_prop_data_Thal_plot  , summary_prop_data_UPR75_plot,
  Bonferroni_Heatmap_plot_AT8_Braak_Braak  , Bonferroni_Heatmap_plot_AT8_UPR85  , summary_prop_data_Braak_Braak_plot , summary_prop_data_UPR85_plot,
  Bonferroni_Heatmap_plot_ABC ,   Bonferroni_Heatmap_plot_ABETA_AT8_UPR75, summary_prop_data_ABC_plot  , summary_prop_data_UPR75_comb_plot,
  ncol = 4, nrow = 4
) +  
  plot_layout(widths = c(1,1,.8,.8), heights = rep(1,4), guides = "collect") &  # **Force equal column widths & row heights**
  theme(
    legend.position = "none",      # Remove all legends
    axis.title.y = element_blank(), # Remove y-axis labels everywhere initially
    axis.text.y = element_blank(),  # Remove y-axis text
    axis.ticks.y = element_blank(), # Remove y-axis ticks
    axis.title.x = element_blank(), # Remove x-axis labels everywhere initially
    axis.text.x = element_blank(),  # Remove x-axis text
    axis.ticks.x = element_blank(), # Remove x-axis ticks
    plot.margin = margin(0, 0, 0, 0),  # Reduce white space around plots
    panel.spacing = unit(0.05, "npc"),  # **Force minimal spacing between columns**# Reduces the gaps between columns
    panel.background = element_rect(fill = "transparent", color = NA),  # **Transparent background**
    plot.background = element_rect(fill = "transparent", color = NA)
  )

# Restore y-axis labels **ONLY** for the **left-most column**
for (i in seq(1, 13, 4)) { 
  grid_plot[[i]] <- grid_plot[[i]] + theme(
    axis.title.y = element_blank(),  
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank()
  )
}

# Restore x-axis labels **ONLY** for the **bottom row**
for (i in 13:14) {  
  grid_plot[[i]] <- grid_plot[[i]] + theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
}
# Display the final **4x4** faceted grid plot
print(grid_plot)

# Save the final version
ggsave("final_facet_grid_4x4_reduced_right_cols.png", grid_plot, width = 16, height = 16, dpi = 300)


######################COMBINED SUMMARY PROP BARPLOT & HEATMAP2#################

library(ggplot2)
library(patchwork)

# Arrange plots into a properly aligned 4x4 grid
grid_plot <- wrap_plots(
  summary_prop_data_Braak_plot  , summary_prop_data_UPR75_ASYN_plot,Bonferroni_Heatmap_plot_ASYN_Braak  , Bonferroni_Heatmap_plot_ASYN_UPR75  , 
  summary_prop_data_Thal_plot  , summary_prop_data_UPR75_plot,Bonferroni_Heatmap_plot_ABETA_Thal, Bonferroni_Heatmap_plot_ABETA_UPR75  , 
  summary_prop_data_Braak_Braak_plot , summary_prop_data_UPR85_plot,Bonferroni_Heatmap_plot_AT8_Braak_Braak  , Bonferroni_Heatmap_plot_AT8_UPR85  , 
  summary_prop_data_ABC_plot  , summary_prop_data_UPR75_comb_plot,Bonferroni_Heatmap_plot_ABC ,   Bonferroni_Heatmap_plot_ABETA_AT8_UPR75, 
  ncol = 4, nrow = 4
) +  
  plot_layout(widths = c(1,1,.8,.8), heights = rep(1,4), guides = "collect") &  # **Force equal column widths & row heights**
  theme(
    legend.position = "none",      # Remove all legends
    axis.title.y = element_blank(), # Remove y-axis labels everywhere initially
    axis.text.y = element_blank(),  # Remove y-axis text
    axis.ticks.y = element_blank(), # Remove y-axis ticks
    axis.title.x = element_blank(), # Remove x-axis labels everywhere initially
    axis.text.x = element_blank(),  # Remove x-axis text
    axis.ticks.x = element_blank(), # Remove x-axis ticks
    plot.margin = margin(0, 0, 0, 0),  # Reduce white space around plots
    panel.spacing = unit(0.05, "npc"),  # **Force minimal spacing between columns**# Reduces the gaps between columns
    panel.background = element_rect(fill = "transparent", color = NA),  # **Transparent background**
    plot.background = element_rect(fill = "transparent", color = NA)
  )

# Restore y-axvis labels **ONLY** for the **left-most column**
for (i in seq(1, 13, 4)) { 
  grid_plot[[i]] <- grid_plot[[i]] + theme(
    axis.title.y = element_blank(),  
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank()
  )
}

# Restore x-axis labels **ONLY** for the **bottom row**
for (i in 15:16) {  
  grid_plot[[i]] <- grid_plot[[i]] + theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
}
# Display the final **4x4** faceted grid plot
print(grid_plot)

# Save the final version
ggsave("final_facet_grid_4x4_reduced_right_cols_2.png", grid_plot, width = 16, height = 16, dpi = 300)


####