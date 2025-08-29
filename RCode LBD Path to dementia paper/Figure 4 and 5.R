
#MULTIVARIATE LOGISTICAL REGRESSUION#####################
# Load required libraries
library(readxl)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(FactoMineR)
library(factoextra)
library(dplyr)
library(ggeffects)
library(gtsummary)
library(flextable)

# Set directories
setwd("DIR")
datadir <- "DIR"
dir.create(datadir, showWarnings = FALSE)
setwd(datadir)

# Load data
ex <- read_excel("EXCEL", 
                 sheet = "SHEET")

excel <- as.data.frame(ex)
excel_main <- excel
########### Bin variables: Duration>15 and Age_at_Death>77 
# Load dplyr package
library(dplyr)

# Filter the dataframe to remove rows where ApoE and INFARCT have NA values
# Ensure no missing values
excel_main <- excel_main %>% filter(!is.na(Duration))

# Calculate the median of Duration
median_duration <- median(excel_main$Duration, na.rm = TRUE, 0)  
print(median_duration)
# Bin the values at the median
excel_main$Duration_Binned <- cut(excel_main$Duration, 
                                  breaks = c(-Inf, median_duration, Inf),  
                                  labels = c(paste0("< ", median_duration, "yrs"), 
                                             paste0("≥ ", median_duration, "yrs")),  # Use median in labels
                                  include.lowest = TRUE, right = FALSE)  # left-inclusive binning

table(excel_main$Duration_Binned)


# Ensure no missing values
excel_main <- excel_main %>% filter(!is.na(Age_at_Death))

# Calculate the median of Age_at_Death
median_Age_at_Death <- median(excel_main$Age_at_Death, na.rm = TRUE)  
print(median_Age_at_Death)
# Bin the values at the median
excel_main$Age_at_Death_Binned <- cut(excel_main$Age_at_Death, 
                                      breaks = c(-Inf, median_Age_at_Death, Inf),  
                                      labels = c(paste0("< ", median_Age_at_Death, "yrs"), 
                                                 paste0("≥ ", median_Age_at_Death, "yrs")),  # Use median in labels
                                      include.lowest = TRUE, right = FALSE)  # left-inclusive binning

table(excel_main$Age_at_Death_Binned)


# Convert categorical variables to factors
excel_main$Duration_Binned <- as.factor(excel_main$Duration_Binned)
excel_main$Age_at_Death_Binned <- as.factor(excel_main$Age_at_Death_Binned)
excel_main$INFARCT <- as.factor(excel_main$INFARCT)
excel_main$ApoE <- as.factor(excel_main$ApoE)

# View first few rows to check binning
head(excel_main[, c("Duration", "Duration_Binned", "Age_at_Death", "Age_at_Death_Binned")])

# View first few rows to check binning
head(excel_main)

####################################################FULL_MODEL
UPPR_DIR <- paste0(datadir,"/FULL_MODEL")
dir.create(UPPR_DIR)
setwd(UPPR_DIR)

excel_main <- subset(excel_main, select = c(DEMENTIA, ApoE, ASYN_FRNT_HIPP, AT8_FRNT_HIPP, ABETA_FRNT_HIPP, INFARCT, Duration_Binned, Age_at_Death_Binned, BIOLOGICAL_SEX))



# Define predictors and outcome
predictors <- c("ASYN_FRNT_HIPP", "AT8_FRNT_HIPP", "ABETA_FRNT_HIPP", "INFARCT", "Duration_Binned", "Age_at_Death_Binned", "BIOLOGICAL_SEX")
outcome <- "DEMENTIA"

# Ensure outcome variable is a factor
excel_main[[outcome]] <- as.factor(excel_main[[outcome]])
####################################################APOE
# Manually define x-axis limits for continuous predictors
xlims_list <- list(
  "ASYN_FRNT_HIPP" = c(0, 50),
  "ABETA_FRNT_HIPP" = c(0, 8),
  "AT8_FRNT_HIPP"   = c(0, 15)
)

# Run the analysis separately for ApoE3 and ApoE4
for (apoe_type in c("ApoE3", "ApoE4")) {
  # Filter dataset for current ApoE type
  
  #apoe_type <- "ApoE3"
  
  subset_data <- excel_main %>% filter(ApoE == apoe_type)
  
  # Train logistic regression model
  model_base <- glm(DEMENTIA ~ ., data = subset_data[, c(predictors, outcome)], family = "binomial")
  library(car)
  vif(model_base)
  
  library(sjPlot)
  library(sjmisc)
  library(ggeffects)
  library(cowplot)
  library(RColorBrewer)
  library(ggplot2)
  
  # Generate marginal effects for all predictors
  effects_list <- ggpredict(model_base)  # Get predicted probabilities
  plot_order <- c(
    "ASYN_FRNT_HIPP", "ABETA_FRNT_HIPP", "AT8_FRNT_HIPP",
    "BIOLOGICAL_SEX", "INFARCT", "Duration_Binned", "Age_at_Death_Binned"
  )
  
  effects_list <- effects_list[plot_order]
  
  # Define categorical variables
  categorical_vars <- c("INFARCT", "Age_at_Death_Binned", "Duration_Binned", "BIOLOGICAL_SEX")
  
  # Define colors for continuous variable lines
  continuous_colors <- c("α-synuclein" = "forestgreen",
                         "Aβ" = "firebrick",
                         "pTau" = "dodgerblue")
  
  # Define x-axis label mapping
  x_axis_labels <- c(
    "ASYN_FRNT_HIPP" = "α-synuclein",
    "ABETA_FRNT_HIPP" = "Aβ",
    "AT8_FRNT_HIPP" = "pTau",
    "INFARCT" = "Ischaemia",
    "BIOLOGICAL_SEX" = "Biological sex",
    "Duration_Binned" = "Duration",
    "Age_at_Death_Binned" = "Age at death"
  )
  
  # Recode levels for INFARCT
  infarct_labels <- c("INF_0" = "Absent", "INF_1" = "Present")
  bsex_labels <- c("M" = "Male", "F" = "Female")
  
  # Create individual plots
  common_ylim <- c(0, 100)
  plot_list <- lapply(seq_along(effects_list), function(i) {
    var_name <- names(effects_list)[i]
    effects_data <- as.data.frame(effects_list[[i]])
    
    if (nrow(effects_data) == 0) return(NULL)  # Skip empty frames
    
    is_categorical <- var_name %in% categorical_vars
    
    if (!is_categorical) {
      plot_obj <- ggplot(effects_data, aes(x = x, y = predicted * 100)) +
        geom_ribbon(
          aes(ymin = conf.low * 100, ymax = conf.high * 100),
          fill = continuous_colors[[x_axis_labels[[var_name]]]],
          alpha = 0.2
        ) +
        geom_line(
          size = 3,
          color = continuous_colors[[x_axis_labels[[var_name]]]]
        ) +
        labs(
          x = x_axis_labels[[var_name]],
          y = if (i == 1) "Dementia probability (%)" else NULL
        ) +
        xlim(xlims_list[[var_name]]) +  # 👈 Apply manual x-axis limit here
        ylim(common_ylim) +
        theme_minimal() +
        theme(
          plot.title = element_blank(),
          axis.title.x = element_text(size = 34),
          axis.title.y = element_text(size = 34),
          axis.text = element_text(size = 34),
          plot.margin = margin(t = 40, r = 5, b = 5, l = 5, unit = "pt"),
          legend.position = "none"
        )
    } else {
      if (var_name == "INFARCT") {
        effects_data$x <- factor(effects_data$x,
                                 levels = names(infarct_labels),
                                 labels = infarct_labels)
      } else if (var_name == "BIOLOGICAL_SEX") {
        effects_data$x <- factor(effects_data$x,
                                 levels = names(bsex_labels),
                                 labels = bsex_labels)
      }
      
      plot_obj <- ggplot(effects_data, aes(x = x, y = predicted * 100, color = as.factor(x))) +
        geom_point(size = 10) +
        geom_errorbar(aes(ymin = conf.low * 100, ymax = conf.high * 100),
                      width = 0.7, linetype = "dotted", size = 2) +
        scale_color_manual(values = c("orchid1", "darkorchid4")) +
        labs(
          x = x_axis_labels[[var_name]],
          y = if (i == 1) "Dementia probability (%)" else NULL,
          color = "Category"
        ) +
        ylim(common_ylim) +
        theme_minimal() +
        theme(
          plot.title = element_blank(),
          axis.title.x = element_text(size = 34),
          axis.title.y = element_text(size = 34),
          axis.text = element_text(size = 34),
          plot.margin = margin(t = 40, r = 5, b = 5, l = 5, unit = "pt"),
          legend.position = "none"
        )
    }
    
    # Remove y-axis title for plots after the first one
    if (i != 1) {
      plot_obj <- plot_obj + theme(axis.title.y = element_blank())
    }
    
    return(plot_obj)
  })
  
  # Remove NULLs from empty predictors
  plot_list <- plot_list[!sapply(plot_list, is.null)]
  
  # Arrange plots
  sjplot <- cowplot::plot_grid(plotlist = plot_list, nrow = 1, align = "h")
  
  # Display plot
  print(sjplot)
  # Save
  ggsave(paste0("sjplot_UPPERQUARTILE_", apoe_type, ".png"), sjplot, width = 35, height = 5, dpi = 300)
  
}

#######################DECISION TREE################


# Define continuous variables
continuous_vars <- c("ASYN_FRNT_HIPP", "ABETA_FRNT_HIPP", "AT8_FRNT_HIPP")

# Apply per group
get_fixed_splits_by_apoe <- function(data, apoe_group) {
  df_sub <- data %>%
    filter(ApoE == apoe_group) %>%
    mutate(DEMENTIA_BIN = ifelse(DEMENTIA == "Dem", 1, 0))
  
  # Define cutoffs
  asyn_75 <- quantile(df_sub$ASYN_FRNT_HIPP, 0.75, na.rm = TRUE)
  abeta_75 <- quantile(df_sub$ABETA_FRNT_HIPP, 0.75, na.rm = TRUE)
  at8_85 <- quantile(df_sub$AT8_FRNT_HIPP, 0.85, na.rm = TRUE)
  
  tibble(
    ApoE = apoe_group,
    variable = c("ASYN_FRNT_HIPP", "ABETA_FRNT_HIPP", "AT8_FRNT_HIPP"),
    percentile = c("75th", "75th", "85th"),
    fixed_cut = c(asyn_75, abeta_75, at8_85)
  )
}

# Run for both ApoE groups
split_apoe3 <- get_fixed_splits_by_apoe(df_binned, "APOE ε3")
split_apoe4 <- get_fixed_splits_by_apoe(df_binned, "APOE ε4")

# Combine
split_results_all <- bind_rows(split_apoe3, split_apoe4)

# View results
print(split_results_all)

# Save to CSV
write_csv(split_results_all, "logistic_best_splits_by_apoe.csv")

# Define the fixed splits for each ApoE group
fixed_splits <- split_results_all %>%
  arrange(ApoE, variable) %>%
  select(ApoE, variable, fixed_cut) %>%
  mutate(fixed_cut = round(fixed_cut, 2))
df_binned <- df_binned %>%
  mutate(DEMENTIA_BIN = ifelse(DEMENTIA == "Dem", 1, 0)) 

###DT_apoe3
# -- 1. Rename variables in importance table
result_apoe3_renamed <- as.data.frame(result_apoe3$importance) %>%
  mutate(Variable = case_when(
    Variable == "INFARCTINF_1" ~ "INFARCT",
    Variable == "BIOLOGICAL_SEXM" ~ "BIOLOGICAL_SEX",
    Variable == "Age_at_Death_Binned≥ 77" ~ "Age_at_Death_Binned",
    Variable == "Duration_Binned≥ 15" ~ "Duration_Binned",
    TRUE ~ Variable
  ))

# -- 2. Get ordered variables by importance
importance_apoe3 <- result_apoe3_renamed %>%
  arrange(desc(Importance)) %>%
  pull(Variable)

# -- 3. Merge with fixed splits
ordered_variables_apoe3 <- fixed_splits %>%
  filter(ApoE == "APOE ε3") %>%
  arrange(match(variable, importance_apoe3))
ordered_vars <- importance_apoe3

var_rename <- c(
  ASYN_FRNT_HIPP = "α-synuclein",
  ABETA_FRNT_HIPP = "Aβ",
  Duration_Binned = "Duration",
  INFARCT = "Ischaemia",
  Age_at_Death_Binned = "Age at death",
  BIOLOGICAL_SEX = "Biological sex",
  AT8_FRNT_HIPP = "pTau"
)

# Apply renaming
ordered_vars <- unname(var_rename[ordered_vars])
ordered_vars

# -- 4. Create binned columns
cutoffs <- ordered_variables_apoe3$fixed_cut
names(cutoffs) <- ordered_variables_apoe3$variable

df_binned1 <- df_binned %>%
  mutate(
    ASYN_FRNT_HIPP_CAT = ifelse(ASYN_FRNT_HIPP >= cutoffs["ASYN_FRNT_HIPP"], paste0("\u2265", cutoffs["ASYN_FRNT_HIPP"]), paste0("<", cutoffs["ASYN_FRNT_HIPP"])),
    ABETA_FRNT_HIPP_CAT = ifelse(ABETA_FRNT_HIPP >= cutoffs["ABETA_FRNT_HIPP"], paste0("\u2265", cutoffs["ABETA_FRNT_HIPP"]), paste0("<", cutoffs["ABETA_FRNT_HIPP"])),
    AT8_FRNT_HIPP_CAT   = ifelse(AT8_FRNT_HIPP   >= cutoffs["AT8_FRNT_HIPP"],   paste0("\u2265", cutoffs["AT8_FRNT_HIPP"]),   paste0("<", cutoffs["AT8_FRNT_HIPP"]))
  )

# -- 5. Prepare dataset for APOE ε3 group

df_apoe3 <- df_binned1 %>%
  filter(ApoE == "APOE ε3") %>%
  mutate(
    `α-synuclein` = ASYN_FRNT_HIPP_CAT,
    `Aβ` = ABETA_FRNT_HIPP_CAT,
    `pTau` = AT8_FRNT_HIPP_CAT,
    Ischaemia = factor(INFARCT, levels = c("INF_0", "INF_1"), labels = c("Absent", "Present")),
    `Biological sex` = factor(BIOLOGICAL_SEX, levels = c("F", "M"), labels = c("FEMALE", "MALE")),
    `Age at death` = Age_at_Death_Binned,
    Duration = Duration_Binned
  ) %>%
  select(DEMENTIA, DEMENTIA_BIN, `α-synuclein`, `Aβ`, `pTau`, Ischaemia, `Biological sex`, `Age at death`, Duration) %>%
  mutate(across(everything(), as.character))

# -- 6. Evaluate path-level risk
library(dplyr)

# -- 1. Evaluate path-level risk
tree_paths_eval_apoe3 <- df_apoe3 %>%
  group_by(`α-synuclein`, `Aβ`, `pTau`, Ischaemia, `Biological sex`, `Age at death`, Duration) %>%
  summarise(
    n = n(),
    pct_high = mean(DEMENTIA_BIN == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    risk_label = case_when(
      is.na(pct_high) ~ "No data",
      pct_high >= 0.7 ~ "High risk",
      TRUE ~ "Low risk"
    ),
    label = paste0(risk_label, "\n(n=", n, ", ",
                   ifelse(is.na(pct_high), "NA", round(pct_high * 100)), "%)"),
    display_label = risk_label
  )
write.csv(tree_paths_eval_apoe3, "ApoE3_risk_bins_OBS.csv", row.names = FALSE)
# -- 2. Define ordered variables for tree depth (same as image structure)
ordered_vars 

# -- 3. Create custom PathString that stops branching when pct_high ≥ 0.7 <20
tree_paths_eval_apoe3$PathString <- apply(tree_paths_eval_apoe3, 1, function(row) {
  path <- c("Start")
  label_to_use <- row[["display_label"]]  # default
  
  for (v in ordered_vars) {
    value <- row[[v]]
    path <- c(path, paste0(v, ": ", value))
    
    # Check if this subgroup exceeds threshold
    subset_rows <- tree_paths_eval_apoe3
    for (i in seq_along(path)[-1]) {
      parts <- strsplit(path[i], ": ")[[1]]
      subset_rows <- subset_rows[subset_rows[[parts[1]]] == parts[2], ]
    }
    
    if (nrow(subset_rows) > 0) {
      avg_pct <- mean(subset_rows$pct_high)
      
      if (avg_pct >= 0.6) {
        label_to_use <- "High risk"
        break
      } else if (avg_pct <= 0.30) {
        label_to_use <- "Low risk"
        break
      }
    }
  }  # <-- closes the for loop
  
  paste(c(path, label_to_use), collapse = "/")
}) 

library(dplyr)
library(DiagrammeR)
library(stringr)
library(digest)
# ---- Step 1: Extract node paths from tree_paths_eval 
paths <- strsplit(tree_paths_eval_apoe3$PathString, "/")

# ---- Step 2: Create edges and unique node labels 
edges <- data.frame(from = character(), to = character(), label = character())
nodes <- data.frame(id = character(), label = character(), group = character(), stringsAsFactors = FALSE)

for (p in paths) {
  for (i in 2:length(p)) {
    from <- paste(p[1:(i - 1)], collapse = "/")
    to   <- paste(p[1:i], collapse = "/")
    label <- p[i]
    
    # Generate unique, clean IDs for Graphviz
    from_id <- paste0("node_", digest(from, algo = "crc32"))
    to_id   <- paste0("node_", digest(to, algo = "crc32"))
    
    edges <- rbind(edges, data.frame(from = from_id, to = to_id, label = label))
    
    group <- ifelse(grepl("risk", label), "risk",
                    ifelse(grepl("^α-syn", label), "asyn",
                           ifelse(grepl("^Aβ", label), "abeta",
                                  ifelse(grepl("^pTau", label), "ptau",
                                         ifelse(grepl("Ischa", label), "isch",
                                                ifelse(grepl("Sex", label), "sex",
                                                       ifelse(grepl("Duration", label), "dur", "default")))))))
    
    nodes <- rbind(nodes, data.frame(
      id = to_id,
      label = label,
      group = group,
      stringsAsFactors = FALSE
    ))
  }
}

# Remove duplicates
nodes <- nodes[!duplicated(nodes$id), ]
edges <- edges[!duplicated(edges), ]

# Get root node ID from the first edge
root_id <- edges$from[1]

# If it's not in the node list, add it manually
if (!(root_id %in% nodes$id)) {
  nodes <- rbind(nodes, data.frame(
    id = root_id,
    label = "APOE ε3",
    group = "apoe",
    stringsAsFactors = FALSE
  ))
}


# ---- Step 3: Build Graphviz DOT syntax 
color_map <- c(
  "asyn" = "lawngreen",
  "abeta" = "lightpink",
  "isch" = "khaki1",
  "ptau" = "lightskyblue1",
  "sex" = "plum1",
  "dur" = "lemonchiffon",
  "risk" = "mistyrose",
  "apoe" = "dodgerblue1",           # ← custom color for APOE root
  "default" = "white"
)
# Fix labels: escape quotes, remove newlines for Graphviz compatibility
nodes$label <- gsub("\"", "'", nodes$label)            # Replace double quotes
nodes$label <- gsub("\n", "\\n", nodes$label)          # Escape line breaks
node_defs <- paste0(
  nodes$id, ' [label="', nodes$label,
  '", style=filled, fillcolor="', color_map[nodes$group], '"];'
)

edge_defs <- paste0(
  edges$from, ' -> ', edges$to, ';'
)

# ---- Step 4: Wrap in grViz 
dot_code_apoe3 <- paste0(
  "digraph G {\n",
  "  graph [layout = dot, rankdir = TB];\n",
  "  node [shape=box, fontname=Helvetica];\n\n",
  paste(node_defs, collapse = "\n"), "\n\n",
  paste(edge_defs, collapse = "\n"), "\n}"
)

# ---- Step 5: Render the final publication-style graph
grViz(dot_code_apoe3)
library(rsvg)
library(DiagrammeRsvg)
# Render and export to PDF
grViz(dot_code_apoe3) %>%
  export_svg() %>%
  charToRaw() %>%
  rsvg_pdf("decision_tree_apoe3_min_split_ObsOnly.pdf")

###DT_apoe4
# -- 1. Rename variables in importance table
result_apoe4_renamed <- as.data.frame(result_apoe4$importance) %>%
  mutate(Variable = case_when(
    Variable == "INFARCTINF_1" ~ "INFARCT",
    Variable == "BIOLOGICAL_SEXM" ~ "BIOLOGICAL_SEX",
    Variable == "Age_at_Death_Binned≥ 77" ~ "Age_at_Death_Binned",
    Variable == "Duration_Binned≥ 15" ~ "Duration_Binned",
    TRUE ~ Variable
  ))

# -- 2. Get ordered variables by importance
importance_apoe4 <- result_apoe4_renamed %>%
  arrange(desc(Importance)) %>%
  pull(Variable)

# -- 3. Merge with fixed splits
ordered_variables_apoe4 <- fixed_splits %>%
  filter(ApoE == "APOE ε4") %>%
  arrange(match(variable, importance_apoe4))

ordered_vars <- importance_apoe4
var_rename <- c(
  ASYN_FRNT_HIPP = "α-synuclein",
  ABETA_FRNT_HIPP = "Aβ",
  Duration_Binned = "Duration",
  INFARCT = "Ischaemia",
  Age_at_Death_Binned = "Age at death",
  BIOLOGICAL_SEX = "Biological sex",
  AT8_FRNT_HIPP = "pTau"
)

# Apply renaming
ordered_vars <- unname(var_rename[ordered_vars])


# -- 4. Create binned columns
cutoffs <- ordered_variables_apoe4$fixed_cut
names(cutoffs) <- ordered_variables_apoe4$variable

df_binned1 <- df_binned %>%
  mutate(
    ASYN_FRNT_HIPP_CAT = ifelse(ASYN_FRNT_HIPP >= cutoffs["ASYN_FRNT_HIPP"], paste0("\u2265", cutoffs["ASYN_FRNT_HIPP"]), paste0("<", cutoffs["ASYN_FRNT_HIPP"])),
    ABETA_FRNT_HIPP_CAT = ifelse(ABETA_FRNT_HIPP >= cutoffs["ABETA_FRNT_HIPP"], paste0("\u2265", cutoffs["ABETA_FRNT_HIPP"]), paste0("<", cutoffs["ABETA_FRNT_HIPP"])),
    AT8_FRNT_HIPP_CAT   = ifelse(AT8_FRNT_HIPP   >= cutoffs["AT8_FRNT_HIPP"],   paste0("\u2265", cutoffs["AT8_FRNT_HIPP"]),   paste0("<", cutoffs["AT8_FRNT_HIPP"]))
  )

# -- 5. Prepare dataset for APOE ε4 group

df_apoe4 <- df_binned1 %>%
  filter(ApoE == "APOE ε4") %>%
  mutate(
    `α-synuclein` = ASYN_FRNT_HIPP_CAT,
    `Aβ` = ABETA_FRNT_HIPP_CAT,
    `pTau` = AT8_FRNT_HIPP_CAT,
    Ischaemia = factor(INFARCT, levels = c("INF_0", "INF_1"), labels = c("Absent", "Present")),
    `Biological sex` = factor(BIOLOGICAL_SEX, levels = c("F", "M"), labels = c("FEMALE", "MALE")),
    `Age at death` = Age_at_Death_Binned,
    Duration = Duration_Binned
  ) %>%
  select(DEMENTIA, DEMENTIA_BIN, `α-synuclein`, `Aβ`, `pTau`, Ischaemia, `Biological sex`, `Age at death`, Duration) %>%
  mutate(across(everything(), as.character))

# -- 6. Evaluate path-level risk
library(dplyr)

# -- 1. Evaluate path-level risk
tree_paths_eval_apoe4 <- df_apoe4 %>%
  group_by(`α-synuclein`, `Aβ`, `pTau`, Ischaemia, `Biological sex`, `Age at death`, Duration) %>%
  summarise(
    n = n(),
    pct_high = mean(DEMENTIA_BIN == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    risk_label = case_when(
      is.na(pct_high) ~ "No data",
      pct_high >= 0.7 ~ "High risk",
      TRUE ~ "Low risk"
    ),
    label = paste0(risk_label, "\n(n=", n, ", ",
                   ifelse(is.na(pct_high), "NA", round(pct_high * 100)), "%)"),
    display_label = risk_label
  )
write.csv(tree_paths_eval_apoe4, "ApoE4_risk_bins_OBS.csv", row.names = FALSE)
# -- 2. Define ordered variables for tree depth (same as image structure)
ordered_vars

# -- 3. Create custom PathString that stops branching when pct_high ≥ 0.8
tree_paths_eval_apoe4$PathString <- apply(tree_paths_eval_apoe4, 1, function(row) {
  path <- c("Start")
  label_to_use <- row[["display_label"]]  # default
  
  for (v in ordered_vars) {
    value <- row[[v]]
    path <- c(path, paste0(v, ": ", value))
    
    # Check if this subgroup exceeds threshold
    subset_rows <- tree_paths_eval_apoe4
    for (i in seq_along(path)[-1]) {
      parts <- strsplit(path[i], ": ")[[1]]
      subset_rows <- subset_rows[subset_rows[[parts[1]]] == parts[2], ]
    }
    
    if (nrow(subset_rows) > 0) {
      avg_pct <- mean(subset_rows$pct_high)
      
      if (avg_pct >= 0.66) {
        label_to_use <- "High risk"
        break
      } else if (avg_pct <= 0.3) {
        label_to_use <- "Low risk"
        break
      }
    }
  }  # <-- closes the for loop
  
  paste(c(path, label_to_use), collapse = "/")
}) 

library(dplyr)
library(DiagrammeR)
library(stringr)
library(digest)
# ---- Step 1: Extract node paths from tree_paths_eval 
paths <- strsplit(tree_paths_eval_apoe4$PathString, "/")

# ---- Step 2: Create edges and unique node labels 
edges <- data.frame(from = character(), to = character(), label = character())
nodes <- data.frame(id = character(), label = character(), group = character(), stringsAsFactors = FALSE)

for (p in paths) {
  for (i in 2:length(p)) {
    from <- paste(p[1:(i - 1)], collapse = "/")
    to   <- paste(p[1:i], collapse = "/")
    label <- p[i]
    
    # Generate unique, clean IDs for Graphviz
    from_id <- paste0("node_", digest(from, algo = "crc32"))
    to_id   <- paste0("node_", digest(to, algo = "crc32"))
    
    edges <- rbind(edges, data.frame(from = from_id, to = to_id, label = label))
    
    group <- ifelse(grepl("risk", label), "risk",
                    ifelse(grepl("^α-syn", label), "asyn",
                           ifelse(grepl("^Aβ", label), "abeta",
                                  ifelse(grepl("^pTau", label), "ptau",
                                         ifelse(grepl("Ischa", label), "isch",
                                                ifelse(grepl("Sex", label), "sex",
                                                       ifelse(grepl("Duration", label), "dur", "default")))))))
    
    nodes <- rbind(nodes, data.frame(
      id = to_id,
      label = label,
      group = group,
      stringsAsFactors = FALSE
    ))
  }
}

# Remove duplicates
nodes <- nodes[!duplicated(nodes$id), ]
edges <- edges[!duplicated(edges), ]

# Get root node ID from the first edge
root_id <- edges$from[1]

# If it's not in the node list, add it manually
if (!(root_id %in% nodes$id)) {
  nodes <- rbind(nodes, data.frame(
    id = root_id,
    label = "APOE ε4",
    group = "apoe",
    stringsAsFactors = FALSE
  ))
}


# ---- Step 3: Build Graphviz DOT syntax 
color_map <- c(
  "asyn" = "lawngreen",
  "abeta" = "lightpink",
  "isch" = "khaki1",
  "ptau" = "lightskyblue1",
  "sex" = "plum1",
  "dur" = "lemonchiffon",
  "risk" = "mistyrose",
  "apoe" = "firebrick2",           # ← custom color for APOE root
  "default" = "white"
)

node_defs <- paste0(
  nodes$id, ' [label="', nodes$label,
  '", style=filled, fillcolor="', color_map[nodes$group], '"];'
)

edge_defs <- paste0(
  edges$from, ' -> ', edges$to, ';'
)

# ---- Step 4: Wrap in grViz 
dot_code_apoe4 <- paste0(
  "digraph G {\n",
  "  graph [layout = dot, rankdir = TB];\n",
  "  node [shape=box, fontname=Helvetica];\n\n",
  paste(node_defs, collapse = "\n"), "\n\n",
  paste(edge_defs, collapse = "\n"), "\n}"
)

# ---- Step 5: Render the final publication-style graph
grViz(dot_code_apoe4)
library(rsvg)
library(DiagrammeRsvg)
# Render and export to PDF
grViz(dot_code_apoe4) %>%
  export_svg() %>%
  charToRaw() %>%
  rsvg_pdf("decision_tree_apoe4_min_split_ObsOnly.pdf")


