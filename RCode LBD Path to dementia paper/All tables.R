
#All Tables Categorical data######




required <- c("readxl","openxlsx","dplyr","tidyr","purrr","stringr","tibble")
to_install <- setdiff(required, rownames(installed.packages()))
if(length(to_install)) install.packages(to_install)

library(readxl)
library(openxlsx)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(tibble)




excel_path  <- # <- change if needed
  input_sheet <- NULL      # or a sheet name, e.g. "Sheet1"
output_sheet <- "ChiSq_Results"


#HELPERS


parse_success_total <- function(x) {
  if (is.na(x) || !nzchar(as.character(x))) return(c(success=NA_integer_, total=NA_integer_))
  s <- as.character(x)
  # capture first "a/b" occurrence anywhere in the cell
  m <- str_match(s, "(\\d+)\\s*/\\s*(\\d+)")
  if (!is.na(m[1,1])) {
    return(c(success = as.integer(m[1,2]), total = as.integer(m[1,3])))
  }
  # fallback: pure integer
  if (str_detect(s, "^\\d+$")) return(c(success=as.integer(s), total=NA_integer_))
  c(success=NA_integer_, total=NA_integer_)
}

# Overall test (RxC):
# - If all expected >=5  -> standard Chi-square
# - Else if 2x2          -> Fisher exact
# - Else (RxC, not 2x2)  -> Monte-Carlo Chi-square (or simulated Fisher as fallback)
overall_test <- function(tab, simulateN = 2e5) {
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE))
  exp_min <- min(chi$expected)
  
  if (exp_min >= 5) {
    return(list(method = "Chi-square", statistic = unname(chi$statistic), p.value = chi$p.value))
  }
  
  # small expected counts
  if (nrow(tab) == 2 && ncol(tab) == 2) {
    # exact Fisher is fine for 2x2
    fis <- suppressWarnings(fisher.test(tab))
    return(list(method = "Fisher's exact (2x2)", statistic = NA_real_, p.value = fis$p.value))
  }
  
  # RxC (not 2x2): prefer Monte-Carlo Chi-square to avoid Fisher workspace errors
  chi_mc <- suppressWarnings(chisq.test(tab, correct = FALSE, simulate.p.value = TRUE, B = simulateN))
  if (is.finite(chi_mc$p.value)) {
    return(list(method = paste0("Chi-square (Monte Carlo, B=", simulateN, ")"),
                statistic = unname(chi_mc$statistic), p.value = chi_mc$p.value))
  }
  
  # Fallback: Fisher with simulated p-value (also Monte Carlo)
  fis_sim <- suppressWarnings(fisher.test(tab, simulate.p.value = TRUE, B = simulateN))
  list(method = paste0("Fisher's exact (simulated, B=", simulateN, ")"),
       statistic = NA_real_, p.value = fis_sim$p.value)
}

# Pairwise tests between cohorts (2x2)
pairwise_tests <- function(tab, p_adjust = "BH") {
  stopifnot(nrow(tab) == 2)
  groups <- colnames(tab)
  if (length(groups) < 2) return(tibble())
  combs <- t(combn(groups, 2))
  results <- apply(combs, 1, function(gp) {
    sub <- tab[, gp, drop = FALSE]
    chi <- suppressWarnings(chisq.test(sub, correct = FALSE))
    exp_min <- min(chi$expected)
    if (exp_min < 5 || any(sub < 5)) {
      p <- suppressWarnings(fisher.test(sub)$p.value)
      method <- "Fisher (2x2)"
    } else {
      succ <- sub[1, ]
      tot  <- colSums(sub)
      p <- suppressWarnings(prop.test(x = succ, n = tot, correct = TRUE)$p.value)
      method <- "Chi-square (prop.test)"
    }
    tibble(group1 = gp[1], group2 = gp[2], p_raw = p, method = method)
  }) %>% bind_rows()
  results %>% mutate(p_adj = p.adjust(p_raw, method = p_adjust))
}

# Build 2xK tables from a "cohorts-in-rows, variables-in-columns" sheet.
# First column = Cohort, remaining columns = variables that may contain "a/b (p%)" cells.
build_tabs_from_columns <- function(df) {
  # Ensure character to allow regex parsing
  df_chr <- df %>% mutate(across(everything(), as.character))
  stopifnot(ncol(df_chr) >= 2)
  
  cohort_col <- names(df_chr)[1]
  cohorts <- df_chr[[cohort_col]]
  var_cols <- names(df_chr)[-1]
  
  tabs <- map(var_cols, function(vname) {
    vals <- df_chr[[vname]]
    parsed <- map_dfr(vals, ~{
      st <- parse_success_total(.x)
      tibble(success = st[1], total = st[2])
    })
    
    # Need totals present and >= successes, and at least 2 cohorts with data
    if (any(is.na(parsed$total)) || any(is.na(parsed$success))) return(NULL)
    if (any(parsed$success > parsed$total, na.rm = TRUE)) return(NULL)
    
    yes <- as.numeric(parsed$success)
    no  <- as.numeric(parsed$total - parsed$success)
    
    # Drop cohorts with missing totals or negative ‘no’
    keep <- which(!is.na(yes) & !is.na(no) & no >= 0)
    if (length(keep) < 2) return(NULL)
    
    m <- rbind(Yes = yes[keep], No = no[keep])
    colnames(m) <- cohorts[keep]
    m
  })
  
  names(tabs) <- var_cols
  # Keep only non-null 2xK tables
  tabs[!map_lgl(tabs, is.null)]
}

#LOAD DATA### 
sheets <- excel_sheets(excel_path)
if (is.null(input_sheet)) input_sheet <- sheets[1]
df_raw <- read_excel(excel_path, sheet = input_sheet)

# BUILD TABLES & RUN TESTS
tabs <- build_tabs_from_columns(df_raw)
if (!length(tabs)) stop("No categorical variables with 'success/total' format found per column. Make sure your columns like 'Male (%)' use 'a/b (...)' per cohort row.")

# Overall RxC per variable
overall_df <- imap_dfr(tabs, function(tab, varname) {
  ot <- overall_test(tab)
  tibble(
    variable = varname,
    method_overall = ot$method,
    chisq_stat = ot$statistic,
    p_overall = ot$p.value
  )
})

# Pairwise within each variable
pairwise_df <- imap_dfr(tabs, function(tab, varname) {
  if (nrow(tab) != 2 || ncol(tab) < 2) return(tibble())
  pw <- pairwise_tests(tab, p_adjust = "BH")
  pw %>% mutate(variable = varname, .before = 1)
})

# Tidy counts table (auditable)
counts_df <- imap_dfr(tabs, function(tab, varname) {
  as_tibble(tab, rownames = "Level") %>%
    pivot_longer(-Level, names_to = "Cohort", values_to = "Count") %>%
    mutate(variable = varname, .before = 1)
})

#WRITE RESULTS
wb <- loadWorkbook(excel_path)
if (output_sheet %in% names(wb)) removeWorksheet(wb, output_sheet)
addWorksheet(wb, output_sheet)

writeData(wb, output_sheet, overall_df, startRow = 1, startCol = 1, withFilter = TRUE)
start_row_pw <- nrow(overall_df) + 3
writeData(wb, output_sheet, pairwise_df, startRow = start_row_pw, startCol = 1, withFilter = TRUE)
start_row_counts <- start_row_pw + nrow(pairwise_df) + 3
writeData(wb, output_sheet, counts_df, startRow = start_row_counts, startCol = 1, withFilter = TRUE)

# Style headers
hs <- createStyle(textDecoration = "bold")
addStyle(wb, output_sheet, hs, rows = 1, cols = 1:ncol(overall_df), gridExpand = TRUE)
if (nrow(pairwise_df) > 0)
  addStyle(wb, output_sheet, hs, rows = start_row_pw, cols = 1:ncol(pairwise_df), gridExpand = TRUE)
addStyle(wb, output_sheet, hs, rows = start_row_counts, cols = 1:ncol(counts_df), gridExpand = TRUE)

saveWorkbook(wb, excel_path, overwrite = TRUE)
message(sprintf("Done. Results written to sheet '%s' in %s", output_sheet, excel_path))

#Table 1 Continuous data######
# 
# Continuous-only stats for SHEET = "TABLE 1"
# - Auto-detect continuous/ordinal variables from headers/content
# - RAW: Wilcoxon (2 groups) / Kruskal–Wallis (>2)
# - SUMMARY: pairwise Welch from mean±sd + n (parsed from the sheet)
# - Two outputs in a NEW workbook:
#     * TABLE 1_Cont
#     * TABLE 1_PDDDLB_merged_Cont (PDD & DLB collapsed)
#

required <- c("readxl","openxlsx","dplyr","tidyr","purrr","stringr","tibble","stringi")
to_install <- setdiff(required, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, quiet = TRUE)

library(readxl)
library(openxlsx)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(tibble)
library(stringi)

# 
# CONFIG — set your file
# 
excel_path <- #Change as needed
sheet_name <- "TABLE 1"

# Save to a NEW workbook (don’t overwrite original)
out_path <- sub("\\.xlsx$", "_ContStats.xlsx", excel_path)


# HELPERS
# Strip readxl suffixes like "...16"
strip_readxl_suffix <- function(nms) {
  trimws(sub("\\.{3}\\d+$", "", nms))
}

# Canonicalize text (greek->latin, accents off, lowercase, collapse spaces)
canon <- function(x){
  x <- gsub("α","alpha",x,ignore.case=TRUE)
  x <- gsub("β","beta", x,ignore.case=TRUE)
  x <- gsub("τ","tau",  x,ignore.case=TRUE)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- tolower(gsub("[^a-z0-9]+"," ", x))
  gsub("^\\s+|\\s+$","", x)
}

# Roman to int for I–VI (and "stage X")
roman_to_int <- function(v){
  v <- trimws(toupper(gsub("stage\\s*","", as.character(v))))
  map <- c(I=1, II=2, III=3, IV=4, V=5, VI=6)
  out <- suppressWarnings(as.numeric(v))
  out[is.na(out) & v %in% names(map)] <- map[v[is.na(out) & v %in% names(map)]]
  out
}

# Coerce ordinal-ish to numeric if possible
coerce_ordinal_numeric <- function(x){
  if (is.numeric(x)) return(x)
  if (is.ordered(x)) return(as.numeric(x))
  z <- roman_to_int(x)
  if (all(is.na(z))) {
    z <- suppressWarnings(as.numeric(stringr::str_extract(as.character(x), "\\d+\\.?\\d*")))
  }
  z
}

# Parse summary forms
parse_mean_sd <- function(x) {
  s <- as.character(x)
  if (is.na(s) || !nzchar(s)) return(c(mean=NA_real_, sd=NA_real_))
  m <- stringr::str_match(s, "\\s*([+-]?[0-9]*\\.?[0-9]+)\\s*(?:±|\\+\\-)\\s*([0-9]*\\.?[0-9]+)")
  if (!is.na(m[1,1])) return(c(mean=as.numeric(m[1,2]), sd=as.numeric(m[1,3])))
  c(mean=NA_real_, sd=NA_real_)
}
parse_med_range <- function(x) {
  s <- as.character(x)
  if (is.na(s) || !nzchar(s)) return(c(median=NA_real_, lo=NA_real_, hi=NA_real_))
  m <- stringr::str_match(s, "\\s*([+-]?[0-9]*\\.?[0-9]+)\\s*\\[\\s*([+-]?[0-9]*\\.?[0-9]+)\\s*[–-]\\s*([+-]?[0-9]*\\.?[0-9]+)\\s*\\]")
  if (!is.na(m[1,1])) return(c(median=as.numeric(m[1,2]), lo=as.numeric(m[1,3]), hi=as.numeric(m[1,4])))
  c(median=NA_real_, lo=NA_real_, hi=NA_real_)
}
infer_total_for_row <- function(df_row) {
  # Try typical N columns
  n_col <- names(df_row)[tolower(names(df_row)) %in% c("n","total","n_total","ntotal","overalln","noverall")]
  if (length(n_col)) {
    nval <- suppressWarnings(as.numeric(df_row[[n_col[1]]]))
    if (!is.na(nval)) return(nval)
  }
  # Else look for "a/b"
  for (cn in names(df_row)) {
    s <- as.character(df_row[[cn]])
    m <- stringr::str_match(s, "(\\d+)\\s*/\\s*(\\d+)")
    if (!is.na(m[1,1])) return(as.numeric(m[1,3]))
  }
  NA_real_
}
welch_p_from_summary <- function(m1, sd1, n1, m2, sd2, n2) {
  if (any(is.na(c(m1,sd1,n1,m2,sd2,n2))) || any(c(n1,n2) < 2) || sd1 < 0 || sd2 < 0) return(NA_real_)
  se2 <- (sd1^2 / n1) + (sd2^2 / n2)
  if (se2 <= 0) return(NA_real_)
  tval <- (m1 - m2) / sqrt(se2)
  df <- se2^2 / (((sd1^2 / n1)^2 / (n1 - 1)) + ((sd2^2 / n2)^2 / (n2 - 1)))
  2 * stats::pt(-abs(tval), df = df)
}

# Detect which column is the group (Cohort) by either header or values
detect_group_col <- function(df) {
  # 1) Header match
  header_match <- intersect(c("Cohort","COHORT","cohort"), names(df))
  if (length(header_match)) return(header_match[1])
  # 2) Value-driven guess
  candidates <- names(df)
  wanted_vals <- c("PD","PDD","DLB","Control","Controls","PDD+DLB")
  for (cn in candidates) {
    vals <- unique(na.omit(as.character(df[[cn]])))
    if (sum(vals %in% wanted_vals) >= 2) return(cn)
  }
  stop("Could not detect a grouping column (Cohort).")
}

# Heuristic: is a column continuous/ordinal?
# TRUE if: (a) numeric after coercion OR (b) has many numeric-like tokens
# OR (c) looks like mean±sd or median [..] in >= 2 rows
is_continuous_like <- function(x) {
  x_chr <- as.character(x)
  # mean±sd present?
  mean_sd_hits <- sum(!is.na(t(vapply(x_chr, parse_mean_sd, numeric(2)))[,1]))
  if (mean_sd_hits >= 2) return(TRUE)
  # median [..] present?
  med_hits <- sum(!is.na(t(vapply(x_chr, parse_med_range, numeric(3)))[,1]))
  if (med_hits >= 2) return(TRUE)
  # numeric coercion coverage
  xn <- suppressWarnings(as.numeric(x_chr))
  num_hits <- sum(!is.na(xn))
  if (num_hits >= 3) return(TRUE)
  # ordinal coercion (roman/stage/digits)
  xo <- coerce_ordinal_numeric(x_chr)
  ord_hits <- sum(!is.na(xo))
  if (ord_hits >= 3) return(TRUE)
  FALSE
}

# Build long-format for RAW tests (only numeric-coercible cols)
make_long_raw <- function(df, group_col, cont_cols) {
  tmp <- df[, c(group_col, cont_cols), drop = FALSE]
  # Coerce stage-like / numeric
  for (nm in cont_cols) {
    tmp[[nm]] <- coerce_ordinal_numeric(tmp[[nm]])
  }
  num_cols <- cont_cols[sapply(tmp[cont_cols], function(v) sum(!is.na(v)) >= 2 && is.numeric(v))]
  if (!length(num_cols)) return(NULL)
  tmp %>%
    tidyr::pivot_longer(cols = dplyr::all_of(num_cols), names_to = "Variable", values_to = "Value") %>%
    dplyr::rename(Group = dplyr::all_of(group_col)) %>%
    dplyr::filter(!is.na(Value), !is.na(Group))
}

# RAW tests
test_continuous_long <- function(df_long) {
  splitter <- split(df_long, df_long$Variable, drop = TRUE)
  overall <- purrr::map_dfr(splitter, function(dd) {
    dd <- dd %>% dplyr::filter(!is.na(Value), !is.na(Group))
    if (!nrow(dd) || dplyr::n_distinct(dd$Group) < 2) return(tibble())
    if (dplyr::n_distinct(dd$Group) == 2) {
      w <- suppressWarnings(wilcox.test(Value ~ Group, data = dd, exact = FALSE))
      tibble(
        Variable    = unique(dd$Variable),
        Method      = "Wilcoxon rank-sum",
        Groups      = paste(levels(factor(dd$Group)), collapse = " vs "),
        N_total     = nrow(dd),
        N_per_group = paste(sapply(split(dd$Value, dd$Group), length), collapse = " / "),
        Statistic   = unname(w$statistic),
        P_value     = unname(w$p.value)
      )
    } else {
      kw <- suppressWarnings(kruskal.test(Value ~ Group, data = dd))
      tibble(
        Variable    = unique(dd$Variable),
        Method      = "Kruskal–Wallis",
        Groups      = paste(levels(factor(dd$Group)), collapse = " / "),
        N_total     = nrow(dd),
        N_per_group = paste(sapply(split(dd$Value, dd$Group), length), collapse = " / "),
        Statistic   = unname(kw$statistic),
        P_value     = unname(kw$p.value)
      )
    }
  })
  if (nrow(overall) > 0) overall <- overall %>% mutate(P_adj_BH = p.adjust(P_value, method = "BH"))
  
  pairwise <- purrr::map_dfr(splitter, function(dd) {
    dd <- dd %>% dplyr::filter(!is.na(Value), !is.na(Group))
    ng <- dplyr::n_distinct(dd$Group)
    if (ng < 2) return(tibble())
    if (ng == 2) {
      w <- suppressWarnings(wilcox.test(Value ~ Group, data = dd, exact = FALSE))
      g <- levels(factor(dd$Group))
      return(tibble(
        Variable = unique(dd$Variable)[1],
        Method   = "Pairwise Wilcoxon (BH)",
        group1   = g[1],
        group2   = g[2],
        p_adj    = stats::p.adjust(unname(w$p.value), method = "BH", n = 1)
      ))
    }
    pw <- suppressWarnings(pairwise.wilcox.test(dd$Value, dd$Group, p.adjust.method = "BH"))
    out <- as.data.frame(as.table(pw$p.value), stringsAsFactors = FALSE) %>%
      dplyr::filter(!is.na(Freq)) %>%
      dplyr::rename(group1 = Var1, group2 = Var2, p_adj = Freq)
    if (!nrow(out)) return(tibble())
    tibble(
      Variable = unique(dd$Variable)[1],
      Method   = "Pairwise Wilcoxon (BH)",
      group1   = out$group1,
      group2   = out$group2,
      p_adj    = out$p_adj
    )
  })
  list(overall = overall, pairwise = pairwise)
}

# Write a continuous-only result sheet
write_continuous_to_sheet <- function(wb, out_ws, df, group_col, cont_cols) {
  if (out_ws %in% names(wb)) removeWorksheet(wb, out_ws)
  addWorksheet(wb, out_ws)
  row_ptr <- 1
  title_style <- createStyle(textDecoration = "bold")
  add_block <- function(title, df_block) {
    if (!is.null(df_block) && nrow(df_block) > 0) {
      writeData(wb, out_ws, x = title, startRow = row_ptr, startCol = 1)
      addStyle(wb, out_ws, title_style, rows = row_ptr, cols = 1, gridExpand = TRUE)
      row_ptr <<- row_ptr + 1
      writeData(wb, out_ws, x = df_block, startRow = row_ptr, startCol = 1, withFilter = TRUE)
      row_ptr <<- row_ptr + nrow(df_block) + 2
    }
  }
  
  # ---- RAW ----
  raw_overall <- raw_pairwise <- tibble()
  df_long <- make_long_raw(df, group_col, cont_cols)
  if (!is.null(df_long) && nrow(df_long)) {
    res <- test_continuous_long(df_long)
    raw_overall  <- res$overall
    raw_pairwise <- res$pairwise
  }
  
  # ---- SUMMARY (mean±sd + n) Welch ----
  cont_welch <- tibble()
  med_notes  <- tibble()
  if (length(cont_cols)) {
    df_sum <- df[, c(group_col, cont_cols), drop = FALSE] %>%
      mutate(across(everything(), as.character))
    if (nrow(df_sum) && ncol(df_sum) >= 2) {
      grp <- df_sum[[group_col]]
      for (v in cont_cols) {
        col <- df_sum[[v]]
        msd <- t(vapply(col, parse_mean_sd, numeric(2)))
        Ns  <- vapply(seq_len(nrow(df_sum)), function(i) infer_total_for_row(df_sum[i, , drop=FALSE]), numeric(1))
        if (sum(!is.na(msd[,1])) >= 2 && sum(!is.na(Ns)) >= 2) {
          dfv <- tibble(Variable=v, Group=grp, Mean=msd[,1], SD=msd[,2], N=Ns) %>%
            filter(!is.na(Mean) & !is.na(SD) & !is.na(N))
          if (nrow(dfv) >= 2) {
            combs <- t(combn(unique(dfv$Group), 2))
            res <- apply(combs, 1, function(gp) {
              g1 <- gp[1]; g2 <- gp[2]
              r1 <- dfv %>% filter(Group == g1) %>% slice(1)
              r2 <- dfv %>% filter(Group == g2) %>% slice(1)
              p  <- welch_p_from_summary(r1$Mean, r1$SD, r1$N, r2$Mean, r2$SD, r2$N)
              tibble(
                variable = v,
                group1   = g1,
                group2   = g2,
                n1 = r1$N, mean1 = r1$Mean, sd1 = r1$SD,
                n2 = r2$N, mean2 = r2$Mean, sd2 = r2$SD,
                p_raw = p,
                method = "Welch t-test from summary (mean±sd, n)"
              )
            }) %>% bind_rows()
            if (nrow(res)) cont_welch <- bind_rows(cont_welch, res %>% mutate(p_adj = p.adjust(p_raw, "BH")))
          }
        } else {
          has_med <- any(!is.na(t(vapply(col, parse_med_range, numeric(3)))[,1]))
          if (has_med) {
            med_notes <- bind_rows(med_notes, tibble(
              variable = v,
              note = "Only median [range/IQR] summaries detected; inferential tests require raw values."
            ))
          }
        }
      }
    }
  }
  
  # ---- WRITE ----
  add_block("D) Continuous/ordinal – RAW Overall (Wilcoxon / Kruskal–Wallis)", raw_overall)
  add_block("E) Continuous/ordinal – RAW Pairwise Wilcoxon (BH)",             raw_pairwise)
  add_block("F) Continuous/ordinal – SUMMARY (mean±sd) Pairwise Welch (BH)",  cont_welch)
  add_block("G) Summary-only medians/ranges (no inferential tests)",           med_notes)
}


# RUN (TABLE 1 ONLY)
df <- suppressWarnings(read_excel(
  excel_path, sheet = sheet_name,
  na = c("NA","N/A","#N/A","#DIV/0!",""," ")
)) %>% as.data.frame()

if (!nrow(df)) stop("TABLE 1 is empty or not found.")

# Clean headers
names(df) <- strip_readxl_suffix(names(df))
names(df) <- make.unique(names(df), sep = "_dup")  # keep uniqueness if duplicates remain

# Detect group column
group_col <- detect_group_col(df)

# Identify continuous-like columns automatically (exclude group column)
all_cols <- setdiff(names(df), group_col)
cont_cols <- all_cols[sapply(df[all_cols], is_continuous_like)]

if (!length(cont_cols)) {
  warning("No continuous/ordinal variables detected on TABLE 1. No stats produced.")
}

# Create workbook and write default analysis
wb <- createWorkbook()

write_continuous_to_sheet(
  wb = wb,
  out_ws = paste0(sheet_name, "_Cont"),
  df = df,
  group_col = group_col,
  cont_cols = cont_cols
)

# Build merged (PD, PDD+DLB, Controls) and write
df_merged <- df
df_merged[[group_col]] <- dplyr::case_when(
  df_merged[[group_col]] %in% c("PDD","DLB") ~ "PDD+DLB",
  df_merged[[group_col]] == "PD"             ~ "PD",
  tolower(df_merged[[group_col]]) %in% c("control","controls") ~ "Controls",
  TRUE                                        ~ as.character(df_merged[[group_col]])
)
df_merged <- df_merged %>%
  dplyr::filter(.data[[group_col]] %in% c("PD","PDD+DLB","Controls"))

write_continuous_to_sheet(
  wb = wb,
  out_ws = paste0(sheet_name, "_PDDDLB_merged_Cont"),
  df = df_merged,
  group_col = group_col,
  cont_cols = cont_cols
)

saveWorkbook(wb, out_path, overwrite = TRUE)
message("Done. Wrote: ", out_path)

#Supplementary tables Continuous data##############
#
# Stats runner (per sheet) grouping by Cohort
# - Categorical (a/b): χ²/Fisher + pairwise + counts
# - Continuous/ordinal:
#     * RAW values -> Wilcoxon/Kruskal–Wallis (+ pairwise Wilcoxon, BH)
#     * mean ± sd summaries -> pairwise Welch from summaries (BH)
#     * median [range/IQR] summaries -> noted (no tests)
# Output: a "<sheet>_Stats" worksheet is added to the same file
# 

required <- c("readxl","openxlsx","dplyr","tidyr","purrr","stringr","tibble","stringi")
to_install <- setdiff(required, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)

library(readxl)
library(openxlsx)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(tibble)
library(stringi)

# 
# CONFIG — set your file
# 
excel_path <- #change as needed

# Variables of interest (as you'd *like* to see them)
categorical_vars <- c(
  "Male (%)","APOE ε4 carriers (%)","Dementia (%)",
  "Orthostatic hypotension (%)","TDP43 LATE stage","Ischaemic pathology (%)"
)

continuous_vars <- c(
  "Age at onset, y","Disease duration, y","Age at death, y",
  "Lewy body density (LB/mm2)","%Aβ","%pTau",
  "Braak LB stage","Thal phase","pTau Braak & Braak stage","ADNC level (NIA-AA)"
)

# Candidate names for the grouping column (case-insensitive)
group_col_candidates <- c("Cohort","COHORT","cohort")

# 
# HELPERS — generic
# 
sanitize_ws_name <- function(x) {
  x <- gsub("[\\[\\]\\*\\?/\\\\:]", "_", x)
  x <- gsub("^'|'$", "", x)
  if (nchar(x) > 28) x <- substr(x, 1, 28)
  paste0(x, "_Stats")
}

# Canonicalize text: greek->latin, strip accents, compact to [a-z0-9 ] and trim
canon <- function(x){
  x <- gsub("α","alpha",x,ignore.case=TRUE)
  x <- gsub("β","beta", x,ignore.case=TRUE)
  x <- gsub("τ","tau",  x,ignore.case=TRUE)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- tolower(gsub("[^a-z0-9]+"," ", x))
  gsub("^\\s+|\\s+$","", x)
}

# Map Roman numerals I–VI (and allow "stage X") to integers
roman_to_int <- function(v){
  v <- trimws(toupper(gsub("stage\\s*","", as.character(v))))
  map <- c(I=1, II=2, III=3, IV=4, V=5, VI=6)
  out <- suppressWarnings(as.numeric(v))
  out[is.na(out) & v %in% names(map)] <- map[v[is.na(out) & v %in% names(map)]]
  out
}

# Try to coerce ordinal/roman/“stage 3” columns to numeric
coerce_ordinal_numeric <- function(x){
  if (is.numeric(x)) return(x)
  if (is.ordered(x)) return(as.numeric(x))
  z <- roman_to_int(x)
  if (all(is.na(z))) {
    z <- suppressWarnings(as.numeric(stringr::str_extract(as.character(x), "\\d+")))
  }
  z
}

# 
# HELPERS — categorical
# 
parse_success_total <- function(x) {
  if (is.na(x) || !nzchar(as.character(x))) return(c(success=NA_integer_, total=NA_integer_))
  s <- as.character(x)
  m <- str_match(s, "(\\d+)\\s*/\\s*(\\d+)")
  if (!is.na(m[1,1])) return(c(success = as.integer(m[1,2]), total = as.integer(m[1,3])))
  if (str_detect(s, "^\\d+$")) return(c(success=as.integer(s), total=NA_integer_))
  c(success=NA_integer_, total=NA_integer_)
}

overall_test <- function(tab, simulateN = 2e5) {
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE))
  exp_min <- min(chi$expected)
  if (exp_min >= 5) {
    return(list(method = "Chi-square", statistic = unname(chi$statistic), p.value = chi$p.value))
  }
  if (nrow(tab) == 2 && ncol(tab) == 2) {
    fis <- suppressWarnings(fisher.test(tab))
    return(list(method = "Fisher's exact (2x2)", statistic = NA_real_, p.value = fis$p.value))
  }
  chi_mc <- suppressWarnings(chisq.test(tab, correct = FALSE, simulate.p.value = TRUE, B = simulateN))
  if (is.finite(chi_mc$p.value)) {
    return(list(method = paste0("Chi-square (Monte Carlo, B=", simulateN, ")"),
                statistic = unname(chi_mc$statistic), p.value = chi_mc$p.value))
  }
  fis_sim <- suppressWarnings(fisher.test(tab, simulate.p.value = TRUE, B = simulateN))
  list(method = paste0("Fisher's exact (simulated, B=", simulateN, ")"),
       statistic = NA_real_, p.value = fis_sim$p.value)
}

pairwise_tests <- function(tab, p_adjust = "BH") {
  stopifnot(nrow(tab) == 2)
  groups <- colnames(tab)
  if (length(groups) < 2) return(tibble())
  combs <- t(combn(groups, 2))
  results <- apply(combs, 1, function(gp) {
    sub <- tab[, gp, drop = FALSE]
    chi <- suppressWarnings(chisq.test(sub, correct = FALSE))
    exp_min <- min(chi$expected)
    if (exp_min < 5 || any(sub < 5)) {
      p <- suppressWarnings(fisher.test(sub)$p.value)
      method <- "Fisher (2x2)"
    } else {
      succ <- sub[1, ]
      tot  <- colSums(sub)
      p <- suppressWarnings(prop.test(x = succ, n = tot, correct = TRUE)$p.value)
      method <- "Chi-square (prop.test)"
    }
    tibble(group1 = gp[1], group2 = gp[2], p_raw = p, method = method)
  }) %>% bind_rows()
  results %>% mutate(p_adj = p.adjust(p_raw, method = p_adjust))
}

# Build 2xK table for ONE categorical variable using a/b cells per group row
build_tab_for_categorical <- function(df, var, group_col) {
  stopifnot(group_col %in% names(df), var %in% names(df))
  vals <- as.character(df[[var]])
  parsed <- map_dfr(vals, ~{
    st <- parse_success_total(.x)
    tibble(success = st[1], total = st[2])
  })
  if (any(is.na(parsed$total)) || any(is.na(parsed$success))) return(NULL)
  if (any(parsed$success > parsed$total, na.rm = TRUE)) return(NULL)
  yes <- as.numeric(parsed$success)
  no  <- as.numeric(parsed$total - parsed$success)
  keep <- which(!is.na(yes) & !is.na(no) & no >= 0)
  if (length(keep) < 2) return(NULL)
  m <- rbind(Yes = yes[keep], No = no[keep])
  colnames(m) <- as.character(df[[group_col]])[keep]
  m
}

# 
# HELPERS — continuous
#
parse_mean_sd <- function(x) {
  s <- as.character(x)
  if (is.na(s) || !nzchar(s)) return(c(mean=NA_real_, sd=NA_real_))
  m <- str_match(s, "\\s*([+-]?[0-9]*\\.?[0-9]+)\\s*(?:±|\\+\\-)\\s*([0-9]*\\.?[0-9]+)")
  if (!is.na(m[1,1])) return(c(mean=as.numeric(m[1,2]), sd=as.numeric(m[1,3])))
  c(mean=NA_real_, sd=NA_real_)
}
parse_med_range <- function(x) {
  s <- as.character(x)
  if (is.na(s) || !nzchar(s)) return(c(median=NA_real_, lo=NA_real_, hi=NA_real_))
  m <- str_match(s, "\\s*([+-]?[0-9]*\\.?[0-9]+)\\s*\\[\\s*([+-]?[0-9]*\\.?[0-9]+)\\s*[–-]\\s*([+-]?[0-9]*\\.?[0-9]+)\\s*\\]")
  if (!is.na(m[1,1])) return(c(median=as.numeric(m[1,2]), lo=as.numeric(m[1,3]), hi=as.numeric(m[1,4])))
  c(median=NA_real_, lo=NA_real_, hi=NA_real_)
}
infer_total_for_row <- function(df_row) {
  n_col <- names(df_row)[tolower(names(df_row)) %in% c("n","total","n_total","ntotal","overalln","noverall")]
  if (length(n_col)) {
    nval <- suppressWarnings(as.numeric(df_row[[n_col[1]]]))
    if (!is.na(nval)) return(nval)
  }
  for (cn in names(df_row)) {
    s <- as.character(df_row[[cn]])
    m <- str_match(s, "(\\d+)\\s*/\\s*(\\d+)")
    if (!is.na(m[1,1])) return(as.numeric(m[1,3]))
  }
  NA_real_
}
welch_p_from_summary <- function(m1, sd1, n1, m2, sd2, n2) {
  if (any(is.na(c(m1,sd1,n1,m2,sd2,n2))) || any(c(n1,n2) < 2) || sd1 < 0 || sd2 < 0) return(NA_real_)
  se2 <- (sd1^2 / n1) + (sd2^2 / n2)
  if (se2 <= 0) return(NA_real_)
  tval <- (m1 - m2) / sqrt(se2)
  df <- se2^2 / (((sd1^2 / n1)^2 / (n1 - 1)) + ((sd2^2 / n2)^2 / (n2 - 1)))
  2 * stats::pt(-abs(tval), df = df)
}

# Long-ify RAW variables: allow numeric OR ordinal stage-like cols
make_long_raw <- function(df, group_col, present_cont) {
  if (!nrow(df) || !(group_col %in% names(df))) return(NULL)
  # Coerce stage-like columns to numeric if needed
  for (nm in present_cont) {
    if (!is.numeric(df[[nm]]) && grepl("braak|stage|thal", canon(nm))) {
      df[[nm]] <- coerce_ordinal_numeric(df[[nm]])
    }
  }
  # keep numeric only
  num_cols <- intersect(names(df)[sapply(df, is.numeric)], present_cont)
  if (!length(num_cols)) return(NULL)
  df %>%
    pivot_longer(cols = all_of(num_cols), names_to = "Variable", values_to = "Value") %>%
    rename(Group = all_of(group_col))
}

test_continuous_long <- function(df_long) {
  splitter <- split(df_long, df_long$Variable, drop = TRUE)
  overall <- map_dfr(splitter, function(dd) {
    dd <- dd %>% filter(!is.na(Value), !is.na(Group))
    if (!nrow(dd) || n_distinct(dd$Group) < 2) return(tibble())
    if (n_distinct(dd$Group) == 2) {
      w <- suppressWarnings(wilcox.test(Value ~ Group, data = dd, exact = FALSE))
      tibble(
        Variable    = unique(dd$Variable),
        Method      = "Wilcoxon rank-sum",
        Groups      = paste(levels(factor(dd$Group)), collapse = " vs "),
        N_total     = nrow(dd),
        N_per_group = paste(sapply(split(dd$Value, dd$Group), length), collapse = " / "),
        Statistic   = unname(w$statistic),
        P_value     = unname(w$p.value)
      )
    } else {
      kw <- suppressWarnings(kruskal.test(Value ~ Group, data = dd))
      tibble(
        Variable    = unique(dd$Variable),
        Method      = "Kruskal–Wallis",
        Groups      = paste(levels(factor(dd$Group)), collapse = " / "),
        N_total     = nrow(dd),
        N_per_group = paste(sapply(split(dd$Value, dd$Group), length), collapse = " / "),
        Statistic   = unname(kw$statistic),
        P_value     = unname(kw$p.value)
      )
    }
  })
  if (nrow(overall) > 0) overall <- overall %>% mutate(P_adj_BH = p.adjust(P_value, method = "BH"))
  
  pairwise <- map_dfr(splitter, function(dd) {
    dd <- dd %>% filter(!is.na(Value), !is.na(Group))
    ng <- n_distinct(dd$Group)
    if (ng < 2) return(tibble())
    
    if (ng == 2) {
      w <- suppressWarnings(wilcox.test(Value ~ Group, data = dd, exact = FALSE))
      g <- levels(factor(dd$Group))
      return(tibble(
        Variable = unique(dd$Variable)[1],
        Method   = "Pairwise Wilcoxon (BH)",
        group1   = g[1],
        group2   = g[2],
        p_adj    = stats::p.adjust(unname(w$p.value), method = "BH", n = 1)
      ))
    }
    
    pw <- suppressWarnings(pairwise.wilcox.test(dd$Value, dd$Group, p.adjust.method = "BH"))
    out <- as.data.frame(as.table(pw$p.value), stringsAsFactors = FALSE) %>%
      filter(!is.na(Freq)) %>%
      rename(group1 = Var1, group2 = Var2, p_adj = Freq)
    if (!nrow(out)) return(tibble())
    tibble(
      Variable = unique(dd$Variable)[1],
      Method   = "Pairwise Wilcoxon (BH)",
      group1   = out$group1,
      group2   = out$group2,
      p_adj    = out$p_adj
    )
  })
  
  list(overall = overall, pairwise = pairwise)
}

# 
# MAIN — iterate all sheets
# 
sheets <- excel_sheets(excel_path)
wb <- loadWorkbook(excel_path)

for (sh in sheets) {
  
  # --- Read with NA handling for Excel error tokens ---
  df <- suppressWarnings(read_excel(
    excel_path, sheet = sh,
    na = c("NA","N/A","#N/A","#DIV/0!",""," ")
  )) %>% as.data.frame()
  if (!nrow(df)) next
  
  # --- Resolve group column (Cohort/COHORT/...) ---
  gc <- intersect(group_col_candidates, names(df))
  if (!length(gc)) next
  group_col <- gc[1]
  
  # --- Canonical presence checks (names may differ in accents/greek/punct) ---
  can_df   <- canon(names(df))
  can_cont <- canon(continuous_vars)
  can_cats <- canon(categorical_vars)
  
  present_cont <- names(df)[can_df %in% can_cont]
  present_cats <- names(df)[can_df %in% can_cats]
  
  # --- Prepare output sheet ---
  out_ws <- sanitize_ws_name(sh)
  if (out_ws %in% names(wb)) removeWorksheet(wb, out_ws)
  addWorksheet(wb, out_ws)
  row_ptr <- 1
  title_style <- createStyle(textDecoration = "bold")
  
  add_block <- function(title, df_block) {
    if (nrow(df_block) > 0) {
      writeData(wb, out_ws, x = title, startRow = row_ptr, startCol = 1)
      addStyle(wb, out_ws, title_style, rows = row_ptr, cols = 1, gridExpand = TRUE)
      row_ptr <<- row_ptr + 1
      writeData(wb, out_ws, x = df_block, startRow = row_ptr, startCol = 1, withFilter = TRUE)
      row_ptr <<- row_ptr + nrow(df_block) + 2
    }
  }
  
  # ---------- A) Categorical (a/b) ----------
  cat_overall <- cat_pairwise <- cat_counts <- tibble()
  if (length(present_cats)) {
    for (v in present_cats) {
      tab <- try(build_tab_for_categorical(df, v, group_col), silent = TRUE)
      if (inherits(tab, "try-error") || is.null(tab)) next
      
      ot <- overall_test(tab)
      cat_overall <- bind_rows(cat_overall,
                               tibble(variable = v, method_overall = ot$method, chisq_stat = ot$statistic, p_overall = ot$p.value))
      if (nrow(tab) == 2 && ncol(tab) >= 2) {
        pw <- pairwise_tests(tab, p_adjust = "BH")
        if (nrow(pw)) cat_pairwise <- bind_rows(cat_pairwise, pw %>% mutate(variable = v, .before = 1))
      }
      cat_counts <- bind_rows(cat_counts,
                              as_tibble(tab, rownames = "Level") %>%
                                pivot_longer(-Level, names_to = "Cohort", values_to = "Count") %>%
                                mutate(variable = v, .before = 1))
    }
  }
  
  # ---------- B) Continuous / ordinal ----------
  cont_welch <- tibble()
  med_notes  <- tibble()
  all_na_notes <- tibble()
  not_found_notes <- tibble()
  
  # Audit: requested but not present by name (canonical)
  if (length(continuous_vars)) {
    missing_vars <- setdiff(continuous_vars, continuous_vars[canon(continuous_vars) %in% can_df])
    if (length(missing_vars)) {
      not_found_notes <- tibble(variable = missing_vars, note = "Requested but not found in sheet headers (canonical match).")
    }
  }
  
  # 1) RAW path (preferred)
  raw_overall <- raw_pairwise <- tibble()
  if (length(present_cont)) {
    
    # Coerce obvious stage-like columns before long-ify
    # (Handles 'Braak LB stage', 'Thal phase', etc.)
    for (nm in present_cont) {
      if (!is.numeric(df[[nm]]) && grepl("braak|stage|thal", canon(nm))) {
        df[[nm]] <- coerce_ordinal_numeric(df[[nm]])
      }
    }
    
    # Record any present variables that ended up all-NA after coercion
    for (v in present_cont) {
      if (all(is.na(df[[v]]))) {
        all_na_notes <- bind_rows(all_na_notes, tibble(
          variable = v, note = "Present in sheet but all values are NA after import/coercion; skipped in RAW tests."
        ))
      }
    }
    
    # Build long only from numeric
    df_long <- make_long_raw(df[, c(group_col, present_cont), drop = FALSE], group_col, present_cont)
    
    if (!is.null(df_long) && nrow(df_long)) {
      res <- test_continuous_long(df_long)
      raw_overall  <- res$overall
      raw_pairwise <- res$pairwise
    }
  }
  
  # 2) SUMMARY path (mean±sd or med[IQR/range])
  if (length(present_cont)) {
    df_sum <- df[, c(group_col, present_cont), drop = FALSE] %>%
      mutate(across(everything(), as.character))
    
    if (nrow(df_sum) && ncol(df_sum) >= 2) {
      grp <- df_sum[[group_col]]
      for (v in present_cont) {
        col <- df_sum[[v]]
        
        # Try mean ± sd
        msd <- t(vapply(col, parse_mean_sd, numeric(2)))
        Ns  <- vapply(seq_len(nrow(df_sum)), function(i) infer_total_for_row(df_sum[i, , drop=FALSE]), numeric(1))
        
        if (sum(!is.na(msd[,1])) >= 2 && sum(!is.na(Ns)) >= 2) {
          dfv <- tibble(Variable=v, Group=grp, Mean=msd[,1], SD=msd[,2], N=Ns) %>%
            filter(!is.na(Mean) & !is.na(SD) & !is.na(N))
          if (nrow(dfv) >= 2) {
            combs <- t(combn(unique(dfv$Group), 2))
            res <- apply(combs, 1, function(gp) {
              g1 <- gp[1]; g2 <- gp[2]
              r1 <- dfv %>% filter(Group == g1) %>% slice(1)
              r2 <- dfv %>% filter(Group == g2) %>% slice(1)
              p  <- welch_p_from_summary(r1$Mean, r1$SD, r1$N, r2$Mean, r2$SD, r2$N)
              tibble(
                variable = v,
                group1   = g1,
                group2   = g2,
                n1 = r1$N, mean1 = r1$Mean, sd1 = r1$SD,
                n2 = r2$N, mean2 = r2$Mean, sd2 = r2$SD,
                p_raw = p,
                method = "Welch t-test from summary (mean±sd, n)"
              )
            }) %>% bind_rows()
            if (nrow(res)) {
              res <- res %>% mutate(p_adj = p.adjust(p_raw, method = "BH"))
              cont_welch <- bind_rows(cont_welch, res)
            }
          }
        } else {
          # If it looks like median [range/IQR], just note it
          has_med <- any(!is.na(t(vapply(col, parse_med_range, numeric(3)))[,1]))
          if (has_med) {
            med_notes <- bind_rows(med_notes, tibble(
              variable = v,
              note = "Only median [range/IQR] summaries detected on this sheet; inferential tests require raw data."
            ))
          }
        }
      }
    }
  }
  
  # ---------- WRITE ----------
  add_block("A) Categorical (a/b) – Overall (Chi-square / Fisher)", cat_overall)
  add_block("B) Categorical (a/b) – Pairwise (BH)",                 cat_pairwise)
  add_block("C) Categorical (a/b) – Counts (auditable)",            cat_counts)
  add_block("D) Continuous/ordinal – RAW Overall (Wilcoxon / Kruskal–Wallis)", raw_overall)
  add_block("E) Continuous/ordinal – RAW Pairwise Wilcoxon (BH)",             raw_pairwise)
  add_block("F) Continuous/ordinal – SUMMARY (mean±sd) Pairwise Welch (BH)",  cont_welch)
  add_block("G) Summary-only medians/ranges (no inferential tests)",           med_notes)
  add_block("H) Present but all NA (audit)",                                   all_na_notes)
  add_block("I) Requested but not found (name audit)",                         not_found_notes)
}

saveWorkbook(wb, excel_path, overwrite = TRUE)
message("Done. A '<sheet>_Stats' worksheet has been added for each input sheet.")
