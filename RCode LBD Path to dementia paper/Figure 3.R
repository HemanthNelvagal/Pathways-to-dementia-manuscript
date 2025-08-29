#APOE e3 vs. e4 graphs################
setwd(#set location)
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(dplyr)
  library(dunn.test)  # for dunn.test()
  # install.packages("FSA") if you want the fallback
  library(FSA)
  datadir <- #Set location
    LGDIR <- paste0(datadir, "/PDvPDD_LINE_GRAPHS")
  dir.create(LGDIR)
  setwd(LGDIR)
  
  ##########################################################ASYN
  ASYN <- read_excel(#set excel) %>%
    as.data.frame()
    
    ASYN_PD <- ASYN %>%
      filter(GROUP == "PD")
    
    # Ensure DEMENTIA is factor (in case it's being read as character)
    ASYN_PD <- ASYN_PD %>%
      mutate(ApoE = as.factor(ApoE))
    
    # Get numeric columns (excluding categorical columns)
    numeric_cols <- ASYN_PD %>%
      select(where(is.numeric)) %>%
      colnames()
    
    # Pivot longer (keeping `Braak ASyn` and `DEMENTIA` unchanged)
    ASYN_PD_long <- ASYN_PD %>%
      pivot_longer(cols = all_of(numeric_cols),  # Only pivot numeric region columns
                   names_to = "Region",
                   values_to = "Value")
    # Define x-axis order based on the original data structure
    column_order <- unique(ASYN_PD_long$Region)
    
    # Summarize data: Calculate mean Value for each Region and Braak ASyn group
    ASYN_PD_summary <- ASYN_PD_long %>%
      group_by(ApoE, Region) %>%
      summarise(Mean_Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
      filter(!is.nan(Mean_Value))  # Remove cases where all values were NA
    
    
    ASYN_PD_plot <- ggplot(ASYN_PD_summary, aes(x = Region, y = Mean_Value, color = as.factor(ApoE), group = as.factor(ApoE))) +
      geom_line(size = 0.8) +  # Thinner lines
      geom_point(size = 2.5) +   # Smaller points
      scale_x_discrete(limits = column_order) +  # Maintain original x-axis order
      ylim(0,100)  +
      scale_color_manual(
        values = c("ApoE3" = "dodgerblue", "ApoE4" = "darkblue"),
        labels = c("ApoE3" = bquote("PD"~italic("APOE")~"ε3"), "ApoE4" = bquote("PD"~italic("APOE")~"ε4"))  
      ) +
      labs(
        title = "",
        x = NULL,  # Remove x-axis title
        y = "LB/mm²",
        color = NULL  # Remove color legend title
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 20),  # Rotate and adjust size
        axis.text.y = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        plot.title = element_text(size = 30, hjust = 0.5),
        legend.position = "top",  # Adjust legend position
        legend.text = element_text(size = 20),
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_blank()  # Remove minor gridlines
      )+
      guides(color = guide_legend(override.aes = list(size = 5)))  # **Increase legend icon size**
    
    # Display the plot
    print(ASYN_PD_plot)
    
    # Save Dementia (DEM_1) plot
    ggsave(filename = "ASYN_PD_ApoE.png",
           plot = ASYN_PD_plot,
           width = 10, height = 7, dpi = 300)
    
    ASYN_PDD <- ASYN %>%
      filter(GROUP == "PDD")
    
    # Ensure DEMENTIA is factor (in case it's being read as character)
    ASYN_PDD <- ASYN_PDD %>%
      mutate(ApoE = as.factor(ApoE))
    
    # Get numeric columns (excluding categorical columns)
    numeric_cols <- ASYN_PDD %>%
      select(where(is.numeric)) %>%
      colnames()
    
    # Pivot longer (keeping `Braak ASyn` and `DEMENTIA` unchanged)
    ASYN_PDD_long <- ASYN_PDD %>%
      pivot_longer(cols = all_of(numeric_cols),  # Only pivot numeric region columns
                   names_to = "Region",
                   values_to = "Value")
    # Define x-axis order based on the original data structure
    column_order <- unique(ASYN_PDD_long$Region)
    
    # Summarize data: Calculate mean Value for each Region and Braak ASyn group
    ASYN_PDD_summary <- ASYN_PDD_long %>%
      group_by(ApoE, Region) %>%
      summarise(Mean_Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
      filter(!is.nan(Mean_Value))  # Remove cases where all values were NA
    
    
    ASYN_PDD_plot <- ggplot(ASYN_PDD_summary, aes(x = Region, y = Mean_Value, color = as.factor(ApoE), group = as.factor(ApoE))) +
      geom_line(size = 0.8) +  # Thinner lines
      geom_point(size = 2.5) +   # Smaller points
      scale_x_discrete(limits = column_order) +  # Maintain original x-axis order
      ylim(0,100)  +
      scale_color_manual(
        values = c("ApoE3" = "orange", "ApoE4" = "darkred"),
        labels = c("ApoE3" = bquote("PDD"~italic("APOE")~"ε3"), "ApoE4" = bquote("PDD"~italic("APOE")~"ε4")) 
      ) +
      labs(
        title = "",
        x = NULL,  # Remove x-axis title
        y = "LB/mm²",
        color = NULL  # Remove color legend title
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 20),  # Rotate and adjust size
        axis.text.y = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        plot.title = element_text(size = 30, hjust = 0.5),
        legend.position = "top",  # Adjust legend position
        legend.text = element_text(size = 20),
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_blank()  # Remove minor gridlines
      )+
      guides(color = guide_legend(override.aes = list(size = 5)))  # **Increase legend icon size**
    
    # Display the plot
    print(ASYN_PDD_plot)
    
    # Save Dementia (DEM_1) plot
    ggsave(filename = "ASYN_PDD_ApoE.png",
           plot = ASYN_PDD_plot,
           width = 10, height = 7, dpi = 300)
    
    library(dunn.test)
    
    # Function to run tests and export results
    
    
    dunn_test_export <- function(df_long, label) {
      omnibus_list <- list()
      pairwise_list <- list()
      
      for (region in unique(df_long$Region)) {
        region_data <- df_long %>%
          filter(Region == region) %>%
          filter(!is.na(Value), !is.na(ApoE)) %>%
          mutate(ApoE = droplevels(factor(ApoE)))
        
        # need at least 2 groups
        k <- nlevels(region_data$ApoE)
        if (k < 2) next
        
        # group sizes
        n_per_group <- table(region_data$ApoE)
        # if any group has <2 obs, dunn.test can misbehave
        if (any(n_per_group < 2)) {
          # do an omnibus if k==2 and both groups have at least 1 obs
          if (k == 2 && all(n_per_group >= 1)) {
            w <- wilcox.test(Value ~ ApoE, data = region_data, exact = FALSE)
            omnibus_list[[region]] <- data.frame(
              Region   = region,
              Test     = "Wilcoxon rank-sum",
              Groups   = paste(levels(region_data$ApoE), collapse = " vs "),
              Stat     = unname(w$statistic),
              P_value  = unname(w$p.value),
              stringsAsFactors = FALSE
            )
          }
          next
        }
        
        if (k == 2) {
          # With two groups, use Wilcoxon instead of Dunn
          w <- wilcox.test(Value ~ ApoE, data = region_data, exact = FALSE)
          omnibus_list[[region]] <- data.frame(
            Region   = region,
            Test     = "Wilcoxon rank-sum",
            Groups   = paste(levels(region_data$ApoE), collapse = " vs "),
            Stat     = unname(w$statistic),
            P_value  = unname(w$p.value),
            stringsAsFactors = FALSE
          )
          # no pairwise table to add (Wilcoxon already is the pairwise)
          
        } else {
          # k >= 3: Kruskal + Dunn
          kw <- kruskal.test(Value ~ ApoE, data = region_data)
          omnibus_list[[region]] <- data.frame(
            Region   = region,
            Test     = "Kruskal-Wallis",
            Groups   = paste(levels(region_data$ApoE), collapse = " / "),
            Stat     = unname(kw$statistic),
            P_value  = unname(kw$p.value),
            stringsAsFactors = FALSE
          )
          
          # Try dunn.test; if it errors (ties/edge cases), fall back to FSA::dunnTest
          dt <- try(
            dunn.test(region_data$Value, region_data$ApoE, method = "bh", kw = FALSE),
            silent = TRUE
          )
          
          if (inherits(dt, "try-error")) {
            # Uncomment if you want the fallback:
            # dt2 <- FSA::dunnTest(Value ~ ApoE, data = region_data, method = "bh")
            # res_df <- dt2$res %>%
            #   transmute(
            #     Region = region,
            #     Comparison = Comparison,
            #     Z = Z,
            #     p = P.unadj,
            #     p_adj = P.adj
            #   )
            # pairwise_list[[paste0(region, "_FSA")]] <- res_df
          } else {
            res_df <- data.frame(
              Region     = region,
              Comparison = dt$comparisons,
              Z          = dt$Z,
              p          = dt$P,
              p_adj      = dt$P.adjusted,
              stringsAsFactors = FALSE
            )
            pairwise_list[[region]] <- res_df
          }
        }
      }
      
      omnibus <- dplyr::bind_rows(omnibus_list)
      pairwise <- dplyr::bind_rows(pairwise_list)
      
      # optional: adjust omnibus p-values across regions
      if (nrow(omnibus) > 0) {
        omnibus$P_adj_BH <- p.adjust(omnibus$P_value, method = "BH")
      }
      
      write.csv(omnibus,  paste0("ASYN_", label, "_Omnibus.csv"), row.names = FALSE)
      write.csv(pairwise, paste0("ASYN_", label, "_DunnPairwise.csv"), row.names = FALSE)
      
      invisible(list(omnibus = omnibus, pairwise = pairwise))
    }
    
    # Run for PD
    dunn_test_export(ASYN_PD_long, "PD")
    
    # Run for PDD
    dunn_test_export(ASYN_PDD_long, "PDD")
    
    
    
    ##########################################################ABETA
    ABETA <- read_excel(#set excel) %>%
      as.data.frame()
      
      ABETA_PD <- ABETA %>%
        filter(GROUP == "PD")
      
      # Ensure DEMENTIA is factor (in case it's being read as character)
      ABETA_PD <- ABETA_PD %>%
        mutate(ApoE = as.factor(ApoE))
      
      # Get numeric columns (excluding categorical columns)
      numeric_cols <- ABETA_PD %>%
        select(where(is.numeric)) %>%
        colnames()
      
      # Pivot longer (keeping `Braak ABETA` and `DEMENTIA` unchanged)
      ABETA_PD_long <- ABETA_PD %>%
        pivot_longer(cols = all_of(numeric_cols),  # Only pivot numeric region columns
                     names_to = "Region",
                     values_to = "Value")
      # Define x-axis order based on the original data structure
      column_order <- unique(ABETA_PD_long$Region)
      
      # Summarize data: Calculate mean Value for each Region and Braak ABETA group
      ABETA_PD_summary <- ABETA_PD_long %>%
        group_by(ApoE, Region) %>%
        summarise(Mean_Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
        filter(!is.nan(Mean_Value))  # Remove cases where all values were NA
      
      
      ABETA_PD_plot <- ggplot(ABETA_PD_summary, aes(x = Region, y = Mean_Value, color = as.factor(ApoE), group = as.factor(ApoE))) +
        geom_line(size = 0.8) +  # Thinner lines
        geom_point(size = 2.5) +   # Smaller points
        scale_x_discrete(limits = column_order) +  # Maintain original x-axis order
        ylim(0,5)  +
        scale_color_manual(
          values = c("ApoE3" = "dodgerblue", "ApoE4" = "darkblue"),
          labels = c("ApoE3" = bquote("PD"~italic("ApoE")~"ε3"), "ApoE4" = bquote("PD"~italic("ApoE")~"ε4"))
        ) +
        labs(
          title = "",
          x = NULL,  # Remove x-axis title
          y = "% Aβ",
          color = NULL  # Remove color legend title
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 20),  # Rotate and adjust size
          axis.text.y = element_text(size = 25),
          axis.title.y = element_text(size = 25),
          plot.title = element_text(size = 30, hjust = 0.5),
          legend.position = "none",  # Adjust legend position
          legend.text = element_text(size = 20),
          panel.grid.major = element_line(color = "gray80"),
          panel.grid.minor = element_blank()  # Remove minor gridlines
        )
      
      # Display the plot
      print(ABETA_PD_plot)
      
      # Save Dementia (DEM_1) plot
      ggsave(filename = "ABETA_PD_ApoE.png",
             plot = ABETA_PD_plot,
             width = 10, height = 7, dpi = 300)
      
      ABETA_PDD <- ABETA %>%
        filter(GROUP == "PDD")
      
      # Ensure DEMENTIA is factor (in case it's being read as character)
      ABETA_PDD <- ABETA_PDD %>%
        mutate(ApoE = as.factor(ApoE))
      
      # Get numeric columns (excluding categorical columns)
      numeric_cols <- ABETA_PDD %>%
        select(where(is.numeric)) %>%
        colnames()
      
      # Pivot longer (keeping `Braak ABETA` and `DEMENTIA` unchanged)
      ABETA_PDD_long <- ABETA_PDD %>%
        pivot_longer(cols = all_of(numeric_cols),  # Only pivot numeric region columns
                     names_to = "Region",
                     values_to = "Value")
      # Define x-axis order based on the original data structure
      column_order <- unique(ABETA_PDD_long$Region)
      
      # Summarize data: Calculate mean Value for each Region and Braak ABETA group
      ABETA_PDD_summary <- ABETA_PDD_long %>%
        group_by(ApoE, Region) %>%
        summarise(Mean_Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
        filter(!is.nan(Mean_Value))  # Remove cases where all values were NA
      
      
      ABETA_PDD_plot <- ggplot(ABETA_PDD_summary, aes(x = Region, y = Mean_Value, color = as.factor(ApoE), group = as.factor(ApoE))) +
        geom_line(size = 0.8) +  # Thinner lines
        geom_point(size = 2.5) +   # Smaller points
        scale_x_discrete(limits = column_order) +  # Maintain original x-axis order
        ylim(0,5)  +
        scale_color_manual(
          values = c("ApoE3" = "orange", "ApoE4" = "darkred"),
          labels = c("ApoE3" = bquote("PDD"~italic("ApoE")~"ε3"), "ApoE4" = bquote("PDD"~italic("ApoE")~"ε4"))
        ) +
        labs(
          title = "",
          x = NULL,  # Remove x-axis title
          y = "% Aβ",
          color = NULL  # Remove color legend title
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 20),  # Rotate and adjust size
          axis.text.y = element_text(size = 25),
          axis.title.y = element_text(size = 25),
          plot.title = element_text(size = 30, hjust = 0.5),
          legend.position = "none",  # Adjust legend position
          legend.text = element_text(size = 20),
          panel.grid.major = element_line(color = "gray80"),
          panel.grid.minor = element_blank()  # Remove minor gridlines
        )
      
      # Display the plot
      print(ABETA_PDD_plot)
      
      # Save Dementia (DEM_1) plot
      ggsave(filename = "ABETA_PDD_ApoE.png",
             plot = ABETA_PDD_plot,
             width = 10, height = 7, dpi = 300)
      
      library(dunn.test)
      
      # Function to run tests and export results
      library(dplyr)
      library(dunn.test)  # for dunn.test()
      # install.packages("FSA") if you want the fallback
      # library(FSA)
      
      dunn_test_export <- function(df_long, label) {
        omnibus_list <- list()
        pairwise_list <- list()
        
        for (region in unique(df_long$Region)) {
          region_data <- df_long %>%
            filter(Region == region) %>%
            filter(!is.na(Value), !is.na(ApoE)) %>%
            mutate(ApoE = droplevels(factor(ApoE)))
          
          # need at least 2 groups
          k <- nlevels(region_data$ApoE)
          if (k < 2) next
          
          # group sizes
          n_per_group <- table(region_data$ApoE)
          # if any group has <2 obs, dunn.test can misbehave
          if (any(n_per_group < 2)) {
            # do an omnibus if k==2 and both groups have at least 1 obs
            if (k == 2 && all(n_per_group >= 1)) {
              w <- wilcox.test(Value ~ ApoE, data = region_data, exact = FALSE)
              omnibus_list[[region]] <- data.frame(
                Region   = region,
                Test     = "Wilcoxon rank-sum",
                Groups   = paste(levels(region_data$ApoE), collapse = " vs "),
                Stat     = unname(w$statistic),
                P_value  = unname(w$p.value),
                stringsAsFactors = FALSE
              )
            }
            next
          }
          
          if (k == 2) {
            # With two groups, use Wilcoxon instead of Dunn
            w <- wilcox.test(Value ~ ApoE, data = region_data, exact = FALSE)
            omnibus_list[[region]] <- data.frame(
              Region   = region,
              Test     = "Wilcoxon rank-sum",
              Groups   = paste(levels(region_data$ApoE), collapse = " vs "),
              Stat     = unname(w$statistic),
              P_value  = unname(w$p.value),
              stringsAsFactors = FALSE
            )
            # no pairwise table to add (Wilcoxon already is the pairwise)
            
          } else {
            # k >= 3: Kruskal + Dunn
            kw <- kruskal.test(Value ~ ApoE, data = region_data)
            omnibus_list[[region]] <- data.frame(
              Region   = region,
              Test     = "Kruskal-Wallis",
              Groups   = paste(levels(region_data$ApoE), collapse = " / "),
              Stat     = unname(kw$statistic),
              P_value  = unname(kw$p.value),
              stringsAsFactors = FALSE
            )
            
            # Try dunn.test; if it errors (ties/edge cases), fall back to FSA::dunnTest
            dt <- try(
              dunn.test(region_data$Value, region_data$ApoE, method = "bh", kw = FALSE),
              silent = TRUE
            )
            
            if (inherits(dt, "try-error")) {
              # Uncomment if you want the fallback:
              # dt2 <- FSA::dunnTest(Value ~ ApoE, data = region_data, method = "bh")
              # res_df <- dt2$res %>%
              #   transmute(
              #     Region = region,
              #     Comparison = Comparison,
              #     Z = Z,
              #     p = P.unadj,
              #     p_adj = P.adj
              #   )
              # pairwise_list[[paste0(region, "_FSA")]] <- res_df
            } else {
              res_df <- data.frame(
                Region     = region,
                Comparison = dt$comparisons,
                Z          = dt$Z,
                p          = dt$P,
                p_adj      = dt$P.adjusted,
                stringsAsFactors = FALSE
              )
              pairwise_list[[region]] <- res_df
            }
          }
        }
        
        omnibus <- dplyr::bind_rows(omnibus_list)
        pairwise <- dplyr::bind_rows(pairwise_list)
        
        # optional: adjust omnibus p-values across regions
        if (nrow(omnibus) > 0) {
          omnibus$P_adj_BH <- p.adjust(omnibus$P_value, method = "BH")
        }
        
        write.csv(omnibus,  paste0("ASYN_", label, "_Omnibus.csv"), row.names = FALSE)
        write.csv(pairwise, paste0("ASYN_", label, "_DunnPairwise.csv"), row.names = FALSE)
        
        invisible(list(omnibus = omnibus, pairwise = pairwise))
      }
      
      # Run for PD
      dunn_test_export(ABETA_PD_long, "PD")
      
      # Run for PDD
      dunn_test_export(ABETA_PDD_long, "PDD")
      
      
      
      ##########################################################AT8
      AT8 <- read_excel(#set excel) %>%
        as.data.frame()
        
        AT8_PD <- AT8 %>%
          filter(GROUP == "PD")
        
        # Ensure DEMENTIA is factor (in case it's being read as character)
        AT8_PD <- AT8_PD %>%
          mutate(ApoE = as.factor(ApoE))
        
        # Get numeric columns (excluding categorical columns)
        numeric_cols <- AT8_PD %>%
          select(where(is.numeric)) %>%
          colnames()
        
        # Pivot longer (keeping `Braak AT8` and `DEMENTIA` unchanged)
        AT8_PD_long <- AT8_PD %>%
          pivot_longer(cols = all_of(numeric_cols),  # Only pivot numeric region columns
                       names_to = "Region",
                       values_to = "Value")
        # Define x-axis order based on the original data structure
        column_order <- unique(AT8_PD_long$Region)
        
        # Summarize data: Calculate mean Value for each Region and Braak AT8 group
        AT8_PD_summary <- AT8_PD_long %>%
          group_by(ApoE, Region) %>%
          summarise(Mean_Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
          filter(!is.nan(Mean_Value))  # Remove cases where all values were NA
        
        
        AT8_PD_plot <- ggplot(AT8_PD_summary, aes(x = Region, y = Mean_Value, color = as.factor(ApoE), group = as.factor(ApoE))) +
          geom_line(size = 0.8) +  # Thinner lines
          geom_point(size = 2.5) +   # Smaller points
          scale_x_discrete(limits = column_order) +  # Maintain original x-axis order
          scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +  
          scale_color_manual(
            values = c("ApoE3" = "dodgerblue", "ApoE4" = "darkblue"),
            labels = c("ApoE3" = bquote("PD"~italic("APOE")~"ε3"), "ApoE4" = bquote("PD"~italic("APOE")~"ε4"))
          ) +
          labs(
            title = "",
            x = NULL,  # Remove x-axis title
            y = "% pTau",
            color = NULL  # Remove color legend title
          ) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 20),  # Rotate and adjust size
            axis.text.y = element_text(size = 25),
            axis.title.y = element_text(size = 25),
            plot.title = element_text(size = 30, hjust = 0.5),
            legend.position = "none",  # Adjust legend position
            legend.text = element_text(size = 20),
            panel.grid.major = element_line(color = "gray80"),
            panel.grid.minor = element_blank()  # Remove minor gridlines
          )
        
        # Display the plot
        print(AT8_PD_plot)
        
        # Save Dementia (DEM_1) plot
        ggsave(filename = "AT8_PD_ApoE.png",
               plot = AT8_PD_plot,
               width = 10, height = 7, dpi = 300)
        
        AT8_PDD <- AT8 %>%
          filter(GROUP == "PDD")
        
        # Ensure DEMENTIA is factor (in case it's being read as character)
        AT8_PDD <- AT8_PDD %>%
          mutate(ApoE = as.factor(ApoE))
        
        # Get numeric columns (excluding categorical columns)
        numeric_cols <- AT8_PDD %>%
          select(where(is.numeric)) %>%
          colnames()
        
        # Pivot longer (keeping `Braak AT8` and `DEMENTIA` unchanged)
        AT8_PDD_long <- AT8_PDD %>%
          pivot_longer(cols = all_of(numeric_cols),  # Only pivot numeric region columns
                       names_to = "Region",
                       values_to = "Value")
        # Define x-axis order based on the original data structure
        column_order <- unique(AT8_PDD_long$Region)
        
        # Summarize data: Calculate mean Value for each Region and Braak AT8 group
        AT8_PDD_summary <- AT8_PDD_long %>%
          group_by(ApoE, Region) %>%
          summarise(Mean_Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
          filter(!is.nan(Mean_Value))  # Remove cases where all values were NA
        
        
        AT8_PDD_plot <- ggplot(AT8_PDD_summary, aes(x = Region, y = Mean_Value, color = as.factor(ApoE), group = as.factor(ApoE))) +
          geom_line(size = 0.8) +  # Thinner lines
          geom_point(size = 2.5) +   # Smaller points
          scale_x_discrete(limits = column_order) +  # Maintain original x-axis order
          scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +  
          scale_color_manual(
            values = c("ApoE3" = "orange", "ApoE4" = "darkred"),
            labels = c("ApoE3" = bquote("PDD"~italic("APOE")~"ε3"), "APOE4" = bquote("PDD"~italic("APOE")~"ε4"))
          ) +
          labs(
            title = "",
            x = NULL,  # Remove x-axis title
            y = "% pTau",
            color = NULL  # Remove color legend title
          ) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 20),  # Rotate and adjust size
            axis.text.y = element_text(size = 25),
            axis.title.y = element_text(size = 25),
            plot.title = element_text(size = 30, hjust = 0.5),
            legend.position = "none",  # Adjust legend position
            legend.text = element_text(size = 20),
            panel.grid.major = element_line(color = "gray80"),
            panel.grid.minor = element_blank()  # Remove minor gridlines
          )
        
        # Display the plot
        print(AT8_PDD_plot)
        
        # Save Dementia (DEM_1) plot
        ggsave(filename = "AT8_PDD_ApoE.png",
               plot = AT8_PDD_plot,
               width = 10, height = 7, dpi = 300)
        
        library(dunn.test)
        
        # Function to run tests and export results
        library(dplyr)
        library(dunn.test)  # for dunn.test()
        # install.packages("FSA") if you want the fallback
        # library(FSA)
        
        dunn_test_export <- function(df_long, label) {
          omnibus_list <- list()
          pairwise_list <- list()
          
          for (region in unique(df_long$Region)) {
            region_data <- df_long %>%
              filter(Region == region) %>%
              filter(!is.na(Value), !is.na(ApoE)) %>%
              mutate(ApoE = droplevels(factor(ApoE)))
            
            # need at least 2 groups
            k <- nlevels(region_data$ApoE)
            if (k < 2) next
            
            # group sizes
            n_per_group <- table(region_data$ApoE)
            # if any group has <2 obs, dunn.test can misbehave
            if (any(n_per_group < 2)) {
              # do an omnibus if k==2 and both groups have at least 1 obs
              if (k == 2 && all(n_per_group >= 1)) {
                w <- wilcox.test(Value ~ ApoE, data = region_data, exact = FALSE)
                omnibus_list[[region]] <- data.frame(
                  Region   = region,
                  Test     = "Wilcoxon rank-sum",
                  Groups   = paste(levels(region_data$ApoE), collapse = " vs "),
                  Stat     = unname(w$statistic),
                  P_value  = unname(w$p.value),
                  stringsAsFactors = FALSE
                )
              }
              next
            }
            
            if (k == 2) {
              # With two groups, use Wilcoxon instead of Dunn
              w <- wilcox.test(Value ~ ApoE, data = region_data, exact = FALSE)
              omnibus_list[[region]] <- data.frame(
                Region   = region,
                Test     = "Wilcoxon rank-sum",
                Groups   = paste(levels(region_data$ApoE), collapse = " vs "),
                Stat     = unname(w$statistic),
                P_value  = unname(w$p.value),
                stringsAsFactors = FALSE
              )
              # no pairwise table to add (Wilcoxon already is the pairwise)
              
            } else {
              # k >= 3: Kruskal + Dunn
              kw <- kruskal.test(Value ~ ApoE, data = region_data)
              omnibus_list[[region]] <- data.frame(
                Region   = region,
                Test     = "Kruskal-Wallis",
                Groups   = paste(levels(region_data$ApoE), collapse = " / "),
                Stat     = unname(kw$statistic),
                P_value  = unname(kw$p.value),
                stringsAsFactors = FALSE
              )
              
              # Try dunn.test; if it errors (ties/edge cases), fall back to FSA::dunnTest
              dt <- try(
                dunn.test(region_data$Value, region_data$ApoE, method = "bh", kw = FALSE),
                silent = TRUE
              )
              
              if (inherits(dt, "try-error")) {
                # Uncomment if you want the fallback:
                # dt2 <- FSA::dunnTest(Value ~ ApoE, data = region_data, method = "bh")
                # res_df <- dt2$res %>%
                #   transmute(
                #     Region = region,
                #     Comparison = Comparison,
                #     Z = Z,
                #     p = P.unadj,
                #     p_adj = P.adj
                #   )
                # pairwise_list[[paste0(region, "_FSA")]] <- res_df
              } else {
                res_df <- data.frame(
                  Region     = region,
                  Comparison = dt$comparisons,
                  Z          = dt$Z,
                  p          = dt$P,
                  p_adj      = dt$P.adjusted,
                  stringsAsFactors = FALSE
                )
                pairwise_list[[region]] <- res_df
              }
            }
          }
          
          omnibus <- dplyr::bind_rows(omnibus_list)
          pairwise <- dplyr::bind_rows(pairwise_list)
          
          # optional: adjust omnibus p-values across regions
          if (nrow(omnibus) > 0) {
            omnibus$P_adj_BH <- p.adjust(omnibus$P_value, method = "BH")
          }
          
          write.csv(omnibus,  paste0("ASYN_", label, "_Omnibus.csv"), row.names = FALSE)
          write.csv(pairwise, paste0("ASYN_", label, "_DunnPairwise.csv"), row.names = FALSE)
          
          invisible(list(omnibus = omnibus, pairwise = pairwise))
        }
        
        # Run for PD
        dunn_test_export(ASYN_PD_long, "PD")
        
        # Run for PDD
        dunn_test_export(ASYN_PDD_long, "PDD")
        
        
        
#PD vs. PDD graphs and all comparisons#############
        
        library(readxl)
        library(dplyr)
        library(tidyr)
        library(ggplot2)
        
        datadir <- "DIR"
        infile  <- file.path(datadir, ".xlsx")
        
        outdir  <- file.path(datadir, "PD_PDD_LINE_GRAPHS_PDvsPDD_byApoE")
        dir.create(outdir, showWarnings = FALSE)
        setwd(outdir)
        
        
        # Helpers
        
        # Read + reshape a sheet, then keep only PD/PDD rows
        make_long <- function(sheet_name) {
          df <- read_excel(infile, sheet = sheet_name) |> as.data.frame()
          stopifnot(all(c("GROUP","ApoE") %in% names(df)))
          df <- df |> filter(GROUP %in% c("PD","PDD"))
          num_cols <- df |> select(where(is.numeric)) |> colnames()
          df_long <- df |>
            pivot_longer(cols = all_of(num_cols),
                         names_to = "Region",
                         values_to = "Value") |>
            mutate(
              GROUP = factor(GROUP, levels = c("PD","PDD")),
              ApoE  = factor(ApoE,  levels = c("ApoE3","ApoE4"))
            )
          df_long
        }
        
        # Per-genotype PD vs PDD stats for each Region
        wilcoxon_pd_pdd_by_region <- function(df_long, genotype_label) {
          dat <- df_long |> filter(ApoE == genotype_label)
          regions <- sort(unique(dat$Region))
          res <- lapply(regions, function(rg) {
            sub <- dat |> filter(Region == rg) |> filter(!is.na(Value))
            if (length(unique(sub$GROUP)) < 2) return(NULL)
            if (any(table(sub$GROUP) == 0)) return(NULL)
            w <- suppressWarnings(wilcox.test(Value ~ GROUP, data = sub, exact = FALSE))
            data.frame(
              ApoE   = genotype_label,
              Region = rg,
              Test   = "Wilcoxon rank-sum",
              Groups = "PD vs PDD",
              N_PD   = sum(sub$GROUP == "PD",  na.rm = TRUE),
              N_PDD  = sum(sub$GROUP == "PDD", na.rm = TRUE),
              Stat   = unname(w$statistic),
              P_value = unname(w$p.value),
              stringsAsFactors = FALSE
            )
          })
          res <- bind_rows(res)
          if (nrow(res) > 0) res$P_adj_BH <- p.adjust(res$P_value, method = "BH")
          res
        }
        
        # Plot PD vs PDD within a genotype
        plot_pd_pdd <- function(df_long, genotype_label,
                                y_lab, y_lim = NULL, y_breaks = waiver(),
                                color_PD, color_PDD,
                                title_prefix) {
          df_sum <- df_long |>
            filter(ApoE == genotype_label) |>
            group_by(GROUP, Region) |>
            summarise(Mean_Value = mean(Value, na.rm = TRUE), .groups = "drop") |>
            filter(!is.nan(Mean_Value))
          x_order <- unique(df_long$Region)
          gg <- ggplot(df_sum, aes(x = Region, y = Mean_Value,
                                   color = GROUP, group = GROUP)) +
            geom_line(size = 0.8) +
            geom_point(size = 2.5) +
            scale_x_discrete(limits = x_order) +
            labs(title = paste0(title_prefix, " – ", genotype_label),
                 x = NULL, y = y_lab, color = NULL) +
            theme_minimal() +
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1, size = 20),
              axis.text.y = element_text(size = 25),
              axis.title.y = element_text(size = 25),
              plot.title   = element_text(size = 28, hjust = 0.5),
              legend.position = "top",
              legend.text = element_text(size = 18),
              panel.grid.major = element_line(color = "gray80"),
              panel.grid.minor = element_blank()
            ) +
            scale_color_manual(values = c("PD" = color_PD, "PDD" = color_PDD),
                               labels = c("PD" = "PD", "PDD" = "PDD"))
          if (!is.null(y_lim)) gg <- gg + scale_y_continuous(limits = y_lim, breaks = y_breaks)
          gg
        }
        
        
        #Cross-genotype comparator
        
        # Compare two specific (GROUP, ApoE) cohorts per Region (Wilcoxon), with plot
        wilcoxon_cross_by_region <- function(df_long,
                                             group1, apoe1,
                                             group2, apoe2,
                                             label_for_plot_1, label_for_plot_2,
                                             color1, color2,
                                             y_lab, y_lim = NULL, y_breaks = waiver(),
                                             file_tag, suffix_tag) {
          sub <- df_long |> filter((GROUP == group1 & ApoE == apoe1) | (GROUP == group2 & ApoE == apoe2))
          sub <- sub |> mutate(Combo = ifelse(GROUP == group1 & ApoE == apoe1, label_for_plot_1, label_for_plot_2))
          regions <- sort(unique(sub$Region))
          
          # Stats per Region
          res <- lapply(regions, function(rg) {
            s <- sub |> filter(Region == rg) |> filter(!is.na(Value))
            if (length(unique(s$Combo)) < 2) return(NULL)
            if (any(table(s$Combo) == 0)) return(NULL)
            w <- suppressWarnings(wilcox.test(Value ~ Combo, data = s, exact = FALSE))
            data.frame(
              Region  = rg,
              Test    = "Wilcoxon rank-sum",
              Groups  = paste(label_for_plot_1, "vs", label_for_plot_2),
              N_1     = sum(s$Combo == label_for_plot_1, na.rm = TRUE),
              N_2     = sum(s$Combo == label_for_plot_2, na.rm = TRUE),
              Stat    = unname(w$statistic),
              P_value = unname(w$p.value),
              stringsAsFactors = FALSE
            )
          }) |> bind_rows()
          if (nrow(res) > 0) res$P_adj_BH <- p.adjust(res$P_value, method = "BH")
          
          # Mean lines for each combo
          df_sum <- sub |>
            group_by(Combo, Region) |>
            summarise(Mean_Value = mean(Value, na.rm = TRUE), .groups = "drop") |>
            filter(!is.nan(Mean_Value))
          x_order <- unique(sub$Region)
          g <- ggplot(df_sum, aes(x = Region, y = Mean_Value, color = Combo, group = Combo)) +
            geom_line(size = 0.8) +
            geom_point(size = 2.5) +
            scale_x_discrete(limits = x_order) +
            labs(title = paste0(file_tag, " – ", label_for_plot_1, " vs ", label_for_plot_2),
                 x = NULL, y = y_lab, color = NULL) +
            theme_minimal() +
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1, size = 20),
              axis.text.y = element_text(size = 25),
              axis.title.y = element_text(size = 25),
              plot.title   = element_text(size = 26, hjust = 0.5),
              legend.position = "top",
              legend.text = element_text(size = 18),
              panel.grid.major = element_line(color = "gray80"),
              panel.grid.minor = element_blank()
            ) +
            scale_color_manual(values = c(setNames(color1, label_for_plot_1),
                                          setNames(color2, label_for_plot_2)))
          if (!is.null(y_lim)) g <- g + scale_y_continuous(limits = y_lim, breaks = y_breaks)
          
          # Save
          ggsave(paste0(file_tag, "_", suffix_tag, ".png"), g, width = 10, height = 7, dpi = 300)
          if (nrow(res) > 0) {
            write.csv(res, paste0(file_tag, "_", suffix_tag, "_Wilcoxon.csv"), row.names = FALSE)
          }
          
          list(plot = g, stats = res)
        }
        
        # Run a whole module (sheet) → plots + stats for ApoE3 and ApoE4
        run_module <- function(sheet_name, y_lab, y_lim = NULL, y_breaks = waiver(),
                               col_PD_ApoE3 = "dodgerblue",
                               col_PDD_ApoE3 = "orange",
                               col_PD_ApoE4 = "darkblue",
                               col_PDD_ApoE4 = "darkred",
                               file_tag) {
          
          df_long <- make_long(sheet_name)
          
          # === Within-genotype PD vs PDD ===
          p3 <- plot_pd_pdd(df_long, "ApoE3", y_lab, y_lim, y_breaks,
                            color_PD = col_PD_ApoE3, color_PDD = col_PDD_ApoE3,
                            title_prefix = file_tag)
          ggsave(paste0(file_tag, "_ApoE3_PDvsPDD.png"), p3, width = 10, height = 7, dpi = 300)
          stats3 <- wilcoxon_pd_pdd_by_region(df_long, "ApoE3")
          if (nrow(stats3) > 0) write.csv(stats3, paste0(file_tag, "_ApoE3_PDvsPDD_Wilcoxon.csv"), row.names = FALSE)
          
          p4 <- plot_pd_pdd(df_long, "ApoE4", y_lab, y_lim, y_breaks,
                            color_PD = col_PD_ApoE4, color_PDD = col_PDD_ApoE4,
                            title_prefix = file_tag)
          ggsave(paste0(file_tag, "_ApoE4_PDvsPDD.png"), p4, width = 10, height = 7, dpi = 300)
          stats4 <- wilcoxon_pd_pdd_by_region(df_long, "ApoE4")
          if (nrow(stats4) > 0) write.csv(stats4, paste0(file_tag, "_ApoE4_PDvsPDD_Wilcoxon.csv"), row.names = FALSE)
          
          # === NEW: Cross-genotype comparisons ===
          # 1) PD (ApoE3) vs PDD (ApoE4)
          cross1 <- wilcoxon_cross_by_region(
            df_long,
            group1 = "PD",  apoe1 = "ApoE3",
            group2 = "PDD", apoe2 = "ApoE4",
            label_for_plot_1 = "PD_ApoE3",
            label_for_plot_2 = "PDD_ApoE4",
            color1 = col_PD_ApoE3,
            color2 = col_PDD_ApoE4,
            y_lab = y_lab, y_lim = y_lim, y_breaks = y_breaks,
            file_tag = file_tag, suffix_tag = "PD_ApoE3_vs_PDD_ApoE4"
          )
          
          # 2) PD (ApoE4) vs PDD (ApoE3)
          cross2 <- wilcoxon_cross_by_region(
            df_long,
            group1 = "PD",  apoe1 = "ApoE4",
            group2 = "PDD", apoe2 = "ApoE3",
            label_for_plot_1 = "PD_ApoE4",
            label_for_plot_2 = "PDD_ApoE3",
            color1 = col_PD_ApoE4,
            color2 = col_PDD_ApoE3,
            y_lab = y_lab, y_lim = y_lim, y_breaks = y_breaks,
            file_tag = file_tag, suffix_tag = "PD_ApoE4_vs_PDD_ApoE3"
          )
          
          list(
            plots = list(ApoE3 = p3, ApoE4 = p4,
                         Cross_PD_ApoE3_vs_PDD_ApoE4 = cross1$plot,
                         Cross_PD_ApoE4_vs_PDD_ApoE3 = cross2$plot),
            stats = list(ApoE3 = stats3, ApoE4 = stats4,
                         Cross_PD_ApoE3_vs_PDD_ApoE4 = cross1$stats,
                         Cross_PD_ApoE4_vs_PDD_ApoE3 = cross2$stats)
          )
        }
        
        
        # RUN: ASYN, ABETA, AT8
        
        
        # ASYN: LB/mm², 0–100
        res_asyn <- run_module(
          sheet_name = "ASYN",
          y_lab = "LB/mm²",
          y_lim = c(0, 100),
          y_breaks = seq(0, 100, 20),
          file_tag = "ASYN"
        )
        
        # ABETA: % Aβ, 0–5
        res_abeta <- run_module(
          sheet_name = "ABETA",
          y_lab = "% Aβ",
          y_lim = c(0, 5),
          y_breaks = seq(0, 5, 1),
          file_tag = "ABETA"
        )
        
        # AT8: % pTau, 0–10
        res_at8 <- run_module(
          sheet_name = "AT8",
          y_lab = "% pTau",
          y_lim = c(0, 10),
          y_breaks = seq(0, 10, 2),
          file_tag = "AT8"
        )
        
        # Optional: preview one plot
        print(res_asyn$plots$ApoE3)
        

        
