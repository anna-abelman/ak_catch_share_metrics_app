build_metrics_gt <- function(cleaned_data, selected_vars, header_bg = "#1E293B") {
  
  # Check if 'sector' exists in the incoming data
  has_sector <- "sector" %in% names(cleaned_data)
  
  # 1. PREP THE DATA
  raw_filtered <- cleaned_data %>% 
    filter(varname %in% selected_vars) %>%
    mutate(
      varname = factor(varname, levels = selected_vars), # Lock row order
      Season = as.character(Season),
      Value_num = suppressWarnings(as.numeric(str_remove_all(Value, "[\\$, ]"))),
      Value_num = na_if(Value_num, 0) 
    ) 
  
  # Determine chronological order
  all_seasons <- unique(raw_filtered$Season)
  numeric_years <- sort(as.numeric(all_seasons[all_seasons != "BASELINE"]))
  correct_season_order <- c("BASELINE", as.character(numeric_years))
  
  # Apply factor to Season and setup dynamic grouping
  if (has_sector) {
    raw_filtered <- raw_filtered %>%
      mutate(Season = factor(Season, levels = correct_season_order, ordered = TRUE)) %>%
      arrange(sector, varname, Season)
    
    group_vars <- c("sector", "varname")
  } else {
    raw_filtered <- raw_filtered %>%
      mutate(Season = factor(Season, levels = correct_season_order, ordered = TRUE)) %>%
      arrange(varname, Season)
    
    group_vars <- "varname"
  }
  
  last_5_years <- tail(as.character(numeric_years), 5)
  
  # 2. SUMMARIZE THE DATA
  spark_df <- raw_filtered %>% 
    group_by(across(all_of(group_vars))) %>% # Dynamically groups by sector if it exists
    summarise(
      Baseline = Value[Season == "BASELINE"][1],
      `Yr-4` = nth(Value[Season != "BASELINE"], -5),
      `Yr-3` = nth(Value[Season != "BASELINE"], -4),
      `Yr-2` = nth(Value[Season != "BASELINE"], -3),
      `Yr-1` = nth(Value[Season != "BASELINE"], -2),
      `Current` = last(Value[Season != "BASELINE"]),
      
      Baseline_num = Value_num[Season == "BASELINE"][1],
      Yr_1_num = nth(Value_num[Season != "BASELINE"], -2),
      Current_num = last(Value_num[Season != "BASELINE"]),
      
      `% Change (1-Yr)` = if_else(
        !is.na(Yr_1_num) & Yr_1_num != 0,
        (Current_num - Yr_1_num) / Yr_1_num,
        NA_real_
      ),
      `% Change (vs Baseline)` = if_else(
        !is.na(Baseline_num) & Baseline_num != 0,
        (Current_num - Baseline_num) / Baseline_num,
        NA_real_
      ),
      Trend = list(Value_num), 
      .groups = "drop"
    ) %>%
    select(-Baseline_num, -Yr_1_num, -Current_num) 
  
  # 3. FORMATTING TWEAKS
  target_cols <- c("Baseline", "Yr-4", "Yr-3", "Yr-2", "Yr-1", "Current")
  
  spark_df <- spark_df %>%
    mutate(
      across(all_of(target_cols), ~ case_when(
        varname == "Season length" & !is.na(.) & . != "Conf." & . != "-" ~ .,
        varname == "Gini coefficient" & !is.na(.) & . != "Conf." & . != "-" ~ format(round(as.numeric(.), 2), nsmall = 2),
        TRUE ~ as.character(.) 
      ))
    )
  
  # 4. INITIALIZE GT TABLE (With or Without Row Grouping)
  if (has_sector) {
    gt_tbl <- spark_df %>% gt(rowname_col = "varname", groupname_col = "sector")
  } else {
    gt_tbl <- spark_df %>% gt(rowname_col = "varname")
  }
  
  # 5. BUILD & RETURN THE TABLE
  final_table <- gt_tbl %>%
    cols_label(
      `Yr-4` = as.character(last_5_years[1]),
      `Yr-3` = as.character(last_5_years[2]),
      `Yr-2` = as.character(last_5_years[3]),
      `Yr-1` = as.character(last_5_years[4]),
      `Current` = as.character(last_5_years[5]),
      `% Change (1-Yr)` = "YoY Change",
      `% Change (vs Baseline)` = "Change vs Baseline"
    ) %>%
    fmt_percent(
      columns = c(`% Change (1-Yr)`, `% Change (vs Baseline)`), 
      decimals = 1, 
      force_sign = TRUE
    ) %>%
    sub_missing(columns = everything(), missing_text = "-") %>%
    gtExtras::gt_plt_sparkline(
      column = Trend, 
      type = "default",
      same_limit = FALSE, 
      palette = c("black", "black", "red", "blue", "lightgrey"),
      fig_dim = c(10, 45)
    ) %>%
    opt_table_font(font = google_font("Inter")) %>%
    cols_align(align = "left", columns = stub()) %>%
    cols_align(align = "right", columns = everything()) %>% 
    cols_width(
      stub() ~ px(220),            
      Baseline ~ px(100),          
      c(`% Change (1-Yr)`, `% Change (vs Baseline)`) ~ px(75), 
      Trend ~ px(160),             
      everything() ~ px(100)       
    ) %>%
    opt_row_striping() %>%
    tab_options(
      table.width = pct(100),
      column_labels.background.color = header_bg, # Dynamic Header Color
      column_labels.font.weight = "bold",
      column_labels.text_transform = "uppercase",
      column_labels.font.size = px(12),
      
      # Style the sector grouping rows (only shows up if sector exists)
      row_group.background.color = "#E2E8F0",
      row_group.font.weight = "bold",
      
      row.striping.background_color = "#F8FAFC",
      stub.font.size = px(13),
      table.font.size = px(13),
      data_row.padding = px(8), 
      table_body.hlines.color = "#E2E8F0",
      table_body.hlines.width = px(1),
      table.border.top.style = "hidden",
      table.border.bottom.style = "hidden"
    ) %>%
    tab_style(
      style = cell_text(color = "white"),
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = cell_text(whitespace = "nowrap"),
      locations = cells_body()
    ) %>%
    tab_spanner(label = "Past 5 Years", columns = c(`Yr-4`, `Yr-3`, `Yr-2`, `Yr-1`, `Current`)) %>%
    tab_spanner(label = "Performance", columns = c(`% Change (1-Yr)`, `% Change (vs Baseline)`)) %>%
    tab_spanner(label = "Full History", columns = c(Trend)) %>%
    tab_footnote("'-' indicates data not available for metric. Confidential data is suppressed and specified by 'Conf.' in the table above.")
  
  return(final_table)
}