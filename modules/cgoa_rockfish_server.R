cgoa_rockfish_server <- function(id,  cgoa_rock_joined, cgoa_rock_acl_plot_All, cgoa_rock_acl_plot_CP,  cgoa_rock_acl_plot_CV,
                                 cgoa_rock_ent_plot, cgoa_rock_utilz_plot, 
                                 cgoa_rock_eff_plot, cgoa_rock_rev_plot_All,cgoa_rock_rev_plot_CP, cgoa_rock_rev_plot_CV,
                                 cgoa_rock_gini_plot_All, cgoa_rock_gini_plot_CP, cgoa_rock_gini_plot_CV,
                                 cgoa_rock_rev_per_plot) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    ## ---------------------------------------------------------------------------------------------------
    ## CGOA Rockfish Data Reactives
    ## --------------------------------------------------------------------------------------------------
    
    # 1. BASE FORMATTED DATA (Long format, ALL Sectors) - Used for the Modal
    cgoa_rf_base_formatted <- reactive({
      cgoa_rock_joined %>% 
        mutate(across(-c("Season"), ~ifelse(is.na(.), "-", .))) %>% 
        mutate(across(-c("Season"), ~as.character(.))) %>% 
        pivot_longer(., cols= -c("Season", "sector"), names_to = "varname", values_to = "Value") %>% 
        mutate(Value = suppressWarnings(case_when(!(varname %in% c("ACL exceeded species", 
                                                                   "Gini coefficient","Season length", 
                                                                   "Average price")) & Value != "-" ~ format(round(as.numeric(Value),0), big.mark = ","),
                                                  TRUE ~ as.character(Value)))) %>% 
        mutate(Value = suppressWarnings(case_when((varname %in% c("Aggregate revenue from Catch Share species",
                                                                  "Aggregate revenue from non-catch share species",
                                                                  "Average price",
                                                                  "Cost recovery fee amount",
                                                                  "Total Revenue/vessel",
                                                                  "Total Revenue/trip" ,
                                                                  "Total Revenue/day at sea") & Value != "-") ~ paste("$", Value),
                                                  TRUE ~ as.character(Value)))) %>% 
        mutate(Value = ifelse(varname != "ACL exceeded species", str_replace_all(Value, " ", ""), Value)) %>% 
        mutate(varname = case_when(varname == "Aggregate Landings" ~ "Aggregate Landings (mt)",
                                   varname == "Quota allocated to CS program" ~ "Quota allocated to CS program (mt)",
                                   varname == "Average price" ~ "Average price ($/mt)",
                                   TRUE ~ varname))
    })
    
    # 2. MAIN UI FILTERED METRICS (Pivoted, Filtered by Sector) - Used for UI Dropdowns
    cgoa_rf_final_metrics <- reactive({
      cgoa_rf_base_formatted() %>% 
        pivot_wider(., names_from = "Season", values_from = "Value") %>% 
        filter(!if_all(.cols = -c(sector, varname), .fns = ~ .x == "-") ) %>% 
        filter(sector == input$cgoa_rf_sector)
    })
    
    observe({
      updatePickerInput(session = session, inputId = "cgoa_rf_varname",
                        choices = unique(cgoa_rf_final_metrics()$varname), 
                        selected = c("Aggregate Landings (mt)","Aggregate revenue from Catch Share species")) 
    })
    
    # 3. RENDER MAIN UI TABLE (Uses the smart function)
    output$cgoa_rf_table <- render_gt({
      
      filtered_data <- cgoa_rf_base_formatted() %>%
        filter(sector == input$cgoa_rf_sector)
      
      build_metrics_gt(
        cleaned_data = filtered_data,
        selected_vars = input$cgoa_rf_varname,
        header_bg = "#2C3544" # CGOA Rockfish Color
      )
    })
    
    ## ---------------------------------------------------------------------------------------------------
    ## PREVIEW MODAL LOGIC
    ## --------------------------------------------------------------------------------------------------
    
    # Trigger Modal
    observeEvent(input$cgoa_rf_preview_btn, {
      show_preview_modal(
        ns = ns, 
        table_id = "cgoa_rf_preview_table",  # Updated ID
        download_id = "cgoa_rf_download_csv", # Updated ID
        title = "CGOA Rockfish Full Dataset Preview"
      )
    })
    
    # Render Modal Table using the UNFILTERED base data (All Sectors)
    output$cgoa_rf_preview_table <- render_gt({
      build_preview_gt(
        raw_data = cgoa_rf_base_formatted(),
        header_bg = "#2C3544" # CGOA Rockfish Color
      )
    })
    
    # Download Handler for CSV
    output$cgoa_rf_download_csv <- downloadHandler(
      filename = function() { "cgoa_rockfish_full_metrics.csv" },
      content = function(file) {
        write.csv(cgoa_rock_joined, file, row.names = FALSE)
      }
    )
    
    ## ---------------------------------------------------------------------------------------------------
    ## PLOTS
    ## --------------------------------------------------------------------------------------------------
    
    output$cgoa_rf_lands_plot <- renderPlotly({
      if(input$cgoa_rf_lands_sector == "All"){
        cgoa_rock_acl_plot_All$All
      }else if(input$cgoa_rf_lands_sector == "CP"){
        cgoa_rock_acl_plot_CP$All
      } else if(input$cgoa_rf_lands_sector == "CV"){
        cgoa_rock_acl_plot_CV$All
      }  
    })
    
    output$cgoa_rf_hs_plot <- renderPlotly({
      cgoa_rock_ent_plot
    })
    
    output$cgoa_rf_utliz_plot <- renderPlotly({
      cgoa_rock_utilz_plot
    })
    
    output$cgoa_rf_effort_plot <- renderPlotly({
      if(input$cgoa_rf_effort == "Active vessels"& !is.null(cgoa_rock_eff_plot$`Active vessels`)){
        cgoa_rock_eff_plot$`Active vessels`
      } else if(input$cgoa_rf_effort == "Days at sea"& !is.null(cgoa_rock_eff_plot$`Days at sea`)){
        cgoa_rock_eff_plot$`Days at sea` %>% 
          layout(
            annotations = list(
              x = 1,  
              y = -0.15,
              text = "Note: CGOA Rockfish CV days at sea only begins in 2008",
              showarrow = FALSE,
              xref = 'paper', 
              yref = 'paper',
              xanchor = 'right',
              yanchor = 'top',
              font = list(size = 12, color = 'gray')
            ),
            margin = list(b = 100)  # add bottom margin for space
          )
      } else if(input$cgoa_rf_effort == "Season length"& !is.null(cgoa_rock_eff_plot$`Season length`)){
        cgoa_rock_eff_plot$`Season length`
      } 
    })
    
    output$cgoa_rf_rev_plot <- renderPlotly({
      if(input$cgoa_rf_rev_sector == "All"){
        cgoa_rock_rev_plot_All
      }else if(input$cgoa_rf_rev_sector == "CP"){
        cgoa_rock_rev_plot_CP
      } else if(input$cgoa_rf_rev_sector == "CV"){
        cgoa_rock_rev_plot_CV
      }  
    })
    
    output$cgoa_rf_gini_plot <- renderPlotly({
      if(input$cgoa_rf_gini_sector == "All"){
        cgoa_rock_gini_plot_All
      }else if(input$cgoa_rf_gini_sector == "CP"){
        cgoa_rock_gini_plot_CP
      } else if(input$cgoa_rf_gini_sector == "CV"){
        cgoa_rock_gini_plot_CV
      }  
    })
    
    output$cgoa_rf_rev_per_plot <- renderPlotly({
      if(input$cgoa_rf_rev_per == "Total Revenue/vessel"& !is.null(cgoa_rock_rev_per_plot[[1]])){
        cgoa_rock_rev_per_plot[[1]]
      } else if(input$cgoa_rf_rev_per == "Total Revenue/day at sea"& !is.null(cgoa_rock_rev_per_plot[[2]])){
        cgoa_rock_rev_per_plot[[2]]
      } 
    })
    
  })
}