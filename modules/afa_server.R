afa_server <- function(id, afa_joined, afa_acl_plot_All, afa_acl_plot_CP,  afa_acl_plot_CV, 
                       afa_ent_plot, afa_utilz_plot, afa_eff_plot, afa_rev_plot_All, 
                       afa_rev_plot_CP, afa_rev_plot_CV, afa_gini_plot_All,
                       afa_gini_plot_CP, afa_gini_plot_CV,
                       afa_rev_per_plot) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # 1. BASE FORMATTED DATA (Long format, ALL Sectors) - Used for the Modal
    afa_base_formatted <- reactive({
      afa_joined %>% 
        mutate(across(-c("Season"), ~ifelse(is.na(.), "-", .))) %>% 
        mutate(across(-c("Season"), ~as.character(.))) %>% 
        pivot_longer(., cols= -c("Season", "sector"), names_to = "varname", values_to = "Value") %>% 
        mutate(Value = suppressWarnings(case_when(!(varname %in% c("ACL exceeded species", "Share cap in place %",
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
    
    # 2. MAIN UI FILTERED METRICS (Pivoted, Filtered by Sector) - Used for the Main Table
    afa_final_metrics <- reactive({
      afa_base_formatted() %>% 
        pivot_wider(., names_from = "Season", values_from = "Value") %>% 
        filter(!if_all(.cols = -c(sector, varname), .fns = ~ .x == "-")) %>% 
        filter(sector == input$afa_sector)
    })
    
    observe({
      updatePickerInput(session = session, inputId = "afa_varname",
                        choices = unique(afa_final_metrics()$varname), 
                        selected = c("Aggregate Landings (mt)","Aggregate revenue from Catch Share species")) 
    })
    
    # Use the LONG base formatted data, filter by sector, and pass to the smart function
    output$afa_table <- render_gt({
      
      filtered_data <- afa_base_formatted() %>%
        filter(sector == input$afa_sector) # Filter down to the user's selected sector
      
      build_metrics_gt(
        cleaned_data = filtered_data,
        selected_vars = input$afa_varname,
        header_bg = "#205E42" # AFA Color!
      )
    })
    
    # -----------------------------------------------------
    # PREVIEW MODAL LOGIC
    # -----------------------------------------------------
    
    # Trigger Modal
    observeEvent(input$afa_preview_btn, {
      show_preview_modal(
        ns = ns, 
        table_id = "afa_preview_table", 
        download_id = "afa_download_csv",
        title = "AFA Full Dataset Preview"
      )
    })
    
    # Render Modal Table using the UNFILTERED base data
    output$afa_preview_table <- render_gt({
      build_preview_gt(
        raw_data = afa_base_formatted(),
        header_bg = "#205E42" # AFA Color
      )
    })
    
    # Download Handler for CSV
    output$afa_download_csv <- downloadHandler(
      filename = function() { "afa_full_metrics.csv" },
      content = function(file) {
        write.csv(afa_joined, file, row.names = FALSE)
      }
    )
    
    # -----------------------------------------------------
    # PLOTS
    # -----------------------------------------------------
    
    output$afa_lands_plot <- renderPlotly({
      if(input$afa_lands_sector == "All"){
        afa_acl_plot_All$All
      }else if(input$afa_lands_sector == "CP"){
        afa_acl_plot_CP$All
      } else if(input$afa_lands_sector == "CV"){
        afa_acl_plot_CV$All
      }  
    })
    
    output$afa_hs_plot <- renderPlotly({
      afa_ent_plot
    })
    
    output$afa_utliz_plot <- renderPlotly({
      afa_utilz_plot      
    })
    
    output$afa_effort_plot <- renderPlotly({
      if(input$afa_effort == "Active vessels" & !is.null(afa_eff_plot$`Active vessels`)){
        afa_eff_plot$`Active vessels`
      } else if(input$afa_effort == "Days at sea" & !is.null(afa_eff_plot$`Days at sea`)){
        afa_eff_plot$`Days at sea` %>% 
          layout(
            annotations = list(
              x = 1,  
              y = -0.15,
              text = "Note: AFA CV days at sea only begins in 2008",
              showarrow = FALSE,
              xref = 'paper', 
              yref = 'paper',
              xanchor = 'right',
              yanchor = 'top',
              font = list(size = 12, color = 'gray')
            ),
            margin = list(b = 100)  # add bottom margin for space
          )
      } else if(input$afa_effort == "Season length" & !is.null(afa_eff_plot$`Season length`)){
        afa_eff_plot$`Season length`
      } 
    })
    
    output$afa_rev_plot <- renderPlotly({
      if(input$afa_rev_sector == "All"){
        afa_rev_plot_All
      }else if(input$afa_rev_sector == "CP"){
        afa_rev_plot_CP
      } else if(input$afa_rev_sector == "CV"){
        afa_rev_plot_CV
      }  
    })
    
    output$afa_gini_plot <- renderPlotly({
      if(input$afa_gini_sector == "All"){
        afa_gini_plot_All
      }else if(input$afa_gini_sector == "CP"){
        afa_gini_plot_CP
      } else if(input$afa_gini_sector == "CV"){
        afa_gini_plot_CV
      }  
    })
    
    output$afa_rev_per_plot <- renderPlotly({
      if(input$afa_rev_per == "Total Revenue/vessel" & !is.null(afa_rev_per_plot[[1]])){
        afa_rev_per_plot[[1]]
      } else if(input$afa_rev_per == "Total Revenue/day at sea" & !is.null(afa_rev_per_plot[[2]])){
        afa_rev_per_plot[[2]]
      }
    })
    
  })
}