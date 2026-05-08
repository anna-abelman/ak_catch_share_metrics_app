crab_server <- function(id, crab_joined, crab_acl_plot,
                        crab_ent_plot, crab_utilz_plot, crab_eff_plot, crab_rev_plot,
                        crab_gini_plot,crab_rev_per_plot) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    ## ---------------------------------------------------------------------------------------------------
    ## BSAI Crab Data Reactives
    ## --------------------------------------------------------------------------------------------------
    
    # 1. BASE FORMATTED DATA (Long format) - Used for Modal & Main Table 
    bsai_crab_base_formatted <- reactive({
      crab_joined %>% 
        mutate(across(-c("Season"), ~ifelse(is.na(.), "-", .))) %>% 
        mutate(across(-c("Season"), ~as.character(.))) %>% 
        pivot_longer(., cols= -c("Season"), names_to = "varname", values_to = "Value") %>% 
        mutate(Value = suppressWarnings(
          case_when(!(varname %in% c("ACL exceeded", "ACL exceeded species", "Share cap in place",
                                     "Cost recovery fee amount",
                                     "Gini coefficient","Season length", 
                                     "Average price")) & Value != "-" ~ format(round(as.numeric(Value),0), 
                                                                               big.mark = ","),
                    TRUE ~ as.character(Value)))) %>% 
        mutate(Value = suppressWarnings(
          case_when((varname %in% c("Aggregate revenue from Catch Share species",
                                    "Aggregate revenue from non-catch share species",
                                    "Cost recovery fee amount",
                                    "Average price",
                                    "Total Revenue/vessel",
                                    "Total Revenue/trip" ,
                                    "Total Revenue/day at sea") & Value != "-") ~ paste("$", Value),
                    TRUE ~ as.character(Value)))) %>%
        mutate(Value = ifelse(varname != "ACL exceeded species", str_replace_all(Value, " ", ""), Value)) %>% 
        mutate(varname = case_when(varname == "Aggregate Landings" ~ "Aggregate Landings (lbs)",
                                   varname == "Quota allocated to CS program" ~ "Quota allocated to CS program (lbs)",
                                   varname == "Average price" ~ "Average price ($/lbs)",
                                   TRUE ~ varname)) 
    })
    
    # 2. Update UI Dropdown
    observe({
      updatePickerInput(session = session, inputId = "crab_varname",
                        choices = unique(bsai_crab_base_formatted()$varname), 
                        selected = c("Aggregate Landings (lbs)","Aggregate revenue from Catch Share species")) 
    })
    
    # 3. RENDER MAIN UI TABLE (Uses the smart function)
    output$crab_table <- render_gt({
      build_metrics_gt(
        cleaned_data = bsai_crab_base_formatted(),
        selected_vars = input$crab_varname,
        header_bg = "#475D52" # BSAI Crab Color
      )
    })
    
    ## ---------------------------------------------------------------------------------------------------
    ## PREVIEW MODAL LOGIC
    ## --------------------------------------------------------------------------------------------------
    
    # Trigger Modal
    observeEvent(input$crab_preview_btn, {
      show_preview_modal(
        ns = ns, 
        table_id = "crab_preview_table", 
        download_id = "crab_download_csv",
        title = "BSAI Crab Full Dataset Preview"
      )
    })
    
    # Render Modal Table
    output$crab_preview_table <- render_gt({
      build_preview_gt(
        raw_data = bsai_crab_base_formatted(),
        header_bg = "#475D52" # BSAI Crab Color
      )
    })
    
    # Download Handler for CSV
    output$crab_download_csv <- downloadHandler(
      filename = function() { "bsai_crab_full_metrics.csv" },
      content = function(file) {
        write.csv(crab_joined, file, row.names = FALSE)
      }
    )
    
    ## ---------------------------------------------------------------------------------------------------
    ## PLOTS
    ## --------------------------------------------------------------------------------------------------
    
    output$crab_lands_plot <- renderPlotly({
      if(length(input$crab_lands) > 1){
        crab_acl_plot$All
      }else if(input$crab_lands == "Aggregate Landings"){
        crab_acl_plot$`Aggregate Landings`
      } else if(input$crab_lands == "Quota allocated to CS program"){
        crab_acl_plot$`Quota allocated to CS program`
      }  
    })
    
    output$crab_hs_plot <- renderPlotly({
      crab_ent_plot
    })
    
    output$crab_utliz_plot <- renderPlotly({
      crab_utilz_plot
    })
    
    output$crab_effort_plot <- renderPlotly({
      if(input$crab_effort == "Active vessels" & !is.null(crab_eff_plot$`Active vessels`)){
        crab_eff_plot$`Active vessels`
      } else if(input$crab_effort == "Days at sea" & !is.null(crab_eff_plot$`Days at sea`)){
        crab_eff_plot$`Days at sea`
      } else if(input$crab_effort == "Season length" & !is.null(crab_eff_plot$`Season length`)){
        crab_eff_plot$`Season length`
      } else if(input$crab_effort == "Trips" & !is.null(crab_eff_plot$Trips)){
        crab_eff_plot$Trips
      }
    })
    
    output$crab_rev_plot <- renderPlotly({
      crab_rev_plot
    })
    
    output$crab_gini_plot <- renderPlotly({
      crab_gini_plot
    })
    
    output$crab_rev_per_plot <- renderPlotly({
      if(input$crab_rev_per == "Total Revenue/vessel"& !is.null(crab_rev_per_plot[[1]])){
        crab_rev_per_plot[[1]]
      } else if(input$crab_rev_per == "Total Revenue/day at sea"& !is.null(crab_rev_per_plot[[2]])){
        crab_rev_per_plot[[2]]
      } else if(input$crab_rev_per == "Total Revenue/trip"& !is.null(crab_rev_per_plot[[3]])){
        crab_rev_per_plot[[3]]
      }  
    })
    
  })
}