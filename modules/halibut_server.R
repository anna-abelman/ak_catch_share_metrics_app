halibut_server <- function(id, hal_joined, hal_acl_plot, 
                           hal_ent_plot, hal_utilz_plot, hal_eff_plot, hal_rev_plot, hal_gini_plot,
                           hal_rev_per_plot) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    ## ---------------------------------------------------------------------------------------------------
    ## Halibut Data Reactives
    ## --------------------------------------------------------------------------------------------------
    
    # 1. BASE FORMATTED DATA (Long format) - Used for Modal & Main Table
    halibut_base_formatted <- reactive({
      hal_joined %>% 
        mutate(across(-c("Season"), ~ifelse(is.na(.), "-", .))) %>% 
        mutate(across(-c("Season"), ~as.character(.))) %>% 
        pivot_longer(., cols= -c("Season"), names_to = "varname", values_to = "Value") %>% 
        mutate(Value = suppressWarnings(
          case_when(!(varname %in% c("ACL exceeded", "ACL exceeded species", "Share cap in place",
                                     "Share cap in place %", "Gini coefficient","Season length", 
                                     "Average price")) & Value != "-" ~ 
                      format(round(as.numeric(Value),0), big.mark = ","),
                    TRUE ~ as.character(Value)))) %>% 
        mutate(Value = suppressWarnings(
          case_when((varname %in% c("Aggregate revenue from Catch Share species",
                                    "Aggregate revenue from non-catch share species",
                                    "Average price",
                                    "Cost recovery fee amount",
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
      updatePickerInput(session = session, inputId = "halibut_varname",
                        choices = unique(halibut_base_formatted()$varname), 
                        selected = c("Aggregate Landings (lbs)","Aggregate revenue from Catch Share species")) 
    })
    
    # 3. RENDER MAIN UI TABLE (Uses the smart function)
    output$halibut_table <- render_gt({
      build_metrics_gt(
        cleaned_data = halibut_base_formatted(),
        selected_vars = input$halibut_varname,
        header_bg = "#70262B" # Halibut Color
      )
    })
    
    ## ---------------------------------------------------------------------------------------------------
    ## PREVIEW MODAL LOGIC
    ## --------------------------------------------------------------------------------------------------
    
    # Trigger Modal
    observeEvent(input$halibut_preview_btn, {
      show_preview_modal(
        ns = ns, 
        table_id = "halibut_preview_table", 
        download_id = "halibut_download_csv",
        title = "Halibut Full Dataset Preview"
      )
    })
    
    # Render Modal Table
    output$halibut_preview_table <- render_gt({
      build_preview_gt(
        raw_data = halibut_base_formatted(),
        header_bg = "#70262B" # Halibut Color
      )
    })
    
    # Download Handler for CSV
    output$halibut_download_csv <- downloadHandler(
      filename = function() { "halibut_full_metrics.csv" },
      content = function(file) {
        write.csv(hal_joined, file, row.names = FALSE)
      }
    )
    
    ## ---------------------------------------------------------------------------------------------------
    ## PLOTS
    ## --------------------------------------------------------------------------------------------------
    
    output$halibut_lands_plot <- renderPlotly({
      if(length(input$halibut_lands) > 1){
        hal_acl_plot$All
      }else if(input$halibut_lands == "Aggregate Landings"){
        hal_acl_plot$`Aggregate Landings`
      } else if(input$halibut_lands == "Quota allocated to CS program"){
        hal_acl_plot$`Quota allocated to CS program`
      }  
    })
    
    output$halibut_hs_plot <- renderPlotly({
      hal_ent_plot
    })
    
    output$halibut_utliz_plot <- renderPlotly({
      hal_utilz_plot
    })
    
    output$halibut_effort_plot <- renderPlotly({
      if(input$halibut_effort == "Active vessels" & !is.null(hal_eff_plot$`Active vessels`)){
        hal_eff_plot$`Active vessels`
      } else if(input$halibut_effort == "Days at sea"&  !is.null(hal_eff_plot$`Days at sea`)){
        hal_eff_plot$`Days at sea` %>% 
          layout(
            annotations = list(
              x = 1,  
              y = -0.15,
              text = "Note: Halibut days at sea only begins in 2007",
              showarrow = FALSE,
              xref = 'paper', 
              yref = 'paper',
              xanchor = 'right',
              yanchor = 'top',
              font = list(size = 12, color = 'gray')
            ),
            margin = list(b = 100)  # add bottom margin for space
          )
      } else if(input$halibut_effort == "Season length" & !is.null(hal_eff_plot$`Season length`)){
        hal_eff_plot$`Season length`
      } else if(input$halibut_effort =="Trips"& !is.null(hal_eff_plot$Trips)){
        hal_eff_plot$Trips
      }  
    })
    
    output$halibut_rev_plot <- renderPlotly({
      hal_rev_plot
    })
    
    output$halibut_gini_plot <- renderPlotly({
      hal_gini_plot
    })
    
    output$halibut_rev_per_plot <- renderPlotly({
      if(input$halibut_rev_per == "Total Revenue/vessel"& !is.null(hal_rev_per_plot[[1]])){
        hal_rev_per_plot[[1]]
      } else if(input$halibut_rev_per == "Total Revenue/day at sea"& !is.null(hal_rev_per_plot[[2]])){
        hal_rev_per_plot[[2]]
      } else if(input$halibut_rev_per == "Total Revenue/trip"& !is.null(hal_rev_per_plot[[3]])){
        hal_rev_per_plot[[3]]
      }   
    })
    
  })
}