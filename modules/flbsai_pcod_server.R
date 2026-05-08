flbsai_pcod_server <- function(id, flbsai_joined, flbsai_plot_df, flbsai_acl_plot,
                               flbsai_ent_plot, flbsai_utilz_plot, flbsai_eff_plot, flbsai_rev_plot,
                               flbsai_gini_plot,flbsai_rev_per_plot) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    ## ---------------------------------------------------------------------------------------------------
    ## FLBSAI P.cod Data Reactives
    ## --------------------------------------------------------------------------------------------------
    
    # 1. BASE FORMATTED DATA (Long format) - Used for Modal & Main Table
    flbsai_base_formatted <- reactive({
      flbsai_joined %>% 
        mutate(across(-c("Season"), ~ifelse(is.na(.), "-", .))) %>% 
        mutate(across(-c("Season"), ~as.character(.))) %>% 
        pivot_longer(., cols= -c("Season"), names_to = "varname", values_to = "Value") %>% 
        mutate(Value = suppressWarnings(
          case_when(!(varname %in% c("ACL exceeded", "ACL exceeded species", 
                                     "Limited Entry Y/N","Gini Coefficient","Season length", 
                                     "Average price")) &
                      Value != "-" ~ format(round(as.numeric(Value),0), big.mark = ","),
                    TRUE ~ as.character(Value)))) %>% 
        mutate(Value = suppressWarnings(
          case_when((varname %in% c("Aggregate revenue from species in fishery",
                                    "Aggregate revenue from species not in fishery",
                                    "Cost recovery fee amount",
                                    "Average price",
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
    
    # 2. Update UI Dropdown
    observe({
      updatePickerInput(session = session, inputId = "flbsai_varname",
                        choices = unique(flbsai_base_formatted()$varname), 
                        selected = c("Aggregate Landings (mt)","Aggregate revenue from species in the fisherys")) 
    })
    
    # 3. RENDER MAIN UI TABLE (Uses the smart function)
    output$flbsai_table <- render_gt({
      build_metrics_gt(
        cleaned_data = flbsai_base_formatted(),
        selected_vars = input$flbsai_varname,
        header_bg = "#3C3F42" # FLBSAI Color
      )
    })
    
    ## ---------------------------------------------------------------------------------------------------
    ## PREVIEW MODAL LOGIC
    ## --------------------------------------------------------------------------------------------------
    
    # Trigger Modal
    observeEvent(input$flbsai_preview_btn, {
      show_preview_modal(
        ns = ns, 
        table_id = "flbsai_preview_table", 
        download_id = "flbsai_download_csv",
        title = "FLBSAI P.cod Full Dataset Preview"
      )
    })
    
    # Render Modal Table
    output$flbsai_preview_table <- render_gt({
      build_preview_gt(
        raw_data = flbsai_base_formatted(),
        header_bg = "#3C3F42" # FLBSAI Color
      )
    })
    
    # Download Handler for CSV
    output$flbsai_download_csv <- downloadHandler(
      filename = function() { "flbsai_pcod_full_metrics.csv" },
      content = function(file) {
        write.csv(flbsai_joined, file, row.names = FALSE)
      }
    )
    
    ## ---------------------------------------------------------------------------------------------------
    ## PLOTS
    ## --------------------------------------------------------------------------------------------------
    
    output$flbsai_lands_plot <- renderPlotly({
      if(length(input$flbsai_lands) > 1){
        flbsai_acl_plot$All
      }else if(input$flbsai_lands == "Aggregate Landings"){
        flbsai_acl_plot$`Aggregate Landings`
      } else if(input$flbsai_lands == "ACL or Quota/TAC"){
        flbsai_acl_plot$`ACL or Quota/TAC`
      }  
    })
    
    output$flbsai_utliz_plot <- renderPlotly({
      flbsai_utilz_plot
    })
    
    output$flbsai_effort_plot <- renderPlotly({
      if(input$flbsai_effort == "Active vessels" & !is.null(flbsai_eff_plot$`Active vessels`)){
        flbsai_eff_plot$`Active vessels`
      } else if(input$flbsai_effort == "Days at sea"& !is.null(flbsai_eff_plot$`Days at sea`)){
        flbsai_eff_plot$`Days at sea` %>% 
          layout(
            annotations = list(
              x = 1,  
              y = -0.15,
              text = "Note: No data is currently available to calculate days at sea for this fishery prior to 2007.",
              showarrow = FALSE,
              xref = 'paper', 
              yref = 'paper',
              xanchor = 'right',
              yanchor = 'top',
              font = list(size = 12, color = 'gray')
            ),
            margin = list(b = 100)  # add bottom margin for space
          )
      } else if(input$flbsai_effort == "Season length"& !is.null(flbsai_eff_plot$`Season length`)){
        flbsai_eff_plot$`Season length`
      }
    })
    
    output$flbsai_rev_plot <- renderPlotly({
      flbsai_rev_plot
    })
    
    output$flbsai_rev_per_plot <- renderPlotly({
      if(input$flbsai_rev_per == "Total Revenue/vessel" & !is.null(flbsai_rev_per_plot[[1]])){
        flbsai_rev_per_plot[[1]]
      } else if(input$flbsai_rev_per == "Total Revenue/day at sea" & !is.null(flbsai_rev_per_plot[[2]])){
        flbsai_rev_per_plot[[2]]
      } 
    })
    
    output$flbsai_gini_plot <- renderPlotly({
      df_f <- flbsai_plot_df %>% filter(varname == "Gini Coefficient")
      
      # Generating custom ggplotly for FLBSAI Gini
      flbsai_custom_gini_plot <- ggplot(data= df_f,
                                        aes(x = Season, y = Val,group = varname, fill=varname, 
                                            text = paste("Season:", Season, "<br> Gini coefficient", Val)),
                                        show.legend = FALSE, color = "grey")+
        geom_line()+  
        geom_point()+
        theme_minimal()+
        scale_fill_manual(values = c("#CD888C"))+
        scale_color_manual(values = c("#CD888C"))+
        scale_y_continuous(expand = expansion(mult = c(0, 0.01)), limits = c(0, max(df_f$Val)))+
        labs(x = "Season", y = "Gini coefficient")+
        theme(axis.text.x = element_text(angle = -45, vjust = 0.5, hjust=1))
      
      ggplotly(flbsai_custom_gini_plot, tooltip = "text") %>% 
        layout(showlegend=FALSE)
    })
    
  })
}