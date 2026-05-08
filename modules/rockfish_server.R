rockfish_server <- function(id, rock_joined, rock_plot_df, rock_acl_plot,
                            rock_ent_plot, rock_utilz_plot, rock_eff_plot, rock_rev_plot, 
                            rock_gini_plot, rock_rev_per_plot) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    ## ---------------------------------------------------------------------------------------------------
    ## Rockfish Data Reactives
    ## --------------------------------------------------------------------------------------------------
    
    # 1. BASE FORMATTED DATA (Long format) - Used for Modal & Main Table
    rockfish_base_formatted <- reactive({
      rock_joined %>% 
        mutate(across(-c("Season"), ~ifelse(is.na(.), "-", .))) %>% 
        mutate(across(-c("Season"), ~as.character(.))) %>% 
        pivot_longer(., cols= -c("Season"), names_to = "varname", values_to = "Value") %>% 
        mutate(Value = suppressWarnings(
          case_when(!(varname %in% c("ACL exceeded", "ACL exceeded species",  
                                     "Limited Entry Y/N", "Gini Coefficient","Season length", 
                                     "Average price")) & 
                      Value != "-" ~ format(round(as.numeric(Value),0), big.mark = ","),
                    TRUE ~ as.character(Value)))) %>% 
        mutate(Value = suppressWarnings(
          case_when((varname %in% c("Aggregate revenue from species in fishery",
                                    "Aggregate revenue from species not in fishery",
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
    
    # 2. Update UI Dropdown
    observe({
      updatePickerInput(session = session, inputId = "goa_rock_varname",
                        choices = unique(rockfish_base_formatted()$varname), 
                        selected = c("Aggregate Landings (mt)","Aggregate revenue from species in the fishery")) 
    })
    
    # 3. RENDER MAIN UI TABLE (Uses the smart function)
    output$goa_rock_table <- render_gt({
      build_metrics_gt(
        cleaned_data = rockfish_base_formatted(),
        selected_vars = input$goa_rock_varname,
        header_bg = "#2C3544" # GOA Rockfish Color
      )
    })
    
    ## ---------------------------------------------------------------------------------------------------
    ## PREVIEW MODAL LOGIC
    ## --------------------------------------------------------------------------------------------------
    
    # Trigger Modal
    observeEvent(input$goa_rock_preview_btn, {
      show_preview_modal(
        ns = ns, 
        table_id = "goa_rock_preview_table", 
        download_id = "goa_rock_download_csv",
        title = "Rockfish Full Dataset Preview"
      )
    })
    
    # Render Modal Table
    output$goa_rock_preview_table <- render_gt({
      build_preview_gt(
        raw_data = rockfish_base_formatted(),
        header_bg = "#2C3544" # GOA Rockfish Color
      )
    })
    
    # Download Handler for CSV
    output$goa_rock_download_csv <- downloadHandler(
      filename = function() { "rockfish_full_metrics.csv" },
      content = function(file) {
        write.csv(rock_joined, file, row.names = FALSE)
      }
    )
    
    ## ---------------------------------------------------------------------------------------------------
    ## PLOTS
    ## --------------------------------------------------------------------------------------------------
    
    output$rockfish_lands_plot <- renderPlotly({
      if(length(input$rockfish_lands) > 1){
        rock_acl_plot$All
      }else if(input$rockfish_lands == "Aggregate Landings"){
        rock_acl_plot$`Aggregate Landings`
      } else if(input$rockfish_lands == "ACL or Quota/TAC"){
        rock_acl_plot$`ACL or Quota/TAC`
      }  
    })
    
    output$rockfish_utliz_plot <- renderPlotly({
      rock_utilz_plot
    })
    
    output$rockfish_effort_plot <- renderPlotly({
      if(input$rockfish_effort == "Active vessels"& !is.null(rock_eff_plot$`Active vessels`)){
        rock_eff_plot$`Active vessels`
      } else if(input$rockfish_effort == "Season length"& !is.null(rock_eff_plot$`Season length`)){
        rock_eff_plot$`Season length`
      } 
    })
    
    output$rockfish_rev_plot <- renderPlotly({
      rock_rev_plot  
    })
    
    output$rockfish_rev_per_plot <- renderPlotly({
      if(input$rockfish_rev_per == "Total Revenue/vessel"& !is.null(rock_rev_per_plot[[1]])){
        rock_rev_per_plot[[1]]
      } 
    })
    
    output$rockfish_gini_plot <- renderPlotly({
      df_f <- rock_plot_df %>% filter(varname == "Gini Coefficient")
      
      rockfish_custom_gini_plot <- ggplot(data= df_f,
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
      
      ggplotly(rockfish_custom_gini_plot, tooltip = "text") %>% 
        layout(showlegend=FALSE)
    }) 
    
  })
}