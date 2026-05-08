scallop_server <- function(id, scallop_joined, scallop_plot_df, scallop_acl_plot,
                           scallop_ent_plot, scallop_utilz_plot, scallop_eff_plot,
                           scallop_rev_plot, scallop_gini_plot, scallop_rev_per_plot) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    ## ---------------------------------------------------------------------------------------------------
    ## Scallop Data Reactives
    ## --------------------------------------------------------------------------------------------------
    
    # 1. BASE FORMATTED DATA (Long format) - Used for Modal & Main Table
    scallop_base_formatted <- reactive({
      scallop_joined %>% 
        mutate(across(-c("Season"), ~ifelse(is.na(.), "-", .))) %>% 
        mutate(across(-c("Season"), ~as.character(.))) %>% 
        pivot_longer(., cols= -c("Season"), names_to = "varname", values_to = "Value") %>% 
        mutate(Value = suppressWarnings(
          case_when(!(varname %in% c("ACL exceeded species","Limited Entry Y/N", 
                                     "Gini Coefficient", "Average price")) 
                    & Value != "-" ~ format(round(as.numeric(Value),0), big.mark = ","),
                    TRUE ~ as.character(Value)))) %>% 
        mutate(Value = suppressWarnings(
          case_when((varname %in% c("Aggregate revenue from species in the fishery",
                                    "Aggregate revenue from species not in the fishery",
                                    "Average price",
                                    "Cost recovery fee amount",
                                    "Total Revenue/vessel",
                                    "Total Revenue/trip" ,
                                    "Total Revenue/day at sea") & Value != "-") ~ paste("$", Value),
                    TRUE ~ as.character(Value)))) %>%
        mutate(Value = ifelse(varname != "ACL exceeded species", str_replace_all(Value, " ", ""), Value)) %>% 
        mutate(Value = str_replace_all(Value, "Pws", "PWS")) %>% 
        mutate(Value = str_replace_all(Value, "Ne", "NE")) %>% 
        mutate(Value = str_replace_all(Value, "^Ak ", "AK ")) %>% 
        mutate(varname = case_when(varname == "Aggregate Landings" ~ "Aggregate Landings (lbs)",
                                   varname == "Quota allocated to CS program" ~ "Quota allocated to CS program (lbs)",
                                   varname == "Average price" ~ "Average price ($/lbs)",
                                   TRUE ~ varname)) %>% 
        mutate(Value = case_when(varname == "Gini Coefficient" & Value == "-" ~ "Conf.",
                                 TRUE ~ Value))
    })
    
    # 2. Update UI Dropdown
    observe({
      updatePickerInput(session = session, inputId = "scallop_varname",
                        choices = unique(scallop_base_formatted()$varname), 
                        selected = c("Aggregate Landings (lbs)","Aggregate revenue from species in the fishery")) 
    })
    
    # 3. RENDER MAIN UI TABLE (Uses the smart function + Custom Footnote)
    output$scallop_table <- render_gt({
      
      base_table <- build_metrics_gt(
        cleaned_data = scallop_base_formatted(),
        selected_vars = input$scallop_varname,
        header_bg = "#015875" # Scallop Color
      )
      
      # Append the custom Scallop footnote
      base_table %>%
        tab_footnote(html('The following metrics were found in the <a href= https://meetings.npfmc.org/CommentReview/DownloadFile?p=9c98c730-537c-4367-9bad-9c982662ceed.pdf&fileName=Scallop%20SAFE%202024%20(REVISED).pdf target="_blank">2024 Scallop Stock Assessment and Fishery Evaluation (SAFE) report:</a> Aggregate revenue from species in the fishery, Average price ($/lbs), and Total Revenue/vessel.'))
    })
    
    ## ---------------------------------------------------------------------------------------------------
    ## PREVIEW MODAL LOGIC
    ## --------------------------------------------------------------------------------------------------
    
    # Trigger Modal
    observeEvent(input$scallop_preview_btn, {
      show_preview_modal(
        ns = ns, 
        table_id = "scallop_preview_table", 
        download_id = "scallop_download_csv",
        title = "Scallop Full Dataset Preview"
      )
    })
    
    # Render Modal Table (Uses the smart function + Custom Footnote)
    output$scallop_preview_table <- render_gt({
      
      base_preview <- build_preview_gt(
        raw_data = scallop_base_formatted(),
        header_bg = "#015875" # Scallop Color
      )
      
      # Append the custom Scallop footnote to the modal as well
      base_preview %>%
        tab_footnote(html('The following metrics were found in the <a href= https://meetings.npfmc.org/CommentReview/DownloadFile?p=9c98c730-537c-4367-9bad-9c982662ceed.pdf&fileName=Scallop%20SAFE%202024%20(REVISED).pdf target="_blank">2024 Scallop Stock Assessment and Fishery Evaluation (SAFE) report:</a> Aggregate revenue from species in the fishery, Average price ($/lbs), and Total Revenue/vessel.'))
    })
    
    # Download Handler for CSV
    output$scallop_download_csv <- downloadHandler(
      filename = function() { "scallop_full_metrics.csv" },
      content = function(file) {
        write.csv(scallop_joined, file, row.names = FALSE)
      }
    )
    
    ## ---------------------------------------------------------------------------------------------------
    ## PLOTS
    ## --------------------------------------------------------------------------------------------------
    
    output$scallop_lands_plot <- renderPlotly({
      if(length(input$scallop_lands) > 1){
        scallop_acl_plot$All
      }else if(input$scallop_lands == "Aggregate Landings"){
        scallop_acl_plot$`Aggregate Landings`
      } else if(input$scallop_lands == "ACL or Quota/TAC"){
        scallop_acl_plot$`ACL or Quota/TAC`
      }  
    })
    
    output$scallop_utliz_plot <- renderPlotly({
      scallop_utilz_plot
    })
    
    output$scallop_effort_plot <- renderPlotly({
      if(input$scallop_effort == "Active vessels" & !is.null(scallop_eff_plot$`Active vessels`)){
        scallop_eff_plot$`Active vessels`
      } else if(input$scallop_effort == "Season length" & !is.null(scallop_eff_plot$`Season length`)){
        scallop_eff_plot$`Season length`
      }
    })
    
    output$scallop_rev_plot <- renderPlotly({
      scallop_rev_plot
    })
    
    output$scallop_rev_per_plot <- renderPlotly({
      if(input$scallop_rev_per == "Total Revenue/vessel"& !is.null(scallop_rev_per_plot[[1]])){
        scallop_rev_per_plot[[1]]
      } 
    })
    
    output$scallop_gini_plot <- renderPlotly({
      df_f <- scallop_plot_df %>% filter(varname == "Gini Coefficient")
      
      scallop_custom_gini_plot <- ggplot(data= df_f,
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
      
      ggplotly(scallop_custom_gini_plot, tooltip = "text") %>% 
        layout(showlegend=FALSE)
    })
    
  })
}