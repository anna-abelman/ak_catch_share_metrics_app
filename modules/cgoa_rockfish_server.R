cgoa_rockfish_server <- function(id,  cgoa_rock_joined, cgoa_rock_acl_plot_All, cgoa_rock_acl_plot_CP,  cgoa_rock_acl_plot_CV,
                                 cgoa_rock_ent_plot, cgoa_rock_utilz_plot, 
                                 cgoa_rock_eff_plot, cgoa_rock_rev_plot_All,cgoa_rock_rev_plot_CP, cgoa_rock_rev_plot_CV,
                                 cgoa_rock_gini_plot_All, cgoa_rock_gini_plot_CP, cgoa_rock_gini_plot_CV,
                                 cgoa_rock_rev_per_plot) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
 
    ## ---------------------------------------------------------------------------------------------------
    ## CGOA Rockfish Plots
    ## --------------------------------------------------------------------------------------------------
    
    cgoa_rf_final_metrics <- reactive({
      cgoa_rf_final_metrics <- cgoa_rock_joined %>% 
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
                                   TRUE ~ varname)) %>% 
        pivot_wider(., names_from = "Season", values_from = "Value") %>% 
        filter(!if_all(.cols = -c(sector, varname), .fns = ~ .x == "-") ) %>% 
        filter(sector == input$cgoa_rf_sector)
        
    })
    
    observe({
      updatePickerInput(session = session, inputId = "cgoa_rf_varname",
                        choices = unique(cgoa_rf_final_metrics()$varname), 
                        selected = c("Aggregate Landings (mt)","Aggregate revenue from Catch Share species")) 
      
    })
    
    
    cgoa_rf_df_stats <- reactive({
      
      cgoa_rf_final_metrics <- cgoa_rf_final_metrics() %>% 
        filter(varname %in% c(input$cgoa_rf_varname) & sector == input$cgoa_rf_sector) %>% 
        select(-sector) %>% 
        gt(rowname_col = "varname") %>% 
        opt_row_striping() %>%
        tab_options(
          column_labels.background.color = "#2C3544",
          column_labels.font.weight = "bold",
          row.striping.background_color = "#f9f9fb",
          heading.title.font.size = px(22),
          column_labels.font.size = px(16),
          stub.font.size = "small",
          table.font.size = "small",
          data_row.padding = px(2),
        ) %>%
        cols_width(everything() ~px(100)) %>%
        tab_style(
          style = list(
            cell_text(color = "white")
          ),
          locations = cells_column_labels()
        )%>% 
        tab_footnote("'-' indicates data not available for metric. Confidential data is suppressed and specified by 'Conf.' in the table above.")
    })
    
    output$cgoa_rf_table <- render_gt({
      expr = cgoa_rf_df_stats()
    })
    
    cgoa_rf_csv<-reactive(cgoa_rock_joined)
    
    output$cgoa_rf_csv<-downloadHandler(
      filename = function() { "cgoa_rf_full_metrics.csv" },
      
      content = function(file) {
        write.csv(cgoa_rf_csv(), file)
      }
    )
    
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
            p,
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