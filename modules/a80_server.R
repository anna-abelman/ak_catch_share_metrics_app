a80_server <- function(id, a80_joined, a80_acl_plot,
                       a80_ent_plot, a80_utilz_plot, a80_eff_plot, a80_rev_plot,
                       a80_gini_plot,a80_rev_per_plot) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    
    a80_final_metrics <- reactive({
      
      a80_final_metrics <- a80_joined %>% 
        mutate(across(-c("Season"), ~ifelse(is.na(.), "-", .))) %>% 
        mutate(across(-c("Season"), ~as.character(.))) %>% 
        pivot_longer(., cols= -c("Season"), names_to = "varname", values_to = "Value") %>% 
        mutate(Value = suppressWarnings(case_when(!(varname %in% c("ACL exceeded Y/N", "Share cap in place",
                                                                   "Share cap %","Gini coefficient",
                                                                   "Season length", "Average price"
        )) & Value != "-" ~ format(round(as.numeric(Value),0), big.mark = ","),
        TRUE ~ as.character(Value)))) %>%
        mutate(Value = suppressWarnings(case_when((varname %in% c("Aggregate revenue from Catch Share species",
                                                                  "Aggregate revenue from non-catch share species",
                                                                  "Average price",
                                                                  "Cost recovery fee amount",
                                                                  "Total Revenue/vessel",
                                                                  "Total Revenue/trip" ,
                                                                  "Total Revenue/day at sea") & Value != "-") ~ paste("$", Value),
                                                  TRUE ~ as.character(Value)))) %>% 
        mutate(Value = str_replace_all(Value, " ", "")) %>% 
        mutate(varname = case_when(varname == "Aggregate Landings" ~ "Aggregate Landings (mt)",
                                   varname == "Quota allocated to CS program" ~ "Quota allocated to CS program (mt)",
                                   varname == "Average price" ~ "Average price ($/mt)",
                                   TRUE ~ varname)) 
    })
    
    observe({
      updatePickerInput(session = session, inputId = "a80_varname",
                        choices = unique(a80_final_metrics()$varname), 
                        selected = c("Aggregate revenue from Catch Share species",
                                     "Aggregate revenue from non-catch share species",
                                     "Average Price ($/mt)",
                                     "Total Revenue/vessel",
                                     "Total Revenue/trip" ,
                                     "Total Revenue/day at sea")) 
    })
    
    
    a80_df_stats <- reactive({
      
      a80_final_metrics <- a80_final_metrics() %>% 
        pivot_wider(., names_from = "Season", values_from = "Value")  %>%   
        filter(varname %in% c(input$a80_varname)) %>% 
        gt(rowname_col = "varname") %>% 
        opt_row_striping() %>%
        tab_options(
          column_labels.background.color = "#262626",
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
    
    output$a80_table <- render_gt({
      expr = a80_df_stats()
    })
    
    a80_csv<-reactive(a80_joined)
    
    output$a80_csv<-downloadHandler(
      filename = function() { "a80_full_metrics.csv" },
      
      content = function(file) {
        write.csv(a80_csv(), file)
      }
    )
    
    
    output$a80_lands_plot <- renderPlotly({
      
      
      if(length(input$a80_lands) > 1){
        a80_acl_plot$All
      }else if(input$a80_lands == "Aggregate Landings"){
        a80_acl_plot$`Aggregate Landings`
        
      } else if(input$a80_lands == "Quota allocated to CS program"){
        a80_acl_plot$`Quota allocated to CS program`
        
      }
      
    })
    
    
    output$a80_hs_plot <- renderPlotly({
      a80_ent_plot
    })
    
    output$a80_utliz_plot <- renderPlotly({
      a80_utilz_plot
    })
    
    output$a80_effort_plot <- renderPlotly({
      
      if(input$a80_effort == "Active vessels"& !is.null(a80_eff_plot[2])){
        a80_eff_plot$`Active vessels`
      } else if(input$a80_effort == "Days at sea"& !is.null(a80_eff_plot[3])){
        a80_eff_plot$`Days at sea`
      
      } else if(input$a80_effort == "Season length" & !is.null(a80_eff_plot[1])){
        a80_eff_plot$`Season length`
      } 
    })
    
    output$a80_rev_plot <- renderPlotly({
      a80_rev_plot
    })
    
    output$a80_gini_plot <- renderPlotly({
      a80_gini_plot
    })
    
    output$a80_rev_per_plot <- renderPlotly({
      if(input$a80_rev_per == "Total Revenue/vessel"& !is.null(a80_rev_per_plot[[1]])){
        a80_rev_per_plot[[1]]
      } else if(input$a80_rev_per == "Total Revenue/day at sea"& !is.null(a80_rev_per_plot[[2]])){
        a80_rev_per_plot[[2]]
        } 
      
    })
    
    
  })
  
}
