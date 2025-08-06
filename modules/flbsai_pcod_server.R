
flbsai_pcod_server <- function(id, flbsai_joined, flbsai_plot_df, flbsai_acl_plot,
                               flbsai_ent_plot, flbsai_utilz_plot, flbsai_eff_plot, flbsai_rev_plot,
                               flbsai_gini_plot,flbsai_rev_per_plot) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    

    
    
    ## ---------------------------------------------------------------------------------------------------
    ## FLBSAI P.cod Plots
    ## --------------------------------------------------------------------------------------------------
    flbsai_final_metrics <- reactive({
      
      flbsai_final_metrics <- flbsai_joined %>% 
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
  
  observe({
    updatePickerInput(session = session, inputId = "flbsai_varname",
                      choices = unique(flbsai_final_metrics()$varname), 
                      selected = c("Aggregate Landings (mt)","Aggregate revenue from species in the fisherys")) 
    
  })
  
  flbsai_df_stats <- reactive({
    
    flbsai_final_metrics <- flbsai_final_metrics() %>% 
      pivot_wider(., names_from = "Season", values_from = "Value") %>% 
        filter(varname %in% c(input$flbsai_varname)) %>%
        gt(rowname_col = "varname") %>% 
        opt_row_striping() %>%
        tab_options(
          column_labels.background.color = "#3C3F42",
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
    
    output$flbsai_table <- render_gt({
      expr = flbsai_df_stats()
    })
    
    flbsai_csv<-reactive(flbsai_joined)
    
    output$flbsai_csv<-downloadHandler(
      filename = function() { "flbsai_full_metrics.csv" },
      
      content = function(file) {
        write.csv(flbsai_csv(), file)
      }
    )
    
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
            p,
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
      
      flbsai_gini_plot <- ggplot(data= df_f,
                                 aes(x = Season, y = Val,group = varname, fill=varname, 
                                     text = paste("Season:", Season, "<br> Gini coefficient", Val)),
                                 show.legend = FALSE, color = "grey")+
        geom_line()+  
        geom_point()+
        theme_minimal()+
        scale_fill_manual(values = c("#CD888C"))+
        scale_color_manual(values = c("#CD888C"))+
        # scale_x_discrete(breaks = scales::breaks_pretty(n =8))+
        scale_y_continuous(expand = expansion(mult = c(0, 0.01)), limits = c(0, max(df_f$Val)))+
        labs(x = "Season", y = "Gini coefficient")+
        theme(axis.text.x = element_text(angle = -45, vjust = 0.5, hjust=1))
      
      
      ggplotly(flbsai_gini_plot, tooltip = "text") %>% 
        layout(showlegend=FALSE)
    })
    
  })
}