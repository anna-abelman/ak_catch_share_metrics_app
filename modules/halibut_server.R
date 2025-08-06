halibut_server <- function(id, hal_joined, hal_acl_plot, 
                           hal_ent_plot, hal_utilz_plot, hal_eff_plot, hal_rev_plot, hal_gini_plot,
                           hal_rev_per_plot) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
 
    halibut_final_metrics <- reactive({
      
      halibut_final_metrics <- hal_joined %>% 
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
    
    observe({
      updatePickerInput(session = session, inputId = "halibut_varname",
                        choices = unique(halibut_final_metrics()$varname), 
                        selected = c("Aggregate Landings (lbs)","Aggregate revenue from Catch Share species")) 
      
    })
    
    
    halibut_df_stats <- reactive({
      
      halibut_final_metrics <- halibut_final_metrics() %>% 
        pivot_wider(., names_from = "Season", values_from = "Value") %>% 
        filter(varname %in% c(input$halibut_varname)) %>% 
        gt(rowname_col = "varname") %>% 
        opt_row_striping() %>%
        tab_options(
          column_labels.background.color = "#70262B",
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
    
    output$halibut_table <- render_gt({
      expr = halibut_df_stats()
    })
    
    halibut_csv<-reactive(hal_joined)
    
    output$halibut_csv<-downloadHandler(
      filename = function() { "halibut_full_metrics.csv" },
      
      content = function(file) {
        write.csv(halibut_csv(), file)
      }
    )
    
    output$halibut_lands_plot <- renderPlotly({
      if(length(input$halibut_lands) > 1){
        hal_acl_plot$All
      }else if(input$halibut_lands == "Aggregate Landings"){
        hal_acl_plot$`Aggregate Landings`
      } else if(input$halibut_lands == "Quota allocated to CS program"){
        hal_acl_plot$`Quota allocated to CS program`
      }  })
    
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
            p,
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
