sablefish_server <- function(id,  sab_joined, sab_plot_df, sab_acl_plot_All, sab_acl_plot_CP,  sab_acl_plot_CV, 
                             sab_ent_plot, sab_utilz_plot, 
                             sab_eff_plot, sab_rev_plot_All,sab_rev_plot_CP, sab_rev_plot_CV, sab_gini_plot_All,
                             sab_gini_plot_CP, sab_gini_plot_CV,
                             sab_rev_per_plot) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
   
    
    ## ---------------------------------------------------------------------------------------------------
    ## Sablefish Plots
    ## --------------------------------------------------------------------------------------------------
    sablefish_final_metrics <- reactive({
      
      sablefish_final_metrics <- sab_joined %>% 
        mutate(across(-c("Season"), ~ifelse(is.na(.), "-", .))) %>% 
        mutate(across(-c("Season"), ~as.character(.))) %>% 
        pivot_longer(., cols= -c("Season", "sector"), names_to = "varname", values_to = "Value") %>% 
        mutate(Value = suppressWarnings(
          case_when(!(varname %in% c("ACL exceeded Y/N", "ACL exceeded species", "Share cap in place",
                                     "Share cap %", "Gini coefficient","Season length", 
                                     "Average price")) & Value != "-" ~ format(round(as.numeric(Value),0), big.mark = ","),
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
                                   TRUE ~ varname)) %>% 
        pivot_wider(., names_from = "Season", values_from = "Value") %>% 
        filter(!if_all(.cols = -c(sector, varname), .fns = ~ .x == "-")) %>% 
        filter(sector == input$sablefish_sector)
      
    })
    
    observe({
      updatePickerInput(session = session, inputId = "sablefish_varname",
                        choices = unique(sablefish_final_metrics()$varname), 
                        selected = c("Aggregate Landings (lbs)","Aggregate revenue from Catch Share species")) 
      
    })
        
    sablefish_df_stats <- reactive({
      
      
      sablefish_final_metrics <- sablefish_final_metrics() %>% 
        filter(varname %in% c(input$sablefish_varname) ) %>% 
        select(-sector) %>% 
        gt(rowname_col = "varname") %>% 
        opt_row_striping() %>%
        tab_options(
          column_labels.background.color = "#46412A",
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
        ) %>% 
        tab_footnote("'-' indicates data not available for metric. Confidential data is suppressed and specified by 'Conf.' in the table above.")
    })
    
    output$sablefish_table <- render_gt({
      expr = sablefish_df_stats()
    })
    
    sablefish_csv<-reactive(sab_joined)
    
    output$sablefish_csv<-downloadHandler(
      filename = function() { "sablefish_full_metrics.csv" },
      
      content = function(file) {
        write.csv(sablefish_csv(), file)
      }
    )
    
    output$sablefish_lands_plot <- renderPlotly({
      
      if(input$sablefish_lands_sector == "All"){
        sab_acl_plot_All$All
      }else if(input$sablefish_lands_sector == "CP"){
        sab_acl_plot_CP$`Aggregate Landings`
      } else if(input$sablefish_lands_sector == "CV"){
        sab_acl_plot_CV$`Aggregate Landings`
      }  
      
    })
    
    output$sablefish_hs_plot <- renderPlotly({
      
      sab_ent_plot
    })
    
    output$sablefish_utliz_plot <- renderPlotly({
      
      utilz_df <- sab_plot_df %>% 
        filter(varname %in% c("Utilization") & sector == "All")
      
      ut_plot <- ggplot()+
        geom_line(data = utilz_df, aes(x = as.factor(Season), y = Val, group = sector, color = sector,
                                       text = paste("Season:", 
                                                    Season,"<br> % of Utilization: <br>", 
                                                    round(Val, 4))))+
        theme_minimal()+
        scale_color_manual(values = c("#353229","#A4C8D8"), name = "Sector")+
        theme(legend.position="bottom",legend.box = "horizontal")+
        scale_y_continuous(expand = expansion(mult = c(0.1, 0.1)),
                           breaks = seq(0, 105, by = 25),
                           limits = c(0, 105))+
        labs(x = "Season", y = "% of Utilization")+
        theme(axis.text.x = element_text(angle = -45, vjust = 0.5, hjust=1))+
        theme(legend.position = "bottom", legend.box = "horizontal")
      
      ggplotly(ut_plot, tooltip = "text", height = 210) %>% 
        layout(
          legend = list(orientation = "h", 
                        xanchor = "center", 
                        x = 0.5,
                        y=-0.6))
    })
    
    output$sablefish_effort_plot <- renderPlotly({
      
      if(input$sablefish_effort == "Active vessels" & !is.null(sab_eff_plot$`Active vessels`)){
        sab_eff_plot$`Active vessels`
      } else if(input$sablefish_effort == "Days at sea" & !is.null(sab_eff_plot$`Days at sea`)){
        sab_eff_plot$`Days at sea` %>% 
          layout(
            p,
            annotations = list(
              x = 1,  
              y = -0.15,
              text = "Note: IFQ Sablefish CV days at sea only begins in 2008",
              showarrow = FALSE,
              xref = 'paper', 
              yref = 'paper',
              xanchor = 'right',
              yanchor = 'top',
              font = list(size = 12, color = 'gray')
            ),
            margin = list(b = 100)  # add bottom margin for space
          )
      } else if(input$sablefish_effort == "Season length" & !is.null(sab_eff_plot$`Season length`)){
        sab_eff_plot$`Season length`
      } else if(input$sablefish_effort =="Trips" & !is.null(sab_eff_plot$Trips)){
        sab_eff_plot$Trips
      }  
    })
    
    output$sablefish_rev_plot <- renderPlotly({
      
      if(input$sablefish_rev_sector == "All"){
        sab_rev_plot_All
      }else if(input$sablefish_rev_sector == "CP"){
        sab_rev_plot_CP
      } else if(input$sablefish_rev_sector == "CV"){
        sab_rev_plot_CV
      }
    })
    
    output$sablefish_gini_plot <- renderPlotly({
      
      if(input$sablefish_gini_sector == "All"){
        sab_gini_plot_All
      }else if(input$sablefish_gini_sector == "CP"){
        sab_gini_plot_CP
      } else if(input$sablefish_gini_sector == "CV"){
        sab_gini_plot_CV
      }  
      
    })
    
    output$sablefish_rev_per_plot <- renderPlotly({
      if(input$sablefish_rev_per == "Total Revenue/vessel"& !is.null(sab_rev_per_plot[[1]])){
        sab_rev_per_plot[[1]]
      } else if(input$sablefish_rev_per == "Total Revenue/day at sea"& !is.null(sab_rev_per_plot[[2]])){
        sab_rev_per_plot[[2]]
      }
      
    })
    
  })
  
}