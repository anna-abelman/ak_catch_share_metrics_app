cgoa_rockfish_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList( 
    
    fluidRow(
      column(width = 2, img(src='goa_rock_pic.png', width = 225, height =275)),
      column(width = 10, 
             layout_column_wrap(
               width = 1/2, 
               selectInput(ns("cgoa_rf_sector"), label = ("Select sector:"), 
                           choices = c("All", "CP", "CV"), 
                           selected = "All"),
               pickerInput(
                 inputId = ns("cgoa_rf_varname"), 
                 label = "Select metric(s):", 
                 choices = NULL, 
                 selected = c("Aggregate Landings","Aggregate revenue from Catch Share species"),
                 options = list(`actions-box` = TRUE),multiple = T
               )),
             br(),
             card( shinycssloaders::withSpinner(gt_output(outputId = ns("cgoa_rf_table")), type = 7)),
             downloadButton(ns("cgoa_rf_csv"),"Download data"))
    ),
    br(),
    tags$div(style="background-color:#873e23 !important; color: white; padding-top: 7px; padding-bottom: 7px; padding-left: 7px; font-size: 20px",
             tags$header(strong("CATCH & LANDINGS PERFORMANCE METRICS"))),
    p("The catch and landings performance metrics include the amount of Rockfish Program species total
                                    allowable catches (TACs) allocated to the program, the landings of Rockfish Program species in
                                    the Rockfish Program, the number of entities holding Rockfish Program QS, and the percentage of allocated species that are landed (percent utilization).
                                    Annual metrics are compared with a “baseline” period prior to the implementation of the Rockfish
                                    Pilot Program in 2007, which is the average of the three years prior to Rockfish Pilot Program
                                    implementation (2004-2006)"),
    fluidRow(
      layout_column_wrap(
        width = NULL, height = 575, fill = FALSE,
        heights_equal = "row",
        style = css(grid_template_columns = "3fr 2fr"),
        card(selectizeInput(
          inputId = ns("cgoa_rf_lands_sector"), 
          label = "Select sector:", 
          choices =  c("All", "CP", "CV"), 
          selected = "All",
          multiple = FALSE
        ),
        shinycssloaders::withSpinner(plotlyOutput(outputId = ns("cgoa_rf_lands_plot")), type = 7)),
        layout_column_wrap(
          width = 1,
          heights_equal = "row",
          card(
            shinycssloaders::withSpinner(plotlyOutput(outputId = ns("cgoa_rf_hs_plot"), height = 225), type = 7)),
          card(
            shinycssloaders::withSpinner(plotlyOutput(outputId = ns("cgoa_rf_utliz_plot"), height = 225), type = 7)))
      )),
    tags$div(style="background-color:#873e23 !important; color: white; padding-top: 7px; padding-bottom: 7px; padding-left: 7px; font-size: 20px",
             tags$header(strong("EFFORT PERFORMANCE METRICS"))),
    p("The effort performance metrics include the season length index, the number of active vessels, the number of days at sea, and
                                    the number of trips taken. The season length index is defined as the number of days in which at least one vessel was fishing divided by the maximum regulatory season
                                    length possible for the fishery, equal to 199 days in all years (opening on May 1st and closing on
                                    November 15th). This index measures the relative proportion of the legal fishing season during
                                    which some or all vessels actively fished Rockfish Program species allocations."),
    fluidRow(box(width =10, 
                 card(selectizeInput(
                   inputId = ns("cgoa_rf_effort"), 
                   label = "Select metric:", 
                   choices = c("Season length","Active vessels",  "Days at sea"), 
                   selected = "Season length",
                   multiple = FALSE
                 ),
                 shinycssloaders::withSpinner(plotlyOutput(outputId = ns("cgoa_rf_effort_plot")), type = 7)))),
    tags$div(style="background-color:#873e23 !important; color: white; padding-top: 7px; padding-bottom: 7px; padding-left: 7px; font-size: 20px",
             tags$header(strong("REVENUE PERFORMANCE METRICS"))),
    p("The revenue performance metrics are the aggregate revenue from Rockfish Program species, average
                                    prices of Rockfish Program species, the revenue per active vessel, days at sea, and trip, and the Gini coefficient which is a
                                    measure of revenue concentration among the active vessels. For the Rockfish Program,
                                    revenues are reported in their native format, such that the price received by CVs is the weighted
                                    annual ex-vessel price while the price received by CPs is the weighted annual first-wholesale price.
                                    This enables a comparison between the revenues that each type of vessel receives on offloading
                                    their catch from the vessel."),
    fluidRow(
      layout_column_wrap(
        width = NULL, height = 575, fill = FALSE,
        heights_equal = "row",
        style = css(grid_template_columns = "2.5fr 1fr"),    
        card( 
          selectInput(ns("cgoa_rf_rev_sector"), label = ("Select sector:"), 
                      choices = c("All", "CP", "CV"), 
                      selected = "All"),
          shinycssloaders::withSpinner(plotlyOutput(outputId = ns("cgoa_rf_rev_plot"), height = 450), type = 7)),
        card( selectInput(ns("cgoa_rf_gini_sector"), label = ("Select sector:"), 
                          choices = c("All", "CP", "CV"), 
                          selected = "All"),
              shinycssloaders::withSpinner(plotlyOutput(outputId = ns("cgoa_rf_gini_plot"), height = 375), type = 7))
      )),
    fluidRow(
      layout_column_wrap(
        width = NULL, height = 450, fill = FALSE,
        style = css(grid_template_columns = "2.5fr 1fr"),         
        card( fillable = FALSE ,fill = FALSE, full_screen = TRUE, 
              selectizeInput(
                inputId = ns("cgoa_rf_rev_per"), 
                label = "Select metric:", 
                choices = c("Total Revenue/vessel", "Total Revenue/day at sea"), 
                selected = "Total Revenue/vessel",
                multiple = FALSE),
              shinycssloaders::withSpinner(plotlyOutput(outputId = ns("cgoa_rf_rev_per_plot")), type = 7)))),
    tags$br(),
    tags$hr(style = "border-top: 5px solid #033C73;")
  )
}