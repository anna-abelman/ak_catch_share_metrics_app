a80_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 2, img(src='a80_pic.png', width = 225, height =275)),
      column(width = 10, 
             layout_column_wrap(
               width = 1/2, 
               pickerInput(
                 inputId = ns("a80_varname"), 
                 label = "Select metric(s):", 
                 choices = NULL, 
                 options = list(`actions-box` = TRUE),multiple = T
               )),
             card(shinycssloaders::withSpinner(gt_output(outputId = ns("a80_table")), type = 7)),
             downloadButton(ns("a80_csv"),"Download data"),
      )),                   
    br(),
    tags$div(style="background-color:#873e23 !important; color: white; padding-top: 7px; padding-bottom: 7px; padding-left: 7px; font-size: 20px",
             tags$header(strong("CATCH & LANDINGS PERFORMANCE METRICS"))),
    p("The catch and landings performance metrics for the Amendment 80 Program include the amount
                                                                    of Amendment 80 species allocated to the program, the landings of Amendment 80 species in the
                                                                    Amendment 80 Program, the number of entities holding Amendment 80 QS, and the percentage of Amendment 80 species allocated to the program
                                                                    that is landed (percent utilization). Annual metrics are compared with a “baseline” period prior to
                                                                    program implementation, which is the average of the three years prior to program implementation
                                                                    (2005-2007)"),
    fluidRow(
      layout_column_wrap(
        width = NULL, height = 575, fill = FALSE,
        heights_equal = "row",
        style = css(grid_template_columns = "3fr 2fr"),
        card(pickerInput(
          inputId = ns("a80_lands"), 
          label = "Select variable:", 
          choices = c("Aggregate Landings", "Quota allocated to CS program"), 
          selected = c("Aggregate Landings", "Quota allocated to CS program"), 
          options = list(`actions-box` = TRUE),
          multiple = TRUE
        ),
        shinycssloaders::withSpinner(plotlyOutput(outputId = ns("a80_lands_plot")), type = 7)),
        layout_column_wrap(
          width = 1,
          heights_equal = "row",
          card(
            shinycssloaders::withSpinner(plotlyOutput(outputId = ns("a80_hs_plot"), height = 210), type = 7)),
          card(
            shinycssloaders::withSpinner(plotlyOutput(outputId = ns("a80_utliz_plot"), height = 220), type = 7)))
      )),
    tags$div(style="background-color:#873e23 !important; color: white; padding-top: 7px; padding-bottom: 7px; padding-left: 7px; font-size: 20px",
             tags$header(strong("EFFORT PERFORMANCE METRICS"))),
    p("The effort performance metrics include the season length index, the number of active vessels, days at sea, and the number of trips taken. The season length index is defined as the number
                                                                    of days in which at least one vessel was fishing divided by the maximum regulatory season length
                                                                    possible for the fishery, equal to 346 days, which is an opening on January 20th and closure on
                                                                    December 31st. This index measures the relative proportion of the legal fishing season during which
                                                                    some or all vessels actively fished Amendment 80 species allocations each year. For the baseline
                                                                    period, we assume the same 346 day regulatory open period which allows for a constant comparison
                                                                    of the season length before and after the implementation of Amendment 80."),
    fluidRow(box(width =10, card(selectizeInput(
      inputId = ns("a80_effort"), 
      label = "Select metric:", 
      choices = c("Season length","Active vessels",  "Days at sea"), 
      selected = "Season length",
      multiple = FALSE
    ),
    shinycssloaders::withSpinner(plotlyOutput(outputId = ns("a80_effort_plot")), type = 7)))),
    tags$div(style="background-color:#873e23 !important; color: white; padding-top: 7px; padding-bottom: 7px; padding-left: 7px; font-size: 20px",
             tags$header(strong("REVENUE PERFORMANCE METRICS"))),
    p("The revenue performance metrics are the aggregate revenue from Amendment 80 Program species,
                                                                    average prices of Amendment 80 species, the revenue per active vessel, day at sea, and trip, and the Gini coefficient which
                                                                    is a measure of revenue concentration among active vessels. As all vessels in the Amendment 80
                                                                    program are CPs, revenues are reported as first wholesale value of the processed fish products that are
                                                                    offloaded from the vessels."),
    fluidRow(
      layout_column_wrap(
        width = NULL, height = 500, fill = FALSE,
        heights_equal = "row",
        style = css(grid_template_columns = "2.5fr 1fr"),              
        card(shinycssloaders::withSpinner(plotlyOutput(outputId = ns("a80_rev_plot"), height = 450), type = 7)),
        card( shinycssloaders::withSpinner(plotlyOutput(outputId = ns("a80_gini_plot"),  height = 450), type = 7)))
    ),
    fluidRow( 
      layout_column_wrap(
        width = NULL, height = 525, fill = FALSE,
        style = css(grid_template_columns = "2.5fr 1fr"),         
        card( 
          selectizeInput(
            inputId = ns("a80_rev_per"), 
            label = "Select metric:", 
            choices = c("Total Revenue/vessel", "Total Revenue/day at sea"), 
            selected = "Total Revenue/vessel",
            multiple = FALSE),
          shinycssloaders::withSpinner(plotlyOutput(outputId = ns("a80_rev_per_plot"), width="1000px", height="400px"), type = 7)))
    ),
    tags$br(),
    tags$hr(style = "border-top: 5px solid #033C73;")
  )
  
  
}