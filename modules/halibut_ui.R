halibut_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 2, img(src='halibut_pic.png', width = 225, height =275)),
      column(width = 10, 
             layout_column_wrap(
               width = 1/2,  
               pickerInput(
                 inputId = ns("halibut_varname"), 
                 label = "Select metric(s):", 
                 choices = NULL, 
                 selected = c("Aggregate Landings","Aggregate revenue from Catch Share species"),
                 options = list(`actions-box` = TRUE),multiple = T
               )),
             card(shinycssloaders::withSpinner(gt_output(outputId = ns("halibut_table")), type = 7)),
             downloadButton(ns("halibut_csv"),"Download data")
      )),
    br(),
    tags$div(style="background-color:#873e23 !important; color: white; padding-top: 7px; padding-bottom: 7px; padding-left: 7px; font-size: 20px",
             tags$header(strong("CATCH & LANDINGS PERFORMANCE METRICS"))),
    p("The catch and landings performance metrics include the amount of IFQ allocated to the program, the landings of IFQ halibut, 
                                  the number of entities holding QS, and the percentage of the IFQ that is landed (percent utilization).
                                  Annual metrics are compared with a “baseline” period prior to program implementation, which is
                                  the average of the three years prior to program implementation (1992-1994)."),
    fluidRow(
      layout_column_wrap(
        width = NULL, height = 575, fill = FALSE,
        heights_equal = "row",
        style = css(grid_template_columns = "3fr 2fr"),
        card(pickerInput(
          inputId = ns("halibut_lands"), 
          label = "Select variable:", 
          choices = c("Aggregate Landings", "Quota allocated to CS program"), 
          selected = c("Aggregate Landings", "Quota allocated to CS program"), 
          options = list(`actions-box` = TRUE),
          multiple = TRUE
        ),
        shinycssloaders::withSpinner(plotlyOutput(outputId = ns("halibut_lands_plot")), type = 7)),
        layout_column_wrap(
          width = 1,
          heights_equal = "row",
          card(
            shinycssloaders::withSpinner(plotlyOutput(outputId = ns("halibut_hs_plot"), height = 225), type = 7)),
          card(
            shinycssloaders::withSpinner(plotlyOutput(outputId = ns("halibut_utliz_plot"), height = 225), type = 7)))
      )),
    tags$div(style="background-color:#873e23 !important; color: white; padding-top: 7px; padding-bottom: 7px; padding-left: 7px; font-size: 20px",
             tags$header(strong("EFFORT PERFORMANCE METRICS"))),
    p("The effort performance metrics include the season length index, the number of active vessels, the number of days at sea, and the number of trips.
                                  The season length index is defined as the number of days in which at
                                  least one vessel was fishing divided by the number of days in the regulatory fishing season. This
                                  index is necessary to create a single unit-less metric of season length that can be aggregated over all
                                  8 areas, in which vessel participation varies throughout the season. This index measures the relative
                                  proportion of the legal fishing season during which some or all vessels actively fished for halibut IFQ.
                                  During the baseline, some areas were only open to fishing for halibut for a few days (for the most
                                  demanded areas) while others were open for most of the year. To calculate an aggregate halibut
                                  IFQ program season length index, we use the weighted harmonic mean number of days active by
                                  area using catch as weights and then divide by the regulatory fishing season length. For the baseline
                                  period, we assume a 246 day regulatory fishing season which is the number of days allowed for
                                  the first 8 years post-IFQ and is the best hypothetical season length to use to compare pre-IFQ
                                  with post-IFQ."),
    fluidRow(box(width =10, card(selectizeInput(
      inputId = ns("halibut_effort"), 
      label = "Select metric:", 
      choices = c("Season length", "Active vessels", "Days at sea", "Trips"), 
      selected = "Season length",
      multiple = FALSE
    ),
    shinycssloaders::withSpinner(plotlyOutput(outputId = ns("halibut_effort_plot")), type = 7)))),
    tags$div(style="background-color:#873e23 !important; color: white; padding-top: 7px; padding-bottom: 7px; padding-left: 7px; font-size: 20px",
             tags$header(strong("REVENUE PERFORMANCE METRICS"))),
    p("The revenue performance metrics are the aggregate revenue from halibut IFQ, average prices of halibut IFQ, the revenue 
                                  per active vessel, day at sea, and trip and the Gini coefficient which measures the concentration of 
                                  revenues among active vessels. Aggregate revenue from halibut IFQ has been higher for all years after
                                  program implementation relative to the baseline
                                  period."),
    fluidRow(
      layout_column_wrap(
        width = NULL, height = 500, fill = FALSE,
        heights_equal = "row",
        style = css(grid_template_columns = "2.5fr 1fr"),             
        card(shinycssloaders::withSpinner(plotlyOutput(outputId = ns("halibut_rev_plot"), height = 450), type = 7)),
        card(shinycssloaders::withSpinner(plotlyOutput(outputId = ns("halibut_gini_plot"), height = 450), type = 7))
      )),
    fluidRow(
      layout_column_wrap(
        width = NULL, height = 550, fill = FALSE,
        style = css(grid_template_columns = "2.5fr 1fr"),            
        card(
          selectizeInput(
            inputId = ns("halibut_rev_per"), 
            label = "Select metric:", 
            choices = c("Total Revenue/vessel", "Total Revenue/day at sea", "Total Revenue/trip"), 
            selected = "Total Revenue/vessel",
            multiple = FALSE),
          shinycssloaders::withSpinner(plotlyOutput(outputId = ns("halibut_rev_per_plot")), type = 7)))),
    tags$br(),
    tags$hr(style = "border-top: 5px solid #033C73;")
  )
  
}