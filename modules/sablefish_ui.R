sablefish_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 2, img(src='sablefish_pic.png', width = 225, height =275)),
      column(width = 10, 
             layout_column_wrap(
               width = 1/2, 
               selectInput(ns("sablefish_sector"), label = ("Select sector:"), 
                           choices = c("All", "CP", "CV"), 
                           selected = "All"),
               pickerInput(
                 inputId = ns("sablefish_varname"), 
                 label = "Select metric(s):", 
                 choices = NULL, 
                 selected = c("Aggregate Landings","Aggregate revenue from Catch Share species"),
                 options = list(`actions-box` = TRUE),multiple = T
               )),
             br(),
             card( shinycssloaders::withSpinner(gt_output(outputId = ns("sablefish_table")), type = 7)),
             downloadButton(ns("sablefish_csv"),"Download data"))
    ),
    br(),
    tags$div(style="background-color:#873e23 !important; color: white; padding-top: 7px; padding-bottom: 7px; padding-left: 7px; font-size: 20px",
             tags$header(strong("CATCH & LANDINGS PERFORMANCE METRICS"))),
    p("The catch and landings performance metrics include the amount of IFQ allocated to the program,
                                    the landings of IFQ sablefish, the number of entities holding QS, and the percentage of the IFQ allocated that is landed (percent
                                    utilization). Annual metrics are compared with a “baseline” period prior to program implementation,
                                    which is the average of the three years prior to program implementation (1992-1994)."),
    fluidRow(
      layout_column_wrap(
        width = NULL, height = 575, fill = FALSE,
        heights_equal = "row",
        style = css(grid_template_columns = "3fr 2fr"),
        card(selectizeInput(
          inputId = ns("sablefish_lands_sector"), 
          label = "Select sector:", 
          choices =  c("All", "CP", "CV"), 
          selected = "All",
          multiple = FALSE
        ),
        shinycssloaders::withSpinner(plotlyOutput(outputId = ns("sablefish_lands_plot")), type = 7)),
        layout_column_wrap(
          width = 1,
          heights_equal = "row",
          card(
            shinycssloaders::withSpinner(plotlyOutput(outputId = ns("sablefish_hs_plot"),height = 225), type = 7)),
          card(
            shinycssloaders::withSpinner(plotlyOutput(outputId = ns("sablefish_utliz_plot"), height = 225), type = 7))
        ))),
    tags$div(style="background-color:#873e23 !important; color: white; padding-top: 7px; padding-bottom: 7px; padding-left: 7px; font-size: 20px",
             tags$header(strong("EFFORT PERFORMANCE METRICS"))),
    p("The effort performance metrics include season length index, the number of active vessels, the number of days at sea, and the
                                    number of trips taken. The season length index is defined as the number of days in which at
                                    least one vessel was fishing divided by the number of days in the regulatory fishing season. This
                                    index is necessary to create a single unit-less metric of season length that can be aggregated over
                                    all 6 sablefish areas, in which levels of vessel participation vary throughout the season. This index
                                    measures the relative proportion of the legal fishing season during which one or more vessels actively
                                    fished sablefish IFQ. During the baseline, some areas were only open to fishing for sablefish for a few
                                    days (for the most demanded areas) while others were open for most of the year. To calculate an
                                    aggregate sablefish IFQ program season length index, we use the weighted harmonic mean number
                                    of days active by area using catch as weights and then divide by the regulatory season length. For
                                    the baseline period, we assume a 246 day regulatory season length which is the number of days
                                    allowed for the first 8 years post-IFQ and is the best hypothetical season length to use to compare
                                    pre-IFQ with post-IFQ. The number of active vessels reflects the number of sablefish CVs and CPs with any commercial
                                    landings of IFQ Program sablefish in a given year. The baseline value represents the average
                                    number of unique vessels per year with commercial sablefish landings from 1992-1994."),
    fluidRow(box(width =10, 
                 card(selectizeInput(
                   inputId = ns("sablefish_effort"), 
                   label = "Select metric:", 
                   choices = c( "Season length","Active vessels", "Days at sea", "Trips"), 
                   selected = "Season length",
                   multiple = FALSE
                 ),
                 shinycssloaders::withSpinner(plotlyOutput(outputId = ns("sablefish_effort_plot")), type = 7)))),
    tags$div(style="background-color:#873e23 !important; color: white; padding-top: 7px; padding-bottom: 7px; padding-left: 7px; font-size: 20px",
             tags$header(strong("REVENUE PERFORMANCE METRICS"))),
    p("The revenue performance metrics are the aggregate revenue from sablefish IFQ, average prices
                                    of sablefish IFQ, the revenue per active vessel, day at sea, and vessel, and the Gini coefficient which is a measure of
                                    revenue concentration among the active vessels. Each revenue performance metric is differentiated by CVs and CPs. For the
                                    Sablefish IFQ Program, revenues are reported in their native format, such that the price received
                                    by CVs is the weighted annual ex-vessel price while the price received by CPs is the weighted
                                    annual first-wholesale price. This enables a comparison between the revenues that each type of
                                    vessel receives on offloading their catch from the vessel."),
    fluidRow(
      layout_column_wrap(
        width = NULL, height = 575, fill = FALSE,
        heights_equal = "row",
        style = css(grid_template_columns = "2.5fr 1fr"),  
        card(
          selectInput(ns("sablefish_rev_sector"), label = ("Select sector:"), 
                      choices = c("All", "CP", "CV"), 
                      selected = "All"),
          shinycssloaders::withSpinner(plotlyOutput(outputId = ns("sablefish_rev_plot"), height = 450), type = 7)),
        card(selectInput(ns("sablefish_gini_sector"), label = ("Select sector:"), 
                         choices = c("All", "CP", "CV"), 
                         selected = "All"),
             shinycssloaders::withSpinner(plotlyOutput(outputId = ns("sablefish_gini_plot"), height = 375), type = 7)))
    ),
    fluidRow(
      layout_column_wrap(
        width = NULL, height = 575, fill = FALSE,
        style = css(grid_template_columns = "2.5fr 1fr"),      
        card(
          selectizeInput(
            inputId = ns("sablefish_rev_per"), 
            label = "Select metric:", 
            choices = c("Total Revenue/vessel", "Total Revenue/day at sea"), 
            selected = "Total Revenue/vessel",
            multiple = FALSE),
          shinycssloaders::withSpinner(plotlyOutput(outputId = ns("sablefish_rev_per_plot")), type = 7)))),
    tags$br(),
    tags$hr(style = "border-top: 5px solid #033C73;")
  )
  
}