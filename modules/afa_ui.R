afa_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 2, img(src='afa_picture.png', width = 225, height =275)),
      column(width = 10,
             layout_column_wrap(
               width = 1/2,
               selectInput(ns("afa_sector"), label = ("Select sector:"),
                           choices = c("All", "CP", "CV"),
                           selected = "All"),
               pickerInput(
                 inputId = ns("afa_varname"),
                 label = "Select metric(s):",
                 choices = NULL,
                 selected = "Quota allocated to CS program",
                 options = list(`actions-box` = TRUE),multiple = T
               )),
             br(),
             card(shinycssloaders::withSpinner(gt_output(outputId = ns("afa_table")), type = 7)),
             downloadButton(ns("afa_csv"),"Download data"))
    ),
    br(),
    tags$div(style="background-color:#873e23 !important; color: white; padding-top: 7px; padding-bottom: 7px; padding-left: 7px; font-size: 20px",
             tags$header(strong("CATCH & LANDINGS PERFORMANCE METRICS"))),
    p("The catch and landings performance metrics include the amount of pollock TAC (quota) allocated
                                                              to the program, the landings of AFA pollock, the number of entities receiving
                                                              an exclusive harvest privilege in the AFA pollock program (quota), and the percentage of the quota allocated that is
                                                              landed (percent utilization). These annual metrics are compared with a “baseline” period prior to
                                                              program implementation, which is the average of the three years prior to any part of the program
                                                              implementation (1996-1998). The baseline quota value represents the average total non-CDQ
                                                              directed pollock allocation (inshore and offshore). For this report, the CV and mothership sectors
                                                              are combined into a single CV sector which remains separate from the CP sector."),
    fluidRow(
      layout_column_wrap(
        width = NULL, height = 550, fill = FALSE,
        heights_equal = "row",
        style = css(grid_template_columns = "3fr 2fr"),
        card(height = 520, selectizeInput(
          inputId = ns("afa_lands_sector"),
          label = "Select sector:",
          choices =  c("All", "CP", "CV"),
          selected = "All",
          multiple = FALSE
        ),
        shinycssloaders::withSpinner(plotlyOutput(outputId = ns("afa_lands_plot")), type = 7)),
        layout_column_wrap(
          width = 1,
          heights_equal = "row",
          card(
            shinycssloaders::withSpinner(plotlyOutput(outputId = ns("afa_hs_plot"), height = 225), type = 7)),
          card(
            full_screen = TRUE,shinycssloaders::withSpinner(plotlyOutput(outputId = ns("afa_utliz_plot"), height = 225), type = 7))
        ))),
    tags$div(style="background-color:#873e23 !important; color: white; padding-top: 7px; padding-bottom: 7px; padding-left: 7px; font-size: 20px",
             tags$header(strong("EFFORT PERFORMANCE METRICS"))),
    p("The effort performance metrics include the season length index and the number of active vessels, days at sea, and trips taken. The
                                                              season length index is defined as the number of days in which at least one vessel was fishing divided
                                                              by the maximum regulatory season length permissible for the fishery, equal to 286 days (opening on
                                                              January 20th and closing on November 1st). This index measures the relative proportion of the legal
                                                              fishing season during which some or all vessels actively fished for pollock. For the baseline period,
                                                              we assume the same 286 day regulatory open period which allows for a relative comparison of the
                                                              season length pre-AFA with post-AFA."),
    fluidRow(box(width =10, card(selectizeInput(
      inputId = ns("afa_effort"),
      label = "Select metric:",
      choices = c("Season length","Active vessels",  "Days at sea"),
      selected = "Season length",
      multiple = FALSE
    ),
    shinycssloaders::withSpinner(plotlyOutput(outputId = ns("afa_effort_plot")), type = 7)))),
    tags$div(style="background-color:#873e23 !important; color: white; padding-top: 7px; padding-bottom: 7px; padding-left: 7px; font-size: 20px",
             tags$header(strong("REVENUE PERFORMANCE METRICS"))),
    p("The revenue performance metrics are the aggregate revenue from AFA pollock, average prices of
                                                              AFA pollock, the revenue per active vessel, day at sea, and trip, and the Gini coefficient which is a measure of revenue
                                                              concentration among the active vessels. For the AFA Pollock Program, revenues
                                                              are reported in their native format, such that the price received by CVs is the weighted annual
                                                              ex-vessel price while the price received by CPs is the weighted annual first wholesale price. This
                                                              enables a comparison between the revenues that each type of vessel receives on offloading their
                                                              catch from the vessel."),
    fluidRow(
      layout_column_wrap(
        width = NULL, height = 575, fill = FALSE,
        heights_equal = "row",
        style = css(grid_template_columns = "2.5fr 1fr"),            
        card(selectInput(ns("afa_rev_sector"), label = ("Select sector:"),
                         choices = c("All", "CP", "CV"),
                         selected = "All"),
             shinycssloaders::withSpinner(plotlyOutput(outputId = ns("afa_rev_plot"), height = 450), type = 7)),
        card( selectInput(ns("afa_gini_sector"), label = ("Select sector:"),
                          choices = c("All", "CP", "CV"),
                          selected = "All"),
              shinycssloaders::withSpinner(plotlyOutput(outputId = ns("afa_gini_plot"), height = 450), type = 7))
      )),
    fluidRow(
      layout_column_wrap(
        width = NULL, height = 450, fill = FALSE,
        style = css(grid_template_columns = "2.5fr 1fr"),         
        card( fillable = FALSE ,fill = FALSE, full_screen = TRUE, 
              selectizeInput(
                inputId = ns("afa_rev_per"),
                label = "Select metric:",
                choices = c("Total Revenue/vessel", "Total Revenue/day at sea"),
                selected = "Total Revenue/vessel",
                multiple = FALSE),
              shinycssloaders::withSpinner(plotlyOutput(outputId = ns("afa_rev_per_plot")), type = 7)))),
    tags$br(),
    tags$hr(style = "border-top: 5px solid #033C73;")
  )
}

