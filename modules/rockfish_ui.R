### scallops ------

rockfish_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 2, img(src='goa_othr_rock.png', width = 225, height =275)),
      column(width = 10,
             
             layout_column_wrap(
               width = 1/2, 
               pickerInput(
                 inputId = ns("goa_rock_varname"),
                 label = "Select metric(s):",
                 choices = NULL,
                 selected = c("Aggregate Landings","Aggregate revenue from species in fishery"),
                 options = list(`actions-box` = TRUE),multiple = T
               )),
             card(shinycssloaders::withSpinner(gt_output(outputId = ns("goa_rock_table")), type = 7)),
             downloadButton(ns("goa_rock_csv"),"Download data"))
    ),
    br(),
    tags$div(style="background-color:#873e23 !important; color: white; padding-top: 7px; padding-bottom: 7px; padding-left: 7px; font-size: 20px",
             tags$header(strong("CATCH & LANDINGS PERFORMANCE METRICS"))),
    p("The catch and landings performance metrics include the amount of the GOA Rockfish excluding the Central GOA Rockfish Program, the aggregate landings, and the
                                        percentage of the allocation that is landed (percent utilization)."),
    fluidRow(
      layout_column_wrap(
        width = NULL, height = 550, fill = FALSE,
        heights_equal = "row",
        style = css(grid_template_columns = "3fr 2fr"),
        card(pickerInput(
          inputId = ns("rockfish_lands"), 
          label = "Select variable:", 
          choices = c("Aggregate Landings", "ACL or Quota/TAC"), 
          selected = c("Aggregate Landings", "ACL or Quota/TAC"),
          options = list(`actions-box` = TRUE),
          multiple = TRUE
          ),
        shinycssloaders::withSpinner(plotlyOutput(outputId = ns("rockfish_lands_plot")), type = 7)),
        card( shinycssloaders::withSpinner(plotlyOutput(outputId = ns("rockfish_utliz_plot"), height = 500), type = 7)))),
    tags$div(style="background-color:#873e23 !important; color: white; padding-top: 7px; padding-bottom: 7px; padding-left: 7px; font-size: 20px",
             tags$header(strong("EFFORT PERFORMANCE METRICS"))),
    p("The effort performance metrics include the season length index and the number of active vessels, days at sea, and trips taken. The season length index is defined as the number
                                       of days in which at least one vessel was fishing divided by the maximum regulatory season length
                                       possible for the fishery, equal to 365 days in normal years and 366 days in leap years. This index
                                       measures the relative proportion of the legal fishing season during which some or all vessels actively
                                       fished."),
    fluidRow(box(width =10, 
                 card(selectizeInput(
                   inputId = ns("rockfish_effort"), 
                   label = "Select metric:", 
                   choices = c("Season length","Active vessels"), 
                   selected = "Season length",
                   multiple = FALSE
                 ),
                 shinycssloaders::withSpinner(plotlyOutput(outputId = ns("rockfish_effort_plot")), type = 7)))),
    tags$div(style="background-color:#873e23 !important; color: white; padding-top: 7px; padding-bottom: 7px; padding-left: 7px; font-size: 20px",
             tags$header(strong("REVENUE PERFORMANCE METRICS"))),
    p("The revenue performance metrics are the aggregate revenue from GOA Rockfish, average price per pound
                                      of GOA Rockfish, the revenue per active vessel, day at sea, and trip, and the Gini coefficient which is a measure of revenue
                                      concentration among the active vessels. "),
    fluidRow(
      layout_column_wrap(
        width = NULL, height = 525, fill = FALSE,
        heights_equal = "row",
        style = css(grid_template_columns = "2.5fr 1fr"),    
        card(shinycssloaders::withSpinner(plotlyOutput(outputId = ns("rockfish_rev_plot"), height = 450), type = 7)),
        card(shinycssloaders::withSpinner(plotlyOutput(outputId = ns("rockfish_gini_plot"), height = 450), type = 7))
      )
    ),
    fluidRow(
      layout_column_wrap(
        width = NULL, height = 525, fill = FALSE,
        style = css(grid_template_columns = "2.5fr 1fr"), 
        card(selectizeInput(
          inputId = ns("rockfish_rev_per"), 
          label = "Select metric:", 
          choices = c("Total Revenue/vessel"), 
          selected = "Total Revenue/vessel",
          multiple = FALSE),
          shinycssloaders::withSpinner(plotlyOutput(outputId = ns("rockfish_rev_per_plot")), type = 7)))
    ),
    tags$br(),
    tags$hr(style = "border-top: 5px solid #033C73;")
  )
}