crab_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 2, img(src='crab_pic.png', width = 225, height =275)),
      column(width = 10, 
             layout_column_wrap(
               width = 1/2,  
               pickerInput(
                 inputId = ns("crab_varname"), 
                 label = "Select metric(s):", 
                 choices = NULL, 
                 selected = c("Aggregate Landings","Aggregate revenue from Catch Share species"),
                 options = list(`actions-box` = TRUE),multiple = T
               )),
             card(shinycssloaders::withSpinner(gt_output(outputId = ns("crab_table")), type = 7)),
             downloadButton(ns("crab_csv"),"Download data"))
    ),
    br(),
    tags$div(style="background-color:#873e23 !important; color: white; padding-top: 7px; padding-bottom: 7px; padding-left: 7px; font-size: 20px",
             tags$header(strong("CATCH & LANDINGS PERFORMANCE METRICS"))),
    p("The catch and landings performance metrics for the BSAI Crab Rationalization Program include the amount of BSAI crab species allocated to the program,
                                                  the landings of BSAI crab in the rationalization program, and the percentage of species allocated to the program that is landed (percent utilization). 
                                                  Annual metrics are compared with a “baseline” period prior to program implementation, which is the average of 1998/99, 2001/02, and 2004/05 fishing seasons rather than three
                                                  consecutive years preceding program implementation. This is based on the North Pacific Fishery Management Council's specifications for reference years for the
                                                  BSAI Crab Rationalization Program Review."),
    fluidRow(
      layout_column_wrap(
        width = NULL, height = 575, fill = FALSE,
        heights_equal = "row",
        style = css(grid_template_columns = "3fr 2fr"),
        card(pickerInput(
          inputId = ns("crab_lands"), 
          label = "Select variable:", 
          choices = c("Aggregate Landings", "Quota allocated to CS program"), 
          selected = c("Aggregate Landings", "Quota allocated to CS program"), 
          options = list(`actions-box` = TRUE),
          multiple = TRUE
        ),
        shinycssloaders::withSpinner(plotlyOutput(outputId = ns("crab_lands_plot")), type = 7)),
        layout_column_wrap(
          width = 1,
          heights_equal = "row",
          card(
            shinycssloaders::withSpinner(plotlyOutput(outputId = ns("crab_hs_plot"), height = 225), type = 7)),
          card(
            shinycssloaders::withSpinner(plotlyOutput(outputId = ns("crab_utliz_plot"), height = 225), type = 7)))
      )),
    tags$div(style="background-color:#873e23 !important; color: white; padding-top: 7px; padding-bottom: 7px; padding-left: 7px; font-size: 20px",
             tags$header(strong("EFFORT PERFORMANCE METRICS"))),
    p("The effort performance metrics include the number of active vessels, the number of days at sea, the number of entities eligible to hold quota share to fish in a crab fishery, and the season length index.
                                                   Season length varies in duration, timing, and the fleet's utilization of these resources.  In general, less than the  entire season length is typically used due to fishing and sea ice conditions, as well as
                                                  economic incentives to limit operating time during the fishery, e.g., market forces, processor capacity, processor and harvester interests, and the costs of sustaining remote operations in the Bering Sea."),
    p("A season length index was constructed to account for the differences in season length, the fleet's utilization of these seasons and to construct an indicator that accounts for change over time in the active fishing
                                                  season length across multiple fisheries. The season length index represents the proportion of days when fishing actually occurred compared to the maximum number of days when fishing was allowed. Using 
                                                  this index provides an indication of the temporal utilization of the crab resource and changes each year even if the regulatory season length remains constant. As a result, utilizing this unit-less index allows the 
                                                  season length index to be combined over multiple crab species to achieve an overall program season length."),
    fluidRow(box(width =10, card(selectizeInput(
      inputId = ns("crab_effort"), 
      label = "Select metric:", 
      choices = c("Season length","Active vessels",  "Days at sea", "Trips"), 
      selected = "Season length",
      multiple = FALSE
    ),
    shinycssloaders::withSpinner(plotlyOutput(outputId = ns("crab_effort_plot")), type = 7)))),
    tags$div(style="background-color:#873e23 !important; color: white; padding-top: 7px; padding-bottom: 7px; padding-left: 7px; font-size: 20px",
             tags$header(strong("REVENUE PERFORMANCE METRICS"))),
    p("The revenue performance metrics are the aggregate revenue from crab fishery , average prices, the revenue per active vessel, and the Gini coefficient which is a measure of
                                                  revenue concentration among active vessels.Aggregate revenue is calculated by using the estimated ex-vessel value of IFQ commercial landings or limited access crab during baseline."),
    p("A portion of the fleet participating in the IFQ Program lease and land crab in Community Development Quota (CDQ) allocation programs on the same trips in which IFQ Program crab are caught and landed. These other
                                                  landings contribute to overall revenue for associated vessel and crews. Non-IFQ crab revenue is produced almost exclusively from landings of CDQ crab quota, which is issued as 10% of the allowable catch compared to 90% 
                                                  issued to IFQ; consequently, non-IFQ revenues are of a similar proportion, ranging from 5-10% of total the revenue proportion is primarily a function of whether CDQ crab pounds were used on the same trips as IFQ Program crab pounds, rather than 
                                                  vessels' taking exclusive CDQ crab trips, which are not accounted for in this framework of metrics, which are intended to measure performance in the IFQ portion of the CR program."),
    fluidRow(
      layout_column_wrap(
        width = NULL, height = 525, fill = FALSE,
        heights_equal = "row",
        style = css(grid_template_columns = "2.5fr 1fr"),               
        card( shinycssloaders::withSpinner(plotlyOutput(outputId = ns("crab_rev_plot"),  height = 450), type = 7)),
        card(shinycssloaders::withSpinner(plotlyOutput(outputId = ns("crab_gini_plot"),  height = 450), type = 7))
      )),
    fluidRow(
      layout_column_wrap(
        width = NULL, height = 525, fill = FALSE,
        style = css(grid_template_columns = "2.5fr 1fr"),          
        card( 
          selectizeInput(
            inputId = ns("crab_rev_per"), 
            label = "Select metric:", 
            choices = c("Total Revenue/vessel", "Total Revenue/day at sea", "Total Revenue/trip"), 
            selected = "Total Revenue/vessel",
            multiple = FALSE),
          shinycssloaders::withSpinner(plotlyOutput(outputId = ns("crab_rev_per_plot")), type = 7)))),
    tags$br(),
    tags$hr(style = "border-top: 5px solid #033C73;")
  )
}