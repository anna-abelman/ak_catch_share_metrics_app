library(shiny)
library(shinydashboard)
library(shinyjs)
library(here)
library(tidyverse)
library(janitor)
library(plotly)
library(wesanderson)
library(gt)
library(kableExtra)
library(shinycssloaders)
library(shinyWidgets)
library(bslib)
library(targets)

# Read the RDS data created with targets and save_data.R
data <- readRDS(here("exports", "final_metrics_2025-08-06.rds"))

# set current year
current_yr = 2023

# load all modules
for(ii in dir("modules",pattern="\\.R$",full.names=TRUE))
  source(ii)


# UI --------------------------------------------------------------------------------------------------------------------
ui <- bslib::page_navbar(
  theme = bs_theme(font_scale = 0.85,
                   'navbar-bg' = "white"),
  title = "Alaska Catch Share Performance Metrics",
  id = "tabs",
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  ### home ----
  bslib::nav_panel(title = "Home", value = "home", icon = icon("fish"),
                   tags$div(class = "background",
                            div(class = "title-container",
                                "Performance Metrics for North Pacific Catch Share Programs"),
                            div(class = "image-row",
                                tags$img(src = "afa_picture.png", 
                                         style = "cursor:pointer;",
                                         onclick = "Shiny.setInputValue('afa_img_clicked', true);"),
                                tags$img(src = "a80_pic.png", 
                                         style = "cursor:pointer;",
                                         onclick = "Shiny.setInputValue('a80_img_clicked', true);"),
                                tags$img(src = "goa_rock_pic.png", 
                                         style = "cursor:pointer;",
                                         onclick = "Shiny.setInputValue('cgoa_img_clicked', true);"),
                                tags$img(src = "sablefish_pic.png", 
                                         style = "cursor:pointer;",
                                         onclick = "Shiny.setInputValue('sab_img_clicked', true);"),
                                tags$img(src = "halibut_pic.png", 
                                         style = "cursor:pointer;",
                                         onclick = "Shiny.setInputValue('hal_img_clicked', true);"),
                                tags$img(src = "crab_pic.png", 
                                         style = "cursor:pointer;",
                                         onclick = "Shiny.setInputValue('crab_img_clicked', true);")
                            )),
                   div(class = "wrap-section",
                       tags$img(src = "about_cs.jpg", class = "wrap-image-left"),
                       div(class = "wrap-header-left", h4("ABOUT CATCH SHARE PROGRAMS")),
                       p("Catch share programs are a fishery management tool that allocates a secure 
                         share of the fishery resource to individual fishermen, fishing cooperatives,
                         fishing communities, or other entities to harvest a fixed quantity of fish
                         each year. Catch shares do not directly impact the total allowable catch (TAC)
                         of each species, and are merely a mechanism to allocate the TAC across various
                         individuals and user groups. The North Pacific region has been the most active
                         region in the U.S. in developing catch share programs, and contains 7 of the 17
                         programs currently in operation throughout the U.S. These programs are: the 
                         Western Alaska Community Development Quota (CDQ) (implemented in 1992), Alaska 
                         Halibut and Sablefish IFQ (implemented in 1995), American Fisheries Act (AFA) 
                         Pollock Cooperatives (implemented in 1999/2000), BSAI Crab Rationalization 
                         (implemented in 2005), Non-Pollock Trawl Catcher/Processor Groundfish 
                         Cooperatives (Amendment 80, implemented in 2008), and the Central Gulf of 
                         Alaska (GOA) Rockfish Program (extended the Rockfish Pilot Program in place
                         from 2007-2011 and was implemented in 2012). This dashboard does not include 
                         performance metrics for the CDQ Program. The fisheries included in this chapter 
                         account for approximately 67% of all state and federal North Pacific groundfish 
                         landings in 2017 and approximately one third of all U.S. commercial landings",
                         tags$a(href = "https://repository.library.noaa.gov/view/noaa/4601",
                                class = "custom-link",
                                target = "_blank",         
                                "(NMFS, 2017).")),
                       p("Catch share programs have a variety of designs which reflect unique circumstances
                         in each fishery and stated goals of the program. In the North Pacific, these designs 
                         include individual fishing quota (IFQ) programs such as the Alaska Halibut and Sablefish 
                         IFQ program, cooperative programs such as AFA pollock, Amendment 80, and the Central GOA 
                         Rockfish Program, combined IFQ and cooperative programs such as the BSAI Crab Rationalization,
                         as well as community allocation programs like the CDQ program. There have been several
                         stated goals for these programs, including: meeting conservation requirements,
                         improving economic efficiency and/or flexibility, improving bycatch management, reducing 
                         excess capacity, eliminating derby fishing conditions, and improving safety at sea."),
                   ),
                   div(class = "wrap-section",
                       tags$img(src = "cs_metrics.jpg", class = "wrap-image-right"),
                       div(class = "wrap-header-right", h4("CATCH SHARE PERFORMANCE METRICS")),
                       p(" This section develops a consistent set of indicators to assess various
                         dimensions of the economic performance of five catch share programs including
                         the halibut IFQ program (which is managed by NOAA Fisheries and the International 
                         Pacific Halibut Commission), the sablefish IFQ program (implemented together with 
                         the halibut IFQ program but will be considered separately), the AFA pollock cooperatives
                         program, the Amendment 80 program, and the central GOA Rockfish Program, as well as one 
                         quasi-catch share program, the Bering Sea Freezer Longline Catcher/Processors. These
                         indicators were developed by NOAA Fisheries’ regional economists, anthropologists, and 
                         sociologists as the most representative indicators of economic performance for which data
                         are available and can be regularly updated and were first summarized in ",
                         tags$a(href = "https://repository.library.noaa.gov/view/noaa/4601",
                                class = "custom-link",
                                target = "_blank",
                        "Brinson and Thunberg (2013)."), "These indicators can be broken down into three general categories: catch and landings,
                         effort, and revenue, and their descriptions are listed in the tables below."),
                       tags$a(
                         href = "https://www.fisheries.noaa.gov/alaska/sustainable-fisheries/catch-share-and-limited-access-programs-alaska",
                         target = "_blank",         
                         class = "custom-btn",  
                         icon("circle-info"),           
                         "Click here to learn more about these catch share programs"  ,
                       )
                   ),
                   fluidRow(
                     bslib::accordion(title = "Indicators", 
                               bslib::accordion_panel(title = "Catch & Landings",  #### catch and landing table ------
                                                      bslib::card(
                                                        full_screen = TRUE,
                                                        bslib::card_body(
                                                          tags$div(tags$fieldset(style= "width: 40%;","The catch and landings metrics are the annual catch limit (ACL) or quota level, aggregate landings,
                                      the % of the quota that was utilized, as well as whether the ACL or quota was exceeded for any species in the program. While the quota amount is set based on the biological
                                      condition of the species in the program, the landings and the percentage of the quota that is landed (% utilization) reflect economic conditions and regulatory constraints of the fishery"),
                                                                   tags$fieldset(style= "width: 50%; float: right;",
                                                                                 tags$table(class="styled-table",
                                                                                            tags$thead(
                                                                                              tags$tr(
                                                                                                tags$th('Indicator'),
                                                                                                tags$th('Definition')
                                                                                              )
                                                                                            ),
                                                                                            tags$tbody(
                                                                                              tags$tr(
                                                                                                tags$td('Quota allocated to catch share program'),
                                                                                                tags$td('Annual quota of combined catch share program species, in terms of weight.')
                                                                                              ),
                                                                                              tags$tr(
                                                                                                tags$td('Aggregate landings'),
                                                                                                tags$td('Annual total weight of combined catch share program species generated by vessels that fish quota')
                                                                                              ),
                                                                                              tags$tr(
                                                                                                tags$td('ACL exceeded'),
                                                                                                tags$td('Was the ACL exceeded for any species/stock within the catch share program? (Y/N)')
                                                                                              ),
                                                                                              tags$tr(
                                                                                                tags$td('Utilization (%)'),
                                                                                                tags$td('Portion of target species TAC that is caught ad retained within a fishing year. Aggregate landings divided by quota allocated to catch share program.')
                                                                                              )
                                                                                            )
                                                                                 )
                                                                   )
                                                          )
                                                        )
                                                      )
                               ),                   
                               
                               bslib::accordion_panel("Fishing Effort",  #### fishing effort table ------
                                                      bslib::card(
                                                        full_screen = TRUE,
                                                        bslib::card_body(
                                                          tags$div(tags$fieldset(style= "width: 40%;","The effort metrics are the season length index, the number of active vessels, and the number of entities
                                       holding share. The season length index is defined as the number of days in which at least one vessel was fishing divided
                                       by the number of days in the regulatory fishing season. This index provides a single, unit-less metric of season length
                                       that can be aggregated over multiple areas or species with different season lengths within the same program. The index
                                       measures the relative proportion of the legal fishing season during which some or all vessels actively fished. The
                                       aggregate program level season length index is calculated as the weighted harmonic mean number of days in which at
                                       least one vessel was fishing by area using catch volume as weights and then divided by the regulatory fishing season
                                       length. The number of active vessels is one indicator of the scale of participation and effort in the fishery and can
                                       indicate changes in the expansion or consolidation of vessels in the fishery after rationalization. The number of
                                       entities holding share reflects the number of quota share owners that may be reduced as a result of consolidation or
                                       increase with new entrants over time and indicates the level of ownership accumulation in the fishery"),
                                                                   tags$fieldset(style= "width: 50%; float: right;",
                                                                                 tags$table(class="styled-table",
                                                                                            tags$thead(
                                                                                              tags$tr(
                                                                                                tags$th('Indicator'),
                                                                                                tags$th('Definition')
                                                                                              )
                                                                                            ),
                                                                                            tags$tbody(
                                                                                              tags$tr(
                                                                                                tags$td('Season length index'),
                                                                                                tags$td('The number of days in which at least one vessel was fishing divided by the number of days
                                                                  in the regulatory fishing season.')
                                                                                              ),
                                                                                              tags$tr(
                                                                                                tags$td('Active vessels'),
                                                                                                tags$td('Annual number of vessels that fish quota and landing one or more pounds of any catch share program species.')
                                                                                              ),
                                                                                              tags$tr(
                                                                                                tags$td('Entities holding share'),
                                                                                                tags$td('Annual total number of entities/individuals/vessel owners/permit holders receiving quota share at the beginning of the year.')
                                                                                              ),
                                                                                              tags$tr(
                                                                                                tags$td('Trips'),
                                                                                                tags$td('Annual total number of trips taken by vessels fishing quota on which one or more pounds of any catch share program species were landed.')
                                                                                              ),
                                                                                              tags$tr(
                                                                                                tags$td('Days at sea'),
                                                                                                tags$td('Annual total number of days absent on trips taken by vessels fishing quota on which one or more pounds of any catch share program species were landed.')
                                                                                              )
                                                                                            )
                                                                                 )
                                                                   )
                                                          )
                                                        )
                                                      )
                               ),
                               bslib::accordion_panel(title = "Landing Revenue", #### landing revenue table ------
                                                      bslib::card(
                                                        full_screen = TRUE,
                                                        bslib::card_body(
                                                          tags$div(tags$fieldset(style= "width: 40%;","The revenue metrics are the aggregate revenue from catch share species, average prices of catch share species,
                                       the revenue per active vessel, and the Gini coefficient. Revenues are a function of landings and prices, which may
                                       trend in opposite directions due to changes in the demand for the species that may or may not be caused by the
                                       movement to catch share management. Prices may be affected by catch share management, but they are also influenced
                                       by external market factors such as price and availability of substitute products, fluctuating exchange rates, and
                                       changes in demand. While changes in prices cannot be solely attributed to catch shares, they provide a useful metric
                                       to compare the performance of the fishery over time in terms of improving quality and marketability. The Gini
                                       coefficient is a measure of the evenness of the distribution of revenue among the active vessels, which increases
                                       as revenues become more concentrated on fewer vessels, and is useful to examine the distributional impacts of catch
                                       share programs across vessels"),
                                                                   tags$fieldset(style= "width: 50%; float: right;",
                                                                                 tags$table(class="styled-table",
                                                                                            tags$thead(
                                                                                              tags$tr(
                                                                                                tags$th('Indicator'),
                                                                                                tags$th('Definition')
                                                                                              )
                                                                                            ),
                                                                                            tags$tbody(
                                                                                              tags$tr(
                                                                                                tags$td('Aggregate revenue from Catch Share species'),
                                                                                                tags$td('Annual total revenue of combined catch share program species generated by vessels that fish quota.')
                                                                                              ),
                                                                                              tags$tr(
                                                                                                tags$td('Aggregate revenue from non-Catch Share species'),
                                                                                                tags$td('Aggregate ex-vessel revenue from non-catch share species caught on catch share program trips.')
                                                                                              ),
                                                                                              tags$tr(
                                                                                                tags$td('Average price'),
                                                                                                tags$td('Aggregate revenue from catch share species divided by aggregate landings')
                                                                                              ),
                                                                                              tags$tr(
                                                                                                tags$td('Revenue per active vessel'),
                                                                                                tags$td('Aggregate revenue divided by active vessels')
                                                                                              ),
                                                                                              tags$tr(
                                                                                                tags$td('Revenue per trip'),
                                                                                                tags$td('Aggregate revenue divided by trip')
                                                                                              ),
                                                                                              tags$tr(
                                                                                                tags$td('Revenue per day at sea'),
                                                                                                tags$td('Aggregate revenue divided by day at sea')
                                                                                              ),
                                                                                              tags$tr(
                                                                                                tags$td('Gini Coefficient'),
                                                                                                tags$td('A measure of the evenness of the distribution of revenue among the
                                                          active vessels. The Gini coefficient increases as
                                                          revenues become more concentrated on fewer vessels.')
                                                                                              )
                                                                                            )
                                                                                 )
                                                                   )
                                                          )
                                                        )
                                                      )
                               ),
                               bslib::accordion_panel("Other", #### other table ------
                                                      bslib::card(
                                                        full_screen = TRUE,
                                                        bslib::card_body(
                                                          tags$div(tags$fieldset(style= "width: 40%;","Depending on the program, other metrics are calculated that may be beneficial when determining the influence of the implemented catch-share program.
                                               The other metrics and their descriptions can be found here."),
                                                                   tags$fieldset(style= "width: 50%; float: right;",
                                                                                 tags$table(class="styled-table",
                                                                                            tags$thead(
                                                                                              tags$tr(
                                                                                                tags$th('Indicator'),
                                                                                                tags$th('Definition')
                                                                                              )
                                                                                            ),
                                                                                            tags$tbody(
                                                                                              tags$tr(
                                                                                                tags$td('Cost recovery fee'),
                                                                                                tags$td('Amount collected for cost recovery.')
                                                                                              ),
                                                                                              tags$tr(
                                                                                                tags$td('Share cap in place'),
                                                                                                tags$td('An ownership share and/or allocation cap is any measure consistent with the MSA LAPP purpose
                                                                  and intent whether or not the catch share program is required to have an excessive share cap. Y/N')
                                                                                              ),
                                                                                              
                                                                                            )
                                                                                 )
                                                                   )
                                                          )
                                                        )
                                                      )
                               )
                     ),
                     
                   ),
                   
                   column(width=12,
                          tags$div(class = 'wrap-section',
                                   tags$img(src = "fish_stacked.jpg",  class="wrap-image-left"),
                                   tags$div(class = 'wrap-header-left',
                                            h4(paste0("PERCENTAGE CHANGE FOR ", current_yr-1," TO ",  current_yr))),
                                   bslib::card(  class = "border-0 shadow-none", tableOutput("cs_percent_tbl")),
                          )),
                   tags$footer(tags$a(href="https://www.psmfc.org/program/alaska-fisheries-information-network-akfin?pid=17",
                                      img(src='PSMFC-Logo-new.png',
                                          align = "right", width = "75", height = "75")),
                               tags$a(href="https://www.fisheries.noaa.gov/about/alaska-fisheries-science-center",
                                      img(src='nmfslogo_color.png',
                                          align = "right", width = "175", height = "75")),
                               style= "background-color:  #dae5ed")
  ),
  
  ### catch share species tabs ------------------------------------------------------------
  #### a80 ------
  bslib::nav_menu(title = "Amendment 80", value = "a80",
           bslib::nav_panel("About", value = "a80_about",
                     bslib::layout_columns(
                       col_widths = c(4, 8),
                       bslib::card(fill = FALSE, fillable = FALSE,  class = "border-0 shadow-none",
                            includeHTML(path = "./www/a80_page_new.html")),
                       bslib::card(fill = FALSE, fillable = FALSE,
                            tags$div(style="background-color:#262626 !important; color: white; padding-top: 7px; padding-bottom: 7px; padding-left: 7px; font-size: 20px",
                                     tags$header(strong("MANAGEMENT CONTEXT"))),
                            p("The Bering Sea/Aleutian Islands non-Pollock Trawl Catcher-Processor Groundfish Cooperatives Program (also known as Amendment 80) 
                                                        was implemented in 2008 for those groundfish catcher/processors (CPs) fishing in the Bering Sea/Aleutian Islands (BSAI) region
                                                        that were not specifically listed as eligible to participate in the American Fisheries Act (AFA) Pollock Cooperatives Program. 
                                                        NOAA Fisheries identified 28 CP vessels that were eligible to participate in the Amendment 80 Program (Amendment 80 sector) and 
                                                        has issued Amendment 80 quota share (QS) to 27 eligible persons. The program provides an allocation of six groundfish species 
                                                        including Atka mackerel, Aleutian Islands Pacific ocean perch, flathead sole, Pacific cod, rock sole, and yellowfin sole, 
                                                        prohibited species catch (PSC) allowances for halibut and crab, as well as sideboard limits for five groundfish species 
                                                        in the Gulf of Alaska (GOA) to Amendment 80 vessels, and authorizes program participants to form cooperatives. Amendment 80
                                                        vessels are typically smaller in size and processing capacity than the AFA CPs. Prior to the Amendment 80 program, these 
                                                        vessels primarily produced headed and gutted products, but as the race for fish has been eliminated and Amendment 80 initially 
                                                        implemented increased groundfish retention standards, they are increasingly producing other product forms."),
                            p("The goals of the Amendment 80 program are to improve retention and utilization and reduce bycatch for the Amendment 80 sector. 
                                                        The program also includes sideboard allowances in the GOA for pollock, Pacific cod, Pacific Ocean perch, northern rockfish, and
                                                        pelagic shelf rockfish (dusky rockfish) to limit these vessels’ participation in other fisheries to their historic levels. 
                                                        One cooperative formed in 2008 that included 16 of 24 participating vessels while the other vessels participated in the Amendment
                                                        80 limited access sector until 2011 when those vessels formed a second cooperative. Since 2017, a single cooperative comprises all Amendment 80 vessels."),
                            tags$div(style="background-color:#262626 !important; color: white; padding-top: 7px; padding-bottom: 7px; padding-left: 7px; font-size: 20px",
                                     tags$header(strong("CATCH SHARE PRIVILEGE CHARACTERISTICS"))),
                            p("Amendment 80 QS allocations are tied to the respective eligible vessels (or to the associated LLP in cases where a vessel is 
                                                        lost or is withdrawn from the program), and are allocated to their cooperative based on the vessel’s catch history. Amendment
                                                        80 vessels that do not join a cooperative do not receive an exclusive harvest privilege and must fish in the Amendment 80 
                                                        limited access sector. Amendment 80 QS can be transferred by selling the vessel, its permits, and accompanying catch history.
                                                        It is also possible to sell Amendment 80 QS separate from an Amendment 80 vessel under specific circumstances, but sellers are 
                                                        required to include all allocated Amendment 80 QS species in the sale, and therefore would be precluded from participating in 
                                                        the Amendment 80 fishery. Amendment 80 cooperatives can transfer annual QS pounds, called cooperative quota (CQ), to other 
                                                        Amendment 80 vessels within and between cooperatives. Amendment 80 catch share privileges are revocable, but were allocated 
                                                        in perpetuity. The Amendment 80 Program has an excessive share provision that limits a person to holding 30% of the QS and CQ 
                                                        assigned to the Amendment 80 sector. Vessel use caps also limit an Amendment 80 vessel to harvesting 20% of the Amendment 80 
                                                        species catch limits allocated to the Amendment 80 sector."),
                       )
                     )
           ),
           bslib::nav_panel("Performance Metrics", value = "a80_metrics",
                     a80_ui('a80')
           )
  ),
  #### halibut --------
  bslib::nav_menu(title = "IFQ Halibut", value = "halibut",
                  bslib::nav_panel("About", value = "halibut_about",
                                   bslib::layout_columns(
                       col_widths = c(4, 8),
                       bslib::card(fill = FALSE, fillable = FALSE,class = "border-0 shadow-none",
                            includeHTML(path = "./www/halibut_page_new.html")),
                       bslib::card(fill = FALSE, fillable = FALSE,
                            tags$div(style="background-color:#70262B !important; color: white; padding-top: 7px; padding-bottom: 7px; padding-left: 7px; font-size: 20px",
                                     tags$header(strong("MANAGEMENT CONTEXT"))),
                            p("The North Pacific Halibut IFQ program was implemented simultaneously with the North Pacific
                                                      Sablefish IFQ Program, but the sablefish IFQ program will be considered separately below. Halibut
                                                      in the North Pacific are commercially caught by catcher vessels (CVs) that deliver their catch
                                                      onshore and catcher/processor vessels (CPs) that catch and process their catch at sea using longline
                                                      gear. Halibut are also caught as prohibited species catch (PSC) by vessels using trawl gear which
                                                      means they cannot be retained by these vessels. The IFQ program only applies to halibut caught
                                                      with longline gear in the directed commercial fishery. In addition to the directed commercial fishery,
                                                      there are substantial recreational and subsistence sectors that depend on the halibut resource.
                                                      Beginning in 2014, charter operators are able to lease a limited amount of commercial IFQ in Areas
                                                      2C and 3A as part of the Pacific Halibut Catch Sharing Plan. Additionally, through the Community
                                                      Development Quota (CDQ) Program, a percentage of the Bering Sea and Aleutian Islands (BSAI)
                                                      halibut catch limits, which vary by management area, is allocated to entities representing eligible
                                                      Western Alaska communities designated in the Magnuson-Stevens Act. However, this section only
                                                      examines the performance of the halibut IFQ portion of the program."),
                            p("Halibut fisheries off the coast of Alaska are managed by two agencies: the International Pacific
                                                      Halibut Commission (IPHC) and the North Pacific Fishery Management Council (NPFMC). The
                                                      IPHC is responsible for assessment of the halibut stock and establishes the annual Total Constant
                                                      Exploitation Yield (which is comparable to an ACL for the directed commercial fishery). The
                                                      NPFMC is responsible for allocating the catch limits established for the halibut management areas
                                                      off the coast of Alaska among various user groups. The halibut IFQ program was developed by the
                                                      NPFMC and implemented by NOAA Fisheries in 1995 to manage the directed commercial halibut
                                                      fishery in Alaska. Prior to the IFQ program, the fishery operated as a derby and often only lasted a
                                                      few days per year in certain areas. Quota Share (QS) was initially issued based on both historic and
                                                      recent participation of persons who, in 1988, 1989, or 1990, owned or leased vessels with qualifying
                                                      landings. QS allocations were issued in amounts commensurate with creditable halibut landings
                                                      during the “best five” of 7 years from 1984-1990. The primary objectives of the IFQ Program are to:
                                                      1) eliminate gear conflicts; 2) address safety concerns; and 3) improve product quality and value"),
                            p("The Halibut and Sablefish IFQ program includes a cost recovery provision in which the fishermen
                                                      pay a fee based on the cost to the government to manage the program. Recoverable costs cannot
                                                      exceed 3% of the total ex-vessel value of the fishery and include the costs related to management,
                                                      data collection, and enforcement of a Limited Access Privilege Program (LAPP) or Community
                                                      Development Quota Program. Cost recovery began in 2000 for the halibut IFQ program and has
                                                      ranged from 1.0% to 3% of the ex-vessel value of the fishery. Since 2015, the 
                                                      fishery did not reach the 3% limit in only four years (2017, 2018, 2021, and 2022)."),
                            tags$div(style="background-color:#70262B !important; color: white; padding-top: 7px; padding-bottom: 7px; padding-left: 7px; font-size: 20px",
                                     tags$header(strong("CATCH SHARE PRIVILEGE CHARACTERISTICS"))),
                            p("There are two forms of quota in the Halibut and Sablefish IFQ Program, QS and the annual
                                                      allocation of IFQ in pounds derived from the QS. The QS are a revocable, indefinite privilege that
                                                      entitles the holder to a share of the total area- and vessel class-specific IFQ allocated each year.
                                                      Individuals as well as non-individuals (such as a corporation) can hold QS and IFQ. Prior to the
                                                      beginning of each fishing season, IFQ is allocated to QS holders based upon their held QS, the total
                                                      allowable catch (TAC) in each area which is recommended by the IPHC, and the total amount
                                                      of QS in each management area (QS pool). QS and the resulting IFQ are designated for use in
                                                      specific areas and on vessels of a specific size. These provisions are intended to limit catch by area
                                                      and maintain a fleet with a range of vessel sizes. The IFQ Program also contains a number of QS
                                                      and IFQ use restrictions, including use caps and designation of small QS blocks that are intended
                                                      to prevent consolidation and maintain participation opportunities for small operations and new
                                                      entrants. IFQ are valid only for one year, but there are rollover provisions that allow QS holders
                                                      to carry over to the next year up to 10% of their unused IFQ and any overages (up to 10%) are
                                                      taken from the following year’s IFQ allocation."),
                            p("Catcher vessel QS are transferable to other initial issuees or to those who have become transfer eligible through obtaining NOAA Fisheries’ approval by submitting an Application for Eligibility to
                                                      Receive QS/IFQ. To be eligible, potential QS/IFQ recipients must be a U.S. citizen and have 150
                                                      or more days of experience working as part of a harvesting crew in any U.S. commercial fishery.
                                                      Halibut QS can be sold with or without the annual IFQ derived therefrom (plus adjustments from
                                                      prior year QS used). However, CV IFQ can be leased annually to other eligible permit holders only under limited circumstances. Non-individual entities new to the program are only able to purchase
                                                      QS or lease IFQ for the largest vessel class of “catcher/processor” quota (category A)."),
                            p("The IFQ Program has a number of excessive share provisions. There are QS holding caps on both
                                                      individuals as well as entities. No person, individually or collectively, can hold/control more than
                                                      0.5%-1.5% of halibut QS in specific areas and combinations of areas. In addition, vessel use caps
                                                      limit each vessel to harvesting from 0.5%-1% of the halibut TAC in specific areas and combinations
                                                      of areas. Halibut CDQ fishing is not subject to excessive share provisions. There are also owner
                                                      on-board requirements for CV QS and IFQ to limit the use of hired skippers.")))),
                  bslib::nav_panel("Performance Metrics", value = "halibut_metrics",
                     halibut_ui("halibut")
           )
  ),
  #### crab ------
  bslib::nav_menu(title = "BSAI Crab", value = "crab",
                  bslib::nav_panel("About", value = "crab_about",
                                   bslib::layout_columns(
                       col_widths = c(4, 8),
                       bslib::card(fill = FALSE, fillable = FALSE, class = "border-0 shadow-none",
                            includeHTML(path = "./www/crab_page_new.html")),
                       bslib::card(fill = FALSE, fillable = FALSE,
                            tags$div(style="background-color:#475D52 !important; color: white; padding-top: 7px; padding-bottom: 7px; padding-left: 7px; font-size: 20px",
                                     tags$header(strong("MANAGEMENT CONTEXT"))),
                            p("The Bering Sea and Aleutian Islands crab fisheries comprise large,
                                                        industrial vessels using pot gear and a large-scale onshore processing
                                                        sector. The fishery management plan (FMP) governing these fisheries, the
                                                        Bering Sea and Aleutian Islands king and Tanner Crab FMP, was approved
                                                        by the Secretary of Commerce on June 2, 1989. The FMP establishes a
                                                        State/Federal cooperative management regime that defers crab management
                                                        to the State of Alaska with Federal oversight. State regulations are
                                                        subject to the provisions of the FMP, including its goals and
                                                        objectives, the Magnuson-Stevens Act, the National Standards and other
                                                        applicable federal laws. The FMP has been amended several times since
                                                        its implementation to limit access to the fisheries, establish a vessel
                                                        license limitation program, define essential fish habitat and associated
                                                        protection measures, amongst other topics."),
                            p('Managing capacity in these fisheries has been a challenge since the
                                                          inception of the FMP. Overcapacity in the Bering Sea and Aleutian
                                                          Islands (BSAI) Crab Fishery required season limitations to control catch
                                                          levels, with seasons in some fisheries only lasting five days. The
                                                          resulting "derby fishery" led to unsafe fishing conditions and
                                                          numerous fatalities for crew, particularly in winter months when most
                                                          crab fisheries are prosecuted. Harvesting and processing capacity
                                                          expanded to accommodate highly abbreviated seasons, leading to further
                                                          economic inefficiencies.'),
                            p("To address overcapacity, the North Pacific Fishery Management Council
                                                        took a series of actions to limit access to these resources, including a
                                                        moratorium on new vessels entering the fishery (1996); a vessel license
                                                        limitation program (2000); a capacity reduction (buyback) program
                                                        (2004); and, in 2005, the BSAI Crab Rationalization Program. The BSAI
                                                        Crab Rationalization Program includes most king and Tanner crab
                                                        fisheries in the Bering Sea and Aleutian Islands. The BSAI Crab
                                                        Rationalization Program applies to the following Bering Sea and Aleutian
                                                        Islands crab fisheries: Bristol Bay red king crab, Western Aleutian
                                                        Islands (Adak) golden king crab, Eastern Aleutian Islands golden king
                                                        crab, Western Aleutian Islands red king crab, Pribilof Islands red and
                                                        blue king crab, St. Matthew Island blue king crab, Bering Sea snow crab,
                                                        Eastern Bering Sea Tanner crab and Western Bering Sea Tanner crab."),
                            p('Prior to implementation of the BSAI Crab Rationalization Program, the
                                                        Bering Sea Tanner Crab fishery was closed to fishing due to low stock
                                                        abundance. Two fisheries (Western Aleutian Islands red king crab and
                                                        Pribilof Island red and blue king crab) have been closed to fishing
                                                        throughout the duration of the Crab Rationalization Program. The St.
                                                        Matthew Island blue king crab fishery was closed for four of the six
                                                        years of the IFQ Program. In the second year of the IFQ Program and
                                                        following a stock assessment, the Bering Sea Tanner Crab fishery was
                                                        split into the Western and Eastern Bering Sea Tanner Crab fisheries. The
                                                        Western Bering Sea Tanner crab fishery was closed for two of the five
                                                        years, while the Eastern Bering Sea Tanner Crab fishery was closed for
                                                        one year since this split during the IFQ Program.'),
                            tags$div(style="background-color:#475D52 !important; color: white; padding-top: 7px; padding-bottom: 7px; padding-left: 7px; font-size: 20px",
                                     tags$header(strong("CATCH SHARE PRIVILEGE CHARACTERISTICS"))),
                            p("The North Pacific Fishery Management Council developed the BSAI Crab
                                                        Rationalization Program over a six-year period. In 2005, the BSAI Crab
                                                        Rationalization Program was implemented to address the race to harvest,
                                                        high bycatch and discard mortality, product quality issues and balance
                                                        the interests of those who depend on crab fisheries. The BSAI Crab
                                                        Rationalization Program includes share allocations to harvesters and
                                                        processors. Processor quota was incorporated to preserve the viability
                                                        of processing facilities in dependent communities and particularly to
                                                        maintain competitive conditions in ex-vessel markets. Community
                                                        interests are protected by Community Development Quota (CDQ) and Adak
                                                        Community allocations, and regional landings and processing
                                                        requirements, as well as several community protection measures. The
                                                        performance indicator information provided herein refers only to the IFQ
                                                        component of the BSAI Crab Rationalization Program."),
                            p("Sablefish quota share can be sold with or without the annual IFQ derived from the quota share.
                                                        Catcher vessel quota share can be transferred to other initial issuees or to those who have become
                                                        eligible to receive QS by transfer. To be eligible, potential QS/IFQ recipients must be a U.S.
                                                        citizen and have worked as part of a harvesting crew in any U.S. commercial fishery for at least
                                                        150 days. IFQ can be leased annually to other eligible permit holders under limited circumstances.
                                                        Non-individual entities that are not initial issuees are only able to purchase QS or lease IFQ for the
                                                        largest vessel class of “catcher/processor” quota (category A). The IFQ Program has a number of
                                                        excessive share provisions. There are ownership caps on both individuals as well as entities. No
                                                        individual can hold/control more than 1% of sablefish QS in specific areas and combinations of areas.
                                                        In addition, vessel use caps limit each vessel to harvesting 1% of the sablefish TAC in specific areas
                                                        and combinations of areas. Sablefish CDQ fishing is not subject to the excessive share provisions.
                                                        There are also limits on the use of hired skippers through a requirement that the holder of QS be
                                                        on board when using CV QS and IFQ. There is also a revolving loan program implemented by the
                                                        NPFMC and NOAA Fisheries to assist entry level and small vessel fishermen acquire funding. The
                                                        loan program is capitalized through a portion of the cost recovery fees collected."),
                            tags$div(style="background-color:#475D52 !important; color: white; padding-top: 7px; padding-bottom: 7px; padding-left: 7px; font-size: 20px",
                                     tags$header(strong("KEY EVENTS/FEATURES"))),
                            p("King and Tanner crab are harvested in nine distinct fisheries that are
                                                          defined by a combination of species and spatial areas. Uniquely, the
                                                          Council was granted special Congressional authority to allocate
                                                          processor quota in addition to harvesting quota. IFQ privileges are
                                                          delineated as quota shares (that provide the holder a percentage of the
                                                          IFQ allocation), which represent the annual harvestable pounds (derived
                                                          from the shares) to harvesters, which must be matched with individual
                                                          processor quota when making a delivery to a processor. The initial
                                                          allocation issued harvest shares to license limitation program (LLP)
                                                          crab license holders and crew who were state permit holders (typically
                                                          vessel captains) based on creditable historical landings. Processor
                                                          shares were issued to processors with specific history in the crab
                                                          fisheries. Harvest quota share and processor quota share are
                                                          transferable, subject to limitations. Shares issued to LLP crab permit
                                                          holders comprise 97% of all harvesting quota share; the remaining 3%
                                                          were issued as captain/crew quota share. Both harvest and processor
                                                          quota share are split into catcher vessel shares and catcher/processor
                                                          shares. Annual individual processing quota is issued in the amounts
                                                          matched to the amounts of catcher vessel LLP harvest quota for the nine
                                                          fisheries."),
                            p("This program requires reporting of some economic cost and revenue data
                                                          from vessel owners. Processors also submit data on crew costs. These
                                                          data were intended to help determine if the program meets Council
                                                          objectives over time, including the use of processor quota share."),
                            p("Section 304(d)(2) of the Magnuson-Stevens Act authorizes the Secretary
                                                          to adopt regulations implementing a cost recovery program to recover the
                                                          actual costs related to management, data collection and enforcement of a
                                                          Limited Access Privilege Program or Community Development Quota Program.
                                                          The Magnuson-Stevens Act also allows for additional collections to cover
                                                          a loan program that provides assistance for quota share purchase by new
                                                          entrants and small vessel owners. These fees can be a maximum of 3% of
                                                          the ex-vessel value of the program species. During the Baseline Period,
                                                          the cost recovery program was not applicable to the Crab Fishery. The
                                                          cost recovery fee for the Crab Program varies each year because by
                                                          regulation, the fee percentage is computed at the start of the fishing
                                                          season, using prior year costs. This makes it possible to
                                                          have years in which no fees are collected, as was the case in 2009/10.
                                                          In 2014/15, $1.48 million was collected for the cost recovery program,
                                                          less than 1% of IFQ Crab revenue."),
                            p('The purpose of excessive quota share caps is to prevent quota holders
                                                          from controlling production (and processing) as well as achieving
                                                          management objectives, per the Magnuson-Stevens Act and the National
                                                          Standards. The BSAI Crab Rationalization Program has share caps in place
                                                          for all harvester and processor quota share holders. The excessive share
                                                          cap varies from 1-20% of initial harvest quota share based on fishery
                                                          or area, quota type, and entity type for owner quota share and from
                                                          2-20% of initial harvest quota share for crew quota share. Processors
                                                          may not hold or use more than 30% of processor shares in each fishery.'),
                            p('The management year begins July 1 and ends June 30 of the following
                                                          year. Annual data are for the fishing year (e.g., the 2006/07 fishing
                                                          year). Crab quota refers to all of the IFQ fisheries combined.')))),
                  bslib::nav_panel("Performance Metrics", value = "crab_metrics",
                     crab_ui("crab")
           )
  ),
  #### afa  ---------------
  bslib::nav_menu(title = "AFA", value = "afa",
                  bslib::nav_panel("About", value = "afa_about",
                                   bslib::layout_columns(
                                     col_widths = c(4, 8),
                                     bslib::card(fill = FALSE, fillable = FALSE,class = "border-0 shadow-none",
                                          includeHTML(path = "./www/afa_page_new.html")),
                                     bslib::card(fill = FALSE, fillable = FALSE,
                                          tags$div(style="background-color:#205E42 !important; color: white; padding-top: 7px; padding-bottom: 7px; padding-left: 7px; font-size: 20px",
                                                   tags$header(strong("MANAGEMENT CONTEXT"))),
                                          p("There are three types of vessels that participate in the Bering Sea and Aleutian Islands (BSAI) 
                                          walleye pollock fishery: catcher vessels (CVs) that deliver their catch onshore, catcher/processors (CPs) 
                                          that catch and process their catch at sea, and motherships that are at-sea processors receiving catch from CVs 
                                          but do not catch any of their own fish. Pollock in the BSAI management area are targeted only with pelagic (midwater) 
                                          trawl gear. Landings averaged approximately 1.3 million metric tons per year over the past five-years, which represents over half of Alaska 
                                          groundfish production volume and makes it the largest fishery in the United States by volume. 
                                          Ten percent of the BSAI total allowable catch (TAC) is allocated to communities through the Community Development Quota (CDQ) Program. 
                                          There is no recreational sector for pollock in the North Pacific."),
                                          p("The American Fisheries Act (AFA) Pollock Cooperatives Program was established by the U.S. Congress under the American Fisheries Act in 1998, and was implemented for the 
                                          CP sector in 1999 and the CV and mothership sectors in 2000. The goals of the AFA were to resolve frequent allocation disputes between the inshore (CVs) and offshore (CPs and motherships) 
                                          sectors and reduce externalities as a result of the race for fish. The AFA established minimum U.S. ownership requirements, vessel and processor participation requirements, defined the list of 
                                          eligible vessels, finalized the TAC allocation among sectors, provided an allocation to the CDQ Program, and authorized the formation of cooperatives. The allocation of the Bering Sea TAC to the AFA 
                                          (after the 10% allocation to the CDQ program and incidental catch allowance in other fisheries are deducted), is 50% to the CV sector, 40% to the CP sector, and 10% to the mothership sector."),
                                          tags$div(style="background-color:#205E42 !important; color: white; padding-top: 7px; padding-bottom: 7px; padding-left: 7px; font-size: 20px",
                                                   tags$header(strong("CATCH SHARE PRIVILEGE CHARACTERISTICS"))),
                                          p("Participation in the AFA pollock fishery is permitted only by the vessels listed in the American 
                                            Fisheries Act, and those eligible vessels are authorized to form cooperatives which receive an 
                                            allocation (exclusive harvest privilege) of a percentage of the Bering Sea pollock TAC from NOAA
                                            Fisheries. Five inshore cooperatives have formed between CVs and eligible shoreside processors, 
                                            and CVs are required to deliver 90% of their BSAI pollock to a cooperative member processor. The 
                                            CV cooperatives are allocated a portion of the pollock TAC as a directed fishing allowance based 
                                            on the catch history of its member vessels. The CP and mothership sectors have each formed a voluntary 
                                            cooperative to receive and harvest the exclusive privilege allocated to the sector. Starting in 2011
                                            with the passage of Amendment 91 to the BSAI Fishery Management Plan, incentive plan agreements (IPA)
                                            were put in place for AFA participants to self-regulate and reduce the number of incidentally caught 
                                            salmon in the pollock fishery and allowed NOAA Fisheries to allocate transferable prohibited species 
                                            catch (PSC) allowance for Chinook salmon to vessels in the pollock fishery."),
                                          p("Catch share privileges under the AFA are revocable, but were allocated in perpetuity. There is a single
                                            cooperative in the CP and mothership sectors, and contracts among members of the cooperative have been 
                                            developed to allocate their catch across vessels. Catcher vessel cooperatives can exchange directed 
                                            fishing allowance among their member vessels as they see fit, but since the CV cooperative allocations
                                            are based on the membership of their vessels, vessels have to change cooperatives to exchange CV directed
                                            fishing allowance across cooperatives. If a vessel owner decides to change cooperatives, the vessel is 
                                            required fish for one year in the limited access fishery and is not allowed to participate in the cooperative
                                            system, unless the vessel owner’s current cooperative approves delivery to another cooperative member processor.
                                            Catcher vessel cooperatives are also able to contract with non-member AFA eligible vessels to harvest a portion 
                                            of their allocation. The contract must be approved by both the non-member vessel and that vessel’s cooperative,
                                            which is similar to a quota lease. There are also excessive use caps in both the inshore harvesting and processing
                                            sectors which state that no entity can harvest more than 17.5% or process more than 30% of the directed fishing
                                            allowance of pollock allocated to the inshore sector.")))),
                  bslib::nav_panel("Performance Metrics", value = "afa_metrics",
                                   afa_ui("afa")
                  )
  ),
  #### sablefish ----           
  bslib::nav_menu(title = "IFQ Sablefish", value = "sablefish",
                  bslib::nav_panel("About", value = "sablefish_about",
                                   bslib::layout_columns(
                       col_widths = c(4, 8),
                       bslib::card(fill = FALSE, fillable = FALSE, class = "border-0 shadow-none",
                            includeHTML(path = "./www/sablefish_page_new.html")),
                       bslib::card(fill = FALSE, fillable = FALSE,
                            tags$div(style="background-color:#46412A !important; color: white; padding-top: 7px; padding-bottom: 7px; padding-left: 7px; font-size: 20px",
                                     tags$header(strong("MANAGEMENT CONTEXT"))),
                            p("The North Pacific Sablefish IFQ Program was implemented simultaneously with the North Pacific
                             Halibut IFQ Program, but they will be assessed separately in this report. Sablefish (also known as
                             black cod) in the North Pacific are commercially caught by catcher vessels (CVs) that deliver their
                             catch onshore and catcher/processor vessels (CPs) that catch and process their catch at sea using
                             longline (hook-and-line, jig, troll, and handline), pot, and trawl gear, but the IFQ program only
                             applies to longline and pot gears. Twenty percent of the Bering Sea and Aleutian Islands (BSAI)
                             sablefish total allowable catch (TAC) is allocated to vessels using hook-and-line or pot gear and 7.5%
                             of the sablefish TAC allocated to trawl gear are reserved for use in the Community Development
                             Quota (CDQ) program. There is not a substantial recreational sector for sablefish in the North
                             Pacific. Similar to the Halibut IFQ program, this section only examines the performance of the
                             sablefish IFQ portion of the program."),
                            p("The sablefish IFQ program was developed by the North Pacific Fishery Management Council
                              (NPFMC) and implemented by NOAA Fisheries in 1995. The sablefish IFQ program is managed
                              by the NPFMC, which is responsible for establishing Annual Catch Limits (ACLs) and TACs for
                              sablefish and allocating TACs among various user groups. Prior to the IFQ program, the fisheries
                              operated as a derby fishery which often lasted a few days per year in some management areas.
                              Quota Share (QS) was initially issued to persons based on both historic and recent participation of
                              persons who, in 1988, 1989, or 1990, owned or leased vessels with qualifying landings. Quota share
                              were issued in amounts commensurate with creditable landings during the “best five” of 6 years
                              1985-1990. The primary objectives of the IFQ Program are to 1) eliminate gear conflicts; 2) address
                              safety concerns; and 3) improve product quality and value."),
                            p("The Halibut and Sablefish IFQ Program includes a cost recovery provision whereby the fishermen
                              are assessed a fee based on the cost to the government to manage the program. The costs that can
                              be recovered include the costs related to management, data collection, and enforcement of a Limited
                              Access Privilege Program (LAPP) or Community Development Quota Program, and cannot exceed
                              3% of the total ex-vessel value of the fishery. Cost recovery began in 2000 for sablefish IFQ and has 
                              reached the 3% limit in 2015, 2016, 2019, 2020, 2023, and 2024."),
                            tags$div(style="background-color:#46412A !important; color: white; padding-top: 7px; padding-bottom: 7px; padding-left: 7px; font-size: 20px",
                                     tags$header(strong("CATCH SHARE PRIVILEGE CHARACTERISTICS"))),
                            p("There are two forms of quota in the sablefish IFQ Program, QS and annual IFQ in pounds derived
                              from QS. Quota shares are a revocable, indefinite privilege that entitles the holder to a share of the
                              total area- and vessel class-specific IFQ allocated each year. Quota share holders can be individuals
                              or non-individuals (such as a corporation). Prior to the beginning of each fishing season, IFQ is
                              allocated to QS holders based upon their held QS, the total amount of quota in each management
                              area (QS pool), and the total allowable catch (TAC) in each area. Quota shares and the derived
                              IFQ are specified for use in particular areas and on vessels of a particular size. These conditions are
                              intended to maintain a diverse fleet of vessels and limit catch by area. The IFQ program also includes
                              use caps and small QS blocks that are intended to limit consolidation and maintain participation
                              opportunities for small operations and new entrants. IFQ are valid only for one year, but there are
                              provisions that allow QS holders to carry over to the next year up to 10% of their unused IFQ and
                              any overages (up to 10%) are taken from the following year’s IFQ allocation."),
                            p("Sablefish quota share can be sold with or without the annual IFQ derived from the quota share.
                              Catcher vessel quota share can be transferred to other initial issuees or to those who have become
                              eligible to receive QS by transfer. To be eligible, potential QS/IFQ recipients must be a U.S.
                              citizen and have worked as part of a harvesting crew in any U.S. commercial fishery for at least
                              150 days. IFQ can be leased annually to other eligible permit holders under limited circumstances.
                              Non-individual entities that are not initial issuees are only able to purchase QS or lease IFQ for the
                              largest vessel class of “catcher/processor” quota (category A). The IFQ Program has a number of
                              excessive share provisions. There are ownership caps on both individuals as well as entities. No
                              individual can hold/control more than 1% of sablefish QS in specific areas and combinations of areas.
                              In addition, vessel use caps limit each vessel to harvesting 1% of the sablefish TAC in specific areas
                              and combinations of areas. Sablefish CDQ fishing is not subject to the excessive share provisions.
                              There are also limits on the use of hired skippers through a requirement that the holder of QS be
                              on board when using CV QS and IFQ. There is also a revolving loan program implemented by the
                              NPFMC and NOAA Fisheries to assist entry level and small vessel fishermen acquire funding. The
                              loan program is capitalized through a portion of the cost recovery fees collected.")))),
                  bslib::nav_panel("Performance Metrics", value = "sablefish_metrics",
                     sablefish_ui("sablefish")
           )
  ),
  #### cgoa rockfish -----
  bslib::nav_menu(title = "CGOA Rockfish", value = "cgoa_rock",
                  bslib::nav_panel("About", value = "cgoa_rock_about",
                                   bslib::layout_columns(
                       col_widths = c(4, 8),
                       bslib::card(fill = FALSE, fillable = FALSE, class = "border-0 shadow-none",
                            includeHTML(path = "./www/cgoa_rock_page_new.html")),
                       bslib::card(fill = FALSE, fillable = FALSE,
                            tags$div(style="background-color:#2C3544 !important; color: white; padding-top: 7px; padding-bottom: 7px; padding-left: 7px; font-size: 20px",
                                     tags$header(strong("MANAGEMENT CONTEXT"))),
                            p("The Central Gulf of Alaska Rockfish Program (Rockfish Program) was implemented in 2012
                                          is a ten year extension of a pilot program that ran from 2007-2011 under similar regulations.
                                          In 2021, the Rockfish Program was reauthorized without an expiration date. Prior to 2007, the fishery 
                                          operated under the License Limitation Program (LLP). The Rockfish
                                          Program is a cooperative program that allocates exclusive harvesting privileges to catcher vessel
                                          (CV) and catcher/processor (CP) vessel cooperatives using trawl gear for primary and
                                          secondary rockfish species as well as an allocation for halibut prohibited species catch (PSC). The primary rockfish
                                           species are northern rockfish, Pacific Ocean perch, and pelagic shelf (dusky) rockfish. The
                                          secondary rockfish  species are Pacific cod, rougheye rockfish, shortraker rockfish, sablefish, and
                                          thornyhead rockfish. The rockfish program also includes a small entry level longline fishery, but
                                          vessels participating in the entry level longline fishery are not eligible to join cooperatives, are not
                                          allocated exclusive harvest privileges, and therefore do not hold quota share."),
                            p("The Rockfish Program was designed to improve resource conservation and improve economic efficiency
                                          by establishing cooperatives that receive exclusive harvest privileges. The four goals of the program
                                          are to 1) reduce bycatch and discards; 2) encourage conservation-minded practices; 3) improve
                                          product quality and value; and 4) provide stability to the processing labor force. The Rockfish
                                          Program allows CPs to form cooperatives and allows CVs to form cooperatives in association with
                                          shoreside processors in Kodiak, AK. These CVs are not required to deliver to the processor with
                                          which their cooperative has formed an association but the processor must be located in Kodiak, AK. This allows shoreside processors in Kodiak to
                                          better time deliveries of rockfish and salmon in the summer months."),
                            p("The Rockfish Program includes a cost recovery provision whereby the fishermen are assessed a fee
                                          based on the cost to the government to manage the program. The costs that can be recovered
                                          include the costs related to management, data collection, and enforcement of a Limited Access
                                          Privilege Program (LAPP) or Community Development Quota Program, and cannot exceed 3% of
                                          the total ex-vessel value of the fishery. Cost recovery was not part of the Rockfish Pilot Program
                                          (2007-2011), but it was implemented in 2012 with the implementation of the Rockfish Program.
                                          Cost recovery fees are assessed for harvests of Rockfish Program primary and secondary species by
                                          participants using trawl gear. Cost recovery fees are not assessed for harvests of Rockfish Program
                                          species by participants in the limited entry longline fishery because they do not receive an exclusive
                                          harvest privilege. In 2020, 2023, and 2024, the Rockfish Program fee reached the 3% statutory cap."),
                            tags$div(style="background-color:#2C3544 !important; color: white; padding-top: 7px; padding-bottom: 7px; padding-left: 7px; font-size: 20px",
                                     tags$header(strong("CATCH SHARE PRIVILEGE CHARACTERISTICS"))),
                            p("Rockfish Program quota share (QS) are allocated to eligible LLP license holders, but that LLP
                                          license must be assigned to a Rockfish Program cooperative in order to participate in the Rockfish
                                          Program. Cooperative quota (CQ) for Rockfish Program primary species, secondary species, and
                                          halibut PSC is allocated annually to each cooperative based on the QS holdings of its membership.
                                          Quota share for Rockfish Program primary species were allocated to eligible LLP license holders
                                          based on their catch history of those species, so the LLP owners have a limited ability to sell their
                                          QS, which can be transferred only by selling their LLP license on which the Rockfish Program QS is
                                          designated. Cooperatives within a sector can transfer CQ within and between cooperatives, subject
                                          to excessive share limits. Catcher vessel cooperatives cannot transfer CQ to CP cooperatives, but
                                          CP cooperatives are allowed to transfer CQ to cooperatives in either sector (with the exception of
                                          rougheye or shortraker rockfish CQ)."),
                            p("The Rockfish Program allocated revocable shares, initially  authorized until
                                          December 31st, 2021 (10 years from the start of the program), but have been reauthorized to indefinite. The Rockfish Program includes
                                          excessive share provisions, which include the following: No person may hold or use more than 4% of
                                          the CV QS and resulting CQ, or 40% of the CP QS and resulting CQ; No vessel may harvest more than 8%
                                          of the CV CQ or 60% of the CP CQ; and no processor may receive or process more than 40% of the
                                          CV CQ.")))),
                  bslib::nav_panel("Performance Metrics", value = "cgoa_rock_metrics",
                     cgoa_rockfish_ui("cgoa_rockfish")
           )
  ),
  ### non catch share home-------
  bslib::nav_menu(title = "Non-Catch Share Metrics", value = "non_catch",
                  bslib::nav_panel("About", value = "non_catch_about",
                     tags$div(class = "background",
                              div(class = "title-container",
                                  "Performance Metrics for North Pacific Non-Catch Share Programs"),
                              div(class = "image-row",
                                  tags$img(src = "goa_othr_rock.png", 
                                           style = "cursor:pointer;",
                                           onclick = "Shiny.setInputValue('goa_othr_img_clicked', true);"),
                                  tags$img(src = "scallop_pic.png", 
                                           style = "cursor:pointer;",
                                           onclick = "Shiny.setInputValue('scallop_img_clicked', true);"),
                                  tags$img(src = "longliners_pic.png", 
                                           style = "cursor:pointer;",
                                           onclick = "Shiny.setInputValue('cgoa_img_clicked', true);"),
                              )),
                     div(class = "wrap-section",
                         tags$img(src = "non_cs_fishing.jpg", class = "wrap-image-left"),
                         div(class = "wrap-header-left", h4("ABOUT NON-CATCH SHARE PROGRAMS")),
                         p("Catch share programs are a fishery management tool that allocates a secure
                           share of the fishery resource to individual fishermen, fishing cooperatives,
                           fishing communities, or other entities to harvest a fixed quantity of fish 
                           each year. Catch shares do not directly impact the total allowable catch (TAC)
                           of each species, and are merely a mechanism to allocate the TAC across various
                           individuals and user groups. The North Pacific region has been the most active
                           region in the U.S. in developing catch share programs, and contains 7 of the 17 
                           programs currently in operation throughout the U.S. This dashboard provides data on 
                           indicators of fishery performance for three selected fisheries not managed on the basis
                           of catch shares, including trends over time."),
                     ),
                     div(class = "wrap-section",
                         tags$img(src = "non_cs_boat2.jpg", class = "wrap-image-right"),
                         div(class = "wrap-header-right", h4("CATCH SHARE PERFORMANCE METRICS")),
                         p("This section develops a consistent set of indicators to assess various 
                           dimensions of the economic performance of three non-catch share programsin 
                           the Alaska region including Central Gulf of Alaska Rockfish Program, 
                           Alaska Weathervane Scallop Program, as well as one quasi-catch share program, 
                           the Bering Sea Freezer Longline Catcher/Processors. These indicators were developed
                           by NOAA Fisheries’ regional economists, anthropologists, and sociologists as the most
                           representative indicators of economic performance for which data are available and can
                           be regularly updated and were first summarized in",
                           tags$a(href = "https://repository.library.noaa.gov/view/noaa/4601",
                                  class = "custom-link",
                                  target = "_blank",
                                  "Brinson and Thunberg (2013)."),
                           "These indicators can be broken down into three general categories: 
                           catch and landings, effort, and revenue, and their descriptions are listed in the tables below. "),
                         tags$a(
                           href = "https://www.fisheries.noaa.gov/alaska/sustainable-fisheries/catch-share-and-limited-access-programs-alaska",  # Replace with your target link
                           target = "_blank",         
                           class = "custom-btn",  
                           icon("circle-info"),           
                           "Click here to learn more about these catch share programs"  ,
                         )
                     ),
                     br(),
                     fluidRow(
                       bslib::accordion(title = "Indicators", 
                                 bslib::accordion_panel(title = "Catch & Landings",  #### catch and landing table ------
                                                        bslib::card(
                                                          full_screen = TRUE,
                                                          bslib::card_body(
                                                            tags$div(tags$fieldset(style= "width: 40%;","The catch and landings metrics are the annual catch limit (ACL) or quota level, aggregate landings,
                                                                the % of the quota that was utilized, as well as whether the ACL or quota was exceeded for any species in the program. While the quota amount is set based on the biological
                                                                condition of the species in the program, the landings and the percentage of the quota that is landed (% utilization) reflect economic conditions and regulatory constraints of the fishery"),
                                                                     tags$fieldset(style= "width: 50%; float: right;",
                                                                                   tags$table(class="styled-table",
                                                                                              tags$thead(
                                                                                                tags$tr(
                                                                                                  tags$th('Indicator'),
                                                                                                  tags$th('Definition')
                                                                                                )
                                                                                              ),
                                                                                              tags$tbody(
                                                                                                tags$tr(
                                                                                                  tags$td('Non-Catch Share Quota)'),
                                                                                                  tags$td('Annual quota of combined non-catch share program species, in terms of weight.')
                                                                                                ),
                                                                                                tags$tr(
                                                                                                  tags$td('Aggregate landings'),
                                                                                                  tags$td('Annual total weight of all species in the fishery landed on trips attributed to the fishery.')
                                                                                                ),
                                                                                                tags$tr(
                                                                                                  tags$td('Annual Catch Limit (ACL) exceeded '),
                                                                                                  tags$td('Was the ACL exceeded for any species/stock within the non-catch share fishery? (Y/N)')
                                                                                                ),
                                                                                                tags$tr(
                                                                                                  tags$td('Utilization (%)'),
                                                                                                  tags$td('Portion of target species TAC that is caught ad retained within a fishing year. Landings/Quota attributed to the non-catch share fishery.')
                                                                                                )
                                                                                              )
                                                                                   )
                                                                     )
                                                            )
                                                          ))),
                                 bslib::accordion_panel("Fishing Effort",  #### fishing effort table ------
                                                        bslib::card(
                                                          full_screen = TRUE,
                                                          bslib::card_body(
                                                            tags$div(tags$fieldset(style= "width: 40%;","The effort metrics are the season length index, the number of active vessels, and the number of entities 
                                                               holding share, trips, and days at sea. The season length index is defined as the number of days in which at least one vessel was fishing divided
                                                               by the number of days in the regulatory fishing season. This index provides a single, unit-less metric of season length 
                                                               that can be aggregated over multiple areas or species with different season lengths within the same program. The index 
                                                               measures the relative proportion of the legal fishing season during which some or all vessels actively fished. The 
                                                               aggregate program level season length index is calculated as the weighted harmonic mean number of days in which at 
                                                               least one vessel was fishing by area using catch volume as weights and then divided by the regulatory fishing season 
                                                               length. The number of active vessels is one indicator of the scale of participation and effort in the fishery and can 
                                                               indicate changes in the expansion or consolidation of vessels in the fishery after rationalization. The number of 
                                                               entities holding share reflects the number of quota share owners that may be reduced as a result of consolidation or 
                                                               increase with new entrants over time and indicates the level of ownership accumulation in the fishery"),
                                                                     tags$fieldset(style= "width: 50%; float: right;",
                                                                                   tags$table(class="styled-table",
                                                                                              tags$thead(
                                                                                                tags$tr(
                                                                                                  tags$th('Indicator'),
                                                                                                  tags$th('Definition')
                                                                                                )
                                                                                              ),
                                                                                              tags$tbody(
                                                                                                tags$tr(
                                                                                                  tags$td('Season length'),
                                                                                                  tags$td('Number of days the fishery is open in a given year.')
                                                                                                ),
                                                                                                tags$tr(
                                                                                                  tags$td('Active vessels'),
                                                                                                  tags$td('Annual number of vessels that fish quota and landing one or more pounds of any catch share program species.')
                                                                                                ),
                                                                                                tags$tr(
                                                                                                  tags$td('Number of permits'),
                                                                                                  tags$td('Number of uniquely permitted vessels for the fishery at a given point in a year.')
                                                                                                ),
                                                                                                tags$tr(
                                                                                                  tags$td('Trips'),
                                                                                                  tags$td('Number of trips attributed to the fishery in a given year')
                                                                                                ),
                                                                                                tags$tr(
                                                                                                  tags$td('Days at sea'),
                                                                                                  tags$td('Number of days absent on trips attributed to the fishery in a given year.')
                                                                                                ),
                                                                                                tags$tr(
                                                                                                  tags$td('Limited entry'),
                                                                                                  tags$td('Is the non-catch share fishery under a limited entry program? (Y/N)')
                                                                                                ),
                                                                                                tags$tr(
                                                                                                  tags$td('Limited entry components'),
                                                                                                  tags$td('List the components of the fishery that are under a limited entry program.')
                                                                                                )
                                                                                              )
                                                                                   )
                                                                     )
                                                            )
                                                          ))),
                                 bslib::accordion_panel(title = "Landing Revenue", #### landing revenue table ------
                                                        bslib::card(
                                                          full_screen = TRUE,
                                                          bslib::card_body( 
                                                            tags$div(tags$fieldset(style= "width: 40%;","The revenue metrics are the aggregate revenue from catch share species, average prices of non-catch share species, 
                                                             the revenue per active vessel, trip, and day at sea, and the Gini coefficient. Revenues are a function of landings and prices, which may 
                                                             trend in opposite directions due to changes in the demand for the species that may or may not be caused by the 
                                                             movement to catch share management. Prices may be affected by catch share management, but they are also influenced
                                                             by external market factors such as price and availability of substitute products, fluctuating exchange rates, and 
                                                             changes in demand. While changes in prices cannot be solely attributed to catch shares, they provide a useful metric
                                                             to compare the performance of the fishery over time in terms of improving quality and marketability. The Gini 
                                                             coefficient is a measure of the evenness of the distribution of revenue among the active vessels, which increases 
                                                             as revenues become more concentrated on fewer vessels, and is useful to examine the distributional impacts of catch
                                                             share programs across vessels"),
                                                                     tags$fieldset(style= "width: 50%; float: right;",
                                                                                   tags$table(class="styled-table",
                                                                                              tags$thead(
                                                                                                tags$tr(
                                                                                                  tags$th('Indicator'),
                                                                                                  tags$th('Definition')
                                                                                                )
                                                                                              ),
                                                                                              tags$tbody(
                                                                                                tags$tr(
                                                                                                  tags$td('Fishery species revenue'),
                                                                                                  tags$td('Aggregate ex-vessel revenue from species in the fishery landed on trips attributed to the fishery in a given year.')
                                                                                                ),
                                                                                                tags$tr(
                                                                                                  tags$td('Other species revenue'),
                                                                                                  tags$td('Aggregate ex-vessel revenue from species not in the fishery landed on trips attributed to the fishery in a given year.')
                                                                                                ),
                                                                                                tags$tr(
                                                                                                  tags$td('Average price'),
                                                                                                  tags$td('Aggregate revenue from species in the fishery landed on trips attributed to the fishery, divided by aggregate landings')
                                                                                                ),
                                                                                                tags$tr(
                                                                                                  tags$td('Revenue per active vessel'),
                                                                                                  tags$td('Aggregate revenue divided by active vessels')
                                                                                                ),
                                                                                                tags$tr(
                                                                                                  tags$td('Revenue per trip'),
                                                                                                  tags$td('Aggregate revenue divided by trip')
                                                                                                ),
                                                                                                tags$tr(
                                                                                                  tags$td('Revenue per day at sea'),
                                                                                                  tags$td('Aggregate revenue divided by day at sea')
                                                                                                ),
                                                                                                tags$tr(
                                                                                                  tags$td('Gini Coefficient'),
                                                                                                  tags$td('A measure of the evenness of the distribution of revenue among the 
                                                                                active vessels. The Gini coefficient increases as
                                                                                revenues become more concentrated on fewer vessels.')
                                                                                                )
                                                                                              )
                                                                                   )
                                                                     )
                                                            )
                                                          ))),
                                 bslib::accordion_panel("Other", #### other table ------
                                                        bslib::card(
                                                          full_screen = TRUE,
                                                          bslib::card_body(  
                                                            tags$div(tags$fieldset(style= "width: 40%;","Depending on the program, other metrics are calculated that may be beneficial when determining the current status of the non-catch share fisheries.  
                                                                                 The other metrics and their descriptions can be found here."),
                                                                     tags$fieldset(style= "width: 50%; float: right;",
                                                                                   tags$table(class="styled-table",
                                                                                              tags$thead(
                                                                                                tags$tr(
                                                                                                  tags$th('Indicator'),
                                                                                                  tags$th('Definition')
                                                                                                )
                                                                                              ),
                                                                                              tags$tbody(
                                                                                                tags$tr(
                                                                                                  tags$td('Cost recovery fee'),
                                                                                                  tags$td('Amount collected for cost recovery.')
                                                                                                ),
                                                                                                tags$tr(
                                                                                                  tags$td('Share cap in place (Y/N) '),
                                                                                                  tags$td('An ownership share and/or allocation cap is any measure consistent with the MSA LAPP purpose
                                                                                                    and intent whether or not the catch share program is required to have an excessive share cap. Y/N')
                                                                                                ),
                                                                                                
                                                                                              )
                                                                                   )
                                                                     )
                                                            )
                                                          ))))),
                     br(),
                     column(width=12,
                            tags$div(class = 'wrap-section',
                                     tags$img(src = "non_cs_fish.jpg",  class="wrap-image-left"),
                                     tags$div(class = 'wrap-header-left',
                                              h4(paste0("PERCENTAGE CHANGE FOR ", current_yr-1," TO ",  current_yr))),
                                     bslib::card(  class = "border-0 shadow-none", tableOutput("non_cs_percent_tbl")),
                            )),
                     br()
           ),
           ### non catch share tabs-------
           #### scallops -------
           bslib::nav_panel("Scallops", value = "scallop_metrics",
                     scallops_ui("scallops")
           ),
           #### goa other rockfish  ----------
           bslib::nav_panel("GOA Other Rockfish", value = "goa_rock_metrics",
                     rockfish_ui("rockfish")
           ),
           #### bsai longliners ------
           bslib::nav_panel("BSAI Longliners", value = "flbsai_metrics",
                     flbsai_pcod_ui("flbsai_pcod")
           )
  ),
  ## data resources ----
  nav_panel(title = "Resources",
            tags$div(class = "resource-page",
                     div(class = "title-container",
                         "Data Resources")),
            tags$ul(
              tags$li(
                tags$a(href = "https://www.adfg.alaska.gov/index.cfm?adfg=fishlicense.fishtickets",
                       class = "custom-link",
                       target = "_blank",         
                       "Alaska Department of Fish & Game, Alaska Commercial Fishery Entry Commission. Alaska Gross Earnings and Fish Ticket File. Data compiled by AKFIN in Comprehensive_FT.")
              ),
              tags$li(
                tags$a(href = "https://www.adfg.alaska.gov/index.cfm?adfg=fishlicense.coar",
                       class = "custom-link",
                       target = "_blank",  
                       "Alaska Department of Fish & Game. Commercial Operators Annual Report. Data compiled by AKFIN in Comprehensive_ENCOAR_BUY and Comprehensive_ENCOAR_PROD."
                )
                ),
                tags$li(
                  tags$a(href = "https://repository.library.noaa.gov/view/noaa/4601",
                         class = "custom-link",
                         target = "_blank", 
                         "Brinson, A. A., & Thunberg, E.M. (2013). The economic performance of U.S. catch share programs (NOAA Technical Memorandum NMFS‑F/SPO 133a) [PDF].
                          U.S. Department of Commerce, National Marine Fisheries Service. Retrieved from NOAA Institutional Repository."  )
                  ),
                  tags$li(
                    tags$a(href = "https://www.fisheries.noaa.gov/inport/item/21987",
                           class = "custom-link",
                           target = "_blank",  
                "NOAA Fisheries Alaska Region. At-Sea Groundfish Production Reports. Data compiled by AKFIN in Comprehensive_WPR."
                )
                ),
              tags$li(
                tags$a(href = "https://www.fisheries.noaa.gov/alaska/commercial-fishing/cost-recovery-programs-fee-collection-and-fee-payment-alaska",
                       class = "custom-link",
                       target = "_blank", 
                "NOAA Fisheries Alaska Region. Cost recovery data. Data provided by AKFIN."
                )
              ),
              tags$li(
                tags$a(href = "https://www.fisheries.noaa.gov/alaska/sustainable-fisheries/alaska-catch-accounting-system",
                       class = "custom-link",
                       target = "_blank", 
                "NOAA Fisheries Alaska Region. Groundfish Catch Accounting System. Data compiled by AKFIN in Comprehensive_BLEND_CA.")
              ),
              tags$li(
                tags$a(href = "https://www.fisheries.noaa.gov/permit/alaska-ifq-halibut-sablefish-and-cdq-halibut-program-fishery-applications-and-reporting",
                       class = "custom-link",
                       target = "_blank",  
                "NOAA Fisheries Alaska Region. IFQ Accounting System. Data provided by AKFIN."
                )
              ),
              tags$li(
                tags$a(href = "https://www.fisheries.noaa.gov/alaska/commercial-fishing/permits-and-licenses-issued-alaska.",
                       class = "custom-link",
                       target = "_blank", 
                "NOAA Fisheries Alaska Region. Permits and Licenses Issued in Alaska. Data provided by AKFIN and additionally accessed at https://www.fisheries.noaa.gov/alaska/commercial-fishing/permits-and-licenses-issued-alaska."
                )
              ),
              tags$li(
                tags$a(href = " https://alaskafisheries.noaa.gov/status-of-fisheries/",
                       class = "custom-link",
                       target = "_blank", 
                "NOAA Fisheries Alaska Region. Status of Fisheries 2013-Present."
                )
              ),
              tags$li(
                tags$a(href = "https://meetings.npfmc.org/CommentReview/DownloadFile?p=9c98c730-537c-4367-9bad-9c982662ceed.pdf&fileName=Scallop%20SAFE%202024%20(REVISED).pdf",
                       class = "custom-link",
                       target = "_blank", 
                "North Pacific Fishery Management Council. (2024). Scallop Stock Assessment and Fishery Evaluation (SAFE) report (Revised)."),
            )
            ))
  
)


# Server --------------------------------------------------------------------------------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  observeEvent(input$afa_img_clicked, {
    updateTabsetPanel(session, "tabs", selected = "afa_about")
  })
  observeEvent(input$a80_img_clicked, {
    updateTabsetPanel(session, "tabs", selected = "a80_about")
  })
  observeEvent(input$hal_img_clicked, {
    updateTabsetPanel(session, "tabs", selected = "halibut_about")
  })
  observeEvent(input$crab_img_clicked, {
    updateTabsetPanel(session, "tabs", selected = "crab_about")
  })
  observeEvent(input$sab_img_clicked, {
    updateTabsetPanel(session, "tabs", selected = "sablefish_about")
  })
  observeEvent(input$cgoa_img_clicked, {
    updateTabsetPanel(session, "tabs", selected = "cgoa_rock_about")
  })
  observeEvent(input$goa_othr_img_clicked, {
    updateTabsetPanel(session, "tabs", selected = "goa_rock_metrics")
  })
  observeEvent(input$scallop_img_clicked, {
    updateTabsetPanel(session, "tabs", selected = "scallop_metrics")
  })
  observeEvent(input$longliners_img_clicked, {
    updateTabsetPanel(session, "tabs", selected = "flbsai_metrics")
  })
  
  ## catch share server items ----------------------------
  ### catch share percent data frame -------
  cs_pct_tbl <- reactive({
    rbind(data$afa_pct, data$a80_pct, data$hal_pct, data$cgoa_rock_pct,  data$crab_pct) %>% 
      rename("Catch Share Program" = species) %>%  
      pivot_longer(., -c("Catch Share Program")) %>% 
      pivot_wider(., names_from = c("name"), values_from = "value") %>% 
      mutate(across(c(8:10), ~ as.character(.))) %>% 
      full_join(., data$sab_pct) %>% 
      mutate(`Catch Share Program` = str_replace_all(`Catch Share Program`, "_", " "))
      
   
  })
  
  ### catch share percent table -------
  
  output$cs_percent_tbl <- function() {
    cs_pct_tbl() %>% 
      kbl("html", align = "c", booktabs = T, escape=FALSE) %>%
      kable_styling(bootstrap_options = c("hover", "responsive"),  full_width = F, position = "float_right") %>%
      row_spec(0, bold = TRUE, background = "#2E5C61", color = "white") %>%
      column_spec(1, width = "5cm", bold = TRUE) %>%
      column_spec(2:7, width = "5cm") %>%
      column_spec(8:10, width = "7cm")
    #  footnote(symbol = "Color scheme is reversed to indicate that increases in the Gini reflect increases in inequality of revenues across vessels.")
    
  }
 
  
  ### A80 -------------------------------------------------------------------------------------------------
  
  a80_server("a80",data$a80_joined, data$a80_acl_plot,
             data$a80_ent_plot,data$a80_utilz_plot, data$a80_eff_plot,
             data$a80_rev_plot, data$a80_gini_plot, data$a80_rev_per_plot)
  
  
  
  ### Halibut ------------------------------------------------------------------------------------------

  halibut_server("halibut", data$hal_joined, data$hal_acl_plot,
                 data$hal_ent_plot, data$hal_utilz_plot, data$hal_eff_plot,
                 data$hal_rev_plot, data$hal_gini_plot, data$hal_rev_per_plot)

  ### Crab -------------------------------------------------------------------------------------------------

  crab_server("crab", data$crab_joined, data$crab_acl_plot,
              data$crab_ent_plot, data$crab_utilz_plot, data$crab_eff_plot,
              data$crab_rev_plot, data$crab_gini_plot, data$crab_rev_per_plot)

  ### AFA ---------------------------------------------------------------------------------------------------

  afa_server("afa", data$afa_joined, data$afa_acl_plot_All, data$afa_acl_plot_CP,  data$afa_acl_plot_CV,
             data$afa_ent_plot, data$afa_utilz_plot, data$afa_eff_plot, data$afa_rev_plot_All, 
             data$afa_rev_plot_CP, data$afa_rev_plot_CV, data$afa_gini_plot_All, data$afa_gini_plot_CP,
             data$afa_gini_plot_CV, data$afa_rev_per_plot)


  ### Sablefish ---------------------------------------------------------------------------------------------

  sablefish_server("sablefish", data$sab_joined, data$sab_plot_df, data$sab_acl_plot_All, data$sab_acl_plot_CP,  
                   data$sab_acl_plot_CV, data$sab_ent_plot, data$sab_utilz_plot,
                   data$sab_eff_plot, data$sab_rev_plot_All, data$sab_rev_plot_CP, data$sab_rev_plot_CV,
                   data$sab_gini_plot_All, data$sab_gini_plot_CP, data$sab_gini_plot_CV,
                   data$sab_rev_per_plot)


  ### CGOA Rockfish ------------------------------------------------------------------------------------------

  cgoa_rockfish_server("cgoa_rockfish",data$cgoa_rock_joined, data$cgoa_rock_acl_plot_All, data$cgoa_rock_acl_plot_CP,
                       data$cgoa_rock_acl_plot_CV, data$cgoa_rock_ent_plot, data$cgoa_rock_utilz_plot,
                       data$cgoa_rock_eff_plot, data$cgoa_rock_rev_plot_All, data$cgoa_rock_rev_plot_CP,
                       data$cgoa_rock_rev_plot_CV, data$cgoa_rock_gini_plot_All, data$cgoa_rock_gini_plot_CP, 
                       data$cgoa_rock_gini_plot_CV,data$cgoa_rock_rev_per_plot)

  ## non-catch share server items --------------------------------------------------

  ### Non-catch share percent change table-----------------------------------------------------------------------
  non_cs_pct_tbl <- reactive({
    rbind(data$scallop_pct, data$flbsai_pct, data$rock_pct) %>% 
      mutate(species = str_replace_all(species, "_", " ")) %>%
      rename("Non-Catch Share Program" = species) %>%
      mutate(across(everything(), ~ as.character(.)) ) %>% 
      pivot_longer(., -c("Non-Catch Share Program")) %>%
      pivot_wider(., names_from = "Non-Catch Share Program", values_from = "value") %>%
      rename(" " = name) %>% 
      mutate(across(everything(), ~ifelse(is.na(.), "-", .)))
    
  })

  #### non-catch share percent table
  output$non_cs_percent_tbl <- function() {
    non_cs_pct_tbl() %>%
      kbl("html", align = "c", booktabs = T, escape=FALSE) %>%
      kable_styling(bootstrap_options = c("hover", "responsive"),  full_width = F, position = "float_right") %>%
      row_spec(0, bold = TRUE, background = "#2E5C61", color = "white") %>%
      column_spec(1, width = "3cm", bold = TRUE) %>%
      column_spec(2:4, width = "2cm") %>%
      footnote(symbol = "Most recent year wholesale revenue data not available.")

  }

  ### Scallops ------------------------------------------------------------------------------------------

  scallop_server("scallops", data$scallop_joined,data$scallop_plot_df, data$scallop_acl_plot,
                 data$scallop_ent_plot, data$scallop_utilz_plot, data$scallop_eff_plot,
                 data$scallop_rev_plot, data$scallop_gini_plot, data$scallop_rev_per_plot)


  ### Rockfish ------------------------------------------------------------------------------------------

  rockfish_server("rockfish", data$rock_joined, data$rock_plot_df, data$rock_acl_plot,
                  data$rock_ent_plot, data$rock_utilz_plot, data$rock_eff_plot, data$rock_rev_plot,
                  data$rock_gini_plot, data$rock_rev_per_plot)


  ### FL BSAI Pacific Cod ------------------------------------------------------------------------------------------

  flbsai_pcod_server("flbsai_pcod", data$flbsai_joined, data$flbsai_plot_df, data$flbsai_acl_plot,
                     data$flbsai_ent_plot, data$flbsai_utilz_plot, data$flbsai_eff_plot, 
                     data$flbsai_rev_plot, data$flbsai_gini_plot, data$flbsai_rev_per_plot)

 }

## Run the application --------------------------------------------------------------------------------------------------------------------------------------------------------
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
