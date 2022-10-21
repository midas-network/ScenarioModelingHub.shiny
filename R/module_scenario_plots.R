##################################################
#' Function returns a set of scenario checkboxes
#' with a inputId = `id', and given specifics about the
#' round (`r`) and scenario number (`s`)
#'
#' @importFrom shiny checkboxGroupInput HTML
#' @importFrom stringr str_extract
#'
#' @noRd
#' @export
#'
scen_checkbox_UI <- function(id, r, s, show) {

  scen_name = scen_info[rnd_num==r & scen_num == s]$scenario_name
  scen_val = scen_info[rnd_num==r & scen_num == s]$scenario_id
  select = scen_val
  if (show == FALSE) {
     select = FALSE
  }

  chxbox = checkboxGroupInput(
    inputId = id,
    label = NULL,
    choiceNames = list(
      HTML('<div style="font-size:14px;"><b>Scenario ',
           stringr::str_extract(scen_name, "(?<=\\()."),
           "</b></br>",
           gsub(" \\(","</br>(",scen_name) %>%
             gsub(", | \\+ ", "</br>", .),
           "</div>"
           )
      ),
    choiceValues = scen_val,
    selected = select
    )

  return(chxbox)
}

##################################################
#' Function returns a set of scenario radio buttons
#' with a inputId = `id`, and given specifics about the
#' round (`r`)
#'
#' @importFrom shiny radioButtons HTML
#'
#' @noRd
#' @export
#'
scen_radiobutton_UI <- function(id, r) {

  scen_names = scen_info[rnd_num==r]$scenario_name
  scen_vals = scen_info[rnd_num==r]$scenario_id

  rdiobttns = radioButtons(
    inputId = id,
    label = NULL,
    choiceNames = lapply(scen_names, function(x) {
      HTML('<div style="font-size:14px;">', x, '</div>')
      }),
    choiceValues = scen_vals,
    selected = scen_vals[1],
    width="100%"
  )

  return(rdiobttns)
}


##################################################
#' Function returns a set of target/outcome radio
#' buttons, given a round (`r`)
#'
#' @importFrom shiny radioButtons
#'
#' @noRd
#' @export
#'
target_radiobutton_UI <- function(id) {
  radioButtons(
    inputId = id,
    label = HTML('<h4 style="color:#606060"><strong>Target:</strong></h4>
                 <h5 style="color:#606060">
                 Each model projects different targets</h5>'),
    choiceNames = get_target_choiceNames(),
    choiceValues = get_target_choiceValues()
  )
}

##################################################
#' Function returns a set of target/outcome radio
#' checkboxes, given a round (`r`)
#'
#' @importFrom shiny checkboxGroupInput
#'
#' @noRd
#' @export
#'
target_checkbox_UI <- function(id) {

  checkboxGroupInput(
    inputId = id,
    label = HTML('<h4 style="color:#606060"><strong>Target:</strong></h4>
                 <h5 style="color:#606060">
                 Each model projects different targets</h5>'),
    choiceNames = get_target_choiceNames(),
    choiceValues = get_target_choiceValues(),
    selected = get_target_choiceValues()[1]
  )
}

#' This version of the default ensemble function is based on pre-selected
#' List of round numbers and specific default ensembles
#'
#'
#' @noRd
#' @export
#'
get_default_ensemble_fixed <- function(r) {
  return(
    unique(round_info[rnd_num == r, ens_default])
  )
}

#' This function makes the target user input (i.e. outcomes), choice Names
#'
#' @importFrom purrr map
#' @importFrom stringr str_starts
#' @noRd
#' @export
#'
get_target_choiceNames <- function(target_type = c("Incident|Cumulative",
                                                   "Incident", "Cumulative")) {

  target_names = names(getOption("gs_targets"))
  target_type = match.arg(target_type)

  purrr::map(
    paste0(
      '<div style="font-size:14px;">',
      target_names[str_starts(target_names,target_type)],
      "</div>"),
    HTML)
}

#' This function makes the target user input (i.e. outcomes), choice Values
#'
#' @importFrom stringr str_starts
#' @noRd
#' @export
#'
get_target_choiceValues <- function(target_type = c("Incident|Cumulative",
                                                    "Incident", "Cumulative")) {
  target_names = names(getOption("gs_targets"))
  target_type = match.arg(target_type)
  target_names[str_starts(target_names,target_type)]
}

#'
#' @importFrom shiny NS tagList fluidRow br column h2 div h3 strong HTML hr h4
#' @importFrom shiny selectInput helpText radioButtons
#' @importFrom shinyjs hidden
#' @import data.table
#' @importFrom stringr str_extract
#' @importFrom purrr map
#' @noRd
#' @export
#
round_scenario_plots_row_UI <- function(id) {

  # set the namespace
  ns = NS(id)

  # get round number
  r = as.integer(str_extract(id,"\\d+"))

  # get the unique model names
  #models = unique(model_data$model_name)

  # get the default ensemble for this round
  default_ensemble = unique(round_info[rnd_num == r, ens_default])

  # get the scenario descriptors for this round
  round_scenario_desc <- rnd_desc[[r]]

  # values for showing scenarios
  showA = unlist(scen_info[rnd_num == r & grepl("A-", scenario_id), "show"])
  showB = unlist(scen_info[rnd_num == r & grepl("B-", scenario_id), "show"])
  showC = unlist(scen_info[rnd_num == r & grepl("C-", scenario_id), "show"])
  showD = unlist(scen_info[rnd_num == r & grepl("D-", scenario_id), "show"])

  # Now we make the tagList of UI elements for this round
  tagList(
      fluidRow(
        br(),
        # This is the "sidebar".. Not a formal shiny sidebar, but a column on
        # the left that contains the user inputs for this round
        column(3,
           h2("Model Projection", style = "color:#2d5973"),
           div("New scenario for models are defined in each round",
               style='font-size:18px;'),
           h3(strong(paste0("Round ",r)), style = "color:#2d5973"),
           HTML(paste0('<div style="font-size:14px;">', round_scenario_desc,
                       '</div>')),
           br(),
           # Add scenario checkboxes and radio buttons (originally hidden)
           column(6, scen_checkbox_UI(ns("scen_sel1"),r,1,showA),
                  scen_checkbox_UI(ns("scen_sel3"),r,3,showC)),
           column(6, scen_checkbox_UI(ns("scen_sel2"),r,2,showB),
                  scen_checkbox_UI(ns("scen_sel4"),r,4,showD)),
           column(12,hidden(scen_radiobutton_UI(ns("scen_radio"), r)),
                  style='padding:0px;'),
           # separate with horizontal line
           hr(style = "border:1px solid ; border-color: #bfbfbf;
              margin-left:15px;margin-right:45px;margin-top:200px"),
           # Add location dropdown, with US initially selected
           selectInput(ns("location"), h4(strong("Location:"),
                                          style = "color:#606060"),
                       choices = model_location,
                       selected = "US"),
           helpText(HTML('<div style="font-size:14px;">Special regions
                         (American Samoa, Guam, Northern Marianas Island,
                         Virgin Islands) not included</div>')),
           # Add target radiobuttons and checkbox group (originally hidden)
           target_radiobutton_UI(id = ns("target")),
           hidden(target_checkbox_UI(id=ns("target_chkboxes"))),
           # Add uncertainty interval radio buttons
           radioButtons(ns("pi"), label = h4(strong("Uncertainty Interval:"),
                                             style = "color:#606060"),
                        choiceNames = purrr::map(
                          paste0('<div style="font-size:14px;">',
                                 c("None", "50 %", "95 %", "multi"), "</div>"),
                          HTML),
                        choiceValues = list(0, 50, 95, -1),
                        selected = -1),
           hidden(
             div(id = ns("multidesc"), "'multi' displays 95%, 90%, 80%, and 50%
                 uncertainty intervals, shaded from lightest (95%) to darkest
                 (50%)")
           ),
           style = 'margin-left:30px;margin-bottom:30px;border-right: 2px solid;
            border-right-color: #bfbfbf;'
           ),
        # This next column is all the individual plot tabs for this round
        if (unlist(unique(round_info[rnd_num == r, "print_rnd"])) == "TRUE") {
          if (exists("multipat_disclaimer")) {
            disclaimer <- multipat_disclaimer
          } else {
            disclaimer <- NULL
          }
          column(8,generate_tabsetPanel(r,ns,default_ensemble, disclaimer))
        } else {
          column(8, HTML(unlist(unique(round_info[rnd_num == r, "print_rnd"]))))
        }
      ),
      hr(style = "border:1px solid ; border-color: #bfbfbf;"),
      round_summary_notes(ns, r)
  )

}


#' Functions for round summary deifnitions
#'
#' @importFrom shiny includeHTML column
#' @noRd
#' @export
get_definitions <- function(ns, rnd_num) {

  defs <- function(rnd) {
    definition_list[[unlist(unique(round_info[rnd_num == rnd, "definition"]))]]
  }
  link_image <- dir("../code/www/", paste0("round", rnd_num,"(_.+)?\\.html"),
                    full.names = TRUE)
  if (all(file.exists(link_image))) {
    if (length(link_image) == 2) {

      multipat_tabpanel <- function(ns,rnd_num) {
        pat_tab <-
          list(tabPanel(
            paste0(getOption("pathogen"), " - Scenario"),
            includeHTML(grep("_", link_image, invert = TRUE,
                            value = TRUE))))
        # add id and selected
        pat_tab$id = ns("pat_tab")
        pat_tab$selected=paste0(getOption("pathogen"), " - Scenario")
        do.call(tabsetPanel, pat_tab)
      }

      this_row <- fluidRow(
        column(3, defs(rnd_num)),
        column(9, multipat_tabpanel(ns, rnd_num)),
        br()
      )

    } else {
      this_row <- fluidRow(
        column(3, defs(rnd_num)),
        column(9, includeHTML(link_image)),
        br()
      )
    }
  } else {
    this_row <- fluidRow(
      column(12, defs(rnd_num)),
      br()
    )
  }
  return(this_row)
}

#' Functions for round summary notes
#'
#' @importFrom shiny fluidRow column HTML helpText br
#' @noRd
#' @export
get_notes <- function(rnd_num) {
  round <- rnd_num
  round <- unlist(unique(round_info[rnd_num == round, "notes"]))
  this_row <- fluidRow(
    column(1),
    column(10,
           HTML('<h2 style="color:#2d5973">Notes:</h2>'),
           column(6,
                  helpText(HTML(note_info[[round]][["first_col"]])),
                  style='border-right: 2px solid;border-right-color: #bfbfbf;
                    padding-right:25px'),
           column(5, helpText(HTML(note_info[[round]][["second_col"]])),
                  style='margin-left:50px;'),
           style='margin-top:50px;margin-bottom:50px;border: 2px solid;
            border-color:#bfbfbf;padding-bottom:25px;'),
    br()
    )
  return(this_row)
}


#' Functions for round summary notes
#'
#' @noRd
#' @export
round_summary_notes <- function(ns, rnd_num) {

  defs = get_definitions(ns, rnd_num)
  notes = get_notes(rnd_num)

  return(list("defs"=defs, "notes"=notes))
}

##############################################################
## SERVER SIDE LOGIC

# dynamic server modules
#'
#' @importFrom shiny moduleServer reactive observeEvent updateCheckboxInput
#' @importFrom shiny updateSelectizeInput bindCache
#' @importFrom plotly renderPlotly
#' @importFrom shinyjs click
#' @importFrom stringr str_extract
#' @noRd
#' @export
scenario_plots_server <- function(id, tab_data=NULL) {

  moduleServer(
    id,
    function(input, output, session) {

      # Let's make a note of which plot tab we are on!
      current_plot_tab <- reactive(input$plot_tab)

      # get round number
      r = as.integer(str_extract(id,"\\d+"))

      # get default ensemble
      def_ens <- unique(round_info[rnd_num == r, ens_default])
      ens_exc <- unique(round_info[rnd_num == r, ens_excl])

      # When this plot tab changes (we observe it here ...)
      # we want to hide/show certain UI elements.. Since
      # this update is complicated, we push to function:
      # update_sidebar_inputs()
      observeEvent(
        current_plot_tab(),{
          update_sidebar_inputs(
            pt = current_plot_tab(),
            model_names = grep(ens_exc, unique(
              tab_data()$model_data$model_name), value = TRUE,
              invert = TRUE),
            default_ensemble = def_ens,
            input = input,
            session = session,
            r = r)
          # update the ensemble checkbox
          if (unique(round_info[rnd_num == r, n_ens]) > 1) {
            updateCheckboxInput(session, "ensemble_chkbox_spec", value = FALSE)
            updateCheckboxInput(session, "ensemble_chkbox_trend", value = FALSE)
          }
          if (current_plot_tab() == "MultiPathogen Plot") {
            appendTab("pat_tab",
                      tabPanel(paste0(
                        str_extract(grep("_",
                                         dir("../code/www/",
                                             paste0("round", r,"(_.+)?\\.html"),
                                             full.names = TRUE), value = TRUE),
                                    "(?<=_).*(?=.html)"), " - Scenario"),
                        includeHTML(grep("_",
                                        dir("../code/www/",
                                            paste0("round", r,"(_.+)?\\.html"),
                                            full.names = TRUE), value = TRUE)))

                      )
          } else {
            removeTab("pat_tab", paste0(
              str_extract(grep("_", dir("../code/www/",
                                        paste0("round", r,"(_.+)?\\.html"),
                                        full.names = TRUE), value = TRUE),
                          "(?<=_).*(?=.html)"), " - Scenario"))
          }
          # update multi-pathogen plot, if necessary
          if (isTRUE(unlist(round_info[rnd_num == r, "multipat_plot"]))) {
            updateRadioButtons(
              session, "other_scen",
              label = paste(str_to_title(unique(
                tab_data()$multi_data$other$pathogen)), "Round",
                unique(str_extract(other_round[unique(
                  tab_data()$multi_data$other$scenario_id)], "[[:digit:]]+")),
                'Scenario Selection'),
              choiceValues = unique(tab_data()$multi_data$other$scenario_id),
              choiceNames = paste(other_info[unique(
                tab_data()$multi_data$other$scenario_id)], " (", unique(
                  tab_data()$multi_data$other$scenario_id), ")", sep = ""),
              selected = unique(tab_data()$multi_data$other$scenario_id)[1],
              inline = FALSE)
            updateSelectInput(
              session, "other_quant",
              label = paste(str_to_title(unique(
                tab_data()$multi_data$other$pathogen)), 'Quantile'))
          }
          # update the dropdown for Trend map
          updateSelectizeInput(session, "trend_model_spec",
                               choices=grep(ens_exc, unique(
            tab_data()$model_data$model_name), value = TRUE,
            invert = TRUE), selected=def_ens)
          # trigger a click to update value
          shinyjs::click("trend_update_button")
       })

      show_ens_spec <- reactive(input$ensemble_chkbox_spec)

      if (unlist(unique(round_info[rnd_num == r, "print_rnd"])) == "TRUE") {
        observeEvent(
          show_ens_spec(), {
            update_sidebar_inputs(
              pt = current_plot_tab(),
              model_names = if (isFALSE(show_ens_spec())) grep(ens_exc, unique(
                tab_data()$model_data$model_name), value = TRUE,
                invert = TRUE) else unique(tab_data()$model_data$model_name),
              default_ensemble = def_ens,
              input = input,
              session = session,
              r = r)
          })
        }

      show_ens_trend <- reactive(input$ensemble_chkbox_trend)

      if (unlist(unique(round_info[rnd_num == r, "print_rnd"])) == "TRUE") {
        observeEvent(
          show_ens_trend(), {
            update_sidebar_inputs(
              pt = current_plot_tab(),
              model_names = if (isFALSE(show_ens_trend())) grep(ens_exc, unique(
                tab_data()$model_data$model_name), value = TRUE,
                invert = TRUE) else unique(tab_data()$model_data$model_name),
              default_ensemble = def_ens,
              input = input,
              session = session,
              r = r)
          })
      }

      ta <- reactive(input$target)
      lo <- reactive(input$location)
      ss <- reactive({
        c(input$scen_sel1, input$scen_sel2, input$scen_sel3, input$scen_sel4)

      })

      # get the radio button scenario selection
      scen_radio = reactive(input$scen_radio)

      # get the target checkbox selection(s)
      ta_chk = reactive(input$target_chkboxes)

      # Get the ensemble checkbox selection
      show_ens <- reactive(input$ensemble_chkbox)
      ens_chk = reactive(input$ensemble_chkbox_dist)

      # get the uncertainty level input
      pi <- reactive(input$pi)

      # get the scale type for state deviation
      scale_type_dev = reactive(input$scale_type_dev)

      # get the week choice input selector
      wk_dist = reactive(input$wk_choice_dist)

      # get the scen comparison start week selector
      sc_start_wk = reactive(input$sc_start_wk)

      model_spec_target_types = reactive(input$targ_model_spec)
      model_dist_target_types = reactive(input$targ_model_dist)
      model_model_name = reactive(input$model_model_spec)

      trend_model_name = reactive(input$trend_model_spec)
      trend_model_extent = reactive(input$trend_model_extent)

      # if peak
      if (isTRUE(round_info[rnd_num ==r, peak_mod])) {
        peak_model_name = reactive(input$peak_model_spec)
      }

      # if multi comparison
      if (isTRUE(round_info[rnd_num ==r, multi_comp])) {
        sc_selcomp = reactive(input$sc_selcomp)
      } else {
        sc_selcomp = reactive(NULL)
      }

      # if multi-pathogen plot
      if (isTRUE(unlist(round_info[rnd_num == r, "multipat_plot"]))) {
        ss2 <- reactive(input$other_scen)
        pi1 <- reactive(input$model_quant)
        pi2 <- reactive(input$other_quant)
      }

      # wrap these in eventReactive
      trend_map_choices = eventReactive(input$trend_update_button,{
        list("name" = trend_model_name(),
             "extent" = trend_model_extent())
      },ignoreNULL = FALSE)

      # ########################################################
      # BASIC SCENARIO VISUALIZATION
      # ########################################################

      output$visual_scenario <- renderPlotly({
        server_plot(tab_data()$model_data, ta(), lo(), ss(), pi(), id,
                    show_ens())
      }) %>% bindCache("visscen", ta(), lo(), ss(), pi(), id, show_ens())

      # ########################################################
      # MODEL SPECIFIC VISUALIZATION
      # ########################################################
      output$model_spec <- renderPlotly({
        create_model_specific_plotly(model_data = tab_data()$model_data,
                                     location = lo(),
                                     rtab = id,
                                     target_type = model_spec_target_types(),
                                     model_name = model_model_name(),
                                     pi=pi())

      }) %>% bindCache("modspec", lo(), id, model_spec_target_types(),
                       model_model_name(), pi())

      # ########################################################
      # SCENARIO COMPARISON
      # ########################################################

      output$scen_comparison <- renderPlotly({
        scen_comp_data <- tab_data()$scen_com
        if(!is.null(scen_comp_data)) {
          if(isTRUE(round_info[rnd_num ==r, specific_plot])) {
            create_scenario_comparison_plotly(
              scen_comp_data[week==sc_start_wk()-1], loc_name = lo())
          } else {
            if (isTRUE(round_info[rnd_num ==r, multi_comp])) {
              sel_comp <- unlist(strsplit(sc_selcomp(), " & "))[1]
              sel_comp <- gsub("\\(", "\\\\(", sel_comp) %>%
                gsub("\\)", "\\\\)", .)
              scen_comp_data <- scen_comp_data[unlist(purrr::map(
                scen_comp_data, function(x) any(grepl(sel_comp, unique(
                  unlist(x[,"comparison"]))))))][[1]]
              if (isTRUE(unique(round_info[rnd_num == r, zeroed]))) {
                create_scenario_comparison_plotly(scen_comp_data[week==0],
                                                  loc_name = lo())
              } else {
                create_scenario_comparison_plotly(scen_comp_data,
                                                  loc_name = lo())
              }
            } else {
              if (isTRUE(unique(round_info[rnd_num == r, zeroed]))) {
                create_scenario_comparison_plotly(scen_comp_data[week==0],
                                                  loc_name = lo())
              } else {
                create_scenario_comparison_plotly(scen_comp_data,
                                                  loc_name = lo())
              }
            }
          }
        }
      }) %>% bindCache("comp", sc_start_wk(), lo(),sc_selcomp(), id)

      # ########################################################
      # STATE DEVIATION
      # ########################################################
      #output$stdev_note <- renderUI(scenario_selector_note)
      output$state_deviation <- renderPlotly({

        if(!ta() %in% c("Incident Cases", "Incident Hospitalizations",
                        "Incident Deaths")) {
          return(NULL)
        }

        st_deviation_data <- tab_data()$st_dev
        if(!is.null(st_deviation_data)) {
          create_state_variation_plotly(
            state_variation_data = st_deviation_data,
            scen = scen_radio(),
            scale_type = scale_type_dev(),
            outcome_choice = ta()
          )
        }
      }) %>% bindCache("dev", scale_type_dev(), ta(), scen_radio(), id)

      # ########################################################
      # RISK MAP
      # ########################################################
      #output$risk_map_note <- renderUI(scenario_selector_note)
      output$risk_map <- renderPlotly({

        if(!ta() %in% c("Cumulative Cases", "Cumulative Hospitalizations",
                        "Cumulative Deaths")) {
          return(NULL)
        }

        risk_data <- tab_data()$risk_data
        if(!is.null(risk_data)) {
          if (isTRUE(unique(round_info[rnd_num == r, zeroed]))) {
            create_risk_map_zero_plotly(risk_data,scen = scen_radio(),
                                        outcome_choice = ta())
          } else {
            create_risk_map_plot_plotly(risk_data,scen = scen_radio(),
                                        outcome_choice = ta())
          }

        }
      }) %>% bindCache("risk", scen_radio(), ta(), id)

      # ########################################################
      # QUASI MAP
      # ########################################################
      #output$quasi_map_note <- renderUI(scenario_selector_note)
      output$quasi_map <- renderPlotly({

        if(!ta() %in% c("Incident Cases", "Incident Hospitalizations",
                        "Incident Deaths")) {
          return(NULL)
        }

        quasi_data <- get_quasi_map_data(
          rnd_num = r,
          model_data = tab_data()$model_data,
          model_to_show = trend_map_choices()$name,
          forward = trend_map_choices()$extent
        )

        if(!is.null(quasi_data)) {
          create_quasi_map(quasi_data,scen = scen_radio(),target_outcome = ta())
        }
      }) %>% bindCache("quasi", scen_radio(), ta(), trend_map_choices()$name,
                       trend_map_choices()$extent,id)

      # ########################################################
      # MODEL DISTRIBUTION / BOXPLOTS
      # ########################################################

      output$model_dist <- renderPlotly({

        dist_data <- tab_data()$model_data
        z_data <- tab_data()$zero_data

        if(!is.null(dist_data)) {
          create_model_dist_plotly(model_data = dist_data,
                                   zero_data = z_data,
                                   location=lo(),
                                   wk = as.integer(wk_dist()),
                                   outcome_type = model_dist_target_types(),
                                   scenarios = ss(),
                                   rd_num = r,
                                   ens_chk = ens_chk())
        }
      }) %>% bindCache("dist", model_dist_target_types(), lo(), wk_dist(), id,
                       ss(), ens_chk())

      # ########################################################
      # PROJECTION PEAKS
      # ########################################################

      if (isTRUE(round_info[rnd_num ==r, peak_mod])) {
        output$peak_plot <- renderPlotly({

          peak_data <- get_peak_period_data(rnd=r,
                                            model_data = tab_data()$peak_data)

          if(!is.null(peak_data)) {
              create_peak_plotly(
                peak_data = peak_data,
                scen_sel = ss(),
                model_sel = peak_model_name()
              )
          }
        }) %>% bindCache("peak", ss(), id, peak_model_name())
      } else {
        output$peak_plot <- renderPlotly({

          peak_data <- get_peak_period_data(rnd=r,
                                            model_data = tab_data()$model_data)

          if(!is.null(peak_data)) {
              create_peak_plotly(
                peak_data = peak_data,
                scen_sel = ss(),
                outcomes = ta_chk()
              )
            }
        }) %>% bindCache("peak", ss(), ta_chk(), id)
      }

      # ########################################################
      # PEAK SIZE
      # ########################################################

      output$peak_size <- renderPlotly({

        peak_size_data <- tab_data()$peak_size

        if(!is.null(peak_size_data)) {

          peak_size_data <- get_peak_size_data(
            rnd = r,  model_data = peak_size_dat,
            ensemble = def_ens)

          create_peak_size_plotly(
            peak_size_data = peak_size_data,
            scen_sel = ss(),
            location = lo()
          )
        }
      }) %>% bindCache("peak", ss(), lo(), id)

      #' ########################################################
      #' MULTIDISEASE PLOTS
      #' ########################################################

      output$multipat_plot <- renderPlotly({

        if(!is.null(tab_data()$multi_data)) {
          create_multipat_plotly(tab_data()$multi_data, location = lo(),
                                 scen_sel = ss(), target = ta(),
                                 scen_sel2 = ss2(), pi1 = pi1(), pi2 = pi2(),
                                 rd_num =r
          )
        }
      }) %>% bindCache("multipat", lo(), ss(), ss2(), ta(), pi1(),
                       pi2(), id)

    }
  )

}


