## Server Side Update Function for Side Bar Inputs
#' This function receives the name of the current plot
#' and tries to run some shinyJS to hide/show etc,
#' or modifies the input, etc
#' `pt` is the string name of the plot tab
#' `r` is the round number
#'
#' @noRd
#' @export
update_sidebar_inputs <- function(pt, model_names, default_ensemble, input,
                                  session, r) {

  update_chooser <- list(
    "Scenario Plot" = sidebar_for_scenario_plot,
    "Model Specific Plots" = sidebar_for_model_specific,
    "Scenario Comparison" = sidebar_for_scenario_comparison,
    "State Deviation" = sidebar_for_state_deviation,
    "Trend Maps" = sidebar_for_trend_maps,
    "Risk Maps" = sidebar_for_risk_maps,
    "Model Distribution" = sidebar_for_model_distribution,
    "Projection Peaks" = sidebar_for_peak_plot,
    "Peak Size" = sidebar_for_peak_size_plot,
    "MultiPathogen Plot" = sidebar_for_multidisease_plot
  )
  if(pt %in% names(update_chooser)) {
    update_chooser[[pt]](input=input, session=session, model_names=model_names,
                         default_ensemble=default_ensemble, r = r)
  } else {
    print(paste0("No sidebar update function for this plot tab: ", pt))
  }
}

#'
#' @importFrom shiny updateRadioButtons
#' @importFrom shinyjs hide show
#' @importFrom purrr walk map
#' @noRd
#' @export
#'
sidebar_for_scenario_plot <- function(input, session,r,...) {

  #hide the radio button selector for scenario
  hide("scen_radio")

  #show description of multi if round ==11
  if (isTRUE(round_info[rnd_num == r, multi])) {
    show("multidesc")
  }
  #show the regular scenario selector
  purrr::walk(c("scen_sel1", "scen_sel2", "scen_sel3", "scen_sel4"), show)

  # hide the target checkboxes and show the target radiobuttons instead
  show("target")
  hide("target_chkboxes")

  #enable all inputs
  purrr::walk(c("location", "target", "scen_sel1", "scen_sel2", "scen_sel3",
                "scen_sel4", "pi"), enable)


  # Since target is enabled, make sure all choices are there
  updateRadioButtons(
    session, "target",choiceNames = get_target_choiceNames(),
    choiceValues = get_target_choiceValues()
  )

  # Hide additional ensemble checkbox for rounds before round 5
  # (only 1 ensemble)
  if (round_info[rnd_num == r, n_ens] == 1) hide("ensemble_chkbox")

  # update the choices for model_specific PI intervals
  pi_options = list("names" = c("None", "50 %", "95 %"),
                    "values" = list(0,50,95))

  if (isTRUE(round_info[rnd_num == r, multi])) {
    pi_options[["names"]] = c(pi_options[["names"]], "multi")
    pi_options[["values"]] = list(0,50,95,-1)
  }

  updateRadioButtons(
    session, "pi",
    choiceNames = purrr::map(paste0('<div style="font-size:14px;">',
                                    pi_options[["names"]], "</div>"), HTML),
    choiceValues = pi_options[["values"]],
    selected = ifelse(isTRUE(round_info[rnd_num == r, multi]),-1,95)
    ##Make default multi-mode when round is 11, 95% otherwise.
  )
}

#'
#' @importFrom shiny updateRadioButtons updateSelectizeInput
#' @importFrom shinyjs hide show
#' @importFrom purrr walk map
#' @noRd
#' @export
#'
sidebar_for_model_specific <- function(input, session,model_names,
                                       default_ensemble, r,...) {

  #hide the radio button selector for scenario
  hide("scen_radio")
  hide("multidesc")

  #show the regular scenario selector
  purrr::walk(c("scen_sel1", "scen_sel2", "scen_sel3", "scen_sel4"), show)

  # hide the target checkboxes and show the target radiobuttons instead
  show("target")
  hide("target_chkboxes")

  purrr::walk(c("target", "scen_sel1", "scen_sel2", "scen_sel3", "scen_sel4"),
              disable)

  purrr::walk(c("pi", "location"), enable)

  # Hide additional ensemble checkbox for rounds before round 5
  # (only 1 ensemble)
  if (round_info[rnd_num == r, n_ens] == 1) hide("ensemble_chkbox_spec")

  # update the choices for model_specific PI intervals
  pi_options = list("names" = c("None", "50 %", "95 %"),
                    "values" = list(0,50,95))

  updateRadioButtons(
    session, "pi",
    choiceNames = purrr::map(paste0('<div style="font-size:14px;">',
                                    pi_options[["names"]], "</div>"), HTML),
    choiceValues = pi_options[["values"]],
    selected = 95
  )
  # update the dropdown
  updateSelectizeInput(session, "model_model_spec", choices=model_names,
                       selected=default_ensemble)
}

#'
#' @importFrom shinyjs hide show
#' @importFrom purrr walk
#' @noRd
#' @export
sidebar_for_scenario_comparison <- function(input, session,...) {

  #hide the radio button selector for scenario
  hide("scen_radio")
  #show the regular scenario selector
  purrr::walk(c("scen_sel1", "scen_sel2", "scen_sel3", "scen_sel4"), show)

  # hide the target checkboxes and show the target radiobuttons instead
  show("target")
  hide("target_chkboxes")

  #disable most of the ui elements
  purrr::walk(c("target", "scen_sel1", "scen_sel2", "scen_sel3", "scen_sel4",
                "pi"), disable)
  enable("location")
}

#' @importFrom shiny updateRadioButtons
#' @importFrom shinyjs hide show
#' @importFrom purrr walk
#' @noRd
#' @export
#'
sidebar_for_state_deviation <- function(input, session,...) {

  #hide the radio button selector for scenario
  show("scen_radio")
  #hide the regular scenario selector
  purrr::walk(c("scen_sel1", "scen_sel2", "scen_sel3", "scen_sel4"), hide)

  # hide the target checkboxes and show the target radiobuttons instead
  show("target")
  hide("target_chkboxes")

  # disable the location, prediction intervals
  purrr::walk(c("location", "pi"), disable)
  # enable the rest
  purrr::walk(c("scen_sel1", "scen_sel2", "scen_sel3", "scen_sel4",
                "target"), enable)

  # Limit the outcome choices to Incident only
  updateRadioButtons(
    session, "target",
    choiceNames = get_target_choiceNames("Incident"),
    choiceValues = get_target_choiceValues("Incident")
  )
}


#' @importFrom shiny updateRadioButtons updateSelectizeInput
#' @importFrom shinyjs hide show
#' @importFrom purrr walk map
#' @noRd
#' @export
#'
sidebar_for_trend_maps <- function(input, session,model_names,
                                   default_ensemble, r, ...) {

  #show the radio button selector for scenario
  show("scen_radio")
  #hide the regular scenario selector
  purrr::walk(c("scen_sel1", "scen_sel2", "scen_sel3", "scen_sel4"), hide)

  # hide the target checkboxes and show the target radiobuttons instead
  show("target")
  hide("target_chkboxes")

  # Hide additional ensemble checkbox for rounds before round 5
  # (only 1 ensemble)
  if (round_info[rnd_num == r, n_ens] == 1) hide("ensemble_chkbox_trend")
  # disable the location, prediction intervals
  purrr::walk(c("location", "pi"), disable)
  # enable the rest
  purrr::walk(c("scen_sel1", "scen_sel2", "scen_sel3", "scen_sel4", "target"),
              enable)

  # Limit the outcome choices to Incident only
  updateRadioButtons(
    session, "target",
    choiceNames = get_target_choiceNames("Incident"),
    choiceValues = get_target_choiceValues("Incident")
  )

  # update the dropdown
  updateSelectizeInput(session, "trend_model_spec", choices=model_names,
                       selected=default_ensemble)
}

#' @importFrom shiny updateRadioButtons
#' @importFrom shinyjs hide show
#' @importFrom purrr walk
#' @noRd
#' @export
#'
sidebar_for_risk_maps <- function(input, session,...) {

  #show the radio button selector for scenario
  show("scen_radio")
  #hide the regular scenario selector
  purrr::walk(c("scen_sel1", "scen_sel2", "scen_sel3", "scen_sel4"), hide)

    # hide the target checkboxes and show the target radiobuttons instead
  show("target")
  hide("target_chkboxes")

  # disable the location, prediction intervals
  purrr::walk(c("location", "pi"), disable)
  # enable the rest
  purrr::walk(c("scen_sel1", "scen_sel2", "scen_sel3", "scen_sel4", "target"),
              enable)

  # Limit the outcome choices to Cumulative only
  updateRadioButtons(
    session, "target",
    choiceNames = get_target_choiceNames("Cumulative"),
    choiceValues = get_target_choiceValues("Cumulative")
  )
}


#' @importFrom shinyjs hide show
#' @importFrom purrr walk
#' @noRd
#'
#' @export
sidebar_for_model_distribution <- function(input, session, r, ...) {

  #hide the radio button selector for scenario
  hide("scen_radio")

  #show the regular scenario selector
  purrr::walk(c("scen_sel1", "scen_sel2", "scen_sel3", "scen_sel4"), show)

  # hide the target checkboxes and show the target radiobuttons instead
  show("target")
  hide("target_chkboxes")

  # Hide additional ensemble checkbox for rounds before round 5
  # (only 1 ensemble)
  if (round_info[rnd_num == r, n_ens] == 1) hide("ensemble_chkbox_dist")

  # disable the target, prediction intervals
  purrr::walk(c("target", "pi"), disable)

  # enable the rest
  purrr::walk(c("scen_sel1", "scen_sel2", "scen_sel3", "scen_sel4", "location"),
              enable)
}

#' @importFrom shiny updateCheckboxGroupInput
#' @importFrom shinyjs hide show
#' @importFrom purrr walk map
#' @noRd
#' @export
#'
sidebar_for_peak_plot <- function(input, session,r, model_names,
                                  default_ensemble, ...) {

  #hide the radio button selector for scenario
  hide("scen_radio")

  #show the regular scenario selector
  purrr::walk(c("scen_sel1", "scen_sel2", "scen_sel3", "scen_sel4"), show)

  # hide the target radios and show the target checkboxes instead
  hide("target")
  # disable the prediction intervals and location
  purrr::walk(c("pi","location"), disable)

  # enable the rest
  if (isFALSE(round_info[rnd_num == r, peak_mod])) {
    purrr::walk(c("scen_sel1", "scen_sel2", "scen_sel3", "scen_sel4",
                  "target_chkboxes"), enable)

    # Limit the outcome choices to Incident Only
    updateCheckboxGroupInput(
      session,
      "target_chkboxes",
      choiceNames = get_target_choiceNames("Incident"),
      choiceValues = get_target_choiceValues("Incident"),
      selected=get_target_choiceValues("Incident")[1]
    )
    show("target_chkboxes")
  } else {
    purrr::walk(c("scen_sel1", "scen_sel2", "scen_sel3", "scen_sel4"), enable)
    purrr::walk(c("target"), disable)

    # update the dropdown
    updateSelectizeInput(session, "peak_model_spec", choices=model_names,
                         selected=default_ensemble)
  }

}

#' @importFrom purrr walk map
#' @noRd
#' @export
#'
sidebar_for_peak_size_plot <- function(input, session,r, model_names,
                                       default_ensemble, ...) {

  #hide the radio button selector for scenario
  hide("scen_radio")

  #show the regular scenario selector
  purrr::walk(c("scen_sel1", "scen_sel2", "scen_sel3", "scen_sel4"), show)

  # hide the target checkboxes and show the target radiobuttons instead
  show("target")
  hide("target_chkboxes")

  # disable the target, prediction intervals
  purrr::walk(c("target", "pi"), disable)

  # enable the rest
  purrr::walk(c("scen_sel1", "scen_sel2", "scen_sel3", "scen_sel4", "location"),
              enable)
}

sidebar_for_multidisease_plot  <- function(input, session,r, ...) {

  #hide the radio button selector for scenario
  hide("scen_radio")
  #hide the regular scenario selector
  # purrr::walk(c("scen_sel1", "scen_sel2", "scen_sel3", "scen_sel4"), hide)

  hide("multidesc")

  # hide the target checkboxes and show the target radiobuttons instead
  show("target")
  hide("target_chkboxes")

  purrr::walk(c("pi"), #"scen_sel1", "scen_sel2", "scen_sel3", "scen_sel4"),
              disable)


  updateRadioButtons(
    session, "target",
    choiceNames = get_target_choiceNames("Incident"),
    choiceValues = get_target_choiceValues("Incident"),
    selected =  get_target_choiceValues("Incident")[1]
  )
  purrr::walk(c("target", "location"), enable)

}
