#'
#' @importFrom shiny NS tabPanel includeMarkdown
#' @importFrom stringr str_extract
#'
#' @noRd
#' @export
#'
scenario_defintions_UI <- function(id) {

  ns = NS(id)

  r = as.integer(str_extract(id,"\\d+"))

  ui_out =
      tabPanel(
      paste0("Round ", r),
      includeMarkdown(paste0(
        "../visualization/data-visualization/scenario_info/scenario_round", r,
        ".md"))
    )

  return(ui_out)

}

