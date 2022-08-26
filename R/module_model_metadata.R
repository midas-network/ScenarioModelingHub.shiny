#'
#'
#' @importFrom shiny NS tabPanel selectInput
#' @importFrom stringr str_extract
#'
#' @noRd
#' @export
model_metadata_UI <- function(id) {

  ns = NS(id)

  r = as.integer(str_extract(id,"\\d+"))

  # get the abstract models
  abstracts = abstract_info(r)[["models"]]

  ui_out =
    tabPanel(
      paste0("Round ", r),
      selectInput(inputId = ns("abstract_model"), label="Model",
                  choices = abstracts),
      uiOutput(outputId=ns("abstract_file"))
    )

  return(ui_out)

}

#'
#' @importFrom shiny moduleServer reactive renderUI includeMarkdown
#' @importFrom stringr str_extract
#'
#' @noRd
#' @export
#'
model_metadata_server <- function(id) {

  moduleServer(
    id,
    function(input, output, session) {

      # get round number
      r = as.integer(str_extract(id,"\\d+"))

      model = reactive(input$abstract_model)

      output$abstract_file <- renderUI({
        if(model() != "") {
          abstract = abstract_info(r, model_name = model())
          file <- paste0(abstract[["path"]],"/",abstract[["files"]])
          includeMarkdown(file)
        }
      })
    }
  )
}

#'
#' @importFrom stringr str_remove_all str_detect
#'
#' @noRd
#' @export
abstract_info <- function(r, model_name = "all") {

  # get the path
  abstract_path = paste0(
    "../visualization/data-visualization/model_abstracts/round", r)
  # get the files
  abstract_files = dir(path=abstract_path,pattern = "*.md")

  # get the models
  model_names = stringr::str_remove_all(
    abstract_files, pattern = "(^\\d{4}-\\d{2}-\\d{2}-)|-(A|a)bstract.md")

  if(model_name!="all") {
    # only return the abstract files with model_name included
    abstract_files = abstract_files[which(
      stringr::str_detect(abstract_files, paste0(model_name,
                                                 "(-A|-a)bstract")))]
  }
  # return as a list
  return(
    list(
      path = abstract_path,
      files = abstract_files,
      models = model_names
    )
  )

}
