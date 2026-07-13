#' Launch the conindex Shiny GUI
#'
#' Opens a point-and-click interface to [conindex()]: upload a CSV (or use a data
#' frame already in your session), pick the outcome, ranking, weight and cluster
#' variables, choose an index, and view the estimate. This is the R analogue of a
#' Stata dialog (the original package ships no `.dlg`, so this GUI is provided as
#' a convenience).
#'
#' @param data Optional data frame to preload. If `NULL`, the app offers a CSV
#'   upload control.
#' @param ... Passed to [shiny::runApp()].
#' @return Invisibly `NULL`; called for its side effect (runs the app).
#' @examples
#' if (interactive()) {
#'   run_conindex_app(mtcars)
#' }
#' @export
run_conindex_app <- function(data = NULL, ...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("The 'shiny' package is required for the GUI. Install it with install.packages('shiny').",
         call. = FALSE)
  }
  shiny::runApp(.conindex_app(data), ...)
  invisible(NULL)
}

#' @keywords internal
#' @noRd
.conindex_app <- function(preload = NULL) {
  index_choices <- c(
    "Concentration index / Gini (truezero)" = "ci",
    "Generalized"                            = "generalized",
    "Modified (limits = min)"                = "modified",
    "Wagstaff (bounded)"                     = "wagstaff",
    "Erreygers (bounded)"                    = "erreygers",
    "Extended (v)"                           = "extended",
    "Symmetric (beta)"                       = "symmetric"
  )

  ui <- shiny::fluidPage(
    shiny::titlePanel("conindex - concentration indices"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        if (is.null(preload)) shiny::fileInput("file", "Upload CSV", accept = ".csv"),
        shiny::uiOutput("var_ui"),
        shiny::selectInput("index", "Index", choices = index_choices),
        shiny::conditionalPanel(
          "input.index == 'extended'",
          shiny::numericInput("v", "v (> 1)", value = 1.5, min = 1.0001, step = 0.5)
        ),
        shiny::conditionalPanel(
          "input.index == 'symmetric'",
          shiny::numericInput("beta", "beta (> 1)", value = 1.5, min = 1.0001, step = 0.5)
        ),
        shiny::conditionalPanel(
          "input.index == 'modified' || input.index == 'wagstaff' || input.index == 'erreygers'",
          shiny::numericInput("lmin", "limit min", value = 0),
          shiny::conditionalPanel(
            "input.index == 'wagstaff' || input.index == 'erreygers'",
            shiny::numericInput("lmax", "limit max", value = 1)
          )
        ),
        shiny::checkboxInput("robust", "Robust SE", FALSE),
        shiny::actionButton("go", "Estimate", class = "btn-primary")
      ),
      shiny::mainPanel(
        shiny::verbatimTextOutput("result"),
        shiny::tableOutput("head")
      )
    )
  )

  server <- function(input, output, session) {
    get_data <- shiny::reactive({
      if (!is.null(preload)) return(preload)
      shiny::req(input$file)
      utils::read.csv(input$file$datapath)
    })

    output$var_ui <- shiny::renderUI({
      df <- get_data()
      nums <- names(df)[vapply(df, is.numeric, logical(1))]
      shiny::tagList(
        shiny::selectInput("outcome", "Outcome (h)", choices = nums),
        shiny::selectInput("rankvar", "Rank variable", choices = c("(outcome)" = "", nums)),
        shiny::selectInput("weights", "Weights", choices = c("(none)" = "", nums)),
        shiny::selectInput("cluster", "Cluster", choices = c("(none)" = "", nums))
      )
    })

    output$head <- shiny::renderTable(utils::head(get_data()))

    res <- shiny::eventReactive(input$go, {
      df <- get_data()
      args <- list(
        data = df, outcome = input$outcome,
        rankvar = if (nzchar(input$rankvar)) input$rankvar else NULL,
        weights = if (nzchar(input$weights)) input$weights else NULL,
        cluster = if (nzchar(input$cluster)) input$cluster else NULL,
        robust = isTRUE(input$robust)
      )
      idx <- input$index
      if (idx == "ci") args$truezero <- TRUE
      if (idx == "generalized") { args$generalized <- TRUE; args$truezero <- TRUE }
      if (idx == "modified") args$limits <- input$lmin
      if (idx == "wagstaff") { args$wagstaff <- TRUE; args$bounded <- TRUE; args$limits <- c(input$lmin, input$lmax) }
      if (idx == "erreygers") { args$erreygers <- TRUE; args$bounded <- TRUE; args$limits <- c(input$lmin, input$lmax) }
      if (idx == "extended") { args$v <- input$v; args$truezero <- TRUE }
      if (idx == "symmetric") { args$beta <- input$beta; args$truezero <- TRUE }
      tryCatch(do.call(conindex, args), error = function(e) conditionMessage(e))
    })

    output$result <- shiny::renderPrint({
      r <- res()
      if (is.character(r)) cat("Error:", r) else print(r)
    })
  }

  shiny::shinyApp(ui, server)
}
