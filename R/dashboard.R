#' Was the SafeShiny monitoring tab requested in the URL?
#'
#' Lets an app show the monitoring tab only when it is opened with a query parameter such as
#' \code{?safeshinytab=1}, e.g. \code{if (SafeShinyTabRequested(request)) ...} in a UI function, or
#' \code{if (SafeShinyTabRequested(session)) SafeShinyTabServer("monitor")} in the server.
#'
#' @param x a Shiny request object (as passed to a UI function; its \code{QUERY_STRING} is used),
#'   a Shiny session (its URL query string is read), or a query string such as
#'   \code{"?safeshinytab=1&a=2"}.
#' @param param character string, name of the query parameter (default \code{"safeshinytab"}).
#' @return logical scalar, \code{TRUE} if the parameter is \code{1}, \code{true}, \code{TRUE} or
#'   \code{yes}.
#'
#' @examples
#' SafeShinyTabRequested("?safeshinytab=1")
#' SafeShinyTabRequested("?other=1")
#'
#' @export
SafeShinyTabRequested <- function(x, param = "safeshinytab") {
  qs <- if (is.character(x)) {
    x
  } else if (!is.null(x$clientData)) {
    shiny::isolate(x$clientData$url_search)
  } else {
    x$QUERY_STRING
  }
  if (is.null(qs) || length(qs) != 1 || is.na(qs) || !nzchar(qs)) {
    return(FALSE)
  }
  val <- shiny::parseQueryString(qs)[[param]]
  !is.null(val) && tolower(val) %in% c("1", "true", "yes")
}

#' SafeShiny monitoring tab (UI)
#'
#' A general monitoring dashboard for a Shiny session: a Start/Stop button for execution-time
#' tracking (see \code{\link{StartSafeShinyTracking}}); when tracking is stopped the interactive
#' flame chart (\code{\link{PlotSafeShinyFlameHTML}}), the timing summary and a per-label timing
#' table appear; and an error log of every error caught by the \code{Safe*} wrappers
#' (\code{\link{GetSafeShinyErrors}}). Raw data and the flame chart can be downloaded.
#'
#' Use \code{SafeShinyTabUI()} / \code{SafeShinyTabServer()} as a Shiny module, or
#' \code{SafeShinyTabPanel()} to get a ready-made \code{tabPanel()} for a \code{navbarPage()}.
#' Combine with \code{\link{SafeShinyTabRequested}} to show it only to developers.
#'
#' @param id character string, the module id.
#' @param title character string, title of the tab (default \code{"SafeShiny"}).
#' @param ... further arguments passed to \code{shiny::tabPanel()}.
#' @return \code{SafeShinyTabUI()} an \code{htmltools} tag list; \code{SafeShinyTabPanel()} a
#'   \code{tabPanel}.
#'
#' @examples
#' ui <- shiny::fluidPage(SafeShinyTabUI("monitor"))
#' server <- function(input, output, session) SafeShinyTabServer("monitor")
#' class(ui)
#'
#' @importFrom shiny NS tagList uiOutput tabsetPanel tabPanel downloadButton actionButton
#' @importFrom shiny verbatimTextOutput tableOutput h4 div p
#' @export
SafeShinyTabUI <- function(id) {
  ns <- shiny::NS(id)
  tableOut <- function(outId) {
    if (requireNamespace("DT", quietly = TRUE)) DT::DTOutput(ns(outId)) else shiny::tableOutput(ns(outId))
  }
  shiny::tagList(
    shiny::div(
      style = "padding: 10px 15px;",
      shiny::h4("SafeShiny monitor"),
      shiny::div(
        style = "margin-bottom: 10px;",
        shiny::uiOutput(ns("controls"), inline = TRUE),
        shiny::uiOutput(ns("status"), inline = TRUE)
      ),
      shiny::tabsetPanel(
        shiny::tabPanel(
          "Flame chart",
          shiny::div(style = "margin-top: 10px;",
                     shiny::uiOutput(ns("flame")),
                     shiny::div(style = "margin-top: 8px;",
                                shiny::downloadButton(ns("dlFlame"), "Flame chart (HTML)"),
                                shiny::downloadButton(ns("dlRaw"), "Raw timing (CSV)")))
        ),
        shiny::tabPanel(
          "Timing summary",
          shiny::div(style = "margin-top: 10px;",
                     shiny::verbatimTextOutput(ns("summary")),
                     tableOut("timing"))
        ),
        shiny::tabPanel(
          "Error log",
          shiny::div(style = "margin-top: 10px;",
                     shiny::actionButton(ns("refreshErrors"), "Refresh"),
                     shiny::actionButton(ns("clearErrors"), "Clear"),
                     shiny::div(style = "margin-top: 8px;", tableOut("errors")))
        )
      )
    )
  )
}

#' @rdname SafeShinyTabUI
#' @export
SafeShinyTabPanel <- function(id, title = "SafeShiny", ...) {
  shiny::tabPanel(title, SafeShinyTabUI(id), ...)
}

#' SafeShiny monitoring tab (server)
#'
#' Server side of \code{\link{SafeShinyTabUI}}. Its own outputs and observers are plain Shiny (not
#' \code{Safe*} wrappers), so they never appear in the recorded timings.
#'
#' @param id character string, the module id (same as in \code{SafeShinyTabUI()}).
#' @return nothing - called for its side effects (it registers the module's outputs/observers).
#'
#' @importFrom shiny moduleServer reactiveVal reactiveValues observeEvent renderUI renderPrint
#' @importFrom shiny downloadHandler req
#' @export
SafeShinyTabServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- shiny::reactiveValues(tracking = IsSafeShinyTracking(session), stopped = FALSE,
                                started = NULL, version = 0, errVersion = 0, snap = NULL)

    shiny::observeEvent(input$start, {
      StartSafeShinyTracking(session = session, reset = TRUE)
      rv$tracking <- TRUE
      rv$stopped <- FALSE
      rv$started <- Sys.time()
      rv$snap <- NULL
      rv$version <- rv$version + 1
    })
    shiny::observeEvent(input$stop, {
      StopSafeShinyTracking(session = session)
      rv$tracking <- FALSE
      rv$stopped <- TRUE
      rv$snap <- list(
        summary = SummarizeSafeShinyTiming(session = session),
        timing = GetSafeShinyTiming(session = session),
        raw = GetSafeShinyTimingRaw(session = session)
      )
      rv$version <- rv$version + 1
      rv$errVersion <- rv$errVersion + 1
    })
    shiny::observeEvent(input$refreshErrors, rv$errVersion <- rv$errVersion + 1)
    shiny::observeEvent(input$clearErrors, {
      ResetSafeShinyErrors(session = session)
      rv$errVersion <- rv$errVersion + 1
    })

    output$controls <- shiny::renderUI({
      if (IsSafeShinyTrackingDisabled()) {
        shiny::span(style = "color: #c00;",
                    "Tracking is disabled (options(SafeShiny.trackTime = FALSE)).")
      } else if (isTRUE(rv$tracking)) {
        shiny::actionButton(ns("stop"), "Stop tracking", class = "btn-danger")
      } else {
        shiny::actionButton(ns("start"), "Start tracking", class = "btn-success")
      }
    })
    output$status <- shiny::renderUI({
      if (IsSafeShinyTrackingDisabled()) {
        NULL
      } else if (isTRUE(rv$tracking)) {
        shiny::span(style = "margin-left: 10px; color: #c00;",
                    paste0("Tracking since ", format(rv$started, "%H:%M:%S"),
                           " - interact with the app, then press Stop."))
      } else if (isTRUE(rv$stopped)) {
        shiny::span(style = "margin-left: 10px; color: #666;",
                    paste0("Stopped. ", nrow(rv$snap$raw), " tracked calls recorded."))
      } else {
        shiny::span(style = "margin-left: 10px; color: #666;",
                    "Not tracking. Press Start, interact with the app, then press Stop.")
      }
    })

    output$flame <- shiny::renderUI({
      rv$version
      if (isTRUE(rv$tracking)) {
        return(shiny::p("The flame chart appears when tracking is stopped."))
      }
      if (!isTRUE(rv$stopped)) {
        return(shiny::p("No tracking run yet."))
      }
      w <- PlotSafeShinyFlameHTML(session = session)
      if (is.null(w)) shiny::p("No tracked calls were recorded.") else w
    })

    output$summary <- shiny::renderPrint({
      shiny::req(rv$snap)
      print(rv$snap$summary)
    })

    renderTbl <- function(expr) {
      if (requireNamespace("DT", quietly = TRUE)) {
        DT::renderDT(expr, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
      } else {
        shiny::renderTable(expr, striped = TRUE, digits = 4)
      }
    }
    output$timing <- renderTbl({
      shiny::req(rv$snap)
      t <- rv$snap$timing
      num <- vapply(t, is.numeric, logical(1))
      t[num] <- lapply(t[num], signif, 4)
      t
    })
    output$errors <- renderTbl({
      rv$errVersion
      e <- GetSafeShinyErrors(session = session)
      e$time <- format(e$time, "%H:%M:%S")
      e$call <- NULL  # the condition's call is mostly internal tryCatch plumbing; see GetSafeShinyErrors()
      e[rev(seq_len(nrow(e))), , drop = FALSE]
    })

    output$dlFlame <- shiny::downloadHandler(
      filename = function() "safeshiny_flame.html",
      content = function(file) {
        w <- PlotSafeShinyFlameHTML(session = session)
        shiny::req(w)
        htmltools::save_html(w, file)
      }
    )
    output$dlRaw <- shiny::downloadHandler(
      filename = function() "safeshiny_timing_raw.csv",
      content = function(file) {
        utils::write.csv(GetSafeShinyTimingRaw(session = session), file, row.names = FALSE)
      }
    )
  })
}
