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
#' Combine with \code{\link{SafeShinyTabRequested}} to show it only to developers. While tracking
#' is disabled (\code{\link{IsSafeShinyTrackingDisabled}}) the tab shows a "Tracking is disabled"
#' notice instead of the Start button; to not offer it at all, also require
#' \code{!IsSafeShinyTrackingDisabled()} when building the UI and the server.
#'
#' @param id character string, the module id.
#' @param height character string or number, maximum height of the area below the Start/Stop
#'   button; its content scrolls inside it. A percentage such as \code{"60\%"} is read as a share of
#'   the \emph{viewport} height (a percentage of an auto-height parent would not work); any other CSS
#'   length (\code{"500px"}, \code{"60vh"}) is used as is, and a plain number means pixels. Lower it
#'   if a fixed footer of the app covers the bottom of the tab. Default \code{"60\%"}.
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
#' @importFrom shiny verbatimTextOutput tableOutput h4 div p checkboxInput
#' @export
SafeShinyTabUI <- function(id, height = "60%") {
  ns <- shiny::NS(id)
  h <- .safeShinyCssHeight(height)
  tableOut <- function(outId) {
    if (requireNamespace("DT", quietly = TRUE)) DT::DTOutput(ns(outId)) else shiny::tableOutput(ns(outId))
  }
  # each sub-tab scrolls on its own, so wide/long tables never run off the window
  scroll <- function(...) {
    shiny::div(style = paste0("margin-top: 10px; max-height: ", h, "; overflow: auto;"), ...)
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
          scroll(
            shiny::checkboxInput(
              ns("trim"), "Trim idle time before the first and after the last tracked call",
              value = TRUE),
            shiny::numericInput(
              ns("trimMinTime"), "Trim window starts at the first and ends at the last call lasting at least (s); shorter calls outside it are dropped",
              value = 0, min = 0, step = 0.01, width = "560px"),
            shiny::uiOutput(ns("flame")),
            shiny::div(style = "margin-top: 8px;",
                       shiny::downloadButton(ns("dlFlame"), "Flame chart (HTML)"),
                       shiny::downloadButton(ns("dlRaw"), "Raw timing (CSV)")))
        ),
        shiny::tabPanel(
          "Timing summary",
          scroll(shiny::verbatimTextOutput(ns("summary")), tableOut("timing"))
        ),
        shiny::tabPanel(
          "Error log",
          scroll(shiny::actionButton(ns("refreshErrors"), "Refresh"),
                 shiny::actionButton(ns("clearErrors"), "Clear"),
                 shiny::div(style = "margin-top: 8px;", tableOut("errors")))
        ),
        shiny::tabPanel(
          "Console",
          shiny::div(
            style = "margin-top: 10px;",
            shiny::checkboxInput(
              ns("consoleOn"), paste0("Capture console output (stdout and stderr). Process-wide: shows ",
                                      "all sessions; messages/warnings are not echoed to the server ",
                                      "log while capturing."), value = FALSE),
            shiny::actionButton(ns("consoleRefresh"), "Refresh"),
            shiny::actionButton(ns("consoleClear"), "Clear"),
            shiny::downloadButton(ns("dlConsole"), "Download (.txt)"),
            shiny::checkboxInput(ns("consoleAuto"), "Auto-refresh every 2 s", value = FALSE),
            shiny::div(style = paste0("max-height: calc(", h, " - 130px); overflow: auto;"),
                       shiny::verbatimTextOutput(ns("console")))
          )
        )
      )
    )
  )
}

#' @rdname SafeShinyTabUI
#' @export
SafeShinyTabPanel <- function(id, title = "SafeShiny", height = "60%", ...) {
  shiny::tabPanel(title, SafeShinyTabUI(id, height = height), ...)
}

#' Turn the tab's height argument into a CSS length
#' @param height character string or number, see \code{\link{SafeShinyTabUI}}.
#' @return a CSS length string; percentages become viewport-height units.
#' @keywords internal
.safeShinyCssHeight <- function(height) {
  if (is.numeric(height)) return(paste0(height, "px"))
  if (!is.character(height) || length(height) != 1 || !nzchar(height)) {
    stop("`height` must be a CSS length string such as \"60%\" or \"500px\", or a number of pixels.")
  }
  if (grepl("^[0-9.]+ *%$", height)) paste0(sub(" *%$", "", height), "vh") else height
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

    # trim edges: NA / negative / non-numeric input counts as 0 (every call counts)
    trimMin <- shiny::reactive({
      m <- suppressWarnings(as.numeric(input$trimMinTime))
      if (length(m) != 1 || is.na(m) || m < 0) 0 else m
    })

    output$flame <- shiny::renderUI({
      rv$version
      if (isTRUE(rv$tracking)) {
        return(shiny::p("The flame chart appears when tracking is stopped."))
      }
      if (!isTRUE(rv$stopped)) {
        return(shiny::p("No tracking run yet."))
      }
      w <- PlotSafeShinyFlameHTML(session = session, trim = isTRUE(input$trim), trimMinTime = trimMin())
      if (is.null(w)) shiny::p("No tracked calls were recorded.") else w
    })

    output$summary <- shiny::renderPrint({
      rv$version
      if (is.null(rv$snap)) {
        cat("No finished tracking run.\n")
      } else {
        print(SummarizeSafeShinyTiming(session = session, trim = isTRUE(input$trim), trimMinTime = trimMin()))
      }
    })

    renderTbl <- function(expr) {
      # capture the expression and evaluate it afresh inside the render function; passing `expr`
      # on as a promise would force it once and freeze the table at its first value
      q <- substitute(expr)
      env <- parent.frame()
      fn <- function() eval(q, env)
      if (requireNamespace("DT", quietly = TRUE)) {
        DT::renderDT(fn(), rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
      } else {
        shiny::renderTable(fn(), striped = TRUE, digits = 4)
      }
    }
    output$timing <- renderTbl({
      # an empty table (not req()) so the previous run's rows are really replaced
      t <- if (is.null(rv$snap)) GetSafeShinyTiming(session = NULL)[0, ] else rv$snap$timing
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
        w <- PlotSafeShinyFlameHTML(session = session, trim = isTRUE(input$trim), trimMinTime = trimMin())
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

    # Console capture (opt-in, process-wide; see console.R)
    consoleTick <- shiny::reactiveVal(0)
    # fail soft: an error here must not take the whole session down
    consoleDo <- function(f) {
      tryCatch(f(), error = function(e) {
        shiny::showNotification(paste("Console capture:", conditionMessage(e)), type = "error")
      })
      consoleTick(consoleTick() + 1)
    }
    shiny::observeEvent(input$consoleOn, consoleDo(function() {
      if (isTRUE(input$consoleOn)) .safeShinyConsoleStart(session) else .safeShinyConsoleStop(session)
    }), ignoreInit = TRUE)
    shiny::observeEvent(input$consoleRefresh, consoleDo(function() NULL))
    shiny::observeEvent(input$consoleClear, consoleDo(function() .safeShinyConsoleClear(session)))
    output$console <- shiny::renderText({
      if (isTRUE(input$consoleAuto)) shiny::invalidateLater(2000, session)
      consoleTick()
      lines <- tryCatch(.safeShinyConsoleGet(session), error = function(e) {
        paste("Could not read the console log:", conditionMessage(e))
      })
      if (!isTRUE(input$consoleOn)) {
        "Console capture is off. Tick the box above to start capturing."
      } else if (length(lines) == 0) {
        "(no output yet)"
      } else {
        paste(lines, collapse = "\n")
      }
    })
    output$dlConsole <- shiny::downloadHandler(
      filename = function() "safeshiny_console.txt",
      content = function(file) {
        writeLines(.safeShinyConsoleGet(session, max = Inf, maxBytes = Inf), file, useBytes = TRUE)
      }
    )
  })
}
