#' Call a render-function generator as if invoked directly from `env`
#'
#' @description
#' Some render-function generators (e.g. \code{DT::renderDT()}) resolve their own \code{...}
#' arguments via \code{parent.frame()} \emph{inside their own body}, rather than via the explicit
#' \code{env} parameter they otherwise honor for the main render expression. A plain
#' \code{renderFunc(wrapped, env = env, quoted = TRUE, ...)} call makes the calling function's own
#' frame - not \code{env} - the apparent direct caller from \code{renderFunc}'s perspective, so a
#' \code{...} argument that's a closure referencing something only defined in \code{env} (e.g. a
#' reactive) silently loses access to it when later re-evaluated. See pmxlab/SafeShiny#1.
#'
#' Fixed by constructing the call and evaluating it \emph{as if written directly in \code{env}}
#' via \code{eval(call, envir = env)} - confirmed this makes a called function's own
#' \code{parent.frame()} resolve to \code{env}, not wherever \code{eval()} itself was invoked
#' from. \code{renderFunc} and \code{wrapped} (already-built language objects) are protected from
#' being re-evaluated as code by binding them to hidden names in a throwaway child environment of
#' \code{env} and referencing them by symbol, rather than embedding them directly in the
#' constructed call. The caller's own \code{...} arguments are recovered via \code{match.call()}
#' as their original, still-unevaluated expressions (not forced to values), so that a reactive
#' closure stays genuinely reactive - re-evaluated by \code{renderFunc} on every render exactly as
#' it would be for a direct, unwrapped call - rather than being frozen at whatever its value was
#' when \code{SafeRender()} itself was first called.
#'
#' @param renderFunc the render-function generator to call.
#' @param wrapped the already-built (via \code{bquote()}) wrapped render expression.
#' @param env the environment the call should appear to originate from.
#' @param dotsCall the calling frame's own \code{match.call(expand.dots = FALSE)}, used to recover
#'   \code{...}'s original unevaluated expressions.
#'
#' @return whatever \code{renderFunc} returns.
#' @keywords internal
.safeShinyCallRenderFuncInEnv <- function(renderFunc, wrapped, env, dotsCall) {
  evalEnv <- new.env(parent = env)
  assign(".safeShiny_renderFunc__", renderFunc, envir = evalEnv)
  assign(".safeShiny_wrapped__", wrapped, envir = evalEnv)
  assign(".safeShiny_env__", env, envir = evalEnv)

  dotsExprs <- as.list(dotsCall)[["..."]]

  callExpr <- as.call(c(
    quote(.safeShiny_renderFunc__),
    quote(.safeShiny_wrapped__),
    list(env = quote(.safeShiny_env__), quoted = TRUE),
    dotsExprs
  ))
  eval(callExpr, envir = evalEnv)
}

#' Build the tryCatch-wrapped render expression shared by all Safe* render wrappers
#' @keywords internal
.safeShinyBuildWrappedExpr <- function(expr, onError, trackTime, label, quiet, context) {
  if (is.null(label)) {
    label <- .safeShinyDefaultLabel(expr)
  }
  domain <- shiny::getDefaultReactiveDomain()
  handlers <- .safeShinyBuildHandlers(
    label = label, domain = domain, onError = onError, trackTime = trackTime, quiet = quiet,
    context = context, reraise = TRUE
  )
  bquote({
    .safeShiny_start_time <- .(handlers$startFn)()
    tryCatch(
      {
        .safeShiny_result <- .(expr)
        .(handlers$recordFn)("ok", .safeShiny_start_time)
        .safeShiny_result
      },
      error = function(.safeShiny_e) {
        .(handlers$errorHandler)(.safeShiny_e, .safeShiny_start_time)
      }
    )
  })
}

#' Generic observability wrapper for any Shiny render function
#'
#' @description
#' Wraps any Shiny-style render-function generator (\code{shiny::renderPlot}, \code{shiny::renderUI},
#' \code{DT::renderDT}, a custom one, ...) so a genuine error raised inside \code{expr} is caught,
#' passed to \code{onError()}, and then \strong{re-raised unchanged}.
#'
#' Unlike \code{\link{SafeObserve}}/\code{\link{SafeObserveEvent}}, this does not change what the
#' user sees: a \code{reactive()}/render body consumed by an output already degrades gracefully
#' on its own (Shiny converts the error into that output's own localized error display, verified
#' empirically - see the "User Guide" vignette). \code{SafeRender()} exists to give the
#' \emph{application} a hook into an error Shiny is already silently containing - e.g. to roll a
#' failed plot/table into some other piece of application state - not to prevent a crash that
#' wasn't going to happen anyway. \code{shiny::req()}/\code{shiny::validate()}'s intentional
#' silent-stop condition is re-raised unchanged either way and never reaches \code{onError()}.
#'
#' \code{\link{SafeRenderPlot}}/\code{\link{SafeRenderUI}}/\code{\link{SafeRenderTable}} are thin
#' convenience wrappers over this for the three most common render functions.
#'
#' @param renderFunc a Shiny-style render-function generator, e.g. \code{shiny::renderPlot} -
#'   any function with the standard \code{function(expr, env, quoted, ...)} signature shiny's own
#'   render functions share.
#' @param expr the render body, exactly as for \code{renderFunc} itself.
#' @param onError optional function called with the caught condition object whenever a genuine
#'   error is caught (not called for a \code{shiny::req()}/\code{validate()} silent stop).
#' @param trackTime logical, default \code{FALSE}. When \code{TRUE}, the wall-clock time spent
#'   evaluating \code{expr} is recorded - see \code{\link{GetSafeShinyTiming}}/
#'   \code{\link{SummarizeSafeShinyTiming}}.
#' @param label optional character string identifying this render for timing/error-logging
#'   purposes. Defaults to a short, truncated deparse of \code{expr} when not supplied.
#' @param quiet logical, default \code{FALSE}. When \code{FALSE}, a caught error is also reported
#'   via \code{message()} (in addition to calling \code{onError}, if supplied).
#' @param env,quoted,... passed through to \code{renderFunc} unchanged.
#'
#' @return whatever \code{renderFunc} returns - typically a function suitable for assigning to
#'   \code{output$x}.
#'
#' @examples
#' \dontrun{
#' library(shiny)
#' library(SafeShiny)
#'
#' server <- function(input, output, session) {
#'   output$plot <- SafeRender(shiny::renderPlot, {
#'     plot(1:input$n)
#'   }, onError = function(e) message("plot failed: ", conditionMessage(e)))
#' }
#' }
#'
#' @importFrom shiny getDefaultReactiveDomain
#' @export
SafeRender <- function(renderFunc, expr, onError = NULL, trackTime = FALSE, label = NULL,
                        quiet = FALSE, env = parent.frame(), quoted = FALSE, ...) {
  # Captured first, and used directly (not forwarded through another "..." layer) - match.call()
  # only reliably recovers "..."'s original expressions one hop away from where they were
  # actually written; forwarding them through a second function call (as SafeRenderPlot() etc.
  # used to do, by calling this SafeRender() with their own "..." in turn) surfaces them as
  # unresolvable ..1-style placeholders instead. See pmxlab/SafeShiny#1.
  dotsCall <- match.call(expand.dots = FALSE)
  if (!quoted) {
    expr <- substitute(expr)
  }
  wrapped <- .safeShinyBuildWrappedExpr(expr, onError, trackTime, label, quiet, "SafeRender")
  .safeShinyCallRenderFuncInEnv(renderFunc, wrapped, env, dotsCall)
}

#' Safe version of shiny::renderPlot() with an error-observability hook
#'
#' @description
#' A thin wrapper over \code{\link{SafeRender}} for \code{shiny::renderPlot}. See
#' \code{\link{SafeRender}} for the full behavior - in short, this does \strong{not} change what
#' the user sees (Shiny already shows its own local error display for a failed plot); it only
#' adds an \code{onError} hook plus optional timing.
#'
#' @inheritParams SafeRender
#' @param expr the plotting code, exactly as for \code{shiny::renderPlot}.
#'
#' @return a function suitable for assigning to \code{output$x}, exactly as for
#'   \code{shiny::renderPlot}.
#'
#' @examples
#' \dontrun{
#' library(shiny)
#' library(SafeShiny)
#'
#' server <- function(input, output, session) {
#'   output$plot <- SafeRenderPlot({
#'     plot(1:input$n)
#'   }, onError = function(e) message("plot failed: ", conditionMessage(e)))
#' }
#' }
#'
#' @importFrom shiny renderPlot
#' @export
SafeRenderPlot <- function(expr, onError = NULL, trackTime = FALSE, label = NULL, quiet = FALSE,
                            env = parent.frame(), quoted = FALSE, ...) {
  dotsCall <- match.call(expand.dots = FALSE)
  if (!quoted) {
    expr <- substitute(expr)
  }
  wrapped <- .safeShinyBuildWrappedExpr(expr, onError, trackTime, label, quiet, "SafeRenderPlot")
  .safeShinyCallRenderFuncInEnv(shiny::renderPlot, wrapped, env, dotsCall)
}

#' Safe version of shiny::renderUI() with an error-observability hook
#'
#' @description
#' A thin wrapper over \code{\link{SafeRender}} for \code{shiny::renderUI}. See
#' \code{\link{SafeRender}} for the full behavior - this does \strong{not} change what the user
#' sees; it only adds an \code{onError} hook plus optional timing.
#'
#' @inheritParams SafeRender
#' @param expr the UI-building code, exactly as for \code{shiny::renderUI}.
#'
#' @return a function suitable for assigning to \code{output$x}, exactly as for
#'   \code{shiny::renderUI}.
#'
#' @examples
#' \dontrun{
#' library(shiny)
#' library(SafeShiny)
#'
#' server <- function(input, output, session) {
#'   output$icon <- SafeRenderUI({
#'     tags$i(class = state$statusIconClass[[id]])
#'   }, onError = function(e) message("icon render failed: ", conditionMessage(e)))
#' }
#' }
#'
#' @importFrom shiny renderUI
#' @export
SafeRenderUI <- function(expr, onError = NULL, trackTime = FALSE, label = NULL, quiet = FALSE,
                          env = parent.frame(), quoted = FALSE, ...) {
  dotsCall <- match.call(expand.dots = FALSE)
  if (!quoted) {
    expr <- substitute(expr)
  }
  wrapped <- .safeShinyBuildWrappedExpr(expr, onError, trackTime, label, quiet, "SafeRenderUI")
  .safeShinyCallRenderFuncInEnv(shiny::renderUI, wrapped, env, dotsCall)
}

#' Safe version of DT::renderDT() with an error-observability hook
#'
#' @description
#' A thin wrapper over \code{\link{SafeRender}} for \code{DT::renderDT()} (the current name for
#' what used to be called \code{DT::renderDataTable()} - they are the same function).
#'
#' \strong{This one does change what the user sees less obviously than you might expect, and it's
#' worth reading before relying on it.} Confirmed empirically (a real \code{DT::renderDT()} body
#' made to throw, driven through a live session via \code{chromote}): the session survives the
#' same way it does for \code{shiny::renderPlot()}/\code{renderText()} - a second, unrelated
#' observer in the same session kept firing normally afterward. But unlike base Shiny render
#' functions, which show a visible \code{"An error has occurred"} box, a failed
#' \code{DT::renderDT()} renders as a \strong{silently blank, hidden widget} - no
#' \code{shiny-output-error} class, no message, nothing indicating anything went wrong. So for DT
#' tables specifically, the \code{onError} hook isn't just an observability nice-to-have on top of
#' a working native error display - it is, practically, the \emph{only} way the user (or the app)
#' finds out the table failed at all. Strongly consider pairing \code{SafeRenderTable()} with an
#' \code{onError} that shows a \code{shiny::showNotification()} or similar, since DT itself won't.
#'
#' @inheritParams SafeRender
#' @param expr the table-building code, exactly as for \code{DT::renderDT}.
#'
#' @return a function suitable for assigning to \code{output$x}, exactly as for
#'   \code{DT::renderDT}.
#'
#' @examples
#' \dontrun{
#' library(shiny)
#' library(DT)
#' library(SafeShiny)
#'
#' server <- function(input, output, session) {
#'   output$table <- SafeRenderTable({
#'     myData()
#'   }, onError = function(e) showNotification("Table failed to render", type = "error"))
#' }
#' }
#'
#' @export
SafeRenderTable <- function(expr, onError = NULL, trackTime = FALSE, label = NULL, quiet = FALSE,
                             env = parent.frame(), quoted = FALSE, ...) {
  dotsCall <- match.call(expand.dots = FALSE)
  if (!quoted) {
    expr <- substitute(expr)
  }
  if (!requireNamespace("DT", quietly = TRUE)) {
    stop("SafeRenderTable() requires the 'DT' package to be installed.")
  }
  wrapped <- .safeShinyBuildWrappedExpr(expr, onError, trackTime, label, quiet, "SafeRenderTable")
  .safeShinyCallRenderFuncInEnv(DT::renderDT, wrapped, env, dotsCall)
}

#' Safe version of shiny::downloadHandler() with an error-observability hook
#'
#' @description
#' Wraps \code{shiny::downloadHandler()} so a genuine error raised inside \code{content} is
#' caught, passed to \code{onError()}, and then \strong{re-raised unchanged} - confirmed
#' empirically that Shiny already turns a \code{content} error into an HTTP 500 for that one
#' download request, with the session fully alive afterward (a second, unrelated observer kept
#' firing normally). \code{SafeDownloadHandler()} does not change that; it only adds an
#' \code{onError} hook plus optional timing on top. \code{shiny::req()}/\code{shiny::validate()}'s
#' intentional silent-stop condition is re-raised unchanged either way and never reaches
#' \code{onError()}.
#'
#' \strong{Limitation - read before relying on this for a multi-item report/workbook}: this only
#' wraps the \emph{outer} \code{content} function as a whole. It gives you one failure hook for
#' "the whole download failed" - it does \emph{not} give per-item resilience inside a loop that
#' builds several pieces of one download (e.g. one sheet per compound in a multi-sheet Excel
#' report). If one iteration of such a loop fails, the whole download still fails - this function
#' alone does not make the other iterations survive. That needs a \code{tryCatch} at each loop
#' iteration \emph{inside} the report-generation code itself, which is specific to that code and
#' out of scope here.
#'
#' @param filename as for \code{shiny::downloadHandler()}: a string, or a function returning one.
#' @param content as for \code{shiny::downloadHandler()}: a function of one argument (the file
#'   path to write to).
#' @param onError optional function called with the caught condition object whenever a genuine
#'   error is caught (not called for a \code{shiny::req()}/\code{validate()} silent stop).
#' @param trackTime logical, default \code{FALSE}. When \code{TRUE}, the wall-clock time spent
#'   running \code{content} is recorded - see \code{\link{GetSafeShinyTiming}}/
#'   \code{\link{SummarizeSafeShinyTiming}}.
#' @param label optional character string identifying this download for timing/error-logging
#'   purposes. Defaults to \code{filename} when it's a plain string, or \code{"download"}
#'   otherwise.
#' @param quiet logical, default \code{FALSE}. When \code{FALSE}, a caught error is also reported
#'   via \code{message()} (in addition to calling \code{onError}, if supplied).
#' @param contentType,outputArgs passed through to \code{shiny::downloadHandler()} unchanged.
#'
#' @return as for \code{shiny::downloadHandler()} - a function suitable for assigning to
#'   \code{output$x}.
#'
#' @examples
#' \dontrun{
#' library(shiny)
#' library(SafeShiny)
#'
#' server <- function(input, output, session) {
#'   output$report <- SafeDownloadHandler(
#'     filename = "report.xlsx",
#'     content = function(file) writeReportWorkbook(file),
#'     onError = function(e) message("report generation failed: ", conditionMessage(e))
#'   )
#' }
#' }
#'
#' @importFrom shiny downloadHandler getDefaultReactiveDomain
#' @export
SafeDownloadHandler <- function(filename, content, onError = NULL, trackTime = FALSE,
                                 label = NULL, quiet = FALSE, contentType = NA,
                                 outputArgs = list()) {
  if (is.null(label)) {
    label <- if (is.character(filename) && length(filename) == 1) filename else "download"
  }

  domain <- shiny::getDefaultReactiveDomain()
  handlers <- .safeShinyBuildHandlers(
    label = label, domain = domain, onError = onError, trackTime = trackTime, quiet = quiet,
    context = "SafeDownloadHandler", reraise = TRUE
  )

  wrappedContent <- function(file) {
    startTime <- handlers$startFn()
    tryCatch(
      {
        result <- content(file)
        handlers$recordFn("ok", startTime)
        result
      },
      error = function(e) {
        handlers$errorHandler(e, startTime)
      }
    )
  }

  shiny::downloadHandler(
    filename = filename, content = wrappedContent, contentType = contentType,
    outputArgs = outputArgs
  )
}
