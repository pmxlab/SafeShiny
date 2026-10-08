#' Safe version of shiny::reactive() with an error-observability hook
#'
#' @description
#' Wraps \code{shiny::reactive()} so a genuine error raised inside \code{expr} is caught, passed
#' to \code{onError()}, and then \strong{re-raised unchanged}. Like \code{\link{SafeRender}} - and
#' unlike \code{\link{SafeObserve}} - this does not swallow the error: Shiny already degrades
#' gracefully for whatever consumes a failing \code{reactive()} (e.g. an output's own localized
#' error display), and that still happens exactly as before. \code{SafeReactive()} is purely an
#' additive hook, for (1) rolling a computation failure into application-level status
#' bookkeeping and (2) \code{trackTime} coverage of the computation layer, where most real
#' compute time typically lives.
#'
#' Normal reactive caching is unaffected: once the expression has thrown, re-accessing the
#' reactive before its next invalidation re-raises the same cached error without recomputing,
#' and therefore without firing \code{onError()} again. \code{shiny::req()}/
#' \code{shiny::validate()}'s intentional silent-stop condition is re-raised unchanged and never
#' reaches \code{onError()}.
#'
#' @inheritParams SafeRender
#' @param expr the reactive expression, exactly as for \code{shiny::reactive()}.
#' @param label optional character string identifying this reactive for timing/error-logging
#'   purposes, also passed on as the reactive's own label. Defaults to a short, truncated deparse
#'   of \code{expr} for SafeShiny's purposes (and to Shiny's own default for the reactive).
#' @param domain passed to \code{shiny::reactive()}; the reactive domain, by default the current one.
#'
#' @return a reactive expression, exactly as returned by \code{shiny::reactive()}.
#'
#' @examples
#' \dontrun{
#' library(shiny)
#' library(SafeShiny)
#'
#' server <- function(input, output, session) {
#'   result <- SafeReactive({
#'     expensiveFit(input$n)
#'   }, onError = function(e) message("fit failed: ", conditionMessage(e)), trackTime = TRUE)
#'   output$summary <- renderPrint(result())
#' }
#' }
#'
#' @importFrom shiny reactive getDefaultReactiveDomain
#' @export
SafeReactive <- function(expr, onError = NULL, trackTime = FALSE, label = NULL, quiet = FALSE,
                          env = parent.frame(), quoted = FALSE,
                          domain = shiny::getDefaultReactiveDomain()) {
  if (!quoted) {
    expr <- substitute(expr)
  }
  wrapped <- .safeShinyBuildWrappedExpr(expr, onError, trackTime, label, quiet, "SafeReactive")
  if (is.null(label)) {
    shiny::reactive(wrapped, env = env, quoted = TRUE, domain = domain)
  } else {
    shiny::reactive(wrapped, env = env, quoted = TRUE, label = label, domain = domain)
  }
}
