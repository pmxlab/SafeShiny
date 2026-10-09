#' Build the shared error/timing handling pieces used by every Safe* function
#'
#' Not exported. \code{SafeObserve()}/\code{SafeObserveEvent()} (swallowing: \code{reraise =
#' FALSE}, their default) and \code{SafeRender()}/\code{SafeDownloadHandler()} (observability-
#' only: \code{reraise = TRUE}) all delegate here so the tryCatch/timing wiring exists in exactly
#' one place.
#'
#' @param label character string, the label to record timing under and to report in error
#'   messages.
#' @param domain a Shiny reactive domain (session object), or \code{NULL}.
#' @param onError optional function called with the caught condition.
#' @param trackTime \code{TRUE}, \code{FALSE} or \code{NA} (auto, see \code{\link{SafeObserve}}):
#'   whether to record execution time via \code{\link{.recordSafeShinyTiming}}. Decided each time the
#'   call runs; never \code{TRUE} while \code{\link{IsSafeShinyTrackingDisabled}()}.
#' @param quiet logical, whether to suppress the default \code{message()} logged when an error
#'   is caught (the error is still caught either way - this only controls the console/log line).
#' @param context character string, used in the default logged message (e.g. \code{"SafeObserve"}).
#' @param reraise logical, default \code{FALSE}. When \code{FALSE} (used by
#'   \code{SafeObserve()}/\code{SafeObserveEvent()}), a genuine caught error is swallowed after
#'   \code{onError()} runs - there is no other consumer to hand it to, so letting it propagate
#'   would still crash the session. When \code{TRUE} (used by \code{SafeRender()}/
#'   \code{SafeDownloadHandler()}), a genuine caught error is re-raised unchanged after
#'   \code{onError()} runs - Shiny already degrades these gracefully on its own (a local error
#'   display, or an HTTP 500 for that one download), so the wrapper is purely an observability
#'   hook layered on top, not a change to what the user sees. A \code{shiny.silent.error}
#'   condition is always re-raised unchanged regardless of \code{reraise}, and never reaches
#'   \code{onError()} - that case isn't a real error either way.
#'
#' @return a list with elements \code{startFn} (function(), returns an opaque token that is
#'   \code{NULL} when the call is not tracked), \code{recordFn} (function(status,
#'   startTime), where \code{startTime} is the token from \code{startFn()}) and
#'   \code{errorHandler} (function(e, startTime)), all to be spliced into the generated
#'   \code{tryCatch} expression via \code{bquote()}.
#'
#' @details
#' \code{errorHandler()} is deliberately a *single* handler covering both the
#' \code{"shiny.silent.error"} case (\code{shiny::req()}/\code{validate()}'s intentional silent
#' stop) and genuine errors, rather than two separate named \code{tryCatch} handlers
#' (\code{shiny.silent.error = ...}, \code{error = ...}). Re-throwing a condition from inside one
#' handler of a \code{tryCatch} call can itself be caught by a *sibling* handler of that very
#' same call - a real, surprising, base R behavior confirmed outside of Shiny entirely (not a
#' Shiny quirk) - so two sibling handlers would have the silent-stop handler's own \code{stop(e)}
#' immediately re-caught by the generic \code{error} handler one line down, defeating the whole
#' point. Checking \code{inherits(e, "shiny.silent.error")} inside one handler and re-throwing
#' from there avoids that self-recapture entirely, since there is no sibling handler left at that
#' same level to catch it.
#' @keywords internal
.safeShinyBuildHandlers <- function(label, domain, onError, trackTime, quiet, context, reraise = FALSE) {
  type <- .safeShinyTypeFromContext(context)
  # options(SafeShiny.trackTime = FALSE) is a master kill switch, resolved once here so every
  # call of this wrapper takes the cheapest possible path.
  staticallyOff <- IsSafeShinyTrackingDisabled()

  startFn <- function() {
    if (staticallyOff || !.safeShinyShouldTrack(trackTime, domain)) {
      return(NULL)
    }
    .startSafeShinyTiming(domain = domain, label = label, type = type)
  }

  recordFn <- function(status, startTime) {
    if (!is.list(startTime)) {
      return(invisible(NULL))
    }
    .endSafeShinyTiming(domain = domain, token = startTime, status = status)
  }

  errorHandler <- function(e, startTime) {
    if (inherits(e, "shiny.silent.error")) {
      recordFn("silent", startTime)
      stop(e)
    }

    recordFn("error", startTime)
    .recordSafeShinyError(domain = domain, label = label, type = type, e = e)
    if (!isTRUE(quiet)) {
      message("[SafeShiny] ", context, " (", label, ") caught an error: ", conditionMessage(e))
    }
    if (is.function(onError)) {
      onError(e)
    }
    if (isTRUE(reraise)) {
      stop(e)
    }
    invisible(NULL)
  }

  list(startFn = startFn, recordFn = recordFn, errorHandler = errorHandler)
}

#' Safe version of shiny::observe() that catches errors instead of crashing the session
#'
#' @description
#' A drop-in replacement for \code{shiny::observe()}: the observer body is evaluated inside a
#' \code{tryCatch()} so that an uncaught error inside it is caught and reported instead of
#' propagating and terminating the whole Shiny session - see the "User Guide" vignette
#' (\code{vignette("SafeShiny", package = "SafeShiny")}) for why this matters for observers (but
#' not for reactives consumed only by render outputs, which Shiny already handles gracefully on
#' its own).
#'
#' \code{shiny::req()}/\code{shiny::validate()}'s intentional silent-stop condition
#' (\code{"shiny.silent.error"}) is always re-raised unchanged, never caught/reported as an
#' error - \code{SafeObserve()} only changes behavior for genuine, unclassed errors.
#'
#' @param x the observer body, exactly as for \code{shiny::observe()}.
#' @param onError optional function called with the caught condition object whenever a genuine
#'   error is caught (not called for a \code{shiny::req()}/\code{validate()} silent stop). Use
#'   for application-specific handling, e.g. recording the failure against a specific piece of
#'   application state, or showing a \code{shiny::showNotification()}.
#' @param trackTime logical, default \code{NA} ("auto"): the call is timed only while tracking is
#'   switched on, i.e. when \code{options(SafeShiny.trackTime = TRUE)} is set or the session's
#'   tracking was started with \code{\link{StartSafeShinyTracking}} - decided each time the call runs.
#'   \code{TRUE}/\code{FALSE} force tracking on/off, except that \code{options(SafeShiny.trackTime = FALSE)}
#'   is a master switch that disables tracking everywhere (see \code{\link{IsSafeShinyTrackingDisabled}}). When tracked, the wall-clock time spent
#'   evaluating \code{x} is recorded (whether it finishes normally, is caught by \code{onError},
#'   or hits a \code{req()}/\code{validate()} silent stop - all three consume real time) - see
#'   \code{\link{GetSafeShinyTiming}}/\code{\link{SummarizeSafeShinyTiming}}.
#' @param label optional character string identifying this observer for timing/error-logging
#'   purposes. Defaults to a short, truncated deparse of \code{x} when not supplied.
#' @param quiet logical, default \code{FALSE}. When \code{FALSE}, a caught error is also reported
#'   via \code{message()} (in addition to calling \code{onError}, if supplied). Set \code{TRUE}
#'   to silence this and rely entirely on \code{onError} for error reporting.
#' @param env,quoted,...,suspended,priority,domain,autoDestroy passed through to
#'   \code{shiny::observe()} unchanged - see \code{\link[shiny]{observe}}.
#'
#' @return the \code{shiny::Observer} object, exactly as for \code{shiny::observe()}.
#'
#' @examples
#' \dontrun{
#' library(shiny)
#' library(SafeShiny)
#'
#' server <- function(input, output, session) {
#'   SafeObserve({
#'     if (input$boom > 0) stop("deliberate error")
#'   }, onError = function(e) showNotification(conditionMessage(e), type = "error"))
#' }
#' }
#'
#' @importFrom shiny observe getDefaultReactiveDomain
#' @export
SafeObserve <- function(x, onError = NULL, trackTime = NA, label = NULL, quiet = FALSE,
                         env = parent.frame(), quoted = FALSE, ...,
                         suspended = FALSE, priority = 0,
                         domain = shiny::getDefaultReactiveDomain(), autoDestroy = TRUE) {
  if (!quoted) {
    x <- substitute(x)
  }
  if (is.null(label)) {
    label <- .safeShinyDefaultLabel(x)
  }

  handlers <- .safeShinyBuildHandlers(
    label = label, domain = domain, onError = onError, trackTime = trackTime, quiet = quiet,
    context = "SafeObserve"
  )

  wrapped <- bquote({
    .safeShiny_start_time <- .(handlers$startFn)()
    tryCatch(
      {
        .safeShiny_result <- .(x)
        .(handlers$recordFn)("ok", .safeShiny_start_time)
        .safeShiny_result
      },
      error = function(.safeShiny_e) {
        .(handlers$errorHandler)(.safeShiny_e, .safeShiny_start_time)
      }
    )
  })

  shiny::observe(
    wrapped, env = env, quoted = TRUE, ...,
    label = label, suspended = suspended, priority = priority,
    domain = domain, autoDestroy = autoDestroy
  )
}

#' Safe version of shiny::observeEvent() that catches errors instead of crashing the session
#'
#' @description
#' Same as \code{\link{SafeObserve}}, but for \code{shiny::observeEvent()}. Both
#' \code{handlerExpr} (the code that runs when the event fires) and \code{eventExpr} (the trigger
#' being watched) are protected. An error raised while evaluating \code{eventExpr} - typically a
#' \code{reactive()} that throws - would otherwise be an unhandled observer error that makes Shiny
#' close the whole session; here it is reported like a handler error (message, error log,
#' \code{onError}) and the observer then stops quietly, without running \code{handlerExpr}. The
#' value of \code{eventExpr} is passed through unchanged, so trigger detection is not affected.
#'
#' When time is tracked, the handler is recorded under \code{label}; the evaluation of
#' \code{eventExpr} is recorded as a separate call \code{"<label> (event)"}, so that tracked
#' reactives evaluated as part of the event appear nested under it. It is recorded only when it ran
#' another tracked call, took at least 1 ms, or ended in an error or a \code{req()} silent stop -
#' plain \code{input$x} events leave no entry.
#'
#' @inheritParams SafeObserve
#' @param eventExpr the expression to watch for changes, exactly as for
#'   \code{shiny::observeEvent()}.
#' @param handlerExpr the code to run when \code{eventExpr} changes - this is the part wrapped
#'   in \code{tryCatch} and, when tracked (see \code{trackTime}), timed.
#' @param event.env,event.quoted,handler.env,handler.quoted,...,ignoreNULL,ignoreInit,once passed
#'   through to \code{shiny::observeEvent()} unchanged - see \code{\link[shiny]{observeEvent}}.
#' @param suspended,priority,domain,autoDestroy passed through to \code{shiny::observeEvent()}
#'   unchanged - see \code{\link[shiny]{observeEvent}}.
#'
#' @return the \code{shiny::Observer} object, exactly as for \code{shiny::observeEvent()}.
#'
#' @examples
#' \dontrun{
#' library(shiny)
#' library(SafeShiny)
#'
#' server <- function(input, output, session) {
#'   SafeObserveEvent(input$boom, {
#'     stop("deliberate error")
#'   }, onError = function(e) showNotification(conditionMessage(e), type = "error"))
#' }
#' }
#'
#' @importFrom shiny observeEvent getDefaultReactiveDomain
#' @export
SafeObserveEvent <- function(eventExpr, handlerExpr, onError = NULL, trackTime = NA,
                              label = NULL, quiet = FALSE,
                              event.env = parent.frame(), event.quoted = FALSE,
                              handler.env = parent.frame(), handler.quoted = FALSE, ...,
                              suspended = FALSE, priority = 0,
                              domain = shiny::getDefaultReactiveDomain(), autoDestroy = TRUE,
                              ignoreNULL = TRUE, ignoreInit = FALSE, once = FALSE) {
  if (!event.quoted) {
    eventExpr <- substitute(eventExpr)
  }
  if (!handler.quoted) {
    handlerExpr <- substitute(handlerExpr)
  }
  if (is.null(label)) {
    label <- .safeShinyDefaultLabel(handlerExpr)
  }

  handlers <- .safeShinyBuildHandlers(
    label = label, domain = domain, onError = onError, trackTime = trackTime, quiet = quiet,
    context = "SafeObserveEvent"
  )

  wrappedHandlerExpr <- bquote({
    .safeShiny_start_time <- .(handlers$startFn)()
    tryCatch(
      {
        .safeShiny_result <- .(handlerExpr)
        .(handlers$recordFn)("ok", .safeShiny_start_time)
        .safeShiny_result
      },
      error = function(.safeShiny_e) {
        .(handlers$errorHandler)(.safeShiny_e, .safeShiny_start_time)
      }
    )
  })

  # The event expression needs the same protection: an error raised while observeEvent() evaluates
  # it (typically a reactive that throws) is otherwise an unhandled observer error, which makes
  # Shiny close the whole session. A genuine error is reported exactly like a handler error; the
  # observer then stops quietly (req(FALSE)) without running the handler. req()/validate() silent
  # stops raised by the event expression are re-raised unchanged by errorHandler().
  # Its time is tracked as a separate call "<label> (event)" so that reactives evaluated as part of
  # the event nest under it; trivial events are not recorded (see .safeShinyEndOrDiscardTiming()).
  eventHandlers <- .safeShinyBuildHandlers(
    label = paste0(label, " (event)"), domain = domain, onError = onError, trackTime = trackTime,
    quiet = quiet, context = "SafeObserveEvent"
  )
  endEventFn <- function(token) .safeShinyEndOrDiscardTiming(domain, token)
  wrappedEventExpr <- bquote({
    .safeShiny_event_start <- .(eventHandlers$startFn)()
    tryCatch(
      {
        .safeShiny_event_value <- .(eventExpr)
        .(endEventFn)(.safeShiny_event_start)
        .safeShiny_event_value
      },
      error = function(.safeShiny_e) {
        .(eventHandlers$errorHandler)(.safeShiny_e, .safeShiny_event_start)
        shiny::req(FALSE)
      }
    )
  })

  shiny::observeEvent(
    wrappedEventExpr, wrappedHandlerExpr,
    event.env = event.env, event.quoted = TRUE,
    handler.env = handler.env, handler.quoted = TRUE, ...,
    label = label, suspended = suspended, priority = priority,
    domain = domain, autoDestroy = autoDestroy,
    ignoreNULL = ignoreNULL, ignoreInit = ignoreInit, once = once
  )
}
