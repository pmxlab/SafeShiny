#' @keywords internal
.safeShinyEnv <- new.env(parent = emptyenv())
.safeShinyEnv$stores <- list()
.safeShinyEnv$errors <- list()
.safeShinyEnv$tracking <- list()
.safeShinyEnv$cleanup <- list()
.safeShinyEnv$windows <- list()
.safeShinyEnv$console <- new.env(parent = emptyenv())  # see console.R
.safeShinyEnv$console$sessions <- character(0)
.safeShinyEnv$console$offsets <- list()

#' Resolve the storage key for a Shiny session/domain
#'
#' Keys the timing store by the session's own token when available, so concurrent sessions'
#' timings never mix. Falls back to a single \code{"global"} key when there is no reactive
#' domain (e.g. called outside a running Shiny session, such as in tests).
#'
#' @param domain a Shiny reactive domain (session object), or \code{NULL}.
#' @return a character string key.
#' @keywords internal
.safeShinySessionKey <- function(domain) {
  if (!is.null(domain) && !is.null(domain$token)) {
    domain$token
  } else {
    "global"
  }
}

#' Record one timed execution
#'
#' @param domain a Shiny reactive domain (session object), or \code{NULL}.
#' @param label character string identifying the call site.
#' @param elapsed numeric, elapsed time in seconds.
#' @param status character string, one of \code{"ok"}, \code{"silent"} (a \code{shiny::req()}/
#'   \code{validate()} silent stop), or \code{"error"}.
#' @param start \code{POSIXct} start time of the call. Defaults to the end time minus
#'   \code{elapsed}.
#' @param id integer id of the call, unique within the session store, or \code{NA}.
#' @param parent integer id of the enclosing tracked call, or \code{NA} for a top-level call.
#' @param depth integer nesting depth (0 for a top-level call).
#' @param type character string, the kind of tracked call: \code{"observe"}, \code{"react"},
#'   \code{"render"}, \code{"download"}, or \code{NA}.
#' @return nothing - side effect only.
#' @keywords internal
.recordSafeShinyTiming <- function(domain, label, elapsed, status, start = NULL, id = NA_integer_,
                                   parent = NA_integer_, depth = 0L, type = NA_character_) {
  key <- .safeShinySessionKey(domain)
  .safeShinyEnsureStore(key)
  now <- Sys.time()
  if (is.null(start)) {
    start <- now - elapsed
  }
  st <- .safeShinyEnv$stores[[key]]
  n <- st$n + 1L
  st$n <- n
  # one binding per record in a hashed environment: O(1) append, no copying of earlier records
  assign(as.character(n), list(
    label = label, elapsed = elapsed, status = status, timestamp = now,
    start = start, id = as.integer(id), parent = as.integer(parent), depth = as.integer(depth),
    type = as.character(type)
  ), envir = st$records)
  invisible(NULL)
}

#' Make sure a timing store exists for a session key
#' @param key character string, see \code{.safeShinySessionKey}.
#' @return nothing - side effect only.
#' @keywords internal
.safeShinyEnsureStore <- function(key) {
  if (is.null(.safeShinyEnv$stores[[key]])) {
    # Environments (not lists) so appending a record never copies the earlier ones.
    st <- new.env(parent = emptyenv())
    st$records <- new.env(hash = TRUE, parent = emptyenv())
    st$n <- 0L
    st$firstTime <- Sys.time()
    st$stack <- integer(0)
    st$nextId <- 1L
    .safeShinyEnv$stores[[key]] <- st
  }
  invisible(NULL)
}

#' Start timing a tracked call and push it onto the session's call stack
#'
#' The parent of the new call is whichever tracked call is currently running in the same
#' session (the top of the stack), so tracked calls evaluated synchronously inside one another
#' (e.g. a \code{SafeReactive} read from a \code{SafeObserve}) record their nesting.
#'
#' @param domain a Shiny reactive domain (session object), or \code{NULL}.
#' @param label character string identifying the call site.
#' @param type character string, see \code{.recordSafeShinyTiming}.
#' @return an opaque token (a list) to be passed to \code{.endSafeShinyTiming()}.
#' @keywords internal
.startSafeShinyTiming <- function(domain, label, type = NA_character_) {
  key <- .safeShinySessionKey(domain)
  .safeShinyEnsureStore(key)
  .safeShinyRegisterCleanup(domain)
  store <- .safeShinyEnv$stores[[key]]
  id <- store$nextId
  parent <- if (length(store$stack)) store$stack[length(store$stack)] else NA_integer_
  depth <- length(store$stack)
  store$nextId <- id + 1L
  store$stack <- c(store$stack, id)
  list(start = Sys.time(), id = id, parent = parent, depth = depth, label = label, type = type)
}

#' Finish timing a tracked call: pop it off the call stack and record it
#'
#' Any calls above \code{token} on the stack (left over because they never finished, e.g. an
#' interrupt) are dropped with it, so the stack cannot get permanently out of step.
#'
#' @param domain a Shiny reactive domain (session object), or \code{NULL}.
#' @param token the token returned by \code{.startSafeShinyTiming()}.
#' @param status character string, see \code{.recordSafeShinyTiming}.
#' @return nothing - side effect only.
#' @keywords internal
.endSafeShinyTiming <- function(domain, token, status) {
  key <- .safeShinySessionKey(domain)
  store <- .safeShinyEnv$stores[[key]]
  if (!is.null(store)) {
    pos <- match(token$id, store$stack)
    if (!is.na(pos)) {
      store$stack <- store$stack[seq_len(pos - 1L)]
    }
  }
  elapsed <- as.numeric(difftime(Sys.time(), token$start, units = "secs"))
  .recordSafeShinyTiming(
    domain = domain, label = token$label, elapsed = elapsed, status = status,
    start = token$start, id = token$id, parent = token$parent, depth = token$depth,
    type = token$type
  )
}

#' Finish a timing token, or discard it if it turned out to be uninteresting
#'
#' Used for the event-expression phase of \code{SafeObserveEvent()}: the call is recorded only if it
#' ran at least one other tracked call (those are then drawn nested under it), took at least
#' \code{minElapsed} seconds, or ended with a non-"ok" status; otherwise it is just popped off the
#' call stack, so trivial \code{input$x} events do not clutter the timing.
#'
#' @param domain a Shiny reactive domain (session object), or \code{NULL}.
#' @param token the token returned by \code{.startSafeShinyTiming()}, or \code{NULL} (a no-op).
#' @param minElapsed numeric, seconds.
#' @return nothing - side effect only.
#' @keywords internal
.safeShinyEndOrDiscardTiming <- function(domain, token, minElapsed = 0.001) {
  if (!is.list(token)) return(invisible(NULL))
  store <- .safeShinyEnv$stores[[.safeShinySessionKey(domain)]]
  elapsed <- as.numeric(difftime(Sys.time(), token$start, units = "secs"))
  hadChildren <- !is.null(store) && store$nextId > token$id + 1L
  if (hadChildren || elapsed >= minElapsed) {
    .endSafeShinyTiming(domain, token, "ok")
  } else if (!is.null(store)) {
    pos <- match(token$id, store$stack)
    if (!is.na(pos)) store$stack <- store$stack[seq_len(pos - 1L)]
  }
  invisible(NULL)
}

#' Get tracked execution-time records for a Shiny session
#'
#' Returns a per-label summary of every \code{\link{SafeObserve}}/\code{\link{SafeObserveEvent}}
#' call made with \code{trackTime = TRUE} in the given session, aggregated across however many
#' times each label has fired so far. Records from a \code{shiny::req()}/\code{validate()}
#' silent stop and from a caught error are both included (either way, real wall-clock time was
#' spent before execution stopped) - use \code{status} if you need to distinguish them, which
#' is only possible via \code{\link{GetSafeShinyTimingRaw}}.
#'
#' @param session a Shiny reactive domain (session object). Defaults to
#'   \code{shiny::getDefaultReactiveDomain()}; pass \code{NULL} explicitly (or call from outside
#'   a running session) to read the fallback \code{"global"} store used when no session exists.
#'
#' @return a data.frame with columns \code{label}, \code{type} (\code{"observe"}, \code{"react"},
#'   \code{"render"} or \code{"download"}; the first type seen for that label), \code{n},
#'   \code{total_time}, \code{mean_time}, \code{last_time}, sorted by \code{total_time} descending. Zero rows if nothing has been
#'   tracked yet.
#'
#' @examples
#' SafeShiny::ResetSafeShinyTiming(session = NULL)
#' SafeShiny:::.recordSafeShinyTiming(domain = NULL, label = "demo", elapsed = 0.01, status = "ok")
#' GetSafeShinyTiming(session = NULL)
#'
#' @export
GetSafeShinyTiming <- function(session = shiny::getDefaultReactiveDomain()) {
  raw <- GetSafeShinyTimingRaw(session = session)
  empty <- data.frame(
    label = character(0), type = character(0), n = integer(0), total_time = numeric(0),
    mean_time = numeric(0), last_time = numeric(0), stringsAsFactors = FALSE
  )
  if (nrow(raw) == 0) {
    return(empty)
  }

  labels <- unique(raw$label)
  rows <- lapply(labels, function(l) {
    sub <- raw[raw$label == l, , drop = FALSE]
    data.frame(
      label = l,
      type = sub$type[1],
      n = nrow(sub),
      total_time = sum(sub$elapsed),
      mean_time = mean(sub$elapsed),
      last_time = sub$elapsed[which.max(sub$timestamp)],
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out[order(-out$total_time), , drop = FALSE]
}

#' Get the raw, per-call tracked execution-time records for a Shiny session
#'
#' Unlike \code{\link{GetSafeShinyTiming}}, this returns one row per tracked call (not
#' aggregated by label), including the \code{status} of each call - useful to separate
#' successful runs from caught errors or \code{shiny::req()}/\code{validate()} silent stops.
#'
#' @inheritParams GetSafeShinyTiming
#' @return a data.frame with columns \code{label}, \code{elapsed}, \code{status}, \code{timestamp}
#'   (the end time), \code{start}, \code{id}, \code{parent} (the \code{id} of the enclosing tracked
#'   call, \code{NA} at top level), \code{depth} and \code{type} (one row per tracked call, in the order they
#'   finished). Zero rows if nothing has been
#'   tracked yet.
#'
#' @examples
#' SafeShiny::ResetSafeShinyTiming(session = NULL)
#' SafeShiny:::.recordSafeShinyTiming(domain = NULL, label = "demo", elapsed = 0.01, status = "ok")
#' GetSafeShinyTimingRaw(session = NULL)
#'
#' @export
GetSafeShinyTimingRaw <- function(session = shiny::getDefaultReactiveDomain()) {
  key <- .safeShinySessionKey(session)
  st <- .safeShinyEnv$stores[[key]]
  recs <- if (is.null(st)) NULL else unname(mget(as.character(seq_len(st$n)), envir = st$records))
  if (is.null(recs) || length(recs) == 0) {
    return(data.frame(
      label = character(0), elapsed = numeric(0), status = character(0),
      timestamp = as.POSIXct(character(0)), start = as.POSIXct(character(0)),
      id = integer(0), parent = integer(0), depth = integer(0), type = character(0),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    label = vapply(recs, function(r) r$label, character(1)),
    elapsed = vapply(recs, function(r) r$elapsed, numeric(1)),
    status = vapply(recs, function(r) r$status, character(1)),
    timestamp = do.call(c, lapply(recs, function(r) r$timestamp)),
    start = do.call(c, lapply(recs, function(r) r$start)),
    id = vapply(recs, function(r) r$id, integer(1)),
    parent = vapply(recs, function(r) r$parent, integer(1)),
    depth = vapply(recs, function(r) r$depth, integer(1)),
    type = vapply(recs, function(r) if (is.null(r$type)) NA_character_ else r$type, character(1)),
    stringsAsFactors = FALSE
  )
}

#' Summarize tracked execution time vs. wall-clock elapsed time for a Shiny session
#'
#' Splits the wall-clock time elapsed since the first tracked call in this session into time
#' actually spent executing tracked user code (the sum of every top-level \code{\link{SafeObserve}}/
#' \code{\link{SafeObserveEvent}} call made with \code{trackTime = TRUE}) and the remainder,
#' labelled \code{untracked_time} - an approximation of time spent in Shiny's own reactive-graph
#' maintenance (invalidation, scheduling, flushing) plus anything not wrapped with
#' \code{trackTime = TRUE}.
#'
#' Nested tracked calls (e.g. a \code{SafeReactive} read inside a \code{SafeObserve}) are
#' counted only once, through their top-level ancestor, so the total is not inflated by
#' nesting. Per-label times in \code{timing} remain inclusive of nested children.
#'
#' @inheritParams GetSafeShinyTiming
#' @param trim logical, default \code{FALSE}. Which time window is the "wall clock"?
#'   \code{TRUE}: from the start of the first tracked call to the end of the last one, so idle time
#'   before and after the activity is ignored (calls outside that window are left out of
#'   \code{total_tracked_time}; the \code{timing} table always covers all calls). \code{FALSE}: the whole tracking window, i.e. from
#'   \code{\link{StartSafeShinyTracking}} to \code{\link{StopSafeShinyTracking}} (or to now while
#'   tracking is still on); if tracking was not switched on with these functions (e.g. via the
#'   \code{SafeShiny.trackTime} option), from the first tracked call to now.
#' @param trimMinTime numeric, seconds, default \code{0} (every call counts). With \code{trim = TRUE}, calls shorter
#'   than this do not set the edges of the window - e.g. \code{0.01} to ignore tiny calls: tiny calls such as a tab-shown observer fire
#'   on the user's own navigation (e.g. back to the monitoring tab to press Stop) and would
#'   otherwise defeat the trimming. If no call is long enough, all calls are used.
#' @return an object of class \code{"SafeShinyTimingSummary"} (a list with elements
#'   \code{total_tracked_time}, \code{wall_clock_elapsed}, \code{untracked_time}, \code{n_labels}
#'   and \code{timing}, the same data.frame \code{\link{GetSafeShinyTiming}} returns) with a
#'   \code{print} method. \strong{The \code{untracked_time} estimate is only meaningful once
#'   every relevant observer in the session is wrapped with \code{trackTime = TRUE}} - otherwise
#'   it also includes time spent in anything left unwrapped, not just genuine Shiny overhead.
#'
#' @examples
#' SafeShiny::ResetSafeShinyTiming(session = NULL)
#' SafeShiny:::.recordSafeShinyTiming(domain = NULL, label = "demo", elapsed = 0.01, status = "ok")
#' print(SummarizeSafeShinyTiming(session = NULL))
#'
#' @export
SummarizeSafeShinyTiming <- function(session = shiny::getDefaultReactiveDomain(), trim = FALSE,
                                     trimMinTime = 0) {
  key <- .safeShinySessionKey(session)
  store <- .safeShinyEnv$stores[[key]]
  timing <- GetSafeShinyTiming(session = session)

  raw <- GetSafeShinyTimingRaw(session = session)
  tw <- if (nrow(raw) > 0 && isTRUE(trim)) .safeShinyTrimWindow(raw, trimMinTime) else NULL
  if (!is.null(tw)) raw <- raw[tw$keep, , drop = FALSE]
  total_tracked <- sum(raw$elapsed[raw$depth == 0])
  wall_clock <- if (!is.null(tw)) {
    as.numeric(difftime(tw$end, tw$origin, units = "secs"))
  } else if (!is.null(store) && !is.null(store$firstTime)) {
    win <- .safeShinyEnv$windows[[key]]
    from <- if (!is.null(win)) win$start else store$firstTime
    to <- if (!is.null(win) && !is.null(win$stop)) win$stop else Sys.time()
    as.numeric(difftime(to, from, units = "secs"))
  } else {
    NA_real_
  }
  untracked <- if (!is.na(wall_clock)) max(wall_clock - total_tracked, 0) else NA_real_

  structure(
    list(
      total_tracked_time = total_tracked,
      wall_clock_elapsed = wall_clock,
      untracked_time = untracked,
      n_labels = nrow(timing),
      timing = timing
    ),
    class = "SafeShinyTimingSummary"
  )
}

#' The time window of a trimmed summary/flame chart
#'
#' @param raw data.frame as returned by \code{GetSafeShinyTimingRaw()}, at least one row.
#' @param minTime numeric, seconds: calls shorter than this do not set the window edges (all calls
#'   are used if none is long enough).
#' @return a list with \code{origin} and \code{end} (\code{POSIXct}) and \code{keep}, a logical
#'   vector marking the calls that lie inside the window.
#' @keywords internal
.safeShinyTrimWindow <- function(raw, minTime = 0) {
  ends <- raw$start + raw$elapsed
  sig <- raw$elapsed >= minTime
  if (!any(sig)) sig <- rep(TRUE, nrow(raw))
  origin <- min(raw$start[sig])
  end <- max(ends[sig])
  list(origin = origin, end = end, keep = raw$start >= origin & ends <= end)
}

#' Print a SafeShiny timing summary
#'
#' @param x a \code{"SafeShinyTimingSummary"} object, as returned by
#'   \code{\link{SummarizeSafeShinyTiming}}.
#' @param ... unused, present for S3 consistency with \code{\link[base]{print}}.
#' @return \code{x}, invisibly.
#' @export
print.SafeShinyTimingSummary <- function(x, ...) {
  cat("SafeShiny timing summary\n")
  cat("  Tracked user-code time: ", round(x$total_tracked_time, 3), "s across ", x$n_labels,
      " label(s)\n", sep = "")
  cat("  Wall-clock elapsed: ", round(x$wall_clock_elapsed, 3), "s\n",
      sep = "")
  cat("  Approx. untracked time (Shiny overhead + anything not wrapped): ",
      round(x$untracked_time, 3), "s\n", sep = "")
  cat("  NOTE: 'untracked time' only approximates Shiny's own reactive-graph overhead once\n")
  cat("        EVERY relevant observer in this session is wrapped with trackTime = TRUE -\n")
  cat("        otherwise it also includes time spent in anything left unwrapped.\n")
  invisible(x)
}

#' Reset tracked execution-time records for a Shiny session
#'
#' @inheritParams GetSafeShinyTiming
#' @return \code{NULL}, invisibly.
#'
#' @examples
#' ResetSafeShinyTiming(session = NULL)
#'
#' @export
ResetSafeShinyTiming <- function(session = shiny::getDefaultReactiveDomain()) {
  key <- .safeShinySessionKey(session)
  .safeShinyEnv$stores[[key]] <- NULL
  .safeShinyEnv$windows[[key]] <- NULL
  invisible(NULL)
}

#' Default label for an untimed/unlabelled call site
#'
#' @param expr a quoted expression.
#' @return a short character string derived from the deparsed expression.
#' @keywords internal
.safeShinyDefaultLabel <- function(expr) {
  txt <- paste(deparse(expr), collapse = " ")
  txt <- trimws(gsub("\\s+", " ", txt))
  substr(txt, 1, 40)
}

#' Kind of tracked call for a wrapper context
#'
#' @param context character string, e.g. \code{"SafeObserve"} or \code{"SafeRenderPlot"}.
#' @return one of \code{"observe"}, \code{"react"}, \code{"render"}, \code{"download"}.
#' @keywords internal
.safeShinyTypeFromContext <- function(context) {
  if (startsWith(context, "SafeObserve")) {
    "observe"
  } else if (identical(context, "SafeReactive")) {
    "react"
  } else if (identical(context, "SafeDownloadHandler")) {
    "download"
  } else {
    "render"
  }
}

#' Should a call be timed right now?
#'
#' @param trackTime \code{TRUE}, \code{FALSE} or \code{NA}/\code{NULL} (auto).
#' @param domain a Shiny reactive domain (session object), or \code{NULL}.
#' @return logical scalar. Auto means: the \code{SafeShiny.trackTime} option is \code{TRUE}, or
#'   tracking was started for this session with \code{\link{StartSafeShinyTracking}}.
#' @keywords internal
.safeShinyShouldTrack <- function(trackTime, domain) {
  if (IsSafeShinyTrackingDisabled()) return(FALSE)
  if (isTRUE(trackTime)) return(TRUE)
  if (isFALSE(trackTime)) return(FALSE)
  isTRUE(getOption("SafeShiny.trackTime", FALSE)) || IsSafeShinyTracking(domain)
}

#' Start timing every tracked call in a session
#'
#' Switches execution-time tracking on for one Shiny session, at run time: every \code{Safe*}
#' call that doesn't force \code{trackTime} on or off is timed from now on, including calls
#' created by other packages (e.g. MMVshiny-generated observers/reactives). Because the switch is
#' per session, other users of the same R process are not affected.
#'
#' @inheritParams GetSafeShinyTiming
#' @param reset logical, default \code{TRUE}: discard previously recorded timings first.
#' @return \code{NULL}, invisibly.
#'
#' @examples
#' StartSafeShinyTracking(session = NULL)
#' IsSafeShinyTracking(session = NULL)
#' StopSafeShinyTracking(session = NULL)
#'
#' @export
StartSafeShinyTracking <- function(session = shiny::getDefaultReactiveDomain(), reset = TRUE) {
  if (IsSafeShinyTrackingDisabled()) {
    message("[SafeShiny] Tracking is disabled by options(SafeShiny.trackTime = FALSE); not started.")
    return(invisible(NULL))
  }
  key <- .safeShinySessionKey(session)
  if (isTRUE(reset)) {
    ResetSafeShinyTiming(session = session)
  }
  .safeShinyEnv$tracking[[key]] <- TRUE
  .safeShinyEnv$windows[[key]] <- list(start = Sys.time(), stop = NULL)
  .safeShinyRegisterCleanup(session)
  invisible(NULL)
}

#' @rdname StartSafeShinyTracking
#' @export
StopSafeShinyTracking <- function(session = shiny::getDefaultReactiveDomain()) {
  key <- .safeShinySessionKey(session)
  .safeShinyEnv$tracking[[key]] <- FALSE
  win <- .safeShinyEnv$windows[[key]]
  if (!is.null(win) && is.null(win$stop)) {
    win$stop <- Sys.time()
    .safeShinyEnv$windows[[key]] <- win
  }
  invisible(NULL)
}

#' @rdname StartSafeShinyTracking
#' @return \code{IsSafeShinyTracking()} returns a logical scalar.
#' @export
IsSafeShinyTracking <- function(session = shiny::getDefaultReactiveDomain()) {
  isTRUE(.safeShinyEnv$tracking[[.safeShinySessionKey(session)]])
}

#' Is execution-time tracking disabled globally?
#'
#' \code{options(SafeShiny.trackTime = FALSE)} is a master kill switch, e.g. for production: wrappers
#' created while it is set take the cheapest possible path for every call (the decision is made once,
#' at creation), \code{\link{StartSafeShinyTracking}} does nothing, and not even an explicit
#' \code{trackTime = TRUE} (or the monitoring tab) can switch tracking on. When the option is unset,
#' tracking is "auto" (switchable per session at run time); when \code{TRUE}, always on.
#'
#' @return a logical scalar, \code{TRUE} if the option is exactly \code{FALSE}.
#'
#' @examples
#' IsSafeShinyTrackingDisabled()
#'
#' @export
IsSafeShinyTrackingDisabled <- function() {
  isFALSE(getOption("SafeShiny.trackTime"))
}

#' Free a session's timing/error/tracking state when the session ends
#'
#' Registers (once per session) an \code{onSessionEnded} callback; a no-op for \code{NULL} or for
#' objects without \code{onSessionEnded}.
#'
#' @param domain a Shiny reactive domain (session object), or \code{NULL}.
#' @return nothing - side effect only.
#' @keywords internal
.safeShinyRegisterCleanup <- function(domain) {
  if (is.null(domain) || is.null(domain$token) || !is.function(domain$onSessionEnded)) {
    return(invisible(NULL))
  }
  key <- .safeShinySessionKey(domain)
  if (isTRUE(.safeShinyEnv$cleanup[[key]])) {
    return(invisible(NULL))
  }
  .safeShinyEnv$cleanup[[key]] <- TRUE
  domain$onSessionEnded(function() {
    .safeShinyEnv$stores[[key]] <- NULL
    .safeShinyEnv$errors[[key]] <- NULL
    .safeShinyEnv$tracking[[key]] <- NULL
    .safeShinyEnv$windows[[key]] <- NULL
    .safeShinyConsoleRemove(key)
    .safeShinyEnv$cleanup[[key]] <- NULL
  })
  invisible(NULL)
}

#' Record a caught (non-silent) error
#'
#' Keeps the most recent 500 per session.
#'
#' @param domain a Shiny reactive domain (session object), or \code{NULL}.
#' @param label character string identifying the call site.
#' @param type character string, the kind of call (see \code{.recordSafeShinyTiming}).
#' @param e the caught condition.
#' @return nothing - side effect only.
#' @keywords internal
.recordSafeShinyError <- function(domain, label, type, e) {
  key <- .safeShinySessionKey(domain)
  call <- conditionCall(e)
  rec <- list(time = Sys.time(), label = label, type = type, message = conditionMessage(e),
              call = if (is.null(call)) NA_character_ else paste(deparse(call), collapse = " "))
  errs <- c(.safeShinyEnv$errors[[key]], list(rec))
  if (length(errs) > 500) errs <- errs[(length(errs) - 499):length(errs)]
  .safeShinyEnv$errors[[key]] <- errs
  .safeShinyRegisterCleanup(domain)
  invisible(NULL)
}

#' Get the errors caught by the Safe* wrappers in a Shiny session
#'
#' Every genuine error caught by a \code{Safe*} wrapper (not \code{shiny::req()}/\code{validate()}
#' silent stops) is recorded, whether or not timing is tracked; the most recent 500 per session are
#' kept.
#'
#' @inheritParams GetSafeShinyTiming
#' @return a data.frame with columns \code{time}, \code{label}, \code{type}, \code{message}
#'   and \code{call}, oldest first. Zero rows if no error was caught.
#'
#' @examples
#' ResetSafeShinyErrors(session = NULL)
#' GetSafeShinyErrors(session = NULL)
#'
#' @export
GetSafeShinyErrors <- function(session = shiny::getDefaultReactiveDomain()) {
  errs <- .safeShinyEnv$errors[[.safeShinySessionKey(session)]]
  if (length(errs) == 0) {
    return(data.frame(time = as.POSIXct(character(0)), label = character(0), type = character(0),
                      message = character(0), call = character(0), stringsAsFactors = FALSE))
  }
  data.frame(
    time = do.call(c, lapply(errs, function(r) r$time)),
    label = vapply(errs, function(r) r$label, character(1)),
    type = vapply(errs, function(r) r$type, character(1)),
    message = vapply(errs, function(r) r$message, character(1)),
    call = vapply(errs, function(r) r$call, character(1)),
    stringsAsFactors = FALSE
  )
}

#' @rdname GetSafeShinyErrors
#' @export
ResetSafeShinyErrors <- function(session = shiny::getDefaultReactiveDomain()) {
  .safeShinyEnv$errors[[.safeShinySessionKey(session)]] <- NULL
  invisible(NULL)
}
