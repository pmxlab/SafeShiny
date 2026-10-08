#' @keywords internal
.safeShinyEnv <- new.env(parent = emptyenv())
.safeShinyEnv$stores <- list()

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
#' @return nothing - side effect only.
#' @keywords internal
.recordSafeShinyTiming <- function(domain, label, elapsed, status, start = NULL, id = NA_integer_,
                                   parent = NA_integer_, depth = 0L) {
  key <- .safeShinySessionKey(domain)
  .safeShinyEnsureStore(key)
  now <- Sys.time()
  if (is.null(start)) {
    start <- now - elapsed
  }
  n <- length(.safeShinyEnv$stores[[key]]$records)
  .safeShinyEnv$stores[[key]]$records[[n + 1]] <- list(
    label = label, elapsed = elapsed, status = status, timestamp = now,
    start = start, id = as.integer(id), parent = as.integer(parent), depth = as.integer(depth)
  )
  invisible(NULL)
}

#' Make sure a timing store exists for a session key
#' @param key character string, see \code{.safeShinySessionKey}.
#' @return nothing - side effect only.
#' @keywords internal
.safeShinyEnsureStore <- function(key) {
  if (is.null(.safeShinyEnv$stores[[key]])) {
    .safeShinyEnv$stores[[key]] <- list(records = list(), firstTime = Sys.time(),
                                        stack = integer(0), nextId = 1L)
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
#' @return an opaque token (a list) to be passed to \code{.endSafeShinyTiming()}.
#' @keywords internal
.startSafeShinyTiming <- function(domain, label) {
  key <- .safeShinySessionKey(domain)
  .safeShinyEnsureStore(key)
  store <- .safeShinyEnv$stores[[key]]
  id <- store$nextId
  parent <- if (length(store$stack)) store$stack[length(store$stack)] else NA_integer_
  depth <- length(store$stack)
  .safeShinyEnv$stores[[key]]$nextId <- id + 1L
  .safeShinyEnv$stores[[key]]$stack <- c(store$stack, id)
  list(start = Sys.time(), id = id, parent = parent, depth = depth, label = label)
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
      .safeShinyEnv$stores[[key]]$stack <- store$stack[seq_len(pos - 1L)]
    }
  }
  elapsed <- as.numeric(difftime(Sys.time(), token$start, units = "secs"))
  .recordSafeShinyTiming(
    domain = domain, label = token$label, elapsed = elapsed, status = status,
    start = token$start, id = token$id, parent = token$parent, depth = token$depth
  )
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
#' @return a data.frame with columns \code{label}, \code{n}, \code{total_time}, \code{mean_time},
#'   \code{last_time}, sorted by \code{total_time} descending. Zero rows if nothing has been
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
    label = character(0), n = integer(0), total_time = numeric(0),
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
#'   call, \code{NA} at top level) and \code{depth} (one row per tracked call, in the order they
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
  recs <- .safeShinyEnv$stores[[key]]$records
  if (is.null(recs) || length(recs) == 0) {
    return(data.frame(
      label = character(0), elapsed = numeric(0), status = character(0),
      timestamp = as.POSIXct(character(0)), start = as.POSIXct(character(0)),
      id = integer(0), parent = integer(0), depth = integer(0), stringsAsFactors = FALSE
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
SummarizeSafeShinyTiming <- function(session = shiny::getDefaultReactiveDomain()) {
  key <- .safeShinySessionKey(session)
  store <- .safeShinyEnv$stores[[key]]
  timing <- GetSafeShinyTiming(session = session)

  raw <- GetSafeShinyTimingRaw(session = session)
  total_tracked <- sum(raw$elapsed[raw$depth == 0])
  wall_clock <- if (!is.null(store) && !is.null(store$firstTime)) {
    as.numeric(difftime(Sys.time(), store$firstTime, units = "secs"))
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
  cat("  Wall-clock elapsed since first tracked call: ", round(x$wall_clock_elapsed, 3), "s\n",
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
