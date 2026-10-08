# Opt-in capture of the R console output (stdout and stderr) for the monitoring tab.
#
# R has a single stdout and a single stderr per process, so the capture is process-wide: the output
# of every session in the process ends up in the same log. It is reference counted by session, and
# only active while at least one session has asked for it.

#' Start capturing the console output for a session
#'
#' Diverts R's standard output (still echoed to the console) and standard error (messages and
#' warnings; \strong{no longer echoed} while capturing, because R cannot split that stream) into a
#' temporary log file. The capture is process-wide and reference counted per session.
#'
#' @param session a Shiny session, or \code{NULL}.
#' @return \code{TRUE} if the capture is active, invisibly.
#' @keywords internal
.safeShinyConsoleStart <- function(session = shiny::getDefaultReactiveDomain()) {
  key <- .safeShinySessionKey(session)
  e <- .safeShinyEnv$console
  if (is.null(e$con)) {
    path <- tempfile("safeshiny-console-", fileext = ".log")
    con <- file(path, open = "w")
    sink(con, split = TRUE)
    sink(con, type = "message")
    e$con <- con
    e$path <- path
    e$sinkLevel <- sink.number()
  }
  e$sessions <- union(e$sessions, key)
  .safeShinyRegisterCleanup(session)
  invisible(TRUE)
}

#' Stop capturing the console output for a session
#'
#' The process-wide capture ends when the last session using it stops.
#'
#' @inheritParams .safeShinyConsoleStart
#' @return nothing - side effect only.
#' @keywords internal
.safeShinyConsoleStop <- function(session = shiny::getDefaultReactiveDomain()) {
  .safeShinyConsoleRemove(.safeShinySessionKey(session))
}

#' Remove a session key from the console capture, ending the capture if it was the last one
#' @param key character string, see \code{.safeShinySessionKey}.
#' @return nothing - side effect only.
#' @keywords internal
.safeShinyConsoleRemove <- function(key) {
  e <- .safeShinyEnv$console
  e$offsets[[key]] <- NULL
  if (!(key %in% e$sessions)) return(invisible(NULL))
  e$sessions <- setdiff(e$sessions, key)
  if (length(e$sessions) == 0 && !is.null(e$con)) {
    sink(type = "message")
    if (sink.number() == e$sinkLevel) {
      sink()
    } else {
      message("[SafeShiny] Another sink was opened after the console capture; leaving it in place.")
    }
    try(close(e$con), silent = TRUE)
    unlink(e$path)
    e$con <- NULL
    e$path <- NULL
  }
  invisible(NULL)
}

#' Is the console output being captured for a session?
#' @inheritParams .safeShinyConsoleStart
#' @return a logical scalar.
#' @keywords internal
.safeShinyConsoleActive <- function(session = shiny::getDefaultReactiveDomain()) {
  .safeShinySessionKey(session) %in% .safeShinyEnv$console$sessions
}

#' Get the captured console output of a session
#'
#' Reads from the byte offset of the last clear, and at most the last \code{maxBytes} bytes, so a
#' huge log is never read in full. Bytes that are not valid UTF-8 are replaced by their
#' \code{<xx>} hex escape, because invalid strings cannot be sent to the browser.
#'
#' @inheritParams .safeShinyConsoleStart
#' @param max integer, keep only the last \code{max} lines.
#' @param maxBytes numeric, read at most this many trailing bytes (default 2 MB).
#' @return a character vector of lines since the capture started or the last
#'   \code{.safeShinyConsoleClear()}; empty if not capturing.
#' @keywords internal
.safeShinyConsoleGet <- function(session = shiny::getDefaultReactiveDomain(), max = 5000L,
                                 maxBytes = 2e6) {
  e <- .safeShinyEnv$console
  key <- .safeShinySessionKey(session)
  if (is.null(e$con) || !(key %in% e$sessions)) return(character(0))
  try(flush(e$con), silent = TRUE)
  off <- e$offsets[[key]]
  if (is.null(off)) off <- 0
  size <- file.size(e$path)
  if (is.na(size) || size <= off) return(character(0))
  start <- max(off, size - maxBytes)
  rc <- file(e$path, open = "rb")
  on.exit(close(rc), add = TRUE)
  if (start > 0) seek(rc, start)
  lines <- readLines(rc, warn = FALSE)
  if (start > off && length(lines) > 0) lines <- lines[-1]  # first line is cut mid-way
  lines <- iconv(lines, from = "UTF-8", to = "UTF-8", sub = "byte")
  lines[is.na(lines)] <- ""
  if (length(lines) > max) lines <- lines[(length(lines) - max + 1):length(lines)]
  lines
}

#' Clear the captured console output of a session
#'
#' Only hides what was captured so far for this session (other sessions' views are unaffected).
#'
#' @inheritParams .safeShinyConsoleStart
#' @return nothing - side effect only.
#' @keywords internal
.safeShinyConsoleClear <- function(session = shiny::getDefaultReactiveDomain()) {
  e <- .safeShinyEnv$console
  key <- .safeShinySessionKey(session)
  if (is.null(e$con) || !(key %in% e$sessions)) return(invisible(NULL))
  try(flush(e$con), silent = TRUE)
  e$offsets[[key]] <- file.size(e$path)
  invisible(NULL)
}
