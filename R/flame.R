#' Plot a nested flame chart of tracked execution time
#'
#' Draws a profvis-style flame chart of every tracked call
#' (\code{trackTime = TRUE}) recorded so far in a session: time runs along the x-axis and
#' nesting depth up the y-axis, so a tracked call evaluated synchronously inside another one
#' (e.g. a \code{\link{SafeReactive}} read from a \code{\link{SafeObserve}}) is drawn as a bar
#' directly above its parent. Bars are filled by the kind of call (observer, reactive, render,
#' download), and calls that ended in an error or a \code{shiny::req()}/\code{validate()} silent
#' stop get a red or amber outline.
#'
#' Nesting is inferred from a per-session stack of currently-running tracked calls, so it is
#' only reliable for synchronous code: with \code{promises}/\code{future} the intervals of
#' different calls interleave. Time not spent in any tracked call (Shiny's own overhead and
#' anything not wrapped) is simply empty space, and is summarised in a label in the bottom-right
#' corner (the chart's time span minus the time covered by top-level tracked calls).
#'
#' @param session a Shiny session, or \code{NULL} for the global store used outside a running
#'   app. Defaults to the current reactive domain.
#' @param minTime numeric, default \code{0}. Calls shorter than this many seconds are not
#'   labelled (they are still drawn).
#' @param main character string, plot title.
#'
#' @return the data.frame of drawn calls (see \code{\link{GetSafeShinyTimingRaw}}, with an
#'   extra \code{t0} column, seconds from the first call's start), invisibly. \code{NULL}
#'   invisibly if nothing has been tracked yet.
#'
#' @examples
#' ResetSafeShinyTiming(session = NULL)
#' outer <- SafeShiny:::.startSafeShinyTiming(NULL, "observer", "observe")
#' inner <- SafeShiny:::.startSafeShinyTiming(NULL, "reactive", "react")
#' Sys.sleep(0.01)
#' SafeShiny:::.endSafeShinyTiming(NULL, inner, "ok")
#' SafeShiny:::.endSafeShinyTiming(NULL, outer, "ok")
#' PlotSafeShinyFlame(session = NULL)
#'
#' @importFrom graphics plot rect text legend
#' @export
PlotSafeShinyFlame <- function(session = shiny::getDefaultReactiveDomain(), minTime = 0,
                               main = "SafeShiny flame chart") {
  raw <- GetSafeShinyTimingRaw(session = session)
  if (nrow(raw) == 0) {
    message("[SafeShiny] No tracked calls to plot.")
    return(invisible(NULL))
  }
  raw <- raw[order(raw$start), , drop = FALSE]
  origin <- min(raw$start)
  raw$t0 <- as.numeric(difftime(raw$start, origin, units = "secs"))
  raw$t1 <- raw$t0 + raw$elapsed

  typeCols <- c(observe = "#4C78A8", react = "#54A24B", render = "#B279A2", download = "#9D755D")
  statusCols <- c(silent = "#F2B134", error = "#E45756")
  fill <- typeCols[raw$type]
  fill[is.na(fill)] <- "grey60"
  border <- ifelse(raw$status %in% names(statusCols), statusCols[raw$status], "white")
  maxDepth <- max(raw$depth)
  xmax <- max(raw$t1)
  if (xmax <= 0) xmax <- 1

  plot(NULL, xlim = c(0, xmax), ylim = c(0, max(maxDepth + 1, 5)), xlab = "Time since first call (s)",
       ylab = "", yaxt = "n", main = main)
  rect(raw$t0, raw$depth, raw$t1, raw$depth + 1, col = fill, border = border,
       lwd = ifelse(raw$status %in% names(statusCols), 2, 1))
  lab <- raw$elapsed >= minTime
  text((raw$t0 + raw$t1)[lab] / 2, raw$depth[lab] + 0.5,
       sprintf("%s (%.3gs)", raw$label[lab], raw$elapsed[lab]), cex = 0.7, col = "white")
  top <- raw[raw$depth == 0, ]
  untracked <- max(xmax - sum(top$elapsed), 0)
  legend("bottomright", bty = "n", cex = 0.8, text.col = "grey30",
         legend = sprintf("Untracked: %.3gs (%.0f%% of %.3gs)", untracked,
                          100 * untracked / xmax, xmax))
  legend("topright", bty = "n", cex = 0.8,
         legend = c(names(typeCols), paste("outline:", names(statusCols))),
         fill = c(typeCols, rep(NA, length(statusCols))),
         border = c(rep("black", length(typeCols)), rep(NA, length(statusCols))),
         col = c(rep(NA, length(typeCols)), statusCols),
         lty = c(rep(NA, length(typeCols)), rep(1, length(statusCols))), lwd = 2)
  invisible(raw)
}
