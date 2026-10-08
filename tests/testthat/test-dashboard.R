test_that("SafeShinyTabRequested reads the query parameter", {
  expect_true(SafeShinyTabRequested("?safeshinytab=1"))
  expect_true(SafeShinyTabRequested("?a=2&safeshinytab=true"))
  expect_false(SafeShinyTabRequested("?safeshinytab=0"))
  expect_false(SafeShinyTabRequested("?other=1"))
  expect_false(SafeShinyTabRequested(""))
  expect_true(SafeShinyTabRequested(list(QUERY_STRING = "?safeshinytab=1")))
  expect_false(SafeShinyTabRequested(list(QUERY_STRING = "")))
  expect_true(SafeShinyTabRequested("?dev=1", param = "dev"))
})

test_that("Start/Stop/IsSafeShinyTracking are per session and Start resets timings", {
  s1 <- list(token = paste0("trk1-", as.numeric(Sys.time())))
  s2 <- list(token = paste0("trk2-", as.numeric(Sys.time())))
  expect_false(IsSafeShinyTracking(s1))
  SafeShiny:::.recordSafeShinyTiming(domain = s1, label = "old", elapsed = 0.1, status = "ok")
  StartSafeShinyTracking(s1)
  expect_true(IsSafeShinyTracking(s1))
  expect_false(IsSafeShinyTracking(s2))
  expect_equal(nrow(GetSafeShinyTimingRaw(s1)), 0)
  StopSafeShinyTracking(s1)
  expect_false(IsSafeShinyTracking(s1))
})

test_that("trackTime = NA (default) follows the session tracking switch at run time", {
  old <- options(SafeShiny.trackTime = NULL)
  on.exit(options(old), add = TRUE)
  n <- NULL
  shiny::testServer(function(input, output, session) {
    ResetSafeShinyTiming(session = session)
    SafeObserveEvent(input$x, { 1 + 1 }, ignoreInit = TRUE, label = "obs")
  }, expr = {
    settle(session)
    trigger(session, x = 1)
    expect_equal(nrow(GetSafeShinyTimingRaw(session = session)), 0)
    StartSafeShinyTracking(session = session)
    trigger(session, x = 2)
    expect_equal(GetSafeShinyTimingRaw(session = session)$label, "obs")
    StopSafeShinyTracking(session = session)
    trigger(session, x = 3)
    expect_equal(nrow(GetSafeShinyTimingRaw(session = session)), 1)
  })
})

test_that("caught errors are recorded (silent stops are not) and can be reset", {
  shiny::testServer(function(input, output, session) {
    ResetSafeShinyErrors(session = session)
    SafeObserveEvent(input$x, { stop("boom") }, ignoreInit = TRUE, quiet = TRUE, label = "bad")
    SafeObserveEvent(input$y, { shiny::req(FALSE) }, ignoreInit = TRUE, quiet = TRUE, label = "quiet")
  }, expr = {
    settle(session)
    trigger(session, x = 1)
    trigger(session, y = 1)
    errs <- GetSafeShinyErrors(session = session)
    expect_equal(nrow(errs), 1)
    expect_equal(errs$label, "bad")
    expect_equal(errs$type, "observe")
    expect_match(errs$message, "boom")
    ResetSafeShinyErrors(session = session)
    expect_equal(nrow(GetSafeShinyErrors(session = session)), 0)
  })
})

test_that("SafeShinyTabUI/Panel build, and the module server starts and stops tracking", {
  expect_s3_class(SafeShinyTabUI("m"), "shiny.tag.list")
  expect_s3_class(SafeShinyTabPanel("m", title = "Mon"), "shiny.tag")

  shiny::testServer(SafeShinyTabServer, {
    expect_false(IsSafeShinyTracking(session))
    session$setInputs(start = 1)
    expect_true(IsSafeShinyTracking(session))
    session$setInputs(stop = 1)
    expect_false(IsSafeShinyTracking(session))
    expect_match(output$summary, "SafeShiny timing summary", fixed = TRUE)
  })
})

test_that("options(SafeShiny.trackTime = FALSE) is a master kill switch", {
  old <- options(SafeShiny.trackTime = FALSE)
  on.exit(options(old), add = TRUE)
  expect_true(IsSafeShinyTrackingDisabled())

  fake <- list(token = paste0("kill-", as.numeric(Sys.time())))
  expect_message(StartSafeShinyTracking(fake), "disabled")
  expect_false(IsSafeShinyTracking(fake))

  shiny::testServer(function(input, output, session) {
    ResetSafeShinyTiming(session = session)
    SafeObserveEvent(input$x, { 1 }, ignoreInit = TRUE, label = "auto")
    SafeObserveEvent(input$y, { 1 }, ignoreInit = TRUE, label = "forced", trackTime = TRUE)
  }, expr = {
    settle(session)
    trigger(session, x = 1)
    trigger(session, y = 1)
    expect_equal(nrow(GetSafeShinyTimingRaw(session = session)), 0)
  })

  options(SafeShiny.trackTime = NULL)
  expect_false(IsSafeShinyTrackingDisabled())
  options(SafeShiny.trackTime = TRUE)
  expect_false(IsSafeShinyTrackingDisabled())
})

test_that("trim controls whether the summary/flame window covers idle time before and after", {
  s <- list(token = paste0("trim-", as.numeric(Sys.time())))
  StartSafeShinyTracking(s)
  Sys.sleep(0.3)  # idle before the first tracked call
  tok <- SafeShiny:::.startSafeShinyTiming(s, "work", "observe")
  Sys.sleep(0.1)
  SafeShiny:::.endSafeShinyTiming(s, tok, "ok")
  Sys.sleep(0.3)  # idle after the last tracked call
  StopSafeShinyTracking(s)
  Sys.sleep(0.2)  # time after Stop never counts

  trimmed <- SummarizeSafeShinyTiming(s, trim = TRUE)
  full <- SummarizeSafeShinyTiming(s, trim = FALSE)
  expect_equal(trimmed$wall_clock_elapsed, 0.1, tolerance = 0.5)
  expect_lt(trimmed$untracked_time, 0.05)
  expect_gt(full$wall_clock_elapsed, 0.65)
  expect_lt(full$wall_clock_elapsed, 0.9)
  # no drift once stopped
  expect_equal(SummarizeSafeShinyTiming(s)$wall_clock_elapsed, full$wall_clock_elapsed)

  expect_match(as.character(PlotSafeShinyFlameHTML(s, trim = TRUE)), '"xmax":0.1', fixed = TRUE)
  xmax <- function(w) as.numeric(sub('.*"xmax":([0-9.]+).*', "\\1", as.character(w)))
  expect_gt(xmax(PlotSafeShinyFlameHTML(s, trim = FALSE)), 0.65)
  ResetSafeShinyTiming(s)
})

test_that("console capture records output, clears per session and restores the sinks", {
  s <- list(token = paste0("con-", as.numeric(Sys.time())))
  n0 <- sink.number()
  expect_false(SafeShiny:::.safeShinyConsoleActive(s))
  SafeShiny:::.safeShinyConsoleStart(s)
  expect_true(SafeShiny:::.safeShinyConsoleActive(s))
  cat("hello console\n")
  message("a message")
  lines <- SafeShiny:::.safeShinyConsoleGet(s)
  expect_true("hello console" %in% lines)
  expect_true("a message" %in% lines)
  SafeShiny:::.safeShinyConsoleClear(s)
  expect_length(SafeShiny:::.safeShinyConsoleGet(s), 0)
  cat("after clear\n")
  expect_equal(SafeShiny:::.safeShinyConsoleGet(s), "after clear")
  SafeShiny:::.safeShinyConsoleStop(s)
  expect_false(SafeShiny:::.safeShinyConsoleActive(s))
  expect_equal(sink.number(), n0)
  expect_length(SafeShiny:::.safeShinyConsoleGet(s), 0)
})

test_that("tab height: percentages are viewport shares, numbers are pixels, junk errors", {
  f <- SafeShiny:::.safeShinyCssHeight
  expect_equal(f("60%"), "60vh")
  expect_equal(f("500px"), "500px")
  expect_equal(f(400), "400px")
  expect_error(f(NULL))
  expect_match(as.character(SafeShinyTabUI("x", height = "45%")), "max-height: 45vh", fixed = TRUE)
  expect_match(as.character(SafeShinyTabPanel("x", height = 300)), "max-height: 300px", fixed = TRUE)
})

test_that("console output with invalid UTF-8 is sanitised and a huge log is read from the tail", {
  s <- list(token = paste0("con2-", as.numeric(Sys.time())))
  SafeShiny:::.safeShinyConsoleStart(s)
  on.exit(SafeShiny:::.safeShinyConsoleStop(s), add = TRUE)
  cat(rawToChar(as.raw(c(0x61, 0xff, 0x62))), "\n")
  lines <- SafeShiny:::.safeShinyConsoleGet(s)
  expect_true(all(validUTF8(lines)))
  expect_match(lines[1], "a<ff>b", fixed = TRUE)
  SafeShiny:::.safeShinyConsoleClear(s)
  for (i in 1:200) cat("line number", i, "\n")
  tail <- SafeShiny:::.safeShinyConsoleGet(s, maxBytes = 200)
  expect_lt(length(tail), 20)
  expect_match(tail[length(tail)], "line number 200")
  expect_true(all(grepl("^line number", tail)))  # the cut first line is dropped
})

test_that("trim ignores tiny calls (e.g. tab-shown observers) when finding the window edges", {
  s <- list(token = paste0("trim2-", as.numeric(Sys.time())))
  rec <- function(label, sleep) {
    tok <- SafeShiny:::.startSafeShinyTiming(s, label, "observe")
    Sys.sleep(sleep)
    SafeShiny:::.endSafeShinyTiming(s, tok, "ok")
  }
  rec("tab shown (before)", 0)
  Sys.sleep(0.4)
  rec("real work", 0.1)
  Sys.sleep(0.4)
  rec("tab shown (after)", 0)

  full <- SummarizeSafeShinyTiming(s, trim = TRUE)  # default trimMinTime = 0: every call counts
  trimmed <- SummarizeSafeShinyTiming(s, trim = TRUE, trimMinTime = 0.01)
  expect_gt(full$wall_clock_elapsed, 0.85)
  expect_equal(trimmed$wall_clock_elapsed, 0.1, tolerance = 0.5)
  expect_lt(trimmed$untracked_time, 0.05)

  xmax <- function(w) as.numeric(sub('.*"xmax":([0-9.]+).*', "\\1", as.character(w)))
  expect_lt(xmax(PlotSafeShinyFlameHTML(s, trim = TRUE, trimMinTime = 0.01)), 0.3)
  expect_gt(xmax(PlotSafeShinyFlameHTML(s, trim = TRUE)), 0.85)
  expect_false(grepl("tab shown", as.character(PlotSafeShinyFlameHTML(s, trim = TRUE, trimMinTime = 0.01)), fixed = TRUE))
  ResetSafeShinyTiming(s)
})
