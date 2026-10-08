test_that("GetSafeShinyTiming/SummarizeSafeShinyTiming return empty results when nothing tracked", {
  fake <- list(token = paste0("empty-", as.numeric(Sys.time())))
  ResetSafeShinyTiming(session = fake)

  timing <- GetSafeShinyTiming(session = fake)
  expect_equal(nrow(timing), 0)
  expect_named(timing, c("label", "n", "total_time", "mean_time", "last_time"))

  summ <- SummarizeSafeShinyTiming(session = fake)
  expect_s3_class(summ, "SafeShinyTimingSummary")
  expect_equal(summ$total_tracked_time, 0)
  expect_true(is.na(summ$wall_clock_elapsed))
})

test_that("timing store does not leak between different session tokens", {
  fake1 <- list(token = "sess-aaa")
  fake2 <- list(token = "sess-bbb")
  ResetSafeShinyTiming(session = fake1)
  ResetSafeShinyTiming(session = fake2)

  SafeShiny:::.recordSafeShinyTiming(domain = fake1, label = "x", elapsed = 0.5, status = "ok")
  SafeShiny:::.recordSafeShinyTiming(domain = fake2, label = "y", elapsed = 0.2, status = "ok")

  t1 <- GetSafeShinyTiming(session = fake1)
  t2 <- GetSafeShinyTiming(session = fake2)

  expect_equal(t1$label, "x")
  expect_equal(t2$label, "y")
})

test_that("GetSafeShinyTiming aggregates multiple calls to the same label correctly", {
  fake <- list(token = paste0("agg-", as.numeric(Sys.time())))
  ResetSafeShinyTiming(session = fake)

  SafeShiny:::.recordSafeShinyTiming(domain = fake, label = "a", elapsed = 0.1, status = "ok")
  SafeShiny:::.recordSafeShinyTiming(domain = fake, label = "a", elapsed = 0.3, status = "ok")
  SafeShiny:::.recordSafeShinyTiming(domain = fake, label = "b", elapsed = 1.0, status = "error")

  timing <- GetSafeShinyTiming(session = fake)
  a <- timing[timing$label == "a", ]
  b <- timing[timing$label == "b", ]

  expect_equal(a$n, 2)
  expect_equal(a$total_time, 0.4)
  expect_equal(a$mean_time, 0.2)
  expect_equal(b$n, 1)
  expect_equal(b$total_time, 1.0)

  # "error" status calls still count toward total_time - real time was spent either way.
  expect_equal(sum(timing$total_time), 1.4)

  # sorted by total_time descending
  expect_equal(timing$label[1], "b")
})

test_that("GetSafeShinyTimingRaw preserves per-call status and one row per call", {
  fake <- list(token = paste0("raw-", as.numeric(Sys.time())))
  ResetSafeShinyTiming(session = fake)

  SafeShiny:::.recordSafeShinyTiming(domain = fake, label = "a", elapsed = 0.1, status = "ok")
  SafeShiny:::.recordSafeShinyTiming(domain = fake, label = "a", elapsed = 0.2, status = "silent")

  raw <- GetSafeShinyTimingRaw(session = fake)
  expect_equal(nrow(raw), 2)
  expect_setequal(raw$status, c("ok", "silent"))
})

test_that("SummarizeSafeShinyTiming's untracked_time is wall-clock minus tracked total", {
  fake <- list(token = paste0("summary-", as.numeric(Sys.time())))
  ResetSafeShinyTiming(session = fake)

  SafeShiny:::.recordSafeShinyTiming(domain = fake, label = "a", elapsed = 0.05, status = "ok")
  Sys.sleep(0.1)
  SafeShiny:::.recordSafeShinyTiming(domain = fake, label = "a", elapsed = 0.05, status = "ok")

  summ <- SummarizeSafeShinyTiming(session = fake)
  expect_equal(summ$total_tracked_time, 0.1)
  expect_gte(summ$wall_clock_elapsed, 0.1)
  expect_equal(summ$untracked_time, summ$wall_clock_elapsed - summ$total_tracked_time)
  expect_gte(summ$untracked_time, 0)
})

test_that("ResetSafeShinyTiming clears only the targeted session's records", {
  fake1 <- list(token = "reset-aaa")
  fake2 <- list(token = "reset-bbb")
  ResetSafeShinyTiming(session = fake1)
  ResetSafeShinyTiming(session = fake2)

  SafeShiny:::.recordSafeShinyTiming(domain = fake1, label = "x", elapsed = 0.1, status = "ok")
  SafeShiny:::.recordSafeShinyTiming(domain = fake2, label = "y", elapsed = 0.1, status = "ok")

  ResetSafeShinyTiming(session = fake1)

  expect_equal(nrow(GetSafeShinyTiming(session = fake1)), 0)
  expect_equal(nrow(GetSafeShinyTiming(session = fake2)), 1)
})

test_that("print.SafeShinyTimingSummary prints without erroring and mentions the caveat", {
  fake <- list(token = paste0("print-", as.numeric(Sys.time())))
  ResetSafeShinyTiming(session = fake)
  SafeShiny:::.recordSafeShinyTiming(domain = fake, label = "a", elapsed = 0.1, status = "ok")

  summ <- SummarizeSafeShinyTiming(session = fake)
  expect_output(print(summ), "untracked time")
})

test_that("nested tracked calls record parent/depth/start", {
  fake <- list(token = paste0("nest-", as.numeric(Sys.time())))
  ResetSafeShinyTiming(session = fake)

  a <- SafeShiny:::.startSafeShinyTiming(fake, "a")
  b <- SafeShiny:::.startSafeShinyTiming(fake, "b")
  Sys.sleep(0.01)
  SafeShiny:::.endSafeShinyTiming(fake, b, "ok")
  c <- SafeShiny:::.startSafeShinyTiming(fake, "c")
  SafeShiny:::.endSafeShinyTiming(fake, c, "error")
  SafeShiny:::.endSafeShinyTiming(fake, a, "ok")
  d <- SafeShiny:::.startSafeShinyTiming(fake, "d")
  SafeShiny:::.endSafeShinyTiming(fake, d, "ok")

  raw <- GetSafeShinyTimingRaw(session = fake)
  byLabel <- function(l) raw[raw$label == l, ]
  expect_equal(byLabel("a")$depth, 0L)
  expect_true(is.na(byLabel("a")$parent))
  expect_equal(byLabel("b")$parent, byLabel("a")$id)
  expect_equal(byLabel("c")$parent, byLabel("a")$id)
  expect_equal(byLabel("b")$depth, 1L)
  expect_equal(byLabel("d")$depth, 0L)
  expect_true(byLabel("a")$start <= byLabel("b")$start)
  expect_true(byLabel("a")$elapsed >= byLabel("b")$elapsed)
})

test_that("ending an outer call drops unfinished inner calls from the stack", {
  fake <- list(token = paste0("unbal-", as.numeric(Sys.time())))
  ResetSafeShinyTiming(session = fake)
  a <- SafeShiny:::.startSafeShinyTiming(fake, "a")
  SafeShiny:::.startSafeShinyTiming(fake, "never-finished")
  SafeShiny:::.endSafeShinyTiming(fake, a, "ok")
  e <- SafeShiny:::.startSafeShinyTiming(fake, "e")
  expect_equal(e$depth, 0L)
})

test_that("SafeObserve nesting a SafeReactive records parent/child", {
  shiny::testServer(function(input, output, session) {
    ResetSafeShinyTiming(session = session)
    r <- SafeReactive({ Sys.sleep(0.005); 1 }, trackTime = TRUE, label = "inner")
    SafeObserve({ r() }, trackTime = TRUE, label = "outer")
    session$userData$done <- TRUE
  }, {
    settle(session)
    raw <- GetSafeShinyTimingRaw(session = session)
    expect_equal(raw$depth[raw$label == "inner"], 1L)
    expect_equal(raw$parent[raw$label == "inner"], raw$id[raw$label == "outer"])
    expect_equal(raw$depth[raw$label == "outer"], 0L)
  })
})

test_that("PlotSafeShinyFlame draws and returns data, and is quiet when empty", {
  fake <- list(token = paste0("plot-", as.numeric(Sys.time())))
  ResetSafeShinyTiming(session = fake)
  expect_message(expect_null(PlotSafeShinyFlame(session = fake)), "No tracked calls")

  a <- SafeShiny:::.startSafeShinyTiming(fake, "a")
  b <- SafeShiny:::.startSafeShinyTiming(fake, "b")
  SafeShiny:::.endSafeShinyTiming(fake, b, "silent")
  SafeShiny:::.endSafeShinyTiming(fake, a, "ok")
  pdf(NULL)
  on.exit(dev.off())
  out <- PlotSafeShinyFlame(session = fake)
  expect_equal(nrow(out), 2)
  expect_true(all(c("t0", "t1") %in% names(out)))
})

test_that("SummarizeSafeShinyTiming does not double-count nested calls", {
  fake <- list(token = paste0("sumnest-", as.numeric(Sys.time())))
  ResetSafeShinyTiming(session = fake)
  a <- SafeShiny:::.startSafeShinyTiming(fake, "a")
  b <- SafeShiny:::.startSafeShinyTiming(fake, "b")
  Sys.sleep(0.02)
  SafeShiny:::.endSafeShinyTiming(fake, b, "ok")
  SafeShiny:::.endSafeShinyTiming(fake, a, "ok")

  raw <- GetSafeShinyTimingRaw(session = fake)
  summ <- SummarizeSafeShinyTiming(session = fake)
  expect_equal(summ$total_tracked_time, raw$elapsed[raw$label == "a"])
})

test_that("trackTime defaults to getOption('SafeShiny.trackTime', FALSE); explicit value wins", {
  run <- function(...) {
    out <- NULL
    shiny::testServer(function(input, output, session) {
      ResetSafeShinyTiming(session = session)
      SafeObserve({ 1 + 1 }, label = "default", ...)
      SafeReactive({ 1 + 1 }, label = "react_default", ...)
    }, expr = {
      settle(session)
      out <<- GetSafeShinyTiming(session = session)$label
    })
    out
  }

  withr_old <- options(SafeShiny.trackTime = NULL)
  on.exit(options(withr_old), add = TRUE)

  expect_length(run(), 0)                       # option unset: not tracked

  options(SafeShiny.trackTime = TRUE)
  expect_equal(run(), "default")                # option set: observer tracked (reactive never read)
  expect_length(run(trackTime = FALSE), 0)      # explicit FALSE wins over the option
})
