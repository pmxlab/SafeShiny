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
