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
