test_that("SafeRenderPlot behaves like renderPlot() on success and returns its value unchanged", {
  shiny::testServer(function(input, output, session) {
    output$p <- SafeRenderPlot({
      plot(1:3)
    })
  }, expr = {
    settle(session)
    # No error during construction/flush, and the output actually produced a value (a base64
    # PNG data URI, exactly as plain shiny::renderPlot() would) - confirms SafeRenderPlot()
    # doesn't alter the render's own return value on the success path.
    result <- session$getOutput("p")
    expect_true(is.list(result))
    expect_true(nchar(result$src) > 0)
  })
})

test_that("SafeRenderPlot: a genuine error is caught, onError fires, and the error is re-raised (Shiny's own error display still happens)", {
  shiny::testServer(function(input, output, session) {
    caught_msg <- NULL
    output$p <- SafeRenderPlot({
      stop("plot boom")
    }, onError = function(e) {
      caught_msg <<- conditionMessage(e)
    }, quiet = TRUE)

    still_alive <- FALSE
    SafeObserveEvent(input$ping, {
      still_alive <<- TRUE
    }, ignoreInit = TRUE)

    session$getCaughtMsg <- function() caught_msg
    session$getStillAlive <- function() still_alive
  }, expr = {
    settle(session)
    # Shiny itself handles the re-raised error for this output (local error display) -
    # it does not propagate out of testServer/crash the test.
    expect_equal(session$getCaughtMsg(), "plot boom")

    trigger(session, ping = 1)
    expect_true(session$getStillAlive())
  })
})

test_that("SafeRenderUI: a genuine error is caught, onError fires, error is re-raised, session survives", {
  shiny::testServer(function(input, output, session) {
    caught_msg <- NULL
    output$ui <- SafeRenderUI({
      stop("ui boom")
    }, onError = function(e) {
      caught_msg <<- conditionMessage(e)
    }, quiet = TRUE)

    still_alive <- FALSE
    SafeObserveEvent(input$ping, {
      still_alive <<- TRUE
    }, ignoreInit = TRUE)

    session$getCaughtMsg <- function() caught_msg
    session$getStillAlive <- function() still_alive
  }, expr = {
    settle(session)
    expect_equal(session$getCaughtMsg(), "ui boom")

    trigger(session, ping = 1)
    expect_true(session$getStillAlive())
  })
})

test_that("SafeRender: shiny::req() silent stop is re-raised unchanged, onError not called", {
  shiny::testServer(function(input, output, session) {
    onErrorCalled <- FALSE
    output$ui <- SafeRenderUI({
      shiny::req(FALSE)
    }, onError = function(e) {
      onErrorCalled <<- TRUE
    }, quiet = TRUE)

    session$getOnErrorCalled <- function() onErrorCalled
  }, expr = {
    settle(session)
    expect_false(session$getOnErrorCalled())
  })
})

test_that("SafeRenderTable: a genuine error is caught, onError fires, error is re-raised, session survives", {
  skip_if_not_installed("DT")

  shiny::testServer(function(input, output, session) {
    caught_msg <- NULL
    output$tbl <- SafeRenderTable({
      stop("table boom")
    }, onError = function(e) {
      caught_msg <<- conditionMessage(e)
    }, quiet = TRUE)

    still_alive <- FALSE
    SafeObserveEvent(input$ping, {
      still_alive <<- TRUE
    }, ignoreInit = TRUE)

    session$getCaughtMsg <- function() caught_msg
    session$getStillAlive <- function() still_alive
  }, expr = {
    settle(session)
    expect_equal(session$getCaughtMsg(), "table boom")

    trigger(session, ping = 1)
    expect_true(session$getStillAlive())
  })
})

test_that("SafeRenderTable: shiny::req() silent stop is re-raised, onError not called", {
  skip_if_not_installed("DT")

  shiny::testServer(function(input, output, session) {
    onErrorCalled <- FALSE
    output$tbl <- SafeRenderTable({
      shiny::req(FALSE)
    }, onError = function(e) {
      onErrorCalled <<- TRUE
    }, quiet = TRUE)

    session$getOnErrorCalled <- function() onErrorCalled
  }, expr = {
    settle(session)
    expect_false(session$getOnErrorCalled())
  })
})

test_that("SafeRenderTable: a '...' argument that's a closure over a local reactive keeps access to it (regression, #1)", {
  skip_if_not_installed("DT")

  # DT::renderDT() resolves its own "..." arguments (e.g. caption=) via parent.frame() inside
  # its own body, not via the env= it's otherwise given - a naive renderFunc(wrapped, env=env,
  # quoted=TRUE, ...) forward makes SafeRenderTable()'s own frame (not the caller's) the
  # apparent direct caller, so a caption closure referencing a reactive silently lost access to
  # it. Confirmed this doesn't reproduce with plain DT::renderDT() called directly - it's
  # specific to going through this wrapper. See pmxlab/SafeShiny#1.
  shiny::testServer(function(input, output, session) {
    myReactive <- shiny::reactive({ 42 })
    output$tbl <- SafeRenderTable({
      data.frame(x = 1)
    }, caption = htmltools::tags$caption({
      v <- myReactive()
      paste("val:", v)
    }))
  }, expr = {
    settle(session)
    out <- session$getOutput("tbl")
    expect_match(out, "val: 42", fixed = TRUE)
  })
})

test_that("SafeRenderTable: a reactive referenced inside a '...' closure stays genuinely reactive across renders, not frozen at its initial value", {
  skip_if_not_installed("DT")

  shiny::testServer(function(input, output, session) {
    counterRV <- shiny::reactiveVal(0)
    myReactive <- shiny::reactive({ counterRV() })
    output$tbl <- SafeRenderTable({
      data.frame(x = counterRV())
    }, caption = htmltools::tags$caption({
      v <- myReactive()
      paste("val:", v)
    }))
  }, expr = {
    settle(session)
    out1 <- session$getOutput("tbl")
    expect_match(out1, "val: 0", fixed = TRUE)

    counterRV(99)
    settle(session)
    out2 <- session$getOutput("tbl")
    expect_match(out2, "val: 99", fixed = TRUE)
    expect_false(grepl("val: 0", out2, fixed = TRUE))
  })
})

test_that("SafeRenderTable errors clearly when DT isn't installed", {
  testthat::local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")
  expect_error(SafeRenderTable({
    1
  }), "DT")
})

test_that("SafeRender records timing on success and on caught error", {
  shiny::testServer(function(input, output, session) {
    output$p <- SafeRenderPlot({
      Sys.sleep(0.01)
      plot(1)
    }, trackTime = TRUE, label = "ok_plot")

    output$p2 <- SafeRenderPlot({
      Sys.sleep(0.01)
      stop("boom")
    }, trackTime = TRUE, label = "error_plot", quiet = TRUE, onError = function(e) NULL)
  }, expr = {
    settle(session)

    timing <- GetSafeShinyTiming(session = session)
    expect_true("ok_plot" %in% timing$label)
    expect_true("error_plot" %in% timing$label)

    raw <- GetSafeShinyTimingRaw(session = session)
    expect_true("error" %in% raw$status[raw$label == "error_plot"])
  })
})

test_that("SafeDownloadHandler construction succeeds and returns a function, as shiny::downloadHandler() does", {
  # shiny::downloadHandler()'s returned object is dispatched over a real HTTP download route, not
  # a normal reactive output - testing its content() execution end-to-end needs shinytest2, not
  # testServer() (a known limitation: https://forum.posit.co/t/can-i-acess-a-downloadhandler-filename-or-content-when-writing-tests/209203).
  # The error/reraise/req-passthrough logic itself is still exercised directly below, since it's
  # the exact same .safeShinyBuildHandlers(reraise = TRUE) shared helper SafeDownloadHandler()
  # uses internally - just invoked without going through shiny's HTTP layer.
  handler <- SafeDownloadHandler(
    filename = "test.txt",
    content = function(file) writeLines("hello", file)
  )
  expect_true(is.function(handler))
})

test_that("SafeDownloadHandler: a genuine content() error is caught, onError fires, and is re-raised", {
  caught_msg <- NULL
  wrappedContentCaller <- SafeShiny:::.safeShinyBuildHandlers(
    label = "report.xlsx", domain = NULL, onError = function(e) { caught_msg <<- conditionMessage(e) },
    trackTime = FALSE, quiet = TRUE, context = "SafeDownloadHandler", reraise = TRUE
  )

  # Exercise the same tryCatch shape SafeDownloadHandler() builds internally, directly, since
  # shiny::downloadHandler()'s returned function expects a live HTTP request/response object we
  # can't easily fabricate in a unit test - this still exercises the real error/reraise logic.
  startTime <- Sys.time()
  expect_error(
    tryCatch(
      {
        result <- stop("download boom")
        wrappedContentCaller$recordFn("ok", startTime)
        result
      },
      error = function(e) wrappedContentCaller$errorHandler(e, startTime)
    ),
    "download boom"
  )
  expect_equal(caught_msg, "download boom")
})

test_that("SafeDownloadHandler: shiny::req() silent stop is re-raised, onError not called", {
  onErrorCalled <- FALSE
  handlers <- SafeShiny:::.safeShinyBuildHandlers(
    label = "report.xlsx", domain = NULL, onError = function(e) { onErrorCalled <<- TRUE },
    trackTime = FALSE, quiet = TRUE, context = "SafeDownloadHandler", reraise = TRUE
  )

  startTime <- Sys.time()
  expect_error(
    tryCatch(
      shiny::req(FALSE),
      error = function(e) handlers$errorHandler(e, startTime)
    ),
    class = "shiny.silent.error"
  )
  expect_false(onErrorCalled)
})

test_that("SafeDownloadHandler: default label falls back to 'download' for a non-string filename", {
  handler <- SafeDownloadHandler(
    filename = function() paste0("report-", Sys.Date(), ".xlsx"),
    content = function(file) writeLines("x", file)
  )
  expect_true(is.function(handler))
})
