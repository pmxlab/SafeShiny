test_that("SafeObserve behaves like observe() on success", {
  shiny::testServer(function(input, output, session) {
    ran <- FALSE
    SafeObserve({
      ran <<- TRUE
    })
    session$getRan <- function() ran
  }, expr = {
    settle(session)
    expect_true(session$getRan())
  })
})

test_that("SafeObserve catches an error, calls onError, and the session keeps working", {
  shiny::testServer(function(input, output, session) {
    caught_msg <- NULL
    SafeObserve({
      stop("boom")
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
    expect_equal(session$getCaughtMsg(), "boom")

    trigger(session, ping = 1)
    expect_true(session$getStillAlive())
  })
})

test_that("SafeObserveEvent catches an error in the handler without breaking the session", {
  shiny::testServer(function(input, output, session) {
    caught <- FALSE
    SafeObserveEvent(input$boom, {
      stop("deliberate error")
    }, onError = function(e) {
      caught <<- TRUE
    }, quiet = TRUE, ignoreInit = TRUE)

    counter <- 0
    SafeObserveEvent(input$safe, {
      counter <<- counter + 1
    }, ignoreInit = TRUE)

    session$getCaught <- function() caught
    session$getCounter <- function() counter
  }, expr = {
    settle(session)

    trigger(session, boom = 1)
    expect_true(session$getCaught())

    trigger(session, safe = 1)
    expect_equal(session$getCounter(), 1)
    trigger(session, safe = 2)
    expect_equal(session$getCounter(), 2)
  })
})

test_that("SafeObserveEvent does not wrap eventExpr, only handlerExpr", {
  shiny::testServer(function(input, output, session) {
    handler_ran <- FALSE
    # eventExpr itself is a plain value read - it should never go through error handling,
    # only handlerExpr should.
    SafeObserveEvent(input$go, {
      handler_ran <<- TRUE
    }, ignoreInit = TRUE)
    session$getHandlerRan <- function() handler_ran
  }, expr = {
    settle(session)
    trigger(session, go = 1)
    expect_true(session$getHandlerRan())
  })
})

test_that("shiny::req() silent stop is re-raised unchanged, not treated as an error", {
  shiny::testServer(function(input, output, session) {
    onErrorCalled <- FALSE
    SafeObserveEvent(input$trig, {
      shiny::req(FALSE)
    }, onError = function(e) {
      onErrorCalled <<- TRUE
    }, quiet = TRUE, ignoreInit = TRUE)

    counter <- 0
    SafeObserveEvent(input$safe, {
      counter <<- counter + 1
    }, ignoreInit = TRUE)

    session$getOnErrorCalled <- function() onErrorCalled
    session$getCounter <- function() counter
  }, expr = {
    settle(session)

    trigger(session, trig = 1)
    expect_false(session$getOnErrorCalled())

    trigger(session, safe = 1)
    expect_equal(session$getCounter(), 1)
  })
})

test_that("shiny::validate() silent stop is re-raised unchanged in SafeObserve too", {
  shiny::testServer(function(input, output, session) {
    onErrorCalled <- FALSE
    SafeObserve({
      shiny::validate(shiny::need(FALSE, "nope"))
    }, onError = function(e) {
      onErrorCalled <<- TRUE
    }, quiet = TRUE)
    session$getOnErrorCalled <- function() onErrorCalled
  }, expr = {
    settle(session)
    expect_false(session$getOnErrorCalled())
  })
})

test_that("default label is a truncated deparse of the wrapped expression, used for timing", {
  shiny::testServer(function(input, output, session) {
    SafeObserve({
      1 + 1
    }, trackTime = TRUE)
  }, expr = {
    settle(session)
    timing <- GetSafeShinyTiming(session = session)
    expect_equal(nrow(timing), 1)
    expect_true(nchar(timing$label[1]) > 0)
  })
})

test_that("quiet = TRUE suppresses the default error message; quiet = FALSE (default) emits one", {
  expect_message(
    shiny::testServer(function(input, output, session) {
      SafeObserve({
        stop("loud failure")
      })
    }, expr = {
      settle(session)
    }),
    "loud failure"
  )

  expect_no_message(
    shiny::testServer(function(input, output, session) {
      SafeObserve({
        stop("silent failure")
      }, quiet = TRUE)
    }, expr = {
      settle(session)
    })
  )
})

test_that("SafeObserveEvent also contains an error in the event expression (a throwing reactive)", {
  shiny::testServer(function(input, output, session) {
    caught <- NULL
    handler_runs <- 0
    boom <- shiny::reactive({
      if (isTRUE(input$explode)) stop("event reactive failed")
      input$go
    })
    SafeObserveEvent(boom(), {
      handler_runs <<- handler_runs + 1
    }, onError = function(e) caught <<- conditionMessage(e), quiet = TRUE, ignoreInit = TRUE)

    other <- 0
    SafeObserveEvent(input$other, {
      other <<- other + 1
    }, ignoreInit = TRUE)
    session$getCaught <- function() caught
    session$getHandlerRuns <- function() handler_runs
    session$getOther <- function() other
  }, expr = {
    settle(session)
    trigger(session, go = 1)
    expect_equal(session$getHandlerRuns(), 1)  # normal operation unchanged

    # no error escapes (an unhandled observer error would fail the test via testServer)
    trigger(session, explode = TRUE, go = 2)
    expect_equal(session$getCaught(), "event reactive failed")
    expect_equal(session$getHandlerRuns(), 1)  # handler not run
    expect_equal(nrow(GetSafeShinyErrors(session)), 1)

    trigger(session, other = 1)  # session still alive
    expect_equal(session$getOther(), 1)
  })
})

test_that("SafeObserveEvent: req() in the event expression is still a silent stop, not an error", {
  shiny::testServer(function(input, output, session) {
    onErrorCalled <- FALSE
    runs <- 0
    SafeObserveEvent({ shiny::req(input$go > 1); input$go }, {
      runs <<- runs + 1
    }, onError = function(e) onErrorCalled <<- TRUE, quiet = TRUE)
    session$getOnErrorCalled <- function() onErrorCalled
    session$getRuns <- function() runs
  }, expr = {
    settle(session)
    trigger(session, go = 1)
    expect_false(session$getOnErrorCalled())
    expect_equal(session$getRuns(), 0)
    trigger(session, go = 2)
    expect_equal(session$getRuns(), 1)
  })
})

test_that("SafeObserveEvent tracks event evaluation as '<label> (event)', nesting reactives, only when interesting", {
  shiny::testServer(function(input, output, session) {
    ResetSafeShinyTiming(session = session)
    ev <- SafeReactive({ Sys.sleep(0.02); input$go }, label = "ev", trackTime = TRUE)
    SafeObserveEvent(ev(), { Sys.sleep(0.01) }, label = "obs", trackTime = TRUE, ignoreInit = TRUE)
    SafeObserveEvent(input$plain, { Sys.sleep(0.01) }, label = "plainobs", trackTime = TRUE,
                     ignoreInit = TRUE)
    boom <- SafeReactive({ if (isTRUE(input$explode)) stop("bad event"); input$go2 },
                         label = "boom", trackTime = TRUE)
    SafeObserveEvent(boom(), { }, label = "failobs", trackTime = TRUE, quiet = TRUE,
                     ignoreInit = TRUE)
  }, expr = {
    settle(session)
    ResetSafeShinyTiming(session = session)  # drop the initial evaluation
    trigger(session, go = 1)
    raw <- GetSafeShinyTimingRaw(session)
    evRow <- raw[raw$label == "obs (event)", ]
    expect_equal(nrow(evRow), 1)
    expect_equal(evRow$depth, 0)
    expect_equal(raw$parent[raw$label == "ev"], evRow$id)          # reactive nested under the event
    expect_equal(raw$depth[raw$label == "ev"], 1)
    expect_equal(raw$depth[raw$label == "obs"], 0)                  # handler is a sibling
    expect_gte(evRow$elapsed, 0.02)

    ResetSafeShinyTiming(session = session)
    trigger(session, plain = 1)                                      # trivial event: no "(event)" row
    raw <- GetSafeShinyTimingRaw(session)
    expect_true("plainobs" %in% raw$label)
    expect_false("plainobs (event)" %in% raw$label)

    ResetSafeShinyTiming(session = session)
    trigger(session, explode = TRUE, go2 = 1)                        # failing event
    raw <- GetSafeShinyTimingRaw(session)
    failRow <- raw[raw$label == "failobs (event)", ]
    expect_equal(failRow$status, "error")
    expect_false("failobs" %in% raw$label)                           # handler never ran
    # call stack is clean afterwards: a new top-level call has depth 0
    trigger(session, plain = 2)
    raw <- GetSafeShinyTimingRaw(session)
    expect_equal(tail(raw$depth[raw$label == "plainobs"], 1), 0)
  })
})
