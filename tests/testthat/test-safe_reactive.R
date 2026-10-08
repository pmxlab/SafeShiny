test_that("SafeReactive returns the value unchanged and caches like reactive()", {
  shiny::testServer(function(input, output, session) {
    n_calls <- 0
    r <- SafeReactive({
      n_calls <<- n_calls + 1
      input$x * 2
    })
    session$getR <- function() r()
    session$getCalls <- function() n_calls
  }, expr = {
    session$setInputs(x = 3)
    expect_equal(session$getR(), 6)
    expect_equal(session$getR(), 6)
    expect_equal(session$getCalls(), 1)
  })
})

test_that("SafeReactive: error fires onError once, is re-raised, and cached error does not refire", {
  shiny::testServer(function(input, output, session) {
    n_hook <- 0
    r <- SafeReactive({
      stop("react boom")
    }, onError = function(e) n_hook <<- n_hook + 1, quiet = TRUE)
    session$tryR <- function() tryCatch(r(), error = function(e) conditionMessage(e))
    session$getHook <- function() n_hook
  }, expr = {
    expect_equal(session$tryR(), "react boom")
    expect_equal(session$tryR(), "react boom")
    expect_equal(session$getHook(), 1)
  })
})

test_that("SafeReactive: req() silent stop is re-raised, onError not called", {
  shiny::testServer(function(input, output, session) {
    called <- FALSE
    r <- SafeReactive({
      shiny::req(FALSE)
    }, onError = function(e) called <<- TRUE, quiet = TRUE)
    session$tryR <- function() tryCatch(r(), shiny.silent.error = function(e) "silent")
    session$getCalled <- function() called
  }, expr = {
    expect_equal(session$tryR(), "silent")
    expect_false(session$getCalled())
  })
})

test_that("SafeReactive: session survives a failing reactive consumed by an output", {
  shiny::testServer(function(input, output, session) {
    r <- SafeReactive(stop("boom"), quiet = TRUE)
    output$o <- shiny::renderText(r())
    alive <- FALSE
    SafeObserveEvent(input$ping, alive <<- TRUE, ignoreInit = TRUE)
    session$getAlive <- function() alive
  }, expr = {
    settle(session)
    trigger(session, ping = 1)
    expect_true(session$getAlive())
  })
})
