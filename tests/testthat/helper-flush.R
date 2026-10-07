# shiny::testServer()'s mock session needs one initial flush after the server function runs
# for its observers to "settle" (register their baseline dependency state) before any
# subsequent change can be detected correctly - without it, even a plain shiny::observeEvent()
# never sees a later input/reactiveVal change. settle() does that one required flush; call it
# first, before touching any input or trigger. trigger() sets a value and flushes once more,
# for every change after that.

settle <- function(session) {
  session$flushReact()
}

trigger <- function(session, ...) {
  session$setInputs(...)
  session$flushReact()
}
