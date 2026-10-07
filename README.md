# SafeShiny

An uncaught error inside a plain `shiny::observe()`/`shiny::observeEvent()` terminates the whole
Shiny session for that user - unlike an error inside a `reactive()` consumed only by a render
output, which Shiny already converts into a localized, session-surviving error display on its
own. SafeShiny closes that gap for observers.

```r
# install.packages("remotes")
remotes::install_github("pmxlab/SafeShiny")
```

```r
library(shiny)
library(SafeShiny)

server <- function(input, output, session) {
  SafeObserveEvent(input$go, {
    result <- 1 / input$denominator
    showNotification(paste("Result:", result))
  }, onError = function(e) {
    showNotification(paste("Something went wrong:", conditionMessage(e)), type = "error")
  })
}
```

`SafeObserve()` and `SafeObserveEvent()` are drop-in replacements for `shiny::observe()`/
`shiny::observeEvent()`: same arguments, same behavior on success, but a genuine error is caught
and reported instead of crashing the session - while `shiny::req()`/`shiny::validate()`'s
intentional silent-stop conditions are still re-raised unchanged, exactly as with the originals.

Both also support optional execution-time tracking (`trackTime = TRUE`), to help separate time
spent in your own business logic from time spent in Shiny's own reactive-graph maintenance - see
`GetSafeShinyTiming()` / `SummarizeSafeShinyTiming()`.

See `vignette("SafeShiny", package = "SafeShiny")` for the full write-up of the underlying Shiny
error-handling mechanism and worked examples.

## License

MIT
