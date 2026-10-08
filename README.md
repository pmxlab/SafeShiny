# SafeShiny

An uncaught error inside a plain `shiny::observe()`/`shiny::observeEvent()` terminates the whole
Shiny session for that user - unlike an error inside a `reactive()` consumed only by a render
output, which Shiny already converts into a localized, session-surviving error display on its
own. SafeShiny closes that gap for observers, and adds error hooks and timing for reactives,
renders and downloads.

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

The remaining wrappers - `SafeReactive()`, `SafeRender()`/`SafeRenderPlot()`/`SafeRenderUI()`/
`SafeRenderTable()` and `SafeDownloadHandler()` - are different: Shiny already contains errors
there, so they do not swallow anything. They catch, call an optional `onError()` hook (e.g. to
roll the failure into application status bookkeeping), and re-raise the original error unchanged.
They support `trackTime = TRUE` too; `SafeReactive()` extends timing coverage to the computation
layer, where most real compute time lives.

See `vignette("SafeShiny", package = "SafeShiny")` for the full write-up of the underlying Shiny
error-handling mechanism and worked examples.

## License

MIT
