details <- function(summary, ...) {
  tags$details(tags$summary(summary), ...)
}

alert <- function(..., status = "warning", class = "", style = "margin-bottom: 10px; padding: 10px 15px;") {
  class <- c(class, "alert", sprintf("alert-%s", status))
  tags$div(class = class, style = style, ...)
}

popover <- function(trigger, ..., class = "") {
  if (is.character(trigger)) {
    triggerId <- sprintf("%s-trigger", trigger)
    bodyId <- sprintf("%s-body", trigger)
    trigger <- NULL
  } else {
    randomId <- paste(sample(c(0:9, letters), 10, replace = T), collapse = "")
    triggerId <- sprintf("%s-trigger", randomId)
    bodyId <- sprintf("%s-body", randomId)
    trigger$attribs$id <- triggerId
  }

  timeout <- 1000
  js <- HTML(sprintf("setTimeout(function() {
    $('#%s').popover({
      html: true,
      content: $('#%s').detach().removeClass('hidden')
    })
  }, %s)", triggerId, bodyId, timeout))

  class <- c(class, "hidden")

  list(
    tags$script(type = "text/javascript", js),
    trigger,
    div(id = bodyId, class = class, ...))
}

toolbar <- function(..., class = "well") {
  css <- HTML(".toolbar > * { margin-right: 15px; }",
              ".toolbar > .form-group { width: auto; }",
              ".toolbar > .form-group > div { display: inline-block; vertical-align: middle; }",
              ".toolbar .selectize-control { height: 34px; min-width: 150px; margin-bottom: 0; }",
              ".toolbar .form-vertical .form-group { display: block; margin-bottom: 15px; }",
              ".toolbar .form-vertical .colourpicker-input-container { background: white; }")
  class <- c(class, "toolbar form-inline")
  list(tags$style(type = "text/css", css),
       tags$div(class = class, ...))
}
