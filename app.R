library(dplyr)
library(purrr)
library(survival)
library(shiny)
library(datamods)
library(colourpicker)
library(DT)
library(gt)
library(gtsummary)
library(ggsurvfit)

source("misc.R")

rules <- validate::validator(!is.na(time), !is.na(status), !is.na(group),
                             if (!is.na(time)) time >= 0,
                             if (!is.na(status)) status %in% c(0,1))
validate::label(rules) <- c("Time not empty", "Status not empty", "Group not empty",
                            "Time ≥  0",
                            "Status 0 or 1")

ui <- fixedPage(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    toolbar(actionButton("importButton", "Import data"),
            varSelectInput("varTime",   "Time",   data = NULL),
            varSelectInput("varStatus", "Status", data = NULL),
            varSelectInput("varGroup",  "Group",  data = NULL),
            checkboxInput("addCensorMark", "Add censoring?"),
            checkboxInput("addConfInt", "Add 95% CI?")),
    div(div(uiOutput("warningsUI")),
        div(class = "position-relative",
            plotOutput("survPlot", height = "auto"),
            popover(tags$span(class = "btn btn-default btn-xs position-absolute top-0 right-0", icon("gear"), "Options"),
                    numericInput("size", "Font size", 16),
                    numericInput("width",  "Width",  800),
                    numericInput("height", "Height", 570),
                    textInput("xlab", "X label", "Time"),
                    textInput("ylab", "Y label", "Probability"),
                    tags$label("Palette"),
                    uiOutput("palInput"),
                    sliderInput("alpha", "Opacity", 0, 1, .1, step = .01))),
        gt_output("propsTable"),
        gt_output("timesTable"),
        collapsible(tags$button(class = "btn btn-default btn-sm", icon("caret-down"), "Complete results"),
                    dataTableOutput("resultsTable"))))

server <- function(input, output, session) {
    observeEvent(input$importButton, {
        import_modal(id = "importModal", from = c("file", "copypaste", "googlesheets", "url", "env"))
    })

    import <- import_server("importModal", return_class = "tbl_df")

    data <- reactive({
        data <- import$data()
        if (is.null(data)) {
            return(with(aml, data.frame(time, status, group = x)))
        }
        data %>%
            na_if("")
    })

    observeEvent(data(), {
        data <- data()
        updateVarSelectInput(session, "varTime",   data = data, selected = "time")
        updateVarSelectInput(session, "varStatus", data = data, selected = "status")
        updateVarSelectInput(session, "varGroup",  data = data, selected = "group")
    })

    fit_data <- reactive({
        data <- data()
        varTime   <- input$varTime
        varStatus <- input$varStatus
        varGroup  <- input$varGroup
        validate(need(length(varTime)   > 0 && !is.null(data[[varTime]]),   label = "Time"),
                 need(length(varStatus) > 0 && !is.null(data[[varStatus]]), label = "Status"),
                 need(length(varGroup)  > 0 && !is.null(data[[varGroup]]),  label = "Group"))
        select(data, time = !!varTime, status = !!varStatus, group = !!varGroup)
    })

    validation <- validation_server("validation", fit_data, rules = rules)

    output$warningsUI <- renderUI({
        result <- validation$details()
        warnings <- keep(result, ~ .$status != "OK")
        map(warnings, ~ alert(HTML(.$label)))
    })

    output$palInput <- renderUI({
        fit_data <- fit_data()
        groups <- unique(na.omit(fit_data$group))
        pal <- scales::hue_pal()(length(groups))
        imap(groups, function(group, i) {
            inputId <- sprintf("pal%d", i)
            col <- pal[i]
            colourInput(inputId, NULL, col)
        })
    })

    pal <- reactive({
        fit_data <- fit_data()
        groups <- unique(na.omit(fit_data$group))
        pal <- imap(groups, function(group, i) {
            inputId <- sprintf("pal%d", i)
            input[[inputId]]
        })
        if (all(map_lgl(pal, is.null))) return()
        as.character(pal)
    })

    fit <- reactive(survfit2(Surv(time, status) ~ group, fit_data()))

    xbreaks <- reactive({
        time <- fit()$time
        scales::breaks_extended(5)(time)
    })

    size   <- reactive(input$size)
    width  <- reactive(input$width)
    height <- reactive(input$height)

    output$survPlot <- renderPlot({
        fit <- fit()

        addConfInt    <- input$addConfInt
        addCensorMark <- input$addCensorMark

        xlab <- input$xlab
        xbreaks <- xbreaks()

        ylab <- input$ylab
        ybreaks <- seq(0,1,.2)

        pal <- pal()
        alpha <- input$alpha

        ggsurvfit(fit, size = 1, theme = theme_classic(size())) +
            { if (addConfInt) add_confidence_interval(alpha = alpha) } +
            { if (addCensorMark) add_censor_mark(size = 4, stroke = 1) } +
            scale_ggsurvfit(x_scales = list(name = xlab, breaks = xbreaks),
                            y_scales = list(name = ylab, breaks = ybreaks)) +
            { if (!is.null(pal)) scale_color_manual(values = pal) } +
            theme(legend.position = "bottom",
                  legend.text = element_text(size = rel(1)))
    }, width = width, height = height)

    output$propsTable <- render_gt({
        fit_data <- fit_data()
        fit <- eval(rlang::expr(survfit(Surv(time, status) ~ group, !!fit_data)))
        fit %>%
            tbl_survfit(probs = .5) %>%
            add_n() %>%
            add_nevent() %>%
            add_p() %>%
            as_gt()
    })

    output$timesTable <- render_gt({
        xbreaks <- keep(xbreaks(), ~ . > 0)
        fit <- fit()
        fit %>%
            tbl_survfit(times = xbreaks, label = ~ "") %>%
            modify_table_body(. %>% filter(row_type == "level")) %>%
            as_gt()
    })

    output$resultsTable <- renderDataTable({
        fit <- fit()
        df <- fit %>%
            tidy_survfit() %>%
            { if (is.null(.[["strata"]])) mutate(., strata = "All") } %>%
            select(strata, time, n.risk, n.event, n.censor, estimate, std.error, conf.low, conf.high)
        colnames <- c("Group", "Time", "At risk", "Events", "Censored", "Survival", "SE", "95% CI low", "95% CI high")
        datatable(df,
                  colnames = colnames,
                  rownames = NULL,
                  selection = "none",
                  filter = "top",
                  extensions = "RowGroup",
                  options = list(dom = 't',
                                 scrollY = 500,
                                 info = F,
                                 paging = F,
                                 rowGroup = list(dataSrc = 0))) %>%
            formatRound(c("time"), 2) %>%
            formatPercentage(c("estimate", "std.error", "conf.low", "conf.high"), 2)
    })
}

shinyApp(ui = ui, server = server)
