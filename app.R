library(dplyr)
library(purrr)
library(survival)
library(shiny)
library(datamods)
library(colourpicker)
library(labelled)
library(gt)
library(gtsummary)
library(ggsurvfit)

source("misc.R")

ui <- fluidPage(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    toolbar(actionButton("importButton", "Import data"),
            varSelectInput("varTime",   "Time",   data = NULL),
            varSelectInput("varStatus", "Status", data = NULL),
            varSelectInput("varGroup",  "Group",  data = NULL),
            checkboxInput("showCensor", "Show censoring?")),
    div(class = "container",
        div(class = "position-relative",
            plotOutput("survPlot", height = "auto"),
            popover(tags$span(class = "btn btn-default btn-xs position-absolute top-0 right-0", icon("gear"), "Options"),
                    numericInput("width",  "Width",  800),
                    numericInput("height", "Height", 570),
                    textInput("xlab", "X label", "Time"),
                    textInput("ylab", "Y label", "Probability"),
                    tags$label("Palette"),
                    uiOutput("palInput"))),
        gt_output("propsTable"),
        gt_output("timesTable")))

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
            na_if("") %>%
            as_tibble(.name_repair = "universal_quiet") %>%
            set_variable_labels(.labels = colnames(data))
    })

    observeEvent(data(), {
        data <- data()
        updateVarSelectInput(session, "varTime",   data = data, selected = "time")
        updateVarSelectInput(session, "varStatus", data = data, selected = "status")
        updateVarSelectInput(session, "varGroup",  data = data, selected = "group")
    })

    output$palInput <- renderUI({
        data <- data()
        varGroup <- as.character(input$varGroup)
        validate(need(length(varGroup) > 0 && !is.null(data[[varGroup]]), label = "Group"))

        groups <- unique(na.omit(data[[varGroup]]))
        pal <- scales::hue_pal()(length(groups))
        imap(groups, function(group, i) {
            inputId <- sprintf("pal%d", i)
            col <- pal[i]
            colourInput(inputId, NULL, col)
        })
    })

    pal <- reactive({
        data <- data()
        varGroup <- as.character(input$varGroup)
        validate(need(length(varGroup) > 0 && !is.null(data[[varGroup]]), label = "Group"))

        groups <- unique(na.omit(data[[varGroup]]))
        pal <- imap(groups, function(group, i) {
            inputId <- sprintf("pal%d", i)
            input[[inputId]]
        })
        if (all(map_lgl(pal, is.null))) return()
        as.character(pal)
    })

    fla <- reactive({
        data <- data()
        varTime   <- as.character(input$varTime)
        varStatus <- as.character(input$varStatus)
        varGroup  <- as.character(input$varGroup)
        validate(need(length(varTime)   > 0 && !is.null(data[[varTime]]),   label = "Time"),
                 need(length(varStatus) > 0 && !is.null(data[[varStatus]]), label = "Status"),
                 need(length(varGroup)  > 0 && !is.null(data[[varGroup]]),  label = "Group"))
        as.formula(sprintf("Surv(%s, %s) ~ %s", varTime, varStatus, varGroup))
    })

    fit <- reactive(survfit2(fla(), data()))

    xbreaks <- reactive({
        time <- fit()$time
        scales::breaks_extended(5)(time)
    })

    width  <- reactive(input$width)
    height <- reactive(input$height)

    output$survPlot <- renderPlot({
        fit <- fit()
        pal <- pal()

        showCensor <- input$showCensor

        xlab <- input$xlab
        xbreaks <- xbreaks()

        ylab <- input$ylab
        ybreaks <- seq(0,1,.2)

        ggsurvfit(fit, size = 1, theme = theme_classic(15)) +
            { if (showCensor) add_censor_mark(size = 4, stroke = 1) } +
            scale_ggsurvfit(x_scales = list(name = xlab, breaks = xbreaks),
                            y_scales = list(name = ylab, breaks = ybreaks)) +
            { if (!is.null(pal)) scale_color_manual(values = pal) } +
            theme(legend.position = "bottom",
                  legend.text = element_text(size = 15))
    }, width = width, height = height)

    output$propsTable <- render_gt({
        data <- data()
        fla <- fla()
        fit <- eval(rlang::expr(survfit(!!fla, !!data)))
        tbl <- fit %>%
            tbl_survfit(probs = .5) %>%
            add_n() %>%
            add_nevent() %>%
            add_p()
            # modify_table_body(. %>% filter(row_type == "level"))
        as_gt(tbl)
    })

    output$timesTable <- render_gt({
        data <- data()
        fla <- fla()
        fit <- eval(rlang::expr(survfit(!!fla, !!data)))
        xbreaks <- keep(xbreaks(), ~ . > 0)
        tbl <- fit %>%
            tbl_survfit(times = xbreaks, label = ~ "") %>%
            modify_table_body(. %>% filter(row_type == "level"))
        as_gt(tbl)
    })
}

shinyApp(ui = ui, server = server)
