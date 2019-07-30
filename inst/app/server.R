

# Define server
shinyServer(function(input, output, session) {

  # if (test.mode) {
  #   # remove file, to avoid merge conflicts during pulling
  #   # fout <- file.path(Sys.getenv("DSDB_PATH"), "out", "dta.RData")
  #   # if (exists(fout)) file.remove(fout)

  #   owd <- getwd()
  #   setwd(Sys.getenv("DSDB_PATH"))

  #   system("git fetch origin chris")
  #   system("git reset --hard origin/chris")
  #   source(file.path(Sys.getenv("DSDB_PATH"), "collect.R"))

  #   setwd(owd)

  #   load(file.path(Sys.getenv("DSDB_PATH") , "out", "dta.RData"))
  #   setkey(.DT, id, time)
  #   setkey(.meta_dim, id)

  #   setkey(.meta_main, var)
  #   setkey(.meta_series, id)
  #   setkey(.meta_disabled, id)
  # }

# --- initialisazion -----------------------------------------------------------
  g <- new.env()  # 'global', non reactive values
  g$init.state <- 0
  g$colors <- character(0)
  # g$dygraph.range <- c("", "")

  r <- reactiveValues(main = list(),
                      # series = list(id = character(0)),
                      # series.trans = list(id = character(0)),
                      stack = list(id = character(0)),
                      disabled = list(id = character(0)),
                      split.ordered = list(id = character(0)),
                      init = list(state = 0),
                      colors = character(0)
                      )

  qstr <- isolate(session$clientData$url_search)
  ql <- parseQueryString(qstr)

  if (!is.null(ql$main)){
    init.select <- tolower(strsplit(ql$select, split = ",")[[1]])
    init.split <- tolower(strsplit(ql$split, split = ",")[[1]])
    init.var <- tolower(strsplit(ql$var, split = ",")[[1]])
    init.main <- tolower(ql$main)
    init.freq <- tolower(ql$freq)
    g$init.start <- ql$start
    g$init.end <- ql$end

    # modified PRE2 where everything is selcted as specified in url
    dt0 <- .meta_dim[var == init.main]

    dt0[dim_class == "select", css_class := gsub("selected", "", css_class, fixed = TRUE)]
    dt0[dim_class == "select" & dim_id %in% init.select, css_class := paste(css_class, "selected")]

    dt0[dim_class == "split", css_class := gsub("split-selected", "", css_class, fixed = TRUE)]
    dt0[dim_class == "split" & dim_id %in% init.split, css_class := paste(css_class, "split-selected")]

    dt0[dim_class == "var", css_class := gsub("selected", "", css_class, fixed = TRUE)]
    dt0[dim_class == "var" & dim_id %in% init.var, css_class := paste(css_class, "selected")]

    backup <- PRE2[[init.main]]
    PRE2[[init.main]] <- html_all_dt(dt0)

    g$init.state <- 2
  }


# --- var updates (search etc) -------------------------------------------------

 # by select input
  observe({
    if (!is.null(input$iSearch)){
      if (input$iSearch != ""){
        r$main$var <- input$iSearch
      }
  }
  })

  # recreate search html after init screen its a bit hacky due to the fact that
  # I cannot update selectize via javascrpt, wichout destroying it.
  observe({
    # set the state var to one as soon as the first series is looked up
    req(r$main$var)
    g$init.state <- 1
  })

  # do this in the beginning (init screen) and once again when the state
  # variable is 1. Do not run later on.
  output$oSearch <- renderUI({
    selected <- isolate(r$main$var)
    if ((g$init.state) == 0){
      html_search()
    } else if ((g$init.state) == 2){
      html_search(FALSE, init.main, noinit = TRUE)
    } else if ((g$init.state) == 1){
      # restore default if init state has been supplied
      if (exists("init.main")){
        PRE2[[init.main]] <- backup
        PRE2 <<- PRE2
      }
      html_search(FALSE, selected)
    }
  })

  # output$oDownloadButton <- renderUI({
  #   st <- r$stack$id
  #   if (length(st) > 0){
  #     tags$li(
  #       tags$div(class = "media-body",
  #          tags$button(id="iDownloadCsv", type="button", target="_blank", style = "margin-bottom: 10px;",
  #           class="btn btn-sm btn-primary pull-right shiny-download-link", "Download"
  #         )
  #       )
  #     )
  #   }
  # })


# --- Dim transformation -------------------------------------------------------

  observe({

    did <- isolate(r$split.ordered$id)
    disabled <- isolate(r$disabled$id)

    colors <- rSeriesTrans()$colors

    # dots need to be ecaped to be usable as element id. More complicated to do
    # it in JS.
    did <- gsub(".", "\\.", did, fixed = TRUE)
    disabled <- gsub(".", "\\.", disabled, fixed = TRUE)
    # print(last.did)
    data <- list(id = did, color = colors, disabled_id = disabled)
    session$sendInputMessage("iSplit", data)

  })


      #   # do not change previously assigned colors
      #   observe({

      #     sid <- rSeries()$id

      # # browser()
      #     # sid <- isolate(r$split.ordered$id)
      #     colors.old <- g$colors

      #     sid.in.use <- intersect(sid, names(colors.old))
      #     sid.not.in.use <- setdiff(sid, names(colors.old))

      #     colors.in.use <- colors.old[sid.in.use]
      #     colors.not.in.use <- setdiff(.colors, colors.in.use)

      #     colors.new <- colors.not.in.use[1:length(sid.not.in.use)]

      #     colors.new <- c(colors.in.use, colors.new)
      #     names(colors.new) <- c(sid.in.use, sid.not.in.use)

      #     colors.new <- colors.new[sid]
      #     g$colors <- colors.new

      #     print(colors.new)
      #     r$colors <- unname(colors.new)

      #   })


  # split dim is in order of appearance
  observe({
    old <- isolate(r$split.ordered$id)
    new <- input$iSplit

    to.rm <- setdiff(old, new)
    to.add <- setdiff(new, old)

    r$split.ordered$id <- c(setdiff(old, to.rm), to.add)
  })


# --- Update inputs ------------------------------------------------------------

  output$oSplit <- renderUI({
    req(r$main$var)
    # vapply(pp, function(e) attr(e, "dim_class"), "")
    pp <- PRE2[[r$main$var]]
    is.split <- vapply(pp, function(e) attr(e, "dim_class"), "") == "split"
    if (any(is.split)){
        input_split(pp[is.split][[1]])
    }

  })

  output$oSelect <- renderUI({
    req(r$main$var)
        # vapply(pp, function(e) attr(e, "dim_class"), "")
    pp <- PRE2[[r$main$var]]
    is.select <- vapply(pp, function(e) attr(e, "dim_class"), "") == "select"

    if (any(is.select)){
        with_select_callback(tagList(pp[is.select]), "iSelect")
    }

  })

  output$oVar <- renderUI({
    req(r$main$var)
        # vapply(pp, function(e) attr(e, "dim_class"), "")
    pp <- PRE2[[r$main$var]]
    is.var <- vapply(pp, function(e) attr(e, "dim_class"), "") == "var"

    if (any(is.var)){
        with_id_callback(tagList(pp[is.var]), "iVar")
    }

  })


# --- Series Lookup ------------------------------------------------------------

  # update
  rSeries <- reactive({
    # observer is right here, because we are updating r$disabled and r$series,
    # based on inputs from all dimensions

    req(r$main$var)

    select <- unlist(input$iSelect)
    split <- r$split.ordered$id
    var <- unlist(input$iVar)

    freq <- input$iFreq
    main <- r$main$var

    z <- unique(c(select, split, var))

    # do not use input from non-updated fields
    is.main <- vapply(strsplit(z, split = ".", fixed = TRUE), function(e) e[1], "") == main
    zz <- z[is.main]

    r$disabled$id <- disabled_id(zz)

    sid <- if (length(zz) > 0) series_id(zz) else ""
    sid <- intersect(sid, .meta_allid)

    cn <- .meta_series[sid, h2]
    xtss <- get_xts(sid, to = freq)
    if (!is.null(xtss)){
      cn <- .meta_series[sid, h2]
      colnames(xtss) <- cn
    }

    list(id = sid,
                     ylab = .meta_series[sid, unit][1],
                     xts = xtss)
  })


# --- Series Transform ---------------------------------------------------------

  rSeriesTrans <- reactive({

    # also, this more like a reactive expressions, one input -> one ouptut

    # This should be put in a function, would be straigtforeward
    # series.trans <- transf_series(xts)

    # But we leave it as long as it works fine...
    req(rSeries()$xts)
    # if (is.null(rSeries()$xts)){
    #   rSeriesTrans()$xts <- NULL
    #   return(NULL)
    # }

    sid <- rSeries()$id
    xtss <- rSeries()$xts
    ylab <- rSeries()$ylab

    win.numeric <- input$iWindow  # Callback from dygraphs

    transf <- input$iTransform

    if (!is.null(win.numeric)){
      st.win <- as.Date(as.POSIXct(win.numeric/1000, origin="1970-01-01", tz = "GMT"))
      # manual win selection drops initial selection
      g$init.start <- ""
      g$init.end <- ""
    } else if (exists("init.start", envir = g) && g$init.start != "") {
      # the first time this runs, make sure to have a symetric window
      if (g$init.end == ""){
        g$init.end <- as.character(tail(index(xtss), 1))
      }
      st.win <- c(g$init.start, g$init.end)
    } else {
      st.win <- c(start(xtss), end(xtss))
    }


    if (transf == "pc"){
      xtss <- pc(xtss)
      per <- periodicity(xtss)
      ylab <- paste("percentage change, to previous", per$label)
    } else if (transf == "pcy"){
      ylab <- "percentage change, to previous year"
      xtss <- pcy(xtss)
    } else if (transf == "indA"){

      # if win earlier than data, get start from data
      start <- max(st.win[1], head(index(na.omit(xtss)), 1))

      # take the first date from data before win
      idx <- index(xtss)
      start <- head(idx[idx >= start], 1)

      val <- as.numeric(xtss[start])
      for (i in 1:NCOL(xtss)){
        xtss[, i] <- 100 * (xtss[, i] / val[i])
      }

      date.txt <- pretty_date(start, periodicity(xtss)$scale)
      ylab <- paste0("index (", date.txt, " = 100)")

    } else if (transf == "indB"){

      # if win later than data, get end from data
      end <- min(st.win[2], tail(index(na.omit(xtss)), 1))

      # take the first date from data before win
      idx <- index(xtss)
      end <- tail(idx[idx <= end], 1)

      val <- as.numeric(xtss[end])
      for (i in 1:NCOL(xtss)){
        xtss[, i] <- 100 * (xtss[, i] / val[i])
      }

      date.txt <- pretty_date(end, periodicity(xtss)$scale)
      ylab <- paste0("index (", date.txt, " = 100)")
    } else if (transf == "log") {
      ylab <- paste0(ylab, " (log scale)")
    }


    st <- list(id = sid, xts = xtss, ylab = ylab, win = st.win, transf = transf)


    st$var <- isolate(r$main$var)


    colors.old <- g$colors

    sid.in.use <- intersect(sid, names(colors.old))
    sid.not.in.use <- setdiff(sid, names(colors.old))

    colors.in.use <- colors.old[sid.in.use]
    colors.not.in.use <- setdiff(.colors, colors.in.use)

    colors.new <- colors.not.in.use[1:length(sid.not.in.use)]

    colors.new <- c(colors.in.use, colors.new)
    names(colors.new) <- c(sid.in.use, sid.not.in.use)

    colors.new <- colors.new[sid]
    g$colors <- colors.new

    print(colors.new)
    st$colors <- unname(colors.new)

    st

  })


# --- Dygraph ------------------------------------------------------------------

  output$oMainPlot <- renderDygraph({

    # validate(need(!is.null(rSeriesTrans()$xts), "Please select at least one series."))

    req(rSeriesTrans()$xts)
    st <- rSeriesTrans()

    # series.trans object contains all the stuff needed for printing
    # - sid
    # - xts
    # - ylab
    # - transf
    # - colors

    # to dis / enable zoom on moblie
    zoom <- input$iZoom

    d <- dygraph(st$xts, ylab = st$ylab)
    d <- dyAxis(d, "x", pixelsPerLabel = 50, drawGrid = FALSE)
    d <- dyAxis(d, "y", pixelsPerLabel = 50, axisLineColor = "#FFF")
    d <- dyLegend(d, show = "always", width = 300, labelsDiv = "oLabel")

    # 0.8 reduces the highligh flickering, but still draws a reasonabl line after zooming
    d <- dyHighlight(d, highlightSeriesBackgroundAlpha = 1, highlightCircleSize = 0, highlightSeriesOpts = list(strokeWidth = 0.8, highlightCircleSize = 3))

    # turn off interaction model on button (on mobile)
    if (!is.null(zoom) && zoom == "disabled"){
      d$x$attrs$interactionModel <- list()
    }

    # this works! but not sure how to bring back to R
    d <- dyCallbacks(d, zoomCallback = I("function(minDate, maxDate, yRange) {
      var range = [minDate, maxDate]
      Shiny.onInputChange('iWindow', range)
              }"))

    logscale <- st$transf == "log"

    # need to set all options at once! otherwhise reset to defaults!
    d <- dyOptions(d, rightGap = 5,
                   logscale = logscale,
                   animatedZooms = TRUE,
                   colors = st$colors,
                   retainDateWindow = TRUE,
                   gridLineColor = "#E1E5EA",
                   mobileDisableYTouch = FALSE,
                   axisLineColor = "#303030")


    if (exists("init.start", envir = g)){
      if (!is.null(g$init.start) && g$init.start != ""){
        d$x$attrs$dateWindow <- c(dygraphs:::asISO8601Time(st$win[1]), dygraphs:::asISO8601Time(st$win[2]))
        # g$init.start <- ""
        # g$init.end <- ""
      }
    }

    d
  })


# --- Stack --------------------------------------------------------------------

  # update stack, triggered by add to list button
  observeEvent(input$iActionAddToList, {
    old <- isolate(r$stack$id)
    new <- isolate(rSeries()$id)
    r$stack$id <- unique(c(old, new))
  })

  # update stack, triggered by callback from stack
  observeEvent(input$iStack, {
    cb <- input$iStack
    if (is.null(cb)) return(NULL)
    # to close
    if (grepl("cl.", cb, fixed = TRUE)){
      to.rm <- gsub("cl.", "", cb, fixed = TRUE)
      old <- isolate(r$stack$id)
      r$stack$id <- setdiff(old, to.rm)
    }
  })

  # update stack, if 'Remove All' is clicked
  observeEvent(input$iStackReset, {
    r$stack$id <- character(0)
  })

  # send number of series in Stack to client
  observe({
    lsid <- length(r$stack$id)
    session$sendCustomMessage(type = "cmLengthStack", lsid)
  })

  # render all elements in stack to show in modal
  output$oStack <- renderUI({
    sid <- r$stack$id
    ll <- (lapply(sid, html_stack_entry))
    if (length(ll) > 0){
      with_id_callback(ll, "iStack")
    }
  })
  # render on add, rather than when modal is shown
  outputOptions(output, "oStack", suspendWhenHidden = FALSE)


# --- Source / Info ------------------------------------------------------------


  output$oSource <- renderUI({
    req(r$main$var)
    var <- r$main$var
      if (exists("init.freq")){
        freq.selected <- init.freq
      } else {
        freq.selected <- ""
      }
    html_source(var, freq.selected = freq.selected)
  })


# --- URL sharing --------------------------------------------------------------

  rUrlString <- reactive({

    req(rSeriesTrans()$xts)

    third_element <- function(x){
      if (length(x) == 0) return("")
      spl <- strsplit(x, split = ".", fixed = TRUE)
      id <- vapply(spl, function(e) e[3], "")
    }

    hname <- isolate(session$clientData$url_hostname)
    if (hname == "127.0.0.1"){
      # hname <- "www.dataseries.org"
      hname <- paste0(hname, ":", isolate(session$clientData$url_port))
    }

    url_string <- function(baseurl = paste0("http://", hname),
                        main, split, select = "", var = "", freq = "", start = "", end = "", trans = "none"){
                        paste0(baseurl, "?main=", main,
                               "&split=", paste(third_element(split), collapse = ","),
                               "&select=", paste(third_element(select), collapse = ","),
                               "&var=", third_element(var),
                               "&freq=", freq,
                               "&start=", start,
                               "&end=", end,
                               "&trans=", trans)
    }

    st <- rSeriesTrans()

    main <- r$main$var
    select <- unlist(input$iSelect)
    split <- r$split.ordered$id
    var <- unlist(input$iVar)
    trans <- input$iTransform
    freq <- input$iFreq
    win <- as.Date(st$win)

    # a week of 'tolerance', so that 'open' margins can be choosen easier.

    if (!is.null(win)){
      if (head(index(st$xts), 1) > win[1] - 7){
        start <- ""
      } else {
        start <- win[1]
      }
      if (tail(index(st$xts), 1) < win[2] + 7){
        end <- ""
      } else {
        end <- win[2]
      }
    } else {
      start <- end <- ""
    }

    us <- url_string(main = main, split = split, select = select, var = var, freq = freq, start = start, end = end, trans = trans)

  })

  observe({
     session$sendCustomMessage(type='rUrlString', rUrlString())
  })

  output$oUrl <- renderUI({
    rUrlString()
    # HTML(paste("<input onClick='this.setSelectionRange(0, this.value.length)' class = 'cp-box', readonly value = '", rUrlString(), "'></input>"))
  })


# --- embedding ----------------------------------------------------------------

  rEmbed <- reactive({
    req(rSeriesTrans()$xts)
    url.string <- isolate(rUrlString())

    st <- rSeriesTrans()

    z <- html_embedd(st, url.string)

    # writeLines(z, con = "~/git/embed/aa.html")
    # browseURL("../../git/embed/aa.html")

    z
  })


  output$oEmbed <- renderText({


    height <- 445 + NCOL(isolate(rSeriesTrans()$xts)) * 15

    htmltools::htmlEscape(
    paste0('<iframe style="border-width: 0px" width="100%" height="', height ,'px" srcdoc = "',
          paste(as.character(rEmbed()), collapse = "\n"),
          '"><p>Your browser does not support iframes.</p></iframe>'))
  })


  output$oEmbedExample <- renderUI({


    height <- 445 + NCOL(isolate(rSeriesTrans()$xts)) * 15

    str <- as.character(htmltools::htmlEscape(paste(as.character(rEmbed()), collapse = "\n")))

    str <- gsub("&gt;", ">", str, fixed = TRUE)
    str <- gsub("&lt;", "<", str, fixed = TRUE)

    tags$iframe(srcdoc = str,
                style = "border-width: 0px",
                width = "100%",
                height = paste0(height, 'px'))

  })


# --- R API --------------------------------------------------------------------

  output$oRCodeStack <- renderText({
    paste(strwrap(paste0("dataseries::ds(", paste(deparse(r$stack$id), collapse = ""), ")"),
            width = 60, prefix = "                 ", initial = ""), collapse = "\n")
  })

  output$oRCodeGraph <- renderText({
    req(rSeries()$id)
    paste(strwrap(paste0("dataseries::ds(", paste(deparse(rSeries()$id), collapse = ""), ")"),
            width = 60, prefix = "                 ", initial = ""), collapse = "\n")
  })


# --- Data downloaders ---------------------------------------------------------

  output$oDownloadStackCsv <- downloadHandler(
  filename = "download.csv",
  content = function(file) {
      sid <- isolate(r$stack$id)
      sid <- intersect(sid, .meta_allid)
      xtss <- get_xts(sid)
      cn <- .meta_series[sid, label]
      dta <- data.frame(time = time(xtss), xtss)
      names(dta)[-1] <- cn
      write.csv(dta, file, row.names = FALSE)
  },
  contentType = "text/csv")

  output$oDownloadStackXlsx <- downloadHandler(
  filename = "download.xlsx",
  content = function(file) {
    req(rSeries()$id)
      sid <- isolate(r$stack$id)
      sid <- intersect(sid, .meta_allid)
      xtss <- get_xts(sid)
      cn <- .meta_series[sid, label]
      dta <- data.frame(time = time(xtss), xtss)
      names(dta)[-1] <- cn
      openxlsx::write.xlsx(dta, file, row.names = FALSE)
  },
  contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")

  output$oDownloadGraphCsv <- downloadHandler(
  filename = "download.csv",
  content = function(file) {
      validate(need(length(rSeries()$id) > 0, "No series selected"))

      # if (length(rSeries()$id) == 0) stop("No series selected")
      sid <- isolate(rSeries()$id)
      sid <- intersect(sid, .meta_allid)
      xtss <- get_xts(sid)
      cn <- .meta_series[sid, label]
      dta <- data.frame(time = time(xtss), xtss)
      names(dta)[-1] <- cn
      write.csv(dta, file, row.names = FALSE)
  },
  contentType = "text/csv")

  output$oDownloadGraphXlsx <- downloadHandler(
  filename = "download.xlsx",
  content = function(file) {
      validate(need(length(rSeries()$id) > 0, "No series selected"))
      # if (length(rSeries()$id) == 0) stop("No series selected")
      sid <- isolate(rSeries()$id)
      sid <- intersect(sid, .meta_allid)
      xtss <- get_xts(sid)
      cn <- .meta_series[sid, label]
      dta <- data.frame(time = time(xtss), xtss)
      names(dta)[-1] <- cn
      openxlsx::write.xlsx(dta, file, row.names = FALSE)
  },
  contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")





  # graphs
  output$oDownloadPdf <- downloadHandler(
  filename = "download.pdf",
  content = function(file) {
      validate(need(length(rSeries()$id) > 0, "No series selected"))
      sid <- isolate(rSeriesTrans()$id)
      xtss <- isolate(rSeriesTrans()$xts)
      ylab <- isolate(rSeriesTrans()$ylab)
      transf <- isolate(rSeriesTrans()$transf)
      win <- isolate(rSeriesTrans()$win)

      pdf(file, width = 8, height = 5)
      print(ggplot_sid(sid = sid, xtss = xtss, transf = transf, win = win, ylab = ylab))
      dev.off()
      extrafont::embed_fonts(file)

      url <- isolate(rUrlString())
  },
  contentType = "application/pdf")

  output$oDownloadSvg <- downloadHandler(
  filename = "download.svg",
  content = function(file) {
      validate(need(length(rSeries()$id) > 0, "No series selected"))
      sid <- isolate(rSeriesTrans()$id)
      xtss <- isolate(rSeriesTrans()$xts)
      ylab <- isolate(rSeriesTrans()$ylab)
      transf <- isolate(rSeriesTrans()$transf)
      win <- isolate(rSeriesTrans()$win)

      svg(file, width = 8, height = 5)
      print(ggplot_sid(sid = sid, xtss = xtss, transf = transf, win = win, ylab = ylab))
      dev.off()
  },
  contentType = "image/svg")

  output$oDownloadPng <- downloadHandler(
  filename = "download.png",
  content = function(file) {
      validate(need(length(rSeries()$id) > 0, "No series selected"))
      sid <- isolate(rSeriesTrans()$id)
      xtss <- isolate(rSeriesTrans()$xts)
      ylab <- isolate(rSeriesTrans()$ylab)
      transf <- isolate(rSeriesTrans()$transf)
      win <- isolate(rSeriesTrans()$win)

      png(file, width = 8, height = 5, units = "in", res = 150)
      print(ggplot_sid(sid = sid, xtss = xtss, transf = transf, win = win, ylab = ylab))
      dev.off()
  },
  contentType = "image/png")

})
