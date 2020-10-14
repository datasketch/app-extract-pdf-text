library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(shi18ny)
library(V8)
library(dsmodules)
library(dspins)
library(pdftools)
library(magick)
library(shinycustomloader)



ui <- panelsPage(useShi18ny(),
                 showDebug(),
                 panel(title = ui_("upload_data"),
                       width = 200,
                       body = uiOutput("text_input")),
                 panel(title = ui_("dataset"), 
                       width = 300,
                       body = uiOutput("data_preview")),
                 panel(title = ui_("options"),
                       color = "chardonnay",
                       width = 250,
                       body = uiOutput("controls")),
                 panel(title = ui_("viz"),
                       title_plugin = uiOutput("download"),
                       color = "chardonnay",
                       can_collapse = FALSE,
                       body = div(langSelectorInput("lang", position = "fixed"),
                                  # withLoader(uiOutput("result", width = "100%"), type = "image", loader = "loading_gris.gif"))))
                                  withLoader(uiOutput("result"), type = "image", loader = "loading_gris.gif"))))



server <- function(input, output, session) {
  
  i18n <- list(defualtLang = "en", availableLangs = c("es", "en", "pt_BR"))
  lang <- callModule(langSelector, "lang", i18n = i18n, showSelector = FALSE)
  observeEvent(lang(), {uiLangUpdate(input$shi18ny_ui_classes, lang())})
  
  output$text_input <- renderUI({
    fileInput("upload", i_("upload_lb", lang()), buttonLabel = i_("upload_bt_lb", lang()), placeholder = i_("upload_pl", lang()))
  })
  
  path <- "parmesan"
  parmesan <- parmesan_load(path)
  parmesan_input <- parmesan_watch(input, parmesan)
  parmesan_alert(parmesan, env = environment())
  parmesan_lang <- reactive({i_(parmesan, lang(), keys = c("label", "choices", "text", "placeholder"))})
  output_parmesan("controls",
                  parmesan = parmesan_lang,
                  input = input,
                  output = output, 
                  env = environment())
  
  
  td <- reactiveValues()
  
  # pdf_upload <- reactive({
  observe({
    req(input$upload)
    safe_pdf <- purrr::safely(pdf_text)
    res <- safe_pdf(input$upload$datapath)
    td$text_up <- res
    if (!is.null(res$result)) {
      td$text <- td$text_up$result
      d0 <- image_read_pdf(input$upload$datapath, density = 50)
      td$pages <- length(d0)
      d1 <- "der2r23r" # nombre aleatorio
      # d1 <- tempdir()
      d2 <- substr(Sys.time(), 12, 19)
      lapply(paste0("www/", d1, "/", list.files(paste0("www/", d1))), file.remove)
      # unlink(paste0("www/", d1, "/"), recursive = TRUE)
      if (dir.exists("www")) {
        dir.create(paste0("www/", d1))
      } else {
        dir.create("www")
        dir.create(paste0("www/", d1))
      }
      i0 <- seq_along(d0)
      i1 <- lapply(i0, function(e) {
        image_write(d0[e], paste0("www/", d1, "/", e, "_", d2,".png"),  )
        # image_write(d0[e], paste0("www/", d1, "/", e, "_", d2,".svg"),  )
      })
      td$dir <- d2
    } else {
      td$dir <- NULL
      td$text <- infomessage(p(res$error$message))
    }
  })
  
  # deleting temporary file when session ends
  session$onSessionEnded(function() {
    system(paste("rm -r", "www/der2r23r/"))
  })
  
  observeEvent(list(input$show, input$search, input$pdf_pages, input$upload), {
    req(input$show, input$pdf_pages)
    t0 <- td$text_up$result
    if (!grepl("All pages", input$show)) {
      nm <- as.numeric(strsplit(input$pdf_pages, "_")[[1]][1])
      t0 <- t0[nm]
    }  
    if (nzchar(input$search)) {
      t0 <- t0[grep(input$search, t0)] 
      # falta con ignore case true
      # m0 <- regmatches(t0, regexpr(input$search, t0, ignore.case = TRUE))
      t0 <- gsub(input$search, paste0("<span style = 'background-color: #b845805c;'>", input$search, "</span>"), t0)
      # t0 <- lapply(m0, function(d) {
      #   gsub(d, paste0("<span style = 'background-color: #b845805c;'>", d, "</span>"), t0, ignore.case = TRUE)
      # })
    }
    td$text <- as.character(t0)
  })
  
  output$data_preview <- renderUI({
    req(td$pages, td$dir)
    # d1 <- td$dir
    d1 <- "der2r23r"
    buttonImageInput("pdf_pages",
                     # "Pages", 
                     images = paste0(1:td$pages, "_", td$dir),
                     # format = "png",
                     # format = "svg",
                     path = paste0(d1, "/"), 
                     imageStyle = list(borderSize = "none",
                                       shadow = TRUE),
                     ncol = 1,
                     classImg = "",
                     tooltips = paste0(i_("page", lang()), 1:td$pages))
  })
  
  output$download <- renderUI({
    lb <- i_("download_file", lang())
    dw <- i_("download", lang())
    gl <- i_("get_link", lang())
    
    mb <- list(textInput("name", i_("gl_name", lang())),
               textInput("description", i_("gl_description", lang())),
               selectInput("license", i_("gl_license", lang()), choices = c("CC0", "CC-BY")),
               selectizeInput("tags", i_("gl_tags", lang()), choices = list("No tag" = "no-tag"), multiple = TRUE, options = list(plugins= list('remove_button', 'drag_drop'))),
               selectizeInput("category", i_("gl_category", lang()), choices = list("No category" = "no-category")))
    downloadDsUI("download_data_button", dropdownLabel = lb, text = dw, formats = c("txt", "docx", "html"),
                 display = "dropdown", dropdownWidth = 170, getLinkLabel = gl, modalTitle = gl, modalBody = mb,
                 modalButtonLabel = i_("gl_save", lang()), modalLinkLabel = i_("gl_url", lang()), modalIframeLabel = i_("gl_iframe", lang()),
                 modalFormatChoices = c("HTML" = "html", "PNG" = "png"))
  })
  
  output$result <- renderUI({
    req(td$text)
    if (grepl("<div class=\"infomessage warning \">", td$text)) {
      td$text
    } else {
      HTML(paste0("<div style = 'box-shadow: -3px 3px 5px 2px rgba(0, 0, 0, 0.06); max-width: 800px;
                  padding: 12px 10px;'>", td$text, "</div>"))
    }
  })
  
  # url params
  par <- list(user_name = "brandon", org_name = NULL)
  url_par <- reactive({
    url_params(par, session)
  })
  
  # función con user board connect y set locale
  pin_ <- function(x, bkt, ...) {
    x <- dsmodules:::eval_reactives(x)
    bkt <- dsmodules:::eval_reactives(bkt)
    nm <- input$`download_data_button-modal_form-name`
    if (!nzchar(input$`download_data_button-modal_form-name`)) {
      nm <- paste0("saved", "_", gsub("[ _:]", "-", substr(as.POSIXct(Sys.time()), 1, 19)))
      updateTextInput(session, "download_data_button-modal_form-name", value = nm)
    }
    # dv <- dsviz(x,
    #             name = nm,
    #             description = input$`download_data_button-modal_form-description`,
    #             license = input$`download_data_button-modal_form-license`,
    #             tags = input$`download_data_button-modal_form-tags`,
    #             category = input$`download_data_button-modal_form-category`)
    dspins_user_board_connect(bkt)
    Sys.setlocale(locale = "en_US.UTF-8")
    pin(dv, bucket_id = bkt)
  }
  
  # descargas
  observe({
    downloadDsServer("download_data_button", element = reactive(td$text), formats = c("txt", "docx", "html"),
                     errorMessage = i_("gl_error", lang()),
                     modalFunction = pin_, reactive(td$text),
                     bkt = url_par()$inputs$user_name)
  })
  
  # callModule(downloadText, "download_data_button",  text = reactive(td$text), formats = c("link", "txt", "docx", "html"))
  
}



shinyApp(ui, server)
