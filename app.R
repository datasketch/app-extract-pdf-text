library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(shi18ny)
library(V8)
library(dsmodules)
library(pdftools)
library(magick)
library(pander)
library(knitr)

# ¿en buttonImage sólo se puede si es de imágenes guardadas en www?
# no se pueden seleccionar múltiples


ui <- panelsPage(useShi18ny(),
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
                       color = "chardonnay",
                       can_collapse = FALSE,
                       body = div(langSelectorInput("lang", position = "fixed"),
                                  uiOutput("result"),
                                  shinypanels::modal(id = "download",
                                                     title = ui_("download_file"),
                                                     uiOutput("modal"))),
                       footer = shinypanels::modalButton(label = "Download file", modal_id = "download")))



server <- function(input, output, session) {
  
  i18n <- list(defualtLang = "en", availableLangs = c("es", "en", "pt_BR"))
  lang <- callModule(langSelector, "lang", i18n = i18n, showSelector = FALSE)
  observeEvent(lang(), {uiLangUpdate(input$shi18ny_ui_classes, lang())})
  
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
  
  output$text_input <- renderUI({
    fileInput("upload", i_("upload_lb", lang()), buttonLabel = i_("upload_bt_lb", lang()), placeholder = i_("upload_pl", lang()))
  })
  
  td <- reactiveValues()

  # pdf_upload <- reactive({
  observe({
    req(input$upload)
    td$text_up <- pdf_text(input$upload$datapath)
    td$text <- td$text_up
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
    })
    td$dir <- d2
  })
  
  # deleting temporary file when session ends
  session$onSessionEnded(function() {
    system(paste("rm -r", "www/der2r23r/"))
  })
  
  observeEvent(list(input$show, input$search, input$pdf_pages, input$upload), {
    req(input$show, input$pdf_pages)
    t0 <- td$text_up
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
                     format = "png",
                     path = paste0(d1, "/"), 
                     imageStyle = list(borderSize = "none",
                                       shadow = TRUE),
                     ncol = 1,
                     classImg = "",
                     tooltips = paste0(i_("page", lang()), 1:td$pages))
  })
  
  output$result <- renderUI({
    lapply(c("txt", "docx", "html"), function(z) {
      buttonId <- paste0("download_data_button-DownloadTxt", z)
      session$sendCustomMessage("setButtonState", c("none", buttonId)) 
    })
    HTML(paste0("<div style = 'box-shadow: -3px 3px 5px 2px rgba(0, 0, 0, 0.06); max-width: 1000px; padding: 12px 10px;'>", td$text, "</div>"))
  })
  
  output$modal <- renderUI({
    dw <- i_("download", lang())
    downloadTextUI("download_data_button", dw, c("txt", "docx", "html"))
  })
  
  callModule(downloadText, "download_data_button",  text = reactive(td$text), formats = c("txt", "docx", "html"))
  
}



shinyApp(ui, server)
