library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(car)
library(readxl)
library(shinyWidgets)

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = span(""),
    
    tags$li(
      class = "dropdown",
      style = "padding: 8px 15px 0 0; margin-left: auto;",
      tags$div(
        style = "display: flex; align-items: center; gap: 10px; color: white;",
  
        tags$span("Aplikasi ANOVA", style = "font-weight: bold; font-size: 18px;"),
        tags$img(src = "logo.png", height = "30px")
      )
    )
  ),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                br(), br(),
                div(style = "padding: 20px;",
                    radioButtons(
                      inputId = "dark_mode",
                      label = "Mode Tampilan",
                      choices = c("Light" = "light", "Dark" = "dark"),
                      selected = "light",
                      inline = TRUE
                    )
                ),
                menuItem("Home", tabName = "home", icon = icon("home")),
                menuItem("Input Data", tabName = "input", icon = icon("upload")),
                menuItem("Uji Kenormalan", tabName = "normal", icon = icon("chart-line")),
                menuItem("Uji Varians", tabName = "varians", icon = icon("balance-scale")),
                menuItem("ANOVA", tabName = "anova", icon = icon("project-diagram")),
                menuItem("Tukey", tabName = "tukey", icon = icon("list"))
    )
  ),
 dashboardBody(
    tags$head(tags$style(HTML("
.content-wrapper, .right-side {
    background-image: url('motif.jpg');
    background-size: cover;
    background-repeat: no-repeat;
    background-attachment: fixed;
    background-position: top center;
  }
.box {
    background-color: rgba(255, 255, 255, 0.85) !important;
  }
.content {
    background-color: transparent !important;
  }
.skin-blue .main-sidebar {
    background-color: #1e2b38 !important;
    color: white !important;
  }
  .skin-blue .main-sidebar .sidebar a {
    color: white !important;
  }
  .skin-blue .main-sidebar .sidebar-menu > li > a {
    color: white !important;
    font-weight: 500;
  }
  .skin-blue .main-sidebar .sidebar-menu > li.active > a {
    background-color: #007bff !important;
    color: white !important;
  }
  .main-sidebar .radio label {
    color: white !important;
  }
  .main-sidebar h4, .main-sidebar h3, .main-sidebar h2, .main-sidebar h1,
  .main-sidebar p {
    color: white !important;
  }
  color: black !important;
  }
")),
      tags$script(HTML("
      Shiny.addCustomMessageHandler('toggleDark', function(message) {
        if (message) {
          document.body.classList.add('dark-mode');
        } else {
          document.body.classList.remove('dark-mode');
        }
      });
    "))
    ),         
   
    tabItems(
      tabItem("home", 
              div(
                style = "text-align: center; padding: 30px;",
                img(src = "stat.jpg", width = "60%", 
                    style = "border-radius: 15px; box-shadow: 0 4px 12px rgba(0,0,0,0.3); margin-bottom: 20px;"),
                h2("SELAMAT DATANG DI APLIKASI ANOVA!", 
                   style = "font-weight: bold; color: #2c3e50;"),
                p("Aplikasi ini membantu Anda melakukan analisis ANOVA satu arah secara interaktif.", 
                  style = "font-size: 16px; color: #555; margin-top: 10px;")
              )
  ),
      tabItem("input",
              fluidRow(
                box(title = "Upload dan Pilih Data", status = "primary", solidHeader = TRUE, width = 4, fileInput("file1", "Upload CSV"),
                    radioButtons("dataSourceType", "Pilih Sumber Data",
                                 choices = c("Upload Data" = "upload",
                                             "Dataset R" = "builtin"),
                                 selected = "upload"),
                  
                    conditionalPanel(
                      condition = "input.dataSourceType == 'upload'",
                      fileInput("file1", "Upload File (CSV/XLSX/XLS/TSV)"),
                      radioButtons("sep", "Pemisah", 
                                   choices = c("koma ," = ",", "titik koma ;" = ";", "tab \\t" = "\t"))
                    ),
                    
                    conditionalPanel(
                      condition = "input.dataSourceType == 'builtin'",
                      selectInput("dataSource", "Pilih Dataset",
                                  choices = c("iris", "PlantGrowth", "mtcars"))
                    ),
                    
                    sliderInput("alpha", "Taraf Signifikansi", 0.01, 0.1, 0.05, 0.01),
                    uiOutput("varSelect"),
                    uiOutput("groupSelect"),
                    actionButton("submitData", "OK")
                ),
                
                box(title = "Preview Data", status = "primary", solidHeader = TRUE, width = 8,
                    div(style = "overflow-x: auto;", tableOutput("dataPreview"))
                )
              )
      ),
      tabItem("normal",
              uiOutput("statusBox"),
              fluidRow(
                box(title = "Hipotesis Uji Kenormalan", status = "primary", solidHeader = TRUE width = 6, 
                withMathJax("$$
                \\begin{aligned}
                H_0 &: X \\sim \\mathcal{N}(\\mu, \\sigma^2) \\\\
                H_1 &: X \\not\\sim \\mathcal{N}(\\mu, \\sigma^2)
                \\end{aligned}
                $$")),
                box(title = "Taraf Signifikansi", status = "primary", solidHeader = TRUE, width = 6, textOutput("tarafNormal"))
              ),
              fluidRow(
               box(title = "Hasil", status = "primary", solidHeader = TRUE, width = 12, verbatimTextOutput("hasilNormal")),
                box(title = "Keputusan", status = "primary", solidHeader = TRUE, width = 12, verbatimTextOutput("keputusanNormal")),
                box(title = "Kesimpulan", status = "primary", solidHeader = TRUE, width = 12, verbatimTextOutput("kesimpulanNormal")),
                box(title = "Histogram", status = "primary", solidHeader = TRUE, width = 12, plotOutput("plotNormal", height = "800px")),
                box(title = "Q-Q Plot per Kelompok", status = "primary", solidHeader = TRUE, width = 12, plotOutput("qqNormal")),
                box(title = "Interpretasi Visual", status = "primary", solidHeader = TRUE, width = 12, verbatimTextOutput("interpretasiNormal"))
              ),
              actionButton("toVarians", "Lanjut ke Uji Varians")
      ),
      tabItem("varians",
              uiOutput("statusBox"),
              fluidRow(
                box( title = "Hipotesis Uji Homogenitas", status = "primary", solidHeader = TRUE, width = 6,
                withMathJax("$$
                H_0 : \\sigma^2_1 = \\sigma^2_2 = \\cdots = \\sigma^2_k \\\\
                H_1 : \\sigma^2_i \\ne \\sigma^2_j, \\forall i \\ne j; \\ i,j = 1,2,3,...,k
                $$")),
                box(title = "Taraf Signifikansi", status = "primary", solidHeader = TRUE, width = 6, textOutput("tarafVarians"))
              ),
              fluidRow(
                box(title = "Hasil", status = "primary", solidHeader = TRUE, width = 12, verbatimTextOutput("variansResult")),
                box(title = "Keputusan", status = "primary", solidHeader = TRUE, width = 12, verbatimTextOutput("keputusanVarians")),
                box(title = "Kesimpulan", status = "primary", solidHeader = TRUE, width = 12, verbatimTextOutput("kesimpulanVarians"))
              ),
              actionButton("toAnova", "Lanjut ke Uji ANOVA")
      ),
      tabItem("anova",
              uiOutput("statusBox"),
              fluidRow(
                box(title = "Hipotesis Uji ANOVA", status = "primary", solidHeader = TRUE, width = 6, 
                withMathJax("$$
                H_0: \\mu_1 = \\mu_2 = \\cdots = \\mu_k \\\\
                H_1: \\mu_i \\ne \\mu_j, \\ \\forall i \\ne j;\\ i,j = 1,2,3,...,k \\\\ 
                $$")),
                box(title = "Taraf Signifikansi", status = "primary", solidHeader = TRUE, width = 6, textOutput("tarafAnova"))
              ),
              fluidRow(
                box(title = "Hasil", status = "primary", solidHeader = TRUE, width = 12, verbatimTextOutput("anovaResult")),
                box(title = "Keputusan", status = "primary", solidHeader = TRUE, width = 12, verbatimTextOutput("keputusanAnova")),
                box(title = "Kesimpulan", status = "primary", solidHeader = TRUE, width = 12, verbatimTextOutput("kesimpulanAnova")),
                box(title = "Boxplot Antar Kelompok", status = "primary", solidHeader = TRUE, width = 12, plotOutput("boxplotVarians"))
              ),
              conditionalPanel("output.anovaSig == true", actionButton("toTukey", "Lanjut ke Uji Tukey"))
      ),
      tabItem("tukey",
              uiOutput("statusBox"),
              fluidRow(
                box(title = "Hipotesis Uji Tukey", status = "primary", solidHeader = TRUE, width = 6, 
                withMathJax("$$
                H_0 : \\mu_i = \\mu_j \\\\
                H_1 : \\mu_i \\ne \\mu_j
                $$")),
                box(title = "Taraf Signifikansi", status = "primary", solidHeader = TRUE, width = 6, textOutput("tarafTukey"))
              ),
              fluidRow(
                box(title = "Hasil", status = "primary", solidHeader = TRUE, width = 12, verbatimTextOutput("tukeyResult")),
                box(title = "Keputusan", status = "primary", solidHeader = TRUE, width = 12, verbatimTextOutput("keputusanTukey")),
                box(title = "Kesimpulan", status = "primary", solidHeader = TRUE, width = 12, verbatimTextOutput("kesimpulanTukey")),
                box(title = "Plot", status = "primary", solidHeader = TRUE, width = 12, plotOutput("plotTukey"))
              ),
              box(width = 12,
                  div(style = "text-align: center;",
                      div(style = "display: inline-block; margin-right: 20px;",
                          actionButton("back_to_home", "Kembali ke Halaman Awal", class = "btn btn-primary",style = "color: white; font-weight: bold;")
                      ),
                      div(style = "display: inline-block;",
                          actionButton("uji_data_lain", "Ayo Uji Data Lain", class = "btn btn-primary",style = "color: white; font-weight: bold;")
                      )
            )
          )
        )
      )
   )
)

server <- function(input, output, session) {
  dataInput <- reactiveVal()
  hasilNormal <- reactiveVal()
  hasilVarians <- reactiveVal()
  hasilAnova <- reactiveVal()
  hasilTukey <- reactiveVal()
  selectedNumericVar <- reactiveVal()
  selectedGroupVar <- reactiveVal()
  
  observeEvent(input$file1, {
    req(input$dataSourceType == "upload", input$file1, input$sep)
    ext <- tools::file_ext(input$file1$name)
    tryCatch({
      df <- switch(ext,
                   "csv" = read.csv(input$file1$datapath, sep = input$sep),
                   "xls" = read_excel(input$file1$datapath),
                   "xlsx" = read_excel(input$file1$datapath),
                   "tsv" = read.delim(input$file1$datapath),
                   stop("Format file tidak didukung.")
      )
      dataInput(as.data.frame(df))
    }, error = function(e) {
      showModal(modalDialog(
        title = "Gagal Membaca File",
        paste("Kesalahan:", e$message)
      ))
    })
  })
  
  observeEvent(input$dataSource, {
    req(input$dataSourceType == "builtin")
    df <- switch(input$dataSource,
                 "iris" = iris,
                 "PlantGrowth" = PlantGrowth,
                 "mtcars" = mtcars)
    dataInput(df)
  })
  
  output$dataPreview <- renderTable({ req(dataInput()); head(dataInput(), 10) })
  
  output$varSelect <- renderUI({
    req(dataInput())
    selectInput("numericVar", "Variabel Numerik", names(Filter(is.numeric, dataInput())))
  })
  
  output$groupSelect <- renderUI({
    req(dataInput())
    selectInput("groupVar", "Variabel Grup", names(dataInput()))
  })
  
    observe({
    req(input$dark_mode)
    session$sendCustomMessage(
      type = "toggleDark",
      message = input$dark_mode == "dark"
    )
  })
  
  observeEvent(input$submitData, { 
    selectedNumericVar(input$numericVar) 
    selectedGroupVar(input$groupVar)
    updateTabItems(session, "tabs", "normal") })
  
  output$tarafNormal <- renderText({ 
    req(selectedNumericVar(), selectedGroupVar())
    paste("\u03B1 =", input$alpha)
  })
  
  output$hasilNormal <- renderPrint({
    req(dataInput(), selectedNumericVar(), selectedGroupVar())
    df <- dataInput()
    result <- lapply(split(df[[selectedNumericVar()]], df[[selectedGroupVar()]]), shapiro.test)
    hasilNormal(result)
    for (group in names(result)) {
      cat("Kelompok", group, ": Statistik =", result[[group]]$statistic, ", p-value =", result[[group]]$p.value, "\n")
    }
  })
  
  output$keputusanNormal <- renderPrint({
    req(hasilNormal())
    for (group in names(hasilNormal())) {
      pval <- hasilNormal()[[group]]$p.value
      cat("Kelompok", group, ":",
          ifelse(pval < input$alpha,
                 paste("Tolak H0 karena p-value =", signif(pval, 5), "< α =", input$alpha),
                 paste("Terima H0 karena p-value =", signif(pval, 5), "≥ α =", input$alpha)),
          "\n")
    }
  })
  
  output$kesimpulanNormal <- renderPrint({
    req(hasilNormal())
    if (all(sapply(hasilNormal(), function(x) x$p.value > input$alpha))) {
      cat("Kesimpulan: Data berdistribusi normal.")
    } else {
      cat("Kesimpulan: Data tidak berdistribusi normal.")
    }
  })
  
  output$plotNormal <- renderPlot({
    req(dataInput(), selectedNumericVar(), selectedGroupVar())
    df <- dataInput()
    
    # Histogram per kategori (facet)
    p1 <- ggplot(df, aes_string(x = selectedNumericVar())) +
      geom_histogram(color = "black", fill = "skyblue", bins = 20) +
      facet_wrap(as.formula(paste("~", selectedGroupVar())), ncol = 2, scales = "free_y") +
      theme_minimal() +
      labs(
        title = "Histogram per Kelompok",
        y = "Frekuensi",
        x = selectedNumericVar()
      )
    
    # Histogram seluruh data
    p2 <- ggplot(df, aes_string(x = selectedNumericVar())) +
      geom_histogram(color = "black", fill = "salmon", bins = 20) +
      theme_minimal() +
      labs(
        title = "Histogram Seluruh Data",
        y = "Frekuensi",
        x = selectedNumericVar()
      )
    
    # Gabungkan keduanya
    gridExtra::grid.arrange(p1, p2, ncol = 1)
  })
  
  output$qqNormal <- renderPlot({
    req(dataInput(), selectedNumericVar(), selectedGroupVar())
    df <- dataInput()
    
    ggplot(df, aes_string(sample = selectedNumericVar())) +
      stat_qq() + stat_qq_line() +
      facet_wrap(as.formula(paste("~", selectedGroupVar()))) +
      theme_minimal() +
      labs(title = "Q-Q Plot per Kelompok")
  })
  
  output$interpretasiNormal <- renderPrint({
    req(hasilNormal())
    for (group in names(hasilNormal())) {
      pval <- hasilNormal()[[group]]$p.value
      cat("Kelompok", group, ":\n")
      if (pval > input$alpha) {
        cat("- P-value >", input$alpha, ": Data berdistribusi normal\n")
        cat("- Histogram dan Q-Q Plot seharusnya menunjukkan simetris & mengikuti garis diagonal\n")
      } else {
        cat("- P-value <", input$alpha, ": Data tidak berdistribusi normal\n")
        cat("- Histogram tidak simetris atau Q-Q Plot menyimpang dari garis normal\n")
      }
      cat("\n")
    }
  })
  
  observeEvent(input$toVarians, { updateTabItems(session, "tabs", "varians") })
  
  output$tarafVarians <- renderText({ paste("\u03B1 =", input$alpha) })
  
  output$variansResult <- renderPrint({
    req(dataInput(), selectedNumericVar(), selectedGroupVar())
    df <- dataInput()
    result <- leveneTest(as.formula(paste(selectedNumericVar(), "~", selectedGroupVar())), data = df)
    hasilVarians(result)
    print(result)
  })
  
  output$keputusanVarians <- renderPrint({
    req(hasilVarians())
    pval <- as.numeric(hasilVarians()[["Pr(>F)"]][1])
    if (pval < input$alpha) {
      cat("Keputusan: Tolak H0 karena varians tidak homogen.\n")
    } else {
      cat("Keputusan: Terima H0 karena varians homogen.\n")
    }
  })
  
  output$kesimpulanVarians <- renderPrint({
    req(hasilVarians())
    pval <- as.numeric(hasilVarians()[["Pr(>F)"]][1])
    if (pval < input$alpha) {
      cat("Kesimpulan: Varians antar kelompok tidak homogen.")
    } else {
      cat("Kesimpulan: Varians antar kelompok homogen.")
    }
  })
  
  observeEvent(input$toAnova, { updateTabItems(session, "tabs", "anova") })
  
  output$tarafAnova <- renderText({ paste("\u03B1 =", input$alpha) })
  
  output$anovaResult <- renderPrint({
    req(dataInput(), selectedNumericVar(), selectedGroupVar())
    df <- na.omit(dataInput())
    model <- aov(as.formula(paste(selectedNumericVar(), "~", selectedGroupVar())), data = df)
    hasil <- summary(model)
    hasilAnova(hasil)  # simpan ke reactiveVal
    print(hasil)
  })
  
  output$keputusanAnova <- renderPrint({
    req(hasilAnova())
    pval <- hasilAnova()[[1]]$"Pr(>F)"[1]
    
    if (is.na(pval)) {
      cat("Keputusan: Tidak dapat dihitung karena nilai p-value tidak tersedia (NA).")
    } else if (pval < input$alpha) {
      cat("Keputusan: Tolak H0 karena terdapat perbedaan rata-rata antar kelompok.")
    } else {
      cat("Keputusan: Terima H0 karena tidak terdapat perbedaan rata-rata antar kelompok.")
    }
  })
  
  output$kesimpulanAnova <- renderPrint({
    req(hasilAnova())
    pval <- hasilAnova()[[1]]$"Pr(>F)"[1]
    
    if (is.na(pval)) {
      cat("Kesimpulan: Tidak dapat disimpulkan karena nilai p-value tidak tersedia (NA).")
    } else if (pval < input$alpha) {
      cat("Kesimpulan: Rata-rata antar kelompok berbeda secara signifikan.")
    } else {
      cat("Kesimpulan: Rata-rata antar kelompok tidak berbeda secara signifikan.")
    }
  })
  
  output$anovaSig <- reactive({
    res <- hasilAnova()
    if (!is.null(res)) return(res[[1]]$"Pr(>F)"[1] < input$alpha)
    FALSE
  })
  outputOptions(output, "anovaSig", suspendWhenHidden = FALSE)
  
  output$boxplotVarians <- renderPlot({
    req(dataInput(), selectedNumericVar(), selectedGroupVar())
    df <- dataInput()
    ggplot(df, aes_string(x = selectedGroupVar(), y = selectedNumericVar())) +
      geom_boxplot(aes(fill = .data[[selectedGroupVar()]]), color = "black") +
      theme_minimal() +
      labs(
        title = "",
        y = selectedNumericVar(),
        x = selectedGroupVar()
      )
  })
  
  observeEvent(input$toTukey, { updateTabItems(session, "tabs", "tukey") })
  output$tarafTukey <- renderText({ paste("\u03B1 =", input$alpha) })
  
  output$tukeyResult <- renderPrint({
    req(dataInput(), selectedNumericVar(), selectedGroupVar())
    df <- dataInput()
    model <- aov(as.formula(paste(selectedNumericVar(), "~", selectedGroupVar())), data = df)
    tukey <- TukeyHSD(model)
    
    hasilTukey(tukey[[selectedGroupVar()]]) 
    print(tukey[[selectedGroupVar()]])
  })
  
  output$keputusanTukey <- renderPrint({
    req(hasilTukey())
    tukey_df <- as.data.frame(hasilTukey())
    for (i in seq_len(nrow(tukey_df))) {
      pair <- rownames(tukey_df)[i]
      pval <- tukey_df[i, "p adj"]
      
      cat("Pasangan", pair, ":\n")
      
      if (is.na(pval)) {
        cat("- p-value tidak tersedia. Tidak dapat diambil keputusan.\n\n")
      } else if (pval < input$alpha) {
        cat("- p-value =", signif(pval, 5), "< α =", input$alpha, 
            "→ Tolak H0 → Ada perbedaan signifikan\n\n")
      } else {
        cat("- p-value =", signif(pval, 5), "≥ α =", input$alpha, 
            "→ Terima H0 → Tidak ada perbedaan signifikan\n\n")
      }
    }
  })
  
  output$kesimpulanTukey <- renderPrint({
    req(hasilTukey())
    tukey_df <- as.data.frame(hasilTukey())
    pvals <- tukey_df[["p adj"]]
    
    # Cek apakah ada p-value yang valid dan < alpha
    if (!is.null(pvals) && any(!is.na(pvals) & pvals < input$alpha)) {
      signif_rows <- rownames(tukey_df)[!is.na(pvals) & pvals < input$alpha]
      
      cat("Kesimpulan: Terdapat perbedaan signifikan antara pasangan kelompok berikut:\n")
      for (pair in signif_rows) {
        cat("-", pair, "\n")
      }
    } else {
      cat("Kesimpulan: Tidak terdapat perbedaan signifikan antara pasangan kelompok manapun.")
    }
  })
  
  output$plotTukey <- renderPlot({
    req(dataInput(), selectedNumericVar(), selectedGroupVar())
    df <- dataInput()
    model <- aov(as.formula(paste(selectedNumericVar(), "~", selectedGroupVar())), data = df)
    plot(TukeyHSD(model))
  })

  observeEvent(input$back_to_home, {
    updateTabItems(session, "tabs", selected = "home")
  })
  
  observeEvent(input$uji_data_lain, {
    updateTabItems(session, "tabs", selected = "input")  
  })
            
  output$statusBox <- renderUI({
    if (is.null(hasilNormal())) return()
    normColor <- if (all(sapply(hasilNormal(), function(x) x$p.value > input$alpha))) "green" else "red"
    
    varPval <- tryCatch({
      as.numeric(hasilVarians()$"Pr(>F)"[1])
    }, error = function(e) NA)
    varColor <- if (!is.na(varPval) && varPval > input$alpha) "green" else "red"
    
    aovPval <- tryCatch({
      hasilAnova()[[1]][["Pr(>F)"]][1]
    }, error = function(e) NA)
    aovColor <- if (!is.na(aovPval) && aovPval <= input$alpha) "green" else "green"
    
    tagList(
      tags$style(HTML("
      .status-row { display: flex; justify-content: space-between; margin-bottom: 10px; }
      .status-box {
        flex: 1;
        padding: 15px;
        margin-right: 10px;
        text-align: center;
        font-weight: bold;
        color: white;
        border-radius: 10px;
      }
      .green { background-color: #28a745; }
      .red { background-color: #dc3545; }
    ")),
      div(class = "status-row",
          div(class = paste("status-box", normColor), "Normalitas: ", ifelse(normColor == "green", "Normal", "Tidak Normal")),
          div(class = paste("status-box", varColor), "Homogenitas: ", ifelse(varColor == "green", "Homogen", "Tidak Homogen")),
          div(class = paste("status-box", aovColor), "ANOVA: ", ifelse(aovColor == "green", "Rata-Rata Kelompok Berbeda", "Rata-Rata Kelompok Tidak Berbeda"))
      )
    )
  })
}
shinyApp(ui, server)
