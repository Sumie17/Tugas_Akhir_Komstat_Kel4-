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
        
        # Judul dan Logo saja, TANPA switchInput
        tags$span("AnovaLab", style = "font-weight: bold; font-size: 18px;")
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
                menuItem("Uji Homogenitas", tabName = "varians", icon = icon("balance-scale")),
                menuItem("ANOVA", tabName = "anova", icon = icon("project-diagram")),
                menuItem("Tukey", tabName = "tukey", icon = icon("list"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "dark-theme.css"),
      
      tags$style(HTML("
  /* === KONTEN UTAMA === */
  /* Background konten utama (selalu pakai motif.jpg, baik light/dark) */
  .content-wrapper, .right-side {
    background-image: url('motif.jpg');
    background-size: cover;
    background-repeat: no-repeat;
    background-attachment: fixed;
    background-position: top center;
  }
  /* Kotak box putih di atas motif */
  .box {
    background-color: rgba(255, 255, 255, 0.85) !important;
  }
  /* Tab home transparan */
  #tab-home .content {
    background-color: transparent !important;
  }
  /* === SIDEBAR === */
  /* Sidebar selalu gelap, teks putih */
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
  /* Radio button tetap terbaca di sidebar */
  .main-sidebar .radio label {
    color: white !important;
  }
  /* Judul dan teks di sidebar */
  .main-sidebar h4, .main-sidebar h3, .main-sidebar h2, .main-sidebar h1,
  .main-sidebar p {
    color: white !important;
  }
    /* Override warna teks di tab home */
  #home {
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
              withMathJax(),
              div(
                style = "padding: 30px; font-family: 'Segoe UI', sans-serif;",
                
                # Header dan Gambar Depan
                div(
                  style = "text-align: center;",
                  img(src = "stat.jpg", width = "60%", 
                      style = "border-radius: 15px; box-shadow: 0 4px 12px rgba(0,0,0,0.3); margin-bottom: 20px;"),
                  h2("SELAMAT DATANG DI AnovaLab!", 
                     style = "font-weight: bold; color: #2c3e50;"),
                  p("AnovaLab membantu Anda melakukan analisis ANOVA satu arah secara interaktif dan menyenangkan.", 
                    style = "font-size: 16px; color: #555; margin-top: 10px; max-width: 700px; margin-left: auto; margin-right: auto;")
                ),
                
                br(), hr(), br(),
                
                # Apa itu ANOVA
                h3("📌 Apa itu ANOVA Satu Arah?"),
                p("ANOVA satu arah (Analysis of Variance) adalah metode statistik untuk menguji apakah terdapat perbedaan yang signifikan antara rata-rata tiga kelompok atau lebih. ANOVA memungkinkan kita menghindari pengujian berulang menggunakan uji t dan mengontrol kesalahan tipe I."),
                
                br(),
                
                # Rumus ANOVA
                h3("📈 Uji ANOVA Satu Arah (One-Way ANOVA)"),
                p("Digunakan untuk mengecek apakah terdapat perbedaan rata-rata antara tiga kelompok atau lebih."),
                h4("📊 Tabel ANOVA"),
                div(style = "text-align: center;",
                    p("$$F = \\frac{MS_{\\text{Between}}}{MS_{\\text{Within}}} = \\frac{\\frac{SS_{\\text{Between}}}{k - 1}}{\\frac{SS_{\\text{Within}}}{N - k}}$$")
                ),
                tags$table(
                  border = 1, 
                  cellpadding = "6", 
                  style = "margin: auto; font-size: 15px; background-color: white; border-collapse: collapse; box-shadow: 0 2px 5px rgba(0,0,0,0.2);",
                  
                  tags$tr(
                    style = "background-color: #007bff; color: white; font-weight: bold;",
                    tags$th("Sumber Variasi", style = "padding: 8px 12px;"),
                    tags$th("df", style = "padding: 8px 12px;"),
                    tags$th("SS", style = "padding: 8px 12px;"),
                    tags$th("MS", style = "padding: 8px 12px;"),
                    tags$th("F Hitung", style = "padding: 8px 12px;")
                  ),
                  
                  tags$tr(
                    tags$td("Antar Grup (Between)", style = "padding: 8px 12px;"),
                    tags$td("k−1", style = "padding: 8px 12px;"),
                    tags$td("SS_Between", style = "padding: 8px 12px;"),
                    tags$td("MS_Between = SS_Between / (k−1)", style = "padding: 8px 12px;"),
                    tags$td("F = MS_Between / MS_Within", style = "padding: 8px 12px;")
                  ),
                  tags$tr(
                    tags$td("Dalam Grup (Within)", style = "padding: 8px 12px;"),
                    tags$td("N−k", style = "padding: 8px 12px;"),
                    tags$td("SS_Within", style = "padding: 8px 12px;"),
                    tags$td("MS_Within = SS_Within / (N−k)", style = "padding: 8px 12px;"),
                    tags$td("", style = "padding: 8px 12px;")
                  ),
                  tags$tr(
                    tags$td("Total", style = "padding: 8px 12px;"),
                    tags$td("N−1", style = "padding: 8px 12px;"),
                    tags$td("SS_Total", style = "padding: 8px 12px;"),
                    tags$td("", style = "padding: 8px 12px;"),
                    tags$td("", style = "padding: 8px 12px;")
                  )
                ),
                tags$p(strong("✅ Interpretasi:")),
                tags$ul(
                  tags$li("Gagal Tolak \\(H_0\\): \\(p > 0.05\\) → Rata-rata kelompok sama."),
                  tags$li("Tolak \\(H_0\\): \\(p \\leq 0.05\\) → Terdapat perbedaan signifikan antar kelompok.")
                ),
                
                br(), hr(), br(),
                
                # Mengapa ANOVA
                h3("🤔 Mengapa Menggunakan ANOVA?"),
                tags$ul(
                  tags$li("✔ Menghindari uji t berulang-ulang antar pasangan grup."),
                  tags$li("✔ Mengontrol kesalahan tipe I yang meningkat saat melakukan banyak pengujian."),
                  tags$li("✔ Menyediakan analisis menyeluruh terhadap data multikelompok.")
                ),
                
                br(),
                
                # Asumsi Dasar ANOVA
                h3("⚠ Asumsi Dasar ANOVA"),
                p("Sebelum melakukan uji ANOVA, ada dua asumsi penting yang perlu diuji terlebih dahulu, yaitu kenormalan dan homogenitas varians."),
                
                # Shapiro-Wilk Test
                h4("1️⃣ Uji Kenormalan (Shapiro-Wilk)"),
                p("Digunakan untuk mengecek apakah data berdistribusi normal."),
                div(style = "text-align: center;",
                    p("$$W = \\frac{\\left(\\sum_{i=1}^{n/2} a_i x_i \\right)^2}{\\sum_{i=1}^{n} (x_i - \\bar{x})^2}$$")
                ),
                tags$ul(
                  tags$li("\\(x_i\\): data yang diurutkan"),
                  tags$li("\\(\\bar{x}\\): rata-rata data"),
                  tags$li("\\(a_i\\): konstanta berdasarkan kovarians normal"),
                  tags$li("Nilai \\(W\\) mendekati 1 → distribusi mendekati normal")
                ),
                tags$p(strong("✅ Interpretasi:")),
                tags$ul(
                  tags$li("Gagal Tolak \\(H_0\\): \\(p > 0.05\\) → Data berdistribusi normal."),
                  tags$li("Tolak \\(H_0\\): \\(p \\leq 0.05\\) → Data tidak berdistribusi normal.")
                ),
                
                br(),
                
                # Levene's Test
                h4("2️⃣ Uji Homogenitas Varians (Levene's Test)"),
                p("Digunakan untuk mengecek apakah variansi antar grup adalah homogen."),
                div(style = "text-align: center;",
                    p("$$W = \\frac{(N - k)}{(k - 1)} \\cdot \\frac{\\sum_{i=1}^{k} n_i (Z_{i\\cdot} - Z_{\\cdot\\cdot})^2}{\\sum_{i=1}^{k} \\sum_{j=1}^{n_i} (Z_{ij} - Z_{i\\cdot})^2}$$")
                ),
                tags$ul(
                  tags$li("\\(Z_{ij} = |Y_{ij} - \\tilde{Y}_i|\\): selisih absolut antara data ke-\\(j\\) pada grup ke-\\(i\\) dengan median grup"),
                  tags$li("\\(Z_{i\\cdot}\\): rata-rata nilai \\(Z\\) pada grup ke-\\(i\\)"),
                  tags$li("\\(Z_{\\cdot\\cdot}\\): rata-rata keseluruhan nilai \\(Z\\)"),
                  tags$li("\\(N\\): total sampel, \\(k\\): jumlah grup")
                ),
                tags$p(strong("✅ Interpretasi:")),
                tags$ul(
                  tags$li("Gagal Tolak \\(H_0\\): \\(p \\geq 0.05\\) → Varians homogen."),
                  tags$li("Tolak \\(H_0\\): \\(p < 0.05\\) → Varians tidak homogen.")
                ),
                
                br(), hr(), br(),
                
                # Tukey
                h3("🔍 Uji Lanjutan Tukey HSD (Post-hoc)"),
                p("Jika hasil ANOVA signifikan, maka uji Tukey dilakukan untuk mengetahui pasangan grup mana yang berbeda signifikan."),
                div(style = "text-align: center;",
                    p("$$HSD = q_{(a, k, df_{\\text{within}})} \\cdot \\sqrt{\\frac{MS_{\\text{within}}}{n}}$$")
                ),
                tags$ul(
                  tags$li("\\(q_{(a, k, df_{\\text{within}})}\\): nilai kritis dari distribusi studentized range"),
                  tags$li("\\(MS_{\\text{within}}\\): galat kuadrat rata-rata dalam grup (dari ANOVA)"),
                  tags$li("\\(n\\): jumlah pengamatan per grup (jika data seimbang)")
                ),
                tags$p(strong("✅ Interpretasi:")),
                tags$ul(
                  tags$li("Gagal Tolak \\(H_0\\): \\(p \\geq 0.05\\) → Rata-rata antar kelompok tidak berbeda signifikan."),
                  tags$li("Tolak \\(H_0\\): \\(p < 0.05\\) → Rata-rata antar kelompok berbeda signifikan.")
                ),
                
                # Cara Menggunakan
                h3("🛠️ Cara Menggunakan Aplikasi Ini"),
                tags$ol(
                  tags$li("Buka tab 'Input Data' untuk unggah file CSV/XLSX atau gunakan data contoh."),
                  tags$li("Pilih variabel numerik dan kategori, lalu klik tombol OK."),
                  tags$li("Gunakan tab 'Uji Kenormalan' untuk menjalankan Shapiro-Wilk."),
                  tags$li("Gunakan tab 'Uji Homogenitas' untuk menjalankan Levene’s Test."),
                  tags$li("Masuk ke tab 'ANOVA' untuk melihat hasil pengujian F."),
                  tags$li("Jika signifikan, buka tab 'Tukey' untuk melihat pasangan grup yang berbeda.")
                ),
                
                br(), hr(), br(),
                
                # Penutup
                div(style = "text-align: center; font-size: 14px; color: black;",
                    "Aplikasi ini dikembangkan dengan semangat belajar statistik dan cinta terhadap data. 💙")
              )
      ),
  
      tabItem("input",
              fluidRow(
                box(title = "Upload dan Pilih Data", status = "primary", solidHeader = TRUE, width = 4,
                    
                    # RADIO BUTTON PILIH SUMBER DATA
                    radioButtons("dataSourceType", "Pilih Sumber Data",
                                 choices = c("Upload Data" = "upload",
                                             "Dataset R" = "builtin"),
                                 selected = "upload"),
                    
                    # KONDISI UPLOAD
                    conditionalPanel(
                      condition = "input.dataSourceType == 'upload'",
                      fileInput("file1", "Upload File (CSV/XLSX/XLS/TSV)"),
                      radioButtons("sep", "Pemisah", 
                                   choices = c("koma ," = ",", "titik koma ;" = ";", "tab \\t" = "\t"))
                    ),
                    
                    # KONDISI DATA BAWAAN
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
                box(title = "Hipotesis Uji Kenormalan", status = "primary", solidHeader = TRUE, width = 6, 
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
    aovColor <- if (!is.na(aovPval) && aovPval <= input$alpha) "green" else "red"
    
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
