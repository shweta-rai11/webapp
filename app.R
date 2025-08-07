library(shiny)
library(GEOquery)
library(limma)
library(DT)
library(ggplot2)
library(pheatmap)
library(bslib)
library(shinyjs)
library(googledrive)
library(promises)
library(future)
library(matrixStats)
future::plan(future::multisession)


ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = font_google("Roboto"),
    heading_font = font_google("Montserrat"),
    primary = "#1abc9c",
    secondary = "#34495e"
  ),
  
  # Head: CSS + JS combined
  tags$head(
    tags$style(HTML("
      html, body {
        height: 100%;
        margin:0; padding:0;
        font-family:'Roboto', sans-serif;
        color:#2c3e50;
        overflow-x:hidden;
        background-color: #f5f7fa;
        transition: all 0.3s ease;
      }

      body::before {
        content:'';
        position:fixed; top:0; left:0;
        width:100%; height:100%;
        background: 
          linear-gradient(45deg, rgba(26,188,156,0.15), rgba(231,76,60,0.15), rgba(52,152,219,0.15), rgba(241,196,15,0.15)),
          url('https://images.unsplash.com/photo-1581092795368-b8fa623b09f6?auto=format&fit=crop&w=1600&q=80') no-repeat center center fixed;
        background-size: cover;
        z-index:-3;
      }

      .container-fluid {
        min-height: calc(100vh - 60px);
        background-color: rgba(255,255,255,0.95);
        border-radius: 15px;
        box-shadow: 0 12px 40px rgba(0,0,0,0.25);
        padding: 30px 40px;
        margin-top: 20px;
        margin-bottom: 40px;
        position: relative;
        z-index: 1;
        transition: all 0.3s ease;
      }
      .container-fluid:hover {
        transform: translateY(-5px);
        box-shadow: 0 20px 50px rgba(0,0,0,0.3);
      }

      .sidebarPanel {
        background: linear-gradient(135deg, #1abc9c, #16a085, #9b59b6, #f39c12);
        color:white;
        border-radius:15px;
        box-shadow:0 8px 30px rgba(0,0,0,0.3);
        padding: 25px;
        background-size: 400% 400%;
        animation: sidebarGradient 12s ease infinite;
        transition: all 0.3s ease;
      }
      .sidebarPanel:hover {
        box-shadow: 0 12px 40px rgba(0,0,0,0.5);
        transform: scale(1.02);
      }
      @keyframes sidebarGradient {
        0%{background-position:0% 50%;}
        50%{background-position:100% 50%;}
        100%{background-position:0% 50%;}
      }
      .sidebarPanel h4 {color:#fff; margin-bottom:20px; font-weight:700;}

      .btn-run {
        border-radius: 50px !important;
        background: linear-gradient(45deg, #e74c3c, #f39c12, #e74c3c);
        color:white; font-weight:bold; width:100%;
        box-shadow: 0 0 15px rgba(231,76,60,0.6);
        transition: all 0.3s ease;
        font-size:1.1rem;
        letter-spacing:0.5px;
        background-size: 200% 200%;
      }
      .btn-run:hover {
        background-position: right center;
        box-shadow: 0 0 50px rgba(26,188,156,0.9);
        transform: scale(1.1);
      }

      .tab-content {
        border-left: 5px solid #1abc9c;
        padding: 30px;
        border-radius: 20px;
        background: rgba(255,255,255,0.98);
        box-shadow: 0 8px 35px rgba(0,0,0,0.18);
        transition: all 0.4s ease;
      }
      .tab-content:hover {
        transform: translateY(-5px) scale(1.01);
        box-shadow: 0 12px 45px rgba(0,0,0,0.25);
      }

      input, select, textarea {transition: all 0.3s ease;}
      input:focus, select:focus, textarea:focus {
        border-color: #1abc9c;
        box-shadow: 0 0 8px rgba(26,188,156,0.5);
      }

      .dataTables_wrapper .dataTables_filter input {
        border-radius: 20px;
        border: 1px solid #1abc9c;
        padding: 5px 15px;
        transition: all 0.3s ease;
      }
      .dataTables_wrapper .dataTables_filter input:focus {
        box-shadow: 0 0 10px rgba(26,188,156,0.6);
      }

      .floating-dna {
        position: fixed;
        font-size: 120px;
        color: rgba(255,255,255,0.9);
        text-shadow: 0 0 15px rgba(0,0,0,0.6);
        z-index: 9999;
        user-select: none;
        pointer-events: none;
        animation: floatDNA linear infinite, dnaFade 10s ease-in-out infinite alternate;
        filter: drop-shadow(0 0 10px rgba(26,188,156,0.8));
      }

      @keyframes dnaFade {
        0% {opacity:0.7;}
        50% {opacity:1;}
        100% {opacity:0.9;}
      }

      .dna-left { left: 2%; transform: rotate(-10deg); animation-duration: 50s; }
      .dna-left2 { left: 25%; transform: rotate(-5deg); animation-duration: 60s; }
      .dna-right { right: 25%; transform: rotate(10deg); animation-duration: 65s; }
      .dna-right2 { right: 2%; transform: rotate(5deg); animation-duration: 55s; }

      @keyframes floatDNA {
        0% {transform: translateY(-20%) rotate(0deg);}
        100% {transform: translateY(120%) rotate(360deg);}
      }

      footer {
        text-align:center;
        padding:10px;
        font-size:0.85rem;
        color:#888;
        text-shadow: 0 0 8px rgba(26,188,156,0.5);
        transition: all 0.3s ease;
      }
      footer:hover { color:#555; }

      h3, h4 {transition: all 0.3s ease;}
      h3:hover, h4:hover {
        color:#1abc9c;
        text-shadow: 0 2px 5px rgba(0,0,0,0.2);
      }

      #download_up_female, #download_down_female,
      #download_up_male, #download_down_male {
        animation: pulse 2.5s infinite;
      }
      @keyframes pulse {
        0% { transform: scale(1); box-shadow:0 0 10px rgba(231,76,60,0.5);}
        50% { transform: scale(1.05); box-shadow:0 0 20px rgba(231,76,60,0.7);}
        100% { transform: scale(1); box-shadow:0 0 10px rgba(231,76,60,0.5);}
      }
      /* Fire glow animation for tab hover */
@keyframes fireGlowBorder {
  0%, 100% {
    box-shadow:
      0 0 5px #ff4500,
      0 0 10px #ff6347,
      0 0 15px #ff4500,
      0 0 20px #ff6347,
      0 0 30px #ff4500;
  }
  50% {
    box-shadow:
      0 0 10px #ffa07a,
      0 0 15px #ff4500,
      0 0 20px #ff6347,
      0 0 25px #ffa07a,
      0 0 40px #ff4500;
  }
}

/* Target the entire tab button on hover */
.nav-tabs > li > a:hover {
  animation: fireGlowBorder 1.5s infinite alternate;
  color: #fff !important;
  background-color: #2c3e50 !important;
  border-color: #ff4500 !important;
  /* To have that fire glow effect all around */
  box-shadow: 0 0 15px #ff4500;
  transition: all 0.3s ease;
  position: relative;
  z-index: 2;
}

/* Also add a subtle glowing effect for the active tab */
.nav-tabs > li.active > a, 
.nav-tabs > li.active > a:focus, 
.nav-tabs > li.active > a:hover {
  background-color: #1abc9c !important;
  border-color: #ff4500 !important;
  color: white !important;
  box-shadow:
    0 0 10px #ff4500,
    0 0 20px #ff6347,
    0 0 30px #ff4500;
  transition: all 0.3s ease;
}
.welcome-header {
  font-weight: 900;
  font-size: 3.5rem;
  color: #1abc9c;
  text-align: center;
  text-shadow:
    2px 2px 5px rgba(26, 188, 156, 0.7),
    0 0 20px rgba(26, 188, 156, 0.8),
    0 0 30px rgba(26, 188, 156, 0.9);
  margin-bottom: 20px;
  padding: 15px;
  border-radius: 15px;
  background: rgba(26, 188, 156, 0.1);
  box-shadow:
    0 0 20px rgba(26, 188, 156, 0.4);
  transition: all 0.4s ease;
}
.welcome-header:hover {
  color: #16a085;
  text-shadow:
    2px 2px 8px rgba(22, 160, 133, 0.9),
    0 0 30px rgba(22, 160, 133, 1),
    0 0 40px rgba(22, 160, 133, 1.2);
  background: rgba(22, 160, 133, 0.15);
  box-shadow:
    0 0 40px rgba(22, 160, 133, 0.7);
  transform: scale(1.05);
}


    ")),
    
    # Confetti JS
    tags$script(src = "https://cdn.jsdelivr.net/npm/canvas-confetti@1.5.1/dist/confetti.browser.min.js"),
    tags$script(HTML("
      function celebrate() {
        var duration = 4000; // 4 seconds
        var end = Date.now() + duration;
        (function frame() {
          confetti({
            particleCount: 5 + Math.random() * 5,
            angle: 60,
            spread: 55,
            origin: { x: Math.random(), y: Math.random() - 0.2 }
          });
          confetti({
            particleCount: 5 + Math.random() * 5,
            angle: 120,
            spread: 55,
            origin: { x: Math.random(), y: Math.random() - 0.2 }
          });
          if (Date.now() < end) {
            requestAnimationFrame(frame);
          }
        }());
      }
    "))
  ),
  
  tags$div(
    id = "successModal",
    style = "display:none; position: fixed; top: 50%; left: 50%; transform: translate(-50%, -50%);
             background-color: white; border: 4px solid #4CAF50; padding: 40px; z-index: 9999;
             text-align: center; font-size: 20px; font-weight: bold; color: #4CAF50; 
             box-shadow: 0 5px 15px rgba(0,0,0,0.3); border-radius: 15px;",
    "🎉 Data successfully loaded! 🎉"
  ),
  
  # JavaScript to auto-hide the modal after 4 seconds
  tags$script(HTML("
    Shiny.addCustomMessageHandler('showSuccessModal', function(message) {
      var modal = document.getElementById('successModal');
      modal.style.display = 'block';
      setTimeout(function() {
        modal.style.display = 'none';
      }, 4000);
    });
  ")),
  
  
  # Four floating DNA strands
  tags$div(class="floating-dna dna-left", "🧬"),
  tags$div(class="floating-dna dna-right2", "🧬"),
  
  div(class = "container-fluid",
      titlePanel(
        div(style = "font-weight:700; font-size:3rem; margin-bottom:1rem; color:#2c3e50;",
            "Differential Gene Expression Analysis Explorer")
      ),
      
      sidebarLayout(
        conditionalPanel(
          condition = "input.tabs !== 'Home'",
          sidebarPanel(
            tags$h4("Bioinformatics Explorer"),
            p("Explore GEO datasets by gender. Set thresholds and run analysis to view DEGs, volcano plots, and heatmaps."),
            textInput("geo_id", "Enter GEO ID:", value = "GSE93272"),
            numericInput("logfc_thresh", "Log2 Fold Change Threshold", value = 0.2, step = 0.1),
            numericInput("pval_thresh", "Adjusted P-Value Threshold", value = 0.05, step = 0.01),
            actionButton("run", "Run DEG Analysis", class = "btn-run"),
            br(), br(),
            p("Select tabs to view expression, phenotype, DEG results, and heatmaps."),
            width = 3
          )
        ),
        mainPanel(
          tabsetPanel(
            id = "tabs",
            tabPanel("Home",
                     br(),
                     div(style="padding:20px; color:#2c3e50;",
                         h3("Welcome to the Lab Shadow",style = "font-weight: bold; color: #6a0dad;"),
                         h2("Kovalchuk's Lab"),
                         p("Perform differential gene expression analysis from GEO datasets stratified by gender."),
                         p("Explore expression data, phenotype data, volcano plots, and heatmaps."),
                         tags$hr(),
                         p(
                           span("Differential gene expression analysis", 
                                style = "font-weight: bold; color: #6a0dad; background-color: #f3e8ff; padding: 2px 5px; border-radius: 4px;"),
                           " is a powerful technique used to identify genes whose expression levels differ significantly between different biological conditions or groups, such as healthy vs diseased tissues or males vs females. This analysis helps uncover the molecular mechanisms underlying biological processes and diseases, enabling researchers to identify potential ",
                           span("biomarkers", style = "font-weight: bold; color: #6a0dad;"),
                           " and therapeutic targets. By comparing gene expression patterns, scientists gain insights into how genes contribute to health and disease, facilitating the development of personalized medicine and advancing our understanding of complex biological systems."
                         ),
                         
                         h4("What you can do with this app:"),
                         tags$ul(
                           tags$li("Load GEO datasets by accession number."),
                           tags$li("Visualize gene expression by gender groups."),
                           tags$li("Identify Upregulated and Downregulated genes."),
                           tags$li("Generate interactive volcano plots and heatmaps."),
                           tags$li("Download results for further analysis.")
                         ),
                         
                         tags$hr(),
                         
                         
                         tags$footer("© 2025 Kovalchuk's Lab")
                     )),
            tabPanel("Expression Data", DTOutput("expr_table")),
            tabPanel("Phenotype Data", DTOutput("pheno_table")),
            tabPanel("DEG Female",
                     verbatimTextOutput("deg_summary_female"),
                     plotOutput("volcano_female", height="400px"),
                     br(),
                     h4("Download Female Genes"),
                     fluidRow(
                       column(6, downloadButton("download_up_female","Download Upregulated")),
                       column(6, downloadButton("download_down_female","Download Downregulated"))
                     ),
                     br(),
                     DTOutput("deg_table_female")),
            tabPanel("DEG Male",
                     verbatimTextOutput("deg_summary_male"),
                     plotOutput("volcano_male", height="400px"),
                     br(),
                     h4("Download Male Genes"),
                     fluidRow(
                       column(6, downloadButton("download_up_male","Download Upregulated")),
                       column(6, downloadButton("download_down_male","Download Downregulated"))
                     ),
                     br(),
                     DTOutput("deg_table_male")),
            tabPanel("Female Heatmap", plotOutput("heatmap_female", height="600px")),
            tabPanel("Male Heatmap", plotOutput("heatmap_male", height="600px")),
            tabPanel("Summary Stats", verbatimTextOutput("summary_counts")),
            tabPanel("Sample Info", verbatimTextOutput("sample_info"))
          ),
          width = 9
        )
      )
      
        )
      )
  

library(shiny)

server <- function(input, output, session) {
  gse_data <- reactiveVal(NULL)
  data_obj <- reactiveValues()
  
  observe({
    tryCatch({
      file_id <- "1Jxd5Cv1QkqKcfBDXChRhg4h2RQDXuW8K"  # your file ID
      temp_file <- tempfile(fileext = ".rds")
      
      # Deauthorize to access public file without authentication
      drive_deauth()
      
      drive_download(as_id(file_id), path = temp_file, overwrite = TRUE)
      
      gse <- readRDS(temp_file)
      gse_data(gse)
      
      showNotification("✅ GSE data loaded successfully")
      
    }, error = function(e) {
      showNotification(paste("❌ Failed to load data:", e$message), type = "error")
    })
  })



  observeEvent(input$run, {
    req(gse_data())
    showModal(modalDialog("Processing data...", footer = NULL))

    future_promise({
      gse <- gse_data()[[1]]

      exprs_data <- exprs(gse)
      pheno_data <- pData(gse)
      feature_data <- fData(gse)

      # Preprocessing with minimal copies
      exprs_data <- exprs_data[rowSums(is.na(exprs_data)) == 0, , drop = FALSE]
      exprs_data <- log2(exprs_data + 1)

      # Keep only top N most variable genes to reduce memory
      gene_var <- matrixStats::rowVars(exprs_data)
      keep_idx <- order(gene_var, decreasing = TRUE)
      top_n <- min(length(keep_idx), 5000L)
      exprs_data <- exprs_data[keep_idx[seq_len(top_n)], , drop = FALSE]

      # Lightweight annotation mapping only
      gene_symbol <- feature_data[rownames(exprs_data), "Gene symbol"]
      gene_symbol <- sapply(strsplit(as.character(gene_symbol), "///", fixed = TRUE), `[`, 1)
      annotation_df <- data.frame(
        probe_id = rownames(exprs_data),
        gene_symbol = gene_symbol,
        stringsAsFactors = FALSE
      )

      run_deg <- function(sex_label) {
        samples <- pheno_data$`gender:ch1` == sex_label
        exprs_matrix <- exprs_data[, samples, drop = FALSE]
        pheno_sub <- pheno_data[samples, ]

        group <- factor(pheno_sub$`disease state:ch1`)
        design <- model.matrix(~0 + group)
        colnames(design) <- make.names(colnames(design))
        contrast.matrix <- makeContrasts(RA_vs_Control = groupRA - grouphealthy.control, levels = design)

        fit <- lmFit(exprs_matrix, design)
        fit2 <- contrasts.fit(fit, contrast.matrix)
        fit2 <- eBayes(fit2)
        results <- topTable(fit2, adjust.method = "fdr", number = Inf)
        results$probe_id <- rownames(results)

        results_annot <- merge(results, annotation_df, by = "probe_id", all.x = TRUE)
        results_annot$Regulation <- ifelse(
          results_annot$adj.P.Val < input$pval_thresh & results_annot$logFC > input$logfc_thresh, "Upregulated",
          ifelse(results_annot$adj.P.Val < input$pval_thresh & results_annot$logFC < -input$logfc_thresh, "Downregulated", "Not Significant")
        )

        # Limit heatmap matrix to top 100 variable genes for this subset
        subset_var <- matrixStats::rowVars(exprs_matrix)
        top_heat_idx <- head(order(subset_var, decreasing = TRUE), 100)
        heat_mat <- exprs_matrix[top_heat_idx, , drop = FALSE]

        list(results = results_annot, heat = heat_mat)
      }

      # Process one sex at a time to lower peak RAM
      res_female <- run_deg("F")
      gc()
      res_male <- run_deg("M")
      gc()

      list(
        expr = exprs_data,
        pheno = pheno_data,
        deg_female = res_female$results,
        deg_male = res_male$results,
        heat_female = res_female$heat,
        heat_male = res_male$heat
      )
    }) %...>% (
      function(res) {
        data_obj$expr <- res$expr
        data_obj$pheno <- res$pheno
        data_obj$deg_female <- res$deg_female
        data_obj$deg_male <- res$deg_male
        data_obj$heat_female <- res$heat_female
        data_obj$heat_male <- res$heat_male

        data_obj$up_female <- subset(data_obj$deg_female, adj.P.Val < input$pval_thresh & logFC > input$logfc_thresh)
        data_obj$down_female <- subset(data_obj$deg_female, adj.P.Val < input$pval_thresh & logFC < -input$logfc_thresh)
        data_obj$up_male <- subset(data_obj$deg_male, adj.P.Val < input$pval_thresh & logFC > input$logfc_thresh)
        data_obj$down_male <- subset(data_obj$deg_male, adj.P.Val < input$pval_thresh & logFC < -input$logfc_thresh)

        removeModal()
        showNotification("🎉 DEG analysis complete!", type = "message")
        runjs("celebrate();")
      }
    ) %...!% (
      function(e) {
        removeModal()
        showNotification(paste0("❌ Analysis failed: ", e$message), type = "error")
      }
    )
  })

  # Remaining outputs (no change needed)
  output$expr_table <- renderDT({
    req(data_obj$expr)
    expr_preview <- data_obj$expr[seq_len(min(1000L, nrow(data_obj$expr))), , drop = FALSE]
    datatable(as.data.frame(expr_preview), options = list(scrollX = TRUE))
  })
  output$pheno_table <- renderDT({ req(data_obj$pheno); datatable(data_obj$pheno, options = list(scrollX = TRUE)) })
  output$deg_table_female <- renderDT({ req(data_obj$deg_female); datatable(data_obj$deg_female, options = list(scrollX = TRUE)) })
  output$deg_table_male <- renderDT({ req(data_obj$deg_male); datatable(data_obj$deg_male, options = list(scrollX = TRUE)) })
  output$deg_summary_female <- renderPrint({ req(data_obj$deg_female); print(table(data_obj$deg_female$Regulation)) })
  output$deg_summary_male <- renderPrint({ req(data_obj$deg_male); print(table(data_obj$deg_male$Regulation)) })

  output$volcano_female <- renderPlot({
    req(data_obj$deg_female)
    ggplot(data_obj$deg_female, aes(x = logFC, y = -log10(adj.P.Val), color = Regulation)) +
      geom_point(alpha = 0.5) +
      scale_color_manual(values = c("Upregulated" = "#e74c3c", "Downregulated" = "#3498db", "Not Significant" = "grey70")) +
      theme_minimal() +
      labs(title = "Volcano Plot - Female", x = "log2 Fold Change", y = "-log10 Adjusted P-value") +
      theme(plot.title = element_text(face = "bold", size = 16))
  })

  output$volcano_male <- renderPlot({
    req(data_obj$deg_male)
    ggplot(data_obj$deg_male, aes(x = logFC, y = -log10(adj.P.Val), color = Regulation)) +
      geom_point(alpha = 0.5) +
      scale_color_manual(values = c("Upregulated" = "#e74c3c", "Downregulated" = "#3498db", "Not Significant" = "grey70")) +
      theme_minimal() +
      labs(title = "Volcano Plot - Male", x = "log2 Fold Change", y = "-log10 Adjusted P-value") +
      theme(plot.title = element_text(face = "bold", size = 16))
  })

  output$heatmap_female <- renderPlot({
    req(data_obj$heat_female)
    tryCatch({
      mat <- as.matrix(data_obj$heat_female)
      mat <- mat[rowSums(is.na(mat)) == 0, ]
      mat <- mat[1:min(50, nrow(mat)), ]
      pheatmap(mat, scale = "row", main = "Female Heatmap")
    }, error = function(e) {
      showNotification("Error generating Female Heatmap.", type = "error")
    })
  })

  output$heatmap_male <- renderPlot({
    req(data_obj$heat_male)
    tryCatch({
      mat <- as.matrix(data_obj$heat_male)
      mat <- mat[rowSums(is.na(mat)) == 0, ]
      mat <- mat[1:min(50, nrow(mat)), ]
      pheatmap(mat, scale = "row", main = "Male Heatmap")
    }, error = function(e) {
      showNotification("Error generating Male Heatmap.", type = "error")
    })
  })

  # Downloads
  output$download_up_female <- downloadHandler(
    filename = function() { "upregulated_female.csv" },
    content = function(file) { write.csv(data_obj$up_female, file, row.names = FALSE) }
  )
  output$download_down_female <- downloadHandler(
    filename = function() { "downregulated_female.csv" },
    content = function(file) { write.csv(data_obj$down_female, file, row.names = FALSE) }
  )
  output$download_up_male <- downloadHandler(
    filename = function() { "upregulated_male.csv" },
    content = function(file) { write.csv(data_obj$up_male, file, row.names = FALSE) }
  )
  output$download_down_male <- downloadHandler(
    filename = function() { "downregulated_male.csv" },
    content = function(file) { write.csv(data_obj$down_male, file, row.names = FALSE) }
  )

  output$summary_counts <- renderPrint({
    req(data_obj$deg_female)
    cat("Disease State Counts:\n")
    print(table(data_obj$pheno$`disease state:ch1`))
    cat("\nGender Counts:\n")
    print(table(data_obj$pheno$gender))
  })

  output$sample_info <- renderPrint({
    req(data_obj$pheno)
    cat(capture.output(str(data_obj$pheno)), sep = "\n")
  })
}


shinyApp(ui, server)
