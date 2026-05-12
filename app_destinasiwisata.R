# =============================================================================
#   DASHBOARD ANALITIK: Segmentasi Destinasi Wisata di 5 Kota Besar Indonesia
#   Menggunakan Algoritma K-Means Clustering + Hierarchical Clustering
#
#   Dataset:
#     - tourism_with_id.csv  : Data destinasi wisata (nama, kota, kategori, harga, rating)
#     - tourism_rating.csv   : Rating yang diberikan user ke destinasi
#     - user.csv             : Data demografi pengguna
#     - package_tourism.csv  : Paket wisata per kota
#
#   Paket yang dibutuhkan (install sekali saja):
#   install.packages(c("shiny","shinydashboard","DT","plotly","dplyr",
#                      "ggplot2","factoextra","cluster","RColorBrewer",
#                      "shinycssloaders","tidyr","dendextend"))
# =============================================================================

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)
library(factoextra)
library(cluster)
library(RColorBrewer)
library(shinycssloaders)
library(tidyr)
library(dendextend)   # untuk hierarchical clustering

# =============================================================================
# 1. LOAD & PERSIAPAN DATA
# =============================================================================

# -- Ganti path di bawah sesuai lokasi file CSV di komputer kamu --
tourism  <- read.csv("tourism_with_id.csv")
ratings  <- read.csv("tourism_rating.csv")
users    <- read.csv("user.csv")
packages <- read.csv("package_tourism.csv")

# Bersihkan kolom tidak perlu dari tourism
tourism <- tourism %>%
  select(Place_Id, Place_Name, Category, City, Price, Rating, Lat, Long) %>%
  filter(!is.na(Rating), !is.na(Price))

# Hitung rata-rata rating per destinasi dari data pengguna
avg_rating_user <- ratings %>%
  group_by(Place_Id) %>%
  summarise(
    Avg_User_Rating = round(mean(Place_Ratings, na.rm = TRUE), 2),
    Jumlah_Review   = n()
  )

# Gabungkan tourism dengan rata-rata rating pengguna
tourism <- tourism %>%
  left_join(avg_rating_user, by = "Place_Id") %>%
  mutate(
    Avg_User_Rating = ifelse(is.na(Avg_User_Rating), Rating, Avg_User_Rating),
    Jumlah_Review   = ifelse(is.na(Jumlah_Review), 0, Jumlah_Review)
  )

# Label kategori harga
tourism <- tourism %>%
  mutate(
    Kategori_Harga = case_when(
      Price == 0       ~ "Gratis",
      Price <= 20000   ~ "Murah (<=20rb)",
      Price <= 100000  ~ "Sedang (20-100rb)",
      TRUE             ~ "Mahal (>100rb)"
    )
  )

# Palet warna muted multicolor
warna_kota <- c(
  "Jakarta"    = "#5B8DB8",
  "Yogyakarta" = "#E07B54",
  "Bandung"    = "#6BAE75",
  "Semarang"   = "#C9A84C",
  "Surabaya"   = "#9B6BB5"
)

warna_cluster <- c(
  "#5B8DB8", "#E07B54", "#6BAE75",
  "#C9A84C", "#9B6BB5", "#B05070"
)

warna_kategori <- c(
  "#5B8DB8", "#E07B54", "#6BAE75",
  "#C9A84C", "#9B6BB5", "#B05070"
)


# =============================================================================
# 2. FUNGSI K-MEANS CLUSTERING
# =============================================================================

jalankan_kmeans <- function(data, k, fitur) {
  df_fitur <- data %>%
    select(all_of(fitur)) %>%
    na.omit() %>%
    scale()
  
  baris_valid <- as.integer(rownames(df_fitur))
  
  set.seed(42)
  km <- kmeans(df_fitur, centers = k, nstart = 25, iter.max = 100)
  
  data_hasil <- data[baris_valid, ] %>%
    mutate(Cluster = factor(paste("Cluster", km$cluster)))
  
  list(model = km, data_hasil = data_hasil, df_scaled = df_fitur)
}


# =============================================================================
# 2b. FUNGSI HIERARCHICAL CLUSTERING
# =============================================================================

jalankan_hclust <- function(data, k, fitur, metode_linkage, maks_obs = 150) {
  df_fitur <- data %>%
    select(all_of(fitur)) %>%
    na.omit() %>%
    scale()
  
  baris_valid <- as.integer(rownames(df_fitur))
  
  # Batasi observasi agar dendrogram tidak terlalu padat
  if (nrow(df_fitur) > maks_obs) {
    set.seed(42)
    idx_sampel <- sort(sample(nrow(df_fitur), maks_obs))
    df_plot    <- df_fitur[idx_sampel, ]
    baris_plot <- baris_valid[idx_sampel]
  } else {
    df_plot    <- df_fitur
    baris_plot <- baris_valid
  }
  
  # Hitung jarak & lakukan hierarchical clustering (untuk dendrogram)
  dist_mat  <- dist(df_plot, method = "euclidean")
  hc_model  <- hclust(dist_mat, method = metode_linkage)
  
  # Potong dendrogram -> label klaster untuk sampel
  klaster_sampel <- cutree(hc_model, k = k)
  
  # Untuk semua data: potong berdasarkan full distance matrix
  dist_full  <- dist(df_fitur, method = "euclidean")
  hc_full    <- hclust(dist_full, method = metode_linkage)
  klaster_semua <- cutree(hc_full, k = k)
  
  data_hasil <- data[baris_valid, ] %>%
    mutate(Cluster = factor(paste("Cluster", klaster_semua)))
  
  list(
    hc_model   = hc_model,
    hc_full    = hc_full,
    data_hasil = data_hasil,
    df_scaled  = df_fitur,
    df_plot    = df_plot,
    klaster_sampel = klaster_sampel,
    baris_plot = baris_plot,
    k          = k
  )
}


# =============================================================================
# 2c. FUNGSI HELPER INTERPRETASI CLUSTER
# =============================================================================

buat_interpretasi <- function(df_stat) {
  harga_max  <- max(df_stat$avg_harga)
  rating_max <- max(df_stat$avg_rating)
  review_max <- max(df_stat$avg_review)
  
  lapply(seq_len(nrow(df_stat)), function(i) {
    row <- df_stat[i, ]
    
    label <- dplyr::case_when(
      row$avg_harga == harga_max                             ~ "Destinasi Premium",
      row$avg_rating == rating_max & row$avg_harga < 50000  ~ "Hidden Gem Berkualitas",
      row$avg_review == review_max                          ~ "Destinasi Populer",
      row$avg_harga  < 15000                                ~ "Wisata Ekonomis",
      row$avg_rating >= 4.4                                 ~ "Destinasi Unggulan",
      TRUE                                                  ~ "Destinasi Umum"
    )
    
    narasi <- paste0(
      "Berisi <b>", row$n, " destinasi</b> dengan rata-rata harga ",
      "<b>Rp ", format(round(row$avg_harga), big.mark = "."), "</b>, ",
      "rating resmi <b>", round(row$avg_rating, 2), "</b>, ",
      "dan rating pengguna <b>", round(row$avg_rating_user, 2), "</b>. ",
      "Didominasi kota <b>", row$kota_dominan, "</b> ",
      "dengan kategori terbanyak <b>", row$kat_dominan, "</b>."
    )
    
    rekomendasi <- dplyr::case_when(
      row$avg_harga == harga_max ~
        "Rekomendasi: Cocok untuk paket wisata premium & wisatawan mancanegara. Tingkatkan fasilitas dan layanan eksklusif.",
      row$avg_review == review_max ~
        "Rekomendasi: Destinasi ini sudah populer - fokus pada manajemen keramaian dan peningkatan kapasitas.",
      row$avg_rating == rating_max & row$avg_harga < 50000 ~
        "Rekomendasi: Promosikan lebih aktif di media sosial dan platform wisata digital karena kualitas tinggi dengan harga terjangkau.",
      row$avg_harga < 15000 ~
        "Rekomendasi: Optimalkan aksesibilitas dan fasilitas dasar untuk menarik wisatawan lokal & pelajar.",
      TRUE ~
        "Rekomendasi: Pertimbangkan paket bundling dengan destinasi lain di kota yang sama untuk meningkatkan daya tarik."
    )
    
    list(
      cluster          = as.character(row$cluster),
      label            = label,
      narasi           = narasi,
      rekomendasi      = rekomendasi,
      avg_harga        = row$avg_harga,
      avg_rating       = row$avg_rating,
      avg_rating_user  = row$avg_rating_user,
      n                = row$n
    )
  })
}

warna_badge <- function(label) {
  dplyr::case_when(
    grepl("Premium",  label) ~ "#9B6BB5",
    grepl("Hidden",   label) ~ "#E07B54",
    grepl("Populer",  label) ~ "#C9A84C",
    grepl("Ekonomis", label) ~ "#6BAE75",
    grepl("Unggulan", label) ~ "#5B8DB8",
    TRUE                     ~ "#888888"
  )
}

render_interpretasi_ui <- function(hasil_list, warna_cluster_vec) {
  kartu_list <- lapply(seq_along(hasil_list), function(i) {
    item  <- hasil_list[[i]]
    warna <- warna_cluster_vec[i]
    badge_color <- warna_badge(item$label)
    
    tags$div(
      style = paste0(
        "border-left:5px solid ", warna, ";",
        "background:#fff; border-radius:8px;",
        "padding:14px 18px; margin-bottom:14px;",
        "box-shadow:0 2px 6px rgba(0,0,0,0.08);"
      ),
      
      # Header: nama cluster + label
      tags$div(
        style = "display:flex; align-items:center; gap:10px; margin-bottom:8px;",
        tags$span(
          style = paste0("background:", warna,
                         "; color:white; font-weight:bold;",
                         " padding:3px 14px; border-radius:20px; font-size:13px;"),
          item$cluster
        ),
        tags$span(
          style = paste0("background:", badge_color,
                         "; color:white; padding:3px 14px;",
                         " border-radius:20px; font-size:12px;"),
          item$label
        )
      ),
      
      # Metrik ringkas
      tags$div(
        style = "display:flex; gap:12px; margin-bottom:10px; flex-wrap:wrap;",
        tags$div(
          style = "background:#f5f7fa; border-radius:6px; padding:6px 14px; font-size:12px; color:#555;",
          tags$b("Harga"), tags$br(),
          paste0("Rp ", format(round(item$avg_harga), big.mark = "."))
        ),
        tags$div(
          style = "background:#f5f7fa; border-radius:6px; padding:6px 14px; font-size:12px; color:#555;",
          tags$b("Rating Resmi"), tags$br(),
          round(item$avg_rating, 2)
        ),
        tags$div(
          style = "background:#f5f7fa; border-radius:6px; padding:6px 14px; font-size:12px; color:#555;",
          tags$b("Rating User"), tags$br(),
          round(item$avg_rating_user, 2)
        ),
        tags$div(
          style = "background:#f5f7fa; border-radius:6px; padding:6px 14px; font-size:12px; color:#555;",
          tags$b("Jumlah"), tags$br(),
          paste0(item$n, " destinasi")
        )
      ),
      
      # Narasi
      tags$p(HTML(item$narasi),
             style = "font-size:13px; color:#444; margin-bottom:8px; line-height:1.6;"),
      
      # Rekomendasi
      tags$div(
        style = "background:#fffde7; border-radius:6px; padding:8px 12px; font-size:12px; color:#5D4037;",
        HTML(item$rekomendasi)
      )
    )
  })
  
  tagList(
    tags$p(
      style = "color:#1B5E20; font-weight:bold; font-size:14px; margin-bottom:12px;",
      "Karakteristik & Rekomendasi untuk Setiap Cluster:"
    ),
    kartu_list
  )
}


# =============================================================================
# 3. UI
# =============================================================================

ui <- dashboardPage(
  skin = "green",
  
  # --- Header ---
  dashboardHeader(
    title = tags$span(
      tags$img(src = "https://img.icons8.com/color/32/marker--v1.png",
               height = "26px", style = "margin-right:6px; vertical-align:middle;"),
      "Dashboard KLP 6"
    ),
    titleWidth = 230
  ),
  
  # --- Sidebar ---
  dashboardSidebar(
    width = 230,
    sidebarMenu(
      id = "menu",
      menuItem("Beranda",                tabName = "beranda",     icon = icon("home")),
      menuItem("Eksplorasi Data",        tabName = "eksplorasi",  icon = icon("chart-bar")),
      menuItem("K-Means Clustering",     tabName = "kmeans",      icon = icon("circle-nodes")),
      menuItem("Hierarchical Clustering",tabName = "hclust",      icon = icon("sitemap")),
      menuItem("Peta Destinasi",         tabName = "peta",        icon = icon("map")),
      menuItem("Tabel Data",             tabName = "tabel",       icon = icon("table")),
      
      hr(),
      
      # Filter Kota
      tags$div(
        style = "padding: 0 12px 8px;",
        tags$p("Filter Kota",
               style = "color:#A5D6A7; font-weight:bold; margin-bottom:4px; font-size:13px;"),
        selectInput(
          inputId   = "filter_kota",
          label     = NULL,
          choices   = c("Jakarta","Yogyakarta","Bandung","Semarang","Surabaya"),
          selected  = c("Jakarta","Yogyakarta","Bandung","Semarang","Surabaya"),
          multiple  = TRUE,
          selectize = FALSE,
          size      = 5
        )
      ),
      
      # Filter Kategori
      tags$div(
        style = "padding: 0 12px 8px;",
        tags$p("Filter Kategori",
               style = "color:#A5D6A7; font-weight:bold; margin-bottom:4px; font-size:13px;"),
        selectInput(
          inputId   = "filter_kategori",
          label     = NULL,
          choices   = c("Budaya","Taman Hiburan","Cagar Alam",
                        "Bahari","Pusat Perbelanjaan","Tempat Ibadah"),
          selected  = c("Budaya","Taman Hiburan","Cagar Alam",
                        "Bahari","Pusat Perbelanjaan","Tempat Ibadah"),
          multiple  = TRUE,
          selectize = FALSE,
          size      = 6
        )
      )
    )
  ),
  
  # --- Body ---
  dashboardBody(
    
    tags$head(tags$style(HTML("
      body, .content-wrapper, .right-side { background-color: #f5f7fa; font-family: 'Segoe UI', sans-serif; }

      /* Header & sidebar - HIJAU */
      .main-header .logo   { background-color: #1B5E20 !important; font-size:14px; }
      .main-header .navbar { background-color: #2E7D32 !important; }
      .main-sidebar        { background-color: #1B5E20 !important; }
      .sidebar-menu > li.active > a { background-color: #388E3C !important; border-left:4px solid #A5D6A7; }
      .sidebar-menu > li > a:hover  { background-color: #2E7D32 !important; }

      /* Dropdown select dalam sidebar */
      .main-sidebar select {
        width: 100%;
        background-color: #2E7D32;
        color: #fff;
        border: 1px solid #66BB6A;
        border-radius: 5px;
        padding: 5px 6px;
        font-size: 12px;
        cursor: pointer;
      }
      .main-sidebar select option { background-color: #1B5E20; color: #fff; }
      .main-sidebar select:focus  { outline: none; border-color: #A5D6A7; }
      .main-sidebar label         { color: #A5D6A7 !important; font-size:12px; }

      /* Box */
      .box { border-radius:10px; box-shadow:0 2px 8px rgba(0,0,0,0.09); border-top:none !important; }
      .box-header { background-color:#E8F5E9 !important; border-bottom:2px solid #A5D6A7; border-radius:10px 10px 0 0; }
      h3.box-title { color:#1B5E20; font-weight:bold; font-size:14px; }

      /* Value boxes */
      .small-box { border-radius:10px; }
      .small-box.bg-green { background-color:#2E7D32 !important; }

      /* Slider */
      .irs-bar, .irs-bar-edge, .irs-single { background:#2E7D32 !important; border-color:#2E7D32 !important; }

      /* Tabel cluster stat */
      #tabel_cluster_stat table { width:100% !important; table-layout:auto !important; }
      #tabel_cluster_stat .dataTables_wrapper { overflow-x:auto; }

      /* Tabel hclust stat */
      #tabel_hclust_stat table { width:100% !important; table-layout:auto !important; }
      #tabel_hclust_stat .dataTables_wrapper { overflow-x:auto; }
    "))),
    
    tabItems(
      
      # ==============================================================
      # TAB 1: BERANDA
      # ==============================================================
      tabItem(tabName = "beranda",
              fluidRow(
                column(12,
                       tags$div(
                         style = "background:linear-gradient(135deg,#1B5E20,#43A047);
                       color:white; padding:28px; border-radius:12px;
                       margin-bottom:18px; box-shadow:0 4px 15px rgba(0,0,0,0.18);",
                         tags$h2("Segmentasi Destinasi Wisata Indonesia",
                                 style = "font-weight:bold; margin:0 0 8px;"),
                         tags$p("Analisis klaster K-Means & Hierarchical untuk 5 kota besar: Jakarta, Yogyakarta, Bandung, Semarang, dan Surabaya.",
                                style = "font-size:15px; opacity:.9; margin:0;")
                       )
                )
              ),
              fluidRow(
                valueBoxOutput("vbox_destinasi", width = 3),
                valueBoxOutput("vbox_kota",      width = 3),
                valueBoxOutput("vbox_kategori",  width = 3),
                valueBoxOutput("vbox_user",      width = 3)
              ),
              fluidRow(
                box(title = "Distribusi Kategori Wisata", width = 6, status = "success", solidHeader = FALSE,
                    withSpinner(plotlyOutput("plot_kategori_bar", height = 320), color = "#2E7D32")),
                box(title = "Jumlah Destinasi per Kota",  width = 6, status = "success", solidHeader = FALSE,
                    withSpinner(plotlyOutput("plot_kota_bar",     height = 320), color = "#2E7D32"))
              ),
              fluidRow(
                box(title = "Rata-Rata Rating per Kota", width = 6, status = "success", solidHeader = FALSE,
                    withSpinner(plotlyOutput("plot_rating_kota",  height = 300), color = "#2E7D32")),
                box(title = "Distribusi Harga Tiket",    width = 6, status = "success", solidHeader = FALSE,
                    withSpinner(plotlyOutput("plot_harga_dist",   height = 300), color = "#2E7D32"))
              )
      ),
      
      # ==============================================================
      # TAB 2: EKSPLORASI DATA
      # ==============================================================
      tabItem(tabName = "eksplorasi",
              fluidRow(
                box(title = "Hubungan Harga vs Rating",    width = 6, status = "success", solidHeader = FALSE,
                    withSpinner(plotlyOutput("plot_scatter_harga_rating", height = 380), color = "#2E7D32")),
                box(title = "Boxplot Rating per Kategori", width = 6, status = "success", solidHeader = FALSE,
                    withSpinner(plotlyOutput("plot_box_rating_kat",       height = 380), color = "#2E7D32"))
              ),
              fluidRow(
                box(title = "Heatmap: Kategori x Kota", width = 6, status = "success", solidHeader = FALSE,
                    withSpinner(plotlyOutput("plot_heatmap",              height = 350), color = "#2E7D32")),
                box(title = "Demografi Usia Pengguna",   width = 6, status = "success", solidHeader = FALSE,
                    withSpinner(plotlyOutput("plot_usia",                 height = 350), color = "#2E7D32"))
              )
      ),
      
      # ==============================================================
      # TAB 3: K-MEANS CLUSTERING
      # ==============================================================
      tabItem(tabName = "kmeans",
              fluidRow(
                box(title = "Pengaturan K-Means", width = 3, status = "success", solidHeader = FALSE,
                    tags$p("Pilih jumlah klaster dan fitur clustering.", style = "color:#555; font-size:13px;"),
                    sliderInput("k_cluster", "Jumlah Klaster (K):", min = 2, max = 6, value = 3, step = 1),
                    checkboxGroupInput(
                      "fitur_cluster", "Fitur untuk Clustering:",
                      choices  = c("Price" = "Price", "Rating Resmi" = "Rating",
                                   "Rating Pengguna" = "Avg_User_Rating", "Jumlah Review" = "Jumlah_Review"),
                      selected = c("Price","Rating","Avg_User_Rating")
                    ),
                    hr(),
                    actionButton("btn_run_kmeans", "Jalankan K-Means",
                                 class = "btn-success btn-block", style = "font-weight:bold;"),
                    hr(),
                    tags$p("Gunakan grafik Elbow untuk menentukan K optimal.",
                           style = "color:#777; font-size:12px;")
                ),
                box(title = "Hasil Visualisasi Cluster (PCA 2D)", width = 9, status = "success", solidHeader = FALSE,
                    withSpinner(plotlyOutput("plot_cluster_pca", height = 420), color = "#2E7D32"))
              ),
              
              # Interpretasi K-Means
              fluidRow(
                box(
                  title = "Interpretasi Otomatis Hasil Clustering", width = 12,
                  status = "success", solidHeader = FALSE,
                  style = "background-color:#f0faf0;",
                  uiOutput("interpretasi_kmeans")
                )
              ),
              
              fluidRow(
                box(title = "Elbow Method", width = 5, status = "success", solidHeader = FALSE,
                    withSpinner(plotlyOutput("plot_elbow", height = 320), color = "#2E7D32"),
                    tags$p("Cari titik 'siku' - K di situ adalah yang optimal.",
                           style = "color:#777; font-size:12px; margin-top:8px;")
                ),
                box(
                  title = "Ringkasan Statistik per Cluster", width = 7,
                  status = "success", solidHeader = FALSE,
                  tags$div(
                    style = "overflow-x:auto; width:100%;",
                    withSpinner(DTOutput("tabel_cluster_stat"), color = "#2E7D32")
                  )
                )
              ),
              fluidRow(
                box(title = "Komposisi Kota dalam Setiap Cluster",     width = 6, status = "success", solidHeader = FALSE,
                    withSpinner(plotlyOutput("plot_cluster_kota",     height = 320), color = "#2E7D32")),
                box(title = "Komposisi Kategori dalam Setiap Cluster", width = 6, status = "success", solidHeader = FALSE,
                    withSpinner(plotlyOutput("plot_cluster_kategori", height = 320), color = "#2E7D32"))
              )
      ),
      
      # ==============================================================
      # TAB 4: HIERARCHICAL CLUSTERING
      # ==============================================================
      tabItem(tabName = "hclust",
              fluidRow(
                # Panel kontrol
                box(
                  title = "Pengaturan Hierarchical", width = 3,
                  status = "success", solidHeader = FALSE,
                  tags$p("Pilih jumlah klaster, fitur, dan metode linkage.",
                         style = "color:#555; font-size:13px;"),
                  
                  sliderInput("hc_k", "Jumlah Klaster (K):",
                              min = 2, max = 6, value = 3, step = 1),
                  
                  checkboxGroupInput(
                    "hc_fitur", "Fitur untuk Clustering:",
                    choices  = c("Price" = "Price", "Rating Resmi" = "Rating",
                                 "Rating Pengguna" = "Avg_User_Rating",
                                 "Jumlah Review"   = "Jumlah_Review"),
                    selected = c("Price","Rating","Avg_User_Rating")
                  ),
                  
                  selectInput(
                    "hc_linkage", "Metode Linkage:",
                    choices  = c("Ward D2"  = "ward.D2",
                                 "Complete" = "complete",
                                 "Average"  = "average",
                                 "Single"   = "single"),
                    selected = "ward.D2"
                  ),
                  
                  hr(),
                  actionButton("btn_run_hclust", "Jalankan Hierarchical",
                               class = "btn-success btn-block", style = "font-weight:bold;"),
                  hr(),
                  tags$p(
                    "Ward D2 umumnya menghasilkan klaster yang paling seimbang.",
                    style = "color:#777; font-size:12px;"
                  ),
                  tags$p(
                    "Dendrogram ditampilkan untuk maks. 150 observasi (subsampel acak).",
                    style = "color:#aaa; font-size:11px;"
                  )
                ),
                
                # Dendrogram
                box(
                  title = "Dendrogram Hierarchical Clustering", width = 9,
                  status = "success", solidHeader = FALSE,
                  withSpinner(plotOutput("plot_dendrogram", height = 430), color = "#2E7D32")
                )
              ),
              
              fluidRow(
                # Scatter PCA
                box(
                  title = "Sebaran Cluster pada Ruang PCA 2D", width = 7,
                  status = "success", solidHeader = FALSE,
                  withSpinner(plotlyOutput("plot_hclust_pca", height = 380), color = "#2E7D32")
                ),
                
                # Statistik ringkasan
                box(
                  title = "Ringkasan Statistik per Cluster", width = 5,
                  status = "success", solidHeader = FALSE,
                  tags$div(
                    style = "overflow-x:auto; width:100%;",
                    withSpinner(DTOutput("tabel_hclust_stat"), color = "#2E7D32")
                  )
                )
              ),
              
              # Interpretasi Hierarchical
              fluidRow(
                box(
                  title = "Interpretasi Otomatis Hasil Clustering", width = 12,
                  status = "success", solidHeader = FALSE,
                  style = "background-color:#f0faf0;",
                  uiOutput("interpretasi_hclust")
                )
              ),
              
              fluidRow(
                # Komposisi kota
                box(
                  title = "Komposisi Kota per Cluster", width = 6,
                  status = "success", solidHeader = FALSE,
                  withSpinner(plotlyOutput("plot_hclust_kota", height = 320), color = "#2E7D32")
                ),
                
                # Komposisi kategori
                box(
                  title = "Komposisi Kategori per Cluster", width = 6,
                  status = "success", solidHeader = FALSE,
                  withSpinner(plotlyOutput("plot_hclust_kategori", height = 320), color = "#2E7D32")
                )
              ),
              
              fluidRow(
                # Perbandingan rata-rata fitur antar cluster
                box(
                  title = "Rata-Rata Fitur per Cluster (Radar / Bar)", width = 12,
                  status = "success", solidHeader = FALSE,
                  withSpinner(plotlyOutput("plot_hclust_radar", height = 340), color = "#2E7D32")
                )
              )
      ),
      
      # ==============================================================
      # TAB 5: PETA
      # ==============================================================
      tabItem(tabName = "peta",
              fluidRow(
                box(title = "Peta Sebaran Destinasi Wisata", width = 12, status = "success", solidHeader = FALSE,
                    tags$p("Klik titik pada peta untuk detail destinasi.", style = "color:#555; font-size:13px;"),
                    withSpinner(plotlyOutput("plot_peta", height = 550), color = "#2E7D32")
                )
              )
      ),
      
      # ==============================================================
      # TAB 6: TABEL DATA
      # ==============================================================
      tabItem(tabName = "tabel",
              fluidRow(
                box(
                  title = "Data Lengkap", width = 12, status = "success", solidHeader = FALSE,
                  tabsetPanel(
                    type = "tabs",
                    
                    tabPanel("Destinasi Wisata",
                             br(),
                             downloadButton("dl_tourism", "Unduh CSV Destinasi", class = "btn-success"),
                             br(), br(),
                             tags$div(style = "overflow-x:auto;",
                                      withSpinner(DTOutput("tabel_tourism"), color = "#2E7D32"))
                    ),
                    
                    tabPanel("Rating Pengguna",
                             br(),
                             downloadButton("dl_ratings", "Unduh CSV Rating", class = "btn-success"),
                             br(), br(),
                             tags$div(style = "overflow-x:auto;",
                                      withSpinner(DTOutput("tabel_ratings"), color = "#2E7D32"))
                    ),
                    
                    tabPanel("Data Pengguna",
                             br(),
                             downloadButton("dl_users", "Unduh CSV Pengguna", class = "btn-success"),
                             br(), br(),
                             tags$div(style = "overflow-x:auto;",
                                      withSpinner(DTOutput("tabel_users"), color = "#2E7D32"))
                    ),
                    
                    tabPanel("Paket Wisata",
                             br(),
                             downloadButton("dl_packages", "Unduh CSV Paket", class = "btn-success"),
                             br(), br(),
                             tags$div(style = "overflow-x:auto;",
                                      withSpinner(DTOutput("tabel_packages"), color = "#2E7D32"))
                    )
                  )
                )
              )
      )
      
    ) # end tabItems
  )   # end dashboardBody
)     # end dashboardPage


# =============================================================================
# 4. SERVER
# =============================================================================

server <- function(input, output, session) {
  
  # Data reaktif
  data_filtered <- reactive({
    req(input$filter_kota, input$filter_kategori)
    tourism %>%
      filter(City %in% input$filter_kota, Category %in% input$filter_kategori)
  })
  
  # Hasil K-Means
  hasil_km <- eventReactive(input$btn_run_kmeans, {
    req(length(input$fitur_cluster) >= 2)
    jalankan_kmeans(data = data_filtered(), k = input$k_cluster, fitur = input$fitur_cluster)
  }, ignoreNULL = FALSE)
  
  # Hasil Hierarchical Clustering
  hasil_hc <- eventReactive(input$btn_run_hclust, {
    req(length(input$hc_fitur) >= 2)
    jalankan_hclust(
      data           = data_filtered(),
      k              = input$hc_k,
      fitur          = input$hc_fitur,
      metode_linkage = input$hc_linkage
    )
  }, ignoreNULL = FALSE)
  
  
  # ==========================================================================
  # BERANDA - Value Boxes
  # ==========================================================================
  
  output$vbox_destinasi <- renderValueBox({
    valueBox(nrow(data_filtered()), "Total Destinasi", icon = icon("map-pin"), color = "green")
  })
  output$vbox_kota <- renderValueBox({
    valueBox(length(input$filter_kota), "Kota Aktif", icon = icon("city"), color = "green")
  })
  output$vbox_kategori <- renderValueBox({
    valueBox(length(input$filter_kategori), "Kategori", icon = icon("tags"), color = "green")
  })
  output$vbox_user <- renderValueBox({
    valueBox(nrow(users), "Pengguna Terdaftar", icon = icon("users"), color = "green")
  })
  
  output$plot_kategori_bar <- renderPlotly({
    df <- data_filtered() %>% count(Category) %>% arrange(desc(n))
    plot_ly(df, x = ~reorder(Category, n), y = ~n, type = "bar",
            marker = list(color = warna_kategori[seq_len(nrow(df))])) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Jumlah Destinasi"),
             plot_bgcolor = "#f5f7fa", paper_bgcolor = "#f5f7fa", margin = list(b = 90))
  })
  
  output$plot_kota_bar <- renderPlotly({
    df <- data_filtered() %>% count(City) %>% arrange(desc(n))
    plot_ly(df, x = ~City, y = ~n, type = "bar",
            marker = list(color = warna_kota[df$City])) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Jumlah Destinasi"),
             plot_bgcolor = "#f5f7fa", paper_bgcolor = "#f5f7fa")
  })
  
  output$plot_rating_kota <- renderPlotly({
    df <- data_filtered() %>%
      group_by(City) %>% summarise(Avg_Rating = round(mean(Rating, na.rm = TRUE), 2))
    plot_ly(df, x = ~City, y = ~Avg_Rating, type = "bar",
            marker = list(color = warna_kota[df$City])) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Rating Rata-Rata", range = c(3.5, 5)),
             plot_bgcolor = "#f5f7fa", paper_bgcolor = "#f5f7fa")
  })
  
  output$plot_harga_dist <- renderPlotly({
    df    <- data_filtered() %>% count(Kategori_Harga)
    palet <- c("#5B8DB8","#E07B54","#6BAE75","#C9A84C")
    plot_ly(df, labels = ~Kategori_Harga, values = ~n, type = "pie",
            marker = list(colors = palet), textinfo = "label+percent") %>%
      layout(paper_bgcolor = "#f5f7fa", showlegend = TRUE,
             legend = list(orientation = "h", y = -0.15))
  })
  
  
  # ==========================================================================
  # EKSPLORASI DATA
  # ==========================================================================
  
  output$plot_scatter_harga_rating <- renderPlotly({
    df <- data_filtered()
    plot_ly(df, x = ~Price, y = ~Rating, color = ~City,
            colors = warna_kota[names(warna_kota) %in% df$City],
            type = "scatter", mode = "markers",
            marker = list(size = 8, opacity = 0.75),
            text = ~paste0("<b>", Place_Name, "</b><br>Kota: ", City,
                           "<br>Harga: Rp ", format(Price, big.mark = "."),
                           "<br>Rating: ", Rating),
            hoverinfo = "text") %>%
      layout(xaxis = list(title = "Harga Tiket (Rp)"), yaxis = list(title = "Rating"),
             plot_bgcolor = "#f5f7fa", paper_bgcolor = "#f5f7fa")
  })
  
  output$plot_box_rating_kat <- renderPlotly({
    df <- data_filtered()
    plot_ly(df, x = ~Category, y = ~Rating, type = "box",
            color = ~Category, colors = warna_kategori) %>%
      layout(xaxis = list(title = "", tickangle = -25), yaxis = list(title = "Rating"),
             showlegend = FALSE, plot_bgcolor = "#f5f7fa", paper_bgcolor = "#f5f7fa")
  })
  
  output$plot_heatmap <- renderPlotly({
    df <- data_filtered() %>%
      count(City, Category) %>%
      pivot_wider(names_from = Category, values_from = n, values_fill = 0)
    mat  <- as.matrix(df[, -1])
    kota <- df$City
    plot_ly(z = mat, x = colnames(mat), y = kota, type = "heatmap",
            colorscale = list(c(0, "#f1f8f1"), c(0.5, "#43A047"), c(1, "#1B5E20"))) %>%
      layout(xaxis = list(title = "", tickangle = -30), yaxis = list(title = ""),
             plot_bgcolor = "#f5f7fa", paper_bgcolor = "#f5f7fa")
  })
  
  output$plot_usia <- renderPlotly({
    df <- users %>%
      mutate(Kelompok_Usia = cut(Age, breaks = c(0,17,25,35,50,100),
                                 labels = c("<18","18-25","26-35","36-50",">50"))) %>%
      count(Kelompok_Usia)
    plot_ly(df, x = ~Kelompok_Usia, y = ~n, type = "bar",
            marker = list(color = warna_kategori[seq_len(nrow(df))])) %>%
      layout(xaxis = list(title = "Kelompok Usia"), yaxis = list(title = "Jumlah Pengguna"),
             plot_bgcolor = "#f5f7fa", paper_bgcolor = "#f5f7fa")
  })
  
  
  # ==========================================================================
  # K-MEANS CLUSTERING
  # ==========================================================================
  
  output$plot_elbow <- renderPlotly({
    req(length(input$fitur_cluster) >= 2)
    df_sc <- data_filtered() %>% select(all_of(input$fitur_cluster)) %>% na.omit() %>% scale()
    wss <- sapply(2:8, function(k) { set.seed(42); kmeans(df_sc, centers = k, nstart = 20)$tot.withinss })
    plot_ly(x = 2:8, y = wss, type = "scatter", mode = "lines+markers",
            line   = list(color = "#2E7D32", width = 2),
            marker = list(color = "#1B5E20", size = 8)) %>%
      layout(
        xaxis  = list(title = "Jumlah Klaster (K)", tickmode = "linear"),
        yaxis  = list(title = "Total Within-Cluster SS"),
        shapes = list(list(type = "line",
                           x0 = input$k_cluster, x1 = input$k_cluster,
                           y0 = 0, y1 = max(wss),
                           line = list(color = "#E07B54", dash = "dot", width = 2))),
        plot_bgcolor = "#f5f7fa", paper_bgcolor = "#f5f7fa"
      )
  })
  
  output$plot_cluster_pca <- renderPlotly({
    km_res <- hasil_km(); req(km_res)
    pca_res <- prcomp(km_res$df_scaled)
    df_pca  <- as.data.frame(pca_res$x[, 1:2])
    df_pca$Cluster    <- km_res$data_hasil$Cluster
    df_pca$Place_Name <- km_res$data_hasil$Place_Name
    df_pca$City       <- km_res$data_hasil$City
    df_pca$Rating     <- km_res$data_hasil$Rating
    df_pca$Price      <- km_res$data_hasil$Price
    pct_var <- round(100 * summary(pca_res)$importance[2, 1:2], 1)
    n_cl <- length(levels(df_pca$Cluster))
    plot_ly(df_pca, x = ~PC1, y = ~PC2, color = ~Cluster,
            colors = warna_cluster[seq_len(n_cl)],
            type = "scatter", mode = "markers",
            marker = list(size = 9, opacity = 0.8),
            text = ~paste0("<b>", Place_Name, "</b><br>Kota: ", City,
                           "<br>Klaster: ", Cluster,
                           "<br>Rating: ", Rating,
                           "<br>Harga: Rp ", format(Price, big.mark = ".")),
            hoverinfo = "text") %>%
      layout(xaxis  = list(title = paste0("PC1 (", pct_var[1], "% variansi)")),
             yaxis  = list(title = paste0("PC2 (", pct_var[2], "% variansi)")),
             legend = list(title = list(text = "<b>Klaster</b>")),
             plot_bgcolor = "#f5f7fa", paper_bgcolor = "#f5f7fa")
  })
  
  output$tabel_cluster_stat <- renderDT({
    km_res <- hasil_km(); req(km_res)
    df_stat <- km_res$data_hasil %>%
      group_by(Cluster) %>%
      summarise(
        N                 = n(),
        `Avg Harga`       = paste0("Rp ", format(round(mean(Price), 0), big.mark = ".")),
        `Rating`          = round(mean(Rating, na.rm = TRUE), 2),
        `Rating User`     = round(mean(Avg_User_Rating, na.rm = TRUE), 2),
        `Review`          = round(mean(Jumlah_Review, na.rm = TRUE), 1),
        `Kota Dominan`    = names(sort(table(City),     decreasing = TRUE))[1],
        `Kategori Dominan`= names(sort(table(Category), decreasing = TRUE))[1]
      )
    datatable(
      df_stat,
      options  = list(dom = "t", pageLength = 10, scrollX = TRUE,
                      autoWidth = FALSE,
                      columnDefs = list(list(width = "110px", targets = "_all"))),
      rownames = FALSE,
      class    = "stripe hover compact"
    ) %>%
      formatStyle("Cluster",
                  backgroundColor = styleEqual(
                    levels(km_res$data_hasil$Cluster),
                    colorRampPalette(c("#C8E6C9", "#1B5E20"))(input$k_cluster)
                  ))
  })
  
  output$plot_cluster_kota <- renderPlotly({
    km_res <- hasil_km(); req(km_res)
    df <- km_res$data_hasil %>% count(Cluster, City)
    plot_ly(df, x = ~Cluster, y = ~n, color = ~City,
            colors = warna_kota[names(warna_kota) %in% df$City],
            type = "bar") %>%
      layout(barmode = "stack", xaxis = list(title = "Klaster"), yaxis = list(title = "Jumlah Destinasi"),
             legend = list(title = list(text = "<b>Kota</b>")),
             plot_bgcolor = "#f5f7fa", paper_bgcolor = "#f5f7fa")
  })
  
  output$plot_cluster_kategori <- renderPlotly({
    km_res <- hasil_km(); req(km_res)
    df <- km_res$data_hasil %>% count(Cluster, Category)
    plot_ly(df, x = ~Cluster, y = ~n, color = ~Category,
            colors = warna_kategori, type = "bar") %>%
      layout(barmode = "stack", xaxis = list(title = "Klaster"), yaxis = list(title = "Jumlah Destinasi"),
             legend = list(title = list(text = "<b>Kategori</b>")),
             plot_bgcolor = "#f5f7fa", paper_bgcolor = "#f5f7fa")
  })
  
  # Output Interpretasi K-Means
  output$interpretasi_kmeans <- renderUI({
    km_res <- hasil_km(); req(km_res)
    
    df_stat <- km_res$data_hasil %>%
      group_by(cluster = Cluster) %>%
      summarise(
        n               = n(),
        avg_harga       = mean(Price, na.rm = TRUE),
        avg_rating      = mean(Rating, na.rm = TRUE),
        avg_rating_user = mean(Avg_User_Rating, na.rm = TRUE),
        avg_review      = mean(Jumlah_Review, na.rm = TRUE),
        kota_dominan    = names(sort(table(City),     decreasing = TRUE))[1],
        kat_dominan     = names(sort(table(Category), decreasing = TRUE))[1],
        .groups = "drop"
      )
    
    hasil <- buat_interpretasi(df_stat)
    n_cl  <- nrow(df_stat)
    render_interpretasi_ui(hasil, warna_cluster[seq_len(n_cl)])
  })
  
  
  # ==========================================================================
  # HIERARCHICAL CLUSTERING
  # ==========================================================================
  
  output$plot_dendrogram <- renderPlot({
    hc_res <- hasil_hc(); req(hc_res)
    k      <- hc_res$k
    dend   <- as.dendrogram(hc_res$hc_model)
    
    dend <- color_branches(dend, k = k,
                           col = warna_cluster[seq_len(k)])
    
    par(mar = c(4, 3, 3, 1), bg = "#f5f7fa")
    plot(dend,
         main  = paste0("Dendrogram - Linkage: ", toupper(input$hc_linkage),
                        "  |  K = ", k),
         xlab  = "Destinasi Wisata (subsampel)",
         ylab  = "Jarak (Euclidean)",
         leaflab = "none",
         cex.main = 1.1, col.main = "#1B5E20")
    
    h_cut <- mean(c(hc_res$hc_model$height[length(hc_res$hc_model$height) - k + 1],
                    hc_res$hc_model$height[length(hc_res$hc_model$height) - k + 2]))
    abline(h = h_cut, col = "#E07B54", lty = 2, lwd = 1.8)
    legend("topright",
           legend = paste("Cluster", seq_len(k)),
           fill   = warna_cluster[seq_len(k)],
           border = NA, bty = "n", cex = 0.85)
  }, bg = "#f5f7fa")
  
  output$plot_hclust_pca <- renderPlotly({
    hc_res <- hasil_hc(); req(hc_res)
    pca_res <- prcomp(hc_res$df_scaled)
    df_pca  <- as.data.frame(pca_res$x[, 1:2])
    df_pca$Cluster    <- hc_res$data_hasil$Cluster
    df_pca$Place_Name <- hc_res$data_hasil$Place_Name
    df_pca$City       <- hc_res$data_hasil$City
    df_pca$Rating     <- hc_res$data_hasil$Rating
    df_pca$Price      <- hc_res$data_hasil$Price
    pct_var <- round(100 * summary(pca_res)$importance[2, 1:2], 1)
    n_cl <- length(levels(df_pca$Cluster))
    plot_ly(df_pca, x = ~PC1, y = ~PC2, color = ~Cluster,
            colors = warna_cluster[seq_len(n_cl)],
            type = "scatter", mode = "markers",
            marker = list(size = 9, opacity = 0.8),
            text = ~paste0("<b>", Place_Name, "</b><br>Kota: ", City,
                           "<br>Klaster: ", Cluster,
                           "<br>Rating: ", Rating,
                           "<br>Harga: Rp ", format(Price, big.mark = ".")),
            hoverinfo = "text") %>%
      layout(xaxis  = list(title = paste0("PC1 (", pct_var[1], "% variansi)")),
             yaxis  = list(title = paste0("PC2 (", pct_var[2], "% variansi)")),
             legend = list(title = list(text = "<b>Klaster</b>")),
             plot_bgcolor = "#f5f7fa", paper_bgcolor = "#f5f7fa")
  })
  
  output$tabel_hclust_stat <- renderDT({
    hc_res <- hasil_hc(); req(hc_res)
    df_stat <- hc_res$data_hasil %>%
      group_by(Cluster) %>%
      summarise(
        N                 = n(),
        `Avg Harga`       = paste0("Rp ", format(round(mean(Price), 0), big.mark = ".")),
        `Rating`          = round(mean(Rating, na.rm = TRUE), 2),
        `Rating User`     = round(mean(Avg_User_Rating, na.rm = TRUE), 2),
        `Review`          = round(mean(Jumlah_Review, na.rm = TRUE), 1),
        `Kota Dominan`    = names(sort(table(City),     decreasing = TRUE))[1],
        `Kat. Dominan`    = names(sort(table(Category), decreasing = TRUE))[1]
      )
    datatable(
      df_stat,
      options  = list(dom = "t", pageLength = 10, scrollX = TRUE,
                      autoWidth = FALSE,
                      columnDefs = list(list(width = "100px", targets = "_all"))),
      rownames = FALSE,
      class    = "stripe hover compact"
    ) %>%
      formatStyle("Cluster",
                  backgroundColor = styleEqual(
                    levels(hc_res$data_hasil$Cluster),
                    colorRampPalette(c("#C8E6C9", "#1B5E20"))(input$hc_k)
                  ))
  })
  
  output$plot_hclust_kota <- renderPlotly({
    hc_res <- hasil_hc(); req(hc_res)
    df <- hc_res$data_hasil %>% count(Cluster, City)
    plot_ly(df, x = ~Cluster, y = ~n, color = ~City,
            colors = warna_kota[names(warna_kota) %in% df$City],
            type = "bar") %>%
      layout(barmode = "stack",
             xaxis = list(title = "Klaster"), yaxis = list(title = "Jumlah Destinasi"),
             legend = list(title = list(text = "<b>Kota</b>")),
             plot_bgcolor = "#f5f7fa", paper_bgcolor = "#f5f7fa")
  })
  
  output$plot_hclust_kategori <- renderPlotly({
    hc_res <- hasil_hc(); req(hc_res)
    df <- hc_res$data_hasil %>% count(Cluster, Category)
    plot_ly(df, x = ~Cluster, y = ~n, color = ~Category,
            colors = warna_kategori, type = "bar") %>%
      layout(barmode = "stack",
             xaxis = list(title = "Klaster"), yaxis = list(title = "Jumlah Destinasi"),
             legend = list(title = list(text = "<b>Kategori</b>")),
             plot_bgcolor = "#f5f7fa", paper_bgcolor = "#f5f7fa")
  })
  
  output$plot_hclust_radar <- renderPlotly({
    hc_res <- hasil_hc(); req(hc_res)
    fitur_num <- intersect(input$hc_fitur,
                           c("Price","Rating","Avg_User_Rating","Jumlah_Review"))
    req(length(fitur_num) >= 1)
    
    df_mean <- hc_res$data_hasil %>%
      group_by(Cluster) %>%
      summarise(across(all_of(fitur_num), ~ round(mean(.x, na.rm = TRUE), 2)),
                .groups = "drop") %>%
      pivot_longer(-Cluster, names_to = "Fitur", values_to = "Nilai")
    
    label_map <- c(Price = "Harga (Rp)", Rating = "Rating Resmi",
                   Avg_User_Rating = "Rating Pengguna", Jumlah_Review = "Jml Review")
    df_mean$Fitur <- dplyr::recode(df_mean$Fitur, !!!label_map)
    
    n_cl <- length(unique(df_mean$Cluster))
    plot_ly(df_mean, x = ~Fitur, y = ~Nilai, color = ~Cluster,
            colors = warna_cluster[seq_len(n_cl)],
            type = "bar", barmode = "group") %>%
      layout(xaxis  = list(title = ""),
             yaxis  = list(title = "Rata-Rata Nilai"),
             legend = list(title = list(text = "<b>Klaster</b>")),
             plot_bgcolor = "#f5f7fa", paper_bgcolor = "#f5f7fa")
  })
  
  # Output Interpretasi Hierarchical
  output$interpretasi_hclust <- renderUI({
    hc_res <- hasil_hc(); req(hc_res)
    
    df_stat <- hc_res$data_hasil %>%
      group_by(cluster = Cluster) %>%
      summarise(
        n               = n(),
        avg_harga       = mean(Price, na.rm = TRUE),
        avg_rating      = mean(Rating, na.rm = TRUE),
        avg_rating_user = mean(Avg_User_Rating, na.rm = TRUE),
        avg_review      = mean(Jumlah_Review, na.rm = TRUE),
        kota_dominan    = names(sort(table(City),     decreasing = TRUE))[1],
        kat_dominan     = names(sort(table(Category), decreasing = TRUE))[1],
        .groups = "drop"
      )
    
    hasil <- buat_interpretasi(df_stat)
    n_cl  <- nrow(df_stat)
    render_interpretasi_ui(hasil, warna_cluster[seq_len(n_cl)])
  })
  
  
  # ==========================================================================
  # PETA
  # ==========================================================================
  
  output$plot_peta <- renderPlotly({
    df <- data_filtered() %>% filter(!is.na(Lat), !is.na(Long))
    plot_ly(df, lat = ~Lat, lon = ~Long, type = "scattermapbox", mode = "markers",
            color = ~City, colors = warna_kota[names(warna_kota) %in% df$City],
            marker = list(size = 9, opacity = 0.8),
            text = ~paste0("<b>", Place_Name, "</b><br>Kota: ", City,
                           "<br>Kategori: ", Category,
                           "<br>Rating: ", Rating,
                           "<br>Harga: Rp ", format(Price, big.mark = ".")),
            hoverinfo = "text") %>%
      layout(mapbox = list(style = "open-street-map", zoom = 5,
                           center = list(lat = -7.5, lon = 110.5)),
             margin = list(t = 0, b = 0, l = 0, r = 0))
  })
  
  
  # ==========================================================================
  # TABEL DATA
  # ==========================================================================
  
  output$tabel_tourism <- renderDT({
    df <- data_filtered() %>%
      select(Place_Id, Place_Name, City, Category, Price,
             Rating, Avg_User_Rating, Jumlah_Review, Kategori_Harga)
    datatable(df, filter = "top", rownames = FALSE,
              colnames = c("ID","Nama Destinasi","Kota","Kategori","Harga (Rp)",
                           "Rating Resmi","Rating Pengguna","Jml Review","Kat. Harga"),
              options  = list(pageLength = 15, scrollX = TRUE, dom = "ltipr"),
              class    = "stripe hover compact") %>%
      formatCurrency("Price", currency = "Rp ", digits = 0, mark = ".")
  })
  
  output$tabel_ratings <- renderDT({
    datatable(ratings, filter = "top", rownames = FALSE,
              options = list(pageLength = 15, scrollX = TRUE, dom = "ltipr"),
              class   = "stripe hover compact")
  })
  
  output$tabel_users <- renderDT({
    datatable(users, filter = "top", rownames = FALSE,
              options = list(pageLength = 15, scrollX = TRUE, dom = "ltipr"),
              class   = "stripe hover compact")
  })
  
  output$tabel_packages <- renderDT({
    datatable(packages, filter = "top", rownames = FALSE,
              options = list(pageLength = 15, scrollX = TRUE, dom = "ltipr"),
              class   = "stripe hover compact")
  })
  
  # Download handlers
  output$dl_tourism <- downloadHandler(
    filename = function() paste0("destinasi_wisata_", Sys.Date(), ".csv"),
    content  = function(f) write.csv(data_filtered(), f, row.names = FALSE)
  )
  output$dl_ratings <- downloadHandler(
    filename = function() paste0("rating_pengguna_", Sys.Date(), ".csv"),
    content  = function(f) write.csv(ratings, f, row.names = FALSE)
  )
  output$dl_users <- downloadHandler(
    filename = function() paste0("data_pengguna_", Sys.Date(), ".csv"),
    content  = function(f) write.csv(users, f, row.names = FALSE)
  )
  output$dl_packages <- downloadHandler(
    filename = function() paste0("paket_wisata_", Sys.Date(), ".csv"),
    content  = function(f) write.csv(packages, f, row.names = FALSE)
  )
  
} # end server


# =============================================================================
# 5. JALANKAN APLIKASI
# =============================================================================

shinyApp(ui = ui, server = server)