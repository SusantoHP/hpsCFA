library(shiny)
library(readxl)
library(psych)
library(corrplot)
library(lavaan)
library(semPlot)
library(writexl)


ui<- fluidPage(
  h2(" VALIDITAS KONSTRUK menggunakan ANALISIS FAKTOR KONFIRMATORI",style="font-family: 'cursive';color: blue;text-align:center"),br(),
  column(width = 12,
         sidebarPanel("INPUT",width = 3,
                      fileInput("ambildatahps", "Choose  File",  accept = c( "text/csv",   "text/comma-separated-values,text/plain","csv","xlsx",".xls")),
                      radioButtons("pemisahvariabel", "Separator",choices = c(Comma = ",",Semicolon = ";",  Tab = "\t"),selected = ",", inline = TRUE),
                      radioButtons("ekstensi","File Extention", choices = c( "csv","xlsx"),selected = "xlsx", inline = TRUE),
                      hr(),hr(),
                      h3("by Hari Purnomo Susanto"),hr(),hr()

         ),

         mainPanel(width = 8,
                   navbarPage("",
                              tabPanel("Data Kamu",
                                       DT::DTOutput("inputdatahps")
                              ),
                              tabPanel("Uji Asumsi",
                                       column(width = 12,
                                              fluidRow(
                                                column(width = 4,h3("UJI ASUMSI KMO",style="font-family: 'cursive';color: blue;text-align:center"),br(),br(),
                                                       tableOutput("kmotest"),
                                                       downloadButton("downloadkmotes","unduh"),br(),br(),
                                                       tableOutput( "kmobutir"),
                                                       downloadButton("downloadkmobutir","unduh")
                                                ),
                                                column(width = 8,h3("UJI ASUM MULTIKOLINIERITAS",style="font-family: 'cursive';color: blue;text-align:center"),br(),br(),
                                                       plotOutput( "multikol",width = "100%")
                                                )))),
                              tabPanel("Masukan Konstruk",
                                       column(width = 12,
                                              fluidRow(
                                                column(width = 12,
                                                       h2("Analisis CFA "),hr(),
                                                       textInput("aspek1","Tuliskan model",width = "100%"),hr(),
                                                       h4("Contoh menuliskan Model:",style="font-family: 'cursive';color: blue;text-align:blue"),
                                                       h5("jika kontruk instrumen anda ada tiga faktor yaitu Y1, Y2, dan Y3, maka dapat ditulisakan sebagai berikut:"),
                                                       htmlOutput("keter"),br(),
                                                       h4("Catatan: Setiap Faktor dipisahkan dengan tanda titik koma (;)",style="font-family: 'cursive';color: red;text-align:left"),hr(),
                                                       numericInput("observasi",label = "Banyak Sampel:",value = 100,min = 40,max = 10000,step = 1),br(),
                                                       actionButton("analisis","Analisis",width = "30%",icon =icon("book"))

                                                )))),
                              tabPanel("Gambar Konstruk",
                                       plotOutput("gambarcfa"),br(),
                                       h4("Jika Faktor loading pada pada gambar diatas kurang jelas, Anda dapat melihatnya pada tabel berikut di kolom paling akhir (pilih f_loading)",style="font-family: 'cursive';color: blue;text-align:left"),
                                       radioButtons("loading","Pilih Data yang ditampilkan",choices = c("f_loading","semua"),selected = "semua"),
                                       tableOutput("tabelcfa"),
                                       downloadButton("downloadfaktorloading","unduh")
                              ),
                              tabPanel("Analisis dengan Tabel",
                                       h3("KESIMPULAN ANALISIS CFA",style="font-family: 'cursive';color: blue;text-align:center"),
                                       tableOutput("keputusann"),
                                       downloadButton("downloadkeputusan","unduh"),
                                       h3("Catatan:",style="font-family: 'cursive';color: red;text-align:left"),
                                       h4("1. Jika tidak memenuhi, Lihat pada gambar apakah ada butir yang faktor loadingnya masih <0.3 atau <0.45 atau  <0.5 gunakan sesuai referesi Anda (Hilangkan satu persatu dan lihat kesimpulan, jika belum memenuhi lakukan  samapai memenuhi pada bagian KESIMPULAN):",style="font-family: 'cursive';color: blue;text-align:left"),
                                       h4("2. Jika sudah memenuhi TETAP hilangkan butir yang faktor loadingnya masih <0.3 atau <0.45 atau  <0.5 gunakan sesuai referesi kamu. (caranya sama dengan no.1):",style="font-family: 'cursive';color: blue;text-align:left"),hr(),
                                       h3("RELIABILILTAS",style="font-family: 'cursive';color: blue;text-align:center"),hr(),hr(),
                                       tableOutput("reliabilitas"),
                                       downloadButton("downloadreliable","unduh"),br(),
                                       h4("Lihat koefisien Alpah untuk Butir Fit dan tentukan level Reliabilitasnya menggunakan Tabel berikut",style="font-family: 'cursive';color: red;text-align:left"),
                                       tableOutput("criteriareliabel")

                              )
                   )
         )
  )
)
