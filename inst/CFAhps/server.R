
server <- function(input, output, session) {
  hpsok<- reactive({
    ambil_file_data<- input$ambildatahps
    if(is.null(ambil_file_data))
      return(NULL)
    if(input$ekstensi=="xlsx"){
      dataset<- read_xlsx(ambil_file_data$datapath, sheet=1)
    }
    if(input$ekstensi=="csv"){
      dataset<- read.csv(ambil_file_data$datapath, sep=input$pemisahvariabel)
    }

    p<-as.data.frame(dataset)
    colnames(p)<-paste0("X",1:ncol(p))

    return(p)
  })
  output$keter<-renderUI({
    HTML(paste("<b>untuk Order 1 tuliskan:</b>","Y1=~X2+X4+X5+X6+X1+X3;","Y2=~X7+X8+X10+X11;","Y3=~X12+X13+X14+X17+X16", "<b>untuk Order2 tambahkan:</b>","Y=~Y1+Y2+Y3",sep ="<br/>"))
  })
  output$inputdatahps<-DT::renderDT({
    if(is.null(hpsok()))  return(NULL)


    DT::datatable(hpsok(), caption = "", rownames = TRUE,
                  options = list(autoWidth = T, scrollX = TRUE,pageLength = 15,
                                 columnDefs = list(list(width = '100px', targets = 1)),
                                 paging =T, searching = FALSE), selection='none')

  })

  syaratKMOtest<-reactive({
    kmo<-KMO(hpsok())
    kmotes<-kmo$MSA
    kmotesket<-ifelse (kmotes>0.5,"Data Anda mencukupi untuk digunkan uji analisis Faktor","Data Anda mencukupi untuk digunkan uji analisis Faktor")
    kmotesok<-data.frame(kmotes,kmotesket)
    colnames(kmotesok)<-c("Nilai KMO Tes","Interpretasi KMO Tes")

    return(kmotesok)
  })
  output$kmotest<-renderTable({
    tryCatch(expr = {
      syaratKMOtest()
    },error = function(e){""})

  },width = "100%",bordered = TRUE,align = "l")

  syaratKMObutir<-reactive({
    kmo<-KMO(hpsok())
    kmobutir<-kmo$MSAi
    i<-1:length(kmobutir)
    kmobutirket<-ifelse(kmobutir>=0.5,paste("Sampel untuk no-",i,"CUKUP"),paste("Sampel untuk no-",i,"TIDAK CUKUP"))
    kmobutirok<-data.frame(kmobutir,kmobutirket)
    colnames(kmobutirok)<-c("KMO Butir","Interpretasi")
    return(kmobutirok)
  })
  output$kmobutir<-renderTable({
    tryCatch(expr = {
      syaratKMObutir()
    },error = function(e){""})

  },width = "100%",bordered = TRUE,align = "l")

  output$multikol<-renderPlot({
    corrplot(cor(hpsok()))
  })
  output$downloadkmotes<- downloadHandler(
    filename = function() {
      paste("kmotest", ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(syaratKMOtest(), file)
    }
  )
  output$downloadkmobutir<- downloadHandler(
    filename = function() {
      paste("kmobutir", ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(syaratKMObutir(), file)
    }
  )
  #Perhitungan CFA
  CFA<-reactive({
    if (isTRUE(input$analisi==0)) return(NULL)
    if(!isTRUE(input$analisis==0)){
      model<-input$aspek1
      Out<-cfa(model,hpsok(), sample.nobs =input$observasi)}
    return(Out)
  })

  tableCFAall<-reactive({
    summarycfa<-summary(CFA(),fit.measures=TRUE,standardized=TRUE)
    oksumary<-data.frame(summarycfa$pe[,1:3],round(summarycfa$pe[,4:ncol(summarycfa$pe)],2))
    return(oksumary)
  })

  tabelCFA<-reactive({
    oksumary<-tableCFAall()
    if(input$loading!="semua"){

      oksumary<-oksumary[which(oksumary$op=="=~"),]
    }
    return(oksumary)
  })
  output$downloadfaktorloading<- downloadHandler(
    filename = function() {
      paste("Faktor_Loading", ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(tabelCFA(), file)
    }
  )

  #table hasil analisis
  output$tabelcfa<-renderTable({
    tryCatch(expr = {
      tabelCFA()
    },error = function(e){})

  },width = "100%",bordered = TRUE,align = "l")
  #gmbar dari konstruk
  gambarCFA<-reactive({
    semPaths(CFA(), what='std', layout="tree",title=TRUE, style="LISREL",posCol=5, sizeMan = 5, sizeMan2 = 5, weighted = FALSE, nCharNodes = 15)
  })

  output$gambarcfa<-renderPlot({
    tryCatch(expr = {
      gambarCFA()

    },error = function(e){})

  })

  # Statistik Penentu KESIMMPULAN
  keputusan<-reactive({
    keputusan<-round(fitMeasures(CFA(), c("chisq", "df", "pvalue", "cfi", "rmsea")),3)
    ket1<-ifelse(keputusan[[3]]>=0.05,"Memenuhi","Tidak_memenuhi")
    ket2<-ifelse(keputusan[[4]]>0.9,"Memenuhi","Tidak_memenuhi")
    ket3<-ifelse(keputusan[[5]]<0.08,"Memenuhi","Tidak_memenuhi")
    ketok<-ifelse(ket1=="Memenuhi" && ket2=="Memenuhi"&& ket1=="Memenuhi","Secara Empiris instrumen Anda Telah memiliki konstruk yang valid.","Secara Empiris instrumen Anda Belum memiliki konstruk yang valid.")
    keputusan1<-c("__","__",ket1,ket2,ket3,ketok)
    keputusanok<-data.frame(c(keputusan,"-"),keputusan1)
    colnames(keputusanok)<-c("nilai","Interpretasi")
    rownames(keputusanok)<-c("Chi_Square","df","P_Value","CFI","RMSEA","KESIMPULAN")
    return(keputusanok)
  })
  output$keputusann<-renderTable({
    tryCatch(expr = {
      keputusan()
    },error = function(e){})

  },width = "100%",bordered = TRUE,align = "l",rownames = TRUE)

  output$downloadkeputusan<- downloadHandler(
    filename = function() {
      paste("Tabel_kesimpulanCFA", ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(keputusan(), file)
    }
  )
  #butir yang digunakan saja untuk model fit
  Datause<-reactive({
    usedata<-tableCFAall()
    usedata1<-usedata$rhs[which(usedata$op=="=~")]
    usedata2<-c()
    for(i in 1:length(usedata1)){
      usedata2<-c(usedata2,which(usedata1[i]==colnames(hpsok())))
    }
    usedata2<-sort(unique(usedata2))
    return(usedata2)
  })
  reliable<-reactive({
    reliabel1<-reliability(keys=NULL,hpsok()[,Datause()])
    reliabel2<-reliability(keys=NULL,hpsok())
    reliabel<-rbind(reliabel1$result.df,reliabel2$result.df)
    rownames(reliabel)<-c("Butir_FIT_saja","Semua_Butir")
    return(reliabel)
  })
  output$reliabilitas<-renderTable({
    tryCatch(expr = {
      reliable()
    },error = function(e){})

  },width = "100%",bordered = TRUE,align = "l",rownames = TRUE)

  output$downloadreliable<- downloadHandler(
    filename = function() {
      paste("Tabel_reliabilitas", ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(data.frame(reliable()), file)
    }
  )

  levelreliability<-reactive({
    Cronbach_Alpha<-c("a>=0.9","0.8<= a <0.9","0.7<= a <0.8","0.6<= a <0.7","a <0.6")
    Level_reliabilitas<-c("Sangat Amat Baik(Excellent)","Sangat Baik(Very Good)","Baik(Good)","Sendang(Moderate)","Buruk(Poor)")
    relibiliti<-data.frame(Cronbach_Alpha, Level_reliabilitas)
    return(relibiliti)
  })
  output$criteriareliabel<-renderTable({
    tryCatch(expr = {
      levelreliability()
    },error = function(e){})
  },width = "100%",bordered = TRUE,align = "l")
}

