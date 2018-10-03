#' Shiny app server object
#'
#' @importFrom graphics hist
#' @import shiny

shinyAppServer = function(input, output, session){

  myData <- reactive({
    # validate(
    #  need(input$myData != "", "")
    # )

    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    data <- read.csv(inFile$datapath, header = TRUE,row.names=1)
    data
  })



  output$varselect <- renderUI({

    #if (identical(myData(), '') || identical(myData(),data.frame())) return(NULL)

    x=sapply(myData(),class)
    x=(x!="factor")
    df=myData()[,x]

    if (identical(df, '') || identical(df,data.frame())) return(NULL)

    # Variable selection:
    selectInput("vars", "Variables to use:",
                names(df), names(df), multiple =TRUE)
  })

  output$ncp <-renderUI({
    p<-length(input$vars)
    numericInput(inputId = "ncp_s",label = "Number of PC",min = 2,max = p,value = p,step = 1)
  })

  output$addlabels <-renderUI({
    selectInput(inputId = "addlabels2",label = "Add labels",choices = c(T,F),selected = T,multiple = F)
  })

  output$varSupQuant <- renderUI({

    # if (identical(myData(), '') || identical(myData(),data.frame())) return(NULL)

    x=sapply(myData(),class)
    x=(x!="factor")
    df=myData()[,x]

    if (identical(df, '') || identical(df,data.frame())) return(NULL)

    # Variable selection:
    selectInput("varsSup", "Quantitative Supplementary variable:",
                names(df), selected=c(), multiple =T)
  })


  output$varSupQuali <- renderUI({


    x=sapply(myData(),class)
    x=(x=="factor")
    df=names(myData())[x]
    #if (identical(df, '') || identical(df,data.frame())) return(NULL)
    # Variable selection:
    selectInput("varsSupQuali", "Qualitative Supplementary variable:",
                df, df, multiple =T)
  })



  output$ncp_main <- renderUI({
    real_vars=setdiff(as.character(input$vars),as.character(input$varsSup))
    p=length(real_vars)
    numericInput(inputId = "ncp_main2",label =  "Number of PC",min = 4,max = p,value = 5)
  })

  output$scale <- renderUI({
    selectInput(inputId = "scale2",label =  "Scale the data",choices = c(T,F),selected = T,multiple = F)
  })

  output$table <- renderTable({

    if (is.null(input$vars) || length(input$vars)==0) return(NULL)

    return(myData()[,input$vars,drop=FALSE])
  })

  output$varsSup_s1 <- renderUI({
    selectInput("varsSup_s2", "Quantitative Supplementary variable:",choices = input$varsSup, selected = c(), multiple =T)

  })


  output$varsSupQuali_s1 <- renderUI({
    selectInput("varsSupQuali_s2", "Qualitative Supplementary variable:",choices = input$varsSupQuali, selected = c(), multiple =F)

  })

  pca_res<-reactive({
    real_vars=setdiff(as.character(input$vars),as.character(input$varsSup))
    vars=c(real_vars,as.character(input$varsSup),as.character(input$varsSupQuali))
    l=length(real_vars)
    df<-myData()[,vars]
    df=as.data.frame(df)
    ll=length(input$varsSup)
    if(ll>0) pc<-PCA(X = df,ncp = input$ncp_main2,scale.unit = input$scale2,quanti.sup = (l+1):(l+ll),quali.sup = (l+ll+1):ncol(df),graph=F)
    if(ll==0) pc<-PCA(X = df,ncp = input$ncp_main2,scale.unit = input$scale2,quali.sup = (l+1):ncol(df),graph=F)
    pc
  })





  output$pca<-renderTable({
    df=data.frame(pca_res()$eig)
    colnames(df)=c("Eigen value","% of Variance","Cumulative %")
    df[,2]=paste0(round(df[,2],2),"%",sep="")
    df[,3]=paste0(round(df[,3],2),"%",sep="")
    return(df)
  })

  screeplot <-reactive({
    if (is.null(myData())) return(NULL)
    p<-fviz_screeplot(pca_res(), ncp = input$ncp_s,addlabels = input$addlabels2) + theme_classic()
    print(p)
  })

  output$screeplot1 <-renderPlot({
    print(screeplot())
  })

  output$downloadScreeplot <- downloadHandler(
    filename = "fig.png",
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
      ggsave(file, plot = screeplot(), device = device)
    }
  )

  ## Individual tables


  ### Coordinates


  coord_pc_react<-reactive({
    if (is.null(myData())) return(NULL)
    df=data.frame(pca_res()$ind$coord)
    rownames(df)=rownames(myData())
    round(df,3)
  })

  output$coord_pc<-renderDataTable({

    datatable(coord_pc_react(),options = list(
      columnDefs = list(list(className = 'dt-center',width = '200px'))
    ))
  })

  output$download_coord_pc <- downloadHandler(
    filename = function() { paste(gsub(pattern = ".csv",replacement = "", x = input$file1 ), '_ind_coord.csv', sep='') },
    content = function(file) {
      write.csv(coord_pc_react(), file)
    }
  )


  ### Cos2

  coord_pc_cos2_react<-reactive({
    if (is.null(myData())) return(NULL)
    df=data.frame(pca_res()$ind$cos2)
    rownames(df)=rownames(myData())
    round(df,3)
  })


  output$coord_cos2_pc<-renderDataTable({
    datatable(coord_pc_cos2_react(),options = list(
      columnDefs = list(list(className = 'dt-center',width = '200px'))
    ))
  })


  output$download_coord_cos2_pc <- downloadHandler(
    filename = function() { paste(gsub(pattern = ".csv",replacement = "", x = input$file1 ), '_ind_cos2.csv', sep='') },
    content = function(file) {
      write.csv(coord_pc_cos2_react(), file)
    }
  )

  ### Contribution

  coord_pc_cont_react<-reactive({
    if (is.null(myData())) return(NULL)
    df=data.frame(pca_res()$ind$contrib)
    rownames(df)=rownames(myData())
    round(df,3)
  })



  output$coord_cont_pc<-renderDataTable({
    datatable(coord_pc_cont_react(),options = list(
      columnDefs = list(list(className = 'dt-center',width = '200px'))
    ))
  })


  output$download_coord_cont_pc <- downloadHandler(
    filename = function() { paste(gsub(pattern = ".csv",replacement = "", x = input$file1 ), '_ind_contrib.csv', sep='') },
    content = function(file) {
      write.csv(coord_pc_cos2_react(), file)
    }
  )
  ###


  output$axes1_ind <-renderUI({
    p<-length(input$vars)
    numericInput(inputId = "axes1_ind_s",label = "Horizontal Axis",min = 1,max = p,value = 1,step = 1)
  })

  output$axes2_ind <-renderUI({
    p<-length(input$vars)
    numericInput(inputId = "axes2_ind_s",label = "Vertical Axis",min = 1,max = p,value = 2,step = 1)
  })


  output$geom_ind <-renderUI({
    selectInput(inputId = "geom_ind_s",label = "Geom",choices = c("point","text"),selected = "point",multiple = T)
  })

  output$repel_ind <-renderUI({
    selectInput(inputId = "repel_ind_s",label = "Repel",choices = c(T,F),selected = F,multiple = F)
  })


  output$hab_ind <-renderUI({
    selectInput(inputId = "hab_ind_s",label = "Habilage",choices = c(T,F),selected = F,multiple = F)
  })

  output$ell_ind <-renderUI({
    selectInput(inputId = "addEllipse_ind_s",label = "Ellipse",choices = c(T,F),selected = F,multiple = F)
  })

  output$alpha_ind <-renderUI({
    sliderInput(inputId = "alpha_ind_s",label = "alpha",min = 0,max = 1,value = .5)
  })

  output$select_ind <-renderUI({
    p=nrow(myData())
    sliderInput(inputId = "select_ind_s",label = "Cos2",min = 0,max = p,value = p)
  })

  ind_plot<-reactive({
    if (is.null(myData())) return(NULL)
    if(input$hab_ind_s==T) hab_x=input$varsSupQuali_s2
    else hab_x="none"
    fviz_pca_ind(pca_res(),axes = c(input$axes1_ind_s,input$axes2_ind_s),geom = input$geom_ind_s,
                 repel = input$repel_ind_s,habillage = hab_x,
                 addEllipses = input$addEllipse_ind_s,
                 alpha.ind = as.numeric(input$alpha_ind_s),select.ind = list(cos2 = input$select_ind_s)
    )
  })

  output$ind_plot1 <-renderPlot({
    if (is.null(myData())) return(NULL)
    print(ind_plot())
  })

  output$downloadIndplot <- downloadHandler(
    filename = "fig.png",
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
      ggsave(file, plot = ind_plot(), device = device)
    })

  ### Variables

  coord_var_pc_react<-reactive({
    if (is.null(myData())) return(NULL)
    df=data.frame(pca_res()$var$coord)
    round(df,3)
  })


  output$coord_var_pc<-renderDataTable({
    datatable(coord_var_pc_react(),options = list(
      columnDefs = list(list(className = 'dt-center',width = '200px'))
    ))
  })

  output$download_coord_var_pc <- downloadHandler(
    filename = function() { paste(gsub(pattern = ".csv",replacement = "", x = input$file1 ), '_var_coord.csv', sep='') },
    content = function(file) {
      write.csv(coord_var_pc_react(), file)
    }
  )


  ###  Cos2


  coord_cos2_var_pc_react<-reactive({
    if (is.null(myData())) return(NULL)
    df=data.frame(pca_res()$var$cos2)
    round(df,3)
  })


  output$coord_cos2_var_pc<-renderDataTable({
    datatable(coord_cos2_var_pc_react(),options = list(
      columnDefs = list(list(className = 'dt-center',width = '200px'))
    ))
  })


  output$download_cos2_var_pc <- downloadHandler(
    filename = function() { paste(gsub(pattern = ".csv",replacement = "", x = input$file1 ), '_var_cos2.csv', sep='') },
    content = function(file) {
      write.csv(coord_cos2_var_pc_react(), file)
    }
  )

  ### Contribution


  coord_cont_var_pc_react<-reactive({
    if (is.null(myData())) return(NULL)
    df=data.frame(pca_res()$var$contrib)
    round(df,3)
  })



  output$coord_cont_var_pc<-renderDataTable({
    datatable(coord_cont_var_pc_react(),options = list(
      columnDefs = list(list(className = 'dt-center',width = '200px'))
    ))
  })


  output$download_cont_var_pc <- downloadHandler(
    filename = function() { paste(gsub(pattern = ".csv",replacement = "", x = input$file1 ), '_var_cont.csv', sep='') },
    content = function(file) {
      write.csv(coord_cont_var_pc_react(), file)
    }
  )


  ### correlation

  coord_cor_var_pc_react<-reactive({
    if (is.null(myData())) return(NULL)
    df=data.frame(pca_res()$var$cor)
    round(df,3)
  })


  output$coord_cor_var_pc<-renderDataTable({
    datatable(coord_cor_var_pc_react(),options = list(
      columnDefs = list(list(className = 'dt-center',width = '200px'))
    ))
  })


  output$download_cor_var_pc <- downloadHandler(
    filename = function() { paste(gsub(pattern = ".csv",replacement = "", x = input$file1 ), '_var_cor.csv', sep='') },
    content = function(file) {
      write.csv(coord_cor_var_pc_react(), file)
    }
  )


  ###  plot of variables

  output$axes1_var<-renderUI({
    p<-length(input$vars)
    numericInput(inputId = "axes1_var_s",label = "Horizontal Axis",min = 1,max = p,value = 1,step = 1)
  })

  output$axes2_var <-renderUI({
    p<-length(input$vars)
    numericInput(inputId = "axes2_var_s",label = "Vertical Axis",min = 1,max = p,value = 2,step = 1)
  })


  output$geom_var <-renderUI({
    selectInput(inputId = "geom_var_s",label = "Geom",choices = c("point","text","arrow"),selected = "arrow",multiple = T)
  })

  output$repel_var <-renderUI({
    selectInput(inputId = "repel_var_s",label = "Repel",choices = c(T,F),selected = F,multiple = F)
  })


  output$SupVar <-renderUI({
    selectInput(inputId = "SupVar_s",label = "Supplementary Variable",choices = c(T,F),selected = F,multiple = F)
  })


  output$select_var <-renderUI({
    sliderInput(inputId = "select_var_s",label = "Cos2",min = 0,max = 1,value = 0)
  })





  var_plot<-reactive({
    if (is.null(myData())) return(NULL)
    dviz_var(pca_res(),axes =c(input$axes1_var_s,input$axes2_var_s),
             geom = input$geom_var_s,
             namesV = input$varsSup_s2,supvar = input$SupVar_s,
             repel = input$repel_var_s,
             select.var = input$select_var_s,
             scale=input$scale2)

  })

  output$var_plot1 <-renderPlot({
    if (is.null(myData())) return(NULL)
    print(var_plot())
  })



  output$downloadVarplot <- downloadHandler(
    filename = "fig.png",
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
      ggsave(file, plot = var_plot(), device = device)
    })


  ## Download all


  #output$download_all <- downloadHandler(
  #  filename = function() { paste(gsub(pattern = ".csv",replacement = "", x = input$file1 ), '_all.RData', sep='') },
  #  content = function(file) {
  #    save(list = list(pca_res=pca_res(),ind_plot=ind_plot(),var_plot=var_plot()), file = file)
  #  }
  #)

  ## biplot

  output$varsSupQuali_s2 <- renderUI({
    selectInput("varsSupQuali_s3", "Qualitative Supplementary variable:",choices = input$varsSupQuali, selected = c(), multiple =F)

  })

  output$axes1_bi<-renderUI({
    p<-length(input$vars)
    numericInput(inputId = "axes1_ind_s2",label = "Horizontal Axis",min = 1,max = p,value = 1,step = 1)
  })

  output$axes2_bi <-renderUI({
    p<-length(input$vars)
    numericInput(inputId = "axes2_ind_s2",label = "Vertical Axis",min = 1,max = p,value = 2,step = 1)
  })

  ##biplot=T,varlab=T,indlab=T,indpoint=T,
  ## alpha=.4)

  output$geom_var2 <-renderUI({
    selectInput(inputId = "geom_var_s2",label = "Variables",choices = c("point","text","arrow"),selected = "point",multiple = T)
  })

  output$geom_ind2 <-renderUI({
    selectInput(inputId = "geom_ind_s2",label = "Individuals",choices = c("point","text","nothing"),selected = "point",multiple = T)
  })

  output$alpha <-renderUI({
    sliderInput(inputId = "alpha_s",label = "alpha",min = 0,max = 1,value = .5)
  })


  biplot_draw<-reactive({
    if (is.null(myData())) return(NULL)
    zfactor=myData()[,input$varsSupQuali_s3]
    pc=pca_res()
    axes=c(input$axes1_ind_s2,input$axes2_ind_s2)
    DrawHullConvex_PC(pc=pc,axes=axes,zfactor=zfactor,convex=input$convex,
                      varnames=rownames(pc$var$coord),
                      indnames=rownames(pc$ind$coord),
                      biplot=input$biplot,
                      varlab=input$geom_var_s2,
                      indlab=input$geom_ind_s2,
                      alpha=input$alpha_s)
  })



  output$biplot1 <-renderPlot({
    if (is.null(myData())) return(NULL)
    print(biplot_draw())
  })



  output$downloadBiplot <- downloadHandler(
    filename = "fig.png",
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
      ggsave(file, plot = biplot_draw(), device = device)
    })


}
