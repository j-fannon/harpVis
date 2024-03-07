# server for shiny_plot_spatial_verif
options(shiny.maxRequestSize=20*1024^2)
Sys.setenv(TZ='UTC')

read_sql <- function(filepath,score=NULL){
    #TODO: this function might need a more appropriate name
    sqlite_con <- RSQLite::dbConnect(RSQLite::SQLite(), gsub("\\\\","/",filepath))
    scores <- DBI::dbListTables(sqlite_con)
    if(is.null(score)) {score=scores[1]}
    verif_data <- dplyr::tbl(sqlite_con, score) %>% dplyr::collect(n=Inf)
    RSQLite::dbDisconnect(sqlite_con)

    items <- list("verif_data" = verif_data, "scores" = scores)
    return(items) #returns a list of dataframe and list of scores
}

update_options <- function(input,scores,session) {
    #TODO: this function might need a more appropriate name
    dates <-  harpCore::unixtime_to_dttm(unique(input$fcdate))
    models <- unique(input$model)
    params <- unique(input$prm)
    leadtimes <- unique(input$leadtime)/3600
    updateSelectInput(session,'score',      choices=c(scores),
                                                selected=c(scores)[1])
    updateDateRangeInput(session,'dates',   start=dates[1],
                                            end=tail(dates,n=1),
                                            min=min(dates),
                                            max=max(dates))
    updateSelectInput(session,'model',      choices=c(models),
                                                selected=c(models)[1])
    updateSelectInput(session,'leadtime',   choices=c(leadtimes),
                                                selected = c(leadtimes))
    updateSelectInput(session,'param',      choices=c(params),
                                                selected=c(params)[1])

}

server <- function(input, output, session) {


  ############################################################
  # LOAD DATA                                                #
  ############################################################
  
  getData <- reactive({
    req(input$filein)
    if(is.null(input$filein)) return(NULL)
    read_sql(input$filein$datapath)
  })

  # getData is a list! getData()$verif_data is a tibble and getData()$scores is a vector
  output$fileUploaded <- reactive({
    return(!is.null(getData()))
  })

  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)

  observe({
    req(input$filein)
    verif_data <- getData()$verif_data
    scores <- getData()$scores
    update_options(verif_data,scores, session)
  })
    
  ############################################################
  # SHOW DATA                                                #
  ############################################################

  ## needs to check selection on the form and pass them as plotting options to plot_spatial_verif!
  ## if score name changes with selection then read_sql needs to be called again to get the correct dataframe!

  output$plot <- renderPlot({

    req(input$showdata)
    score <- isolate(input$score)
    models <- isolate(input$model)
    leadtimes <- isolate(input$leadtime)
    fcdate_range <- isolate(input$dates)
#    thresholds <- isolate(input$threshold) #TODO, coming with plotting options
#    scales <- isolate(input$scale)         #TODO, coming with plotting options
    params <- isolate(input$param)
    
    fcbdate <- gsub("-", "", fcdate_range[1])
    fcedate <- gsub("-", "", fcdate_range[2])

    verif_data <- read_sql(input$filein$datapath, score)$verif_data
    filter_by <- vars(
      model    %in% models, 
      leadtime %in% leadtimes,
      fcdate >= harpCore::as_dttm(fcbdate) & fcdate <= harpCore::as_dttm(fcedate),
#      threshold   %in% thresholds,         #TODO, dependent on score 
#      scale   %in% scales,                 #TODO, dependent on score 
      prm      %in% params,
    )
    #plot_opts = ...                        # TODO, include plotting options to interface

    harpVis:::plot_spatial_verif(verif_data, {{score}}, filter_by = filter_by)

  },width = 800, height = 600)

  output$table <- renderDataTable({

    req(input$showdata)
    score <- input$score
    verif_data <- read_sql(input$filein$datapath, score)$verif_data
    return(verif_data)
  })

}
