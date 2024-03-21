require(shiny)
require(bs4Dash)
require(shinyWidgets)
require(reactable)
require(reactablefmtr)
require(readxl)
require(tidyverse)
require(shinycssloaders)
require(sortable)
library(RCurl)


body <- bs4Dash::dashboardBody(
  useSweetAlert(),
  fluidRow(
    column(
      width = 4,
      tabBox(
        width = 12,
        id = "tabcard",
        solidHeader = T,
        status = "gray-dark",
        type = "tabs",
        tabPanel(
          title = "Wgrywanie danych",
          
          column(
            offset = 9, width = 3,
            actionBttn(
              inputId = "default",
              label = "Dane domyślne",
              style = "simple",
              size = "xs",
              color =  "success"
            )
          ),
          fileInput("fileinput", "Wybierz plik .xlsx.", accept = ".xlsx"),

          pickerInput(
            inputId = "selectinput",
            label = "Wybierz kolumny, z którymi chcesz pracować", 
            choices = NULL,
            options = list(
              `actions-box` = TRUE,
              `selected-text-format` = "count > 3"), 
            multiple = TRUE
          )
        ),
        
        tabPanel(
          title = "Metody Porównawcze",
          value = "2",
          selectInput(
            "selecttrend",
            label = "Wybierz kolumny, w których należy odwrócić trend",
            choices = NULL,
            selected = NULL,
            multiple = T
          ),
          
          radioGroupButtons(
            inputId = "method",
            label = "Wybierz metodę, którą chcesz zastosować",
            choices = c("Dominacji","Leksykograficzna", "Laplace'a", "Addytywna"),
            status = "success"
          ),
          
          uiOutput("sortableUI"),
          uiOutput("gauge.ui"),
          
          actionButton(
            inputId = "go",
            label = "See result"
          )
        )
      ),
    ),
    column(
      width  = 8,
      tabBox(
        width = 12,
        id = "tabcard2",
        solidHeader = T,
        status = "gray-dark",
        type = "tabs",
        tabPanel(
          title = "Oryginalne",
          reactableOutput("table_original") %>% withSpinner()
        ),
        tabPanel(
          title = "Znormalizowane",
          reactableOutput("table_tidy") %>% withSpinner()
        ),
        tabPanel(
          title = "Wynikowe",
          reactableOutput("table_method") %>% withSpinner(),
          reactableOutput("text") %>% withSpinner()
        )
      )
    )
  )
)

sidebar <- bs4Dash::dashboardSidebar(
  disable = T
)

header <- bs4Dash::dashboardHeader(
  
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize=100*1024^2)
  
  observeEvent(input$default, {
    updateTabsetPanel(session, "tabcard", selected = "2")
  })

  
  contents <- reactive({
    
    file <- input$fileinput
    ext <- tools::file_ext(file$datapath)
    
    req(ext)
    if(ext == "xlsx"){
      observe({
        sendSweetAlert(
          session = session,
          title = "Gratualcje!",
          text = "Wgrano plik!",
          type = "success"
        )
      })
      content(read_excel(file$datapath, sheet = 1))
    } else {
      observe({
        sendSweetAlert(
          session = session,
          title = "Ejże!",
          text = "Należy załadować plik .xslx o strukturze podobnej do danych domyślnych",
          type = "warning"
        )
      })
      content(data.frame())
    }
  })
  
  content <- reactiveVal(data.frame())
  
  observeEvent(input$default, {
    observe({
      sendSweetAlert(
        session = session,
        title = "Gratualcje!",
        text = "Wgrano plik!",
        type = "success"
      )
    })
    content(read_excel("MI_Klienci_14.xlsx"))
  })
  
  output$sortableUI <- renderUI({
    if(input$method == "Leksykograficzna") {
      rank_list(
        text = "Ułóż kolumny wg wagi, od największej do najmniejszej",
        labels = names(content_fitered() %>% select(where(is.numeric))),
        input_id = "order",
        options = sortable_options(multiDrag = TRUE)
      )
    }
  })
  
  observe({
    updatePickerInput(
      session,
      "selectinput",
      choices = names(content()),
      selected = names(content())
    )
  })
  
  observe({
    updateSelectInput(
      session,
      "selecttrend",
      choices = names(content() %>% select(where(is.numeric)))
    )
  })
  
  output$gauge.ui <- renderUI({
    if(input$method == "Addytywna")
      fluidRow(
        lapply(1:length(content_fitered() %>% select(where(is.numeric))), function(x) {
          shinyWidgets::knobInput(paste0("gauge", x),
                                  (content_fitered() %>% select(where(is.numeric)) %>% names)[x],
                                  100 / length(content_fitered() %>% select(where(is.numeric))),
                                  angleOffset = 180,
                                  displayPrevious = T,
                                  width = "90px",
                                  post = "%"
          )
        })
      )
  })
  
  content_fitered <- reactive({
    content() %>% select(input$selectinput) 
  })
  
  output$table_original <- renderReactable({
    if (input$go > 0)
    isolate(content_fitered()) %>%
      reactable(
        theme = flatly(),
        defaultColDef = colDef(style = color_scales(content_fitered(), colors = c("#d8f3dc", "#52b788", "#4e878c"), opacity = .5))
      )
  })
  
  output$table_tidy <- renderReactable({
    if (input$go > 0)
    
    tidy() %>% 
      reactable(
        theme = flatly(),
        defaultColDef = colDef(style = color_scales(tidy(), colors = c("#d8f3dc", "#52b788", "#4e878c"), opacity = .5))
      )
  })
  
  
  tidy <- reactive({
    # Przygotowywanie ramki do pracy z danymi
    df.tid <- (content_fitered())
    
    # Normalizacja
    normalize <- function(x) {
      ((x - min(x)) / (max(x) - min(x))) %>% round(2)
    }
    
    df.tid <- 
      df.tid %>%
      mutate(
        across(
          .cols = where(is.numeric),
          .fns = ~normalize(.x)
        )
      )
    # Odwrócenie trendu
    reverse <- function(x) {
      max(x) - x
    }
    
    df.tid <- 
      df.tid %>%
      mutate(
        across(
          .cols = isolate(input$selecttrend),
          .fns = ~reverse(.x)
        )
      )
    df.tid
  })
  
  
  
  output$table_method <- renderReactable({
    if (input$go > 0)
    
    if(isolate(input$method) == "Dominacji") {
      print("1")
      df.tid <- tidy()%>% select(where(is.numeric))
      # Liczba kryteriów
      K <- ncol(df.tid) - 1
      # Liczba wariantów
      W <- nrow(df.tid) 
      
      DOM <- NULL
      exclude <- NULL
      
      DOM[1:W] <- NA
      exclude[1:W] <- NA
      print("2")
      # Potencjalny wariant dominujący i
      for(i in 1:W) {
        # Potencjalny wariant zdominiowany j
        for(j in 1:W) {
          # Jeżeli i == j, iteruj j
          if(i!=j) {
            # Czy istnieje ostra dominacja?
            st_const <- sum(
              (df.tid[i, -1] %>% as.numeric) > (df.tid[j, -1] %>% as.numeric) %>% 
                as.numeric
            ) > 1
            # Jeżeli istnieje ostra dominacja to...
            
            if(st_const) {
              
              # Czy w każdym kryterium zachodzi przynajmniej nieostra dominacja
              nd_const <- sum(
                (df.tid[i, -1] %>% as.numeric) >= (df.tid[j, -1] %>% as.numeric) %>% 
                  as.numeric
              ) == K
              # Jeżeli zawsze zachodzi przynajmniej nieostra dominacja, a chociaż raz ostra, to..
              if(nd_const) {
                # Wektor wartości zdominowanych (opisowy)
                if(is.na(DOM[j])) {
                  DOM[j] <- paste("Wariant", j, "zdominowany przez wariant", i)  
                } else {
                  DOM[j] <- paste(DOM[j], i, sep = ", ")
                }
                # Wektor wartości, zdominowanych (TRUE/NA)
                exclude[j] <- T
              } 
            } 
          }
        } 
      }
      # Tabela po wykluczeniu wariantów zdominowanych
      
      df.tid <- content_fitered()[is.na(exclude), ] 
      reactable(
        df.tid,
        theme = flatly(),
        defaultColDef = colDef(style = color_scales(df.tid, colors = c("#d8f3dc", "#52b788", "#4e878c"), opacity = .5))
      )
    } else if(isolate(input$method) == "Leksykograficzna") {
      df.tid <- tidy()
      # Liczba kryteriów
      K <- ncol(df.tid) - 1
      # Liczba wariantów
      W <- nrow(df.tid)
      
      df.lg <- df.tid
      for(i in 1:length(input$order)) {
        win <- max(df.lg[[input$order[i]]]) == df.lg[[input$order[i]]]
        df.lg <- df.lg[win,]
        if(sum(win) == 1) break()  
      }
      
      df.lg <- content_fitered()[df.lg[[1]] == content_fitered()[[1]],]
      
      reactable(
        df.lg,
        theme = flatly(),
        defaultColDef = colDef(style = color_scales(df.lg, colors = c("#d8f3dc", "#52b788", "#4e878c"), opacity = .5))
      )
    } else if(isolate(input$method) == "Laplace'a") {
      df.tid <- tidy()
      print(df.tid)
      
      df.tid <- 
        apply(df.tid %>% select(where(is.numeric)), 1, mean) %>%
        data.frame(
          NazwaWariantu = df.tid$NazwaWariantu,
          LaPlace = round(.,2)
        ) %>% select(-`.`)
      
      reactable(
        df.tid,
        theme = flatly(),
        defaultColDef = colDef(style = color_scales(df.tid, colors = c("#d8f3dc", "#52b788", "#4e878c"), opacity = .5))
      )
    } else if(isolate(input$method) == "Addytywna") {
      df.tid <- tidy() %>% select(where(is.numeric))
      
      waga <- NULL
      for(i in 1:length(df.tid %>% select(where(is.numeric)))) {
        waga <- reactiveValuesToList(input)[[paste0("gauge", i)]]
        
        df.tid[, i] <- 
          df.tid[[i]] * waga
      }
      
      
      df.tid <- 
        apply(df.tid, 1, mean) %>%
        data.frame(
          NazwaWariantu = tidy()$NazwaWariantu,
          LaPlace = round(.,2)
        ) %>% select(-`.`)
      
      reactable(
        df.tid,
        theme = flatly(),
        defaultColDef = colDef(style = color_scales(df.tid, colors = c("#d8f3dc", "#52b788", "#4e878c"), opacity = .5))
      )
    }
  })
  
  output$text <- renderReactable({
    if (input$go > 0)
    
    if(isolate(input$method) == "Dominacji") {
      df.tid <- tidy()%>% select(where(is.numeric))
      # Liczba kryteriów
      K <- ncol(df.tid) - 1
      # Liczba wariantów
      W <- nrow(df.tid) 
      
      DOM <- NULL
      exclude <- NULL
      
      DOM[1:W] <- NA
      exclude[1:W] <- NA
      
      # Potencjalny wariant dominujący i
      for(i in 1:W) {
        # Potencjalny wariant zdominiowany j
        for(j in 1:W) {
          # Jeżeli i == j, iteruj j
          if(i!=j) {
            # Czy istnieje ostra dominacja?
            st_const <- sum(
              (df.tid[i, -1] %>% as.numeric) > (df.tid[j, -1] %>% as.numeric) %>% 
                as.numeric
            ) > 1
            # Jeżeli istnieje ostra dominacja to...
            if(st_const) {
              # Czy w każdym kryterium zachodzi przynajmniej nieostra dominacja
              nd_const <- sum(
                (df.tid[i, -1] %>% as.numeric) >= (df.tid[j, -1] %>% as.numeric) %>% 
                  as.numeric
              ) == K
              # Jeżeli zawsze zachodzi przynajmniej nieostra dominacja, a chociaż raz ostra, to..
              if(nd_const) {
                # Wektor wartości zdominowanych (opisowy)
                if(is.na(DOM[j])) {
                  DOM[j] <- paste("Wariant", j, "zdominowany przez wariant", i)  
                } else {
                  DOM[j] <- paste(DOM[j], i, sep = ", ")
                }
                # Wektor wartości, zdominowanych (TRUE/NA)
                exclude[j] <- T
              } 
            } 
          }
        } 
      }
      DOM %>% na.omit %>% unique %>% data.frame(Dominacja = .) %>%
        reactable(
          theme = flatly()
        )
    }
  })
  
}



shinyApp(ui = bs4Dash::dashboardPage(header = header, sidebar = sidebar, body = body, dark = NULL), server)
