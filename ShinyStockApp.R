# Ładowanie wymaganych bibliotek
library(shiny)         
library(quantmod)    
library(TTR)         
library(ggplot2)      
library(readxl)        
library(plotly)      
library(dplyr)         

# Pobieranie i przetwarzanie danych o symbolach giełdowych i nazwach firm
stock_info <- stockSymbols()          # Pobiera informacje o dostępnych symbolach giełdowych
names_list <- setNames(stock_info$Name, stock_info$Symbol) # Tworzy listę nazw firm powiązaną z symbolami

# Definicja interfejsu użytkownika aplikacji
ui <- fluidPage(
  titlePanel("Analiza szeregów czasowych"),  # Tytuł aplikacji
  sidebarLayout(
    sidebarPanel(
      # Elementy interfejsu umieszczone w panelu bocznym
      textInput("symbol", "Symbol giełdowy", "AAPL"),  # Pole tekstowe do wprowadzania symbolu giełdowego
      dateRangeInput("dates", "Zakres dat", start = Sys.Date() - 365, end = Sys.Date()),  # Wybór zakresu dat
      actionButton("submit", "Pokaż dane"),  # Przycisk do wysyłania danych
      textOutput("companyName")  # Wyświetlanie nazwy firmy
    ),
    mainPanel(
      # Główny panel do wyświetlania wyników
      plotlyOutput("pricePlot"),  # Miejsce na wykres cen
      tags$div(style = "margin-top: 20px;"),  # Dodanie odstępu między wykresami
      plotlyOutput("returnPlot"),  # Miejsce na wykres stóp zwrotu
      tags$div(style = "margin-top: 20px;"),  # Dodanie odstępu przed sekcją statystyk
      verbatimTextOutput("statsOutput")  # Wyświetlanie statystyk
    )
  )
)

# Definicja serwera aplikacji
server <- function(input, output) {
  # Reaktywna funkcja do pobierania danych na podstawie wejścia użytkownika
  dataInput <- eventReactive(input$submit, {
    # Sprawdzenie, czy zakres dat jest wystarczająco duży
    if (difftime(input$dates[2], input$dates[1], units = "days") < 60) {
      # Wyświetlenie komunikatu o błędzie, jeśli zakres dat jest zbyt krótki
      showModal(modalDialog(
        title = "Błąd",
        "Zakres dat jest zbyt krótki. Wybierz dłuższy zakres dat."
      ))
      return(NULL)
    }
    
    # Próba pobrania danych z Yahoo Finance; obsługa błędów
    tryCatch({
      getSymbols(input$symbol, src = "yahoo", from = input$dates[1], to = input$dates[2], auto.assign = FALSE)
    }, error = function(e) {
      # Wyświetlenie komunikatu o błędzie, jeśli nie można pobrać danych
      showModal(modalDialog(
        title = "Błąd",
        "Nie można pobrać danych. Sprawdź symbol giełdowy."
      ))
      NULL
    })
  })
  
  # Wyświetlanie nazwy firmy na podstawie wprowadzonego symbolu giełdowego
  output$companyName <- renderText({
    symbol <- input$symbol
    if (!is.null(names_list[symbol])) {
      names_list[symbol]
    } else {
      "Nazwa firmy nieznana"
    }
  })
  
  # Generowanie interaktywnego wykresu cen
  output$pricePlot <- renderPlotly({
    data <- dataInput()
    price <- Cl(data)  # Pobranie cen zamknięcia
    # Obliczanie średnich ruchomych
    movAvg1 <- SMA(price, n = 30)  # Średnia ruchoma 30-dniowa
    movAvg2 <- SMA(price, n = 60)  # Średnia ruchoma 60-dniowa
    
    # Przygotowanie danych do wykresu
    df <- data.frame(Date = index(price), Price = as.numeric(price), MA30 = as.numeric(movAvg1), MA60 = as.numeric(movAvg2))
    # Tworzenie wykresu
    plot_ly(df, x = ~Date, y = ~Price, type = 'scatter', mode = 'lines', name = 'Cena') %>%
      add_trace(y = ~MA30, name = 'Średnia ruchoma 30 dni', mode = 'lines') %>%
      add_trace(y = ~MA60, name = 'Średnia ruchoma 60 dni', mode = 'lines') %>%
      layout(title = "Ceny i średnie ruchome", xaxis = list(title = "Data"), yaxis = list(title = "Cena"))
  })
  
  # Generowanie interaktywnego wykresu stóp zwrotu
  output$returnPlot <- renderPlotly({
    data <- dataInput()
    returns <- dailyReturn(Cl(data), type = 'log')  # Obliczanie logarytmicznych stóp zwrotu
    # Obliczanie średnich ruchomych dla stóp zwrotu
    movAvg1 <- SMA(returns, n = 30)  # Średnia ruchoma 30-dniowa
    movAvg2 <- SMA(returns, n = 60)  # Średnia ruchoma 60-dniowa
    
    # Przygotowanie danych do wykresu
    df <- data.frame(Date = index(returns), Return = as.numeric(returns), MA30 = as.numeric(movAvg1), MA60 = as.numeric(movAvg2))
    # Tworzenie wykresu
    plot_ly(df, x = ~Date, y = ~Return, type = 'scatter', mode = 'lines', name = 'Stopa zwrotu') %>%
      add_trace(y = ~MA30, name = 'Średnia ruchoma 30 dni', mode = 'lines') %>%
      add_trace(y = ~MA60, name = 'Średnia ruchoma 60 dni', mode = 'lines') %>%
      layout(title = "Logarytmiczne stopy zwrotu i ich średnie ruchome", xaxis = list(title = "Data"), yaxis = list(title = "Stopa zwrotu"))
  })
  
  # Obliczanie i wyświetlanie podstawowych statystyk dla wybranej spółki
  output$statsOutput <- renderPrint({
    data <- dataInput()
    price <- Cl(data)  # Pobranie cen zamknięcia
    
    # Konwersja danych do formatu data.frame
    df <- data.frame(Price = as.numeric(price))
    
    # Obliczanie statystyk
    stats <- df %>%
      summarise(Średnia = mean(Price, na.rm = TRUE),
                Mediana = median(Price, na.rm = TRUE),
                Odch.Std = sd(Price, na.rm = TRUE),
                Min = min(Price, na.rm = TRUE),
                Max = max(Price, na.rm = TRUE))
    stats
  })
}

# Uruchomienie aplikacji
shinyApp(ui = ui, server = server)
