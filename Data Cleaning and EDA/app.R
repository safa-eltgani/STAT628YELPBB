library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(dplyr)
library(DT)
library(reshape2)
library(shinyjs)
library(htmlwidgets)

brunch_data <- read.csv("https://raw.githubusercontent.com/safa-eltgani/STAT628YELPBB/main/data.csv")

food_data <- read.table(text = "index,Food,NEGATIVE,POSITIVE
0, Ham,3.85,4.03
1,Acai Bowl,4.57,4.48
2,Bacon,3.88,3.98
3,Bagels,3.63,3.46
4,Breakfast Burrito,3.89,4.08
5,Breakfast Sandwich,3.94,4.04
6,Breakfast Tacos,3.6,4.25
7,Cinnamon Rolls,0.0,4.25
8,Coffee Cake,3.88,4.22
9,Corned Beef Hash,4.07,3.88
10,Crepes,4.5,4.38
11,Croissant,4.17,4.31
12,Danish,3.9,3.6
13,Eggs Benedict,4.06,4.16
14,French Toast,3.98,4.06
15,Fried Chicken,4.03,4.08
16,Fries,3.5,3.87
17,Fruit Salad,4.06,4.21
18,Granola,4.03,4.07
19,Grits,4.14,4.0
20,Hash Browns,3.75,4.0
21,Huevos Rancheros,4.16,4.17
22,Muffins,4.2,4.0
23,Oatmeal,3.84,3.92
24,Omelette,3.98,4.06
25,Pancakes,3.86,3.9
26,Porridge,3.96,4.02
27,Sausage,3.92,4.0
28,Scones,4.25,4.29
29,Toast,3.99,4.15
30,Turkey Bacon,3.63,3.95
31,Waffles,3.89,4.0
32,Yogurt Parfait,3.75,3.67", header = TRUE, sep = ",")

# UI Definition
ui <- dashboardPage(
    dashboardHeader(title = "Brunch Restaurants Analysis"),
    dashboardSidebar(disable = TRUE), # Disable the sidebar
    dashboardBody(
        useShinyjs(),
        tags$head(
            tags$style(HTML("
        .bubble {
            display: inline-block;
            margin: 5px;
            padding: 10px;
            background-color: #f0f0f0;
            border-radius: 50%;
            text-align: center;
            color: #333;
            font-size: 16px;
            transition: background-color 0.3s;
        }
        .bubble:hover {
            background-color: #e0e0e0;
        }
    "))
        ),
        tabBox(
            # First tab: Geographic Location Analysis
            tabPanel("Geographic Analysis", 
                     fluidRow(
                         # First column: Dropdown menus (20% of area)
                         column(2,
                                selectInput("clusterFilter", "Select Cluster:", choices = c('None' = '', unique(brunch_data$cluster))),
                                selectInput("businessFilter", "Highlight Business:", choices = c('None' = '', unique(brunch_data$name)))
                         ),
                         
                         # Second column: Map (40% of area)
                         column(4, leafletOutput("map", height = 500)),
                         
                         # Third column: Bar chart (40% of area)
                         column(4, plotOutput("ratingHistogram"))
                     )
            ),
            # Second tab: Operating Hours Analysis
            tabPanel("Operating Hours", 
                     fluidRow(
                         column(3, 
                                checkboxGroupInput("selectedDays", "Select Days:", 
                                                   choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                                                   selected = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
                                ),
                                selectInput("startHour", "Start Hour:", choices = 0:23, selected = 0),
                                selectInput("endHour", "End Hour:", choices = 0:23, selected = 23)
                         ),
                         column(9, 
                                plotOutput("hoursPlot"),
                                verbatimTextOutput("averageRating")
                         )
                     )
            ),
            # Third tab: Sentiment Analysis
            tabPanel("Menu Impact Analysis",
                     fluidRow(
                         column(6,
                                selectInput("foodSelection", "Select Food Items:", choices = food_data$Food, multiple = TRUE)
                         ),
                         column(6,
                                verbatimTextOutput("dynamicAverage"),
                                div(id = "bubbles")  # Container for bubbles
                         )
                     )
            ),
            # Fourth tab: Summary and Suggestions
            tabPanel("Summary and Suggestions",
                     div(
                         style = "font-family: Arial, sans-serif; padding: 20px;",
                         h3("Summary and Recommendations:"),
                         p("Based on our comprehensive analysis of breakfast and brunch restaurants, we have identified several key insights and recommendations to enhance customer satisfaction and ratings:"),
                         h4("Location:"),
                         p("Restaurants located ", tags$b("downtown"), " typically require less travel for customers and consistently receive ", tags$b("higher reviews"), "."),
                         h4("Optimal Food Choices:"),
                         p("Incorporating specific food items into the menu can significantly impact customer ratings. Notable items that lead to ", tags$b("higher ratings"), " include:"),
                         tags$ul(
                             tags$li(tags$b("Breakfast Tacos")),
                             tags$li(tags$b("Fries")),
                             tags$li(tags$b("Coffee Cake")),
                             tags$li(tags$b("Turkey Bacon")),
                             tags$li(tags$b("Hash Browns"))
                         ),
                         h4("Operating Hours:"),
                         p("Our analysis suggests that operating between ", tags$b("7am and 2pm"), " is most effective in attracting customers and enhancing their dining experience.")
                     )
            )
            
            ,
            
            tabPanel("Contact Us",
                     div(
                         style = "font-family: Arial, sans-serif; padding: 20px;",
                         h2("Welcome to Our Breakfast/Brunch Restaurant Analysis App!"),
                         p("This application was developed as part of the STAT628 class Module 3 project. Our aim is to utilize Yelp reviews data to assist business owners in making informed decisions."),
                         p("We chose breakfast/brunch restaurants as our focus due to the growing popularity of this category. Our analysis is centered on Indianapolis, a city with an abundance of relevant data."),
                         p("The data used in this project was sourced from the Yelp reviews dataset, Yelp businesses dataset, and the U.S. Department of Transportation's Bureau of Transportation Statistics dataset of trips taken in each area."),
                         hr(),
                         h3("Group Members' Contact Information:"),
                         p(tags$b("Safa Eltgani:"), " seltgani@wisc.edu"),
                         p(tags$b("Osama Kheshaifaty:"), " kheshaifaty@wisc.edu"),
                         p(tags$b("Chixu Ni:"), " cni28@wisc.edu"),
                         p("For any inquiries or support, feel free to reach out to us via the email addresses provided.")
                     )
            )
            ,
            # Specify the width to take full width of the container
            width = 12
        )
    )
)

# Server Definition
server <- function(input, output, session) {
    

    # Load the CSV data
    brunch_data <- read.csv("https://raw.githubusercontent.com/safa-eltgani/STAT628YELPBB/main/data.csv")
    colorPalette <- colorRampPalette(c("white", "darkblue"))
    
    # Populate the dropdown for clusters and business IDs initially
    observe({
        updateSelectInput(session, "clusterFilter", choices = c('None' = '', unique(brunch_data$cluster)))
        updateSelectInput(session, "businessFilter", choices = c('None' = '', unique(brunch_data$name)))
    })
    
    # Reactive value to store the selected restaurant's name
    selectedRestaurant <- reactiveVal()
    
    # Observer for map marker clicks
    observeEvent(input$map_marker_click, {
        clickedMarker <- input$map_marker_click
        if (!is.null(clickedMarker)) {
            selectedRestaurant(clickedMarker$name)
        }
    })
    
    # Apply filters based on dropdown selections
    filteredData <- reactive({
        filtered <- brunch_data
        if (!is.null(input$clusterFilter) && input$clusterFilter != '') {
            filtered <- filtered[filtered$cluster == input$clusterFilter, ]
        }
        filtered
    })
    
    
    # Logic for map rendering with interactivity and color encoding
    output$map <- renderLeaflet({
        map <- leaflet(brunch_data) %>%
            addTiles()
        
        # Add markers for all restaurants
        map <- map %>%
            addCircleMarkers(lng = ~longitude, lat = ~latitude,
                             color = ~colorPalette(length(unique(brunch_data$stars_x)))[as.integer(cut(stars_x, breaks = length(unique(stars_x))))],
                             fill = TRUE, fillOpacity = 1.0, radius = 3,
                             popup = ~paste("Name:", name, "<br>Cluster:", cluster), group = "restaurants")
        
        # Add a label for the highlighted restaurant
        if (!is.null(selectedRestaurant())) {
            selectedBusinessID <- selectedRestaurant()
            highlightedRestaurant <- brunch_data[brunch_data$name == selectedBusinessID, ]
            if (nrow(highlightedRestaurant) > 0) {
                map <- map %>%
                    addPopups(lng = highlightedRestaurant$longitude, 
                              lat = highlightedRestaurant$latitude, 
                              popup = "Selected Business!", 
                              options = popupOptions(closeButton = FALSE))
            }
        }
        
        # Add heat layer based on 'Number of Trips'
        map <- map %>%
            addHeatmap(lng = ~longitude, lat = ~latitude, intensity = ~Number.of.Trips, radius = 20, blur = 30)

        map
    })
    
    
    # Render the histogram
    output$ratingHistogram <- renderPlot({
        p <- ggplot(filteredData(), aes(x = stars_x, fill = as.factor(cluster))) +
            geom_histogram(bins = 10, position = "dodge") +
            labs(title = "Ratings Histogram by Cluster", x = "Rating", y = "Frequency") +
            scale_fill_discrete(name = "Cluster")
        
        # Add Gaussian fit
        p <- p + stat_function(fun = function(x) 10 * dnorm(x, 
                                                           mean = mean(filteredData()$stars_x, na.rm = TRUE), 
                                                           sd = sd(filteredData()$stars_x, na.rm = TRUE)), 
                               color = "blue")
        
        # Highlighting logic for the histogram
        if (!is.null(input$businessFilter) && input$businessFilter %in% filteredData()$name) {
            highlightBusiness <- input$businessFilter
            selectedRating <- filteredData()$stars_x[filteredData()$name == highlightBusiness]
            p <- p + geom_vline(xintercept = selectedRating, color = "red", linetype = "dashed", yend = 20) +
                geom_text(aes(x = selectedRating, label = "You are here!", y = 20), vjust = -2)
        }
        
        p
    })
    
    
    # Replace the placeholder for operating hours plot with heatmap
    output$hoursPlot <- renderPlot({
        # Define your array
        ratings <- matrix(c(
            2.63269231, 2.68181818, 2.68231047, 2.75328947, 2.88901869, 2.9032634, 2.63953488,
            2.59810127, 2.60670732, 2.61976048, 2.69444444, 2.80976431, 2.83110368, 2.65142857,
            2.72807018, 2.73636364, 2.71929825, 2.69852941, 2.65079365, 2.69354839, 2.75735294,
            2.76315789, 2.79411765, 2.79411765, 2.83333333, 2.65957447, 2.74444444, 2.84848485,
            3.25806452, 3.24193548, 3.24193548, 3.31818182, 3.11842105, 3.17142857, 2.92857143,
            3.27222222, 3.3062201, 3.31904762, 3.33253589, 3.30188679, 3.290625, 3.24,
            3.38643371, 3.41163793, 3.42720971, 3.4378801, 3.44606543, 3.40535918, 3.31336088,
            3.46825765, 3.53797468, 3.5468696, 3.55707368, 3.56477591, 3.49509002, 3.42382022,
            3.47404844, 3.567, 3.58220503, 3.59156293, 3.59906181, 3.57562902, 3.51430843,
            3.47537743, 3.56972698, 3.58896338, 3.5985843, 3.60555983, 3.59427521, 3.54428251,
            3.4441675, 3.54718851, 3.56823079, 3.57955088, 3.58523267, 3.58282136, 3.53589048,
            3.45300416, 3.56521739, 3.58543184, 3.59600249, 3.60201356, 3.59448902, 3.54798658,
            3.45294466, 3.56506776, 3.58649876, 3.59691358, 3.60472904, 3.59636667, 3.54914766,
            3.44619778, 3.55729281, 3.57843343, 3.58927075, 3.59675419, 3.58693799, 3.53801634,
            3.36234458, 3.47493298, 3.49716787, 3.50796506, 3.51798835, 3.50945596, 3.44043219,
            3.26586801, 3.38785942, 3.4132622, 3.42950471, 3.44159928, 3.43549877, 3.33839633,
            3.23297966, 3.36449684, 3.39597104, 3.416122, 3.42408377, 3.41145501, 3.29571106,
            3.20878871, 3.35656566, 3.3977736, 3.42299383, 3.42981121, 3.41198858, 3.28752887,
            3.18004808, 3.32727273, 3.37458746, 3.40081301, 3.4111926, 3.39312977, 3.25782202,
            3.13923077, 3.29293313, 3.34217038, 3.37237027, 3.38625512, 3.37078261, 3.23010381,
            3.08634176, 3.23543124, 3.27504394, 3.30971747, 3.33530371, 3.33347023, 3.17878459,
            2.95392954, 3.07466568, 3.10646521, 3.14891381, 3.24539701, 3.24642244, 3.00376884,
            2.80305466, 2.87956204, 2.89539749, 2.94775132, 3.11007828, 3.11363636, 2.85681115,
            2.67009132, 2.75103734, 2.74032587, 2.80501931, 2.97270115, 2.9713467, 2.71382289
        ), ncol = 7, byrow = TRUE)
        
        # Define the column names (weekdays) and row names (hours)
        colnames(ratings) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
        rownames(ratings) <- format(seq(ISOdate(2000,1,1,0,0,0), by="hour", length.out=24), "%I %p")
        
        # Melt the matrix into a data frame for ggplot
        ratings_melted <- melt(ratings)
        
        # Create the heatmap with adjusted color scale
        ggplot(ratings_melted, aes(x=Var2, y=Var1, fill=value)) +
            geom_tile() +
            scale_fill_gradient(low = "blue", high = "yellow", limits=c(min(ratings_melted$value), max(ratings_melted$value))) +
            labs(x = "Day of the Week", y = "Hour of the Day", fill = "Average Rating") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$averageRating <- renderText({
        
        ratings <- matrix(c(
            2.63269231, 2.68181818, 2.68231047, 2.75328947, 2.88901869, 2.9032634, 2.63953488,
            2.59810127, 2.60670732, 2.61976048, 2.69444444, 2.80976431, 2.83110368, 2.65142857,
            2.72807018, 2.73636364, 2.71929825, 2.69852941, 2.65079365, 2.69354839, 2.75735294,
            2.76315789, 2.79411765, 2.79411765, 2.83333333, 2.65957447, 2.74444444, 2.84848485,
            3.25806452, 3.24193548, 3.24193548, 3.31818182, 3.11842105, 3.17142857, 2.92857143,
            3.27222222, 3.3062201, 3.31904762, 3.33253589, 3.30188679, 3.290625, 3.24,
            3.38643371, 3.41163793, 3.42720971, 3.4378801, 3.44606543, 3.40535918, 3.31336088,
            3.46825765, 3.53797468, 3.5468696, 3.55707368, 3.56477591, 3.49509002, 3.42382022,
            3.47404844, 3.567, 3.58220503, 3.59156293, 3.59906181, 3.57562902, 3.51430843,
            3.47537743, 3.56972698, 3.58896338, 3.5985843, 3.60555983, 3.59427521, 3.54428251,
            3.4441675, 3.54718851, 3.56823079, 3.57955088, 3.58523267, 3.58282136, 3.53589048,
            3.45300416, 3.56521739, 3.58543184, 3.59600249, 3.60201356, 3.59448902, 3.54798658,
            3.45294466, 3.56506776, 3.58649876, 3.59691358, 3.60472904, 3.59636667, 3.54914766,
            3.44619778, 3.55729281, 3.57843343, 3.58927075, 3.59675419, 3.58693799, 3.53801634,
            3.36234458, 3.47493298, 3.49716787, 3.50796506, 3.51798835, 3.50945596, 3.44043219,
            3.26586801, 3.38785942, 3.4132622, 3.42950471, 3.44159928, 3.43549877, 3.33839633,
            3.23297966, 3.36449684, 3.39597104, 3.416122, 3.42408377, 3.41145501, 3.29571106,
            3.20878871, 3.35656566, 3.3977736, 3.42299383, 3.42981121, 3.41198858, 3.28752887,
            3.18004808, 3.32727273, 3.37458746, 3.40081301, 3.4111926, 3.39312977, 3.25782202,
            3.13923077, 3.29293313, 3.34217038, 3.37237027, 3.38625512, 3.37078261, 3.23010381,
            3.08634176, 3.23543124, 3.27504394, 3.30971747, 3.33530371, 3.33347023, 3.17878459,
            2.95392954, 3.07466568, 3.10646521, 3.14891381, 3.24539701, 3.24642244, 3.00376884,
            2.80305466, 2.87956204, 2.89539749, 2.94775132, 3.11007828, 3.11363636, 2.85681115,
            2.67009132, 2.75103734, 2.74032587, 2.80501931, 2.97270115, 2.9713467, 2.71382289
        ), ncol = 7, byrow = TRUE)
        
        # Define the column names (weekdays) and row names (hours)
        colnames(ratings) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
        rownames(ratings) <- format(seq(ISOdate(2000,1,1,0,0,0), by="hour", length.out=24), "%I %p")
        
        # Melt the matrix into a data frame for ggplot
        ratings_melted <- melt(ratings)
        
        # Convert selectedDays to column indices
        dayNames <- colnames(ratings)
        selectedDaysIndices <- match(input$selectedDays, dayNames)
        
        # Adjust hour indexing
        startHour <- as.numeric(input$startHour) + 1
        endHour <- as.numeric(input$endHour) + 1
        
        # Ensure indices are within bounds
        if (startHour < 1) startHour <- 1
        if (endHour > nrow(ratings)) endHour <- nrow(ratings)
        if (startHour > endHour) return("Invalid hour range selected.")
        
        # Filter the ratings
        filtered_ratings <- ratings[startHour:endHour, selectedDaysIndices, drop = FALSE]
        avg_rating <- mean(filtered_ratings, na.rm = TRUE)
        
        # Benchmark average for comparison
        benchmarkAverage <- 3.2
        
        # Calculate the difference
        difference <- round(avg_rating, 2) - benchmarkAverage
        
        # Calculate the difference and round to one decimal place
        difference <- round(difference, 2)
        avg_rating <- round(avg_rating, 2)
        
        # Generate the message
        comparisonMessage <- ifelse(difference > 0, 
                                    paste("This is", difference, "points higher than the benchmark average."),
                                    ifelse(difference < 0, 
                                           paste("This is", abs(difference), "points lower than the benchmark average."),
                                           "This is equal to the benchmark average."))
        
        # Combine the messages
        if (is.finite(avg_rating)) {
            paste("Average rating for selected period:", round(avg_rating, 2), comparisonMessage)
        } else {
            "No data available for the selected period."
        }
    
    })
    
    
    output$dynamicAverage <- renderText({
        food_data <- read.table(text = "index,Food,NEGATIVE,POSITIVE
0, Ham,3.85,4.03
1,Acai Bowl,4.57,4.48
2,Bacon,3.88,3.98
3,Bagels,3.63,3.46
4,Breakfast Burrito,3.89,4.08
5,Breakfast Sandwich,3.94,4.04
6,Breakfast Tacos,3.6,4.25
7,Cinnamon Rolls,0.0,4.25
8,Coffee Cake,3.88,4.22
9,Corned Beef Hash,4.07,3.88
10,Crepes,4.5,4.38
11,Croissant,4.17,4.31
12,Danish,3.9,3.6
13,Eggs Benedict,4.06,4.16
14,French Toast,3.98,4.06
15,Fried Chicken,4.03,4.08
16,Fries,3.5,3.87
17,Fruit Salad,4.06,4.21
18,Granola,4.03,4.07
19,Grits,4.14,4.0
20,Hash Browns,3.75,4.0
21,Huevos Rancheros,4.16,4.17
22,Muffins,4.2,4.0
23,Oatmeal,3.84,3.92
24,Omelette,3.98,4.06
25,Pancakes,3.86,3.9
26,Porridge,3.96,4.02
27,Sausage,3.92,4.0
28,Scones,4.25,4.29
29,Toast,3.99,4.15
30,Turkey Bacon,3.63,3.95
31,Waffles,3.89,4.0
32,Yogurt Parfait,3.75,3.67", header = TRUE, sep = ",")
        
        # Transforming the data
        food_data$Ratings_Impact <- food_data$POSITIVE - food_data$NEGATIVE
        food_data <- food_data[, c("Food", "Ratings_Impact")]
        
        selectedFoods <- input$foodSelection
        if (length(selectedFoods) == 0) {
            return("Average rating: 3.2")
        } else {
            selectedImpact <- sum(food_data$Ratings_Impact[food_data$Food %in% selectedFoods])
            newAverage <- min(max(3.2 + selectedImpact, 0), 5)
            paste("Dynamic Average Rating:", round(newAverage, 2))
        }
    })
    
    # Observe changes in foodSelection input
    observe({
        selectedFoods <- input$foodSelection
        
        # JavaScript code to manage bubbles
        jsCode <- paste0("$('#bubbles').html('",
                         paste(sapply(selectedFoods, function(food) {
                             paste0("<div class=\"bubble\">", food, "</div>")
                         }), collapse = ""),
                         "');")
        
        # Run the JavaScript code
        runjs(jsCode)
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)
