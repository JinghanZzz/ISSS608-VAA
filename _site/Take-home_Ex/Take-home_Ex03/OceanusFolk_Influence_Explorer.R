# 🌊 Oceanus Folk Influence Explorer
# Shiny App

library(shiny)
library(tidyverse)
library(jsonlite)
library(tidygraph)
library(ggraph)
library(plotly)
library(DT)
library(scales)
library(forcats)
library(visNetwork)
library(SmartEDA)
library(patchwork)

# ---- Data Preparation ----
# 1. 读取数据
kg <- fromJSON("data/MC1_graph.json")

# 2. year-like字段转为整数
gk_nodes <- as_tibble(kg$nodes) %>%
  mutate(
    release_date   = as.integer(release_date),
    written_date   = as.integer(written_date),
    notoriety_date = as.integer(notoriety_date)
  )

# 3. 提取nodes和edges
gk_edges <- as_tibble(kg$links)

# 4. id映射（为from/to做准备）
id_map <- tibble(id = gk_nodes$id, index = seq_len(nrow(gk_nodes)))

# 5. 将source/target映射为from/to
edges_tbl <- gk_edges %>%
  left_join(id_map, by = c("source" = "id")) %>%
  rename(from = index) %>%
  left_join(id_map, by = c("target" = "id")) %>%
  rename(to = index)

# 6. 过滤无效边
edges_tbl <- edges_tbl %>% filter(!is.na(from), !is.na(to))

# 7. nodes_tbl
nodes_tbl <- gk_nodes

# 8. Edge Type字段全小写
edges_tbl <- edges_tbl %>% mutate(`Edge Type` = tolower(`Edge Type`))

# 9. influence_types全小写
influence_types <- c("instyleof", "coverof", "interpolatesfrom", "directlysamples", "lyricalreferenceto")

# All genres
all_genres <- nodes_tbl %>% filter(`Node Type` %in% c("Song", "Album")) %>% distinct(genre) %>% arrange(genre) %>% pull(genre)

# All years
all_years <- nodes_tbl %>% filter(!is.na(release_date)) %>% pull(release_date)
min_year <- min(all_years, na.rm=TRUE)
max_year <- max(all_years, na.rm=TRUE)

# ---- UI ----
ui <- fluidPage(
  titlePanel("\U1F30A Oceanus Folk Influence Explorer"),
  tabsetPanel(
    tabPanel("Influence Timeline",
      sidebarLayout(
        sidebarPanel(
          sliderInput("year_range", "Select Year Range:",
                      min = 1990, max = 2040,
                      value = c(1990, 2040), sep = "", step = 1),
          numericInput("bin_size", "Heatmap Bin Size (years):", value = 5, min = 1, max = 20)
        ),
        mainPanel(
          plotlyOutput("timeline_plot")
        )
      )
    ),
    tabPanel("Genre Impact Analysis",
      sidebarLayout(
        sidebarPanel(
          h4("Genre Impact by Oceanus Folk"),
          p("This panel shows which genres are most influenced by Oceanus Folk."),
          selectInput("influence_type_genre", "Influence Type:", 
                     choices = influence_types, selected = influence_types, multiple = TRUE),
          sliderInput("min_influence_count", "Minimum Influence Count:", 
                     min = 1, max = 20, value = 1, step = 1),
          hr(),
          h5("Network Legend:"),
          p("• 🟠 Orange node: Oceanus Folk (source)"),
          p("• 🟢 Green nodes: Genres influenced by Oceanus Folk"),
          p("• Node size reflects influence count"),
          p("• 🟠 Orange edges: Influence connections")
        ),
        mainPanel(
          visNetworkOutput("genre_impact_network", height = "700px"),
          hr(),
          fluidRow(
            column(6, plotlyOutput("genre_influence_bar")),
            column(6, DT::dataTableOutput("genre_influence_table"))
          )
        )
      )
    ),
    # tabPanel("Genres Influencing Oceanus Folk",
    #   sidebarLayout(
    #     sidebarPanel(
    #       numericInput("top_n_genre_in", "Show Top N Genres:", value = 10, min = 1, max = 30)
    #     ),
    #     mainPanel(
    #       visNetworkOutput("genre_influence_on_of_network", height = "700px"),
    #       plotlyOutput("genre_influence_on_of_bar"),
    #       DT::dataTableOutput("genre_influence_on_of_table")
    #     )
    #   )
    # ),
  )
)

# ---- Server ----
server <- function(input, output, session) {
  # --- Influence Timeline ---
  output$timeline_plot <- renderPlotly({
    df <- nodes_tbl %>%
      filter(genre == "Oceanus Folk", `Node Type` %in% c("Song", "Album"),
             !is.na(release_date),
             between(release_date, input$year_range[1], input$year_range[2])) %>%
      count(release_date, `Node Type`, name = "count") %>%
      pivot_wider(names_from = `Node Type`, values_from = count, values_fill = 0) %>%
      mutate(total = Song + Album,
             year_bin = floor(release_date / input$bin_size) * input$bin_size)

    heatmap_df <- df %>%
      group_by(year_bin) %>%
      summarise(bin_total = sum(total), Album = sum(Album), Song = sum(Song), .groups = "drop") %>%
      arrange(year_bin) %>%
      mutate(change = bin_total - lag(bin_total),
             hover_text = paste0("Period: ", year_bin, "–", year_bin + input$bin_size - 1,
                                 "<br>Total: ", bin_total,
                                 "<br>Change: ", ifelse(is.na(change), "NA", ifelse(change > 0, paste0("+", change), change))))

    p <- ggplot() +
      geom_tile(data = heatmap_df,
                aes(x = year_bin + input$bin_size/2, y = -5, fill = bin_total, text = hover_text),
                width = input$bin_size, height = 3) +
      geom_line(data = df, aes(x = release_date, y = total, group = 1), color = "#2f4b7c", linewidth = 1) +
      geom_point(data = df, aes(x = release_date, y = total, text = paste0("Year: ", release_date, "<br>Total: ", total, "<br>Album: ", Album, "<br>Song: ", Song)), color = "#2f4b7c", size = 2) +
      scale_fill_gradient(low = "#a8dadc", high = "#2f4b7c") +
      labs(title = "Oceanus Folk Releases by Year with Heatmap", x = "Release Year", y = "Total Count", fill = paste(input$bin_size, "Year Total")) +
      theme_minimal()
    ggplotly(p, tooltip = "text")
  })

  # --- Panel2: Genre Impact Analysis ---
  
  # Calculate genre influence statistics (simplified)
  genre_influence_data <- reactive({
    # Find Oceanus Folk works
    of_works <- nodes_tbl %>%
      filter(genre == "Oceanus Folk", `Node Type` %in% c("Song", "Album"))
    
    # Find influenced works
    influence_edges <- edges_tbl %>%
      filter(from %in% of_works$id, `Edge Type` %in% input$influence_type_genre)
    
    # Get influenced works and their genres
    influenced_works <- influence_edges %>%
      left_join(nodes_tbl %>% select(id, name, genre, `Node Type`), by = c("to" = "id")) %>%
      filter(!is.na(genre), genre != "Oceanus Folk")
    
    # Count influence by genre
    genre_stats <- influenced_works %>%
      count(genre, name = "influence_count") %>%
      arrange(desc(influence_count)) %>%
      filter(influence_count >= input$min_influence_count)
    
    list(
      influenced_works = influenced_works,
      genre_stats = genre_stats,
      influence_edges = influence_edges
    )
  })

  # --- Genre Impact Network (Panel 2: Genre Influence Analysis) ---
  output$genre_impact_network <- renderVisNetwork({
    data <- genre_influence_data()
    
    # Simple approach: just show genres with their influence counts
    nodes <- data$genre_stats %>%
      mutate(
        id = as.character(row_number()),
        label = genre,
        title = paste0("Genre: ", genre, "<br>Influence Count: ", influence_count),
        color = "#2ecc71",
        size = 10 + influence_count * 3,  # Size based on influence count
        group = "Genre"
      )
    
    # Add Oceanus Folk node
    of_node <- tibble(
      id = "0",
      label = "Oceanus Folk",
      title = "Oceanus Folk (Source)",
      color = "#e67e22",
      size = 50,
      group = "Oceanus Folk"
    )
    
    all_nodes <- bind_rows(of_node, nodes)
    
    # Create edges from Oceanus Folk to each genre
    edges <- data$genre_stats %>%
      mutate(
        from = "0",
        to = as.character(row_number()),
        color = "#e67e22",
        arrows = "to",
        title = paste0("Influenced by Oceanus Folk")
      )
    
    # Create visNetwork
    visNetwork(nodes = all_nodes, edges = edges) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visPhysics(stabilization = FALSE) %>%
      visLayout(randomSeed = 123) %>%
      visInteraction(navigationButtons = TRUE, keyboard = TRUE)
  })

  # --- Genre Influence Bar Chart (Panel 2) ---
  output$genre_influence_bar <- renderPlotly({
    data <- genre_influence_data()
    
    p <- ggplot(data$genre_stats, aes(x = reorder(genre, influence_count), y = influence_count)) +
      geom_col(fill = "#2ecc71", alpha = 0.8) +
      coord_flip() +
      labs(
        title = "Genres Most Influenced by Oceanus Folk",
        x = "Genre",
        y = "Number of Influenced Works"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12)
      )
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(
        margin = list(l = 50, r = 50, t = 50, b = 50)
      )
  })
  
  # --- Genre Influence Table (Panel 2) ---
  output$genre_influence_table <- renderDT({
    data <- genre_influence_data()
    
    data$genre_stats %>%
      mutate(
        `Influence Count` = influence_count
      ) %>%
      select(Genre = genre, `Influence Count`) %>%
      datatable(
        options = list(
          pageLength = 10,
          order = list(list(1, 'desc'))
        ),
        rownames = FALSE
      )
  })
  
  # --- Node Selection Observer ---
  observeEvent(input$selected_node, {
    if (!is.null(input$selected_node)) {
      # Get node info
      node_info <- nodes_tbl %>%
        filter(id == as.numeric(input$selected_node)) %>%
        slice(1)
      
      if (nrow(node_info) > 0) {
        if (node_info$`Node Type`[1] %in% c("Song", "Album")) {
          # If it's a work, show its details
          showModal(modalDialog(
            title = paste("Work Details:", node_info$name[1]),
            paste0(
              "Name: ", node_info$name[1], "<br>",
              "Type: ", node_info$`Node Type`[1], "<br>",
              "Genre: ", node_info$genre[1]
            ),
            easyClose = TRUE
          ))
        } else if (!is.na(node_info$genre[1])) {
          # If it's a genre, filter to show only that genre
          showModal(modalDialog(
            title = paste("Genre Details:", node_info$genre[1]),
            paste0(
              "Genre: ", node_info$genre[1], "<br>",
              "This genre has been influenced by Oceanus Folk works."
            ),
            easyClose = TRUE
          ))
        }
      }
    }
  })
}

# ---- Run App ----
shinyApp(ui, server) 