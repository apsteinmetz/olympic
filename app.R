# Olympic country  ranker on user selected weights
library(shiny)
library(bslib)
library(tidyverse)
library(gt)
library(ggrepel)

# use Paris2024-Variable font if on a windows machine
if (Sys.info()[['sysname']] == "Windows") {
    local_base_font = bslib::font_face(family = "Paris2024-Variable",
                                       src = "url('/Paris2024-Variable.ttf') format('truetype')")
} else { # shinyapps.io uses Ubuntu and can't use local font file 
    local_base_font = bslib::font_google("Turret Road")
}

# vector of country names from countries to annotate
countries_to_annotate <- c("United States", "China", "Russia", "France", "Germany", "United Kingdom", "Japan",
                   "Australia", "Canada", "Brazil", "India", "Mexico", "Argentina", "Dominica",
                   "New Zealand","Georgia","Jamaica")

medal_counts <- read_csv(here::here("data/medal_counts_iso.csv")) |> 
    select(-Total,-country_name) |>
    pivot_longer(cols = -c(country_code), names_to = "Medal", values_to = "Count") |> 
    mutate(Medal = str_remove(Medal, " Medal")) |> 
    mutate(Medal = as_factor(Medal)) |> 
    mutate(country_code = as_factor(country_code))

# medal_counts

macro_data <- read_csv(here::here("data/macro_data.csv")) |> 
    mutate(country_code = as.factor(country_code))

medal_weights <- tibble(
    Medal = as_factor(c("Gold", "Silver", "Bronze")),
    Weight = c(3, 2, 1)
)

medals_data <- left_join(medal_counts, medal_weights, by = "Medal") 

# medals_data

change_weights <- function(dt=medals_data,g = 1,s = 1, b = 1){
    medal_weights <- tibble(
        Medal = c("Gold", "Silver", "Bronze"),
        Weight = c(g, s, b)
    )
    return(left_join(select(dt,-Weight), medal_weights, by = "Medal"))
}
country_rollup <- function(df,
                           main_country_code,
                           other_country_codes,
                           main_country_name) {
    df |>
        mutate(country_code = str_replace(
            country_code,
            paste(other_country_codes, collapse = "|"),
            main_country_code
        )) |>
        mutate(
            country_name = if_else(
                country_code == main_country_code,
                main_country_name,
                country_name
            )
        ) |>
        summarise(
            .by = c(country_code, country_name),
            Score_Wgt = sum(Score_Wgt),
            GDP = sum(GDP),
            population = sum(population)
        ) |>
        left_join(select(macro_data, country_code, flag_url)) |>
        ungroup()
}


adjust_china <- function(df,rollup_flag){
    if(rollup_flag){
        return(country_rollup(df,"CHN",c("TWN","HKG"),"China (incl. HK and TWN)"))
    } else {
        return(df)
    }
}


sort_countries <- function(dt,sort_by = c("medal_wgt","pop_wgt","gdp_wgt","all_wgt")){
        # swtich based on sort_by
        dt <- case_when(
            sort_by == "medal_wgt" ~ arrange(dt, desc(Score_Wgt)),
            sort_by == "pop_wgt" ~ arrange(dt,desc(Score_per_MM_pop)),
            sort_by == "gdp_wgt" ~ arrange(dt,desc(Score_per_GDP_USD_BN)),
            sort_by == "all_wgt" ~ arrange(dt,desc(Score_per_capita_GDP))
        )
    return(dt)
}

# predefined UI elements -------------------------------------------------------
sidebar_inputs <-   sidebar(
    h3("Select Weights for Medals\n",
        "and Sorting Criterion"),
    p("Weights of 1 will show absolute number of medals."),
    sliderInput(
        "gw",
        "Gold Weight:",
        min = 0,
        max = 10,
        value = 3
    ),
    sliderInput(
        "sw",
        "Silver Weight:",
        min = 0,
        max = 10,
        value = 2
    ),
    sliderInput(
        "bw",
        "Bronze Weight:",
        min = 0,
        max = 10,
        value = 1
    ),
    # add radio button
    radioButtons(
        "sorting",
        "Sort On:",
        choices = c("medal_wgt", "pop_wgt", "gdp_wgt", "all_wgt")
        ),
        # add checkbox
        checkboxInput("big_china", "Rollup HK and Taiwan into China?", value = FALSE)
    
)

banner <- div(img(
    src = "banner.png",
    height = 80,
    # width = 20,
    style = "margin:1px 1px"),
    "My Country is The Best!")

about_page <- page_fillable(
    title = "About",
    card(
        card_header(h2(strong("Paris 2024 Olympic Medal Country Ranker"))),
        p("Chose your own relative weights for Gold, Silver and Bronze",
          "Also weight by country, population and/or GDP.",
          "Heatmapped column reflects weighting choice.",
          "Sort the results on any column."),
        h2(strong("Data sources:")),
        a(href = "https://www.kaggle.com/datasets/piterfm/paris-2024-olympic-summer-games",
          "Paris 2024 Olympic Results from Kaggle"),
        a(href = "https://data.worldbank.org/indicator/NY.GDP.PCAP.CD","GDP and Population from World Bank Open Data"),
        a(href = "https://flagpedia.net/download/api","Country flags from flagpedia.net"),
        a(href = "https://wisabo.com/item/1/paris-2024-variable/","Official Paris 2024 font from Wisabo.com"),
        p(strong("Created by Art Steinmetz using R and Shiny from"), 
          a(href = "https://www.posit.co", "Posit.co. ")),
          p(a(href = "https://github.com/apsteinmetz/olympic.git","Code on Github. "),
          "See more of my work at",
          a(href = "https://outsiderdata.netlify.app/", "outsiderdata.net.")
          )
        

        
        
        
    )
)

# UI ---------------------------------------------------------------------------
ui <- page_navbar(
    theme = bs_theme(
        version = 5,
        bg = "#e8ecf7", # bg color in logo
        fg = "#000",
        base_font = font_face(family = "Paris2024-Variable",
                              src = "url('/Paris2024-Variable.ttf') format('truetype')")
    ),
    # Application title
    title = banner,
    nav_panel(
        "Medal Scoreboard",
        page_sidebar(
            # Sidebar with a slider input for number of bins
            sidebar = sidebar_inputs,
            layout_columns(col_widths = c(7, 5),
                           card(gt_output("table")),
                           card(card(plotOutput("gdpscatter")),
                                card(plotOutput("popscatter"))
                                )
                           )
        )
    ),
    nav_panel("About", about_page)
)

# Server -----------------------------------------------------------------------
server <- function(input, output) {
    score_countries <- reactive({
            medals_data |> 
            change_weights(g = input$gw, s = input$sw, b = input$bw) |> 
            mutate(Score_Wgt = Count * Weight) |>
            summarize(.by = c(country_code), across(starts_with("Score"),\(x) sum(x, na.rm = TRUE))) |> 
            left_join(macro_data, by = "country_code") |> 
            adjust_china(input$big_china) |>
            select(-country_code) |> 
            na.omit() |>
            # score per million people
            mutate(Score_per_MM_pop = (Score_Wgt/population)) |>
            # score per $billion GDP
            mutate(Score_per_GDP_USD_BN = (Score_Wgt/GDP)) |>
            mutate(Score_per_capita_GDP = (Score_Wgt/GDP/population)) |> 
            select(flag_url,country_name,starts_with("Score"),everything()) |>
            sort_countries(sort_by = input$sorting)
    })
    col_to_color <- reactive({
        switch(input$sorting,
            "medal_wgt" = "Score_Wgt",
            "pop_wgt" = "Score_per_MM_pop",
            "gdp_wgt" = "Score_per_GDP_USD_BN",
            "all_wgt" = "Score_per_capita_GDP"
        )
    })
    
    output$table <- render_gt(
        score_countries() |>
            gt() |>
            opt_interactive(use_pagination = FALSE,use_resizers = TRUE) |> 
            fmt_number(columns = c(Score_Wgt), decimals = 0) |>
            fmt_number(columns = c(population,GDP), decimals = 1) |>
            fmt_number(
                columns = c(Score_per_MM_pop, Score_per_GDP_USD_BN, Score_per_capita_GDP),
                decimals = 2
            ) |>
            cols_width(is.numeric ~ px(100)) |>
            cols_width(country_name ~ px(150)) |>
            cols_width(flag_url ~ px(100)) |>
            data_color(
                columns = c(col_to_color()),
                method = "numeric",
                palette = "viridis") |> 
            tab_header(title = "Medal Scoreboard") |> 
            fmt_image(
                columns = "flag_url",
                width = px(40)
            ) |>
#            text_transform(
#                locations = cells_body(columns = flag_url),
#                fn = function(x) { web_image(url = x, height = 30) }
#            ) |>
            cols_label(
                flag_url = "Flag",
                Score_Wgt = "Medal Score",
                Score_per_MM_pop = "Pop Score",
                Score_per_GDP_USD_BN = "GDP Score",
                Score_per_capita_GDP = "Per Capita GDP Score",
                country_name = "Country",
                population = "Pop.(MM)",
                GDP = "GDP ($ BN)")

    )
    output$gdpscatter <- renderPlot({
        ggplot(score_countries(), aes(x = GDP, y = Score_Wgt)) +
            geom_point() +
            geom_smooth() +
            # label points from countries_to_annotate
            geom_text_repel(
                data = score_countries() |> 
                    filter(country_name %in% countries_to_annotate),
                aes(label = country_name)
            ) +
            scale_x_log10(labels = scales::dollar) +
            labs(title = "GDP vs Medal Score", x = "GDP (Log USD Billions)", y = "Medal Score")
    })
    output$popscatter <- renderPlot({
        ggplot(score_countries(), aes(x = population, y = Score_Wgt)) +
            geom_point() +
            geom_smooth() +
            geom_text_repel(
                data = score_countries() |> 
                    filter(country_name %in% countries_to_annotate),
                aes(label = country_name)
            ) +
            scale_x_log10() +
            labs(title = "Population vs Medal Score", x = "Population (Log Millions)", y = "Medal Score")
    })
}

# run app ----------------------------------------------------------------------
shinyApp(ui = ui, server = server)
