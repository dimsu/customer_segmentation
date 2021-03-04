# CUSTOMER SEGMENTATION FUNCTIONS

# 1.0 LIBRARIES ----
library(tidyverse)
library(tidyquant)
library(broom)
library(umap)
library(ggrepel)
library(plotly)

bike_orderlines_tbl <- read_rds("../00_data/bike_sales/data_wrangled/bike_orderlines.rds")

# 2.0 HEAT MAP PLOT ----
# - Refer to Chapter 4: Advanced Heat Map
plot_customer_heatmap <- function(interactive = FALSE) {
    
    # DATA MANIPULATION
    pct_sales_by_customer_tbl <- bike_orderlines_tbl %>%
        
        select(bikeshop_name, category_1, category_2, quantity) %>%
        
        group_by(bikeshop_name, category_1, category_2) %>%
        summarise(total_qty = sum(quantity)) %>%
        ungroup() %>%
        
        group_by(bikeshop_name) %>%
        mutate(pct = total_qty / sum(total_qty)) %>%
        ungroup() %>%
        
        mutate(bikeshop_name = as.factor(bikeshop_name) %>% fct_rev()) %>%
        
        mutate(label_text = str_glue("Customer: {bikeshop_name}
                                     Category: {category_1}
                                     Sub-Category: {category_2}
                                     Quantity Purchased: {total_qty}
                                     Percent of Sales: {scales::percent(pct)}"))
    
    # VISUALIZATION
    g <- pct_sales_by_customer_tbl %>%
        ggplot(aes(category_2, bikeshop_name)) +
        
        # Geoms
        geom_tile(aes(fill = pct)) +
        
        #   __aes(label, text) => to provide the text to the ggplotly(tooltip = "text") ----
    geom_text(aes(label = scales::percent(pct, accuracy = .1), text = label_text),
              size = 3) +
        facet_wrap(~ category_1, scales = "free_x") +
        
        # Formatting
        scale_fill_gradient(low = "white", high = "#2c3e50") +
        theme_tq() +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none",
            plot.title = element_text(face = "bold"),
            strip.text.x = element_text(margin = margin(5, 5, 5, 5, unit = "pt"))
        ) +
        labs(title = "Heatmap of Purchasing Habits", 
             subtitle = "Author : Ralph D. Tasing")
    
    # INTERACTIVE VS STATIC
    if (interactive) {
        
        # __empty labs to don't have x/y absisses names when plot the interactive graph ----
        g <- g +
            labs(x = "", y = "")
        
        return(ggplotly(g, tooltip = "text"))
        
    } else {
        
        g <- g +
            labs(x = "Bike Type (Category 2)", y = "Customer")
        
        return(g)
        
    }
    
}

plot_customer_heatmap()
plot_customer_heatmap(interactive = FALSE)

# 3.0 CUSTOMER SEGMENTATION PLOT ----

#   __Always set the seed = .. to make the kmeans reproducible we add seed in the function argument ----
get_customer_segments <- function(k = 4, seed = 123) {
    
    # 1.0 CUSTOMER TRENDS
    customer_trends_tbl <- bike_orderlines_tbl %>%
        
        select(bikeshop_name, price, model, category_1, 
               category_2, frame_material, quantity) %>%
        
        group_by_at(.vars = vars(bikeshop_name:frame_material)) %>%
        summarise(total_qty = sum(quantity)) %>%
        ungroup() %>%
        
        group_by(bikeshop_name) %>%
        mutate(pct = total_qty / sum(total_qty)) %>%
        ungroup()
    
    customer_product_tbl <- customer_trends_tbl %>%
        select(bikeshop_name, model, pct) %>%
        spread(key = model, value = pct, fill = 0)
    
    
    # 2.0 MODELING: K-MEANS CLUSTERING
    #   __set.seed = set.seed to the kmeans to make it reproducible ----
    set.seed(seed)
    kmeans_obj <- customer_product_tbl %>%
        select(-bikeshop_name) %>%
        kmeans(centers = k, nstart = 100)
    
    kmeans_tbl <- kmeans_obj %>%
        augment(customer_product_tbl) %>%
        select(bikeshop_name, .cluster)
    
    # 3.0 UMAP
    # setting the seed to UMAP is different to the Kmean, we need to provide a umap.config ----
    #  umap.config => renferme les paramètres par défaut de UMAP (umap_conf$random = seed) ----
    umap_configuration <- umap.defaults
    umap_configuration$random_state <- seed
    
    umap_obj <- customer_product_tbl %>%
        select(-bikeshop_name) %>%
        as.matrix() %>%
        umap(config = umap_configuration) 
    
    umap_tbl <- umap_obj %>%
        pluck("layout") %>%
        as_tibble() %>%
        set_names(c("x", "y")) %>%
        bind_cols(
            customer_product_tbl %>% select(bikeshop_name)
        )
    
    # 4.0 COMBINE UMAP & K-MEANS
    combined_tbl <- umap_tbl %>%
        left_join(kmeans_tbl, by = "bikeshop_name") %>%
        mutate(label_text = str_glue("Customer: {bikeshop_name}
                                     Cluster: {.cluster}"))
    
    return(combined_tbl)
    
}

get_customer_segments(k = 4, seed = 123)

plot_customer_segments <- function(k = 4, seed = 123, interactive = TRUE) {
    
    # DATA MANIPULATION
    combined_tbl <- get_customer_segments(k = k, seed = seed)
    
    # VISUALIZATION
    g <- combined_tbl %>%
        
        ggplot(aes(x, y, color = .cluster)) +
        
        # Geoms
        geom_point(aes(text = label_text)) +
        
        # Formatting
        theme_tq() +
        scale_color_tq() +
        labs(
            title = "Customer Segmentation: 2D Projection",
            subtitle = "UMAP 2D Projection with K-Means Cluster Assignment"
        ) +
        theme(legend.position = "none")
    
    
    # INTERACTIVE VS STATIC
    if (interactive) {
        ggplotly(g, tooltip = "text")
    } else {
        g + geom_label_repel(aes(label = label_text), size = 3)
    }
    
}

plot_customer_segments(k = 4, seed = 123, interactive = TRUE)
plot_customer_segments(k = 4, seed = 123, interactive = FALSE)


# 4.0 VISUALIZE CUSTOMER BEHAVIOR ----

plot_customer_behavior_by_cluster <- function(top_n_products = 10, 
                                              k = 4, seed = 123, 
                                              interactive = TRUE) {
    
    # DATA MANIPULATION
    combined_tbl <- get_customer_segments(k = k, seed = seed)
    
    top_n_tbl <- bike_orderlines_tbl %>%
        select(bikeshop_name, model, category_1, category_2, price, quantity) %>%
        
        group_by_at(.vars = vars(bikeshop_name:price)) %>%
        summarize(total_qty = sum(quantity)) %>%
        ungroup() %>%
        
        group_by(bikeshop_name) %>%
        
        #   __arrange don't work on group , we need to include the argument .by_group = TRUE ----
    arrange(desc(total_qty), .by_group = TRUE) %>%
        #slice_max(n=top_n_products) %>%
        slice(1:top_n_products) %>%
        ungroup() %>%
        
        left_join(
            combined_tbl %>% select(bikeshop_name, .cluster), by = "bikeshop_name"
        ) %>%
        
        mutate(label_text = str_glue("Bike Shop: {bikeshop_name}
                                     Model: {model}
                                     Category 1: {category_1}
                                     Category 2: {category_2}
                                     Price: {scales::dollar(price)}")) 
    
    
    # VISUALIZATION
    g <- top_n_tbl %>%
        ggplot(aes(category_1, price, color = .cluster)) +
        
        # Geoms
        geom_violin() +
        # geom_jitter(aes(text = labeltext)) as geom_point we used that to plot in interactive plot ----
    geom_jitter(aes(text = label_text), width = 0.2, alpha = 0.5) +
        facet_wrap(~ .cluster, ncol = 2) +
        
        # Formatting
        theme_tq() +
        theme(strip.text.x = element_text(margin = margin(5, 5, 5, 5, "pt"))) +
        scale_color_tq() +
        #   __scale_y_log10() => transform the y axis in log10(y) ----
    scale_y_log10(labels = scales::dollar_format(accuracy = 1)) +
        labs(
            title = str_glue("Top {top_n_products} Bike Models by Customer and Cluster"),
            x = "Category 1", y = "Unit Price (Log Scale)"
        )
    
    
    # INTERACTIVE VS STATIC
    if (interactive) {
        ggplotly(g, tooltip = "text")
    } else {
        return(g)
    }
    
}

plot_customer_behavior_by_cluster(top_n_products = 5, 
                                  k = 4, seed = 123,
                                  interactive = FALSE)

plot_customer_behavior_by_cluster(top_n_products = 10, 
                                  k = 4, seed = 123,
                                  interactive = FALSE)

# 5.0 SAVE FUNCTIONS ----

function_names <- c("get_customer_segments", "plot_customer_segments",
                    "plot_customer_heatmap", "plot_customer_behavior_by_cluster")

dump(function_names, file = "../00_scripts/plot_customer_segmentation.R")

