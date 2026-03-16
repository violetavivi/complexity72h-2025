#%%
rm(list = ls())
library(reshape2)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(tibble)
#%% 2) load matrices
A <- data.frame(read.csv("smapCoef_Fig2.csv", header = TRUE, row.names = 1))
# NB signs are the opposite!!
A_nolag <- - A %>%
select(-contains("lag")) %>%
select(-contains("const_for"))


cols <- colnames(A_nolag)

# 2. Extract 'from' and 'to' numbers using regex
# This matches the digits after 'from_' and 'to_'
matches <- regexec("from_(\\d+)_to_(\\d+)", cols)
coords <- do.call(rbind, lapply(regmatches(cols, matches), function(x) as.numeric(x[2:3])))

# 3. Determine matrix dimensions (max index found)
n <- max(coords)

# 4. Initialize a zero matrix
adj_matrix <- matrix(0, nrow = n, ncol = n)

# 5. Populate the matrix with 1s at (x, y) coordinates
adj_matrix[coords] <- 1

# Optional: Add names for clarity
colnames(adj_matrix) <- rownames(adj_matrix) <- 1:n
#%%
diag(adj_matrix) <- 0
library(igraph)

# 1. Create the graph object from the adjacency matrix
# 'mode = "directed"' ensures the 'from-to' relationship is shown with arrows
g <- graph_from_adjacency_matrix(adj_matrix, mode = "directed")

# 2. Plot the graph
pdf("network_plot.pdf", width = 10, height = 10)
g_plot <- plot(g, 
     vertex.size = 20,           # Size of the circles
     vertex.color = "lightblue",  # Color of the nodes
     vertex.label.cex = 0.8,     # Size of the text inside nodes
     edge.arrow.size = 0.5,      # Size of the arrows
     edge.curved = 0.2,          # Adds a slight curve to overlapping edges
     main = "Network of smapc Links")
dev.off()
#%%

# This removes any row containing at least one NA
A_nolag_clean <- A_nolag %>% 
  drop_na()
# ONLY delete if the WHOLE row is NA
A_nolag_only_full_na_removed <- A_nolag[rowSums(is.na(A_nolag)) != ncol(A_nolag), ]  
#%%
# 1. Identify the Giant Component
# This finds all connected clusters and picks the largest one
components <- components(g)
giant_v_ids <- which(components$membership == which.max(components$csize))
giant_nodes <- V(g)[giant_v_ids]$name # Get the node labels (e.g., "1", "5", etc.)

# 2. Define a function to check if both 'from' and 'to' are in the giant component
is_in_giant <- function(col_name, nodes_list) {
  # Extract digits using regex
  nums <- as.numeric(unlist(regmatches(col_name, gregexpr("\\d+", col_name))))
  # Check if both extracted numbers exist in our giant_nodes list
  return(all(nums %in% nodes_list))
}
# 3. Filter the columns
# We use 'sapply' to create a logical vector for selection
cols_to_keep <- sapply(colnames(A_nolag_only_full_na_removed), 
                       is_in_giant, 
                       nodes_list = giant_nodes)

A_giant <- A_nolag_only_full_na_removed[, cols_to_keep]

#%%
# 1. Reshape the data
A_long <- A_giant %>%
  mutate(time = row_number()) %>%
  pivot_longer(cols = -time, names_to = "connection", values_to = "value")

# 2. Plot with your specific styling
p1 <- ggplot(A_long, aes(x = time, y = value)) +
  # Add the horizontal line at zero first (so it's behind the data)
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  # Use geom_line with a grouping trick or two layers for green/red
  # For a simple 'above/below' color logic:
  geom_line(aes(group = connection, color = value > 0), linewidth = 0.7) +
  # Define colors: TRUE (above 0) = green, FALSE (below 0) = red
  scale_color_manual(values = c("TRUE" = "forestgreen", "FALSE" = "firebrick")) +
  facet_wrap(~ connection, scales = "free_y") +
  theme_bw() + # Starts with a box (border)
  theme(
    panel.grid.major = element_blank(), # Delete major grids
    panel.grid.minor = element_blank(), # Delete minor grids
    legend.position = "none",           # Remove the TRUE/FALSE legend
    strip.background = element_rect(fill = "white"), # Clean facet labels
    strip.text = element_text(face = "bold", size = 8)
  ) +
  labs(x = "Time", y = "Interaction Value")
print(p1)
ggsave("Time-varying-matrix1.png",p1)
#%%
# 1. Prepare data and extract From/To metadata
A_long_grid <- A_giant %>%
  mutate(time = row_number()) %>%
  pivot_longer(cols = -time, names_to = "connection", values_to = "value") %>%
  # Extract the numbers from strings like "smapc_from_8_to_4"
  mutate(
    from_node = as.numeric(str_extract(connection, "(?<=from_)\\d+")),
    to_node   = as.numeric(str_extract(connection, "(?<=to_)\\d+"))
  )

# 2. Create the Matrix-style Grid Plot
p1 <- ggplot(A_long_grid, aes(x = time, y = value)) +
  # Horizontal line at zero
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  # Green if above zero, Red if below
  geom_line(aes(group = connection, color = value > 0), linewidth = 0.5) +
  scale_color_manual(values = c("TRUE" = "forestgreen", "FALSE" = "firebrick")) +
  # THE KEY STEP: Organize by From (rows) and To (cols)
  facet_grid(from_node ~ to_node, scales = "free_y") + 
  # Styling
  theme_bw() + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = "none",
    strip.background = element_rect(fill = "gray95"),
    strip.text = element_text(size = 7, face = "bold"),
    axis.text = element_blank(), # Optional: hide internal axis text for cleaner look
    axis.ticks = element_blank()
  ) +
  labs(
    title = "Time-varying Matrix",
    x = "Time",
    y = "Value"
  )
  print(p1)
ggsave("Time-varying-matrix2.png",p1)
