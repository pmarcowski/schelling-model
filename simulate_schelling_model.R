# Title: Simulating Schelling's Model of Segregation
# Author: Przemyslaw Marcowski, PhD
# Email: p.marcowski@gmail.com
# Date: 2024-02-03
# Copyright (c) 2024 Przemyslaw Marcowski

# This code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# This script simulates a variation of Schelling's model of segregation.
# Agents of two types are randomly placed on a grid. During each time step,
# each agent decides whether it is happy based on the types of neighboring agents.
# Unhappy agents will move to a random empty space on the grid.

get_neighbors <- function(coords, cells_side) {
  # Calculates the coordinates of all neighboring cells for a given cell
  # in a toroidal grid (a grid where edges are connected to the opposite edge, 
  # creating a continuous surface). It considers eight possible directions for 
  # neighbors (N, NE, E, SE, S, SW, W, NW) relative to the given cell's coordinates.
  #
  # Args:
  #   coords: A numeric vector of length 2 representing the coordinates (row, 
  #           column) of the cell for which neighbors are to be found. Coordinates 
  #           are 1-indexed.
  #   cells_side: An integer indicating the number of cells on one side of the 
  #               square grid. This value is used to ensure the toroidal property
  #               of the grid, wrapping around coordinates that exceed grid 
  #               boundaries.
  #
  # Returns:
  #   A matrix where each row represents the coordinates of a neighboring cell. 
  #   There are 8 rows in total, one for each direction from the input cell. 
  #   Coordinates are adjusted to maintain the toroidal nature of the grid, 
  #   ensuring that all returned coordinates are valid positions within the grid's 
  #   dimensions.

  n <- c()
  directions <-
    rbind(
      c(1, 0), c(1, 1), c(0, 1), c(-1, 1),
      c(-1, 0), c(-1, -1), c(0, -1), c(1, -1)
    )

  # Check all 8 directions
  for (i in 1:8) {
    x <- coords[1] + directions[i, 1]
    y <- coords[2] + directions[i, 2]

    # Ensure the grid is toroidal (edges are connected)
    x <- ifelse(x < 1, cells_side, ifelse(x > cells_side, 1, x))
    y <- ifelse(y < 1, cells_side, ifelse(y > cells_side, 1, y))

    n <- rbind(n, c(x, y))
  }
  return(n)
}

# Initialize parameters
num_agents <- 2000 # total number of agents
cells_side <- 51 # side length of the square simulation grid, defines grid boundaries and handles toroidal edge wrapping
grid_size <- cells_side^2 # total number of cells in simulation grid
alike_preference <- .7 # threshold for agents to be happy based on alike neighbors
tlength <- 100 # number of simulation steps

# Create vector representing all cells in the grid
group <- c(rep(0, grid_size - num_agents), rep(1, num_agents / 2), rep(2, num_agents / 2))

# Create a grid and randomly distribute the agents
grid <- matrix(sample(group, grid_size, replace = FALSE), ncol = cells_side)

# Set up the plotting environment with two plots side by side.
par(mfrow = c(1, 2), pty = "s")

# Initial distribution of agents
image(grid, col = c("white", "red", "blue"), axes = FALSE)

# Placeholder for happiness plot
plot(runif(100, 0, 1), ylab = "Percent happy", xlab = "Time", col = "white", ylim = c(0, 1))

# Create vector to store happiness tracking
happiness_tracker <- c()

# Main loop for the simulation
for (t in c(1:tlength)) {
  happy_cells <- c() # track happy agents
  unhappy_cells <- c() # track unhappy agents

  # Check each cell on the grid
  for (j in c(1:cells_side)) {
    for (k in c(1:cells_side)) {
      current <- c(j, k)
      value <- grid[j, k]

      # Proceed if cell is not empty
      if (value > 0) {
        like_neighbors <- 0
        all_neighbors <- 0
        neighbors <- get_neighbors(current, cells_side)

        # Count like and total neighbors
        for (i in c(1:nrow(neighbors))) {
          x <- neighbors[i, 1]
          y <- neighbors[i, 2]

          if (grid[x, y] > 0) {
            all_neighbors <- all_neighbors + 1
            if (grid[x, y] == value) {
              like_neighbors <- like_neighbors + 1
            }
          }
        }

        # Determine if the agent is happy and update respective lists
        if (!is.nan(like_neighbors / all_neighbors)) {
          if ((like_neighbors / all_neighbors) < alike_preference) {
            unhappy_cells <- rbind(unhappy_cells, c(current[1], current[2]))
          } else {
            happy_cells <- rbind(happy_cells, c(current[1], current[2]))
          }
        } else {
          happy_cells <- rbind(happy_cells, c(current[1], current[2]))
        }
      }
    }
  }

  # Update happiness tracker with the percentage of happy agents
  happiness_tracker <-
    append(
      happiness_tracker,
      length(happy_cells) / (length(happy_cells) + length(unhappy_cells))
    )

  # Move unhappy agents to a random empty cell
  rand <- sample(nrow(unhappy_cells))
  for (i in rand) {
    mover <- unhappy_cells[i, ]
    mover_val <- grid[mover[1], mover[2]]
    move_to <- c(sample(1:cells_side, 1), sample(1:cells_side, 1))
    move_to_val <- grid[move_to[1], move_to[2]]

    # Find an empty cell
    while (move_to_val > 0) {
      move_to <- c(sample(1:cells_side, 1), sample(1:cells_side, 1))
      move_to_val <- grid[move_to[1], move_to[2]]
    }
    grid[mover[1], mover[2]] <- 0 # empty the original cell
    grid[move_to[1], move_to[2]] <- mover_val # move the agent to the new cell
  }

  # Update and display plot with agent distribution
  image(grid, col = c("white", "red", "blue"), axes = FALSE)
  title(main = paste("Agent distribution in Schelling's Model"))
  box()

  # Add legend for cell types
  legend(
    x = "bottom",
    inset = c(.5, -.3),
    legend = c("Empty space", "Agent type 1", "Agent type 2"),
    fill = c("white", "red", "blue"),
    horiz = FALSE,
    xpd = TRUE,
    title = "Cell type"
  )

  mtext(paste("Time:", t, "/", tlength), side = 1, line = .5)

  # Update and display plot with percentage of happy agents out of all agents over time
  plot(runif(100, 0, 1), ylab = "Percent happy", xlab = "Time", col = "white", ylim = c(0, 1))
  lines(happiness_tracker, oma = c(0, 0, 2, 0), col = "red")
  mtext(paste("Alike preference threshold:", alike_preference), side = 3, line = 1)

  Sys.sleep(.1) # pause for animation effect

  # Message simulation complete
  if (t == tlength) {
    message("Simulation complete after specified time: ", tlength)
  }
}
