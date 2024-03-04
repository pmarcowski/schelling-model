# Simulating Schelling's model of segregation
This script simulates a variation of Schelling's model of segregation, where agents of two types are randomly placed on a grid. Each agent decides if it is happy based on the types of neighboring agents. Unhappy agents move to a random empty space on the grid, simulating the dynamics of segregation. The model is implemented on a toroidal grid with configurable parameters for customized simulation and a visual representation of agent distribution and happiness over time.

## License
This code is licensed under the MIT License. For more details, see the LICENSE file in the root directory of this source tree.

## Usage
To run the simulation, ensure you have R installed on your computer. Execute the script in an R environment. The main parameters for simulation such as `num_agents`, `cells_side`, `alike_preference`, and `tlength` can be adjusted to explore different scenarios.

### Parameters
- `num_agents`: Total number of agents participating in the simulation.
- `cells_side`: The length of one side of the square simulation grid. This defines the grid's boundaries and handles the toroidal edge wrapping.
- `alike_preference`: Threshold for agents to be considered happy based on the proportion of neighboring agents of the same type.
- `tlength`: Total number of time steps the simulation will run.

The script initializes with default parameters for the simulation, which can be modified as needed to explore different dynamics.

## Installation
No additional installation is required beyond having R and necessary libraries installed. Clone this repository or download the script file to run it within your R environment.
