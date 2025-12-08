# Overtaking Scenario (Example 5)

## Overview

This example demonstrates a two-player dynamic game where a faster vehicle overtakes a slower vehicle on a single-lane road. Both vehicles use unicycle dynamics and solve a Nash equilibrium through iterative LQ approximation.

<img src="https://pub-research.s3.eu-de.cloud-object-storage.appdomain.cloud/github/lqgames/ex5anim.gif" height="600">

## Scenario Description

Two vehicles start in the same lane, traveling in the same direction (northbound, heading = π/2):

- **Player 1 (Red car)**: Faster vehicle positioned behind
- **Player 2 (Green car)**: Slower vehicle positioned in front

Both vehicles have the same goal position, but different velocity preferences, creating a natural overtaking scenario.

## Player Configurations

### Player 1: Faster Vehicle (Red)
- **Initial state**: (-2.0, -20.0, π/2, 5.0)
  - Position: (-2.0, -20.0)
  - Heading: π/2 radians (90°, pointing north)
  - Velocity: 5.0 m/s
- **Goal**: (-2.0, 50.0)
- **Preferred lane**: x = -2.0
- **Velocity preferences**:
  - Nominal velocity: 10.0 m/s
  - Maximum velocity: 10.0 m/s
  - Minimum velocity: 0.0 m/s
- **Key behaviors**:
  - High proximity cost (weight = 15) to avoid collisions
  - Prefers higher speed, creating pressure to overtake
  - Will deviate from lane to pass slower vehicle

### Player 2: Slower Vehicle (Green)
- **Initial state**: (-2.0, -10.0, π/2, 1.0)
  - Position: (-2.0, -10.0)
  - Heading: π/2 radians (90°, pointing north)
  - Velocity: 1.0 m/s
- **Goal**: (-2.0, 50.0)
- **Preferred lane**: x = -2.0
- **Velocity preferences**:
  - Nominal velocity: 2.0 m/s
  - Maximum velocity: 5.0 m/s
  - Minimum velocity: 0.0 m/s
- **Key behaviors**:
  - No proximity cost (weight = 0) - does not actively avoid other vehicles
  - Prefers to maintain lane and moderate speed
  - Low goal weight (weight = 1) - less urgency to reach goal

## Cost Function Components

Both players optimize a combination of the following costs:

1. **Goal cost**: Penalizes distance from target position
2. **Lane cost**: Penalizes deviation from preferred lane center (x = -2.0)
3. **Lane boundary cost**: Soft constraint to stay within lane boundaries (±2.5 units)
4. **Velocity costs**: Penalizes deviation from nominal, minimum, and maximum velocities
5. **Proximity cost**: Penalizes proximity to other vehicles (only Player 1)
6. **Input costs**: Penalizes angular velocity and acceleration

## Simulation Parameters

```haskell
SimulationParametersWithHorizon
  { iterationCount = 100  -- Number of LQ iterations
  , sample = 0.1          -- Time step (seconds)
  , horizon = 100         -- Planning horizon (steps)
  }
```

- **Total planning horizon**: 10 seconds (100 steps × 0.1s)
- **Convergence iterations**: Up to 100 iterations of LQ approximation

## Expected Behavior

The Nash equilibrium solution exhibits the following characteristic behaviors:

1. **Initial phase**: Player 1 approaches Player 2 from behind
2. **Overtaking maneuver**: Player 1 deviates from the lane to pass Player 2
3. **Lane return**: After passing, Player 1 returns to the preferred lane
4. **Simultaneous optimization**: Both players react to each other's predicted trajectories

The absence of heading constraints (removed in commit 642a1de) allows vehicles to maneuver more freely during the overtaking phase, resulting in more natural lateral movements.

## Key Differences from Other Examples

- **No heading cost**: Unlike some examples, vehicles are not penalized for deviating from a nominal heading, allowing natural steering during overtaking
- **Asymmetric proximity costs**: Only the overtaking vehicle (Player 1) cares about collision avoidance, modeling a scenario where the slower vehicle is unaware or unresponsive
- **Velocity differential**: Large difference in preferred velocities (10 m/s vs 2 m/s) creates strong overtaking incentive

## Running the Example

```bash
stack build
stack exec example5
```

This will:
1. Run the iterative LQ game solver
2. Print the final costs for both players
3. Generate an animated GIF visualization in `examples/Example5/`

## Files

- `Main.hs`: Entry point, runs simulation and creates visualization
- `Players.hs`: Defines the two vehicle players with their cost functions and parameters
- `Simulation.hs`: Configures simulation parameters, dynamics, and initial conditions
- `TotalCost.hs`: Implements the combined cost function for each player
