# Head-On Scenario (Example 6)

## Overview

This example demonstrates a two-player dynamic game where two vehicles approach each other head-on in the same lane, requiring coordinated collision avoidance. Both vehicles use unicycle dynamics and must negotiate which direction to deviate to avoid collision while solving for a Nash equilibrium through iterative LQ approximation.

## Scenario Description

Two vehicles start on the same lane but traveling in opposite directions, creating a head-on collision scenario:

- **Player 1 (Red car)**: Traveling northbound (heading = π/2)
- **Player 2 (Green car)**: Traveling southbound (heading = -π/2)

Both vehicles prefer to stay in the same lane (x = -2.0) but are on a direct collision course. The Nash equilibrium determines how both players coordinate to avoid collision.

## Player Configurations

### Player 1: Northbound Vehicle (Red)
- **Initial state**: (-2.0, -30.0, π/2, 5.0)
  - Position: (-2.0, -30.0)
  - Heading: π/2 radians (90°, pointing north)
  - Velocity: 5.0 m/s
- **Goal**: (-2.0, 50.0) - northbound destination
- **Preferred lane**: x = -2.0
- **Nominal heading**: π/2 (maintain northward direction)
- **Velocity preferences**:
  - Nominal velocity: 10.0 m/s
  - Maximum velocity: 10.0 m/s
  - Minimum velocity: 0.0 m/s
- **Proximity threshold**: 6.0 units
- **Key behaviors**:
  - High proximity cost (weight = 30) for strong collision avoidance
  - Nominal heading cost (weight = 10) to prefer straight northward travel
  - Lane preference (weight = 25) to stay at x = -2.0
  - Lane boundary constraint (weight = 3) within ±2.5 units

### Player 2: Southbound Vehicle (Green)
- **Initial state**: (-2.0, 30.0, -π/2, 5.0)
  - Position: (-2.0, 30.0)
  - Heading: -π/2 radians (-90°, pointing south)
  - Velocity: 5.0 m/s
- **Goal**: (-2.0, -50.0) - southbound destination
- **Preferred lane**: x = -2.0
- **Nominal heading**: -π/2 (maintain southward direction)
- **Velocity preferences**:
  - Nominal velocity: 10.0 m/s
  - Maximum velocity: 10.0 m/s
  - Minimum velocity: 0.0 m/s
- **Proximity threshold**: 6.0 units
- **Key behaviors**:
  - High proximity cost (weight = 30) for strong collision avoidance
  - Nominal heading cost (weight = 10) to prefer straight southward travel
  - Lane preference (weight = 25) to stay at x = -2.0
  - Lane boundary constraint (weight = 3) within ±2.5 units
  - Lower angular velocity input cost (weight = 5 vs 10) - slightly more willing to turn

## Cost Function Components

Both players optimize a combination of the following costs:

1. **Goal cost**: Penalizes distance from target position (weight = 1)
2. **Lane cost**: Penalizes deviation from preferred lane center at x = -2.0 (weight = 25)
3. **Lane boundary cost**: Soft constraint to stay within lane boundaries ±2.5 units (weight = 3)
4. **Velocity costs**: Penalizes deviation from nominal (10 m/s), minimum (0 m/s), and maximum (10 m/s) velocities (weight = 10 each)
5. **Proximity cost**: Strong penalty for proximity to other vehicles within 6 units (weight = 30)
6. **Nominal heading cost**: Defined but currently commented out in implementation (Players.hs:22,65 vs TotalCost.hs:32)
7. **Input costs**: Penalizes angular velocity (Player 1: weight = 10, Player 2: weight = 5) and acceleration (weight = 10)

## Simulation Parameters

```haskell
SimulationParametersWithHorizon
  { iterationCount = 400  -- Number of LQ iterations
  , sample = 0.1          -- Time step (seconds)
  , horizon = 100         -- Planning horizon (steps)
  }
```

- **Total planning horizon**: 10 seconds (100 steps × 0.1s)
- **Convergence iterations**: Up to 400 iterations of LQ approximation
  - Significantly more than Example 5 (100 iterations), indicating this is a more complex game requiring more iterations to converge to Nash equilibrium

## Expected Behavior

The Nash equilibrium solution exhibits the following characteristic behaviors:

1. **Approach phase**: Both vehicles approach each other while monitoring proximity
2. **Conflict detection**: As vehicles enter proximity range (~6 units), proximity costs activate
3. **Coordinated avoidance**: Both players simultaneously decide lateral deviations to avoid collision
   - The solution negotiates who deviates left vs right (or both partially deviate)
   - Player 2's lower angular velocity cost (weight = 5) may make it slightly more willing to turn
4. **Lane return**: After passing, both vehicles return toward their preferred lane
5. **Goal approach**: Continue toward respective goals

Unlike the asymmetric Example 5 (overtaking), this scenario is nearly symmetric - both players have similar cost structures and must cooperate to avoid collision.

## Key Differences from Other Examples

- **Head-on collision scenario**: Opposite-direction travel on same lane, unlike same-direction scenarios
- **Symmetric game structure**: Both players have nearly identical cost functions and weights, creating a coordination game
- **Bilateral collision avoidance**: Both players actively avoid collision (both have proximity weight = 30), unlike Example 5 where only one player cared
- **Higher iteration count**: Requires 400 iterations (vs 100-200 in other examples) suggesting more complex equilibrium convergence
- **Nominal heading defined**: Both players have nominal heading costs in their configuration (though currently commented out in the total cost function)
- **Asymmetric input costs**: Player 2 has lower angular velocity cost (5 vs 10), potentially affecting the equilibrium solution

## Running the Example

```bash
stack build
stack exec example6
```

This will:
1. Run the iterative LQ game solver (up to 400 iterations)
2. Print the final costs for both players
3. Generate an animated GIF visualization in `examples/Example6/`

The animation will show how both vehicles coordinate to avoid the head-on collision.

## Files

- `Main.hs`: Entry point, runs simulation and creates visualization
- `Players.hs`: Defines the two vehicle players with their cost functions and parameters
- `Simulation.hs`: Configures simulation parameters, dynamics, and initial conditions
- `TotalCost.hs`: Implements the combined cost function for each player

## Notes

- The nominal heading cost is defined in player configurations but commented out in `TotalCost.hs:32`
- Uncommenting the nominal heading cost would make vehicles prefer to maintain their original heading direction more strongly, potentially creating more complex avoidance maneuvers
