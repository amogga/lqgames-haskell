# lq-games

[![Build Status](https://github.com/amogga/lqgames-haskell/actions/workflows/haskell.yml/badge.svg?branch=main)](https://github.com/amogga/lqgames-haskell/actions)
[![License](https://img.shields.io/badge/License-BSD--3--Clause-blue)](https://opensource.org/licenses/BSD-3-Clause)

A Haskell library for solving linear-quadratic (LQ) games in autonomous driving multi-agent interaction scenarios. This implementation provides an efficient backend for computing Nash equilibria in multi-player dynamic games using iterative LQ approximation. _This library serves as a testing and comparison baseline for a new framework currently under development._

## Overview

This project implements the algorithm proposed by Fridovich-Keil et al. in [Efficient Iterative Linear-Quadratic Approximations for Nonlinear Multi-Player General-Sum Differential Games](https://arxiv.org/abs/1909.04694). The original C++ implementation can be found at [HJReachability/ilqgames](https://github.com/HJReachability/ilqgames).

The library uses an iterative algorithm that:
1. Linearizes nonlinear dynamics around the current trajectory (using automatic differentiation)
2. Quadratizes nonlinear cost functions around the current trajectory
3. Solves the resulting LQ game via dynamic programming (backward Riccati recursion)
4. Updates controls using the computed feedback policy
5. Simulates forward using RK4 integration
6. Iterates until convergence to a Nash equilibrium

## Features

- **Automatic differentiation**: Uses the `ad` package for computing Jacobians and Hessians without manual derivatives
- **Efficient linear algebra**: Built on `hmatrix` with BLAS/LAPACK backends
- **Multiple vehicle models**: Unicycle and bicycle dynamics
- **Modular cost functions**: Goal tracking, lane keeping, proximity avoidance, velocity regulation, heading control
- **Visualization**: Trajectory plotting and GIF animation generation
- **Comprehensive examples**: Six interactive scenarios demonstrating different game-theoretic behaviors

## Examples

| Example 1 | Example 2 | Example 3 |
|-----------|-----------|-----------|
| ![Example 1](examples/Example1/plots/animation.gif) | ![Example 2](examples/Example2/plots/animation.gif) | ![Example 3](examples/Example3/plots/animation.gif) |

| Example 4 | Example 5 | Example 6 |
|-----------|-----------|-----------|
| ![Example 4](examples/Example4/plots/animation.gif) | ![Example 5](https://pub-research.s3.eu-de.cloud-object-storage.appdomain.cloud/github/lqgames/ex5anim.gif) | ![Example 6](https://pub-research.s3.eu-de.cloud-object-storage.appdomain.cloud/github/lqgames/ex6anim.gif) |

See individual example directories for detailed scenario descriptions:
- **Example 5**: [Overtaking scenario](examples/Example5/README.md) - Faster vehicle overtaking slower vehicle
- **Example 6**: [Head-on scenario](examples/Example6/README.md) - Coordinated collision avoidance

## Dependencies

### System Libraries

**macOS** (via Homebrew):
```bash
brew install imagemagick pkg-config
```

**Linux** (Ubuntu/Debian):
```bash
sudo apt-get install imagemagick pkg-config libpango1.0-dev libblas-dev liblapack-dev
```

### Haskell Dependencies

The project uses Stack (Stackage LTS 24.11) and key dependencies include:
- `hmatrix` (≥0.20.2) - Linear algebra operations
- `ad` (≥4.5.6) - Automatic differentiation
- `vector` (≥0.13.1.0) - Efficient arrays
- `diagrams-lib`, `diagrams-cairo` - Visualization

See `package.yaml` for the complete dependency list.

## Building

```bash
# Build library and executables
stack build

# Build with tests and benchmarks
stack build --test --bench

# Run tests
stack test

# Run benchmarks
stack bench
```

## Running Examples

```bash
# Run a specific example (1-6)
stack exec example1
stack exec example2
# ... etc

# Each example will:
# 1. Run the iterative LQ game solver
# 2. Print final costs for all players
# 3. Generate an animated GIF in examples/ExampleN/plots/
```

## Project Structure

```
lq-games/
├── src/
│   ├── Type/           # Core type definitions
│   ├── Solver/         # LQ game solver (dynamic programming)
│   ├── Dynamics/       # Vehicle models and linearization
│   ├── Algorithm/      # Discretization and ODE solvers
│   ├── Example/        # Simulation loop and cost functions
│   └── Plot/           # Visualization and animation
├── examples/
│   ├── Example1/       # Scenario 1
│   ├── Example2/       # Scenario 2
│   └── ...             # Additional scenarios
├── test/               # Test suite
└── bench/              # Benchmarks
```

## License

This project is licensed under the BSD-3-Clause License - see the [LICENSE](LICENSE) file for details.

## References

Fridovich-Keil, D., Ratner, E., Peters, L., Dragan, A. D., & Tomlin, C. J. (2019).
*Efficient Iterative Linear-Quadratic Approximations for Nonlinear Multi-Player General-Sum Differential Games.*
IEEE International Conference on Robotics and Automation (ICRA).
[arXiv:1909.04694](https://arxiv.org/abs/1909.04694)
