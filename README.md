# Interacting Particle Systems

My Mathematical Statistics Master's thesis titled "Phase-Type Distributions for Finite Interacting
Particle Systems", University of Gothenburg 2019/2020.
Advisor: Jeff Steif.

![Voter simulation](./figures/voter_simulation_torus_100.png)

## Presentation
  - [Recorded Zoom link](https://www.youtube.com/watch?v=d7G9LOkMQEA)
  - https://www.slideshare.net/st00f/phasetype-distributions-for-finite-interacting-particle-systems
  - Code for the beamer slides is [available here](slides.tex)

## Abstract

Phase-type distributions represent the absorption time in an absorbing Markov chain with one absorbing state.
We look at the phase-type distributions of the voter model and the contact process on finite graphs.
Exact densities of the phase-type distribution are computed for simple cases and numeric approximations are computed for more complicated cases.
In the complete graph case we derive a formula for the expected value of the absorption time on a graph with N nodes for both the voter model and contact process.
The limiting distribution of the contact process is computed for a fixed sized complete graph as the infection rate goes to infinity.

### 1D Voter model Simulation

![1D Voter model Simulation](./figures/voter_simulation_1d_300.png)

### 2D Voter model Complete Simulation

![2D Voter model Complete Simulation](./figures/voter_simulation_1d_complete_split_100.png)

### Voter Model Dual Process

![Voter model dual process](./figures/voter_model_dual.png)

## Introduction

In an absorbing Markov chain with exactly one absorbing state the time
until absorption is called a phase-type distribution. We touch briefly on
the background of phase-type distributions, first studied by Neuts [1975],
including some interesting theorems on closure properties. For readers interested in phase-type distributions a comprehensive account is found in
Neuts [1981].

Interacting particle systems are stochastic processes with a state space
being graphs with different values on the nodes. These have been studied
since the 1970’s with seminal work by Liggett published in 1985, and much
work has been done since then. We focus on the case of two specific particle
systems, the voter model and the contact process, and restrict ourselves
mainly to the case where the number of nodes in the graph is finite. A short
background on these interacting particle systems are included with a few
key theorems including voter model clustering. Simulations are shown to
illustrate the behavior of the different models.

We focus on the finite case where we can find the exact distribution of
the absorption time for these models. Other work has been done on finite
models such as Cox [1989] and Durrett and Liu [1988]. In these works the
behavior of the processes are often computed for fixed parameters with the
number of nodes going to infinity. In contrast, we fix the number of nodes
and vary the infection rate in the contact process.

We start with the voter model on a complete graph and first find the
phase-type distribution for the n = 2, 3, 4 cases. The expected value is
computed in the general case with n nodes. For the contact process we
find the exact form of the phase-type distribution for the n = 2 case as a
function of the infection rate. The main result for the contact process is
showing that the limiting distribution of the n node complete graph, when
scaled appropriately, tends to an exponential distribution with rate 1. We
also numerically compute the phase-type distributions of the complete graph
with 3 nodes, a line with n = 2, 3, and torus with n = 2, 3, 4 nodes. The
expected values are graphed and compared.

A student with an introductory course in probability (understanding of
random variables, probability spaces, distributions, etc.) should be able to
follow this thesis as we include all relevant definitions and background.
