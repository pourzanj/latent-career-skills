# Low Dimensional Latent Career Skills

## Data

We have $$N$$ players with $$M$$ observed stats over a varying amount of time.

$$\mathbf{y_{it}} \in \mathbb{R}^{M}$$ : vector of observed stats for player $$i$$ at time $$t$$

TODO:
  - Extract data using datasources
  - Identify appropriate stats to include
  - Characterize variance of the observations ($$\mathbf{\sigma_{it}^2}$$ in below models)

## Model 1 : Individual Curves per Player

In this variation, each player possesses a set of latent curves.  Let $$L$$ denote the number of latent curves to describe the underlying skill trajectory of players.

$$Z_{it} \in \mathbb{R}^{M \times L}$$ : matrix of basis functions for player $$i$$ evaluated at time $$t$$.

$$
\begin{align*}
\mathbf{y_{it}} &\sim MVN(\mathbf{\mu} + W \Lambda Z_{it}, \mathbf{\sigma_{it}^2} I) \\
Z_{it} &= f(t ; \mathbf{\theta_{i}}) \\
\end{align*}
$$

The choice of $$f(t;\mathbf{\theta_i})$$ represents the underlying shape of the latent career trajectory.  Implemented as of now are negative quadtratic basis functions parameterized via roots.

$$
f(t; a_i, b_i, c_i) = -c_i(t - a_i)(t - b_i)
$$

Notes:
  - Shared loading matrix (How observed stats depend on the underlying latent stats is the same for each player)
  - Player level basis functions (Could implement multiple hierarchical levels)

TODO:
  - Fix identifiability issues (Stiefel parameterizations/priors)
  - Reasonable priors
  - Explore parameterizations of the different curves
  - Fit to actual data (Account for copula component - link functions depends on the type of the observed stat)
  - Work on interpretation

## Model 2 : Shared Basis Functions with Players Based Loadings

This version gives a fixed pool of basis functions to represent the latent curves.  Variation in each player is captured by letting the loading weights differ between each player. 

$$Z_{it} \in \mathbb{R}^{M \times L}$$ : matrix of basis functions evaluated at time $$t$$.

$$
\begin{align*}
\mathbf{y_{it}} &\sim MVN(\mathbf{\mu} + W_{i} Z_{t}, \mathbf{\sigma_{it}^2} I) \\
Z_{t} &= f(t) \\
\end{align*}
$$

The choice of $$f(t)$$ represents the flexible basis that makes up the latent trajectories. Implemented as of now are L-th degree B-Splines. 

TODO:
  - Look at identifiability (PPC checks out but doubts about actual identifiability)
  - Reasonable priors
  - Fit to actual data (Account for copula component - link functions depends on the type of the observed stat)
  - Work on interpretation
