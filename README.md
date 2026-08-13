# hobbs (High dimensiOnal Bayesian omniBus Sampler)

### R package for high dimensional Bayesian data analysis

 - Enables **high dimensional** statistical modeling using Bayesian inference.

 - **Probabilistic programming language** for high dimensional problems.

**See more on the website: <https://hobbs.github.io>**

**Visit the community forum: <https://groups.google.com/g/hobbs-users>**

## Installation

**hobbs** requires a C compiler and Rust with Cargo.

**Install Rust: <https://rustup.rs/>**

Windows users also need Rtools: <https://cran.r-project.org/bin/windows/Rtools/>

macOS users also need the Xcode Command Line Tools. Install them by running the following command in Terminal:

```
xcode-select --install
```

Linux users need a C compiler and standard build tools. For example, on Ubuntu or Debian:

```
sudo apt install build-essential
```

Install the development version from GitHub:

```r
install.packages("pak")
pak::install_github("hobbs-dev/hobbs")
```

Once the package is installed, check your compiler toolchain, build the sampler, and check the sampler:

```r
library(hobbs)
hobbs_check_toolchain()
hobbs_build_sampler()
hobbs_check_sampler()
```

## A first block example: Gaussian linear regression

This example simulates a regression problem with an `x` matrix, estimates the regression coefficients, and estimates the residual standard deviation through `logsigma`.

```r
library(hobbs)

set.seed(1)

n <- 1000L
p <- 5L

x <- matrix(rnorm(n * p), nrow = n, ncol = p)
x <- cbind(1, x)        # first column is the intercept
p <- ncol(x)

beta_true <- c(0.5, 1.0, -0.75, 0.5, 0.0, -0.25)
sigma_true <- 0.75

y <- as.numeric(x %*% beta_true + rnorm(n, 0, sigma_true))

data <- list(
  n = n,
  p = p,
  x = x,
  y = y
)
```

The model below declares two parameter blocks:

- `beta(p)` for the intercept and regression coefficients;
- `logsigma(1)` for the log residual standard deviation.

The block `beta(j)` updates one coefficient at a time. The block `logsigma(1)` updates the residual scale.

The update rule for each block is to **evaluate the prior contribution** for the parameter 
being updated and then evaluate **all direct children** of that parameter. 
In this example, the full likelihood is the only direct child of every parameter.

```r
model_src <- '
param beta(p);
param logsigma(1);

func y_lpdf() {
  double sigma = exp(logsigma(1));

  for (i = 1:n) {
    double mu = 0.0;
    for (j = 1:p) {
      mu += beta(j) * x(i,j);
    }
    y(i) ~ dnorm(mu,sigma);
  }
}

block beta(j) {
  beta(j) ~ dnorm(0,10);
  y_lpdf();
}

block logsigma(1) {
  logsigma(1) ~ dnorm(0,2);
  y_lpdf();
}
'

fit <- hobbs(
  model = model_src,
  data = data,
  samples = 2000,
  burnin = 1000,
  out = "chain_regression.bin"
)

draws <- read_hobbs("chain_regression.bin")
head(draws)
```

Posterior means can be compared with the simulated truth:

```r
beta_cols <- paste0("beta[", seq_len(p), "]")

colMeans(draws[, beta_cols])
beta_true

exp(mean(draws[, "logsigma[1]"]))
sigma_true
```

## Optimizing the regression with a deterministic cache

In the plain model, every update to one coefficient rebuilds the whole linear predictor:

```text
mu_i = beta(1) * x(i,1) + ... + beta(p) * x(i,p)
```

That costs roughly `O(n * p)` per scalar coefficient proposal. For large `p`, this is wasteful because changing `beta(j)` only changes `mu_i` by one rank-one update:

```text
mu_i <- mu_i + (new beta_j - old beta_j) * x(i,j)
```

hobbs supports attached deterministic caches for this pattern. The next model declares a persistent cached vector `mu(n)`, initializes it once, and then updates it whenever `beta(j)` changes.

```r
model_src_cached <- '
param beta(p) save=mean;
param logsigma(1);

func y_lpdf() {
  double sigma = exp(logsigma(1));

  for (i = 1:n) {
    y(i) ~ dnorm(mu(i),sigma);
  }
}

block beta(j) {
  beta(j) ~ dnorm(0,10);
  y_lpdf();
} cache mu(n) {
  for (i = 1:n) {
    for (k = 1:p) {
      mu(i) += beta(k) * x(i,k);
    }
  }
} update mu(n) {
  for (i = 1:n) {
    mu(i) += (proposal(beta(j)) - current(beta(j))) * x(i,j);
  }
}

block logsigma(1) {
  logsigma(1) ~ dnorm(0,2);
  y_lpdf();
}
'

fit_cached <- hobbs(
  model = model_src_cached,
  data = data,
  samples = 2000,
  burnin = 1000,
  out = "chain_regression_cached.bin"
)

draws <- read_hobbs("chain_regression_cached.bin")
draws_mean <- read_hobbs("chain_regression_cached.mean.bin")
```

The cached model is mathematically the same model as the plain version. The difference is computational: `mu(n)` is maintained incrementally instead of being rebuilt from all `p` predictors after every scalar coefficient proposal.

The important cache rules are:

- `cache mu(n) { ... }` declares and initializes the persistent cached vector;
- `update mu(n) { ... }` tells hobbs how to update that cache for the attached block;
- `proposal(beta(j))` is the proposed scalar value being evaluated;
- `current(beta(j))` is the currently accepted scalar value;
- if a proposal is rejected, hobbs restores the cache automatically.

This optimization is most useful when `p` is large and parameters are updated one at a time. For small regressions, the plain model may already be fast enough.

## Next steps

The same block syntax can be used for discrete parameters, sparse variable-selection models, random effects, and other models where only part of the likelihood changes for each parameter update. See <https://hobbs.github.io> for more examples and reference material.
