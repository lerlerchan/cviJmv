# contentvalidity

An R package for content validity analysis in scale development.

## Installation

```r
# Install from local source
install.packages("path/to/contentvalidity", repos = NULL, type = "source")

# Or using devtools
devtools::install_local("path/to/contentvalidity")
```

## Overview

The `contentvalidity` package provides comprehensive tools for calculating content validity metrics:

- **CVI (Content Validity Index)**: Item-level and scale-level
- **CVR (Content Validity Ratio)**: Lawshe's formula
- **UA (Universal Agreement)**: Perfect agreement indicator

## Quick Start

```r
library(contentvalidity)

# Prepare your data (rows = experts, columns = items)
ratings <- data.frame(
  item1 = c(4, 4, 3, 4, 3),
  item2 = c(4, 4, 4, 4, 4),
  item3 = c(3, 2, 2, 3, 2),
  item4 = c(4, 3, 4, 4, 3)
)

# Run full analysis
result <- cvi(ratings)
print(result)
summary(result)
plot(result)

# Individual metrics
icvi(ratings)      # Item-level CVI
scvi_ave(ratings)  # Scale-level CVI (average)
scvi_ua(ratings)   # Scale-level CVI (universal agreement)
cvr(ratings)       # Content Validity Ratio
```

## Supported Rating Scales

- **4-point** (default): Values 3-4 = relevant
- **3-point**: Values 2-3 = relevant
- **binary**: 0/1 or 1/2 coding
- **custom**: User-specified relevant values

```r
# Custom 5-point scale
cvi(data, scale = "custom", relevant_values = c(4, 5))
```

## Interpretation Guidelines

| Metric | Threshold | Reference |
|--------|-----------|-----------|
| I-CVI | >= 0.78 | Lynn (1986) |
| S-CVI/Ave | >= 0.90 | Polit & Beck (2006) |
| S-CVI/UA | >= 0.80 | - |

## Building the Package

```r
# Generate documentation
devtools::document()

# Run tests
devtools::test()

# Check package
devtools::check()

# Build package
devtools::build()
```

## Generating Example Data

Run the script in `data-raw/make_data.R` to regenerate the example dataset:

```r
setwd("contentvalidity")
source("data-raw/make_data.R")
```

## License

GPL (>= 3)
