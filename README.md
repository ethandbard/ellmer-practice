# ellmer-practice

A Shiny **teaching workbench** for data-science ideas, driven by natural language.

You ask in English. The model calls one tool. R runs the SQL or fits the model and refreshes every pane. The model does not see the raw rows by default — it sees a **statistical profile** of the table (roles, ranges, missingness) and, after each tool call, the numbers it just produced.

Two pages, same loop:

- **Explore** — SQL on a local DuckDB table. Chart, SQL, and the ggplot2 that drew it (one recipe, so those two cannot drift). The featured chart is ggplot by default; turn on **Interactive (Plotly)** in the sidebar for hover and zoom. Themes, palettes, and highlights stick across turns.
- **Model** — a preview fit, not a production pipeline. Linear / logistic / Poisson / tree / forest / ridge / elastic net / GAM / forecast, plus t-tests, ANOVA, chi-square, PCA, k-means, and a correlation heatmap. Holdout against a **named baseline**. Training-only cross-validation. The R pane prints the call that ran.

Switch provider in the sidebar. This bills the **xAI API** or the **Anthropic API**, not a ChatGPT / Claude.ai / grok.com subscription.

This app is **one browser session**. The DuckDB table is shared: a second tab will overwrite the first tab’s data.

## 1. Put a few dollars on an API

A website subscription does not count. You only need **one** provider to launch.

**xAI / Grok**

1. Open [console.x.ai](https://console.x.ai) and sign in.
2. Add a payment method and load a small credit. **$5–$10 is plenty**.
3. Create a key and copy it once.

**Anthropic / Claude**

1. Open [platform.claude.com](https://platform.claude.com/) and sign in (Console is a different product from Claude.ai).
2. Add a payment method and load a small credit.
3. Create a key at [platform.claude.com/settings/keys](https://platform.claude.com/settings/keys).
4. Copy the `sk-ant-...` string once. You will not see it again.

## 2. Put the key on this machine

In R (Positron or RStudio):

```r
install.packages("usethis")
usethis::edit_r_environ()
```

Add the line(s) you need (no quotes):

```
XAI_API_KEY=xai-your-key-here
ANTHROPIC_API_KEY=sk-ant-your-key-here
```

Save, then **restart R**. Confirm:

```r
nzchar(Sys.getenv("XAI_API_KEY"))           # TRUE if you added xAI
nzchar(Sys.getenv("ANTHROPIC_API_KEY"))     # TRUE if you added Claude
```

Do not commit the key. `.Renviron` is gitignored.

## 3. Install packages and launch

Open the `ellmer-practice` folder as the project / working directory.

```r
source("setup.R")
```

Optional: prove the key works (fraction of a cent):

```r
source("check-key.R")
```

You want a `grok-ok` and/or `claude-ok` reply for each key you set. Then:

```r
shiny::runApp()
```

Or open `app.R` and click **Run App**.

## What to try

Load `orders`, then `food_delivery.csv`, then Anscombe. The profile — not a hardcoded coffee-shop schema — should drive the answers.

### Explore

- What's in this table?
- Which region has the highest revenue?
- A violin of delivery time by cuisine
- Scatter with a linear smooth
- Make it dark, then colorblind-safe, then highlight the West
- Reset preview

Each question should fire **one** tool, `set_dashboard`. R runs read-only SQL and rebuilds the chart from a plot recipe. If the model skips the tool, say “update the dashboard.”

### Model

- Forecast the next 3 months of revenue
- Predict revenue from quantity, channel, and promo
- Show me a decision tree for promo
- Will this order use a promo?
- Which cuisine has the best ratings, and is the difference real?
- Show the ROC curve
- Cluster the numeric columns

Each question should fire **one** tool, `set_model`. R fits a preview, reports holdout metrics against a named baseline, and draws the fit. **Reset preview** restores the default view for the active tab, without another model call.

## How the pieces map

| File | Role |
|---|---|
| `data/orders.csv` | Starter table (fake coffee-shop line items) |
| `data/food_delivery.csv`, `data/anscombe-quartet.csv` | Extra files the profiler was never hand-tuned for |
| `greeting.md` / `ml-greeting.md` | Static welcomes (startup does not spend tokens) |
| `extra-instructions.md` | Explore *judgment* (when to slice vs break down, how to read the return) |
| `ml-instructions.md` | Model *judgment* (when to test vs forecast vs lasso, how to read CV) |
| `R/profile.R` | Column roles from statistics |
| `R/plot-recipe.R` | One recipe → chart pane and code pane |
| `R/model-tidymodels.R` | parsnip / recipes / workflows / yardstick |
| `R/stats-test.R`, `R/unsupervised.R` | t / ANOVA / chi-square / cor; PCA / k-means / heatmap |
| `R/tools.R` | Typed tool schemas and sticky style |
| `app.R` | Shiny UI + wiring |
| `setup.R` / `check-key.R` | Install and auth |
| `tests/testthat/` | Characterization + agreement tests |

The sidebar picks the provider. xAI is used when `XAI_API_KEY` is set; otherwise Claude. Change the model there at any time.

## If something breaks

| Symptom | Likely cause |
|---|---|
| App stops immediately about an API key | Neither key is in `.Renviron`, or R was not restarted |
| 401 / authentication | Website key, typo, or the console org has $0 credit |
| `could not find data/orders.csv` | Working directory is not `ellmer-practice` |
| Package not found | Run `source("setup.R")` again |
| Second browser tab overwrites the first | One DuckDB table per app process — use one tab |

## Next, if this clicks

Swap `orders` for a dplyr pull from Postgres (or a Parquet file) and keep the same QueryChat wrapper. The chat side does not change — only the data source does.
