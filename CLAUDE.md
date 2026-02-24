# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

UNPIE2 (Understanding Pensions in Europe 2) is an interactive web-based educational platform for pension finance literacy. It runs **entirely in the browser** — R computations execute client-side via WebR (R compiled to WebAssembly). There is no build step and no backend server.

Live site: https://www.sebastianstoeckl.com/unpie2/
Repository: https://github.com/sstoeckl/UNPIE2.git

## Development Workflow

Since this is a static website, development is direct file editing without a build step:

1. Edit R functions in `docs/r/` and/or HTML apps in `docs/`
2. Open the HTML file locally in a browser to test (WebR loads from CDN)
3. Use `?debug=true` URL parameter to reveal the hidden debug pane in apps
4. Use `?lang=en` or `?lang=de` to switch language (default: `en_en`)
5. Commit and push to deploy

## Architecture

### Technology Stack
- **R logic**: Runs client-side via [WebR](https://webr.r-wasm.org/) (WASM), loaded from CDN
- **Visualizations**: Chart.js 4.4.1 (via CDN)
- **Styling**: Tailwind CSS (static pages) / vanilla CSS (apps)
- **R environment**: renv with R 4.5.1 (for local R development only)

### App Structure Pattern

Each interactive app (`docs/caseN.html`, `docs/addN.html`) follows this pattern:

```
Input sliders → WebR executes R function from docs/r/ → Chart.js renders results
```

**JavaScript flow:**
```javascript
import { WebR } from "https://webr.r-wasm.org/latest/webr.mjs";
const webr = new WebR();
// Load R function source from server
await webr.evalR(codeString);
// On slider change, call function and update chart
const result = await webr.evalR(`ucase1(rate=${val}, nper=${n}, pv=${pv})`);
```

**R function return convention** — every function returns:
```r
list(
  ok     = TRUE,
  inputs  = list(...),   # echo of inputs
  results = list(...)    # computed values for charting
)
```

### File Naming Conventions

| Pattern | Description |
|---------|-------------|
| `docs/r/ucase#.R` | Basic case study R functions (1–9, plus `m` variants for mortality) |
| `docs/r/ucase#n.R` | Advanced mortality/longevity model variants (14n–16n) |
| `docs/r/uadd#.R` | Stochastic/Monte Carlo R functions (1–4) |
| `docs/case#.html` | Corresponding app UIs for basic cases |
| `docs/add#.html` | App UIs for stochastic/advanced cases |

### Bilingual i18n Pattern

All apps embed translation strings directly in a `const I18N` object:
```javascript
const I18N = {
  en_en: { title: "...", label1: "...", ... },
  de_de: { title: "...", label1: "...", ... }
};
const lang = (new URLSearchParams(location.search)).get("lang") || "en_en";
```

### App Categories

- **case1–case6**: Future/present value basics (compound interest, inflation, annuities)
- **case7–case9** + **m variants**: Mortality-adjusted calculations (Gompertz-Makeham model)
- **case14n–case16n**: Advanced longevity/mortality models
- **add1–add4**: Stochastic Monte Carlo pension scenarios (10,000 simulations default), covering: savings accumulation, required savings, years-to-ruin, max sustainable spending

### Responsive Layout

Apps use a two-column grid (control panel 320px + chart) that stacks to single column below 900px.

## Key External Dependencies (all via CDN, no local install)

- WebR latest: `https://webr.r-wasm.org/latest/webr.mjs`
- Chart.js 4.4.1: `https://cdn.jsdelivr.net/npm/chart.js`
- Tailwind CSS: CDN (static pages only)
