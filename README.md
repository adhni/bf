# Business Forecasting Course Site

Quarto website for ETF3231/ETF5231 Business Forecasting.

## What This Repo Contains

- Weekly teaching pages (`week1` to `week12`)
- Workshop activity pages (`week*/activities.qmd`)
- Assignment pages (`assignments/*.qmd`)
- Course-wide styling and helper logic (`numbat.css`, `course_info.R`)

## Local Development

From repo root:

```bash
make build
```

Start live preview:

```bash
make preview
```

Then open:

- http://localhost:8003/
- http://127.0.0.1:8003/

`Makefile` will use `quarto` from `PATH` when available, and otherwise fall back to the Quarto binary bundled with RStudio at `/Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto`.

## Deployment

Deployment is automated via GitHub Actions when `main` is updated.

Workflow file:

- `.github/workflows/quarto-gh-pages-html.yml`

Site URL:

- https://adhni.github.io/bf/

## Key Project Files

- `_quarto.yml`: site configuration, navbar, render settings
- `course_info.R`: helper functions for weekly overview cards and links
- `numbat.css`: custom styling

## Notes

- The project uses Quarto freeze (`freeze: auto` in `_quarto.yml`).
- If a page appears stale, run a full render from repo root and refresh the browser.
