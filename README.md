# VALUATION ENGINE

Summary

This project is an R Shiny application for equity valuation using publicly available financial statement data.
It implements Free Cash Flow (FCF) construction, Discounted Cash Flow (DCF) valuation, and comparable-company analysis in a modular and reproducible structure.

What Was Done:

1. Retrieved financial statement data via the Financial Modeling Prep (FMP) API

2. Constructed Free Cash Flow from Income Statement and Balance Sheet data

3. Performed valuation using:

     a) Discounted Cash Flow (DCF)

     b) Comparable-company multiples

c) Integrated valuation logic into an interactive Shiny application

How It Was Done?

1. Data ingestion is handled in R/data_sources/

2. Valuation logic (FCF, DCF, comps) is implemented in R/valuation/

3. User interface and interactivity are implemented in R/shiny/app.R

4. Valuation logic is kept separate from the Shiny layer to ensure clarity and reusability.

Limitations

1. Financial data availability is constrained by the free tier of the FMP API

2. Coverage and completeness vary across companies and regions

3. Projections are mechanically derived and do not incorporate management guidance
