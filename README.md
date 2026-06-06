Yes. Here's the exact README text. Copy and paste it directly into the GitHub editor:

---

# Problem Solving with Jonny 5

> Real-time macro intelligence for operators who run on fuel and food.

A free weekly tool that translates live macro data into plain language verdicts for truckers, fleet operators, restaurant owners, and e-commerce operators.

## The Tool

Open `jonny5.html` in any browser. Four tabs, four calculators, one problem solved per tab.

**Trucker** — Is your fuel surcharge set right? Enter your miles, MPG, trucks, and current surcharge. See if you are covered or absorbing costs at forecasted diesel prices. Regional pricing by PADD district.

**Fleet** — What will fuel cost on jobs you are bidding today? Enter job details and bid rate. See fuel cost and margin under three scenarios before you commit.

**Restaurant** — What should you lock in with suppliers this week? The GPPI tracks upstream food cost pressure 4-8 weeks before it hits your invoices. Margin calculator shows the dollar impact.

**E-Commerce** — What do you need to order and when? Enter inventory, sales velocity, lead time, and revenue per unit. Get stockout risk, reorder point, and revenue at risk in dollars.

## The Pipeline

Run from terminal:

```
Rscript run_report.R
```

Pulls live data from FRED and EIA, runs forecasting models, updates the data cache. Runtime ~0.3 minutes. After running, rebuild jonny5.html in RStudio and commit to git.

## Setup

Requires R 4.x and packages: tidyverse, fredr, fable, fabletools, tsibble, lubridate, scales

Get a free FRED API key at fred.stlouisfed.org/docs/api/api_key.html

Create a `.env` file in the project root with your key:

```
FRED_API_KEY=your_32_character_key_here
```

## Data Sources

| Source | What it provides | Frequency |
|--------|-----------------|-----------|
| FRED (Federal Reserve) | Retail fuel prices by PADD region, CPI, PPI, consumer sentiment | Weekly / Monthly |
| EIA (Energy Information Administration) | WTI crude oil, regional diesel by district | Daily / Weekly |
| BLS (Bureau of Labor Statistics) | Food at home CPI, category price indices | Monthly |

## Track Record

Built by the same quantitative system that fed Wall Street earnings guidance at Intuit (sub-3% error on a multi-billion dollar revenue base), forecast route-level demand across 8 airlines and 13,000 daily flights at Gogo Air, and ran a quantitative trading operation generating consistent 3-5% annual ROI for 7 years.

## This is the public version

Want Jonny 5 running on your actual business data? [Let's talk.](mailto:jmizel312@gmail.com)

---
