# Fuel & Grocery Price Forward Outlook

> **What's coming at the pump and the grocery store — and why.**

A free, weekly public report that translates macro energy data into plain language forward price outlooks for the people who actually pay fuel and grocery bills.

---

## The Problem With Every Other Fuel and Food Price Tool

**GasBuddy** tells you what gas costs right now at the nearest pump. That's useful. But it's a rearview mirror. It doesn't tell you what's coming or why.

**The EIA** publishes monthly government forecasts. Dense. Technical. Written for energy analysts, not fleet operators or households.

**Financial media** covers oil prices for investors. Not for the independent trucker deciding whether to lock in fuel costs or the family trying to plan a grocery budget.

**Nobody is connecting the dots in plain language:**

Crude oil prices move. Diesel prices follow — usually within 1-2 weeks. Trucking costs rise. Food manufacturers pay more to move product. Grocery prices adjust — usually within 4-8 weeks of the original crude move.

That chain is quantifiable. That chain is predictable. Nobody is explaining it to the people it affects most.

This tool does.

---

## What This Report Covers

### Module 1 — Fuel Forward (Weekly)

Where gas and diesel prices are going in the next 4-13 weeks, based on:
- Current WTI crude oil price and trajectory
- US crude, gasoline, and distillate inventory levels vs seasonal norms
- Refinery utilization
- Seasonal demand patterns
- Three scenarios: Base / Conservative / Supply Disruption

**Who this is for:**
- Independent truckers and fleet operators — fuel is 25-35% of your operating cost
- Agriculture operators — diesel timing matters when margins are thin
- Contractors and service businesses — fuel is a direct job cost
- Anyone making a vehicle or equipment purchase decision

**Fleet Cost Calculator:**
Enter your fleet size, weekly miles, and average MPG. The report tells you what each scenario means for your actual fuel bill — not just price per gallon, but total weekly and monthly cost.

---

### Module 2 — Grocery Price Pressure Index (Monthly)

A composite index built from four upstream signals that lead grocery prices by 4-8 weeks:

1. **Transportation cost pressure** — diesel trend as a cost input to every food delivery
2. **Agricultural input cost pressure** — energy costs flowing into farming operations
3. **Upstream manufacturing pressure** — what food manufacturers are paying before it hits shelves
4. **Freight cost pressure** — trucking rates as a direct food cost input

**The GPPI Score:**
- Above +1.5 — Significant pressure building. Expect above-average food inflation ahead.
- +0.5 to +1.5 — Moderate pressure. Food prices likely to rise modestly.
- -0.5 to +0.5 — Neutral. Food prices likely to track recent trend.
- Below -0.5 — Relief building. Food price inflation likely to moderate.

**Category breakdown:** Cereals and bakery, meat and poultry, dairy, fruits and vegetables — each with a current reading, direction, and plain language outlook.

---

## The Chain Nobody Shows You

```
WTI Crude Oil
    ↓  1-2 weeks
Diesel Retail Price
    ↓  2-4 weeks
PPI Truck Transportation (freight costs)
    ↓  4-6 weeks
CPI Food at Home (your grocery bill)
```

This report tracks every link in that chain with current readings and forward projections. When crude moves, you'll see it here before you feel it at the pump or the checkout line.

---

## Data Sources

All data is public and sourced from:
- **FRED** (Federal Reserve Economic Data) — retail fuel prices, CPI, PPI, inventory levels
- **EIA** (US Energy Information Administration) — weekly petroleum supply data
- **BLS** (Bureau of Labor Statistics) — consumer and producer price indices

No proprietary data. No paywalls. Fully reproducible.

---

## Methodology

Built in R using the Fable forecasting framework (Hyndman methodology). Models are multivariate time series — each forecast incorporates leading macro indicators, not just historical price patterns.

The GPPI is a composite index scoring four upstream cost signals on a normalized scale, weighted by their historical lead relationship to food CPI.

Full methodology documented in REQUIREMENTS.md. All code is open source and reproducible.

---

## Track Record

This tool is built by the same quantitative system that:

- Fed Wall Street earnings guidance at Intuit — sub-3% forecast error on a multi-billion dollar revenue base
- Forecast route-level wifi demand across 8 airlines and 13,000 daily flights at Gogo Air
- Built pricing intelligence infrastructure for 200 BevMo locations
- Ran a quantitative operation generating consistent 3-5% annual ROI on multi-million dollar portfolios for 7 years

The methodology isn't new. The application to fuel and grocery prices is.

---

## How to Use This Report

**If you're a fleet operator or trucker:**
1. Check the Fuel Forward section every Monday
2. Use the fleet cost calculator to translate the forecast into your actual weekly fuel cost
3. Use the Supply Disruption scenario for contingency planning

**If you're managing a household or food budget:**
1. Check the GPPI score monthly
2. Look at the category breakdown for the items most relevant to your household
3. Use the 3-month projection to plan ahead during high-pressure periods

**If you're a restaurant or food service operator:**
Watch both modules. Your costs are exposed to both direct fuel (delivery, equipment) and upstream food price pressure simultaneously.

---

## Refresh Schedule

- **Fuel Forward** — Every Monday, aligned with EIA weekly petroleum data release
- **Grocery Price Pressure Index** — First Monday after monthly BLS CPI release

---

## About

Built by an independent quantitative analyst with 20+ years building forecasting systems across aviation, retail, SaaS, and financial markets.

The same macro integration layer that made enterprise forecasts accurate — external factors baked in before they show up in your numbers — applied to the two cost line items that affect everyone.

*Helping you see what's coming before you feel it.*

---

## Technical

Built with R. Data via FRED API. Open source. Reproducible.

See REQUIREMENTS.md for full methodology and data source documentation.
