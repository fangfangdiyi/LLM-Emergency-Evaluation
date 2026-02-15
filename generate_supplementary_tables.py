"""
Generate Supplementary Table S5: Individual Case Scores by Model

This script reads the two rater scoring files and computes mean scores
(2 raters x 3 queries = 6 ratings per cell) for each case/model/dimension.

It also generates a representative case triplicate query summary for
the manuscript's Supplementary Box S2.

Dependencies: pandas, numpy, openpyxl
"""

import pandas as pd
import numpy as np
from openpyxl import Workbook
from openpyxl.styles import Font, Alignment
from openpyxl.utils import get_column_letter

# ==================== CONFIGURATION ====================
# Modify these paths to point to your local rater scoring files
RATER1_FILE = "rater1_scores.xlsx"  # Rater 1 scoring file
RATER2_FILE = "rater2_scores.xlsx"  # Rater 2 scoring file
OUTPUT_FILE = "output/Supplementary_Table_S5.xlsx"

# Case ID for the representative case example (Case 24)
REPRESENTATIVE_CASE_ID = 24

# Column names in the scoring Excel files (modify if your files use different names)
COL_CASE_ID = "病历号"  # Case ID column
COL_ROUND = "轮次"      # Round/dimension column (1=Diagnosis, 2=Next Steps, 3=Treatment)

# Model mapping (column identifiers in the scoring files)
# Columns named: 大模型1-1, 大模型1-2, 大模型1-3, ..., 大模型6-3
MODEL_MAPPING = {
    1: "Claude Sonnet 4.5",
    2: "GPT-5.1",
    3: "Grok 4",
    4: "DeepSeek V3.1",
    5: "DeepSeek R1",
    6: "Gemini 3 Pro",
}

DIMENSION_MAPPING = {1: "Diagnosis", 2: "Next Steps", 3: "Treatment"}
DIMENSION_SHORT = {1: "Diag", 2: "Next", 3: "Treat"}

# ==================== DATA READING ====================
print("=" * 60)
print("Generating Supplementary Table S5")
print("=" * 60)

print("\n1. Reading rater scoring files...")
r1_df = pd.read_excel(RATER1_FILE, sheet_name="Sheet1")
r2_df = pd.read_excel(RATER2_FILE, sheet_name="Sheet1")

print(f"   Rater 1: {r1_df.shape} ({r1_df.shape[0]} rows x {r1_df.shape[1]} cols)")
print(f"   Rater 2: {r2_df.shape} ({r2_df.shape[0]} rows x {r2_df.shape[1]} cols)")

# Diagnostic: print column names to verify
print(f"\n   Rater 1 columns: {list(r1_df.columns)}")
print(f"   Rater 2 columns: {list(r2_df.columns)}")

# Verify expected columns exist
for col_name in [COL_CASE_ID, COL_ROUND]:
    if col_name not in r1_df.columns:
        print(f"\n   ERROR: Column '{col_name}' not found in Rater 1 file!")
        print(f"   Available columns: {list(r1_df.columns)}")
        print(f"   Please update COL_CASE_ID and COL_ROUND at the top of this script.")
    if col_name not in r2_df.columns:
        print(f"\n   ERROR: Column '{col_name}' not found in Rater 2 file!")
        print(f"   Available columns: {list(r2_df.columns)}")

# ==================== TABLE S5 COMPUTATION ====================
print("\n2. Computing Table S5 (mean of 6 ratings per cell)...")

table_s5_data = []
nan_count = 0

for case_id in range(1, 55):
    row_data = {"Case": case_id}

    for model_num in range(1, 7):
        model_name = MODEL_MAPPING[model_num]
        col_prefix = f"大模型{model_num}"

        for dim in [1, 2, 3]:
            # Filter rows using explicit column names (matching original working code)
            r1_row = r1_df[
                (r1_df[COL_CASE_ID] == case_id) & (r1_df[COL_ROUND] == dim)
            ]
            r2_row = r2_df[
                (r2_df[COL_CASE_ID] == case_id) & (r2_df[COL_ROUND] == dim)
            ]

            if len(r1_row) == 0 or len(r2_row) == 0:
                row_data[f"{model_name}_{DIMENSION_SHORT[dim]}"] = np.nan
                nan_count += 1
                continue

            # Column names: 大模型{model_num}-1, 大模型{model_num}-2, 大模型{model_num}-3
            scores_r1 = [
                r1_row[f"{col_prefix}-1"].values[0],
                r1_row[f"{col_prefix}-2"].values[0],
                r1_row[f"{col_prefix}-3"].values[0],
            ]
            scores_r2 = [
                r2_row[f"{col_prefix}-1"].values[0],
                r2_row[f"{col_prefix}-2"].values[0],
                r2_row[f"{col_prefix}-3"].values[0],
            ]

            # Mean of all 6 scores (2 raters x 3 queries)
            avg = np.mean(scores_r1 + scores_r2)
            row_data[f"{model_name}_{DIMENSION_SHORT[dim]}"] = round(avg, 2)

    table_s5_data.append(row_data)

df_s5 = pd.DataFrame(table_s5_data)
print(f"   Table S5: {df_s5.shape}")
if nan_count > 0:
    print(f"   WARNING: {nan_count} cells have NaN (missing data)")
else:
    print(f"   All cells computed successfully (0 NaN)")

# ==================== REPRESENTATIVE CASE (TRIPLICATE QUERIES) ====================
print(f"\n3. Computing triplicate query summary for Case {REPRESENTATIVE_CASE_ID}...")


def get_triplicate(case_id):
    """Get per-query scores (mean of 2 raters) for a specific case."""
    result = {}
    for model_num in range(1, 7):
        model_name = MODEL_MAPPING[model_num]
        result[model_name] = {}
        col_prefix = f"大模型{model_num}"

        for dim in [1, 2, 3]:
            r1_row = r1_df[
                (r1_df[COL_CASE_ID] == case_id) & (r1_df[COL_ROUND] == dim)
            ]
            r2_row = r2_df[
                (r2_df[COL_CASE_ID] == case_id) & (r2_df[COL_ROUND] == dim)
            ]

            if len(r1_row) == 0 or len(r2_row) == 0:
                print(f"   WARNING: No data for Case {case_id}, "
                      f"Dim {DIMENSION_MAPPING[dim]}, Model {model_name}")
                result[model_name][DIMENSION_SHORT[dim]] = [np.nan] * 3
                continue

            query_scores = []
            for q in [1, 2, 3]:
                s1 = r1_row[f"{col_prefix}-{q}"].values[0]
                s2 = r2_row[f"{col_prefix}-{q}"].values[0]
                query_scores.append(round((s1 + s2) / 2, 1))

            result[model_name][DIMENSION_SHORT[dim]] = query_scores
    return result


trip_data = get_triplicate(REPRESENTATIVE_CASE_ID)


def fmt_scores(scores):
    """Format scores as Q1/Q2/Q3 string, handling NaN gracefully."""
    parts = []
    for s in scores:
        if pd.isna(s):
            parts.append("NA")
        else:
            parts.append(str(int(s)) if s == int(s) else str(s))
    return "/".join(parts)


trip_rows = []
for model_name in MODEL_MAPPING.values():
    d = trip_data[model_name]["Diag"]
    n = trip_data[model_name]["Next"]
    t = trip_data[model_name]["Treat"]
    all_s = [s for s in d + n + t if not pd.isna(s)]
    mean_t = round(np.mean(all_s), 1) if all_s else np.nan
    trip_rows.append(
        {
            "Model": model_name,
            "Diagnosis (Q1/Q2/Q3)": fmt_scores(d),
            "Next Steps (Q1/Q2/Q3)": fmt_scores(n),
            "Treatment (Q1/Q2/Q3)": fmt_scores(t),
            "Mean Total": mean_t,
        }
    )

df_trip = pd.DataFrame(trip_rows).sort_values("Mean Total", ascending=False)
print(f"   Triplicate summary:")
print(df_trip.to_string(index=False))

# ==================== WRITE TO EXCEL ====================
print("\n4. Writing to Excel...")

wb = Workbook()

# --- Sheet 1: Table S5 ---
ws1 = wb.active
ws1.title = "Table S5"

# Headers
ws1["A1"] = "Case"
ws1["A1"].font = Font(bold=True)
ws1["A1"].alignment = Alignment(horizontal="center")
ws1["A2"] = ""

col = 2
for model_name in MODEL_MAPPING.values():
    ws1.merge_cells(
        start_row=1, start_column=col, end_row=1, end_column=col + 2
    )
    c = ws1.cell(row=1, column=col, value=model_name)
    c.font = Font(bold=True)
    c.alignment = Alignment(horizontal="center")

    for i, dim_s in enumerate(["Diag", "Next", "Treat"]):
        c2 = ws1.cell(row=2, column=col + i, value=dim_s)
        c2.font = Font(bold=True)
        c2.alignment = Alignment(horizontal="center")
    col += 3

# Data rows
for row_idx, case_data in enumerate(table_s5_data, start=3):
    ws1.cell(
        row=row_idx, column=1, value=case_data["Case"]
    ).alignment = Alignment(horizontal="center")
    col = 2
    for model_name in MODEL_MAPPING.values():
        for dim_s in ["Diag", "Next", "Treat"]:
            val = case_data.get(f"{model_name}_{dim_s}", "")
            c = ws1.cell(row=row_idx, column=col, value=val)
            c.alignment = Alignment(horizontal="center")
            col += 1

ws1.column_dimensions["A"].width = 8
for c in range(2, 20):
    ws1.column_dimensions[get_column_letter(c)].width = 10

# --- Sheet 2: Representative Case Triplicate ---
ws2 = wb.create_sheet(f"Case_{REPRESENTATIVE_CASE_ID}_Triplicate")

headers = [
    "Model",
    "Diagnosis (Q1/Q2/Q3)",
    "Next Steps (Q1/Q2/Q3)",
    "Treatment (Q1/Q2/Q3)",
    "Mean Total",
]
for ci, h in enumerate(headers, 1):
    c = ws2.cell(row=1, column=ci, value=h)
    c.font = Font(bold=True)
    c.alignment = Alignment(horizontal="center", wrap_text=True)

for ri, rd in enumerate(df_trip.to_dict("records"), 2):
    for ci, h in enumerate(headers, 1):
        c = ws2.cell(row=ri, column=ci, value=rd[h])
        c.alignment = Alignment(horizontal="center")

for ci, w in enumerate([20, 20, 20, 20, 12], 1):
    ws2.column_dimensions[get_column_letter(ci)].width = w

wb.save(OUTPUT_FILE)

print(f"\nSaved: {OUTPUT_FILE}")
print("  Sheet 1: Table S5 (54 cases x 6 models x 3 dimensions)")
print(f"  Sheet 2: Case {REPRESENTATIVE_CASE_ID} triplicate query scores")
print("\nDone.")
