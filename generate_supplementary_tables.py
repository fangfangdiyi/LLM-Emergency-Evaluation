"""
Generate Supplementary Table S1: Individual Case Scores by Model

This script reads the two rater scoring files and computes mean scores
(2 raters x 3 queries = 6 ratings per cell) for each case/model/dimension.

It also generates a representative case triplicate query summary for
the manuscript's Supplementary Box S1.

Dependencies: pandas, numpy, openpyxl
"""

import pandas as pd
import numpy as np
from openpyxl.styles import Font, Alignment
import os

# ==================== CONFIGURATION ====================
# Using relative paths for better portability on GitHub
DATA_DIR = "data"
RATER1_FILENAME = "rater1_scores.xlsx" 
RATER2_FILENAME = "rater2_scores.xlsx"
OUTPUT_FILE = "Supplementary_Table_S1.xlsx"

REPRESENTATIVE_CASE_ID = 24  # Case ID used for the Supplementary Box example

MODEL_MAPPING = {
    1: "Claude Sonnet 4.5",
    2: "GPT-5.1",
    3: "Grok 4",
    4: "DeepSeek V3.1",
    5: "DeepSeek R1",
    6: "Gemini 3 Pro",
}

def setup_excel_style(writer, sheet_name):
    """Apply professional formatting to the Excel sheet."""
    workbook = writer.book
    worksheet = workbook[sheet_name]
    for row in worksheet.iter_rows():
        for cell in row:
            cell.alignment = Alignment(horizontal="center", vertical="center")
            if cell.row == 1:
                cell.font = Font(bold=True)

# ==================== DATA PROCESSING ====================

def run_analysis():
    # 1. Load Data
    path1 = os.path.join(DATA_DIR, RATER1_FILENAME)
    path2 = os.path.join(DATA_DIR, RATER2_FILENAME)
    
    if not (os.path.exists(path1) and os.path.exists(path2)):
        print(f"Error: Data files not found in {DATA_DIR} folder.")
        return

    r1_df = pd.read_excel(path1)
    r2_df = pd.read_excel(path2)

    # 2. Generate Sheet 2: Representative Case Triplicate
    print(f"Processing Representative Case {REPRESENTATIVE_CASE_ID}...")
    case_rows = []
    for m_idx, m_name in MODEL_MAPPING.items():
        # Each list stores scores for Round 1, 2, 3
        scores = {"Diag": [], "Next": [], "Treat": []}
        
        for round_val in [1, 2, 3]:
            # r1_df['轮次'] corresponds to the triplicate query instance
            row1 = r1_df[(r1_df["病历号"] == REPRESENTATIVE_CASE_ID) & (r1_df["轮次"] == round_val)]
            row2 = r2_df[(r2_df["病历号"] == REPRESENTATIVE_CASE_ID) & (r2_df["轮次"] == round_val)]
            
            if not row1.empty and not row2.empty:
                # Column '-1' is Diagnosis, '-2' is Next Steps, '-3' is Treatment
                scores["Diag"].append((row1[f"大模型{m_idx}-1"].values[0] + row2[f"大模型{m_idx}-1"].values[0]) / 2)
                scores["Next"].append((row1[f"大模型{m_idx}-2"].values[0] + row2[f"大模型{m_idx}-2"].values[0]) / 2)
                scores["Treat"].append((row1[f"大模型{m_idx}-3"].values[0] + row2[f"大模型{m_idx}-3"].values[0]) / 2)
        
        fmt = lambda s_list: "/".join([str(int(s)) if s == int(s) else str(s) for s in s_list])
        
        all_vals = scores["Diag"] + scores["Next"] + scores["Treat"]
        case_rows.append({
            "Model": m_name,
            "Diagnosis (Q1/Q2/Q3)": fmt(scores["Diag"]),
            "Next Steps (Q1/Q2/Q3)": fmt(scores["Next"]),
            "Treatment (Q1/Q2/Q3)": fmt(scores["Treat"]),
            "Mean Total": round(np.mean(all_vals), 2) if all_vals else 0
        })
    
    df_rep = pd.DataFrame(case_rows).sort_values("Mean Total", ascending=False)

    # 3. Generate Sheet 1: Full Summary Table
    print("Generating full summary table...")
    summary_data = []
    for c_id in sorted(r1_df["病历号"].unique()):
        row_data = {"Case": c_id}
        for m_idx, m_name in MODEL_MAPPING.items():
            r1_case = r1_df[r1_df["病历号"] == c_id]
            r2_case = r2_df[r2_df["病历号"] == c_id]
            
            for step_idx, step_name in enumerate(["Diag", "Next", "Treat"], 1):
                col = f"大模型{m_idx}-{step_idx}"
                all_scores = r1_case[col].tolist() + r2_case[col].tolist()
                row_data[f"{m_name}_{step_name}"] = round(np.mean(all_scores), 2) if all_scores else np.nan
        summary_data.append(row_data)
    
    df_sum = pd.DataFrame(summary_data)

    # 4. Save with Formatting
    with pd.ExcelWriter(OUTPUT_FILE, engine='openpyxl') as writer:
        df_sum.to_excel(writer, sheet_name="Table S1", index=False)
        df_rep.to_excel(writer, sheet_name="Representative Case", index=False)
        setup_excel_style(writer, "Table S1")
        setup_excel_style(writer, "Representative Case")

    print(f"Success! Results saved to {OUTPUT_FILE}")

if __name__ == "__main__":
    run_analysis()
