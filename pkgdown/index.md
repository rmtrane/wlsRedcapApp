# NACC T-Cog Neuropsychological Assessment
Ralph Møller Trane

This R package provides functions to work with outcomes from the NACC
T-Cog Neuropsychological Assessment. The main purpose is the creation of
a summary table used when dementia diagnoses are decided on through
consensus (see table below[^1])

``` r
library(wlsRedcapApp)

main_table(
  demo_data, 
  demo_data$ptid[1], 
  demo_data$cog_test_date[1]
)
```

<div id="bsfdybmgrm" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#bsfdybmgrm table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#bsfdybmgrm thead, #bsfdybmgrm tbody, #bsfdybmgrm tfoot, #bsfdybmgrm tr, #bsfdybmgrm td, #bsfdybmgrm th {
  border-style: none;
}

#bsfdybmgrm p {
  margin: 0;
  padding: 0;
}

#bsfdybmgrm .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#bsfdybmgrm .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#bsfdybmgrm .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#bsfdybmgrm .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#bsfdybmgrm .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#bsfdybmgrm .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#bsfdybmgrm .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#bsfdybmgrm .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: bold;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#bsfdybmgrm .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: bold;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#bsfdybmgrm .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#bsfdybmgrm .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#bsfdybmgrm .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#bsfdybmgrm .gt_spanner_row {
  border-bottom-style: hidden;
}

#bsfdybmgrm .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: bold;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#bsfdybmgrm .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: bold;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#bsfdybmgrm .gt_from_md > :first-child {
  margin-top: 0;
}

#bsfdybmgrm .gt_from_md > :last-child {
  margin-bottom: 0;
}

#bsfdybmgrm .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#bsfdybmgrm .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#bsfdybmgrm .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#bsfdybmgrm .gt_row_group_first td {
  border-top-width: 2px;
}

#bsfdybmgrm .gt_row_group_first th {
  border-top-width: 2px;
}

#bsfdybmgrm .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#bsfdybmgrm .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#bsfdybmgrm .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#bsfdybmgrm .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#bsfdybmgrm .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#bsfdybmgrm .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#bsfdybmgrm .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#bsfdybmgrm .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#bsfdybmgrm .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#bsfdybmgrm .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#bsfdybmgrm .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#bsfdybmgrm .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#bsfdybmgrm .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#bsfdybmgrm .gt_left {
  text-align: left;
}

#bsfdybmgrm .gt_center {
  text-align: center;
}

#bsfdybmgrm .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#bsfdybmgrm .gt_font_normal {
  font-weight: normal;
}

#bsfdybmgrm .gt_font_bold {
  font-weight: bold;
}

#bsfdybmgrm .gt_font_italic {
  font-style: italic;
}

#bsfdybmgrm .gt_super {
  font-size: 65%;
}

#bsfdybmgrm .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#bsfdybmgrm .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#bsfdybmgrm .gt_indent_1 {
  text-indent: 5px;
}

#bsfdybmgrm .gt_indent_2 {
  text-indent: 10px;
}

#bsfdybmgrm .gt_indent_3 {
  text-indent: 15px;
}

#bsfdybmgrm .gt_indent_4 {
  text-indent: 20px;
}

#bsfdybmgrm .gt_indent_5 {
  text-indent: 25px;
}

#bsfdybmgrm .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#bsfdybmgrm div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

<table class="gt_table do-not-create-environment cell"
data-quarto-postprocess="true" style="table-layout: fixed;"
data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<thead>
<tr class="header gt_col_headings gt_spanner_row">
<th rowspan="2" class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col"></th>
<th rowspan="2" id="Raw"
class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">Raw</th>
<th colspan="2" id="Standardized"
class="gt_center gt_columns_top_border gt_column_spanner_outer"
data-quarto-table-cell-role="th" scope="colgroup"><span
class="gt_column_spanner">Standardized</span></th>
<th rowspan="2" id="Percentile"
class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col">Percentile</th>
<th rowspan="2" id="Description"
class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col">Description</th>
</tr>
<tr class="odd gt_col_headings">
<th id="z-score"
class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">z-score</th>
<th id="SS" class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">SS</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr class="odd gt_group_heading_row">
<td colspan="6" id="General Cognition" class="gt_group_heading"
data-quarto-table-cell-role="th" scope="colgroup">General Cognition</td>
</tr>
<tr class="even gt_row_group_first">
<td class="gt_row gt_left gt_stub gt_indent_5"
headers="General Cognition stub_1_1 stub_1">CDR Global (1.5 SOB)</td>
<td class="gt_row gt_right"
headers="General Cognition stub_1_1 Raw">0.5</td>
<td class="gt_row gt_right"
headers="General Cognition stub_1_1 z-score">—</td>
<td class="gt_row gt_right"
headers="General Cognition stub_1_1 SS">—</td>
<td class="gt_row gt_left" style="text-align: center;"
headers="General Cognition stub_1_1 Percentile">—</td>
<td class="gt_row gt_left"
headers="General Cognition stub_1_1 Description">Normal</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left gt_stub gt_indent_5"
headers="General Cognition stub_1_2 stub_1">MoCA Blind</td>
<td class="gt_row gt_right"
headers="General Cognition stub_1_2 Raw">16</td>
<td class="gt_row gt_right"
headers="General Cognition stub_1_2 z-score">—</td>
<td class="gt_row gt_right"
headers="General Cognition stub_1_2 SS">—</td>
<td class="gt_row gt_left" style="text-align: center;"
headers="General Cognition stub_1_2 Percentile">—</td>
<td class="gt_row gt_left"
headers="General Cognition stub_1_2 Description">—</td>
</tr>
<tr class="even">
<td class="gt_row gt_left gt_stub gt_indent_5"
headers="General Cognition stub_1_3 stub_1">TICS-m</td>
<td class="gt_row gt_right"
headers="General Cognition stub_1_3 Raw">25</td>
<td class="gt_row gt_right"
headers="General Cognition stub_1_3 z-score">—</td>
<td class="gt_row gt_right"
headers="General Cognition stub_1_3 SS">—</td>
<td class="gt_row gt_left" style="text-align: center;"
headers="General Cognition stub_1_3 Percentile">—</td>
<td class="gt_row gt_left"
headers="General Cognition stub_1_3 Description">—</td>
</tr>
<tr class="odd gt_group_heading_row">
<td colspan="6" id="Attention/Processing" class="gt_group_heading"
data-quarto-table-cell-role="th"
scope="colgroup">Attention/Processing</td>
</tr>
<tr class="even gt_row_group_first">
<td class="gt_row gt_left gt_stub gt_indent_5"
headers="Attention/Processing stub_1_4 stub_1">Oral Trailmaking Part A -
Completion Time</td>
<td class="gt_row gt_right"
headers="Attention/Processing stub_1_4 Raw">10</td>
<td class="gt_row gt_right"
headers="Attention/Processing stub_1_4 z-score">1.22</td>
<td class="gt_row gt_right"
headers="Attention/Processing stub_1_4 SS">—</td>
<td class="gt_row gt_left" style="text-align: center;"
headers="Attention/Processing stub_1_4 Percentile"><div
style="flex-grow:1;margin-left:8px;background:#e1e1e1;">
<div
style="background:purple;width:11.1727684970483%;height:16px;display:flex;align-items:center;justify-content:center;color:#000000;font-weight:bold;font-size:10px;position:relative;">
<span
style="color:#000000;position:absolute;left:0%;margin-left:11.1727684970483px;font-weight:bold;font-size:10px;">11.2%</span>
</div>
</div></td>
<td class="gt_row gt_left"
headers="Attention/Processing stub_1_4 Description"
style="background-color: #FFA500">Low Average</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left gt_stub gt_indent_5"
headers="Attention/Processing stub_1_5 stub_1">Oral Trailmaking Part A -
Errors</td>
<td class="gt_row gt_right"
headers="Attention/Processing stub_1_5 Raw">0</td>
<td class="gt_row gt_right"
headers="Attention/Processing stub_1_5 z-score">−0.04</td>
<td class="gt_row gt_right"
headers="Attention/Processing stub_1_5 SS">—</td>
<td class="gt_row gt_left" style="text-align: center;"
headers="Attention/Processing stub_1_5 Percentile"><div
style="flex-grow:1;margin-left:8px;background:#e1e1e1;">
<div
style="background:purple;width:51.6617786490363%;height:16px;display:flex;align-items:center;justify-content:flex-start;position:relative;">
<span
style="color:#FFFFFF;position:absolute;left:0px;margin-left:5px;font-weight:bold;font-size:10px;">51.7%</span>
</div>
</div></td>
<td class="gt_row gt_left"
headers="Attention/Processing stub_1_5 Description"
style="background-color: #FFFF00">Average</td>
</tr>
<tr class="even">
<td class="gt_row gt_left gt_stub gt_indent_5"
headers="Attention/Processing stub_1_6 stub_1">Number Span Forward -
Total</td>
<td class="gt_row gt_right"
headers="Attention/Processing stub_1_6 Raw">7</td>
<td class="gt_row gt_right"
headers="Attention/Processing stub_1_6 z-score">−0.35</td>
<td class="gt_row gt_right"
headers="Attention/Processing stub_1_6 SS">—</td>
<td class="gt_row gt_left" style="text-align: center;"
headers="Attention/Processing stub_1_6 Percentile"><div
style="flex-grow:1;margin-left:8px;background:#e1e1e1;">
<div
style="background:purple;width:36.3985398480648%;height:16px;display:flex;align-items:center;justify-content:flex-start;position:relative;">
<span
style="color:#FFFFFF;position:absolute;left:0px;margin-left:5px;font-weight:bold;font-size:10px;">36.4%</span>
</div>
</div></td>
<td class="gt_row gt_left"
headers="Attention/Processing stub_1_6 Description"
style="background-color: #FFFF00">Average</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left gt_stub gt_indent_5"
headers="Attention/Processing stub_1_7 stub_1">Number Span Forward -
Span Length</td>
<td class="gt_row gt_right"
headers="Attention/Processing stub_1_7 Raw">6</td>
<td class="gt_row gt_right"
headers="Attention/Processing stub_1_7 z-score">−0.38</td>
<td class="gt_row gt_right"
headers="Attention/Processing stub_1_7 SS">—</td>
<td class="gt_row gt_left" style="text-align: center;"
headers="Attention/Processing stub_1_7 Percentile"><div
style="flex-grow:1;margin-left:8px;background:#e1e1e1;">
<div
style="background:purple;width:35.0261197051924%;height:16px;display:flex;align-items:center;justify-content:flex-start;position:relative;">
<span
style="color:#FFFFFF;position:absolute;left:0px;margin-left:5px;font-weight:bold;font-size:10px;">35%</span>
</div>
</div></td>
<td class="gt_row gt_left"
headers="Attention/Processing stub_1_7 Description"
style="background-color: #FFFF00">Average</td>
</tr>
<tr class="even">
<td class="gt_row gt_left gt_stub gt_indent_5"
headers="Attention/Processing stub_1_8 stub_1">Number Span Backward -
Total</td>
<td class="gt_row gt_right"
headers="Attention/Processing stub_1_8 Raw">6</td>
<td class="gt_row gt_right"
headers="Attention/Processing stub_1_8 z-score">−0.14</td>
<td class="gt_row gt_right"
headers="Attention/Processing stub_1_8 SS">—</td>
<td class="gt_row gt_left" style="text-align: center;"
headers="Attention/Processing stub_1_8 Percentile"><div
style="flex-grow:1;margin-left:8px;background:#e1e1e1;">
<div
style="background:purple;width:44.3201503183532%;height:16px;display:flex;align-items:center;justify-content:flex-start;position:relative;">
<span
style="color:#FFFFFF;position:absolute;left:0px;margin-left:5px;font-weight:bold;font-size:10px;">44.3%</span>
</div>
</div></td>
<td class="gt_row gt_left"
headers="Attention/Processing stub_1_8 Description"
style="background-color: #FFFF00">Average</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left gt_stub gt_indent_5"
headers="Attention/Processing stub_1_9 stub_1">Number Span Backward -
Span Length</td>
<td class="gt_row gt_right"
headers="Attention/Processing stub_1_9 Raw">5</td>
<td class="gt_row gt_right"
headers="Attention/Processing stub_1_9 z-score">0.31</td>
<td class="gt_row gt_right"
headers="Attention/Processing stub_1_9 SS">—</td>
<td class="gt_row gt_left" style="text-align: center;"
headers="Attention/Processing stub_1_9 Percentile"><div
style="flex-grow:1;margin-left:8px;background:#e1e1e1;">
<div
style="background:purple;width:62.0841763236855%;height:16px;display:flex;align-items:center;justify-content:flex-start;position:relative;">
<span
style="color:#FFFFFF;position:absolute;left:0px;margin-left:5px;font-weight:bold;font-size:10px;">62.1%</span>
</div>
</div></td>
<td class="gt_row gt_left"
headers="Attention/Processing stub_1_9 Description"
style="background-color: #FFFF00">Average</td>
</tr>
<tr class="even gt_group_heading_row">
<td colspan="6" id="Language" class="gt_group_heading"
data-quarto-table-cell-role="th" scope="colgroup">Language</td>
</tr>
<tr class="odd gt_row_group_first">
<td class="gt_row gt_left gt_stub gt_indent_5"
headers="Language stub_1_10 stub_1">Animal Fluency</td>
<td class="gt_row gt_right" headers="Language stub_1_10 Raw">5</td>
<td class="gt_row gt_right"
headers="Language stub_1_10 z-score">−2.63</td>
<td class="gt_row gt_right" headers="Language stub_1_10 SS">—</td>
<td class="gt_row gt_left" style="text-align: center;"
headers="Language stub_1_10 Percentile"><div
style="flex-grow:1;margin-left:8px;background:#e1e1e1;">
<div
style="background:purple;width:0.430136178460005%;height:16px;display:flex;align-items:center;justify-content:center;color:#000000;font-weight:bold;font-size:10px;position:relative;">
<span
style="color:#000000;position:absolute;left:0%;margin-left:0.430136178460005px;font-weight:bold;font-size:10px;">0.4%</span>
</div>
</div></td>
<td class="gt_row gt_left" headers="Language stub_1_10 Description"
style="background-color: #FF0000">Impaired</td>
</tr>
<tr class="even">
<td class="gt_row gt_left gt_stub gt_indent_5"
headers="Language stub_1_11 stub_1">Vegetable Fluency</td>
<td class="gt_row gt_right" headers="Language stub_1_11 Raw">12</td>
<td class="gt_row gt_right"
headers="Language stub_1_11 z-score">0.09</td>
<td class="gt_row gt_right" headers="Language stub_1_11 SS">—</td>
<td class="gt_row gt_left" style="text-align: center;"
headers="Language stub_1_11 Percentile"><div
style="flex-grow:1;margin-left:8px;background:#e1e1e1;">
<div
style="background:purple;width:53.5155166974005%;height:16px;display:flex;align-items:center;justify-content:flex-start;position:relative;">
<span
style="color:#FFFFFF;position:absolute;left:0px;margin-left:5px;font-weight:bold;font-size:10px;">53.5%</span>
</div>
</div></td>
<td class="gt_row gt_left" headers="Language stub_1_11 Description"
style="background-color: #FFFF00">Average</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left gt_stub gt_indent_5"
headers="Language stub_1_12 stub_1">F+L Words</td>
<td class="gt_row gt_right" headers="Language stub_1_12 Raw">11</td>
<td class="gt_row gt_right"
headers="Language stub_1_12 z-score">−1.47</td>
<td class="gt_row gt_right" headers="Language stub_1_12 SS">—</td>
<td class="gt_row gt_left" style="text-align: center;"
headers="Language stub_1_12 Percentile"><div
style="flex-grow:1;margin-left:8px;background:#e1e1e1;">
<div
style="background:purple;width:7.07012537433064%;height:16px;display:flex;align-items:center;justify-content:center;color:#000000;font-weight:bold;font-size:10px;position:relative;">
<span
style="color:#000000;position:absolute;left:0%;margin-left:7.07012537433064px;font-weight:bold;font-size:10px;">7.1%</span>
</div>
</div></td>
<td class="gt_row gt_left" headers="Language stub_1_12 Description"
style="background-color: #FF8C00">Borderline</td>
</tr>
<tr class="even">
<td class="gt_row gt_left gt_stub gt_indent_5"
headers="Language stub_1_13 stub_1">F+L+C Words</td>
<td class="gt_row gt_right" headers="Language stub_1_13 Raw">12</td>
<td class="gt_row gt_right" headers="Language stub_1_13 z-score">—</td>
<td class="gt_row gt_right" headers="Language stub_1_13 SS">5</td>
<td class="gt_row gt_left" style="text-align: center;"
headers="Language stub_1_13 Percentile"><div
style="flex-grow:1;margin-left:8px;background:#e1e1e1;">
<div
style="background:purple;width:5%;height:16px;display:flex;align-items:center;justify-content:center;color:#000000;font-weight:bold;font-size:10px;position:relative;">
<span
style="color:#000000;position:absolute;left:0%;margin-left:5px;font-weight:bold;font-size:10px;">5%</span>
</div>
</div></td>
<td class="gt_row gt_left" headers="Language stub_1_13 Description"
style="background-color: #FF8C00">Borderline</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left gt_stub gt_indent_5"
headers="Language stub_1_14 stub_1">F Words</td>
<td class="gt_row gt_right" headers="Language stub_1_14 Raw">10</td>
<td class="gt_row gt_right"
headers="Language stub_1_14 z-score">−0.50</td>
<td class="gt_row gt_right" headers="Language stub_1_14 SS">—</td>
<td class="gt_row gt_left" style="text-align: center;"
headers="Language stub_1_14 Percentile"><div
style="flex-grow:1;margin-left:8px;background:#e1e1e1;">
<div
style="background:purple;width:30.8537538725987%;height:16px;display:flex;align-items:center;justify-content:flex-start;position:relative;">
<span
style="color:#FFFFFF;position:absolute;left:0px;margin-left:5px;font-weight:bold;font-size:10px;">30.9%</span>
</div>
</div></td>
<td class="gt_row gt_left" headers="Language stub_1_14 Description"
style="background-color: #FFFF00">Average</td>
</tr>
<tr class="even">
<td class="gt_row gt_left gt_stub gt_indent_5"
headers="Language stub_1_15 stub_1">L Words</td>
<td class="gt_row gt_right" headers="Language stub_1_15 Raw">10</td>
<td class="gt_row gt_right"
headers="Language stub_1_15 z-score">−0.30</td>
<td class="gt_row gt_right" headers="Language stub_1_15 SS">—</td>
<td class="gt_row gt_left" style="text-align: center;"
headers="Language stub_1_15 Percentile"><div
style="flex-grow:1;margin-left:8px;background:#e1e1e1;">
<div
style="background:purple;width:38.2900299668467%;height:16px;display:flex;align-items:center;justify-content:flex-start;position:relative;">
<span
style="color:#FFFFFF;position:absolute;left:0px;margin-left:5px;font-weight:bold;font-size:10px;">38.3%</span>
</div>
</div></td>
<td class="gt_row gt_left" headers="Language stub_1_15 Description"
style="background-color: #FFFF00">Average</td>
</tr>
<tr class="odd gt_group_heading_row">
<td colspan="6" id="Memory" class="gt_group_heading"
data-quarto-table-cell-role="th" scope="colgroup">Memory</td>
</tr>
<tr class="even gt_row_group_first">
<td class="gt_row gt_left gt_stub gt_indent_5"
headers="Memory stub_1_16 stub_1">Craft Immediate - Verbatim</td>
<td class="gt_row gt_right" headers="Memory stub_1_16 Raw">12</td>
<td class="gt_row gt_right"
headers="Memory stub_1_16 z-score">−1.00</td>
<td class="gt_row gt_right" headers="Memory stub_1_16 SS">—</td>
<td class="gt_row gt_left" style="text-align: center;"
headers="Memory stub_1_16 Percentile"><div
style="flex-grow:1;margin-left:8px;background:#e1e1e1;">
<div
style="background:purple;width:15.8655253931457%;height:16px;display:flex;align-items:center;justify-content:center;color:#000000;font-weight:bold;font-size:10px;position:relative;">
<span
style="color:#000000;position:absolute;left:0%;margin-left:15.8655253931457px;font-weight:bold;font-size:10px;">15.9%</span>
</div>
</div></td>
<td class="gt_row gt_left" headers="Memory stub_1_16 Description"
style="background-color: #FFA500">Low Average</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left gt_stub gt_indent_5"
headers="Memory stub_1_17 stub_1">Craft Immediate - Paraphrase</td>
<td class="gt_row gt_right" headers="Memory stub_1_17 Raw">13</td>
<td class="gt_row gt_right"
headers="Memory stub_1_17 z-score">−0.20</td>
<td class="gt_row gt_right" headers="Memory stub_1_17 SS">—</td>
<td class="gt_row gt_left" style="text-align: center;"
headers="Memory stub_1_17 Percentile"><div
style="flex-grow:1;margin-left:8px;background:#e1e1e1;">
<div
style="background:purple;width:42.2441210252248%;height:16px;display:flex;align-items:center;justify-content:flex-start;position:relative;">
<span
style="color:#FFFFFF;position:absolute;left:0px;margin-left:5px;font-weight:bold;font-size:10px;">42.2%</span>
</div>
</div></td>
<td class="gt_row gt_left" headers="Memory stub_1_17 Description"
style="background-color: #FFFF00">Average</td>
</tr>
<tr class="even">
<td class="gt_row gt_left gt_stub gt_indent_5"
headers="Memory stub_1_18 stub_1">Craft Delay - Verbatim</td>
<td class="gt_row gt_right" headers="Memory stub_1_18 Raw">20</td>
<td class="gt_row gt_right" headers="Memory stub_1_18 z-score">0.47</td>
<td class="gt_row gt_right" headers="Memory stub_1_18 SS">—</td>
<td class="gt_row gt_left" style="text-align: center;"
headers="Memory stub_1_18 Percentile"><div
style="flex-grow:1;margin-left:8px;background:#e1e1e1;">
<div
style="background:purple;width:68.1032594704521%;height:16px;display:flex;align-items:center;justify-content:flex-start;position:relative;">
<span
style="color:#FFFFFF;position:absolute;left:0px;margin-left:5px;font-weight:bold;font-size:10px;">68.1%</span>
</div>
</div></td>
<td class="gt_row gt_left" headers="Memory stub_1_18 Description"
style="background-color: #FFFF00">Average</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left gt_stub gt_indent_5"
headers="Memory stub_1_19 stub_1">Craft Delay - Paraphrase</td>
<td class="gt_row gt_right" headers="Memory stub_1_19 Raw">14</td>
<td class="gt_row gt_right" headers="Memory stub_1_19 z-score">0.16</td>
<td class="gt_row gt_right" headers="Memory stub_1_19 SS">—</td>
<td class="gt_row gt_left" style="text-align: center;"
headers="Memory stub_1_19 Percentile"><div
style="flex-grow:1;margin-left:8px;background:#e1e1e1;">
<div
style="background:purple;width:56.4658383626831%;height:16px;display:flex;align-items:center;justify-content:flex-start;position:relative;">
<span
style="color:#FFFFFF;position:absolute;left:0px;margin-left:5px;font-weight:bold;font-size:10px;">56.5%</span>
</div>
</div></td>
<td class="gt_row gt_left" headers="Memory stub_1_19 Description"
style="background-color: #FFFF00">Average</td>
</tr>
<tr class="even gt_group_heading_row">
<td colspan="6" id="Executive Functioning" class="gt_group_heading"
data-quarto-table-cell-role="th" scope="colgroup">Executive
Functioning</td>
</tr>
<tr class="odd gt_row_group_first">
<td class="gt_row gt_left gt_stub gt_indent_5"
headers="Executive Functioning stub_1_20 stub_1">Oral Trailmaking Part B
- Completion Time</td>
<td class="gt_row gt_right"
headers="Executive Functioning stub_1_20 Raw">39</td>
<td class="gt_row gt_right"
headers="Executive Functioning stub_1_20 z-score">−0.40</td>
<td class="gt_row gt_right"
headers="Executive Functioning stub_1_20 SS">—</td>
<td class="gt_row gt_left" style="text-align: center;"
headers="Executive Functioning stub_1_20 Percentile"><div
style="flex-grow:1;margin-left:8px;background:#e1e1e1;">
<div
style="background:purple;width:65.5780085560428%;height:16px;display:flex;align-items:center;justify-content:flex-start;position:relative;">
<span
style="color:#FFFFFF;position:absolute;left:0px;margin-left:5px;font-weight:bold;font-size:10px;">65.6%</span>
</div>
</div></td>
<td class="gt_row gt_left"
headers="Executive Functioning stub_1_20 Description"
style="background-color: #FFFF00">Average</td>
</tr>
<tr class="even">
<td class="gt_row gt_left gt_stub gt_indent_5"
headers="Executive Functioning stub_1_21 stub_1">Oral Trailmaking Part B
- Errors</td>
<td class="gt_row gt_right"
headers="Executive Functioning stub_1_21 Raw">2</td>
<td class="gt_row gt_right"
headers="Executive Functioning stub_1_21 z-score">1.93</td>
<td class="gt_row gt_right"
headers="Executive Functioning stub_1_21 SS">—</td>
<td class="gt_row gt_left" style="text-align: center;"
headers="Executive Functioning stub_1_21 Percentile"><div
style="flex-grow:1;margin-left:8px;background:#e1e1e1;">
<div
style="background:purple;width:2.69564186050653%;height:16px;display:flex;align-items:center;justify-content:center;color:#000000;font-weight:bold;font-size:10px;position:relative;">
<span
style="color:#000000;position:absolute;left:0%;margin-left:2.69564186050653px;font-weight:bold;font-size:10px;">2.7%</span>
</div>
</div></td>
<td class="gt_row gt_left"
headers="Executive Functioning stub_1_21 Description"
style="background-color: #FF0000">Impaired</td>
</tr>
<tr class="odd gt_group_heading_row">
<td colspan="6" id="Mood" class="gt_group_heading"
data-quarto-table-cell-role="th" scope="colgroup">Mood</td>
</tr>
<tr class="even gt_row_group_first">
<td class="gt_row gt_left gt_stub gt_indent_5"
headers="Mood stub_1_22 stub_1">GDS-15 (Depression Symptoms)</td>
<td class="gt_row gt_right" headers="Mood stub_1_22 Raw">1</td>
<td class="gt_row gt_right" headers="Mood stub_1_22 z-score">—</td>
<td class="gt_row gt_right" headers="Mood stub_1_22 SS">—</td>
<td class="gt_row gt_left" style="text-align: center;"
headers="Mood stub_1_22 Percentile">—</td>
<td class="gt_row gt_left"
headers="Mood stub_1_22 Description">Minimal</td>
</tr>
</tbody>
</table>

</div>

## Get Started

Install this package from GitHub:

``` r
remotes::install_github("rmtrane/wlsRedcapApp")
```

The main Shiny application, which allows you to sift through summary
tables for all participants can be launched by first loaded the package,
then calling `wlsRedcapApp()`.

``` r
library(wlsRedcapApp)

wlsRedcapApp()
```

This package includes a demo data set (`demo_data`), which allows you to
explore the functionality of the package and in particular the Shiny
app. Not that these data might seem completely made up… because they
are. Visits might be out of order (as in, later visits have dates
earlier than first visits), and outcomes might seem contradictory. These
data were created by scrambling actual PHI data, and the sole purpose is
testing and showcasing this R package. If you have access to the WLS
REDCap project, you can download the full data from REDCap and choose to
use these data in the Shiny app. Simply follow the instructions in the
app.

[^1]: Note that Percentile numbers are rendered weird on this front
    page. For more accurate representation of the table, see the
    `main_table()` reference page
