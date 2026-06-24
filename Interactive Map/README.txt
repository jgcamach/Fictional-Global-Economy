Interactive Realistic Map Grid Editor — Multi-Select Version

Open interactive_realistic_map_grid_editor_multi_select.html.

Required files in the same folder:
- realistic_map_simple_overlay_no_grid.png
- realistic_map_grid_with_color_levels_5px.csv

New selection behavior:
- Drag across the map to select multiple invisible 5px grid blocks.
- Brush: select adds grid blocks.
- Brush: deselect removes grid blocks from the current selection.
- Brush: toggle flips selected/unselected status.
- Shift + drag still pans the map.
- Hovering shows the grid block under the mouse.
- Click sets the active cell.
- The active cell is the one shown in the right-side climate/user-attribution panel.
- “Apply form to selected cells” writes the current form to every selected grid block.
- “Clear selected cells” removes annotations from every selected grid block.

Shortcuts:
- V = select brush
- X = deselect brush
- T = toggle brush
- Ctrl/Cmd + S = save active cell

If the map appears but the cell data does not load, click “Load grid CSV manually” and select:
realistic_map_grid_with_color_levels_5px.csv

Edits are saved to browser localStorage. Export JSON/CSV regularly.
