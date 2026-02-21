## 2024-05-23 - Terminal Dashboard Polish
**Learning:** Terminal-based dashboards need absolute cursor positioning to maintain layout integrity. Relying on stream-based `DISPLAY` destroys the visual structure of table shells.
**Action:** Use ANSI escape sequences for absolute cursor positioning (`ESC [ line ; col H`) when updating dashboard status rows.

## 2024-05-24 - Visual Feedback in Terminal
**Learning:** Adding color to status messages (Green for success, Red for failure) significantly improves the readability of terminal outputs without compromising accessibility (text remains).
**Action:** Use ANSI color codes (`ESC [ 32 m`, `ESC [ 31 m`) to wrap status text in COBOL applications. Always reset color (`ESC [ 0 m`) immediately after.

## 2025-05-27 - Text-Based Icons for Accessibility
**Learning:** Color-coded status messages (Green/Red) are insufficient for accessibility (e.g., color blindness) or monochrome terminals. Adding distinct text icons (`[+]`/`[X]`) provides immediate, unambiguous feedback.
**Action:** Augment status messages with symbolic prefixes (e.g., `[+]` for success, `[X]` for failure) to ensure state is communicable without color.

## 2026-02-17 - Contextual Color Coding
**Learning:** In tabular TUI displays, colorizing the data values (e.g., pressure, heat) to match their status (Red for failure, Green for success) creates a stronger visual link between cause and effect than coloring the status column alone.
**Action:** Apply conditional ANSI color codes to value fields based on their validation state, ensuring to reset color immediately after the value.

## 2026-02-18 - Instructional Context
**Learning:** Sparse interfaces (like login screens) can feel welcoming and professional by adding a single line of instructional text or system status.
**Action:** Include a brief, helpful instruction or status message (e.g., "[i] Authorized Personnel Only") on entry screens to provide context and reduce cognitive load.

## 2026-02-19 - TUI Cursor Stability
**Learning:** Frequent screen repainting in a Terminal User Interface (TUI) causes visible cursor flicker, distracting the user.
**Action:** Hide the cursor (`ESC [ ? 25 l`) before starting a batch of UI updates and restore it (`ESC [ ? 25 h`) immediately after completion to ensure a stable, professional appearance.

## 2026-02-20 - Explicit Skipped States
**Learning:** In sequential processes where failure halts execution, leaving subsequent steps blank is ambiguous. Users may wonder if the process crashed or if the steps were forgotten.
**Action:** Explicitly mark skipped steps with a neutral status (e.g., `[-] SKIPPED` in Grey/White) to confirm the process logic was followed but the steps were intentionally bypassed.

## 2026-02-21 - TUI Column Alignment
**Learning:** In fixed-width TUI layouts, status messages must strictly adhere to column constraints. Adding a longer status like "SKIPPED" (11 chars) to a 11-char column requires starting at the very first available position (ignoring padding) to prevent overwriting the border.
**Action:** When designing TUI tables, verify the maximum length of all dynamic content against the exact column width (excluding borders) and set the starting cursor position to the cell's absolute start if necessary.
