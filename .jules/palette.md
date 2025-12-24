## 2024-05-23 - Terminal Dashboard Polish
**Learning:** Terminal-based dashboards need absolute cursor positioning to maintain layout integrity. Relying on stream-based `DISPLAY` destroys the visual structure of table shells.
**Action:** Use ANSI escape sequences for absolute cursor positioning (`ESC [ line ; col H`) when updating dashboard status rows.

## 2024-05-24 - Visual Feedback in Terminal
**Learning:** Adding color to status messages (Green for success, Red for failure) significantly improves the readability of terminal outputs without compromising accessibility (text remains).
**Action:** Use ANSI color codes (`ESC [ 32 m`, `ESC [ 31 m`) to wrap status text in COBOL applications. Always reset color (`ESC [ 0 m`) immediately after.
