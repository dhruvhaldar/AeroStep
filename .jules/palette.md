## 2024-05-23 - Terminal Dashboard Polish
**Learning:** Terminal-based dashboards need absolute cursor positioning to maintain layout integrity. Relying on stream-based `DISPLAY` destroys the visual structure of table shells.
**Action:** Use ANSI escape sequences for absolute cursor positioning (`ESC [ line ; col H`) when updating dashboard status rows.
