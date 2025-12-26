## 2024-05-23 - Terminal Dashboard Polish
**Learning:** Terminal-based dashboards need absolute cursor positioning to maintain layout integrity. Relying on stream-based `DISPLAY` destroys the visual structure of table shells.
**Action:** Use ANSI escape sequences for absolute cursor positioning (`ESC [ line ; col H`) when updating dashboard status rows.

## 2024-05-24 - Visual Feedback in Terminal
**Learning:** Adding color to status messages (Green for success, Red for failure) significantly improves the readability of terminal outputs without compromising accessibility (text remains).
**Action:** Use ANSI color codes (`ESC [ 32 m`, `ESC [ 31 m`) to wrap status text in COBOL applications. Always reset color (`ESC [ 0 m`) immediately after.

## 2025-05-27 - Text-Based Icons for Accessibility
**Learning:** Color-coded status messages (Green/Red) are insufficient for accessibility (e.g., color blindness) or monochrome terminals. Adding distinct text icons (`[+]`/`[X]`) provides immediate, unambiguous feedback.
**Action:** Augment status messages with symbolic prefixes (e.g., `[+]` for success, `[X]` for failure) to ensure state is communicable without color.

## 2025-12-26 - High-Fidelity CLI Borders
**Learning:** Users perceive applications using Unicode box-drawing characters as significantly more polished and "professional" than those using ASCII fallback characters (+, -, |). However, inserting these multi-byte characters directly into COBOL strings can cause line-length issues and fragility.
**Action:** When implementing high-fidelity borders in COBOL, define the Unicode characters as hexadecimal constants (e.g., `WS-BOX-TOP-START PIC X(3) VALUE X'E2948C'`) in `WORKING-STORAGE` and construct lines programmatically or via concatenation.
