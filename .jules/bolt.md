## 2024-10-24 - Artificial Delays in Legacy Code
**Learning:** Legacy systems often include artificial delays (like `C$SLEEP`) to simulate processing time or pace the user interface.
**Action:** Always inspect `C$SLEEP` or delay loops to see if they are necessary for functional correctness or just "simulated" performance.
