## 2026-01-01 - Weak Secret Truncation in COBOL
**Vulnerability:** Silent Truncation of Secrets in `ACCEPT`
**Learning:** In COBOL, if a variable is defined as `PIC X(20)` and the input is longer, `ACCEPT` silently truncates the excess. This reduces the effective entropy of long passwords to the size of the buffer, making them easier to brute-force.
**Prevention:** Always define variables holding secrets (passwords, tokens) with sufficient length (e.g., `PIC X(128)`) to accommodate the maximum expected input size, even if the "typical" password is short.
