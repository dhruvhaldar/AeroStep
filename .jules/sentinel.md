## 2024-05-23 - Legacy COBOL Security Controls
**Vulnerability:** Missing Authentication and Input Masking in Terminal Applications
**Learning:** Legacy COBOL applications often lack built-in security features like authentication or password masking. When adding these, standard `ACCEPT` statements echo input to the screen.
**Prevention:** Use ANSI escape codes (e.g., `[8m` for conceal) to simulate password masking in terminal environments where `SCREEN SECTION` or Curses libraries are not fully utilized or behave inconsistently. Additionally, utilize Environment Variables to inject secrets rather than hardcoding credentials in the source.
## 2025-12-23 - COBOL Log Injection & Audit Gaps
**Vulnerability:** Log Injection via Unsanitized `ACCEPT` and Missing Audit Trails
**Learning:** Standard COBOL `ACCEPT` does not sanitize input, allowing ANSI escape codes or control characters to corrupt logs or terminal displays (Log Injection). Furthermore, opening a file (`OPEN OUTPUT`) without subsequent `WRITE` statements creates a false sense of security regarding audit trails.
**Prevention:** Explicitly sanitize all user inputs from `ACCEPT` using `INSPECT ... REPLACING` to neutralize control characters before processing or logging. Ensure all critical business events trigger a `WRITE` to a secured report/log file.
