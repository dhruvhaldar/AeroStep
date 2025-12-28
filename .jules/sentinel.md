## 2024-05-23 - Legacy COBOL Security Controls
**Vulnerability:** Missing Authentication and Input Masking in Terminal Applications
**Learning:** Legacy COBOL applications often lack built-in security features like authentication or password masking. When adding these, standard `ACCEPT` statements echo input to the screen.
**Prevention:** Use ANSI escape codes (e.g., `[8m` for conceal) to simulate password masking in terminal environments where `SCREEN SECTION` or Curses libraries are not fully utilized or behave inconsistently. Additionally, utilize Environment Variables to inject secrets rather than hardcoding credentials in the source.

## 2025-12-28 - CSV Injection in COBOL Logs
**Vulnerability:** CSV Injection (Formula Injection) in log files
**Learning:** Legacy systems often log to flat files or CSVs. COBOL `ACCEPT` input is raw. If not sanitized, attackers can inject spreadsheet formulas (`=`, `+`, `-`, `@`) that execute when logs are opened in Excel/Calc.
**Prevention:** Explicitly use `INSPECT ... REPLACING` to strip or replace all formula trigger characters from user input before writing to logs.
