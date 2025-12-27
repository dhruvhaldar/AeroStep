## 2024-05-23 - Legacy COBOL Security Controls
**Vulnerability:** Missing Authentication and Input Masking in Terminal Applications
**Learning:** Legacy COBOL applications often lack built-in security features like authentication or password masking. When adding these, standard `ACCEPT` statements echo input to the screen.
**Prevention:** Use ANSI escape codes (e.g., `[8m` for conceal) to simulate password masking in terminal environments where `SCREEN SECTION` or Curses libraries are not fully utilized or behave inconsistently. Additionally, utilize Environment Variables to inject secrets rather than hardcoding credentials in the source.

## 2024-12-27 - CSV Injection (Formula Injection)
**Vulnerability:** Unsanitized user input written to CSV-formatted log files allowed characters like `=`, `+`, `-`, and `@` to persist.
**Learning:** Even simple text log files can be vectors for attacks if they are intended to be opened by spreadsheet software (CSV Injection). Standard text sanitization often overlooks these formula triggers.
**Prevention:** Explicitly filter or escape characters that trigger spreadsheet formulas (`=`, `+`, `-`, `@`) when writing user-supplied data to CSV logs. In COBOL, `INSPECT ... REPLACING` is an effective way to neutralize these characters.
