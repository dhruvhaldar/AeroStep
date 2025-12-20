## 2024-05-23 - Legacy COBOL Security Controls
**Vulnerability:** Missing Authentication and Input Masking in Terminal Applications
**Learning:** Legacy COBOL applications often lack built-in security features like authentication or password masking. When adding these, standard `ACCEPT` statements echo input to the screen.
**Prevention:** Use ANSI escape codes (e.g., `[8m` for conceal) to simulate password masking in terminal environments where `SCREEN SECTION` or Curses libraries are not fully utilized or behave inconsistently. Additionally, utilize Environment Variables to inject secrets rather than hardcoding credentials in the source.
