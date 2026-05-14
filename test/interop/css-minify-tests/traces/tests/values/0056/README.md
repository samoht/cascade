# Calc combine like terms preserving incompatible units

`calc(100% - 50px + 0%)` has both percentage and px terms. Like units are combined: `100% + 0%` = `100%`, while `-50px` remains unchanged. The result is `calc(100% - 50px)` -- calc() is preserved because % and px cannot be resolved together at specified-value time.
