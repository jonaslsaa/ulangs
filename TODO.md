# TODO

## Major

* Implement automatic prolog generation
  * Find a good verifier method (Prolog errors + LLM as a verifier on output?)
* Automatic code splitting (can be done in its own step)
* Finish proof-of-concept VSCode extension
* Investigate agentic behaviour using *tool-use*.

## Minor

* Investigate what happens to "Invalid line number" type errors (should be added to the 'Others' list of errors)
* Dynamically increase reasoning_effort from the second repair attempt and onwards.
* Find a good way to handle increasingly large contexts
  * Restart context sometimes?
  * Also compress assistant code blocks (only diffs? llm compression?)
* Way to strip out comments from code
* Restore to a previous checkpoint candidate if we have messed up badly
