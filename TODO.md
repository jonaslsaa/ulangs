# TODO

## Major

* Automatic code splitting (can be done in its own step)
* Finish proof-of-concept VSCode extension
* Investigate agentic behaviour using *tool-use*.
* Inter-file tree merging adapter synthesis

## Minor

* When in repair loop, all judge scoring should be in the same chat thread (`messages`).
* Investigate what happens to "Invalid line number" type errors (should be added to the 'Others' list of errors)
* Dynamically increase reasoning_effort from the second repair attempt and onwards.
* Find a good way to handle increasingly large contexts
  * Restart context sometimes?
  * Also compress assistant code blocks (only diffs? llm compression?)
* Find a way to strip out comments from code examples
* Restore to a previous checkpoint candidate if we have messed up badly
