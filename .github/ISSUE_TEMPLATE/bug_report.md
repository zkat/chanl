---
name: Bug report
about: Report failures and unexpected behavior
title: "[BUG] Failure XYZ when running test suite"
labels: ''
assignees: ''

---

**Describe the bug**
It is best to isolate the bug, although if you only have the failure of some large system, reporting that can help begin the process of isolation.

**To Reproduce**
Steps to reproduce the behavior:
1. Go to '...'
2. Click on '....'
3. Scroll down to '....'
4. See error

**Expected behavior**
A clear and concise description of what you expected to happen.

**Screenshots or Error Logs**
Text is preferable over screenshots, unless you edit the screenshot to highlight the failure.

**System (please complete the following information):**
 - Hardware: [e.g. x86_64, ...]
 - OS: [e.g. iOS]
 - Compiler [e.g. sbcl, ccl, ...]
 - Version [e.g. SBCL 2.3.4, CCL 12, ...]

**Test Suite**
If possible, please include the output from running the test suite on your target system. Note that the test suite might not catch your specific error, and/or might catch different errors than the one you found. It can be invoked using `(asdf:test-system "chanl")`.
