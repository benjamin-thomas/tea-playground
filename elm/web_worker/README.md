# Playing with shared worker

The worker can be launched via nodejs, or the browser.

When 2 tabs or windows connect to the same domain, then they connect to a unique shared worker.

This shared worker handles its own state (an increasing counter that can be reset).

This allows all tabs to be synchronized with a specific piece of state!
