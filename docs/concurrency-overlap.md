By default, if an `Action` triggers for a particular `HyperView` when one is already being processed, the system will `Drop` the newer action without running it. This prevents destructive actions from accidentally happening more than once.

However, some user inputs only load data and overlap many times, such as an autocomplete or mouse events. In these cases, it can be better set the `Concurrency` of the `HyperView` to `Replace`. This will cancel the currently running action, and immediately run the new one instead.

    #EMBED Example.Concurrency.Overlap instance (Debug :> es) => HyperView OverlapReplace

See [[/data/autocomplete]] for a complete example
