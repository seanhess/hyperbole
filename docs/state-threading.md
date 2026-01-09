The simplest way to add state to a `HyperView` is to pass it back and forth between the `Action` and the `View`. In this implementation of the classic counter example, each constructor of the `Action` expects an `Int`, which represents the current count:

    #EMBED Example.Counter instance HyperView Counter

Our `View` Function also expects the current count as an input. It includes it in each `Action`, and the cycle continues:

    #EMBED Example.Counter viewCount
