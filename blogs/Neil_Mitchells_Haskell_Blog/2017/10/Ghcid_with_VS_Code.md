# Ghcid with VS Code

*Summary: New versions of Ghcid and the VS Code extension work even better together.*

I've just released [Ghcid v0.6.8](https://hackage.haskell.org/package/ghcid) and the associated VS Code extension [haskell-ghcid v0.2.0](https://marketplace.visualstudio.com/items?itemName=ndmitchell.haskell-ghcid). Together they vastly simplify the Ghcid VS Code experience.

## Ghcid reads .ghcid files

A new feature in Ghcid is that if there is a `.ghcid` file in the current directory it will load it as additional arguments. For example, in the Shake repo I have [a .ghcid file](https://github.com/ndmitchell/shake/blob/master/.ghcid):

```
-c "ghci -fno-code -ferror-spans"
```

Which tells `ghcid` to not guess at the command (e.g. using `stack` if you have a `.stack-work`) but always run `ghci -fno-code -ferror-spans`. This command works because I have [a .ghci file](https://github.com/ndmitchell/shake/blob/master/.ghci) which loads all the necessary files, while `-fno-code` speeds up compilation and `-ferror-spans` gives better error highlighting.

## Ghcid VS Code starts ghcid

A new feature in the VS Code extension is the action `Start Ghcid` which starts a new `ghcid` terminal, writes the output to a temporary file, and uses that output to populate the Problems pane. Importantly, it runs `ghcid` with no additional arguments, so having a sensible `.ghcid` file is important.

The effect of these changes is that to start `ghcid` in VS Code is now a few key strokes, whereas before it required special flags, opening files, running commands etc.
