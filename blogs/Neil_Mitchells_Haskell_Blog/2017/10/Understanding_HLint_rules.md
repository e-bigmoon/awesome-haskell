# Understanding HLint rules

*Summary: I added a degenerate foldr to map rule in the new version of HLint, here I describe how it works.*

I've just released [HLint 2.0.10](https://hackage.haskell.org/package/hlint-2.0.10), which includes a rule to recognise uses of `foldr` that should really be `map`. As an example:

`foldr (\curr acc -> (+1) curr : acc) []`

Can be rewritten as:

`map (\curr -> (+1) curr)`

Which is much more readable (and then subsequently HLint will suggest `map (+1)`, which is vastly clearer than the initial foldr). The change required to HLint was to add a rule to the [hlint.yaml](https://github.com/ndmitchell/hlint/blob/master/data/hlint.yaml) saying:

- `warn: {lhs: "foldr (\\c a -> x : a) []", rhs: "map (\\c -> x)"}`

You can read this statement as saying if you see `foldr (\c a -> x : a) []`, suggest `map (\c -> x)` as a warning. The HLint matching engine then applies that template to every subexpression in your program. In the rest of the post I'll talk through the steps HLint performs.

## Step 1: Unification

The first step is to try unifying the template `foldr (\c a -> x : a) []` against the users subexpression, namely `foldr (\curr acc -> (+1) curr : acc) []`. HLint is trying to find assignments for the single-letter variables in the template (namely `c`, `a` and `x`) which cause it to match the subexpression. Unification proceeds top-down, and if it finds anything concrete that does not match (e.g. the user had written `foldl`) then it fails. In this case the unification succeeds with the bindings:

- `c = curr` (from the first argument to the lambda)
- `a = acc` (from the second argument to the lambda)
- `x = (+1) curr` (from before the cons)
- `a = acc` (from after the cons)

An example of a subexpression that would have failed unification is `foldl (\curr acc -> (+1) curr : acc) []`.

## Step 2: Validity

The next step is to check that any value which has been bound more than once is equal in all bindings. In our case only a has been used twice, and it always binds to `acc`, so the unification is valid.

An example of a subexpression that would have failed validity is `foldr (\curr acc -> (+1) curr : xs) []`.

## Step 3: Substitution

Now we've got some bindings, we can substitute them into the RHS, namely `map (\c -> x)`. We replace `c` and `x` using the bindings above. Note that a isn't mentioned on the RHS, so we don't use it. After substitution we get:

`map (\curr -> (+1) curr)`

## Step 4: Free variable check

Consider the expression `foldr (\curr acc -> f acc : acc) []`. Using the rules above we'd end up with `map (\curr -> f acc)`, which is terrible, since we've gone from referring to a locally bound `acc` to whatever `acc` is in scope (if any). To solve that, we check that the result doesn't introduce any new free variables:

``(freeVars result \\ freeVars hintRuleRHS) `isSubsetOf` freeVars original``

Specifically any free variables introduced in the result, which weren't in the RHS (excluding the fake unification variables), must have been in the original subexpression.

With that, for `foldr`, we're done. There are a handful of other steps that apply in some cases.

## Step A: Dot expansion in the template

If you write a hint `map f (map g x) ==> map (f . g) x` then HLint notices that also implies the rule `map f . map g ==> map (f . g)` and adds it. As a result, you shouldn't write your HLint rules in point-free style.

## Step B: Dot/dollar expansion in the subexpression

When matching a subexpression HLint will expand `f $ x` and `(f . g) x` if doing so results in a match. These operators are used commonly enough that they are often treated more like brackets than functions.

## Step C: Scope matching

When unifying qualified function names, HLint uses the active imports to guess whether they match. If you have `import qualified Data.Vector as V` then the subexpression `V.length` will unify with `Data.Vector.length`. Since HLint doesn't have complete import information it uses a few heuristics to figure out matching.

## Step D: Scope moving

Similarly to scope matching on the LHS of a rule, after matching, HLint tries to requalify any necessary values on the RHS. As an example, assuming we are producing `Data.Vector.null`, if we know about `import qualified Data.Vector as V` then we suggest `V.null`.

## Full code

To see the full code and all supporting definitions go to [the HLint source](https://github.com/ndmitchell/hlint/blob/f4466eed8a8bf6beccfd11052f2e3cfb074f2b44/src/Hint/Match.hs#L100-L114), which defines `matchIdea` - here I show a gently simplified version. Given scope information, a rule (LHS and RHS) and a subexpression, we optionally produce a resulting expression after substitution.

```haskell
matchIdea :: Scope -> HintRule -> Exp_ -> Maybe Exp_
matchIdea s HintRule{..} original = do
    u <- unifyExp hintRuleLHS original
    u <- validSubst u
    -- need to check free vars before unqualification, but after subst (with e)
    -- need to unqualify before substitution (with res)
    let result = substitute u hintRuleRHS
    guard $ (freeVars result Set.\\ Set.filter (not . isUnifyVar) (freeVars hintRuleRHS))
            `Set.isSubsetOf` freeVars original
        -- check no unexpected new free variables
    return result
```
