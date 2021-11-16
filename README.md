* Julia Implementation of MicroKanren(Yes, it says MiniKanren, I'll reach there eventually in a year.)
* For learning purposes only.


### TODO
* The following code does not work

```julia
 fives = x -> @conde([===(x, 5)], [fives(x)])
 @test @run(5, [q], fives(q)) == [5, 5, 5, 5, 5]

```

## Siqi Trial
