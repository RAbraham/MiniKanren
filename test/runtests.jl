using MiniKanren
using Test
import MiniKanren:@conde, @ldisjplus, @delaygoal, ===, @fresh, @lconjplus, @runstar, @run


v0 = v(0)
v1 = v(1)
v2 = v(2)

@testset "Walk" begin
 s_map = Dict(LVar(0) => LVar(1), LVar(1) => "banana")
 @test walk(LVar(0), s_map) == "banana"
 @test walk(LVar(1), s_map) == "banana"
 @test walk("mango", s_map) == "mango"
 @test walk(LVar(2), s_map) == LVar(2)

 s_map_transitive = Dict(LVar(0) => LVar(1), LVar(1) => LVar(2))
 @test walk(LVar(0), s_map_transitive) == LVar(2)


 # TODO: Test for Cyclic Path
 # cyclic_map = Dict(LVar(0) => LVar(1), LVar(1) => LVar(0))
 # @test walk(LVar(0), cyclic_map)

end



@testset "Unify" begin
    @test unify("banana", "mango", Dict()) == nothing
    @test unify("banana", "mango", Dict(v0 => "banana")) == nothing
    @test unify("banana", v0, Dict(v0 => "mango")) == nothing

#   TODO: I keep doing the same test with inverted arguments like below. Is there an easy way of capturing that
    @test unify(v0, "banana", Dict()) == Dict(v0 => "banana")
    @test unify("banana", v0, Dict()) == Dict(v0 => "banana")
#   Same Value
    @test unify("banana", v1, Dict(v1 => "banana")) == Dict(v1 => "banana")

#   Conflicting Value
    @test unify(v0, "banana", Dict(v1 => "mango")) == Dict(v0 => "banana", v1 => "mango")

#   Fresh
    @test unify(v1, v2, Dict(v0 => v1)) == Dict(v0 => v1, v1 => v2)

#   Unify Freshies with same values
    @test unify(v0, v1, Dict(v0 => v1)) == Dict(v0 => v1)
#   Unify Freshies with conflicting values
    @test unify(v0, v2, Dict(v0 => v1)) == Dict(v0 => v1, v1 => v2)

    @test unify(v0, v1, Dict{LVar, Any}(v1 => "mango")) == Dict(v0 => "mango", v1 => "mango")
end



@testset "LazyStream" begin
    aa = ImmatureStream(() -> MatureStream(1 + 1))
    bb = ImmatureStream(() -> MatureStream(20 + 20))

    s = trampoline(mergestreams(aa, bb))
    @test s.head == 2
    @test s.next.head == 40
    @test s.next.next == EmptyStream()

end


@testset "Goals" begin
    @testset "Basic" begin
        test = (g, s, c) -> @test streamtoseq(g(emptystate)) == [State(s, c)]
        map([[===(1, 1), Dict(), 0],
             [===(v0,1), Dict(v0 => 1), 0],
             [===(1, v0), Dict(v0 => 1), 0],
             [callfresh(x -> ===(x, 1)), Dict(v0 => 1), 1]]) do args
            g = args[1]
            s = args[2]
            c = args[3]
            test(g, s, c)
        end
    end

    @testset "Composite" begin
        a = v0
        b = v1
        args = [
             [ldisj(===(a, 1), ===(2, 2)), [State(Dict(a => 1), 0), State(Dict(), 0)]],
             [ldisj(===(a, 1), ===(a, 2)), [State(Dict(a => 1), 0), State(Dict(a => 2), 0)]],
             [@ldisjplus(===(a, 1), ===(a, 2)), [State(Dict(a => 1), 0), State(Dict(a => 2), 0)]],
             [@ldisjplus(===(a, 1), ===(b, 2)), [State(Dict(a => 1), 0), State(Dict(b => 2), 0)]],
             [lconj(===(a, 1), ===(b, 2)), [State(Dict(a => 1, b => 2), 0)]],
             [@lconjplus(===(a, 1), ===(b, 2)), [State(Dict(a => 1, b => 2), 0)]],
             [lconj(===(a, 1), ===(a, 2)), []],
             [@delaygoal(===(a, 1)), [State(Dict(a => 1), 0)]],
            [@conde([===(a, 1)], [===(a, 2)]), [State(Dict(a => 1), 0), State(Dict(a => 2), 0)]],
            [@conde([===(a, 1), ===(b, 2)], [===(a, 7), ===(b, 12)]), [State(Dict(a => 1, b => 2), 0), State(Dict(a => 7, b => 12), 0)]],
            [@fresh([x], ===(x, 42)), [State(Dict(v0 => 42), 1)]],
            [@fresh([x, y], ===(x, 42), ===(y, x)), [State(Dict(v0 => 42, v1=> 42), 2)]],
            [@fresh([x, y], ===(x, 1), ===(y, 2)), [State(Dict(v0 => 1, v1=> 2), 2)]],
        ]
        map(args) do aa
            g = aa[1]
            states = aa[2]
            @test streamtoseq(g(emptystate)) == states
        end
    end

    @testset "Run" begin
        @test @runstar([q], @conde([===(q, 1)], [===(q, 7)])) == [1, 7]

        @test @run(1, [q], @conde([===(q, 1)], [===(q, 7)])) == [1]
        @test @run(2, [q], @conde([===(q, 1)], [===(q, 7)])) == [1, 7]
#         TODO: The below fails. Help.
#         fives = x -> @conde([===(x, 5)], [fives(x)])
#         @test @run(5, [q], fives(q)) == [5, 5, 5, 5, 5]
    end
end
