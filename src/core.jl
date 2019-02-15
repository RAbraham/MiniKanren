#=
core.jl:
- Julia version: 1.0
- Author: rabraham
- Date: 2018-11-07
=#

import Base: *
export  walk, unify, LVar, v, MatureStream, ImmatureStream, EmptyStream, mergestreams, trampoline,
 ===,  deepwalk, conde, ldisjplus, emptystate, streamtoseq, State, callfresh,
 ldisj, lconj, delaygoal, fresh,  lconjplus, runstar, run

struct  LVar
 id :: Int
end

abstract type LazyStream end

struct EmptyStream <: LazyStream end

struct MatureStream <: LazyStream
    head
    next::LazyStream
    MatureStream(head) = new(head, EmptyStream())
    MatureStream(head, stream) = new(head, stream)
end

struct ImmatureStream <: LazyStream
    thunk::Function
end

struct State
    smap :: Dict
    nextid :: Int
end

Base.hash(a::State, h::UInt) = hash(a.nextid, hash(a.smap, hash(:State, h)))
Base.:(==)(a::State, b::State) = isequal(a.nextid, b.nextid) && isequal(a.smap, b.smap) && true

const emptystate = State(Dict(), 0)

# Convenience constructor
v(id::Int) = LVar(id)

function addsubstitution(s_map::Dict, lvar::LVar, value)
    if s_map != nothing
        merge(s_map, Dict(lvar => value))
    else
        s_map
    end
end

function walk(u, s_map::Dict)
    if u == nothing
        nothing
    elseif haskey(s_map, u)
        walk(s_map[u], s_map)
    else
        u
    end
end

function unify(u, v, s_map)
    final_u = walk(u, s_map)
    final_v = walk(v, s_map)
    if final_u == final_v
        s_map
    else
        unify_terms(final_u, final_v, s_map)
    end
end

unify_terms(u, v, s_map::Dict) = nothing
unify_terms(u::LVar, v, s_map::Dict) = addsubstitution(s_map, u, v)
unify_terms(u::LVar, v::LVar, s_map::Dict) = addsubstitution(s_map, u, v)
unify_terms(u, v::LVar, s_map::Dict) = addsubstitution(s_map, v, u)


# Empty Stream
mergestreams(stream1::EmptyStream, stream2::LazyStream)::LazyStream = stream2

mapcatstream(stream1::EmptyStream, func::Function)::LazyStream = stream1

realizestreamhead(stream::EmptyStream)::LazyStream = stream

streamtoseq(stream::EmptyStream)::AbstractVector = []


# Mature Stream
mergestreams(maturestream::MatureStream, otherstream::LazyStream)::LazyStream =
    MatureStream(maturestream.head,
                 mergestreams(maturestream.next,
                              otherstream))

mapcatstream(stream1::MatureStream, func::Function)::LazyStream =
    mergestreams(func(stream1.head), mapcatstream(stream1.next, func))

realizestreamhead(stream::MatureStream)::LazyStream = stream

streamtoseq(stream::MatureStream)::AbstractVector = append!([stream.head], streamtoseq(stream.next))


# Immature Streams
mergestreams(this::ImmatureStream, otherstream::LazyStream)::LazyStream =
    ImmatureStream(() -> mergestreams(otherstream, this.thunk()))

mapcatstream(this::ImmatureStream, func::Function)::LazyStream =
    ImmatureStream(() -> mapcatstream(this.thunk(), func))

realizestreamhead(this::ImmatureStream)::LazyStream = trampoline(this)

streamtoseq(this::ImmatureStream)::AbstractVector = streamtoseq(realizestreamhead(this))

trampoline(stream::ImmatureStream) = trampoline(stream.thunk())
trampoline(value) = value

# Goal Constructors
function ===(u, v)
    function unifygoal(state :: State)
        smapprime = unify(u, v, state.smap)
        if smapprime != nothing
            MatureStream(State(smapprime, state.nextid))
        else
            EmptyStream()
        end
    end
end


ldisj(goal1, goal2) = (state) -> mergestreams(goal1(state), goal2(state))

macro ldisjplus(goal)
    :(@delaygoal($(esc(goal))))
end

macro ldisjplus(goal, goals...)
    egoals = map(esc, goals)
    return quote
        dg = @delaygoal($(esc(goal)))
        ldisj(dg, @ldisjplus($(egoals...)))
    end
end


lconj(goal1, goal2) = (state) -> mapcatstream(goal1(state), goal2)


function callfresh(goalconstructor)
    function freshgoal(state :: State)
        goal = goalconstructor(LVar(state.nextid))
        newstate = State(state.smap, state.nextid + 1)
        goal(newstate)
    end
end



macro runstar(freshvarvec, goals...)

    escgoals = escargs(goals)

    return quote
        freshed = @fresh($(freshvarvec), $(escgoals...))
        goaled = callemptystate(freshed)
        streamed = streamtoseq(goaled)
        map(reifystatefirstvar, streamed)
    end
end


macro fresh(varvec, clauses...)

    escclauses = escargs(clauses)
    if in(:head, fieldnames(typeof(varvec))) && varvec.head == :vect
        v = varvec.args
    elseif typeof(varvec) == Vector{Any}
        v = varvec
    else

        throw(ArgumentError("$(varvec) should be a vector"))
    end

    if isempty(v)
        :(@lconjplus($(clauses...)))
    else
        return quote
            fnfresh = $(v[1]) -> @fresh($(v[2:end]), $(clauses...))
            callfresh(fnfresh)
        end
    end
end

macro lconjplus(goal)
#     TODO: duplicate with ldisjplus
    :(@delaygoal($(esc(goal))))
end

macro lconjplus(goal, goals...)
# TODO: duplicate with ldisjplus
    return quote
        dg = @delaygoal($(esc(goal)))
        lconj(dg, @lconjplus($(esc(goals...))))
    end
end


macro delaygoal(goal)
    return quote
        function delayedgoalouter(state)
            ImmatureStream(() -> $(esc(goal))(state))
        end
    end
end

macro conde(clauselistexprs...)
    conjify = clauses -> :(@lconjplus($(clauses...)))
    escapify = args -> map(esc, args)

    clauseargslist = [x.args for x in clauselistexprs]
    esclistlist = map(escapify, clauseargslist)
    conjlist = map(conjify, esclistlist)
    :(@ldisjplus($(conjlist...)))
end

callemptystate(goal) = goal(emptystate)

function reifysstar(v::LVar, smap)
    n = reifyname(length(smap))
    addsubstitution(smap, v, n)
end

reifysstar(v, smap) = smap

function deepwalk(v, smap)
    walked = walk(v, smap)
    deepwalkstar(walked, smap)
end

deepwalkstar(v, smap) = v

function reifystatefirstvar(state)
    v = deepwalk(LVar(0), state.smap)
    deepwalk(v, reifys(v, Dict()))
end

reifyname(n) = Symbol(string("_."), string(n))

reifys(v, smap) = reifysstar(walk(v, smap), smap)

escargs(args) = map(esc, args)


macro run(n, freshvarvec, goals...)
    escgoals = goals

    return quote
        arr = @runstar($(freshvarvec), $(escgoals...))
        arr[1:$n]
    end
end
