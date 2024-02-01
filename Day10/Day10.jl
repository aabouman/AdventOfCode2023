import LinearAlgebra: cross
import Base: Base.(!), Base.(==)
@enum Direction begin
    East
    West
    North
    South
end
function (d::Direction)()
    d == East && return (0,1)
    d == West && return (0,-1)
    d == North && return (-1,0)
    return (1,0)
end
function (!)(d::Direction)
    d == East && return West
    d == West && return East
    d == North && return South
    return North
end

abstract type AbstractPipe end
struct Start <: AbstractPipe end
struct Pipe <: AbstractPipe
    end1::Direction
    end2::Direction
end
(==)(p1::Pipe, p2::Pipe) = (p1.end1 == p2.end1 && p1.end2 == p2.end2) || (p1.end1 == p2.end2 && p1.end2 == p2.end1)
struct Ground <: AbstractPipe end
const Sketch = Matrix{AbstractPipe}
struct State
    location::CartesianIndex{2}
    heading::Direction
    pipe::AbstractPipe
end

canTravelPipe(::Start, ::Direction) = True
canTravelPipe(::Ground, ::Direction) = False
canTravelPipe(p::Pipe, heading::Direction) = heading == !p.end1 || heading == !p.end2

headingThroughPipe(::Start, heading::Direction) = heading
headingThroughPipe(::Ground, ::Direction) = Nothing
function headingThroughPipe(p::Pipe, heading::Direction)
    !canTravelPipe(p, heading) && return Nothing
    heading == !p.end1 && return p.end2
    return p.end1
end

function step(sketch::Sketch, s::State)
    newLocation = s.location + CartesianIndex{2}(s.heading())
    (!checkbounds(Bool, sketch, newLocation)) && return Nothing
    newPipe = sketch[newLocation]
    newHeading = headingThroughPipe(newPipe, s.heading)
    (newHeading == Nothing) && return Nothing
    return State(newLocation, newHeading, newPipe)
end

const pipeDict = Dict{Char, AbstractPipe}(
    '|' => Pipe(North, South),
    '-' => Pipe(East, West),
    'L' => Pipe(North, East),
    'J' => Pipe(North, West),
    '7' => Pipe(South, West),
    'F' => Pipe(South, East),
    '.' => Ground(),
    'S' => Start(),
);

function parseSketch(file_path)::Matrix{AbstractPipe}
    char_matrix = [collect(line) for line in readlines(file_path)]
    n, m = (length(char_matrix), length(char_matrix[1]))
    return [pipeDict[char_matrix[i][j]] for i in 1:n, j in 1:m]
end

@enum PathOrientation begin
    Clockwise
    CounterClockwise
end
const Path = Vector{State}

# struct Path
#     path::Vector{State}
    # orientation::PathOrientation

    # function Path(path::Vector{State})
    #     function helper(point1, point2)
    #         (x1, y1) = Tuple(point1.location)
    #         (x2, y2) = Tuple(point2.location)
    #         return (x2 - x1) * (y2+y1)
    #     end
    #     sum = helper(path[end], path[1])
    #     for i = 1:(length(path)-1)
    #         sum += helper(path[i], path[i+1])
    #     end
    #     new(path, sum > 0 ? Clockwise : CounterClockwise)
    # end
# end

function findPath(sketch::Sketch)::Path
    function findPathHelp(sketch, startState::State)
        s1 = startState
        vec = [s1]
        while (true)
            s2 = step(sketch, s1)
            s2 == Nothing && return Nothing               # Incomplete loop base case.
            sketch[s2.location] == Start() && return vec  # Back at start base case.
            push!(vec, s2)
            s1 = s2
        end
    end

    startInd = findfirst(s->s == Start(), sketch)
    startState = map(x->State(startInd, x, sketch[startInd]), [North,South,East,West])
    paths = map(x->findPathHelp(sketch, x), startState)
    return argmin(length, filter!(x-> x!=Nothing, paths))
end

# %%
sketch = parseSketch(joinpath(@__DIR__, "data", "test1.txt"))
path = findPath(sketch)
length(path.path) ÷ 2

# %%
function isConcavePipe(path::Path, state::State)
    pipeBend = (Pipe(West, North), Pipe(North, East), Pipe(East, South), Pipe(South, North))
    isPipeBend = state.pipe in cornerPipes

    row_lb, col_lb = Tuple(minimum(x->x.location, path))
    row_ub, col_ub = Tuple(maximum(x->x.location, path))
    isNotCornerPipe = row_lb < state.location[1] < row_ub && row_lb < state.location[2] < row_ub

    return isPipeBend && isNotCornerPipe
end

