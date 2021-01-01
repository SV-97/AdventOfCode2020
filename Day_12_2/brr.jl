raw = open(f->read(f, String), "input.txt")
shipLocation = [0; 0]
shipOrientation = [1; 0]
waypoint = [10; 1];

N = [0; 1];
S = [0; -1];
E = [1; 0];
W = [-1; 0];
L = [0 -1;
     1 0];
R = [0 1;
    -1 0];

function f((shipL, shipO, waypoint), s)
    action = s[1]
    n = parse(Int64, s[2:end])
    
    if action == 'N'
        (shipL, shipO, waypoint + n * N)
    elseif action == 'S'
        (shipL, shipO, waypoint + n * S)
    elseif action == 'E'
        (shipL, shipO, waypoint + n * E)
    elseif action == 'W'
        (shipL, shipO, waypoint + n * W)
    elseif action == 'F'
        (shipL + n * (waypoint - shipL), shipO, waypoint + n * (waypoint - shipL))
    elseif action == 'L'
        k = div(n, 90)
        (shipL, shipO, L^k * (waypoint - shipL) + shipL)
    elseif action == 'R'
        k = div(n, 90)
        (shipL, shipO, R^k * (waypoint - shipL) + shipL)
    end
end

manhattanNorm(x, y) = abs.(x - y)' * ones(size(x))

println(foldl(f, split(raw); init=(shipLocation, shipOrientation, waypoint)) |> (ship -> manhattanNorm(ship[1], [0; 0])))
