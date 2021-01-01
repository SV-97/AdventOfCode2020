raw = open(f->read(f, String), "input.txt")
shipLocation = [0; 0]
shipOrientation = [1; 0]

N = [0; 1];
S = [0; -1];
E = [1; 0];
W = [-1; 0];
L = [0 -1;
     1 0];
R = [0 1;
    -1 0];

function f((shipL, shipO), s)
    action = s[1]
    n = parse(Int64, s[2:end])
    
    if action == 'N'
        (shipL + n * N, shipO)
    elseif action == 'S'
        (shipL + n * S, shipO)
    elseif action == 'E'
        (shipL + n * E, shipO)
    elseif action == 'W'
        (shipL + n * W, shipO)
    elseif action == 'F'
        (shipL + n * shipO, shipO)
    elseif action == 'L'
        k = div(n, 90)
        (shipL, L^k * shipO)
    elseif action == 'R'
        k = div(n, 90)
        (shipL, R^k * shipO)
    end
end

manhattanNorm(x, y) = abs.(x - y)' * ones(size(x))

println(foldl(f, split(raw); init=(shipLocation, shipOrientation)) |> (ship -> manhattanNorm(ship[1], [0; 0])))
