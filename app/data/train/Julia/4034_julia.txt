module JuliaFEM

type Beam
    L::Float64
    rhoA::Float64
    EI::Float64
    Beam(l, ra, ei) = new(l, ra, ei)
end


function mass_matrix(beam::Beam)
    a = beam.L / 2
    beam.rhoA * a / 105 * [78. 22  27 -13;
                           22   8  13  -6;
                           27  13  78 -22;
                          -13  -6 -22   8]
end


function stiff_matrix(beam::Beam)
    a = beam.L / 2

    beam.EI/(2*a^3) * [3.  3a  -3  3a;
                       3a  4a^2 -3a 2a^2;
                       -3  -3a  3 -3a;
                       3a 2a^2 -3a 4a^2]
end


function rhs_function(beam, p)
    a = beam.L / 2
    [p*a, p*a*a/3, p*a, -p*a*a/3]
end



type Beam2d
    L::Float64
    rhoA::Float64
    EI::Float64
    EA::Float64
    Beam(l, ra, ei, ea) = new(l, ra, ei, ea)
end


function mass_matrix(b::Beam2d)
    a = b.L/2
    b.rho*b.A*a/105.0 * [70.0  0   0  35  0  0;
                         0    78 22a   0 27 -13a;
                         0  22a 8a^2 0 13a -6a^2;
                         35 0     0   70  0  0;
                         0  27  13a  0  78 -22a;
                         0 -13a -6a^2 0 -22a 8a^2]
end

function stiff_matrix(b::Beam2d)
    a = b.L/2
    r2 = b.EI / b.EA

    f = a^2 / r2

    b.E*b.I/(2a^3) * [ f  0  0 -f  0  0;
                       0  3 3a  0 -3 3a;
                       0 3a 4a^2 0 -3a 2a^2;
                      -f  0  0  f  0   0;
                       0 -3  -3a  0  3 -3a;
                       0 3a 2a^2 0 -3a 4a^2]
end




end
