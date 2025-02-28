%% This is an example. Feel free to copy and reuse as you wish.

-module(matrix3d).
-export([rotate_x/1, rotate_y/1, rotate_z/1, multiply/2, apply/2]).

% Rotation matrix around X-axis (angle in radians)
rotate_x(Angle) ->
    Cos = math:cos(Angle),
    Sin = math:sin(Angle),
    [[1, 0,    0   ],
     [0, Cos, -Sin ],
     [0, Sin,  Cos]].

% Rotation matrix around Y-axis
rotate_y(Angle) ->
    Cos = math:cos(Angle),
    Sin = math:sin(Angle),
    [[ Cos,  0, Sin ],
     [ 0,    1, 0   ],
     [-Sin,  0, Cos]].

% Rotation matrix around Z-axis
rotate_z(Angle) ->
    Cos = math:cos(Angle),
    Sin = math:sin(Angle),
    [[Cos, -Sin, 0 ],
     [Sin,  Cos, 0 ],
     [0,    0,   1]].

% Multiply two 3x3 matrices
multiply(A, B) ->
    [[A11, A12, A13],
     [A21, A22, A23],
     [A31, A32, A33]] = A,
    [[B11, B12, B13],
     [B21, B22, B23],
     [B31, B32, B33]] = B,
    [[A11*B11 + A12*B21 + A13*B31, A11*B12 + A12*B22 + A13*B32, A11*B13 + A12*B23 + A13*B33],
     [A21*B11 + A22*B21 + A23*B31, A21*B12 + A22*B22 + A23*B32, A21*B13 + A22*B23 + A23*B33],
     [A31*B11 + A32*B21 + A33*B31, A31*B12 + A32*B22 + A33*B32, A31*B13 + A32*B23 + A33*B33]].

% Apply a 3x3 matrix to a 3D vertex {X, Y, Z}
apply(Matrix, {X, Y, Z}) ->
    [[M11, M12, M13],
     [M21, M22, M23],
     [M31, M32, M33]] = Matrix,
    NewX = M11 * X + M12 * Y + M13 * Z,
    NewY = M21 * X + M22 * Y + M23 * Z,
    NewZ = M31 * X + M32 * Y + M33 * Z,
    {NewX, NewY, NewZ}.
