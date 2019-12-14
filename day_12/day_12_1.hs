main = do
    let totalEnergy  = sum $ map energy $ iterate tick planets !! 1000
    print totalEnergy

-- Input
planets = [(5, -1, 5, 0, 0, 0), (0, -14, 2, 0, 0, 0), (16, 4, 0, 0, 0, 0), (18, 1, 16, 0, 0, 0)]

tick planets = map tick' $ map (\p -> foldl gravity p planets) planets
tick' (x, y, z, xv, yv, zv) = (x + xv, y + yv, z + zv, xv, yv, zv)

gravity (x1, y1, z1, xv, yv, zv) (x2, y2, z2, _, _, _) = (x1, y1, z1, xv', yv', zv')
  where
    xv' = xv + gravity' x1 x2
    yv' = yv + gravity' y1 y2
    zv' = zv + gravity' z1 z2
gravity' a b = if a < b then 1 else if a > b then -1 else 0

energy (x, y, z, xv, yv, zv) = (abs x + abs y + abs z) * (abs xv + abs yv + abs zv)
