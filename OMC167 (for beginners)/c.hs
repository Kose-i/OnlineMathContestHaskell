f :: Int -> [[Int]]
--      0,1,2,3,4,5,6,7,8,9,10,11,12,13
f 1 = [[1,0,0,0,0,0,0,0,0,0, 0, 0, 0, 0],
       [0,1,0,0,0,0,0,0,0,0, 0, 0, 0, 0]]
f x = [g!!1, (zipWith (+) ([0]++(take 13 (g!!1))) ([0]++(take 13 (g!!0))))] where g = f(x-1)

w :: Int
w = ((h!!0)!!9) + ((h!!0)!!10) + ((h!!0)!!11) + ((h!!0)!!12) +
    ((h!!1)!!9) + ((h!!1)!!10) + ((h!!1)!!11) + ((h!!1)!!12)   where h = f(13)

main = print(w)