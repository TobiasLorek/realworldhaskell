mydrop n xs = if n <= 0 || xs == []
              then xs
              else mydrop (n-1) (tail xs)
