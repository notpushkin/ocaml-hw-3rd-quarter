let rec bin_find a y = 
  let half b =
    let len = (Array.length b) / 2 in
    let c = Array.make len (b.[0]) in
    let d = Array.make len (b.[0]) in
    Array.blit b 0 c 0 len;
    Array.blit b len d 0 len;
    (c, d)
  in

  if a.[x] < y then
    bin_find (fst (half a)) y
  else if a.[x] = y then
    x
  else
    ((Array.length a) / 2) + 
    (bin_find (snd (half a)) y);;