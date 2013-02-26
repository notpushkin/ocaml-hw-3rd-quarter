let find_root a1 b1 a2 b2 =
  if a1=0 && a2=0 then 
    if b1<=0 && b2<=0 then
      -1
    else
      0
  else if a1=0 && a2<>0 then
    if b1<=0 then
      -1
    else
      0
  else if a1<>0 && a2=0 then
    if b2<=0 then
      -1
    else
      0
  else if a1<0 && a2<0 then
    -1
  else if a1<0 && a2>0 then
    if b1/a1 > b2/a2 then
      ((b1/a1) - (b2/a2)+1)
    else
      0
  else if a1>0 && a2<0 then
    if b2/a2 > b1/a1 then
      (b2/a2 - b1/a1 +1) 
    else 0
  else if a1>0 && a2>0 then
    -1
  else
    0;;