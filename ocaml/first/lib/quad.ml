
type t = { min : int; max : int; tot : int; count : int }

let single x = { min = x; max = x; tot = x; count = 1 }

let combine =
  fun
    {min=min1;max=max1;tot=tot1;count=count1}
    {min=min2;max=max2;tot=tot2;count=count2}
  ->
  let _ = min1,min2,max1,max2,tot1,tot2,count1,count2 in
  let min = min min1 min2 in
  let max = max max1 max2 in
  let tot = tot1 + tot2 in
  let count = count1 + count2 in
  {min;max;tot;count}

let min_mean_max {min;max;tot;count} =
  let f = Float.of_int tot /. 10. /. Float.of_int count *. 10. in
  let mean = Float.to_int (Float.floor (f +. 0.5)) in
  (min,mean,max)
