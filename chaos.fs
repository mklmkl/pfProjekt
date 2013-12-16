(* first try*)

open System
open System.Drawing
open System.Drawing.Imaging

let generate x (dice:Random) = 
  (fun i j -> dice.NextDouble()) |> Array2D.init x x;;
  
let choose_inter a =
  match a with
    |1 -> fun l r x -> (1.0 - x) * l + x * r //linear
    |_ -> fun l r x -> ((2.0 * l - 2.0 * r) * x * x * x + (3.0 * r - 3.0 * l) * x * x + l);;
  
let quadra i j x y (board:float[,]) layers =
  let rec inside layer num =
    match layer with
    | 0 -> []
    | x -> 
      let xl = if num = 0 then i else i / num * num
      let yl = if num = 0 then j else j / num * num
      let xm = xl + num
      let ym = yl + num
      let nextnum = if num = 0 then 1 else num * 2
      let xproportion = if num = 0 then 0.0 else float(i % num) / (float num)
      let yproportion = if num = 0 then 0.0 else float(j % num) / (float num)
      (board.[xl, yl], board.[xm, yl], board.[xl, ym], board.[xm, ym], xproportion, yproportion)::(inside (layer - 1) nextnum)
  inside layers 0;;
  
let count inter list =
  let rec inside list acc =
    match list with
    |[] -> acc
    |(lg, pg, ld, pd, xp, yp)::rest -> 
      let a = inter lg ld yp
      let b = inter pg pd yp
      let c = inter a b xp
      inside rest (c::acc)
  inside list [];;
  
let sum list =
  let rec inside list acc max now =
    match list with
    |[]   -> acc / max
    |h::t -> inside t (acc + h * now) (max + now) (now / 2.0)
  inside list 0.0 0.0 1.0;;
  
let color min_color max_color proportion =
  let (ir, ig, ib) = min_color
  let (ar, ag, ab) = max_color
  //printf "\n%f\n" proportion
  let r = int((1.0 - proportion) * (float ir) + proportion * (float ar))
  let g = int((1.0 - proportion) * (float ig) + proportion * (float ag))
  let b = int((1.0 - proportion) * (float ib) + proportion * (float ab))
  Color.FromArgb(r, g, b);;
  
let make_noise board inter (x:int) (y:int) min_color max_color layers =
  let bitmap = new Bitmap(x, y, PixelFormat.Format32bppArgb)
  for i in 0..(x - 1) do
      for j in 0..(y - 1) do
        bitmap.SetPixel(i, j, quadra i j x y board layers |> count inter |> sum |> color min_color max_color)
  bitmap;;
  
let main =
  let dice  = new Random()
  let board = generate 513 dice
  let interpolation = choose_inter 2
  let bitmap = make_noise board interpolation 512 512 (0, 0, 0) (255, 255, 255) 8
  ("/home/izrafel/Dokumenty/Funkcyjne/Projekt/chaos.bmp", ImageFormat.Bmp) |> bitmap.Save;;
  
