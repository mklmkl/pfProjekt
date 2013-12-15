(* first try*)

open System
open System.Drawing
open System.Drawing.Imaging

let generate x (dice:Random) = 
  (fun i j -> dice.NextDouble()) |> Array2D.init x x;;
  
let choose_inter a =
  match a with
    _ -> fun l r x h -> (1.0 - x / h) * l + x * r;; //linear
    
let count (board:float[,]) x y inter min_color max_color layers =
  let (ir, ig, ib) = min_color
  let (ar, ag, ab) = max_color
  let rec sum layer num =
    let complicated =
      let xl = if x % num = 0 then x else x / num * num
      let yl = if y % num = 0 then y else y / num * num
      let xm = if x % num = 0 then x else (x + num) / num * num
      let ym = if y % num = 0 then y else (y + num) / num * num
      let a = inter board.[xl, yl] board.[xl, ym] (float y) num
      let b = inter board.[xm, yl] board.[xm, ym] (float y) num
      let c = inter a b (float x) num
      c / num + (sum (layer + 1) (2.0 * num))
    match layer with
    | e when e > layers -> 0
    | e -> complicated
  let sth = (sum layers 1.0) / (2.0 - 2.0 ** (float layers-1.0))
  let r = (1.0 - sth) * (float ir) + sth * (float ar)
  let g = (1.0 - sth) * (float ig) + sth * (float ag)
  let b = (1.0 - sth) * (float ib) + sth * (float ab)
  Color.FromArgb(r, g, b);;
  
let make_noise board inter (x:int) (y:int) min_color max_color layers =
  let bitmap = new Bitmap(x, y, PixelFormat.Format32bppArgb)
  for i in 0..(x - 1) do
      for j in 0..(y - 1) do
        bitmap.SetPixel(i,j,count board i j inter min_color max_color layers)
  bitmap;;
  
let main = 
  let dice  = new Random()
  let board = generate 512 dice
  let interpolation = choose_inter 1
  let bitmap = make_noise board interpolation 512 512 (0, 0, 0) (255, 255, 255) 1
  ("/home/izrafel/Dokumenty/Funkcyjne/Projekt/chaos.bmp", ImageFormat.Bmp) |> bitmap.Save;;
  