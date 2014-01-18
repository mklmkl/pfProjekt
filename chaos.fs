(* first try*)

open System
open System.Drawing
open System.Drawing.Imaging
open System.Windows.Forms

let generate x (dice:Random) = 
  (fun i j -> dice.NextDouble()) |> Array2D.init x x;;
  
let choose_inter a =
  match a with
    |0 -> fun l r x -> (r - l) * x + l //linear
    |1 -> fun l r x -> (r - l) * sin (3.14 * x / 2.0) + l//sinusoidal
    |2 -> fun l r x -> (r - l) * (3.0 * x * x - 2.0 * x * x * x) + l //polynomial 3rd degree
    |_ -> fun l r x -> (r - l) * (6.0 * x * x * x * x * x - 15.0 * x * x * x * x + 10.0 * x * x * x) + l;; //polynomial 5th degree
   
let color minColor maxColor proportion =
  let (ir, ig, ib) = minColor
  let (ar, ag, ab) = maxColor
  let r = int((1.0 - proportion) * (float ir) + proportion * (float ar))
  let g = int((1.0 - proportion) * (float ig) + proportion * (float ag))
  let b = int((1.0 - proportion) * (float ib) + proportion * (float ab))
  Color.FromArgb(r, g, b);;
  
let quadra i j x y (board:float[,]) layers mode =
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
      match mode with
      |0 ->
        match xproportion > yproportion with
        |true  -> 
          [board.[xl, ym]; board.[xm, yl]; board.[xm, ym]; xproportion; yproportion]::(inside (layer - 1) nextnum)
        |false -> 
          [board.[xl, ym]; board.[xm, yl]; board.[xl, yl]; xproportion; yproportion]::(inside (layer - 1) nextnum)
      |_ ->
        [board.[xl, yl]; board.[xm, yl]; board.[xl, ym]; board.[xm, ym]; xproportion; yproportion]::(inside (layer - 1) nextnum)
  inside layers 0;;
  
let count mode inter list =
  let rec inside list acc =
    match mode with
    |1 ->
      match list with
      |[] -> acc
      |[lg; pg; ld; pd; xp; yp]::rest -> 
        let a = inter lg ld yp
        let b = inter pg pd yp
        let c = inter a b xp
        inside rest (c::acc)
      |_ -> []
    |_ ->
      match list with
      |[] -> acc
      |[lp; pp; o; xp; yp]::rest -> ////////////////////////////////////////////////
        let a = inter lp pp yp
        let b = inter o  pp yp
        let c = inter a b xp
        inside rest (c::acc)
      |_ -> []
  inside list [];;
  
let sum list =
  let rec inside list acc max now =
    match list with
    |[]   -> acc / max
    |h::t -> inside t (acc + h * now) (max + now) (now / 2.0)
  inside list 0.0 0.0 1.0;;
  
let make_noise board inter (x:int) (y:int) min_color max_color layers mode =
  let bitmap = new Bitmap(x, y, PixelFormat.Format32bppArgb)
  for i in 0..(x - 1) do
      for j in 0..(y - 1) do
        bitmap.SetPixel(i, j, quadra i j x y board layers mode |> count mode inter |> sum |> color min_color max_color)
  bitmap;;
  
let mutable (dice:Random) = new Random()
let mutable (seed:int) = dice.Next()
dice <- new Random(seed)

let mutable board = generate 0 dice

let mutable (bitmap:Bitmap) = null

let mutable initiated = false

let mutable mode = -1

//forms start

//helpForm start

let helpForm = new Form()
helpForm.Visible <- false
helpForm.Text <- "Help"

let helpText = new Label()
helpText.Text <- "There should be something heplful"
helpForm.Controls.Add helpText |> ignore

let helpButton = new Button()
helpButton.Text <- "Close"
helpButton.Location <- new Point(150, 250)
helpButton.Click.Add (fun _ -> helpForm.Close())
helpForm.Controls.Add helpButton |> ignore

//helpForm end

//mainForm start

let mainForm = new Form()
mainForm.Visible <- true
mainForm.Text <- "NoisyOne"
mainForm.Size <- new Size(700, 500)

//imgBox start

let imgBox = new PictureBox()
imgBox.SizeMode <- PictureBoxSizeMode.StretchImage
imgBox.BorderStyle <- BorderStyle.FixedSingle
imgBox.Size <- new Size(mainForm.Width - 210, mainForm.Height - 50)
mainForm.Controls.Add imgBox

//imgBox end

//layerText start

let layerText = new TextBox()
layerText.Text <- "8"
layerText.Location <- new Point(550, 0)
mainForm.Controls.Add layerText

//layerText end

//interBox start

let interBox = new ComboBox()
interBox.Location <- new Point(550, 50)
interBox.Items.Add("Linear") |> ignore
interBox.Items.Add("Sinusoidal") |> ignore
interBox.Items.Add("Polynomial 3rd") |> ignore
interBox.Items.Add("Polynomial 5th") |> ignore
mainForm.Controls.Add interBox

//interBox end

//colorText start

let minColorText = new TextBox()
let maxColorText = new TextBox()
minColorText.Text <- "0x000000"
maxColorText.Text <- "0xFFFFFF"
minColorText.Location <- new Point(550, 100)
maxColorText.Location <- new Point(550, 150)
mainForm.Controls.Add minColorText
mainForm.Controls.Add maxColorText

//colorText end

//interBox start

let modeBox = new ComboBox()
modeBox.Location <- new Point(550, 200)
modeBox.Items.Add("Simplex") |> ignore
modeBox.Items.Add("Perlin")  |> ignore
mainForm.Controls.Add modeBox

//interBox end

//generating start

let buttonGen = new Button()
buttonGen.Text <- "Generate"
buttonGen.Location <- new Point(550, 250)
let buttonGenFun _ =
  if not initiated then
    MessageBox.Show("First initiate new noise.", "Not so fast") |> ignore
  else if interBox.SelectedIndex < 0 then
    MessageBox.Show("First choose interpolation.", "Not so fast") |> ignore
  else if modeBox.SelectedIndex < 0 then
    MessageBox.Show("First choose mode.", "Not so fast") |> ignore
  else
    let extractColor code =
      (code / 256 / 256, (code / 256) % 256, code % 256)
    let minColor = int minColorText.Text
    let maxColor = int maxColorText.Text
    bitmap <- make_noise board (choose_inter interBox.SelectedIndex) 512 512 (extractColor minColor) (extractColor maxColor) (int layerText.Text) modeBox.SelectedIndex;
  imgBox.Image <- bitmap
buttonGen.Click.Add buttonGenFun |> ignore
mainForm.Controls.Add buttonGen

//generating end

//mainMenu start

mainForm.Menu <- new MainMenu()

let mainMenu = mainForm.Menu

//menuFile start

let menuFile = mainMenu.MenuItems.Add "File" 

let newNoise = new MenuItem "New noise"
let newNoiseFun _ =
  dice <- new Random();
  seed <- dice.Next();
  dice <- new Random(seed);
  bitmap <- null;
  imgBox.Image <- bitmap;
  initiated <- true;
  board <- generate 513 dice
newNoise.Click.Add newNoiseFun
menuFile.MenuItems.Add newNoise |> ignore

let saveNoise = new MenuItem "Save noise"
saveNoise.Click.Add (fun _ -> ())//saving table
menuFile.MenuItems.Add saveNoise |> ignore

let loadNoise = new MenuItem "Load noise"
loadNoise.Click.Add (fun _ -> ())//loading table
menuFile.MenuItems.Add loadNoise |> ignore

let saveBMP = new MenuItem "Save image"
let saveBMPDialog = new SaveFileDialog()
saveBMPDialog.AddExtension <- true
saveBMPDialog.DefaultExt <- "bmp"
saveBMPDialog.Filter <- "Bmp files (*.bmp)|*.bmp|All files (*.*)|*.*"
let saveBMPFun _ =
  match bitmap with
  | null -> MessageBox.Show("First make some noise.", "Not yet") |> ignore
  | sth  -> 
    if saveBMPDialog.ShowDialog() = DialogResult.OK then
      (saveBMPDialog.FileName, ImageFormat.Bmp) |> bitmap.Save
saveBMP.Click.Add saveBMPFun
menuFile.MenuItems.Add saveBMP |> ignore

let exit = new MenuItem "Exit"
exit.Click.Add(fun _ -> mainForm.Close())
menuFile.MenuItems.Add exit |> ignore

//menuFile end

//menuHelp start

let menuHelp = mainMenu.MenuItems.Add "Help"

let help = new MenuItem "Help"
help.Click.Add (fun _ -> helpForm.Show())//some help in rich textbox
menuHelp.MenuItems.Add help |> ignore

let about = new MenuItem "About"
about.Click.Add (fun _ -> MessageBox.Show("Izrafel made this.\nFor personal use only.","About") |> ignore)
menuHelp.MenuItems.Add about |> ignore

//menuHelp end

//mainMenu end

//mainForm end

//forms end

//main start
 
[<STAThread>]
Application.Run(mainForm);;
  
//main end