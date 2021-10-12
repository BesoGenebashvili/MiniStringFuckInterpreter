open System
open System.Windows.Forms
open System.Drawing

let asString = List.toArray >> String

let interpretMiniStringFuck (code: string) =
    let rec resolve cell =
        function
        | '+' :: xs -> resolve ((cell + 1) % 256) xs
        | '.' :: xs -> char cell :: resolve cell xs
        | _   :: xs -> resolve cell xs
        | _         -> []
    
    Seq.toList code
    |> resolve 0
    |> asString

let textToMiniStringFuck (text: string) =
    let rec accumulate cell =
        function
        | x :: xs when int x = cell -> '.' :: accumulate cell xs
        | x :: xs                   -> '+' :: accumulate ((cell + 1) % 256) (x :: xs)
        | _                         -> []
    
    Seq.toList text
    |> accumulate 0
    |> asString

let showInterpreterForm =
    let font (name: string) emSize =
        new Font(name,
                 emSize,
                 FontStyle.Regular,
                 GraphicsUnit.Point)

    let firaCodeFont =
        font "Fira Code"

    let inputTextBox = 
        new TextBox(Font = firaCodeFont 15.75F,
                    Size = Size(560, 222),
                    Multiline = true,
                    Location = Point(12, 12),
                    ScrollBars = ScrollBars.Vertical,
                    Text = textToMiniStringFuck "Hello world!")

    let outputTextBox = 
        new TextBox(Font = font "Arial" 15.75F,
                    Size = Size(560, 103),
                    Multiline = true,
                    Location = Point(12, 346),
                    ReadOnly = true,
                    Text = "Hello world!")

    let label = 
        new Label(Font = firaCodeFont 20.25F,
                  Location = Point(166, 275),
                  Size = Size(253, 33),
                  Text = "MiniStringFuck")

    let form = new Form(Text = "MSFI",
                        Width = 600,
                        Height = 500,
                        MaximizeBox = false,
                        AutoScaleMode = AutoScaleMode.None,
                        FormBorderStyle = FormBorderStyle.FixedSingle)

    let inputTextBoxTextChangedHandler = 
        EventHandler(
            fun _ _ -> outputTextBox.Text <- interpretMiniStringFuck inputTextBox.Text)

    inputTextBox.TextChanged.AddHandler inputTextBoxTextChangedHandler
    form.Controls.AddRange [| inputTextBox; outputTextBox; label |]
    form.Show()