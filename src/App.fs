module App

open Fable.Core.JsInterop
open Fable.Import
open GameOfLife

let window = Browser.Dom.window

// Get our canvas context 
// As we'll see later, myCanvas is mutable hence the use of the mutable keyword
// the unbox keyword allows to make an unsafe cast. Here we assume that getElementById will return an HTMLCanvasElement 
let myCanvas : Browser.Types.HTMLCanvasElement = unbox window.document.getElementById "myCanvas"  // myCanvas is defined in public/index.html

let mutable p = seq {
  yield [| false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false |] |> Seq.ofArray
  yield [| false; false; false; false; false; false; false; false; false; false; false; true; false; false; false; false; false; false; false; false; false; false; false; false |] |> Seq.ofArray
  yield [| false; false; false; false; false; false; false; false; false; false; false; true; false; false; false; false; false; false; false; false; false; false; false; false |] |> Seq.ofArray
  yield [| false; false; false; false; false; false; false; false; false; false; false; true; false; false; false; false; false; false; false; false; false; false; false; false |] |> Seq.ofArray
  yield [| false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false |] |> Seq.ofArray
  yield [| false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false |] |> Seq.ofArray
  yield [| false; false; true; false; false; false; false; false; false; false; false; false ; false; false; true; false; false; false; false; false; false; false; false; false  |] |> Seq.ofArray
  yield [| false; false; false; true; false; false; false; false; false; false; false; false ; false; false; false; true; false; false; false; false; false; false; false; false  |] |> Seq.ofArray
  yield [| false; true; true; true; false; false; false; false; false; false; false; false   ; false; true; true; true; false; false; false; false; false; false; false; false    |] |> Seq.ofArray
  yield [| false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false |] |> Seq.ofArray
  yield [| false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false |] |> Seq.ofArray
  yield [| false; true; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false |] |> Seq.ofArray
  yield [| false; true; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false |] |> Seq.ofArray
  yield [| false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false |] |> Seq.ofArray
  yield [| false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false |] |> Seq.ofArray
  yield [| false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false |] |> Seq.ofArray
  yield [| false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false |] |> Seq.ofArray
  yield [| false; false; true; false; false; false; false; false; false; false; false; false ; false; false; true; false; false; false; false; false; false; false; false; false  |] |> Seq.ofArray
  yield [| false; false; false; true; false; false; false; false; false; false; false; false ; false; false; false; true; false; false; false; false; false; false; false; false  |] |> Seq.ofArray
  yield [| false; true; true; true; false; false; false; false; false; false; false; false   ; false; true; true; true; false; false; false; false; false; false; false; false    |] |> Seq.ofArray
  yield [| false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false |] |> Seq.ofArray
  yield [| false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false |] |> Seq.ofArray
  yield [| false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false |] |> Seq.ofArray
  yield [| false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false; false |] |> Seq.ofArray
}

// Get the context
let ctx = myCanvas.getContext_2d()

// All these are immutables values
let w = myCanvas.width
let h = myCanvas.height
let squareSize = 20

// resize our canvas to the size of our grid
// the arrow <- indicates we're mutating a value. It's a special operator in F#.
myCanvas.width <- 1000.
myCanvas.height <- 1000.

let draw() =
  p <- Tick p GridType.Unbound
              
  ctx.clearRect(0., 0., w, h)
  
  ctx.strokeStyle <- !^"ddd"
  
  p 
  |> Seq.iteri 
    (fun y row ->
      row 
      |> Seq.iteri 
        (fun x live -> 
          if live
          then ctx.fillStyle <- !^"#000"
          else ctx.fillStyle <- !^"#fff"
          ctx.fillRect(float(x * squareSize), float(y * squareSize), float(squareSize), float(squareSize))
          ctx.rect(float(x * squareSize), float(y * squareSize), float(squareSize), float(squareSize))
        )
    )
  
  ctx.stroke()

window.setInterval(draw, 200, [||]) |> ignore
