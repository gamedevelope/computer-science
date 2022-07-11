#lang racket/base

(module+ main
  (require raylib/2d/unsafe)

  (InitWindow 800 600 "raylib [core] example - basic window")
  
  (let loop ()
    (when (not (WindowShouldClose))
      (BeginDrawing)
      (ClearBackground RAYWHITE)
      (DrawFPS 10 10)
      (DrawText (string-append "X:" (number->string (GetMouseX))) 190 200 20 DARKGRAY)
      (DrawText (string-append "Y:" (number->string (GetMouseY))) 280 200 20 DARKGRAY)
      (DrawText (string-append "Key:" (number->string (GetKeyPressed))) 340 200 20 DARKGRAY)
      (EndDrawing)
      (loop)))
  
  (CloseWindow))