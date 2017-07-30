# [GUI-SDL2][]

**GUI-SDL2** is **SDL2**,**SDL2_image** and **SDL2_ttf** based desktop GUI that is completely written in Haskell.

This instruction for install GUI-SDL2 use SDL 2.0.5 and haskell packages sdl2 2.2.0 and sdl2-ttf 1.0.0, 
sdl2-image 2.0.0. 

This GUI is in the design process now. 
   
Before you get started, you should:

-   Install **SDL**, **sdl2**, **sdl2-ttf**  

-   Load sources from  https://github.com/haskell-game/sdl2 , 
    https://github.com/carldong/sdl2-ttf and https://github.com/sbidin/sdl2-image.git.
    I assume the installation of project directories as subdirectories of one common directory.
       
-   For each **sdl2**, **sdl2-ttf**, **sdl2-image** need add `cpp-options: -D_SDL_main_h` (bug fix).
         
-   For **sdl2-ttf**, **sdl2-image** add path to **sdl2** project directory in `stack.yaml`, `packages:` section.
    (`stack.yaml` was created after `stack init`).

-   Unpack this package (GUI-SDL2) in same directory as the directories **sdl2**, **sdl2-ttf**, **sdl2-image**.
    (It's not necessary, but I did it and it's easier.)

-   Modify `stack.yaml`, `packages:` section of this package if need (check paths).

``` sh
# Build and run the GUIDemo .
stack build --flag GUI-SDL2:examples --exec GUIDemo
```
- When you first start **GUIDemo**, you will receive a message about which directory you want 
  to copy the **GUI.Resources** directory. Copy to the first of the directories specified in this message. 

- Change **EXAMPLE_NUM** in `src\Main.hs` to other numbers **0,1,2...** by selecting different examples to view and 
  repeat `stack build --flag GUI-SDL2:examples --exec GUIDemo`. 

[GUI-SDL2]: https://github.com/KolodeznyDiver/GUI-SDL2
