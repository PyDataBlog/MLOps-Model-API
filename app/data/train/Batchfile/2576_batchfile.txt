@echo off
FOR /l %%b in (0,1,9) do (
  FOR /l %%a in (0,1,23) DO (
     ren "map-3 [www.imagesplitter.net]-%%a-%%b.jpeg" "%%a-%%b.jpeg"
  )
)