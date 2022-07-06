@if exist _mixins.scss del _mixins.scss
dir /b /s /t:c /o:d .\modules\*.mixins.scss>_tmp.txt

@if exist _mixins.scss del _mixins.scss
for /f %%f in (_tmp.txt) do (
  echo.@import "%%f";>>_mixins.scss
)
